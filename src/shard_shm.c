/*
 * shard_shm.c - Cross-platform shared memory implementation
 *
 * This file provides the core shared memory functionality for the shard package.
 * It supports three platforms:
 * - Linux: POSIX shm_open + mmap, /dev/shm for shm backing
 * - macOS: POSIX shm_open + mmap (with size limitations)
 * - Windows: CreateFileMapping + MapViewOfFile
 *
 * All platforms support file-backed mmap as a fallback.
 */

#include "shard_shm.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>

#ifdef _WIN32
/* Windows implementation */
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <process.h>  /* _getpid() */
#define getpid _getpid

#else
/* Unix implementation (Linux, macOS) */
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

/* Check for POSIX shared memory support */
#if defined(__linux__) || defined(__APPLE__)
#define HAVE_SHM_OPEN 1
#endif

#endif /* _WIN32 */

/* Maximum path length for temp files */
#define SHARD_MAX_PATH 4096

/* Prefix for POSIX shm object names. Kept short: the whole name (INCLUDING
 * this leading-slash prefix) must fit within PSHMNAMLEN, which is only 31 on
 * Darwin. See shard_shm_name() for the compact hex encoding. */
#define SHARD_SHM_PREFIX "/shd"

/* Simple counter for unique IDs (thread-safe not needed for R) */
static unsigned int shard_counter = 0;

/* Get a unique ID for path generation (avoids system RNG) */
static unsigned int shard_unique_id(void) {
    return ++shard_counter;
}

/*
 * Per-process attach registry (Phase 3.4)
 *
 * Workers attach to the same shared object for every task that touches it;
 * without a registry that is one open+fstat+mmap round trip per task. The
 * registry is a singly-linked list of live, attached (not created) segments.
 * shard_segment_attach() consults it; shard_segment_close() removes an entry
 * when its last handle closes (unmap-at-zero -- no zero-ref cache).
 *
 * Why unmap-at-zero instead of an LRU cache: it is correct by construction
 * (a registry entry always has >= 1 live handle, so a cached mapping can
 * never outlive its references, and no shm/disk space is pinned for
 * unlinked-but-cached files), it needs no flush hooks at shutdown, and it
 * still captures the win: consecutive tasks in a worker overlap because R
 * finalizes lazily, so the previous task's mapping is almost always still
 * live when the next task attaches.
 *
 * Registry key: exact path string + backing + readonly mode, validated by
 * (st_dev, st_ino) of a freshly opened fd against the identity captured at
 * map time. On a real-inode filesystem the entry's own fd stays open for its
 * lifetime and therefore pins the inode: the kernel cannot recycle (dev, ino)
 * into a different file while the entry lives, so dev/ino equality proves
 * identity and a recreated file at the same path (new inode) can never
 * produce a stale hit. This is the default file-backed mmap path everywhere,
 * and Linux /dev/shm (the real POSIX shm platform), which have genuine
 * inodes.
 *
 * Degenerate identity: some platforms report (dev, ino) == (0, 0) for POSIX
 * shm objects (e.g. macOS), so an unlink+recreate at the same path would
 * match every key field and wrong-hit a stale mapping. Such objects bypass
 * the registry entirely (mapped directly, never registered), so the
 * cross-process reuse optimization simply does not apply to them and
 * correctness is preserved. In practice this case is unreachable through the
 * package API (macOS shm segment creation fails outright, generated shm names
 * are unique per process, and explicit paths downgrade to file-backed mmap);
 * the guard is belt-and-suspenders.
 *
 * Single-threaded by design: all attach/close calls run on the R main
 * thread of one process (each worker process has its own registry).
 */
static shard_segment_t *g_attach_registry = NULL;

/* Diagnostics counters (exposed via C_shard_shm_registry_stats) */
static unsigned long long g_attach_calls = 0; /* shard_segment_attach calls */
static unsigned long long g_attach_hits = 0;  /* attaches served from registry */
static unsigned long long g_map_calls = 0;    /* actual mmap/MapViewOfFile ops */

static void registry_remove(shard_segment_t *seg) {
    if (!seg->in_registry) return;
    shard_segment_t **pp = &g_attach_registry;
    while (*pp && *pp != seg) pp = &(*pp)->reg_next;
    if (*pp) *pp = seg->reg_next;
    seg->in_registry = 0;
    seg->reg_next = NULL;
}

/* Initialize subsystem */
void shard_shm_init(void) {
    /* Seed the counter with current time */
    shard_counter = (unsigned int)time(NULL);
}

/* Cleanup subsystem */
void shard_shm_cleanup(void) {
    /* Nothing to cleanup currently - segments are cleaned via finalizers */
}

/*
 * Generate a unique temporary path
 */
char *shard_temp_path(const char *prefix) {
    char *path = (char *)malloc(SHARD_MAX_PATH);
    if (!path) return NULL;

#ifdef _WIN32
    char temp_dir[MAX_PATH];
    if (GetTempPathA(MAX_PATH, temp_dir) == 0) {
        free(path);
        return NULL;
    }
    /* Generate unique filename */
    snprintf(path, SHARD_MAX_PATH, "%s%s%lld_%d",
             temp_dir, prefix ? prefix : "shard_",
             (long long)time(NULL), shard_unique_id());
#else
    const char *tmpdir = getenv("TMPDIR");
    if (!tmpdir) tmpdir = getenv("TMP");
    if (!tmpdir) tmpdir = getenv("TEMP");
    if (!tmpdir) tmpdir = "/tmp";

    snprintf(path, SHARD_MAX_PATH, "%s/%s%d_%lld_%d",
             tmpdir, prefix ? prefix : "shard_",
             (int)getpid(), (long long)time(NULL), shard_unique_id());
#endif

    return path;
}

/*
 * Generate a unique shm name (for POSIX shm_open)
 */
static char *shard_shm_name(void) {
    char *name = (char *)malloc(256);
    if (!name) return NULL;

    /*
     * POSIX shm object names must fit within PSHMNAMLEN (only 31 on Darwin),
     * INCLUDING the leading slash. The old decimal "/shard_<pid>_<time>_<ctr>"
     * form ran ~34 chars (10-digit epoch + a counter seeded from time), so
     * shm_open() always failed with ENAMETOOLONG on macOS. Encode pid/time/
     * counter in hex to stay within the limit while preserving uniqueness:
     * the pid makes the name unique per live process and the monotonic counter
     * makes it unique across rapid successive calls within a process. Worst
     * case (all fields a full 32-bit hex): "/shd" + 8 + "_" + 8 + "_" + 8 = 30
     * characters, safely <= 31 on every platform.
     */
    snprintf(name, 256, "%s%x_%x_%x",
             SHARD_SHM_PREFIX,
             (unsigned int)getpid(),
             (unsigned int)time(NULL),
             (unsigned int)shard_unique_id());
    return name;
}

#ifdef _WIN32
/*
 * Windows implementation
 */

shard_segment_t *shard_segment_create(size_t size, shard_backing_t backing,
                                      const char *path, int readonly) {
    shard_segment_t *seg = (shard_segment_t *)calloc(1, sizeof(shard_segment_t));
    if (!seg) return NULL;

    seg->size = size;
    seg->backing = (backing == SHARD_BACKING_AUTO) ? SHARD_BACKING_MMAP : backing;
    seg->readonly = 0;  /* Start writable for initialization */
    seg->owns_shm = 1;

    DWORD protect = PAGE_READWRITE;
    DWORD access = FILE_MAP_ALL_ACCESS;

    if (seg->backing == SHARD_BACKING_SHM && path == NULL) {
        /* Named kernel mapping (no file on disk) */
        char *name = shard_shm_name();
        if (!name) {
            free(seg);
            return NULL;
        }
        seg->path = name;

        LARGE_INTEGER li;
        li.QuadPart = size;

        seg->map_handle = CreateFileMappingA(
            INVALID_HANDLE_VALUE,  /* Use page file */
            NULL,                  /* Default security */
            protect,
            li.HighPart,
            li.LowPart,
            name
        );

        if (seg->map_handle == NULL) {
            free(seg->path);
            free(seg);
            return NULL;
        }
        seg->file_handle = INVALID_HANDLE_VALUE;
    } else {
        /* File-backed mapping (MMAP or SHM with explicit path) */
        seg->backing = SHARD_BACKING_MMAP;

        if (path == NULL) {
            /* Generate a temp file path, mirroring Unix behavior */
            char *temp_path = shard_temp_path("shard_");
            if (!temp_path) {
                free(seg);
                return NULL;
            }
            seg->path = temp_path;
        } else {
            seg->path = strdup(path);
            if (!seg->path) {
                free(seg);
                return NULL;
            }
        }

        /* Create the file with secure permissions */
        SECURITY_ATTRIBUTES sa;
        sa.nLength = sizeof(SECURITY_ATTRIBUTES);
        sa.lpSecurityDescriptor = NULL;
        sa.bInheritHandle = FALSE;

        seg->file_handle = CreateFileA(
            seg->path,
            GENERIC_READ | GENERIC_WRITE,
            FILE_SHARE_READ | FILE_SHARE_WRITE,
            &sa,
            CREATE_ALWAYS,
            FILE_ATTRIBUTE_NORMAL,
            NULL
        );

        if (seg->file_handle == INVALID_HANDLE_VALUE) {
            free(seg->path);
            free(seg);
            return NULL;
        }

        /* Set file size */
        LARGE_INTEGER li;
        li.QuadPart = size;
        if (!SetFilePointerEx(seg->file_handle, li, NULL, FILE_BEGIN) ||
            !SetEndOfFile(seg->file_handle)) {
            CloseHandle(seg->file_handle);
            DeleteFileA(seg->path);
            free(seg->path);
            free(seg);
            return NULL;
        }

        /* Create file mapping */
        seg->map_handle = CreateFileMappingA(
            seg->file_handle,
            NULL,
            protect,
            li.HighPart,
            li.LowPart,
            NULL
        );

        if (seg->map_handle == NULL) {
            CloseHandle(seg->file_handle);
            DeleteFileA(seg->path);
            free(seg->path);
            free(seg);
            return NULL;
        }
    }

    /* Map the view */
    seg->addr = MapViewOfFile(seg->map_handle, access, 0, 0, size);
    if (seg->addr == NULL) {
        CloseHandle(seg->map_handle);
        if (seg->file_handle != INVALID_HANDLE_VALUE) {
            CloseHandle(seg->file_handle);
            DeleteFileA(seg->path);
        }
        free(seg->path);
        free(seg);
        return NULL;
    }

    seg->refcount = 1;
    g_map_calls++;
    return seg;
}

shard_segment_t *shard_segment_open(const char *path, shard_backing_t backing,
                                    int readonly) {
    shard_segment_t *seg = (shard_segment_t *)calloc(1, sizeof(shard_segment_t));
    if (!seg) return NULL;

    seg->backing = backing;
    seg->readonly = readonly;
    seg->owns_shm = 0;
    seg->path = strdup(path);
    if (!seg->path) {
        free(seg);
        return NULL;
    }

    DWORD access = readonly ? FILE_MAP_READ : FILE_MAP_ALL_ACCESS;

    if (backing == SHARD_BACKING_SHM) {
        /* Open existing named mapping */
        seg->map_handle = OpenFileMappingA(access, FALSE, path);
        if (seg->map_handle == NULL) {
            free(seg->path);
            free(seg);
            return NULL;
        }
        seg->file_handle = INVALID_HANDLE_VALUE;
    } else {
        /* Open existing file */
        DWORD file_access = readonly ? GENERIC_READ : (GENERIC_READ | GENERIC_WRITE);
        DWORD share = FILE_SHARE_READ | FILE_SHARE_WRITE;

        seg->file_handle = CreateFileA(
            path, file_access, share, NULL, OPEN_EXISTING, 0, NULL);

        if (seg->file_handle == INVALID_HANDLE_VALUE) {
            free(seg->path);
            free(seg);
            return NULL;
        }

        /* Get file size */
        LARGE_INTEGER li;
        if (!GetFileSizeEx(seg->file_handle, &li)) {
            CloseHandle(seg->file_handle);
            free(seg->path);
            free(seg);
            return NULL;
        }
        seg->size = (size_t)li.QuadPart;

        /* Create mapping */
        DWORD protect = readonly ? PAGE_READONLY : PAGE_READWRITE;
        seg->map_handle = CreateFileMappingA(
            seg->file_handle, NULL, protect, 0, 0, NULL);

        if (seg->map_handle == NULL) {
            CloseHandle(seg->file_handle);
            free(seg->path);
            free(seg);
            return NULL;
        }
    }

    /* Map the view */
    seg->addr = MapViewOfFile(seg->map_handle, access, 0, 0, 0);
    if (seg->addr == NULL) {
        CloseHandle(seg->map_handle);
        if (seg->file_handle != INVALID_HANDLE_VALUE) {
            CloseHandle(seg->file_handle);
        }
        free(seg->path);
        free(seg);
        return NULL;
    }

    /* For named mappings, get size from the view */
    if (backing == SHARD_BACKING_SHM) {
        MEMORY_BASIC_INFORMATION mbi;
        if (VirtualQuery(seg->addr, &mbi, sizeof(mbi))) {
            seg->size = mbi.RegionSize;
        }
    }

    seg->refcount = 1;
    g_map_calls++;
    return seg;
}

/*
 * Windows: conservative pass-through (no registry caching). There is no
 * cheap dev/inode staleness validation here, and named mappings are already
 * deduplicated by the kernel object namespace. Counters still record the
 * attach so diagnostics stay meaningful cross-platform.
 */
shard_segment_t *shard_segment_attach(const char *path, shard_backing_t backing,
                                      int readonly) {
    g_attach_calls++;
    return shard_segment_open(path, backing, readonly);
}

void shard_segment_close(shard_segment_t *seg, int unlink) {
    if (!seg) return;

    /* Shared handle (attach registry): drop one reference only. */
    if (seg->refcount > 1) {
        seg->refcount--;
        return;
    }
    registry_remove(seg);

    if (seg->addr) {
        UnmapViewOfFile(seg->addr);
        seg->addr = NULL;
    }

    if (seg->map_handle) {
        CloseHandle(seg->map_handle);
        seg->map_handle = NULL;
    }

    if (seg->file_handle != INVALID_HANDLE_VALUE) {
        CloseHandle(seg->file_handle);
        seg->file_handle = INVALID_HANDLE_VALUE;
    }

    if (unlink && seg->owns_shm && seg->path) {
        if (seg->backing == SHARD_BACKING_MMAP) {
            DeleteFileA(seg->path);
        }
        /* Named mappings are automatically cleaned up when all handles close */
    }

    if (seg->path) {
        free(seg->path);
        seg->path = NULL;
    }

    free(seg);
}

int shard_segment_protect(shard_segment_t *seg) {
    if (!seg || !seg->addr) return -1;

    /*
     * Change the protection of the mapped view to read-only, mirroring the
     * mprotect(PROT_READ) semantics of the Unix implementation. This is
     * valid for views created with MapViewOfFile: PAGE_READONLY is
     * compatible with mappings created as PAGE_READWRITE.
     */
    DWORD old_protect = 0;
    if (!VirtualProtect(seg->addr, seg->size, PAGE_READONLY, &old_protect)) {
        return -1;
    }
    seg->readonly = 1;
    return 0;
}

#else
/*
 * Unix implementation (Linux, macOS)
 */

shard_segment_t *shard_segment_create(size_t size, shard_backing_t backing,
                                      const char *path, int readonly) {
    shard_segment_t *seg = (shard_segment_t *)calloc(1, sizeof(shard_segment_t));
    if (!seg) return NULL;

    seg->size = size;
    seg->readonly = 0;  /* Start writable for initialization */
    seg->owns_shm = 1;

    /* Choose backing type */
    if (backing == SHARD_BACKING_AUTO) {
#if defined(__linux__) && defined(HAVE_SHM_OPEN)
        /* On Linux, POSIX shm lives on /dev/shm (tmpfs): RAM-backed, no
         * disk writeback for GB-scale segments. Only when the caller did
         * not request an explicit file path; explicit paths must remain
         * file-backed mmap. Cleanup goes through the existing
         * SHARD_BACKING_SHM shm_unlink path in shard_segment_close(). */
        backing = (path == NULL) ? SHARD_BACKING_SHM : SHARD_BACKING_MMAP;
#else
        /* Default to mmap elsewhere: shm_open has name/size restrictions
         * on macOS and may fail; mmap works consistently on all
         * Unix-like systems. (Windows resolves AUTO in its own branch.) */
        backing = SHARD_BACKING_MMAP;
#endif
    }
    seg->backing = backing;

#ifdef HAVE_SHM_OPEN
    if (backing == SHARD_BACKING_SHM && path == NULL) {
        /* Create POSIX shared memory segment */
        char *name = shard_shm_name();
        if (!name) {
            free(seg);
            return NULL;
        }
        seg->path = name;

        /* Create shm with secure permissions (0600) */
        seg->fd = shm_open(name, O_CREAT | O_EXCL | O_RDWR, S_IRUSR | S_IWUSR);
        if (seg->fd < 0) {
            free(seg->path);
            free(seg);
            return NULL;
        }

        /* Set size */
        if (ftruncate(seg->fd, (off_t)size) < 0) {
            close(seg->fd);
            shm_unlink(name);
            free(seg->path);
            free(seg);
            return NULL;
        }
    } else
#endif
    {
        /* File-backed mmap */
        char *temp_path = NULL;
        if (path == NULL) {
            temp_path = shard_temp_path("shard_");
            if (!temp_path) {
                free(seg);
                return NULL;
            }
            seg->path = temp_path;
        } else {
            seg->path = strdup(path);
            if (!seg->path) {
                free(seg);
                return NULL;
            }
        }
        seg->backing = SHARD_BACKING_MMAP;

        /* Create file with secure permissions (0600) */
        seg->fd = open(seg->path, O_CREAT | O_EXCL | O_RDWR, S_IRUSR | S_IWUSR);
        if (seg->fd < 0) {
            free(seg->path);
            free(seg);
            return NULL;
        }

        /* Set size */
        if (ftruncate(seg->fd, (off_t)size) < 0) {
            close(seg->fd);
            unlink(seg->path);
            free(seg->path);
            free(seg);
            return NULL;
        }
    }

    /* Map the segment */
    int prot = PROT_READ | PROT_WRITE;
    seg->addr = mmap(NULL, size, prot, MAP_SHARED, seg->fd, 0);
    if (seg->addr == MAP_FAILED) {
        close(seg->fd);
        if (seg->backing == SHARD_BACKING_SHM) {
#ifdef HAVE_SHM_OPEN
            shm_unlink(seg->path);
#endif
        } else {
            unlink(seg->path);
        }
        free(seg->path);
        free(seg);
        return NULL;
    }

    seg->refcount = 1;
    g_map_calls++;
    return seg;
}

/*
 * Phase 3.5: advisory paging hints for freshly mapped readonly segments.
 * Return values are deliberately ignored -- madvise is purely advisory and
 * failure must never affect correctness.
 */
static void segment_advise_readonly(shard_segment_t *seg) {
    if (!seg || !seg->addr || !seg->readonly || seg->size == 0) return;
#ifdef MADV_WILLNEED
    (void)madvise(seg->addr, seg->size, MADV_WILLNEED);
#endif
#ifdef MADV_HUGEPAGE
    /* Linux only: transparent huge pages for large segments (>= 2 MB)
     * cut TLB pressure on first full scans. */
    if (seg->size >= (size_t)2 * 1024 * 1024) {
        (void)madvise(seg->addr, seg->size, MADV_HUGEPAGE);
    }
#endif
}

/* Open the fd for an existing segment (shm_open or open by backing). */
static int segment_open_fd(const char *path, shard_backing_t backing,
                           int readonly) {
    int flags = readonly ? O_RDONLY : O_RDWR;
#ifdef HAVE_SHM_OPEN
    if (backing == SHARD_BACKING_SHM) {
        return shm_open(path, flags, 0);
    }
#endif
    return open(path, flags, 0);
}

/*
 * Build a segment handle around an already-open fd (takes ownership of fd:
 * on failure the fd is closed). `st` must be the result of fstat(fd).
 */
static shard_segment_t *segment_map_fd(int fd, const char *path,
                                       shard_backing_t backing, int readonly,
                                       const struct stat *st) {
    shard_segment_t *seg = (shard_segment_t *)calloc(1, sizeof(shard_segment_t));
    if (!seg) {
        close(fd);
        return NULL;
    }

    seg->backing = backing;
    seg->readonly = readonly;
    seg->owns_shm = 0;
    seg->fd = fd;
    seg->size = (size_t)st->st_size;
    seg->id_dev = (unsigned long long)st->st_dev;
    seg->id_ino = (unsigned long long)st->st_ino;
    seg->refcount = 1;
    seg->path = strdup(path);
    if (!seg->path) {
        close(fd);
        free(seg);
        return NULL;
    }

    int prot = readonly ? PROT_READ : (PROT_READ | PROT_WRITE);
    seg->addr = mmap(NULL, seg->size, prot, MAP_SHARED, seg->fd, 0);
    if (seg->addr == MAP_FAILED) {
        close(seg->fd);
        free(seg->path);
        free(seg);
        return NULL;
    }

    g_map_calls++;
    segment_advise_readonly(seg);
    return seg;
}

shard_segment_t *shard_segment_open(const char *path, shard_backing_t backing,
                                    int readonly) {
    int fd = segment_open_fd(path, backing, readonly);
    if (fd < 0) return NULL;

    struct stat st;
    if (fstat(fd, &st) < 0) {
        close(fd);
        return NULL;
    }

    return segment_map_fd(fd, path, backing, readonly, &st);
}

/*
 * Phase 3.4: registry-backed attach. See the registry comment block at the
 * top of this file for the keying/staleness invariants.
 */
shard_segment_t *shard_segment_attach(const char *path, shard_backing_t backing,
                                      int readonly) {
    g_attach_calls++;

    int fd = segment_open_fd(path, backing, readonly);
    if (fd < 0) return NULL;

    struct stat st;
    if (fstat(fd, &st) < 0) {
        close(fd);
        return NULL;
    }

    /*
     * Degenerate (dev, ino) == (0, 0) cannot serve as an identity key (an
     * unlink+recreate at the same path would match every field), so bypass
     * the registry entirely: map directly and do not register. See the
     * registry comment block for why this case does not arise in practice.
     */
    if ((unsigned long long)st.st_dev == 0 && (unsigned long long)st.st_ino == 0) {
        return segment_map_fd(fd, path, backing, readonly, &st);
    }

    /*
     * Registry lookup. A hit requires the exact same path string, backing,
     * and access mode, AND the same (dev, ino) as when the entry was mapped.
     * The entry's open fd pins its inode, so (dev, ino) equality with a
     * freshly opened fd proves the path still names the same file object;
     * an unlink+recreate at the same path yields a new inode and misses.
     */
    for (shard_segment_t *e = g_attach_registry; e; e = e->reg_next) {
        if (e->backing == backing && e->readonly == readonly &&
            e->id_dev == (unsigned long long)st.st_dev &&
            e->id_ino == (unsigned long long)st.st_ino &&
            e->path && strcmp(e->path, path) == 0) {
            close(fd);
            e->refcount++;
            g_attach_hits++;
            return e;
        }
    }

    /* Miss: map from the fd we already hold and register the entry. */
    shard_segment_t *seg = segment_map_fd(fd, path, backing, readonly, &st);
    if (!seg) return NULL;

    seg->in_registry = 1;
    seg->reg_next = g_attach_registry;
    g_attach_registry = seg;
    return seg;
}

void shard_segment_close(shard_segment_t *seg, int unlink_seg) {
    if (!seg) return;

    /* Shared handle (attach registry): drop one reference only. The mapping
     * stays valid for the remaining handles; unlink requests are moot here
     * because registry entries never own the underlying file (owns_shm=0). */
    if (seg->refcount > 1) {
        seg->refcount--;
        return;
    }
    registry_remove(seg);

    if (seg->addr && seg->addr != MAP_FAILED) {
        munmap(seg->addr, seg->size);
        seg->addr = NULL;
    }

    if (seg->fd >= 0) {
        close(seg->fd);
        seg->fd = -1;
    }

    if (unlink_seg && seg->owns_shm && seg->path) {
#ifdef HAVE_SHM_OPEN
        if (seg->backing == SHARD_BACKING_SHM) {
            shm_unlink(seg->path);
        } else
#endif
        {
            unlink(seg->path);
        }
    }

    if (seg->path) {
        free(seg->path);
        seg->path = NULL;
    }

    free(seg);
}

int shard_segment_protect(shard_segment_t *seg) {
    if (!seg || !seg->addr) return -1;

    if (mprotect(seg->addr, seg->size, PROT_READ) < 0) {
        return -1;
    }
    seg->readonly = 1;
    return 0;
}

#endif /* _WIN32 */

/*
 * Common functions (platform-independent)
 */

void *shard_segment_addr(shard_segment_t *seg) {
    return seg ? seg->addr : NULL;
}

size_t shard_segment_size(shard_segment_t *seg) {
    return seg ? seg->size : 0;
}

const char *shard_segment_path(shard_segment_t *seg) {
    return seg ? seg->path : NULL;
}

int shard_segment_write(shard_segment_t *seg, const void *data, size_t offset,
                        size_t size) {
    if (!seg || !seg->addr || !data) return -1;
    if (seg->readonly) return -1;
    /* Overflow-safe bounds check: offset + size can wrap size_t. */
    if (offset > seg->size || size > seg->size - offset) return -1;

    memcpy((char *)seg->addr + offset, data, size);
    return 0;
}

static size_t shard_segment_type_size(int type) {
    switch (type) {
        case REALSXP: return sizeof(double);
        case INTSXP: return sizeof(int);
        case LGLSXP: return sizeof(int);
        case RAWSXP: return sizeof(Rbyte);
        default:
            error("Unsupported buffer type");
    }
    return 0; /* unreachable */
}

static void shard_segment_check_type(SEXP values, int type) {
    if (TYPEOF(values) != type) {
        error("Values do not match requested buffer type");
    }
}

static size_t shard_segment_index_offset(shard_segment_t *seg, SEXP idx,
                                         R_xlen_t pos, size_t elem_size) {
    if (TYPEOF(idx) != INTSXP) {
        error("idx must be an integer vector");
    }

    int one_based = INTEGER(idx)[pos];
    if (one_based == NA_INTEGER || one_based < 1) {
        error("Index out of bounds");
    }

    size_t zero_based = (size_t)(one_based - 1);
    if (elem_size != 0 && zero_based > SIZE_MAX / elem_size) {
        error("Index offset overflow");
    }

    size_t offset = zero_based * elem_size;
    if (offset > seg->size || elem_size > seg->size - offset) {
        error("Index out of bounds");
    }

    return offset;
}

/*
 * Validate an R-supplied byte offset/length (arriving as a double) before
 * casting to size_t. A negative value wraps to a huge size_t, and a
 * non-finite or out-of-range value is undefined on cast; either would defeat
 * the downstream bounds checks and drive out-of-bounds access. `what` names
 * the argument for the error message.
 */
static size_t shard_checked_byte_arg(double d, const char *what) {
    if (!R_FINITE(d) || d < 0 || d > (double)SIZE_MAX) {
        error("%s must be a finite, non-negative byte count within range", what);
    }
    return (size_t)d;
}

/*
 * R interface functions
 */

/* Finalizer for segment external pointer */
static void segment_finalizer(SEXP ptr) {
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(ptr);
    if (seg) {
        /*
         * IMPORTANT: only unlink the underlying shm/file when this handle owns
         * it. Worker-opened handles must not unlink, or they can break later
         * re-opens (e.g., after worker recycling) and can invalidate other
         * processes expecting the name/path to remain valid.
         */
        shard_segment_close(seg, seg->owns_shm ? 1 : 0);
        R_ClearExternalPtr(ptr);
    }
}

/*
 * Wrap a shard_segment_t* in an externalptr with the standard finalizer.
 * This is used by both the R-callable create/open entrypoints and by other
 * C code (e.g., ALTREP unserialize) that needs to open segments.
 */
SEXP shard_segment_wrap_xptr(shard_segment_t *seg) {
    if (!seg) {
        error("Invalid segment");
    }

    SEXP ptr = PROTECT(R_MakeExternalPtr(seg, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, segment_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

/* Create segment and return external pointer */
SEXP C_shard_segment_create(SEXP size, SEXP backing, SEXP path, SEXP readonly) {
    double dsize = REAL(size)[0];
    if (!R_FINITE(dsize) || dsize <= 0 || dsize > (double)SIZE_MAX) {
        error("Invalid size");
    }

    shard_backing_t back = (shard_backing_t)INTEGER(backing)[0];
    const char *cpath = (path == R_NilValue) ? NULL : CHAR(STRING_ELT(path, 0));
    int ro = LOGICAL(readonly)[0];

    shard_segment_t *seg = shard_segment_create((size_t)dsize, back, cpath, ro);
    if (!seg) {
        error("Failed to create shared memory segment");
    }

    return shard_segment_wrap_xptr(seg);
}

/* Open existing segment (via the attach registry: repeated opens of the
 * same live segment share one mapping, see shard_segment_attach) */
SEXP C_shard_segment_open(SEXP path, SEXP backing, SEXP readonly) {
    const char *cpath = CHAR(STRING_ELT(path, 0));
    shard_backing_t back = (shard_backing_t)INTEGER(backing)[0];
    int ro = LOGICAL(readonly)[0];

    shard_segment_t *seg = shard_segment_attach(cpath, back, ro);
    if (!seg) {
        error("Failed to open shared memory segment");
    }

    return shard_segment_wrap_xptr(seg);
}

/* Close segment */
SEXP C_shard_segment_close(SEXP seg_ptr, SEXP unlink) {
    if (TYPEOF(seg_ptr) != EXTPTRSXP) {
        error("Invalid segment pointer");
    }

    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (seg) {
        shard_segment_close(seg, LOGICAL(unlink)[0]);
        R_ClearExternalPtr(seg_ptr);
    }

    return R_NilValue;
}

/* Get address (as raw memory info) */
SEXP C_shard_segment_addr(SEXP seg_ptr) {
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (!seg) error("Invalid segment");

    /* Return as double (portable way to represent pointer address) */
    return ScalarReal((double)(uintptr_t)seg->addr);
}

/* Get size */
SEXP C_shard_segment_size(SEXP seg_ptr) {
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (!seg) error("Invalid segment");

    return ScalarReal((double)seg->size);
}

/* Get path */
SEXP C_shard_segment_path(SEXP seg_ptr) {
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (!seg) error("Invalid segment");

    if (seg->path) {
        return ScalarString(mkChar(seg->path));
    }
    return R_NilValue;
}

/* Write raw data to segment */
SEXP C_shard_segment_write_raw(SEXP seg_ptr, SEXP data, SEXP offset) {
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (!seg) error("Invalid segment");
    if (seg->readonly) error("Segment is read-only");

    size_t off = shard_checked_byte_arg(REAL(offset)[0], "offset");
    /* Use XLENGTH: LENGTH() truncates (or errors) for long vectors (> 2^31-1
     * elements), which are exactly the workload shared segments target. */
    size_t len = (size_t)XLENGTH(data);

    if (TYPEOF(data) == RAWSXP) {
        if (shard_segment_write(seg, RAW(data), off, len) < 0) {
            error("Write failed");
        }
    } else if (TYPEOF(data) == REALSXP) {
        len *= sizeof(double);
        if (shard_segment_write(seg, REAL(data), off, len) < 0) {
            error("Write failed");
        }
    } else if (TYPEOF(data) == INTSXP) {
        len *= sizeof(int);
        if (shard_segment_write(seg, INTEGER(data), off, len) < 0) {
            error("Write failed");
        }
    } else if (TYPEOF(data) == LGLSXP) {
        len *= sizeof(int);
        if (shard_segment_write(seg, LOGICAL(data), off, len) < 0) {
            error("Write failed");
        }
    } else {
        error("Unsupported data type");
    }

    return ScalarReal((double)len);
}

/* Read raw data from segment */
SEXP C_shard_segment_read_raw(SEXP seg_ptr, SEXP offset, SEXP size) {
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (!seg) error("Invalid segment");

    size_t off = shard_checked_byte_arg(REAL(offset)[0], "offset");
    size_t len = shard_checked_byte_arg(REAL(size)[0], "size");

    /* Overflow-safe bounds check: off + len can wrap size_t. */
    if (off > seg->size || len > seg->size - off) {
        error("Read exceeds segment bounds");
    }

    SEXP result = PROTECT(allocVector(RAWSXP, len));
    memcpy(RAW(result), (char *)seg->addr + off, len);
    UNPROTECT(1);

    return result;
}

/* Gather typed elements at 1-based indices from a segment.
 *
 * Each index is validated exactly once (shard_segment_index_offset both
 * bounds-checks and computes the byte offset), and the type dispatch is
 * hoisted out of the per-element loop: one typed loop per branch instead of
 * a switch per element. shard_segment_type_size() has already rejected any
 * unsupported type, so the SEXP type passed to allocVector is always valid.
 */
SEXP C_shard_segment_gather_read(SEXP seg_ptr, SEXP idx, SEXP type) {
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (!seg || !seg->addr) error("Invalid segment");

    int sexp_type = asInteger(type);
    size_t elem_size = shard_segment_type_size(sexp_type);
    R_xlen_t nidx = XLENGTH(idx);

    SEXP result = PROTECT(allocVector(sexp_type, nidx));
    const char *base = (const char *)seg->addr;

    switch (sexp_type) {
        case REALSXP: {
            double *out = REAL(result);
            for (R_xlen_t k = 0; k < nidx; k++) {
                size_t off = shard_segment_index_offset(seg, idx, k, elem_size);
                memcpy(&out[k], base + off, sizeof(double));
            }
            break;
        }
        case INTSXP: {
            int *out = INTEGER(result);
            for (R_xlen_t k = 0; k < nidx; k++) {
                size_t off = shard_segment_index_offset(seg, idx, k, elem_size);
                memcpy(&out[k], base + off, sizeof(int));
            }
            break;
        }
        case LGLSXP: {
            int *out = LOGICAL(result);
            for (R_xlen_t k = 0; k < nidx; k++) {
                size_t off = shard_segment_index_offset(seg, idx, k, elem_size);
                memcpy(&out[k], base + off, sizeof(int));
            }
            break;
        }
        case RAWSXP: {
            Rbyte *out = RAW(result);
            for (R_xlen_t k = 0; k < nidx; k++) {
                size_t off = shard_segment_index_offset(seg, idx, k, elem_size);
                memcpy(&out[k], base + off, sizeof(Rbyte));
            }
            break;
        }
        default:
            error("Unsupported buffer type");
    }

    UNPROTECT(1);
    return result;
}

/* Scatter typed values to 1-based indices in input order. */
SEXP C_shard_segment_scatter_write(SEXP seg_ptr, SEXP idx, SEXP values, SEXP type) {
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (!seg || !seg->addr) error("Invalid segment");
    if (seg->readonly) error("Segment is read-only");

    int sexp_type = asInteger(type);
    shard_segment_check_type(values, sexp_type);

    size_t elem_size = shard_segment_type_size(sexp_type);
    R_xlen_t nidx = XLENGTH(idx);
    if (XLENGTH(values) != nidx) {
        error("values length must match idx length");
    }

    /* One typed loop per branch; each index validated once (index_offset
     * bounds-checks and computes the byte offset in a single pass). */
    char *base = (char *)seg->addr;
    switch (sexp_type) {
        case REALSXP: {
            const double *in = REAL(values);
            for (R_xlen_t k = 0; k < nidx; k++) {
                size_t off = shard_segment_index_offset(seg, idx, k, elem_size);
                memcpy(base + off, &in[k], sizeof(double));
            }
            break;
        }
        case INTSXP: {
            const int *in = INTEGER(values);
            for (R_xlen_t k = 0; k < nidx; k++) {
                size_t off = shard_segment_index_offset(seg, idx, k, elem_size);
                memcpy(base + off, &in[k], sizeof(int));
            }
            break;
        }
        case LGLSXP: {
            const int *in = LOGICAL(values);
            for (R_xlen_t k = 0; k < nidx; k++) {
                size_t off = shard_segment_index_offset(seg, idx, k, elem_size);
                memcpy(base + off, &in[k], sizeof(int));
            }
            break;
        }
        case RAWSXP: {
            const Rbyte *in = RAW(values);
            for (R_xlen_t k = 0; k < nidx; k++) {
                size_t off = shard_segment_index_offset(seg, idx, k, elem_size);
                memcpy(base + off, &in[k], sizeof(Rbyte));
            }
            break;
        }
        default:
            error("Unsupported buffer type");
    }

    return ScalarReal((double)nidx * (double)elem_size);
}

/*
 * Typed contiguous range read: copy n_elems values of the requested type,
 * starting at a byte offset, directly from the mapped segment into a fresh R
 * vector. This backs a zero-intermediate-copy buffer read from R (no raw
 * staging vector + readBin round trip). Logical values are stored as 4-byte
 * ints, mirroring C_shard_segment_gather_read.
 *
 * @param seg_ptr  segment external pointer
 * @param offset   byte offset into the segment (REALSXP scalar, >= 0)
 * @param n_elems  element count (REALSXP scalar, >= 0)
 * @param type     one of "double", "integer", "logical", "raw" (STRSXP scalar)
 */
SEXP C_shard_segment_read_range(SEXP seg_ptr, SEXP offset, SEXP n_elems, SEXP type) {
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (!seg || !seg->addr) error("Invalid segment");

    if (TYPEOF(type) != STRSXP || XLENGTH(type) < 1) {
        error("type must be a character scalar");
    }
    const char *type_str = CHAR(STRING_ELT(type, 0));
    int sexp_type;
    if (strcmp(type_str, "double") == 0) sexp_type = REALSXP;
    else if (strcmp(type_str, "integer") == 0) sexp_type = INTSXP;
    else if (strcmp(type_str, "logical") == 0) sexp_type = LGLSXP;
    else if (strcmp(type_str, "raw") == 0) sexp_type = RAWSXP;
    else error("Unsupported type: %s", type_str);

    size_t elem_size = shard_segment_type_size(sexp_type);
    size_t off = shard_checked_byte_arg(REAL(offset)[0], "offset");
    size_t n = shard_checked_byte_arg(REAL(n_elems)[0], "n_elems");

    /* Overflow-safe bounds: n * elem_size must fit in seg->size - off, without
     * ever forming the (possibly wrapping) product n * elem_size. */
    if (off > seg->size || n > (seg->size - off) / elem_size) {
        error("Read range exceeds segment bounds");
    }

    SEXP result = PROTECT(allocVector(sexp_type, (R_xlen_t)n));
    if (n > 0) {
        void *dst;
        switch (sexp_type) {
            case REALSXP: dst = REAL(result); break;
            case INTSXP:  dst = INTEGER(result); break;
            case LGLSXP:  dst = LOGICAL(result); break;
            case RAWSXP:  dst = RAW(result); break;
            default:      dst = NULL; break; /* unreachable */
        }
        memcpy(dst, (const char *)seg->addr + off, n * elem_size);
    }
    UNPROTECT(1);
    return result;
}

/* Make segment read-only */
SEXP C_shard_segment_protect(SEXP seg_ptr) {
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (!seg) error("Invalid segment");

    if (shard_segment_protect(seg) < 0) {
        warning("Could not protect segment");
    }

    return R_NilValue;
}

/* Get segment info as list */
SEXP C_shard_segment_info(SEXP seg_ptr) {
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (!seg) error("Invalid segment");

    SEXP result = PROTECT(allocVector(VECSXP, 5));
    SEXP names = PROTECT(allocVector(STRSXP, 5));

    SET_STRING_ELT(names, 0, mkChar("size"));
    SET_STRING_ELT(names, 1, mkChar("backing"));
    SET_STRING_ELT(names, 2, mkChar("path"));
    SET_STRING_ELT(names, 3, mkChar("readonly"));
    SET_STRING_ELT(names, 4, mkChar("owns"));

    SET_VECTOR_ELT(result, 0, ScalarReal((double)seg->size));

    const char *backing_str;
    switch (seg->backing) {
        case SHARD_BACKING_MMAP: backing_str = "mmap"; break;
        case SHARD_BACKING_SHM:  backing_str = "shm"; break;
        default:                 backing_str = "auto"; break;
    }
    SET_VECTOR_ELT(result, 1, ScalarString(mkChar(backing_str)));

    if (seg->path) {
        SET_VECTOR_ELT(result, 2, ScalarString(mkChar(seg->path)));
    } else {
        SET_VECTOR_ELT(result, 2, R_NilValue);
    }

    SET_VECTOR_ELT(result, 3, ScalarLogical(seg->readonly));
    SET_VECTOR_ELT(result, 4, ScalarLogical(seg->owns_shm));

    setAttrib(result, R_NamesSymbol, names);

    UNPROTECT(2);
    return result;
}

/* Attach registry diagnostics (Phase 3.4). Counters are cumulative for the
 * process; live_segments/live_refs reflect the current registry contents. */
SEXP C_shard_shm_registry_stats(void) {
    double live_segments = 0;
    double live_refs = 0;
    for (shard_segment_t *e = g_attach_registry; e; e = e->reg_next) {
        live_segments += 1;
        live_refs += (double)e->refcount;
    }

    SEXP result = PROTECT(allocVector(VECSXP, 5));
    SEXP names = PROTECT(allocVector(STRSXP, 5));

    SET_STRING_ELT(names, 0, mkChar("attach_calls"));
    SET_STRING_ELT(names, 1, mkChar("attach_hits"));
    SET_STRING_ELT(names, 2, mkChar("map_calls"));
    SET_STRING_ELT(names, 3, mkChar("live_segments"));
    SET_STRING_ELT(names, 4, mkChar("live_refs"));

    SET_VECTOR_ELT(result, 0, ScalarReal((double)g_attach_calls));
    SET_VECTOR_ELT(result, 1, ScalarReal((double)g_attach_hits));
    SET_VECTOR_ELT(result, 2, ScalarReal((double)g_map_calls));
    SET_VECTOR_ELT(result, 3, ScalarReal(live_segments));
    SET_VECTOR_ELT(result, 4, ScalarReal(live_refs));

    setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}

/* Check if running on Windows */
SEXP C_shard_is_windows(void) {
#ifdef _WIN32
    return ScalarLogical(TRUE);
#else
    return ScalarLogical(FALSE);
#endif
}

/* Check available backing types */
SEXP C_shard_available_backings(void) {
    SEXP result = PROTECT(allocVector(STRSXP, 2));
    int idx = 0;

    SET_STRING_ELT(result, idx++, mkChar("mmap"));

#if defined(HAVE_SHM_OPEN) || defined(_WIN32)
    SET_STRING_ELT(result, idx++, mkChar("shm"));
#endif

    /* Trim to actual size */
    if (idx < 2) {
        SEXP trimmed = PROTECT(allocVector(STRSXP, idx));
        for (int i = 0; i < idx; i++) {
            SET_STRING_ELT(trimmed, i, STRING_ELT(result, i));
        }
        UNPROTECT(2);
        return trimmed;
    }

    UNPROTECT(1);
    return result;
}
