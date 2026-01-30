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

/* Prefix for temporary shared memory segments */
#define SHARD_SHM_PREFIX "/shard_"

/* Simple counter for unique IDs (thread-safe not needed for R) */
static unsigned int shard_counter = 0;

/* Get a unique ID for path generation (avoids system RNG) */
static unsigned int shard_unique_id(void) {
    return ++shard_counter;
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

    snprintf(name, 256, "%s%d_%lld_%d",
             SHARD_SHM_PREFIX, (int)getpid(), (long long)time(NULL), shard_unique_id());
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

    if (seg->backing == SHARD_BACKING_SHM || path == NULL) {
        /* Create anonymous file mapping (named mapping for cross-process) */
        char *name = shard_shm_name();
        if (!name) {
            free(seg);
            return NULL;
        }
        seg->path = name;

        /* Create named mapping */
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
        /* File-backed mapping */
        seg->path = strdup(path);
        if (!seg->path) {
            free(seg);
            return NULL;
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
            FILE_ATTRIBUTE_TEMPORARY,
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

    return seg;
}

void shard_segment_close(shard_segment_t *seg, int unlink) {
    if (!seg) return;

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

    /* Windows doesn't support changing protection easily after mapping */
    /* The segment is effectively protected by not sharing write access */
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
        /* Default to mmap for reliability across platforms.
         * shm_open has restrictions on macOS and may fail.
         * mmap works consistently on all Unix-like systems. */
        backing = SHARD_BACKING_MMAP;
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

    int flags = readonly ? O_RDONLY : O_RDWR;

#ifdef HAVE_SHM_OPEN
    if (backing == SHARD_BACKING_SHM) {
        seg->fd = shm_open(path, flags, 0);
    } else
#endif
    {
        seg->fd = open(path, flags, 0);
    }

    if (seg->fd < 0) {
        free(seg->path);
        free(seg);
        return NULL;
    }

    /* Get size from file */
    struct stat st;
    if (fstat(seg->fd, &st) < 0) {
        close(seg->fd);
        free(seg->path);
        free(seg);
        return NULL;
    }
    seg->size = (size_t)st.st_size;

    /* Map the segment */
    int prot = readonly ? PROT_READ : (PROT_READ | PROT_WRITE);
    seg->addr = mmap(NULL, seg->size, prot, MAP_SHARED, seg->fd, 0);
    if (seg->addr == MAP_FAILED) {
        close(seg->fd);
        free(seg->path);
        free(seg);
        return NULL;
    }

    return seg;
}

void shard_segment_close(shard_segment_t *seg, int unlink_seg) {
    if (!seg) return;

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
    if (offset + size > seg->size) return -1;

    memcpy((char *)seg->addr + offset, data, size);
    return 0;
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
    if (dsize <= 0 || dsize > (double)SIZE_MAX) {
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

/* Open existing segment */
SEXP C_shard_segment_open(SEXP path, SEXP backing, SEXP readonly) {
    const char *cpath = CHAR(STRING_ELT(path, 0));
    shard_backing_t back = (shard_backing_t)INTEGER(backing)[0];
    int ro = LOGICAL(readonly)[0];

    shard_segment_t *seg = shard_segment_open(cpath, back, ro);
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

    size_t off = (size_t)REAL(offset)[0];
    size_t len = LENGTH(data);

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

    size_t off = (size_t)REAL(offset)[0];
    size_t len = (size_t)REAL(size)[0];

    if (off + len > seg->size) {
        error("Read exceeds segment bounds");
    }

    SEXP result = PROTECT(allocVector(RAWSXP, len));
    memcpy(RAW(result), (char *)seg->addr + off, len);
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
