/*
 * shard_shm.h - Cross-platform shared memory backend for shard R package
 *
 * Platform support:
 * - Linux/macOS: POSIX shm_open + mmap
 * - Windows: File-backed memory mapping (CreateFileMapping/MapViewOfFile)
 *
 * CRAN compatibility:
 * - No fork() requirement
 * - Finalizer cleanup for all resources
 * - Secure temp file permissions (0600)
 */

#ifndef SHARD_SHM_H
#define SHARD_SHM_H

#include "shard_r.h"
#include <stddef.h>
#include <stdint.h>

/* Backing types for shared memory segments */
typedef enum {
    SHARD_BACKING_AUTO = 0,  /* Let system choose */
    SHARD_BACKING_MMAP = 1,  /* File-backed mmap */
    SHARD_BACKING_SHM  = 2   /* POSIX shared memory (Unix) / named mapping (Windows) */
} shard_backing_t;

/* Shared memory segment handle.
 *
 * Lifetime (Phase 3.4): a shard_segment_t may be shared by several R handles
 * (external pointers) via the per-process attach registry. `refcount` counts
 * live handles; shard_segment_close() only unmaps/frees when the last handle
 * closes. Structs obtained from shard_segment_create() are never registered
 * and always have refcount == 1. All calls happen on the R main thread of a
 * single process (each worker has its own registry), so no locking is needed.
 */
typedef struct shard_segment {
    void *addr;              /* Mapped address */
    size_t size;             /* Size in bytes */
    shard_backing_t backing; /* Backing type */
    int readonly;            /* Read-only flag */
    char *path;              /* File path (mmap) or shm name */
    int owns_shm;            /* Whether this handle owns the underlying segment */
    int refcount;            /* Live handles sharing this struct (>= 1) */
    int in_registry;         /* Linked into the process attach registry */
    struct shard_segment *reg_next; /* Attach registry list link */
#ifdef _WIN32
    void *file_handle;       /* Windows file handle */
    void *map_handle;        /* Windows mapping handle */
#else
    int fd;                  /* File descriptor (Unix) */
    unsigned long long id_dev; /* st_dev at map time (staleness validation) */
    unsigned long long id_ino; /* st_ino at map time (staleness validation) */
#endif
} shard_segment_t;

/* Initialize the shared memory subsystem (called on package load) */
attribute_visible void shard_shm_init(void);

/* Cleanup the shared memory subsystem (called on package unload) */
attribute_visible void shard_shm_cleanup(void);

/*
 * Create a new shared memory segment
 *
 * @param size     Size in bytes
 * @param backing  Backing type (auto, mmap, shm)
 * @param path     Optional file path for mmap (NULL for temp file)
 * @param readonly Create as read-only (after initial write)
 * @return         Pointer to segment handle, or NULL on error
 */
shard_segment_t *shard_segment_create(size_t size, shard_backing_t backing,
                                      const char *path, int readonly);

/*
 * Open an existing shared memory segment
 *
 * @param path     Path or shm name
 * @param backing  Backing type
 * @param readonly Open as read-only
 * @return         Pointer to segment handle, or NULL on error
 */
shard_segment_t *shard_segment_open(const char *path, shard_backing_t backing,
                                    int readonly);

/*
 * Attach to an existing shared memory segment through the per-process
 * attach registry (Phase 3.4).
 *
 * Semantics are identical to shard_segment_open(), except that repeated
 * attaches to the same live (path, backing, readonly) segment within one
 * process return the SAME shard_segment_t with its refcount bumped instead
 * of a fresh open+fstat+mmap. Staleness (the path unlinked and recreated)
 * is detected by comparing the (st_dev, st_ino) of a freshly opened fd
 * against the identity captured when the cached entry was mapped: an open
 * fd pins its inode, so equality proves it is the same file object.
 *
 * On Windows this conservatively degrades to shard_segment_open() (no
 * caching): there is no cheap dev/inode staleness validation and named
 * mappings are already deduplicated by the kernel object namespace.
 *
 * @param path     Path or shm name
 * @param backing  Backing type
 * @param readonly Open as read-only
 * @return         Pointer to (possibly shared) segment handle, or NULL
 */
shard_segment_t *shard_segment_attach(const char *path, shard_backing_t backing,
                                      int readonly);

/*
 * Close and release a shared memory segment.
 *
 * With the attach registry, this decrements the refcount and only tears the
 * mapping down (munmap/close/free + optional unlink) when the last handle
 * closes.
 *
 * @param seg      Segment handle
 * @param unlink   If true, unlink the underlying file/shm
 */
void shard_segment_close(shard_segment_t *seg, int unlink);

/*
 * Get the mapped address of a segment
 */
void *shard_segment_addr(shard_segment_t *seg);

/*
 * Get the size of a segment
 */
size_t shard_segment_size(shard_segment_t *seg);

/*
 * Get the path/name of a segment
 */
const char *shard_segment_path(shard_segment_t *seg);

/*
 * Copy data into a segment (for initialization)
 */
int shard_segment_write(shard_segment_t *seg, const void *data, size_t offset,
                        size_t size);

/*
 * Make a segment read-only after initialization
 */
int shard_segment_protect(shard_segment_t *seg);

/*
 * Generate a unique temporary path for shared memory
 */
char *shard_temp_path(const char *prefix);

/* R-callable functions (registered in init.c) */
attribute_visible SEXP C_shard_segment_create(SEXP size, SEXP backing, SEXP path, SEXP readonly);
attribute_visible SEXP C_shard_segment_open(SEXP path, SEXP backing, SEXP readonly);
attribute_visible SEXP C_shard_segment_close(SEXP seg_ptr, SEXP unlink);
attribute_visible SEXP C_shard_segment_addr(SEXP seg_ptr);
attribute_visible SEXP C_shard_segment_size(SEXP seg_ptr);
attribute_visible SEXP C_shard_segment_path(SEXP seg_ptr);
attribute_visible SEXP C_shard_segment_write_raw(SEXP seg_ptr, SEXP data, SEXP offset);
attribute_visible SEXP C_shard_segment_read_raw(SEXP seg_ptr, SEXP offset, SEXP size);
attribute_visible SEXP C_shard_segment_gather_read(SEXP seg_ptr, SEXP idx, SEXP type);
attribute_visible SEXP C_shard_segment_scatter_write(SEXP seg_ptr, SEXP idx, SEXP values, SEXP type);
attribute_visible SEXP C_shard_segment_protect(SEXP seg_ptr);

/* Wrap an already-open shard_segment_t* in an externalptr with finalizer */
attribute_visible SEXP shard_segment_wrap_xptr(shard_segment_t *seg);

/* Get segment info as list */
attribute_visible SEXP C_shard_segment_info(SEXP seg_ptr);

/* Check if running on Windows */
attribute_visible SEXP C_shard_is_windows(void);

/* Check available backing types */
attribute_visible SEXP C_shard_available_backings(void);

/* Attach registry diagnostics (Phase 3.4): attach/map/hit counters and
 * live registry contents, for tests and benchmarking. */
attribute_visible SEXP C_shard_shm_registry_stats(void);

#endif /* SHARD_SHM_H */
