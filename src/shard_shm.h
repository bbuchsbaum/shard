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

/* Shared memory segment handle */
typedef struct shard_segment {
    void *addr;              /* Mapped address */
    size_t size;             /* Size in bytes */
    shard_backing_t backing; /* Backing type */
    int readonly;            /* Read-only flag */
    char *path;              /* File path (mmap) or shm name */
    int owns_shm;            /* Whether this handle owns the underlying segment */
#ifdef _WIN32
    void *file_handle;       /* Windows file handle */
    void *map_handle;        /* Windows mapping handle */
#else
    int fd;                  /* File descriptor (Unix) */
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
 * Close and release a shared memory segment
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
attribute_visible SEXP C_shard_segment_protect(SEXP seg_ptr);

/* Wrap an already-open shard_segment_t* in an externalptr with finalizer */
attribute_visible SEXP shard_segment_wrap_xptr(shard_segment_t *seg);

/* Get segment info as list */
attribute_visible SEXP C_shard_segment_info(SEXP seg_ptr);

/* Check if running on Windows */
attribute_visible SEXP C_shard_is_windows(void);

/* Check available backing types */
attribute_visible SEXP C_shard_available_backings(void);

#endif /* SHARD_SHM_H */
