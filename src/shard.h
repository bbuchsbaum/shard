/*
 * shard - Deterministic, Zero-Copy Parallel Execution for R
 *
 * Core header file with shared memory structures and platform abstractions
 */

#ifndef SHARD_H
#define SHARD_H

#include <R.h>
#include <Rinternals.h>
#include <stdint.h>
#include <stddef.h>

/* Platform detection */
#ifdef _WIN32
  #define SHARD_WINDOWS 1
#elif defined(__APPLE__)
  #define SHARD_MACOS 1
#else
  #define SHARD_LINUX 1
#endif

/* Backing store types */
typedef enum {
    SHARD_BACKING_AUTO = 0,
    SHARD_BACKING_MMAP = 1,
    SHARD_BACKING_SHM  = 2
} shard_backing_t;

/* Segment types */
typedef enum {
    SHARD_SEGMENT_INPUT  = 0,
    SHARD_SEGMENT_OUTPUT = 1
} shard_segment_type_t;

/* Data types for buffers */
typedef enum {
    SHARD_TYPE_DOUBLE  = REALSXP,
    SHARD_TYPE_INTEGER = INTSXP,
    SHARD_TYPE_LOGICAL = LGLSXP,
    SHARD_TYPE_RAW     = RAWSXP
} shard_type_t;

/* Shared segment descriptor */
typedef struct {
    char            *name;          /* Segment name for shm or file path for mmap */
    void            *data;          /* Mapped memory pointer */
    size_t           size;          /* Total size in bytes */
    shard_backing_t  backing;       /* Backing store type */
    shard_type_t     type;          /* R data type */
    int              readonly;      /* Read-only flag */
    int              refcount;      /* Reference count for attached workers */

    /* Dimensions */
    int              ndim;
    R_xlen_t        *dims;

    /* Diagnostics counters */
    uint64_t         dataptr_calls;
    uint64_t         materialize_calls;
    uint64_t         materialized_bytes;
} shard_segment_t;

/* Function prototypes - segment management */
shard_segment_t* shard_create_segment(SEXP x, shard_backing_t backing,
                                       const char *path, int readonly);
shard_segment_t* shard_attach_segment(const char *handle);
void             shard_detach_segment(shard_segment_t *seg);
void             shard_destroy_segment(shard_segment_t *seg);

/* Function prototypes - buffer management */
shard_segment_t* shard_create_buffer(shard_type_t type, int ndim,
                                      R_xlen_t *dims, shard_backing_t backing,
                                      const char *path);
int              shard_buffer_write(shard_segment_t *seg, R_xlen_t *indices,
                                     size_t n_indices, SEXP values);

/* Function prototypes - ALTREP */
void             shard_init_altrep(DllInfo *dll);
SEXP             shard_make_altrep(shard_segment_t *seg);

/* Utility macros */
#define SHARD_ERROR(msg) Rf_error("shard: %s", msg)
#define SHARD_WARN(msg)  Rf_warning("shard: %s", msg)

#endif /* SHARD_H */
