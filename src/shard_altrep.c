/*
 * shard_altrep.c - ALTREP implementation for zero-copy shared vectors
 *
 * This file implements ALTREP classes for integer, real, logical, and raw
 * vectors backed by shared memory segments. Views (subsets) share the
 * underlying memory without copying.
 *
 * ALTREP data layout:
 *   data1: External pointer to shard_altrep_info struct
 *   data2: Either:
 *     - R_NilValue (normal case / views)
 *     - A non-ALTREP vector holding a private materialized copy (COW on write)
 *
 * The shard_altrep_info struct contains:
 *   - Pointer to the segment external pointer (to prevent GC)
 *   - Optional parent ALTREP (views) so derived views can "see" a parent's
 *     materialized copy, if one exists
 *   - Byte offset into segment
 *   - Element count
 *   - Element size in bytes
 *   - Read-only flag
 *   - Diagnostic counters (dataptr_calls, materialize_calls)
 */

#include "shard_altrep.h"
#include "shard_shm.h"
#include <stdlib.h>
#include <string.h>
#include <R_ext/BLAS.h>
#include <R_ext/RS.h>

/* Info struct stored in ALTREP data1 */
typedef struct shard_altrep_info {
    SEXP segment_ptr;        /* External pointer to segment (protected) */
    SEXP parent;             /* Parent ALTREP for views (or R_NilValue) */
    size_t offset;           /* Byte offset into segment */
    R_xlen_t length;         /* Number of elements */
    size_t element_size;     /* Size of each element in bytes */
    int readonly;            /* Read-only flag */
    int deny_write;          /* Error on write attempts when readonly (cow='deny') */
    int sexp_type;           /* R type (INTSXP, REALSXP, etc.) */

    /* Diagnostic counters */
    R_xlen_t dataptr_calls;      /* Times a data pointer was requested */
    R_xlen_t materialize_calls;  /* Times vector was materialized */
} shard_altrep_info_t;

/* ALTREP class objects - one per supported type */
static R_altrep_class_t shard_int_class;
static R_altrep_class_t shard_real_class;
static R_altrep_class_t shard_lgl_class;
static R_altrep_class_t shard_raw_class;

/* Helper: Get info struct from ALTREP object */
static shard_altrep_info_t *get_info(SEXP x) {
    SEXP data1 = R_altrep_data1(x);
    if (TYPEOF(data1) != EXTPTRSXP) return NULL;
    return (shard_altrep_info_t *)R_ExternalPtrAddr(data1);
}

/* Helper: Get segment struct from info */
static shard_segment_t *get_segment(shard_altrep_info_t *info) {
    if (!info || info->segment_ptr == R_NilValue) return NULL;
    if (TYPEOF(info->segment_ptr) != EXTPTRSXP) return NULL;
    return (shard_segment_t *)R_ExternalPtrAddr(info->segment_ptr);
}

/* Helper: Return the underlying data pointer for a standard (non-ALTREP)
 * atomic vector of the given type. Avoid R's internal dataptr macro so
 * R CMD check does not flag non-API usage. */
static void *vec_data_ptr(SEXP v, int type) {
    if (TYPEOF(v) != type) return NULL;
    switch (type) {
        case INTSXP: return (void *)INTEGER(v);
        case REALSXP: return (void *)REAL(v);
        case LGLSXP: return (void *)LOGICAL(v);
        case RAWSXP: return (void *)RAW(v);
        default: return NULL;
    }
}

/* Helper: Get data pointer for the vector.
 * If the vector has been materialized (data2 is a non-ALTREP vector), returns that.
 * Note: Views store the parent ALTREP in data2 for reference counting, so we
 * must check if data2 is ALTREP (parent ref) vs regular vector (materialized). */
static void *get_data_ptr(SEXP x, shard_altrep_info_t *info) {
    /* Check if we have a materialized copy in data2 */
    SEXP data2 = R_altrep_data2(x);
    if (data2 != R_NilValue && !ALTREP(data2)) {
        /* data2 is a regular vector - this is our materialized COW copy */
        return vec_data_ptr(data2, TYPEOF(data2));
    }

    /*
     * If this is a view, and the parent (or an ancestor) has been
     * materialized, use the ancestor's materialized copy. This ensures that
     * subviews of a materialized view see the updated data.
     */
    shard_altrep_info_t *cur_info = info;
    while (cur_info && cur_info->parent != R_NilValue && ALTREP(cur_info->parent)) {
        SEXP parent = cur_info->parent;
        shard_altrep_info_t *pinfo = get_info(parent);
        if (!pinfo) break;

        SEXP p_data2 = R_altrep_data2(parent);
        if (p_data2 != R_NilValue && !ALTREP(p_data2)) {
            size_t delta = 0;
            if (info->offset >= pinfo->offset) {
                delta = info->offset - pinfo->offset;
            }
            /* delta is in bytes */
            void *pbase = vec_data_ptr(p_data2, TYPEOF(p_data2));
            return pbase ? (char *)pbase + delta : NULL;
        }

        cur_info = pinfo;
    }

    /* Otherwise use the shared memory segment */
    shard_segment_t *seg = get_segment(info);
    if (!seg) return NULL;

    void *base = shard_segment_addr(seg);
    if (!base) return NULL;

    return (char *)base + info->offset;
}

/* Finalizer for info struct */
static void info_finalizer(SEXP ptr) {
    shard_altrep_info_t *info = (shard_altrep_info_t *)R_ExternalPtrAddr(ptr);
    if (info) {
        /* Release protection of segment_ptr */
        R_ReleaseObject(info->segment_ptr);
        if (info->parent != R_NilValue) {
            R_ReleaseObject(info->parent);
        }
        free(info);
        R_ClearExternalPtr(ptr);
    }
}

/* Get the ALTREP class for a given SEXP type */
static R_altrep_class_t get_class_for_type(int type) {
    switch (type) {
        case INTSXP: return shard_int_class;
        case REALSXP: return shard_real_class;
        case LGLSXP: return shard_lgl_class;
        case RAWSXP: return shard_raw_class;
        default: return shard_int_class; /* fallback */
    }
}

/* Get element size for a given SEXP type */
static size_t element_size_for_type(int type) {
    switch (type) {
        case INTSXP: return sizeof(int);
        case REALSXP: return sizeof(double);
        case LGLSXP: return sizeof(int);  /* R logicals are int */
        case RAWSXP: return sizeof(Rbyte);
        default: return 0;
    }
}

/*
 * ALTREP method implementations
 */

/* Length method - required */
static R_xlen_t altrep_length(SEXP x) {
    shard_altrep_info_t *info = get_info(x);
    return info ? info->length : 0;
}

/* Inspect method - for debugging */
static Rboolean altrep_inspect(SEXP x, int pre, int deep, int pvec,
                                void (*inspect_subtree)(SEXP, int, int, int)) {
    shard_altrep_info_t *info = get_info(x);
    if (!info) {
        Rprintf(" shard_altrep (invalid)\n");
        return TRUE;
    }

    Rprintf(" shard_altrep [%s, n=%lld, off=%zu, ro=%d, dp=%lld, mat=%lld]\n",
            type2char(info->sexp_type),
            (long long)info->length,
            info->offset,
            info->readonly,
            (long long)info->dataptr_calls,
            (long long)info->materialize_calls);
    return TRUE;
}

/* Duplicate method - creates a view (no copy) if possible */
static SEXP altrep_duplicate(SEXP x, Rboolean deep) {
    shard_altrep_info_t *info = get_info(x);
    if (!info) return R_NilValue;

    /* For deep copy, materialize to regular vector */
    if (deep) {
        info->materialize_calls++;
        R_xlen_t n = info->length;
        SEXP result = PROTECT(allocVector(info->sexp_type, n));

        void *src = get_data_ptr(x, info);
        void *dst = vec_data_ptr(result, info->sexp_type);
        if (src && dst) {
            memcpy(dst, src, n * info->element_size);
        }
        UNPROTECT(1);
        return result;
    }

    /* Shallow copy: create another ALTREP view */
    shard_altrep_info_t *new_info = (shard_altrep_info_t *)malloc(sizeof(shard_altrep_info_t));
    if (!new_info) return R_NilValue;

    memcpy(new_info, info, sizeof(shard_altrep_info_t));
    new_info->dataptr_calls = 0;
    new_info->materialize_calls = 0;
    new_info->parent = x;

    /* Protect segment reference */
    R_PreserveObject(new_info->segment_ptr);
    R_PreserveObject(new_info->parent);

    SEXP info_ptr = PROTECT(R_MakeExternalPtr(new_info, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(info_ptr, info_finalizer, TRUE);

    /* Create new ALTREP object */
    R_altrep_class_t cls = get_class_for_type(info->sexp_type);
    SEXP result = R_new_altrep(cls, info_ptr, R_NilValue);

    /* Preserve user-visible attributes needed for policy enforcement */
    setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
    {
        SEXP sym_cow = install("shard_cow");
        SEXP sym_ro = install("shard_readonly");
        setAttrib(result, sym_cow, getAttrib(x, sym_cow));
        setAttrib(result, sym_ro, getAttrib(x, sym_ro));
    }

    UNPROTECT(1);
    return result;
}

/* Coerce method - materialize when coercing */
static SEXP altrep_coerce(SEXP x, int type) {
    shard_altrep_info_t *info = get_info(x);
    if (info) info->materialize_calls++;
    return NULL; /* Use default coercion */
}

/* Serialization: serialize as standard vector */
static SEXP altrep_serialized_state(SEXP x) {
    shard_altrep_info_t *info = get_info(x);
    if (!info) return R_NilValue;

    /*
     * Serialize as a small reopenable descriptor (path/backing/offset/length).
     *
     * This is critical for PSOCK clusters: sending a large materialized vector
     * defeats the whole point of shared segments. Workers can re-open the
     * segment by path/name and rebuild the ALTREP wrapper.
     *
     * Fallback: if the segment isn't reopenable (no path/name), materialize.
     */
    shard_segment_t *seg = get_segment(info);
    if (!seg || !seg->path) {
        info->materialize_calls++;
        R_xlen_t n = info->length;
        SEXP result = PROTECT(allocVector(info->sexp_type, n));
        void *src = get_data_ptr(x, info);
        void *dst = vec_data_ptr(result, info->sexp_type);
        if (src && dst) {
            memcpy(dst, src, n * info->element_size);
        }
        UNPROTECT(1);
        return result;
    }

    SEXP state = PROTECT(allocVector(VECSXP, 7));
    SEXP names = PROTECT(allocVector(STRSXP, 7));

    SET_STRING_ELT(names, 0, mkChar("path"));
    SET_STRING_ELT(names, 1, mkChar("backing"));
    SET_STRING_ELT(names, 2, mkChar("offset"));
    SET_STRING_ELT(names, 3, mkChar("length"));
    SET_STRING_ELT(names, 4, mkChar("readonly"));
    SET_STRING_ELT(names, 5, mkChar("cow"));
    SET_STRING_ELT(names, 6, mkChar("type"));
    setAttrib(state, R_NamesSymbol, names);

    SET_VECTOR_ELT(state, 0, ScalarString(mkChar(seg->path)));
    SET_VECTOR_ELT(state, 1, ScalarInteger((int)seg->backing));
    SET_VECTOR_ELT(state, 2, ScalarReal((double)info->offset));
    SET_VECTOR_ELT(state, 3, ScalarReal((double)info->length));
    SET_VECTOR_ELT(state, 4, ScalarLogical(info->readonly));
    {
        SEXP sym_cow = install("shard_cow");
        SEXP cow_attr = getAttrib(x, sym_cow);
        if (TYPEOF(cow_attr) == STRSXP && XLENGTH(cow_attr) > 0) {
            SET_VECTOR_ELT(state, 5, cow_attr);
        } else {
            SET_VECTOR_ELT(state, 5, mkString(info->deny_write ? "deny" : "allow"));
        }
    }
    SET_VECTOR_ELT(state, 6, ScalarInteger(info->sexp_type));

    UNPROTECT(2);
    return state;
}

static SEXP altrep_unserialize(SEXP cls, SEXP state) {
    /*
     * Backwards/compat fallback: if state is a standard vector (older
     * serialization or "unreopenable" fallback), just return it.
     */
    if (TYPEOF(state) != VECSXP || XLENGTH(state) < 7) {
        return state;
    }

    SEXP path_sexp = VECTOR_ELT(state, 0);
    if (TYPEOF(path_sexp) != STRSXP || XLENGTH(path_sexp) < 1) {
        return state;
    }
    const char *path = CHAR(STRING_ELT(path_sexp, 0));

    int backing = INTEGER(VECTOR_ELT(state, 1))[0];
    size_t off = (size_t)REAL(VECTOR_ELT(state, 2))[0];
    R_xlen_t len = (R_xlen_t)REAL(VECTOR_ELT(state, 3))[0];
    int ro = LOGICAL(VECTOR_ELT(state, 4))[0];
    int deny_write = 0;
    const char *cow_str = NULL;
    SEXP cow_sexp = VECTOR_ELT(state, 5);
    if (TYPEOF(cow_sexp) == STRSXP && XLENGTH(cow_sexp) > 0) {
        cow_str = CHAR(STRING_ELT(cow_sexp, 0));
        if (strcmp(cow_str, "deny") == 0) {
            deny_write = 1;
        }
    } else if (TYPEOF(cow_sexp) == LGLSXP) {
        /* Back-compat for older serialized state */
        deny_write = LOGICAL(cow_sexp)[0];
    }
    int sexp_type = INTEGER(VECTOR_ELT(state, 6))[0];

    size_t elem_size = element_size_for_type(sexp_type);
    if (elem_size == 0) {
        error("Unsupported SEXP type in serialized state: %d", sexp_type);
    }

    shard_segment_t *seg = shard_segment_open(path, (shard_backing_t)backing, ro);
    if (!seg) {
        error("Failed to open shared memory segment for ALTREP unserialize");
    }

    /* Validate bounds against segment size */
    size_t seg_size = shard_segment_size(seg);
    if (off + (size_t)len * elem_size > seg_size) {
        shard_segment_close(seg, 0);
        error("Serialized ALTREP range exceeds segment size");
    }

    SEXP seg_xptr = PROTECT(shard_segment_wrap_xptr(seg));

    shard_altrep_info_t *info = (shard_altrep_info_t *)malloc(sizeof(shard_altrep_info_t));
    if (!info) {
        UNPROTECT(1);
        error("Failed to allocate ALTREP info");
    }

    info->segment_ptr = seg_xptr;
    info->parent = R_NilValue;
    info->offset = off;
    info->length = len;
    info->element_size = elem_size;
    info->readonly = ro;
    info->deny_write = deny_write;
    info->sexp_type = sexp_type;
    info->dataptr_calls = 0;
    info->materialize_calls = 0;

    R_PreserveObject(seg_xptr);

    SEXP info_ptr = PROTECT(R_MakeExternalPtr(info, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(info_ptr, info_finalizer, TRUE);

    /* Rebuild as the correct shard ALTREP class for this stored type */
    R_altrep_class_t acls = get_class_for_type(sexp_type);
    SEXP result = R_new_altrep(acls, info_ptr, R_NilValue);

    /* Restore user-visible attributes for policy enforcement */
    setAttrib(result, R_ClassSymbol, mkString("shard_shared_vector"));
    {
        SEXP sym_cow = install("shard_cow");
        SEXP sym_ro = install("shard_readonly");
        if (cow_str) {
            setAttrib(result, sym_cow, mkString(cow_str));
        } else {
            setAttrib(result, sym_cow, mkString(deny_write ? "deny" : "allow"));
        }
        setAttrib(result, sym_ro, ScalarLogical(ro));
    }

    UNPROTECT(2);
    return result;
}

/*
 * ALTVEC methods
 */

/* Dataptr method - returns pointer to data (increments counter) */
static void *altvec_dataptr(SEXP x, Rboolean writable) {
    shard_altrep_info_t *info = get_info(x);
    if (!info) return NULL;

    /*
     * Enforce readonly via copy-on-write: when writable access is requested
     * but the vector is readonly, materialize to a private copy stored in
     * data2. Subsequent accesses will use the materialized copy.
     */
    if (writable && info->readonly) {
        SEXP data2 = R_altrep_data2(x);

        /*
         * data2 is used to stash a private, materialized copy. Views historically
         * stored the parent ALTREP in data2, so treat ALTREP data2 as "not yet
         * materialized" and overwrite it with the materialized vector.
         */
        if (data2 != R_NilValue && !ALTREP(data2)) {
            info->dataptr_calls++;
            return vec_data_ptr(data2, TYPEOF(data2));
        }

        /* Materialize: allocate R vector and copy shared memory data */
        info->materialize_calls++;
        R_xlen_t n = info->length;
        SEXP materialized = PROTECT(allocVector(info->sexp_type, n));

        void *src = get_data_ptr(x, info);
        void *dst = vec_data_ptr(materialized, info->sexp_type);
        if (src && dst && n > 0) {
            memcpy(dst, src, n * info->element_size);
        }

        /* Store in data2 for future access */
        R_set_altrep_data2(x, materialized);
        UNPROTECT(1);

        info->dataptr_calls++;
        return dst;
    }

    info->dataptr_calls++;
    return get_data_ptr(x, info);
}

/* Dataptr_or_null - returns pointer without side effects if possible */
static const void *altvec_dataptr_or_null(SEXP x) {
    shard_altrep_info_t *info = get_info(x);
    if (!info) return NULL;
    return get_data_ptr(x, info);
}

/* Extract subset - returns view when possible */
static SEXP altvec_extract_subset(SEXP x, SEXP indx, SEXP call) {
    shard_altrep_info_t *info = get_info(x);
    if (!info) return NULL;

    /* Check if indices form a contiguous range */
    R_xlen_t n = XLENGTH(indx);
    if (n == 0) {
        return allocVector(info->sexp_type, 0);
    }

    /* Only handle integer indices for now */
    if (TYPEOF(indx) != INTSXP && TYPEOF(indx) != REALSXP) {
        return NULL; /* Use default subsetting */
    }

    /* Check for contiguous range */
    R_xlen_t start, end;
    int contiguous = 1;

    if (TYPEOF(indx) == INTSXP) {
        int *idx = INTEGER(indx);
        if (idx[0] == NA_INTEGER || idx[0] <= 0) {
            return NULL; /* Handle NA/negative indices with default */
        }
        start = idx[0] - 1;  /* Convert to 0-based */
        end = start;

        for (R_xlen_t i = 1; i < n; i++) {
            if (idx[i] == NA_INTEGER || idx[i] <= 0) {
                return NULL;
            }
            if (idx[i] != idx[i-1] + 1) {
                contiguous = 0;
                break;
            }
            end = idx[i] - 1;
        }
    } else {
        double *idx = REAL(indx);
        if (ISNA(idx[0]) || idx[0] <= 0) {
            return NULL;
        }
        start = (R_xlen_t)(idx[0] - 1);
        end = start;

        for (R_xlen_t i = 1; i < n; i++) {
            if (ISNA(idx[i]) || idx[i] <= 0) {
                return NULL;
            }
            if (idx[i] != idx[i-1] + 1) {
                contiguous = 0;
                break;
            }
            end = (R_xlen_t)(idx[i] - 1);
        }
    }

    /* Validate range */
    if (start < 0 || end >= info->length) {
        return NULL; /* Out of bounds - let default handle error */
    }

    /* If contiguous, create a view */
    if (contiguous) {
        R_xlen_t view_len = end - start + 1;

        shard_altrep_info_t *new_info = (shard_altrep_info_t *)malloc(sizeof(shard_altrep_info_t));
        if (!new_info) return NULL;

        new_info->segment_ptr = info->segment_ptr;
        new_info->parent = x;
        new_info->offset = info->offset + start * info->element_size;
        new_info->length = view_len;
        new_info->element_size = info->element_size;
        new_info->readonly = info->readonly;
        new_info->deny_write = info->deny_write;
        new_info->sexp_type = info->sexp_type;
        new_info->dataptr_calls = 0;
        new_info->materialize_calls = 0;

        /* Protect segment reference */
        R_PreserveObject(new_info->segment_ptr);
        R_PreserveObject(new_info->parent);

        SEXP info_ptr = PROTECT(R_MakeExternalPtr(new_info, R_NilValue, R_NilValue));
        R_RegisterCFinalizerEx(info_ptr, info_finalizer, TRUE);

        R_altrep_class_t cls = get_class_for_type(info->sexp_type);
        SEXP result = R_new_altrep(cls, info_ptr, R_NilValue);

        /* Preserve user-visible attributes needed for policy enforcement */
        setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
        {
            SEXP sym_cow = install("shard_cow");
            SEXP sym_ro = install("shard_readonly");
            setAttrib(result, sym_cow, getAttrib(x, sym_cow));
            setAttrib(result, sym_ro, getAttrib(x, sym_ro));
        }

        UNPROTECT(1);
        return result;
    }

    /* Non-contiguous: materialize */
    info->materialize_calls++;
    return NULL; /* Use default subsetting */
}

/*
 * ALTINT methods (integer vectors)
 */

static int altint_elt(SEXP x, R_xlen_t i) {
    shard_altrep_info_t *info = get_info(x);
    if (!info || i < 0 || i >= info->length) return NA_INTEGER;

    int *data = (int *)get_data_ptr(x, info);
    return data ? data[i] : NA_INTEGER;
}

static R_xlen_t altint_get_region(SEXP x, R_xlen_t start, R_xlen_t size, int *buf) {
    shard_altrep_info_t *info = get_info(x);
    if (!info) return 0;

    /* Clamp to valid range */
    if (start >= info->length) return 0;
    if (start + size > info->length) {
        size = info->length - start;
    }

    int *data = (int *)get_data_ptr(x, info);
    if (!data) return 0;

    memcpy(buf, data + start, size * sizeof(int));
    return size;
}

/*
 * ALTREAL methods (real/double vectors)
 */

static double altreal_elt(SEXP x, R_xlen_t i) {
    shard_altrep_info_t *info = get_info(x);
    if (!info || i < 0 || i >= info->length) return NA_REAL;

    double *data = (double *)get_data_ptr(x, info);
    return data ? data[i] : NA_REAL;
}

static R_xlen_t altreal_get_region(SEXP x, R_xlen_t start, R_xlen_t size, double *buf) {
    shard_altrep_info_t *info = get_info(x);
    if (!info) return 0;

    if (start >= info->length) return 0;
    if (start + size > info->length) {
        size = info->length - start;
    }

    double *data = (double *)get_data_ptr(x, info);
    if (!data) return 0;

    memcpy(buf, data + start, size * sizeof(double));
    return size;
}

/*
 * ALTLOGICAL methods
 */

static int altlogical_elt(SEXP x, R_xlen_t i) {
    shard_altrep_info_t *info = get_info(x);
    if (!info || i < 0 || i >= info->length) return NA_LOGICAL;

    int *data = (int *)get_data_ptr(x, info);
    return data ? data[i] : NA_LOGICAL;
}

static R_xlen_t altlogical_get_region(SEXP x, R_xlen_t start, R_xlen_t size, int *buf) {
    /* Same as integer since R logicals are stored as int */
    return altint_get_region(x, start, size, buf);
}

/*
 * ALTRAW methods
 */

static Rbyte altraw_elt(SEXP x, R_xlen_t i) {
    shard_altrep_info_t *info = get_info(x);
    if (!info || i < 0 || i >= info->length) return 0;

    Rbyte *data = (Rbyte *)get_data_ptr(x, info);
    return data ? data[i] : 0;
}

static R_xlen_t altraw_get_region(SEXP x, R_xlen_t start, R_xlen_t size, Rbyte *buf) {
    shard_altrep_info_t *info = get_info(x);
    if (!info) return 0;

    if (start >= info->length) return 0;
    if (start + size > info->length) {
        size = info->length - start;
    }

    Rbyte *data = (Rbyte *)get_data_ptr(x, info);
    if (!data) return 0;

    memcpy(buf, data + start, size * sizeof(Rbyte));
    return size;
}

/*
 * Initialize ALTREP classes
 */
void shard_altrep_init(DllInfo *dll) {
    /* Integer class */
    shard_int_class = R_make_altinteger_class("shard_int", "shard", dll);
    R_set_altrep_Length_method(shard_int_class, altrep_length);
    R_set_altrep_Inspect_method(shard_int_class, altrep_inspect);
    R_set_altrep_Duplicate_method(shard_int_class, altrep_duplicate);
    R_set_altrep_Coerce_method(shard_int_class, altrep_coerce);
    R_set_altrep_Serialized_state_method(shard_int_class, altrep_serialized_state);
    R_set_altrep_Unserialize_method(shard_int_class, altrep_unserialize);

    R_set_altvec_Dataptr_method(shard_int_class, altvec_dataptr);
    R_set_altvec_Dataptr_or_null_method(shard_int_class, altvec_dataptr_or_null);
    R_set_altvec_Extract_subset_method(shard_int_class, altvec_extract_subset);

    R_set_altinteger_Elt_method(shard_int_class, altint_elt);
    R_set_altinteger_Get_region_method(shard_int_class, altint_get_region);

    /* Real class */
    shard_real_class = R_make_altreal_class("shard_real", "shard", dll);
    R_set_altrep_Length_method(shard_real_class, altrep_length);
    R_set_altrep_Inspect_method(shard_real_class, altrep_inspect);
    R_set_altrep_Duplicate_method(shard_real_class, altrep_duplicate);
    R_set_altrep_Coerce_method(shard_real_class, altrep_coerce);
    R_set_altrep_Serialized_state_method(shard_real_class, altrep_serialized_state);
    R_set_altrep_Unserialize_method(shard_real_class, altrep_unserialize);

    R_set_altvec_Dataptr_method(shard_real_class, altvec_dataptr);
    R_set_altvec_Dataptr_or_null_method(shard_real_class, altvec_dataptr_or_null);
    R_set_altvec_Extract_subset_method(shard_real_class, altvec_extract_subset);

    R_set_altreal_Elt_method(shard_real_class, altreal_elt);
    R_set_altreal_Get_region_method(shard_real_class, altreal_get_region);

    /* Logical class */
    shard_lgl_class = R_make_altlogical_class("shard_lgl", "shard", dll);
    R_set_altrep_Length_method(shard_lgl_class, altrep_length);
    R_set_altrep_Inspect_method(shard_lgl_class, altrep_inspect);
    R_set_altrep_Duplicate_method(shard_lgl_class, altrep_duplicate);
    R_set_altrep_Coerce_method(shard_lgl_class, altrep_coerce);
    R_set_altrep_Serialized_state_method(shard_lgl_class, altrep_serialized_state);
    R_set_altrep_Unserialize_method(shard_lgl_class, altrep_unserialize);

    R_set_altvec_Dataptr_method(shard_lgl_class, altvec_dataptr);
    R_set_altvec_Dataptr_or_null_method(shard_lgl_class, altvec_dataptr_or_null);
    R_set_altvec_Extract_subset_method(shard_lgl_class, altvec_extract_subset);

    R_set_altlogical_Elt_method(shard_lgl_class, altlogical_elt);
    R_set_altlogical_Get_region_method(shard_lgl_class, altlogical_get_region);

    /* Raw class */
    shard_raw_class = R_make_altraw_class("shard_raw", "shard", dll);
    R_set_altrep_Length_method(shard_raw_class, altrep_length);
    R_set_altrep_Inspect_method(shard_raw_class, altrep_inspect);
    R_set_altrep_Duplicate_method(shard_raw_class, altrep_duplicate);
    R_set_altrep_Coerce_method(shard_raw_class, altrep_coerce);
    R_set_altrep_Serialized_state_method(shard_raw_class, altrep_serialized_state);
    R_set_altrep_Unserialize_method(shard_raw_class, altrep_unserialize);

    R_set_altvec_Dataptr_method(shard_raw_class, altvec_dataptr);
    R_set_altvec_Dataptr_or_null_method(shard_raw_class, altvec_dataptr_or_null);
    R_set_altvec_Extract_subset_method(shard_raw_class, altvec_extract_subset);

    R_set_altraw_Elt_method(shard_raw_class, altraw_elt);
    R_set_altraw_Get_region_method(shard_raw_class, altraw_get_region);
}

/*
 * R interface functions
 */

/* Create an ALTREP vector from a segment */
SEXP C_shard_altrep_create(SEXP seg, SEXP type, SEXP offset, SEXP length, SEXP readonly, SEXP cow) {
    /* Validate segment */
    if (TYPEOF(seg) != EXTPTRSXP) {
        error("seg must be an external pointer to a segment");
    }
    shard_segment_t *segment = (shard_segment_t *)R_ExternalPtrAddr(seg);
    if (!segment) {
        error("Invalid segment pointer");
    }

    /* Parse type */
    int sexp_type;
    if (TYPEOF(type) == STRSXP) {
        const char *type_str = CHAR(STRING_ELT(type, 0));
        if (strcmp(type_str, "integer") == 0) sexp_type = INTSXP;
        else if (strcmp(type_str, "double") == 0 || strcmp(type_str, "numeric") == 0) sexp_type = REALSXP;
        else if (strcmp(type_str, "logical") == 0) sexp_type = LGLSXP;
        else if (strcmp(type_str, "raw") == 0) sexp_type = RAWSXP;
        else error("Unsupported type: %s", type_str);
    } else {
        sexp_type = INTEGER(type)[0];
    }

    size_t elem_size = element_size_for_type(sexp_type);
    if (elem_size == 0) {
        error("Unsupported SEXP type: %d", sexp_type);
    }

    size_t off = (size_t)REAL(offset)[0];
    R_xlen_t len = (R_xlen_t)REAL(length)[0];
    int ro = LOGICAL(readonly)[0];
    int deny_write = 0;

    if (TYPEOF(cow) == STRSXP && XLENGTH(cow) > 0) {
        const char *cow_str = CHAR(STRING_ELT(cow, 0));
        if (strcmp(cow_str, "deny") == 0) {
            deny_write = 1;
        } else if (strcmp(cow_str, "copy") == 0 ||
                   strcmp(cow_str, "allow") == 0 ||
                   strcmp(cow_str, "audit") == 0) {
            deny_write = 0;
        } else {
            error("Unsupported cow policy: %s", cow_str);
        }
    }

    /* deny_write is only meaningful for readonly shared memory */
    if (!ro) {
        deny_write = 0;
    }

    /* Validate bounds */
    size_t seg_size = shard_segment_size(segment);
    if (off + len * elem_size > seg_size) {
        error("Requested range exceeds segment size (offset=%zu, length=%lld, elem_size=%zu, seg_size=%zu)",
              off, (long long)len, elem_size, seg_size);
    }

    /* Create info struct */
    shard_altrep_info_t *info = (shard_altrep_info_t *)malloc(sizeof(shard_altrep_info_t));
    if (!info) {
        error("Failed to allocate ALTREP info");
    }

    info->segment_ptr = seg;
    info->parent = R_NilValue;
    info->offset = off;
    info->length = len;
    info->element_size = elem_size;
    info->readonly = ro;
    info->deny_write = deny_write;
    info->sexp_type = sexp_type;
    info->dataptr_calls = 0;
    info->materialize_calls = 0;

    /* Protect segment from GC */
    R_PreserveObject(seg);

    /* Create external pointer for info */
    SEXP info_ptr = PROTECT(R_MakeExternalPtr(info, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(info_ptr, info_finalizer, TRUE);

    /* Create ALTREP object */
    R_altrep_class_t cls = get_class_for_type(sexp_type);
    SEXP result = R_new_altrep(cls, info_ptr, R_NilValue);

    /*
     * Mark as a shard shared object and record policy. We use a lightweight
     * S3 class so R-level replacement can enforce cow='deny' without relying
     * on dataptr(writable=TRUE), which some read-only algorithms may request.
     */
    setAttrib(result, R_ClassSymbol, mkString("shard_shared_vector"));
    {
        SEXP sym_cow = install("shard_cow");
        SEXP sym_ro = install("shard_readonly");
        if (TYPEOF(cow) == STRSXP && XLENGTH(cow) > 0) {
            setAttrib(result, sym_cow, cow);
        } else {
            setAttrib(result, sym_cow, mkString("allow"));
        }
        setAttrib(result, sym_ro, ScalarLogical(ro));
    }

    UNPROTECT(1);
    return result;
}

/* Create a view (subset) of an existing ALTREP vector */
SEXP C_shard_altrep_view(SEXP x, SEXP start, SEXP length) {
    if (!ALTREP(x)) {
        error("x must be a shard ALTREP vector");
    }

    shard_altrep_info_t *info = get_info(x);
    if (!info) {
        error("Invalid shard ALTREP vector");
    }

    R_xlen_t st = (R_xlen_t)REAL(start)[0];
    R_xlen_t len = (R_xlen_t)REAL(length)[0];

    /* Validate range */
    if (st < 0 || st >= info->length) {
        error("start index out of bounds");
    }
    if (st + len > info->length) {
        error("view extends beyond vector bounds");
    }

    /* Create new info for the view */
    shard_altrep_info_t *new_info = (shard_altrep_info_t *)malloc(sizeof(shard_altrep_info_t));
    if (!new_info) {
        error("Failed to allocate view info");
    }

    new_info->segment_ptr = info->segment_ptr;
    new_info->parent = x;
    new_info->offset = info->offset + st * info->element_size;
    new_info->length = len;
    new_info->element_size = info->element_size;
    new_info->readonly = info->readonly;
    new_info->deny_write = info->deny_write;
    new_info->sexp_type = info->sexp_type;
    new_info->dataptr_calls = 0;
    new_info->materialize_calls = 0;

    /* Protect segment reference */
    R_PreserveObject(new_info->segment_ptr);
    R_PreserveObject(new_info->parent);

    SEXP info_ptr = PROTECT(R_MakeExternalPtr(new_info, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(info_ptr, info_finalizer, TRUE);

    R_altrep_class_t cls = get_class_for_type(info->sexp_type);
    /* data2 is reserved for a private, materialized COW copy on write. */
    SEXP result = R_new_altrep(cls, info_ptr, R_NilValue);

    /* Preserve user-visible attributes needed for policy enforcement */
    setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
    {
        SEXP sym_cow = install("shard_cow");
        SEXP sym_ro = install("shard_readonly");
        setAttrib(result, sym_cow, getAttrib(x, sym_cow));
        setAttrib(result, sym_ro, getAttrib(x, sym_ro));
    }

    UNPROTECT(1);
    return result;
}

/* Get diagnostic counters */
SEXP C_shard_altrep_diagnostics(SEXP x) {
    if (!ALTREP(x)) {
        error("x must be a shard ALTREP vector");
    }

    shard_altrep_info_t *info = get_info(x);
    if (!info) {
        error("Invalid shard ALTREP vector");
    }

    SEXP result = PROTECT(allocVector(VECSXP, 6));
    SEXP names = PROTECT(allocVector(STRSXP, 6));

    SET_STRING_ELT(names, 0, mkChar("dataptr_calls"));
    SET_STRING_ELT(names, 1, mkChar("materialize_calls"));
    SET_STRING_ELT(names, 2, mkChar("length"));
    SET_STRING_ELT(names, 3, mkChar("offset"));
    SET_STRING_ELT(names, 4, mkChar("readonly"));
    SET_STRING_ELT(names, 5, mkChar("type"));

    SET_VECTOR_ELT(result, 0, ScalarReal((double)info->dataptr_calls));
    SET_VECTOR_ELT(result, 1, ScalarReal((double)info->materialize_calls));
    SET_VECTOR_ELT(result, 2, ScalarReal((double)info->length));
    SET_VECTOR_ELT(result, 3, ScalarReal((double)info->offset));
    SET_VECTOR_ELT(result, 4, ScalarLogical(info->readonly));
    SET_VECTOR_ELT(result, 5, ScalarString(mkChar(type2char(info->sexp_type))));

    setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}

/* Check if object is a shard ALTREP */
SEXP C_is_shard_altrep(SEXP x) {
    if (!ALTREP(x)) return ScalarLogical(FALSE);

    /* Check if it's one of our classes by trying to get info */
    shard_altrep_info_t *info = get_info(x);
    return ScalarLogical(info != NULL);
}

/* Get the underlying segment pointer */
SEXP C_shard_altrep_segment(SEXP x) {
    if (!ALTREP(x)) {
        error("x must be a shard ALTREP vector");
    }

    shard_altrep_info_t *info = get_info(x);
    if (!info) {
        error("Invalid shard ALTREP vector");
    }

    return info->segment_ptr;
}

/* Reset diagnostic counters */
SEXP C_shard_altrep_reset_diagnostics(SEXP x) {
    if (!ALTREP(x)) {
        error("x must be a shard ALTREP vector");
    }

    shard_altrep_info_t *info = get_info(x);
    if (!info) {
        error("Invalid shard ALTREP vector");
    }

    info->dataptr_calls = 0;
    info->materialize_calls = 0;

    return R_NilValue;
}

/* Materialize an ALTREP vector to a standard R vector */
SEXP C_shard_altrep_materialize(SEXP x) {
    if (!ALTREP(x)) {
        error("x must be a shard ALTREP vector");
    }

    shard_altrep_info_t *info = get_info(x);
    if (!info) {
        error("Invalid shard ALTREP vector");
    }

    /* Increment materialize counter */
    info->materialize_calls++;

    /* Allocate standard R vector */
    R_xlen_t n = info->length;
    SEXP result = PROTECT(allocVector(info->sexp_type, n));

    /* Copy data from shared memory to R vector */
    void *src = get_data_ptr(x, info);
    void *dst = vec_data_ptr(result, info->sexp_type);
    if (src && dst && n > 0) {
        memcpy(dst, src, n * info->element_size);
    }

    UNPROTECT(1);
    return result;
}

SEXP C_shard_mat_block_col_sums(SEXP x, SEXP row_start, SEXP row_end,
                                SEXP col_start, SEXP col_end) {
    if (!ALTREP(x)) {
        error("x must be a shard ALTREP vector (shared matrix backing)");
    }

    shard_altrep_info_t *info = get_info(x);
    if (!info) {
        error("Invalid shard ALTREP vector");
    }

    SEXP dim = getAttrib(x, R_DimSymbol);
    if (TYPEOF(dim) != INTSXP || XLENGTH(dim) != 2) {
        error("x must have matrix dimensions");
    }
    int nrow = INTEGER(dim)[0];
    int ncol = INTEGER(dim)[1];
    if (nrow < 0 || ncol < 0) {
        error("invalid matrix dimensions");
    }

    if (info->sexp_type != REALSXP) {
        error("C_shard_mat_block_col_sums currently supports double matrices only");
    }

    int rs = asInteger(row_start);
    int re = asInteger(row_end);
    int cs = asInteger(col_start);
    int ce = asInteger(col_end);

    if (rs == NA_INTEGER || re == NA_INTEGER || cs == NA_INTEGER || ce == NA_INTEGER) {
        error("row/col bounds must be non-NA integers");
    }
    if (rs < 1 || re < rs || re > nrow) {
        error("row bounds out of range");
    }
    if (cs < 1 || ce < cs || ce > ncol) {
        error("col bounds out of range");
    }

    double *data = (double *)get_data_ptr(x, info);
    if (!data) {
        error("failed to access matrix data");
    }

    int out_ncol = ce - cs + 1;
    SEXP out = PROTECT(allocVector(REALSXP, out_ncol));
    double *outp = REAL(out);

    int r0 = rs - 1;
    int r1 = re - 1;
    int c0 = cs - 1;

    for (int j = 0; j < out_ncol; j++) {
        int col_idx = c0 + j;
        R_xlen_t base = (R_xlen_t)col_idx * (R_xlen_t)nrow;

        double acc = 0.0;
        int any_na = 0;
        for (int i = r0; i <= r1; i++) {
            double v = data[base + (R_xlen_t)i];
            if (ISNA(v) || ISNAN(v)) {
                any_na = 1;
                break;
            }
            acc += v;
        }
        outp[j] = any_na ? NA_REAL : acc;
    }

    UNPROTECT(1);
    return out;
}

static double *mat_double_ptr(SEXP x, shard_altrep_info_t **info_out, int *nrow_out, int *ncol_out) {
    if (info_out) *info_out = NULL;

    SEXP dim = getAttrib(x, R_DimSymbol);
    if (TYPEOF(dim) != INTSXP || XLENGTH(dim) != 2) {
        error("x must have matrix dimensions");
    }
    int nrow = INTEGER(dim)[0];
    int ncol = INTEGER(dim)[1];
    if (nrow < 0 || ncol < 0) {
        error("invalid matrix dimensions");
    }

    if (ALTREP(x)) {
        shard_altrep_info_t *info = get_info(x);
        if (!info) {
            error("invalid shard ALTREP backing");
        }
        if (info->sexp_type != REALSXP) {
            error("expected double matrix backing");
        }
        double *p = (double *)get_data_ptr(x, info);
        if (!p) {
            error("failed to access matrix data");
        }
        if (info_out) *info_out = info;
        if (nrow_out) *nrow_out = nrow;
        if (ncol_out) *ncol_out = ncol;
        return p;
    }

    if (TYPEOF(x) != REALSXP) {
        error("expected a double matrix");
    }
    if (nrow_out) *nrow_out = nrow;
    if (ncol_out) *ncol_out = ncol;
    return REAL(x);
}

SEXP C_shard_mat_crossprod_block(SEXP x, SEXP y,
                                 SEXP row_start, SEXP row_end,
                                 SEXP x_col_start, SEXP x_col_end,
                                 SEXP y_col_start, SEXP y_col_end) {
    int x_nrow = 0, x_ncol = 0, y_nrow = 0, y_ncol = 0;
    shard_altrep_info_t *xinfo = NULL, *yinfo = NULL;

    double *xp = mat_double_ptr(x, &xinfo, &x_nrow, &x_ncol);
    double *yp = mat_double_ptr(y, &yinfo, &y_nrow, &y_ncol);

    int rs = asInteger(row_start);
    int re = asInteger(row_end);
    int xcs = asInteger(x_col_start);
    int xce = asInteger(x_col_end);
    int ycs = asInteger(y_col_start);
    int yce = asInteger(y_col_end);

    if (rs == NA_INTEGER || re == NA_INTEGER || xcs == NA_INTEGER || xce == NA_INTEGER ||
        ycs == NA_INTEGER || yce == NA_INTEGER) {
        error("bounds must be non-NA integers");
    }

    if (rs < 1 || re < rs) error("row bounds out of range");
    if (rs > x_nrow || re > x_nrow) error("row bounds exceed x nrow");
    if (rs > y_nrow || re > y_nrow) error("row bounds exceed y nrow");

    if (xcs < 1 || xce < xcs || xce > x_ncol) error("x col bounds out of range");
    if (ycs < 1 || yce < ycs || yce > y_ncol) error("y col bounds out of range");

    int nr = re - rs + 1;        /* rows participating in product */
    int kx = xce - xcs + 1;      /* x columns */
    int ky = yce - ycs + 1;      /* y columns */

    /* BLAS expects column-major with leading dimension = full nrow */
    int lda = x_nrow;
    int ldb = y_nrow;

    /* Offset pointers to the requested (row_start, col_start) */
    double *A = xp + (R_xlen_t)(xcs - 1) * (R_xlen_t)x_nrow + (R_xlen_t)(rs - 1);
    double *B = yp + (R_xlen_t)(ycs - 1) * (R_xlen_t)y_nrow + (R_xlen_t)(rs - 1);

    /* Result: t(A) %*% B => (kx x ky) */
    SEXP out = PROTECT(allocMatrix(REALSXP, kx, ky));
    double *C = REAL(out);

    const char transa = 'T';
    const char transb = 'N';
    const double alpha = 1.0;
    const double beta = 0.0;
    int m = kx;
    int n = ky;
    int k = nr;
    int ldc = kx;

    F77_CALL(dgemm)(&transa, &transb, &m, &n, &k,
                    &alpha, A, &lda, B, &ldb,
                    &beta, C, &ldc FCONE FCONE);

    UNPROTECT(1);
    return out;
}
