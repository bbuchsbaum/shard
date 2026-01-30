/*
 * shard_taskq.c - Shared-memory task queue implementation
 */

#include "shard_taskq.h"
#include "shard_r.h"

#include <string.h>

#if !defined(_WIN32)
#include <time.h>
#endif

#if __STDC_VERSION__ >= 201112L && !defined(__STDC_NO_ATOMICS__)
#include <stdatomic.h>
#define SHARD_HAVE_ATOMICS 1
#else
#define SHARD_HAVE_ATOMICS 0
#endif

/* The queue is stored inside a shard_segment_t's mapped address. */

#if SHARD_HAVE_ATOMICS
typedef struct {
    atomic_int state;
    atomic_int retry_count;
    atomic_int claimed_by;
} shard_taskq_task_t;

typedef struct {
    atomic_int n_tasks;
    atomic_int done_count;
    atomic_int failed_count;
    /* tasks follow */
    shard_taskq_task_t tasks[1];
} shard_taskq_header_t;
#else
typedef struct {
    int state;
    int retry_count;
    int claimed_by;
} shard_taskq_task_t;

typedef struct {
    int n_tasks;
    int done_count;
    int failed_count;
    shard_taskq_task_t tasks[1];
} shard_taskq_header_t;
#endif

static shard_segment_t *seg_from_xptr(SEXP seg_ptr) {
    if (TYPEOF(seg_ptr) != EXTPTRSXP) {
        error("seg_ptr must be an external pointer");
    }
    shard_segment_t *seg = (shard_segment_t *)R_ExternalPtrAddr(seg_ptr);
    if (!seg) error("segment pointer is NULL");
    return seg;
}

static shard_taskq_header_t *hdr_from_seg(shard_segment_t *seg) {
    void *addr = shard_segment_addr(seg);
    if (!addr) error("segment address is NULL");
    return (shard_taskq_header_t *)addr;
}

static size_t taskq_required_size(int n_tasks) {
    if (n_tasks < 1) return 0;
    return sizeof(shard_taskq_header_t) + (size_t)(n_tasks - 1) * sizeof(shard_taskq_task_t);
}

SEXP C_shard_taskq_supported(void) {
#if SHARD_HAVE_ATOMICS
    return ScalarLogical(1);
#else
    return ScalarLogical(0);
#endif
}

SEXP C_shard_taskq_init(SEXP seg_ptr, SEXP n_tasks_) {
    shard_segment_t *seg = seg_from_xptr(seg_ptr);
    int n_tasks = asInteger(n_tasks_);
    if (n_tasks == NA_INTEGER || n_tasks < 1) {
        error("n_tasks must be a positive integer");
    }

    size_t need = taskq_required_size(n_tasks);
    size_t have = shard_segment_size(seg);
    if (have < need) {
        error("segment too small for task queue (need %zu bytes, have %zu)", need, have);
    }

    shard_taskq_header_t *hdr = hdr_from_seg(seg);
    /* Zero the entire region used by the queue */
    memset((void *)hdr, 0, need);

#if SHARD_HAVE_ATOMICS
    atomic_store(&hdr->n_tasks, n_tasks);
    atomic_store(&hdr->done_count, 0);
    atomic_store(&hdr->failed_count, 0);
    for (int i = 0; i < n_tasks; i++) {
        atomic_store(&hdr->tasks[i].state, SHARD_TASKQ_PENDING);
        atomic_store(&hdr->tasks[i].retry_count, 0);
        atomic_store(&hdr->tasks[i].claimed_by, 0);
    }
#else
    hdr->n_tasks = n_tasks;
    hdr->done_count = 0;
    hdr->failed_count = 0;
    for (int i = 0; i < n_tasks; i++) {
        hdr->tasks[i].state = SHARD_TASKQ_PENDING;
        hdr->tasks[i].retry_count = 0;
        hdr->tasks[i].claimed_by = 0;
    }
#endif

    return ScalarLogical(1);
}

SEXP C_shard_taskq_claim(SEXP seg_ptr, SEXP worker_id_) {
    shard_segment_t *seg = seg_from_xptr(seg_ptr);
    int worker_id = asInteger(worker_id_);
    if (worker_id == NA_INTEGER || worker_id < 1) error("worker_id must be >= 1");

    shard_taskq_header_t *hdr = hdr_from_seg(seg);

#if SHARD_HAVE_ATOMICS
    int n = atomic_load(&hdr->n_tasks);
    for (int i = 0; i < n; i++) {
        int expected = SHARD_TASKQ_PENDING;
        if (atomic_compare_exchange_strong(&hdr->tasks[i].state, &expected, SHARD_TASKQ_CLAIMED)) {
            atomic_store(&hdr->tasks[i].claimed_by, worker_id);
            return ScalarInteger(i + 1);
        }
    }
    return ScalarInteger(0);
#else
    /* Fallback: no atomics => no true shm_queue mode. */
    return ScalarInteger(0);
#endif
}

SEXP C_shard_taskq_mark_done(SEXP seg_ptr, SEXP task_id_) {
    shard_segment_t *seg = seg_from_xptr(seg_ptr);
    int task_id = asInteger(task_id_);
    if (task_id == NA_INTEGER || task_id < 1) error("task_id must be >= 1");

    shard_taskq_header_t *hdr = hdr_from_seg(seg);

#if SHARD_HAVE_ATOMICS
    int n = atomic_load(&hdr->n_tasks);
    if (task_id > n) error("task_id out of range");
    int idx = task_id - 1;
    atomic_store(&hdr->tasks[idx].state, SHARD_TASKQ_DONE);
    atomic_store(&hdr->tasks[idx].claimed_by, 0);
    atomic_fetch_add(&hdr->done_count, 1);
    return ScalarLogical(1);
#else
    return ScalarLogical(0);
#endif
}

SEXP C_shard_taskq_mark_error(SEXP seg_ptr, SEXP task_id_, SEXP max_retries_) {
    shard_segment_t *seg = seg_from_xptr(seg_ptr);
    int task_id = asInteger(task_id_);
    int max_retries = asInteger(max_retries_);
    if (task_id == NA_INTEGER || task_id < 1) error("task_id must be >= 1");
    if (max_retries == NA_INTEGER || max_retries < 0) error("max_retries must be >= 0");

    shard_taskq_header_t *hdr = hdr_from_seg(seg);

#if SHARD_HAVE_ATOMICS
    int n = atomic_load(&hdr->n_tasks);
    if (task_id > n) error("task_id out of range");
    int idx = task_id - 1;

    int rc = atomic_fetch_add(&hdr->tasks[idx].retry_count, 1) + 1;
    atomic_store(&hdr->tasks[idx].claimed_by, 0);
    if (rc > max_retries) {
        atomic_store(&hdr->tasks[idx].state, SHARD_TASKQ_FAILED);
        atomic_fetch_add(&hdr->failed_count, 1);
        return ScalarLogical(0);
    } else {
        atomic_store(&hdr->tasks[idx].state, SHARD_TASKQ_PENDING);
        return ScalarLogical(1);
    }
#else
    return ScalarLogical(0);
#endif
}

SEXP C_shard_taskq_reset_claims(SEXP seg_ptr, SEXP worker_id_) {
    shard_segment_t *seg = seg_from_xptr(seg_ptr);
    int worker_id = asInteger(worker_id_);
    if (worker_id == NA_INTEGER || worker_id < 1) error("worker_id must be >= 1");

    shard_taskq_header_t *hdr = hdr_from_seg(seg);

#if SHARD_HAVE_ATOMICS
    int n = atomic_load(&hdr->n_tasks);
    int reset = 0;
    for (int i = 0; i < n; i++) {
        int st = atomic_load(&hdr->tasks[i].state);
        if (st == SHARD_TASKQ_CLAIMED && atomic_load(&hdr->tasks[i].claimed_by) == worker_id) {
            atomic_store(&hdr->tasks[i].claimed_by, 0);
            atomic_store(&hdr->tasks[i].state, SHARD_TASKQ_PENDING);
            reset++;
        }
    }
    return ScalarInteger(reset);
#else
    return ScalarInteger(0);
#endif
}

SEXP C_shard_taskq_stats(SEXP seg_ptr) {
    shard_segment_t *seg = seg_from_xptr(seg_ptr);
    shard_taskq_header_t *hdr = hdr_from_seg(seg);

#if SHARD_HAVE_ATOMICS
    int n = atomic_load(&hdr->n_tasks);
    int done = atomic_load(&hdr->done_count);
    int failed = atomic_load(&hdr->failed_count);

    SEXP out = PROTECT(allocVector(VECSXP, 3));
    SEXP names = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, mkChar("n_tasks"));
    SET_STRING_ELT(names, 1, mkChar("done"));
    SET_STRING_ELT(names, 2, mkChar("failed"));
    SET_VECTOR_ELT(out, 0, ScalarInteger(n));
    SET_VECTOR_ELT(out, 1, ScalarInteger(done));
    SET_VECTOR_ELT(out, 2, ScalarInteger(failed));
    setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(2);
    return out;
#else
    SEXP out = PROTECT(allocVector(VECSXP, 1));
    SEXP names = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(names, 0, mkChar("n_tasks"));
    SET_VECTOR_ELT(out, 0, ScalarInteger(0));
    setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(2);
    return out;
#endif
}

SEXP C_shard_taskq_failures(SEXP seg_ptr) {
    shard_segment_t *seg = seg_from_xptr(seg_ptr);
    shard_taskq_header_t *hdr = hdr_from_seg(seg);

#if SHARD_HAVE_ATOMICS
    int n = atomic_load(&hdr->n_tasks);
    /* Count failures */
    int nf = 0;
    for (int i = 0; i < n; i++) {
        if (atomic_load(&hdr->tasks[i].state) == SHARD_TASKQ_FAILED) nf++;
    }
    SEXP out = PROTECT(allocVector(INTSXP, nf));
    int j = 0;
    for (int i = 0; i < n; i++) {
        if (atomic_load(&hdr->tasks[i].state) == SHARD_TASKQ_FAILED) {
            INTEGER(out)[j++] = i + 1;
        }
    }
    UNPROTECT(1);
    return out;
#else
    return allocVector(INTSXP, 0);
#endif
}
