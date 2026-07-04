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

/*
 * Header layout note: this struct is private to this translation unit (the
 * queue lives in shared memory, but master and workers always run the same
 * compiled shard.so, and a queue never outlives a single dispatch), so its
 * layout may change between package versions. The header is padded to 64
 * bytes so the hot counters (claim_cursor in particular) do not share a
 * cache line with task slots.
 *
 * claim_cursor is the O(1)-amortized claim fast path: each claimer does a
 * fetch_add to obtain a unique candidate slot and CASes it PENDING->CLAIMED.
 * Slots that are requeued *behind* the cursor (retry via mark_error, or
 * reset_claims after a worker death) are picked up by a fallback linear
 * sweep that runs only once the cursor has reached the end of the array.
 */
#define SHARD_TASKQ_HEADER_BYTES 64

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
    atomic_int retry_total;
    atomic_int claim_cursor; /* next candidate slot for the claim fast path */
    char pad_[SHARD_TASKQ_HEADER_BYTES - 5 * sizeof(atomic_int)];
    /* C99 flexible array member -- avoids UBSAN out-of-bounds on tasks[i] */
    shard_taskq_task_t tasks[];
} shard_taskq_header_t;

_Static_assert(sizeof(shard_taskq_header_t) == SHARD_TASKQ_HEADER_BYTES,
               "taskq header must be exactly 64 bytes");
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
    int retry_total;
    int claim_cursor; /* mirrors the atomic layout; see fallback note below */
    char pad_[SHARD_TASKQ_HEADER_BYTES - 5 * sizeof(int)];
    shard_taskq_task_t tasks[]; /* C99 flexible array member */
} shard_taskq_header_t;
/*
 * Fallback note (no C11 atomics): taskq_supported() returns FALSE, the R
 * layer never selects shm_queue dispatch (see R/shard_map.R), and claim/
 * mark_done/mark_error below are inert stubs. The struct mirrors the atomic
 * layout only so sizeof arithmetic and init/stats stay consistent; there is
 * no cross-process synchronization in this branch by design.
 */
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
    return sizeof(shard_taskq_header_t) + (size_t)n_tasks * sizeof(shard_taskq_task_t);
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
    atomic_store(&hdr->retry_total, 0);
    atomic_store(&hdr->claim_cursor, 0);
    for (int i = 0; i < n_tasks; i++) {
        atomic_store(&hdr->tasks[i].state, SHARD_TASKQ_PENDING);
        atomic_store(&hdr->tasks[i].retry_count, 0);
        atomic_store(&hdr->tasks[i].claimed_by, 0);
    }
#else
    hdr->n_tasks = n_tasks;
    hdr->done_count = 0;
    hdr->failed_count = 0;
    hdr->retry_total = 0;
    hdr->claim_cursor = 0;
    for (int i = 0; i < n_tasks; i++) {
        hdr->tasks[i].state = SHARD_TASKQ_PENDING;
        hdr->tasks[i].retry_count = 0;
        hdr->tasks[i].claimed_by = 0;
    }
#endif

    return ScalarLogical(1);
}

#if SHARD_HAVE_ATOMICS
/* Try to claim slot i (0-based). Returns 1 on success. */
static int taskq_try_claim_slot(shard_taskq_header_t *hdr, int i, int worker_id) {
    int expected = SHARD_TASKQ_PENDING;
    if (atomic_compare_exchange_strong(&hdr->tasks[i].state, &expected, SHARD_TASKQ_CLAIMED)) {
        atomic_store(&hdr->tasks[i].claimed_by, worker_id);
        return 1;
    }
    return 0;
}

/*
 * Claim one task. Returns the 1-based task id, or 0 if no task is claimable
 * right now (all done/failed/claimed-by-others). The state CAS is the sole
 * claim gate, so a task can never be double-claimed; the cursor is only an
 * iteration hint.
 *
 * Fast path: fetch_add on claim_cursor hands each caller a unique candidate
 * slot; non-PENDING candidates (done/failed/claimed) are skipped by
 * advancing again, so claims are O(1) amortized over the run.
 *
 * Fallback sweep: tasks requeued behind the cursor (mark_error retry, or
 * reset_claims after a worker death) are found by a linear sweep that runs
 * only once the cursor has reached the end AND unfinished tasks remain.
 */
static int taskq_claim_one(shard_taskq_header_t *hdr, int worker_id) {
    int n = atomic_load(&hdr->n_tasks);

    for (;;) {
        int i = atomic_fetch_add(&hdr->claim_cursor, 1);
        if (i >= n) {
            /* Clamp the cursor back to n so repeated polling cannot creep
             * it toward INT_MAX. CAS failure (someone else advanced or
             * already clamped) is harmless. */
            int cur = i + 1;
            if (cur > n) {
                atomic_compare_exchange_strong(&hdr->claim_cursor, &cur, n);
            }
            break;
        }
        if (taskq_try_claim_slot(hdr, i, worker_id)) return i + 1;
    }

    /* Cursor exhausted. Sweep only if some tasks are neither done nor
     * failed: those may be PENDING again behind the cursor (retries), or
     * still running on other workers (sweep finds nothing; caller polls). */
    int done = atomic_load(&hdr->done_count);
    int failed = atomic_load(&hdr->failed_count);
    if (done + failed >= n) return 0;

    for (int i = 0; i < n; i++) {
        if (taskq_try_claim_slot(hdr, i, worker_id)) return i + 1;
    }
    return 0;
}
#endif /* SHARD_HAVE_ATOMICS */

SEXP C_shard_taskq_claim(SEXP seg_ptr, SEXP worker_id_) {
    shard_segment_t *seg = seg_from_xptr(seg_ptr);
    int worker_id = asInteger(worker_id_);
    if (worker_id == NA_INTEGER || worker_id < 1) error("worker_id must be >= 1");

    shard_taskq_header_t *hdr = hdr_from_seg(seg);

#if SHARD_HAVE_ATOMICS
    return ScalarInteger(taskq_claim_one(hdr, worker_id));
#else
    /* Fallback: no atomics => no true shm_queue mode (taskq_supported()
     * returns FALSE and the R layer never dispatches via shm_queue). */
    (void)hdr;
    return ScalarInteger(0);
#endif
}

/*
 * Batched claim: claim up to max_tasks tasks in one call and return their
 * 1-based ids as an integer vector. Length 0 means nothing is claimable
 * right now (same convention as a 0 return from C_shard_taskq_claim; the
 * caller should consult stats to distinguish "drained" from "in flight").
 */
SEXP C_shard_taskq_claim_range(SEXP seg_ptr, SEXP worker_id_, SEXP max_tasks_) {
    shard_segment_t *seg = seg_from_xptr(seg_ptr);
    int worker_id = asInteger(worker_id_);
    int k = asInteger(max_tasks_);
    if (worker_id == NA_INTEGER || worker_id < 1) error("worker_id must be >= 1");
    if (k == NA_INTEGER || k < 1) error("max_tasks must be >= 1");

    shard_taskq_header_t *hdr = hdr_from_seg(seg);

#if SHARD_HAVE_ATOMICS
    int n = atomic_load(&hdr->n_tasks);
    if (k > n) k = n;

    SEXP ids = PROTECT(allocVector(INTSXP, k));
    int m = 0;
    while (m < k) {
        int id = taskq_claim_one(hdr, worker_id);
        if (id == 0) break;
        INTEGER(ids)[m++] = id;
    }
    if (m < k) {
        SEXP trimmed = PROTECT(allocVector(INTSXP, m));
        if (m > 0) memcpy(INTEGER(trimmed), INTEGER(ids), (size_t)m * sizeof(int));
        UNPROTECT(2);
        return trimmed;
    }
    UNPROTECT(1);
    return ids;
#else
    (void)hdr;
    return allocVector(INTSXP, 0);
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
    (void)hdr;
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
        /* Count scheduled retries (not total error events). */
        atomic_fetch_add(&hdr->retry_total, 1);
        return ScalarLogical(1);
    }
#else
    (void)hdr;
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
    (void)hdr;
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
    int retries = atomic_load(&hdr->retry_total);

    SEXP out = PROTECT(allocVector(VECSXP, 4));
    SEXP names = PROTECT(allocVector(STRSXP, 4));
    SET_STRING_ELT(names, 0, mkChar("n_tasks"));
    SET_STRING_ELT(names, 1, mkChar("done"));
    SET_STRING_ELT(names, 2, mkChar("failed"));
    SET_STRING_ELT(names, 3, mkChar("retries"));
    SET_VECTOR_ELT(out, 0, ScalarInteger(n));
    SET_VECTOR_ELT(out, 1, ScalarInteger(done));
    SET_VECTOR_ELT(out, 2, ScalarInteger(failed));
    SET_VECTOR_ELT(out, 3, ScalarInteger(retries));
    setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(2);
    return out;
#else
    (void)hdr;
    SEXP out = PROTECT(allocVector(VECSXP, 4));
    SEXP names = PROTECT(allocVector(STRSXP, 4));
    SET_STRING_ELT(names, 0, mkChar("n_tasks"));
    SET_STRING_ELT(names, 1, mkChar("done"));
    SET_STRING_ELT(names, 2, mkChar("failed"));
    SET_STRING_ELT(names, 3, mkChar("retries"));
    SET_VECTOR_ELT(out, 0, ScalarInteger(0));
    SET_VECTOR_ELT(out, 1, ScalarInteger(0));
    SET_VECTOR_ELT(out, 2, ScalarInteger(0));
    SET_VECTOR_ELT(out, 3, ScalarInteger(0));
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
    SEXP ids = PROTECT(allocVector(INTSXP, nf));
    SEXP rcs = PROTECT(allocVector(INTSXP, nf));
    int j = 0;
    for (int i = 0; i < n; i++) {
        if (atomic_load(&hdr->tasks[i].state) == SHARD_TASKQ_FAILED) {
            INTEGER(ids)[j] = i + 1;
            INTEGER(rcs)[j] = atomic_load(&hdr->tasks[i].retry_count);
            j++;
        }
    }
    SEXP out = PROTECT(allocVector(VECSXP, 2));
    SEXP names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("task_id"));
    SET_STRING_ELT(names, 1, mkChar("retry_count"));
    SET_VECTOR_ELT(out, 0, ids);
    SET_VECTOR_ELT(out, 1, rcs);
    setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(4);
    return out;
#else
    (void)hdr;
    SEXP ids = PROTECT(allocVector(INTSXP, 0));
    SEXP rcs = PROTECT(allocVector(INTSXP, 0));
    SEXP out = PROTECT(allocVector(VECSXP, 2));
    SEXP names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("task_id"));
    SET_STRING_ELT(names, 1, mkChar("retry_count"));
    SET_VECTOR_ELT(out, 0, ids);
    SET_VECTOR_ELT(out, 1, rcs);
    setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(4);
    return out;
#endif
}
