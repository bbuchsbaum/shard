/*
 * init.c - R package initialization for shard
 *
 * Registers C routines with R and handles package load/unload.
 */

#include "shard_shm.h"
#include "shard_altrep.h"
#include "shard_utils.h"
#include "shard_taskq.h"
#include "shard_affinity.h"

/* Callable methods from R */
static const R_CallMethodDef CallEntries[] = {
    /* Segment functions */
    {"C_shard_segment_create",    (DL_FUNC) &C_shard_segment_create,    4},
    {"C_shard_segment_open",      (DL_FUNC) &C_shard_segment_open,      3},
    {"C_shard_segment_close",     (DL_FUNC) &C_shard_segment_close,     2},
    {"C_shard_segment_addr",      (DL_FUNC) &C_shard_segment_addr,      1},
    {"C_shard_segment_size",      (DL_FUNC) &C_shard_segment_size,      1},
    {"C_shard_segment_path",      (DL_FUNC) &C_shard_segment_path,      1},
    {"C_shard_segment_write_raw", (DL_FUNC) &C_shard_segment_write_raw, 3},
    {"C_shard_segment_read_raw",  (DL_FUNC) &C_shard_segment_read_raw,  3},
    {"C_shard_segment_protect",   (DL_FUNC) &C_shard_segment_protect,   1},
    {"C_shard_segment_info",      (DL_FUNC) &C_shard_segment_info,      1},
    {"C_shard_is_windows",        (DL_FUNC) &C_shard_is_windows,        0},
    {"C_shard_available_backings",(DL_FUNC) &C_shard_available_backings,0},
    /* ALTREP functions */
    {"C_shard_altrep_create",           (DL_FUNC) &C_shard_altrep_create,           6},
    {"C_shard_altrep_view",              (DL_FUNC) &C_shard_altrep_view,              3},
    {"C_shard_altrep_diagnostics",       (DL_FUNC) &C_shard_altrep_diagnostics,       1},
    {"C_is_shard_altrep",                (DL_FUNC) &C_is_shard_altrep,                1},
    {"C_shard_altrep_segment",           (DL_FUNC) &C_shard_altrep_segment,           1},
    {"C_shard_altrep_reset_diagnostics", (DL_FUNC) &C_shard_altrep_reset_diagnostics, 1},
    {"C_shard_altrep_materialize",       (DL_FUNC) &C_shard_altrep_materialize,       1},
    {"C_shard_mat_block_col_sums",       (DL_FUNC) &C_shard_mat_block_col_sums,       5},
    {"C_shard_mat_crossprod_block",      (DL_FUNC) &C_shard_mat_crossprod_block,      8},
    {"C_shard_mat_crossprod_gather",     (DL_FUNC) &C_shard_mat_crossprod_gather,     7},
    {"C_shard_mat_crossprod_gather_scratch",(DL_FUNC) &C_shard_mat_crossprod_gather_scratch, 8},
    {"C_shard_obj_address",              (DL_FUNC) &C_shard_obj_address,              1},
    /* Task queue (shm_queue) */
    {"C_shard_taskq_init",               (DL_FUNC) &C_shard_taskq_init,               2},
    {"C_shard_taskq_supported",          (DL_FUNC) &C_shard_taskq_supported,          0},
    {"C_shard_taskq_claim",              (DL_FUNC) &C_shard_taskq_claim,              2},
    {"C_shard_taskq_mark_done",          (DL_FUNC) &C_shard_taskq_mark_done,          2},
    {"C_shard_taskq_mark_error",         (DL_FUNC) &C_shard_taskq_mark_error,         3},
    {"C_shard_taskq_reset_claims",       (DL_FUNC) &C_shard_taskq_reset_claims,       2},
    {"C_shard_taskq_stats",              (DL_FUNC) &C_shard_taskq_stats,              1},
    {"C_shard_taskq_failures",           (DL_FUNC) &C_shard_taskq_failures,           1},
    /* Affinity + madvise (best-effort) */
    {"C_shard_affinity_supported",       (DL_FUNC) &C_shard_affinity_supported,       0},
    {"C_shard_set_affinity",             (DL_FUNC) &C_shard_set_affinity,             1},
    {"C_shard_segment_madvise",          (DL_FUNC) &C_shard_segment_madvise,          2},
    {NULL, NULL, 0}
};

/* Package initialization */
attribute_visible void R_init_shard(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    /* Note: Not using R_forceSymbols to allow string-based .Call */

    /* Initialize shared memory subsystem */
    shard_shm_init();

    /* Initialize ALTREP classes */
    shard_altrep_init(dll);
}

/* Package cleanup (called on unload) */
void R_unload_shard(DllInfo *dll) {
    shard_shm_cleanup();
}
