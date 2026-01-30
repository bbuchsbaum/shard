/*
 * shard_taskq.h - Shared-memory task queue for ultra-low overhead dispatch
 *
 * This is an opt-in mechanism used to reduce per-task RPC overhead for tiny
 * tasks. The queue is stored in a shard_segment_t and uses atomics for
 * cross-process synchronization.
 *
 * Semantics (v1):
 * - tasks are identified by integer ids in [1..n_tasks]
 * - only supports "fire-and-forget" tasks (results are not gathered)
 * - supports retries via per-task retry_count and a max_retries limit
 */

#ifndef SHARD_TASKQ_H
#define SHARD_TASKQ_H

#include "shard_shm.h"
#include <stdint.h>

/* Task states */
typedef enum {
    SHARD_TASKQ_EMPTY   = 0,
    SHARD_TASKQ_PENDING = 1,
    SHARD_TASKQ_CLAIMED = 2,
    SHARD_TASKQ_DONE    = 3,
    SHARD_TASKQ_FAILED  = 4
} shard_taskq_state_t;

/* R-callable API (registered in init.c) */
attribute_visible SEXP C_shard_taskq_init(SEXP seg_ptr, SEXP n_tasks);
attribute_visible SEXP C_shard_taskq_supported(void);
attribute_visible SEXP C_shard_taskq_claim(SEXP seg_ptr, SEXP worker_id);
attribute_visible SEXP C_shard_taskq_mark_done(SEXP seg_ptr, SEXP task_id);
attribute_visible SEXP C_shard_taskq_mark_error(SEXP seg_ptr, SEXP task_id, SEXP max_retries);
attribute_visible SEXP C_shard_taskq_reset_claims(SEXP seg_ptr, SEXP worker_id);
attribute_visible SEXP C_shard_taskq_stats(SEXP seg_ptr);
attribute_visible SEXP C_shard_taskq_failures(SEXP seg_ptr);

#endif /* SHARD_TASKQ_H */
