# Shared-memory task queue (opt-in).
#
# This is a "fast dispatch" mode intended for tiny tasks where per-task socket
# overhead dominates. It is conservative and intentionally limited: tasks are
# identified by integer ids and results are not gathered.

taskq_create <- function(n_tasks,
                         backing = c("auto", "mmap", "shm")) {
  backing <- match.arg(backing)
  n_tasks <- as.integer(n_tasks)
  if (is.na(n_tasks) || n_tasks < 1L) stop("n_tasks must be >= 1", call. = FALSE)

  # Header + per-task records; size computed in C as well.
  # Allocate with a small safety margin.
  # Over-allocate: C validates and will error if too small.
  bytes_per_task <- 64L
  header_bytes <- 128L
  size <- as.double(header_bytes + as.double(n_tasks) * bytes_per_task)

  seg <- segment_create(size, backing = backing)
  .Call("C_shard_taskq_init", seg$ptr, n_tasks, PACKAGE = "shard")

  desc <- list(
    path = segment_path(seg),
    backing = seg$backing,
    size = segment_size(seg),
    n_tasks = n_tasks
  )
  # Keep the owning segment alive in the creating process; do not attempt to
  # serialize the xptr to workers.
  list(desc = desc, owner = seg)
}

taskq_open <- function(desc, readonly = FALSE) {
  if (!is.list(desc) || is.null(desc$path) || is.null(desc$backing)) {
    stop("desc must be a task queue descriptor", call. = FALSE)
  }
  segment_open(desc$path, backing = desc$backing, readonly = readonly)
}

taskq_stats <- function(seg) {
  stopifnot(inherits(seg, "shard_segment"))
  .Call("C_shard_taskq_stats", seg$ptr, PACKAGE = "shard")
}

taskq_claim <- function(seg, worker_id) {
  stopifnot(inherits(seg, "shard_segment"))
  .Call("C_shard_taskq_claim", seg$ptr, as.integer(worker_id), PACKAGE = "shard")
}

taskq_done <- function(seg, task_id) {
  stopifnot(inherits(seg, "shard_segment"))
  .Call("C_shard_taskq_mark_done", seg$ptr, as.integer(task_id), PACKAGE = "shard")
}

taskq_error <- function(seg, task_id, max_retries) {
  stopifnot(inherits(seg, "shard_segment"))
  .Call("C_shard_taskq_mark_error", seg$ptr, as.integer(task_id), as.integer(max_retries), PACKAGE = "shard")
}

taskq_reset_claims <- function(seg, worker_id) {
  stopifnot(inherits(seg, "shard_segment"))
  .Call("C_shard_taskq_reset_claims", seg$ptr, as.integer(worker_id), PACKAGE = "shard")
}

taskq_failures <- function(seg) {
  stopifnot(inherits(seg, "shard_segment"))
  .Call("C_shard_taskq_failures", seg$ptr, PACKAGE = "shard")
}
