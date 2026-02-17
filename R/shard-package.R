#' @keywords internal
"_PACKAGE"

#' shard: Deterministic, Zero-Copy Parallel Execution for R
#'
#' The shard package provides a parallel execution runtime that emphasizes
#' deterministic memory behavior and efficient handling of large shared inputs.
#'
#' @section Core API:
#' * [shard_map()] - Primary parallel execution entry point
#' * [shards()] - Create shard descriptors with autotuning
#' * [results()] - Extract results from a shard_map run
#' * [succeeded()] - Check if shard_map completed without failures
#'
#' @section Zero-Copy Shared Data:
#' * [share()] - Share an R object for parallel access
#' * [fetch()] - Retrieve data from a shared object
#' * [materialize()] - Alias for fetch()
#' * [is_shared()] - Check if an object is shared
#' * [shared_info()] - Get information about a shared object
#'
#' @section Output Buffers:
#' * [buffer()] - Create typed writable output buffer
#' * [buffer_open()] - Open existing buffer from another process
#' * [buffer_path()] - Get buffer path for cross-process sharing
#' * [buffer_info()] - Get buffer information
#' * [buffer_close()] - Close and release buffer
#'
#' @section Worker Pool Management:
#' * [pool_create()] - Create a supervised worker pool
#' * [pool_stop()] - Stop the worker pool
#' * [pool_status()] - Check worker status and RSS
#' * [pool_health_check()] - Monitor and recycle workers
#'
#' @section Task Dispatch:
#' * [dispatch_chunks()] - Execute chunks with supervision
#' * [pool_lapply()] - Parallel lapply with supervision
#' * [pool_sapply()] - Parallel sapply with supervision
#'
#' @docType package
#' @name shard-package
NULL

# On package load, ensure clean state
.onLoad <- function(libname, pkgname) {
  # Initialize pool environment
  .pool_env$pool <- NULL
  .pool_env$dev_path <- NULL

  # Register a finalizer that runs at R exit (onexit = TRUE).

  # .onUnload only fires on explicit unloadNamespace(), NOT on normal R exit.
  # Without this, parallel's PSOCK cluster finalizers fire first during
  # shutdown GC and fail with "Error in unserialize(node$con)" because
  # the worker connections are already being torn down.
  reg.finalizer(.pool_env, function(e) {
    tryCatch(pool_stop(), error = function(e2) NULL)
  }, onexit = TRUE)
}

# On package unload, stop any running pool
.onUnload <- function(libpath) {
  tryCatch(pool_stop(), error = function(e) NULL)
}
