#' @keywords internal
"_PACKAGE"

#' shard: Deterministic, Zero-Copy Parallel Execution for R
#'
#' The shard package provides a parallel execution runtime that emphasizes
#' deterministic memory behavior and efficient handling of large shared inputs.
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
}

# On package unload, stop any running pool
.onUnload <- function(libpath) {
  tryCatch(pool_stop(), error = function(e) NULL)
}
