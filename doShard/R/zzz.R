.onUnload <- function(libpath) {
  # Clean up worker pool when package is unloaded
  if (.doShard_env$registered) {
    tryCatch(
      shard::pool_stop(),
      error = function(e) NULL
    )
  }
}
