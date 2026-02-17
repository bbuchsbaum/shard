# Package load/unload hooks
# Currently empty - using string-based .Call with PACKAGE = "shard"

# Package-level environment for worker-side caching.
# Used instead of globalenv() to avoid R CMD check NOTEs.
.shard_worker_env <- new.env(parent = emptyenv())
