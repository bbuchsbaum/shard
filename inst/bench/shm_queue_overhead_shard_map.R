# Benchmark: shm_queue vs rpc_chunked overhead for tiny tasks
#
# This is a microbenchmark intended to exercise dispatch overhead, not compute.
# It uses a simple out-buffer write so results are not gathered.
#
# Run:
#   R -q -f inst/bench/shm_queue_overhead_shard_map.R

library(shard)

pool_stop()
on.exit(pool_stop(), add = TRUE)

workers <- 4L
n <- 20000L

tiny_kernel <- function(sh, out) {
  out[sh$idx] <- sh$idx
  NULL
}

run_rpc <- function() {
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")
  blocks <- shards(n, block_size = 1L, workers = workers)
  t <- system.time(
    shard_map(
      blocks,
      out = list(out = out),
      fun = tiny_kernel,
      workers = workers,
      chunk_size = 1L,
      autotune = "none",
      diagnostics = FALSE
    )
  )
  list(time = t[["elapsed"]], out = out)
}

run_shm <- function() {
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")
  t <- system.time(
    shard_map(
      n,
      out = list(out = out),
      fun = tiny_kernel,
      workers = workers,
      chunk_size = 1L,
      dispatch_mode = "shm_queue",
      dispatch_opts = list(block_size = 1L),
      diagnostics = FALSE
    )
  )
  list(time = t[["elapsed"]], out = out)
}

rpc <- run_rpc()
shm <- run_shm()

cat("n=", n, "workers=", workers, "\n")
cat("rpc_chunked elapsed:", rpc$time, "sec  (tasks/sec=", n / rpc$time, ")\n")
cat("shm_queue  elapsed:", shm$time, "sec  (tasks/sec=", n / shm$time, ")\n")

stopifnot(identical(as.integer(rpc$out[]), seq_len(n)))
stopifnot(identical(as.integer(shm$out[]), seq_len(n)))

