#!/usr/bin/env Rscript
# Dispatch-overhead benchmark: many tiny chunks, trivial kernel.
#
# Measures:
#   1. wall-clock for a dispatch-bound shard_map() run (N chunks of 1 shard), and
#   2. the actual per-send payload shipped by parallel's sendCall
#      (wrapper function + chunk argument), captured via trace().
#
# Usage:
#   Rscript inst/bench/dispatch-overhead.R [n_shards] [workers]
#
# Run from the package root (uses pkgload if available, otherwise the
# installed package).

args <- commandArgs(trailingOnly = TRUE)
n_shards <- if (length(args) >= 1) as.integer(args[[1]]) else 2000L
workers <- if (length(args) >= 2) as.integer(args[[2]]) else 2L

if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  library(shard)
}

kern <- function(sh) sh$id

# --- Pass 1: wall clock (no tracing overhead) --------------------------------
blocks <- shards(n_shards, block_size = 1L)
t0 <- proc.time()[["elapsed"]]
res <- shard_map(blocks, kern, workers = workers, chunk_size = 1L,
                 health_check_interval = 1000L)
t1 <- proc.time()[["elapsed"]]
pool_stop()
stopifnot(length(results(res)) == n_shards)

# --- Pass 2: per-send payload bytes (trace sendCall on a smaller run) --------
n_probe <- min(n_shards, 200L)
payload_env <- new.env(parent = emptyenv())
payload_env$bytes <- numeric(0)
suppressMessages(trace(
  what = "sendCall", where = asNamespace("parallel"),
  tracer = bquote({
    .e <- .(payload_env)
    .e$bytes <- c(.e$bytes, length(serialize(list(fun = fun, args = args), NULL)))
  }),
  print = FALSE
))
blocks_probe <- shards(n_probe, block_size = 1L)
res2 <- shard_map(blocks_probe, kern, workers = workers, chunk_size = 1L,
                  health_check_interval = 1000L)
suppressMessages(untrace("sendCall", where = asNamespace("parallel")))
pool_stop()
stopifnot(length(results(res2)) == n_probe)

cat(sprintf("chunks: %d  workers: %d\n", n_shards, workers))
cat(sprintf("wall_clock_sec: %.2f  (%.2f ms/chunk)\n",
            t1 - t0, 1000 * (t1 - t0) / n_shards))
b <- payload_env$bytes
cat(sprintf("per_send_payload_bytes: mean=%.0f  median=%.0f  min=%.0f  max=%.0f  (n=%d)\n",
            mean(b), stats::median(b), min(b), max(b), length(b)))
