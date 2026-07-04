# Benchmark: task-queue claim throughput (Phase 3, item 3.1)
#
# Measures:
#   1. Single-process claim throughput: claim all of N tasks sequentially.
#      Before Phase 3 this was O(N^2) total (each claim CAS-scanned the
#      whole array from slot 0); after, the atomic head cursor makes it
#      O(N) total (O(1) amortized per claim).
#   2. Batched claim throughput via taskq_claim_range (post-Phase 3 only;
#      skipped automatically when the entry point is absent so the same
#      script runs at older commits for before/after comparison).
#   3. End-to-end shm_queue dispatch: N tiny tasks, min(8, cores) workers,
#      kernel writes one int per task to an out buffer.
#
# Run from the package root:
#   Rscript inst/bench/taskq-claims.R [N]
# Default N = 1e5.

args <- commandArgs(trailingOnly = TRUE)
N <- if (length(args) >= 1) as.integer(args[[1]]) else 100000L

if (requireNamespace("devtools", quietly = TRUE) && file.exists("DESCRIPTION")) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(shard)
}

if (!shard:::taskq_supported()) {
  stop("taskq not supported on this platform (no C11 atomics)")
}

fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)

cat(sprintf("taskq claim benchmark: N = %s tasks\n", fmt(N)))
cat(sprintf("R %s | %s\n\n", getRversion(), Sys.info()[["sysname"]]))

## 1. Single-process sequential claims -------------------------------------

q <- shard:::taskq_create(N, backing = "mmap")
seg <- shard:::taskq_open(q$desc, readonly = FALSE)

t1 <- system.time({
  for (i in seq_len(N)) {
    id <- shard:::taskq_claim(seg, worker_id = 1L)
    if (id < 1L) stop("claim returned 0 before queue drained at i=", i)
  }
})[["elapsed"]]

cat(sprintf(
  "single claim loop:   %8.3f s  -> %s claims/sec\n",
  t1, fmt(N / t1)
))

shard:::segment_close(seg, unlink = FALSE)
shard:::segment_close(q$owner, unlink = TRUE)

## 2. Batched claims (claim_range), if available ----------------------------

has_range <- is.function(tryCatch(get("taskq_claim_range", envir = asNamespace("shard")),
                                  error = function(e) NULL))
if (has_range) {
  for (k in c(4L, 8L)) {
    q <- shard:::taskq_create(N, backing = "mmap")
    seg <- shard:::taskq_open(q$desc, readonly = FALSE)
    got <- 0L
    t2 <- system.time({
      repeat {
        ids <- shard:::taskq_claim_range(seg, worker_id = 1L, max_tasks = k)
        if (length(ids) == 0L) break
        got <- got + length(ids)
      }
    })[["elapsed"]]
    stopifnot(got == N)
    cat(sprintf(
      "batched claims k=%d:  %8.3f s  -> %s claims/sec\n",
      k, t2, fmt(N / t2)
    ))
    shard:::segment_close(seg, unlink = FALSE)
    shard:::segment_close(q$owner, unlink = TRUE)
  }
} else {
  cat("batched claims:      (taskq_claim_range not available at this commit)\n")
}

## 3. End-to-end shm_queue dispatch -----------------------------------------

workers <- min(8L, parallel::detectCores())
cat(sprintf("\nend-to-end shm_queue dispatch: %s tasks, %d workers\n", fmt(N), workers))

pool_stop()
pool_create(n = workers)

out <- buffer("integer", dim = N, init = 0L, backing = "mmap")

t3 <- system.time({
  res <- shard_map(
    N,
    out = list(out = out),
    fun = function(sh, out) {
      out[sh$idx] <- sh$idx
      NULL
    },
    workers = workers,
    chunk_size = 1,
    dispatch_mode = "shm_queue",
    dispatch_opts = list(block_size = 1L),
    timeout = 3600
  )
})[["elapsed"]]

stopifnot(succeeded(res))
stopifnot(identical(as.integer(out[]), seq_len(N)))

cat(sprintf(
  "shm_queue dispatch:  %8.3f s  -> %s tasks/sec\n",
  t3, fmt(N / t3)
))

buffer_close(out)
pool_stop()
