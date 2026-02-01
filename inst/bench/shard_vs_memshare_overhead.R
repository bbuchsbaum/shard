# =============================================================================
# Benchmark: shard vs memshare (overhead + shared-big-data access)
# =============================================================================
#
# Run: Rscript inst/bench/shard_vs_memshare_overhead.R
#
# Notes:
# - Uses fresh PSOCK workers per test for fair comparison.
# - shard writes results into explicit output buffers (no result gathering).
# - memshare gathers results into lists.
# =============================================================================

suppressPackageStartupMessages({
  library(shard)
  if (!requireNamespace("memshare", quietly = TRUE)) {
    stop("Install memshare: install.packages('memshare')")
  }
  library(memshare)
  library(parallel)
})

n_workers <- 4L
bench_id <- paste0("sv", paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""))

auto_chunk_size <- function(n, workers, target_chunks_per_worker = 2L, min = 1L, max = 256L) {
  n <- as.integer(n)
  workers <- as.integer(workers)
  target_chunks_per_worker <- as.integer(target_chunks_per_worker)

  if (is.na(n) || n <= 0L) return(min)
  if (is.na(workers) || workers <= 0L) return(min)
  if (is.na(target_chunks_per_worker) || target_chunks_per_worker <= 0L) return(min)

  chunk <- as.integer(ceiling(n / (workers * target_chunks_per_worker)))
  chunk <- max(min, min(max, chunk))
  chunk
}

run_test <- function(name, desc, mem_fn, shard_fn) {
  cat(sprintf("Test %s: %s\n", name, desc))
  ns <- paste0(bench_id, "_t", name)

  cl <- makeCluster(n_workers)
  pool_stop()
  pool_create(n_workers)
  invisible(shard_map(n_workers, function(sh) NULL, workers = n_workers, diagnostics = FALSE))

  t_mem <- tryCatch({
    system.time(mem_fn(cl, ns))[["elapsed"]]
  }, error = function(e) {
    cat("  memshare error:", conditionMessage(e), "\n")
    NA_real_
  })

  t_shard <- tryCatch({
    system.time(shard_fn())[["elapsed"]]
  }, error = function(e) {
    cat("  shard error:", conditionMessage(e), "\n")
    NA_real_
  })

  stopCluster(cl)
  pool_stop()

  if (!is.na(t_mem) && !is.na(t_shard)) {
    ratio <- t_mem / t_shard
    cat(sprintf("  memshare: %.3fs | shard: %.3fs | ratio: %.2fx\n\n", t_mem, t_shard, ratio))
  } else {
    ratio <- NA_real_
    cat("\n")
  }

  list(mem = t_mem, shard = t_shard, ratio = ratio)
}

cat("============================================================\n")
cat("Overhead Benchmark: shard vs memshare (", n_workers, "workers)\n", sep = "")
cat("============================================================\n\n")
cat("shard version: ", as.character(utils::packageVersion("shard")), "\n", sep = "")
cat("shard path:    ", find.package("shard"), "\n\n", sep = "")

results <- list()

# =============================================================================
# Test 1: Dispatch throughput (many tiny tasks)
# =============================================================================
set.seed(1)
n_tasks1 <- 20000L
idx1 <- seq_len(n_tasks1)
idx1_d <- as.double(idx1)

results$test1 <- run_test("1", sprintf("Dispatch throughput (%d tiny tasks)", n_tasks1),
  mem_fn = function(cl, ns) {
    tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    on.exit(tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL), add = TRUE)

    res <- memLapply(
      X = as.list(idx1_d),
      FUN = function(i) i,
      CLUSTER = cl,
      NAMESPACE = ns,
      MAX.CORES = n_workers
    )
    stopifnot(identical(unlist(res, use.names = FALSE), idx1))
  },
  shard_fn = function() {
    out <- buffer("int", dim = n_tasks1)
    shard_map(
      n_tasks1,
      borrow = list(idx = idx1),
      out = list(out = out),
      fun = function(sh, idx, out) {
        for (j in sh$idx) out[j] <- idx[j]
        NULL
      },
      workers = n_workers,
      chunk_size = auto_chunk_size(n_tasks1, n_workers, target_chunks_per_worker = 1L, max = 2048L),
      profile = "speed",
      diagnostics = FALSE
    )
    stopifnot(identical(as.integer(out[]), idx1))
  }
)

# =============================================================================
# Test 2: Shared-big-matrix access (random row sums)
# =============================================================================
set.seed(2)
n_rows <- 20000L
n_cols <- 256L
X2 <- matrix(rnorm(n_rows * n_cols), nrow = n_rows, ncol = n_cols)
X2_shard <- share(X2, backing = "mmap")

n_tasks2 <- 50000L
rows2 <- sample.int(n_rows, n_tasks2, replace = TRUE)
rows2_d <- as.double(rows2)
expected2 <- rowSums(X2[rows2, , drop = FALSE])

results$test2 <- run_test("2", sprintf("Shared big X + row sums (%dx%d, %d queries)", n_rows, n_cols, n_tasks2),
  mem_fn = function(cl, ns) {
    tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    on.exit(tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL), add = TRUE)

    registerVariables(ns, list(X = X2))
    res <- memLapply(
      X = as.list(rows2_d),
      FUN = function(r, X) sum(X[as.integer(r), ]),
      CLUSTER = cl,
      NAMESPACE = ns,
      VARS = c("X"),
      MAX.CORES = n_workers
    )
    stopifnot(all.equal(unlist(res, use.names = FALSE), expected2, tolerance = 1e-10))
  },
  shard_fn = function() {
    out <- buffer("double", dim = n_tasks2)
    shard_map(
      n_tasks2,
      borrow = list(X = X2_shard, rows = rows2),
      out = list(out = out),
      fun = function(sh, X, rows, out) {
        for (j in sh$idx) out[j] <- sum(X[rows[j], ])
        NULL
      },
      workers = n_workers,
      chunk_size = auto_chunk_size(n_tasks2, n_workers, target_chunks_per_worker = 2L, max = 8192L),
      profile = "speed",
      diagnostics = FALSE
    )
    stopifnot(all.equal(as.numeric(out[]), expected2, tolerance = 1e-10))
  }
)

# =============================================================================
# Summary
# =============================================================================
cat("============================================================\n")
cat("SUMMARY (ratio > 1 means shard faster)\n")
cat("============================================================\n\n")

cat(sprintf("%-40s %10s %10s %10s\n", "Test", "memshare", "shard", "Ratio"))
cat(strrep("-", 74), "\n")

test_names <- c(
  test1 = "1. Dispatch throughput",
  test2 = "2. Shared big X row sums"
)

for (name in names(results)) {
  r <- results[[name]]
  if (!is.na(r$mem) && !is.na(r$shard)) {
    ratio <- r$mem / r$shard
    winner <- if (ratio > 1.1) "SHARD" else if (ratio < 0.9) "memshare" else "~tie"
    cat(sprintf("%-40s %9.3fs %9.3fs %9.2fx  %s\n",
      test_names[name], r$mem, r$shard, ratio, winner
    ))
  } else {
    cat(sprintf("%-40s %9s %9s %9s\n",
      test_names[name],
      if (is.na(r$mem)) "FAILED" else sprintf("%.3fs", r$mem),
      if (is.na(r$shard)) "FAILED" else sprintf("%.3fs", r$shard),
      "N/A"
    ))
  }
}

cat("\nNotes:\n")
cat("- Each test uses fresh workers for fair comparison\n")
cat("- shard uses explicit output buffers (no result gathering)\n")
cat("- memshare gathers results into lists\n")
cat("\nBenchmark complete.\n")
