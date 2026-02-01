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

env_int <- function(name, default) {
  x <- Sys.getenv(name, unset = "")
  if (!nzchar(x)) return(as.integer(default))
  suppressWarnings(as.integer(x))
}

run_memshare <- function(desc, mem_fn, ns) {
  cl <- NULL
  t_mem <- tryCatch({
    cl <- makeCluster(n_workers)
    on.exit(tryCatch(stopCluster(cl), error = function(e) NULL), add = TRUE)
    system.time(mem_fn(cl, ns))[["elapsed"]]
  }, error = function(e) {
    cat("  memshare error:", conditionMessage(e), "\n")
    NA_real_
  })

  # Ensure workers are torn down even if memshare poisoned the cluster.
  if (!is.null(cl)) {
    tryCatch(stopCluster(cl), error = function(e) NULL)
  }
  t_mem
}

run_shard <- function(desc, shard_fn) {
  pool_stop()
  pool_create(n_workers)
  # Warm shard pool
  invisible(shard_map(n_workers, function(sh) NULL, workers = n_workers, diagnostics = FALSE))
  t_shard <- tryCatch({
    system.time(shard_fn())[["elapsed"]]
  }, error = function(e) {
    cat("  shard error:", conditionMessage(e), "\n")
    NA_real_
  })
  pool_stop()
  t_shard
}

run_test <- function(name, desc, mem_fn, shard_fn) {
  cat(sprintf("Test %s: %s\n", name, desc))
  ns <- paste0(bench_id, "_t", name)

  # Run memshare and shard in separate worker lifecycles so a failure in one
  # (common when experimenting with memshare namespaces) doesn't corrupt the other.
  t_mem <- run_memshare(desc, mem_fn, ns)
  t_shard <- run_shard(desc, shard_fn)

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
n_tasks1 <- env_int("SHARD_BENCH_N_TASKS1", 10000L)
idx1 <- seq_len(n_tasks1)
idx1_d <- as.double(idx1)

results$test1 <- run_test("1", sprintf("Dispatch throughput (%d tiny tasks)", n_tasks1),
  mem_fn = function(cl, ns) {
    tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    on.exit({
      tryCatch(releaseVariables(ns, c("idx")), error = function(e) NULL)
      tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    }, add = TRUE)

    registerVariables(ns, list(idx = as.list(idx1_d)))

    res <- memLapply(
      X = "idx",
      FUN = function(i) i,
      CLUSTER = cl,
      NAMESPACE = ns,
      MAX.CORES = n_workers
    )
    stopifnot(identical(unlist(res, use.names = FALSE), idx1_d))
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

n_tasks2 <- env_int("SHARD_BENCH_N_TASKS2", 50000L)
rows2 <- sample.int(n_rows, n_tasks2, replace = TRUE)
rows2_d <- as.double(rows2)
expected2 <- rowSums(X2[rows2, , drop = FALSE])

results$test2 <- run_test("2", sprintf("Shared big X + row sums (%dx%d, %d queries)", n_rows, n_cols, n_tasks2),
  mem_fn = function(cl, ns) {
    tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    on.exit({
      tryCatch(releaseVariables(ns, c("X", "rows")), error = function(e) NULL)
      tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    }, add = TRUE)

    registerVariables(ns, list(X = X2, rows = as.list(rows2_d)))
    res <- memLapply(
      X = "rows",
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
# Test 3: Many small objects (list of small numeric vectors)
# =============================================================================
set.seed(3)
n_vecs <- env_int("SHARD_BENCH_N_VECS", 20000L)
vec_len <- env_int("SHARD_BENCH_VEC_LEN", 16L)
vec_list <- replicate(n_vecs, rnorm(vec_len), simplify = FALSE)
expected3 <- vapply(vec_list, sum, numeric(1))

results$test3 <- run_test("3", sprintf("Many small vectors (list of %d x len=%d)", n_vecs, vec_len),
  mem_fn = function(cl, ns) {
    tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    on.exit({
      tryCatch(releaseVariables(ns, c("vecs")), error = function(e) NULL)
      tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    }, add = TRUE)

    registerVariables(ns, list(vecs = vec_list))

    res <- memLapply(
      X = "vecs",
      # Avoid primitives here: some parallel/memshare paths end up touching
      # function environments, and primitives have a NULL environment.
      FUN = function(v) sum(v),
      CLUSTER = cl,
      NAMESPACE = ns,
      MAX.CORES = n_workers
    )
    stopifnot(all.equal(unlist(res, use.names = FALSE), expected3, tolerance = 1e-12))
  },
  shard_fn = function() {
    out <- buffer("double", dim = n_vecs)
    shard_map(
      n_vecs,
      # Intentionally unshared list input: this measures the cost of shipping
      # many small objects to workers when they can't be represented as one
      # contiguous shared slab.
      borrow = list(vecs = vec_list),
      out = list(out = out),
      fun = function(sh, vecs, out) {
        for (j in sh$idx) {
          out[j] <- sum(vecs[[j]])
        }
        NULL
      },
      workers = n_workers,
      chunk_size = auto_chunk_size(n_vecs, n_workers, target_chunks_per_worker = 2L, max = 4096L),
      profile = "speed",
      diagnostics = FALSE
    )
    stopifnot(all.equal(as.numeric(out[]), expected3, tolerance = 1e-12))
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
  test2 = "2. Shared big X row sums",
  test3 = "3. Many small vectors"
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
