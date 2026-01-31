# =============================================================================
# Benchmark: shard vs memshare - Apples-to-Apples Comparison
# =============================================================================
#
# This benchmark compares shard and memshare on equivalent operations where
# both packages offer similar functionality.
#
# Prerequisites:
#   install.packages("memshare")
#   install.packages("bench")  # optional, for better timing
#
# Run:
#   Rscript inst/bench/shard_vs_memshare_benchmark.R
#
# =============================================================================

suppressPackageStartupMessages({
  library(shard)
  if (!requireNamespace("memshare", quietly = TRUE)) {
    stop("Please install memshare: install.packages('memshare')")
  }
  library(memshare)
  library(parallel)
})

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

n_workers <- 4L
set.seed(42)

cat("=============================================================================\n")
cat("BENCHMARK: shard vs memshare\n")
cat("=============================================================================\n")
cat("Workers:", n_workers, "\n")
cat("Platform:", R.version$platform, "\n")
cat("R version:", R.version.string, "\n\n")
cat("shard version: ", as.character(utils::packageVersion("shard")), "\n", sep = "")
cat("shard path:    ", find.package("shard"), "\n\n", sep = "")

# Cleanup function
cleanup <- function() {
  suppressWarnings({
    tryCatch(pool_stop(), error = function(e) NULL)
    tryCatch(memshare_gc("bench", cluster = NULL), error = function(e) NULL)
    gc(verbose = FALSE)
  })
}

on.exit(cleanup(), add = TRUE)

# -----------------------------------------------------------------------------
# Helper: Run benchmark with timing
# -----------------------------------------------------------------------------

run_bench <- function(name, expr, times = 3) {
  cat("  Running", name, "...\n")
  timings <- numeric(times)
  result <- NULL
  for (i in seq_len(times)) {
    gc(verbose = FALSE)
    t <- system.time({
      result <- force(expr)
    })
    timings[i] <- t[["elapsed"]]
  }
  list(
    name = name,
    median = median(timings),
    min = min(timings),
    max = max(timings),
    result = result
  )
}

# =============================================================================
# BENCHMARK 1: Parallel Column-wise Apply (Column Means)
# =============================================================================
# memshare: memApply(X, MARGIN=2, FUN)
# shard:    shard_map with column views or shard_apply_matrix
# =============================================================================

cat("\n-----------------------------------------------------------------------------\n")
cat("BENCHMARK 1: Parallel Column-wise Apply (Column Means)\n")
cat("-----------------------------------------------------------------------------\n")

bench1_run <- function(n_rows, n_cols) {
  cat("\nMatrix size:", n_rows, "x", n_cols, "\n")

  X <- matrix(rnorm(n_rows * n_cols), nrow = n_rows, ncol = n_cols)

  # Ground truth
  expected <- colMeans(X)

  memshare_ok <- tryCatch({
    ns0 <- "bench"
    registerVariables(ns0, list(x = as.double(1:8)))
    releaseVariables(ns0, "x")
    TRUE
  }, error = function(e) {
    cat("  NOTE: memshare self-test failed; skipping memshare timings.\n")
    cat("    ", conditionMessage(e), "\n", sep = "")
    FALSE
  })

  # --- memshare ---
  memshare_result <- NULL
  if (isTRUE(memshare_ok)) {
    cleanup()
    cl <- makeCluster(n_workers)
    on.exit(stopCluster(cl), add = TRUE)

    memshare_result <- run_bench("memshare::memApply", {
      res <- memApply(
        X = X,
        MARGIN = 2,
        FUN = function(x) mean(x),
        CLUSTER = cl,
        NAMESPACE = "bench",
        MAX.CORES = n_workers
      )
      unlist(res)
    })

    stopCluster(cl)
    tryCatch(releaseVariables(NAMESPACE = "bench"), error = function(e) NULL)
  }

  # --- shard (shard_map with column iteration) ---
  cleanup()

  shard_result <- run_bench("shard::shard_map", {
    X_shared <- share(X)
    out <- buffer("double", dim = n_cols)

    res <- shard_map(
      n_cols,
      borrow = list(X = X_shared),
      out = list(out = out),
      kernel = "col_means",
      workers = n_workers,
      profile = "speed",
      diagnostics = FALSE
    )
    as.numeric(out[])
  })

  # --- Validate ---
  if (!is.null(memshare_result)) {
    stopifnot(all.equal(memshare_result$result, expected, tolerance = 1e-10))
  }
  stopifnot(all.equal(shard_result$result, expected, tolerance = 1e-10))

  cat("\n  Results:\n")
  if (!is.null(memshare_result)) {
    cat("    memshare: ", sprintf("%.3f", memshare_result$median), "s (median)\n")
    cat("    shard:    ", sprintf("%.3f", shard_result$median), "s (median)\n")
    cat("    Speedup:  ", sprintf("%.2fx", memshare_result$median / shard_result$median), "\n")
  } else {
    cat("    memshare:  SKIPPED\n")
    cat("    shard:     ", sprintf("%.3f", shard_result$median), "s (median)\n")
  }

  list(memshare = memshare_result, shard = shard_result)
}

# Run with different sizes
bench1_small <- bench1_run(1000, 500)
bench1_large <- bench1_run(10000, 1000)


# =============================================================================
# BENCHMARK 2: Parallel Lapply Over List of Matrices
# =============================================================================
# memshare: memLapply(list, FUN)
# shard:    shard_map with shards_list
# =============================================================================

cat("\n-----------------------------------------------------------------------------\n")
cat("BENCHMARK 2: Parallel Lapply Over List of Matrices\n")
cat("-----------------------------------------------------------------------------\n")

bench2_run <- function(n_items, mat_size) {
  cat("\nList length:", n_items, ", Matrix size:", mat_size, "x", mat_size, "\n")

  # Create list of matrices
  mat_list <- lapply(seq_len(n_items), function(i) {
    matrix(rnorm(mat_size * mat_size), nrow = mat_size, ncol = mat_size)
  })

  # Ground truth: Frobenius norm of each matrix
  expected <- sapply(mat_list, function(m) sqrt(sum(m^2)))

  # --- memshare ---
  cleanup()
  cl <- makeCluster(n_workers)

  memshare_result <- run_bench("memshare::memLapply", {
    res <- memLapply(
      X = mat_list,
      FUN = function(m) sqrt(sum(m^2)),
      CLUSTER = cl,
      NAMESPACE = "bench2",
      MAX.CORES = n_workers
    )
    unlist(res)
  })

  stopCluster(cl)
  tryCatch(releaseVariables(NAMESPACE = "bench2"), error = function(e) NULL)

  # --- shard ---
  cleanup()

  shard_result <- run_bench("shard::shard_map (shards_list)", {
    mat_list_shared <- share(mat_list, deep = TRUE)
    out <- buffer("double", dim = n_items)

    res <- shard_map(
      shards_list(seq_len(n_items)),
      borrow = list(mats = mat_list_shared),
      out = list(out = out),
      fun = function(sh, mats, out) {
        for (i in sh$idx) {
          m <- fetch(mats)[[i]]
          out[i] <- sqrt(sum(m^2))
        }
        NULL
      },
      workers = n_workers,
      diagnostics = FALSE
    )
    as.numeric(out[])
  })

  # --- Validate ---
  stopifnot(all.equal(memshare_result$result, expected, tolerance = 1e-10))
  stopifnot(all.equal(shard_result$result, expected, tolerance = 1e-10))

  cat("\n  Results:\n")
  cat("    memshare: ", sprintf("%.3f", memshare_result$median), "s (median)\n")
  cat("    shard:    ", sprintf("%.3f", shard_result$median), "s (median)\n")
  cat("    Speedup:  ", sprintf("%.2fx", memshare_result$median / shard_result$median), "\n")

  list(memshare = memshare_result, shard = shard_result)
}

bench2_small <- bench2_run(100, 50)
bench2_large <- bench2_run(500, 100)


# =============================================================================
# BENCHMARK 3: Many Small Tasks (Dispatch Overhead)
# =============================================================================
# This tests the overhead of dispatching many tiny tasks
# =============================================================================

cat("\n-----------------------------------------------------------------------------\n")
cat("BENCHMARK 3: Many Small Tasks (Dispatch Overhead)\n")
cat("-----------------------------------------------------------------------------\n")

bench3_run <- function(n_tasks) {
  cat("\nNumber of tasks:", n_tasks, "\n")

  # Ground truth
  expected <- seq_len(n_tasks)^2

  # --- memshare ---
  cleanup()
  cl <- makeCluster(n_workers)

  memshare_result <- run_bench("memshare::memLapply", {
    res <- memLapply(
      X = as.list(seq_len(n_tasks)),
      FUN = function(x) x^2,
      CLUSTER = cl,
      NAMESPACE = "bench3",
      MAX.CORES = n_workers
    )
    unlist(res)
  })

  stopCluster(cl)
  tryCatch(releaseVariables(NAMESPACE = "bench3"), error = function(e) NULL)

  # --- shard (rpc_chunked mode) ---
  cleanup()

  shard_rpc_result <- run_bench("shard::shard_map (rpc_chunked)", {
    out <- buffer("double", dim = n_tasks)

    res <- shard_map(
      shards(n_tasks, workers = n_workers),
      out = list(out = out),
      fun = function(sh, out) {
        for (i in sh$idx) {
          out[i] <- i^2
        }
        NULL
      },
      workers = n_workers,
      diagnostics = FALSE
    )
    as.numeric(out[])
  })

  # --- shard (shm_queue mode - if tiny tasks) ---
  shard_shm_result <- NULL
  if (n_tasks >= 1000) {
    cleanup()

    shard_shm_result <- run_bench("shard::shard_map (shm_queue)", {
      out <- buffer("integer", dim = n_tasks, init = 0L)

      res <- shard_map(
        n_tasks,
        out = list(out = out),
        fun = function(sh, out) {
          out[sh$idx] <- as.integer(sh$idx^2)
          NULL
        },
        workers = n_workers,
        chunk_size = 1L,
        dispatch_mode = "shm_queue",
        dispatch_opts = list(block_size = 1L),
        diagnostics = FALSE
      )
      as.numeric(out[])
    })
  }

  # --- Validate ---
  stopifnot(all.equal(memshare_result$result, expected, tolerance = 1e-10))
  stopifnot(all.equal(shard_rpc_result$result, expected, tolerance = 1e-10))
  if (!is.null(shard_shm_result)) {
    stopifnot(all.equal(shard_shm_result$result, expected, tolerance = 1e-10))
  }

  cat("\n  Results:\n")
  cat("    memshare:           ", sprintf("%.3f", memshare_result$median), "s (median)\n")
  cat("    shard (rpc):        ", sprintf("%.3f", shard_rpc_result$median), "s (median)\n")
  if (!is.null(shard_shm_result)) {
    cat("    shard (shm_queue):  ", sprintf("%.3f", shard_shm_result$median), "s (median)\n")
    cat("    Speedup vs memshare:", sprintf("%.2fx", memshare_result$median / shard_shm_result$median), "\n")
  }

  list(memshare = memshare_result, shard_rpc = shard_rpc_result, shard_shm = shard_shm_result)
}

bench3_small <- bench3_run(500)
bench3_large <- bench3_run(5000)


# =============================================================================
# BENCHMARK 4: Large Shared Matrix Access Pattern
# =============================================================================
# Tests zero-copy efficiency when multiple workers read from shared matrix
# =============================================================================

cat("\n-----------------------------------------------------------------------------\n")
cat("BENCHMARK 4: Large Shared Matrix - Row Subsetting\n")
cat("-----------------------------------------------------------------------------\n")

bench4_run <- function(n_rows, n_cols, n_subsets) {
  cat("\nMatrix:", n_rows, "x", n_cols, ", Subsets:", n_subsets, "\n")

  X <- matrix(rnorm(n_rows * n_cols), nrow = n_rows, ncol = n_cols)

  # Random row subsets
  subset_size <- n_rows %/% 10
  subsets <- lapply(seq_len(n_subsets), function(i) {
    sample(n_rows, subset_size)
  })

  # Ground truth
  expected <- sapply(subsets, function(idx) sum(X[idx, ]))

  # --- memshare ---
  cleanup()
  cl <- makeCluster(n_workers)

  # Register X in shared memory
  registerVariables(list(X = X), NAMESPACE = "bench4")

  memshare_result <- run_bench("memshare (registered)", {
    res <- memLapply(
      X = subsets,
      FUN = function(idx, X) {
        sum(X[idx, ])
      },
      CLUSTER = cl,
      NAMESPACE = "bench4",
      VARS = "X",
      MAX.CORES = n_workers
    )
    unlist(res)
  })

  stopCluster(cl)
  tryCatch(releaseVariables(NAMESPACE = "bench4"), error = function(e) NULL)

  # --- shard ---
  cleanup()

  shard_result <- run_bench("shard::share + shard_map", {
    X_shared <- share(X)
    out <- buffer("double", dim = n_subsets)

    # Also share the subsets list
    subsets_shared <- share(subsets, deep = TRUE)

    res <- shard_map(
      shards(n_subsets, workers = n_workers),
      borrow = list(X = X_shared, subsets = subsets_shared),
      out = list(out = out),
      fun = function(sh, X, subsets, out) {
        X_mat <- fetch(X)
        subs <- fetch(subsets)
        for (i in sh$idx) {
          out[i] <- sum(X_mat[subs[[i]], ])
        }
        NULL
      },
      workers = n_workers,
      diagnostics = FALSE
    )
    as.numeric(out[])
  })

  # --- Validate ---
  stopifnot(all.equal(memshare_result$result, expected, tolerance = 1e-10))
  stopifnot(all.equal(shard_result$result, expected, tolerance = 1e-10))

  cat("\n  Results:\n")
  cat("    memshare: ", sprintf("%.3f", memshare_result$median), "s (median)\n")
  cat("    shard:    ", sprintf("%.3f", shard_result$median), "s (median)\n")
  cat("    Speedup:  ", sprintf("%.2fx", memshare_result$median / shard_result$median), "\n")

  list(memshare = memshare_result, shard = shard_result)
}

bench4_result <- bench4_run(5000, 200, 100)


# =============================================================================
# BENCHMARK 5: Cross-product Style Operation
# =============================================================================
# X'Y computation - tests BLAS-3 efficiency with shared data
# =============================================================================

cat("\n-----------------------------------------------------------------------------\n")
cat("BENCHMARK 5: Cross-product (X'Y) with Shared Matrices\n")
cat("-----------------------------------------------------------------------------\n")

bench5_run <- function(n, p, v) {
  cat("\nX:", n, "x", p, ", Y:", n, "x", v, "\n")

  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n * v), nrow = n, ncol = v)

  # Ground truth
  expected <- crossprod(X, Y)

  # --- memshare: Manual tiled crossprod ---
  cleanup()
  cl <- makeCluster(n_workers)

  # Register matrices
  registerVariables(list(X = X, Y = Y), NAMESPACE = "bench5")

  # Create tile indices
  block_p <- ceiling(p / 2)
  block_v <- ceiling(v / 2)

  tiles <- list()
  for (i in seq(1, p, by = block_p)) {
    for (j in seq(1, v, by = block_v)) {
      i_end <- min(i + block_p - 1, p)
      j_end <- min(j + block_v - 1, v)
      tiles <- c(tiles, list(list(i_start = i, i_end = i_end, j_start = j, j_end = j_end)))
    }
  }

  memshare_result <- run_bench("memshare (manual tiles)", {
    res <- memLapply(
      X = tiles,
      FUN = function(tile, X, Y) {
        i_idx <- tile$i_start:tile$i_end
        j_idx <- tile$j_start:tile$j_end
        crossprod(X[, i_idx, drop = FALSE], Y[, j_idx, drop = FALSE])
      },
      CLUSTER = cl,
      NAMESPACE = "bench5",
      VARS = c("X", "Y"),
      MAX.CORES = n_workers
    )

    # Assemble result
    out <- matrix(0, nrow = p, ncol = v)
    for (k in seq_along(tiles)) {
      tile <- tiles[[k]]
      i_idx <- tile$i_start:tile$i_end
      j_idx <- tile$j_start:tile$j_end
      out[i_idx, j_idx] <- res[[k]]
    }
    out
  })

  stopCluster(cl)
  tryCatch(releaseVariables(NAMESPACE = "bench5"), error = function(e) NULL)

  # --- shard: shard_crossprod ---
  cleanup()

  shard_result <- run_bench("shard::shard_crossprod", {
    res <- shard_crossprod(
      X, Y,
      workers = n_workers,
      block_x = block_p,
      block_y = block_v,
      backing = "mmap",
      materialize = "always",
      diagnostics = FALSE
    )
    res$value
  })

  # --- Validate ---
  stopifnot(all.equal(memshare_result$result, expected, tolerance = 1e-8))
  stopifnot(all.equal(shard_result$result, expected, tolerance = 1e-8))

  cat("\n  Results:\n")
  cat("    memshare: ", sprintf("%.3f", memshare_result$median), "s (median)\n")
  cat("    shard:    ", sprintf("%.3f", shard_result$median), "s (median)\n")
  cat("    Speedup:  ", sprintf("%.2fx", memshare_result$median / shard_result$median), "\n")

  list(memshare = memshare_result, shard = shard_result)
}

bench5_result <- bench5_run(3000, 64, 128)


# =============================================================================
# BENCHMARK 6: Memory Efficiency (Peak RSS)
# =============================================================================
# Compare peak memory usage during parallel operations
# =============================================================================

cat("\n-----------------------------------------------------------------------------\n")
cat("BENCHMARK 6: Memory Efficiency During Parallel Operations\n")
cat("-----------------------------------------------------------------------------\n")

bench6_run <- function(n_rows, n_cols) {
  cat("\nMatrix size:", n_rows, "x", n_cols,
      "(", round(n_rows * n_cols * 8 / 1024^2, 1), "MB)\n")

  X <- matrix(rnorm(n_rows * n_cols), nrow = n_rows, ncol = n_cols)
  mat_size_mb <- n_rows * n_cols * 8 / 1024^2

  # --- shard with memory reporting ---
  cleanup()
  gc(verbose = FALSE)

  cat("\n  Running shard with diagnostics...\n")

  X_shared <- share(X)
  out <- buffer("double", dim = n_cols)

  res <- shard_map(
    shards(n_cols, workers = n_workers),
    borrow = list(X = X_shared),
    out = list(out = out),
    fun = function(sh, X, out) {
      X_mat <- fetch(X)
      for (j in sh$idx) {
        out[j] <- sum(X_mat[, j]^2)
      }
      NULL
    },
    workers = n_workers,
    diagnostics = TRUE
  )

  mem <- mem_report(res)
  copy <- copy_report(res)

  cat("\n  shard Memory Report:\n")
  cat("    Main process peak RSS:  ", round(mem$main$peak_mb, 1), "MB\n")
  cat("    Worker peak RSS (mean): ", round(mean(sapply(mem$workers, function(w) w$peak_mb)), 1), "MB\n")
  cat("    View materialized bytes:", copy$view_materialized_bytes, "\n")
  cat("    Buffer write bytes:     ", copy$buffer_write_bytes, "\n")

  cat("\n  Note: memshare doesn't provide equivalent memory diagnostics.\n")
  cat("  shard's explicit buffer approach avoids result gathering overhead.\n")

  list(
    shard_main_peak_mb = mem$main$peak_mb,
    shard_worker_peak_mb = mean(sapply(mem$workers, function(w) w$peak_mb)),
    matrix_size_mb = mat_size_mb
  )
}

bench6_result <- bench6_run(5000, 500)


# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("SUMMARY\n")
cat("=============================================================================\n\n")

cat("Benchmark results show relative performance (speedup > 1 means shard is faster):\n\n")

cat("1. Column-wise Apply:\n")
cat("   - Small matrix: shard ",
    sprintf("%.2fx", bench1_small$memshare$median / bench1_small$shard$median), "\n")
cat("   - Large matrix: shard ",
    sprintf("%.2fx", bench1_large$memshare$median / bench1_large$shard$median), "\n")

cat("\n2. Lapply Over List:\n")
cat("   - Small list:   shard ",
    sprintf("%.2fx", bench2_small$memshare$median / bench2_small$shard$median), "\n")
cat("   - Large list:   shard ",
    sprintf("%.2fx", bench2_large$memshare$median / bench2_large$shard$median), "\n")

cat("\n3. Many Small Tasks:\n")
cat("   - 500 tasks:    shard (rpc) ",
    sprintf("%.2fx", bench3_small$memshare$median / bench3_small$shard_rpc$median), "\n")
if (!is.null(bench3_large$shard_shm)) {
  cat("   - 5000 tasks:   shard (shm_queue) ",
      sprintf("%.2fx", bench3_large$memshare$median / bench3_large$shard_shm$median), "\n")
}

cat("\n4. Large Shared Matrix Access:\n")
cat("   - shard ",
    sprintf("%.2fx", bench4_result$memshare$median / bench4_result$shard$median), "\n")

cat("\n5. Cross-product (X'Y):\n")
cat("   - shard ",
    sprintf("%.2fx", bench5_result$memshare$median / bench5_result$shard$median), "\n")

cat("\n6. Memory Efficiency:\n")
cat("   - shard provides detailed diagnostics (mem_report, copy_report)\n")
cat("   - memshare lacks equivalent memory tracking\n")

cat("\n=============================================================================\n")
cat("KEY DIFFERENTIATORS:\n")
cat("=============================================================================\n")
cat("
shard advantages:
  - Explicit output buffers eliminate result gathering overhead
  - shm_queue mode provides 5-20x speedup for tiny tasks
  - Zero-copy views for matrix operations
  - Comprehensive diagnostics (mem_report, copy_report, recommendations)
  - Worker supervision with automatic recycling
  - Immutability by default prevents silent copies

memshare advantages:
  - Simpler API (fewer parameters)
  - Direct integration with parallel::makeCluster
  - Allows mutable shared data
")

cleanup()
cat("\nBenchmark complete.\n")
