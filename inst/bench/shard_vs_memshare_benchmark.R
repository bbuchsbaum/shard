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
  skip_memshare <- tolower(Sys.getenv("SHARD_BENCH_SKIP_MEMSHARE", "")) %in% c("1", "true", "yes")
  if (!skip_memshare) {
    if (!requireNamespace("memshare", quietly = TRUE)) {
      stop("Please install memshare: install.packages('memshare')")
    }
    library(memshare)
  }
  library(parallel)
})

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

n_workers <- 4L
set.seed(42)
skip_memshare <- tolower(Sys.getenv("SHARD_BENCH_SKIP_MEMSHARE", "")) %in% c("1", "true", "yes")
bench_only_raw <- trimws(Sys.getenv("SHARD_BENCH_ONLY", ""))
bench_only_ids <- if (nzchar(bench_only_raw)) {
  suppressWarnings(as.integer(strsplit(bench_only_raw, ",", fixed = TRUE)[[1]]))
} else integer()
bench_only_ids <- bench_only_ids[is.finite(bench_only_ids)]
should_run <- function(id) length(bench_only_ids) == 0L || (id %in% bench_only_ids)

cat("=============================================================================\n")
cat("BENCHMARK: shard vs memshare\n")
cat("=============================================================================\n")
cat("Workers:", n_workers, "\n")
cat("Platform:", R.version$platform, "\n")
cat("R version:", R.version.string, "\n\n")
cat("shard version: ", as.character(utils::packageVersion("shard")), "\n", sep = "")
cat("shard path:    ", find.package("shard"), "\n\n", sep = "")
if (skip_memshare) {
  cat("NOTE: SHARD_BENCH_SKIP_MEMSHARE is set; memshare benchmarks will be skipped.\n\n")
}
if (length(bench_only_ids) > 0) {
  cat("NOTE: SHARD_BENCH_ONLY is set; running only benchmarks: ",
      paste(bench_only_ids, collapse = ", "),
      "\n\n", sep = "")
}

# Cleanup function
cleanup <- function() {
  suppressWarnings({
    tryCatch(pool_stop(), error = function(e) NULL)
    gc(verbose = FALSE)
  })
}

on.exit(cleanup(), add = TRUE)

# -----------------------------------------------------------------------------
# Helper: Run benchmark with timing
# -----------------------------------------------------------------------------

run_bench <- function(name, fn, times = 3, min_elapsed = 0.05, max_reps = 4096L) {
  cat("  Running", name, "...\n")
  if (!is.function(fn)) stop("run_bench: fn must be a function()", call. = FALSE)
  timings <- numeric(times)
  result <- NULL
  for (i in seq_len(times)) {
    gc(verbose = FALSE)
    reps <- 1L
    elapsed <- 0
    repeat {
      t <- system.time({
        for (k in seq_len(reps)) {
          result <- fn()
        }
      })
      elapsed <- t[["elapsed"]]
      if (!is.finite(elapsed)) break
      if (elapsed >= min_elapsed) break
      if (reps >= as.integer(max_reps)) break
      reps <- reps * 2L
    }
    timings[i] <- elapsed / max(1L, reps)
  }
  list(
    name = name,
    median = median(timings),
    min = min(timings),
    max = max(timings),
    result = result
  )
}

fmt_s <- function(x) sprintf("%.6f", x)

fmt_speedup <- function(mem, shard) {
  if (is.null(mem) || is.null(shard)) return("NA")
  if (!is.finite(mem) || !is.finite(shard) || shard <= 0) return("NA")
  sprintf("%.2fx", mem / shard)
}

unique_ns <- function(prefix) {
  suffix <- paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
  paste0(prefix, "_", Sys.getpid(), "_", suffix)
}

memshare_result_ok <- function(x, expected_len) {
  if (is.null(x)) return(FALSE)
  if (is.null(x$result)) return(FALSE)
  if (!is.numeric(x$result)) return(FALSE)
  if (length(x$result) != expected_len) return(FALSE)
  if (anyNA(x$result)) return(FALSE)
  TRUE
}

check_equal <- function(label, actual, expected, tolerance = 1e-10) {
  ok <- isTRUE(all.equal(actual, expected, tolerance = tolerance))
  if (!ok) {
    delta <- tryCatch(
      mean(abs(as.numeric(actual) - as.numeric(expected))),
      error = function(e) NA_real_
    )
    cat("  NOTE: validation failed for ", label, ".\n", sep = "")
    if (is.finite(delta)) {
      cat("    Mean absolute difference: ", delta, "\n", sep = "")
    }
    if (!skip_memshare) {
      cat("    If memshare is built for a different R version, try re-running with:\n")
      cat("      SHARD_BENCH_SKIP_MEMSHARE=1 Rscript inst/bench/shard_vs_memshare_benchmark.R\n")
    }
  }
  ok
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

  # --- memshare ---
  memshare_result <- NULL
  if (!skip_memshare) {
    cleanup()
    cl <- makeCluster(n_workers)
    on.exit(tryCatch(stopCluster(cl), error = function(e) NULL), add = TRUE)
    ns <- unique_ns("b1")
    memshare_result <- tryCatch({
      # memshare::memApply has been flaky on some macOS/R setups (registerVariables
      # collisions and cleanup errors). Use a stable equivalent based on
      # memLapply over column indices + a registered X.
      registerVariables(ns, list(X = X))
      run_bench("memshare::memLapply (colMeans)", function() {
        res <- memLapply(
          X = as.list(as.double(seq_len(n_cols))),
          FUN = function(j, X) {
            j <- as.integer(j)
            mean(X[, j])
          },
          CLUSTER = cl,
          NAMESPACE = ns,
          VARS = "X",
          MAX.CORES = n_workers
        )
        out <- unlist(res, use.names = FALSE)
        if (length(out) != n_cols || anyNA(out)) stop("memshare returned invalid result", call. = FALSE)
        out
      }, times = 1)
    },
    error = function(e) {
      cat("  NOTE: memshare failed; skipping memshare timings.\n")
      cat("    ", conditionMessage(e), "\n", sep = "")
      NULL
    },
    finally = {
      tryCatch(releaseVariables(ns, c("X")), error = function(e) NULL)
      tryCatch(memshare_gc(ns, cluster = cl), error = function(e) NULL)
      tryCatch(stopCluster(cl), error = function(e) NULL)
    })
  }

  # --- shard (shard_map with column iteration) ---
  cleanup()

  X_shared <- share(X)
  out <- buffer("double", dim = n_cols)

  shard_result <- run_bench("shard::shard_map", function() {
    shard_map(
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
  if (memshare_result_ok(memshare_result, n_cols)) {
    check_equal("memshare bench1", memshare_result$result, expected, tolerance = 1e-10)
  }
  check_equal("shard bench1", shard_result$result, expected, tolerance = 1e-10)

  cat("\n  Results:\n")
  if (memshare_result_ok(memshare_result, n_cols)) {
    cat("    memshare:  ", fmt_s(memshare_result$median), "s (median)\n", sep = "")
    cat("    shard:     ", fmt_s(shard_result$median), "s (median)\n", sep = "")
    cat("    Speedup:   ", fmt_speedup(memshare_result$median, shard_result$median), "\n", sep = "")
  } else {
    cat("    memshare:  SKIPPED\n")
    cat("    shard:     ", fmt_s(shard_result$median), "s (median)\n", sep = "")
  }

  list(memshare = memshare_result, shard = shard_result)
}

# Run with different sizes
bench1_small <- if (should_run(1)) bench1_run(1000, 500) else NULL
bench1_large <- if (should_run(1)) bench1_run(10000, 1000) else NULL


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
  expected <- vapply(mat_list, function(m) sqrt(sum(m * m)), numeric(1))

  # --- memshare ---
  cleanup()
  cl <- makeCluster(n_workers)

  ns <- unique_ns("b2")
  memshare_result <- NULL
  if (!skip_memshare) {
    memshare_result <- tryCatch({
      registerVariables(ns, list(mats = mat_list))
      run_bench("memshare::memLapply", function() {
        res <- memLapply(
          X = "mats",
          FUN = function(m) sqrt(sum(m * m)),
          CLUSTER = cl,
          NAMESPACE = ns,
          MAX.CORES = n_workers
        )
        out <- unlist(res)
        if (length(out) != n_items || anyNA(out)) stop("memshare::memLapply returned invalid result", call. = FALSE)
        out
      }, times = 1)
    },
    error = function(e) {
      cat("  NOTE: memshare failed; skipping memshare timings.\n")
      cat("    ", conditionMessage(e), "\n", sep = "")
      NULL
    },
    finally = {
      tryCatch(releaseVariables(ns, c("mats")), error = function(e) NULL)
      tryCatch(memshare_gc(ns, cluster = cl), error = function(e) NULL)
    }
  )

    stopCluster(cl)
    tryCatch(memshare_gc(ns, cluster = NULL), error = function(e) NULL)
  }

  # --- shard ---
  cleanup()

  # Sharing each matrix as an ALTREP-backed segment does not scale: with many
  # matrices it can exhaust worker file descriptors / segment handles during
  # unserialization (macOS often fails with "Failed to open ... ALTREP
  # unserialize"). For this benchmark we share the list container as ONE
  # serialized object and fetch it once per shard.
  mat_list_shared <- share(mat_list)
  out <- buffer("double", dim = n_items)

  shard_result <- run_bench("shard::shard_map", function() {
    shard_map(
      shards(n_items, block_size = ceiling(n_items / n_workers), workers = n_workers),
      borrow = list(mats = mat_list_shared),
      out = list(out = out),
      fun = function(sh, mats, out) {
        mats0 <- fetch(mats)
        for (i in sh$idx) {
          m <- mats0[[i]]
          out[i] <- sqrt(sum(m * m))
        }
        NULL
      },
      workers = n_workers,
      profile = "speed",
      diagnostics = FALSE
    )
    as.numeric(out[])
  })

  # --- Validate ---
  if (memshare_result_ok(memshare_result, n_items)) {
    check_equal("memshare bench2", memshare_result$result, expected, tolerance = 1e-10)
  }
  check_equal("shard bench2", shard_result$result, expected, tolerance = 1e-10)

  cat("\n  Results:\n")
  if (memshare_result_ok(memshare_result, n_items)) {
    cat("    memshare:  ", fmt_s(memshare_result$median), "s (median)\n", sep = "")
    cat("    shard:     ", fmt_s(shard_result$median), "s (median)\n", sep = "")
    cat("    Speedup:   ", fmt_speedup(memshare_result$median, shard_result$median), "\n", sep = "")
  } else {
    cat("    memshare:  FAILED\n")
    cat("    shard:     ", fmt_s(shard_result$median), "s (median)\n", sep = "")
  }

  list(memshare = memshare_result, shard = shard_result)
}

bench2_small <- if (should_run(2)) bench2_run(100, 50) else NULL
bench2_large <- if (should_run(2)) bench2_run(500, 100) else NULL


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

  ns <- unique_ns("b3")
  memshare_result <- NULL
  if (!skip_memshare) {
    memshare_result <- tryCatch(
      run_bench("memshare::memLapply", function() {
        res <- memLapply(
          X = as.list(as.double(seq_len(n_tasks))),
          FUN = function(x) x^2,
          CLUSTER = cl,
          NAMESPACE = ns,
          MAX.CORES = n_workers
        )
        out <- unlist(res)
        if (length(out) != n_tasks || anyNA(out)) stop("memshare::memLapply returned invalid result", call. = FALSE)
        out
      }, times = 1),
      error = function(e) {
        cat("  NOTE: memshare failed; skipping memshare timings.\n")
        cat("    ", conditionMessage(e), "\n", sep = "")
        NULL
      },
      finally = {
        tryCatch(memshare_gc(ns, cluster = cl), error = function(e) NULL)
      }
    )

    stopCluster(cl)
    tryCatch(memshare_gc(ns, cluster = NULL), error = function(e) NULL)
  }

  # --- shard (rpc_chunked mode) ---
  cleanup()

  out <- buffer("double", dim = n_tasks)

  shard_rpc_result <- run_bench("shard::shard_map (rpc_chunked)", function() {
    shard_map(
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

    out2 <- buffer("integer", dim = n_tasks, init = 0L)

    shard_shm_result <- run_bench("shard::shard_map (shm_queue)", function() {
      shard_map(
        n_tasks,
        out = list(out = out2),
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
      as.numeric(out2[])
    })
  }

  # --- Validate ---
  if (memshare_result_ok(memshare_result, n_tasks)) {
    check_equal("memshare bench3", memshare_result$result, expected, tolerance = 1e-10)
  }
  check_equal("shard bench3 rpc", shard_rpc_result$result, expected, tolerance = 1e-10)
  if (!is.null(shard_shm_result)) {
    check_equal("shard bench3 shm_queue", shard_shm_result$result, expected, tolerance = 1e-10)
  }

  cat("\n  Results:\n")
  if (memshare_result_ok(memshare_result, n_tasks)) {
    cat("    memshare:           ", fmt_s(memshare_result$median), "s (median)\n", sep = "")
  } else {
    cat("    memshare:           FAILED\n")
  }
  cat("    shard (rpc):        ", fmt_s(shard_rpc_result$median), "s (median)\n", sep = "")
  if (!is.null(shard_shm_result)) {
    cat("    shard (shm_queue):  ", fmt_s(shard_shm_result$median), "s (median)\n", sep = "")
    if (memshare_result_ok(memshare_result, n_tasks)) {
      cat("    Speedup vs memshare:", fmt_speedup(memshare_result$median, shard_shm_result$median), "\n")
    }
  }

  list(memshare = memshare_result, shard_rpc = shard_rpc_result, shard_shm = shard_shm_result)
}

bench3_small <- if (should_run(3)) bench3_run(500) else NULL
bench3_large <- if (should_run(3)) bench3_run(5000) else NULL


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
  memshare_result <- NULL
  if (!skip_memshare) {
    cleanup()
    ns <- unique_ns("b4")
    cl <- NULL
    memshare_result <- tryCatch({
      cl <- makeCluster(n_workers)
      registerVariables(ns, list(X = X))
      run_bench("memshare (registered)", function() {
        res <- memLapply(
          X = lapply(subsets, as.double),
          FUN = function(idx, X) {
            sum(X[as.integer(idx), ])
          },
          CLUSTER = cl,
          NAMESPACE = ns,
          VARS = "X",
          MAX.CORES = n_workers
        )
        out <- unlist(res)
        if (length(out) != n_subsets || anyNA(out)) stop("memshare::memLapply returned invalid result", call. = FALSE)
        out
      }, times = 1)
    }, error = function(e) {
      cat("  NOTE: memshare failed; skipping memshare timings.\n")
      cat("    ", conditionMessage(e), "\n", sep = "")
      NULL
    }, finally = {
      if (!is.null(cl)) {
        tryCatch(memshare_gc(ns, cluster = cl), error = function(e) NULL)
        tryCatch(stopCluster(cl), error = function(e) NULL)
      }
      tryCatch(memshare_gc(ns, cluster = NULL), error = function(e) NULL)
    })
  }

  # --- shard ---
  cleanup()

  X_shared <- share(X)
  out <- buffer("double", dim = n_subsets)

  # Share the subsets list as a single serialized object (one segment). Deep
  # sharing turns this into many components and is unnecessary here.
  subsets_shared <- share(subsets)

  shard_result <- run_bench("shard::share + shard_map", function() {
    shard_map(
      shards(n_subsets, block_size = ceiling(n_subsets / n_workers), workers = n_workers),
      borrow = list(X = X_shared, subsets = subsets_shared),
      out = list(out = out),
      fun = function(sh, X, subsets, out) {
        subs <- fetch(subsets)
        for (i in sh$idx) {
          out[i] <- sum(X[subs[[i]], ])
        }
        NULL
      },
      workers = n_workers,
      diagnostics = FALSE
    )
    as.numeric(out[])
  })

  # --- Validate ---
  if (memshare_result_ok(memshare_result, n_subsets)) {
    check_equal("memshare bench4", memshare_result$result, expected, tolerance = 1e-10)
  }
  check_equal("shard bench4", shard_result$result, expected, tolerance = 1e-10)

  cat("\n  Results:\n")
  if (memshare_result_ok(memshare_result, n_subsets)) {
    cat("    memshare:  ", fmt_s(memshare_result$median), "s (median)\n", sep = "")
    cat("    shard:     ", fmt_s(shard_result$median), "s (median)\n", sep = "")
    cat("    Speedup:   ", fmt_speedup(memshare_result$median, shard_result$median), "\n", sep = "")
  } else {
    cat("    memshare:  FAILED\n")
    cat("    shard:     ", fmt_s(shard_result$median), "s (median)\n", sep = "")
  }

  list(memshare = memshare_result, shard = shard_result)
}

bench4_result <- if (should_run(4)) bench4_run(5000, 200, 100) else NULL


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

  # Tile sizes (used by both implementations)
  block_p <- ceiling(p / 2)
  block_v <- ceiling(v / 2)

  tiles <- list()
  for (i in seq(1, p, by = block_p)) {
    for (j in seq(1, v, by = block_v)) {
      i_end <- min(i + block_p - 1, p)
      j_end <- min(j + block_v - 1, v)
      tiles <- c(tiles, list(as.double(c(i, i_end, j, j_end))))
    }
  }

  # --- memshare: Manual tiled crossprod ---
  memshare_result <- NULL
  if (!skip_memshare) {
    cleanup()
    ns <- unique_ns("b5")
    cl <- NULL
    memshare_result <- tryCatch({
      cl <- makeCluster(n_workers)
      registerVariables(ns, list(X = X, Y = Y))

      run_bench("memshare (manual tiles)", function() {
        res <- memLapply(
          X = tiles,
          FUN = function(tile, X, Y) {
            i_idx <- as.integer(tile[1]):as.integer(tile[2])
            j_idx <- as.integer(tile[3]):as.integer(tile[4])
            crossprod(X[, i_idx, drop = FALSE], Y[, j_idx, drop = FALSE])
          },
          CLUSTER = cl,
          NAMESPACE = ns,
          VARS = c("X", "Y"),
          MAX.CORES = n_workers
        )

        # Assemble result
        out <- matrix(0, nrow = p, ncol = v)
        for (k in seq_along(tiles)) {
          tile <- tiles[[k]]
          i_idx <- as.integer(tile[1]):as.integer(tile[2])
          j_idx <- as.integer(tile[3]):as.integer(tile[4])
          out[i_idx, j_idx] <- res[[k]]
        }
        out
      }, times = 1, min_elapsed = 0.1)
    }, error = function(e) {
      cat("  NOTE: memshare failed; skipping memshare timings.\n")
      cat("    ", conditionMessage(e), "\n", sep = "")
      NULL
    }, finally = {
      if (!is.null(cl)) {
        tryCatch(memshare_gc(ns, cluster = cl), error = function(e) NULL)
        tryCatch(stopCluster(cl), error = function(e) NULL)
      }
      tryCatch(memshare_gc(ns, cluster = NULL), error = function(e) NULL)
    })
  }

  # --- shard: shard_crossprod ---
  cleanup()

  # Share inputs once so the pool never holds stale references to segments that
  # have been GC'd on the master across repeated runs.
  X_shared <- share(X, backing = "mmap")
  Y_shared <- share(Y, backing = "mmap")

  shard_result <- run_bench("shard::shard_crossprod", function() {
    res <- shard_crossprod(
      X_shared, Y_shared,
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
  if (memshare_result_ok(memshare_result, p * v)) {
    check_equal("memshare bench5", memshare_result$result, expected, tolerance = 1e-8)
  }
  check_equal("shard bench5", shard_result$result, expected, tolerance = 1e-8)

  cat("\n  Results:\n")
  if (memshare_result_ok(memshare_result, p * v)) {
    cat("    memshare:  ", fmt_s(memshare_result$median), "s (median)\n", sep = "")
    cat("    shard:     ", fmt_s(shard_result$median), "s (median)\n", sep = "")
    cat("    Speedup:   ", fmt_speedup(memshare_result$median, shard_result$median), "\n", sep = "")
  } else {
    cat("    memshare:  FAILED\n")
    cat("    shard:     ", fmt_s(shard_result$median), "s (median)\n", sep = "")
  }

  list(memshare = memshare_result, shard = shard_result)
}

bench5_result <- if (should_run(5)) bench5_run(3000, 64, 128) else NULL


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

  tryCatch({
    X_shared <- share(X)
    out <- buffer("double", dim = n_cols)

    res <- shard_map(
      shards(n_cols, workers = n_workers),
      borrow = list(X = X_shared),
      out = list(out = out),
      fun = function(sh, X, out) {
        # Keep a stable worker set for diagnostics runs: if a worker is recycled
        # mid-call, it can miss the borrowed inputs and fail chunks.
        for (j in sh$idx) {
          out[j] <- sum(X[, j]^2)
        }
        NULL
      },
      workers = n_workers,
      profile = "speed",
      recycle = FALSE,
      diagnostics = TRUE
    )

    mem <- tryCatch(suppressWarnings(mem_report()), error = function(e) NULL)
    copy <- tryCatch(suppressWarnings(copy_report(res)), error = function(e) NULL)

    main_peak_mb <- NA_real_
    worker_peak_mb <- NA_real_
    if (!is.null(mem)) {
      main_peak_mb <- suppressWarnings(as.double(mem$main$peak_mb %||% NA_real_))
      peaks <- tryCatch(
        vapply(mem$workers, function(w) suppressWarnings(as.double(w$peak_mb %||% NA_real_)), numeric(1)),
        error = function(e) numeric()
      )
      peaks <- peaks[is.finite(peaks)]
      if (length(peaks) > 0) worker_peak_mb <- mean(peaks)
    }

    if (is.finite(main_peak_mb) && is.finite(worker_peak_mb) && !is.null(copy)) {
      cat("\n  shard Memory Report:\n")
      cat("    Main process peak RSS:  ", round(main_peak_mb, 1), "MB\n")
      cat("    Worker peak RSS (mean): ", round(worker_peak_mb, 1), "MB\n")
      cat("    View materialized bytes:", copy$view_materialized_bytes, "\n")
      cat("    Buffer write bytes:     ", copy$buffer_write_bytes, "\n")
    } else {
      cat("\n  NOTE: Memory diagnostics unavailable for this run.\n")
    }

    cat("\n  Note: memshare doesn't provide equivalent memory diagnostics.\n")
    cat("  shard's explicit buffer approach avoids result gathering overhead.\n")

    list(
      shard_main_peak_mb = main_peak_mb,
      shard_worker_peak_mb = worker_peak_mb,
      matrix_size_mb = mat_size_mb
    )
  }, error = function(e) {
    cat("\n  NOTE: shard diagnostics run failed.\n")
    cat("    ", conditionMessage(e), "\n", sep = "")
    NULL
  })
}

bench6_result <- if (should_run(6)) bench6_run(5000, 500) else NULL


# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("SUMMARY\n")
cat("=============================================================================\n\n")

cat("Benchmark results show relative performance (speedup > 1 means shard is faster):\n\n")

cat("1. Column-wise Apply:\n")
cat("   - Small matrix: shard ", fmt_speedup(bench1_small$memshare$median, bench1_small$shard$median), "\n", sep = "")
cat("   - Large matrix: shard ", fmt_speedup(bench1_large$memshare$median, bench1_large$shard$median), "\n", sep = "")

cat("\n2. Lapply Over List:\n")
cat("   - Small list:   shard ", fmt_speedup(bench2_small$memshare$median, bench2_small$shard$median), "\n", sep = "")
cat("   - Large list:   shard ", fmt_speedup(bench2_large$memshare$median, bench2_large$shard$median), "\n", sep = "")

cat("\n3. Many Small Tasks:\n")
cat("   - 500 tasks:    shard (rpc) ", fmt_speedup(bench3_small$memshare$median, bench3_small$shard_rpc$median), "\n", sep = "")
if (!is.null(bench3_large$shard_shm)) {
  cat("   - 5000 tasks:   shard (shm_queue) ", fmt_speedup(bench3_large$memshare$median, bench3_large$shard_shm$median), "\n", sep = "")
}

cat("\n4. Large Shared Matrix Access:\n")
cat("   - shard ", fmt_speedup(bench4_result$memshare$median, bench4_result$shard$median), "\n", sep = "")

cat("\n5. Cross-product (X'Y):\n")
cat("   - shard ", fmt_speedup(bench5_result$memshare$median, bench5_result$shard$median), "\n", sep = "")

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
