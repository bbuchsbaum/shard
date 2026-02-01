# =============================================================================
# Quick Benchmark: shard vs memshare
# =============================================================================
# Fair comparison using pre-warmed workers for both packages.
#
# Run: Rscript inst/bench/shard_vs_memshare_quick.R
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
# Keep this short: memshare enforces a <32-char UID limit on macOS.
bench_id <- paste0("sv", paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = ""))

cat("============================================================\n")
cat("Quick Benchmark: shard vs memshare (", n_workers, "workers)\n")
cat("============================================================\n\n")
cat("shard version: ", as.character(utils::packageVersion("shard")), "\n", sep = "")
cat("shard path:    ", find.package("shard"), "\n", sep = "")
cat("NOTE: This benchmark uses the installed 'shard'. If you're iterating locally, run `R CMD INSTALL .` first.\n\n")

results <- list()

run_test <- function(name, desc, mem_fn, shard_fn) {
  cat(sprintf("Test %s: %s\n", name, desc))
  ns <- paste0(bench_id, "_t", name)

  # Create fresh workers for each test (memshare has cluster state issues)
  cl <- makeCluster(n_workers)
  pool_stop()
  pool_create(n_workers)
  # Warm shard pool
  invisible(shard_map(n_workers, function(sh) NULL, workers = n_workers, diagnostics = FALSE))

  t_mem <- tryCatch({
    system.time(mem_fn(cl, ns))[["elapsed"]]
  }, error = function(e) {
    cat("  memshare error:", conditionMessage(e), "\n")
    NA
  })

  t_shard <- tryCatch({
    system.time(shard_fn())[["elapsed"]]
  }, error = function(e) {
    cat("  shard error:", conditionMessage(e), "\n")
    NA
  })

  stopCluster(cl)
  pool_stop()

  if (!is.na(t_mem) && !is.na(t_shard)) {
    ratio <- t_mem / t_shard
    cat(sprintf("  memshare: %.3fs | shard: %.3fs | ratio: %.2fx\n\n",
        t_mem, t_shard, ratio))
  } else {
    ratio <- NA
    cat("\n")
  }

  list(mem = t_mem, shard = t_shard, ratio = ratio)
}

# =============================================================================
# Test 1: Column means
# =============================================================================
set.seed(42)
X1 <- matrix(rnorm(2000 * 500), nrow = 2000, ncol = 500)
expected1 <- colMeans(X1)

results$test1 <- run_test("1", "Column means (2000x500 matrix)",
  mem_fn = function(cl, ns) {
    tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    on.exit({
      tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    }, add = TRUE)

    res <- memApply(X1, 2, mean, CLUSTER = cl, NAMESPACE = ns, MAX.CORES = n_workers)
    stopifnot(all.equal(unlist(res), expected1, tolerance = 1e-10))
  },
  shard_fn = function() {
    out <- buffer("double", dim = 500)
    shard_map(
      500,
      borrow = list(X = X1),
      out = list(out = out),
      kernel = "col_means",
      workers = n_workers,
      profile = "speed",
      diagnostics = FALSE
    )
    stopifnot(all.equal(as.numeric(out[]), expected1, tolerance = 1e-10))
  }
)

# =============================================================================
# Test 2: List of matrices (Frobenius norms)
# =============================================================================
set.seed(456)
mat_list <- lapply(1:100, function(i) matrix(rnorm(50*50), 50, 50))
expected2 <- sapply(mat_list, function(m) sqrt(sum(m^2)))
mat_list_shard <- lapply(mat_list, function(m) share(m, backing = "mmap"))

results$test2 <- run_test("2", "Frobenius norms (100 matrices, 50x50)",
  mem_fn = function(cl, ns) {
    tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    on.exit({
      tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    }, add = TRUE)

    res <- memLapply(
      X = mat_list,
      FUN = function(m) sqrt(sum(m^2)),
      CLUSTER = cl,
      NAMESPACE = ns,
      MAX.CORES = n_workers
    )
    stopifnot(all.equal(unlist(res), expected2, tolerance = 1e-10))
  },
  shard_fn = function() {
    out <- buffer("double", dim = 100)
    shard_map(
      100,
      borrow = list(mats = mat_list_shard),
      out = list(out = out),
      fun = function(sh, mats, out) {
        for (i in sh$idx) {
          out[i] <- sqrt(sum(mats[[i]]^2))
        }
        NULL
      },
      workers = n_workers,
      profile = "speed",
      diagnostics = FALSE
    )
    stopifnot(all.equal(as.numeric(out[]), expected2, tolerance = 1e-10))
  }
)

# =============================================================================
# Test 3: Crossprod X'Y
# =============================================================================
set.seed(789)
X3 <- matrix(rnorm(2000 * 64), nrow = 2000, ncol = 64)
Y3 <- matrix(rnorm(2000 * 128), nrow = 2000, ncol = 128)
expected3 <- crossprod(X3, Y3)

tile_spec <- matrix(
  c(
    1, 32, 1, 64,
    1, 32, 65, 128,
    33, 64, 1, 64,
    33, 64, 65, 128
  ),
  ncol = 4,
  byrow = TRUE
)
colnames(tile_spec) <- c("i_start", "i_end", "j_start", "j_end")
tile_list <- lapply(seq_len(nrow(tile_spec)), function(k) as.double(tile_spec[k, ]))

results$test3 <- run_test("3", "Crossprod X'Y (2000x64) x (2000x128)",
  mem_fn = function(cl, ns) {
    tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    on.exit({
      tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    }, add = TRUE)

    registerVariables(ns, list(X = X3, Y = Y3))
    res_tiles <- memLapply(
      X = tile_list,
      FUN = function(tile, X, Y) {
        i <- as.integer(tile[1]):as.integer(tile[2])
        j <- as.integer(tile[3]):as.integer(tile[4])
        crossprod(X[, i, drop = FALSE], Y[, j, drop = FALSE])
      },
      CLUSTER = cl, NAMESPACE = ns, VARS = c("X", "Y"), MAX.CORES = n_workers
    )
    out_mem <- matrix(0, 64, 128)
    for (k in seq_len(nrow(tile_spec))) {
      tile <- tile_spec[k, ]
      out_mem[tile[1]:tile[2], tile[3]:tile[4]] <- res_tiles[[k]]
    }
    stopifnot(all.equal(out_mem, expected3, tolerance = 1e-8))
  },
  shard_fn = function() {
    res <- shard_crossprod(X3, Y3, workers = n_workers, block_x = 32, block_y = 64,
                           backing = "mmap", materialize = "always", diagnostics = FALSE)
    stopifnot(all.equal(res$value, expected3, tolerance = 1e-8))
  }
)

# =============================================================================
# Test 4: Column variance (more complex per-column operation)
# =============================================================================
set.seed(321)
X4 <- matrix(rnorm(3000 * 300), nrow = 3000, ncol = 300)
expected4 <- apply(X4, 2, var)

results$test4 <- run_test("4", "Column variance (3000x300 matrix)",
  mem_fn = function(cl, ns) {
    tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    on.exit({
      tryCatch(memshare_gc(namespace = ns, cluster = cl), error = function(e) NULL)
    }, add = TRUE)

    res <- memApply(X4, 2, var, CLUSTER = cl, NAMESPACE = ns, MAX.CORES = n_workers)
    stopifnot(all.equal(unlist(res), expected4, tolerance = 1e-10))
  },
  shard_fn = function() {
    out <- buffer("double", dim = 300)
    shard_map(
      300,
      borrow = list(X = X4),
      out = list(out = out),
      kernel = "col_vars",
      workers = n_workers,
      profile = "speed",
      diagnostics = FALSE
    )
    stopifnot(all.equal(as.numeric(out[]), expected4, tolerance = 1e-10))
  }
)

# =============================================================================
# Summary
# =============================================================================
cat("============================================================\n")
cat("SUMMARY (ratio > 1 means shard faster)\n")
cat("============================================================\n\n")

cat(sprintf("%-35s %10s %10s %10s\n", "Test", "memshare", "shard", "Ratio"))
cat(strrep("-", 67), "\n")

test_names <- c(
  test1 = "1. Column means",
  test2 = "2. Matrix Frobenius norms",
  test3 = "3. Crossprod X'Y",
  test4 = "4. Column variance"
)

for (name in names(results)) {
  r <- results[[name]]
  if (!is.na(r$mem) && !is.na(r$shard)) {
    ratio <- r$mem / r$shard
    winner <- if(ratio > 1.1) "SHARD" else if(ratio < 0.9) "memshare" else "~tie"
    cat(sprintf("%-35s %9.3fs %9.3fs %9.2fx  %s\n",
        test_names[name], r$mem, r$shard, ratio, winner))
  } else {
    cat(sprintf("%-35s %9s %9s %9s\n",
        test_names[name],
        if(is.na(r$mem)) "FAILED" else sprintf("%.3fs", r$mem),
        if(is.na(r$shard)) "FAILED" else sprintf("%.3fs", r$shard),
        "N/A"))
  }
}

cat("\n")
cat("Notes:\n")
cat("- Each test uses fresh workers for fair comparison\n")
cat("- shard uses explicit output buffers (no result gathering)\n")
cat("- memshare gathers results into lists\n")
cat("- shard includes shard_crossprod() for built-in tiled matrix ops\n")

cat("\nBenchmark complete.\n")
