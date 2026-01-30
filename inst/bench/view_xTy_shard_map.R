# Benchmark / example: view-enabled X'Y block computation with shard_map
#
# This script demonstrates an end-to-end pipeline where:
# - X and Y are shared once (no per-worker duplication)
# - each shard operates on a block view of Y (no Y[, idx] slice allocation)
# - the hot path uses a BLAS-3 kernel (t(X) %*% Y_block) without view materialization
#
# Run from repo root:
#   R -q -f inst/bench/view_xTy_shard_map.R

suppressPackageStartupMessages(library(shard))

set.seed(1)
n <- 5000L
p <- 32L
v <- 256L

X <- matrix(rnorm(n * p), nrow = n)
Y <- matrix(rnorm(n * v), nrow = n)

colnames(X) <- paste0("x", seq_len(ncol(X)))
colnames(Y) <- paste0("y", seq_len(ncol(Y)))

Xsh <- share(X, backing = "mmap")
Ysh <- share(Y, backing = "mmap")

blocks <- shards(ncol(Y), block_size = 32, workers = 4)

res <- shard_map(
  blocks,
  borrow = list(X = Xsh, Y = Ysh),
  fun = function(shard, X, Y) {
    vY <- view_block(Y, cols = idx_range(shard$start, shard$end))
    shard:::view_xTy(X, vY)
  },
  workers = 4,
  diagnostics = TRUE
)

print(task_report(res))
print(copy_report(res))

# Materialize the full matrix for a quick correctness check.
Z <- do.call(cbind, results(res))
cat("\nResult dim:", paste(dim(Z), collapse = "x"), "\n")
cat("Max abs error vs base crossprod:", max(abs(Z - crossprod(X, Y))), "\n")

