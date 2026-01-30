# Benchmark / example: view-enabled t(X_block) %*% Y_block using two views
#
# This demonstrates a "two-view" hot path:
# - X and Y are shared once
# - shard fun constructs block views for BOTH operands
# - the compute uses BLAS-3 (dgemm) without materializing either view
#
# Run from repo root:
#   R -q -f inst/bench/view_xTy_2views_shard_map.R

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
    vX <- view_block(X, cols = idx_range(1, ncol(X)))
    vY <- view_block(Y, cols = idx_range(shard$start, shard$end))
    shard:::view_crossprod(vX, vY)
  },
  workers = 4,
  diagnostics = TRUE
)

print(task_report(res))
print(copy_report(res))

Z <- do.call(cbind, results(res))
cat("\nResult dim:", paste(dim(Z), collapse = "x"), "\n")
cat("Max abs error vs base crossprod:", max(abs(Z - crossprod(X, Y))), "\n")

