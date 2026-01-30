# Benchmark / example: full crossprod(X, Y) via 2D tiles, block views, and an
# explicit output buffer.
#
# This exercises the "wow" path:
# - X and Y are shared once
# - each task constructs two block views (vX, vY)
# - compute uses BLAS-3 (dgemm) without materializing either view
# - results write directly into a shared output buffer (no gather + bind)
#
# Run from repo root:
#   R -q -f inst/bench/tiled_crossprod_buffer_shard_map.R

suppressPackageStartupMessages(library(shard))

set.seed(1)
n <- 6000L
p <- 128L
v <- 256L

X <- matrix(rnorm(n * p), nrow = n)
Y <- matrix(rnorm(n * v), nrow = n)

colnames(X) <- paste0("x", seq_len(ncol(X)))
colnames(Y) <- paste0("y", seq_len(ncol(Y)))

Xsh <- share(X, backing = "mmap")
Ysh <- share(Y, backing = "mmap")

# Preallocate the full output (p x v).
Z <- buffer("double", dim = c(p, v), init = 0, backing = "mmap")

# 2D tiling over output space. Each tile writes to a disjoint slice in Z.
tiles <- shard:::tiles2d(n_x = p, n_y = v, block_x = 32L, block_y = 64L)

res <- shard_map(
  tiles,
  borrow = list(X = Xsh, Y = Ysh),
  out = list(Z = Z),
  fun = function(tile, X, Y, Z) {
    vX <- view_block(X, cols = idx_range(tile$x_start, tile$x_end))
    vY <- view_block(Y, cols = idx_range(tile$y_start, tile$y_end))
    blk <- shard:::view_crossprod(vX, vY)
    Z[tile$x_start:tile$x_end, tile$y_start:tile$y_end] <- blk
    NULL
  },
  workers = 4,
  diagnostics = TRUE
)

print(task_report(res))
print(copy_report(res))

out <- as.matrix(Z)
cat("\nResult dim:", paste(dim(out), collapse = "x"), "\n")
cat("Max abs error vs base crossprod:", max(abs(out - crossprod(X, Y))), "\n")

