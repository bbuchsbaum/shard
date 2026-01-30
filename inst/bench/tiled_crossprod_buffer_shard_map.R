# Benchmark / example: full crossprod(X, Y) via 2D tiles, block views, and an
# explicit output buffer.
#
# This exercises the "wow" path:
# - X and Y are shared once
# - each task constructs two block views (vX, vY)
# - compute uses BLAS-3 (dgemm) without materializing either view
# - results write directly into a shared output buffer (no gather + bind)
# - kernel dispatch avoids boilerplate: shard_map(..., kernel="crossprod_tile")
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

res <- shard_crossprod(
  X,
  Y,
  workers = 4,
  block_x = 32L,
  block_y = 64L,
  backing = "mmap",
  materialize = "always",
  diagnostics = TRUE
)

print(task_report(res$run))
print(copy_report(res$run))

out <- res$value
cat("\nResult dim:", paste(dim(out), collapse = "x"), "\n")
cat("Max abs error vs base crossprod:", max(abs(out - crossprod(X, Y))), "\n")
