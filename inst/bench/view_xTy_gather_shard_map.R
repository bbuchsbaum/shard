# Benchmark / example: view-enabled t(X_block) %*% Y_gather using a gather view.
#
# This demonstrates a searchlight-style hot path:
# - Y is shared once
# - each shard uses an indexed (gather) view over columns of Y
# - compute packs the gathered columns into scratch explicitly and uses BLAS-3
# - view materialization remains 0; packing bytes are reported separately
#
# Run from repo root:
#   R -q -f inst/bench/view_xTy_gather_shard_map.R

suppressPackageStartupMessages(library(shard))

set.seed(1)
n <- 4000L
p <- 24L
v <- 2000L

X <- matrix(rnorm(n * p), nrow = n)
Y <- matrix(rnorm(n * v), nrow = n)

colnames(X) <- paste0("x", seq_len(ncol(X)))
colnames(Y) <- paste0("y", seq_len(ncol(Y)))

Xsh <- share(X, backing = "mmap")
Ysh <- share(Y, backing = "mmap")

# 200 searchlights, each with 64 (non-contiguous) columns.
idxs <- replicate(200, sort(sample.int(ncol(Y), 64)), simplify = FALSE)
shards_g <- shards_list(idxs)

res <- shard_map(
  shards_g,
  borrow = list(X = Xsh, Y = Ysh),
  fun = function(shard, X, Y) {
    vY <- view(Y, cols = shard$idx, type = "auto")
    shard:::view_xTy(X, vY)
  },
  workers = 4,
  diagnostics = TRUE,
  chunk_size = 2
)

print(task_report(res))
print(copy_report(res))

mats <- results(res)
cat("\nNum results:", length(mats), "\n")
cat("First result dim:", paste(dim(mats[[1]]), collapse = "x"), "\n")
