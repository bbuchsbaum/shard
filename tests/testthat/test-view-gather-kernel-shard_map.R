test_that("shard_map gather views use a BLAS-3 packed path with zero view materialization", {
  skip_on_cran()

  pool_stop()
  shard:::view_reset_diagnostics()

  set.seed(1)
  n <- 250L
  p <- 10L
  v <- 60L

  X <- matrix(rnorm(n * p), nrow = n)
  Y <- matrix(rnorm(n * v), nrow = n)

  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))

  Xsh <- share(X, backing = "mmap")
  Ysh <- share(Y, backing = "mmap")

  # Each shard is a "searchlight": a small, non-contiguous set of columns.
  idxs <- replicate(12, sort(sample.int(ncol(Y), 7)), simplify = FALSE)
  shards_g <- shards_list(idxs)

  res <- shard_map(
    shards_g,
    borrow = list(X = Xsh, Y = Ysh),
    fun = function(shard, X, Y) {
      vY <- view(Y, cols = shard$idx, type = "auto")
      shard:::view_xTy(X, vY)
    },
    workers = 2,
    diagnostics = TRUE
  )

  expect_true(succeeded(res))

  cr <- copy_report(res)
  expect_gt(cr$view_created %||% 0L, 0L)
  expect_equal(cr$view_materialized %||% 0L, 0L)
  expect_equal(cr$view_materialized_bytes %||% 0, 0)
  expect_gt(cr$view_packed_bytes %||% 0, 0)

  # Gather packing reuses worker-local scratch buffers.
  expect_true(is.list(res$diagnostics$scratch_stats))
  expect_gt(res$diagnostics$scratch_stats$misses %||% 0L, 0L)
  expect_gt(res$diagnostics$scratch_stats$hits %||% 0L, 0L)

  mats <- results(res)
  for (i in seq_along(mats)) {
    idx <- idxs[[i]]
    expect_equal(mats[[i]], crossprod(X, Y[, idx, drop = FALSE]), tolerance = 1e-10)
  }

  pool_stop()
})
