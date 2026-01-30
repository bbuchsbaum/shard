test_that("shard_map view-enabled xTy path has zero view materialization in hot path", {
  skip_on_cran()

  pool_stop()
  shard:::view_reset_diagnostics()

  n <- 200L
  p <- 8L
  v <- 24L
  X <- matrix(rnorm(n * p), nrow = n)
  Y <- matrix(rnorm(n * v), nrow = n)

  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))

  Xsh <- share(X, backing = "mmap")
  Ysh <- share(Y, backing = "mmap")

  blocks <- shards(ncol(Y), block_size = 6, workers = 2)

  result <- shard_map(
    blocks,
    borrow = list(X = Xsh, Y = Ysh),
    fun = function(shard, X, Y) {
      vY <- view_block(Y, cols = idx_range(shard$start, shard$end))
      shard:::view_xTy(X, vY)
    },
    workers = 2,
    diagnostics = TRUE
  )

  expect_true(succeeded(result))

  cr <- copy_report(result)
  expect_gt(cr$view_created %||% 0L, 0L)
  expect_equal(cr$view_materialized %||% 0L, 0L)
  expect_equal(cr$view_materialized_bytes %||% 0, 0)

  # Spot-check correctness by recombining blocks.
  mats <- results(result)
  out <- do.call(cbind, mats)
  expect_equal(out, crossprod(X, Y))

  pool_stop()
})

test_that("shard_map view-enabled crossprod(vX, vY) path has zero view materialization", {
  skip_on_cran()

  pool_stop()
  shard:::view_reset_diagnostics()

  n <- 300L
  p <- 16L
  v <- 40L
  X <- matrix(rnorm(n * p), nrow = n)
  Y <- matrix(rnorm(n * v), nrow = n)

  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))

  Xsh <- share(X, backing = "mmap")
  Ysh <- share(Y, backing = "mmap")

  blocks <- shards(ncol(Y), block_size = 10, workers = 2)

  res <- shard_map(
    blocks,
    borrow = list(X = Xsh, Y = Ysh),
    fun = function(shard, X, Y) {
      vX <- view_block(X, cols = idx_range(1, ncol(X)))
      vY <- view_block(Y, cols = idx_range(shard$start, shard$end))
      shard:::view_crossprod(vX, vY)
    },
    workers = 2,
    diagnostics = TRUE
  )

  expect_true(succeeded(res))

  cr <- copy_report(res)
  expect_gt(cr$view_created %||% 0L, 0L)
  expect_equal(cr$view_materialized %||% 0L, 0L)
  expect_equal(cr$view_materialized_bytes %||% 0, 0)

  mats <- results(res)
  out <- do.call(cbind, mats)
  expect_equal(out, crossprod(X, Y))

  pool_stop()
})
