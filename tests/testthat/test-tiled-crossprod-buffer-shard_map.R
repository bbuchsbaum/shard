test_that("tiled crossprod writes into an output buffer with zero view materialization", {
  skip_on_cran()

  pool_stop()
  shard:::view_reset_diagnostics()
  shard:::buffer_reset_diagnostics()

  set.seed(1)
  n <- 200L
  p <- 12L
  v <- 18L

  X <- matrix(rnorm(n * p), nrow = n)
  Y <- matrix(rnorm(n * v), nrow = n)

  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))

  Xsh <- share(X, backing = "mmap")
  Ysh <- share(Y, backing = "mmap")

  # Output is a shared buffer; each tile writes to a disjoint slice.
  Z <- buffer("double", dim = c(p, v), init = 0, backing = "mmap")

  tiles <- shard:::tiles2d(n_x = p, n_y = v, block_x = 4L, block_y = 6L)

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
    workers = 2,
    diagnostics = TRUE
  )

  expect_true(succeeded(res))

  cr <- copy_report(res)
  expect_gt(cr$view_created %||% 0L, 0L)
  expect_equal(cr$view_materialized %||% 0L, 0L)
  expect_equal(cr$view_materialized_bytes %||% 0, 0)

  # Each output element is written exactly once across tiles.
  expect_equal(cr$buffer_bytes %||% 0, as.double(p * v * 8L))

  # Result gathering stays tiny (NULLs only).
  expect_true(all(vapply(results(res), is.null, logical(1))))

  out <- as.matrix(Z)
  expect_equal(out, crossprod(X, Y), tolerance = 1e-10)

  pool_stop()
})

