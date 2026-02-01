test_that("tiled crossprod writes into an output buffer without view materialization", {
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

  expect_true("crossprod_tile" %in% list_kernels())

  res <- shard_map(
    tiles,
    borrow = list(X = Xsh, Y = Ysh),
    out = list(Z = Z),
    kernel = "crossprod_tile",
    workers = 2,
    diagnostics = TRUE
  )

  expect_true(succeeded(res))

  cr <- copy_report(res)
  # This kernel writes tiles directly into the output buffer; it should not
  # materialize any view.
  expect_equal(cr$view_materialized %||% 0L, 0L)
  expect_equal(cr$view_materialized_bytes %||% 0, 0)

  # Each output element is written exactly once across tiles.
  expect_equal(cr$buffer_bytes %||% 0, as.double(p * v * 8L))

  # Result gathering stays tiny (NULLs only).
  expect_true(all(vapply(results(res), is.null, logical(1))))

  out <- as.matrix(Z)
  # Buffers do not currently preserve dimnames; compare values only.
  expect_equal(unname(out), unname(crossprod(X, Y)), tolerance = 1e-10)

  pool_stop()
})

test_that("tiled crossprod re-exports output buffers between calls", {
  skip_on_cran()

  pool_stop()

  set.seed(1)
  n <- 200L
  p <- 12L
  v <- 18L

  X <- matrix(rnorm(n * p), nrow = n)
  Y <- matrix(rnorm(n * v), nrow = n)

  Xsh <- share(X, backing = "mmap")
  Ysh <- share(Y, backing = "mmap")

  tiles <- shard:::tiles2d(n_x = p, n_y = v, block_x = 4L, block_y = 6L)
  expected <- unname(crossprod(X, Y))

  Z1 <- buffer("double", dim = c(p, v), init = 0, backing = "mmap")
  Z2 <- buffer("double", dim = c(p, v), init = 0, backing = "mmap")

  expect_false(identical(buffer_info(Z1)$path, buffer_info(Z2)$path))

  res1 <- shard_map(
    tiles,
    borrow = list(X = Xsh, Y = Ysh),
    out = list(Z = Z1),
    kernel = "crossprod_tile",
    workers = 2,
    diagnostics = FALSE
  )
  expect_true(succeeded(res1))
  expect_equal(unname(as.matrix(Z1)), expected, tolerance = 1e-10)

  # Critical: do NOT pool_stop() here. This ensures workers must switch to the
  # new output buffer descriptor rather than reusing cached file handles.
  res2 <- shard_map(
    tiles,
    borrow = list(X = Xsh, Y = Ysh),
    out = list(Z = Z2),
    kernel = "crossprod_tile",
    workers = 2,
    diagnostics = FALSE
  )
  expect_true(succeeded(res2))
  expect_equal(unname(as.matrix(Z2)), expected, tolerance = 1e-10)

  pool_stop()
})
