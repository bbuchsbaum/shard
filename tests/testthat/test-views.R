# Tests for shard views (block views)

test_that("block views over shared matrices materialize correctly", {
  shard:::view_reset_diagnostics()
  X <- matrix(as.double(1:30), nrow = 5)
  Xsh <- share(X, backing = "mmap")

  expect_true(is_shared_vector(Xsh))
  expect_equal(dim(Xsh), dim(X))

  v <- view_block(Xsh, cols = idx_range(2, 4))
  expect_true(is_view(v))
  expect_true(is_block_view(v))

  info <- view_info(v)
  expect_equal(info$layout, "col_block_contiguous")
  expect_equal(info$slice_dim, c(5L, 3L))
  expect_true(isTRUE(info$base_is_shared))

  Xm <- materialize(v)
  expect_equal(Xm, X[, 2:4, drop = FALSE])
})

test_that("views are serializable and remain usable after unserialize", {
  shard:::view_reset_diagnostics()
  X <- matrix(as.double(1:60), nrow = 6)
  Xsh <- share(X, backing = "mmap")
  v <- view_block(Xsh, cols = idx_range(3, 7))

  raw <- serialize(v, connection = NULL, xdr = FALSE)
  v2 <- unserialize(raw)

  expect_true(is_block_view(v2))
  expect_equal(materialize(v2), X[, 3:7, drop = FALSE])
})

test_that("block views reject non-range selectors (gather not implemented)", {
  shard:::view_reset_diagnostics()
  X <- matrix(as.double(1:20), nrow = 4)
  Xsh <- share(X, backing = "mmap")

  expect_error(view_block(Xsh, cols = 1:3))
  expect_error(view(Xsh, cols = 1:3, type = "auto"))
  expect_error(view(Xsh, cols = 1:3, type = "gather"))
})

test_that("view_col_sums runs without materializing views", {
  shard:::view_reset_diagnostics()

  X <- matrix(rnorm(1000), nrow = 50)
  colnames(X) <- paste0("c", seq_len(ncol(X)))
  Xsh <- share(X, backing = "mmap")

  v <- view_block(Xsh, cols = idx_range(3, 12))
  sums <- shard:::view_col_sums(v)

  expect_equal(unname(sums), unname(colSums(X[, 3:12, drop = FALSE])))
  expect_equal(names(sums), colnames(X)[3:12])

  vd <- view_diagnostics()
  expect_equal(vd$materialized, 0L)
  expect_equal(vd$materialized_bytes, 0)
})

test_that("view_xTy uses BLAS path without materializing views", {
  shard:::view_reset_diagnostics()

  X <- matrix(rnorm(2000), nrow = 100)
  Y <- matrix(rnorm(3000), nrow = 100)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))

  Xsh <- share(X, backing = "mmap")
  Ysh <- share(Y, backing = "mmap")

  vY <- view_block(Ysh, cols = idx_range(4, 11))
  out <- shard:::view_xTy(Xsh, vY)

  expect_equal(out, crossprod(X, Y[, 4:11, drop = FALSE]))
  expect_equal(rownames(out), colnames(X))
  expect_equal(colnames(out), colnames(Y)[4:11])

  vd <- view_diagnostics()
  expect_equal(vd$materialized, 0L)
  expect_equal(vd$materialized_bytes, 0)
})

test_that("view_crossprod consumes two views without materializing", {
  shard:::view_reset_diagnostics()

  X <- matrix(rnorm(1500), nrow = 100)
  Y <- matrix(rnorm(2200), nrow = 100)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))

  Xsh <- share(X, backing = "mmap")
  Ysh <- share(Y, backing = "mmap")

  vX <- view_block(Xsh, cols = idx_range(2, 9))
  vY <- view_block(Ysh, cols = idx_range(5, 11))

  out <- shard:::view_crossprod(vX, vY)

  expect_equal(out, crossprod(X[, 2:9, drop = FALSE], Y[, 5:11, drop = FALSE]))
  expect_equal(rownames(out), colnames(X)[2:9])
  expect_equal(colnames(out), colnames(Y)[5:11])

  vd <- view_diagnostics()
  expect_equal(vd$materialized, 0L)
  expect_equal(vd$materialized_bytes, 0)
})
