# Tests for shard views (block views)

test_that("block views over shared matrices materialize correctly", {
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
  X <- matrix(as.double(1:60), nrow = 6)
  Xsh <- share(X, backing = "mmap")
  v <- view_block(Xsh, cols = idx_range(3, 7))

  raw <- serialize(v, connection = NULL, xdr = FALSE)
  v2 <- unserialize(raw)

  expect_true(is_block_view(v2))
  expect_equal(materialize(v2), X[, 3:7, drop = FALSE])
})

test_that("block views reject non-range selectors (gather not implemented)", {
  X <- matrix(as.double(1:20), nrow = 4)
  Xsh <- share(X, backing = "mmap")

  expect_error(view_block(Xsh, cols = 1:3))
  expect_error(view(Xsh, cols = 1:3, type = "auto"))
  expect_error(view(Xsh, cols = 1:3, type = "gather"))
})

