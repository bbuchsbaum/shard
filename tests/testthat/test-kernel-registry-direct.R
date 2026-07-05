# Direct tests for the built-in kernel implementations, exercised without a
# worker pool by calling the registered impls on shared inputs and buffers.

# NOTE: call the internal `shard:::.kernel_*` bindings directly rather than the
# copies stored in the kernel registry. Coverage instrumentation traces the
# namespace bindings; the registry holds references captured at load time, so
# calling those would not be measured.

test_that("col_means kernel computes column means for a contiguous shard", {
  ref <- matrix(as.double(1:20), nrow = 4)  # 4 x 5
  X <- share(ref)
  on.exit(close(X), add = TRUE)
  out <- buffer("double", dim = ncol(ref))
  on.exit(buffer_close(out), add = TRUE)

  expect_false(is.null(shard:::get_kernel("col_means")))
  shard:::.kernel_col_means(list(start = 2L, end = 4L), X, out)
  expect_equal(out[2:4], colMeans(ref[, 2:4, drop = FALSE]))
})

test_that("col_means kernel validates shard descriptors", {
  X <- share(matrix(as.double(1:20), nrow = 4))
  on.exit(close(X), add = TRUE)
  out <- buffer("double", dim = 5)
  on.exit(buffer_close(out), add = TRUE)
  expect_error(shard:::.kernel_col_means(list(id = 1L), X, out), "requires contiguous shard")
  expect_error(shard:::.kernel_col_means(list(start = 3L, end = 1L), X, out), "Invalid shard start/end")
})

test_that("col_vars kernel computes sample variances for a contiguous shard", {
  ref <- matrix(as.double(1:20), nrow = 4)
  X <- share(ref)
  on.exit(close(X), add = TRUE)
  out <- buffer("double", dim = ncol(ref))
  on.exit(buffer_close(out), add = TRUE)

  shard:::.kernel_col_vars(list(start = 1L, end = 5L), X, out)
  expect_equal(out[1:5], apply(ref, 2, var))

  expect_error(shard:::.kernel_col_vars(list(id = 1L), X, out), "requires contiguous shard")
  expect_error(shard:::.kernel_col_vars(list(start = 3L, end = 1L), X, out), "Invalid shard start/end")
})

test_that("crossprod_tile kernel writes t(X) %*% Y into an output buffer", {
  Xref <- matrix(as.double(1:12), nrow = 4)  # 4 x 3
  Yref <- matrix(as.double(1:20), nrow = 4)  # 4 x 5
  X <- share(Xref); Y <- share(Yref)
  on.exit({ close(X); close(Y) }, add = TRUE)

  Z <- buffer("double", dim = c(ncol(Xref), ncol(Yref)))
  on.exit(buffer_close(Z), add = TRUE)

  tile <- list(x_start = 1L, x_end = ncol(Xref), y_start = 1L, y_end = ncol(Yref))
  shard:::.kernel_crossprod_tile(tile, X, Y, Z)

  expect_equal(as.matrix(Z), crossprod(Xref, Yref))

  # Footprint hint classifies tiny tiles.
  fp <- shard:::get_kernel("crossprod_tile")$footprint(tile)
  expect_true(fp$class %in% c("tiny", "medium", "huge"))
  expect_gt(fp$bytes, 0)
})

test_that("register_kernel validates its arguments", {
  expect_error(register_kernel("", function(s) NULL), "non-empty string")
  expect_error(register_kernel("k", "notfun"), "impl must be a function")
  expect_error(register_kernel("k", function(s) NULL, signature = 1), "signature must be")
  expect_error(register_kernel("k", function(s) NULL, footprint = "x"), "footprint must be")
  expect_true("crossprod_tile" %in% list_kernels())
  expect_null(shard:::get_kernel("no_such_kernel"))
})
