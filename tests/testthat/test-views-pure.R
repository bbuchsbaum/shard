# Unit tests for pure/`.Call`-backed view helpers that do not require a worker
# pool. These exercise the validation, construction, introspection, printing,
# materialization, and BLAS-backed reduction paths in R/views.R.

test_that("elem_size_bytes maps typeof to byte widths", {
  expect_equal(shard:::elem_size_bytes(1L), 4L)
  expect_equal(shard:::elem_size_bytes(1.0), 8L)
  expect_equal(shard:::elem_size_bytes(TRUE), 4L)
  expect_equal(shard:::elem_size_bytes(as.raw(1)), 1L)
  expect_true(is.na(shard:::elem_size_bytes("a")))
})

test_that("idx_range validates arguments and prints", {
  expect_error(idx_range(NA, 5), "start must be a single")
  expect_error(idx_range(1, NA), "end must be a single")
  expect_error(idx_range(c(1, 2), 5), "start must be a single")
  expect_error(idx_range(0, 5), "start must be >= 1")
  expect_error(idx_range(5, 2), "end must be >= start")

  r <- idx_range(2, 6)
  expect_s3_class(r, "shard_idx_range")
  expect_output(print(r), "idx_range \\[2:6\\]")
  expect_equal(shard:::idx_range_print(NULL), "(all)")
})

test_that("idx_range_resolve and idx_range_len handle NULL and bounds", {
  expect_null(shard:::idx_range_resolve(NULL, 10))
  expect_error(shard:::idx_range_resolve(5, 10), "must be NULL or idx_range")
  expect_error(shard:::idx_range_resolve(idx_range(1, 20), 10), "end exceeds dimension")
  expect_equal(shard:::idx_range_resolve(idx_range(2, 4), 10), 2:4)

  expect_equal(shard:::idx_range_len(NULL, 7L), 7L)
  expect_true(is.na(shard:::idx_range_len(42, 7L)))
  expect_equal(shard:::idx_range_len(idx_range(3, 8), 10L), 6L)
})

test_that("idx_vec_validate rejects bad indices", {
  expect_error(shard:::idx_vec_validate("x", 10), "non-empty numeric")
  expect_error(shard:::idx_vec_validate(c(1L, NA_integer_), 10), "must not contain NA")
  expect_error(shard:::idx_vec_validate(c(1L, 11L), 10), "out-of-range")
  expect_equal(shard:::idx_vec_validate(c(2, 4, 6), 10), c(2L, 4L, 6L))
})

test_that("idx_print describes ranges, gathers, and unknowns", {
  expect_equal(shard:::idx_print(NULL), "(all)")
  expect_equal(shard:::idx_print(idx_range(1, 3)), "[1:3]")
  expect_equal(shard:::idx_print(c(1L, 5L, 9L)), "[gather n=3]")
  expect_equal(shard:::idx_print(1.5), "(unknown)")
})

test_that("view_layout classifies selector combinations", {
  expect_equal(shard:::view_layout(c(4L, 5L), NULL, NULL), "full")
  expect_equal(shard:::view_layout(c(4L, 5L), NULL, idx_range(1, 2)), "col_block_contiguous")
  expect_equal(shard:::view_layout(c(4L, 5L), idx_range(1, 2), NULL), "row_block_strided")
  expect_equal(shard:::view_layout(c(4L, 5L), idx_range(1, 2), idx_range(1, 2)), "submatrix_strided")
  expect_equal(shard:::view_layout(c(4L, 5L), NULL, c(1L, 3L)), "col_gather")
  expect_equal(shard:::view_layout(c(2L, 2L, 2L), NULL, NULL), "unsupported")
})

test_that("view_validate_dims rejects non-shared and non-matrix inputs", {
  expect_error(view_block(matrix(1:6, 2), cols = idx_range(1, 1)),
               "must be a shared")
  m <- share(matrix(as.double(1:20), nrow = 4))
  on.exit(close(m), add = TRUE)
  expect_silent(shard:::view_validate_dims(m))
})

test_that("view() auto-selects block vs gather and validates", {
  m <- share(matrix(as.double(1:20), nrow = 4))
  on.exit(close(m), add = TRUE)

  vb <- view(m, cols = idx_range(1, 2))
  expect_s3_class(vb, "shard_view_block")

  vg <- view(m, cols = c(1L, 3L))
  expect_s3_class(vg, "shard_view_gather")

  vg2 <- view(m, cols = c(1L, 3L), type = "gather")
  expect_s3_class(vg2, "shard_view_gather")

  expect_error(view(m, type = "gather"), "gather view requires cols")
  expect_error(view(m, rows = c(1L, 2L)), "Unsupported selectors")
})

test_that("view_block_build rejects out-of-range and gather selectors", {
  m <- share(matrix(as.double(1:20), nrow = 4))
  on.exit(close(m), add = TRUE)
  expect_error(view_block(m, rows = idx_range(1, 99)), "rows end exceeds nrow")
  expect_error(view_block(m, cols = idx_range(1, 99)), "cols end exceeds ncol")
  expect_error(view_block(m, cols = c(1L, 2L)), "must be NULL or idx_range")
})

test_that("view_gather validates rows and cols", {
  m <- share(matrix(as.double(1:20), nrow = 4))
  on.exit(close(m), add = TRUE)
  expect_error(view_gather(m, rows = c(1L, 2L), cols = c(1L, 2L)), "rows must be NULL or idx_range")
  expect_error(view_gather(m, rows = idx_range(1, 99), cols = c(1L, 2L)), "rows end exceeds nrow")
  vg <- view_gather(m, cols = c(1L, 3L))
  expect_equal(vg$slice_dim, c(4L, 2L))
})

test_that("view_info and print methods report metadata", {
  m <- share(matrix(as.double(1:20), nrow = 4))
  on.exit(close(m), add = TRUE)
  vb <- view_block(m, cols = idx_range(1, 2))
  info <- view_info(vb)
  expect_equal(info$layout, "col_block_contiguous")
  expect_true(info$fast_path)
  expect_error(view_info(list()), "must be a shard view")

  expect_output(print(vb), "shard_view_block")
  vg <- view_gather(m, cols = c(1L, 3L))
  expect_output(print(vg), "shard_view_gather")
})

test_that("materialize on block and gather views returns correct submatrices", {
  ref <- matrix(as.double(1:20), nrow = 4)
  m <- share(ref)
  on.exit(close(m), add = TRUE)

  vb <- view_block(m, rows = idx_range(2, 3), cols = idx_range(1, 2))
  expect_equal(materialize(vb), ref[2:3, 1:2, drop = FALSE])

  vb_full <- view_block(m)
  expect_equal(materialize(vb_full), ref)

  vg <- view_gather(m, cols = c(1L, 3L))
  expect_equal(materialize(vg), ref[, c(1L, 3L), drop = FALSE])
})

test_that("view_col_sums and view_col_vars match base R", {
  ref <- matrix(as.double(1:20), nrow = 4)
  m <- share(ref)
  on.exit(close(m), add = TRUE)

  vb <- view_block(m, cols = idx_range(2, 4))
  expect_equal(view_col_sums(vb), colSums(ref[, 2:4, drop = FALSE]))
  expect_equal(view_col_vars(vb), apply(ref[, 2:4, drop = FALSE], 2, var))

  expect_error(view_col_sums(list()), "must be a shard_view_block")
  expect_error(view_col_vars(list()), "must be a shard_view_block")
})

test_that("view_col_sums/vars preserve column dimnames", {
  ref <- matrix(as.double(1:20), nrow = 4,
                dimnames = list(NULL, paste0("c", 1:5)))
  m <- share(ref)
  on.exit(close(m), add = TRUE)
  vb <- view_block(m, cols = idx_range(2, 3))
  expect_equal(names(view_col_sums(vb)), c("c2", "c3"))
})

test_that("view_xTy computes crossproducts for block and gather views", {
  X <- matrix(as.double(1:12), nrow = 4)  # 4x3
  Yref <- matrix(as.double(1:20), nrow = 4)  # 4x5
  Y <- share(Yref)
  on.exit(close(Y), add = TRUE)

  yb <- view_block(Y, cols = idx_range(1, 2))
  expect_equal(view_xTy(X, yb), crossprod(X, Yref[, 1:2, drop = FALSE]))

  yg <- view_gather(Y, cols = c(1L, 4L))
  expect_equal(view_xTy(X, yg), crossprod(X, Yref[, c(1L, 4L), drop = FALSE]))

  expect_error(view_xTy(X, list()), "y_view must be a shard view")
  expect_error(view_xTy(1:4, yb), "x must be a matrix")
})

test_that("view_crossprod computes t(x) %*% y over block views", {
  Xref <- matrix(as.double(1:20), nrow = 4)
  Yref <- matrix(as.double(21:40), nrow = 4)
  X <- share(Xref); Y <- share(Yref)
  on.exit({ close(X); close(Y) }, add = TRUE)

  xb <- view_block(X, cols = idx_range(1, 2))
  yb <- view_block(Y, cols = idx_range(3, 5))
  expect_equal(view_crossprod(xb, yb),
               crossprod(Xref[, 1:2, drop = FALSE], Yref[, 3:5, drop = FALSE]))

  expect_error(view_crossprod(list(), yb), "x_view must be a shard_view_block")
  expect_error(view_crossprod(xb, list()), "y_view must be a shard_view_block")
})

test_that("tiles2d builds a 2D tile descriptor and prints", {
  expect_error(tiles2d(0, 5, 2, 2), "n_x must be a positive integer")
  expect_error(tiles2d(5, 0, 2, 2), "n_y must be a positive integer")
  expect_error(tiles2d(5, 5, 0, 2), "block_x must be a positive integer")
  expect_error(tiles2d(5, 5, 2, 0), "block_y must be a positive integer")

  td <- tiles2d(5, 4, 2, 2)  # 3 x 2 = 6 tiles
  expect_s3_class(td, "shard_tiles")
  expect_equal(td$num_shards, 6L)
  first <- td$shards[[1]]
  expect_equal(c(first$x_start, first$x_end, first$y_start, first$y_end), c(1L, 2L, 1L, 2L))
  expect_output(print(td), "shard tiles \\(2D\\)")
})
