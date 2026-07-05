# Additional branch coverage for the view crossproduct kernels and validators.

test_that("view_xTy honors x_cols, row ranges, and dimnames", {
  Xref <- matrix(as.double(1:12), nrow = 4,
                 dimnames = list(NULL, c("x1", "x2", "x3")))
  Yref <- matrix(as.double(1:20), nrow = 4,
                 dimnames = list(NULL, paste0("y", 1:5)))
  Y <- share(Yref)
  on.exit(close(Y), add = TRUE)

  yb <- view_block(Y, cols = idx_range(1, 2))
  res <- view_xTy(Xref, yb, x_cols = idx_range(2, 3))
  expect_equal(res, crossprod(Xref[, 2:3, drop = FALSE], Yref[, 1:2, drop = FALSE]))
  expect_equal(rownames(res), c("x2", "x3"))
  expect_equal(colnames(res), c("y1", "y2"))

  # gather view path with dimnames.
  yg <- view_gather(Y, cols = c(1L, 4L))
  resg <- view_xTy(Xref, yg)
  expect_equal(resg, crossprod(Xref, Yref[, c(1L, 4L), drop = FALSE]))
  expect_equal(colnames(resg), c("y1", "y4"))
  expect_equal(rownames(resg), c("x1", "x2", "x3"))
})

test_that("view_xTy validates dimensions, types, and selectors", {
  Xref <- matrix(as.double(1:12), nrow = 4)
  Y <- share(matrix(as.double(1:20), nrow = 4))
  on.exit(close(Y), add = TRUE)
  yb <- view_block(Y, cols = idx_range(1, 2))

  # row mismatch
  expect_error(view_xTy(matrix(as.double(1:6), nrow = 2), yb),
               "same number of rows")
  # x_cols out of range
  expect_error(view_xTy(Xref, yb, x_cols = idx_range(1, 99)), "x_cols end exceeds")
  # non-idx_range x_cols
  expect_error(view_xTy(Xref, yb, x_cols = c(1L, 2L)), "x_cols must be NULL or idx_range")
  # integer (non-double) input
  Yi <- share(matrix(1:20, nrow = 4))
  on.exit(close(Yi), add = TRUE)
  ybi <- view_block(Yi, cols = idx_range(1, 2))
  expect_error(view_xTy(matrix(1:12, nrow = 4), ybi), "supports double matrices only")
  # block view without cols
  yb_full <- view_block(Y)
  expect_error(view_xTy(Xref, yb_full), "must specify cols for block views")
})

test_that("view_crossprod requires matching row ranges and double type", {
  Xref <- matrix(as.double(1:20), nrow = 4)
  Yref <- matrix(as.double(21:40), nrow = 4)
  X <- share(Xref); Y <- share(Yref)
  on.exit({ close(X); close(Y) }, add = TRUE)

  # Matching explicit row ranges.
  xb <- view_block(X, rows = idx_range(1, 4), cols = idx_range(1, 2))
  yb <- view_block(Y, rows = idx_range(1, 4), cols = idx_range(1, 3))
  res <- view_crossprod(xb, yb)
  expect_equal(res, crossprod(Xref[, 1:2, drop = FALSE], Yref[, 1:3, drop = FALSE]))

  # Mismatched row ranges error.
  xb2 <- view_block(X, rows = idx_range(1, 2), cols = idx_range(1, 2))
  yb2 <- view_block(Y, rows = idx_range(3, 4), cols = idx_range(1, 2))
  expect_error(view_crossprod(xb2, yb2), "identical row ranges")

  # Non-double type error.
  Xi <- share(matrix(1:20, nrow = 4))
  on.exit(close(Xi), add = TRUE)
  xbi <- view_block(Xi, cols = idx_range(1, 2))
  expect_error(view_crossprod(xbi, yb), "supports double matrices only")
})
