# Coverage for ergonomic apply/lapply wrappers: policy printing, internal
# helpers, argument validation, and small end-to-end runs.

test_that("shard_apply_policy prints its configuration", {
  p <- shard_apply_policy()
  expect_s3_class(p, "shard_apply_policy")
  expect_output(print(p), "shard_apply_policy")
  expect_output(print(p), "auto_share_min_bytes")
})

test_that("internal ergonomic helpers strip attrs and gate auto-sharing", {
  expect_null(shard:::.shard_strip_shared_attrs(NULL))

  sv <- share(1:10, readonly = FALSE)
  stripped <- shard:::.shard_strip_shared_attrs(sv)
  expect_false(inherits(stripped, "shard_shared_vector"))

  # Already-shared input is returned unchanged.
  sh <- share(1:100)
  expect_identical(shard:::.shard_auto_share_one(sh, min_bytes = 1), sh)

  # Non-atomic input is left alone.
  lst <- list(1, 2)
  expect_identical(shard:::.shard_auto_share_one(lst, min_bytes = 1), lst)

  # Below the size threshold: returned as-is.
  small <- 1:3
  expect_identical(shard:::.shard_auto_share_one(small, min_bytes = 1e9), small)

  # Above threshold: auto-shared.
  big <- as.double(1:2000)
  shared_big <- shard:::.shard_auto_share_one(big, min_bytes = 1)
  expect_true(is_shared_vector(shared_big) || is_shared(shared_big))

  # Plain value passes through fetch-if-needed.
  expect_equal(shard:::.shard_fetch_if_needed(42L), 42L)
})

test_that("shard_apply_matrix validates its arguments before running", {
  expect_error(shard_apply_matrix(matrix(1, 2, 2), FUN = 1), "FUN must be a function")
  expect_error(shard_apply_matrix(matrix(1, 2, 2), MARGIN = 1, FUN = mean),
               "Only MARGIN=2")
  expect_error(shard_apply_matrix(1:4, FUN = mean), "X must be a matrix")
  expect_error(shard_apply_matrix(matrix(1, 2, 2), FUN = mean, VARS = 1),
               "VARS must be a named list")
  expect_error(shard_apply_matrix(matrix(1, 2, 2), FUN = mean, VARS = list(1)),
               "VARS must be a named list")
})

test_that("shard_lapply_shared validates its arguments and handles empty input", {
  expect_error(shard_lapply_shared(1:3, function(x) x), "x must be a list")
  expect_error(shard_lapply_shared(list(1), FUN = 1), "FUN must be a function")
  expect_error(shard_lapply_shared(list(1), function(x) x, VARS = 1),
               "VARS must be a named list")
  expect_error(shard_lapply_shared(list(1), function(x) x, VARS = list(1)),
               "VARS must be a named list")
  expect_equal(shard_lapply_shared(list(), function(x) x), list())
})

test_that("shard_apply_matrix computes per-column results end to end", {
  skip_if_conn_exhausted()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  X <- matrix(as.double(1:20), nrow = 4)
  res <- shard_apply_matrix(X, MARGIN = 2, FUN = mean, workers = 1)
  expect_equal(res, colMeans(X))
})

test_that("shard_lapply_shared maps over a list end to end", {
  skip_if_conn_exhausted()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  res <- shard_lapply_shared(as.list(1:4), function(x) x^2, workers = 1)
  expect_equal(unlist(res), (1:4)^2)
})

test_that("shard_lapply_shared refuses oversized gathers", {
  skip_if_conn_exhausted()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  policy <- shard_apply_policy(max_gather_bytes = 1)  # 1 byte ceiling
  expect_error(
    shard_lapply_shared(as.list(1:4), function(x) rep(x, 1000), workers = 1, policy = policy),
    "Refusing to gather"
  )
})
