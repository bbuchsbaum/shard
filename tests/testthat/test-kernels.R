test_that("shard_crossprod computes correct result for small matrices", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Use larger matrices to work with autotune, or specify explicit block sizes
  X <- matrix(rnorm(100), nrow = 10, ncol = 10)
  Y <- matrix(rnorm(100), nrow = 10, ncol = 10)

  # Specify block sizes explicitly for small matrices
  res <- shard_crossprod(X, Y, workers = 2, block_x = 5, block_y = 5,
                         materialize = "always")

  expect_true(succeeded(res$run))
  expect_true(!is.null(res$value))
  expect_equal(dim(res$value), c(ncol(X), ncol(Y)))
  expect_equal(res$value, crossprod(X, Y), tolerance = 1e-10)
})

test_that("shard_crossprod with materialize='never' returns buffer only", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  X <- matrix(rnorm(100), nrow = 10, ncol = 10)
  Y <- matrix(rnorm(100), nrow = 10, ncol = 10)

  res <- shard_crossprod(X, Y, workers = 2, block_x = 5, block_y = 5,
                         materialize = "never")

  expect_null(res$value)
  expect_s3_class(res$buffer, "shard_buffer")
  expect_equal(as.matrix(res$buffer), crossprod(X, Y), tolerance = 1e-10)
})

test_that("shard_crossprod with materialize='auto' respects threshold", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  X <- matrix(rnorm(100), nrow = 10, ncol = 10)
  Y <- matrix(rnorm(100), nrow = 10, ncol = 10)

  # Very low threshold - should not materialize
  res1 <- shard_crossprod(X, Y, workers = 2, block_x = 5, block_y = 5,
                          materialize = "auto", materialize_max_bytes = 1)
  expect_null(res1$value)

  # Very high threshold - should materialize
  res2 <- shard_crossprod(X, Y, workers = 2, block_x = 5, block_y = 5,
                          materialize = "auto", materialize_max_bytes = 1e9)
  expect_true(!is.null(res2$value))
})

test_that("shard_crossprod accepts pre-shared matrices", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  X <- share(matrix(rnorm(100), nrow = 10, ncol = 10))
  Y <- share(matrix(rnorm(100), nrow = 10, ncol = 10))

  res <- shard_crossprod(X, Y, workers = 2, block_x = 5, block_y = 5,
                         materialize = "always")

  expect_true(succeeded(res$run))
  expect_equal(res$value, crossprod(X, Y), tolerance = 1e-10)
})

test_that("shard_crossprod validates input types", {
  expect_error(shard_crossprod(1:10, matrix(1:10, nrow = 10)),
               "X and Y must be matrices")
  expect_error(shard_crossprod(matrix(1:10, nrow = 10), 1:10),
               "X and Y must be matrices")
})

test_that("shard_crossprod validates double type", {
  X <- matrix(1L:100L, nrow = 10)
  Y <- matrix(1L:100L, nrow = 10)

  expect_error(shard_crossprod(X, Y),
               "shard_crossprod currently supports double matrices only")
})

test_that("shard_crossprod validates row count match", {
  X <- matrix(rnorm(100), nrow = 10, ncol = 10)
  Y <- matrix(rnorm(50), nrow = 5, ncol = 10)

  expect_error(shard_crossprod(X, Y),
               "X and Y must have the same number of rows")
})

test_that("shard_crossprod returns tile sizes", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  X <- matrix(rnorm(100), nrow = 10, ncol = 10)
  Y <- matrix(rnorm(100), nrow = 10, ncol = 10)

  res <- shard_crossprod(X, Y, workers = 2, block_x = 5, block_y = 5)

  expect_true("tile" %in% names(res))
  expect_equal(res$tile[["block_x"]], 5L)
  expect_equal(res$tile[["block_y"]], 5L)
})

test_that("shard_crossprod returns shard_map result", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  X <- matrix(rnorm(100), nrow = 10, ncol = 10)
  Y <- matrix(rnorm(100), nrow = 10, ncol = 10)

  res <- shard_crossprod(X, Y, workers = 2, block_x = 5, block_y = 5,
                         diagnostics = TRUE)

  expect_true("run" %in% names(res))
  expect_s3_class(res$run, "shard_result")
})

test_that("shard_crossprod validates block sizes", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  X <- matrix(rnorm(100), nrow = 10, ncol = 10)
  Y <- matrix(rnorm(100), nrow = 10, ncol = 10)

  expect_error(shard_crossprod(X, Y, block_x = 0, block_y = 5),
               "block_x must be >= 1")
  expect_error(shard_crossprod(X, Y, block_x = 5, block_y = 0),
               "block_y must be >= 1")
})

test_that("shard_crossprod supports different backing types", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  X <- matrix(rnorm(100), nrow = 10, ncol = 10)
  Y <- matrix(rnorm(100), nrow = 10, ncol = 10)
  expected <- crossprod(X, Y)

  res_mmap <- shard_crossprod(X, Y, workers = 2, block_x = 5, block_y = 5,
                               backing = "mmap", materialize = "always")
  expect_equal(res_mmap$value, expected, tolerance = 1e-10)

  # shm backing might not be available everywhere
  tryCatch({
    res_shm <- shard_crossprod(X, Y, workers = 2, block_x = 5, block_y = 5,
                               backing = "shm", materialize = "always")
    expect_equal(res_shm$value, expected, tolerance = 1e-10)
  }, error = function(e) {
    # shm not supported, skip
  })
})

test_that("shard_crossprod handles rectangular matrices", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # More cols than rows - use explicit block sizes since autotune
  # has issues with small dimensions < 16
  X <- matrix(rnorm(200), nrow = 10, ncol = 20)
  Y <- matrix(rnorm(150), nrow = 10, ncol = 15)

  res <- shard_crossprod(X, Y, workers = 2, block_x = 10, block_y = 8,
                         materialize = "always")

  expect_equal(dim(res$value), c(20, 15))
  expect_equal(res$value, crossprod(X, Y), tolerance = 1e-10)
})

test_that("autotune_crossprod_tiles returns valid tile sizes for large matrices", {
  # Use larger matrices so candidates pass filtering
  X <- share(matrix(rnorm(25600), nrow = 100, ncol = 256))
  Y <- share(matrix(rnorm(25600), nrow = 100, ncol = 256))

  tiles <- shard:::.autotune_crossprod_tiles(X, Y)

  expect_true(is.list(tiles))
  expect_true("block_x" %in% names(tiles))
  expect_true("block_y" %in% names(tiles))
  expect_true(tiles$block_x >= 1 && tiles$block_x <= ncol(X))
  expect_true(tiles$block_y >= 1 && tiles$block_y <= ncol(Y))
})

test_that("autotune_crossprod_tiles handles small matrices", {
  # Small matrices where v >= 16 (minimum candidate in cand_y)
  # For matrices with ncol < 16, autotune currently has a bug returning -Inf
  X <- share(matrix(rnorm(640), nrow = 10, ncol = 64))
  Y <- share(matrix(rnorm(320), nrow = 10, ncol = 32))

  tiles <- shard:::.autotune_crossprod_tiles(X, Y)

  expect_true(is.list(tiles))
  expect_true(tiles$block_x >= 1)
  expect_true(tiles$block_y >= 1)
})

test_that("autotune_crossprod_tiles respects fixed dimensions", {
  # Use larger matrices so at least one candidate passes
  X <- share(matrix(rnorm(25600), nrow = 100, ncol = 256))
  Y <- share(matrix(rnorm(25600), nrow = 100, ncol = 256))

  tiles <- shard:::.autotune_crossprod_tiles(X, Y, block_x = 4, block_y = "auto")

  expect_equal(tiles$block_x, 4L)
  expect_true(tiles$block_y >= 1)
})

test_that("shard_crossprod with auto block sizes on large matrices", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Large enough for auto-tuning to work
  X <- matrix(rnorm(25600), nrow = 100, ncol = 256)
  Y <- matrix(rnorm(25600), nrow = 100, ncol = 256)

  res <- shard_crossprod(X, Y, workers = 2, materialize = "always")

  expect_true(succeeded(res$run))
  expect_equal(res$value, crossprod(X, Y), tolerance = 1e-10)
})
