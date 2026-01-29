test_that("shard_map() executes simple function", {
  skip_on_cran()

  # Simple computation
  blocks <- shards(100, block_size = 25, workers = 2)

  result <- shard_map(blocks, function(shard) {
    sum(shard$idx)
  }, workers = 2)

  expect_s3_class(result, "shard_result")
  expect_true(succeeded(result))

  # Results should be correct sums
  res <- results(result)
  expect_equal(res[[1]], sum(1:25))
  expect_equal(res[[2]], sum(26:50))
  expect_equal(res[[3]], sum(51:75))
  expect_equal(res[[4]], sum(76:100))

  # Cleanup
  pool_stop()
})

test_that("shard_map() handles borrowed inputs", {
  skip_on_cran()

  # Create test data
  X <- matrix(1:100, nrow = 10, ncol = 10)

  blocks <- shards(ncol(X), block_size = 2, workers = 2)

  result <- shard_map(blocks,
    borrow = list(X = X),
    fun = function(shard, X) {
      colSums(X[, shard$idx, drop = FALSE])
    },
    workers = 2
  )

  expect_true(succeeded(result))

  # Results should be column sums
  res <- unlist(results(result))
  expect_equal(unname(res), unname(colSums(X)))

  pool_stop()
})

test_that("shard_map() accepts integer n directly", {
  skip_on_cran()

  result <- shard_map(10, function(shard) {
    length(shard$idx)
  }, workers = 2)

  expect_s3_class(result, "shard_result")
  expect_true(succeeded(result))

  pool_stop()
})

test_that("shard_map() respects chunk_size", {
  skip_on_cran()

  blocks <- shards(100, block_size = 10, workers = 2)

  # With chunk_size = 5, we dispatch 2 chunks (each with 5 shards)
  result <- shard_map(blocks,
    fun = function(shard) {
      shard$id
    },
    chunk_size = 5,
    workers = 2
  )

  expect_true(succeeded(result))
  expect_equal(result$diagnostics$chunks_dispatched, 2)

  pool_stop()
})

test_that("shard_map() collects diagnostics", {
  skip_on_cran()

  blocks <- shards(20, block_size = 5, workers = 2)

  result <- shard_map(blocks,
    fun = function(shard) {
      Sys.sleep(0.01)
      sum(shard$idx)
    },
    workers = 2,
    diagnostics = TRUE
  )

  expect_true(succeeded(result))
  expect_false(is.null(result$diagnostics))
  expect_gt(result$diagnostics$duration, 0)
  expect_equal(result$diagnostics$shards_processed, 4)

  pool_stop()
})

test_that("shard_map() uses existing pool", {
  skip_on_cran()

  # Create pool explicitly
  pool_create(n = 2)

  blocks <- shards(10, block_size = 5)

  result <- shard_map(blocks,
    fun = function(shard) {
      sum(shard$idx)
    }
    # Not specifying workers - should use existing pool
  )

  expect_true(succeeded(result))
  expect_equal(pool_get()$n, 2)

  pool_stop()
})

test_that("shard_map() handles profile settings", {
  skip_on_cran()

  blocks <- shards(10, block_size = 5, workers = 2)

  # Memory profile - more aggressive recycling
  result <- shard_map(blocks,
    fun = function(shard) sum(shard$idx),
    profile = "memory",
    workers = 2
  )

  expect_true(succeeded(result))
  expect_equal(result$profile, "memory")

  # Speed profile - less recycling
  result <- shard_map(blocks,
    fun = function(shard) sum(shard$idx),
    profile = "speed",
    workers = 2
  )

  expect_true(succeeded(result))
  expect_equal(result$profile, "speed")

  pool_stop()
})

test_that("shard_map() validates input", {
  skip_on_cran()

  # Invalid shards
  expect_error(
    shard_map("not a shard", function(x) x),
    "shard_descriptor"
  )

  # Invalid borrow
  expect_error(
    shard_map(10, function(x) x, borrow = 1:10),
    "named list"
  )

  # Unnamed borrow
  expect_error(
    shard_map(10, function(x) x, borrow = list(1:10)),
    "named"
  )

  pool_stop()
})

test_that("shard_map() print method works", {
  skip_on_cran()

  blocks <- shards(10, block_size = 5, workers = 2)
  result <- shard_map(blocks,
    fun = function(shard) sum(shard$idx),
    workers = 2
  )

  expect_output(print(result), "shard_map result")
  expect_output(print(result), "Duration")
  expect_output(print(result), "Completed")

  pool_stop()
})

test_that("results() extracts results correctly", {
  skip_on_cran()

  blocks <- shards(10, block_size = 5, workers = 2)
  result <- shard_map(blocks,
    fun = function(shard) list(sum = sum(shard$idx), len = length(shard$idx)),
    workers = 2
  )

  res <- results(result)
  expect_type(res, "list")
  expect_length(res, 2)
  expect_equal(res[[1]]$sum, sum(1:5))
  expect_equal(res[[2]]$sum, sum(6:10))

  pool_stop()
})

test_that("succeeded() reports correctly", {
  skip_on_cran()

  blocks <- shards(10, block_size = 5, workers = 2)

  # Successful run
  result <- shard_map(blocks,
    fun = function(shard) sum(shard$idx),
    workers = 2
  )
  expect_true(succeeded(result))

  pool_stop()
})

test_that("shard_map() with multiple borrowed inputs", {
  skip_on_cran()

  X <- matrix(1:20, nrow = 4)
  Y <- 1:5
  Z <- 100

  blocks <- shards(ncol(X), block_size = 2, workers = 2)

  result <- shard_map(blocks,
    borrow = list(X = X, Y = Y, Z = Z),
    fun = function(shard, X, Y, Z) {
      idx <- shard$idx
      sum(X[, idx]) + sum(Y[idx]) + Z
    },
    workers = 2
  )

  expect_true(succeeded(result))

  pool_stop()
})

test_that("shard_map() respects recycle setting", {
  skip_on_cran()

  blocks <- shards(10, block_size = 5, workers = 2)

  # Disable recycling
  result <- shard_map(blocks,
    fun = function(shard) sum(shard$idx),
    recycle = FALSE,
    workers = 2
  )

  expect_true(succeeded(result))

  # Custom recycle threshold
  result <- shard_map(blocks,
    fun = function(shard) sum(shard$idx),
    recycle = 0.25,  # 25% drift threshold
    workers = 2
  )

  expect_true(succeeded(result))

  pool_stop()
})

test_that("shard_map() handles COW policies", {
  skip_on_cran()

  blocks <- shards(10, block_size = 5, workers = 2)

  for (policy in c("deny", "audit", "allow")) {
    result <- shard_map(blocks,
      fun = function(shard) sum(shard$idx),
      cow = policy,
      workers = 2
    )

    expect_true(succeeded(result))
    expect_equal(result$cow_policy, policy)
  }

  pool_stop()
})

test_that("shard_map() handles single-element input", {
  skip_on_cran()

  # Single element with n=1
  result <- shard_map(1, function(shard) {
    sum(shard$idx)
  }, workers = 2)

  expect_s3_class(result, "shard_result")
  expect_true(succeeded(result))

  res <- results(result)
  expect_equal(res[[1]], 1)

  pool_stop()
})

test_that("shard_map() handles single shard", {
  skip_on_cran()

  # Single shard with block_size larger than n
  blocks <- shards(5, block_size = 100, workers = 2)
  expect_equal(blocks$num_shards, 1)

  result <- shard_map(blocks, function(shard) {
    sum(shard$idx)
  }, workers = 2)

  expect_true(succeeded(result))
  expect_equal(results(result)[[1]], sum(1:5))

  pool_stop()
})

test_that("shard_map() handles block_size equal to n", {
  skip_on_cran()

  # Exactly one shard
  blocks <- shards(10, block_size = 10, workers = 2)
  expect_equal(blocks$num_shards, 1)

  result <- shard_map(blocks, function(shard) {
    length(shard$idx)
  }, workers = 2)

  expect_true(succeeded(result))
  expect_equal(results(result)[[1]], 10)

  pool_stop()
})

test_that("shard_map() with borrowed input handles single element", {
  skip_on_cran()

  X <- matrix(1:10, nrow = 10, ncol = 1)

  blocks <- shards(ncol(X), block_size = 10, workers = 2)
  expect_equal(blocks$num_shards, 1)

  result <- shard_map(blocks,
    borrow = list(X = X),
    fun = function(shard, X) {
      sum(X[, shard$idx, drop = FALSE])
    },
    workers = 2
  )

  expect_true(succeeded(result))
  expect_equal(results(result)[[1]], sum(1:10))

  pool_stop()
})
