test_that("registerDoShard creates worker pool", {
  skip_on_cran()

  registerDoShard(workers = 2)
  on.exit(stopDoShard())

  pool <- getDoShardPool()
  expect_s3_class(pool, "shard_pool")
  expect_equal(pool$n, 2)
})

test_that("foreach with doShard executes in parallel", {
  skip_on_cran()

  registerDoShard(workers = 2)
  on.exit(stopDoShard())

  result <- foreach(i = 1:10, .combine = c) %dopar% {
    i * 2
  }

  expect_equal(result, 1:10 * 2)
})

test_that("foreach with .combine = rbind works", {
  skip_on_cran()

  registerDoShard(workers = 2)
  on.exit(stopDoShard())

  result <- foreach(i = 1:5, .combine = rbind) %dopar% {
    data.frame(x = i, y = i^2)
  }

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_equal(result$x, 1:5)
  expect_equal(result$y, (1:5)^2)
})

test_that("foreach with .export works", {
  skip_on_cran()

  registerDoShard(workers = 2)
  on.exit(stopDoShard())

  multiplier <- 10

  result <- foreach(i = 1:5, .combine = c, .export = "multiplier") %dopar% {
    i * multiplier
  }

  expect_equal(result, 1:5 * 10)
})

test_that("foreach with multiple iterators works", {
  skip_on_cran()

  registerDoShard(workers = 2)
  on.exit(stopDoShard())

  result <- foreach(i = 1:5, j = 5:1, .combine = c) %dopar% {
    i + j
  }

  expect_equal(result, rep(6, 5))
})

test_that("stopDoShard cleans up pool", {
  skip_on_cran()

  registerDoShard(workers = 2)
  stopDoShard()

  pool <- getDoShardPool()
  expect_null(pool)
})

test_that("error handling works", {
  skip_on_cran()

  registerDoShard(workers = 2)
  on.exit(stopDoShard())

  # .errorhandling = "stop" should propagate errors

  expect_error(
    foreach(i = 1:5, .combine = c, .errorhandling = "stop") %dopar% {
      if (i == 3) stop("test error")
      i
    },
    "test error"
  )

  # .errorhandling = "remove" should skip errors
  result <- foreach(i = 1:5, .combine = c, .errorhandling = "remove") %dopar% {
    if (i == 3) stop("test error")
    i
  }
  expect_equal(result, c(1, 2, 4, 5))
})

test_that("getDoParName returns doShard when registered", {
  skip_on_cran()

  registerDoShard(workers = 2)
  on.exit(stopDoShard())

  expect_equal(getDoParName(), "doShard")
})

test_that("getDoParWorkers returns correct count", {
  skip_on_cran()

  registerDoShard(workers = 3)
  on.exit(stopDoShard())

  expect_equal(getDoParWorkers(), 3)
})
