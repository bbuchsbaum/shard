test_that("dispatch_chunks processes all chunks", {
  skip_on_cran()

  pool <- pool_create(n = 2)
  on.exit(pool_stop())

  chunks <- lapply(1:5, function(i) list(id = i, value = i * 10))

  result <- dispatch_chunks(
    chunks,
    fun = function(chunk) chunk$value + 1
  )

  expect_s3_class(result, "shard_dispatch_result")
  expect_equal(result$queue_status$completed, 5L)
  expect_equal(result$queue_status$failed, 0L)

  # Check results (ignore names)
  results <- unname(unlist(result$results))
  expect_equal(sort(results), c(11, 21, 31, 41, 51))
})

test_that("pool_lapply works like lapply", {
  skip_on_cran()

  pool <- pool_create(n = 2)
  on.exit(pool_stop())

  x <- 1:10
  result <- pool_lapply(x, function(i) i^2)

  # Compare values, ignoring names
  expect_equal(unname(result), unname(as.list(x^2)))
})

test_that("pool_sapply simplifies results", {
  skip_on_cran()

  pool <- pool_create(n = 2)
  on.exit(pool_stop())

  x <- 1:5
  result <- pool_sapply(x, function(i) i * 2)

  # Compare values, ignoring names
  expect_equal(unname(result), unname(x * 2))
})

test_that("dispatch handles worker death", {
  skip_on_cran()

  pool <- pool_create(n = 2)
  on.exit(pool_stop())

  # Create chunks, one of which will kill the worker
  chunks <- list(
    list(id = 1, action = "normal"),
    list(id = 2, action = "normal"),
    list(id = 3, action = "kill"),  # This one kills the worker
    list(id = 4, action = "normal"),
    list(id = 5, action = "normal")
  )

  result <- suppressWarnings(
    dispatch_chunks(
      chunks,
      fun = function(chunk) {
        if (chunk$action == "kill") {
          # Simulate worker death by quitting
          # In practice, we'll just throw an error to test retry
          stop("simulated_failure")
        }
        chunk$id
      },
      health_check_interval = 2
    )
  )

  # Some chunks should complete
  expect_gte(result$queue_status$completed, 2L)
})

test_that("dispatch respects max_retries", {
  skip_on_cran()

  pool <- pool_create(n = 1)
  on.exit(pool_stop())

  # Chunk that always fails
  chunks <- list(list(id = 1))

  result <- suppressWarnings(
    dispatch_chunks(
      chunks,
      fun = function(chunk) stop("always_fails"),
      max_retries = 2
    )
  )

  # Should be marked as failed after retries exhausted
  expect_equal(result$queue_status$failed, 1L)
  expect_equal(result$queue_status$completed, 0L)
  expect_gte(result$queue_status$total_retries, 2L)
})

test_that("print.shard_dispatch_result produces output", {
  skip_on_cran()

  pool <- pool_create(n = 1)
  on.exit(pool_stop())

  chunks <- list(list(id = 1, val = 42))
  result <- dispatch_chunks(chunks, fun = function(chunk) chunk$val)

  output <- capture.output(print(result))
  expect_true(any(grepl("shard dispatch result", output)))
  expect_true(any(grepl("Duration", output)))
})
