test_that("results_placeholder creates valid placeholder", {
  p <- shard:::.results_placeholder(100)

  expect_s3_class(p, "shard_results_placeholder")
  expect_equal(p$n, 100L)
})

test_that("results_placeholder length method works", {
  p <- shard:::.results_placeholder(50)
  expect_equal(length(p), 50L)

  p2 <- shard:::.results_placeholder(0)
  expect_equal(length(p2), 0L)

  p3 <- shard:::.results_placeholder(1000000)
  expect_equal(length(p3), 1000000L)
})

test_that("results_placeholder [[ returns NULL", {
  p <- shard:::.results_placeholder(10)

  expect_null(p[[1]])
  expect_null(p[[5]])
  expect_null(p[[10]])

  # Out of range still returns NULL (by design)
  expect_null(p[[100]])
})

test_that("results_placeholder [ returns list of NULLs", {
  p <- shard:::.results_placeholder(10)

  # Single index
  r <- p[1]
  expect_true(is.list(r))
  expect_length(r, 1)
  expect_null(r[[1]])

  # Multiple indices
  r2 <- p[c(1, 3, 5)]
  expect_true(is.list(r2))
  expect_length(r2, 3)
  expect_true(all(vapply(r2, is.null, logical(1))))

  # All indices
  r3 <- p[1:10]
  expect_length(r3, 10)
  expect_true(all(vapply(r3, is.null, logical(1))))
})

test_that("results_placeholder [ with missing index returns all NULLs", {
  p <- shard:::.results_placeholder(5)

  r <- p[]
  expect_true(is.list(r))
  expect_length(r, 5)
  expect_true(all(vapply(r, is.null, logical(1))))
})

test_that("results_placeholder print method works", {
  p <- shard:::.results_placeholder(1000)

  out <- capture.output(print(p))
  expect_true(any(grepl("<shard_results_placeholder>", out)))
  expect_true(any(grepl("count:", out)))
  expect_true(any(grepl("1,000", out)))  # formatted with comma
  expect_true(any(grepl("shm_queue mode", out)))
})

test_that("results_placeholder handles zero count", {
  p <- shard:::.results_placeholder(0)

  expect_equal(length(p), 0L)

  r <- p[]
  expect_true(is.list(r))
  expect_length(r, 0)
})

test_that("results_placeholder handles NULL count gracefully", {
  # Edge case: NULL n field
  p <- structure(list(n = NULL), class = "shard_results_placeholder")

  expect_equal(length(p), 0L)
})
