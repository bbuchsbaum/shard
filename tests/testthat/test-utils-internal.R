# Unit tests for internal utility helpers in R/utils.R.

test_that(".worker_override handles blanks, NA, and invalid values", {
  expect_null(shard:::.worker_override(NULL, "opt"))
  expect_null(shard:::.worker_override(NA_character_, "opt"))
  expect_null(shard:::.worker_override("   ", "opt"))
  expect_null(shard:::.worker_override("", "opt"))

  expect_warning(v1 <- shard:::.worker_override("abc", "opt"), "Ignoring invalid opt")
  expect_null(v1)
  expect_warning(v2 <- shard:::.worker_override(0L, "opt"), "Ignoring invalid opt")
  expect_null(v2)

  expect_equal(shard:::.worker_override("4", "opt"), 4L)
  expect_equal(shard:::.worker_override(3L, "opt"), 3L)
})

test_that("retry_with_backoff returns on first success", {
  calls <- 0L
  res <- shard:::retry_with_backoff(function() {
    calls <<- calls + 1L
    42L
  })
  expect_equal(res, 42L)
  expect_equal(calls, 1L)
})

test_that("retry_with_backoff retries then succeeds and calls on_error", {
  attempts <- 0L
  errs <- 0L
  res <- shard:::retry_with_backoff(
    function() {
      attempts <<- attempts + 1L
      if (attempts < 2L) stop("transient")
      "ok"
    },
    max_attempts = 3,
    initial_delay = 0.001,
    on_error = function(attempt, e) errs <<- errs + 1L
  )
  expect_equal(res, "ok")
  expect_equal(attempts, 2L)
  expect_equal(errs, 1L)
})

test_that("retry_with_backoff raises the final error after exhausting attempts", {
  expect_error(
    shard:::retry_with_backoff(function() stop("boom"),
                               max_attempts = 2, initial_delay = 0.001),
    "boom"
  )
})
