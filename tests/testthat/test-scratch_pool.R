test_that("scratch_matrix reuses allocations and can request worker recycle", {
  skip_on_cran()

  # Local-process behavior: reuse counters.
  shard:::scratch_reset_diagnostics()
  scratch_pool_config(max_bytes = "10MB")

  x1 <- scratch_matrix(10, 10)
  x1[] <- 1
  x2 <- scratch_matrix(10, 10)
  expect_true(is.matrix(x2))

  sd <- scratch_diagnostics()
  expect_true((sd$misses %||% 0L) >= 1L)
  expect_true((sd$hits %||% 0L) >= 1L)
  expect_false(isTRUE(sd$needs_recycle))

  # Worker recycle signaling: exceed max_bytes in a worker, then ensure the
  # worker is recycled at the next safe point.
  pool_stop()
  pool_create(1, min_recycle_interval = 0)
  on.exit(pool_stop(), add = TRUE)

  shard:::scratch_reset_diagnostics()

  # Two shards: first triggers needs_recycle, second provides a safe-point to recycle.
  res <- shard_map(shards(2, block_size = 1), fun = function(s) {
    scratch_pool_config(max_bytes = "32KB")
    if (s$id == 1L) {
      # ~320KB
      tmp <- scratch_matrix(200, 200, key = "big")
      tmp[] <- 0
    }
    s$id
  }, workers = 1, diagnostics = TRUE)

  expect_true(succeeded(res))
  expect_true((res$diagnostics$scratch_stats$high_water %||% 0) > 0)

  # Worker should have been recycled once (between shard 1 and 2).
  p <- pool_get()
  expect_true((p$workers[[1]]$recycle_count %||% 0L) >= 1L)
})

test_that("scratch_pool_config validates max_bytes", {
  expect_error(scratch_pool_config(max_bytes = 0), "max_bytes must be positive")
  expect_error(scratch_pool_config(max_bytes = -1), "max_bytes must be positive")
})

test_that("scratch_pool_config accepts string bytes", {
  expect_no_error(scratch_pool_config(max_bytes = "1GB"))
  expect_no_error(scratch_pool_config(max_bytes = "512MB"))
})

test_that("scratch_diagnostics returns complete structure", {
  shard:::scratch_reset_diagnostics()

  sd <- scratch_diagnostics()

  expect_true(is.list(sd))
  expect_true("hits" %in% names(sd))
  expect_true("misses" %in% names(sd))
  expect_true("bytes" %in% names(sd))
  expect_true("high_water" %in% names(sd))
  expect_true("max_bytes" %in% names(sd))
  expect_true("needs_recycle" %in% names(sd))
})

test_that("scratch_matrix validates dimensions", {
  expect_error(scratch_matrix(-1, 10), "nrow must be >= 0")
  expect_error(scratch_matrix(10, -1), "ncol must be >= 0")
})

test_that("scratch_matrix uses custom key when provided", {
  shard:::scratch_reset_diagnostics()

  # First allocation with custom key
  x1 <- scratch_matrix(5, 5, key = "custom_key")
  expect_true(is.matrix(x1))
  expect_equal(dim(x1), c(5, 5))

  # Second allocation with same key should hit cache
  x2 <- scratch_matrix(5, 5, key = "custom_key")

  sd <- scratch_diagnostics()
  expect_true(sd$hits >= 1)
})

test_that("scratch_matrix creates double matrices", {
  x <- scratch_matrix(3, 4)

  expect_true(is.matrix(x))
  expect_equal(dim(x), c(3, 4))
  expect_true(is.double(x))
})

test_that(".scratch_get_double validates inputs", {
  expect_error(shard:::.scratch_get_double(-1, "key"), "n must be >= 0")
  expect_error(shard:::.scratch_get_double(10, ""), "key must be non-empty")
})

test_that("scratch_reset_diagnostics clears counters", {
  # Create some activity
  scratch_matrix(10, 10)

  # Reset
  shard:::scratch_reset_diagnostics()

  sd <- scratch_diagnostics()
  expect_equal(sd$hits, 0L)
  expect_equal(sd$misses, 0L)
  expect_equal(sd$bytes, 0)
  expect_equal(sd$high_water, 0)
  expect_false(sd$needs_recycle)
})
