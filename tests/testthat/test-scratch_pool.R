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
