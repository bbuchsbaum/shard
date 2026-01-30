test_that("scheduler_policy can throttle 'huge' chunks", {
  skip_on_cran()

  pool_stop()

  # A kernel that just sleeps. The footprint marks every chunk as huge so the
  # scheduler must cap concurrency when max_huge_concurrency is set.
  register_kernel(
    name = "sleep_huge",
    impl = function(sh, ...) {
      Sys.sleep(0.02)
      sh$id
    },
    footprint = function(sh) list(class = "huge", bytes = 128 * 1024^2),
    supports_views = FALSE
  )

  res <- shard_map(
    shards(8, block_size = 1),
    kernel = "sleep_huge",
    workers = 2,
    scheduler_policy = list(max_huge_concurrency = 1L),
    diagnostics = TRUE
  )

  expect_true(succeeded(res))
  expect_equal(length(results(res)), 8L)
  expect_true((res$diagnostics$scheduler$throttle_events %||% 0L) > 0L)

  pool_stop()
})

