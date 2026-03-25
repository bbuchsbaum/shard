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

test_that("scheduler throttle clears huge in-flight state after timeouts", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    pool_stop()
  }, add = TRUE)
  setTimeLimit(cpu = Inf, elapsed = 5, transient = TRUE)

  pool_create(n = 1)
  chunks <- list(
    list(id = 1L, footprint_class = "huge"),
    list(id = 2L, footprint_class = "huge")
  )

  res <- suppressWarnings(
    dispatch_chunks(
      chunks,
      fun = function(chunk) {
        Sys.sleep(0.2)
        chunk$id
      },
      timeout = 0.05,
      max_retries = 0L,
      health_check_interval = 1L,
      scheduler_policy = list(max_huge_concurrency = 1L)
    )
  )

  expect_equal(res$queue_status$completed, 0L)
  expect_equal(res$queue_status$failed, 2L)
})
