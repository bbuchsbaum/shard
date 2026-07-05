# Coverage for pin_workers(), including the pinning loop which is otherwise
# skipped on platforms where affinity_supported() is FALSE (e.g. macOS).

test_that("pin_workers errors when no pool is active", {
  pool_stop()
  expect_error(pin_workers(), "No active pool")
})

test_that("pin_workers returns all-FALSE when affinity is unsupported", {
  skip_if_conn_exhausted()
  pool_stop()
  pool_create(1)
  on.exit(pool_stop(), add = TRUE)

  testthat::local_mocked_bindings(affinity_supported = function() FALSE, .package = "shard")
  ok <- pin_workers()
  expect_length(ok, 1L)
  expect_false(any(ok))
})

test_that("pin_workers runs the pinning loop when affinity is supported", {
  skip_if_conn_exhausted()
  pool_stop()
  pool_create(1)
  on.exit(pool_stop(), add = TRUE)

  testthat::local_mocked_bindings(affinity_supported = function() TRUE, .package = "shard")

  # cores = NULL exercises the detectCores() branch; set_affinity() may return
  # FALSE on unsupported platforms, but the loop and clusterCall path still run.
  ok_spread <- pin_workers(strategy = "spread")
  expect_type(ok_spread, "logical")
  expect_length(ok_spread, 1L)

  # Explicit cores exercises the provided-cores branch and compact strategy.
  ok_compact <- pin_workers(strategy = "compact", cores = c(0L, 1L))
  expect_length(ok_compact, 1L)

  expect_error(pin_workers(cores = integer(0)), "cores must be non-empty")
})
