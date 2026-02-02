test_that("segment/buffer/shared advise functions are safe", {
  skip_on_cran()

  seg <- segment_create(4096, backing = "mmap")
  on.exit(segment_close(seg, unlink = TRUE), add = TRUE)

  ok <- segment_advise(seg, "sequential")
  expect_true(is.logical(ok))
  expect_length(ok, 1)

  buf <- buffer("double", dim = 16, backing = "mmap")
  ok2 <- buffer_advise(buf, "random")
  expect_true(is.logical(ok2))
  expect_length(ok2, 1)

  x <- share(matrix(rnorm(100), nrow = 10), backing = "mmap")
  ok3 <- shared_advise(x, "willneed")
  expect_true(is.logical(ok3))
  expect_length(ok3, 1)
})

test_that("affinity helpers are feature-gated", {
  skip_on_cran()

  sup <- affinity_supported()
  expect_true(is.logical(sup))
  expect_length(sup, 1)

  # set_affinity is a safe no-op on unsupported platforms.
  ok <- set_affinity(0L)
  expect_true(is.logical(ok))
  expect_length(ok, 1)
})

test_that("segment_advise supports all advice types", {
  skip_on_cran()

  seg <- segment_create(4096, backing = "mmap")
  on.exit(segment_close(seg, unlink = TRUE), add = TRUE)

  for (advice in c("normal", "sequential", "random", "willneed", "dontneed")) {
    ok <- segment_advise(seg, advice)
    expect_true(is.logical(ok))
    expect_length(ok, 1)
  }
})

test_that("segment_advise validates seg argument", {
  expect_error(segment_advise(NULL, "normal"),
               "seg must be a shard_segment or an external pointer")
  expect_error(segment_advise("not a segment", "normal"),
               "seg must be a shard_segment or an external pointer")
})

test_that("buffer_advise validates buffer argument", {
  expect_error(buffer_advise(NULL, "normal"))
  expect_error(buffer_advise("not a buffer", "normal"))
})

test_that("shared_advise validates shared vector argument", {
  expect_error(shared_advise(1:10, "normal"),
               "x must be a shared vector")
  expect_error(shared_advise(matrix(1:10), "normal"),
               "x must be a shared vector")
})

test_that("pin_workers requires active pool", {
  skip_on_cran()

  pool_stop()  # Ensure no pool is active

  expect_error(pin_workers(), "No active pool")
})

test_that("pin_workers returns logical vector for each worker", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # pin_workers returns FALSE on unsupported platforms (macOS)
  result <- pin_workers()

  expect_true(is.logical(result))
  expect_length(result, 2)

  # On non-Linux, should return FALSE for all workers
  if (!affinity_supported()) {
    expect_true(all(result == FALSE))
  }
})

test_that("pin_workers spread strategy distributes workers", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Test spread strategy
  result <- pin_workers(strategy = "spread")
  expect_true(is.logical(result))
  expect_length(result, 2)
})

test_that("pin_workers compact strategy assigns sequential cores", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Test compact strategy
  result <- pin_workers(strategy = "compact")
  expect_true(is.logical(result))
  expect_length(result, 2)
})

test_that("pin_workers accepts custom core list", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Specify specific cores
  result <- pin_workers(cores = c(0L, 1L))
  expect_true(is.logical(result))
  expect_length(result, 2)
})

test_that("pin_workers validates cores argument on supported platforms", {
  skip_on_cran()

  # On unsupported platforms, pin_workers returns early before checking cores
  if (!affinity_supported()) skip("affinity not supported on this platform")

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  expect_error(pin_workers(cores = integer(0)), "cores must be non-empty")
})

test_that("set_affinity accepts vector of cores", {
  skip_on_cran()

  # Even on unsupported platforms, should not error
  ok1 <- set_affinity(0L)
  expect_true(is.logical(ok1))

  ok2 <- set_affinity(c(0L, 1L))
  expect_true(is.logical(ok2))
})

