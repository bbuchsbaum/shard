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

