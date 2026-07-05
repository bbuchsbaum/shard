# Direct unit tests for the internal worker scratch allocator helpers.

test_that(".scratch_get_double reuses buffers on hit and validates inputs", {
  shard:::scratch_reset_diagnostics()
  b1 <- shard:::.scratch_get_double(5L, key = "d1")
  expect_length(b1, 5L)
  expect_true(is.double(b1))

  b2 <- shard:::.scratch_get_double(5L, key = "d1")  # hit
  expect_length(b2, 5L)

  d <- scratch_diagnostics()
  expect_gte(d$misses, 1L)
  expect_gte(d$hits, 1L)

  expect_error(shard:::.scratch_get_double(-1L, key = "d1"), "n must be >= 0")
  expect_error(shard:::.scratch_get_double(5L, key = ""), "key must be non-empty")
})

test_that("scratch_matrix reshapes a reused buffer of matching length", {
  shard:::scratch_reset_diagnostics()
  m1 <- scratch_matrix(10, 5, key = "reshape")
  expect_equal(dim(m1), c(10L, 5L))

  # Same total length (50) but different shape -> reuse + reshape path.
  m2 <- scratch_matrix(5, 10, key = "reshape")
  expect_equal(dim(m2), c(5L, 10L))
  expect_gte(scratch_diagnostics()$hits, 1L)
})

test_that("scratch_matrix validates dimensions", {
  expect_error(scratch_matrix(-1, 5), "nrow must be >= 0")
  expect_error(scratch_matrix(5, -1), "ncol must be >= 0")
})
