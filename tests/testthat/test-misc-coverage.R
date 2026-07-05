# Small, direct unit tests for otherwise-uncovered helpers across modules.

test_that("shared_vector infers length from the segment and validates it", {
  seg <- segment_create(80)  # room for 10 doubles
  on.exit(segment_close(seg), add = TRUE)

  v <- shared_vector(seg, type = "double")
  expect_true(is_shared_vector(v))
  expect_equal(length(v), 10L)

  expect_error(shared_vector(seg, type = "double", length = -1), "length must be non-negative")
})

test_that("as_shared and shared_reset_diagnostics validate their inputs", {
  expect_error(as_shared(list(1, 2)), "must be an atomic vector")
  expect_error(shared_reset_diagnostics(1:10), "must be a shard ALTREP vector")
})

test_that("segment_create rejects non-positive sizes and segment_read reads all", {
  expect_error(segment_create(-1), "size must be positive")

  seg <- segment_create(16)
  on.exit(segment_close(seg), add = TRUE)
  segment_write(seg, as.integer(1:4), offset = 0)
  raw_all <- segment_read(seg)  # size = NULL -> full segment
  expect_equal(length(raw_all), 16L)
})

test_that(".arena_pop warns on empty stack and on id mismatch", {
  while (shard:::.arena_depth() > 0) suppressWarnings(shard:::.arena_pop("drain"))
  expect_warning(shard:::.arena_pop("nope"), "empty stack")

  shard:::.arena_push()
  expect_warning(shard:::.arena_pop("wrong-id"), "Arena mismatch")
  expect_equal(shard:::.arena_depth(), 0L)
})

test_that("validate_adapter rejects non-list adapters", {
  expect_error(shard:::validate_adapter("not a list", "myclass"), "Adapter must be a list")
})
