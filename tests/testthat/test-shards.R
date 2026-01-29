# Tests for shards() function

test_that("shards() creates valid descriptors", {
  s <- shards(100)

  expect_s3_class(s, "shard_descriptors")
  expect_equal(attr(s, "n"), 100)
  expect_true(length(s) > 0)
})

test_that("shards() covers all indices exactly once", {
  s <- shards(100, block_size = 10)

  all_indices <- unlist(lapply(s, function(x) x$indices))
  expect_equal(sort(all_indices), 1:100)
})

test_that("shards() respects block_size", {
  s <- shards(100, block_size = 25)

  expect_equal(length(s), 4)
  expect_equal(length(s[[1]]$indices), 25)
})

test_that("shards() handles non-divisible sizes", {
  s <- shards(103, block_size = 10)

  all_indices <- unlist(lapply(s, function(x) x$indices))
  expect_equal(sort(all_indices), 1:103)
})

test_that("shards() strided strategy interleaves indices", {
  s <- shards(12, block_size = 4, strategy = "strided")

  # First shard should have 1, 4, 7, 10 (or similar pattern)
  expect_true(all(diff(s[[1]]$indices) > 1))
})

test_that("shards() validates inputs", {
  expect_error(shards(-1))
  expect_error(shards("abc"))
  expect_error(shards(100, block_size = -1))
})
