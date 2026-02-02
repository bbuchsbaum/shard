test_that("shards_lazy creates valid lazy shard descriptor", {
  x <- shard:::shards_lazy(100, block_size = 10)

  expect_s3_class(x, "shard_descriptor_lazy")
  expect_s3_class(x, "shard_descriptor")
  expect_equal(x$n, 100L)
  expect_equal(x$block_size, 10L)
  expect_equal(x$strategy, "contiguous_lazy")
  expect_equal(x$num_shards, 10L)
})

test_that("shards_lazy handles non-evenly-divisible sizes", {
  x <- shard:::shards_lazy(95, block_size = 10)

  expect_equal(x$num_shards, 10L)  # ceiling(95/10)
  expect_equal(length(x), 10L)

  # Last shard should have fewer elements
  last <- x[[10]]
  expect_equal(last$start, 91L)
  expect_equal(last$end, 95L)
  expect_equal(last$len, 5L)
})

test_that("shards_lazy length method works", {
  x <- shard:::shards_lazy(100, block_size = 25)
  expect_equal(length(x), 4L)

  x2 <- shard:::shards_lazy(1000, block_size = 1)
  expect_equal(length(x2), 1000L)
})

test_that("shards_lazy [[ indexing returns correct shard", {
  x <- shard:::shards_lazy(50, block_size = 10)

  # First shard
  s1 <- x[[1]]
  expect_equal(s1$id, 1L)
  expect_equal(s1$start, 1L)
  expect_equal(s1$end, 10L)
  expect_equal(s1$len, 10L)
  expect_equal(s1$idx, 1:10)

  # Middle shard
  s3 <- x[[3]]
  expect_equal(s3$id, 3L)
  expect_equal(s3$start, 21L)
  expect_equal(s3$end, 30L)
  expect_equal(s3$len, 10L)
  expect_equal(s3$idx, 21:30)

  # Last shard
  s5 <- x[[5]]
  expect_equal(s5$id, 5L)
  expect_equal(s5$start, 41L)
  expect_equal(s5$end, 50L)
  expect_equal(s5$len, 10L)
  expect_equal(s5$idx, 41:50)
})

test_that("shards_lazy [ indexing returns list of shards", {
  x <- shard:::shards_lazy(50, block_size = 10)

  # Single index
  s <- x[2]
  expect_true(is.list(s))
  expect_length(s, 1)
  expect_equal(s[[1]]$id, 2L)

  # Multiple indices
  ss <- x[c(1, 3, 5)]
  expect_length(ss, 3)
  expect_equal(ss[[1]]$id, 1L)
  expect_equal(ss[[2]]$id, 3L)
  expect_equal(ss[[3]]$id, 5L)
})

test_that("shards_lazy print method works", {
  x <- shard:::shards_lazy(1000000, block_size = 100)

  out <- capture.output(print(x))
  expect_true(any(grepl("shard descriptor \\(lazy\\)", out)))
  expect_true(any(grepl("Items:", out)))
  expect_true(any(grepl("Block size:", out)))
  expect_true(any(grepl("Strategy:", out)))
  expect_true(any(grepl("Shards:", out)))
})

test_that("shards_lazy validates inputs", {
  expect_error(shard:::shards_lazy(0, block_size = 10), "n must be >= 1")
  expect_error(shard:::shards_lazy(-1, block_size = 10), "n must be >= 1")
  expect_error(shard:::shards_lazy(100, block_size = 0), "block_size must be >= 1")
  expect_error(shard:::shards_lazy(100, block_size = -1), "block_size must be >= 1")
})

test_that("shards_lazy index out of range errors", {
  x <- shard:::shards_lazy(50, block_size = 10)

  expect_error(x[[0]], "shard index out of range")
  expect_error(x[[6]], "shard index out of range")
  expect_error(x[[-1]], "shard index out of range")
})

test_that("shards_lazy works with block_size = 1", {
  x <- shard:::shards_lazy(5, block_size = 1)

  expect_equal(length(x), 5L)

  for (i in 1:5) {
    s <- x[[i]]
    expect_equal(s$id, i)
    expect_equal(s$start, i)
    expect_equal(s$end, i)
    expect_equal(s$len, 1L)
    expect_equal(s$idx, i)
  }
})

test_that("shards_lazy works with single shard", {
  x <- shard:::shards_lazy(10, block_size = 100)

  expect_equal(length(x), 1L)

  s <- x[[1]]
  expect_equal(s$id, 1L)
  expect_equal(s$start, 1L)
  expect_equal(s$end, 10L)
  expect_equal(s$len, 10L)
})
