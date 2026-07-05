# Edge-case coverage for shard descriptor constructors and printing.

test_that("shards() clamps invalid worker counts to 1", {
  sh <- shards(10, workers = 0, block_size = 5)
  expect_s3_class(sh, "shard_descriptor")
  expect_true(sh$num_shards >= 1L)
})

test_that("autotune_block_size clamps workers < 1", {
  bs <- shard:::autotune_block_size(100, workers = 0)
  expect_true(bs >= 1L)
})

test_that("shards_list validates its argument", {
  expect_error(shards_list(1:10), "idxs must be a list")
  expect_error(shards_list(list("a")), "must be a numeric/integer vector")
  expect_error(shards_list(list(integer(0))), "must be non-empty")
  expect_error(shards_list(list(c(1L, NA_integer_))), "must not contain NA")
  expect_error(shards_list(list(c(0L, 1L))), "must be >= 1")

  sh <- shards_list(list(1:3, 4:8))
  expect_equal(length(sh), 2L)
  expect_equal(sh$shards[[2]]$len, 5L)
})

test_that("create_contiguous_shards_window_ validates its window", {
  expect_error(shard:::create_contiguous_shards_window_(5, 2, 1), "Invalid shard window")
  expect_error(shard:::create_contiguous_shards_window_(1, 5, 0), "block_size must be >= 1")
  expect_error(shard:::create_contiguous_shards_window_(1, 5, 1, start_id = 0), "start_id must be >= 1")

  win <- shard:::create_contiguous_shards_window_(1, 10, 4)
  expect_equal(length(win), 3L)
})

test_that("print.shard_descriptor reports uniform and non-uniform sizes", {
  expect_output(print(shards(10, block_size = 5)), "uniform")
  expect_output(print(shards_list(list(1:3, 1:5))), "Shard sizes:")
})
