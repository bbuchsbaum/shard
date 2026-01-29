test_that("shards() creates correct contiguous shards", {
  desc <- shards(100, block_size = 25, strategy = "contiguous")

  expect_s3_class(desc, "shard_descriptor")
  expect_equal(desc$n, 100)
  expect_equal(desc$block_size, 25)
  expect_equal(desc$num_shards, 4)
  expect_equal(length(desc), 4)

  # Check each shard
  expect_equal(desc[[1]]$idx, 1:25)
  expect_equal(desc[[2]]$idx, 26:50)
  expect_equal(desc[[3]]$idx, 51:75)
  expect_equal(desc[[4]]$idx, 76:100)

  # All indices covered exactly once
  all_idx <- unlist(lapply(desc$shards, function(s) s$idx))
  expect_equal(sort(all_idx), 1:100)
})

test_that("shards() creates correct strided shards", {
  desc <- shards(10, block_size = 3, strategy = "strided")

  expect_s3_class(desc, "shard_descriptor")
  expect_equal(desc$strategy, "strided")

  # With block_size=3, we get ceil(10/3)=4 shards
  expect_equal(desc$num_shards, 4)

  # Check strided pattern
  expect_equal(desc[[1]]$idx, c(1, 5, 9))
  expect_equal(desc[[2]]$idx, c(2, 6, 10))
  expect_equal(desc[[3]]$idx, c(3, 7))
  expect_equal(desc[[4]]$idx, c(4, 8))

  # All indices covered exactly once
  all_idx <- unlist(lapply(desc$shards, function(s) s$idx))
  expect_equal(sort(all_idx), 1:10)
})

test_that("shards() handles uneven division", {
  desc <- shards(10, block_size = 3, strategy = "contiguous")

  expect_equal(desc$num_shards, 4)
  expect_equal(desc[[1]]$idx, 1:3)
  expect_equal(desc[[2]]$idx, 4:6)
  expect_equal(desc[[3]]$idx, 7:9)
  expect_equal(desc[[4]]$idx, 10L)  # Last shard has only 1 item

  all_idx <- unlist(lapply(desc$shards, function(s) s$idx))
  expect_equal(sort(all_idx), 1:10)
})

test_that("shards() autotuning creates reasonable block sizes", {
  desc <- shards(1000, block_size = "auto", workers = 4)

  # With 4 workers, default 4-64 shards per worker = 16-256 shards
  expect_gte(desc$num_shards, 16)
  expect_lte(desc$num_shards, 256)

  # All indices covered
  all_idx <- unlist(lapply(desc$shards, function(s) s$idx))
  expect_equal(sort(all_idx), 1:1000)
})

test_that("shards() respects memory constraints", {
  desc <- shards(
    10000,
    block_size = "auto",
    workers = 4,
    scratch_bytes_per_item = 1024,  # 1KB per item
    scratch_budget = "1MB"  # 1MB total
  )

  # Memory constraint: 1MB / 4 workers / 1KB per item = 256 items max per shard
  expect_lte(desc$block_size, 256)

  all_idx <- unlist(lapply(desc$shards, function(s) s$idx))
  expect_equal(sort(all_idx), 1:10000)
})

test_that("shards() parses count strings", {
  desc <- shards(10000, block_size = "1K")
  expect_equal(desc$block_size, 1000)

  desc <- shards(10000, block_size = "500")
  expect_equal(desc$block_size, 500)
})

test_that("shards() validates input", {
  expect_error(shards(0), "positive integer")
  expect_error(shards(-1), "positive integer")
  expect_error(shards(100, block_size = 0), "positive integer")
  expect_error(shards(100, block_size = -1), "positive integer")
})

test_that("shards() handles edge cases", {
  # Single item
  desc <- shards(1, block_size = 10)
  expect_equal(desc$num_shards, 1)
  expect_equal(desc[[1]]$idx, 1L)

  # Block size larger than n
  desc <- shards(5, block_size = 100)
  expect_equal(desc$num_shards, 1)
  expect_equal(desc[[1]]$idx, 1:5)

  # Single shard exactly
  desc <- shards(10, block_size = 10)
  expect_equal(desc$num_shards, 1)
  expect_equal(desc[[1]]$idx, 1:10)
})

test_that("shard_descriptor print method works", {
  desc <- shards(100, block_size = 25)
  expect_output(print(desc), "shard descriptor")
  expect_output(print(desc), "Items: 100")
  expect_output(print(desc), "Shards: 4")
})

test_that("shard_descriptor subsetting works", {
  desc <- shards(100, block_size = 25)

  # Single bracket returns list of shards
  subset <- desc[1:2]
  expect_type(subset, "list")
  expect_length(subset, 2)

  # Double bracket returns single shard
  shard <- desc[[1]]
  expect_type(shard, "list")
  expect_equal(shard$id, 1)
})

test_that("coverage is complete for all n and block_size combinations", {
  for (n in c(1, 2, 7, 10, 100, 1000)) {
    for (bs in c(1, 2, 3, 7, 10, 100)) {
      desc_contig <- shards(n, block_size = bs, strategy = "contiguous")
      desc_strided <- shards(n, block_size = bs, strategy = "strided")

      # Contiguous coverage
      all_idx <- unlist(lapply(desc_contig$shards, function(s) s$idx))
      expect_equal(sort(all_idx), 1:n,
        info = sprintf("Contiguous failed for n=%d, bs=%d", n, bs))

      # Strided coverage
      all_idx <- unlist(lapply(desc_strided$shards, function(s) s$idx))
      expect_equal(sort(all_idx), 1:n,
        info = sprintf("Strided failed for n=%d, bs=%d", n, bs))
    }
  }
})
