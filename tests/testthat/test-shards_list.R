test_that("shards_list builds a shard_descriptor with idx vectors", {
  idxs <- list(1:3, c(2, 5, 7), integer())
  expect_error(shards_list(idxs), "non-empty", fixed = FALSE)

  idxs <- list(1:3, c(2, 5, 7))
  s <- shards_list(idxs)
  expect_true(inherits(s, "shard_descriptor"))
  expect_equal(s$num_shards, 2L)
  expect_equal(s$shards[[1]]$idx, 1:3)
  expect_equal(s$shards[[2]]$len, 3L)
})

