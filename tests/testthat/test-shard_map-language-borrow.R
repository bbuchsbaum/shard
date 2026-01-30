test_that("shard_map borrows language objects without evaluating them", {
  skip_on_cran()

  pool_create(n = 1, rss_limit = "2GB", rss_drift_threshold = 1)
  on.exit(pool_stop(), add = TRUE)

  ex <- quote({ i * 2 })

  res <- shard_map(
    shards = shards(1, workers = 1, block_size = 1),
    borrow = list(ex = ex),
    fun = function(shard, ex) {
      is.language(ex) && grepl("i\\s*\\*\\s*2", paste(deparse(ex), collapse = " "))
    },
    workers = 1,
    diagnostics = FALSE
  )

  expect_true(isTRUE(res$results[[1]][[1]]))
})
