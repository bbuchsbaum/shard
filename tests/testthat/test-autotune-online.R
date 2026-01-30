test_that("shard_map(N) can autotune shard block sizes online and records history", {
  skip_on_cran()

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  # Force growth decisions by making min_shard_time unrealistically high.
  res <- shard_map(
    200,
    fun = function(sh) sum(sh$idx),
    workers = 1,
    chunk_size = 1,
    diagnostics = TRUE,
    autotune = list(mode = "online", max_rounds = 2L, probe_shards_per_worker = 2L, min_shard_time = 10)
  )

  expect_true(succeeded(res))

  # We should see multiple rounds and a block_size change.
  h <- res$diagnostics$autotune$history %||% list()
  expect_true(length(h) >= 2L)
  bss <- vapply(h, function(x) x$block_size %||% NA_integer_, integer(1))
  expect_true(length(unique(bss[!is.na(bss)])) >= 2L)

  # Results are correct for a plausible tuning path (50 -> 100).
  r <- results(res, flatten = FALSE)
  expect_equal(unname(unlist(r)), c(sum(1:50), sum(51:100), sum(101:200)))
})
