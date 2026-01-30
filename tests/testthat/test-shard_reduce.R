test_that("shard_reduce combines results without gathering per-shard values", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  res <- shard_reduce(
    shards(100, block_size = 1),
    map = function(s) sum(s$idx),
    combine = function(acc, x) acc + x,
    init = 0,
    workers = 2,
    chunk_size = 10,
    diagnostics = TRUE
  )

  expect_true(succeeded(res))
  expect_equal(res$value, sum(1:100))

  # Reduction diagnostics should be present and bounded.
  rd <- res$diagnostics$reduce %||% list()
  expect_true((rd$partials %||% 0L) > 0L)
  expect_true((rd$partial_max_bytes %||% 0) > 0)
})

