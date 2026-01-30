test_that("recommendations() suggests shm_queue / chunking for tiny rpc tasks", {
  skip_on_cran()

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  # Create a real shard_result, then pin diagnostics to a deterministic "tiny task"
  # scenario (avoid timing flakiness in CI).
  res <- shard_map(
    shards(10, workers = 2),
    fun = function(sh) NULL,
    workers = 2,
    chunk_size = 1,
    diagnostics = TRUE
  )

  # Force a "tiny task" shape.
  res$diagnostics$dispatch_mode <- "rpc_chunked"
  res$diagnostics$shards_processed <- 10000L
  res$diagnostics$chunks_dispatched <- 10000L
  res$diagnostics$duration <- 1.0

  recs <- recommendations(res)
  expect_true(any(grepl("shm_queue", recs, fixed = TRUE) | grepl("chunk_size", recs, fixed = TRUE)))
})

test_that("recommendations() suggests increasing block_size when shm_queue n_tasks is huge", {
  skip_on_cran()

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  res <- shard_map(
    shards(10, workers = 2),
    fun = function(sh) NULL,
    workers = 2,
    chunk_size = 1,
    diagnostics = TRUE
  )

  res$diagnostics$dispatch_mode <- "shm_queue"
  res$diagnostics$shm_queue <- list(n_tasks = 2000001L, done = 2000001L, failed = 0L)

  recs <- recommendations(res)
  expect_true(any(grepl("dispatch_opts\\$block_size", recs)))
})

