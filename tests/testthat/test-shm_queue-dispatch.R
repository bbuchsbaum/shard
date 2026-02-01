test_that("shard_map(N) can run in shm_queue mode for out-buffer workflows", {
  skip_on_cran()
  if (!shard:::taskq_supported()) skip("shm_queue not supported (no atomics)")

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  n <- 100L
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")

  res <- shard_map(
    n,
    out = list(out = out),
    fun = function(sh, out) {
      out[sh$idx] <- sh$idx
      NULL
    },
    workers = 2,
    chunk_size = 1,
    dispatch_mode = "shm_queue",
    diagnostics = TRUE
  )

  expect_true(succeeded(res))
  expect_equal(res$diagnostics$dispatch_mode %||% NULL, "shm_queue")

  # Results are not gathered in shm_queue mode; expect NULL placeholders.
  rr <- results(res)
  expect_true(inherits(rr, "shard_results_placeholder"))
  expect_true(all(vapply(rr, is.null, logical(1))))

  expect_equal(as.integer(out[]), 1:n)
})

test_that("shm_queue dispatch_opts can override block_size", {
  skip_on_cran()
  if (!shard:::taskq_supported()) skip("shm_queue not supported (no atomics)")

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  n <- 32L
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")

  res <- shard_map(
    n,
    out = list(out = out),
    fun = function(sh, out) {
      out[sh$idx] <- sh$idx
      NULL
    },
    workers = 2,
    chunk_size = 1,
    dispatch_mode = "shm_queue",
    dispatch_opts = list(block_size = 1L),
    diagnostics = TRUE
  )

  expect_true(succeeded(res))
  expect_equal(res$shards$num_shards, n)
  expect_equal(as.integer(out[]), 1:n)
})

test_that("profile='speed' auto-enables shm_queue for scalar-N out-buffer workflows", {
  skip_on_cran()
  if (!shard:::taskq_supported()) skip("shm_queue not supported (no atomics)")

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  n <- 100L
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")

  res <- shard_map(
    n,
    out = list(out = out),
    fun = function(sh, out) {
      out[sh$idx] <- sh$idx
      NULL
    },
    workers = 2,
    chunk_size = 1,
    profile = "speed",
    diagnostics = TRUE
  )

  expect_true(succeeded(res))
  expect_equal(res$diagnostics$dispatch_mode %||% NULL, "shm_queue")
  expect_equal(as.integer(out[]), 1:n)
})

test_that("shard_map(shard_descriptor) can run in shm_queue mode for out-buffer workflows", {
  skip_on_cran()
  if (!shard:::taskq_supported()) skip("shm_queue not supported (no atomics)")

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  n <- 100L
  sh <- shards(n, block_size = 10L, workers = 2)
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")

  res <- shard_map(
    sh,
    out = list(out = out),
    fun = function(shard, out) {
      out[shard$idx] <- shard$idx
      NULL
    },
    workers = 2,
    chunk_size = 1,
    dispatch_mode = "shm_queue",
    diagnostics = TRUE
  )

  expect_true(succeeded(res))
  expect_equal(res$diagnostics$dispatch_mode %||% NULL, "shm_queue")
  expect_equal(as.integer(out[]), 1:n)
})

test_that("shm_queue reports retry accounting and per-task retry_count for failures", {
  skip_on_cran()
  if (!shard:::taskq_supported()) skip("shm_queue not supported (no atomics)")

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  n <- 10L
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")

  res <- shard_map(
    n,
    out = list(out = out),
    fun = function(sh, out) {
      if (identical(sh$idx, 3L)) stop("boom")
      out[sh$idx] <- sh$idx
      NULL
    },
    workers = 2,
    chunk_size = 1,
    max_retries = 1,
    dispatch_mode = "shm_queue",
    dispatch_opts = list(block_size = 1L),
    diagnostics = TRUE
  )

  expect_false(succeeded(res))
  expect_equal(res$queue_status$failed %||% NULL, 1L)
  expect_equal(res$queue_status$total_retries %||% NULL, 1L)

  # Task 3 errors twice: first schedules a retry, second marks as failed.
  expect_true("3" %in% names(res$failures))
  expect_equal(res$failures[["3"]]$id %||% NULL, 3L)
  expect_equal(res$failures[["3"]]$retry_count %||% NULL, 2L)

  # Only task 3 fails; others write through.
  expect_equal(as.integer(out[])[-3], (1:n)[-3])
  expect_equal(as.integer(out[])[3], 0L)
})

test_that("shm_queue can write bounded per-worker error logs when enabled", {
  skip_on_cran()
  if (!shard:::taskq_supported()) skip("shm_queue not supported (no atomics)")

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  n <- 10L
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")

  res <- shard_map(
    n,
    out = list(out = out),
    fun = function(sh, out) {
      if (identical(sh$idx, 3L)) stop("boom")
      out[sh$idx] <- sh$idx
      NULL
    },
    workers = 2,
    chunk_size = 1,
    max_retries = 1,
    dispatch_mode = "shm_queue",
    dispatch_opts = list(block_size = 1L, error_log = TRUE, error_log_max_lines = 10L),
    diagnostics = TRUE
  )

  expect_false(succeeded(res))
  logs <- res$diagnostics$error_logs %||% list()
  expect_true(length(logs) >= 1L)

  paths <- vapply(logs, function(x) x$path %||% NA_character_, character(1))
  expect_true(all(file.exists(paths)))

  lines <- unlist(lapply(paths, function(p) readLines(p, warn = FALSE)), use.names = FALSE)
  expect_true(any(grepl("^3\\tboom$", lines)))
})
