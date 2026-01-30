test_that("shard_map(N) can run in shm_queue mode for out-buffer workflows", {
  skip_on_cran()

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
  expect_true(all(vapply(results(res), is.null, logical(1))))

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
