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

test_that("shard_reduce accepts integer N as shards", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Pass integer directly instead of shard_descriptor
  res <- shard_reduce(
    50L,
    map = function(s) length(s$idx),
    combine = `+`,
    init = 0L,
    workers = 2
  )

  expect_true(succeeded(res))
  expect_equal(res$value, 50L)
})

test_that("shard_reduce print method works", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  res <- shard_reduce(
    20L,
    map = function(s) 1,
    combine = `+`,
    init = 0,
    workers = 2,
    diagnostics = TRUE
  )

  out <- capture.output(print(res))
  expect_true(any(grepl("shard_reduce result", out)))
  expect_true(any(grepl("Duration:", out)))
  expect_true(any(grepl("Partials:", out)))
})

test_that("shard_reduce validates map is a function", {
  expect_error(
    shard_reduce(10, map = "not_a_function", combine = `+`, init = 0),
    "map must be a function"
  )
})

test_that("shard_reduce validates combine is a function", {
  expect_error(
    shard_reduce(10, map = function(s) 1, combine = "not_a_function", init = 0),
    "combine must be a function"
  )
})

test_that("shard_reduce validates shards argument", {
  expect_error(
    shard_reduce("invalid", map = function(s) 1, combine = `+`, init = 0),
    "shards must be a shard_descriptor or integer"
  )
})

test_that("shard_reduce works with list combine operations", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Collect unique items using list append
  res <- shard_reduce(
    shards(10, block_size = 2),
    map = function(s) list(shard_id = s$id),
    combine = function(acc, x) c(acc, list(x)),
    init = list(),
    workers = 2
  )

  expect_true(succeeded(res))
  expect_true(is.list(res$value))
  expect_length(res$value, 5)  # 5 shards from 10 items with block_size 2
})

test_that("shard_reduce with borrow passes shared data", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  M <- share(matrix(1:100, nrow = 10))

  res <- shard_reduce(
    shards(10, block_size = 1),
    map = function(s, M) sum(M[s$idx, ]),
    combine = `+`,
    init = 0,
    borrow = list(M = M),
    workers = 2
  )

  expect_true(succeeded(res))
  expect_equal(res$value, sum(M))
})

test_that("shard_reduce with out= writes to buffer", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  out_buf <- buffer("double", dim = 10, init = 0)

  res <- shard_reduce(
    shards(10, block_size = 1),
    map = function(s, out) {
      out[s$id] <- s$id * 10
      s$id
    },
    combine = `+`,
    init = 0,
    out = list(out = out_buf),
    workers = 2
  )

  expect_true(succeeded(res))
  expect_equal(res$value, sum(1:10))
  expect_equal(as.numeric(out_buf[]), (1:10) * 10)
})

test_that("shard_reduce diagnostics can be disabled", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  res <- shard_reduce(
    20L,
    map = function(s) 1,
    combine = `+`,
    init = 0,
    workers = 2,
    diagnostics = FALSE
  )

  expect_true(succeeded(res))
  expect_null(res$diagnostics)
})

test_that("shard_reduce returns proper structure", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  res <- shard_reduce(
    shards(20, block_size = 5),
    map = function(s) 1,
    combine = `+`,
    init = 0,
    workers = 2,
    profile = "default",
    cow = "deny"
  )

  expect_s3_class(res, "shard_reduce_result")
  expect_true("value" %in% names(res))
  expect_true("failures" %in% names(res))
  expect_true("shards" %in% names(res))
  expect_true("queue_status" %in% names(res))
  expect_true("pool_stats" %in% names(res))
  expect_equal(res$cow_policy, "deny")
  expect_equal(res$profile, "default")
})

test_that("shard_reduce print shows failures when present", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Force a failure
  res <- shard_reduce(
    shards(10, block_size = 1),
    map = function(s) {
      if (s$id == 5) stop("forced error")
      1
    },
    combine = `+`,
    init = 0,
    workers = 2,
    max_retries = 0
  )

  out <- capture.output(print(res))
  expect_true(any(grepl("Failed chunks:", out)))
})

