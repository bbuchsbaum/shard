test_that("dispatch_opts$auto_table streams data.frame returns into a table sink", {
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  n <- 100L
  blocks <- shards(n, workers = 2)

  res <- shard_map(
    blocks,
    fun = function(sh) {
      data.frame(i = sh$idx, v = sh$idx * 2L)
    },
    workers = 2,
    chunk_size = 1L,
    autotune = FALSE,
    dispatch_mode = "rpc_chunked",
    dispatch_opts = list(auto_table = TRUE, auto_table_materialize = "never"),
    diagnostics = TRUE
  )

  expect_true(succeeded(res))
  handle <- results(res)
  expect_true(inherits(handle, "shard_row_groups"))

  expect_equal(stream_count(handle), n)

  df <- collect(handle)
  expect_equal(nrow(df), n)
  expect_equal(sort(df$i), 1:n)
  expect_equal(df$v[order(df$i)], (1:n) * 2L)
})

test_that("auto_table requires out= to be empty", {
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  n <- 10L
  blocks <- shards(n, workers = 2)
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")

  expect_error(
    shard_map(
      blocks,
      out = list(out = out),
      fun = function(sh, out) {
        out[sh$idx] <- sh$idx
        data.frame(i = sh$idx)
      },
      workers = 2,
      chunk_size = 1L,
      autotune = FALSE,
      dispatch_mode = "rpc_chunked",
      dispatch_opts = list(auto_table = TRUE),
      diagnostics = TRUE
    ),
    "requires out="
  )
})

