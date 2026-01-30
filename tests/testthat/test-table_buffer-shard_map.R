test_that("table_buffer can be used as an out= target in shard_map", {
  skip_on_cran()

  pool_stop()
  shard:::table_reset_diagnostics()

  sch <- schema(a = int32(), b = float64())
  s <- shards(10, block_size = 2, workers = 2)
  layout <- row_layout(s, rows_per_shard = function(sh) sh$len)

  tbl <- table_buffer(sch, nrow = 10, backing = "mmap")

  res <- shard_map(
    s,
    out = list(tbl = tbl),
    fun = function(shard, tbl) {
      rows <- layout[[as.character(shard$id)]]
      table_write(tbl, rows, data.frame(
        a = as.integer(shard$idx),
        b = as.double(shard$idx) * 0.5
      ))
      NULL
    },
    workers = 2,
    diagnostics = TRUE
  )

  expect_true(succeeded(res))

  cr <- copy_report(res)
  expect_equal(cr$table_rows %||% 0L, 10L)
  expect_gt(cr$table_bytes %||% 0, 0)

  df <- table_finalize(tbl, materialize = "always")
  expect_equal(df$a, 1:10)
  expect_equal(df$b, (1:10) * 0.5)

  pool_stop()
})
