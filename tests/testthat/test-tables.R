test_that("schema/table_buffer/table_write materialize correctly", {
  shard:::table_reset_diagnostics()

  sch <- schema(
    a = int32(),
    b = float64(),
    ok = bool(),
    g = factor_col(levels = c("x", "y"))
  )

  tbl <- table_buffer(sch, nrow = 5, backing = "mmap")
  table_write(tbl, idx_range(1, 3), data.frame(
    a = c(1L, 2L, 3L),
    b = c(1.5, 2.5, 3.5),
    ok = c(TRUE, FALSE, TRUE),
    g = factor(c("x", "y", "x"), levels = c("x", "y"))
  ))

  df <- table_finalize(tbl, materialize = "always")
  expect_equal(df$a[1:3], 1:3)
  expect_equal(df$b[1:3], c(1.5, 2.5, 3.5))
  expect_equal(df$ok[1:3], c(TRUE, FALSE, TRUE))
  expect_equal(as.character(df$g[1:3]), c("x", "y", "x"))

  td <- table_diagnostics()
  expect_equal(td$writes, 1L)
  expect_equal(td$rows, 3L)
  expect_gt(td$bytes, 0)
})

test_that("row_layout computes disjoint ranges", {
  s <- shards(10, block_size = 4, workers = 2)
  layout <- row_layout(s, rows_per_shard = function(sh) sh$len)
  expect_equal(layout[[1]]$start, 1L)
  expect_equal(layout[[1]]$end, 4L)
  expect_equal(layout[[2]]$start, 5L)
  expect_equal(layout[[3]]$end, 10L)
})
