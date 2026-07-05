# Local coverage for streaming aggregations over on-disk row-group/dataset
# partitions. Exercised without a worker pool by building sinks directly. Both
# the "rds" and "native" partition formats are covered so the decode branches in
# each streaming primitive run.

make_rg <- function(format) {
  s <- schema(g = factor_col(c("a", "b")), x = float64(), y = int32())
  sink <- table_sink(s, mode = "row_groups", format = format)
  table_write(sink, 1L, data.frame(
    g = factor(c("a", "b", "a"), levels = c("a", "b")),
    x = c(1, 2, 3), y = c(10L, 20L, 30L), stringsAsFactors = FALSE))
  table_write(sink, 2L, data.frame(
    g = factor(c("b", "b"), levels = c("a", "b")),
    x = c(4, 5), y = c(40L, 50L), stringsAsFactors = FALSE))
  list(rg = table_finalize(sink), path = sink$path)
}

for (fmt in c("rds", "native")) {
  test_that(paste0("streaming aggregations work over ", fmt, " partitions"), {
    h <- make_rg(fmt)
    rg <- h$rg
    on.exit(unlink(h$path, recursive = TRUE), add = TRUE)

    expect_equal(stream_count(rg), 5L)
    expect_equal(stream_sum(rg, "x"), 15)
    expect_equal(stream_sum(rg, "y"), 150)

    tk <- stream_top_k(rg, "x", k = 2L)
    expect_equal(sort(tk$x, decreasing = TRUE), c(5, 4))

    gs <- stream_group_sum(rg, "g", "x")
    expect_equal(gs$sum[gs$group == "a"], 4)   # 1 + 3
    expect_equal(gs$sum[gs$group == "b"], 11)  # 2 + 4 + 5

    gc <- stream_group_count(rg, "g")
    expect_equal(gc$n[gc$group == "a"], 2L)
    expect_equal(gc$n[gc$group == "b"], 3L)

    # stream_map / stream_reduce over partitions.
    counts <- stream_map(rg, function(chunk) nrow(chunk))
    expect_equal(sum(unlist(counts)), 5L)
    total <- stream_reduce(rg, f = function(chunk) sum(chunk$x),
                           init = 0, combine = function(a, b) a + b)
    expect_equal(total, 15)
  })
}

test_that("stream_filter writes a filtered partitioned dataset", {
  h <- make_rg("rds")
  on.exit(unlink(h$path, recursive = TRUE), add = TRUE)
  out_dir <- file.path(tempdir(), paste0("stream_filter_out_", as.integer(Sys.getpid())))
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  filtered <- stream_filter(h$rg, predicate = function(chunk) chunk$x > 2.5, path = out_dir)
  df <- collect(filtered)
  expect_true(all(df$x > 2.5))
  expect_equal(sort(df$x), c(3, 4, 5))
})

test_that("streaming primitives validate their arguments", {
  h <- make_rg("rds")
  on.exit(unlink(h$path, recursive = TRUE), add = TRUE)
  rg <- h$rg

  expect_error(stream_sum(list(), "x"), "must be a shard_row_groups")
  expect_error(stream_sum(rg, ""), "non-empty column name")
  expect_error(stream_top_k(rg, "x", k = 0), "k must be >= 1")
  expect_error(stream_top_k(rg, "nope"), "Unknown column")
  expect_error(stream_top_k(rg, "g"), "int32\\(\\) or float64\\(\\)")
  expect_error(stream_group_sum(rg, "nope", "x"), "Unknown group column")
  expect_error(stream_group_sum(rg, "g", "nope"), "Unknown value column")
  expect_error(stream_group_sum(rg, "x", "y"), "requires a factor_col")
  expect_error(stream_group_count(rg, "x"), "requires a factor_col")
  expect_error(stream_filter(list(), function(c) TRUE), "must be a shard_row_groups")
  expect_error(stream_filter(rg, "notfun"), "predicate must be a function")
  expect_error(stream_filter(rg, function(chunk) c(TRUE)), "length == nrow")
})
