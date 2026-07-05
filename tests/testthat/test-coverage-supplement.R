# Supplementary pure (worker-free) tests closing small remaining coverage gaps.

test_that("view() explicit block type and matrix-like predicates", {
  m <- share(matrix(as.double(1:20), nrow = 4))
  on.exit(close(m), add = TRUE)
  vb <- view(m, cols = idx_range(1, 2), type = "block")
  expect_s3_class(vb, "shard_view_block")

  expect_false(shard:::is_shared_matrix_like(1:10))          # no dim
  expect_false(shard:::is_shared_matrix_like(matrix(1, 2, 2)))  # not shared

  # Integer selectors for both rows and cols -> unsupported layout.
  expect_equal(shard:::view_layout(c(4L, 5L), c(1L, 2L), c(1L, 2L)), "unsupported")
})

make_rg <- function(format, rows = NULL) {
  s <- schema(g = factor_col(c("a", "b")), x = float64(), y = int32())
  sink <- table_sink(s, mode = "row_groups", format = format)
  if (is.null(rows)) {
    table_write(sink, 1L, data.frame(
      g = factor(c("a", "b", "a"), levels = c("a", "b")),
      x = c(1, 2, 3), y = c(10L, 20L, 30L), stringsAsFactors = FALSE))
  } else {
    table_write(sink, 1L, rows)
  }
  list(rg = table_finalize(sink), path = sink$path)
}

test_that("stream_top_k covers na_drop=FALSE and empty-result paths", {
  for (fmt in c("rds", "native")) {
    h <- make_rg(fmt)
    on.exit(unlink(h$path, recursive = TRUE), add = TRUE)
    tk <- stream_top_k(h$rg, "x", k = 2L, na_drop = FALSE)
    expect_equal(nrow(tk), 2L)
  }

  # A partition that is entirely NA -> empty result branch.
  hna <- make_rg("rds", rows = data.frame(
    g = factor(c("a", "b"), levels = c("a", "b")),
    x = c(NA_real_, NA_real_), y = c(1L, 2L), stringsAsFactors = FALSE))
  on.exit(unlink(hna$path, recursive = TRUE), add = TRUE)
  empty <- stream_top_k(hna$rg, "x", k = 3L, na_drop = TRUE)
  expect_equal(nrow(empty), 0L)
})

test_that("stream_filter requires a schema on the handle", {
  sink <- table_sink(NULL, mode = "row_groups")  # schema-less
  on.exit(unlink(sink$path, recursive = TRUE), add = TRUE)
  table_write(sink, 1L, data.frame(a = 1:3))
  rg <- table_finalize(sink)
  expect_error(stream_filter(rg, function(chunk) rep(TRUE, nrow(chunk))),
               "must contain a schema")
})

test_that("view materialization hotspot recording is exercised when enabled", {
  old <- getOption("shard.view_hotspots", FALSE)
  options(shard.view_hotspots = TRUE)
  on.exit(options(shard.view_hotspots = old), add = TRUE)
  shard:::view_reset_diagnostics()

  m <- share(matrix(as.double(1:20), nrow = 4))
  on.exit(close(m), add = TRUE)
  vb <- view_block(m, cols = idx_range(1, 3))
  res <- materialize(vb)
  expect_equal(dim(res), c(4L, 3L))

  snap <- shard:::view_materialize_hotspots_snapshot_()
  expect_true(length(snap) >= 1L)
  entry <- snap[[1]]
  expect_true(entry$bytes > 0)
  expect_true(entry$count >= 1L)

  # Diagnostics counters reflect the materialization.
  vd <- view_diagnostics()
  expect_true(vd$materialized >= 1L)
})

test_that("table helpers cover remaining validation and warning paths", {
  # Unsupported column kind in .table_cast.
  expect_error(shard:::.table_cast(shard:::.coltype("weird"), 1), "Unsupported column type")

  # .table_write_buffer_impl guards.
  expect_error(shard:::.table_write_buffer_impl(list(), rows = idx_range(1, 1), data = list()),
               "must be a shard_table_buffer")

  # as_tibble warns when the estimated payload exceeds max_bytes.
  s <- schema(x = float64())
  tb <- table_buffer(s, nrow = 1000L)
  expect_warning(as_tibble(tb, max_bytes = 1), "Materializing a large table")
})
