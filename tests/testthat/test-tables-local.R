# Local (worker-free) coverage for schema-driven table outputs: schema/coltype
# validation, table_buffer write + materialize round trips, casting rules, row
# layout, and table_sink partition round trips (rds + native).

test_that("column type constructors and schema validation behave", {
  expect_s3_class(raw_col(), "shard_coltype")
  expect_s3_class(string_col(), "shard_coltype")
  expect_error(factor_col(character(0)), "non-empty character")
  expect_error(factor_col(c("a", NA)), "non-empty character")

  expect_error(schema(), "at least one column")
  expect_error(schema(float64()), "named columns")
  expect_error(schema(x = float64(), x = int32()), "unique")
  expect_error(schema(x = 1), "must be type specs")

  expect_equal(shard:::.coltype_bytes(raw_col()), 1)
  expect_true(is.na(shard:::.coltype_bytes(string_col())))
  expect_error(shard:::.coltype_buffer_type(string_col()), "string columns are not supported")
  expect_error(shard:::.coltype_buffer_type(structure(list(kind = "weird"), class = "shard_coltype")),
               "Unsupported column type")
})

test_that("table_buffer write + as_tibble round-trips all fixed column types", {
  s <- schema(x = float64(), y = int32(), flag = bool(),
              g = factor_col(c("a", "b", "c")), r = raw_col())
  tb <- table_buffer(s, nrow = 4L)
  expect_error(table_buffer(s, nrow = 0L), "positive integer")
  expect_error(table_buffer(list(), nrow = 4L), "must be a shard_schema")

  df <- data.frame(
    x = c(1.5, 2.5, 3.5, 4.5),
    y = 1:4,
    flag = c(TRUE, FALSE, TRUE, FALSE),
    g = factor(c("a", "b", "c", "a"), levels = c("a", "b", "c")),
    stringsAsFactors = FALSE
  )
  df$r <- as.raw(1:4)

  table_write(tb, idx_range(1, 4), df)
  out <- as_tibble(tb)
  expect_equal(as.double(out$x), df$x)
  expect_equal(as.integer(out$y), 1:4)
  expect_equal(as.logical(out$flag), df$flag)
  expect_equal(as.character(out$g), as.character(df$g))
  expect_s3_class(out$g, "factor")

  # table_finalize materialization modes.
  expect_s3_class(table_finalize(tb, materialize = "never"), "shard_table_handle")
  expect_true(is.data.frame(table_finalize(tb, materialize = "always")))
  expect_true(is.data.frame(table_finalize(tb, materialize = "auto")))
})

test_that("factor casting accepts codes/characters and rejects mismatches", {
  s <- schema(g = factor_col(c("a", "b")))
  tb <- table_buffer(s, nrow = 2L)

  # integer codes
  table_write(tb, idx_range(1, 2), list(g = c(1L, 2L)))
  expect_equal(as.character(as_tibble(tb)$g), c("a", "b"))

  # character values
  tb2 <- table_buffer(s, nrow = 2L)
  table_write(tb2, idx_range(1, 2), list(g = c("b", "a")))
  expect_equal(as.character(as_tibble(tb2)$g), c("b", "a"))

  # mismatched factor levels
  tb3 <- table_buffer(s, nrow = 1L)
  expect_error(table_write(tb3, idx_range(1, 1),
                           list(g = factor("z", levels = "z"))),
               "factor levels mismatch")
  # unknown character
  expect_error(table_write(tb3, idx_range(1, 1), list(g = "zzz")),
               "factor levels mismatch")
  # out-of-range code
  expect_error(table_write(tb3, idx_range(1, 1), list(g = 9L)),
               "Invalid factor code")
  # unsupported input type
  expect_error(shard:::.table_cast(shard:::.coltype("factor", levels = c("a", "b")), list(1)),
               "Unsupported factor column input type")
})

test_that("table_write validates columns, lengths, and row ranges", {
  s <- schema(x = float64(), y = int32())
  tb <- table_buffer(s, nrow = 5L)
  expect_error(table_write(tb, idx_range(1, 2), list(x = 1:2)), "columns must match schema")
  expect_error(table_write(tb, idx_range(1, 2), list(x = 1:2, y = 1:2, z = 1:2)), "extra")
  expect_error(table_write(tb, idx_range(1, 2), list(x = 1, y = 1L)), "length mismatch")
  expect_error(table_write(tb, idx_range(1, 9), list(x = as.double(1:9), y = 1:9)),
               "out-of-range")
  expect_error(table_write(tb, idx_range(1, 2), 1:2), "data.frame or named list")
  # zero-length row selector is a no-op.
  expect_null(table_write(tb, integer(0), list(x = double(0), y = integer(0))))
})

test_that("row_layout computes disjoint ranges and validates", {
  sh <- shards(9, block_size = 3)  # 3 shards
  lay <- row_layout(sh, rows_per_shard = 4L)
  expect_equal(unname(vapply(lay, function(r) shard:::.table_rows_len(r), integer(1))), c(4L, 4L, 4L))

  lay_fun <- row_layout(sh, rows_per_shard = function(s) s$len)
  expect_equal(length(lay_fun), 3L)

  # zero-size shard yields a NULL layout entry.
  lay0 <- row_layout(sh, rows_per_shard = function(s) if (s$id == 1L) 0L else 2L)
  expect_null(lay0[[1]])

  expect_error(row_layout(list(), 1L), "must be a shard_descriptor")
  expect_error(row_layout(sh), "rows_per_shard is required")
  expect_error(row_layout(sh, rows_per_shard = function(s) -1L), "invalid size")
  expect_error(row_layout(sh, rows_per_shard = -1L), "must be >= 0")
})

test_that(".table_rows_len and .table_rows_resolve handle vectors and NULL", {
  expect_equal(shard:::.table_rows_len(NULL), 0L)
  expect_equal(shard:::.table_rows_len(c(2L, 4L, 6L)), 3L)
  expect_null(shard:::.table_rows_resolve(NULL))
  expect_equal(shard:::.table_rows_resolve(c(2, 4)), c(2L, 4L))
  expect_equal(shard:::.table_rows_resolve(idx_range(2, 4)), 2:4)
})

test_that("table_sink round-trips rds partitions via iterate/collect", {
  s <- schema(x = float64(), y = int32())
  sink <- table_sink(s, mode = "row_groups", format = "rds")
  on.exit(unlink(sink$path, recursive = TRUE), add = TRUE)

  table_write(sink, 1L, data.frame(x = c(1, 2), y = c(10L, 20L)))
  table_write(sink, 2L, data.frame(x = c(3, 4), y = c(30L, 40L)))

  rg <- table_finalize(sink)
  it <- iterate_row_groups(rg)
  c1 <- it(); c2 <- it()
  expect_true(is.data.frame(c1))
  expect_null(it())

  df <- collect(rg)
  expect_equal(sort(df$y), c(10L, 20L, 30L, 40L))

  expect_error(iterate_row_groups(list()), "must be a shard_row_groups")
})

test_that("table_sink round-trips native partitions with a string column", {
  s <- schema(x = float64(), label = string_col(), g = factor_col(c("a", "b")))
  sink <- table_sink(s, mode = "row_groups", format = "auto")  # -> native
  on.exit(unlink(sink$path, recursive = TRUE), add = TRUE)
  expect_equal(sink$format, "native")

  table_write(sink, 1L, data.frame(x = c(1, 2), label = c("hi", "yo"),
                                    g = c("a", "b"), stringsAsFactors = FALSE))
  rg <- table_finalize(sink)
  df <- collect(rg)
  expect_equal(df$label, c("hi", "yo"))
  expect_equal(as.character(df$g), c("a", "b"))
})

test_that("schema-less sink accepts arbitrary data frames as rds", {
  sink <- table_sink(NULL, mode = "row_groups")
  on.exit(unlink(sink$path, recursive = TRUE), add = TRUE)
  expect_equal(sink$format, "rds")
  table_write(sink, 1L, data.frame(a = 1:3, b = letters[1:3]))
  rg <- table_finalize(sink)
  df <- collect(rg)
  expect_equal(df$a, 1:3)

  expect_error(table_sink(list()), "must be a shard_schema")
})

test_that("table_write.shard_table_sink validates inputs", {
  s <- schema(x = float64())
  sink <- table_sink(s, mode = "row_groups", format = "rds")
  on.exit(unlink(sink$path, recursive = TRUE), add = TRUE)
  expect_error(table_write(sink, 1L, list(x = 1)), "must be a data.frame")
  expect_error(table_write(sink, 1L, data.frame(z = 1)), "columns must match schema")
})
