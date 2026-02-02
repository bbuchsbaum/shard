test_that("stream_* helpers work without collect()", {
  skip_on_cran()

  pool_stop()

  sch <- schema(id = int32(), msg = string_col(), val = float64())
  s <- shards(10, block_size = 3, workers = 2)

  out_path <- file.path(tempdir(), paste0("shard_test_stream_", shard:::unique_id()))
  sink <- table_sink(sch, mode = "partitioned", path = out_path)

  res <- shard_map(
    s,
    out = list(sink = sink),
    fun = function(shard, sink) {
      df <- data.frame(
        id = as.integer(shard$idx),
        msg = paste0("x", shard$idx),
        val = as.double(shard$idx),
        stringsAsFactors = FALSE
      )
      table_write(sink, shard$id, df)
      NULL
    },
    workers = 2
  )

  expect_true(succeeded(res))

  ds <- table_finalize(sink, materialize = "never")
  expect_s3_class(ds, "shard_dataset")

  expect_equal(stream_count(ds), 10L)

  ssum <- stream_reduce(
    ds,
    f = function(chunk) sum(chunk$val),
    init = 0,
    combine = `+`
  )
  expect_equal(ssum, sum(as.double(1:10)))

  filtered_path <- file.path(tempdir(), paste0("shard_test_stream_filt_", shard:::unique_id()))
  ds2 <- stream_filter(ds, predicate = function(chunk) chunk$id %% 2L == 0L, path = filtered_path)
  expect_s3_class(ds2, "shard_dataset")

  df2 <- collect(ds2)
  df2 <- as.data.frame(df2, stringsAsFactors = FALSE)
  expect_equal(df2$id, as.integer(seq(2, 10, by = 2)))

  unlink(out_path, recursive = TRUE, force = TRUE)
  unlink(filtered_path, recursive = TRUE, force = TRUE)
  pool_stop()
})

test_that("stream_map applies function to each partition", {
  skip_on_cran()

  pool_stop()

  sch <- schema(id = int32(), val = float64())
  s <- shards(6, block_size = 2, workers = 2)

  out_path <- file.path(tempdir(), paste0("shard_test_stream_map_", shard:::unique_id()))
  on.exit(unlink(out_path, recursive = TRUE, force = TRUE), add = TRUE)

  sink <- table_sink(sch, mode = "partitioned", path = out_path)

  res <- shard_map(
    s,
    out = list(sink = sink),
    fun = function(shard, sink) {
      df <- data.frame(id = as.integer(shard$idx), val = as.double(shard$idx) * 2)
      table_write(sink, shard$id, df)
      NULL
    },
    workers = 2
  )

  ds <- table_finalize(sink, materialize = "never")

  # stream_map returns list of per-partition results
  results <- stream_map(ds, f = function(chunk) nrow(chunk))
  expect_true(is.list(results))
  expect_equal(sum(unlist(results)), 6L)

  pool_stop()
})

test_that("stream_sum computes correct sum", {
  skip_on_cran()

  pool_stop()

  sch <- schema(id = int32(), val = float64())
  s <- shards(10, block_size = 2, workers = 2)

  out_path <- file.path(tempdir(), paste0("shard_test_stream_sum_", shard:::unique_id()))
  on.exit(unlink(out_path, recursive = TRUE, force = TRUE), add = TRUE)

  sink <- table_sink(sch, mode = "partitioned", path = out_path)

  res <- shard_map(
    s,
    out = list(sink = sink),
    fun = function(shard, sink) {
      df <- data.frame(id = as.integer(shard$idx), val = as.double(shard$idx))
      table_write(sink, shard$id, df)
      NULL
    },
    workers = 2
  )

  ds <- table_finalize(sink, materialize = "never")

  # Sum of 1:10 = 55
  total <- stream_sum(ds, col = "val")
  expect_equal(total, sum(1:10))

  pool_stop()
})

test_that("stream_sum validates inputs", {
  skip_on_cran()

  pool_stop()

  sch <- schema(id = int32(), msg = string_col())
  out_path <- file.path(tempdir(), paste0("shard_test_stream_sum_err_", shard:::unique_id()))
  on.exit(unlink(out_path, recursive = TRUE, force = TRUE), add = TRUE)

  sink <- table_sink(sch, mode = "partitioned", path = out_path)
  res <- shard_map(
    shards(2, block_size = 1),
    out = list(sink = sink),
    fun = function(shard, sink) {
      df <- data.frame(id = shard$id, msg = paste0("x", shard$id))
      table_write(sink, shard$id, df)
      NULL
    },
    workers = 2
  )

  ds <- table_finalize(sink, materialize = "never")

  # String column cannot be summed
  expect_error(stream_sum(ds, col = "msg"), "requires a numeric column")

  # Unknown column
  expect_error(stream_sum(ds, col = "nonexistent"), "Unknown column")

  pool_stop()
})

test_that("stream_reduce validates f and combine", {
  skip_on_cran()

  pool_stop()

  sch <- schema(id = int32())
  out_path <- file.path(tempdir(), paste0("shard_test_reduce_err_", shard:::unique_id()))
  on.exit(unlink(out_path, recursive = TRUE, force = TRUE), add = TRUE)

  sink <- table_sink(sch, mode = "partitioned", path = out_path)
  res <- shard_map(
    shards(2, block_size = 1),
    out = list(sink = sink),
    fun = function(shard, sink) {
      table_write(sink, shard$id, data.frame(id = shard$id))
      NULL
    },
    workers = 2
  )

  ds <- table_finalize(sink, materialize = "never")

  expect_error(stream_reduce(ds, f = "not_a_function", init = 0, combine = `+`),
               "f must be a function")
  expect_error(stream_reduce(ds, f = nrow, init = 0, combine = "not_a_function"),
               "combine must be a function")

  pool_stop()
})

test_that("stream_filter validates predicate", {
  skip_on_cran()

  pool_stop()

  sch <- schema(id = int32())
  out_path <- file.path(tempdir(), paste0("shard_test_filter_err_", shard:::unique_id()))
  on.exit(unlink(out_path, recursive = TRUE, force = TRUE), add = TRUE)

  sink <- table_sink(sch, mode = "partitioned", path = out_path)
  res <- shard_map(
    shards(2, block_size = 1),
    out = list(sink = sink),
    fun = function(shard, sink) {
      table_write(sink, shard$id, data.frame(id = shard$id))
      NULL
    },
    workers = 2
  )

  ds <- table_finalize(sink, materialize = "never")

  expect_error(stream_filter(ds, predicate = "not_a_function"),
               "predicate must be a function")

  pool_stop()
})

test_that("stream_map validates f", {
  skip_on_cran()

  pool_stop()

  sch <- schema(id = int32())
  out_path <- file.path(tempdir(), paste0("shard_test_map_err_", shard:::unique_id()))
  on.exit(unlink(out_path, recursive = TRUE, force = TRUE), add = TRUE)

  sink <- table_sink(sch, mode = "partitioned", path = out_path)
  res <- shard_map(
    shards(2, block_size = 1),
    out = list(sink = sink),
    fun = function(shard, sink) {
      table_write(sink, shard$id, data.frame(id = shard$id))
      NULL
    },
    workers = 2
  )

  ds <- table_finalize(sink, materialize = "never")

  expect_error(stream_map(ds, f = "not_a_function"),
               "f must be a function")

  pool_stop()
})

test_that("stream_top_k finds top k rows", {
  skip_on_cran()

  pool_stop()

  sch <- schema(id = int32(), score = float64())
  out_path <- file.path(tempdir(), paste0("shard_test_topk_", shard:::unique_id()))
  on.exit(unlink(out_path, recursive = TRUE, force = TRUE), add = TRUE)

  sink <- table_sink(sch, mode = "partitioned", path = out_path)

  res <- shard_map(
    shards(10, block_size = 2),
    out = list(sink = sink),
    fun = function(shard, sink) {
      df <- data.frame(id = as.integer(shard$idx), score = as.double(shard$idx))
      table_write(sink, shard$id, df)
      NULL
    },
    workers = 2
  )

  ds <- table_finalize(sink, materialize = "never")

  # Top 3 by score (decreasing)
  top3 <- stream_top_k(ds, col = "score", k = 3, decreasing = TRUE)
  expect_true(nrow(top3) == 3)
  expect_equal(as.integer(top3$id), c(10L, 9L, 8L))

  # Bottom 3 (ascending)
  bottom3 <- stream_top_k(ds, col = "score", k = 3, decreasing = FALSE)
  expect_true(nrow(bottom3) == 3)
  expect_equal(as.integer(bottom3$id), c(1L, 2L, 3L))

  pool_stop()
})

test_that("stream_top_k validates inputs", {
  skip_on_cran()

  pool_stop()

  sch <- schema(id = int32(), msg = string_col())
  out_path <- file.path(tempdir(), paste0("shard_test_topk_err_", shard:::unique_id()))
  on.exit(unlink(out_path, recursive = TRUE, force = TRUE), add = TRUE)

  sink <- table_sink(sch, mode = "partitioned", path = out_path)
  res <- shard_map(
    shards(2, block_size = 1),
    out = list(sink = sink),
    fun = function(shard, sink) {
      table_write(sink, shard$id, data.frame(id = shard$id, msg = paste0("x", shard$id)))
      NULL
    },
    workers = 2
  )

  ds <- table_finalize(sink, materialize = "never")

  # k must be >= 1
  expect_error(stream_top_k(ds, col = "id", k = 0), "k must be >= 1")

  # Unknown column
  expect_error(stream_top_k(ds, col = "nonexistent", k = 3), "Unknown column")

  # String column not supported
  expect_error(stream_top_k(ds, col = "msg", k = 3), "requires an int32\\(\\) or float64\\(\\)")

  pool_stop()
})

test_that("stream_group_sum computes grouped sums", {
  skip_on_cran()

  pool_stop()

  sch <- schema(grp = factor_col(c("A", "B", "C")), val = float64())
  out_path <- file.path(tempdir(), paste0("shard_test_grpsum_", shard:::unique_id()))
  on.exit(unlink(out_path, recursive = TRUE, force = TRUE), add = TRUE)

  sink <- table_sink(sch, mode = "partitioned", path = out_path)

  res <- shard_map(
    shards(9, block_size = 3),
    out = list(sink = sink),
    fun = function(shard, sink) {
      # Create pattern: A=1, B=2, C=3, A=4, B=5, C=6, A=7, B=8, C=9
      grps <- c("A", "B", "C")[(shard$idx - 1L) %% 3L + 1L]
      df <- data.frame(
        grp = factor(grps, levels = c("A", "B", "C")),
        val = as.double(shard$idx)
      )
      table_write(sink, shard$id, df)
      NULL
    },
    workers = 2
  )

  ds <- table_finalize(sink, materialize = "never")

  result <- stream_group_sum(ds, group = "grp", value = "val")

  expect_true(is.data.frame(result))
  expect_true("group" %in% names(result))
  expect_true("sum" %in% names(result))

  # A: 1+4+7=12, B: 2+5+8=15, C: 3+6+9=18
  expect_equal(result$sum[result$group == "A"], 12)
  expect_equal(result$sum[result$group == "B"], 15)
  expect_equal(result$sum[result$group == "C"], 18)

  pool_stop()
})

test_that("stream_group_count counts rows per group", {
  skip_on_cran()

  pool_stop()

  sch <- schema(grp = factor_col(c("X", "Y")), id = int32())
  out_path <- file.path(tempdir(), paste0("shard_test_grpcnt_", shard:::unique_id()))
  on.exit(unlink(out_path, recursive = TRUE, force = TRUE), add = TRUE)

  sink <- table_sink(sch, mode = "partitioned", path = out_path)

  res <- shard_map(
    shards(6, block_size = 2),
    out = list(sink = sink),
    fun = function(shard, sink) {
      # Alternating X, Y
      grps <- c("X", "Y")[(shard$idx - 1L) %% 2L + 1L]
      df <- data.frame(
        grp = factor(grps, levels = c("X", "Y")),
        id = as.integer(shard$idx)
      )
      table_write(sink, shard$id, df)
      NULL
    },
    workers = 2
  )

  ds <- table_finalize(sink, materialize = "never")

  result <- stream_group_count(ds, group = "grp")

  expect_true(is.data.frame(result))
  expect_true("group" %in% names(result))
  expect_true("n" %in% names(result))

  # X: 1,3,5 (3 items), Y: 2,4,6 (3 items)
  expect_equal(result$n[result$group == "X"], 3L)
  expect_equal(result$n[result$group == "Y"], 3L)

  pool_stop()
})

