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

