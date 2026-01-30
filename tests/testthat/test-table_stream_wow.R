test_that("stream_top_k and stream_group_* work on native datasets", {
  skip_on_cran()

  pool_stop()

  sch <- schema(
    id = int32(),
    grp = factor_col(levels = c("odd", "even")),
    msg = string_col(),
    val = float64()
  )

  s <- shards(12, block_size = 4, workers = 2)
  out_path <- file.path(tempdir(), paste0("shard_test_wow_", shard:::unique_id()))
  sink <- table_sink(sch, mode = "partitioned", path = out_path) # format auto -> native

  res <- shard_map(
    s,
    out = list(sink = sink),
    fun = function(shard, sink) {
      df <- data.frame(
        id = as.integer(shard$idx),
        grp = ifelse((as.integer(shard$idx) %% 2L) == 0L, "even", "odd"),
        msg = ifelse(shard$idx == 9L, NA_character_, paste0("x", shard$idx)),
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

  top <- stream_top_k(ds, col = "val", k = 3)
  top <- as.data.frame(top, stringsAsFactors = FALSE)
  expect_equal(top$id, as.integer(c(12, 11, 10)))
  expect_equal(top$val, as.double(c(12, 11, 10)))
  expect_equal(top$msg, c("x12", "x11", "x10"))

  # Group counts should be balanced (6 even, 6 odd).
  cnt <- stream_group_count(ds, group = "grp")
  cnt <- as.data.frame(cnt, stringsAsFactors = FALSE)
  expect_equal(as.integer(cnt$n), c(6L, 6L))

  # Group sums: sum(odd ids) and sum(even ids).
  sm <- stream_group_sum(ds, group = "grp", value = "val")
  sm <- as.data.frame(sm, stringsAsFactors = FALSE)
  expect_equal(round(sm$sum, 6), c(sum(seq(1, 12, by = 2)), sum(seq(2, 12, by = 2))))

  # Numeric sum should ignore strings entirely.
  expect_equal(stream_sum(ds, col = "val"), sum(as.double(1:12)))

  unlink(out_path, recursive = TRUE, force = TRUE)
  pool_stop()
})
