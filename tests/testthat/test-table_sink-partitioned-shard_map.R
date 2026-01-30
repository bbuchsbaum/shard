test_that("table_sink(partitioned) writes a manifest and can be collected", {
  skip_on_cran()

  pool_stop()
  shard:::table_reset_diagnostics()

  sch <- schema(id = int32(), grp = factor_col(levels = c("odd", "even")), msg = string_col(), val = float64())
  s <- shards(12, block_size = 4, workers = 2)

  out_path <- file.path(tempdir(), paste0("shard_test_dataset_", shard:::unique_id()))
  sink <- table_sink(sch, mode = "partitioned", path = out_path)

  res <- shard_map(
    s,
    out = list(sink = sink),
    fun = function(shard, sink) {
      grp <- if ((shard$id %% 2L) == 0L) "even" else "odd"
      df <- data.frame(
        id = as.integer(shard$idx),
        grp = rep(grp, length(shard$idx)),
        msg = paste0("x", shard$idx),
        val = as.double(shard$idx) * 0.1,
        stringsAsFactors = FALSE
      )
      table_write(sink, shard$id, df)
      NULL
    },
    workers = 2,
    diagnostics = TRUE
  )

  expect_true(succeeded(res))

  handle <- table_finalize(sink, materialize = "never")
  expect_s3_class(handle, "shard_dataset")
  expect_true(file.exists(file.path(out_path, "manifest.rds")))
  expect_equal(length(handle$files), s$num_shards)

  df <- collect(handle)
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  expect_equal(df$id, as.integer(1:12))
  expect_equal(df$msg, paste0("x", 1:12))
  expect_equal(df$val, as.double(1:12) * 0.1)

  unlink(out_path, recursive = TRUE, force = TRUE)
  pool_stop()
})

