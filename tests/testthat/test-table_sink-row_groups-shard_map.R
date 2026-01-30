test_that("table_sink(row_groups) can be used as an out= target in shard_map", {
  skip_on_cran()

  pool_stop()
  shard:::table_reset_diagnostics()

  sch <- schema(id = int32(), grp = factor_col(levels = c("odd", "even")), msg = string_col(), val = float64())
  s <- shards(10, block_size = 3, workers = 2)

  sink_path <- file.path(tempdir(), paste0("shard_test_sink_", shard:::unique_id()))
  sink <- table_sink(sch, mode = "row_groups", path = sink_path)

  res <- shard_map(
    s,
    out = list(sink = sink),
    fun = function(shard, sink) {
      if ((shard$id %% 2L) == 1L) {
        # Variable-sized output: some shards emit nothing.
        df <- data.frame(id = integer(0), grp = character(0), msg = character(0), val = numeric(0),
                         stringsAsFactors = FALSE)
      } else {
        df <- data.frame(
          id = as.integer(shard$idx),
          grp = rep("even", length(shard$idx)),
          msg = paste0("x", shard$idx),
          val = as.double(shard$idx) * 0.25,
          stringsAsFactors = FALSE
        )
      }

      table_write(sink, shard$id, df)
      NULL
    },
    workers = 2,
    diagnostics = TRUE
  )

  expect_true(succeeded(res))

  cr <- copy_report(res)
  expect_gt(cr$table_writes %||% 0L, 0L)
  expect_gt(cr$table_bytes %||% 0, 0)

  handle <- table_finalize(sink, materialize = "never")
  expect_s3_class(handle, "shard_row_groups")
  expect_equal(length(handle$files), s$num_shards)

  df <- as_tibble(handle)
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  expected_idx <- unlist(lapply(s$shards, function(sh) {
    if ((sh$id %% 2L) == 0L) sh$idx else integer(0)
  }))

  expect_equal(df$id, as.integer(expected_idx))
  expect_equal(df$msg, paste0("x", expected_idx))
  expect_equal(df$val, as.double(expected_idx) * 0.25)

  unlink(sink_path, recursive = TRUE, force = TRUE)
  pool_stop()
})

