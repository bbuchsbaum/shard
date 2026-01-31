test_that("shard_map re-exports borrow/out after scratch-triggered recycle", {
  skip_on_cran()

  pool_stop()

  n_workers <- 2L
  mats <- lapply(1:8, function(i) matrix(rnorm(10 * 10), 10, 10))
  expected <- vapply(mats, function(m) sqrt(sum(m^2)), numeric(1))

  out <- buffer("double", dim = length(mats))
  blocks <- shards(length(mats), block_size = 1, workers = n_workers)

  res <- suppressWarnings(
    shard_map(
      blocks,
      borrow = list(mats = mats),
      out = list(out = out),
      fun = function(sh, mats, out) {
        # Force a recycle once mid-run. This ensures any replacement worker
        # still has the shard_map borrow/out context re-exported.
        if (isTRUE(sh$id == 1L)) {
          scratch_pool_config(max_bytes = 1)
          scratch_matrix(200, 200, key = "boom")
        }

        i <- sh$idx[[1]]
        out[i] <- sqrt(sum(mats[[i]]^2))
        NULL
      },
      workers = n_workers,
      diagnostics = TRUE
    )
  )

  expect_true(succeeded(res))
  expect_equal(as.numeric(out[]), expected, tolerance = 1e-10)

  pool_stop()
})

