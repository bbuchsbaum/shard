test_that("view materialization hotspots are attributed in diagnostics", {
  skip_on_cran()

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  set.seed(1)
  n <- 50L
  v <- 20L
  Y <- matrix(rnorm(n * v), nrow = n)
  Ysh <- share(Y, backing = "mmap")

  blocks <- shards(ncol(Ysh), workers = 2)
  res <- shard_map(
    blocks,
    borrow = list(Y = Ysh),
    fun = function(sh, Y) {
      vv <- view_block(Y, cols = idx_range(sh$start, sh$end))
      m <- materialize(vv)
      sum(m)
    },
    workers = 2,
    chunk_size = 1L,
    autotune = FALSE,
    diagnostics = TRUE
  )

  expect_true(succeeded(res))

  cr <- copy_report(res)
  expect_gt(cr$view_materialized_bytes %||% 0, 0)
  hs <- cr$view_materialize_hotspots %||% list()
  expect_true(is.list(hs))
  expect_gt(length(hs), 0L)
})

