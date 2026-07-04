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

test_that("view hotspot attribution is gated by option for local materialization", {
  shard:::view_reset_diagnostics()
  withr::local_options(shard.view_hotspots = FALSE)

  X <- matrix(as.double(1:30), nrow = 5)
  Xsh <- share(X, backing = "mmap")
  v <- view_block(Xsh, cols = idx_range(2, 4))

  expect_equal(materialize(v), X[, 2:4, drop = FALSE])
  vd <- view_diagnostics()
  expect_equal(vd$materialized, 1L)
  expect_gt(vd$materialized_bytes, 0)
  expect_equal(shard:::view_materialize_hotspots_snapshot_(), list())
})

test_that("view hotspot attribution can be enabled explicitly", {
  shard:::view_reset_diagnostics()
  withr::local_options(shard.view_hotspots = TRUE)

  X <- matrix(as.double(1:30), nrow = 5)
  Xsh <- share(X, backing = "mmap")
  v <- view_block(Xsh, cols = idx_range(2, 4))

  expect_equal(materialize(v), X[, 2:4, drop = FALSE])
  hs <- shard:::view_materialize_hotspots_snapshot_()
  expect_true(is.list(hs))
  expect_gt(length(hs), 0L)
})
