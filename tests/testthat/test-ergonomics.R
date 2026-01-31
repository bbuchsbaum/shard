test_that("shard_apply_matrix matches col-wise apply for scalar returns", {
  skip_on_cran()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  set.seed(1)
  X <- matrix(rnorm(50), nrow = 10, ncol = 5)

  res <- shard_apply_matrix(
    X,
    MARGIN = 2,
    FUN = function(v) sum(v),
    workers = 2,
    policy = shard_apply_policy(backing = "mmap", block_size = 2)
  )

  expect_equal(res, as.numeric(colSums(X)))
})

test_that("shard_apply_matrix passes VARS through to FUN", {
  skip_on_cran()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  X <- matrix(1:12, nrow = 3, ncol = 4)
  res <- shard_apply_matrix(
    X,
    FUN = function(v, add) sum(v) + add,
    VARS = list(add = 10),
    workers = 2,
    policy = shard_apply_policy(backing = "mmap", block_size = 2)
  )

  expect_equal(res, as.numeric(colSums(X) + 10))
})

test_that("shard_apply_matrix works when X is already shared", {
  skip_on_cran()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  set.seed(1)
  X <- matrix(rnorm(200), nrow = 20, ncol = 10)
  Xsh <- share(X, backing = "mmap", readonly = TRUE)

  res <- shard_apply_matrix(
    Xsh,
    FUN = function(v) mean(v),
    workers = 2,
    policy = shard_apply_policy(backing = "mmap", block_size = 3)
  )

  expect_equal(res, as.numeric(colMeans(X)))
})

test_that("shard_apply_matrix errors if FUN returns a non-scalar", {
  skip_on_cran()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  X <- matrix(1:12, nrow = 3, ncol = 4)
  expect_error(
    suppressWarnings(
      shard_apply_matrix(
        X,
        FUN = function(v) v,
        workers = 2,
        policy = shard_apply_policy(backing = "mmap", block_size = 2)
      )
    ),
    "scalar atomic"
  )
})

test_that("shard_lapply_shared enforces max_gather_bytes guardrail", {
  skip_on_cran()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  x <- as.list(1:4)
  pol <- shard_apply_policy(max_gather_bytes = "1MB", backing = "mmap", block_size = 2)

  expect_error(
    shard_lapply_shared(
      x,
      FUN = function(el) raw(1024 * 1024), # ~1MB per element => ~4MB gathered
      workers = 2,
      policy = pol
    ),
    "Refusing to gather"
  )
})

test_that("shard_lapply_shared behaves like lapply for small outputs", {
  skip_on_cran()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  x <- list(1:3, 4:5, integer())
  res <- shard_lapply_shared(
    x,
    FUN = function(el) length(el),
    workers = 2,
    policy = shard_apply_policy(backing = "mmap", block_size = 2)
  )

  expect_equal(res, lapply(x, length))
})

test_that("shard_lapply_shared exercises auto-share path for list elements", {
  skip_on_cran()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  x <- list(1:3, 4:5)
  pol <- shard_apply_policy(auto_share_min_bytes = 0, backing = "mmap", block_size = 2)

  res <- shard_lapply_shared(
    x,
    FUN = function(el) {
      length(el)
    },
    workers = 2,
    policy = pol
  )

  expect_equal(res, lapply(x, length))
})

test_that("shard_lapply_shared passes VARS through to FUN", {
  skip_on_cran()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  x <- list(1:3, 4:5, integer())
  res <- shard_lapply_shared(
    x,
    FUN = function(el, add) length(el) + add,
    VARS = list(add = 7),
    workers = 2,
    policy = shard_apply_policy(backing = "mmap", block_size = 2)
  )

  expect_equal(res, lapply(x, function(el) length(el) + 7))
})

test_that("shards() and shard_map() tolerate detectCores() returning NA", {
  skip_on_cran()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  # On this environment, parallel::detectCores() may return NA. Ensure that the
  # default workers=NULL path is still safe and chooses a usable fallback.
  blocks <- shards(100, block_size = "auto", workers = NULL)
  expect_true(inherits(blocks, "shard_descriptor"))
  expect_true(blocks$num_shards >= 1L)

  res <- shard_map(10, fun = function(shard) sum(shard$idx), workers = NULL)
  expect_true(succeeded(res))
})
