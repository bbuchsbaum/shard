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

