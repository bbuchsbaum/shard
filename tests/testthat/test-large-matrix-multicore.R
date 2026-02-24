# Tests for large matrix + multicore interaction
#
# These tests verify correctness when parallel workers operate on matrices
# large enough to exercise real-world memory/BLAS patterns that small toy
# matrices miss:
#   - shard_map with borrowed large matrices vs single-threaded baseline
#   - shard_crossprod on 1000+ column matrices
#   - shard_reduce streaming aggregation over large shared data
#   - shard_apply_matrix column-parallel correctness
#   - Workers=1 vs Workers=N equivalence (determinism check)
#   - Buffer output correctness under concurrent tile writes

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------
skip_if_not_large <- function() {
  if (!identical(tolower(Sys.getenv("SHARD_RUN_LARGE", "false")), "true")) {
    skip("Set SHARD_RUN_LARGE=true to run large-matrix multicore tests")
  }
}

# ---------------------------------------------------------------------------
# shard_map: large borrowed matrix, parallel vs serial baseline
# ---------------------------------------------------------------------------
test_that("shard_map: column sums on 1000x500 matrix match base R", {

  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  set.seed(42)
  X <- matrix(rnorm(1000 * 500), nrow = 1000, ncol = 500)
  expected <- colSums(X)

  blocks <- shards(ncol(X), block_size = 50, workers = 2)

  result <- shard_map(blocks,
    borrow = list(X = X),
    fun = function(shard, X) {
      colSums(X[, shard$idx, drop = FALSE])
    },
    workers = 2
  )

  expect_true(succeeded(result))
  res <- unlist(results(result))
  expect_equal(unname(res), unname(expected), tolerance = 1e-12)
})

test_that("shard_map: row-wise crossprod slices on 2000x200 matrix match base R", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  set.seed(7)
  n <- 2000L
  p <- 200L
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  expected <- crossprod(X)  # p x p

  # Partition rows; each shard computes crossprod of its row slice
  blocks <- shards(n, block_size = 250, workers = 2)

  result <- shard_map(blocks,
    borrow = list(X = X),
    fun = function(shard, X) {
      crossprod(X[shard$idx, , drop = FALSE])
    },
    workers = 2
  )

  expect_true(succeeded(result))

  # Sum the partial crossprods
  partials <- results(result)
  accumulated <- Reduce(`+`, partials)
  expect_equal(accumulated, expected, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# shard_crossprod: larger dimensions
# ---------------------------------------------------------------------------
test_that("shard_crossprod: 500x300 x 500x200 matches base crossprod", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  set.seed(99)
  n <- 500L; px <- 300L; py <- 200L
  X <- matrix(rnorm(n * px), nrow = n, ncol = px)
  Y <- matrix(rnorm(n * py), nrow = n, ncol = py)
  expected <- crossprod(X, Y)  # px x py

  res <- shard_crossprod(X, Y, workers = 2, block_x = 64, block_y = 64,
                         materialize = "always")

  expect_true(succeeded(res$run))
  expect_equal(dim(res$value), c(px, py))
  expect_equal(res$value, expected, tolerance = 1e-10)
})

test_that("shard_crossprod: 1000x512 square case matches base crossprod", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(3)
  on.exit(pool_stop(), add = TRUE)

  set.seed(123)
  n <- 1000L; p <- 512L
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  expected <- crossprod(X)  # X'X, p x p

  res <- shard_crossprod(X, X, workers = 3, block_x = 128, block_y = 128,
                         materialize = "always")

  expect_true(succeeded(res$run))
  expect_equal(dim(res$value), c(p, p))
  expect_equal(res$value, expected, tolerance = 1e-10)
})

test_that("shard_crossprod: auto tile sizes on 800x256 x 800x384", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  set.seed(11)
  n <- 800L; px <- 256L; py <- 384L
  X <- matrix(rnorm(n * px), nrow = n, ncol = px)
  Y <- matrix(rnorm(n * py), nrow = n, ncol = py)
  expected <- crossprod(X, Y)

  res <- shard_crossprod(X, Y, workers = 2, materialize = "always")

  expect_true(succeeded(res$run))
  expect_equal(res$value, expected, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Workers=1 vs Workers=N: determinism / correctness equivalence
# ---------------------------------------------------------------------------
test_that("shard_crossprod: workers=1 vs workers=3 produce identical results", {
  skip_on_cran()
  skip_if_not_large()

  set.seed(77)
  n <- 600L; px <- 200L; py <- 150L
  X <- matrix(rnorm(n * px), nrow = n, ncol = px)
  Y <- matrix(rnorm(n * py), nrow = n, ncol = py)

  # Single worker
  pool_stop()
  pool_create(1)
  res1 <- shard_crossprod(X, Y, workers = 1, block_x = 50, block_y = 50,
                          materialize = "always")
  pool_stop()

  # Three workers
  pool_create(3)
  res3 <- shard_crossprod(X, Y, workers = 3, block_x = 50, block_y = 50,
                          materialize = "always")
  pool_stop()

  expect_true(succeeded(res1$run))
  expect_true(succeeded(res3$run))
  expect_equal(res1$value, res3$value, tolerance = 1e-12)
})

test_that("shard_map: workers=1 vs workers=2 give identical column sums", {
  skip_on_cran()
  skip_if_not_large()

  set.seed(55)
  X <- matrix(rnorm(800 * 300), nrow = 800, ncol = 300)

  run_colsums <- function(w) {
    pool_stop()
    pool_create(w)
    blocks <- shards(ncol(X), block_size = 30, workers = w)
    result <- shard_map(blocks,
      borrow = list(X = X),
      fun = function(shard, X) {
        colSums(X[, shard$idx, drop = FALSE])
      },
      workers = w
    )
    pool_stop()
    unname(unlist(results(result)))
  }

  res1 <- run_colsums(1)
  res2 <- run_colsums(2)

  expect_equal(res1, res2, tolerance = 1e-14)
})

# ---------------------------------------------------------------------------
# shard_reduce: streaming aggregation over large shared matrix
# ---------------------------------------------------------------------------
test_that("shard_reduce: streaming row-sum over 1500x400 shared matrix", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  set.seed(33)
  n <- 1500L; p <- 400L
  M <- share(matrix(rnorm(n * p), nrow = n, ncol = p))
  expected <- sum(M)

  # Partition over rows; each shard sums its row block
  res <- shard_reduce(
    shards(n, block_size = 100),
    map = function(s, M) {
      sum(M[s$idx, ])
    },
    combine = `+`,
    init = 0,
    borrow = list(M = M),
    workers = 2
  )

  expect_true(succeeded(res))
  expect_equal(res$value, expected, tolerance = 1e-10)
})

test_that("shard_reduce: streaming crossprod accumulation matches base R", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  set.seed(88)
  n <- 1000L; p <- 100L
  X <- share(matrix(rnorm(n * p), nrow = n, ncol = p))
  expected <- crossprod(X)  # p x p

  res <- shard_reduce(
    shards(n, block_size = 200),
    map = function(s, X) {
      crossprod(X[s$idx, , drop = FALSE])
    },
    combine = function(acc, x) acc + x,
    init = matrix(0, nrow = p, ncol = p),
    borrow = list(X = X),
    workers = 2
  )

  expect_true(succeeded(res))
  expect_equal(res$value, expected, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# shard_apply_matrix: large column-parallel apply
# ---------------------------------------------------------------------------
test_that("shard_apply_matrix: column norms on 1000x600 matrix", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  set.seed(22)
  X <- matrix(rnorm(1000 * 600), nrow = 1000, ncol = 600)
  expected <- apply(X, 2, function(v) sqrt(sum(v^2)))

  res <- shard_apply_matrix(X, MARGIN = 2,
    FUN = function(v) sqrt(sum(v^2)),
    workers = 2
  )

  expect_length(res, 600)
  expect_equal(res, expected, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Buffer output correctness under concurrent tile writes
# ---------------------------------------------------------------------------
test_that("shard_crossprod buffer: concurrent tile writes produce correct result", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(3)
  on.exit(pool_stop(), add = TRUE)

  set.seed(44)
  n <- 700L; px <- 256L; py <- 192L
  X <- matrix(rnorm(n * px), nrow = n, ncol = px)
  Y <- matrix(rnorm(n * py), nrow = n, ncol = py)
  expected <- crossprod(X, Y)

  # Use materialize="never" to get buffer, then read back

  res <- shard_crossprod(X, Y, workers = 3, block_x = 32, block_y = 32,
                         materialize = "never")

  expect_s3_class(res$buffer, "shard_buffer")
  mat <- as.matrix(res$buffer)
  expect_equal(dim(mat), c(px, py))
  expect_equal(mat, expected, tolerance = 1e-10)
})

test_that("shard_crossprod buffer: small tiles (16x16) on large matrix", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  set.seed(66)
  n <- 500L; px <- 128L; py <- 96L
  X <- matrix(rnorm(n * px), nrow = n, ncol = px)
  Y <- matrix(rnorm(n * py), nrow = n, ncol = py)
  expected <- crossprod(X, Y)

  # Many small tiles = many concurrent buffer writes
  res <- shard_crossprod(X, Y, workers = 2, block_x = 16, block_y = 16,
                         materialize = "always")

  expect_true(succeeded(res$run))
  expect_equal(res$value, expected, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Pre-shared large matrices (zero-copy path)
# ---------------------------------------------------------------------------
test_that("shard_map with pre-shared 1200x400 matrix matches base R", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  set.seed(101)
  X_raw <- matrix(rnorm(1200 * 400), nrow = 1200, ncol = 400)
  X <- share(X_raw)
  expected <- colMeans(X_raw)

  blocks <- shards(ncol(X), block_size = 40, workers = 2)

  result <- shard_map(blocks,
    borrow = list(X = X),
    fun = function(shard, X) {
      colMeans(X[, shard$idx, drop = FALSE])
    },
    workers = 2
  )

  expect_true(succeeded(result))
  res <- unlist(results(result))
  expect_equal(unname(res), unname(expected), tolerance = 1e-12)
})

test_that("shard_crossprod with pre-shared 800x256 matrices", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  set.seed(202)
  n <- 800L; px <- 256L; py <- 128L
  X <- share(matrix(rnorm(n * px), nrow = n, ncol = px))
  Y <- share(matrix(rnorm(n * py), nrow = n, ncol = py))
  expected <- crossprod(X, Y)

  res <- shard_crossprod(X, Y, workers = 2, block_x = 64, block_y = 64,
                         materialize = "always")

  expect_true(succeeded(res$run))
  expect_equal(res$value, expected, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Multiple large borrowed inputs
# ---------------------------------------------------------------------------
test_that("shard_map: two large borrowed matrices, element-wise combine", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  set.seed(303)
  n <- 800L; p <- 300L
  A <- matrix(rnorm(n * p), nrow = n, ncol = p)
  B <- matrix(rnorm(n * p), nrow = n, ncol = p)
  expected <- colSums(A * B)

  blocks <- shards(p, block_size = 50, workers = 2)

  result <- shard_map(blocks,
    borrow = list(A = A, B = B),
    fun = function(shard, A, B) {
      colSums(A[, shard$idx, drop = FALSE] * B[, shard$idx, drop = FALSE])
    },
    workers = 2
  )

  expect_true(succeeded(result))
  res <- unlist(results(result))
  expect_equal(unname(res), unname(expected), tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Buffer write with shard_map on large output
# ---------------------------------------------------------------------------
test_that("shard_map: write large output to shared buffer", {
  skip_on_cran()
  skip_if_not_large()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  set.seed(404)
  n <- 1000L; p <- 500L
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  expected <- colSums(X)

  out_buf <- buffer("double", dim = p, init = 0)

  blocks <- shards(p, block_size = 50, workers = 2)

  result <- shard_map(blocks,
    borrow = list(X = X),
    out = list(out = out_buf),
    fun = function(shard, X, out) {
      for (j in shard$idx) {
        out[j] <- sum(X[, j])
      }
      NULL
    },
    workers = 2
  )

  expect_true(succeeded(result))
  expect_equal(as.numeric(out_buf[]), unname(expected), tolerance = 1e-12)
})
