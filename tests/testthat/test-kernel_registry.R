test_that("kernel registry supports dispatch via shard_map(kernel=)", {
  skip_on_cran()

  pool_stop()

  # Built-ins should be registered at load time.
  expect_true("crossprod_tile" %in% list_kernels())
  expect_true("col_means" %in% list_kernels())
  expect_true("col_vars" %in% list_kernels())

  # Register a tiny custom kernel.
  register_kernel(
    name = "test_kernel_id",
    impl = function(sh, ...) sh$id,
    footprint = function(sh) list(class = "tiny", bytes = 8),
    supports_views = FALSE
  )
  expect_true("test_kernel_id" %in% list_kernels())

  res <- shard_map(shards(10, block_size = 1), kernel = "test_kernel_id", workers = 2, diagnostics = TRUE)
  expect_true(succeeded(res))
  expect_equal(unname(unlist(results(res))), 1:10)

  # Diagnostics should record the chosen kernel.
  expect_equal(res$diagnostics$kernel %||% NULL, "test_kernel_id")

  pool_stop()
})

test_that("shard_map errors clearly on unknown kernel", {
  pool_stop()
  pool_create(1)
  on.exit(pool_stop(), add = TRUE)

  expect_error(
    shard_map(shards(2, block_size = 1), kernel = "no_such_kernel", workers = 1),
    "Unknown kernel"
  )
})

test_that("register_kernel validates name argument", {
  expect_error(register_kernel("", impl = function(x) x),
               "name must be a non-empty string")
})

test_that("register_kernel validates impl argument", {
  expect_error(register_kernel("test", impl = "not_a_function"),
               "impl must be a function")
})

test_that("register_kernel validates signature argument", {
  expect_error(register_kernel("test", impl = function(x) x, signature = 123),
               "signature must be a string or NULL")
})

test_that("register_kernel validates description argument", {
  expect_error(register_kernel("test", impl = function(x) x, description = 123),
               "description must be a string or NULL")
})

test_that("register_kernel validates footprint argument", {
  expect_error(register_kernel("test", impl = function(x) x, footprint = "invalid"),
               "footprint must be a function, numeric, or NULL")
})

test_that("register_kernel accepts valid footprint types", {
  # Numeric footprint
  meta1 <- register_kernel("test_numeric_fp", impl = function(x) x, footprint = 1024)
  expect_equal(meta1$footprint, 1024)

  # Function footprint
  fp_fn <- function(sh) list(class = "tiny", bytes = 8)
  meta2 <- register_kernel("test_fn_fp", impl = function(x) x, footprint = fp_fn)
  expect_true(is.function(meta2$footprint))

  # NULL footprint
  meta3 <- register_kernel("test_null_fp", impl = function(x) x, footprint = NULL)
  expect_null(meta3$footprint)
})

test_that("get_kernel returns registered kernel", {
  register_kernel("test_get", impl = function(x) x * 2, description = "test")

  k <- shard:::get_kernel("test_get")
  expect_true(is.list(k))
  expect_equal(k$name, "test_get")
  expect_true(is.function(k$impl))
  expect_equal(k$description, "test")
})

test_that("get_kernel returns NULL for unregistered kernel", {
  k <- shard:::get_kernel("definitely_not_registered_kernel_xyz")
  expect_null(k)
})

test_that("col_means kernel computes correct results", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Create test data
  n_rows <- 100
  n_cols <- 20
  X <- share(matrix(rnorm(n_rows * n_cols), nrow = n_rows, ncol = n_cols))
  out <- buffer("double", dim = n_cols, init = 0)

  # Use the col_means kernel
  res <- shard_map(
    shards(n_cols, block_size = 5),
    kernel = "col_means",
    borrow = list(X = X),
    out = list(out = out),
    workers = 2
  )

  expect_true(succeeded(res))
  expect_equal(as.numeric(out[]), colMeans(as.matrix(X)), tolerance = 1e-10)
})

test_that("col_vars kernel computes correct results", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Create test data
  n_rows <- 100
  n_cols <- 20
  X <- share(matrix(rnorm(n_rows * n_cols), nrow = n_rows, ncol = n_cols))
  out <- buffer("double", dim = n_cols, init = 0)

  # Use the col_vars kernel
  res <- shard_map(
    shards(n_cols, block_size = 5),
    kernel = "col_vars",
    borrow = list(X = X),
    out = list(out = out),
    workers = 2
  )

  expect_true(succeeded(res))

  # Calculate expected using apply
  expected_vars <- apply(as.matrix(X), 2, var)
  expect_equal(as.numeric(out[]), expected_vars, tolerance = 1e-10)
})

test_that("register_kernel records supports_views flag", {
  meta1 <- register_kernel("test_views_true", impl = function(x) x, supports_views = TRUE)
  expect_true(meta1$supports_views)

  meta2 <- register_kernel("test_views_false", impl = function(x) x, supports_views = FALSE)
  expect_false(meta2$supports_views)

  # Default should be TRUE
  meta3 <- register_kernel("test_views_default", impl = function(x) x)
  expect_true(meta3$supports_views)
})
