test_that("kernel registry supports dispatch via shard_map(kernel=)", {
  skip_on_cran()

  pool_stop()

  # Built-ins should be registered at load time.
  expect_true("crossprod_tile" %in% list_kernels())

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
