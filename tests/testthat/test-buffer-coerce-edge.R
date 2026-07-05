# Coverage for buffer coercions, printing, dim, open-mismatch, zero-init
# detection, empty reads, and the 3-D array fallback paths.

test_that("buffer type helpers reject unsupported types", {
  expect_error(shard:::.buffer_type_size("complex"), "Unsupported buffer type")
  expect_error(shard:::.buffer_sexptype("complex"), "Unsupported buffer type")
})

test_that(".buffer_is_zero_init classifies init values per type", {
  expect_false(shard:::.buffer_is_zero_init("double", numeric(0), FALSE))
  expect_true(shard:::.buffer_is_zero_init("double", 0, FALSE))
  expect_false(shard:::.buffer_is_zero_init("double", 5, FALSE))
  expect_true(shard:::.buffer_is_zero_init("integer", 0L, FALSE))
  expect_true(shard:::.buffer_is_zero_init("logical", FALSE, FALSE))
  expect_false(shard:::.buffer_is_zero_init("logical", TRUE, FALSE))
  expect_true(shard:::.buffer_is_zero_init("raw", as.raw(0), FALSE))
  expect_false(shard:::.buffer_is_zero_init("double", NA_real_, FALSE))
  expect_true(shard:::.buffer_is_zero_init("double", NULL, TRUE))
})

test_that("buffer_open rejects a size mismatch", {
  buf <- buffer("double", dim = 8)
  on.exit(buffer_close(buf), add = TRUE)
  path <- buffer_path(buf)
  skip_if(is.null(path))
  expect_error(
    buffer_open(path, type = "double", dim = 1000, backing = "mmap"),
    "Segment size mismatch"
  )
})

test_that("print and dim reflect 1-D vs 2-D buffers", {
  b1 <- buffer("double", dim = 12)
  on.exit(buffer_close(b1), add = TRUE)
  expect_output(print(b1), "Length:")
  expect_null(dim(b1))
  expect_equal(length(b1), 12)

  b2 <- buffer("integer", dim = c(3, 4))
  on.exit(buffer_close(b2), add = TRUE)
  expect_output(print(b2), "Dim:")
  expect_equal(dim(b2), c(3L, 4L))
})

test_that("empty and non-contiguous reads work for all types", {
  br <- buffer("raw", dim = 5)
  on.exit(buffer_close(br), add = TRUE)
  expect_equal(br[integer(0)], raw(0))

  bl <- buffer("logical", dim = 5)
  on.exit(buffer_close(bl), add = TRUE)
  expect_equal(bl[integer(0)], logical(0))

  # Direct empty gather path.
  bd <- buffer("double", dim = 5)
  on.exit(buffer_close(bd), add = TRUE)
  expect_equal(shard:::.buffer_read_indices(bd, integer(0)), double(0))

  # idx_range out of bounds.
  expect_error(bd[idx_range(1, 99)], "Index out of bounds")
})

test_that("full reads return correct values for logical and raw buffers", {
  bl <- buffer("logical", dim = 4)
  bl[1:4] <- c(TRUE, FALSE, TRUE, FALSE)
  on.exit(buffer_close(bl), add = TRUE)
  expect_equal(bl[], c(TRUE, FALSE, TRUE, FALSE))

  br <- buffer("raw", dim = 4)
  br[1:4] <- as.raw(1:4)
  on.exit(buffer_close(br), add = TRUE)
  expect_equal(br[], as.raw(1:4))
})

test_that("buffer coercion generics return typed vectors and arrays", {
  bd <- buffer("double", dim = 5); bd[1:5] <- as.double(1:5)
  on.exit(buffer_close(bd), add = TRUE)
  expect_equal(as.vector(bd), as.double(1:5))
  expect_equal(as.double(bd), as.double(1:5))
  expect_equal(as.vector(bd, mode = "character"), as.character(1:5))

  bi <- buffer("integer", dim = 5); bi[1:5] <- 1:5
  on.exit(buffer_close(bi), add = TRUE)
  expect_equal(as.integer(bi), 1:5)

  bl <- buffer("logical", dim = 3); bl[1:3] <- c(TRUE, FALSE, TRUE)
  on.exit(buffer_close(bl), add = TRUE)
  expect_equal(as.logical(bl), c(TRUE, FALSE, TRUE))

  br <- buffer("raw", dim = 3); br[1:3] <- as.raw(1:3)
  on.exit(buffer_close(br), add = TRUE)
  expect_equal(as.raw(br), as.raw(1:3))

  bm <- buffer("double", dim = c(2, 3)); bm[] <- as.double(1:6)
  on.exit(buffer_close(bm), add = TRUE)
  expect_equal(as.matrix(bm), matrix(as.double(1:6), 2, 3))
  expect_error(as.matrix(bd), "not 2-dimensional")
  expect_equal(as.array(bm), array(as.double(1:6), dim = c(2, 3)))
  expect_equal(as.array(bd), as.double(1:5))
})

test_that("3-D array buffers support read/modify/write via the fallback path", {
  ba <- buffer("double", dim = c(2, 3, 4))
  on.exit(buffer_close(ba), add = TRUE)
  ref <- array(as.double(1:24), dim = c(2, 3, 4))
  ba[] <- as.vector(ref)

  expect_equal(ba[1, 2, 3], ref[1, 2, 3])

  ba[1, 1, 1] <- 99
  ref[1, 1, 1] <- 99
  expect_equal(as.array(ba), ref)
})
