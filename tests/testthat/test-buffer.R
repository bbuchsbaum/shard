# Tests for buffer() - typed writable output buffers

test_that("buffer creates double buffer", {
    buf <- buffer("double", dim = 100)
    expect_s3_class(buf, "shard_buffer")
    expect_equal(buf$type, "double")
    expect_equal(buf$n, 100)
    expect_equal(length(buf), 100)

    # Check initial values are zero
    expect_equal(buf[], rep(0, 100))

    buffer_close(buf)
})

test_that("buffer creates integer buffer", {
    buf <- buffer("integer", dim = 50)
    expect_equal(buf$type, "integer")
    expect_equal(buf[], rep(0L, 50))
    buffer_close(buf)
})

test_that("buffer creates logical buffer", {
    buf <- buffer("logical", dim = 25)
    expect_equal(buf$type, "logical")
    expect_equal(buf[], rep(FALSE, 25))
    buffer_close(buf)
})

test_that("buffer creates raw buffer", {
    buf <- buffer("raw", dim = 10)
    expect_equal(buf$type, "raw")
    expect_equal(buf[], raw(10))
    buffer_close(buf)
})

test_that("buffer with custom init value", {
    buf <- buffer("double", dim = 10, init = 42)
    expect_equal(buf[], rep(42, 10))
    buffer_close(buf)
})

test_that("buffer slice assignment works", {
    buf <- buffer("double", dim = 100)

    # Write to a slice
    buf[1:10] <- 1:10
    expect_equal(buf[1:10], as.double(1:10))
    expect_equal(buf[11], 0)

    # Write to non-contiguous indices
    buf[c(20, 30, 40)] <- c(200, 300, 400)
    expect_equal(buf[c(20, 30, 40)], c(200, 300, 400))

    buffer_close(buf)
})

test_that("buffer slice reading works", {
    buf <- buffer("integer", dim = 100)
    buf[1:100] <- 1:100

    expect_equal(buf[1:10], 1L:10L)
    expect_equal(buf[50], 50L)
    expect_equal(buf[c(1, 50, 100)], c(1L, 50L, 100L))

    buffer_close(buf)
})

test_that("buffer full extraction with [] works", {
    buf <- buffer("double", dim = 10)
    buf[1:5] <- 1:5
    buf[6:10] <- 6:10

    result <- buf[]
    expect_equal(result, as.double(1:10))

    buffer_close(buf)
})

test_that("buffer matrix operations work", {
    buf <- buffer("double", dim = c(10, 5))

    expect_equal(dim(buf), c(10, 5))
    expect_equal(length(buf), 50)

    # Write a column
    buf[, 1] <- 1:10
    expect_equal(buf[, 1], as.double(1:10))

    # Write a row
    buf[1, ] <- 101:105
    expect_equal(buf[1, ], as.double(101:105))

    # Write a submatrix
    buf[2:4, 2:3] <- matrix(1:6, nrow = 3)
    expect_equal(buf[2:4, 2:3], matrix(as.double(1:6), nrow = 3))

    buffer_close(buf)
})

test_that("buffer as.* conversions work", {
    buf <- buffer("double", dim = 10)
    buf[] <- 1:10

    expect_equal(as.vector(buf), as.double(1:10))
    expect_equal(as.double(buf), as.double(1:10))
    expect_equal(as.integer(buf), 1L:10L)

    buffer_close(buf)
})

test_that("buffer matrix conversion works", {
    buf <- buffer("double", dim = c(3, 4))
    buf[] <- 1:12

    mat <- as.matrix(buf)
    expect_true(is.matrix(mat))
    expect_equal(dim(mat), c(3, 4))
    expect_equal(mat[1, 1], 1)
    expect_equal(mat[3, 4], 12)

    buffer_close(buf)
})

test_that("buffer info returns correct information", {
    buf <- buffer("double", dim = c(100, 50))

    info <- buffer_info(buf)
    expect_equal(info$type, "double")
    expect_equal(info$dim, c(100L, 50L))
    expect_equal(info$n, 5000)
    expect_equal(info$bytes, 5000 * 8)

    buffer_close(buf)
})

test_that("buffer path returns path", {
    buf <- buffer("double", dim = 100)
    path <- buffer_path(buf)
    expect_true(!is.null(path))
    expect_true(is.character(path))
    buffer_close(buf)
})

test_that("buffer print works", {
    buf <- buffer("double", dim = c(100, 50))
    expect_output(print(buf), "shard_buffer")
    expect_output(print(buf), "double")
    expect_output(print(buf), "100 x 50")
    buffer_close(buf)
})

test_that("buffer rejects invalid dimensions", {
    expect_error(buffer("double", dim = 0))
    expect_error(buffer("double", dim = -1))
    expect_error(buffer("double", dim = c(10, 0)))
})

test_that("buffer rejects out of bounds indices", {
    buf <- buffer("double", dim = 10)
    expect_error(buf[0])
    expect_error(buf[11])
    expect_error(buf[1:11])
    expect_error(buf[-1] <- 1)
    buffer_close(buf)
})

test_that("buffer works with mmap backing", {
    buf <- buffer("double", dim = 100, backing = "mmap")
    buf[1:50] <- 1:50
    expect_equal(buf[1:50], as.double(1:50))
    buffer_close(buf)
})

test_that("buffer_open attaches to existing buffer", {
    # Create original buffer
    buf1 <- buffer("double", dim = 100)
    buf1[1:10] <- 1:10
    path <- buffer_path(buf1)
    info <- buffer_info(buf1)

    # Open from another "process" (same process but tests API)
    buf2 <- buffer_open(path, type = "double", dim = 100,
                        backing = info$backing)

    # Read what buf1 wrote
    expect_equal(buf2[1:10], as.double(1:10))

    # Write from buf2
    buf2[50:60] <- 50:60

    # Read from buf1
    expect_equal(buf1[50:60], as.double(50:60))

    buffer_close(buf1)
    buffer_close(buf2, unlink = FALSE)  # Don't unlink since buf1 owns it
})

test_that("buffer value recycling works", {
    buf <- buffer("double", dim = 10)

    # Single value recycled to fill indices
    buf[1:5] <- 42
    expect_equal(buf[1:5], rep(42, 5))

    buffer_close(buf)
})

test_that("buffer integer type preserves values", {
    buf <- buffer("integer", dim = 100)
    buf[1:10] <- c(-5L, -4L, -3L, -2L, -1L, 0L, 1L, 2L, 3L, 4L)
    expect_equal(buf[1:10], c(-5L, -4L, -3L, -2L, -1L, 0L, 1L, 2L, 3L, 4L))
    buffer_close(buf)
})

test_that("buffer logical type works", {
    buf <- buffer("logical", dim = 10)
    buf[c(1, 3, 5, 7, 9)] <- TRUE
    expect_equal(buf[c(1, 3, 5, 7, 9)], rep(TRUE, 5))
    expect_equal(buf[c(2, 4, 6, 8, 10)], rep(FALSE, 5))
    buffer_close(buf)
})

test_that("buffer handles single-element dimension", {
    # Single-element double buffer
    buf <- buffer("double", dim = 1)
    expect_equal(buf$n, 1)
    expect_equal(length(buf), 1)
    expect_equal(buf[], 0)

    buf[1] <- 42
    expect_equal(buf[1], 42)
    expect_equal(buf[], 42)
    buffer_close(buf)

    # Single-element integer buffer
    buf <- buffer("integer", dim = 1)
    buf[1] <- 99L
    expect_equal(buf[1], 99L)
    buffer_close(buf)

    # Single-element logical buffer
    buf <- buffer("logical", dim = 1)
    expect_equal(buf[1], FALSE)
    buf[1] <- TRUE
    expect_equal(buf[1], TRUE)
    buffer_close(buf)
})

test_that("buffer handles 1x1 matrix dimension", {
    buf <- buffer("double", dim = c(1, 1))
    expect_equal(dim(buf), c(1, 1))
    expect_equal(length(buf), 1)

    buf[1, 1] <- 42
    expect_equal(buf[1, 1], 42)
    expect_equal(buf[], matrix(42, 1, 1))

    buffer_close(buf)
})
