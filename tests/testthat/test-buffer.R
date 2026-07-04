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

test_that("buffer skips full initialization writes for zero-filled buffers", {
    shard:::buffer_reset_diagnostics()
    buf <- buffer("double", dim = 100, init = NULL)
    expect_equal(buf[], rep(0, 100))
    d <- buffer_diagnostics()
    expect_equal(d$init_writes, 0L)
    expect_equal(d$init_bytes, 0)
    buffer_close(buf)

    shard:::buffer_reset_diagnostics()
    buf <- buffer("integer", dim = 50, init = 0L)
    expect_equal(buf[], rep(0L, 50))
    d <- buffer_diagnostics()
    expect_equal(d$init_writes, 0L)
    expect_equal(d$init_bytes, 0)
    buffer_close(buf)
})

test_that("buffer preserves nonzero initialization writes", {
    shard:::buffer_reset_diagnostics()
    buf <- buffer("double", dim = 10, init = 42)
    expect_equal(buf[], rep(42, 10))
    d <- buffer_diagnostics()
    expect_equal(d$init_writes, 1L)
    expect_equal(d$init_bytes, 10 * 8)
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

test_that("buffer sparse gather reads requested elements for all types", {
    cases <- list(
        double = list(values = as.double(1:8), idx = c(7L, 2L, 5L), size = 8L),
        integer = list(values = as.integer(1:8), idx = c(7L, 2L, 5L), size = 4L),
        logical = list(values = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
                       idx = c(7L, 2L, 5L), size = 4L),
        raw = list(values = as.raw(1:8), idx = c(7L, 2L, 5L), size = 1L)
    )

    for (type in names(cases)) {
        case <- cases[[type]]
        buf <- buffer(type, dim = length(case$values))
        buf[] <- case$values

        shard:::buffer_reset_diagnostics()
        expect_equal(buf[case$idx], case$values[case$idx])
        d <- buffer_diagnostics()
        expect_equal(d$reads, 1L)
        expect_equal(d$read_bytes, length(case$idx) * case$size)

        buffer_close(buf)
    }
})

test_that("buffer sparse scatter writes requested elements for all types", {
    cases <- list(
        double = list(values = c(10, 20, 30), expected = c(20, 30, 10), size = 8L),
        integer = list(values = c(10L, 20L, 30L), expected = c(20L, 30L, 10L), size = 4L),
        logical = list(values = c(TRUE, FALSE, TRUE), expected = c(FALSE, TRUE, TRUE), size = 4L),
        raw = list(values = as.raw(c(10, 20, 30)), expected = as.raw(c(20, 30, 10)), size = 1L)
    )
    idx <- c(6L, 1L, 4L)

    for (type in names(cases)) {
        case <- cases[[type]]
        buf <- buffer(type, dim = 8)

        shard:::buffer_reset_diagnostics()
        buf[idx] <- case$values
        d <- buffer_diagnostics()
        expect_equal(d$writes, 1L)
        expect_equal(d$bytes, length(idx) * case$size)
        expect_equal(buf[c(1L, 4L, 6L)], case$expected)

        buffer_close(buf)
    }
})

test_that("buffer scatter preserves duplicate last-write-wins semantics", {
    buf <- buffer("integer", dim = 6)

    shard:::buffer_reset_diagnostics()
    buf[c(2L, 2L, 4L, 2L)] <- c(10L, 20L, 40L, 30L)
    d <- buffer_diagnostics()

    expect_equal(d$writes, 1L)
    expect_equal(d$bytes, 4L * 4L)
    expect_equal(buf[c(2L, 4L)], c(30L, 40L))

    buffer_close(buf)
})

test_that("buffer sparse indexing rejects NA and out-of-bounds indices", {
    buf <- buffer("double", dim = 5)
    expect_error(buf[NA_integer_], "Index out of bounds")
    expect_error(buf[c(1L, 6L)], "Index out of bounds")
    expect_error(buf[NA_integer_] <- 1, "Index out of bounds")
    expect_error(buf[c(1L, 6L)] <- 1:2, "Index out of bounds")
    buffer_close(buf)

    mat <- buffer("double", dim = c(3, 3))
    expect_error(mat[4L, 1L], "Index out of bounds")
    expect_error(mat[1L, NA_integer_], "Index out of bounds")
    expect_error(mat[4L, 1L] <- 1, "Index out of bounds")
    expect_error(mat[1L, NA_integer_] <- 1, "Index out of bounds")
    buffer_close(mat)
})

test_that("buffer zero-length sparse read and write are no-ops", {
    buf <- buffer("double", dim = 5)
    expect_equal(buf[integer(0)], double(0))

    shard:::buffer_reset_diagnostics()
    buf[integer(0)] <- numeric(0)
    d <- buffer_diagnostics()
    expect_equal(d$writes, 0L)
    expect_equal(d$bytes, 0)
    expect_equal(buf[], rep(0, 5))

    buffer_close(buf)
})

test_that("buffer accepts idx_range selectors without sparse materialization", {
    buf <- buffer("integer", dim = 10)
    buf[] <- 1:10

    shard:::buffer_reset_diagnostics()
    expect_equal(buf[idx_range(3, 6)], 3L:6L)
    d <- buffer_diagnostics()
    expect_equal(d$reads, 1L)
    expect_equal(d$read_bytes, 4L * 4L)

    shard:::buffer_reset_diagnostics()
    buf[idx_range(3, 6)] <- 11:14
    d <- buffer_diagnostics()
    expect_equal(d$writes, 1L)
    expect_equal(d$bytes, 4L * 4L)
    expect_equal(buf[1:8], c(1L, 2L, 11L, 12L, 13L, 14L, 7L, 8L))

    buffer_close(buf)
})

test_that("buffer matrix sparse reads and writes use gathered linear indices", {
    buf <- buffer("double", dim = c(4, 5))
    ref <- matrix(as.double(1:20), nrow = 4)
    buf[] <- as.vector(ref)

    rows <- c(4L, 1L)
    cols <- c(5L, 2L)

    shard:::buffer_reset_diagnostics()
    expect_equal(buf[rows, cols], ref[rows, cols])
    d <- buffer_diagnostics()
    expect_equal(d$reads, 1L)
    expect_equal(d$read_bytes, length(rows) * length(cols) * 8L)

    value <- matrix(c(100, 200, 300, 400), nrow = 2)
    shard:::buffer_reset_diagnostics()
    buf[rows, cols] <- value
    ref[rows, cols] <- value
    d <- buffer_diagnostics()
    expect_equal(d$writes, 1L)
    expect_equal(d$bytes, length(rows) * length(cols) * 8L)
    expect_equal(as.matrix(buf), ref)

    buffer_close(buf)
})

test_that("buffer matrix idx_range selectors read and write expected blocks", {
    buf <- buffer("integer", dim = c(5, 4))
    ref <- matrix(as.integer(1:20), nrow = 5)
    buf[] <- as.vector(ref)

    expect_equal(buf[idx_range(2, 4), idx_range(2, 3)], ref[2:4, 2:3])

    value <- matrix(as.integer(101:106), nrow = 3)
    shard:::buffer_reset_diagnostics()
    buf[idx_range(2, 4), idx_range(2, 3)] <- value
    ref[2:4, 2:3] <- value
    d <- buffer_diagnostics()
    expect_equal(d$writes, 1L)
    expect_equal(d$bytes, length(value) * 4L)
    expect_equal(as.matrix(buf), ref)

    buffer_close(buf)
})

test_that("buffer sparse writes record one logical write operation", {
    buf <- buffer("double", dim = 100)
    idx <- seq.int(1L, 99L, by = 2L)

    shard:::buffer_reset_diagnostics()
    buf[idx] <- seq_along(idx)
    d <- buffer_diagnostics()

    expect_equal(d$writes, 1L)
    expect_equal(d$bytes, length(idx) * 8L)
    expect_equal(buf[idx], as.double(seq_along(idx)))

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
