# Tests for shared memory segment operations

test_that("segment_create creates a segment with correct size", {
    seg <- segment_create(1024)
    expect_s3_class(seg, "shard_segment")
    expect_equal(segment_size(seg), 1024)
    segment_close(seg)
})

test_that("segment_create respects backing type", {
    # Test mmap backing
    seg_mmap <- segment_create(1024, backing = "mmap")
    info <- segment_info(seg_mmap)
    expect_equal(info$backing, "mmap")
    segment_close(seg_mmap)

    # Test auto backing (should work on all platforms)
    seg_auto <- segment_create(1024, backing = "auto")
    expect_s3_class(seg_auto, "shard_segment")
    segment_close(seg_auto)
})

test_that("segment_write and segment_read work with raw data", {
    seg <- segment_create(1024)

    # Write raw data
    data <- as.raw(1:100)
    segment_write(seg, data, offset = 0)

    # Read it back
    result <- segment_read(seg, offset = 0, size = 100)
    expect_equal(result, data)

    segment_close(seg)
})

test_that("segment_write and segment_read work with numeric data", {
    seg <- segment_create(1024)

    # Write numeric data
    data <- c(1.5, 2.5, 3.5, 4.5)
    segment_write(seg, data, offset = 0)

    # Read as raw and convert back
    raw_result <- segment_read(seg, offset = 0, size = length(data) * 8)
    result <- readBin(raw_result, "double", n = length(data))
    expect_equal(result, data)

    segment_close(seg)
})

test_that("segment_write and segment_read work with integer data", {
    seg <- segment_create(1024)

    # Write integer data
    data <- 1L:10L
    segment_write(seg, data, offset = 0)

    # Read as raw and convert back
    raw_result <- segment_read(seg, offset = 0, size = length(data) * 4)
    result <- readBin(raw_result, "integer", n = length(data))
    expect_equal(result, data)

    segment_close(seg)
})

test_that("segment_info returns correct information", {
    seg <- segment_create(2048, backing = "mmap")
    info <- segment_info(seg)

    expect_true(is.list(info))
    expect_equal(info$size, 2048)
    expect_equal(info$backing, "mmap")
    expect_false(info$readonly)
    expect_true(info$owns)
    expect_true(!is.null(info$path))

    segment_close(seg)
})

test_that("segment_path returns the path", {
    seg <- segment_create(1024, backing = "mmap")
    path <- segment_path(seg)

    expect_true(is.character(path) || is.null(path))
    if (is.character(path)) {
        expect_true(nchar(path) > 0)
    }

    segment_close(seg)
})

test_that("segment_protect makes segment read-only", {
    seg <- segment_create(1024)

    # Write before protecting
    data <- as.raw(1:10)
    segment_write(seg, data, offset = 0)

    # Protect
    segment_protect(seg)
    info <- segment_info(seg)
    expect_true(info$readonly)

    segment_close(seg)
})

test_that("segment_open can reopen an existing segment", {
    # Create and write to a segment
    seg1 <- segment_create(1024, backing = "mmap")
    data <- as.raw(42:52)
    segment_write(seg1, data, offset = 0)

    # Get the path
    path <- segment_path(seg1)
    skip_if(is.null(path), "Path is NULL")

    # Open the same segment
    seg2 <- segment_open(path, backing = "mmap", readonly = TRUE)
    result <- segment_read(seg2, offset = 0, size = length(data))
    expect_equal(result, data)

    # Clean up (close seg2 first without unlinking)
    segment_close(seg2, unlink = FALSE)
    segment_close(seg1, unlink = TRUE)
})

test_that("is_windows returns logical", {
    result <- is_windows()
    expect_true(is.logical(result))
    expect_length(result, 1)
})

test_that("available_backings returns character vector", {
    backings <- available_backings()
    expect_true(is.character(backings))
    expect_true("mmap" %in% backings)
})

test_that("print.shard_segment works", {
    seg <- segment_create(1024)
    expect_output(print(seg), "shard_segment")
    expect_output(print(seg), "Size:")
    expect_output(print(seg), "Backing:")
    segment_close(seg)
})

test_that("segment handles large sizes", {
    # 10 MB segment
    size <- 10 * 1024 * 1024
    seg <- segment_create(size)
    expect_equal(segment_size(seg), size)
    segment_close(seg)
})

test_that("segment write at offset works", {
    seg <- segment_create(1024)

    # Write at offset
    data <- as.raw(1:10)
    segment_write(seg, data, offset = 100)

    # Read from that offset
    result <- segment_read(seg, offset = 100, size = 10)
    expect_equal(result, data)

    segment_close(seg)
})
