# Tests for share() - zero-copy shared objects

test_that("share creates a shard_shared object", {
    x <- 1:100
    shared <- share(x)

    expect_s3_class(shared, "shard_shared")
    expect_true(shared$size > 0)
    expect_true(!is.null(shared$path))

    close(shared)
})

test_that("share and local round-trip preserves data", {
    # Integer vector
    x <- 1:100
    shared <- share(x)
    recovered <- fetch(shared)
    expect_identical(x, recovered)
    close(shared)

    # Numeric vector
    x <- rnorm(50)
    shared <- share(x)
    recovered <- fetch(shared)
    expect_equal(x, recovered)
    close(shared)

    # Character vector
    x <- letters
    shared <- share(x)
    recovered <- fetch(shared)
    expect_identical(x, recovered)
    close(shared)

    # Logical vector
    x <- c(TRUE, FALSE, TRUE, NA)
    shared <- share(x)
    recovered <- fetch(shared)
    expect_identical(x, recovered)
    close(shared)
})

test_that("share preserves matrix structure", {
    mat <- matrix(1:20, nrow = 4, ncol = 5)
    shared <- share(mat)

    expect_equal(shared$class_info$type, "matrix")
    expect_equal(shared$class_info$dim, c(4L, 5L))

    recovered <- fetch(shared)
    expect_identical(mat, recovered)

    close(shared)
})

test_that("share preserves array structure", {
    arr <- array(1:24, dim = c(2, 3, 4))
    shared <- share(arr)

    expect_equal(shared$class_info$type, "array")
    expect_equal(shared$class_info$dim, c(2L, 3L, 4L))

    recovered <- fetch(shared)
    expect_identical(arr, recovered)

    close(shared)
})

test_that("share preserves data.frame structure", {
    df <- data.frame(
        a = 1:5,
        b = letters[1:5],
        c = c(TRUE, FALSE, TRUE, FALSE, TRUE),
        stringsAsFactors = FALSE
    )
    shared <- share(df)

    expect_equal(shared$class_info$type, "data.frame")
    expect_equal(shared$class_info$nrow, 5)
    expect_equal(shared$class_info$ncol, 3)

    recovered <- fetch(shared)
    expect_identical(df, recovered)

    close(shared)
})

test_that("share preserves list structure", {
    lst <- list(
        a = 1:5,
        b = "hello",
        c = list(nested = TRUE)
    )
    shared <- share(lst)

    expect_equal(shared$class_info$type, "list")
    expect_equal(shared$class_info$length, 3)

    recovered <- fetch(shared)
    expect_identical(lst, recovered)

    close(shared)
})

test_that("share creates read-only segment by default", {
    x <- 1:10
    shared <- share(x)

    expect_true(shared$readonly)

    info <- shared_info(shared)
    expect_true(info$readonly)

    close(shared)
})

test_that("share with readonly=FALSE creates writable segment", {
    x <- 1:10
    shared <- share(x, readonly = FALSE)

    expect_false(shared$readonly)

    close(shared)
})

test_that("materialize is equivalent to fetch", {
    x <- 1:100
    shared <- share(x)

    via_fetch <- fetch(shared)
    via_materialize <- materialize(shared)

    expect_identical(via_fetch, via_materialize)

    close(shared)
})

test_that("materialize.default returns input unchanged", {
    x <- 1:10
    expect_identical(materialize(x), x)

    y <- list(a = 1, b = 2)
    expect_identical(materialize(y), y)
})

test_that("is_shared correctly identifies shared objects", {
    x <- 1:10
    shared <- share(x)

    expect_true(is_shared(shared))
    expect_false(is_shared(x))
    expect_false(is_shared(NULL))
    expect_false(is_shared(list()))

    close(shared)
})

test_that("shared_info returns complete information", {
    x <- matrix(1:20, nrow = 4)
    shared <- share(x)

    info <- shared_info(shared)

    expect_true(is.list(info))
    expect_true(!is.null(info$path))
    expect_true(!is.null(info$backing))
    expect_true(!is.null(info$size))
    expect_true(!is.null(info$readonly))
    expect_true(!is.null(info$class_info))
    expect_true(!is.null(info$segment_info))

    close(shared)
})

test_that("print.shard_shared works", {
    x <- matrix(1:20, nrow = 4)
    shared <- share(x)

    expect_output(print(shared), "shard_shared")
    expect_output(print(shared), "Size:")
    expect_output(print(shared), "matrix")

    close(shared)
})

test_that("share handles large objects", {
    # 1 million elements
    x <- rnorm(1e6)
    shared <- share(x)

    expect_true(shared$size > 8e6)  # At least 8 bytes per double

    recovered <- fetch(shared)
    expect_equal(x, recovered)

    close(shared)
})

test_that("share_open can reopen existing shared segment", {
    x <- 1:100
    shared1 <- share(x)

    path <- shared1$path
    backing <- shared1$backing
    size <- shared1$size

    skip_if(is.null(path), "Path is NULL")

    # Open the same segment
    shared2 <- share_open(path, backing = backing, size = size)

    # Read from reopened segment
    recovered <- fetch(shared2)
    expect_identical(x, recovered)

    # Close in order (shared2 doesn't unlink)
    close(shared2)
    close(shared1)
})

test_that("share respects backing type", {
    x <- 1:10

    # Test mmap backing
    shared_mmap <- share(x, backing = "mmap")
    expect_equal(shared_mmap$backing, "mmap")
    close(shared_mmap)

    # Test auto backing
    shared_auto <- share(x, backing = "auto")
    expect_s3_class(shared_auto, "shard_shared")
    close(shared_auto)
})

test_that("close releases resources", {
    x <- 1:10
    shared <- share(x)
    path <- shared$path

    # Close should work without error
    expect_silent(close(shared))

    # Segment should be gone (file-backed)
    if (!is.null(path) && shared$backing == "mmap") {
        expect_false(file.exists(path))
    }
})

test_that("share handles empty vectors", {
    x <- integer(0)
    shared <- share(x)

    recovered <- fetch(shared)
    expect_identical(x, recovered)

    close(shared)
})

test_that("share handles NULL elements in lists", {
    lst <- list(a = 1, b = NULL, c = 3)
    shared <- share(lst)

    recovered <- fetch(shared)
    expect_identical(lst, recovered)

    close(shared)
})

test_that("share handles complex objects", {
    # Factor
    f <- factor(c("a", "b", "a", "c"))
    shared <- share(f)
    recovered <- fetch(shared)
    expect_identical(f, recovered)
    close(shared)

    # POSIXct
    dt <- as.POSIXct("2024-01-01 12:00:00")
    shared <- share(dt)
    recovered <- fetch(shared)
    expect_equal(dt, recovered)
    close(shared)

    # Formula (environment-sensitive)
    # Skip this as formulas capture environments
})
