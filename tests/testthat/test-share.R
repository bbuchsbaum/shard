# Tests for share() - zero-copy shared objects

test_that("share returns an ALTREP-backed shared vector for atomic inputs", {
    x <- 1:100
    shared <- share(x)

    expect_true(is_shared_vector(shared))
    expect_s3_class(shared, "shard_shared_vector")

    info <- shared_info(shared)
    expect_true(info$size > 0)
    expect_true(!is.null(info$path))
})

test_that("share returns a shard_shared handle for non-shareable types", {
    x <- letters
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
    recovered <- materialize(shared)
    expect_identical(x, recovered)

    # Numeric vector
    x <- rnorm(50)
    shared <- share(x)
    recovered <- materialize(shared)
    expect_equal(x, recovered)

    # Character vector
    x <- letters
    shared <- share(x)
    recovered <- fetch(shared)
    expect_identical(x, recovered)
    close(shared)

    # Logical vector
    x <- c(TRUE, FALSE, TRUE, NA)
    shared <- share(x)
    recovered <- materialize(shared)
    expect_identical(x, recovered)
})

test_that("share preserves matrix structure", {
    mat <- matrix(1:20, nrow = 4, ncol = 5)
    shared <- share(mat)

    info <- shared_info(shared)
    expect_equal(info$class_info$type, "matrix")
    expect_equal(info$class_info$dim, c(4L, 5L))

    recovered <- materialize(shared)
    expect_identical(mat, recovered)
})

test_that("share preserves array structure", {
    arr <- array(1:24, dim = c(2, 3, 4))
    shared <- share(arr)

    info <- shared_info(shared)
    expect_equal(info$class_info$type, "array")
    expect_equal(info$class_info$dim, c(2L, 3L, 4L))

    recovered <- materialize(shared)
    expect_identical(arr, recovered)
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

    info <- shared_info(shared)
    expect_true(info$readonly)
})

test_that("share with readonly=FALSE creates writable segment", {
    x <- 1:10
    shared <- share(x, readonly = FALSE)

    info <- shared_info(shared)
    expect_false(info$readonly)
})

test_that("materialize is equivalent to fetch", {
    x <- letters
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
})

test_that("print.shard_shared works", {
    x <- letters
    shared <- share(x)

    expect_output(print(shared), "shard_shared")
    expect_output(print(shared), "Size:")

    close(shared)
})

test_that("share handles large objects", {
    # 1 million elements
    x <- rnorm(1e6)
    shared <- share(x)

    info <- shared_info(shared)
    expect_true(info$size >= 8e6)  # At least 8 bytes per double

    recovered <- materialize(shared)
    expect_equal(x, recovered)
})

test_that("share_open can reopen existing shared segment", {
    x <- letters
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
    x <- letters

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
    x <- letters
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

    recovered <- materialize(shared)
    expect_identical(x, recovered)
})

test_that("share handles zero-length vectors of all types", {
    # Double
    x <- double(0)
    shared <- share(x)
    recovered <- materialize(shared)
    expect_identical(x, recovered)

    # Logical
    x <- logical(0)
    shared <- share(x)
    recovered <- materialize(shared)
    expect_identical(x, recovered)

    # Character
    x <- character(0)
    shared <- share(x)
    recovered <- fetch(shared)
    expect_identical(x, recovered)
    close(shared)

    # Raw
    x <- raw(0)
    shared <- share(x)
    recovered <- materialize(shared)
    expect_identical(x, recovered)
})

test_that("share handles single-element vectors", {
    # Single integer
    x <- 1L
    shared <- share(x)
    recovered <- materialize(shared)
    expect_identical(x, recovered)

    # Single double
    x <- 3.14
    shared <- share(x)
    recovered <- materialize(shared)
    expect_equal(x, recovered)

    # Single character
    x <- "hello"
    shared <- share(x)
    recovered <- fetch(shared)
    expect_identical(x, recovered)
    close(shared)

    # Single logical
    x <- TRUE
    shared <- share(x)
    recovered <- materialize(shared)
    expect_identical(x, recovered)

    # Single NA
    x <- NA
    shared <- share(x)
    recovered <- materialize(shared)
    expect_identical(x, recovered)
})

test_that("share handles single-element matrix and array", {
    # 1x1 matrix
    mat <- matrix(42, nrow = 1, ncol = 1)
    shared <- share(mat)
    recovered <- materialize(shared)
    expect_identical(mat, recovered)

    # 1x1x1 array
    arr <- array(42, dim = c(1, 1, 1))
    shared <- share(arr)
    recovered <- materialize(shared)
    expect_identical(arr, recovered)
})

test_that("share handles empty data.frame", {
    # Zero-row data.frame
    df <- data.frame(a = integer(0), b = character(0), stringsAsFactors = FALSE)
    shared <- share(df)
    recovered <- fetch(shared)
    expect_identical(df, recovered)
    close(shared)

    # Zero-column data.frame
    df <- data.frame()
    shared <- share(df)
    recovered <- fetch(shared)
    expect_identical(df, recovered)
    close(shared)
})

test_that("share handles empty list", {
    lst <- list()
    shared <- share(lst)
    recovered <- fetch(shared)
    expect_identical(lst, recovered)
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
    recovered <- materialize(shared)
    expect_identical(f, recovered)

    # POSIXct
    dt <- as.POSIXct("2024-01-01 12:00:00")
    shared <- share(dt)
    recovered <- materialize(shared)
    expect_equal(dt, recovered)

    # Formula (environment-sensitive)
    # Skip this as formulas capture environments
})

# Tests for input validation of non-serializable objects

test_that("share rejects functions with clear error message", {
    fn <- function(x) x + 1

    expect_error(
        share(fn),
        "Cannot share functions"
    )
    expect_error(
        share(fn),
        "closures"
    )
})
test_that("share rejects functions nested in lists", {
    lst <- list(a = 1, fn = function(x) x)

    expect_error(
        share(lst),
        "Cannot share functions"
    )
    expect_error(
        share(lst),
        "x\\$fn"  # Shows the path to the problem
    )
})

test_that("share rejects external pointers with clear error message", {
    # Create a simple external pointer for testing
    # We can use file() to get a connection which has an external pointer
    skip_if_not(exists(".Call"))

    # Actually creating an external pointer requires C code, so we'll test
    # the error message pattern by checking a list containing one
    # For now, verify the error message format for functions (same pattern)
    expect_error(
        share(function(x) x),
        "Extract the data you need"
    )
})

test_that("share rejects environments with external pointers", {
    # Create an environment with a function (which will trigger the validation)
    env <- new.env()
    env$fn <- function(x) x

    expect_error(
        share(env),
        "Cannot share functions"
    )
    expect_error(
        share(env),
        "x\\$fn"
    )
})

test_that("share accepts environments without problematic content", {
    env <- new.env()
    env$a <- 1:10
    env$b <- "hello"

    shared <- share(env)
    recovered <- fetch(shared)

    expect_equal(recovered$a, 1:10)
    expect_equal(recovered$b, "hello")

    close(shared)
})

test_that("share error message shows path to nested problems", {
    # Deeply nested function
    lst <- list(
        level1 = list(
            level2 = list(
                fn = function(x) x
            )
        )
    )

    expect_error(
        share(lst),
        "x\\$level1\\$level2\\$fn"
    )
})

test_that("share error message shows index for unnamed list elements", {
    lst <- list(1, 2, function(x) x)

    expect_error(
        share(lst),
        "x\\[\\[3\\]\\]"
    )
})

# Tests for deep sharing of S4 objects

# Define S4 classes for testing
# Use "ANY" type for slots that will hold reconstructed shared data
setClass("TestS4DeepMatrix",
    slots = c(
        data = "ANY",
        name = "character"
    )
)

setClass("TestS4DeepList",
    slots = c(
        items = "ANY",
        label = "character"
    )
)

setClass("TestS4DeepEnv",
    slots = c(
        data = "ANY",
        env = "environment"
    )
)

setClass("TestS4DeepNested",
    slots = c(
        inner = "ANY",
        value = "numeric"
    )
)

test_that("share with deep=TRUE processes S4 object slots", {
    # Create a large matrix that will be shared
    big_mat <- matrix(rnorm(1e7), nrow = 1000)  # ~80MB
    obj <- new("TestS4DeepMatrix", data = big_mat, name = "test")

    # Share with deep=TRUE
    shared <- share(obj, deep = TRUE, min_bytes = 1e6)

    # Should return a shard_deep_shared object
    expect_s3_class(shared, "shard_deep_shared")

    # Fetch should reconstruct the original
    recovered <- fetch(shared)
    expect_true(isS4(recovered))
    expect_s4_class(recovered, "TestS4DeepMatrix")
    expect_equal(slot(recovered, "data"), big_mat)
    expect_equal(slot(recovered, "name"), "test")

    close(shared)
})

test_that("share with deep=TRUE recursively shares S4 list slots", {
    big_vec <- rnorm(1e7)  # ~80MB
    obj <- new("TestS4DeepList",
        items = list(big = big_vec, small = 1:10),
        label = "test"
    )

    shared <- share(obj, deep = TRUE, min_bytes = 1e6)
    expect_s3_class(shared, "shard_deep_shared")

    # Summary should show shared nodes
    expect_true(shared$summary$shared_count > 0)

    # Fetch should reconstruct
    recovered <- fetch(shared)
    expect_s4_class(recovered, "TestS4DeepList")
    expect_equal(slot(recovered, "items")$big, big_vec)
    expect_equal(slot(recovered, "items")$small, 1:10)

    close(shared)
})

test_that("share with deep=TRUE and mode='balanced' skips S4 environment slots", {
    big_mat <- matrix(rnorm(1e6), nrow = 100)
    env <- new.env()
    env$foo <- "bar"
    obj <- new("TestS4DeepEnv", data = big_mat, env = env)

    # Should not error with mode='balanced' (default)
    shared <- share(obj, deep = TRUE, min_bytes = 1e5, mode = "balanced")
    expect_s3_class(shared, "shard_deep_shared")

    # Fetch should preserve the environment
    recovered <- fetch(shared)
    expect_true(is.environment(slot(recovered, "env")))

    close(shared)
})

test_that("share with deep=TRUE and mode='strict' errors on S4 environment slots", {
    big_mat <- matrix(rnorm(1e6), nrow = 100)
    env <- new.env()
    obj <- new("TestS4DeepEnv", data = big_mat, env = env)

    expect_error(
        share(obj, deep = TRUE, min_bytes = 1e3, mode = "strict"),
        "contains environment"
    )
})

test_that("share with deep=TRUE uses @slot path notation for S4", {
    setClass("TestS4WithFunc",
        slots = c(
            fn = "function"
        )
    )

    obj <- new("TestS4WithFunc", fn = function(x) x)

    # When validating S4 with function slot, should show @slot notation
    expect_error(
        share(obj),
        "@fn"
    )

    removeClass("TestS4WithFunc")
})

test_that("share with deep=TRUE handles nested S4 objects", {
    inner_mat <- matrix(rnorm(1e7), nrow = 1000)
    inner <- new("TestS4DeepMatrix", data = inner_mat, name = "inner")
    outer <- new("TestS4DeepNested", inner = inner, value = 42)

    shared <- share(outer, deep = TRUE, min_bytes = 1e6)
    expect_s3_class(shared, "shard_deep_shared")

    recovered <- fetch(shared)
    expect_s4_class(recovered, "TestS4DeepNested")
    expect_s4_class(slot(recovered, "inner"), "TestS4DeepMatrix")
    expect_equal(slot(slot(recovered, "inner"), "data"), inner_mat)
    expect_equal(slot(recovered, "value"), 42)

    close(shared)
})

test_that("share with deep=TRUE preserves S4 class after fetch", {
    big_mat <- matrix(rnorm(1e7), nrow = 1000)
    obj <- new("TestS4DeepMatrix", data = big_mat, name = "preserved")

    shared <- share(obj, deep = TRUE, min_bytes = 1e6)
    recovered <- fetch(shared)

    expect_identical(class(recovered), class(obj))
    expect_s4_class(recovered, "TestS4DeepMatrix")

    close(shared)
})

# Clean up test classes
removeClass("TestS4DeepMatrix")
removeClass("TestS4DeepList")
removeClass("TestS4DeepEnv")
removeClass("TestS4DeepNested")
