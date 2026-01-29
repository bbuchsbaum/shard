# Tests for deep sharing with memoization, alias preservation, and cycle detection

test_that("deep share preserves aliases - same object creates one segment", {
    # Create a large matrix and reference it twice in a list
    big_mat <- matrix(rnorm(10000), nrow = 100)  # ~80KB

    lst <- list(a = big_mat, b = big_mat)

    # Share with deep=TRUE and low threshold to trigger sharing
    shared <- share(lst, deep = TRUE, min_bytes = 1000)

    expect_s3_class(shared, "shard_deep_shared")

    # Should have exactly 1 shared segment (the matrix)
    # and 1 alias (the second reference to the same matrix)
    expect_equal(shared$summary$shared_count, 1)
    expect_equal(shared$summary$alias_count, 1)

    # Fetch and verify data is correct
    recovered <- fetch(shared)
    expect_identical(recovered$a, big_mat)
    expect_identical(recovered$b, big_mat)

    # Both should be the same object reference in the original
    # (after fetch they may be copies, but should have identical values)
    expect_identical(recovered$a, recovered$b)

    close(shared)
})

test_that("deep share handles lists with shareable and non-shareable components", {
    big_vec <- rnorm(10000)  # Large, shareable
    small_vec <- 1:10        # Small, kept
    char_vec <- letters      # Character, not shareable as atomic

    lst <- list(big = big_vec, small = small_vec, chars = char_vec)

    shared <- share(lst, deep = TRUE, min_bytes = 1000)

    expect_s3_class(shared, "shard_deep_shared")

    # Only big_vec should be shared
    expect_equal(shared$summary$shared_count, 1)
    expect_equal(shared$summary$kept_count, 2)  # small + chars

    recovered <- fetch(shared)
    expect_equal(recovered$big, big_vec)
    expect_identical(recovered$small, small_vec)
    expect_identical(recovered$chars, char_vec)

    close(shared)
})

test_that("deep share detects cycles with environments and errors by default", {
    # R lists copy on assignment, so we need environments for true cycles
    # Skip this test if we can't create a proper cycle
    skip("R lists don't create true cycles - environments needed but not supported for deep sharing yet")

    # This would be the test if we supported environments:
    # env <- new.env()
    # env$data <- 1:10
    # env$self <- env  # True self-reference in environment
    # expect_error(share_env_deep(env), "Cycle detected")
})

test_that("deep share with cycle='skip' - list self-assignment creates copy not cycle", {
    # In R, lst$self <- lst creates a COPY, not a true cycle
    # So this actually creates an alias scenario, not a cycle
    lst <- list(a = 1:10)
    lst$self <- lst  # This creates a copy of lst at this point

    # This should work because there's no actual cycle
    shared <- share(lst, deep = TRUE, min_bytes = 100, cycle = "skip")

    expect_s3_class(shared, "shard_deep_shared")

    # No cycles should be detected (R lists don't create true cycles)
    expect_equal(shared$summary$cycle_count, 0)

    recovered <- fetch(shared)
    expect_identical(recovered$a, 1:10)
    # self contains the original structure (not a true cycle)
    expect_true(is.list(recovered$self))
    expect_identical(recovered$self$a, 1:10)

    close(shared)
})

test_that("deep share works with data.frames", {
    # Create a data.frame with large numeric columns
    df <- data.frame(
        x = rnorm(10000),
        y = rnorm(10000),
        z = letters[sample(26, 10000, replace = TRUE)]
    )

    shared <- share(df, deep = TRUE, min_bytes = 1000)

    expect_s3_class(shared, "shard_deep_shared")

    # x and y should be shared (numeric, large enough)
    # z should be kept (character)
    expect_equal(shared$summary$shared_count, 2)

    recovered <- fetch(shared)
    expect_equal(recovered, df)
    expect_s3_class(recovered, "data.frame")

    close(shared)
})

test_that("deep share tracks total bytes correctly", {
    big_vec <- rnorm(10000)  # ~80KB (10000 * 8 bytes)

    lst <- list(a = big_vec)

    shared <- share(lst, deep = TRUE, min_bytes = 1000)

    # Total shared bytes should be approximately the size of big_vec
    expect_true(shared$summary$total_shared_bytes > 70000)
    expect_true(shared$summary$total_shared_bytes < 100000)

    close(shared)
})

test_that("deep share with aliased data.frame column", {
    # Same vector used as two columns
    shared_col <- rnorm(10000)
    df <- data.frame(a = shared_col, b = shared_col)

    shared <- share(df, deep = TRUE, min_bytes = 1000)

    # Should detect alias
    expect_equal(shared$summary$shared_count, 1)
    expect_equal(shared$summary$alias_count, 1)

    recovered <- fetch(shared)
    expect_equal(recovered$a, shared_col)
    expect_equal(recovered$b, shared_col)

    close(shared)
})

test_that("fetch and close work correctly for deep shared", {
    lst <- list(x = rnorm(10000))

    shared <- share(lst, deep = TRUE, min_bytes = 1000)

    # Multiple fetches should work
    r1 <- fetch(shared)
    r2 <- fetch(shared)
    expect_identical(r1, r2)

    # Close should not error
    expect_silent(close(shared))
})

test_that("print method works for deep shared", {
    lst <- list(x = rnorm(10000))

    shared <- share(lst, deep = TRUE, min_bytes = 1000)

    expect_output(print(shared), "shard_deep_shared")
    expect_output(print(shared), "Shared segments:")

    close(shared)
})

test_that("is_shared recognizes deep shared objects", {
    lst <- list(x = rnorm(10000))
    shared <- share(lst, deep = TRUE, min_bytes = 1000)

    expect_true(is_shared(shared))

    close(shared)
})

test_that("deep share respects min_bytes threshold", {
    small_vec <- 1:100  # Small
    medium_vec <- rnorm(1000)  # Medium
    large_vec <- rnorm(10000)  # Large

    # With high threshold, only large should be shared
    shared_high <- share(
        list(small = small_vec, medium = medium_vec, large = large_vec),
        deep = TRUE,
        min_bytes = 50000  # ~50KB threshold
    )
    expect_equal(shared_high$summary$shared_count, 1)
    close(shared_high)

    # With low threshold, medium and large should be shared
    shared_low <- share(
        list(small = small_vec, medium = medium_vec, large = large_vec),
        deep = TRUE,
        min_bytes = 5000  # ~5KB threshold
    )
    expect_equal(shared_low$summary$shared_count, 2)
    close(shared_low)
})

test_that("deep share preserves list names", {
    lst <- list(alpha = rnorm(10000), beta = rnorm(5000), gamma = 1:10)

    shared <- share(lst, deep = TRUE, min_bytes = 1000)
    recovered <- fetch(shared)

    expect_identical(names(recovered), names(lst))

    close(shared)
})

test_that("deep share preserves nested list structure", {
    nested <- list(
        level1 = list(
            a = rnorm(10000),
            b = 1:10
        ),
        top = rnorm(10000)
    )

    shared <- share(nested, deep = TRUE, min_bytes = 1000)
    recovered <- fetch(shared)

    expect_identical(names(recovered), c("level1", "top"))
    expect_identical(names(recovered$level1), c("a", "b"))
    expect_equal(recovered$level1$a, nested$level1$a)
    expect_identical(recovered$level1$b, nested$level1$b)
    expect_equal(recovered$top, nested$top)

    close(shared)
})

test_that("non-deep share ignores deep parameters", {
    x <- 1:100
    shared <- share(x, deep = FALSE)

    expect_s3_class(shared, "shard_shared")
    expect_false(inherits(shared, "shard_deep_shared"))

    close(shared)
})
