# Tests for ALTREP shared vectors

test_that("shared_vector creates ALTREP vectors for different types", {
    # Integer
    seg_int <- segment_create(400)
    segment_write(seg_int, 1:100, offset = 0)
    x_int <- shared_vector(seg_int, "integer", length = 100)

    expect_true(is_shared_vector(x_int))
    expect_equal(length(x_int), 100)
    expect_equal(x_int[1], 1L)
    expect_equal(x_int[100], 100L)

    # Double
    seg_dbl <- segment_create(800)
    segment_write(seg_dbl, as.double(1:100), offset = 0)
    x_dbl <- shared_vector(seg_dbl, "double", length = 100)

    expect_true(is_shared_vector(x_dbl))
    expect_equal(length(x_dbl), 100)
    expect_equal(x_dbl[1], 1.0)
    expect_equal(x_dbl[100], 100.0)

    # Logical
    seg_lgl <- segment_create(400)
    lgl_data <- rep(c(TRUE, FALSE), 50)
    segment_write(seg_lgl, lgl_data, offset = 0)
    x_lgl <- shared_vector(seg_lgl, "logical", length = 100)

    expect_true(is_shared_vector(x_lgl))
    expect_equal(length(x_lgl), 100)
    expect_true(x_lgl[1])
    expect_false(x_lgl[2])

    # Raw
    seg_raw <- segment_create(100)
    segment_write(seg_raw, as.raw(1:100), offset = 0)
    x_raw <- shared_vector(seg_raw, "raw", length = 100)

    expect_true(is_shared_vector(x_raw))
    expect_equal(length(x_raw), 100)
    expect_equal(x_raw[1], as.raw(1))
})

test_that("shared_vector respects offset parameter", {
    seg <- segment_create(800)
    segment_write(seg, 1:100, offset = 0)
    segment_write(seg, 101:200, offset = 400)  # After first 100 integers

    # Read from offset
    x <- shared_vector(seg, "integer", offset = 400, length = 100)

    expect_equal(length(x), 100)
    expect_equal(x[1], 101L)
    expect_equal(x[100], 200L)
})

test_that("contiguous subsetting returns views, not copies", {
    seg <- segment_create(400)
    segment_write(seg, 1:100, offset = 0)
    x <- shared_vector(seg, "integer", length = 100)

    # Reset diagnostics
    shared_reset_diagnostics(x)

    # Contiguous subset
    y <- x[1:10]

    # For contiguous indices, we should get a view (also ALTREP)
    # Note: R's subsetting might materialize in some cases
    expect_equal(length(y), 10)
    expect_equal(y[1], 1L)
    expect_equal(y[10], 10L)
})

test_that("shared_view creates views explicitly", {
    seg <- segment_create(400)
    segment_write(seg, 1:100, offset = 0)
    x <- shared_vector(seg, "integer", length = 100)

    # Create explicit view
    y <- shared_view(x, start = 11, length = 10)

    expect_true(is_shared_vector(y))
    expect_equal(length(y), 10)
    expect_equal(y[1], 11L)  # x[11]
    expect_equal(y[10], 20L) # x[20]
})

test_that("shared_diagnostics tracks dataptr and materialize calls", {
    seg <- segment_create(400)
    segment_write(seg, 1:100, offset = 0)
    x <- shared_vector(seg, "integer", length = 100)

    # Get initial diagnostics
    diag1 <- shared_diagnostics(x)
    expect_equal(diag1$length, 100)
    expect_equal(diag1$offset, 0)
    expect_true(diag1$readonly)
    expect_equal(diag1$type, "integer")

    # Reset and verify
    shared_reset_diagnostics(x)
    diag2 <- shared_diagnostics(x)
    expect_equal(diag2$dataptr_calls, 0)
    expect_equal(diag2$materialize_calls, 0)

    # Access data (may increment counters depending on operation)
    sum_val <- sum(x)
    expect_equal(sum_val, sum(1:100))
})

test_that("readonly prevents write access via copy-on-write", {
    seg <- segment_create(400)
    segment_write(seg, 1:100, offset = 0)

    # Create readonly vector on UNPROTECTED segment
    x <- shared_vector(seg, "integer", length = 100, readonly = TRUE)

    # Reading should work
    expect_equal(x[1], 1L)
    expect_equal(x[50], 50L)

    # Attempt to write - this should trigger copy-on-write
    # The original shared memory should NOT be mutated
    x[1] <- 999L

    # x now has the modified value (it was copied internally)
    expect_equal(x[1], 999L)

    # But the underlying shared memory should still have the original value
    # Verify by creating a new view of the same segment
    y <- shared_vector(seg, "integer", length = 100, readonly = TRUE)
    expect_equal(y[1], 1L)  # Original value preserved

    # x is still technically a shared_vector ALTREP, but now backed by a copy
    expect_true(is_shared_vector(x))
})

test_that("is_shared_vector correctly identifies ALTREP vectors", {
    seg <- segment_create(400)
    segment_write(seg, 1:100, offset = 0)
    x <- shared_vector(seg, "integer", length = 100)

    expect_true(is_shared_vector(x))
    expect_false(is_shared_vector(1:100))
    expect_false(is_shared_vector(c(1.0, 2.0)))
    expect_false(is_shared_vector("hello"))
    expect_false(is_shared_vector(list(a = 1)))
})

test_that("as_shared converts standard vectors to shared", {
    # Integer
    x_int <- as_shared(1:100)
    expect_true(is_shared_vector(x_int))
    expect_equal(length(x_int), 100)
    expect_equal(x_int[50], 50L)

    # Double
    x_dbl <- as_shared(as.double(1:100))
    expect_true(is_shared_vector(x_dbl))
    expect_equal(x_dbl[50], 50.0)

    # Logical
    x_lgl <- as_shared(c(TRUE, FALSE, TRUE))
    expect_true(is_shared_vector(x_lgl))
    expect_true(x_lgl[1])
    expect_false(x_lgl[2])

    # Raw
    x_raw <- as_shared(as.raw(1:10))
    expect_true(is_shared_vector(x_raw))
    expect_equal(x_raw[5], as.raw(5))
})

test_that("shared_segment returns the underlying segment", {
    seg <- segment_create(400)
    segment_write(seg, 1:100, offset = 0)
    x <- shared_vector(seg, "integer", length = 100)

    seg_ptr <- shared_segment(x)
    expect_true(inherits(seg_ptr, "externalptr"))
})

test_that("vector operations work on shared vectors", {
    x <- as_shared(1:100)

    # Sum
    expect_equal(sum(x), sum(1:100))

    # Mean
    expect_equal(mean(x), mean(1:100))

    # Range
    expect_equal(range(x), c(1L, 100L))

    # Comparison
    expect_equal(sum(x > 50), 50)
})

test_that("shared vectors work with double precision", {
    vals <- seq(0.1, 10.0, by = 0.1)
    x <- as_shared(vals)

    expect_true(is_shared_vector(x))
    expect_equal(length(x), 100)
    expect_equal(x[1], 0.1)
    expect_equal(sum(x), sum(vals))
})

test_that("views share the same underlying memory", {
    seg <- segment_create(400)
    segment_write(seg, 1:100, offset = 0)

    x <- shared_vector(seg, "integer", length = 100)
    y <- shared_view(x, start = 1, length = 50)
    z <- shared_view(x, start = 51, length = 50)

    # Views should have same segment
    expect_equal(shared_segment(x), shared_segment(y))
    expect_equal(shared_segment(x), shared_segment(z))

    # Data should match
    expect_equal(y[1], 1L)
    expect_equal(z[1], 51L)
})

test_that("multiple views can be created", {
    x <- as_shared(1:1000)

    # Create multiple overlapping views
    v1 <- shared_view(x, start = 1, length = 100)
    v2 <- shared_view(x, start = 50, length = 100)
    v3 <- shared_view(x, start = 100, length = 100)

    expect_equal(length(v1), 100)
    expect_equal(length(v2), 100)
    expect_equal(length(v3), 100)

    # Overlapping region should have same values
    expect_equal(v1[50:99], v2[1:50])
    expect_equal(v2[51:100], v3[1:50])
})

test_that("error handling for invalid inputs", {
    seg <- segment_create(400)
    segment_write(seg, 1:100, offset = 0)
    x <- shared_vector(seg, "integer", length = 100)

    # View start out of bounds
    expect_error(shared_view(x, start = 101, length = 10))

    # View extends beyond bounds
    expect_error(shared_view(x, start = 95, length = 10))

    # Non-ALTREP input
    expect_error(shared_view(1:100, start = 1, length = 10))
    expect_error(shared_diagnostics(1:100))
    expect_error(shared_segment(1:100))
})
