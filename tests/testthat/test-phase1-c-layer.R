# Phase 1 (Group A) C-layer regression tests:
#   1.1  XLENGTH long-vector fix in the segment write path
#   1.9  cow='deny' enforced at C level (writable DATAPTR errors)
#   1.11 coercion does not misreport as materialization

# --- 1.1: long-vector segment write ------------------------------------------
#
# Writing a vector with more than 2^31 - 1 elements used to go through
# LENGTH(), which cannot represent long-vector lengths. A raw vector is the
# cheapest type that exercises the fixed path (~2.1 GB for the vector plus
# ~2.1 GB for the segment). Opt in explicitly on machines with enough RAM:
#   SHARD_TEST_LONG_VECTORS=1
test_that("segment write/read handles long vectors (> 2^31 elements)", {
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("SHARD_TEST_LONG_VECTORS"), "1"),
    "Set SHARD_TEST_LONG_VECTORS=1 to run (needs ~5 GB of free RAM)"
  )

  n <- 2^31 + 5
  x <- raw(n)
  x[1] <- as.raw(7)
  x[n - 1] <- as.raw(42)
  x[n] <- as.raw(99)

  seg <- segment_create(n)
  on.exit(segment_close(seg), add = TRUE)

  bytes <- segment_write(seg, x, offset = 0)
  # Pre-fix, LENGTH() could not represent the length: the reported byte
  # count (and the memcpy) was truncated/failed for long vectors.
  expect_equal(as.numeric(bytes), n)
  rm(x)
  gc()

  # Bytes beyond the 2^31 boundary must have been written.
  tail_bytes <- segment_read(seg, offset = n - 2, size = 2)
  expect_identical(tail_bytes, as.raw(c(42, 99)))
  head_byte <- segment_read(seg, offset = 0, size = 1)
  expect_identical(head_byte, as.raw(7))

  # An ALTREP wrapper over the full long segment sees the right length
  # and the right tail elements without materializing.
  sv <- shared_vector(seg, "raw", length = n)
  expect_equal(length(sv), n)
  expect_true(sv[n] == as.raw(99))
  expect_true(sv[n - 1] == as.raw(42))
})

# --- 1.9: cow='deny' -- shared bytes must be unreachable for writes -----------
#
# NOTE (deviation from the plan spec): the plan proposed Rf_error() on any
# writable DATAPTR request for deny vectors. Empirically (R 4.x), base R
# requests writable DATAPTR from purely read-only paths -- identical(),
# range(), `==`, which.max() -- so a hard error makes deny vectors unusable
# for ordinary reads (and breaks worker-side borrows, which unserialize with
# deny_write=1). The enforced invariant is instead: a bypassed write can only
# ever touch a private materialized copy, never the shared segment. These
# tests pin that invariant down at the C level.
test_that("cow='deny': bypassed writes never reach the shared segment", {
  x <- as_shared(1:10, readonly = TRUE, cow = "deny")
  seg <- shared_segment(x)

  # Bypass S3 dispatch: unclass() gives a shard ALTREP without the
  # shard_shared_vector class, so [<- goes straight to the internal
  # subassignment, which requests a writable data pointer at the C level.
  y <- unclass(x)
  expect_true(is_shared_vector(y))
  y[1] <- 99L

  # The write landed in a private COW copy only: the original vector and
  # the underlying shared segment bytes are untouched.
  expect_equal(as.integer(x[1]), 1L)
  expect_identical(
    segment_read(seg, offset = 0, size = 4),
    writeBin(1L, raw())
  )
  expect_equal(sum(x), sum(1:10))
})

test_that("cow='deny': bypassed writes through views never reach the segment", {
  x <- as_shared(as.double(1:20), readonly = TRUE, cow = "deny")
  v <- shared_view(x, start = 5, length = 10)

  y <- unclass(v)
  expect_true(is_shared_vector(y))
  y[1] <- -1

  expect_equal(as.double(v[1]), 5)
  expect_equal(as.double(x[5]), 5)
})

test_that("cow='deny' still errors for classed (R-level) mutation", {
  x <- share(1:10, readonly = TRUE)
  expect_error(x[1] <- 99L, "cow='deny'")

  z <- as_shared(1:10, readonly = TRUE, cow = "deny")
  expect_error(z[1] <- 99L, "cow")
})

test_that("read-only operations on cow='deny' vectors work", {
  x <- as_shared(as.double(1:100), readonly = TRUE, cow = "deny")
  y <- as_shared(as.double(1:100), readonly = TRUE, cow = "deny")
  expect_equal(sum(x), sum(1:100))
  expect_equal(range(x), c(1, 100))
  expect_true(identical(as.double(x[10]), 10))
  expect_true(all(x == y))
  m <- .Call("C_shard_altrep_materialize", x, PACKAGE = "shard")
  expect_identical(m, as.double(1:100))
})

# --- 1.11: coercion is not a materialization ----------------------------------
test_that("coercion does not bump materialize_calls (counts as coerce_calls)", {
  x <- as_shared(1:100, readonly = TRUE)
  shared_reset_diagnostics(x)

  y <- as.numeric(x)
  expect_identical(y, as.numeric(1:100))

  d <- shared_diagnostics(x)
  expect_equal(d$materialize_calls, 0)
  expect_equal(d$coerce_calls, 1)
})

test_that("shared_reset_diagnostics clears coerce_calls", {
  x <- as_shared(1:50, readonly = TRUE)
  invisible(as.numeric(x))
  expect_gt(shared_diagnostics(x)$coerce_calls, 0)

  shared_reset_diagnostics(x)
  d <- shared_diagnostics(x)
  expect_equal(d$coerce_calls, 0)
  expect_equal(d$materialize_calls, 0)
  expect_equal(d$dataptr_calls, 0)
})
