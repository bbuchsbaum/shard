# Closed-segment repair tests.
#
# Before 0.2.1 these were memory-safety bugs rather than errors:
#
#   * A read before close() cached the resolved base pointer. close() unmapped
#     the segment without invalidating that cache, so the next read dereferenced
#     unmapped memory -- a segfault when the address was still unmapped, silent
#     garbage once the OS reused it.
#
#   * When resolution did correctly fail, the Elt methods returned NA and the
#     Get_region methods returned a short count without filling the caller's
#     buffer, so sum()/mean()/max() silently consumed uninitialized memory.
#
# Every dereferencing access path must now raise the same error instead.
# The crashing same-object, failed-resolution, materialization, and view cases
# were measured against 0.2.0 and are bug discriminators. The zero-length case
# also passed there; it is a compatibility guardrail for the repair.

closed_msg <- "no longer valid"

test_that("reading after close() errors instead of dereferencing a stale pointer", {
    # The crashing order specifically: a read populates the pointer cache
    # BEFORE the segment is unmapped.
    x <- share(1:4)
    expect_equal(x[[1L]], 1L)
    close(x)
    expect_error(x[[1L]], closed_msg)
})

test_that("reading after close() errors with no prior read", {
    # Same expectation on the path where nothing was ever cached.
    x <- share(1:4)
    close(x)
    expect_error(x[[1L]], closed_msg)
})

test_that("Get_region consumers error rather than reading uninitialized memory", {
    # sum()/mean()/max() reach the data through ALTREP Get_region, which used
    # to return 0 and leave R's region buffer untouched.
    x <- share(as.numeric(1:1000))
    expect_equal(sum(x), 500500)
    close(x)
    expect_error(sum(x), closed_msg)

    y <- share(as.numeric(1:1000))
    close(y)
    expect_error(sum(y), closed_msg)
    expect_error(mean(y), closed_msg)
    expect_error(max(y), closed_msg)
})

test_that("materializing copies after close() errors rather than returning zeros", {
    # as.integer()/duplicate went through memcpy-if-non-NULL, which silently
    # yielded a freshly allocated (zero-filled) vector.
    x <- share(1:4)
    expect_equal(x[[1L]], 1L)
    close(x)
    expect_error(as.integer(x), closed_msg)

    # Same path with nothing cached beforehand.
    y <- share(1:4)
    close(y)
    expect_error(as.integer(y), closed_msg)
})

test_that("the DATAPTR path errors after close()", {
    # This must force DATAPTR on the SAME object that did the priming read.
    # unclass() would not: it goes through a shallow altrep_duplicate(), which
    # builds a fresh info with an empty pointer cache, so the object being
    # dereferenced was never primed and the stale-cache path goes untested.
    # range()/which.max() request a writable DATAPTR on x itself -- see the
    # comment on altvec_dataptr() in src/shard_altrep.c.
    x <- share(1:4)
    expect_equal(x[[1L]], 1L)
    close(x)
    expect_error(range(x), closed_msg)

    y <- share(1:4)
    expect_equal(y[[1L]], 1L)
    close(y)
    expect_error(which.max(y), closed_msg)
})

test_that("views error after their parent segment is closed", {
    x <- share(as.numeric(1:1000))
    expect_equal(x[[1L]], 1)
    v <- x[10:20]
    expect_equal(v[[1L]], 10)

    close(x)
    expect_error(v[[1L]], closed_msg)
})

test_that("views error after close() even when never read beforehand", {
    x <- share(as.numeric(1:1000))
    v <- x[10:20]
    close(x)
    expect_error(v[[1L]], closed_msg)
})

test_that("close-then-read errors for every supported element type", {
    seg_int <- segment_create(40)
    segment_write(seg_int, 1:10, offset = 0)
    x_int <- shared_vector(seg_int, "integer", length = 10)

    seg_dbl <- segment_create(80)
    segment_write(seg_dbl, as.double(1:10), offset = 0)
    x_dbl <- shared_vector(seg_dbl, "double", length = 10)

    seg_lgl <- segment_create(40)
    segment_write(seg_lgl, rep(c(TRUE, FALSE), 5), offset = 0)
    x_lgl <- shared_vector(seg_lgl, "logical", length = 10)

    seg_raw <- segment_create(10)
    segment_write(seg_raw, as.raw(1:10), offset = 0)
    x_raw <- shared_vector(seg_raw, "raw", length = 10)

    # Read each one first so the pointer cache is populated.
    expect_equal(x_int[[1L]], 1L)
    expect_equal(x_dbl[[1L]], 1)
    expect_true(x_lgl[[1L]])
    expect_equal(x_raw[[1L]], as.raw(1L))

    segment_close(seg_int)
    segment_close(seg_dbl)
    segment_close(seg_lgl)
    segment_close(seg_raw)

    expect_error(x_int[[1L]], closed_msg)
    expect_error(x_dbl[[1L]], closed_msg)
    expect_error(x_lgl[[1L]], closed_msg)
    expect_error(x_raw[[1L]], closed_msg)
})

test_that("length() still works on a closed shared vector", {
    # Length is metadata; it never touches the mapping and must not start
    # erroring just because the segment is gone.
    x <- share(1:4)
    close(x)
    expect_equal(length(x), 4L)
})

test_that("closing one handle does not invalidate a vector on another mapping", {
    # The invalidation is global (a generation bump), so confirm it only forces
    # re-resolution and does not wrongly kill unrelated live vectors.
    a <- share(1:4)
    b <- share(5:8)

    expect_equal(a[[1L]], 1L)
    expect_equal(b[[1L]], 5L)

    close(a)

    expect_error(a[[1L]], closed_msg)
    expect_equal(b[[1L]], 5L)
    expect_equal(sum(b), 26L)

    close(b)
})

test_that("closing one registry handle leaves the shared mapping usable", {
    # The attach registry lets several handles share one mapping. Closing a
    # handle with refcount > 1 must invalidate only the vectors resolving
    # through that handle -- the mapping itself stays alive for the others.
    p <- tempfile()
    s1 <- segment_create(80, backing = "mmap", path = p)
    segment_write(s1, as.numeric(1:10), offset = 0)

    s2 <- segment_open(p, backing = "mmap")
    s3 <- segment_open(p, backing = "mmap")
    v2 <- shared_vector(s2, "double", length = 10)
    v3 <- shared_vector(s3, "double", length = 10)

    expect_equal(v2[[1L]], 1)
    expect_equal(v3[[1L]], 1)

    segment_close(s2)

    expect_error(v2[[1L]], closed_msg)
    expect_equal(v3[[1L]], 1)
    expect_equal(sum(v3), 55)

    segment_close(s3)
    expect_error(v3[[1L]], closed_msg)

    segment_close(s1)
})

test_that("zero-length shared vectors stay usable after close()", {
    # Nothing here dereferences the mapping, so these must not error just
    # because the segment is gone.
    z <- share(integer(0))
    close(z)

    expect_equal(length(z), 0L)
    expect_equal(sum(z), 0L)
    expect_equal(z[integer(0)], integer(0))
    expect_equal(as.integer(z), integer(0))
    expect_equal(materialize(z), integer(0))
    roundtrip <- unserialize(serialize(z, NULL))
    expect_type(roundtrip, "integer")
    expect_length(roundtrip, 0L)
})

test_that("a private copy does not keep a closed vector readable", {
    # A writable DATAPTR request stores a private copy in data2. Closing the
    # public object is still terminal: cache invalidation must not resolve to
    # that copy and silently keep serving values after the segment is gone.
    x <- share(as.numeric(1:10))
    expect_equal(range(x), c(1, 10))
    expect_gt(shared_diagnostics(x)$materialize_calls, 0)

    close(x)
    expect_error(x[[1L]], closed_msg)
    expect_error(range(x), closed_msg)
})

test_that("segment_write reports what overflowed the segment", {
    # "Write failed" gave no way to tell an oversized write from a broken
    # mapping; buffer(init = <vector>) landed here and read as a shard bug.
    seg <- segment_create(16)
    on.exit(segment_close(seg), add = TRUE)

    expect_error(
        segment_write(seg, 1:100, offset = 0),
        "exceeds segment size"
    )
})
