# Repair tests: segment_protect() applied under a live writable vector.
#
# `info->readonly` is captured when a shared vector is created, but the live
# segment can become protected afterwards. Before 0.2.1, one consumer observed
# the live state while serialization still shipped the stale snapshot. The
# repair makes the segment mapping process-private, serializes its effective
# read-only state, and materializes only for an actual classed mutation.
# The crash and stale serialized-worker cases were measured against 0.2.0 and
# are bug discriminators. The no-materialization assertion also passed there;
# it is a performance guardrail that prevents the crash repair from doubling
# memory for read-only base operations.

test_that("writing through a vector created before segment_protect() does not crash", {
    seg <- segment_create(80)
    on.exit(segment_close(seg), add = TRUE)
    segment_write(seg, as.numeric(1:10), offset = 0)

    v <- shared_vector(seg, "double", length = 10, readonly = FALSE)
    expect_equal(v[[1L]], 1)

    segment_protect(seg)

    # Previously: *** caught bus error ***
    v[1] <- 99
    expect_equal(v[[1L]], 99)
})

test_that("such a write lands in a private copy, not the protected mapping", {
    seg <- segment_create(80)
    on.exit(segment_close(seg), add = TRUE)
    segment_write(seg, as.numeric(1:10), offset = 0)

    v <- shared_vector(seg, "double", length = 10, readonly = FALSE)
    expect_equal(v[[1L]], 1)

    segment_protect(seg)
    v[1] <- 99

    # The shared bytes must be unchanged: a second vector over the same
    # segment still sees the original value.
    other <- shared_vector(seg, "double", length = 10)
    expect_equal(other[[1L]], 1)
    expect_equal(as.numeric(other[1:3]), c(1, 2, 3))
})

test_that("protection survives a serialization round trip", {
    # The serialized descriptor carries the read-only flag, and the receiver
    # re-attaches the segment with it. Shipping the stale creation-time value
    # made the reconstructed vector re-open the segment read-write, so writes
    # went straight through the protection that the local vector honoured by
    # copy-on-write -- and reached the backing file.
    p <- tempfile()
    seg <- segment_create(80, backing = "mmap", path = p)
    on.exit(segment_close(seg), add = TRUE)
    segment_write(seg, as.numeric(1:10), offset = 0)

    v <- shared_vector(seg, "double", length = 10, readonly = FALSE)
    segment_protect(seg)

    v2 <- unserialize(serialize(v, NULL))
    expect_equal(v2[[1L]], 1)

    v2[1] <- -777
    expect_equal(v2[[1L]], -777)

    # The shared bytes, and the file behind them, must be unchanged.
    other <- shared_vector(seg, "double", length = 10)
    expect_equal(other[[1L]], 1)

    con <- file(p, "rb")
    on.exit(close(con), add = TRUE)
    expect_equal(readBin(con, "double", n = 3), c(1, 2, 3))
})

test_that("shared_diagnostics reports the effective read-only state", {
    # Without this a user has no supported way to notice that their writable
    # vector silently became copy-on-write.
    seg <- segment_create(80)
    on.exit(segment_close(seg), add = TRUE)
    segment_write(seg, as.numeric(1:10), offset = 0)

    v <- shared_vector(seg, "double", length = 10, readonly = FALSE)
    expect_false(shared_diagnostics(v)$readonly)

    segment_protect(seg)
    expect_true(shared_diagnostics(v)$readonly)
})

test_that("reads through a pre-protection vector keep working", {
    seg <- segment_create(80)
    on.exit(segment_close(seg), add = TRUE)
    segment_write(seg, as.numeric(1:10), offset = 0)

    v <- shared_vector(seg, "double", length = 10, readonly = FALSE)
    segment_protect(seg)

    expect_equal(v[[5L]], 5)
    expect_equal(sum(v), 55)
    expect_equal(as.numeric(v[1:4]), c(1, 2, 3, 4))
})

test_that("DATAPTR-based reads do not fully materialize after protection", {
    # range(), which.max(), and relational operators ask ALTREP for a
    # writable pointer even though they only read it. The crash fix must not
    # mistake that request for a write and eagerly copy the complete vector.
    make_protected <- function() {
        seg <- segment_create(8000)
        segment_write(seg, as.numeric(1:1000), offset = 0)
        v <- shared_vector(seg, "double", length = 1000, readonly = FALSE)
        segment_protect(seg)
        list(seg = seg, v = v)
    }

    assert_not_materialized <- function(read) {
        fixture <- make_protected()
        on.exit(segment_close(fixture$seg))
        shared_reset_diagnostics(fixture$v)

        value <- read(fixture$v)
        expect_true(length(value) > 0L)
        expect_equal(shared_diagnostics(fixture$v)$materialize_calls, 0)
    }

    for (read in list(
        function(v) range(v),
        function(v) which.max(v),
        function(v) v == 500
    )) {
        assert_not_materialized(read)
    }
})

test_that("bypassed writes after protection cannot reach the backing file", {
    # This is the native safety boundary. unclass() bypasses the package's
    # replacement method, so the C layer itself must expose only a
    # process-private mapping to an operation that will write through it.
    p <- tempfile()
    seg <- segment_create(80, backing = "mmap", path = p)
    on.exit(segment_close(seg), add = TRUE)
    segment_write(seg, as.numeric(1:10), offset = 0)

    v <- shared_vector(seg, "double", length = 10, readonly = FALSE)
    segment_protect(seg)

    y <- unclass(v)
    y[1] <- -777
    expect_equal(y[[1L]], -777)

    con <- file(p, "rb")
    on.exit(close(con), add = TRUE)
    expect_equal(readBin(con, "double", n = 3), c(1, 2, 3))

    # A fresh attachment models another process and must also see the original
    # bytes. The local process-private view may observe an unclass() bypass,
    # but it cannot escape the process or become durable corruption.
    fresh_seg <- segment_open(p, backing = "mmap", readonly = TRUE)
    on.exit(segment_close(fresh_seg), add = TRUE)
    fresh <- shared_vector(fresh_seg, "double", length = 10)
    expect_equal(fresh[[1L]], 1)
})

test_that("shard_map cow='deny' cannot write a stale flag through to disk", {
    p <- tempfile()
    seg <- segment_create(80, backing = "mmap", path = p)
    on.exit(segment_close(seg), add = TRUE)
    on.exit(pool_stop(), add = TRUE)
    segment_write(seg, as.numeric(1:10), offset = 0)

    v <- shared_vector(seg, "double", length = 10, readonly = FALSE,
                       cow = "deny")
    segment_protect(seg)

    result <- shard_map(
        1,
        borrow = list(v = v),
        fun = function(shard, v) {
            # Bypass the R replacement method so this proves the worker's
            # unserialized ALTREP and native COW path, not just S3 dispatch.
            y <- unclass(v)
            y[1] <- -777
            y[[1L]]
        },
        workers = 1,
        cow = "deny"
    )

    expect_true(succeeded(result))
    expect_equal(unname(unlist(results(result))), -777)

    con <- file(p, "rb")
    on.exit(close(con), add = TRUE)
    expect_equal(readBin(con, "double", n = 3), c(1, 2, 3))
})
