# Phase 3 (items 3.4-3.7): attach registry, GC-native ownership, cached base
# pointer. These tests pin the lifetime/staleness invariants, not timing.

registry_stats <- function() shard:::shard_shm_registry_stats_()

test_that("registry stats are exposed with expected fields", {
    s <- registry_stats()
    expect_named(s, c("attach_calls", "attach_hits", "map_calls",
                      "live_segments", "live_refs"))
    expect_true(all(vapply(s, is.numeric, logical(1))))
})

test_that("repeated ALTREP unserialize reuses one mapping (3.4)", {
    skip_on_os("windows") # registry is a POSIX-only optimization

    x <- as_shared(as.double(1:1000), readonly = TRUE)
    s <- serialize(x, NULL)

    s0 <- registry_stats()
    y1 <- unserialize(s)
    y2 <- unserialize(s)
    y3 <- unserialize(s)
    s1 <- registry_stats()

    # Three attaches, exactly one real mmap: the 2nd and 3rd are registry hits.
    expect_identical(s1$attach_calls - s0$attach_calls, 3)
    expect_identical(s1$map_calls - s0$map_calls, 1)
    expect_identical(s1$attach_hits - s0$attach_hits, 2)

    # All handles read the same, correct data.
    expect_identical(sum(y1), sum(as.double(1:1000)))
    expect_identical(y2[[500]], 500)
    expect_identical(y3[[1000]], 1000)

    # Dropping all attached handles drains the registry (unmap at zero).
    rm(y1, y2, y3)
    for (i in 1:5) gc(FALSE)
    s2 <- registry_stats()
    expect_identical(s2$live_segments, 0)
    expect_identical(s2$live_refs, 0)
})

test_that("repeated segment_open of the same path shares one mapping (3.4)", {
    skip_on_os("windows")

    seg <- segment_create(4096, backing = "mmap")
    segment_write(seg, as.double(1:16))
    path <- segment_path(seg)

    hold <- segment_open(path, backing = "mmap", readonly = TRUE)
    s0 <- registry_stats()
    h1 <- segment_open(path, backing = "mmap", readonly = TRUE)
    h2 <- segment_open(path, backing = "mmap", readonly = TRUE)
    s1 <- registry_stats()

    expect_identical(s1$map_calls - s0$map_calls, 0)
    expect_identical(s1$attach_hits - s0$attach_hits, 2)

    # Closing one shared handle must not invalidate the others.
    segment_close(h1, unlink = FALSE)
    got <- segment_read(h2, offset = 0, size = 16 * 8)
    expect_identical(readBin(got, "double", 16), as.double(1:16))

    segment_close(h2, unlink = FALSE)
    segment_close(hold, unlink = FALSE)
    segment_close(seg)
})

test_that("stale path (unlink + recreate) never serves old bytes (3.4)", {
    skip_on_os("windows") # relies on POSIX unlink-while-mapped semantics

    path <- tempfile("shard_stale_")

    seg1 <- segment_create(4096, backing = "mmap", path = path)
    segment_write(seg1, as.double(rep(1, 16)))
    old <- segment_open(path, backing = "mmap", readonly = TRUE)

    # Destroy and recreate a DIFFERENT file at the same path.
    segment_close(seg1, unlink = TRUE)
    seg2 <- segment_create(4096, backing = "mmap", path = path)
    segment_write(seg2, as.double(rep(2, 16)))

    fresh <- segment_open(path, backing = "mmap", readonly = TRUE)

    # New attach sees the new inode's data (no stale registry hit) ...
    got_new <- readBin(segment_read(fresh, 0, 16 * 8), "double", 16)
    expect_identical(got_new, as.double(rep(2, 16)))

    # ... while the pre-unlink handle still reads the old (pinned) inode.
    got_old <- readBin(segment_read(old, 0, 16 * 8), "double", 16)
    expect_identical(got_old, as.double(rep(1, 16)))

    segment_close(old, unlink = FALSE)
    segment_close(fresh, unlink = FALSE)
    segment_close(seg2, unlink = TRUE)
})

test_that("two dispatches touching the same shared matrix reuse the mapping in the worker (3.4)", {
    skip_on_cran()
    skip_on_os("windows")

    cl <- parallel::makePSOCKcluster(1L)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Mirror the pool's dev-aware bootstrap so the worker runs the same
    # in-tree code as the master under devtools::load_all().
    dev_path <- tryCatch(getNamespaceInfo("shard", "path"), error = function(e) NULL)
    if (!is.null(dev_path) && !file.exists(file.path(dev_path, ".git"))) {
        dev_path <- NULL
    }
    # NOTE: every function shipped via clusterCall gets a clean environment;
    # otherwise its closure would drag the whole test frame (including the
    # shared vectors) to the worker and distort the attach counters.
    bootstrap <- function(paths, dev_path) {
        .libPaths(paths)
        if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE, compile = FALSE, recompile = FALSE)
        } else {
            suppressPackageStartupMessages(library(shard))
        }
        TRUE
    }
    environment(bootstrap) <- globalenv()
    parallel::clusterCall(cl, bootstrap, .libPaths(), dev_path)

    x <- as_shared(as.double(1:10000), readonly = TRUE)
    # Two distinct views of the same segment serialize as two descriptors,
    # so the worker performs two attaches against one underlying file.
    v1 <- shared_view(x, start = 1, length = 5000)
    v2 <- shared_view(x, start = 5001, length = 5000)

    # NOTE: the views are attached while the worker DESERIALIZES the call
    # arguments, i.e. before the function body runs -- so stats snapshots
    # must bracket each clusterCall from the outside.
    run <- function(views) {
        list(total = sum(views[[1]]) + sum(views[[2]]),
             stats = shard:::shard_shm_registry_stats_())
    }
    environment(run) <- globalenv()
    stats_fn <- function() shard:::shard_shm_registry_stats_()
    environment(stats_fn) <- globalenv()

    s0 <- parallel::clusterCall(cl, stats_fn)[[1]]
    r1 <- parallel::clusterCall(cl, run, list(v1, v2))[[1]]
    r2 <- parallel::clusterCall(cl, run, list(v1, v2))[[1]]

    expect_identical(r1$total, sum(as.double(1:10000)))
    expect_identical(r2$total, sum(as.double(1:10000)))

    a1 <- r1$stats$attach_calls - s0$attach_calls
    m1 <- r1$stats$map_calls - s0$map_calls
    a2 <- r2$stats$attach_calls - r1$stats$attach_calls
    m2 <- r2$stats$map_calls - r1$stats$map_calls

    # Dispatch 1: two views of one segment => 2 attaches, exactly 1 mmap
    # (the second view attaches while the first mapping is live).
    expect_identical(a1, 2)
    expect_identical(m1, 1)
    # Dispatch 2: 2 more attaches, at most 1 mmap (0 when the worker has not
    # yet finalized dispatch 1's handles -- the common case).
    expect_identical(a2, 2)
    expect_lte(m2, 1)
    expect_lt(m1 + m2, a1 + a2)
})

test_that("views keep parents and segment alive across gc (3.6)", {
    x <- as_shared(as.double(1:1e4), readonly = TRUE)
    v <- shared_view(x, start = 101, length = 1000)
    vv <- shared_view(v, start = 11, length = 100)
    vvv <- shared_view(vv, start = 2, length = 50)

    rm(x, v, vv)
    for (i in 1:5) gc(FALSE)

    # Deepest view must still read correctly with every ancestor dropped.
    expect_identical(vvv[[1]], 112)
    expect_identical(sum(vvv), sum(as.double(112:161)))
})

test_that("many views: parents dropped, gc, reads stay correct; finalizers run (3.6)", {
    skip_on_os("windows")

    x <- unserialize(serialize(as_shared(as.double(1:1e4), readonly = TRUE), NULL))
    n_views <- 500L
    views <- vector("list", n_views)
    for (i in seq_len(n_views)) {
        views[[i]] <- shared_view(x, start = i, length = 10)
    }

    s_live <- registry_stats()
    expect_gte(s_live$live_refs, 1)

    rm(x)
    for (i in 1:5) gc(FALSE)

    ok <- vapply(seq_len(n_views),
                 function(i) views[[i]][[1]] == i && sum(views[[i]]) == sum(i:(i + 9)),
                 logical(1))
    expect_true(all(ok))

    # C-level liveness check: dropping the views must eventually release the
    # attached segment (registry drains to zero => finalizers actually ran).
    rm(views)
    for (i in 1:5) gc(FALSE)
    s_done <- registry_stats()
    expect_identical(s_done$live_segments, 0)
    expect_identical(s_done$live_refs, 0)
})

test_that("cached base pointer stays coherent across COW materialization (3.7)", {
    x <- as_shared(as.double(1:100), readonly = TRUE)
    v <- shared_view(x, start = 1, length = 100)

    # Prime the caches via reads.
    expect_identical(x[[1]], 1)
    expect_identical(v[[1]], 1)

    # Force a COW materialization on the parent (writable dataptr).
    w <- unclass(x)
    w[1] <- 999

    # Shared bytes unchanged; the private copy holds the write; the view,
    # whose parent materialized AFTER the view cached its pointer, must have
    # been invalidated by the generation bump and still read shared data
    # consistent with its parent's materialized ancestor rules.
    expect_identical(w[[1]], 999)
    expect_identical(x[[1]], 1)
    expect_identical(v[[1]], 1)
    expect_identical(sum(v), sum(as.double(1:100)))
})

test_that("unrelated caches survive another object's materialization (3.7)", {
    y <- as_shared(as.double(1:100), readonly = TRUE)
    yv <- shared_view(y, start = 11, length = 50)
    # Prime caches.
    expect_identical(y[[2]], 2)
    expect_identical(yv[[1]], 11)

    # Materialize a completely unrelated shared vector: bumps the global
    # generation, which must invalidate-but-not-corrupt y/yv's caches.
    z <- as_shared(as.double(101:200), readonly = TRUE)
    zu <- unclass(z)
    zu[1] <- -1
    expect_identical(zu[[1]], -1)

    expect_identical(y[[2]], 2)
    expect_identical(yv[[1]], 11)
    expect_identical(sum(yv), sum(as.double(11:60)))
    expect_identical(z[[1]], 101)
})
