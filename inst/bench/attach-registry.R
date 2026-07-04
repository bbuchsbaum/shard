# Microbenchmarks for Phase 3 items 3.4-3.7 (attach registry, GC-native
# ownership, cached base pointer).
#
# Run with the package loaded (library(shard) or devtools::load_all()):
#   Rscript -e 'devtools::load_all("."); source("inst/bench/attach-registry.R")'
#
# Three benchmarks:
#   (a) repeated attach/detach of the same segment (10k iterations)  [3.4]
#   (b) per-element access on a deep view chain                      [3.7]
#   (c) create + destroy 5k views (finalization cost)                [3.6]

suppressMessages({
    ok <- requireNamespace("shard", quietly = TRUE)
    if (!ok) stop("shard must be installed/loaded")
})

ns <- asNamespace("shard")
has_stats <- exists("shard_shm_registry_stats_", envir = ns)

timeit <- function(expr) {
    gc(FALSE)
    t <- system.time(expr)
    t[["elapsed"]]
}

cat("== shard attach/view microbenchmarks ==\n")
cat(sprintf("registry stats available: %s\n\n", has_stats))

## ---------------------------------------------------------------- (a) attach
n_attach <- 10000L
seg <- shard::segment_create(1024 * 1024, backing = "mmap")
shard::segment_write(seg, as.raw(rep(1:255, length.out = 1024)))
path <- shard::segment_path(seg)

# Keep one attached handle alive so registry hits are possible after 3.4.
hold <- shard::segment_open(path, backing = "mmap", readonly = TRUE)

if (has_stats) s0 <- ns$shard_shm_registry_stats_()
el_a <- timeit({
    for (i in seq_len(n_attach)) {
        h <- shard::segment_open(path, backing = "mmap", readonly = TRUE)
        shard::segment_close(h, unlink = FALSE)
    }
})
cat(sprintf("(a) %d x segment_open+close of same 1MB segment: %.3f s (%.1f us/attach)\n",
            n_attach, el_a, 1e6 * el_a / n_attach))
if (has_stats) {
    s1 <- ns$shard_shm_registry_stats_()
    cat(sprintf("    registry: attach_calls +%d, map_calls +%d, hits +%d\n",
                s1$attach_calls - s0$attach_calls,
                s1$map_calls - s0$map_calls,
                s1$attach_hits - s0$attach_hits))
}
shard::segment_close(hold, unlink = FALSE)
shard::segment_close(seg)

## ------------------------------------- (a2) ALTREP unserialize attach path
# This is the actual worker hot path: every task that receives a shared
# object unserializes a descriptor and re-attaches the segment.
n_unser <- 10000L
xa <- shard::as_shared(as.double(seq_len(1e5)), readonly = TRUE)
sa <- serialize(xa, NULL)
hold2 <- unserialize(sa)  # keep one mapping live so hits are possible

if (has_stats) s0 <- ns$shard_shm_registry_stats_()
el_a2 <- timeit({
    acc <- 0
    for (i in seq_len(n_unser)) {
        y <- unserialize(sa)
        acc <- acc + y[[1L]]
    }
    stopifnot(acc == n_unser)
})
cat(sprintf("(a2) %d x unserialize(shared vector) [worker attach path]: %.3f s (%.1f us/attach)\n",
            n_unser, el_a2, 1e6 * el_a2 / n_unser))
if (has_stats) {
    s1 <- ns$shard_shm_registry_stats_()
    cat(sprintf("    registry: attach_calls +%d, map_calls +%d, hits +%d\n",
                s1$attach_calls - s0$attach_calls,
                s1$map_calls - s0$map_calls,
                s1$attach_hits - s0$attach_hits))
}
rm(hold2, xa, sa)
invisible(gc(FALSE)); invisible(gc(FALSE))

## ------------------------------------------------------- (b) deep chain Elt
depth <- 64L
n_elt <- 20000L
x <- shard::as_shared(as.double(seq_len(1e5)), readonly = TRUE)
v <- x
for (d in seq_len(depth)) {
    # Each view starts one element later: depth-64 parent chain.
    v <- shard::shared_view(v, start = 2, length = length(v) - 1L)
}
stopifnot(v[[1]] == depth + 1)

el_b <- timeit({
    acc <- 0
    for (i in seq_len(n_elt)) {
        acc <- acc + v[[(i %% 1000L) + 1L]]
    }
})
cat(sprintf("(b) %d single-element reads on depth-%d view chain: %.3f s (%.2f us/elt)\n",
            n_elt, depth, el_b, 1e6 * el_b / n_elt))

# Also region-based access (sum) on the deep chain
n_sum <- 2000L
el_b2 <- timeit({
    for (i in seq_len(n_sum)) invisible(sum(v))
})
cat(sprintf("    %d x sum() on depth-%d chain (n=%d): %.3f s\n",
            n_sum, depth, length(v), el_b2))
rm(v, x)
invisible(gc(FALSE)); invisible(gc(FALSE))

## -------------------------------------------------- (c) create/destroy views
n_views <- 5000L
x <- shard::as_shared(as.double(seq_len(1e5)), readonly = TRUE)
views <- vector("list", n_views)
el_c1 <- timeit({
    for (i in seq_len(n_views)) {
        views[[i]] <- shard::shared_view(x, start = (i %% 1000L) + 1L, length = 16L)
    }
})
el_c2 <- timeit({
    rm(views)
    invisible(gc(FALSE))
    invisible(gc(FALSE))
})
cat(sprintf("(c) create %d views: %.3f s; destroy (rm+2xgc): %.3f s\n",
            n_views, el_c1, el_c2))
rm(x)
invisible(gc(FALSE))

cat("\ndone.\n")
