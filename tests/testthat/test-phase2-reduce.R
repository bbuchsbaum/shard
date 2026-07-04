# Phase 2 regression tests (reduce bundle):
# - 2.3: shard_reduce's chunk reducer has a minimal, namespace-parented
#        environment (no shard_reduce frame capture; payload independent of
#        the number of shards).
# - F1:  shard_reduce(seed=) uses per-shard L'Ecuyer-CMRG streams
#        (reproducible across worker counts; master RNG untouched).
# - 5.1: chunk_size = "auto" batches (~4 chunks/worker) instead of
#        degenerating to per-shard round trips, and a non-neutral init is
#        applied exactly once regardless of chunking.

test_that("chunk reducer environment is minimal and namespace-parented", {
  kern <- function(s) sum(s$idx)
  comb <- function(a, b) a + b

  r <- make_chunk_reducer_(kern, comb)
  e <- environment(r)

  expect_setequal(ls(e, all.names = TRUE), c("map_fun", "combine_fun"))
  expect_identical(parent.env(e), asNamespace("shard"))
  expect_identical(e$map_fun, kern)
  expect_identical(e$combine_fun, comb)
})

test_that("serialized reducer payload is small and independent of shard count", {
  # Kernel/combine with by-reference environments so only the reducer's own
  # capture is measured (srcrefs stripped: keep.source dev artifact).
  kern <- utils::removeSource(function(s) sum(s$idx))
  environment(kern) <- baseenv()

  # Build the reducer inside a frame holding the full shards + chunks lists
  # plus ballast, exactly the shape of the shard_reduce frame that the old
  # local({...}) reducer dragged into every serialization.
  build <- function(n_shards) {
    desc <- shards(n_shards, block_size = 1L)
    chunks <- create_shard_chunks(desc, 1L, borrow = list(), out = list())
    ballast <- as.numeric(seq_len(500000L))
    make_chunk_reducer_(kern, `+`)
  }

  sz <- function(r) length(serialize(utils::removeSource(r), NULL))

  r_small <- build(10L)
  r_big <- build(2000L)

  expect_lt(sz(r_big), 20 * 1024)
  expect_identical(sz(r_big), sz(r_small))
})

test_that("shard_reduce seed= is reproducible across worker counts (exact arithmetic)", {
  skip_on_cran()

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  blocks <- shards(20, block_size = 1)
  # RNG-using map with exact integer arithmetic: combine order cannot
  # introduce FP noise, so any difference is a seeding difference.
  kern <- function(s) sum(sample.int(1000L, 5L))

  run <- function(workers, chunk_size = 4L, seed = 42) {
    shard_reduce(blocks, map = kern, combine = `+`, init = 0L,
                 workers = workers, chunk_size = chunk_size, seed = seed)$value
  }

  v_w1 <- run(workers = 1)
  v_w2 <- run(workers = 2)
  expect_identical(v_w1, v_w2)

  # Repeated run reproduces exactly; different seed differs.
  expect_identical(run(workers = 2), v_w1)
  expect_false(identical(run(workers = 2, seed = 7), v_w1))
})

test_that("shard_reduce seed= with runif map is exact across worker counts at fixed chunk_size", {
  skip_on_cran()

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  blocks <- shards(24, block_size = 2)
  kern <- function(s) sum(runif(length(s$idx)))

  run <- function(workers) {
    shard_reduce(blocks, map = kern, combine = `+`, init = 0,
                 workers = workers, chunk_size = 3L, seed = 99)$value
  }

  # Partials are folded in chunk order (not arrival order), so with identical
  # chunking the FP combine order is identical for any worker count.
  v1 <- run(1)
  v2 <- run(2)
  expect_identical(v1, v2)
  expect_true(is.finite(v1) && v1 > 0)
})

test_that("shard_reduce(N, seed=) is reproducible across worker counts", {
  skip_on_cran()

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  kern <- function(s) sum(sample.int(100L, 3L))
  r1 <- shard_reduce(20L, map = kern, combine = `+`, init = 0L,
                     workers = 1, chunk_size = 2L, seed = 42)
  r2 <- shard_reduce(20L, map = kern, combine = `+`, init = 0L,
                     workers = 2, chunk_size = 2L, seed = 42)
  expect_identical(r1$value, r2$value)
})

test_that("shard_reduce leaves the master's RNG state untouched", {
  skip_on_cran()

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  old_kind <- RNGkind()

  # seed = NULL must not touch RNG state at all.
  set.seed(123)
  before <- .Random.seed
  r0 <- shard_reduce(4L, map = function(s) s$idx[[1L]], combine = `+`,
                     init = 0L, workers = 1, seed = NULL)
  expect_true(succeeded(r0))
  expect_identical(.Random.seed, before)

  # seed = <int> must save and restore the master state exactly.
  set.seed(456)
  before <- .Random.seed
  r1 <- shard_reduce(4L, map = function(s) sum(runif(2)), combine = `+`,
                     init = 0, workers = 1, seed = 42)
  expect_true(succeeded(r1))
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), old_kind)
})

test_that("non-neutral init is applied exactly once regardless of chunking", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  expected <- 10 + sum(1:20)
  for (cs in c(1L, 3L, 7L, 20L)) {
    for (w in c(1L, 2L)) {
      res <- shard_reduce(
        shards(20, block_size = 1),
        map = function(s) s$id,
        combine = `+`,
        init = 10,
        workers = w,
        chunk_size = cs
      )
      expect_true(succeeded(res))
      expect_equal(res$value, expected,
                   info = sprintf("chunk_size=%d workers=%d", cs, w))
    }
  }

  # Single shard: partial is the bare mapped value; init still folds once.
  res1 <- shard_reduce(
    shards(1, block_size = 1),
    map = function(s) s$id,
    combine = `+`,
    init = 10,
    workers = 1
  )
  expect_equal(res1$value, 11)
})

test_that("partials fold in chunk order on the master", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Order-sensitive combine: concatenation. Worker partials preserve shard
  # order within a chunk; the master folds partials in chunk id order, so the
  # result is exactly 1:12 even with two workers racing.
  res <- shard_reduce(
    shards(12, block_size = 1),
    map = function(s) s$id,
    combine = function(acc, x) c(acc, x),
    init = integer(0),
    workers = 2,
    chunk_size = 3L
  )
  expect_true(succeeded(res))
  expect_equal(res$value, 1:12)
})

test_that("chunk_size = 'auto' batches ~4 chunks per worker", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  res <- shard_reduce(
    shards(32, block_size = 1),
    map = function(s) 1L,
    combine = `+`,
    init = 0L,
    workers = 2,
    chunk_size = "auto",
    diagnostics = TRUE
  )

  expect_true(succeeded(res))
  expect_equal(res$value, 32L)
  # chunk_size = max(1, ceiling(32 / (2 * 4))) = 4 -> 8 chunks / 8 partials.
  rd <- res$diagnostics$reduce
  expect_identical(rd$chunk_size, 4L)
  expect_identical(rd$num_chunks, 8L)
  expect_identical(rd$partials, 8L)

  # "auto" is the default.
  res_def <- shard_reduce(
    shards(32, block_size = 1),
    map = function(s) 1L,
    combine = `+`,
    init = 0L,
    workers = 2,
    diagnostics = TRUE
  )
  expect_identical(res_def$diagnostics$reduce$chunk_size, 4L)

  # Fewer shards than workers*4 still yields chunk_size 1.
  res_small <- shard_reduce(
    shards(5, block_size = 1),
    map = function(s) 1L,
    combine = `+`,
    init = 0L,
    workers = 2,
    chunk_size = "auto",
    diagnostics = TRUE
  )
  expect_identical(res_small$diagnostics$reduce$chunk_size, 1L)
  expect_identical(res_small$diagnostics$reduce$num_chunks, 5L)
})
