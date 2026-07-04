# Phase 1 regression tests:
# - 1.3: recycled/restarted workers replay exported state (bootstrap manifest)
# - 1.4: seed= gives per-shard L'Ecuyer-CMRG streams (reproducible under
#        dynamic assignment, worker counts, chunk sizes, and worker restarts)

test_that("restarted worker replays borrow/out state: no retry storm, correct results", {
  skip_on_cran()
  skip_if_conn_exhausted()
  if (is_windows()) skip("signal-based worker tests require POSIX")

  pool_stop()

  # workers = 1 makes this deterministic: after the kill, the ONLY worker is a
  # fresh replacement. Without manifest replay it would lack .shard_borrow /
  # .shard_out and every remaining chunk would fail until retries were
  # exhausted (permanent failures).
  mats <- lapply(1:6, function(i) matrix(as.numeric(i + seq_len(9)), 3, 3))
  expected <- vapply(mats, function(m) sqrt(sum(m^2)), numeric(1))

  out <- buffer("double", dim = length(mats))
  blocks <- shards(length(mats), block_size = 1, workers = 1)
  kill_flag <- tempfile("shard-recycle-kill-")
  on.exit(unlink(kill_flag), add = TRUE)

  res <- suppressWarnings(
    shard_map(
      blocks,
      borrow = list(mats = mats, flag = kill_flag),
      out = list(out = out),
      fun = function(sh, mats, flag, out) {
        if (isTRUE(sh$id == 2L) && !file.exists(flag)) {
          file.create(flag)
          tools::pskill(Sys.getpid(), signal = 9L)
        }
        i <- sh$idx[[1]]
        out[i] <- sqrt(sum(mats[[i]]^2))
        NULL
      },
      workers = 1,
      max_retries = 2L,
      health_check_interval = 1L,
      timeout = 30
    )
  )

  expect_true(succeeded(res))
  expect_equal(as.numeric(out[]), expected, tolerance = 1e-12)
  expect_gte(res$pool_stats$total_deaths, 1L)
  # No retry storm: only the killed chunk should have been retried (allow a
  # small margin for a second in-flight chunk at kill time).
  expect_lte(res$queue_status$total_retries, 3L)

  pool_stop()
})

test_that("seed= gives identical results across worker counts, chunk sizes, and assignment", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()

  kern <- function(sh) sum(runif(length(sh$idx)))
  blocks <- shards(12, block_size = 2, workers = 2) # fixed decomposition: 6 shards

  run <- function(workers, chunk_size = 1L, seed = 42) {
    r <- shard_map(blocks, kern, workers = workers, chunk_size = chunk_size, seed = seed)
    pool_stop()
    unname(unlist(results(r)))
  }

  v_w1 <- run(workers = 1)
  v_w2 <- run(workers = 2)
  v_cs3 <- run(workers = 2, chunk_size = 3L)
  v_cs6 <- run(workers = 2, chunk_size = 6L)

  expect_identical(v_w1, v_w2)
  expect_identical(v_w1, v_cs3)
  expect_identical(v_w1, v_cs6)

  # Different seed -> different results.
  v_seed7 <- run(workers = 2, seed = 7)
  expect_false(identical(v_w1, v_seed7))

  # Repeated run with the same seed reproduces exactly.
  expect_identical(v_w2, run(workers = 2))
})

test_that("shard_map(N, ..., seed=) is reproducible across worker counts", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()

  kern <- function(sh) sum(runif(length(sh$idx)))

  r1 <- shard_map(20L, kern, workers = 1, seed = 42)
  pool_stop()
  r2 <- shard_map(20L, kern, workers = 2, seed = 42)
  pool_stop()

  expect_identical(unname(unlist(results(r1))), unname(unlist(results(r2))))
})

test_that("seed streams survive a mid-run worker kill (requeued shard reproduces)", {
  skip_on_cran()
  skip_if_conn_exhausted()
  if (is_windows()) skip("signal-based worker tests require POSIX")

  pool_stop()

  blocks <- shards(8, block_size = 1, workers = 2)
  kern <- function(sh) sum(runif(10))

  clean <- shard_map(blocks, kern, workers = 2, seed = 42)
  pool_stop()
  v_clean <- unname(unlist(results(clean)))

  kill_flag <- tempfile("shard-seed-kill-")
  on.exit(unlink(kill_flag), add = TRUE)
  killed <- suppressWarnings(
    shard_map(
      blocks,
      borrow = list(flag = kill_flag),
      fun = function(sh, flag) {
        if (isTRUE(sh$id == 3L) && !file.exists(flag)) {
          file.create(flag)
          tools::pskill(Sys.getpid(), signal = 9L)
        }
        sum(runif(10))
      },
      workers = 2,
      seed = 42,
      max_retries = 2L,
      health_check_interval = 1L,
      timeout = 30
    )
  )
  pool_stop()

  expect_true(succeeded(killed))
  expect_gte(killed$pool_stats$total_deaths, 1L)
  expect_identical(unname(unlist(results(killed))), v_clean)
})

test_that("seed=NULL and seed=<int> leave the master's RNG state untouched", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()
  # Create the pool up front: PSOCK cluster creation itself may consume RNG,
  # and that is out of scope here.
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  set.seed(123)
  before <- .Random.seed
  before_kind <- RNGkind()

  r0 <- shard_map(4L, function(sh) sh$idx[[1L]], seed = NULL)
  expect_identical(.Random.seed, before)

  r1 <- shard_map(4L, function(sh) sum(runif(length(sh$idx))), seed = 42)
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), before_kind)

  expect_true(succeeded(r0))
  expect_true(succeeded(r1))

  # Master draws continue as if shard_map had never run.
  set.seed(123)
  expect_identical(runif(3), local({
    assign(".Random.seed", before, envir = globalenv())
    runif(3)
  }))
})

test_that("make_shard_seed_streams_ derives independent per-shard CMRG streams", {
  streams <- make_shard_seed_streams_(42, 5L)
  expect_length(streams, 5L)
  # L'Ecuyer-CMRG .Random.seed vectors are 7 ints with kind code 407.
  for (s in streams) {
    expect_length(s, 7L)
    expect_identical(s[1] %% 100L, 7L)
  }
  # Streams are distinct and deterministic.
  expect_false(identical(streams[[1]], streams[[2]]))
  expect_identical(streams, make_shard_seed_streams_(42, 5L))
  expect_false(identical(streams[[1]], make_shard_seed_streams_(7, 1L)[[1]]))
})

test_that("worker bootstrap manifest records and clears exports", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()
  pool <- pool_create(1)
  on.exit(pool_stop(), add = TRUE)

  m <- pool_manifest_(pool)
  expect_true(is.environment(m))
  expect_identical(m$exports, list())

  export_borrow_to_workers(pool, list(x = 1:3))
  expect_identical(m$exports[[".shard_borrow"]], list(x = 1:3))

  # Empty borrow clears the stale entry.
  export_borrow_to_workers(pool, list())
  expect_false(".shard_borrow" %in% names(m$exports))

  # dispatch_chunks records the dispatch fun/args.
  res <- dispatch_chunks(
    list(list(id = 1L, x = 2)),
    fun = function(chunk, mult) chunk$x * mult,
    mult = 10
  )
  expect_identical(unname(unlist(res$results)), 20)
  expect_true(".shard_dispatch_fun" %in% names(m$exports))
  expect_identical(m$exports[[".shard_dispatch_args"]], list(mult = 10))
})

test_that("shard_map clears stale .shard_out manifest entry when a later run has no out=", {
  # Regression for M1: export_out_to_workers() was called under
  # `if (length(out) > 0)`, so its clear-on-empty branch was unreachable from
  # shard_map. A run with out= followed by a run without out= would leave a
  # stale .shard_out descriptor in the manifest, which a worker recycled during
  # the second run would replay and try to reopen (a now-possibly-closed
  # segment). The exporter is now called unconditionally.
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()
  pool <- pool_create(2)
  on.exit(pool_stop(), add = TRUE)
  m <- pool_manifest_(pool)

  out <- buffer("double", dim = 4L)
  blocks <- shards(4L, block_size = 1, workers = 2)

  # Run 1: with out= -> .shard_out recorded in the manifest.
  res1 <- shard_map(
    blocks,
    out = list(out = out),
    fun = function(sh, out) {
      i <- sh$idx[[1]]
      out[i] <- i * 1.0
      NULL
    },
    workers = 2
  )
  expect_true(succeeded(res1))
  expect_true(".shard_out" %in% names(m$exports))

  # Run 2: no out= -> stale .shard_out must be cleared, not replayed.
  res2 <- shard_map(
    blocks,
    fun = function(sh) sh$idx[[1]] * 2,
    workers = 2
  )
  expect_true(succeeded(res2))
  expect_false(".shard_out" %in% names(m$exports))
})
