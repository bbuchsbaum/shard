# Phase 3 (3.1): O(1) amortized taskq claim cursor + batched claims.
#
# The claim fast path is an atomic fetch_add head cursor; requeued tasks
# (retries, reset claims) behind the cursor are recovered by a fallback
# sweep. These tests pin: exactly-once claiming, retry/failure accounting,
# batched claim correctness, and the end-to-end dispatch paths.

test_that("cursor claim hands out every task exactly once (single-process)", {
  if (!shard:::taskq_supported()) skip("taskq not supported (no atomics)")

  n <- 500L
  q <- shard:::taskq_create(n, backing = "mmap")
  seg <- shard:::taskq_open(q$desc, readonly = FALSE)
  on.exit({
    shard:::segment_close(seg, unlink = FALSE)
    shard:::segment_close(q$owner, unlink = TRUE)
  }, add = TRUE)

  ids <- integer(0)
  repeat {
    id <- shard:::taskq_claim(seg, worker_id = 1L)
    if (id < 1L) break
    ids <- c(ids, id)
    shard:::taskq_done(seg, id)
  }

  expect_equal(sort(ids), seq_len(n))
  st <- shard:::taskq_stats(seg)
  expect_equal(st$done, n)
  expect_equal(st$failed, 0L)
  expect_equal(st$retries, 0L)
})

test_that("taskq_claim_range claims up to k unique ids and validates input", {
  if (!shard:::taskq_supported()) skip("taskq not supported (no atomics)")

  n <- 10L
  q <- shard:::taskq_create(n, backing = "mmap")
  seg <- shard:::taskq_open(q$desc, readonly = FALSE)
  on.exit({
    shard:::segment_close(seg, unlink = FALSE)
    shard:::segment_close(q$owner, unlink = TRUE)
  }, add = TRUE)

  expect_error(shard:::taskq_claim_range(seg, 1L, 0L), "max_tasks must be >= 1")
  expect_error(shard:::taskq_claim_range(seg, 0L, 4L), "worker_id must be >= 1")

  a <- shard:::taskq_claim_range(seg, 1L, 4L)
  expect_type(a, "integer")
  expect_length(a, 4L)

  # Asking for more than remain returns only what is claimable.
  b <- shard:::taskq_claim_range(seg, 2L, 100L)
  expect_length(b, 6L)

  # No overlap, full coverage.
  expect_equal(sort(c(a, b)), seq_len(n))

  # Drained queue: length-0 result.
  for (id in c(a, b)) shard:::taskq_done(seg, id)
  expect_length(shard:::taskq_claim_range(seg, 1L, 4L), 0L)
  expect_equal(shard:::taskq_claim(seg, 1L), 0L)
})

test_that("mixed single and batched claims never double-claim", {
  if (!shard:::taskq_supported()) skip("taskq not supported (no atomics)")

  n <- 200L
  q <- shard:::taskq_create(n, backing = "mmap")
  seg <- shard:::taskq_open(q$desc, readonly = FALSE)
  on.exit({
    shard:::segment_close(seg, unlink = FALSE)
    shard:::segment_close(q$owner, unlink = TRUE)
  }, add = TRUE)

  ids <- integer(0)
  repeat {
    got <- if (length(ids) %% 2L == 0L) {
      shard:::taskq_claim_range(seg, 1L, 7L)
    } else {
      id <- shard:::taskq_claim(seg, 2L)
      if (id < 1L) integer(0) else id
    }
    if (length(got) == 0L) break
    ids <- c(ids, got)
    for (id in got) shard:::taskq_done(seg, id)
  }

  expect_equal(sort(ids), seq_len(n))
})

test_that("fallback sweep recovers tasks requeued behind the cursor", {
  if (!shard:::taskq_supported()) skip("taskq not supported (no atomics)")

  n <- 20L
  q <- shard:::taskq_create(n, backing = "mmap")
  seg <- shard:::taskq_open(q$desc, readonly = FALSE)
  on.exit({
    shard:::segment_close(seg, unlink = FALSE)
    shard:::segment_close(q$owner, unlink = TRUE)
  }, add = TRUE)

  # Drain the queue, failing tasks 3 and 17 on their first claim only.
  # Their requeue lands *behind* the advancing cursor, so the drain loop can
  # only ever see them a second time via the fallback sweep.
  flaky <- c(3L, 17L)
  claimed <- integer(0)
  failed_once <- integer(0)
  repeat {
    id <- shard:::taskq_claim(seg, 1L)
    if (id < 1L) break
    claimed <- c(claimed, id)
    if (id %in% flaky && !(id %in% failed_once)) {
      failed_once <- c(failed_once, id)
      expect_true(shard:::taskq_error(seg, id, max_retries = 2L))
    } else {
      shard:::taskq_done(seg, id)
    }
  }

  # Every task claimed; flaky ones claimed exactly twice (second time via
  # the sweep), everything else exactly once.
  tab <- table(claimed)
  expect_equal(sort(unique(claimed)), seq_len(n))
  expect_equal(as.integer(tab[as.character(flaky)]), c(2L, 2L))
  expect_true(all(tab[setdiff(as.character(seq_len(n)), as.character(flaky))] == 1L))

  st <- shard:::taskq_stats(seg)
  expect_equal(st$done, n)
  expect_equal(st$failed, 0L)
  expect_equal(st$retries, 2L)
})

test_that("reset_claims requeues behind the cursor and the sweep recovers them", {
  if (!shard:::taskq_supported()) skip("taskq not supported (no atomics)")

  n <- 10L
  q <- shard:::taskq_create(n, backing = "mmap")
  seg <- shard:::taskq_open(q$desc, readonly = FALSE)
  on.exit({
    shard:::segment_close(seg, unlink = FALSE)
    shard:::segment_close(q$owner, unlink = TRUE)
  }, add = TRUE)

  # Worker 1 claims three tasks and "dies"; the rest complete normally.
  dead <- shard:::taskq_claim_range(seg, 1L, 3L)
  expect_length(dead, 3L)

  alive <- integer(0)
  repeat {
    id <- shard:::taskq_claim(seg, 2L)
    if (id < 1L) break
    alive <- c(alive, id)
    shard:::taskq_done(seg, id)
  }
  expect_equal(sort(c(dead, alive)), seq_len(n))

  expect_equal(shard:::taskq_reset_claims(seg, 1L), 3L)

  # Cursor is exhausted; requeued tasks are claimable only via the sweep.
  recovered <- shard:::taskq_claim_range(seg, 3L, 8L)
  expect_equal(sort(recovered), sort(dead))
  for (id in recovered) shard:::taskq_done(seg, id)

  st <- shard:::taskq_stats(seg)
  expect_equal(st$done, n)
})

test_that("shm_queue dispatch executes each task exactly once across workers", {
  skip_on_cran()
  if (!shard:::taskq_supported()) skip("shm_queue not supported (no atomics)")

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  n <- 2000L
  # Counting kernel: increments its slot. Any double execution of a task id
  # leaves a value > 1; any lost task leaves 0.
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")

  res <- shard_map(
    n,
    out = list(out = out),
    fun = function(sh, out) {
      out[sh$idx] <- out[sh$idx] + 1L
      NULL
    },
    workers = 4,
    chunk_size = 1,
    dispatch_mode = "shm_queue",
    dispatch_opts = list(block_size = 1L),
    diagnostics = TRUE
  )

  expect_true(succeeded(res))
  counts <- as.integer(out[])
  expect_identical(counts, rep(1L, n))
  expect_equal(res$queue_status$completed, n)
  expect_equal(res$queue_status$failed, 0L)
  buffer_close(out)
})

test_that("shm_queue batched claims execute each task exactly once", {
  skip_on_cran()
  if (!shard:::taskq_supported()) skip("shm_queue not supported (no atomics)")

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  n <- 2000L
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")

  res <- shard_map(
    n,
    out = list(out = out),
    fun = function(sh, out) {
      out[sh$idx] <- out[sh$idx] + 1L
      NULL
    },
    workers = 4,
    chunk_size = 1,
    dispatch_mode = "shm_queue",
    dispatch_opts = list(block_size = 1L, claim_batch = 4L),
    diagnostics = TRUE
  )

  expect_true(succeeded(res))
  expect_identical(as.integer(out[]), rep(1L, n))
  expect_equal(res$queue_status$completed, n)
  buffer_close(out)
})

test_that("shm_queue retry path: transient failures behind the cursor complete", {
  skip_on_cran()
  if (!shard:::taskq_supported()) skip("shm_queue not supported (no atomics)")

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  n <- 100L
  flaky <- c(7L, 40L, 93L)
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")

  # Sentinel dir shared across worker processes: each flaky task fails on
  # its first attempt (before any sentinel exists), succeeds on retry.
  sentinel_dir <- file.path(tempdir(), paste0("shard_p3_retry_", Sys.getpid()))
  dir.create(sentinel_dir, showWarnings = FALSE)
  on.exit(unlink(sentinel_dir, recursive = TRUE), add = TRUE)

  res <- shard_map(
    n,
    borrow = list(flaky = flaky, sentinel_dir = sentinel_dir),
    out = list(out = out),
    fun = function(sh, flaky, sentinel_dir, out) {
      if (sh$idx %in% flaky) {
        marker <- file.path(sentinel_dir, paste0("t", sh$idx))
        if (!file.exists(marker)) {
          file.create(marker)
          stop("transient failure for task ", sh$idx)
        }
      }
      out[sh$idx] <- out[sh$idx] + 1L
      NULL
    },
    workers = 4,
    chunk_size = 1,
    max_retries = 2,
    dispatch_mode = "shm_queue",
    dispatch_opts = list(block_size = 1L),
    diagnostics = TRUE
  )

  expect_true(succeeded(res))
  expect_identical(as.integer(out[]), rep(1L, n))
  expect_equal(res$queue_status$completed, n)
  expect_equal(res$queue_status$failed, 0L)
  # Each flaky task scheduled exactly one retry.
  expect_equal(res$queue_status$total_retries, length(flaky))
  buffer_close(out)
})

test_that("shm_queue batched claims + retries + permanent failure accounting", {
  skip_on_cran()
  if (!shard:::taskq_supported()) skip("shm_queue not supported (no atomics)")

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  n <- 50L
  out <- buffer("integer", dim = n, init = 0L, backing = "mmap")

  res <- shard_map(
    n,
    out = list(out = out),
    fun = function(sh, out) {
      if (identical(sh$idx, 13L)) stop("boom")
      out[sh$idx] <- out[sh$idx] + 1L
      NULL
    },
    workers = 2,
    chunk_size = 1,
    max_retries = 1,
    dispatch_mode = "shm_queue",
    dispatch_opts = list(block_size = 1L, claim_batch = 8L),
    diagnostics = TRUE
  )

  expect_false(succeeded(res))
  expect_equal(res$queue_status$failed, 1L)
  expect_equal(res$queue_status$total_retries, 1L)
  expect_true("13" %in% names(res$failures))
  expect_equal(res$failures[["13"]]$retry_count, 2L)

  counts <- as.integer(out[])
  expect_identical(counts[-13L], rep(1L, n - 1L))
  expect_identical(counts[13L], 0L)
  buffer_close(out)
})

test_that("dispatch_opts$claim_batch is validated", {
  skip_on_cran()
  if (!shard:::taskq_supported()) skip("shm_queue not supported (no atomics)")

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  out <- buffer("integer", dim = 4L, init = 0L, backing = "mmap")
  expect_error(
    shard_map(
      4L,
      out = list(out = out),
      fun = function(sh, out) NULL,
      workers = 1,
      chunk_size = 1,
      dispatch_mode = "shm_queue",
      dispatch_opts = list(claim_batch = 0L)
    ),
    "claim_batch must be >= 1"
  )
  buffer_close(out)
})

test_that("segment_create records the resolved backing for auto", {
  # Phase 3.2: 'auto' resolves platform-dependently in C (POSIX shm on
  # Linux, file-backed mmap elsewhere). The R handle and any descriptors
  # built from it must carry the resolved value so workers reopen with the
  # correct mechanism (shm_open vs open).
  seg <- segment_create(1024, backing = "auto")
  on.exit(segment_close(seg), add = TRUE)

  expect_true(seg$backing %in% c("mmap", "shm"))
  info <- segment_info(seg)
  expect_identical(seg$backing, info$backing)
  if (identical(Sys.info()[["sysname"]], "Linux")) {
    expect_identical(seg$backing, "shm")
  } else {
    expect_identical(seg$backing, "mmap")
  }

  # Reopen via the recorded backing must work.
  seg2 <- segment_open(segment_path(seg), backing = seg$backing, readonly = TRUE)
  expect_s3_class(seg2, "shard_segment")
  segment_close(seg2, unlink = FALSE)
})

test_that("taskq_create with auto backing yields a reopenable descriptor", {
  if (!shard:::taskq_supported()) skip("taskq not supported (no atomics)")

  q <- shard:::taskq_create(4L, backing = "auto")
  on.exit(shard:::segment_close(q$owner, unlink = TRUE), add = TRUE)

  expect_true(q$desc$backing %in% c("mmap", "shm"))
  seg <- shard:::taskq_open(q$desc, readonly = FALSE)
  expect_equal(shard:::taskq_stats(seg)$n_tasks, 4L)
  shard:::segment_close(seg, unlink = FALSE)
})
