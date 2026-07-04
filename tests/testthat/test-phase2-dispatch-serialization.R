# Phase 2 regression tests (dispatch-path serialization):
# - 2.1: the per-send payload (wrapper fun + chunk arg) is small and does NOT
#        grow with the number of chunks (no dispatch-frame capture).
# - 2.2: the user kernel is not embedded per chunk; it travels once per
#        dispatch via the .shard_dispatch_fun channel (and is manifest-recorded
#        so recycled workers replay it).
# - 2.4: diagnostics gating changes telemetry only, never results.

# What parallel's sendCall actually ships per chunk: the wrapper function plus
# its arguments. Under devtools::load_all() the wrapper carries srcrefs whose
# srcfile environments embed the parsed source (a keep.source dev artifact,
# absent from installed packages), so we strip srcrefs before measuring.
# removeSource() does NOT touch the function's environment — if the wrapper
# ever regresses to capturing the dispatch frame, that frame still serializes
# by value here and blows the size assertion.
send_payload_ <- function(chunk, collect_diag = TRUE) {
  list(
    fun = utils::removeSource(shard_dispatch_wrapper_),
    args = list(chunk, collect_diag)
  )
}

test_that("per-send dispatch payload is small and independent of chunk count", {
  # Realistic decomposition: 50 shards over 1e6 elements.
  blocks <- shards(1e6, block_size = 20000L)
  expect_identical(blocks$num_shards, 50L)
  chunks <- create_shard_chunks(blocks, 1L, borrow = list(), out = list())

  bytes <- length(serialize(send_payload_(chunks[[1]]), NULL))
  expect_lt(bytes, 10 * 1024)

  # The payload must not grow with the number of chunks in the run.
  # Same leading shard (1:10000), different total chunk counts (10 vs 100).
  chunks_10 <- create_shard_chunks(shards(1e5, block_size = 10000L), 1L,
                                   borrow = list(), out = list())
  chunks_100 <- create_shard_chunks(shards(1e6, block_size = 10000L), 1L,
                                    borrow = list(), out = list())
  expect_length(chunks_10, 10L)
  expect_length(chunks_100, 100L)

  b10 <- length(serialize(send_payload_(chunks_10[[1]]), NULL))
  b100 <- length(serialize(send_payload_(chunks_100[[1]]), NULL))
  expect_lt(abs(b100 - b10), 256)

  # Same invariance for the exact object shipped (srcrefs included): whatever
  # constant the dev environment contributes, it must not scale with the
  # number of chunks in the run.
  raw_payload_ <- function(chunk) {
    list(fun = shard_dispatch_wrapper_, args = list(chunk, TRUE))
  }
  r10 <- length(serialize(raw_payload_(chunks_10[[1]]), NULL))
  r100 <- length(serialize(raw_payload_(chunks_100[[1]]), NULL))
  expect_lt(abs(r100 - r10), 4096)
})

test_that("dispatch wrapper lives in the package namespace, not a dispatch frame", {
  expect_identical(environment(shard_dispatch_wrapper_), asNamespace("shard"))
  expect_identical(environment(shard_dispatch_diag_snapshot_), asNamespace("shard"))
  expect_identical(environment(shard_dispatch_diag_deltas_), asNamespace("shard"))
  expect_identical(environment(pool_lapply_chunk_fun_), asNamespace("shard"))
})

test_that("chunks no longer embed the user kernel", {
  blocks <- shards(100, block_size = 10L)
  chunks <- create_shard_chunks(blocks, 2L, borrow = list(x = 1), out = list())
  for (ch in chunks) {
    expect_null(ch$fun)
    expect_false("fun" %in% names(ch))
  }
  # The executor carries the kernel instead (once per dispatch).
  kern <- function(sh) sum(sh$idx)
  ex <- make_chunk_executor(auto_table = FALSE, fun = kern)
  expect_identical(environment(ex)$fun, kern)
  # And the executor's enclosure chains to the package namespace, so
  # serializing it does not drag any caller frame.
  expect_identical(parent.env(environment(ex)), asNamespace("shard"))
})

test_that("strided shards are compact on the wire and expanded for user code", {
  blocks <- shards(50000, block_size = 12500L, strategy = "strided", workers = 1)
  chunks <- create_shard_chunks(blocks, 1L, borrow = list(), out = list())

  expect_null(chunks[[1]]$shards[[1]]$idx)
  expect_equal(chunks[[1]]$shards[[1]]$start, blocks$shards[[1]]$start)
  expect_equal(chunks[[1]]$shards[[1]]$stride, blocks$shards[[1]]$stride)
  expect_equal(chunks[[1]]$shards[[1]]$len, blocks$shards[[1]]$len)
  expect_lt(length(serialize(chunks[[1]], NULL)), 10 * 1024)

  # The kernel-visible shard must be identical() to the public descriptor:
  # same field order and same idx storage type. Integer idx where the public
  # object has double silently overflows to NA in kernels doing integer index
  # arithmetic past 2^31.
  ex <- make_chunk_executor(auto_table = FALSE, fun = function(sh) sh)
  got <- ex(chunks[[1]])
  expect_identical(got[[1]], blocks$shards[[1]])
  expect_identical(got[[1]]$idx, blocks$shards[[1]]$idx)

  # Boundary shards (n not divisible by stride, single-element tail) must
  # round-trip identically too, whatever type seq() gave them originally.
  blocks2 <- shards(11, block_size = 1, strategy = "strided", workers = 1)
  chunks2 <- create_shard_chunks(blocks2, 1L, borrow = list(), out = list())
  for (k in seq_along(chunks2)) {
    expect_identical(ex(chunks2[[k]])[[1]], blocks2$shards[[k]], label = paste("shard", k))
  }
})

test_that("results identical with diagnostics=TRUE and diagnostics=FALSE", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()
  blocks <- shards(12, block_size = 2, workers = 2)
  kern <- function(sh) sum(runif(length(sh$idx)))

  r_diag <- shard_map(blocks, kern, workers = 2, seed = 42, diagnostics = TRUE)
  pool_stop()
  r_nodiag <- shard_map(blocks, kern, workers = 2, seed = 42, diagnostics = FALSE)
  pool_stop()

  expect_true(succeeded(r_diag))
  expect_true(succeeded(r_nodiag))
  expect_identical(
    unname(unlist(results(r_diag))),
    unname(unlist(results(r_nodiag)))
  )

  # diagnostics=TRUE keeps the full telemetry contract.
  expect_true(is.list(r_diag$diagnostics))
  expect_true(is.list(r_diag$diagnostics$view_stats))
  expect_true(is.list(r_diag$diagnostics$copy_stats))
  expect_true(is.list(r_diag$diagnostics$scratch_stats))
  # diagnostics=FALSE returns no diagnostics payload.
  expect_null(r_nodiag$diagnostics)
})

test_that("dispatch_chunks(diagnostics=FALSE) skips per-chunk delta payloads", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()
  pool_create(1)
  on.exit(pool_stop(), add = TRUE)

  chunks <- list(list(id = 1L, x = 1), list(id = 2L, x = 2))

  res_on <- dispatch_chunks(chunks, function(chunk) chunk$x, diagnostics = TRUE)
  res_off <- dispatch_chunks(chunks, function(chunk) chunk$x, diagnostics = FALSE)

  expect_identical(unname(unlist(res_on$results)), c(1, 2))
  expect_identical(unname(unlist(res_off$results)), c(1, 2))

  # With diagnostics on, worker deltas were aggregated (structure present and
  # counters are integers/zeros, not NULL).
  expect_true(is.list(res_on$diagnostics$view_stats))
  # With diagnostics off, aggregation still returns the (untouched) zero
  # structure; the important part is results identity, checked above.
  expect_true(is.list(res_off$diagnostics$view_stats))
  expect_identical(res_off$diagnostics$view_stats$created, 0L)
})

test_that("recycled worker completes after once-per-dispatch kernel export", {
  skip_on_cran()
  skip_if_conn_exhausted()
  if (is_windows()) skip("signal-based worker tests require POSIX")

  pool_stop()

  # workers = 1: after the kill, the only worker is a fresh replacement. It
  # must receive the executor (which now carries the user kernel) from the
  # bootstrap manifest, or every remaining chunk fails until retries are
  # exhausted.
  mats <- lapply(1:6, function(i) matrix(as.numeric(i + seq_len(9)), 3, 3))
  expected <- vapply(mats, function(m) sqrt(sum(m^2)), numeric(1))

  out <- buffer("double", dim = length(mats))
  blocks <- shards(length(mats), block_size = 1, workers = 1)
  kill_flag <- tempfile("shard-phase2-kill-")
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
      diagnostics = FALSE,
      max_retries = 2L,
      health_check_interval = 1L,
      timeout = 30
    )
  )

  expect_true(succeeded(res))
  expect_equal(as.numeric(out[]), expected, tolerance = 1e-12)
  expect_gte(res$pool_stats$total_deaths, 1L)
  expect_lte(res$queue_status$total_retries, 3L)

  pool_stop()
})

test_that("pool_lapply delivers FUN once per dispatch (no per-chunk embedding)", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  res <- pool_lapply(1:6, function(x, k) x * k, k = 10L)
  expect_identical(unname(unlist(res)), as.integer(1:6 * 10L))
})

test_that("shard_reduce map fun travels via the dispatch channel", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()
  res <- shard_reduce(
    100L,
    map = function(s) sum(s$idx),
    combine = `+`,
    init = 0,
    workers = 2
  )
  pool_stop()
  expect_identical(res$value, as.numeric(sum(1:100)))
})

