# Regression tests for the shard 0.2.0 pre-CRAN review findings (items A6, A7,
# A9-A15, C5). Each test names the finding it guards. Pool-based tests use
# skip_on_cran() with workers = 2 per the package's core-cap policy.

# Best-effort available-memory probe (bytes); NA when it cannot be determined.
.regr_avail_mem_bytes <- function() {
  if (requireNamespace("ps", quietly = TRUE)) {
    m <- tryCatch(ps::ps_system_memory(), error = function(e) NULL)
    if (!is.null(m) && !is.null(m$avail)) return(as.double(m$avail))
  }
  NA_real_
}

# --- A6: integer overflow in as_shared()/share() size computation ------------

test_that("A6: as_shared() size computation does not overflow for >2GB vectors", {
  # length(x) * elem_size was int*int and overflowed to NA at >= 2^28 doubles,
  # breaking share() on its headline >2GB use case. The allocation is genuinely
  # large (~2GB vector + ~2GB segment), so gate on detected free memory and skip
  # where it is unknown or tight.
  av <- .regr_avail_mem_bytes()
  skip_if(is.na(av) || av < 8e9,
          "A6 overflow check needs > ~8GB free memory to allocate a 2GB vector")

  n <- 268435456L # 2^28; n * 8L overflows integer arithmetic (old code -> NA)
  x <- double(n)
  shx <- share(x, backing = "mmap")
  on.exit(tryCatch(close(shx), error = function(e) NULL), add = TRUE)

  expect_true(is_shared_vector(shx))
  expect_equal(length(shx), n)
  rm(x)
  gc()
})

# --- A7: stale out-handle in shard_reduce after shard_map on the same pool ----

test_that("A7: shard_reduce out-handle is not corrupted by a prior shard_map", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # 1) shard_map with an out buffer named "out": its executor caches the open
  #    handle in the worker as list(key=, obj=) under the key "out".
  buf_map <- buffer("double", dim = 10, init = 0)
  m <- shard_map(
    shards(10, block_size = 1),
    out = list(out = buf_map),
    fun = function(shard, out) {
      out[shard$id] <- shard$id
      NULL
    },
    workers = 2
  )
  expect_true(succeeded(m))
  expect_equal(as.numeric(buf_map[]), as.numeric(1:10))

  # 2) shard_reduce on the SAME pool with an out buffer under the SAME name.
  #    The drifted opener read opened[["out"]] (a list(key=, obj=) wrapper) as
  #    if it were the buffer, so writes went nowhere and buf_red stayed zero.
  buf_red <- buffer("double", dim = 10, init = 0)
  r <- shard_reduce(
    shards(10, block_size = 1),
    map = function(s, out) {
      out[s$id] <- s$id * 10
      s$id
    },
    combine = `+`,
    init = 0,
    out = list(out = buf_red),
    workers = 2
  )
  expect_true(succeeded(r))
  expect_equal(r$value, sum(1:10))
  expect_equal(as.numeric(buf_red[]), as.numeric((1:10) * 10))
})

# --- A9: silent data loss on invalid shard_id in table sinks -----------------

test_that("A9: table_sink write rejects invalid shard_id instead of dropping it", {
  sch <- schema(a = int32())
  sink <- table_sink(sch, mode = "row_groups")

  expect_error(table_write(sink, NA_integer_, data.frame(a = 1L)), "positive integer")
  expect_error(table_write(sink, NA, data.frame(a = 1L)), "positive integer")
  expect_error(table_write(sink, 0L, data.frame(a = 1L)), "positive integer")
  expect_error(table_write(sink, -1L, data.frame(a = 1L)), "positive integer")
  expect_error(table_write(sink, 1.5, data.frame(a = 1L)), "positive integer")
  expect_error(table_write(sink, c(1L, 2L), data.frame(a = 1L)), "single positive integer")
  expect_error(table_write(sink, "1", data.frame(a = 1L)), "single positive integer")

  # A valid write still round-trips and is retained by finalize's part glob.
  table_write(sink, 1L, data.frame(a = 5L))
  table_write(sink, 2L, data.frame(a = 6L))
  fin <- table_finalize(sink, materialize = "always")
  expect_equal(nrow(fin), 2L)
  expect_setequal(fin$a, c(5L, 6L))
})

# --- A10: row_layout() hard-error on a trailing zero-row shard ---------------

test_that("A10: row_layout handles a trailing zero-row shard", {
  sh <- shards(9, block_size = 3) # 3 shards

  lay <- row_layout(sh, rows_per_shard = function(s) if (s$id == 3L) 0L else 2L)
  expect_length(lay, 3L)
  expect_equal(names(lay), c("1", "2", "3"))
  expect_null(lay[[3]])
  expect_false(is.null(lay[[1]]))
  expect_false(is.null(lay[[2]]))
  expect_equal(shard:::.table_rows_len(lay[[1]]), 2L)
  expect_equal(shard:::.table_rows_len(lay[[2]]), 2L)

  # Zero-row shard in the middle keeps positions aligned too.
  lay2 <- row_layout(sh, rows_per_shard = function(s) if (s$id == 2L) 0L else 2L)
  expect_length(lay2, 3L)
  expect_null(lay2[[2]])
  expect_equal(shard:::.table_rows_len(lay2[[3]]), 2L)
})

# --- A11: shard_crossprod auto-tile selection on narrow matrices -------------

test_that("A11: crossprod auto-tile selection works for < 8 column matrices", {
  # p < 8 emptied the candidate list -> max(integer(0)) == -Inf downstream.
  Xsh <- share(matrix(as.double(1:200), nrow = 100, ncol = 2))
  Ysh <- share(matrix(as.double(1:300), nrow = 100, ncol = 3))
  on.exit(tryCatch({
    close(Xsh)
    close(Ysh)
  }, error = function(e) NULL), add = TRUE)

  tune <- shard:::.autotune_crossprod_tiles(Xsh, Ysh)
  expect_true(is.finite(tune$block_x))
  expect_true(is.finite(tune$block_y))
  expect_equal(tune$block_x, 2L)
  expect_equal(tune$block_y, 3L)
})

test_that("A11: shard_crossprod end-to-end on a 2-column matrix", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  X <- matrix(rnorm(200), nrow = 100, ncol = 2)
  Y <- matrix(rnorm(300), nrow = 100, ncol = 3)
  res <- shard_crossprod(X, Y, workers = 2, materialize = "always")
  expect_equal(res$value, crossprod(X, Y), tolerance = 1e-8)
})

# --- A12: shards_list() item count vs shard count ----------------------------

test_that("A12: shards_list reports total items as n, not the shard count", {
  sh <- shards_list(list(1:10, 11:20, 21:30))
  expect_equal(sh$n, 30L) # total items
  expect_equal(sh$num_shards, 3L) # number of shards
  expect_equal(length(sh), 3L) # length() == num_shards

  out <- capture.output(print(sh))
  expect_true(any(grepl("Items:\\s*30", out)))
  expect_true(any(grepl("Shards:\\s*3", out)))
})

# --- A14: pool_create(heartbeat_interval=) deprecated and unused -------------

test_that("A14: pool_create warns on deprecated heartbeat_interval", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()
  expect_warning(pool_create(1, heartbeat_interval = 10), "deprecated")
  pool_stop()

  # The documented default is a no-op and must not warn.
  expect_warning(pool_create(1), NA)
  pool_stop()
})

# --- A15: share(name=) honored on the atomic fast path; warns on deep --------

test_that("A15: share(name=) names the backing segment on the atomic fast path", {
  x <- as.double(1:1000)
  nm <- tempfile("shard_a15_")

  shx <- share(x, backing = "mmap", name = nm)
  on.exit({
    tryCatch(close(shx), error = function(e) NULL)
    tryCatch(unlink(nm), error = function(e) NULL)
  }, add = TRUE)

  expect_true(is_shared_vector(shx))

  # The real promise: a named share is reopenable by name in another process.
  # With the bug the fast path made an anonymous segment and `nm` never existed.
  seg2 <- segment_open(nm, backing = "mmap", readonly = TRUE)
  on.exit(tryCatch(segment_close(seg2, unlink = FALSE), error = function(e) NULL), add = TRUE)
  v2 <- shared_vector(seg2, "double", length = length(x))
  expect_equal(as.numeric(v2), x)
})

test_that("A15: share(name=) warns for deep shares (multi-segment)", {
  lst <- list(a = as.double(1:1000))
  expect_warning(
    sh <- share(lst, deep = TRUE, min_bytes = 1, name = "ignored_deep_name"),
    "ignored for deep"
  )
  tryCatch(close(sh), error = function(e) NULL)
})

# --- C5: object.size() gated on diagnostics in shard_reduce ------------------

test_that("C5: shard_reduce with diagnostics = FALSE returns correct results", {
  skip_on_cran()
  skip_if_conn_exhausted()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # With diagnostics off, partial byte-size accounting is skipped entirely; the
  # reduction must still be exact and diagnostics NULL.
  res <- shard_reduce(
    shards(50, block_size = 5),
    map = function(s) sum(s$idx),
    combine = `+`,
    init = 0,
    workers = 2,
    diagnostics = FALSE
  )
  expect_true(succeeded(res))
  expect_equal(res$value, sum(1:50))
  expect_null(res$diagnostics)
})
