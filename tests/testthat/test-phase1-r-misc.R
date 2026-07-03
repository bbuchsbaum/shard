# Phase 1 (Group C) regression tests:
#   1.2 buffer slice assignment fast path (write amplification + race)
#   1.5 view hotspot key guard for multi-line call heads
#   1.6 parse_count double arithmetic ("3B") + malformed input errors
#   1.7 parse_bytes malformed input errors instead of silent NA
#   1.8 unique_id does not consume the global RNG

# --- Fix 1.2: buf[, j] <- vector takes the targeted fast path ---------------

test_that("buffer column/row slice assignment with vector RHS matches matrix semantics", {
  nr <- 10L
  nc <- 6L
  buf <- buffer("double", dim = c(nr, nc), init = 0)
  ref <- matrix(0, nr, nc)

  # Single column, vector RHS
  buf[, 2] <- 1:nr
  ref[, 2] <- 1:nr

  # Single row, vector RHS
  buf[3, ] <- 101:106
  ref[3, ] <- 101:106

  # Contiguous multi-column block, vector RHS (column-major fill)
  buf[, 4:5] <- 1:(nr * 2)
  ref[, 4:5] <- 1:(nr * 2)

  # Scalar recycled into a block
  buf[2:5, 1] <- 42
  ref[2:5, 1] <- 42

  # Vector recycled across a block (length divides block size)
  buf[, 6] <- c(-1, -2)
  ref[, 6] <- c(-1, -2)

  # Sub-block with vector RHS
  buf[4:9, 2:3] <- seq_len(12)
  ref[4:9, 2:3] <- seq_len(12)

  expect_equal(as.matrix(buf), ref)
  buffer_close(buf)
})

test_that("buffer slice assignment with vector RHS avoids full-buffer rewrite", {
  nr <- 100L
  nc <- 40L
  buf <- buffer("double", dim = c(nr, nc), init = 0)

  # Column write: should write exactly one column's bytes, not the whole buffer.
  shard:::buffer_reset_diagnostics()
  buf[, 3] <- rnorm(nr)
  d <- buffer_diagnostics()
  expect_equal(d$bytes, nr * 8)
  expect_equal(d$writes, 1L)

  # Row write: nc single-element writes, not nr * nc elements.
  shard:::buffer_reset_diagnostics()
  buf[7, ] <- rnorm(nc)
  d <- buffer_diagnostics()
  expect_equal(d$bytes, nc * 8)

  # Scalar recycled into one column: still one column's bytes.
  shard:::buffer_reset_diagnostics()
  buf[, 10] <- 1
  d <- buffer_diagnostics()
  expect_equal(d$bytes, nr * 8)

  buffer_close(buf)
})

test_that("buffer non-contiguous and negative-index assignments still match matrix semantics", {
  nr <- 8L
  nc <- 5L
  buf <- buffer("double", dim = c(nr, nc), init = 0)
  ref <- matrix(0, nr, nc)

  # Non-contiguous columns fall back to the general path
  buf[, c(1, 3)] <- 1:(nr * 2)
  ref[, c(1, 3)] <- 1:(nr * 2)

  # Negative row index (drop first row) must not hit the fast path
  buf[-1, 5] <- seq_len(nr - 1)
  ref[-1, 5] <- seq_len(nr - 1)

  expect_equal(as.matrix(buf), ref)
  buffer_close(buf)
})

test_that("two workers writing disjoint columns with vector RHS both land intact", {
  skip_on_cran()

  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  nr <- 50L
  nc <- 16L
  Z <- buffer("double", dim = c(nr, nc), init = 0, backing = "mmap")

  res <- shard_map(
    nc,
    out = list(Z = Z),
    fun = function(sh, Z) {
      for (col in sh$idx) {
        # Vector RHS column-slice write; must be a targeted write, not a
        # full-buffer read/modify/write (which races across workers).
        Z[, col] <- col * 1000 + seq_len(50L)
      }
      NULL
    },
    workers = 2,
    chunk_size = 1,
    diagnostics = TRUE
  )

  expect_true(succeeded(res))

  expected <- outer(seq_len(nr), seq_len(nc), function(r, c) c * 1000 + r)
  expect_equal(unname(as.matrix(Z)), expected)

  # Every element written exactly once across all workers: total buffer
  # write traffic equals the buffer size (no read/modify/write inflation).
  cr <- copy_report(res)
  expect_equal(cr$buffer_bytes %||% 0, as.double(nr) * nc * 8)

  pool_stop()
  buffer_close(Z)
})

# --- Fix 1.5: view hotspot key guard for multi-line call heads --------------

test_that("view hotspot key tolerates call heads that deparse to length > 1", {
  # The head of `(function(x) {...})(v)` deparses to a multi-element
  # character vector; the scalar `if` in .view_hotspot_key_ must not error.
  key <- (function(x) {
    y <- x + 1L
    shard:::.view_hotspot_key_()
  })(1L)
  expect_true(is.character(key) && length(key) == 1L)
})

test_that("materialization inside an anonymous function call does not error", {
  set.seed(1)
  X <- matrix(rnorm(20 * 4), nrow = 20)
  Xsh <- share(X, backing = "mmap")

  out <- (function(v) {
    # Long body so the calling expression's head deparses across lines.
    a <- 1
    b <- 2
    stopifnot(a + b == 3)
    materialize(view_block(v, cols = idx_range(1L, 2L)))
  })(Xsh)

  expect_equal(out, X[, 1:2])
})

# --- Fix 1.6: parse_count computes in double, errors on malformed input -----

test_that("parse_count handles billions without integer overflow", {
  expect_identical(shard:::parse_count("3B"), 3e9)
  expect_identical(shard:::parse_count("2.5B"), 2.5e9)
  expect_identical(shard:::parse_count("1.5M"), 1500000L)
  expect_identical(shard:::parse_count("10K"), 10000L)
  expect_identical(shard:::parse_count("100"), 100L)
  expect_identical(shard:::parse_count("1k"), 1000L)
  expect_identical(shard:::parse_count(5), 5L)
})

test_that("parse_count errors clearly on malformed input", {
  expect_error(shard:::parse_count("1..5K"), "malformed")
  expect_error(shard:::parse_count("abc"), "Cannot parse count")
  expect_error(shard:::parse_count(""), "Cannot parse count")
  expect_error(shard:::parse_count("K"), "Cannot parse count")
  expect_error(shard:::parse_count(NA_real_), "Cannot parse count")
})

# --- Fix 1.7: parse_bytes errors on malformed input instead of NA -----------

test_that("parse_bytes errors clearly instead of returning NA", {
  expect_error(shard:::parse_bytes("1..5GB"), "malformed")
  expect_error(shard:::parse_bytes("1..5GB"), "1\\.\\.5GB")
  expect_error(shard:::parse_bytes("..GB"), "Cannot parse byte string")
  # Valid inputs unchanged
  expect_equal(shard:::parse_bytes("1.5GB"), 1.5 * 1024^3)
  expect_equal(shard:::parse_bytes("512MB"), 512 * 1024^2)
})

# --- Fix 1.8: unique_id leaves the global RNG untouched ----------------------

test_that("unique_id does not consume or perturb the global RNG", {
  set.seed(123)
  before <- .Random.seed
  ids <- replicate(50, unique_id("t"))
  expect_identical(.Random.seed, before)
  expect_equal(length(unique(ids)), 50L)

  # RNG sequence identical with and without interleaved unique_id() calls
  set.seed(123)
  r1 <- runif(3)
  set.seed(123)
  invisible(unique_id())
  r2 <- runif(3)
  expect_identical(r1, r2)
})

test_that("table_sink construction does not perturb the global RNG", {
  set.seed(99)
  before <- .Random.seed
  sink <- table_sink(NULL, format = "rds")
  expect_identical(.Random.seed, before)
  expect_true(dir.exists(sink$path))
  unlink(sink$path, recursive = TRUE)
})
