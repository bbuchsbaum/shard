# Regression tests for the 0.2.0 pre-release efficiency pass:
# cursor-based chunk queue, minimal completion retention, and n-dimensional
# buffer gather/scatter indexing.

test_that("queue serves chunks in order and requeues failures at the back", {
  chunks <- lapply(1:5, function(i) list(id = i, payload = i * 10))
  q <- shard:::queue_create(chunks)

  c1 <- shard:::queue_next(q, worker_id = 1L)
  c2 <- shard:::queue_next(q, worker_id = 2L)
  expect_identical(c1$id, 1L)
  expect_identical(c2$id, 2L)

  # Fail chunk 1 with requeue: it is served after the remaining pending chunks.
  shard:::queue_fail(q, 1L, error = "boom", requeue = TRUE)
  got <- vapply(1:4, function(i) shard:::queue_next(q, worker_id = 1L)$id, integer(1))
  expect_identical(got, c(3L, 4L, 5L, 1L))
  expect_null(shard:::queue_next(q, worker_id = 1L))

  # Retry metadata survived the requeue.
  st <- shard:::queue_status(q)
  expect_identical(st$total_retries, 1L)
  expect_identical(st$pending, 0L)
  expect_identical(st$in_flight, 5L)
})

test_that("queue_next_where claims from the middle without disturbing order", {
  chunks <- lapply(1:4, function(i) list(id = i, size = i))
  q <- shard:::queue_create(chunks)

  big <- shard:::queue_next_where(q, 1L, predicate = function(ch) ch$size >= 3L)
  expect_identical(big$id, 3L)

  # Remaining chunks come out in order, skipping the claimed slot.
  rest <- vapply(1:3, function(i) shard:::queue_next(q, 1L)$id, integer(1))
  expect_identical(rest, c(1L, 2L, 4L))
  expect_null(shard:::queue_next(q, 1L))
})

test_that("queue results are ordered by id regardless of completion order", {
  chunks <- lapply(1:4, function(i) list(id = i))
  q <- shard:::queue_create(chunks)
  for (i in 1:4) shard:::queue_next(q, 1L)

  # Complete out of order.
  for (i in c(3L, 1L, 4L, 2L)) {
    shard:::queue_complete(q, i, result = i * 100L, retain = FALSE)
  }

  expect_true(shard:::queue_is_done(q))
  res <- shard:::queue_results(q)
  expect_identical(unname(unlist(res)), c(100L, 200L, 300L, 400L))
  expect_identical(names(res), as.character(1:4))
})

test_that("minimal retention stores metadata only; failures keep full chunks", {
  chunks <- list(list(id = 1L, shards = as.list(1:100)),
                 list(id = 2L, shards = as.list(1:100)))
  q <- shard:::queue_create(chunks)
  shard:::queue_next(q, 1L)
  shard:::queue_next(q, 1L)

  shard:::queue_complete(q, 1L, result = "ok", retain = FALSE)
  done <- q$env$completed[[1L]]
  expect_identical(done$result, "ok")
  expect_null(done$shards)

  shard:::queue_fail(q, 2L, error = "boom", requeue = FALSE)
  fail <- shard:::queue_failures(q)[["2"]]
  expect_identical(length(fail$shards), 100L)
  expect_identical(fail$retry_count, 1L)
})

test_that("n-dimensional buffer indexing matches base array semantics", {
  set.seed(42)
  a <- array(rnorm(3 * 4 * 5), c(3, 4, 5))
  buf <- buffer("double", dim = c(3, 4, 5))
  on.exit(buffer_close(buf), add = TRUE)
  buf[1:3, 1:4, 1:5] <- a

  expect_equal(buf[1:2, c(4, 2), 2:3], a[1:2, c(4, 2), 2:3])
  expect_equal(buf[3, 2, 4], a[3, 2, 4])
  expect_equal(buf[1:3, 2, 4, drop = FALSE], a[1:3, 2, 4, drop = FALSE])
  expect_equal(buf[-1, 2:3, 5], a[-1, 2:3, 5])

  # Scatter assignment touches only the selection.
  b2 <- a
  buf[2, c(1, 3), c(2, 5)] <- 99
  b2[2, c(1, 3), c(2, 5)] <- 99
  expect_equal(buf[1:3, 1:4, 1:5], b2)

  # Contiguous slab (range fast path) and value recycling.
  buf[1:3, 1:4, 2] <- 7
  b2[1:3, 1:4, 2] <- 7
  buf[1:2, 1, 1] <- 0.5
  b2[1:2, 1, 1] <- 0.5
  expect_equal(buf[1:3, 1:4, 1:5], b2)

  expect_error(buf[1:2, 1:2], "incorrect number of dimensions")
  expect_error(buf[4, 1, 1], "out of bounds")
})

test_that("n-dimensional integer buffers round-trip", {
  ib <- buffer("integer", dim = c(2, 2, 2))
  on.exit(buffer_close(ib), add = TRUE)
  ib[1:2, 1:2, 1:2] <- 1:8
  expect_identical(ib[1:2, 1:2, 1:2], array(1:8, c(2, 2, 2)))
  expect_identical(ib[2, 2, 2], 8L)
})
