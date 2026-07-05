# Direct unit tests for the chunk queue used by the dispatcher.

test_that("queue_next_where selects by predicate and falls back to FIFO", {
  q <- shard:::queue_create(list(list(id = 1L), list(id = 2L), list(id = 3L)))

  # Predicate picks a specific chunk out of order.
  ch <- shard:::queue_next_where(q, worker_id = 1L, predicate = function(c) c$id == 2L)
  expect_equal(ch$id, 2L)

  # NULL predicate falls back to queue_next (FIFO -> id 1).
  ch1 <- shard:::queue_next_where(q, worker_id = 1L, predicate = NULL)
  expect_equal(ch1$id, 1L)

  # Non-function predicate errors.
  expect_error(shard:::queue_next_where(q, 1L, predicate = 42), "predicate must be a function")

  # No matching chunk -> NULL.
  expect_null(shard:::queue_next_where(q, 1L, predicate = function(c) c$id == 999L))
})

test_that("queue_next_where returns NULL when nothing is pending", {
  q <- shard:::queue_create(list(list(id = 1L)))
  shard:::queue_next(q, 1L)  # drain the single chunk
  expect_null(shard:::queue_next_where(q, 1L, predicate = function(c) TRUE))
})

test_that("queue_fail on an unknown chunk id is a no-op", {
  q <- shard:::queue_create(list(list(id = 1L)))
  expect_null(shard:::queue_fail(q, "nonexistent"))
})

test_that("queue_results orders numeric ids and falls back to insertion order", {
  # Numeric ids -> numeric ordering.
  q <- shard:::queue_create(list(list(id = 2L), list(id = 1L)))
  c1 <- shard:::queue_next(q, 1L)  # id 2
  c2 <- shard:::queue_next(q, 1L)  # id 1
  shard:::queue_complete(q, c1$id, result = "two")
  shard:::queue_complete(q, c2$id, result = "one")
  res <- shard:::queue_results(q)
  expect_equal(names(res), c("1", "2"))
  expect_equal(res[["1"]], "one")

  # Non-numeric ids -> env$order fallback.
  q2 <- shard:::queue_create(list(list(id = "b"), list(id = "a")))
  d1 <- shard:::queue_next(q2, 1L)  # b
  d2 <- shard:::queue_next(q2, 1L)  # a
  shard:::queue_complete(q2, d1$id, result = "B")
  shard:::queue_complete(q2, d2$id, result = "A")
  res2 <- shard:::queue_results(q2)
  expect_equal(names(res2), c("b", "a"))

  # Empty queue -> empty list.
  q3 <- shard:::queue_create(list(list(id = 1L)))
  expect_equal(shard:::queue_results(q3), list())
})
