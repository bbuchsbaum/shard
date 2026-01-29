test_that("queue_create initializes correctly", {
  chunks <- list(list(data = 1), list(data = 2), list(data = 3))
  q <- queue_create(chunks)

  expect_s3_class(q, "shard_queue")
  expect_equal(q$total, 3L)

  status <- queue_status(q)
  expect_equal(status$total, 3L)
  expect_equal(status$pending, 3L)
  expect_equal(status$in_flight, 0L)
  expect_equal(status$completed, 0L)
})

test_that("queue_next returns chunks and tracks in-flight", {
  chunks <- list(list(data = "a"), list(data = "b"))
  q <- queue_create(chunks)

  chunk1 <- queue_next(q, worker_id = 1)
  expect_equal(chunk1$data, "a")

  status <- queue_status(q)
  expect_equal(status$pending, 1L)
  expect_equal(status$in_flight, 1L)

  chunk2 <- queue_next(q, worker_id = 2)
  expect_equal(chunk2$data, "b")

  status <- queue_status(q)
  expect_equal(status$pending, 0L)
  expect_equal(status$in_flight, 2L)

  # No more chunks
  chunk3 <- queue_next(q, worker_id = 1)
  expect_null(chunk3)
})

test_that("queue_complete marks chunks done", {
  chunks <- list(list(data = 1))
  q <- queue_create(chunks)

  chunk <- queue_next(q, worker_id = 1)
  queue_complete(q, chunk$id, result = "done")

  status <- queue_status(q)
  expect_equal(status$completed, 1L)
  expect_equal(status$in_flight, 0L)

  results <- queue_results(q)
  expect_equal(results[[1]], "done")
})

test_that("queue_fail with requeue puts chunk back", {
  chunks <- list(list(data = 1))
  q <- queue_create(chunks)

  chunk <- queue_next(q, worker_id = 1)
  queue_fail(q, chunk$id, error = "oops", requeue = TRUE)

  status <- queue_status(q)
  expect_equal(status$pending, 1L)
  expect_equal(status$in_flight, 0L)
  expect_equal(status$total_retries, 1L)

  # Can get the chunk again
  chunk2 <- queue_next(q, worker_id = 2)
  expect_equal(chunk2$data, 1)
  expect_equal(chunk2$retry_count, 1L)
})

test_that("queue_fail without requeue marks permanent failure", {
  chunks <- list(list(data = 1))
  q <- queue_create(chunks)

  chunk <- queue_next(q, worker_id = 1)
  queue_fail(q, chunk$id, error = "fatal", requeue = FALSE)

  status <- queue_status(q)
  expect_equal(status$failed, 1L)
  expect_equal(status$pending, 0L)

  failures <- queue_failures(q)
  expect_length(failures, 1L)
})

test_that("queue_requeue_worker requeues worker's chunks", {
  chunks <- list(list(data = 1), list(data = 2), list(data = 3))
  q <- queue_create(chunks)

  # Assign chunks to workers
  queue_next(q, worker_id = 1)  # chunk 1 -> worker 1
  queue_next(q, worker_id = 2)  # chunk 2 -> worker 2
  queue_next(q, worker_id = 1)  # chunk 3 -> worker 1

  # Requeue all chunks from worker 1
  requeued <- queue_requeue_worker(q, worker_id = 1)
  expect_equal(requeued, 2L)

  status <- queue_status(q)
  expect_equal(status$pending, 2L)  # chunks 1 and 3 requeued
  expect_equal(status$in_flight, 1L)  # chunk 2 still in flight
})

test_that("queue_is_done reflects completion state", {
  chunks <- list(list(data = 1))
  q <- queue_create(chunks)

  expect_false(queue_is_done(q))

  chunk <- queue_next(q, worker_id = 1)
  expect_false(queue_is_done(q))  # Still in flight

  queue_complete(q, chunk$id)
  expect_true(queue_is_done(q))
})

test_that("print.shard_queue produces output", {
  chunks <- list(list(data = 1), list(data = 2))
  q <- queue_create(chunks)

  output <- capture.output(print(q))
  expect_true(any(grepl("shard chunk queue", output)))
  expect_true(any(grepl("Total chunks: 2", output)))
})
