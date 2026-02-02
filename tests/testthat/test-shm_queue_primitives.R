test_that("taskq_supported returns logical", {
  result <- shard:::taskq_supported()
  expect_true(is.logical(result))
  expect_length(result, 1)
})

test_that("taskq_create creates valid task queue", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  q <- shard:::taskq_create(10)

  expect_true(is.list(q))
  expect_true("desc" %in% names(q))
  expect_true("owner" %in% names(q))

  desc <- q$desc
  expect_equal(desc$n_tasks, 10L)
  expect_true(!is.null(desc$path))
  expect_true(!is.null(desc$backing))
  expect_true(!is.null(desc$size))
})

test_that("taskq_create validates n_tasks", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  expect_error(shard:::taskq_create(0), "n_tasks must be >= 1")
  expect_error(shard:::taskq_create(-1), "n_tasks must be >= 1")
})

test_that("taskq_open opens existing queue", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  q <- shard:::taskq_create(10, backing = "mmap")
  seg <- shard:::taskq_open(q$desc, readonly = FALSE)

  expect_s3_class(seg, "shard_segment")
})

test_that("taskq_open validates descriptor", {
  expect_error(shard:::taskq_open(NULL), "desc must be a task queue descriptor")
  expect_error(shard:::taskq_open(list()), "desc must be a task queue descriptor")
  expect_error(shard:::taskq_open(list(path = NULL, backing = "mmap")),
               "desc must be a task queue descriptor")
})

test_that("taskq_stats returns queue statistics", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  q <- shard:::taskq_create(10, backing = "mmap")
  seg <- shard:::taskq_open(q$desc)

  stats <- shard:::taskq_stats(seg)

  expect_true(is.list(stats))
  expect_true("n_tasks" %in% names(stats))
  expect_true("done" %in% names(stats))
  expect_true("failed" %in% names(stats))
  expect_true("retries" %in% names(stats))

  # Initially no tasks done or failed
  expect_equal(stats$n_tasks, 10L)
  expect_equal(stats$done, 0L)
  expect_equal(stats$failed, 0L)
})

test_that("taskq_claim claims a pending task", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  q <- shard:::taskq_create(5, backing = "mmap")
  seg <- shard:::taskq_open(q$desc)

  # Worker 1 claims a task
  task_id <- shard:::taskq_claim(seg, worker_id = 1L)

  # Should get a valid task id (1-5)
  expect_true(task_id >= 1L && task_id <= 5L)
})

test_that("taskq_claim returns 0 when no tasks available", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  q <- shard:::taskq_create(2, backing = "mmap")
  seg <- shard:::taskq_open(q$desc)

  # Claim both tasks
  t1 <- shard:::taskq_claim(seg, worker_id = 1L)
  t2 <- shard:::taskq_claim(seg, worker_id = 1L)

  # Mark both as done
  shard:::taskq_done(seg, t1)
  shard:::taskq_done(seg, t2)

  # No more tasks available - returns 0
  t3 <- shard:::taskq_claim(seg, worker_id = 1L)
  expect_equal(t3, 0L)
})

test_that("taskq_done marks task as completed", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  q <- shard:::taskq_create(3, backing = "mmap")
  seg <- shard:::taskq_open(q$desc)

  task_id <- shard:::taskq_claim(seg, worker_id = 1L)
  shard:::taskq_done(seg, task_id)

  stats <- shard:::taskq_stats(seg)
  expect_equal(stats$done, 1L)
})

test_that("taskq_error handles retry logic", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  q <- shard:::taskq_create(3, backing = "mmap")
  seg <- shard:::taskq_open(q$desc)

  task_id <- shard:::taskq_claim(seg, worker_id = 1L)

  # First error - returns logical indicating if task was requeued
  result <- shard:::taskq_error(seg, task_id, max_retries = 2L)

  expect_true(is.logical(result))
})

test_that("taskq_failures returns failed task info", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  q <- shard:::taskq_create(3, backing = "mmap")
  seg <- shard:::taskq_open(q$desc)

  # Claim and fail a task with no retries
  task_id <- shard:::taskq_claim(seg, worker_id = 1L)
  shard:::taskq_error(seg, task_id, max_retries = 0L)

  failures <- shard:::taskq_failures(seg)

  # Returns list structure
  expect_true(is.list(failures))
  expect_true("task_id" %in% names(failures))
  expect_true("retry_count" %in% names(failures))
})

test_that("taskq_reset_claims works without error", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  q <- shard:::taskq_create(5, backing = "mmap")
  seg <- shard:::taskq_open(q$desc)

  # Worker 1 claims tasks but "dies" before completing
  t1 <- shard:::taskq_claim(seg, worker_id = 1L)
  t2 <- shard:::taskq_claim(seg, worker_id = 1L)

  # Reset claims from worker 1 - should not error
  expect_no_error(shard:::taskq_reset_claims(seg, worker_id = 1L))
})

test_that("taskq with mmap backing", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  # mmap backing
  q_mmap <- shard:::taskq_create(5, backing = "mmap")
  expect_equal(q_mmap$desc$backing, "mmap")
})

test_that("taskq handles many tasks", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  n <- 1000L
  q <- shard:::taskq_create(n, backing = "mmap")
  seg <- shard:::taskq_open(q$desc)

  stats <- shard:::taskq_stats(seg)
  expect_equal(stats$n_tasks, n)
})

test_that("taskq multiple workers can claim tasks", {
  if (!shard:::taskq_supported()) skip("taskq not supported on this platform")

  q <- shard:::taskq_create(10, backing = "mmap")
  seg <- shard:::taskq_open(q$desc)

  # Different workers claim tasks
  t1 <- shard:::taskq_claim(seg, worker_id = 1L)
  t2 <- shard:::taskq_claim(seg, worker_id = 2L)
  t3 <- shard:::taskq_claim(seg, worker_id = 1L)

  # All should be valid task ids
  expect_true(all(c(t1, t2, t3) >= 1L))

  # Mark all done
  shard:::taskq_done(seg, t1)
  shard:::taskq_done(seg, t2)
  shard:::taskq_done(seg, t3)

  stats <- shard:::taskq_stats(seg)
  expect_equal(stats$done, 3L)
})
