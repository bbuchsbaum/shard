# Tests for diagnostics API

test_that("report() returns shard_report with correct structure", {
  # Without pool
  rpt <- report()
  expect_s3_class(rpt, "shard_report")
  expect_equal(rpt$level, "summary")
  expect_true(!is.null(rpt$timestamp))
  expect_true(!is.null(rpt$memory))

  # With pool
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  rpt <- report()
  expect_s3_class(rpt, "shard_report")
  expect_true(!is.null(rpt$pool))
  expect_equal(rpt$pool$n_workers, 2L)
})

test_that("report() respects detail levels", {
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Summary level - no workers
  rpt_summary <- report("summary")
  expect_null(rpt_summary$workers)
  expect_null(rpt_summary$segments)

  # Workers level - has workers
  rpt_workers <- report("workers")
  expect_true(!is.null(rpt_workers$workers))
  expect_length(rpt_workers$workers, 2)

  # Segments level - has segments
  rpt_segments <- report("segments")
  expect_true(!is.null(rpt_segments$segments))
  expect_true(!is.null(rpt_segments$workers))
})

test_that("report() includes shard_result diagnostics", {
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Use explicit block_size=1 to get exact shard count
  result <- shard_map(shards(10, block_size = 1), function(s) sum(s$idx), workers = 2)

  rpt <- report("tasks", result = result)
  expect_true(!is.null(rpt$tasks))
  expect_s3_class(rpt$tasks, "shard_report")
  expect_equal(rpt$tasks$shards_total, 10)
  expect_true(!is.null(rpt$result_diagnostics))
})

test_that("mem_report() returns memory statistics", {
  # Without pool
  rpt <- mem_report()
  expect_s3_class(rpt, "shard_report")
  expect_equal(rpt$type, "memory")
  expect_false(rpt$pool_active)

  # With pool
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  rpt <- mem_report()
  expect_true(rpt$pool_active)
  expect_equal(rpt$n_workers, 2)
  expect_length(rpt$workers, 2)
  expect_true(all(vapply(rpt$workers, function(w) !is.null(w$id), logical(1))))
})

test_that("mem_report() computes aggregates correctly", {
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  rpt <- mem_report()

  # Should have computed total, peak, mean from worker RSS values
  expect_true(is.numeric(rpt$total_rss))
  expect_true(is.numeric(rpt$peak_rss))
  expect_true(is.numeric(rpt$mean_rss))

  # Total should be sum of workers
  worker_rss <- vapply(rpt$workers, function(w) w$rss %||% 0, numeric(1))
  valid_rss <- worker_rss[!is.na(worker_rss)]
  if (length(valid_rss) > 0) {
    expect_equal(rpt$total_rss, sum(valid_rss))
  }
})

test_that("cow_report() returns COW statistics", {
  rpt <- cow_report()
  expect_s3_class(rpt, "shard_report")
  expect_equal(rpt$type, "cow")
  expect_true(is.na(rpt$policy))

  # With result
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  result <- shard_map(shards(5), function(s) s$idx, cow = "audit", workers = 2)
  rpt <- cow_report(result)
  expect_equal(rpt$policy, "audit")
})

test_that("copy_report() returns data copy statistics", {
  rpt <- copy_report()
  expect_s3_class(rpt, "shard_report")
  expect_equal(rpt$type, "copy")
  expect_equal(rpt$borrow_exports, 0L)

  # With result
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  result <- shard_map(shards(5), function(s) sum(s$idx), workers = 2)
  rpt <- copy_report(result)
  expect_equal(rpt$result_imports, 5)  # 5 shards = 5 results
  expect_true(rpt$result_bytes > 0)  # Results have some size
})

test_that("task_report() returns task execution statistics", {
  rpt <- task_report()
  expect_s3_class(rpt, "shard_report")
  expect_equal(rpt$type, "task")
  expect_equal(rpt$shards_total, 0L)

  # With result - use explicit block_size=1 for exact shard count
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  result <- shard_map(shards(10, block_size = 1), function(s) sum(s$idx), workers = 2)
  rpt <- task_report(result)

  expect_equal(rpt$shards_total, 10)
  expect_equal(rpt$shards_processed, 10)
  expect_equal(rpt$shards_failed, 0L)
  expect_true(!is.na(rpt$duration))
  expect_true(rpt$duration > 0)
  expect_true(!is.na(rpt$throughput))
  expect_true(rpt$throughput > 0)
})

test_that("task_report() reports failures correctly", {
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Create a task that fails for some shards - use block_size=1 for predictable shards
  result <- shard_map(shards(5, block_size = 1), function(s) {
    if (s$id == 3) stop("deliberate failure")
    sum(s$idx)
  }, workers = 2, max_retries = 1L)

  rpt <- task_report(result)
  expect_true(rpt$shards_failed >= 1)  # At least one failure
})

test_that("segment_report() returns segment information", {
  rpt <- segment_report()
  expect_s3_class(rpt, "shard_report")
  expect_equal(rpt$type, "segment")
  expect_true(!is.null(rpt$backing_summary))
  expect_true(!is.null(rpt$backing_summary$available))
  expect_true(!is.null(rpt$backing_summary$platform))
})

test_that("print methods work without error", {
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  result <- shard_map(shards(5), function(s) sum(s$idx), workers = 2)

  # Test all print methods
  expect_output(print(report()))
  expect_output(print(report("workers")))
  expect_output(print(report("tasks", result = result)))
  expect_output(print(report("segments")))
  expect_output(print(mem_report()))
  expect_output(print(cow_report()))
  expect_output(print(cow_report(result)))
  expect_output(print(copy_report()))
  expect_output(print(copy_report(result)))
  expect_output(print(task_report()))
  expect_output(print(task_report(result)))
  expect_output(print(segment_report()))
})

test_that("report() handles missing/dead workers", {
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  # Get report with live workers
  rpt <- report("workers")

  for (w in rpt$workers) {
    expect_true(w$status %in% c("alive", "ok", "missing", "dead"))
  }
})

test_that("recommendations() is deterministic given the same run telemetry", {
  skip_on_cran()

  pool_stop()
  pool_create(2)
  on.exit(pool_stop(), add = TRUE)

  X <- share(matrix(rnorm(200), nrow = 20), backing = "mmap")
  blocks <- shards(10, block_size = 1)

  # Force view materialization inside the task.
  result <- shard_map(blocks, borrow = list(X = X), fun = function(s, X) {
    v <- view_block(X, cols = idx_range(1, 5))
    sum(materialize(v))
  }, workers = 2, diagnostics = TRUE)

  r1 <- recommendations(result)
  r2 <- recommendations(result)
  expect_identical(r1, r2)
  expect_true(any(grepl("Views were materialized", r1, fixed = TRUE)))
})

test_that("reports have consistent timestamp format", {
  rpt1 <- report()
  rpt2 <- mem_report()
  rpt3 <- cow_report()
  rpt4 <- copy_report()
  rpt5 <- task_report()
  rpt6 <- segment_report()

  expect_s3_class(rpt1$timestamp, "POSIXct")
  expect_s3_class(rpt2$timestamp, "POSIXct")
  expect_s3_class(rpt3$timestamp, "POSIXct")
  expect_s3_class(rpt4$timestamp, "POSIXct")
  expect_s3_class(rpt5$timestamp, "POSIXct")
  expect_s3_class(rpt6$timestamp, "POSIXct")
})
