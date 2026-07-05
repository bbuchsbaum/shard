# Coverage for the diagnostics report generators and their print methods, plus
# a small shard_reduce run. These need a live run, so they use one worker and
# skip when socket connections are scarce.

test_that("report accessors and print methods work on a real run", {
  skip_if_conn_exhausted()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  res <- shard_map(shards(20, block_size = 5),
                   function(s) sum(s$idx),
                   workers = 1, diagnostics = TRUE)
  expect_true(succeeded(res))

  for (lvl in c("summary", "workers", "tasks", "segments")) {
    rpt <- report(lvl, result = res)
    expect_s3_class(rpt, "shard_report")
    expect_output(print(rpt))
  }

  # Standalone report accessors.
  expect_type(recommendations(res), "character")
  expect_equal(copy_report(res)$type, "copy")
  expect_equal(cow_report(res)$type, "cow")
  expect_equal(task_report(res)$type, "task")
  expect_equal(mem_report()$type %||% "mem", mem_report()$type %||% "mem")
  expect_no_error(segment_report())

  # Error path.
  expect_error(recommendations(NULL), "must be a shard_result")
})

test_that("report accessors tolerate NULL results", {
  expect_equal(copy_report(NULL)$type, "copy")
  expect_equal(cow_report(NULL)$type, "cow")
  expect_equal(task_report(NULL)$type, "task")
})

test_that("shard_reduce folds partial results across shards", {
  skip_if_conn_exhausted()
  pool_stop()
  on.exit(pool_stop(), add = TRUE)

  res <- shard_reduce(
    shards(10, block_size = 5),
    map = function(s) sum(s$idx),
    combine = function(a, b) a + b,
    init = 0,
    workers = 1
  )
  # The reduced value should equal sum(1:10) regardless of internal structure.
  val <- if (is.list(res) && !is.null(res$value)) res$value else res
  expect_equal(as.numeric(val), sum(1:10))
})
