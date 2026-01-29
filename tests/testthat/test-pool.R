test_that("pool_create creates workers", {
  skip_on_cran()

  pool <- pool_create(n = 2, rss_limit = "1GB")
  on.exit(pool_stop())

  expect_s3_class(pool, "shard_pool")
  expect_equal(pool$n, 2L)

  status <- pool_status()
  expect_equal(nrow(status), 2L)
  expect_true(all(status$status == "ok"))
})

test_that("pool_status shows worker information", {
  skip_on_cran()

  pool <- pool_create(n = 2)
  on.exit(pool_stop())

  status <- pool_status()

  expect_true("worker_id" %in% names(status))
  expect_true("pid" %in% names(status))
  expect_true("status" %in% names(status))
  expect_true("rss_bytes" %in% names(status))

  # PIDs should be valid
  expect_true(all(!is.na(status$pid)))
})

test_that("pool_dispatch executes code on workers", {
  skip_on_cran()

  pool <- pool_create(n = 2)
  on.exit(pool_stop())

  # Simple evaluation
  result <- pool_dispatch(1, quote(1 + 1))
  expect_equal(result, 2)

  # Get worker PID (should be different from main process)
  worker_pid <- pool_dispatch(1, quote(Sys.getpid()))
  expect_true(worker_pid != Sys.getpid())
})

test_that("pool_health_check detects dead workers", {
  skip_on_cran()

  pool <- pool_create(n = 2)
  on.exit(pool_stop())

  # Kill a worker
  worker_pid <- pool_dispatch(1, quote(Sys.getpid()))
  tools::pskill(worker_pid)
  Sys.sleep(0.2)  # Give it time to die

  # Health check should detect and restart
  health <- pool_health_check()

  # Worker 1 should have been restarted
  actions <- vapply(health$worker_actions, function(a) a$action, character(1))
  expect_true("restart" %in% actions)

  # Pool should have recorded a death
  pool <- pool_get()
  expect_gte(pool$stats$total_deaths, 1L)

  # Worker should be alive again
  status <- pool_status()
  expect_true(all(status$status == "ok"))
})

test_that("pool_stop clears the pool", {
  skip_on_cran()

  pool <- pool_create(n = 2)

  # Pool should exist
  expect_false(is.null(pool_get()))

  pool_stop()

  # Pool should be NULL after stop
  expect_null(pool_get())

  # Can create a new pool after stopping
  pool2 <- pool_create(n = 1)
  on.exit(pool_stop())
  expect_false(is.null(pool_get()))
})

test_that("pool_get returns current pool", {
  # Initially NULL
  pool_stop()  # Ensure clean state
  expect_null(pool_get())

  skip_on_cran()

  # After creation, returns pool
  pool <- pool_create(n = 1)
  on.exit(pool_stop())

  expect_identical(pool_get(), pool)
})

test_that("pool creates workers with packages loaded", {
  skip_on_cran()

  pool <- pool_create(n = 1, packages = c("stats"))
  on.exit(pool_stop())

  # stats::sd should be available
  result <- pool_dispatch(1, quote(sd(1:10)))
  expect_equal(result, sd(1:10))
})

test_that("print.shard_pool produces output", {
  skip_on_cran()

  pool <- pool_create(n = 2)
  on.exit(pool_stop())

  output <- capture.output(print(pool))
  expect_true(any(grepl("shard worker pool", output)))
  expect_true(any(grepl("Workers: 2", output)))
})
