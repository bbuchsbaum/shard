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

test_that("pool_stop waits for workers to terminate", {
  skip_on_cran()

  pool <- pool_create(n = 2)

  # Get worker PIDs before stopping
  pids <- vapply(pool$workers, function(w) w$pid, integer(1))
  expect_true(all(!is.na(pids)))

  # Workers should be alive
  alive_before <- vapply(pids, pid_is_alive, logical(1))
  expect_true(all(alive_before))

  pool_stop()

  # Workers should be dead after pool_stop returns
  # Allow small buffer for OS process cleanup
  Sys.sleep(0.3)
  alive_after <- vapply(pids, pid_is_alive, logical(1))
  expect_true(all(!alive_after))
})

test_that("pool_stop returns immediately when workers already dead", {
  skip_on_cran()

  pool <- pool_create(n = 2)
  pids <- vapply(pool$workers, function(w) w$pid, integer(1))

  # Kill workers manually first
  for (pid in pids) {
    tools::pskill(pid, signal = 9L)
  }
  Sys.sleep(0.3)  # Wait for processes to die

  # Verify workers are dead
  expect_true(all(!vapply(pids, pid_is_alive, logical(1))))

  # pool_stop should return quickly (fast path)
  start_time <- Sys.time()
  pool_stop()
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  # Should complete in well under the default 5s timeout
  expect_lt(elapsed, 1)
  expect_null(pool_get())
})

test_that("pool_stop respects timeout parameter", {
  skip_on_cran()

  pool <- pool_create(n = 1)

  # Stop with explicit timeout
  pool_stop(timeout = 2)
  expect_null(pool_get())
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

test_that("pool_health_check survives an unreadable worker RSS", {
  skip_on_cran()

  # Bug discriminator: this mocked case fails on 0.2.0.
  # worker_rss() returns NA when a worker exits between the liveness probe
  # and the RSS read, or when the platform reader fails. Comparing NA against
  # the limit used to abort the whole health check with "missing value where
  # TRUE/FALSE needed", so a worker dying at the wrong moment took the check
  # down with it.
  pool_create(n = 2)
  on.exit(pool_stop(), add = TRUE)

  testthat::local_mocked_bindings(
    worker_rss = function(worker) NA_real_,
    .package = "shard"
  )

  health <- pool_health_check()

  actions <- vapply(health$worker_actions, function(a) a$action, character(1))
  expect_true(all(actions == "none"))

  reasons <- vapply(health$worker_actions, function(a) a$reason, character(1))
  expect_true(all(reasons == "rss_unavailable"))

  # The report must not claim health it never established.
  expect_output(print(health), "unreadable")
})

test_that("pool_health_check survives an NA RSS baseline", {
  skip_on_cran()

  # Bug discriminator: this fixes the baseline independently of the reader,
  # and the same experiment fails on 0.2.0.
  # The baseline is captured with the same reader at spawn, so it can be NA
  # while the current reading succeeds. That path also fed NA into the
  # comparison. Mock a fixed reading so the assertion does not depend on the
  # platform RSS reader working -- the very thing this fix tolerates.
  pool_create(n = 1)
  on.exit(pool_stop(), add = TRUE)

  testthat::local_mocked_bindings(
    worker_rss = function(worker) 1024,
    .package = "shard"
  )

  pool <- pool_get()
  pool$workers[[1]]$rss_baseline <- NA_real_
  assign("pool", pool, envir = shard:::.pool_env)

  expect_no_error(pool_health_check())
  expect_equal(pool_get()$workers[[1]]$rss_baseline, 1024)
})

test_that("pool_create rejects NA memory thresholds", {
  skip_on_cran()

  # These feed the same comparison that the NA-RSS guard protects; an NA here
  # would abort pool_health_check() and the dispatch calling it.
  expect_error(pool_create(n = 1, rss_limit = NA_real_), "rss_limit")
  expect_error(pool_create(n = 1, rss_drift_threshold = NA_real_),
               "rss_drift_threshold")
})

test_that("pool_create rejects non-numeric memory thresholds", {
  skip_on_cran()

  # A character threshold would make the health check compare against a
  # string rather than error -- the same class of defect as the NA case.
  expect_error(pool_create(n = 1, rss_drift_threshold = "big"),
               "rss_drift_threshold")
})
