count_localhost_connections <- function() {
  con <- showConnections(all = TRUE)
  if (is.null(dim(con)) || nrow(con) == 0L) {
    return(0L)
  }

  sum(grepl("localhost", con[, "description"], fixed = TRUE))
}

wait_for_pid_exit <- function(pid, timeout = 2) {
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    if (!pid_is_alive(pid)) {
      return(TRUE)
    }
    Sys.sleep(0.05)
  }

  !pid_is_alive(pid)
}

test_that("pool_stop closes sockets for already-dead workers", {
  skip_on_cran()
  skip_if_conn_exhausted()
  if (is_windows()) skip("signal-based worker tests require POSIX")

  baseline <- count_localhost_connections()
  pool_create(n = 1)

  worker <- pool_get()$workers[[1]]
  worker_pid <- worker$pid

  expect_equal(count_localhost_connections(), baseline + 1L)

  tools::pskill(worker_pid, signal = 9L)
  expect_true(wait_for_pid_exit(worker_pid))

  pool_stop()

  expect_null(pool_get())
  expect_equal(count_localhost_connections(), baseline)
})

test_that("pool_health_check restarts dead workers without leaking sockets", {
  skip_on_cran()
  skip_if_conn_exhausted()
  if (is_windows()) skip("signal-based worker tests require POSIX")

  baseline <- count_localhost_connections()
  pool_create(n = 1)
  on.exit(pool_stop(), add = TRUE)

  old_worker <- pool_get()$workers[[1]]
  old_pid <- old_worker$pid

  tools::pskill(old_pid, signal = 9L)
  expect_true(wait_for_pid_exit(old_pid))

  health <- pool_health_check()
  new_worker <- pool_get()$workers[[1]]

  expect_equal(health$worker_actions[[1]]$action, "restart")
  expect_true(worker_is_alive(new_worker))
  expect_false(identical(new_worker$pid, old_pid))
  expect_equal(count_localhost_connections(), baseline + 1L)
})

test_that("pool_dispatch restarts dead workers without leaking sockets", {
  skip_on_cran()
  skip_if_conn_exhausted()
  if (is_windows()) skip("signal-based worker tests require POSIX")

  baseline <- count_localhost_connections()
  pool_create(n = 1)
  on.exit(pool_stop(), add = TRUE)

  old_worker <- pool_get()$workers[[1]]
  old_pid <- old_worker$pid

  tools::pskill(old_pid, signal = 9L)
  expect_true(wait_for_pid_exit(old_pid))

  expect_equal(pool_dispatch(1, quote(21 + 21)), 42)
  expect_true(worker_is_alive(pool_get()$workers[[1]]))
  expect_false(identical(pool_get()$workers[[1]]$pid, old_pid))
  expect_equal(count_localhost_connections(), baseline + 1L)
})

test_that("pool_dispatch timeout kills the hung worker and leaves the pool reusable", {
  skip_on_cran()
  skip_if_conn_exhausted()

  baseline <- count_localhost_connections()
  pool_create(n = 1)
  on.exit(pool_stop(), add = TRUE)

  start_time <- Sys.time()
  expect_error(
    pool_dispatch(1, quote(Sys.sleep(2)), timeout = 0.2),
    "timed out"
  )
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_lt(elapsed, 1)
  expect_false(worker_is_alive(pool_get()$workers[[1]]))
  expect_equal(count_localhost_connections(), baseline)

  expect_equal(pool_dispatch(1, quote(1 + 1)), 2)
  expect_true(worker_is_alive(pool_get()$workers[[1]]))
  expect_equal(count_localhost_connections(), baseline + 1L)
})
