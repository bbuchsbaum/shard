test_that("rss_self returns current process RSS", {
  rss <- rss_self()

  # Should be either a positive number or NA (if monitoring unavailable)
  if (!is.na(rss)) {
    expect_true(rss > 0)
    # R process should use at least some memory (10MB minimum)
    expect_gt(rss, 10 * 1024^2)
  }
})

test_that("rss_get_pid returns NA for invalid PID", {
  expect_true(is.na(rss_get_pid(NA)))
  expect_silent({
    rss <- rss_get_pid(999999999) # Unlikely to exist
  })
  expect_true(is.na(rss))
})

test_that("rss_get_pid works for current process", {
  expect_silent({
    rss <- rss_get_pid(Sys.getpid())
  })

  # Should match rss_self
  rss_self_val <- rss_self()

  if (!is.na(rss) && !is.na(rss_self_val)) {
    # Allow some variance (10%)
    expect_true(abs(rss - rss_self_val) / rss_self_val < 0.1)
  }
})

test_that("rss_monitor tracks samples over time", {
  mon <- rss_monitor(Sys.getpid())

  # Take some samples
  mon$sample()
  Sys.sleep(0.05)
  mon$sample()
  Sys.sleep(0.05)
  mon$sample()

  hist <- mon$history()
  expect_equal(nrow(hist), 3L)
  expect_true("timestamp" %in% names(hist))
  expect_true("rss" %in% names(hist))

  # Peak should be valid
  peak <- mon$peak()
  if (!is.na(peak)) {
    expect_gt(peak, 0)
  }
})

test_that("rss_monitor drift calculation", {
  mon <- rss_monitor(Sys.getpid())

  # Single sample - drift should be NA
  mon$sample()
  expect_true(is.na(mon$drift()))

  # Second sample - drift should be calculable
  mon$sample()
  drift <- mon$drift()

  # Drift should be small for stable process
  if (!is.na(drift)) {
    expect_true(abs(drift) < 0.5)  # Less than 50% change
  }
})
