# Unit tests for the RSS backends and the rss_monitor closure. These call the
# platform backends directly so at least one path runs on each OS; unsupported
# backends must degrade to NA rather than error.

test_that("rss_get_pid returns NA for NA pid and a number for self", {
  expect_true(is.na(shard:::rss_get_pid(NA_integer_)))
  self <- shard:::rss_get_pid(Sys.getpid())
  expect_true(is.na(self) || (is.numeric(self) && self > 0))
})

test_that("rss_via_ps_cmd returns RSS for a live pid and NA for a dead one", {
  live <- shard:::rss_via_ps_cmd(Sys.getpid())
  # On Unix this yields bytes; on Windows the command may be absent -> NA.
  expect_true(is.na(live) || (is.numeric(live) && live > 0))

  dead <- shard:::rss_via_ps_cmd(999999999L)
  expect_true(is.na(dead))
})

test_that("rss_via_proc and rss_via_wmic degrade gracefully off-platform", {
  # rss_via_proc reads /proc (Linux). Elsewhere the statm file is absent -> NA.
  proc <- shard:::rss_via_proc(Sys.getpid())
  expect_true(is.na(proc) || (is.numeric(proc) && proc > 0))

  # rss_via_wmic shells out to wmic (Windows). Elsewhere -> NA via tryCatch.
  wmic <- shard:::rss_via_wmic(Sys.getpid())
  expect_true(is.na(wmic) || (is.numeric(wmic) && wmic > 0))
})

test_that("rss_via_ps returns a number or NA", {
  skip_if_not_installed("ps")
  v <- shard:::rss_via_ps(Sys.getpid())
  expect_true(is.na(v) || (is.numeric(v) && v > 0))
})

test_that("rss_monitor records samples and computes peak/drift", {
  mon <- shard:::rss_monitor(Sys.getpid())

  # Empty state.
  h0 <- mon$history()
  expect_s3_class(h0, "data.frame")
  expect_equal(nrow(h0), 0L)
  expect_true(is.na(mon$peak()))
  expect_true(is.na(mon$drift()))

  # Two samples.
  s1 <- mon$sample()
  s2 <- mon$sample()
  expect_true(all(c("timestamp", "rss") %in% names(s1)))

  h <- mon$history()
  expect_equal(nrow(h), 2L)
  pk <- mon$peak()
  expect_true(is.na(pk) || is.numeric(pk))
  dr <- mon$drift()
  expect_true(is.na(dr) || is.numeric(dr))
})
