test_that("parse_bytes handles numeric input", {
  expect_equal(parse_bytes(1024), 1024)
  expect_equal(parse_bytes(1.5e9), 1.5e9)
})

test_that("parse_bytes handles byte strings", {
  expect_equal(parse_bytes("1024"), 1024)
  expect_equal(parse_bytes("1KB"), 1024)
  expect_equal(parse_bytes("1 KB"), 1024)
  expect_equal(parse_bytes("1K"), 1024)
  expect_equal(parse_bytes("1MB"), 1024^2)
  expect_equal(parse_bytes("1.5MB"), 1.5 * 1024^2)
  expect_equal(parse_bytes("1GB"), 1024^3)
  expect_equal(parse_bytes("2gb"), 2 * 1024^3)
  expect_equal(parse_bytes("1TB"), 1024^4)
})

test_that("parse_bytes errors on invalid input", {
  expect_error(parse_bytes("invalid"))
  expect_error(parse_bytes(""))
})

test_that("format_bytes produces readable strings", {
  expect_equal(format_bytes(500), "500 B")
  expect_equal(format_bytes(1024), "1.0 KB")
  expect_equal(format_bytes(1536), "1.5 KB")
  expect_equal(format_bytes(1024^2), "1.0 MB")
  expect_equal(format_bytes(1.5 * 1024^3), "1.5 GB")
  expect_equal(format_bytes(NA), "NA")
})

test_that("null coalescing operator works", {
  expect_equal(NULL %||% 5, 5)
  expect_equal(3 %||% 5, 3)
  expect_equal(NA %||% 5, NA)  # NA is not NULL
})

test_that("safe_div handles zero divisors", {
  expect_equal(safe_div(10, 2), 5)
  expect_true(is.na(safe_div(10, 0)))
  expect_equal(safe_div(c(10, 20), c(2, 0)), c(5, NA))
})

test_that("assert_positive_int validates correctly", {
  expect_equal(assert_positive_int(5), 5L)
  expect_equal(assert_positive_int(5.9), 5L)  # Truncates
  expect_error(assert_positive_int(0))
  expect_error(assert_positive_int(-1))
  expect_error(assert_positive_int(NA))
})

test_that("capture_time measures elapsed time", {
  result <- capture_time({
    Sys.sleep(0.1)
    42
  })

  expect_equal(result$result, 42)
  expect_gte(result$elapsed, 0.1)
  expect_lt(result$elapsed, 0.5)  # Should not take too long
})

test_that("unique_id generates unique IDs", {
  id1 <- unique_id("test")
  id2 <- unique_id("test")

  expect_true(startsWith(id1, "test_"))
  expect_true(startsWith(id2, "test_"))
  expect_false(id1 == id2)

  # Without prefix
  id3 <- unique_id()
  expect_false(startsWith(id3, "_"))
})
