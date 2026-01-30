#' @title Utility Functions
#' @description Internal utilities for shard package.
#' @name utils
NULL

#' Null-coalescing Operator
#'
#' @name null_coalesce
#' @aliases %||%
#' @param x Value to check.
#' @param y Default value if x is NULL.
#' @return x if not NULL, otherwise y.
#' @keywords internal
NULL

#' @rdname null_coalesce
`%||%` <- function(x, y) {

  if (is.null(x)) y else x
}

#' Parse Human-Readable Byte Strings
#'
#' Converts strings like "2GB", "512MB", "1.5GB" to bytes.
#'
#' @param x Character or numeric. If numeric, returned as-is.
#' @return Numeric value in bytes.
#' @keywords internal
parse_bytes <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x <- toupper(trimws(x))

  # Extract number and unit
  match <- regexec("^([0-9.]+)\\s*(B|KB|MB|GB|TB|K|M|G|T)?$", x)
  parts <- regmatches(x, match)[[1]]

  if (length(parts) < 2) {
    stop("Cannot parse byte string: ", x, call. = FALSE)
  }

  value <- as.numeric(parts[2])
  unit <- if (length(parts) >= 3) parts[3] else "B"

  multiplier <- switch(
    unit,
    "B" = 1,
    "K" = , "KB" = 1024,
    "M" = , "MB" = 1024^2,
    "G" = , "GB" = 1024^3,
    "T" = , "TB" = 1024^4,
    1
  )

  value * multiplier
}

#' Format Bytes as Human-Readable String
#'
#' @param bytes Numeric value in bytes.
#' @param digits Integer. Decimal places (default 1).
#' @return Character string like "1.5 GB".
#' @keywords internal
format_bytes <- function(bytes, digits = 1) {
  if (is.na(bytes)) {
    return("NA")
  }

  if (bytes < 1024) {
    return(paste(bytes, "B"))
  }

  units <- c("B", "KB", "MB", "GB", "TB", "PB")
  exp <- min(floor(log(bytes, 1024)), length(units) - 1)
  value <- bytes / (1024^exp)

  sprintf(paste0("%.", digits, "f %s"), value, units[exp + 1])
}

#' Safe Divide
#'
#' Division that returns NA instead of Inf/NaN for zero divisors.
#'
#' @param x Numerator.
#' @param y Denominator.
#' @return x/y or NA if y is zero.
#' @keywords internal
safe_div <- function(x, y) {
  ifelse(y == 0, NA_real_, x / y)
}

#' Retry with Backoff
#'
#' Retries a function call with exponential backoff.
#'
#' @param f Function to call.
#' @param max_attempts Integer. Maximum retry attempts.
#' @param initial_delay Numeric. Initial delay in seconds.
#' @param max_delay Numeric. Maximum delay in seconds.
#' @param on_error Function. Called on each error with (attempt, error).
#' @return Result of f() or error on final failure.
#' @keywords internal
retry_with_backoff <- function(f, max_attempts = 3, initial_delay = 0.1,
                               max_delay = 5, on_error = NULL) {
  delay <- initial_delay

  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      list(success = TRUE, value = f()),
      error = function(e) list(success = FALSE, error = e)
    )

    if (result$success) {
      return(result$value)
    }

    if (!is.null(on_error)) {
      on_error(attempt, result$error)
    }

    if (attempt < max_attempts) {
      Sys.sleep(delay)
      delay <- min(delay * 2, max_delay)
    }
  }

  stop(result$error)
}

#' Check Package Availability
#'
#' @param pkg Package name.
#' @return Logical. TRUE if package is available.
#' @keywords internal
pkg_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

#' Generate Unique ID
#'
#' Creates a short unique identifier.
#'
#' @param prefix Character. Prefix for the ID.
#' @return Character string like "prefix_a1b2c3".
#' @keywords internal
unique_id <- function(prefix = "") {
  # Use high-resolution time + random component for uniqueness
  timestamp <- as.numeric(Sys.time()) * 1e6
  random_part <- paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
  hash <- sprintf("%s_%s", format(timestamp, scientific = FALSE), random_part)

  if (nchar(prefix) > 0) {
    paste0(prefix, "_", hash)
  } else {
    hash
  }
}

#' Validate Positive Integer
#'
#' @param x Value to validate.
#' @param name Name for error messages.
#' @return Integer value.
#' @keywords internal
assert_positive_int <- function(x, name = "value") {
  x <- as.integer(x)
  if (is.na(x) || x < 1L) {
    stop(name, " must be a positive integer", call. = FALSE)
  }
  x
}

#' Capture Time
#'
#' Simple timing wrapper.
#'
#' @param expr Expression to time.
#' @return List with result and elapsed time.
#' @keywords internal
capture_time <- function(expr) {
  start <- Sys.time()
  result <- force(expr)
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  list(result = result, elapsed = elapsed)
}
