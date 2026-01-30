# Worker-local scratch pool (R-level).
#
# This is a simple reusable allocator for native/R kernels that want to avoid
# repeated malloc/free churn. It is intentionally conservative: the pool holds
# at most one buffer per key and can request worker recycle if it grows beyond a
# configured limit.

.scratch_env <- new.env(parent = emptyenv())
.scratch_env$pool <- new.env(parent = emptyenv())
.scratch_env$hits <- 0L
.scratch_env$misses <- 0L
.scratch_env$bytes <- 0
.scratch_env$high_water <- 0
.scratch_env$max_bytes <- Inf
.scratch_env$needs_recycle <- FALSE

#' Configure scratch pool limits
#'
#' @param max_bytes Maximum scratch pool bytes allowed in a worker. If exceeded,
#'   the worker is flagged for recycle at the next safe point.
#' @export
scratch_pool_config <- function(max_bytes = Inf) {
  if (is.character(max_bytes)) max_bytes <- parse_bytes(max_bytes)
  max_bytes <- as.double(max_bytes)
  if (!is.finite(max_bytes) || max_bytes <= 0) stop("max_bytes must be positive", call. = FALSE)
  .scratch_env$max_bytes <- max_bytes
  invisible(NULL)
}

scratch_reset_diagnostics <- function() {
  .scratch_env$hits <- 0L
  .scratch_env$misses <- 0L
  .scratch_env$bytes <- 0
  .scratch_env$high_water <- 0
  .scratch_env$needs_recycle <- FALSE
  # Keep the pool; reset is for counters.
  invisible(NULL)
}

#' Scratch pool diagnostics
#'
#' @return A list with counters and current pool bytes.
#' @export
scratch_diagnostics <- function() {
  list(
    hits = .scratch_env$hits,
    misses = .scratch_env$misses,
    bytes = .scratch_env$bytes,
    high_water = .scratch_env$high_water,
    max_bytes = .scratch_env$max_bytes,
    needs_recycle = isTRUE(.scratch_env$needs_recycle)
  )
}

.scratch_update_bytes <- function() {
  keys <- ls(.scratch_env$pool, all.names = TRUE)
  total <- 0
  for (k in keys) {
    x <- get(k, envir = .scratch_env$pool, inherits = FALSE)
    total <- total + as.double(utils::object.size(x))
  }
  .scratch_env$bytes <- total
  if (total > (.scratch_env$high_water %||% 0)) .scratch_env$high_water <- total
  if (is.finite(.scratch_env$max_bytes) && total > .scratch_env$max_bytes) {
    .scratch_env$needs_recycle <- TRUE
  }
  invisible(NULL)
}

.scratch_get_double <- function(n, key) {
  n <- as.integer(n)
  if (is.na(n) || n < 0L) stop("n must be >= 0", call. = FALSE)
  key <- as.character(key)
  if (!nzchar(key)) stop("key must be non-empty", call. = FALSE)

  if (exists(key, envir = .scratch_env$pool, inherits = FALSE)) {
    buf <- get(key, envir = .scratch_env$pool, inherits = FALSE)
    if (is.double(buf) && length(buf) >= n) {
      .scratch_env$hits <- .scratch_env$hits + 1L
      return(buf)
    }
  }

  .scratch_env$misses <- .scratch_env$misses + 1L
  buf <- double(n)
  assign(key, buf, envir = .scratch_env$pool)
  .scratch_update_bytes()
  buf
}

#' Get a scratch matrix
#'
#' Allocates (or reuses) a double matrix in the worker scratch pool.
#'
#' @param nrow,ncol Dimensions.
#' @param key Optional key to control reuse. Defaults to a shape-derived key.
#' @export
scratch_matrix <- function(nrow, ncol, key = NULL) {
  nrow <- as.integer(nrow)
  ncol <- as.integer(ncol)
  if (is.na(nrow) || nrow < 0L) stop("nrow must be >= 0", call. = FALSE)
  if (is.na(ncol) || ncol < 0L) stop("ncol must be >= 0", call. = FALSE)
  if (is.null(key)) key <- paste0("mat_", nrow, "x", ncol)
  buf <- .scratch_get_double(nrow * ncol, key = key)
  dim(buf) <- c(nrow, ncol)
  buf
}
