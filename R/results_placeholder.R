# Lightweight "no-results" placeholder used by shm_queue dispatch.
#
# This avoids allocating a giant list when results are intentionally not
# gathered (out-buffer workflows). It behaves like a list of NULLs for basic
# indexing and length().

.results_placeholder <- function(n) {
  structure(list(n = as.integer(n)), class = "shard_results_placeholder")
}

#' @export
print.shard_results_placeholder <- function(x, ...) {
  cat("<shard_results_placeholder>\n")
  cat("  count:", format(x$n, big.mark = ","), "\n")
  cat("  note: results are not gathered in shm_queue mode; use out= buffers/sinks.\n")
  invisible(x)
}

#' @export
length.shard_results_placeholder <- function(x) {
  as.integer(x$n %||% 0L)
}

#' @export
`[[.shard_results_placeholder` <- function(x, i, ...) {
  # Always NULL; tasks do not return gathered results in shm_queue mode.
  NULL
}

#' @export
`[.shard_results_placeholder` <- function(x, i, ...) {
  if (missing(i)) i <- seq_len(length(x))
  rep(list(NULL), length(i))
}

