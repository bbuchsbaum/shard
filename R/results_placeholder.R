# Lightweight "no-results" placeholder used by shm_queue dispatch.
#
# This avoids allocating a giant list when results are intentionally not
# gathered (out-buffer workflows). It behaves like a list of NULLs for basic
# indexing and length().

.results_placeholder <- function(n) {
  structure(list(n = as.integer(n)), class = "shard_results_placeholder")
}

#' @param x A \code{shard_results_placeholder} object.
#' @param ... Further arguments (ignored).
#' @return The input \code{x}, invisibly.
#' @export
#' @noRd
print.shard_results_placeholder <- function(x, ...) {
  cat("<shard_results_placeholder>\n")
  cat("  count:", format(x$n, big.mark = ","), "\n")
  cat("  note: results are not gathered in shm_queue mode; use out= buffers/sinks.\n")
  invisible(x)
}

#' @param x A \code{shard_results_placeholder} object.
#' @return An integer scalar giving the number of placeholder results.
#' @export
#' @noRd
length.shard_results_placeholder <- function(x) {
  as.integer(x$n %||% 0L)
}

#' @param x A \code{shard_results_placeholder} object.
#' @param i Index.
#' @param ... Further arguments (ignored).
#' @return Always \code{NULL}; results are not gathered in shm_queue mode.
#' @export
#' @noRd
`[[.shard_results_placeholder` <- function(x, i, ...) {
  # Always NULL; tasks do not return gathered results in shm_queue mode.
  NULL
}

#' @param x A \code{shard_results_placeholder} object.
#' @param i Index or indices.
#' @param ... Further arguments (ignored).
#' @return A subset of the object (a list of \code{NULL} values).
#' @export
#' @noRd
`[.shard_results_placeholder` <- function(x, i, ...) {
  if (missing(i)) i <- seq_len(length(x))
  rep(list(NULL), length(i))
}
