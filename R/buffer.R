#' Create a Typed Writable Output Buffer
#'
#' Creates a shared memory buffer that supports disjoint writes by shard index.
#' Multiple workers can write to non-overlapping regions simultaneously.
#'
#' @param type Data type: "double", "integer", "logical", or "raw"
#' @param dim Dimensions of buffer (vector for 1D, or matrix/array dims)
#' @param backing Storage backend: "auto" (default), "mmap", or "shm"
#' @param init Initial value (default NA)
#' @param path Optional file path for mmap backing
#'
#' @return An S3 object of class "shard_buffer" with slice-addressable writes
#'
#' @details
#' Buffers are designed for parallel output collection. Each worker writes
#' to its assigned shard indices without needing locks. Buffer contents
#' persist across worker recycling.
#'
#' @examples
#' \dontrun{
#' # Create output buffer for column means
#' n_cols <- 1000
#' out <- buffer("double", n_cols)
#'
#' # Parallel computation writes directly to buffer
#' shard_map(
#'   shards(n_cols),
#'   borrow = list(X = X_shared),
#'   out = list(means = out),
#'   fun = function(idx, X, means) {
#'     means[idx] <- colMeans(X[, idx, drop = FALSE])
#'   }
#' )
#'
#' # Access results
#' results <- as.vector(out)
#' }
#'
#' @seealso \code{\link{share}}, \code{\link{shard_map}}
#' @export
buffer <- function(type = c("double", "integer", "logical", "raw"),
                   dim,
                   backing = c("auto", "mmap", "shm"),
                   init = NA,
                   path = NULL) {

  type <- match.arg(type)
  backing <- match.arg(backing)


  if (missing(dim) || !is.numeric(dim) || any(dim < 1)) {
    stop("'dim' must be positive integer dimensions", call. = FALSE)
  }


  # TODO: Implement shared buffer creation

  # This will call into C code via .Call()
  .NotYetImplemented()
}

#' @export
print.shard_buffer <- function(x, ...) {
  cat("<shard_buffer>\n")
  cat("  Type:", attr(x, "type"), "\n")
  cat("  Dims:", paste(dim(x) %||% length(x), collapse = " x "), "\n")
  cat("  Backing:", attr(x, "backing"), "\n")
  invisible(x)
}

#' @export
`[<-.shard_buffer` <- function(x, i, j, ..., value) {
  # TODO: Implement slice assignment
  .NotYetImplemented()
}
