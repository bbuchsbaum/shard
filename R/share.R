#' Create a Shared Immutable Memory Segment
#'
#' Produces a shared object with view semantics. The data is stored in shared
#' memory (mmap or POSIX shm) and can be accessed by worker processes without
#' serialization or copying.
#'
#' @param x R object to share (vectors, matrices, or arrays of atomic types)
#' @param backing Storage backend: "auto" (default), "mmap", or "shm"
#' @param path Optional file path for mmap backing
#' @param readonly Whether segment is read-only (default TRUE)
#'
#' @return An S3 object of class "shared_segment" with ALTREP-backed views
#'
#' @details
#' The shared segment is memory-mapped and accessible by all workers in a

#' shard_map() call without per-task serialization. Subsetting returns views
#' (not copies) when possible.
#'
#' Diagnostics tracked:
#' \itemize{
#'   \item \code{dataptr_calls}: Times DATAPTR was invoked
#'   \item \code{materialize_calls}: Times data was materialized
#'   \item \code{materialized_bytes}: Total bytes materialized
#' }
#'
#' @examples
#' \dontrun{
#' # Share a large matrix
#' X <- matrix(rnorm(1e6), nrow = 1000)
#' X_shared <- share(X)
#'
#' # Workers can access without copying
#' result <- shard_map(
#'   shards(ncol(X_shared)),
#'   borrow = list(X = X_shared),
#'   fun = function(idx, X) colMeans(X[, idx])
#' )
#' }
#'
#' @seealso \code{\link{buffer}}, \code{\link{shard_map}}, \code{\link{copy_report}}
#' @export
share <- function(x, backing = c("auto", "mmap", "shm"), path = NULL, readonly = TRUE) {

  backing <- match.arg(backing)


  # Validate input type

  if (!is.atomic(x)) {
    stop("share() requires atomic vectors, matrices, or arrays", call. = FALSE)
  }


  # TODO: Implement shared memory segment creation

  # This will call into C code via .Call()
  .NotYetImplemented()
}

#' @export
print.shared_segment <- function(x, ...) {
  cat("<shared_segment>\n")
  cat("  Type:", typeof(x), "\n")
  cat("  Dims:", paste(dim(x) %||% length(x), collapse = " x "), "\n")
  cat("  Backing:", attr(x, "backing"), "\n")
  cat("  Readonly:", attr(x, "readonly"), "\n")
  invisible(x)
}

#' @export
`[.shared_segment` <- function(x, i, j, ..., drop = TRUE) {
  # TODO: Return view when possible, track materialization otherwise
  .NotYetImplemented()
}
