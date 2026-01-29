#' Create Shard Descriptors for Parallel Iteration
#'
#' Produces shard descriptors (index vectors or range pairs) for use with
#' \code{\link{shard_map}}. Supports automatic block size tuning based on
#' worker count and expected memory usage.
#'
#' @param n Total number of items to shard
#' @param block_size Items per shard. Either "auto" for automatic tuning,
#'   or a positive integer.
#' @param strategy Index assignment strategy: "contiguous" assigns consecutive
#'   indices per shard, "strided" interleaves indices across shards.
#'
#' @return A list of class "shard_descriptors" containing index information
#'   for each shard
#'
#' @details
#' When \code{block_size = "auto"}, the function considers:
#' \itemize{
#'   \item Number of available workers
#'   \item Expected scratch memory per iteration
#'   \item Overhead of task dispatch
#' }
#'
#' The "contiguous" strategy is typically better for cache locality.
#' The "strided" strategy can help with load balancing when iteration
#' times vary.
#'
#' @examples
#' \dontrun
#' # Shard 1000 columns with auto-tuned block size
#' s <- shards(1000)
#'
#' # Explicit block size
#' s <- shards(1000, block_size = 100)
#'
#' # Strided assignment
#' s <- shards(1000, strategy = "strided")
#' }
#'
#' @seealso \code{\link{shard_map}}
#' @export
shards <- function(n,
                   block_size = "auto",
                   strategy = c("contiguous", "strided")) {

  strategy <- match.arg(strategy)


  if (!is.numeric(n) || length(n) != 1 || n < 1) {
    stop("'n' must be a positive integer", call. = FALSE)
  }

  n <- as.integer(n)


  # Determine block size

  if (identical(block_size, "auto")) {
    # TODO: Implement auto-tuning based on worker count and memory
    block_size <- max(1L, n %/% parallel::detectCores())
  } else if (!is.numeric(block_size) || length(block_size) != 1 || block_size < 1) {
    stop("'block_size' must be 'auto' or a positive integer", call. = FALSE)
  }

  block_size <- as.integer(block_size)


  # Create shard descriptors

  if (strategy == "contiguous") {
    # Contiguous blocks
    starts <- seq(1L, n, by = block_size)
    ends <- pmin(starts + block_size - 1L, n)
    descriptors <- mapply(
      function(s, e) list(start = s, end = e, indices = s:e),
      starts, ends,
      SIMPLIFY = FALSE
    )
  } else {
    # Strided assignment
    n_shards <- ceiling(n / block_size)
    descriptors <- lapply(seq_len(n_shards), function(i) {
      indices <- seq(i, n, by = n_shards)
      list(start = min(indices), end = max(indices), indices = indices)
    })
  }


  structure(
    descriptors,
    class = "shard_descriptors",
    n = n,
    block_size = block_size,
    strategy = strategy
  )
}

#' @export
print.shard_descriptors <- function(x, ...) {
  cat("<shard_descriptors>\n")
  cat("  Total items:", attr(x, "n"), "\n")
  cat("  Block size:", attr(x, "block_size"), "\n")
  cat("  Strategy:", attr(x, "strategy"), "\n")
  cat("  Shards:", length(x), "\n")
  invisible(x)
}

#' @export
length.shard_descriptors <- function(x) {
  length(unclass(x))
}
