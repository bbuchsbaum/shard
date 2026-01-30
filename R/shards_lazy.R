# Lazy shard descriptor for contiguous shards.
#
# Used by shm_queue dispatch mode to avoid materializing a giant list of shard
# descriptors on the master.

shards_lazy <- function(n, block_size) {
  n <- as.integer(n)
  block_size <- as.integer(block_size)
  if (is.na(n) || n < 1L) stop("n must be >= 1", call. = FALSE)
  if (is.na(block_size) || block_size < 1L) stop("block_size must be >= 1", call. = FALSE)

  num_shards <- as.integer(ceiling(n / block_size))

  structure(
    list(
      n = n,
      block_size = block_size,
      strategy = "contiguous_lazy",
      num_shards = num_shards
    ),
    class = c("shard_descriptor_lazy", "shard_descriptor")
  )
}

.lazy_shard_at <- function(x, i) {
  i <- as.integer(i)
  if (is.na(i) || i < 1L || i > x$num_shards) stop("shard index out of range", call. = FALSE)
  start <- (i - 1L) * x$block_size + 1L
  end <- min(i * x$block_size, x$n)
  list(id = i, start = start, end = end, idx = start:end, len = end - start + 1L)
}

#' @export
print.shard_descriptor_lazy <- function(x, ...) {
  cat("shard descriptor (lazy)\n")
  cat("  Items:", format(x$n, big.mark = ","), "\n")
  cat("  Block size:", format(x$block_size, big.mark = ","), "\n")
  cat("  Strategy:", x$strategy, "\n")
  cat("  Shards:", x$num_shards, "\n")
  invisible(x)
}

#' @export
length.shard_descriptor_lazy <- function(x) {
  x$num_shards
}

#' @export
`[.shard_descriptor_lazy` <- function(x, i) {
  lapply(i, function(j) .lazy_shard_at(x, j))
}

#' @export
`[[.shard_descriptor_lazy` <- function(x, i) {
  .lazy_shard_at(x, i)
}

