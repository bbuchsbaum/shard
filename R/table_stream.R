# Streaming operations over on-disk table outputs (row-groups / datasets).
#
# Goal: bounded-memory post-processing without forcing collect()/as_tibble().

#' Stream over row-groups/datasets and reduce
#'
#' Applies `f()` to each partition (row-group) and combines results with
#' `combine()` into a single accumulator. This keeps peak memory bounded by the
#' largest single partition (plus your accumulator).
#'
#' @param x A `shard_row_groups` or `shard_dataset` handle.
#' @param f Function `(chunk, ...) -> value` producing a per-partition value.
#' @param init Initial accumulator value.
#' @param combine Function `(acc, value) -> acc` to update the accumulator.
#' @param ... Passed to `f()`.
#' @return The final accumulator.
#' @export
stream_reduce <- function(x, f, init, combine, ...) {
  UseMethod("stream_reduce")
}

#' @export
stream_reduce.shard_row_groups <- function(x, f, init, combine, ...) {
  if (!is.function(f)) stop("f must be a function", call. = FALSE)
  if (!is.function(combine)) stop("combine must be a function", call. = FALSE)

  it <- iterate_row_groups(x)
  acc <- init
  repeat {
    chunk <- it()
    if (is.null(chunk)) break
    val <- f(chunk, ...)
    acc <- combine(acc, val)
  }
  acc
}

#' @export
stream_reduce.shard_dataset <- function(x, f, init, combine, ...) {
  stream_reduce.shard_row_groups(structure(unclass(x), class = "shard_row_groups"),
                                 f = f, init = init, combine = combine, ...)
}

#' Stream over row-groups/datasets and map
#'
#' Applies `f()` to each partition and returns the list of per-partition results.
#' This is still much cheaper than collecting the full dataset when `f()` returns
#' a small summary per partition.
#'
#' @param x A `shard_row_groups` or `shard_dataset` handle.
#' @param f Function `(chunk, ...) -> value`.
#' @param ... Passed to `f()`.
#' @return A list of per-partition values (one per file).
#' @export
stream_map <- function(x, f, ...) {
  UseMethod("stream_map")
}

#' @export
stream_map.shard_row_groups <- function(x, f, ...) {
  if (!is.function(f)) stop("f must be a function", call. = FALSE)
  it <- iterate_row_groups(x)
  out <- list()
  i <- 0L
  repeat {
    chunk <- it()
    if (is.null(chunk)) break
    i <- i + 1L
    out[[i]] <- f(chunk, ...)
  }
  out
}

#' @export
stream_map.shard_dataset <- function(x, f, ...) {
  stream_map.shard_row_groups(structure(unclass(x), class = "shard_row_groups"), f = f, ...)
}

#' Stream row count
#'
#' @param x A `shard_row_groups` or `shard_dataset` handle.
#' @return Total number of rows.
#' @export
stream_count <- function(x) {
  stream_reduce(
    x,
    f = function(chunk) nrow(chunk),
    init = 0L,
    combine = function(acc, n) acc + as.integer(n)
  )
}

#' Stream-filter a dataset/row-groups into a new partitioned dataset
#'
#' Reads each partition, filters rows, and writes a new partitioned dataset.
#' Output is written as one partition per input partition (empty partitions are
#' allowed). This avoids materializing all results.
#'
#' @param x A `shard_row_groups` or `shard_dataset` handle.
#' @param predicate Function `(chunk, ...) -> logical` row mask (length == nrow(chunk)).
#' @param path Output directory. If NULL, a temp dir is created.
#' @param ... Passed to `predicate()`.
#' @return A `shard_dataset` handle.
#' @export
stream_filter <- function(x, predicate, path = NULL, ...) {
  if (!(inherits(x, "shard_row_groups") || inherits(x, "shard_dataset"))) {
    stop("x must be a shard_row_groups or shard_dataset handle", call. = FALSE)
  }
  if (!is.function(predicate)) stop("predicate must be a function", call. = FALSE)

  schema <- x$schema
  if (is.null(schema) || !inherits(schema, "shard_schema")) {
    stop("x must contain a schema to filter", call. = FALSE)
  }

  if (is.null(path)) {
    path <- file.path(tempdir(), paste0("shard_stream_filter_", unique_id()))
  }

  sink <- table_sink(schema, mode = "partitioned", path = path)

  it <- iterate_row_groups(x)
  shard_id <- 0L
  repeat {
    chunk <- it()
    if (is.null(chunk)) break
    shard_id <- shard_id + 1L
    keep <- predicate(chunk, ...)
    keep <- as.logical(keep)
    if (length(keep) != nrow(chunk)) {
      stop("predicate must return a logical vector with length == nrow(chunk)", call. = FALSE)
    }
    out <- chunk[keep, , drop = FALSE]
    table_write(sink, shard_id, out)
  }

  table_finalize(sink, materialize = "never")
}

