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

#' Stream sum of a numeric column
#'
#' Computes the sum of `col` across all partitions without collecting the full
#' dataset. When partitions are native-encoded, this avoids decoding string
#' columns entirely.
#'
#' @param x A `shard_row_groups` or `shard_dataset` handle.
#' @param col Column name to sum.
#' @param na_rm Logical; drop NAs (default TRUE).
#' @return A single numeric sum.
#' @export
stream_sum <- function(x, col, na_rm = TRUE) {
  if (!(inherits(x, "shard_row_groups") || inherits(x, "shard_dataset"))) {
    stop("x must be a shard_row_groups or shard_dataset handle", call. = FALSE)
  }
  col <- as.character(col)
  if (!nzchar(col)) stop("col must be a non-empty column name", call. = FALSE)
  sch <- x$schema$columns %||% list()
  ct <- sch[[col]] %||% NULL
  if (is.null(ct)) stop("Unknown column '", col, "'", call. = FALSE)
  if (ct$kind %in% c("string", "factor", "raw")) {
    stop("stream_sum() requires a numeric column", call. = FALSE)
  }

  it <- iterate_row_groups(x, decode = FALSE)
  acc <- 0
  repeat {
    chunk <- it()
    if (is.null(chunk)) break
    if (inherits(chunk, "shard_table_part_native")) {
      v <- chunk$columns[[col]]
      # Factor codes are integers; sum on codes is rarely meaningful and is
      # likely user error.
      if (inherits(v, "shard_string_blob")) {
        stop("stream_sum() cannot sum a string column", call. = FALSE)
      }
      if (isTRUE(na_rm)) v <- v[!is.na(v)]
      acc <- acc + sum(as.double(v))
    } else {
      v <- chunk[[col]]
      if (isTRUE(na_rm)) v <- v[!is.na(v)]
      acc <- acc + sum(as.double(v))
    }
  }
  acc
}

#' Stream top-k rows by a numeric column
#'
#' Finds the top `k` rows by `col` without collecting the full dataset.
#'
#' For native-encoded partitions, this selects candidate rows using the numeric
#' column without decoding strings, then decodes only the chosen rows for the
#' returned result.
#'
#' @param x A `shard_row_groups` or `shard_dataset` handle.
#' @param col Column name to rank by.
#' @param k Number of rows to keep.
#' @param decreasing Logical; TRUE for largest values (default TRUE).
#' @param na_drop Logical; drop rows where `col` is NA (default TRUE).
#' @return A data.frame (or tibble if installed) with at most `k` rows.
#' @export
stream_top_k <- function(x, col, k = 10L, decreasing = TRUE, na_drop = TRUE) {
  if (!(inherits(x, "shard_row_groups") || inherits(x, "shard_dataset"))) {
    stop("x must be a shard_row_groups or shard_dataset handle", call. = FALSE)
  }
  col <- as.character(col)
  k <- as.integer(k)
  if (is.na(k) || k < 1L) stop("k must be >= 1", call. = FALSE)

  schema <- x$schema
  sch_cols <- schema$columns %||% list()
  ct <- sch_cols[[col]] %||% NULL
  if (is.null(ct)) stop("Unknown column '", col, "'", call. = FALSE)
  if (!(ct$kind %in% c("int32", "float64"))) {
    stop("stream_top_k() requires an int32() or float64() column", call. = FALSE)
  }

  it <- iterate_row_groups(x, decode = FALSE)
  best <- NULL

  keep_best <- function(df) {
    if (is.null(best) || nrow(best) == 0) {
      best <<- df
    } else {
      best <<- rbind(best, df)
    }
    # Keep only top-k in memory.
    v <- best[[col]]
    if (isTRUE(na_drop)) {
      ok <- !is.na(v)
      best <<- best[ok, , drop = FALSE]
      v <- best[[col]]
    }
    ord <- order(v, decreasing = isTRUE(decreasing))
    if (length(ord) > k) ord <- ord[seq_len(k)]
    best <<- best[ord, , drop = FALSE]
  }

  repeat {
    chunk <- it()
    if (is.null(chunk)) break

    if (inherits(chunk, "shard_table_part_native")) {
      v <- chunk$columns[[col]]
      if (inherits(v, "shard_string_blob")) stop("stream_top_k() requires a numeric column", call. = FALSE)
      v <- as.double(v)
      if (isTRUE(na_drop)) {
        ok <- !is.na(v)
        v2 <- v[ok]
        if (length(v2) == 0) next
        idx_all <- which(ok)
      } else {
        v2 <- v
        idx_all <- seq_along(v)
      }

      ord <- order(v2, decreasing = isTRUE(decreasing))
      if (length(ord) > k) ord <- ord[seq_len(k)]
      rows <- idx_all[ord]
      df <- .table_part_native_decode_rows(chunk, schema, rows)
      keep_best(df)
    } else {
      v <- chunk[[col]]
      v <- as.double(v)
      if (isTRUE(na_drop)) {
        ok <- !is.na(v)
        if (!any(ok)) next
        chunk2 <- chunk[ok, , drop = FALSE]
      } else {
        chunk2 <- chunk
      }
      ord <- order(as.double(chunk2[[col]]), decreasing = isTRUE(decreasing))
      if (length(ord) > k) ord <- ord[seq_len(k)]
      keep_best(chunk2[ord, , drop = FALSE])
    }
  }

  if (is.null(best)) {
    df <- data.frame()
    if (pkg_available("tibble")) return(tibble::as_tibble(df))
    return(df)
  }

  if (pkg_available("tibble")) return(tibble::as_tibble(best))
  best
}

#' Stream group-wise sum
#'
#' Computes sum(value) by group across partitions without collecting. This is
#' optimized for factor groups (factor_col()).
#'
#' @param x A `shard_row_groups` or `shard_dataset` handle.
#' @param group Group column name (recommended: factor_col()).
#' @param value Numeric column name to sum.
#' @param na_rm Logical; drop rows where value is NA (default TRUE).
#' @return A data.frame with columns `group` and `sum`.
#' @export
stream_group_sum <- function(x, group, value, na_rm = TRUE) {
  if (!(inherits(x, "shard_row_groups") || inherits(x, "shard_dataset"))) {
    stop("x must be a shard_row_groups or shard_dataset handle", call. = FALSE)
  }
  group <- as.character(group)
  value <- as.character(value)
  schema <- x$schema
  ct_g <- schema$columns[[group]] %||% NULL
  ct_v <- schema$columns[[value]] %||% NULL
  if (is.null(ct_g)) stop("Unknown group column '", group, "'", call. = FALSE)
  if (is.null(ct_v)) stop("Unknown value column '", value, "'", call. = FALSE)

  if (ct_g$kind != "factor") {
    stop("stream_group_sum() currently requires a factor_col() group column", call. = FALSE)
  }
  if (!(ct_v$kind %in% c("int32", "float64"))) {
    stop("stream_group_sum() requires an int32() or float64() value column", call. = FALSE)
  }

  lev <- ct_g$levels %||% character(0)
  acc <- numeric(length(lev))

  it <- iterate_row_groups(x, decode = FALSE)
  repeat {
    chunk <- it()
    if (is.null(chunk)) break

    if (inherits(chunk, "shard_table_part_native")) {
      codes <- as.integer(chunk$columns[[group]])
      vals <- as.double(chunk$columns[[value]])
      ok <- !is.na(codes) & codes >= 1L & codes <= length(lev)
      if (isTRUE(na_rm)) ok <- ok & !is.na(vals)
      if (!any(ok)) next
      rs <- rowsum(vals[ok], group = factor(codes[ok], levels = seq_len(length(lev))), reorder = FALSE)
      acc <- acc + as.double(rs[, 1])
    } else {
      g <- chunk[[group]]
      v <- as.double(chunk[[value]])
      ok <- !is.na(g)
      if (isTRUE(na_rm)) ok <- ok & !is.na(v)
      if (!any(ok)) next
      rs <- rowsum(v[ok], group = factor(g[ok], levels = lev), reorder = FALSE)
      acc <- acc + as.double(rs[, 1])
    }
  }

  data.frame(group = factor(lev, levels = lev), sum = acc, stringsAsFactors = FALSE)
}

#' Stream group-wise count
#'
#' Counts rows per group across partitions without collecting. Optimized for
#' factor groups (factor_col()).
#'
#' @param x A `shard_row_groups` or `shard_dataset` handle.
#' @param group Group column name (recommended: factor_col()).
#' @return A data.frame with columns `group` and `n`.
#' @export
stream_group_count <- function(x, group) {
  if (!(inherits(x, "shard_row_groups") || inherits(x, "shard_dataset"))) {
    stop("x must be a shard_row_groups or shard_dataset handle", call. = FALSE)
  }
  group <- as.character(group)
  schema <- x$schema
  ct_g <- schema$columns[[group]] %||% NULL
  if (is.null(ct_g)) stop("Unknown group column '", group, "'", call. = FALSE)

  if (ct_g$kind != "factor") {
    stop("stream_group_count() currently requires a factor_col() group column", call. = FALSE)
  }

  lev <- ct_g$levels %||% character(0)
  acc <- integer(length(lev))

  it <- iterate_row_groups(x, decode = FALSE)
  repeat {
    chunk <- it()
    if (is.null(chunk)) break

    if (inherits(chunk, "shard_table_part_native")) {
      codes <- as.integer(chunk$columns[[group]])
      ok <- !is.na(codes) & codes >= 1L & codes <= length(lev)
      if (!any(ok)) next
      acc <- acc + tabulate(codes[ok], nbins = length(lev))
    } else {
      g <- chunk[[group]]
      idx <- match(as.character(g), lev)
      idx <- idx[!is.na(idx)]
      if (length(idx) == 0) next
      acc <- acc + tabulate(idx, nbins = length(lev))
    }
  }

  data.frame(group = factor(lev, levels = lev), n = acc, stringsAsFactors = FALSE)
}
