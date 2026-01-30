# Schema-driven columnar table outputs.
#
# v1.0 scope: fixed-row table buffers backed by shard buffers, with strict
# validation and disjoint row-range writes.

# Per-process table write diagnostics (aggregated into shard_map via dispatch
# deltas, similar to views/buffers).
.table_diag_env <- new.env(parent = emptyenv())
.table_diag_env$writes <- 0L
.table_diag_env$rows <- 0L
.table_diag_env$bytes <- 0

#' Table Diagnostics
#'
#' Per-process counters for table writes (number of table_write calls, rows,
#' and bytes written). shard_map uses deltas of these counters to produce
#' run-level diagnostics in copy_report().
#'
#' @return A list with `writes`, `rows`, and `bytes`.
#' @export
table_diagnostics <- function() {
  list(
    writes = .table_diag_env$writes,
    rows = .table_diag_env$rows,
    bytes = .table_diag_env$bytes
  )
}

table_reset_diagnostics <- function() {
  .table_diag_env$writes <- 0L
  .table_diag_env$rows <- 0L
  .table_diag_env$bytes <- 0
  invisible(NULL)
}

.coltype <- function(kind, ...) {
  structure(c(list(kind = kind), list(...)), class = "shard_coltype")
}

#' Column Types
#'
#' Type constructors for schema-driven table outputs.
#'
#' @return A `shard_coltype` object.
#' @name coltypes
NULL

#' @rdname coltypes
#' @export
int32 <- function() .coltype("int32")

#' @rdname coltypes
#' @export
float64 <- function() .coltype("float64")

#' @rdname coltypes
#' @export
bool <- function() .coltype("bool")

#' @rdname coltypes
#' @export
raw_col <- function() .coltype("raw")

#' Categorical column type
#'
#' Stores factors as int32 codes plus shared levels metadata.
#'
#' @param levels Character vector of allowed levels.
#' @return A `shard_coltype` object.
#' @export
factor_col <- function(levels) {
  if (!is.character(levels) || length(levels) < 1L || anyNA(levels)) {
    stop("levels must be a non-empty character vector without NA", call. = FALSE)
  }
  .coltype("factor", levels = unique(levels))
}

.coltype_bytes <- function(ct) {
  switch(ct$kind,
    "int32" = 4,
    "float64" = 8,
    "bool" = 1,   # logical payload size estimate for table bytes (not storage)
    "raw" = 1,
    "factor" = 4,
    NA_real_
  )
}

.coltype_buffer_type <- function(ct) {
  switch(ct$kind,
    "int32" = "integer",
    "float64" = "double",
    "bool" = "logical",
    "raw" = "raw",
    "factor" = "integer",
    stop("Unsupported column type: ", ct$kind, call. = FALSE)
  )
}

#' Define a table schema
#'
#' A schema is a named set of columns with explicit types. It is used to
#' allocate table buffers and validate writes.
#'
#' @param ... Named columns with type specs (e.g., `int32()`, `float64()`).
#' @return A `shard_schema`.
#' @export
schema <- function(...) {
  cols <- list(...)
  if (length(cols) == 0) stop("schema() requires at least one column", call. = FALSE)
  if (is.null(names(cols)) || any(names(cols) == "")) {
    stop("schema() requires named columns", call. = FALSE)
  }
  if (anyDuplicated(names(cols))) stop("schema() column names must be unique", call. = FALSE)

  bad <- vapply(cols, function(x) !inherits(x, "shard_coltype"), logical(1))
  if (any(bad)) {
    stop("schema() columns must be type specs (e.g. int32(), float64(), factor_col())", call. = FALSE)
  }

  structure(list(columns = cols), class = "shard_schema")
}

#' Allocate a fixed-row table buffer
#'
#' Allocates a columnar table output: one typed buffer per column, each of
#' length `nrow`. Intended for lock-free disjoint row-range writes in shard_map.
#'
#' @param schema A `shard_schema`.
#' @param nrow Total number of rows in the final table.
#' @param backing Backing type for buffers (`"auto"`, `"mmap"`, `"shm"`).
#' @return A `shard_table_buffer`.
#' @export
table_buffer <- function(schema, nrow, backing = c("auto", "mmap", "shm")) {
  backing <- match.arg(backing)
  if (!inherits(schema, "shard_schema")) stop("schema must be a shard_schema", call. = FALSE)

  nrow <- as.integer(nrow)
  if (is.na(nrow) || nrow < 1L) stop("nrow must be a positive integer", call. = FALSE)

  bufs <- list()
  for (nm in names(schema$columns)) {
    ct <- schema$columns[[nm]]
    buf_type <- .coltype_buffer_type(ct)
    bufs[[nm]] <- buffer(buf_type, dim = nrow, init = NULL, backing = backing)
  }

  structure(
    list(schema = schema, nrow = nrow, backing = backing, columns = bufs),
    class = "shard_table_buffer"
  )
}

#' Row layout for fixed-row table outputs
#'
#' Computes disjoint row ranges for each shard via prefix-sum, enabling lock-free
#' writes where each shard writes to a unique region.
#'
#' @param shards A `shard_descriptor`.
#' @param rows_per_shard Either a scalar integer or a function(shard)->integer.
#' @return A list mapping shard id (character) to `idx_range(start,end)`.
#' @export
row_layout <- function(shards, rows_per_shard) {
  if (!inherits(shards, "shard_descriptor")) stop("shards must be a shard_descriptor", call. = FALSE)
  if (missing(rows_per_shard)) stop("rows_per_shard is required", call. = FALSE)

  sizes <- integer(shards$num_shards)
  if (is.function(rows_per_shard)) {
    for (i in seq_len(shards$num_shards)) {
      n <- rows_per_shard(shards$shards[[i]])
      n <- as.integer(n)
      if (is.na(n) || n < 0L) stop("rows_per_shard returned invalid size for shard ", i, call. = FALSE)
      sizes[i] <- n
    }
  } else {
    n <- as.integer(rows_per_shard)
    if (is.na(n) || n < 0L) stop("rows_per_shard must be >= 0", call. = FALSE)
    sizes[] <- n
  }

  layout <- vector("list", shards$num_shards)
  cur <- 1L
  for (i in seq_len(shards$num_shards)) {
    n <- sizes[i]
    if (n == 0L) {
      layout[[i]] <- NULL
      next
    }
    layout[[i]] <- idx_range(cur, cur + n - 1L)
    cur <- cur + n
  }
  names(layout) <- as.character(seq_len(shards$num_shards))
  layout
}

.table_cast <- function(ct, x) {
  switch(ct$kind,
    "int32" = {
      xi <- as.integer(x)
      xi
    },
    "float64" = as.double(x),
    "bool" = as.logical(x),
    "raw" = as.raw(x),
    "factor" = {
      lev <- ct$levels
      if (is.factor(x)) {
        # Ensure levels are compatible
        if (!setequal(levels(x), lev)) {
          stop("factor levels mismatch in write", call. = FALSE)
        }
        # Map to schema levels ordering
        match(as.character(x), lev)
      } else if (is.character(x)) {
        match(x, lev)
      } else if (is.integer(x) || is.numeric(x)) {
        as.integer(x)
      } else {
        stop("Unsupported factor column input type", call. = FALSE)
      }
    },
    stop("Unsupported column type: ", ct$kind, call. = FALSE)
  )
}

.table_rows_len <- function(rows) {
  if (is.null(rows)) return(0L)
  if (inherits(rows, "shard_idx_range")) return(as.integer(rows$end - rows$start + 1L))
  rows <- as.integer(rows)
  length(rows)
}

.table_rows_resolve <- function(rows) {
  if (is.null(rows)) return(NULL)
  if (inherits(rows, "shard_idx_range")) return(rows$start:rows$end)
  as.integer(rows)
}

#' Write into a table buffer
#'
#' @param target A `shard_table_buffer`.
#' @param rows Row selector (idx_range or integer vector).
#' @param data A data.frame or named list matching the schema columns.
#' @return NULL (invisibly).
#' @export
table_write <- function(target, rows, data) {
  if (!inherits(target, "shard_table_buffer")) stop("target must be a shard_table_buffer", call. = FALSE)
  if (missing(rows)) stop("rows is required", call. = FALSE)

  row_idx <- .table_rows_resolve(rows)
  n <- .table_rows_len(rows)
  if (n < 1L) return(invisible(NULL))
  if (anyNA(row_idx) || any(row_idx < 1L) || any(row_idx > target$nrow)) {
    stop("rows contain out-of-range indices", call. = FALSE)
  }

  if (is.data.frame(data)) {
    data <- as.list(data)
  }
  if (!is.list(data) || is.null(names(data))) {
    stop("data must be a data.frame or named list", call. = FALSE)
  }

  sch <- target$schema$columns
  if (!setequal(names(data), names(sch))) {
    missing_cols <- setdiff(names(sch), names(data))
    extra_cols <- setdiff(names(data), names(sch))
    msg <- "table_write() data columns must match schema"
    if (length(missing_cols)) msg <- paste0(msg, "; missing: ", paste(missing_cols, collapse = ", "))
    if (length(extra_cols)) msg <- paste0(msg, "; extra: ", paste(extra_cols, collapse = ", "))
    stop(msg, call. = FALSE)
  }

  # Write each column slice.
  bytes <- 0
  for (nm in names(sch)) {
    ct <- sch[[nm]]
    col <- data[[nm]]
    if (length(col) != n) stop("Column '", nm, "' length mismatch (expected ", n, ")", call. = FALSE)
    cast <- .table_cast(ct, col)
    target$columns[[nm]][row_idx] <- cast

    b <- .coltype_bytes(ct)
    if (is.finite(b)) bytes <- bytes + as.double(n) * as.double(b)
  }

  .table_diag_env$writes <- .table_diag_env$writes + 1L
  .table_diag_env$rows <- .table_diag_env$rows + n
  .table_diag_env$bytes <- .table_diag_env$bytes + bytes

  invisible(NULL)
}

#' Finalize a table buffer
#'
#' @param target A `shard_table_buffer`.
#' @param materialize `"never"`, `"auto"`, or `"always"`.
#' @param max_bytes For `"auto"`, materialize only if estimated bytes <= max_bytes.
#' @return A `shard_table_handle` or a materialized data.frame/tibble.
#' @export
table_finalize <- function(target, materialize = c("never", "auto", "always"), max_bytes = 256 * 1024^2) {
  materialize <- match.arg(materialize)
  if (!inherits(target, "shard_table_buffer")) stop("target must be a shard_table_buffer", call. = FALSE)

  handle <- structure(
    list(schema = target$schema, nrow = target$nrow, columns = target$columns),
    class = "shard_table_handle"
  )

  est <- 0
  for (nm in names(target$schema$columns)) {
    b <- .coltype_bytes(target$schema$columns[[nm]])
    if (is.finite(b)) est <- est + as.double(target$nrow) * as.double(b)
  }

  do_mat <- switch(materialize,
    "never" = FALSE,
    "always" = TRUE,
    "auto" = isTRUE(est <= max_bytes)
  )

  if (!do_mat) return(handle)
  as_tibble(handle, max_bytes = max_bytes)
}

#' Materialize a table handle
#'
#' Converts a `shard_table_handle` to an in-memory data.frame (or tibble if the
#' tibble package is installed).
#'
#' @param x A `shard_table_handle` or `shard_table_buffer`.
#' @param max_bytes Warn if estimated payload exceeds this threshold.
#' @return A data.frame (or tibble).
#' @export
as_tibble <- function(x, max_bytes = 256 * 1024^2) {
  if (inherits(x, "shard_table_buffer")) {
    x <- structure(list(schema = x$schema, nrow = x$nrow, columns = x$columns),
                   class = "shard_table_handle")
  }
  if (!inherits(x, "shard_table_handle")) stop("x must be a table handle", call. = FALSE)

  sch <- x$schema$columns
  n <- x$nrow

  est <- 0
  for (nm in names(sch)) {
    b <- .coltype_bytes(sch[[nm]])
    if (is.finite(b)) est <- est + as.double(n) * as.double(b)
  }
  if (is.finite(est) && est > max_bytes) {
    warning("Materializing a large table (", format_bytes(est), "): consider writing to disk in v1.1 sinks.",
            call. = FALSE)
  }

  out <- list()
  for (nm in names(sch)) {
    ct <- sch[[nm]]
    buf <- x$columns[[nm]]
    vec <- buf[]  # full read
    if (ct$kind == "factor") {
      lev <- ct$levels
      codes <- as.integer(vec)
      chr <- rep(NA_character_, length(codes))
      ok <- !is.na(codes) & codes >= 1L & codes <= length(lev)
      chr[ok] <- lev[codes[ok]]
      vec <- factor(chr, levels = lev)
    } else if (ct$kind == "bool") {
      vec <- as.logical(vec)
    }
    out[[nm]] <- vec
  }
  df <- as.data.frame(out, stringsAsFactors = FALSE)

  if (pkg_available("tibble")) {
    return(tibble::as_tibble(df))
  }
  df
}
