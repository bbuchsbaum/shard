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

#' @rdname coltypes
#' @export
string_col <- function() .coltype("string")

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
    "string" = NA_real_,
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
    "string" = stop("string columns are not supported in fixed table_buffer() (use table_sink())", call. = FALSE),
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
    "string" = as.character(x),
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

#' Write tabular results into a table buffer or sink
#'
#' `table_write()` is the common write path for shard table outputs:
#' - For fixed-size outputs, write into a `shard_table_buffer` using a row selector.
#' - For variable-size outputs, write into a `shard_table_sink` using a shard id.
#'
#' @param target A `shard_table_buffer` or `shard_table_sink`.
#' @param rows_or_shard_id For buffers: row selector (idx_range or integer vector).
#'   For sinks: shard id (integer).
#' @param data A data.frame or named list matching the schema columns.
#' @param ... Reserved for future extensions.
#' @export
table_write <- function(target, rows_or_shard_id, data, ...) {
  UseMethod("table_write")
}

.table_write_buffer_impl <- function(target, rows, data) {
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

#' Write into a table buffer
#'
#' @param target A `shard_table_buffer`.
#' @param rows_or_shard_id Row selector (idx_range or integer vector).
#' @param data A data.frame or named list matching the schema columns.
#' @param ... Reserved for future extensions.
#' @return NULL (invisibly).
#' @export
table_write.shard_table_buffer <- function(target, rows_or_shard_id, data, ...) {
  .table_write_buffer_impl(target, rows = rows_or_shard_id, data = data)
}

#' Finalize a table buffer or sink
#'
#' For a `shard_table_buffer`, this returns a lightweight in-memory handle (or a
#' materialized data.frame/tibble, depending on `materialize`).
#'
#' For a `shard_table_sink`, this returns a row-group handle referencing the
#' written partitions (or materializes them if requested).
#'
#' @param target A `shard_table_buffer` or `shard_table_sink`.
#' @param materialize `"never"`, `"auto"`, or `"always"`.
#' @param max_bytes For `"auto"`, materialize only if estimated bytes <= max_bytes.
#' @param ... Reserved for future extensions.
#' @export
table_finalize <- function(target, materialize = c("never", "auto", "always"), max_bytes = 256 * 1024^2, ...) {
  UseMethod("table_finalize")
}

#' Finalize a table buffer
#'
#' @param target A `shard_table_buffer`.
#' @param materialize `"never"`, `"auto"`, or `"always"`.
#' @param max_bytes For `"auto"`, materialize only if estimated bytes <= max_bytes.
#' @param ... Reserved for future extensions.
#' @return A `shard_table_handle` or a materialized data.frame/tibble.
#' @export
table_finalize.shard_table_buffer <- function(target, materialize = c("never", "auto", "always"), max_bytes = 256 * 1024^2, ...) {
  materialize <- match.arg(materialize)

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

#' Materialize a shard table handle as a data.frame/tibble
#'
#' @param x A shard table object.
#' @param max_bytes Warn if estimated payload exceeds this threshold.
#' @param ... Reserved for future extensions.
#' @export
as_tibble <- function(x, max_bytes = 256 * 1024^2, ...) {
  UseMethod("as_tibble")
}

#' Materialize a fixed table handle or buffer
#'
#' Converts a `shard_table_handle` to an in-memory data.frame (or tibble if the
#' tibble package is installed).
#'
#' @param x A `shard_table_handle` or `shard_table_buffer`.
#' @param max_bytes Warn if estimated payload exceeds this threshold.
#' @param ... Reserved for future extensions.
#' @return A data.frame (or tibble).
#' @export
as_tibble.shard_table_buffer <- function(x, max_bytes = 256 * 1024^2, ...) {
  as_tibble(structure(list(schema = x$schema, nrow = x$nrow, columns = x$columns),
                      class = "shard_table_handle"),
            max_bytes = max_bytes)
}

#' @export
as_tibble.shard_table_handle <- function(x, max_bytes = 256 * 1024^2, ...) {
  sch <- x$schema$columns
  n <- x$nrow

  est <- 0
  for (nm in names(sch)) {
    b <- .coltype_bytes(sch[[nm]])
    if (is.finite(b)) est <- est + as.double(n) * as.double(b)
  }
  if (is.finite(est) && est > max_bytes) {
    warning("Materializing a large table (", format_bytes(est), "): consider writing to disk in table_sink().",
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

#' Create a table sink for row-group or partitioned outputs
#'
#' A table sink supports variable-sized outputs without returning large
#' data.frames to the master. Each shard writes a separate row-group file.
#'
#' v1.1 implementation notes:
#' - Storage format is per-shard RDS (portable, CRAN-friendly).
#' - This guarantees bounded master memory during execution; final collection
#'   may still be large if you materialize.
#'
#' @param schema A `shard_schema`.
#' @param mode `"row_groups"` (temp, managed) or `"partitioned"` (persistent path).
#' @param path Directory to write row-group files. If NULL, a temp dir is created.
#' @return A `shard_table_sink`.
#' @export
table_sink <- function(schema,
                       mode = c("row_groups", "partitioned"),
                       path = NULL) {
  mode <- match.arg(mode)
  if (!inherits(schema, "shard_schema")) stop("schema must be a shard_schema", call. = FALSE)

  if (is.null(path)) {
    path <- file.path(tempdir(), paste0("shard_table_sink_", unique_id()))
  }
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

  structure(
    list(schema = schema, mode = mode, path = path, format = "rds"),
    class = "shard_table_sink"
  )
}

.sink_part_path <- function(sink, shard_id) {
  file.path(sink$path, sprintf("part-%06d.rds", as.integer(shard_id)))
}

.sink_manifest_rds_path <- function(sink) {
  file.path(sink$path, "manifest.rds")
}

.sink_manifest_json_path <- function(sink) {
  file.path(sink$path, "manifest.json")
}

.sink_atomic_save_rds <- function(obj, path) {
  tmp <- paste0(path, ".tmp_", Sys.getpid(), "_", sample.int(1e9, 1))
  saveRDS(obj, tmp)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    unlink(tmp)
    stop("Failed to finalize write", call. = FALSE)
  }
  invisible(NULL)
}

.schema_to_plain_list <- function(schema) {
  if (!inherits(schema, "shard_schema")) {
    stop("schema must be a shard_schema", call. = FALSE)
  }
  cols <- lapply(schema$columns, function(ct) {
    # shard_coltype objects are simple lists; strip class for JSON stability.
    out <- as.list(ct)
    class(out) <- NULL
    out
  })
  list(
    version = schema$version %||% 1L,
    schema_hash = schema$schema_hash %||% NULL,
    columns = cols
  )
}

.sink_write_manifest <- function(sink, handle) {
  man_rds <- list(
    version = 1L,
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    mode = sink$mode,
    format = sink$format,
    schema = sink$schema,
    files = basename(handle$files %||% character(0))
  )

  .sink_atomic_save_rds(man_rds, .sink_manifest_rds_path(sink))

  # Optional JSON manifest (Suggests: jsonlite). Keep it behind requireNamespace
  # so shard core stays CRAN-friendly.
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    man_json <- man_rds
    man_json$schema <- .schema_to_plain_list(sink$schema)
    tmp <- paste0(.sink_manifest_json_path(sink), ".tmp_", Sys.getpid(), "_", sample.int(1e9, 1))
    jsonlite::write_json(man_json, tmp, auto_unbox = TRUE, pretty = TRUE)
    ok <- file.rename(tmp, .sink_manifest_json_path(sink))
    if (!isTRUE(ok)) unlink(tmp)
  }

  invisible(NULL)
}

.sink_atomic_write_rds <- function(obj, path) {
  tmp <- paste0(path, ".tmp_", Sys.getpid(), "_", sample.int(1e9, 1))
  saveRDS(obj, tmp)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    unlink(tmp)
    stop("Failed to finalize row-group write", call. = FALSE)
  }
  invisible(NULL)
}

#' Write a shard's row-group output
#'
#' @param target A `shard_table_sink`.
#' @param rows_or_shard_id Integer shard id used to name the row-group file.
#' @param data A data.frame matching the sink schema.
#' @param ... Reserved for future extensions.
#' @return NULL (invisibly).
#' @export
table_write.shard_table_sink <- function(target, rows_or_shard_id, data, ...) {
  sink <- target

  shard_id <- rows_or_shard_id
  if (missing(shard_id)) stop("shard_id is required for table_sink writes", call. = FALSE)
  if (!dir.exists(sink$path)) dir.create(sink$path, recursive = TRUE, showWarnings = FALSE)
  if (is.data.frame(data)) {
    data <- as.list(data)
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(data)) stop("data must be a data.frame", call. = FALSE)

  sch <- sink$schema$columns
  if (!setequal(names(data), names(sch))) {
    missing_cols <- setdiff(names(sch), names(data))
    extra_cols <- setdiff(names(data), names(sch))
    msg <- "table_write(sink, ...) data columns must match schema"
    if (length(missing_cols)) msg <- paste0(msg, "; missing: ", paste(missing_cols, collapse = ", "))
    if (length(extra_cols)) msg <- paste0(msg, "; extra: ", paste(extra_cols, collapse = ", "))
    stop(msg, call. = FALSE)
  }

  # Cast + validate each column.
  out <- data
  for (nm in names(sch)) {
    ct <- sch[[nm]]
    col <- out[[nm]]
    if (ct$kind == "factor") {
      lev <- ct$levels
      if (is.factor(col)) {
        chr <- as.character(col)
      } else if (is.character(col)) {
        chr <- col
      } else if (is.integer(col) || is.numeric(col)) {
        codes <- as.integer(col)
        chr <- rep(NA_character_, length(codes))
        ok <- !is.na(codes) & codes >= 1L & codes <= length(lev)
        chr[ok] <- lev[codes[ok]]
        if (any(!ok & !is.na(codes))) {
          stop("Invalid factor code in column '", nm, "'", call. = FALSE)
        }
      } else {
        stop("Unsupported factor column input type", call. = FALSE)
      }
      bad <- !is.na(chr) & !(chr %in% lev)
      if (any(bad)) stop("factor levels mismatch in write", call. = FALSE)
      out[[nm]] <- factor(chr, levels = lev)
    } else {
      out[[nm]] <- .table_cast(ct, col)
    }
  }

  p <- .sink_part_path(sink, shard_id)
  .sink_atomic_write_rds(out, p)

  n <- nrow(out)
  sz <- tryCatch(file.info(p)$size, error = function(e) NA_real_)

  .table_diag_env$writes <- .table_diag_env$writes + 1L
  .table_diag_env$rows <- .table_diag_env$rows + as.integer(n)
  if (is.finite(sz)) .table_diag_env$bytes <- .table_diag_env$bytes + as.double(sz)

  invisible(NULL)
}

#' Finalize a sink
#'
#' @param target A `shard_table_sink`.
#' @param materialize `"never"`, `"auto"`, or `"always"`.
#' @param max_bytes For `"auto"`, materialize only if estimated bytes <= max_bytes.
#' @param ... Reserved for future extensions.
#' @return A `shard_row_groups` handle (or a materialized data.frame/tibble).
#' @export
table_finalize.shard_table_sink <- function(target, materialize = c("never", "auto", "always"), max_bytes = 256 * 1024^2, ...) {
  materialize <- match.arg(materialize)

  files <- list.files(target$path, pattern = "^part-[0-9]+\\.rds$", full.names = TRUE)
  files <- sort(files)

  cls <- if (identical(target$mode, "partitioned")) "shard_dataset" else "shard_row_groups"
  handle <- structure(
    list(schema = target$schema, path = target$path, files = files, format = target$format, mode = target$mode),
    class = cls
  )

  # Always emit a manifest so users can inspect/ship outputs without reading
  # partitions, and so future backends can build on this directory layout.
  .sink_write_manifest(target, handle)

  # Determine whether to materialize by file sizes (if available).
  total <- sum(vapply(files, function(f) as.double(file.info(f)$size %||% 0), numeric(1)), na.rm = TRUE)
  do_mat <- switch(materialize,
    "never" = FALSE,
    "always" = TRUE,
    "auto" = isTRUE(total <= max_bytes)
  )

  if (!do_mat) return(handle)
  as_tibble(handle, max_bytes = max_bytes)
}

#' Iterate row groups
#'
#' @param x A `shard_row_groups` handle.
#' @return An iterator function with no args that returns the next data.frame or NULL.
#' @export
iterate_row_groups <- function(x) {
  if (!(inherits(x, "shard_row_groups") || inherits(x, "shard_dataset"))) {
    stop("x must be a shard_row_groups or shard_dataset handle", call. = FALSE)
  }
  files <- x$files
  i <- 0L
  function() {
    i <<- i + 1L
    if (i > length(files)) return(NULL)
    readRDS(files[[i]])
  }
}

# Extend as_tibble() to accept row-group handles.
#' @export
as_tibble.shard_row_groups <- function(x, max_bytes = 256 * 1024^2, ...) {
  # max_bytes is accepted for API consistency; row-groups are size-variable and
  # are typically materialized explicitly by users.
  it <- iterate_row_groups(x)
  chunks <- list()
  repeat {
    d <- it()
    if (is.null(d)) break
    chunks[[length(chunks) + 1L]] <- d
  }
  if (length(chunks) == 0) {
    df <- data.frame()
    if (pkg_available("tibble")) return(tibble::as_tibble(df))
    return(df)
  }
  df <- do.call(rbind, chunks)
  if (pkg_available("tibble")) return(tibble::as_tibble(df))
  df
}

#' @export
as_tibble.shard_dataset <- function(x, max_bytes = 256 * 1024^2, ...) {
  # A shard_dataset is currently a directory of per-shard RDS partitions plus a
  # manifest. Materialization is identical to row-groups for now.
  as_tibble(structure(unclass(x), class = "shard_row_groups"), max_bytes = max_bytes)
}

#' Collect a shard table into memory
#'
#' `collect()` is a convenience alias for `as_tibble()` for shard table outputs.
#'
#' @param x A shard table handle.
#' @param ... Passed to `as_tibble()`.
#' @export
collect <- function(x, ...) {
  UseMethod("collect")
}

#' @export
collect.shard_row_groups <- function(x, ...) {
  as_tibble(x, ...)
}

#' @export
collect.shard_dataset <- function(x, ...) {
  as_tibble(x, ...)
}

#' @export
collect.shard_table_handle <- function(x, ...) {
  as_tibble(x, ...)
}
