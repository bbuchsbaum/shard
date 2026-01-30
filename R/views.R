#' Zero-copy Views
#'
#' Views are explicit slice descriptors over shared arrays/matrices. They avoid
#' creating slice-sized allocations (e.g. `Y[, a:b]`) by carrying only metadata
#' plus a reference to the shared backing.
#'
#' This is a low-level optimization handle: arbitrary base R operations may
#' materialize a view; use `materialize()` explicitly when you want a standard
#' matrix/array.
#'
#' @name views
NULL

.views_env <- new.env(parent = emptyenv())
.views_env$created <- 0L
.views_env$materialized <- 0L
.views_env$materialized_bytes <- 0

elem_size_bytes <- function(x) {
  switch(
    typeof(x),
    "integer" = 4L,
    "double" = 8L,
    "logical" = 4L,
    "raw" = 1L,
    NA_integer_
  )
}

is_idx_range <- function(x) inherits(x, "shard_idx_range")

idx_range_validate <- function(start, end) {
  if (!is.numeric(start) || length(start) != 1L || is.na(start)) {
    stop("start must be a single, non-NA number", call. = FALSE)
  }
  if (!is.numeric(end) || length(end) != 1L || is.na(end)) {
    stop("end must be a single, non-NA number", call. = FALSE)
  }
  start <- as.integer(start)
  end <- as.integer(end)
  if (start < 1L) stop("start must be >= 1", call. = FALSE)
  if (end < start) stop("end must be >= start", call. = FALSE)
  list(start = start, end = end)
}

#' Contiguous index range
#'
#' Creates a compact, serializable range descriptor for contiguous indices.
#' This avoids allocating an explicit index vector for large slices.
#'
#' @param start Integer. Start index (1-based, inclusive).
#' @param end Integer. End index (1-based, inclusive).
#' @return An object of class `shard_idx_range`.
#' @export
idx_range <- function(start, end) {
  r <- idx_range_validate(start, end)
  structure(r, class = "shard_idx_range")
}

idx_range_resolve <- function(r, n, name = "range") {
  if (is.null(r)) return(NULL)
  if (!is_idx_range(r)) stop(name, " must be NULL or idx_range()", call. = FALSE)
  if (r$end > n) stop(name, " end exceeds dimension (", n, ")", call. = FALSE)
  r$start:r$end
}

idx_range_len <- function(r, n) {
  if (is.null(r)) return(as.integer(n))
  if (!is_idx_range(r)) return(NA_integer_)
  as.integer(r$end - r$start + 1L)
}

idx_range_print <- function(r) {
  if (is.null(r)) return("(all)")
  paste0("[", r$start, ":", r$end, "]")
}

view_layout <- function(x_dim, rows, cols) {
  if (length(x_dim) != 2L) return("unsupported")
  if (is.null(rows) && is.null(cols)) return("full")

  # Column-major matrices: contiguous column blocks are contiguous in memory.
  if (is.null(rows) && is_idx_range(cols)) return("col_block_contiguous")

  # Row blocks are strided (typically require packing for BLAS).
  if (is_idx_range(rows) && is.null(cols)) return("row_block_strided")

  if (is_idx_range(rows) && is_idx_range(cols)) return("submatrix_strided")

  "unsupported"
}

is_shared_matrix_like <- function(x) {
  d <- dim(x)
  if (is.null(d) || length(d) != 2L) return(FALSE)
  is_shared_vector(x) && typeof(x) %in% c("double", "integer", "logical", "raw")
}

view_validate_dims <- function(x) {
  if (!is_shared_matrix_like(x)) {
    stop(
      "x must be a shared (share()d) atomic matrix with supported type.\n",
      "  Supported types: double/integer/logical/raw.\n",
      "  Hint: use share(matrix(...)) to create a shared input first.",
      call. = FALSE
    )
  }
  d <- dim(x)
  if (length(d) != 2L || any(is.na(d)) || any(d < 0)) {
    stop("Invalid matrix dimensions on shared object", call. = FALSE)
  }
  invisible(d)
}

view_block_validate_selector <- function(sel, which = c("rows", "cols")) {
  which <- match.arg(which)
  if (is.null(sel)) return(invisible(NULL))
  if (is_idx_range(sel)) return(invisible(NULL))

  stop(
    which, " must be NULL or idx_range() for block views.\n",
    "  Gather/indexed views are not implemented yet.",
    call. = FALSE
  )
}

view_block_build <- function(x, rows, cols) {
  d <- dim(x)
  nrow <- d[1]
  ncol <- d[2]

  view_block_validate_selector(rows, "rows")
  view_block_validate_selector(cols, "cols")

  if (!is.null(rows) && rows$end > nrow) stop("rows end exceeds nrow", call. = FALSE)
  if (!is.null(cols) && cols$end > ncol) stop("cols end exceeds ncol", call. = FALSE)

  layout <- view_layout(d, rows, cols)
  if (layout == "unsupported") {
    stop("Unsupported view layout for block view", call. = FALSE)
  }

  # Slice dims
  rlen <- idx_range_len(rows, nrow)
  clen <- idx_range_len(cols, ncol)
  slice_dim <- c(rlen, clen)

  # Estimate payload bytes (what materialize() would copy).
  es <- elem_size_bytes(x)
  nbytes <- if (!is.na(es)) as.double(prod(slice_dim)) * as.double(es) else NA_real_

  .views_env$created <- .views_env$created + 1L

  structure(
    list(
      base = x,
      dim = d,
      rows = rows,
      cols = cols,
      dtype = typeof(x),
      layout = layout,
      slice_dim = slice_dim,
      nbytes_est = nbytes
    ),
    class = c("shard_view_block", "shard_view")
  )
}

#' Create a view over a shared matrix
#'
#' @param x A shared (share()d) atomic matrix (double/integer/logical/raw).
#' @param rows Row selector. NULL (all rows) or idx_range().
#' @param cols Column selector. NULL (all cols) or idx_range().
#' @param type View type. Currently only `"block"` is implemented.
#' @return A view object of class `shard_view_block`.
#' @export
view <- function(x, rows = NULL, cols = NULL, type = c("auto", "block", "gather")) {
  type <- match.arg(type)
  view_validate_dims(x)

  if (type == "gather") {
    stop("Gather views are not implemented yet", call. = FALSE)
  }
  if (type == "auto") {
    # Auto-select block if selectors are ranges, else gather (future).
    if ((!is.null(rows) && !is_idx_range(rows)) || (!is.null(cols) && !is_idx_range(cols))) {
      stop("Gather views are not implemented yet; use idx_range() selectors.", call. = FALSE)
    }
    type <- "block"
  }

  view_block_build(x, rows = rows, cols = cols)
}

#' Create a contiguous block view
#'
#' @param x A shared (share()d) atomic matrix.
#' @param rows NULL or idx_range().
#' @param cols NULL or idx_range().
#' @return A `shard_view_block` object.
#' @export
view_block <- function(x, rows = NULL, cols = NULL) {
  view_validate_dims(x)
  view_block_build(x, rows = rows, cols = cols)
}

#' Introspection for a view
#'
#' Returns metadata about a view without forcing materialization.
#'
#' @param v A shard view.
#' @return A named list of metadata.
#' @export
view_info <- function(v) {
  if (!inherits(v, "shard_view")) stop("v must be a shard view", call. = FALSE)
  base <- v$base
  d <- v$dim

  fast_path <- isTRUE(v$layout == "col_block_contiguous") && identical(v$dtype, "double")

  list(
    dtype = v$dtype,
    dim = d,
    slice_dim = v$slice_dim,
    rows = v$rows,
    cols = v$cols,
    rows_print = idx_range_print(v$rows),
    cols_print = idx_range_print(v$cols),
    layout = v$layout,
    fast_path = fast_path,
    nbytes_est = v$nbytes_est,
    base_is_shared = is_shared_vector(base)
  )
}

#' @export
is_view <- function(x) inherits(x, "shard_view")

#' @export
is_block_view <- function(x) inherits(x, "shard_view_block")

#' @export
print.shard_idx_range <- function(x, ...) {
  cat("idx_range", idx_range_print(x), "\n")
  invisible(x)
}

#' @export
print.shard_view_block <- function(x, ...) {
  info <- view_info(x)
  cat("shard_view_block\n")
  cat("  dtype:", info$dtype, "\n")
  cat("  dim:", paste(info$dim, collapse = "x"), "\n")
  cat("  slice:", paste(info$slice_dim, collapse = "x"),
      " rows=", info$rows_print, " cols=", info$cols_print, "\n", sep = "")
  cat("  layout:", info$layout, " fast_path:", info$fast_path, "\n")
  invisible(x)
}

#' @export
materialize.shard_view_block <- function(x) {
  # Materialization is explicit and counted.
  base <- x$base
  d <- x$dim
  rows <- idx_range_resolve(x$rows, d[1], name = "rows")
  cols <- idx_range_resolve(x$cols, d[2], name = "cols")

  .views_env$materialized <- .views_env$materialized + 1L
  if (!is.null(x$nbytes_est) && is.finite(x$nbytes_est)) {
    .views_env$materialized_bytes <- .views_env$materialized_bytes + x$nbytes_est
  }

  if (is.null(rows)) rows <- seq_len(d[1])
  if (is.null(cols)) cols <- seq_len(d[2])

  base[rows, cols, drop = FALSE]
}

#' View diagnostics
#'
#' Returns global counters for view creation/materialization. This is a simple
#' first step; in future this should be integrated into shard_map run-level
#' diagnostics.
#'
#' @return A list with counters.
#' @export
view_diagnostics <- function() {
  list(
    created = .views_env$created %||% 0L,
    materialized = .views_env$materialized %||% 0L,
    materialized_bytes = .views_env$materialized_bytes %||% 0
  )
}

view_reset_diagnostics <- function() {
  .views_env$created <- 0L
  .views_env$materialized <- 0L
  .views_env$materialized_bytes <- 0
  invisible(NULL)
}

view_col_sums <- function(v) {
  if (!inherits(v, "shard_view_block")) {
    stop("v must be a shard_view_block", call. = FALSE)
  }
  base <- v$base
  d <- v$dim
  if (length(d) != 2L) stop("View base must be a matrix", call. = FALSE)

  rs <- if (is.null(v$rows)) c(1L, d[1]) else c(v$rows$start, v$rows$end)
  cs <- if (is.null(v$cols)) c(1L, d[2]) else c(v$cols$start, v$cols$end)

  out <- .Call(
    "C_shard_mat_block_col_sums",
    base,
    as.integer(rs[1]),
    as.integer(rs[2]),
    as.integer(cs[1]),
    as.integer(cs[2]),
    PACKAGE = "shard"
  )

  dns <- dimnames(base)
  if (!is.null(dns) && length(dns) == 2L && !is.null(dns[[2]])) {
    nms <- dns[[2]][cs[1]:cs[2]]
    if (!is.null(nms)) names(out) <- nms
  }

  out
}
