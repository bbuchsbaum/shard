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
.views_env$packed <- 0L
.views_env$packed_bytes <- 0

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

idx_vec_validate <- function(idx, n, name = "idx") {
  if (!is.numeric(idx) || is.null(idx)) {
    stop(name, " must be a non-empty numeric/integer vector", call. = FALSE)
  }
  idx <- as.integer(idx)
  if (anyNA(idx)) stop(name, " must not contain NA", call. = FALSE)
  if (any(idx < 1L) || any(idx > n)) {
    stop(name, " contains out-of-range indices (1..", n, ")", call. = FALSE)
  }
  idx
}

idx_print <- function(x) {
  if (is.null(x)) return("(all)")
  if (is_idx_range(x)) return(idx_range_print(x))
  if (is.integer(x)) return(paste0("[gather n=", length(x), "]"))
  "(unknown)"
}

view_layout <- function(x_dim, rows, cols) {
  if (length(x_dim) != 2L) return("unsupported")
  if (is.null(rows) && is.null(cols)) return("full")

  # Column-major matrices: contiguous column blocks are contiguous in memory.
  if (is.null(rows) && is_idx_range(cols)) return("col_block_contiguous")

  # Row blocks are strided (typically require packing for BLAS).
  if (is_idx_range(rows) && is.null(cols)) return("row_block_strided")

  if (is_idx_range(rows) && is_idx_range(cols)) return("submatrix_strided")

  if ((is.null(rows) || is_idx_range(rows)) && is.integer(cols)) return("col_gather")

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

#' Create a gather (indexed) view over a shared matrix
#'
#' Gather views describe non-contiguous column (or row) subsets without
#' allocating a slice-sized matrix. shard-aware kernels can then choose to
#' pack the requested indices into scratch explicitly (bounded and reportable)
#' or run gather-aware compute paths.
#'
#' v1 note: only column-gather views are implemented (rows may be NULL or
#' idx_range()).
#'
#' @param x A shared (share()d) atomic matrix (double/integer/logical/raw).
#' @param rows Row selector. NULL (all rows) or idx_range().
#' @param cols Integer vector of column indices (1-based).
#' @return A view object of class `shard_view_gather`.
#' @export
view_gather <- function(x, rows = NULL, cols) {
  view_validate_dims(x)
  d <- dim(x)
  nrow <- d[1]
  ncol <- d[2]

  if (!is.null(rows) && !is_idx_range(rows)) {
    stop("rows must be NULL or idx_range() for gather views", call. = FALSE)
  }
  if (!is.null(rows) && rows$end > nrow) stop("rows end exceeds nrow", call. = FALSE)

  cols <- idx_vec_validate(cols, ncol, name = "cols")

  slice_dim <- c(idx_range_len(rows, nrow), length(cols))
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
      layout = "col_gather",
      slice_dim = slice_dim,
      nbytes_est = nbytes
    ),
    class = c("shard_view_gather", "shard_view")
  )
}

#' Create a view over a shared matrix
#'
#' @param x A shared (share()d) atomic matrix (double/integer/logical/raw).
#' @param rows Row selector. NULL (all rows) or idx_range().
#' @param cols Column selector. NULL (all cols) or idx_range().
#' @param type View type. `"block"` or `"gather"` (or `"auto"`).
#' @return A view object.
#' @export
view <- function(x, rows = NULL, cols = NULL, type = c("auto", "block", "gather")) {
  type <- match.arg(type)
  view_validate_dims(x)

  if (type == "gather") {
    if (is.null(cols)) stop("gather view requires cols", call. = FALSE)
    return(view_gather(x, rows = rows, cols = cols))
  }
  if (type == "auto") {
    # Auto-select block for idx_range selectors, gather for integer selectors.
    if (!is.null(cols) && is.numeric(cols) && !is_idx_range(cols)) {
      return(view_gather(x, rows = rows, cols = cols))
    }
    if ((!is.null(rows) && !is_idx_range(rows)) || (!is.null(cols) && !is_idx_range(cols))) {
      stop("Unsupported selectors for auto view; use idx_range() or integer indices.", call. = FALSE)
    }
    return(view_block_build(x, rows = rows, cols = cols))
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
    rows_print = idx_print(v$rows),
    cols_print = idx_print(v$cols),
    layout = v$layout,
    fast_path = fast_path,
    nbytes_est = v$nbytes_est,
    base_is_shared = is_shared_vector(base)
  )
}

#' View Predicates
#'
#' @param x An object.
#' @return Logical. TRUE if `x` is a shard view (or block view).
#' @export
is_view <- function(x) inherits(x, "shard_view")

#' @rdname is_view
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
print.shard_view_gather <- function(x, ...) {
  info <- view_info(x)
  cat("shard_view_gather\n")
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

#' @export
materialize.shard_view_gather <- function(x) {
  base <- x$base
  d <- x$dim
  rows <- idx_range_resolve(x$rows, d[1], name = "rows")
  cols <- x$cols

  .views_env$materialized <- .views_env$materialized + 1L
  if (!is.null(x$nbytes_est) && is.finite(x$nbytes_est)) {
    .views_env$materialized_bytes <- .views_env$materialized_bytes + x$nbytes_est
  }

  if (is.null(rows)) rows <- seq_len(d[1])
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
    created = .views_env$created,
    materialized = .views_env$materialized,
    materialized_bytes = .views_env$materialized_bytes,
    packed = .views_env$packed,
    packed_bytes = .views_env$packed_bytes
  )
}

view_reset_diagnostics <- function() {
  .views_env$created <- 0L
  .views_env$materialized <- 0L
  .views_env$materialized_bytes <- 0
  .views_env$packed <- 0L
  .views_env$packed_bytes <- 0
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

view_xTy <- function(x, y_view, x_cols = NULL) {
  if (!inherits(y_view, "shard_view")) {
    stop("y_view must be a shard view", call. = FALSE)
  }
  if (!is.matrix(x) && !(is_shared_vector(x) && !is.null(dim(x)) && length(dim(x)) == 2L)) {
    stop("x must be a matrix or a shared matrix backing", call. = FALSE)
  }

  y_base <- y_view$base
  dY <- y_view$dim
  dX <- dim(x)
  if (length(dY) != 2L || length(dX) != 2L) stop("x and y must be matrices", call. = FALSE)
  if (dX[1] != dY[1]) stop("x and y must have the same number of rows", call. = FALSE)

  rs <- if (is.null(y_view$rows)) c(1L, dY[1]) else c(y_view$rows$start, y_view$rows$end)

  if (is.null(x_cols)) {
    xcs <- 1L
    xce <- dX[2]
  } else {
    if (!is_idx_range(x_cols)) stop("x_cols must be NULL or idx_range()", call. = FALSE)
    xcs <- x_cols$start
    xce <- x_cols$end
    if (xce > dX[2]) stop("x_cols end exceeds ncol(x)", call. = FALSE)
  }

  if (!identical(y_view$dtype, "double") || !identical(typeof(x), "double")) {
    # Keep this strict while the kernels are double-only.
    stop("view_xTy currently supports double matrices only", call. = FALSE)
  }

  if (inherits(y_view, "shard_view_block")) {
    if (is.null(y_view$cols)) stop("y_view must specify cols for block views", call. = FALSE)
    ycs <- y_view$cols$start
    yce <- y_view$cols$end

    out <- .Call(
      "C_shard_mat_crossprod_block",
      x,
      y_base,
      as.integer(rs[1]),
      as.integer(rs[2]),
      as.integer(xcs),
      as.integer(xce),
      as.integer(ycs),
      as.integer(yce),
      PACKAGE = "shard"
    )

    dnX <- dimnames(x)
    dnY <- dimnames(y_base)
    if (!is.null(dnX) || !is.null(dnY)) {
      rn <- if (!is.null(dnX) && length(dnX) == 2L) dnX[[2]] else NULL
      cn <- if (!is.null(dnY) && length(dnY) == 2L) dnY[[2]] else NULL
      if (!is.null(rn)) rn <- rn[xcs:xce]
      if (!is.null(cn)) cn <- cn[ycs:yce]
      dimnames(out) <- list(rn, cn)
    }

    return(out)
  }

  if (!inherits(y_view, "shard_view_gather")) {
    stop("Unsupported view type for view_xTy", call. = FALSE)
  }
  if (is.null(y_view$cols) || !is.integer(y_view$cols)) {
    stop("gather view must provide integer cols", call. = FALSE)
  }

  y_cols <- y_view$cols
  out <- .Call(
    "C_shard_mat_crossprod_gather",
    x,
    y_base,
    as.integer(rs[1]),
    as.integer(rs[2]),
    as.integer(xcs),
    as.integer(xce),
    as.integer(y_cols),
    PACKAGE = "shard"
  )

  # Record controlled packing volume (scratch), distinct from view materialization.
  nr <- as.integer(rs[2] - rs[1] + 1L)
  .views_env$packed <- .views_env$packed + 1L
  .views_env$packed_bytes <- .views_env$packed_bytes + (as.double(nr) * as.double(length(y_cols)) * 8)

  dnX <- dimnames(x)
  dnY <- dimnames(y_base)
  rn <- if (!is.null(dnX) && length(dnX) == 2L) dnX[[2]] else NULL
  cn <- if (!is.null(dnY) && length(dnY) == 2L) dnY[[2]] else NULL
  if (!is.null(rn)) rn <- rn[xcs:xce]
  if (!is.null(cn)) cn <- cn[y_cols]
  if (!is.null(rn) || !is.null(cn)) dimnames(out) <- list(rn, cn)

  out
}

view_crossprod <- function(x_view, y_view) {
  if (!inherits(x_view, "shard_view_block")) {
    stop("x_view must be a shard_view_block", call. = FALSE)
  }
  if (!inherits(y_view, "shard_view_block")) {
    stop("y_view must be a shard_view_block", call. = FALSE)
  }

  xb <- x_view$base
  yb <- y_view$base
  dX <- x_view$dim
  dY <- y_view$dim
  if (length(dX) != 2L || length(dY) != 2L) stop("views must be over matrices", call. = FALSE)
  if (dX[1] != dY[1]) stop("x and y must have the same number of rows", call. = FALSE)

  if (!identical(x_view$dtype, "double") || !identical(y_view$dtype, "double")) {
    stop("view_crossprod currently supports double matrices only", call. = FALSE)
  }

  # Require compatible row ranges (or NULL meaning full). Keep behavior strict
  # for now so the kernel can be reasoned about easily.
  if (!is.null(x_view$rows) && !is.null(y_view$rows) &&
      (!is_idx_range(x_view$rows) || !is_idx_range(y_view$rows) ||
       x_view$rows$start != y_view$rows$start || x_view$rows$end != y_view$rows$end)) {
    stop("x_view and y_view must have identical row ranges (or NULL)", call. = FALSE)
  }

  rs <- if (is.null(x_view$rows) && is.null(y_view$rows)) c(1L, dX[1]) else {
    r <- if (!is.null(x_view$rows)) x_view$rows else y_view$rows
    c(r$start, r$end)
  }

  xcs <- if (is.null(x_view$cols)) 1L else x_view$cols$start
  xce <- if (is.null(x_view$cols)) dX[2] else x_view$cols$end
  ycs <- if (is.null(y_view$cols)) 1L else y_view$cols$start
  yce <- if (is.null(y_view$cols)) dY[2] else y_view$cols$end

  out <- .Call(
    "C_shard_mat_crossprod_block",
    xb,
    yb,
    as.integer(rs[1]),
    as.integer(rs[2]),
    as.integer(xcs),
    as.integer(xce),
    as.integer(ycs),
    as.integer(yce),
    PACKAGE = "shard"
  )

  dnX <- dimnames(xb)
  dnY <- dimnames(yb)
  rn <- if (!is.null(dnX) && length(dnX) == 2L) dnX[[2]] else NULL
  cn <- if (!is.null(dnY) && length(dnY) == 2L) dnY[[2]] else NULL
  if (!is.null(rn)) rn <- rn[xcs:xce]
  if (!is.null(cn)) cn <- cn[ycs:yce]
  if (!is.null(rn) || !is.null(cn)) dimnames(out) <- list(rn, cn)

  out
}

tiles2d <- function(n_x, n_y, block_x, block_y) {
  n_x <- as.integer(n_x)
  n_y <- as.integer(n_y)
  block_x <- as.integer(block_x)
  block_y <- as.integer(block_y)

  if (is.na(n_x) || n_x < 1L) stop("n_x must be a positive integer", call. = FALSE)
  if (is.na(n_y) || n_y < 1L) stop("n_y must be a positive integer", call. = FALSE)
  if (is.na(block_x) || block_x < 1L) stop("block_x must be a positive integer", call. = FALSE)
  if (is.na(block_y) || block_y < 1L) stop("block_y must be a positive integer", call. = FALSE)

  x_blocks <- ceiling(n_x / block_x)
  y_blocks <- ceiling(n_y / block_y)
  num_tiles <- x_blocks * y_blocks

  shards <- vector("list", num_tiles)
  id <- 1L
  for (xb in seq_len(x_blocks)) {
    x_start <- (xb - 1L) * block_x + 1L
    x_end <- min(xb * block_x, n_x)
    for (yb in seq_len(y_blocks)) {
      y_start <- (yb - 1L) * block_y + 1L
      y_end <- min(yb * block_y, n_y)

      shards[[id]] <- list(
        id = id,
        x_start = x_start,
        x_end = x_end,
        y_start = y_start,
        y_end = y_end,
        x_len = x_end - x_start + 1L,
        y_len = y_end - y_start + 1L,
        len = (x_end - x_start + 1L) * (y_end - y_start + 1L)
      )
      id <- id + 1L
    }
  }

  structure(
    list(
      n = num_tiles,
      block_size = NA_integer_,
      strategy = "tiles2d",
      num_shards = length(shards),
      shards = shards,
      x_dim = n_x,
      y_dim = n_y,
      x_block = block_x,
      y_block = block_y
    ),
    class = c("shard_tiles", "shard_descriptor")
  )
}

#' @export
print.shard_tiles <- function(x, ...) {
  cat("shard tiles (2D)\n")
  cat("  Tiles:", x$num_shards, "\n")
  cat("  X dim:", x$x_dim, " block:", x$x_block, "\n")
  cat("  Y dim:", x$y_dim, " block:", x$y_block, "\n")
  invisible(x)
}
