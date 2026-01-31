# Kernel registry for shard_map().
#
# This provides a low-ceremony way to select known high-performance kernels
# without forcing users to write the same boilerplate fun() repeatedly.

.kernel_env <- new.env(parent = emptyenv())
.kernel_env$registry <- new.env(parent = emptyenv())

#' Register a shard kernel
#'
#' Registers a named kernel implementation that can be selected by
#' `shard_map(..., kernel = "name")`.
#'
#' A "kernel" is just a function that shard_map can call for each shard. The
#' registry lets shard_map attach additional metadata (footprint hints,
#' supports_views) for scheduling/autotuning.
#'
#' @param name Kernel name (string).
#' @param impl Function implementing the kernel. It must accept the shard
#'   descriptor as its first argument.
#' @param signature Optional short signature string for documentation.
#' @param footprint Optional footprint hint. Either a constant (bytes) or a
#'   function `(shard, ...) -> list(class='tiny'|'medium'|'huge', bytes=...)`.
#' @param supports_views Logical. Whether the kernel is intended to operate on
#'   shard views without slice materialization.
#' @param description Optional human-readable description.
#' @return Invisibly, the registered kernel metadata.
#' @export
register_kernel <- function(name,
                            impl,
                            signature = NULL,
                            footprint = NULL,
                            supports_views = TRUE,
                            description = NULL) {
  name <- as.character(name)
  if (!nzchar(name)) stop("name must be a non-empty string", call. = FALSE)
  if (!is.function(impl)) stop("impl must be a function", call. = FALSE)
  if (!is.null(signature) && !is.character(signature)) stop("signature must be a string or NULL", call. = FALSE)
  if (!is.null(description) && !is.character(description)) stop("description must be a string or NULL", call. = FALSE)
  if (!is.null(footprint) && !(is.function(footprint) || is.numeric(footprint))) {
    stop("footprint must be a function, numeric, or NULL", call. = FALSE)
  }

  meta <- list(
    name = name,
    impl = impl,
    signature = signature,
    footprint = footprint,
    supports_views = isTRUE(supports_views),
    description = description
  )

  assign(name, meta, envir = .kernel_env$registry)
  invisible(meta)
}

#' List registered kernels
#'
#' @return A character vector of registered kernel names.
#' @export
list_kernels <- function() {
  ls(.kernel_env$registry, all.names = TRUE)
}

get_kernel <- function(name) {
  name <- as.character(name)
  if (!exists(name, envir = .kernel_env$registry, inherits = FALSE)) return(NULL)
  get(name, envir = .kernel_env$registry, inherits = FALSE)
}

# Built-in kernel: crossprod tile (t(X_block) %*% Y_block) into an output buffer.
.kernel_crossprod_tile <- function(tile, X, Y, Z) {
  vX <- view_block(X, cols = idx_range(tile$x_start, tile$x_end))
  vY <- view_block(Y, cols = idx_range(tile$y_start, tile$y_end))
  blk <- view_crossprod(vX, vY)
  Z[tile$x_start:tile$x_end, tile$y_start:tile$y_end] <- blk
  NULL
}

register_kernel(
  name = "crossprod_tile",
  impl = .kernel_crossprod_tile,
  signature = "(tile, X, Y, Z) -> NULL",
  footprint = function(tile) {
    bx <- as.integer(tile$x_end - tile$x_start + 1L)
    by <- as.integer(tile$y_end - tile$y_start + 1L)
    bytes <- as.double(bx) * as.double(by) * 8
    cls <- if (bytes >= 64 * 1024^2) "huge" else if (bytes >= 8 * 1024^2) "medium" else "tiny"
    list(class = cls, bytes = bytes)
  },
  supports_views = TRUE,
  description = "Compute a crossprod tile using block views and BLAS-3."
)

.kernel_col_means <- function(shard, X, out) {
  if (is.null(shard$start) || is.null(shard$end)) {
    stop("col_means kernel requires contiguous shard descriptors (start/end)", call. = FALSE)
  }
  cs <- as.integer(shard$start)
  ce <- as.integer(shard$end)
  if (is.na(cs) || is.na(ce) || cs < 1L || ce < cs) stop("Invalid shard start/end", call. = FALSE)
  v <- view_block(X, cols = idx_range(cs, ce))
  out[cs:ce] <- view_col_sums(v) / as.double(nrow(X))
  NULL
}

register_kernel(
  name = "col_means",
  impl = .kernel_col_means,
  signature = "(shard, X, out) -> NULL",
  supports_views = TRUE,
  description = "Compute column means for a contiguous shard using view_col_sums()."
)

.kernel_col_vars <- function(shard, X, out) {
  if (is.null(shard$start) || is.null(shard$end)) {
    stop("col_vars kernel requires contiguous shard descriptors (start/end)", call. = FALSE)
  }
  cs <- as.integer(shard$start)
  ce <- as.integer(shard$end)
  if (is.na(cs) || is.na(ce) || cs < 1L || ce < cs) stop("Invalid shard start/end", call. = FALSE)
  v <- view_block(X, cols = idx_range(cs, ce))
  out[cs:ce] <- view_col_vars(v)
  NULL
}

register_kernel(
  name = "col_vars",
  impl = .kernel_col_vars,
  signature = "(shard, X, out) -> NULL",
  supports_views = TRUE,
  description = "Compute sample column variances for a contiguous shard using view_col_vars()."
)
