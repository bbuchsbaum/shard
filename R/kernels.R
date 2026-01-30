# High-level "low ceremony" helpers for common shard-aware kernels.

#' Parallel crossprod() using shard views + output buffers
#'
#' Computes `crossprod(X, Y)` (i.e. `t(X) %*% Y`) using:
#' - shared/mmap-backed inputs (one copy),
#' - block views (no slice materialization),
#' - BLAS-3 dgemm in each tile,
#' - an explicit shared output buffer (no gather/bind spikes).
#'
#' This is intended as an ergonomic entry point for the "wow" path: users
#' shouldn't have to manually call `share()`, `view_block()`, `buffer()`,
#' `tiles2d()`, and `shard_map()` for common patterns.
#'
#' @param X,Y Double matrices with the same number of rows.
#' @param workers Number of worker processes.
#' @param block_x,block_y Tile sizes over `ncol(X)` and `ncol(Y)`. Use `"auto"`
#'   (default) to autotune on the current machine.
#' @param backing Backing for shared inputs and output buffer (`"mmap"` or `"shm"`).
#' @param materialize Whether to return the result as a standard R matrix:
#'   `"never"` (return buffer handle), `"always"`, or `"auto"` (materialize if
#'   estimated output size is below `materialize_max_bytes`).
#' @param materialize_max_bytes Threshold for `"auto"` materialization.
#' @param diagnostics Whether to collect shard_map diagnostics.
#'
#' @return A list with:
#'   - `buffer`: shard_buffer for the result (p x v)
#'   - `value`: materialized matrix if requested, otherwise NULL
#'   - `run`: the underlying shard_result from shard_map
#'   - `tile`: chosen tile sizes
#' @export
shard_crossprod <- function(X,
                            Y,
                            workers = NULL,
                            block_x = "auto",
                            block_y = "auto",
                            backing = c("mmap", "shm"),
                            materialize = c("auto", "never", "always"),
                            materialize_max_bytes = 512 * 1024^2,
                            diagnostics = TRUE) {
  backing <- match.arg(backing)
  materialize <- match.arg(materialize)

  if (!is.matrix(X) || !is.matrix(Y)) stop("X and Y must be matrices", call. = FALSE)
  if (!identical(typeof(X), "double") || !identical(typeof(Y), "double")) {
    stop("shard_crossprod currently supports double matrices only", call. = FALSE)
  }
  if (nrow(X) != nrow(Y)) stop("X and Y must have the same number of rows", call. = FALSE)

  Xsh <- if (is_shared_vector(X)) X else share(X, backing = backing)
  Ysh <- if (is_shared_vector(Y)) Y else share(Y, backing = backing)

  p <- ncol(Xsh)
  v <- ncol(Ysh)

  if (identical(block_x, "auto") || identical(block_y, "auto")) {
    tuned <- .autotune_crossprod_tiles(Xsh, Ysh,
                                      block_x = block_x, block_y = block_y)
    block_x <- tuned$block_x
    block_y <- tuned$block_y
  } else {
    block_x <- as.integer(block_x)
    block_y <- as.integer(block_y)
  }

  if (is.na(block_x) || block_x < 1L) stop("block_x must be >= 1", call. = FALSE)
  if (is.na(block_y) || block_y < 1L) stop("block_y must be >= 1", call. = FALSE)
  block_x <- min(block_x, p)
  block_y <- min(block_y, v)

  Z <- buffer("double", dim = c(p, v), init = 0, backing = backing)
  tiles <- tiles2d(n_x = p, n_y = v, block_x = block_x, block_y = block_y)

  run <- shard_map(
    tiles,
    borrow = list(X = Xsh, Y = Ysh),
    out = list(Z = Z),
    fun = function(tile, X, Y, Z) {
      vX <- view_block(X, cols = idx_range(tile$x_start, tile$x_end))
      vY <- view_block(Y, cols = idx_range(tile$y_start, tile$y_end))
      blk <- view_crossprod(vX, vY)
      Z[tile$x_start:tile$x_end, tile$y_start:tile$y_end] <- blk
      NULL
    },
    workers = workers,
    diagnostics = diagnostics
  )

  out_bytes <- as.double(p) * as.double(v) * 8
  do_mat <- switch(materialize,
    "never" = FALSE,
    "always" = TRUE,
    "auto" = isTRUE(out_bytes <= materialize_max_bytes)
  )

  list(
    buffer = Z,
    value = if (do_mat) as.matrix(Z) else NULL,
    run = run,
    tile = c(block_x = block_x, block_y = block_y)
  )
}

.autotune_crossprod_tiles <- function(Xsh, Ysh, block_x = "auto", block_y = "auto") {
  p <- ncol(Xsh)
  v <- ncol(Ysh)

  # Candidate sizes are chosen to hit common BLAS sweet spots without creating
  # huge temporary matrices (tile result is kx x ky).
  cand_x <- c(8L, 16L, 32L, 64L)
  cand_y <- c(16L, 32L, 64L, 128L)
  cand_x <- cand_x[cand_x <= p]
  cand_y <- cand_y[cand_y <= v]

  if (!identical(block_x, "auto")) cand_x <- as.integer(block_x)
  if (!identical(block_y, "auto")) cand_y <- as.integer(block_y)

  # For small problems, just take the max that fits.
  if (p <= 64L && v <= 128L) {
    return(list(block_x = min(max(cand_x), p), block_y = min(max(cand_y), v)))
  }

  best <- list(score = -Inf, block_x = min(max(cand_x), p), block_y = min(max(cand_y), v))
  trials <- 3L

  for (bx in cand_x) {
    for (by in cand_y) {
      if (bx > p || by > v) next

      times <- numeric(trials)
      for (t in seq_len(trials)) {
        xs <- if (p == bx) 1L else sample.int(p - bx + 1L, 1L)
        ys <- if (v == by) 1L else sample.int(v - by + 1L, 1L)

        vX <- view_block(Xsh, cols = idx_range(xs, xs + bx - 1L))
        vY <- view_block(Ysh, cols = idx_range(ys, ys + by - 1L))

        t0 <- proc.time()[["elapsed"]]
        view_crossprod(vX, vY)
        times[t] <- proc.time()[["elapsed"]] - t0
      }

      med <- stats::median(times)
      if (!is.finite(med) || med <= 0) next

      # Throughput proxy: output elements per second.
      score <- (as.double(bx) * as.double(by)) / med
      if (score > best$score) best <- list(score = score, block_x = bx, block_y = by)
    }
  }

  list(block_x = best$block_x, block_y = best$block_y)
}
