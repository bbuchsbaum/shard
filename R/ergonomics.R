#' @title Ergonomic Apply/Lapply Wrappers
#' @description
#' Convenience wrappers that provide apply/lapply-style ergonomics while
#' preserving shard's core contract: shared immutable inputs, supervised
#' execution, and diagnostics.
#'
#' These functions are intentionally thin wrappers around [shard_map()] and
#' related primitives.
#' @name ergonomics
NULL

.shard_is_scalar <- function(x) {
  is.atomic(x) && length(x) == 1L && !is.null(x)
}

.shard_fetch_if_needed <- function(x) {
  if (inherits(x, c("shard_shared", "shard_deep_shared"))) {
    return(fetch(x))
  }
  x
}

.shard_auto_share_one <- function(x, min_bytes, readonly = TRUE, backing = "auto") {
  # Keep existing shared objects.
  if (is_shared(x) || is_shared_vector(x)) return(x)

  # Only auto-share atomic vectors/matrices/arrays; otherwise keep as-is.
  # Deep-sharing is powerful but should remain an explicit choice.
  if (!is.atomic(x) || is.null(x)) return(x)

  sz <- as.numeric(utils::object.size(x))
  if (is.na(sz) || sz < min_bytes) return(x)

  share(x, backing = backing, readonly = readonly)
}

#' Apply Wrapper Policy
#'
#' Centralizes safe defaults and guardrails for apply/lapply convenience wrappers.
#'
#' @param auto_share_min_bytes Minimum object size for auto-sharing (default "1MB").
#' @param max_gather_bytes Maximum estimated gathered result bytes before refusing
#'   to run (default "256MB").
#' @param cow Copy-on-write policy for borrowed inputs. One of `"deny"`, `"audit"`,
#'   or `"allow"`. Default `"deny"`.
#' @param profile Execution profile passed through to [shard_map()]. One of
#'   `"default"`, `"memory"`, or `"speed"`. Default `"default"`.
#' @param block_size Shard block size for apply-style workloads. Default `"auto"`.
#' @param backing Backing type used when auto-sharing (`"auto"`, `"mmap"`, `"shm"`).
#' @return An object of class `shard_apply_policy`.
#' @export
shard_apply_policy <- function(auto_share_min_bytes = "1MB",
                               max_gather_bytes = "256MB",
                               cow = c("deny", "audit", "allow"),
                               profile = c("default", "memory", "speed"),
                               block_size = "auto",
                               backing = c("auto", "mmap", "shm")) {
  cow <- match.arg(cow)
  profile <- match.arg(profile)
  backing <- match.arg(backing)

  structure(
    list(
      auto_share_min_bytes = parse_bytes(auto_share_min_bytes),
      max_gather_bytes = parse_bytes(max_gather_bytes),
      cow = cow,
      profile = profile,
      block_size = block_size,
      backing = backing
    ),
    class = "shard_apply_policy"
  )
}

#' @export
print.shard_apply_policy <- function(x, ...) {
  cat("shard_apply_policy\n")
  cat("  auto_share_min_bytes:", format_bytes(x$auto_share_min_bytes), "\n")
  cat("  max_gather_bytes:", format_bytes(x$max_gather_bytes), "\n")
  cat("  cow:", x$cow, "\n")
  cat("  profile:", x$profile, "\n")
  cat("  block_size:", as.character(x$block_size), "\n")
  cat("  backing:", x$backing, "\n")
  invisible(x)
}

#' Apply a Function Over Matrix Columns with Shared Inputs
#'
#' A convenience wrapper for the common "per-column apply" pattern. The matrix
#' is shared once and each worker receives a zero-copy column view when possible.
#'
#' Current limitation: `MARGIN` must be 2 (columns). Row-wise apply would require
#' strided/gather slicing and is intentionally explicit in shard via views/kernels.
#'
#' @param X A numeric/integer/logical matrix (or a shared matrix created by [share()]).
#' @param MARGIN Must be 2 (columns).
#' @param FUN Function of the form `function(v, ...)` returning a scalar atomic.
#' @param VARS Optional named list of extra variables. Large atomic VARS are
#'   auto-shared based on `policy$auto_share_min_bytes`.
#' @param workers Number of workers (passed to [shard_map()]).
#' @param ... Additional arguments forwarded to `FUN`.
#' @param policy A [shard_apply_policy()] object.
#' @return An atomic vector of length `ncol(X)` with the results.
#' @export
shard_apply_matrix <- function(X,
                               MARGIN = 2,
                               FUN,
                               VARS = NULL,
                               workers = NULL,
                               ...,
                               policy = shard_apply_policy()) {
  if (!is.function(FUN)) stop("FUN must be a function", call. = FALSE)
  if (length(MARGIN) != 1 || is.na(MARGIN) || as.integer(MARGIN) != 2L) {
    stop("Only MARGIN=2 (columns) is supported; use shard_map()+views for other patterns.", call. = FALSE)
  }
  if (!is.matrix(X)) stop("X must be a matrix", call. = FALSE)

  # Ensure shared backing (ALTREP vector with dim) so workers can receive a
  # reopenable descriptor rather than a full copy.
  Xsh <- if (is_shared_vector(X) || is_shared(X)) X else share(X, readonly = TRUE, backing = policy$backing)
  if (!is_shared_vector(Xsh) && is_shared(Xsh)) {
    # share() for some objects yields a handle; fetch into a usable matrix
    # (still reopens in workers).
    Xsh <- fetch(Xsh)
  }
  if (!is.matrix(Xsh)) stop("Failed to obtain shared matrix representation", call. = FALSE)
  if (!is_shared_vector(Xsh)) {
    stop("Expected X to be represented as a shared vector-backed matrix; got: ", paste(class(Xsh), collapse = ", "), call. = FALSE)
  }

  vars <- VARS %||% list()
  if (!is.list(vars)) stop("VARS must be a named list (or NULL)", call. = FALSE)
  if (length(vars) > 0 && (is.null(names(vars)) || any(!nzchar(names(vars))))) {
    stop("VARS must be a named list (or NULL)", call. = FALSE)
  }
  extra_args <- list(...)

  # Auto-share large atomic vars to avoid duplicating them into each worker.
  if (length(vars) > 0) {
    for (nm in names(vars)) {
      vars[[nm]] <- .shard_auto_share_one(
        vars[[nm]],
        min_bytes = policy$auto_share_min_bytes,
        readonly = TRUE,
        backing = policy$backing
      )
    }
  }

  p <- ncol(Xsh)
  blocks <- shards(p, workers = workers, block_size = policy$block_size)
  out <- buffer("double", dim = p, backing = policy$backing, init = NA_real_)

  run <- shard_map(
    blocks,
    borrow = c(list(X = Xsh), vars),
    out = list(out = out),
    workers = workers,
    profile = policy$profile,
    cow = policy$cow,
    fun = function(shard, X, out, ...) {
      # Capture user args via parent env serialization.
      FUN <- get("FUN", inherits = TRUE)
      extra_args <- get("extra_args", inherits = TRUE)
      extra_vars <- list(...)

      # We cannot assume base slicing is copy-free; build explicit column views.
      nr <- dim(X)[1]
      for (j in shard$idx) {
        start <- (j - 1L) * nr + 1L
        v <- shared_view(X, start = start, length = nr)

        args <- c(list(v), extra_vars, extra_args)
        res <- do.call(FUN, args, quote = TRUE)
        if (!.shard_is_scalar(res)) {
          stop("FUN must return a scalar atomic value for shard_apply_matrix()", call. = FALSE)
        }
        out[j] <- as.double(res)
      }
      NULL
    }
  )

  # If failures exist, surface them.
  if (length(run$failures %||% list()) > 0) {
    stop("shard_apply_matrix() failed for some shards; see report(run) / task_report(run).", call. = FALSE)
  }

  as.double(out[])
}

#' Apply a Function Over a List with Optional Auto-Sharing
#'
#' A convenience wrapper for list workloads that need supervision and shared
#' inputs. Large atomic list elements are auto-shared based on policy.
#'
#' This wrapper enforces guardrails to avoid accidental huge gathers: it
#' estimates the total gathered result size from a probe call and refuses
#' to run if it exceeds `policy$max_gather_bytes`.
#'
#' @param x A list.
#' @param FUN Function of the form `function(el, ...)`.
#' @param VARS Optional named list of extra variables (auto-shared when large).
#' @param workers Number of workers (passed to [shard_map()]).
#' @param ... Additional arguments forwarded to `FUN`.
#' @param policy A [shard_apply_policy()] object.
#' @return A list of results of length `length(x)`.
#' @export
shard_lapply_shared <- function(x,
                                FUN,
                                VARS = NULL,
                                workers = NULL,
                                ...,
                                policy = shard_apply_policy()) {
  if (!is.list(x)) stop("x must be a list", call. = FALSE)
  if (!is.function(FUN)) stop("FUN must be a function", call. = FALSE)

  vars <- VARS %||% list()
  if (!is.list(vars)) stop("VARS must be a named list (or NULL)", call. = FALSE)
  if (length(vars) > 0 && (is.null(names(vars)) || any(!nzchar(names(vars))))) {
    stop("VARS must be a named list (or NULL)", call. = FALSE)
  }
  extra_args <- list(...)

  # Probe to estimate gathered result size.
  if (length(x) > 0) {
    probe <- do.call(FUN, c(list(x[[1]]), vars, extra_args), quote = TRUE)
    est <- as.numeric(utils::object.size(probe)) * length(x)
    if (is.finite(est) && est > policy$max_gather_bytes) {
      stop(
        "Refusing to gather an estimated ", format_bytes(est), " of results.\n",
        "  Hint: use shard_map()+buffer()/table_sink() or shard_reduce() for large outputs,\n",
        "  or raise policy$max_gather_bytes explicitly.",
        call. = FALSE
      )
    }
  }

  # Auto-share large atomic vars.
  if (length(vars) > 0) {
    for (nm in names(vars)) {
      vars[[nm]] <- .shard_auto_share_one(
        vars[[nm]],
        min_bytes = policy$auto_share_min_bytes,
        readonly = TRUE,
        backing = policy$backing
      )
    }
  }

  # Auto-share large atomic list elements; keep the list of handles small.
  shared_list <- x
  if (length(shared_list) > 0) {
    for (i in seq_along(shared_list)) {
      shared_list[[i]] <- .shard_auto_share_one(
        shared_list[[i]],
        min_bytes = policy$auto_share_min_bytes,
        readonly = TRUE,
        backing = policy$backing
      )
    }
  }

  n <- length(shared_list)
  if (n == 0) return(list())
  blocks <- shards(n, workers = workers, block_size = policy$block_size)

  # We gather results as a list; this wrapper is intentionally for moderate outputs.
  run <- shard_map(
    blocks,
    borrow = c(list(x = shared_list), vars),
    workers = workers,
    profile = policy$profile,
    cow = policy$cow,
    fun = function(shard, x, ...) {
      FUN <- get("FUN", inherits = TRUE)
      extra_args <- get("extra_args", inherits = TRUE)
      extra_vars <- list(...)

      out <- vector("list", length(shard$idx))
      for (k in seq_along(shard$idx)) {
        i <- shard$idx[[k]]
        el <- .shard_fetch_if_needed(x[[i]])
        out[[k]] <- do.call(FUN, c(list(el), extra_vars, extra_args), quote = TRUE)
      }
      names(out) <- as.character(shard$idx)
      out
    }
  )

  if (length(run$failures %||% list()) > 0) {
    stop("shard_lapply_shared() failed for some shards; see report(run) / task_report(run).", call. = FALSE)
  }

  # run$results is list of chunk results; each chunk result is a list of
  # per-shard return values.
  pieces <- run$results %||% list()
  res <- vector("list", n)
  for (chunk in pieces) {
    if (!is.list(chunk)) next
    for (shard_val in chunk) {
      if (!is.list(shard_val) || is.null(names(shard_val))) next
      for (nm in names(shard_val)) {
        idx <- as.integer(nm)
        if (!is.na(idx) && idx >= 1L && idx <= n) res[[idx]] <- shard_val[[nm]]
      }
    }
  }
  res
}
