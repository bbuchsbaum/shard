#' @title Individual Worker Control
#' @description Spawn, monitor, and control individual R worker processes.
#' @name worker
NULL

#' Spawn a Worker Process
#'
#' Creates a new R worker process using parallel sockets.
#'
#' @param id Integer. Worker identifier.
#' @param init_expr Expression to evaluate on startup.
#' @param packages Character vector. Packages to load.
#'
#' @return A `shard_worker` object.
#' @keywords internal
worker_spawn <- function(id, init_expr = NULL, packages = NULL, dev_path = NULL) {
  # Use makeCluster to spawn a single worker
  # This creates a socket connection to an R process
  # Use --vanilla to avoid user/site profiles that may pre-load packages and
  # interfere with deterministic worker setup (notably ALTREP class methods).
  cl <- parallel::makeCluster(1L, type = "PSOCK", outfile = "", rscript_args = "--vanilla")
  node <- cl[[1]]

  # Get the PID of the worker
  pid <- tryCatch(
    parallel::clusterCall(cl, function() Sys.getpid())[[1]],
    error = function(e) NA_integer_
  )

  # Ensure workers can load the same library paths as the master process.
  # This is critical for ALTREP class registration and for loading this
  # in-development package when tests install to a temp lib.
  master_libpaths <- .libPaths()
  parallel::clusterCall(cl, function(paths, dev_path) {
    .libPaths(paths)

    # If shard was loaded via user/site profiles (or other bootstrapping),
    # unload it so we re-load from the intended library path.
    if ("package:shard" %in% search()) {
      try(detach("package:shard", unload = TRUE, character.only = TRUE), silent = TRUE)
    }
    if ("shard" %in% loadedNamespaces()) {
      try(unloadNamespace("shard"), silent = TRUE)
    }

    if (!is.null(dev_path) && nzchar(dev_path)) {
      # In dev/test runs, ensure workers run the same in-tree code as the master.
      #
      # compile = FALSE avoids requiring system build tools as long as an
      # up-to-date `src/shard.so` is present (e.g. from a prior build).
      if (requireNamespace("pkgload", quietly = TRUE)) {
        pkgload::load_all(dev_path, quiet = TRUE, compile = FALSE, recompile = FALSE)
      } else {
        suppressPackageStartupMessages(library(shard))
      }
    } else {
      suppressPackageStartupMessages(library(shard))
    }
    TRUE
  }, master_libpaths, dev_path)

  # Load packages if specified
  if (length(packages) > 0) {
    parallel::clusterCall(cl, function(pkgs) {
      for (pkg in pkgs) {
        if (identical(pkg, "shard")) next
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      }
    }, packages)
  }

  # Run init expression if specified
  if (!is.null(init_expr)) {
    parallel::clusterCall(cl, function(expr) {
      eval(expr, envir = globalenv())
    }, init_expr)
  }

  structure(
    list(
      id = id,
      pid = pid,
      cluster = cl,
      node = node,
      spawned_at = Sys.time(),
      rss_baseline = NA_real_,
      recycle_count = 0L,
      needs_recycle = FALSE,
      status = "ok"
    ),
    class = "shard_worker"
  )
}

#' Check if Worker is Alive
#'
#' Tests whether the worker process is still running.
#'
#' @param worker A `shard_worker` object.
#' @return Logical. TRUE if worker is alive.
#' @keywords internal
worker_is_alive <- function(worker) {
  if (is.null(worker) || is.null(worker$cluster)) {
    return(FALSE)
  }

  # Prefer a PID-level check: cluster round-trips can fail when the worker is
  # legitimately busy (e.g. during async sendCall/recv dispatch).
  if (!is.na(worker$pid) && isTRUE(pid_is_alive(worker$pid))) {
    return(TRUE)
  }

  # Fallback: try a simple ping.
  tryCatch({
    result <- parallel::clusterCall(worker$cluster, function() TRUE)
    isTRUE(result[[1]])
  }, error = function(e) FALSE)
}

#' Get Worker RSS (Resident Set Size)
#'
#' Queries the worker's memory usage via the ps package if available,
#' otherwise falls back to /proc on Linux or ps command.
#'
#' @param worker A `shard_worker` object.
#' @return Numeric. RSS in bytes, or NA if unavailable.
#' @keywords internal
worker_rss <- function(worker) {
  if (is.null(worker) || is.na(worker$pid)) {
    return(NA_real_)
  }

  rss_get_pid(worker$pid)
}

#' Kill a Worker Process
#'
#' Terminates the worker process and closes connections.
#'
#' @param worker A `shard_worker` object.
#' @return NULL (invisibly).
#' @keywords internal
worker_kill <- function(worker) {
  if (is.null(worker)) {
    return(invisible(NULL))
  }

  close_sock <- function(w) {
    if (is.null(w$cluster) || length(w$cluster) < 1L) return(invisible(NULL))
    n <- w$cluster[[1]]
    if (!is.null(n) && !is.null(n$con)) {
      try(close(n$con), silent = TRUE)
    }
    invisible(NULL)
  }

  wait_dead <- function(pid, timeout = 2) {
    if (is.na(pid) || !isTRUE(pid_is_alive(pid))) return(invisible(NULL))
    deadline <- Sys.time() + timeout
    while (Sys.time() < deadline) {
      if (!isTRUE(pid_is_alive(pid))) break
      Sys.sleep(0.05)
    }
    invisible(NULL)
  }

  pid <- worker$pid %||% NA_integer_

  # Best-effort shutdown via parallel, but always fall back to PID kill and
  # always close the socket connection.
  tryCatch(parallel::stopCluster(worker$cluster), error = function(e) NULL)

  if (!is.na(pid) && isTRUE(pid_is_alive(pid))) {
    tryCatch(tools::pskill(pid, signal = 15L), error = function(e) NULL)
    wait_dead(pid, timeout = 1)
    if (isTRUE(pid_is_alive(pid))) {
      tryCatch(tools::pskill(pid, signal = 9L), error = function(e) NULL)
      wait_dead(pid, timeout = 1)
    }
  }

  close_sock(worker)

  invisible(NULL)
}

#' Recycle a Worker
#'
#' Kills the current worker and spawns a fresh replacement.
#' The new worker inherits the same ID but has a fresh R process.
#'
#' @param worker A `shard_worker` object.
#' @param init_expr Expression to evaluate on startup.
#' @param packages Character vector. Packages to load.
#' @return A new `shard_worker` object.
#' @keywords internal
worker_recycle <- function(worker, init_expr = NULL, packages = NULL, dev_path = NULL) {
  id <- worker$id
  old_recycle_count <- worker$recycle_count %||% 0L

  # Kill the old worker

  worker_kill(worker)

  # Spawn a fresh one
  new_worker <- worker_spawn(id, init_expr, packages, dev_path = dev_path)
  new_worker$recycle_count <- old_recycle_count + 1L

  new_worker
}

#' Evaluate Expression in Worker
#'
#' Sends an expression to the worker for evaluation.
#'
#' @param worker A `shard_worker` object.
#' @param expr Expression to evaluate.
#' @param envir Environment containing variables needed by expr.
#' @param timeout Numeric. Seconds to wait for result.
#' @return The result of evaluation.
#' @keywords internal
worker_eval <- function(worker, expr, envir = parent.frame(), timeout = 3600) {
  if (!worker_is_alive(worker)) {
    stop("Worker ", worker$id, " is not alive", call. = FALSE)
  }

  # Capture variables from environment that are referenced in expr
  expr_vars <- all.vars(expr)
  export_env <- new.env(parent = emptyenv())
  for (v in expr_vars) {
    if (exists(v, envir = envir, inherits = TRUE)) {
      export_env[[v]] <- get(v, envir = envir, inherits = TRUE)
    }
  }

  # Send to worker
  tryCatch({
    # Export variables
    if (length(ls(export_env)) > 0) {
      parallel::clusterExport(worker$cluster, ls(export_env), envir = export_env)
    }

    # Evaluate expression
    result <- parallel::clusterCall(worker$cluster, function(e) {
      eval(e, envir = globalenv())
    }, expr)

    result[[1]]
  }, error = function(e) {
    stop("Worker ", worker$id, " evaluation failed: ", conditionMessage(e), call. = FALSE)
  })
}

#' Get Worker Metrics
#'
#' Returns current metrics for a worker.
#'
#' @param worker A `shard_worker` object.
#' @return A list of worker metrics.
#' @keywords internal
worker_metrics <- function(worker) {
  alive <- worker_is_alive(worker)
  rss <- if (alive) worker_rss(worker) else NA_real_

  list(
    worker_id = worker$id,
    pid = worker$pid,
    host = "localhost",
    status = if (alive) "ok" else "dead",
    rss_bytes = rss,
    rss_baseline = worker$rss_baseline,
    recycle_count = worker$recycle_count %||% 0L,
    spawned_at = worker$spawned_at
  )
}

#' @export
print.shard_worker <- function(x, ...) {
  cat("shard worker [", x$id, "]\n", sep = "")
  cat("  PID:", x$pid, "\n")
  cat("  Status:", if (worker_is_alive(x)) "alive" else "dead", "\n")
  cat("  RSS baseline:", format_bytes(x$rss_baseline), "\n")
  cat("  Recycles:", x$recycle_count %||% 0L, "\n")
  cat("  Spawned:", format(x$spawned_at), "\n")
  invisible(x)
}

#' Check if PID is Alive
#'
#' @param pid Process ID to check.
#' @return Logical.
#' @keywords internal
pid_is_alive <- function(pid) {
  if (is.na(pid)) return(FALSE)

  # Use ps package if available
  if (requireNamespace("ps", quietly = TRUE)) {
    tryCatch({
      p <- ps::ps_handle(pid)
      ps::ps_is_running(p)
    }, error = function(e) FALSE)
  } else {
    # Fallback: try to send signal 0
    tryCatch({
      tools::pskill(pid, signal = 0L)
      TRUE
    }, error = function(e) FALSE)
  }
}
