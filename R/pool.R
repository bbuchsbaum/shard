#' @title Worker Pool Management
#' @description Spawn and supervise persistent R worker processes with RSS monitoring.
#' @name pool
NULL

# Module-level pool state (package environment)
.pool_env <- new.env(parent = emptyenv())
.pool_env$pool <- NULL
.pool_env$dev_path <- NULL

#' Create a Worker Pool
#'
#' Spawns N R worker processes that persist across multiple `shard_map()` calls.
#' Workers are supervised and recycled when RSS drift exceeds thresholds.
#'
#' @param n Integer. Number of worker processes to spawn.
#' @param rss_limit Numeric or character. Maximum RSS per worker before recycling.
#'   Can be bytes (numeric) or human-readable (e.g., "2GB"). Default is "2GB".
#' @param rss_drift_threshold Numeric. Fraction of RSS increase from baseline
#'   that triggers recycling (default 0.5 = 50% growth).
#' @param heartbeat_interval Numeric. Seconds between health checks (default 5).
#' @param min_recycle_interval Numeric. Minimum time in seconds between recycling
#'   the same worker (default 1.0). This prevents thrashing PSOCK worker creation
#'   under extremely tight RSS limits.
#' @param init_expr Expression to evaluate in each worker on startup.
#' @param packages Character vector. Packages to load in workers.
#'
#' @return A `shard_pool` object (invisibly). The pool is also stored in the
#'   package environment for reuse.
#'
#' @export
pool_create <- function(n = parallel::detectCores() - 1L,
                        rss_limit = "2GB",
                        rss_drift_threshold = 0.5,
                        heartbeat_interval = 5,
                        min_recycle_interval = 1.0,
                        init_expr = NULL,
                        packages = NULL) {
  # Validate inputs
  n <- as.integer(n)
  if (is.na(n)) n <- 1L
  if (n < 1L) {
    stop("pool_create: n must be >= 1", call. = FALSE)
  }

  rss_limit_bytes <- parse_bytes(rss_limit)

  # Stop existing pool if any
  if (!is.null(.pool_env$pool)) {
    pool_stop()
  }

  # Create pool structure
  dev_path <- tryCatch(getNamespaceInfo("shard", "path"), error = function(e) NULL)
  if (!is.null(dev_path) && isTRUE(file.exists(file.path(dev_path, ".git")))) {
    .pool_env$dev_path <- dev_path
  } else {
    # Best-effort dev mode: if we're in the *shard* repo root, use it as
    # dev_path so workers can load the in-tree R code without requiring build
    # tools.
    repo_root <- tryCatch(normalizePath(".", winslash = "/", mustWork = TRUE), error = function(e) NULL)
    if (!is.null(repo_root) &&
        isTRUE(file.exists(file.path(repo_root, ".git"))) &&
        isTRUE(file.exists(file.path(repo_root, "DESCRIPTION")))) {
      desc <- tryCatch(readLines(file.path(repo_root, "DESCRIPTION"), warn = FALSE), error = function(e) character())
      is_shard_pkg <- any(grepl("^Package:\\s*shard\\s*$", desc))
      looks_like_shard_repo <- isTRUE(file.exists(file.path(repo_root, "R", "pool.R"))) &&
        isTRUE(file.exists(file.path(repo_root, "R", "worker.R")))
      if (!isTRUE(is_shard_pkg && looks_like_shard_repo)) {
        .pool_env$dev_path <- NULL
        dev_path <- NULL
      } else {
        .pool_env$dev_path <- repo_root
        dev_path <- repo_root
      }
    } else {
      .pool_env$dev_path <- NULL
      dev_path <- NULL
    }
  }

  pool <- structure(
    list(
      workers = vector("list", n),
      n = n,
      rss_limit_bytes = rss_limit_bytes,
      rss_drift_threshold = rss_drift_threshold,
      heartbeat_interval = heartbeat_interval,
      min_recycle_interval = as.double(min_recycle_interval),
      dev_path = dev_path,
      init_expr = init_expr,
      packages = packages,
      created_at = Sys.time(),
      stats = list(
        total_recycles = 0L,
        total_deaths = 0L,
        total_tasks = 0L
      )
    ),
    class = "shard_pool"
  )

  # Spawn workers
  for (i in seq_len(n)) {
    pool$workers[[i]] <- worker_spawn(
      id = i,
      init_expr = init_expr,
      packages = packages,
      dev_path = dev_path
    )
  }

  # Record baseline RSS for each worker
  for (i in seq_len(n)) {
    pool$workers[[i]]$rss_baseline <- worker_rss(pool$workers[[i]])
  }

  .pool_env$pool <- pool
  invisible(pool)
}

#' Get the Current Worker Pool
#'
#' Returns the active worker pool, or NULL if none exists.
#'
#' @return A `shard_pool` object or NULL.
#' @export
pool_get <- function() {
  .pool_env$pool
}

#' Stop the Worker Pool
#'
#' Terminates all worker processes and releases resources. Waits for workers
#' to actually terminate before returning.
#'
#' @param pool A `shard_pool` object. If NULL, uses the current pool.
#' @param timeout Numeric. Seconds to wait for workers to terminate (default 5).
#'   Returns after timeout even if workers are still alive.
#' @return NULL (invisibly).
#' @export
pool_stop <- function(pool = NULL, timeout = 5) {
  if (is.null(pool)) {
    pool <- .pool_env$pool
  }

  if (is.null(pool)) {
    return(invisible(NULL))
  }

  # Collect PIDs before killing
  pids <- vapply(pool$workers, function(w) {
    if (is.null(w)) NA_integer_ else w$pid
  }, integer(1))
  alive_pids <- pids[!is.na(pids)]

  # Fast path: if all workers already dead, just clean up
  if (length(alive_pids) > 0) {
    alive_check <- vapply(alive_pids, pid_is_alive, logical(1))
    if (!any(alive_check)) {
      .pool_env$pool <- NULL
      return(invisible(NULL))
    }
  }

  # Kill all workers
  for (w in pool$workers) {
    if (!is.null(w)) {
      tryCatch(worker_kill(w), error = function(e) NULL)
    }
  }

  # Wait for workers to terminate with timeout
  if (length(alive_pids) > 0 && timeout > 0) {
    deadline <- Sys.time() + timeout
    while (Sys.time() < deadline) {
      still_alive <- vapply(alive_pids, pid_is_alive, logical(1))
      if (!any(still_alive)) {
        break
      }
      Sys.sleep(0.1)
    }
  }

  .pool_env$pool <- NULL
  invisible(NULL)
}

#' Check Pool Health
#'
#' Monitors all workers, recycling those with excessive RSS drift or that have died.
#'
#' @param pool A `shard_pool` object. If NULL, uses the current pool.
#' @param busy_workers Optional integer vector of worker ids that are currently
#'   running tasks (used internally by the dispatcher to avoid recycling a worker
#'   while a result is in flight).
#' @return A list with health status per worker and actions taken.
#' @export
pool_health_check <- function(pool = NULL, busy_workers = NULL) {
  if (is.null(pool)) {
    pool <- .pool_env$pool
  }

  if (is.null(pool)) {
    stop("No active pool. Call pool_create() first.", call. = FALSE)
  }

  actions <- vector("list", pool$n)
  busy <- rep(FALSE, pool$n)
  if (!is.null(busy_workers)) {
    busy_workers <- as.integer(busy_workers)
    busy_workers <- busy_workers[busy_workers >= 1L & busy_workers <= pool$n]
    busy[busy_workers] <- TRUE
  }

  for (i in seq_len(pool$n)) {
    w <- pool$workers[[i]]
    action <- list(worker_id = i, action = "none", reason = NA_character_)

    if (is.null(w) || !worker_is_alive(w)) {
      # Worker died - restart it
      action$action <- "restart"
      action$reason <- "worker_dead"
      pool$workers[[i]] <- worker_spawn(
        id = i,
        init_expr = pool$init_expr,
        packages = pool$packages,
        dev_path = pool$dev_path
      )
      pool$workers[[i]]$needs_recycle <- FALSE
      pool$workers[[i]]$rss_baseline <- worker_rss(pool$workers[[i]])
      pool$stats$total_deaths <- pool$stats$total_deaths + 1L
    } else {
      # Check RSS drift
      current_rss <- worker_rss(w)
      baseline <- w$rss_baseline %||% current_rss

      drift <- (current_rss - baseline) / max(baseline, 1)
      exceeds_limit <- current_rss > pool$rss_limit_bytes
      exceeds_drift <- drift > pool$rss_drift_threshold

      if (exceeds_limit || exceeds_drift) {
        action$reason <- if (exceeds_limit) "rss_limit" else "rss_drift"
        action$rss_before <- current_rss

        min_age <- pool$min_recycle_interval %||% 0
        age <- as.numeric(difftime(Sys.time(), w$spawned_at %||% Sys.time(), units = "secs"))

        if (isTRUE(busy[i]) || (!is.na(age) && is.finite(min_age) && age < min_age)) {
          # Avoid recycling a worker while it has an in-flight task. Recycling
          # closes the PSOCK connection and can race with recv/unserialize.
          w$needs_recycle <- TRUE
          pool$workers[[i]] <- w
          action$action <- "defer_recycle"
        } else {
          action$action <- "recycle"
          pool$workers[[i]] <- worker_recycle(w, pool$init_expr, pool$packages, dev_path = pool$dev_path)
          pool$workers[[i]]$needs_recycle <- FALSE
          pool$workers[[i]]$rss_baseline <- worker_rss(pool$workers[[i]])
          pool$stats$total_recycles <- pool$stats$total_recycles + 1L
        }
      } else {
        w$needs_recycle <- isTRUE(w$needs_recycle)
        pool$workers[[i]] <- w
        action$rss_current <- current_rss
        action$rss_drift <- drift
      }
    }

    actions[[i]] <- action
  }

  # Update pool in environment
  .pool_env$pool <- pool

  structure(
    list(
      timestamp = Sys.time(),
      pool_stats = pool$stats,
      worker_actions = actions
    ),
    class = "shard_health_report"
  )
}

#' Get Pool Status
#'
#' Returns current status of all workers in the pool.
#'
#' @param pool A `shard_pool` object. If NULL, uses the current pool.
#' @return A data frame with worker status information.
#' @export
pool_status <- function(pool = NULL) {
  if (is.null(pool)) {
    pool <- .pool_env$pool
  }

  if (is.null(pool)) {
    return(data.frame(
      worker_id = integer(0),
      pid = integer(0),
      status = character(0),
      rss_bytes = numeric(0),
      rss_baseline = numeric(0),
      rss_drift = numeric(0),
      recycle_count = integer(0)
    ))
  }

  rows <- lapply(seq_len(pool$n), function(i) {
    w <- pool$workers[[i]]
    if (is.null(w)) {
      return(data.frame(
        worker_id = i,
        pid = NA_integer_,
        status = "missing",
        rss_bytes = NA_real_,
        rss_baseline = NA_real_,
        rss_drift = NA_real_,
        recycle_count = 0L
      ))
    }

    alive <- worker_is_alive(w)
    rss <- if (alive) worker_rss(w) else NA_real_
    baseline <- w$rss_baseline %||% NA_real_
    drift <- if (!is.na(rss) && !is.na(baseline) && baseline > 0) {
      (rss - baseline) / baseline
    } else {
      NA_real_
    }

    data.frame(
      worker_id = i,
      pid = w$pid,
      status = if (alive) "ok" else "dead",
      rss_bytes = rss,
      rss_baseline = baseline,
      rss_drift = drift,
      recycle_count = w$recycle_count %||% 0L
    )
  })

  do.call(rbind, rows)
}

#' Dispatch Task to Worker
#'
#' Sends a task to a specific worker and waits for the result.
#'
#' @param worker_id Integer. Worker to dispatch to.
#' @param expr Expression to evaluate.
#' @param envir Environment containing variables needed by expr.
#' @param pool A `shard_pool` object. If NULL, uses the current pool.
#' @param timeout Numeric. Seconds to wait for result (default 3600).
#'
#' @return The result of evaluating expr in the worker.
#' @export
pool_dispatch <- function(worker_id, expr, envir = parent.frame(),
                          pool = NULL, timeout = 3600) {
  if (is.null(pool)) {
    pool <- .pool_env$pool
  }

  if (is.null(pool)) {
    stop("No active pool. Call pool_create() first.", call. = FALSE)
  }

  if (worker_id < 1L || worker_id > pool$n) {
    stop("Invalid worker_id: ", worker_id, call. = FALSE)
  }

  w <- pool$workers[[worker_id]]
  if (is.null(w) || !worker_is_alive(w)) {
    # Restart dead worker before dispatch
    pool$workers[[worker_id]] <- worker_spawn(
      id = worker_id,
      init_expr = pool$init_expr,
      packages = pool$packages
    )
    pool$workers[[worker_id]]$rss_baseline <- worker_rss(pool$workers[[worker_id]])
    pool$stats$total_deaths <- pool$stats$total_deaths + 1L
    .pool_env$pool <- pool
    w <- pool$workers[[worker_id]]
  }

  pool$stats$total_tasks <- pool$stats$total_tasks + 1L
  .pool_env$pool <- pool

  worker_eval(w, expr, envir, timeout)
}

#' @export
print.shard_pool <- function(x, ...) {
  cat("shard worker pool\n")
  cat("  Workers:", x$n, "\n")
  cat("  RSS limit:", format_bytes(x$rss_limit_bytes), "\n")
  cat("  Drift threshold:", sprintf("%.0f%%", x$rss_drift_threshold * 100), "\n")
  cat("  Created:", format(x$created_at), "\n")
  cat("  Stats:\n")
  cat("    Total recycles:", x$stats$total_recycles, "\n")
  cat("    Total deaths:", x$stats$total_deaths, "\n")
  cat("    Total tasks:", x$stats$total_tasks, "\n")
  invisible(x)
}

#' @export
print.shard_health_report <- function(x, ...) {
  cat("Pool health check at", format(x$timestamp), "\n")
  cat("Stats: recycles =", x$pool_stats$total_recycles,
      ", deaths =", x$pool_stats$total_deaths,
      ", tasks =", x$pool_stats$total_tasks, "\n")

  actions_summary <- vapply(x$worker_actions, function(a) a$action, character(1))
  if (all(actions_summary == "none")) {
    cat("All workers healthy\n")
  } else {
    cat("Actions taken:\n")
    for (a in x$worker_actions) {
      if (a$action != "none") {
        cat("  Worker", a$worker_id, ":", a$action, "(", a$reason, ")\n")
      }
    }
  }
  invisible(x)
}
