#' @title Worker Pool Management
#' @description Spawn and supervise persistent R worker processes with RSS monitoring.
#' @name pool
NULL

# Module-level pool state (package environment)
.pool_env <- new.env(parent = emptyenv())
.pool_env$pool <- NULL

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
                        init_expr = NULL,
                        packages = NULL) {
  # Validate inputs
  n <- as.integer(n)
  if (n < 1L) {
    stop("pool_create: n must be >= 1", call. = FALSE)
  }

  rss_limit_bytes <- parse_bytes(rss_limit)

  # Stop existing pool if any
  if (!is.null(.pool_env$pool)) {
    pool_stop()
  }

  # Create pool structure
  pool <- structure(
    list(
      workers = vector("list", n),
      n = n,
      rss_limit_bytes = rss_limit_bytes,
      rss_drift_threshold = rss_drift_threshold,
      heartbeat_interval = heartbeat_interval,
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
      packages = packages
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
#' Terminates all worker processes and releases resources.
#'
#' @param pool A `shard_pool` object. If NULL, uses the current pool.
#' @return NULL (invisibly).
#' @export
pool_stop <- function(pool = NULL) {
  if (is.null(pool)) {
    pool <- .pool_env$pool
  }

  if (is.null(pool)) {
    return(invisible(NULL))
  }

  for (w in pool$workers) {
    if (!is.null(w)) {
      worker_kill(w)
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
#' @return A list with health status per worker and actions taken.
#' @export
pool_health_check <- function(pool = NULL) {
  if (is.null(pool)) {
    pool <- .pool_env$pool
  }

  if (is.null(pool)) {
    stop("No active pool. Call pool_create() first.", call. = FALSE)
  }

  actions <- vector("list", pool$n)

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
        packages = pool$packages
      )
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
        action$action <- "recycle"
        action$reason <- if (exceeds_limit) "rss_limit" else "rss_drift"
        action$rss_before <- current_rss

        pool$workers[[i]] <- worker_recycle(w, pool$init_expr, pool$packages)
        pool$workers[[i]]$rss_baseline <- worker_rss(pool$workers[[i]])
        pool$stats$total_recycles <- pool$stats$total_recycles + 1L
      } else {
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
