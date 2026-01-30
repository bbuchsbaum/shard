#' @title Diagnostics API
#' @name diagnostics
#' @description Comprehensive diagnostics for shard parallel execution, providing
#'   insights into memory usage, worker status, task execution, and shared memory
#'   segments.
#'
#' @details
#' The diagnostics API provides multiple views into shard's runtime behavior:
#' \itemize{
#'   \item \code{report()}: Primary entry point with configurable detail levels
#'   \item \code{mem_report()}: Memory usage across workers
#'   \item \code{cow_report()}: Copy-on-write policy tracking
#'   \item \code{copy_report()}: Data transfer statistics
#'   \item \code{task_report()}: Task/chunk execution statistics
#'   \item \code{segment_report()}: Shared memory segment information
#' }
#'
#' All functions return S3 \code{shard_report} objects with appropriate print
#' methods for human-readable output.
NULL

#' Generate Shard Runtime Report
#'
#' Primary entry point for shard diagnostics. Generates a comprehensive report
#' of the current runtime state including pool status, memory usage, and
#' execution statistics.
#'
#' @param level Character. Detail level for the report:
#'   \itemize{
#'     \item \code{"summary"}: High-level overview (default)
#'     \item \code{"workers"}: Include per-worker details
#'     \item \code{"tasks"}: Include task execution history
#'     \item \code{"segments"}: Include shared memory segment details
#'   }
#' @param result Optional. A \code{shard_result} object from \code{\link{shard_map}}
#'   to include execution diagnostics from.
#'
#' @return An S3 object of class \code{shard_report} containing:
#'   \itemize{
#'     \item \code{level}: The requested detail level
#'     \item \code{timestamp}: When the report was generated
#'     \item \code{pool}: Pool status information (if pool exists)
#'     \item \code{memory}: Memory usage summary
#'     \item \code{workers}: Per-worker details (if level includes workers)
#'     \item \code{tasks}: Task execution details (if level includes tasks)
#'     \item \code{segments}: Segment details (if level includes segments)
#'     \item \code{result_diagnostics}: Diagnostics from shard_result (if provided)
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic summary
#' report()
#'
#' # Include worker details
#' report("workers")
#'
#' # Full report with task and segment details
#' report("segments")
#'
#' # Include diagnostics from a shard_map result
#' result <- shard_map(shards(100), function(s) sum(s$idx))
#' report("tasks", result = result)
#' }
report <- function(level = c("summary", "workers", "tasks", "segments"),
                   result = NULL) {
  level <- match.arg(level)

  # Build report based on level
  rpt <- list(
    level = level,
    timestamp = Sys.time(),
    pool = NULL,
    memory = NULL,
    workers = NULL,
    tasks = NULL,
    segments = NULL,
    result_diagnostics = NULL
  )

  # Get pool information
  pool <- pool_get()
  if (!is.null(pool)) {
    rpt$pool <- list(
      n_workers = pool$n,
      rss_limit = pool$rss_limit_bytes,
      drift_threshold = pool$rss_drift_threshold,
      created_at = pool$created_at,
      stats = pool$stats
    )
  }

  # Memory summary (always included)
  rpt$memory <- mem_report()

  # Worker details (for workers level and above)
  if (level %in% c("workers", "tasks", "segments")) {
    if (!is.null(pool)) {
      rpt$workers <- lapply(seq_len(pool$n), function(i) {
        w <- pool$workers[[i]]
        if (is.null(w)) {
          return(list(id = i, status = "missing"))
        }
        worker_metrics(w)
      })
    }
  }

  # Task details (for tasks level and above)
  if (level %in% c("tasks", "segments")) {
    if (!is.null(result) && inherits(result, "shard_result")) {
      rpt$tasks <- task_report(result)
      rpt$result_diagnostics <- result$diagnostics
    }
  }

  # Performance recommendations (only when a result is provided).
  if (!is.null(result) && inherits(result, "shard_result")) {
    rpt$recommendations <- recommendations(result)
  }

  # Segment details (for segments level only)
  if (level == "segments") {
    rpt$segments <- segment_report()
  }

  structure(rpt, class = "shard_report")
}

#' Performance Recommendations
#'
#' Uses run telemetry (copy/materialization stats, packing volume, buffer/table
#' writes, scratch pool stats) to produce actionable recommendations.
#'
#' @param result A `shard_result` from [shard_map()].
#' @return A character vector of recommendations (possibly empty).
#' @export
recommendations <- function(result) {
  if (is.null(result) || !inherits(result, "shard_result")) {
    stop("result must be a shard_result", call. = FALSE)
  }

  recs <- character(0)

  cr <- tryCatch(copy_report(result), error = function(e) NULL)
  if (is.list(cr)) {
    mb <- as.double(cr$view_materialized_bytes %||% 0)
    pb <- as.double(cr$view_packed_bytes %||% 0)
    if (mb > 0) {
      recs <- c(recs, sprintf(
        "Views were materialized (%.1f MB). Prefer view_block()/view_gather() and shard-aware kernels to avoid slice copies.",
        mb / 1024^2
      ))
    }
    if (pb > 0) {
      recs <- c(recs, sprintf(
        "Views required packing (%.1f MB). Prefer contiguous/block shards when possible, or use gather-aware kernels to keep packing bounded.",
        pb / 1024^2
      ))
    }
    bb <- as.double(cr$buffer_bytes %||% 0)
    tb <- as.double(cr$table_bytes %||% 0)
    if (bb == 0 && tb == 0 && (mb > 0 || pb > 0)) {
      recs <- c(recs, "Consider writing large outputs into explicit buffers (buffer()/table_buffer()/table_sink()) to avoid master-side gather/concat.")
    }
  }

  d <- result$diagnostics %||% list()

  # Dispatch overhead guidance (tiny tasks).
  if (is.list(d)) {
    dm <- as.character(d$dispatch_mode %||% "rpc_chunked")
    shards_done <- as.double(d$shards_processed %||% NA_real_)
    chunks_done <- as.double(d$chunks_dispatched %||% NA_real_)
    dur <- as.double(d$duration %||% NA_real_)

    # Heuristic: 1 shard per chunk + very small per-shard time means RPC overhead
    # likely dominates.
    if (identical(dm, "rpc_chunked") &&
        is.finite(shards_done) && is.finite(chunks_done) && is.finite(dur) &&
        shards_done > 0 && chunks_done > 0 &&
        chunks_done == shards_done) {
      mean_shard_time <- dur / shards_done
      if (is.finite(mean_shard_time) && mean_shard_time > 0 && mean_shard_time < 0.02) {
        recs <- c(recs, sprintf(
          "Per-shard work looks tiny (%.3f ms/shard). Consider increasing chunk_size, or use profile='speed' / dispatch_mode='shm_queue' for out-buffer workflows to reduce dispatch overhead.",
          mean_shard_time * 1000
        ))
      }
    }

    # shm_queue mode returns a placeholder list of length n_tasks; for extremely
    # large n_tasks, recommend increasing block_size to reduce bookkeeping.
    if (identical(dm, "shm_queue") && is.list(d$shm_queue)) {
      nt <- as.double(d$shm_queue$n_tasks %||% NA_real_)
      if (is.finite(nt) && nt > 1e6) {
        recs <- c(recs, sprintf(
          "shm_queue is tracking %.0f tasks. If bookkeeping dominates, increase dispatch_opts$block_size to reduce n_tasks (you can keep work-per-task small without going all the way to 1).",
          nt
        ))
      }
    }
  }

  if (is.list(d$scratch_stats)) {
    hw <- as.double(d$scratch_stats$high_water %||% 0)
    if (hw > 0) {
      recs <- c(recs, sprintf(
        "Scratch pool high-water mark: %.1f MB. If allocations churn, consider using scratch_matrix() (and set scratch_pool_config(max_bytes=...)).",
        hw / 1024^2
      ))
    }
  }

  if (is.list(d$scheduler) && is.finite(as.double(d$scheduler$throttle_events %||% 0)) &&
      as.double(d$scheduler$throttle_events %||% 0) > 0) {
    recs <- c(recs, sprintf(
      "Scheduler throttled heavy chunks %d times. Consider tuning scheduler_policy (e.g., max_huge_concurrency) or reducing per-shard footprint.",
      as.integer(d$scheduler$throttle_events)
    ))
  }

  unique(recs)
}

#' Memory Usage Report
#'
#' Generates a report of memory usage across all workers in the pool.
#'
#' @param pool Optional. A \code{shard_pool} object. If NULL, uses the current pool.
#'
#' @return An S3 object of class \code{shard_report} with type \code{"memory"}
#'   containing:
#'   \itemize{
#'     \item \code{type}: "memory"
#'     \item \code{timestamp}: When the report was generated
#'     \item \code{pool_active}: Whether a pool exists
#'     \item \code{n_workers}: Number of workers
#'     \item \code{rss_limit}: RSS limit per worker (bytes)
#'     \item \code{total_rss}: Sum of RSS across all workers
#'     \item \code{peak_rss}: Highest RSS among workers
#'     \item \code{mean_rss}: Mean RSS across workers
#'     \item \code{workers}: Per-worker RSS details
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' pool_create(4)
#' mem_report()
#' }
mem_report <- function(pool = NULL) {
  if (is.null(pool)) {
    pool <- pool_get()
  }

  rpt <- list(
    type = "memory",
    timestamp = Sys.time(),
    pool_active = !is.null(pool),
    n_workers = 0L,
    rss_limit = NA_real_,
    total_rss = 0,
    peak_rss = NA_real_,
    mean_rss = NA_real_,
    workers = list()
  )

  if (is.null(pool)) {
    return(structure(rpt, class = "shard_report"))
  }

  rpt$n_workers <- pool$n
  rpt$rss_limit <- pool$rss_limit_bytes

  # Collect RSS for each worker
  worker_stats <- lapply(seq_len(pool$n), function(i) {
    w <- pool$workers[[i]]
    if (is.null(w)) {
      return(list(
        id = i,
        pid = NA_integer_,
        status = "missing",
        rss = NA_real_,
        rss_baseline = NA_real_,
        drift = NA_real_
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

    list(
      id = i,
      pid = w$pid,
      status = if (alive) "alive" else "dead",
      rss = rss,
      rss_baseline = baseline,
      drift = drift,
      recycle_count = w$recycle_count %||% 0L
    )
  })

  rpt$workers <- worker_stats

  # Compute aggregates
  rss_values <- vapply(worker_stats, function(x) x$rss %||% NA_real_, numeric(1))
  rss_valid <- rss_values[!is.na(rss_values)]

  if (length(rss_valid) > 0) {
    rpt$total_rss <- sum(rss_valid)
    rpt$peak_rss <- max(rss_valid)
    rpt$mean_rss <- mean(rss_valid)
  }

  structure(rpt, class = "shard_report")
}

#' Copy-on-Write Policy Report
#'
#' Generates a report of copy-on-write behavior for borrowed inputs.
#'
#' @param result Optional. A \code{shard_result} object to extract COW stats from.
#'
#' @return An S3 object of class \code{shard_report} with type \code{"cow"}
#'   containing:
#'   \itemize{
#'     \item \code{type}: "cow"
#'     \item \code{timestamp}: When the report was generated
#'     \item \code{policy}: The COW policy used ("deny", "audit", "allow")
#'     \item \code{violations}: Count of COW violations detected (audit mode)
#'     \item \code{copies_triggered}: Estimated copies triggered by mutations
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' result <- shard_map(shards(10), function(s) s$idx, cow = "audit")
#' cow_report(result)
#' }
cow_report <- function(result = NULL) {
  rpt <- list(
    type = "cow",
    timestamp = Sys.time(),
    policy = NA_character_,
    violations = 0L,
    copies_triggered = 0L,
    details = list()
  )

  if (!is.null(result) && inherits(result, "shard_result")) {
    rpt$policy <- result$cow_policy %||% NA_character_

    # Extract COW diagnostics if available
    if (!is.null(result$diagnostics) && !is.null(result$diagnostics$cow_stats)) {
      cow_stats <- result$diagnostics$cow_stats
      rpt$violations <- cow_stats$violations %||% 0L
      rpt$copies_triggered <- cow_stats$copies %||% 0L
      rpt$details <- cow_stats$details %||% list()
    }
  }

  structure(rpt, class = "shard_report")
}

#' Data Copy Report
#'
#' Generates a report of data transfer and copy statistics during parallel
#' execution.
#'
#' @param result Optional. A \code{shard_result} object to extract copy stats from.
#'
#' @return An S3 object of class \code{shard_report} with type \code{"copy"}
#'   containing:
#'   \itemize{
#'     \item \code{type}: "copy"
#'     \item \code{timestamp}: When the report was generated
#'     \item \code{borrow_exports}: Number of borrowed input exports
#'     \item \code{borrow_bytes}: Total bytes in borrowed inputs
#'     \item \code{result_imports}: Number of result imports
#'     \item \code{result_bytes}: Estimated bytes in results
#'     \item \code{buffer_writes}: Number of buffer write operations
#'     \item \code{buffer_bytes}: Total bytes written to buffers
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' X <- matrix(rnorm(1e4), 100, 100)
#' result <- shard_map(shards(10), function(s, X) sum(X[, s$idx]),
#'                     borrow = list(X = X))
#' copy_report(result)
#' }
copy_report <- function(result = NULL) {
  rpt <- list(
    type = "copy",
    timestamp = Sys.time(),
    borrow_exports = 0L,
    borrow_bytes = 0,
    result_imports = 0L,
    result_bytes = 0,
    buffer_writes = 0L,
    buffer_bytes = 0,
    table_writes = 0L,
    table_rows = 0L,
    table_bytes = 0,
    view_created = 0L,
    view_materialized = 0L,
    view_materialized_bytes = 0,
    view_packed = 0L,
    view_packed_bytes = 0
  )

  if (!is.null(result) && inherits(result, "shard_result")) {
    diag <- result$diagnostics

    # Count results
    if (!is.null(result$results)) {
      rpt$result_imports <- length(result$results)
      # Estimate result size
      rpt$result_bytes <- tryCatch(
        as.numeric(utils::object.size(result$results)),
        error = function(e) 0
      )
    }

    # Extract copy diagnostics if available
    if (!is.null(diag) && !is.null(diag$copy_stats)) {
      copy_stats <- diag$copy_stats
      rpt$borrow_exports <- copy_stats$borrow_exports %||% 0L
      rpt$borrow_bytes <- copy_stats$borrow_bytes %||% 0
      rpt$buffer_writes <- copy_stats$buffer_writes %||% 0L
      rpt$buffer_bytes <- copy_stats$buffer_bytes %||% 0
    }

    if (!is.null(diag) && !is.null(diag$table_stats)) {
      ts <- diag$table_stats
      rpt$table_writes <- ts$writes %||% 0L
      rpt$table_rows <- ts$rows %||% 0L
      rpt$table_bytes <- ts$bytes %||% 0
    }
  }

  # Prefer run-level view stats when present (aggregated from workers).
  if (!is.null(result) && inherits(result, "shard_result")) {
    diag <- result$diagnostics
    if (!is.null(diag) && !is.null(diag$view_stats)) {
      vs <- diag$view_stats
      rpt$view_created <- vs$created %||% 0L
      rpt$view_materialized <- vs$materialized %||% 0L
      rpt$view_materialized_bytes <- vs$materialized_bytes %||% 0
      rpt$view_packed <- vs$packed %||% 0L
      rpt$view_packed_bytes <- vs$packed_bytes %||% 0
    }
  }

  if (is.null(rpt$view_created) || is.null(rpt$view_materialized)) {
    # Fallback to global view diagnostics when run-level aggregation isn't available.
    vd <- tryCatch(view_diagnostics(), error = function(e) NULL)
    if (is.list(vd)) {
      rpt$view_created <- vd$created %||% 0L
      rpt$view_materialized <- vd$materialized %||% 0L
      rpt$view_materialized_bytes <- vd$materialized_bytes %||% 0
      rpt$view_packed <- vd$packed %||% 0L
      rpt$view_packed_bytes <- vd$packed_bytes %||% 0
    }
  }

  structure(rpt, class = "shard_report")
}

#' Task Execution Report
#'
#' Generates a report of task/chunk execution statistics from a shard_map result.
#'
#' @param result A \code{shard_result} object from \code{\link{shard_map}}.
#'
#' @return An S3 object of class \code{shard_report} with type \code{"task"}
#'   containing:
#'   \itemize{
#'     \item \code{type}: "task"
#'     \item \code{timestamp}: When the report was generated
#'     \item \code{shards_total}: Total number of shards
#'     \item \code{shards_processed}: Number of shards successfully processed
#'     \item \code{shards_failed}: Number of permanently failed shards
#'     \item \code{chunks_dispatched}: Number of chunk batches dispatched
#'     \item \code{total_retries}: Total number of retry attempts
#'     \item \code{duration}: Total execution duration (seconds)
#'     \item \code{throughput}: Shards processed per second
#'     \item \code{queue_status}: Final queue status
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' result <- shard_map(shards(100), function(s) sum(s$idx))
#' task_report(result)
#' }
task_report <- function(result = NULL) {
  rpt <- list(
    type = "task",
    timestamp = Sys.time(),
    shards_total = 0L,
    shards_processed = 0L,
    shards_failed = 0L,
    chunks_dispatched = 0L,
    total_retries = 0L,
    duration = NA_real_,
    throughput = NA_real_,
    queue_status = NULL,
    health_checks = list()
  )

  if (is.null(result) || !inherits(result, "shard_result")) {
    return(structure(rpt, class = "shard_report"))
  }

  # Extract from shards descriptor
  if (!is.null(result$shards)) {
    rpt$shards_total <- result$shards$num_shards %||% 0L
  }

  # Extract from diagnostics
  if (!is.null(result$diagnostics)) {
    diag <- result$diagnostics
    rpt$shards_processed <- diag$shards_processed %||% 0L
    rpt$chunks_dispatched <- diag$chunks_dispatched %||% 0L
    rpt$duration <- diag$duration %||% NA_real_
    rpt$health_checks <- diag$health_checks %||% list()
  }

  # Extract from queue status
  if (!is.null(result$queue_status)) {
    qs <- result$queue_status
    rpt$queue_status <- qs
    rpt$shards_failed <- qs$failed %||% 0L
    rpt$total_retries <- qs$total_retries %||% 0L
  }

  # Extract from failures
  if (!is.null(result$failures)) {
    rpt$shards_failed <- max(rpt$shards_failed, length(result$failures))
  }

  # Compute throughput
  if (!is.na(rpt$duration) && rpt$duration > 0 && rpt$shards_processed > 0) {
    rpt$throughput <- rpt$shards_processed / rpt$duration
  }

  structure(rpt, class = "shard_report")
}

#' Shared Memory Segment Report
#'
#' Generates a report of active shared memory segments in the current session.
#'
#' @return An S3 object of class \code{shard_report} with type \code{"segment"}
#'   containing:
#'   \itemize{
#'     \item \code{type}: "segment"
#'     \item \code{timestamp}: When the report was generated
#'     \item \code{n_segments}: Number of tracked segments
#'     \item \code{total_bytes}: Total bytes across all segments
#'     \item \code{segments}: List of segment details
#'   }
#'
#' @details
#' This function reports on segments that are currently accessible. Note that
#' segments are automatically cleaned up when their R objects are garbage
#' collected, so this only shows segments with live references.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create some segments
#' seg1 <- segment_create(1024)
#' seg2 <- segment_create(2048)
#' shared <- share(1:1000)
#'
#' segment_report()
#' }
segment_report <- function() {
  rpt <- list(
    type = "segment",
    timestamp = Sys.time(),
    n_segments = 0L,
    total_bytes = 0,
    segments = list(),
    backing_summary = list()
  )

  # Get available backings
  available <- tryCatch(
    available_backings(),
    error = function(e) c("mmap")
  )
  rpt$backing_summary$available <- available

  # Note: We can't enumerate all segments because they're tracked by

# individual R objects. This provides platform information instead.
  rpt$backing_summary$platform <- .Platform$OS.type
  rpt$backing_summary$is_windows <- tryCatch(
    is_windows(),
    error = function(e) .Platform$OS.type == "windows"
  )

  structure(rpt, class = "shard_report")
}

#' @export
print.shard_report <- function(x, ...) {
  # Dispatch based on report type
  if (!is.null(x$type)) {
    switch(x$type,
      "memory" = print_mem_report(x),
      "cow" = print_cow_report(x),
      "copy" = print_copy_report(x),
      "task" = print_task_report(x),
      "segment" = print_segment_report(x),
      print_main_report(x)
    )
  } else {
    print_main_report(x)
  }

  invisible(x)
}

#' Print Main Report
#' @param x A shard_report object.
#' @keywords internal
print_main_report <- function(x) {
  cat("shard_report (", x$level, ")\n", sep = "")
  cat("Generated:", format(x$timestamp), "\n")

  # Pool summary
  if (!is.null(x$pool)) {
    cat("\nPool:\n")
    cat("  Workers:", x$pool$n_workers, "\n")
    cat("  RSS limit:", format_bytes(x$pool$rss_limit), "\n")
    cat("  Drift threshold:", sprintf("%.0f%%", x$pool$drift_threshold * 100), "\n")
    if (!is.null(x$pool$stats)) {
      cat("  Stats: ", x$pool$stats$total_tasks, " tasks, ",
          x$pool$stats$total_recycles, " recycles, ",
          x$pool$stats$total_deaths, " deaths\n", sep = "")
    }
  } else {
    cat("\nPool: (not active)\n")
  }

  # Memory summary
  if (!is.null(x$memory) && inherits(x$memory, "shard_report")) {
    cat("\nMemory:\n")
    if (x$memory$pool_active) {
      cat("  Total RSS:", format_bytes(x$memory$total_rss), "\n")
      cat("  Peak RSS:", format_bytes(x$memory$peak_rss), "\n")
      cat("  Mean RSS:", format_bytes(x$memory$mean_rss), "\n")
    } else {
      cat("  (no pool active)\n")
    }
  }

  # Worker details (if included)
  if (!is.null(x$workers) && length(x$workers) > 0) {
    cat("\nWorkers:\n")
    for (w in x$workers) {
      status_icon <- switch(w$status %||% "unknown",
        "alive" = "+",
        "ok" = "+",
        "dead" = "x",
        "missing" = "?",
        "?"
      )
      cat(sprintf("  [%s] %d: pid=%s, rss=%s, drift=%.1f%%, recycles=%d\n",
        status_icon,
        w$id %||% w$worker_id %||% NA,
        w$pid %||% "NA",
        format_bytes(w$rss %||% w$rss_bytes %||% NA_real_),
        (w$drift %||% 0) * 100,
        w$recycle_count %||% 0
      ))
    }
  }

  # Task details (if included)
  if (!is.null(x$tasks) && inherits(x$tasks, "shard_report")) {
    cat("\nTasks:\n")
    cat("  Total:", x$tasks$shards_total, "\n")
    cat("  Processed:", x$tasks$shards_processed, "\n")
    if (x$tasks$shards_failed > 0) {
      cat("  Failed:", x$tasks$shards_failed, "\n")
    }
    if (!is.na(x$tasks$duration)) {
      cat("  Duration:", sprintf("%.2f seconds", x$tasks$duration), "\n")
    }
    if (!is.na(x$tasks$throughput)) {
      cat("  Throughput:", sprintf("%.1f shards/sec", x$tasks$throughput), "\n")
    }
  }

  # Segment details (if included)
  if (!is.null(x$segments) && inherits(x$segments, "shard_report")) {
    cat("\nSegments:\n")
    cat("  Available backings:", paste(x$segments$backing_summary$available, collapse = ", "), "\n")
    cat("  Platform:", x$segments$backing_summary$platform, "\n")
  }

  if (!is.null(x$recommendations) && length(x$recommendations) > 0) {
    cat("\nRecommendations:\n")
    for (r in x$recommendations) {
      cat("  - ", r, "\n", sep = "")
    }
  }
}

#' Print Memory Report
#' @param x A shard_report object with type "memory".
#' @keywords internal
print_mem_report <- function(x) {
  cat("shard memory report\n")
  cat("Generated:", format(x$timestamp), "\n")

  if (!x$pool_active) {
    cat("\nNo pool active.\n")
    return(invisible(x))
  }

  cat("\nPool: ", x$n_workers, " workers\n", sep = "")
  cat("RSS limit:", format_bytes(x$rss_limit), "\n")

  cat("\nAggregate:\n")
  cat("  Total:", format_bytes(x$total_rss), "\n")
  cat("  Peak:", format_bytes(x$peak_rss), "\n")
  cat("  Mean:", format_bytes(x$mean_rss), "\n")

  cat("\nPer-worker:\n")
  for (w in x$workers) {
    status_icon <- switch(w$status %||% "unknown",
      "alive" = "+",
      "ok" = "+",
      "dead" = "x",
      "missing" = "?",
      "?"
    )
    cat(sprintf("  [%s] %d: rss=%s, baseline=%s, drift=%.1f%%\n",
      status_icon,
      w$id,
      format_bytes(w$rss %||% NA_real_),
      format_bytes(w$rss_baseline %||% NA_real_),
      (w$drift %||% 0) * 100
    ))
  }
}

#' Print COW Report
#' @param x A shard_report object with type "cow".
#' @keywords internal
print_cow_report <- function(x) {
  cat("shard copy-on-write report\n")
  cat("Generated:", format(x$timestamp), "\n")

  cat("\nPolicy:", x$policy %||% "(unknown)", "\n")
  cat("Violations:", x$violations, "\n")
  cat("Copies triggered:", x$copies_triggered, "\n")

  if (length(x$details) > 0) {
    cat("\nDetails:\n")
    for (d in x$details) {
      cat("  -", d, "\n")
    }
  }
}

#' Print Copy Report
#' @param x A shard_report object with type "copy".
#' @keywords internal
print_copy_report <- function(x) {
  cat("shard data copy report\n")
  cat("Generated:", format(x$timestamp), "\n")

  cat("\nBorrowed inputs:\n")
  cat("  Exports:", x$borrow_exports, "\n")
  cat("  Bytes:", format_bytes(x$borrow_bytes), "\n")

  cat("\nResults:\n")
  cat("  Imports:", x$result_imports, "\n")
  cat("  Bytes:", format_bytes(x$result_bytes), "\n")

  cat("\nBuffers:\n")
  cat("  Writes:", x$buffer_writes, "\n")
  cat("  Bytes:", format_bytes(x$buffer_bytes), "\n")

  if (!is.null(x$table_writes) || !is.null(x$table_rows)) {
    cat("\nTables:\n")
    cat("  Writes:", x$table_writes %||% 0L, "\n")
    cat("  Rows:", x$table_rows %||% 0L, "\n")
    cat("  Bytes:", format_bytes(x$table_bytes %||% 0), "\n")
  }

  if (!is.null(x$view_created) || !is.null(x$view_materialized)) {
    cat("\nViews:\n")
    cat("  Created:", x$view_created %||% 0L, "\n")
    cat("  Materialized:", x$view_materialized %||% 0L, "\n")
    cat("  Materialized bytes:", format_bytes(x$view_materialized_bytes %||% 0), "\n")
    if (!is.null(x$view_packed) || !is.null(x$view_packed_bytes)) {
      cat("  Packed:", x$view_packed %||% 0L, "\n")
      cat("  Packed bytes:", format_bytes(x$view_packed_bytes %||% 0), "\n")
    }
  }
}

#' Print Task Report
#' @param x A shard_report object with type "task".
#' @keywords internal
print_task_report <- function(x) {
  cat("shard task report\n")
  cat("Generated:", format(x$timestamp), "\n")

  cat("\nExecution:\n")
  cat("  Total shards:", x$shards_total, "\n")
  cat("  Processed:", x$shards_processed, "\n")
  cat("  Failed:", x$shards_failed, "\n")
  cat("  Chunks dispatched:", x$chunks_dispatched, "\n")

  if (x$total_retries > 0) {
    cat("  Retries:", x$total_retries, "\n")
  }

  cat("\nTiming:\n")
  if (!is.na(x$duration)) {
    cat("  Duration:", sprintf("%.2f seconds", x$duration), "\n")
  }
  if (!is.na(x$throughput)) {
    cat("  Throughput:", sprintf("%.1f shards/sec", x$throughput), "\n")
  }

  if (length(x$health_checks) > 0) {
    cat("\nHealth checks:", length(x$health_checks), "performed\n")
  }
}

#' Print Segment Report
#' @param x A shard_report object with type "segment".
#' @keywords internal
print_segment_report <- function(x) {
  cat("shard segment report\n")
  cat("Generated:", format(x$timestamp), "\n")

  cat("\nBacking types:\n")
  cat("  Available:", paste(x$backing_summary$available, collapse = ", "), "\n")
  cat("  Platform:", x$backing_summary$platform, "\n")
  cat("  Windows:", x$backing_summary$is_windows, "\n")
}
