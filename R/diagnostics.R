#' Run Diagnostics Report
#'
#' Primary entry point for shard run diagnostics. Returns a comprehensive
#' report of memory usage, worker metrics, task execution, and copy events.
#'
#' @param x Shard run handle from \code{\link{shard_map}}, or NULL for
#'   current pool state
#' @param level Detail level: "summary" (default), "workers", "tasks", or "segments"
#'
#' @return An S3 object of class "shard_report"
#'
#' @details
#' Report levels:
#' \describe{
#'   \item{summary}{Workers, recycles, tasks, failures, peak/end RSS, copies, CoW}
#'   \item{workers}{Per-worker time series of memory and activity}
#'   \item{tasks}{Per-task execution metrics}
#'   \item{segments}{Shared segment allocation and usage details}
#' }
#'
#' @examples
#' \dontrun{
#' run <- shard_map(...)
#' print(report(run))
#' print(report(run, level = "workers"))
#' }
#'
#' @seealso \code{\link{mem_report}}, \code{\link{cow_report}},
#'   \code{\link{copy_report}}, \code{\link{task_report}},
#'   \code{\link{segment_report}}
#' @export
report <- function(x = NULL, level = c("summary", "workers", "tasks", "segments")) {
  level <- match.arg(level)

  # TODO: Implement report extraction from run handle
  .NotYetImplemented()
}

#' @export
print.shard_report <- function(x, ...) {
  cat("=== shard run report ===\n\n")

  if (!is.null(x$summary)) {
    cat("Workers:", x$summary$workers, "\n")
    cat("Recycles:", x$summary$recycles, "\n")
    cat("Tasks:", x$summary$tasks_completed, "/", x$summary$tasks_total, "\n")
    cat("Failures:", x$summary$failures, "\n")
    cat("\n")
    cat("Peak RSS:", format_bytes(x$summary$peak_rss), "\n")
    cat("End RSS:", format_bytes(x$summary$end_rss), "\n")
    cat("Materialized:", format_bytes(x$summary$materialized_bytes), "\n")
    cat("CoW violations:", x$summary$cow_violations, "\n")
  }


  invisible(x)
}

#' Memory Metrics Report
#'
#' Returns a data frame of memory metrics from a shard run.
#'
#' @param x Shard run handle
#' @return Data frame with memory metrics
#' @seealso \code{\link{report}}
#' @export
mem_report <- function(x) {
  # TODO: Extract memory metrics from run handle
  .NotYetImplemented()
}

#' Copy-on-Write Event Report
#'
#' Returns a data frame of copy-on-write events detected during a shard run.
#'
#' @param x Shard run handle
#' @return Data frame with CoW events
#' @seealso \code{\link{report}}
#' @export
cow_report <- function(x) {
  # TODO: Extract CoW events from run handle
  .NotYetImplemented()
}

#' Copy/Materialization Event Report
#'
#' Returns a data frame of data materialization and copy events.
#' The key metric is \code{materialized_bytes}: zero indicates true zero-copy.
#'
#' @param x Shard run handle
#' @return Data frame with copy metrics per shared segment
#' @seealso \code{\link{report}}, \code{\link{share}}
#' @export
copy_report <- function(x) {
  # TODO: Extract copy metrics from run handle
  .NotYetImplemented()
}

#' Per-Task Metrics Report
#'
#' Returns a data frame of per-task execution metrics.
#'
#' @param x Shard run handle
#' @return Data frame with columns: task_id, worker_id, duration_ms,
#'   result_bytes, rss_delta, retries
#' @seealso \code{\link{report}}
#' @export
task_report <- function(x) {
  # TODO: Extract task metrics from run handle
  .NotYetImplemented()
}

#' Shared Segment Metrics Report
#'
#' Returns a data frame of shared segment allocation and usage.
#'
#' @param x Shard run handle
#' @return Data frame with segment details
#' @seealso \code{\link{report}}, \code{\link{share}}, \code{\link{buffer}}
#' @export
segment_report <- function(x) {
  # TODO: Extract segment metrics from run handle
  .NotYetImplemented()
}

# Internal helper for byte formatting
format_bytes <- function(bytes) {
  if (is.null(bytes) || is.na(bytes)) return("NA")

  units <- c("B", "KB", "MB", "GB", "TB")
  power <- min(floor(log(bytes + 1, 1024)), length(units) - 1)
  value <- bytes / (1024^power)
  sprintf("%.1f %s", value, units[power + 1])
}
