#' @title RSS Monitoring Utilities
#' @description Cross-platform utilities for monitoring process memory usage.
#' @name rss
NULL

#' Get RSS for a Process ID
#'
#' Retrieves the Resident Set Size (RSS) for a process using the best
#' available method for the current platform.
#'
#' @param pid Integer. Process ID to query.
#' @return Numeric. RSS in bytes, or NA if unavailable.
#' @keywords internal
rss_get_pid <- function(pid) {
  if (is.na(pid)) {
    return(NA_real_)
  }


  # Try ps package first (most reliable, cross-platform)
  if (requireNamespace("ps", quietly = TRUE)) {
    rss <- rss_via_ps(pid)
    if (!is.na(rss)) return(rss)
  }

  # Platform-specific fallbacks
  os <- .Platform$OS.type
  sysname <- Sys.info()[["sysname"]]

  if (sysname == "Linux") {
    rss <- rss_via_proc(pid)
    if (!is.na(rss)) return(rss)
  }

  if (sysname %in% c("Darwin", "Linux")) {
    rss <- rss_via_ps_cmd(pid)
    if (!is.na(rss)) return(rss)
  }

  if (os == "windows") {
    rss <- rss_via_wmic(pid)
    if (!is.na(rss)) return(rss)
  }

  NA_real_
}

#' Get RSS via ps Package
#'
#' @param pid Process ID.
#' @return RSS in bytes or NA.
#' @keywords internal
rss_via_ps <- function(pid) {
  tryCatch({
    p <- ps::ps_handle(pid)
    if (!ps::ps_is_running(p)) {
      return(NA_real_)
    }
    mem <- ps::ps_memory_info(p)
    as.numeric(mem[["rss"]])
  }, error = function(e) NA_real_)
}

#' Get RSS via /proc filesystem (Linux)
#'
#' @param pid Process ID.
#' @return RSS in bytes or NA.
#' @keywords internal
rss_via_proc <- function(pid) {
  statm_path <- file.path("/proc", pid, "statm")

  if (!file.exists(statm_path)) {
    return(NA_real_)
  }

  tryCatch({
    # statm format: size resident shared text lib data dt
    # All values in pages
    statm <- scan(statm_path, what = integer(), quiet = TRUE, n = 2)
    if (length(statm) < 2) return(NA_real_)

    # Get page size
    page_size <- tryCatch(
      as.integer(system("getconf PAGESIZE", intern = TRUE)),
      error = function(e) 4096L  # Default to 4KB
    )

    as.numeric(statm[2]) * page_size
  }, error = function(e) NA_real_)
}

#' Get RSS via ps Command (Unix)
#'
#' @param pid Process ID.
#' @return RSS in bytes or NA.
#' @keywords internal
rss_via_ps_cmd <- function(pid) {
  tryCatch({
    # ps -o rss= gives RSS in KB
    output <- withCallingHandlers(
      system2(
        "ps",
        args = c("-o", "rss=", "-p", as.character(pid)),
        stdout = TRUE,
        stderr = NULL
      ),
      warning = function(w) invokeRestart("muffleWarning")
    )
    status <- attr(output, "status", exact = TRUE)
    if (!is.null(status) && !identical(as.integer(status), 0L)) {
      return(NA_real_)
    }
    if (length(output) == 0 || nchar(trimws(output[1])) == 0) {
      return(NA_real_)
    }
    rss_kb <- as.numeric(trimws(output[1]))
    if (is.na(rss_kb)) return(NA_real_)
    rss_kb * 1024  # Convert to bytes
  }, error = function(e) NA_real_)
}

#' Get RSS via WMIC (Windows)
#'
#' @param pid Process ID.
#' @return RSS in bytes or NA.
#' @keywords internal
rss_via_wmic <- function(pid) {
  tryCatch({
    cmd <- sprintf("wmic process where ProcessId=%d get WorkingSetSize /value", pid)
    output <- shell(cmd, intern = TRUE, mustWork = FALSE)
    # Parse WorkingSetSize=NNNN
    ws_line <- grep("WorkingSetSize=", output, value = TRUE)
    if (length(ws_line) == 0) return(NA_real_)
    ws <- sub("WorkingSetSize=", "", ws_line[1])
    as.numeric(trimws(ws))
  }, error = function(e) NA_real_)
}

#' Get Current Process RSS
#'
#' @return RSS in bytes for the current R process.
#' @keywords internal
rss_self <- function() {
  rss_get_pid(Sys.getpid())
}

#' Monitor RSS Over Time
#'
#' Creates a closure that records RSS samples with timestamps.
#'
#' @param pid Process ID to monitor.
#' @return A list with `sample()` and `history()` functions.
#' @keywords internal
rss_monitor <- function(pid) {
  samples <- list()

  list(
    sample = function() {
      rss <- rss_get_pid(pid)
      entry <- list(timestamp = Sys.time(), rss = rss)
      samples <<- c(samples, list(entry))
      entry
    },
    history = function() {
      if (length(samples) == 0) {
        return(data.frame(
          timestamp = as.POSIXct(character(0)),
          rss = numeric(0)
        ))
      }
      data.frame(
        timestamp = do.call(c, lapply(samples, `[[`, "timestamp")),
        rss = vapply(samples, `[[`, numeric(1), "rss")
      )
    },
    peak = function() {
      if (length(samples) == 0) return(NA_real_)
      max(vapply(samples, `[[`, numeric(1), "rss"), na.rm = TRUE)
    },
    drift = function() {
      if (length(samples) < 2) return(NA_real_)
      rss_vals <- vapply(samples, `[[`, numeric(1), "rss")
      baseline <- rss_vals[1]
      current <- rss_vals[length(rss_vals)]
      if (is.na(baseline) || baseline == 0) return(NA_real_)
      (current - baseline) / baseline
    }
  )
}
