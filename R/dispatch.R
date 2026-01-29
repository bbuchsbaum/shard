#' @title Task Dispatch Engine
#' @description Orchestrates chunk dispatch with worker supervision and failure handling.
#' @name dispatch
NULL

#' Dispatch Chunks to Worker Pool
#'
#' Executes a function over chunks using the worker pool with supervision.
#' Handles worker death and recycling transparently by requeuing failed chunks.
#'
#' @param chunks List of chunk descriptors. Each chunk will be passed to `fun`.
#' @param fun Function to execute. Receives (chunk, ...) as arguments.
#' @param ... Additional arguments passed to `fun`.
#' @param pool A `shard_pool` object. If NULL, uses the current pool.
#' @param health_check_interval Integer. Check pool health every N chunks (default 10).
#' @param max_retries Integer. Maximum retries per chunk before permanent failure (default 3).
#' @param timeout Numeric. Seconds to wait for each chunk (default 3600).
#'
#' @return A `shard_dispatch_result` object with results and diagnostics.
#' @export
dispatch_chunks <- function(chunks, fun, ...,
                            pool = NULL,
                            health_check_interval = 10L,
                            max_retries = 3L,
                            timeout = 3600) {
  if (is.null(pool)) {
    pool <- pool_get()
  }

  if (is.null(pool)) {
    stop("No active pool. Call pool_create() first.", call. = FALSE)
  }

  # Capture additional arguments as a list
  extra_args <- list(...)

  # Create queue
  queue <- queue_create(chunks)

  # Track diagnostics
  diag <- list(
    start_time = Sys.time(),
    health_checks = list(),
    worker_assignments = list()
  )

  chunks_processed <- 0L
  worker_idx <- 1L

  # Main dispatch loop
  while (!queue_is_done(queue)) {
    # Health check at intervals
    if (chunks_processed > 0 && chunks_processed %% health_check_interval == 0) {
      health <- pool_health_check(pool)
      diag$health_checks <- c(diag$health_checks, list(health))

      # Requeue any in-flight chunks from recycled/restarted workers
      for (action in health$worker_actions) {
        if (action$action %in% c("restart", "recycle")) {
          requeued <- queue_requeue_worker(queue, action$worker_id)
          if (requeued > 0) {
            message(sprintf("Requeued %d chunks from worker %d (%s)",
                            requeued, action$worker_id, action$reason))
          }
        }
      }

      # Refresh pool reference
      pool <- pool_get()
    }

    # Get next pending chunk
    if (!queue_has_pending(queue)) {
      # No pending chunks but still have in-flight - wait briefly
      Sys.sleep(0.01)
      next
    }

    # Round-robin worker selection
    worker_id <- worker_idx
    worker_idx <- (worker_idx %% pool$n) + 1L

    # Get chunk and assign to worker
    chunk <- queue_next(queue, worker_id)
    if (is.null(chunk)) next

    # Execute chunk
    result <- tryCatch({
      # Call function with chunk and extra args using do.call
      value <- dispatch_eval_chunk(pool, worker_id, fun, chunk, extra_args, timeout)
      list(success = TRUE, value = value)
    }, error = function(e) {
      list(success = FALSE, error = conditionMessage(e))
    })

    if (result$success) {
      queue_complete(queue, chunk$id, result$value)
    } else {
      # Check retry count
      retry_count <- (chunk$retry_count %||% 0L)
      if (retry_count >= max_retries) {
        queue_fail(queue, chunk$id, error = result$error, requeue = FALSE)
        warning(sprintf("Chunk %s permanently failed after %d retries: %s",
                        chunk$id, retry_count, result$error))
      } else {
        queue_fail(queue, chunk$id, error = result$error, requeue = TRUE)
      }
    }

    chunks_processed <- chunks_processed + 1L
  }

  diag$end_time <- Sys.time()
  diag$duration <- as.numeric(difftime(diag$end_time, diag$start_time, units = "secs"))

  structure(
    list(
      results = queue_results(queue),
      failures = queue_failures(queue),
      queue_status = queue_status(queue),
      diagnostics = diag,
      pool_stats = pool_get()$stats
    ),
    class = "shard_dispatch_result"
  )
}

#' Evaluate Chunk on Worker
#'
#' Internal function to dispatch a chunk to a worker with proper argument handling.
#'
#' @param pool Worker pool.
#' @param worker_id Worker to use.
#' @param fun Function to call.
#' @param chunk Chunk descriptor.
#' @param extra_args List of additional arguments.
#' @param timeout Timeout in seconds.
#' @return Result of evaluation.
#' @keywords internal
dispatch_eval_chunk <- function(pool, worker_id, fun, chunk, extra_args, timeout) {
  w <- pool$workers[[worker_id]]

  if (is.null(w) || !worker_is_alive(w)) {
    # Restart dead worker
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

  # Export function and arguments to worker
  export_env <- new.env(parent = emptyenv())
  export_env$.shard_fun <- fun
  export_env$.shard_chunk <- chunk
  export_env$.shard_args <- extra_args

  parallel::clusterExport(w$cluster, c(".shard_fun", ".shard_chunk", ".shard_args"),
                          envir = export_env)

  # Execute on worker
  result <- parallel::clusterCall(w$cluster, function() {
    do.call(.shard_fun, c(list(.shard_chunk), .shard_args))
  })

  result[[1]]
}

#' Parallel Dispatch with Async Workers
#'
#' An alternative dispatch that uses parallel::parLapply-style execution
#' but with supervision. This is a simpler interface for basic parallel apply.
#'
#' @param X List or vector to iterate over.
#' @param FUN Function to apply to each element.
#' @param ... Additional arguments to FUN.
#' @param pool A `shard_pool` object. If NULL, uses current pool.
#' @param chunk_size Integer. Elements per chunk (default 1).
#'
#' @return A list of results.
#' @export
pool_lapply <- function(X, FUN, ..., pool = NULL, chunk_size = 1L) {
  if (is.null(pool)) {
    pool <- pool_get()
  }

  if (is.null(pool)) {
    stop("No active pool. Call pool_create() first.", call. = FALSE)
  }

  # Capture extra args
  extra_args <- list(...)

  # Create chunks with the actual function embedded
  n <- length(X)
  chunks <- lapply(seq_len(ceiling(n / chunk_size)), function(i) {
    start <- (i - 1L) * chunk_size + 1L
    end <- min(i * chunk_size, n)
    list(
      id = i,
      indices = start:end,
      elements = X[start:end],
      FUN = FUN,
      extra_args = extra_args
    )
  })

  # Wrapper function that processes a chunk
  chunk_fun <- function(chunk) {
    FUN <- chunk$FUN
    extra_args <- chunk$extra_args
    lapply(chunk$elements, function(x) do.call(FUN, c(list(x), extra_args)))
  }

  # Dispatch
  result <- dispatch_chunks(chunks, chunk_fun, pool = pool)

  # Flatten results
  unlist(result$results, recursive = FALSE)
}

#' Parallel sapply with Supervision
#'
#' @param X List or vector to iterate over.
#' @param FUN Function to apply.
#' @param ... Additional arguments to FUN.
#' @param simplify Logical. Simplify result to vector/matrix?
#' @param pool A `shard_pool` object. If NULL, uses current pool.
#'
#' @return Simplified result if possible, otherwise a list.
#' @export
pool_sapply <- function(X, FUN, ..., simplify = TRUE, pool = NULL) {
  result <- pool_lapply(X, FUN, ..., pool = pool)

  if (simplify) {
    tryCatch(
      simplify2array(result),
      error = function(e) result
    )
  } else {
    result
  }
}

#' @export
print.shard_dispatch_result <- function(x, ...) {
  cat("shard dispatch result\n")
  cat("  Duration:", sprintf("%.2f seconds", x$diagnostics$duration), "\n")

  status <- x$queue_status
  cat("  Chunks: ", status$completed, " completed",
      if (status$failed > 0) paste0(", ", status$failed, " failed") else "",
      if (status$total_retries > 0) paste0(" (", status$total_retries, " retries)") else "",
      "\n", sep = "")

  cat("  Pool stats: ", x$pool_stats$total_recycles, " recycles, ",
      x$pool_stats$total_deaths, " deaths\n", sep = "")

  if (length(x$failures) > 0) {
    cat("  Failed chunks:", paste(names(x$failures), collapse = ", "), "\n")
  }

  invisible(x)
}
