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
#' @param scheduler_policy Optional list of scheduling hints (advanced). Currently:
#'   - `max_huge_concurrency`: cap concurrent chunks with `footprint_class=="huge"`.
#' @param on_result Optional callback (advanced). If provided, called on the
#'   master process as `on_result(tag, value, worker_id)` for each successful
#'   chunk completion. Used by [shard_reduce()] to stream reductions.
#' @param store_results Logical (advanced). If FALSE, successful chunk values are
#'   not retained in the returned `results` list (streaming use cases).
#' @param retain_chunks Logical (advanced). If FALSE, completed chunk descriptors
#'   are stored minimally (avoids retaining large shard lists in memory).
#'
#' @return A `shard_dispatch_result` object with results and diagnostics.
#' @export
dispatch_chunks <- function(chunks, fun, ...,
                            pool = NULL,
                            health_check_interval = 10L,
                            max_retries = 3L,
                            timeout = 3600,
                            scheduler_policy = NULL,
                            on_result = NULL,
                            store_results = TRUE,
                            retain_chunks = TRUE) {
  if (is.null(pool)) {
    pool <- pool_get()
  }

  if (is.null(pool)) {
    stop("No active pool. Call pool_create() first.", call. = FALSE)
  }

  # Capture additional arguments as a list
  extra_args <- list(...)

  # Avoid `parallel:::` usage (R CMD check will warn). We still rely on
  # parallel's internal protocol, but obtain the function dynamically.
  parallel_sendCall <- utils::getFromNamespace("sendCall", "parallel")

  # Create queue
  queue <- queue_create(chunks)

  # Track diagnostics
  diag <- list(
    start_time = Sys.time(),
    health_checks = list(),
    worker_assignments = list(),
    scheduler = list(throttle_events = 0L)
  )

  # Export fun + args to workers once to avoid per-chunk re-export overhead.
  # Each worker runs a small wrapper via sendCall/recv that references these globals.
  for (i in seq_len(pool$n)) {
    w <- pool$workers[[i]]
    if (!is.null(w) && worker_is_alive(w)) {
      export_env <- new.env(parent = emptyenv())
      export_env$.shard_dispatch_fun <- fun
      export_env$.shard_dispatch_args <- extra_args
      parallel::clusterExport(w$cluster, c(".shard_dispatch_fun", ".shard_dispatch_args"),
                              envir = export_env)
    }
  }

  dispatch_wrapper <- function(chunk) {
    vd0 <- tryCatch(view_diagnostics(), error = function(e) NULL)
    cd0 <- tryCatch(buffer_diagnostics(), error = function(e) NULL)
    td0 <- tryCatch(table_diagnostics(), error = function(e) NULL)
    sd0 <- tryCatch(scratch_diagnostics(), error = function(e) NULL)
    res <- tryCatch(
      {
        f <- get(".shard_dispatch_fun", envir = globalenv(), inherits = FALSE)
        a <- get(".shard_dispatch_args", envir = globalenv(), inherits = FALSE)
        list(ok = TRUE, value = do.call(f, c(list(chunk), a)))
      },
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
    vd1 <- tryCatch(view_diagnostics(), error = function(e) NULL)
    cd1 <- tryCatch(buffer_diagnostics(), error = function(e) NULL)
    td1 <- tryCatch(table_diagnostics(), error = function(e) NULL)
    sd1 <- tryCatch(scratch_diagnostics(), error = function(e) NULL)

    if (is.list(vd0) && is.list(vd1)) {
      res$view_delta <- list(
        created = (vd1$created %||% 0L) - (vd0$created %||% 0L),
        materialized = (vd1$materialized %||% 0L) - (vd0$materialized %||% 0L),
        materialized_bytes = (vd1$materialized_bytes %||% 0) - (vd0$materialized_bytes %||% 0),
        packed = (vd1$packed %||% 0L) - (vd0$packed %||% 0L),
        packed_bytes = (vd1$packed_bytes %||% 0) - (vd0$packed_bytes %||% 0)
      )
    }

    if (is.list(cd0) && is.list(cd1)) {
      res$copy_delta <- list(
        buffer_writes = (cd1$writes %||% 0L) - (cd0$writes %||% 0L),
        buffer_bytes = (cd1$bytes %||% 0) - (cd0$bytes %||% 0)
      )
    }

    if (is.list(td0) && is.list(td1)) {
      res$table_delta <- list(
        table_writes = (td1$writes %||% 0L) - (td0$writes %||% 0L),
        table_rows = (td1$rows %||% 0L) - (td0$rows %||% 0L),
        table_bytes = (td1$bytes %||% 0) - (td0$bytes %||% 0)
      )
    }

    if (is.list(sd0) && is.list(sd1)) {
      res$scratch_delta <- list(
        hits = (sd1$hits %||% 0L) - (sd0$hits %||% 0L),
        misses = (sd1$misses %||% 0L) - (sd0$misses %||% 0L),
        high_water = sd1$high_water %||% 0
      )
      res$scratch_needs_recycle <- isTRUE(sd1$needs_recycle)
    }

    res
  }

  # Async scheduling state
  idle <- rep(TRUE, pool$n)
  inflight <- vector("list", pool$n) # worker_id -> {chunk_id,start_time}
  inflight_class <- rep(NA_character_, pool$n)

  chunks_processed <- 0L
  view_stats <- list(created = 0L, materialized = 0L, materialized_bytes = 0,
                     packed = 0L, packed_bytes = 0)
  copy_stats <- list(borrow_exports = 0L, borrow_bytes = 0, buffer_writes = 0L, buffer_bytes = 0)
  table_stats <- list(writes = 0L, rows = 0L, bytes = 0)
  scratch_stats <- list(hits = 0L, misses = 0L, high_water = 0)

  policy <- scheduler_policy %||% list()
  max_huge <- as.integer(policy$max_huge_concurrency %||% NA_integer_)
  if (!is.na(max_huge) && max_huge < 1L) max_huge <- 1L

  chunk_class <- function(ch) {
    (ch$footprint_class %||% "tiny")
  }

  huge_inflight <- function() {
    sum(inflight_class == "huge", na.rm = TRUE)
  }

  # Helper: receive a single result (non-blocking with small timeout) from any worker.
  recv_one <- function(timeout_sec = 0.1) {
    socklist <- list()
    idx_map <- integer(0)
    for (i in seq_along(pool$workers)) {
      w <- pool$workers[[i]]
      if (!is.null(w) && worker_is_alive(w)) {
        n <- w$cluster[[1]]
        if (!is.null(n) && !is.null(n$con)) {
          if (isOpen(n$con)) {
            socklist[[length(socklist) + 1L]] <- n$con
            idx_map <- c(idx_map, i)
          }
        }
      }
    }

    if (length(socklist) == 0) return(NULL)

    # socketSelect() is provided by base R (works for socket connections).
    ready <- socketSelect(socklist, timeout = timeout_sec)
    if (length(ready) == 0 || !any(ready)) return(NULL)

    # socketSelect is documented to return a logical vector (like parallel uses),
    # but be defensive if it returns indices on some platforms.
    if (is.logical(ready)) {
      n_local <- which.max(ready)
    } else if (is.numeric(ready)) {
      # Some platforms return a 0/1 numeric vector; others may return indices.
      if (length(ready) == length(socklist)) {
        n_local <- which.max(ready)
      } else {
        n_local <- as.integer(ready[1])
      }
    } else {
      return(NULL)
    }

    if (is.na(n_local) || n_local < 1L || n_local > length(socklist)) {
      return(NULL)
    }
    worker_id <- idx_map[n_local]

    msg <- tryCatch(
      unserialize(socklist[[n_local]]),
      error = function(e) list(tag = NA_character_, value = list(ok = FALSE, error = conditionMessage(e)),
                               .recv_error = TRUE)
    )
    list(worker_id = worker_id, tag = msg$tag, value = msg$value, recv_error = isTRUE(msg$.recv_error))
  }

  # Main dispatch loop: keep all workers busy and collect results as they finish.
  while (!queue_is_done(queue)) {
    # Health check at intervals (based on completions)
    if (chunks_processed > 0 && chunks_processed %% health_check_interval == 0) {
      busy_ids <- which(!idle)
      health <- pool_health_check(pool, busy_workers = busy_ids)
      diag$health_checks <- c(diag$health_checks, list(health))

      # Refresh pool reference after potential recycle/restart.
      pool <- pool_get()

      # Requeue any in-flight chunks from recycled/restarted workers
      for (action in health$worker_actions) {
        if (action$action %in% c("restart", "recycle")) {
          requeued <- queue_requeue_worker(queue, action$worker_id)
          if (requeued > 0) {
            message(sprintf("Requeued %d chunks from worker %d (%s)",
                            requeued, action$worker_id, action$reason))
          }
          idle[action$worker_id] <- TRUE
          inflight[action$worker_id] <- list(NULL)

          # Ensure the replacement worker has the dispatch globals.
          w_new <- pool$workers[[action$worker_id]]
          if (!is.null(w_new)) {
            export_env <- new.env(parent = emptyenv())
            export_env$.shard_dispatch_fun <- fun
            export_env$.shard_dispatch_args <- extra_args
            parallel::clusterExport(w_new$cluster,
                                    c(".shard_dispatch_fun", ".shard_dispatch_args"),
                                    envir = export_env)
          }
        }
      }
    }

    # Fill idle workers
    for (worker_id in which(idle)) {
      if (!queue_has_pending(queue)) break

      pred <- NULL
      if (!is.na(max_huge) && huge_inflight() >= max_huge) {
        pred <- function(ch) chunk_class(ch) != "huge"
      }

      chunk <- queue_next_where(queue, worker_id, pred)
      if (is.null(chunk)) {
        # If we couldn't find a non-huge chunk under throttling, leave the
        # worker idle and wait for an in-flight huge task to finish.
        if (!is.null(pred)) {
          diag$scheduler$throttle_events <- diag$scheduler$throttle_events + 1L
        }
        next
      }

      w <- pool$workers[[worker_id]]
      if (is.null(w) || !worker_is_alive(w)) {
        pool$workers[[worker_id]] <- worker_spawn(
          id = worker_id,
          init_expr = pool$init_expr,
          packages = pool$packages,
          dev_path = pool$dev_path
        )
        pool$workers[[worker_id]]$rss_baseline <- worker_rss(pool$workers[[worker_id]])
        pool$stats$total_deaths <- pool$stats$total_deaths + 1L
        .pool_env$pool <- pool
        w <- pool$workers[[worker_id]]

        # Re-export globals to the restarted worker
        export_env <- new.env(parent = emptyenv())
        export_env$.shard_dispatch_fun <- fun
        export_env$.shard_dispatch_args <- extra_args
        parallel::clusterExport(w$cluster, c(".shard_dispatch_fun", ".shard_dispatch_args"),
                                envir = export_env)
      }

      # If this worker was flagged for recycle while busy, do it now (safe point).
      if (isTRUE(w$needs_recycle)) {
        min_age <- pool$min_recycle_interval %||% 0
        age <- as.numeric(difftime(Sys.time(), w$spawned_at %||% Sys.time(), units = "secs"))
        if (!is.na(age) && is.finite(min_age) && age < min_age) {
          # Cooldown: avoid thrashing PSOCK creation/destruction under extremely
          # tight rss_limit settings.
          # We'll keep processing and recycle on a later safe point.
          w$needs_recycle <- TRUE
        } else {
          pool$workers[[worker_id]] <- worker_recycle(w, pool$init_expr, pool$packages, dev_path = pool$dev_path)
          pool$workers[[worker_id]]$rss_baseline <- worker_rss(pool$workers[[worker_id]])
          pool$workers[[worker_id]]$needs_recycle <- FALSE
          pool$stats$total_recycles <- pool$stats$total_recycles + 1L
          .pool_env$pool <- pool
          w <- pool$workers[[worker_id]]

          export_env <- new.env(parent = emptyenv())
          export_env$.shard_dispatch_fun <- fun
          export_env$.shard_dispatch_args <- extra_args
          parallel::clusterExport(w$cluster, c(".shard_dispatch_fun", ".shard_dispatch_args"),
                                  envir = export_env)
        }
      }

      # Async send
      parallel_sendCall(w$cluster[[1]], fun = dispatch_wrapper, args = list(chunk),
                        return = TRUE, tag = as.character(chunk$id))
      idle[worker_id] <- FALSE
      inflight[[worker_id]] <- list(chunk_id = as.character(chunk$id), start_time = Sys.time())
      inflight_class[worker_id] <- chunk_class(chunk)
      pool$stats$total_tasks <- pool$stats$total_tasks + 1L
      .pool_env$pool <- pool
    }

    # Handle timeouts for in-flight chunks
    if (any(!idle)) {
      now <- Sys.time()
      for (worker_id in which(!idle)) {
        meta <- inflight[[worker_id]]
        if (is.null(meta)) next
        age <- as.numeric(difftime(now, meta$start_time, units = "secs"))
        if (!is.na(age) && age > timeout) {
          # Kill and restart the worker; requeue or permanently fail the chunk.
          chunk_id <- meta$chunk_id
          ch <- queue$env$in_flight[[chunk_id]]
          retry_count <- (ch$retry_count %||% 0L)
          requeue <- retry_count < max_retries
          queue_fail(queue, chunk_id, error = "timeout", requeue = requeue)
          if (!requeue) {
            warning(sprintf("Chunk %s permanently failed after %d retries: %s",
                            chunk_id, retry_count, "timeout"))
          }

          worker_kill(pool$workers[[worker_id]])
          pool$workers[[worker_id]] <- worker_spawn(
            id = worker_id,
            init_expr = pool$init_expr,
            packages = pool$packages,
            dev_path = pool$dev_path
          )
          pool$workers[[worker_id]]$rss_baseline <- worker_rss(pool$workers[[worker_id]])
          pool$stats$total_deaths <- pool$stats$total_deaths + 1L
          .pool_env$pool <- pool

          # Re-export globals to the restarted worker
          export_env <- new.env(parent = emptyenv())
          export_env$.shard_dispatch_fun <- fun
          export_env$.shard_dispatch_args <- extra_args
          parallel::clusterExport(pool$workers[[worker_id]]$cluster,
                                  c(".shard_dispatch_fun", ".shard_dispatch_args"),
                                  envir = export_env)

          idle[worker_id] <- TRUE
          inflight[worker_id] <- list(NULL)
        }
      }
    }

    # Receive at most one completed chunk per loop iteration.
    rec <- recv_one(timeout_sec = 0.1)
    if (is.null(rec)) {
      if (!queue_has_pending(queue) && any(!idle)) {
        Sys.sleep(0.01)
      }
      next
    }

    worker_id <- rec$worker_id
    tag <- rec$tag
    payload <- rec$value

    if (isTRUE(rec$recv_error)) {
      # Treat a receive/unserialize failure as a worker death while running.
      meta <- inflight[[worker_id]]
      chunk_id <- meta$chunk_id %||% as.character(tag)
      if (!is.null(chunk_id) && !is.na(chunk_id)) {
        ch <- queue$env$in_flight[[as.character(chunk_id)]]
        retry_count <- (ch$retry_count %||% 0L)
        requeue <- retry_count < max_retries
        queue_fail(queue, as.character(chunk_id), error = payload$error %||% "recv error", requeue = requeue)
        if (!requeue) {
          warning(sprintf("Chunk %s permanently failed after %d retries: %s",
                          as.character(chunk_id), retry_count, payload$error %||% "recv error"))
        }
      }

      worker_kill(pool$workers[[worker_id]])
      pool$workers[[worker_id]] <- worker_spawn(
        id = worker_id,
        init_expr = pool$init_expr,
        packages = pool$packages,
        dev_path = pool$dev_path
      )
      pool$workers[[worker_id]]$rss_baseline <- worker_rss(pool$workers[[worker_id]])
      pool$stats$total_deaths <- pool$stats$total_deaths + 1L
      .pool_env$pool <- pool

      export_env <- new.env(parent = emptyenv())
      export_env$.shard_dispatch_fun <- fun
      export_env$.shard_dispatch_args <- extra_args
      parallel::clusterExport(pool$workers[[worker_id]]$cluster,
                              c(".shard_dispatch_fun", ".shard_dispatch_args"),
                              envir = export_env)

      idle[worker_id] <- TRUE
      inflight[worker_id] <- list(NULL)
      next
    }

    idle[worker_id] <- TRUE
    inflight[worker_id] <- list(NULL)
    inflight_class[worker_id] <- NA_character_

    if (is.list(payload) && isTRUE(payload$ok)) {
      val <- payload$value
      if (!is.null(on_result)) {
        if (!is.function(on_result)) stop("on_result must be a function or NULL", call. = FALSE)
        # If the reducer throws, fail fast: continuing would silently corrupt results.
        on_result(tag = tag, value = val, worker_id = worker_id)
      }
      queue_complete(queue, tag, if (isTRUE(store_results)) val else NULL, retain = retain_chunks)
    } else {
      err <- if (is.list(payload)) payload$error else "unknown worker error"
      ch <- queue$env$in_flight[[as.character(tag)]]
      retry_count <- (ch$retry_count %||% 0L)
      if (retry_count >= max_retries) {
        queue_fail(queue, tag, error = err, requeue = FALSE)
        warning(sprintf("Chunk %s permanently failed after %d retries: %s",
                        tag, retry_count, err))
      } else {
        queue_fail(queue, tag, error = err, requeue = TRUE)
      }
    }

    if (is.list(payload) && is.list(payload$view_delta)) {
      vd <- payload$view_delta
      view_stats$created <- view_stats$created + (vd$created %||% 0L)
      view_stats$materialized <- view_stats$materialized + (vd$materialized %||% 0L)
      view_stats$materialized_bytes <- view_stats$materialized_bytes + (vd$materialized_bytes %||% 0)
      view_stats$packed <- view_stats$packed + (vd$packed %||% 0L)
      view_stats$packed_bytes <- view_stats$packed_bytes + (vd$packed_bytes %||% 0)
    }

    if (is.list(payload) && is.list(payload$copy_delta)) {
      cd <- payload$copy_delta
      copy_stats$buffer_writes <- copy_stats$buffer_writes + (cd$buffer_writes %||% 0L)
      copy_stats$buffer_bytes <- copy_stats$buffer_bytes + (cd$buffer_bytes %||% 0)
    }

    if (is.list(payload) && is.list(payload$table_delta)) {
      td <- payload$table_delta
      table_stats$writes <- table_stats$writes + (td$table_writes %||% 0L)
      table_stats$rows <- table_stats$rows + (td$table_rows %||% 0L)
      table_stats$bytes <- table_stats$bytes + (td$table_bytes %||% 0)
    }

    if (is.list(payload) && is.list(payload$scratch_delta)) {
      sd <- payload$scratch_delta
      scratch_stats$hits <- scratch_stats$hits + (sd$hits %||% 0L)
      scratch_stats$misses <- scratch_stats$misses + (sd$misses %||% 0L)
      scratch_stats$high_water <- max(as.double(scratch_stats$high_water), as.double(sd$high_water %||% 0))
    }

    if (isTRUE(payload$scratch_needs_recycle)) {
      # Request a recycle at the next safe point (before the worker is reused).
      w <- pool$workers[[worker_id]]
      if (!is.null(w)) {
        w$needs_recycle <- TRUE
        pool$workers[[worker_id]] <- w
        .pool_env$pool <- pool
      }
    }

    chunks_processed <- chunks_processed + 1L
  }

  diag$end_time <- Sys.time()
  diag$duration <- as.numeric(difftime(diag$end_time, diag$start_time, units = "secs"))
  diag$view_stats <- view_stats
  diag$copy_stats <- copy_stats
  diag$table_stats <- table_stats
  diag$scratch_stats <- scratch_stats

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
