# Shared-memory dispatch mode (shm_queue).
#
# This avoids per-task RPC by running a single long-lived loop per worker that
# pulls task ids from a shared segment. This mode is intentionally limited:
# - tasks are identified by integer ids only
# - results are not gathered (use explicit output buffers)

open_out_from_desc_ <- function(out_desc) {
  # Lazily open output buffers once per worker process and cache them.
  out <- list()
  if (length(out_desc) == 0) return(out)

  if (!exists(".shard_out_opened", envir = globalenv(), inherits = FALSE)) {
    assign(".shard_out_opened", new.env(parent = emptyenv()), envir = globalenv())
  }
  opened <- get(".shard_out_opened", envir = globalenv())

  for (nm in names(out_desc)) {
    if (!exists(nm, envir = opened, inherits = FALSE)) {
      d <- out_desc[[nm]]
      if (is.null(d$kind) || identical(d$kind, "buffer")) {
        opened[[nm]] <- buffer_open(
          path = d$path,
          type = d$type,
          dim = d$dim,
          backing = d$backing,
          readonly = FALSE
        )
      } else if (identical(d$kind, "table_buffer")) {
        cols <- list()
        for (cn in names(d$columns)) {
          cd <- d$columns[[cn]]
          cols[[cn]] <- buffer_open(
            path = cd$path,
            type = cd$type,
            dim = cd$dim,
            backing = cd$backing,
            readonly = FALSE
          )
        }
        opened[[nm]] <- structure(
          list(schema = d$schema, nrow = as.integer(d$nrow), backing = d$backing, columns = cols),
          class = "shard_table_buffer"
        )
      } else if (identical(d$kind, "table_sink")) {
        opened[[nm]] <- structure(
          list(schema = d$schema, mode = d$mode, path = d$path, format = d$format),
          class = "shard_table_sink"
        )
      } else {
        stop("Unsupported out descriptor kind: ", d$kind, call. = FALSE)
      }
    }
    out[[nm]] <- opened[[nm]]
  }

  out
}

worker_shm_queue_loop_ <- function(queue_desc,
                                  worker_id,
                                  n,
                                  block_size,
                                  fun,
                                  borrow_names,
                                  out_names,
                                  max_retries,
                                  poll_sleep = 0.0005) {
  # Open queue segment (writable).
  seg <- segment_open(queue_desc$path, backing = queue_desc$backing, readonly = FALSE)
  on.exit(try(segment_close(seg, unlink = FALSE), silent = TRUE), add = TRUE)

  # Get borrow/out descriptors from worker environment.
  borrow <- if (exists(".shard_borrow", envir = globalenv())) {
    get(".shard_borrow", envir = globalenv())
  } else {
    list()
  }
  out_desc <- if (exists(".shard_out", envir = globalenv())) {
    get(".shard_out", envir = globalenv())
  } else {
    list()
  }
  out <- open_out_from_desc_(out_desc)

  block_size <- as.integer(block_size)
  n <- as.integer(n)

  repeat {
    st <- taskq_stats(seg)
    if ((st$done %||% 0L) + (st$failed %||% 0L) >= (st$n_tasks %||% 0L)) break

    task_id <- as.integer(taskq_claim(seg, worker_id))
    if (is.na(task_id) || task_id < 1L) {
      Sys.sleep(poll_sleep)
      next
    }

    start <- (task_id - 1L) * block_size + 1L
    end <- min(task_id * block_size, n)
    shard <- list(id = task_id, start = start, end = end, idx = start:end, len = end - start + 1L)

    args <- list(shard)
    for (nm in borrow_names) args[[nm]] <- borrow[[nm]]
    for (nm in out_names) args[[nm]] <- out[[nm]]

    ok <- TRUE
    tryCatch(
      do.call(fun, args, quote = TRUE),
      error = function(e) {
        ok <<- FALSE
        invisible(e)
      }
    )

    if (ok) {
      taskq_done(seg, task_id)
    } else {
      taskq_error(seg, task_id, max_retries = max_retries)
    }
  }

  # Return final queue stats (small).
  taskq_stats(seg)
}

dispatch_shards_shm_queue_ <- function(n,
                                      block_size,
                                      fun,
                                      borrow,
                                      out,
                                      pool,
                                      max_retries,
                                      timeout = 3600,
                                      queue_backing = c("mmap", "shm")) {
  n <- as.integer(n)
  block_size <- as.integer(block_size)
  if (is.na(n) || n < 1L) stop("n must be >= 1", call. = FALSE)
  if (is.na(block_size) || block_size < 1L) stop("block_size must be >= 1", call. = FALSE)
  queue_backing <- match.arg(queue_backing)

  n_tasks <- as.integer(ceiling(n / block_size))
  q <- taskq_create(n_tasks, backing = queue_backing)
  qdesc <- q$desc
  seg_master <- q$owner
  on.exit(try(segment_close(seg_master, unlink = TRUE), silent = TRUE), add = TRUE)

  borrow_names <- names(borrow)
  out_names <- names(out)

  # Start one long-lived loop per worker. If a worker dies, requeue its claims
  # and restart the loop on a fresh worker.
  parallel_sendCall <- utils::getFromNamespace("sendCall", "parallel")

  idle <- rep(TRUE, pool$n)
  started <- rep(FALSE, pool$n)
  inflight <- vector("list", pool$n)

  start_loop <- function(worker_id) {
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

      # Re-export borrow/out to the restarted worker.
      export_borrow_to_workers(pool, borrow)
      if (length(out) > 0) export_out_to_workers(pool, out)
    }

    parallel_sendCall(
      w$cluster[[1]],
      fun = worker_shm_queue_loop_,
      args = list(qdesc, worker_id, n, block_size, fun, borrow_names, out_names, max_retries),
      return = TRUE,
      tag = paste0("shm_queue_", worker_id)
    )
    started[worker_id] <<- TRUE
    idle[worker_id] <<- FALSE
    inflight[[worker_id]] <<- list(start_time = Sys.time())
  }

  # Kick off loops.
  for (i in seq_len(pool$n)) start_loop(i)

  # Wait for completion. Use queue stats to decide done.
  deadline <- Sys.time() + timeout
  repeat {
    st <- taskq_stats(seg_master)
    if ((st$done %||% 0L) + (st$failed %||% 0L) >= (st$n_tasks %||% 0L)) break
    if (Sys.time() > deadline) stop("shm_queue dispatch timed out", call. = FALSE)

    # Restart dead workers.
    for (i in seq_len(pool$n)) {
      w <- pool$workers[[i]]
      if (!is.null(w) && started[i] && !worker_is_alive(w)) {
        # Requeue tasks claimed by this worker (best-effort).
        taskq_reset_claims(seg_master, i)
        idle[i] <- TRUE
        started[i] <- FALSE
        inflight[[i]] <- NULL
        start_loop(i)
      }
    }

    Sys.sleep(0.01)
  }

  # Drain the single return value from each worker loop to keep PSOCK
  # connections clean (avoid "unused connection" warnings).
  for (i in seq_len(pool$n)) {
    w <- pool$workers[[i]]
    if (is.null(w) || is.null(w$cluster) || length(w$cluster) < 1L) next
    n <- w$cluster[[1]]
    con <- n$con %||% NULL
    if (is.null(con) || !isOpen(con)) next
    ready <- tryCatch(socketSelect(list(con), timeout = 0.1), error = function(e) FALSE)
    if (isTRUE(ready)) {
      tryCatch(unserialize(con), error = function(e) NULL)
    }
  }

  # Aggregate per-worker counters once (cheap).
  view_stats <- list(created = 0L, materialized = 0L, materialized_bytes = 0, packed = 0L, packed_bytes = 0)
  copy_stats <- list(borrow_exports = 0L, borrow_bytes = 0, buffer_writes = 0L, buffer_bytes = 0)
  table_stats <- list(writes = 0L, rows = 0L, bytes = 0)
  scratch_stats <- list(hits = 0L, misses = 0L, high_water = 0)

  for (i in seq_len(pool$n)) {
    w <- pool$workers[[i]]
    if (is.null(w) || !worker_is_alive(w)) next
    vals <- tryCatch(
      parallel::clusterCall(w$cluster, function() {
        list(
          view = tryCatch(view_diagnostics(), error = function(e) NULL),
          buf = tryCatch(buffer_diagnostics(), error = function(e) NULL),
          table = tryCatch(table_diagnostics(), error = function(e) NULL),
          scratch = tryCatch(scratch_diagnostics(), error = function(e) NULL)
        )
      })[[1]],
      error = function(e) NULL
    )
    if (is.null(vals)) next
    if (is.list(vals$view)) {
      view_stats$created <- view_stats$created + (vals$view$created %||% 0L)
      view_stats$materialized <- view_stats$materialized + (vals$view$materialized %||% 0L)
      view_stats$materialized_bytes <- view_stats$materialized_bytes + (vals$view$materialized_bytes %||% 0)
      view_stats$packed <- view_stats$packed + (vals$view$packed %||% 0L)
      view_stats$packed_bytes <- view_stats$packed_bytes + (vals$view$packed_bytes %||% 0)
    }
    if (is.list(vals$buf)) {
      copy_stats$buffer_writes <- copy_stats$buffer_writes + (vals$buf$writes %||% 0L)
      copy_stats$buffer_bytes <- copy_stats$buffer_bytes + (vals$buf$bytes %||% 0)
    }
    if (is.list(vals$table)) {
      table_stats$writes <- table_stats$writes + (vals$table$writes %||% 0L)
      table_stats$rows <- table_stats$rows + (vals$table$rows %||% 0L)
      table_stats$bytes <- table_stats$bytes + (vals$table$bytes %||% 0)
    }
    if (is.list(vals$scratch)) {
      scratch_stats$hits <- scratch_stats$hits + (vals$scratch$hits %||% 0L)
      scratch_stats$misses <- scratch_stats$misses + (vals$scratch$misses %||% 0L)
      scratch_stats$high_water <- max(as.double(scratch_stats$high_water), as.double(vals$scratch$high_water %||% 0))
    }
  }

  # Collect final results (not gathered) + failures.
  fail_ids <- taskq_failures(seg_master)
  failures <- list()
  if (length(fail_ids) > 0) {
    for (i in seq_along(fail_ids)) {
      id <- as.integer(fail_ids[[i]])
      failures[[as.character(id)]] <- list(id = id, last_error = "shm_queue_failed", retry_count = max_retries + 1L)
    }
  }

  list(
    # Fire-and-forget: do not allocate a giant placeholder list for large runs.
    # Use a lightweight results placeholder that behaves like a list of NULLs.
    results = structure(list(n = as.integer(n_tasks)), class = "shard_results_placeholder"),
    failures = failures,
    queue_status = list(
      total = n_tasks,
      pending = 0L,
      in_flight = 0L,
      completed = as.integer(taskq_stats(seg_master)$done %||% 0L),
      failed = as.integer(taskq_stats(seg_master)$failed %||% 0L),
      total_retries = NA_integer_
    ),
    diagnostics = list(
      taskq = taskq_stats(seg_master),
      view_stats = view_stats,
      copy_stats = copy_stats,
      table_stats = table_stats,
      scratch_stats = scratch_stats
    ),
    pool_stats = pool_get()$stats
  )
}
