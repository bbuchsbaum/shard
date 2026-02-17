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

  out_desc_key_ <- function(d) {
    kind <- d$kind %||% "buffer"
    if (identical(kind, "buffer")) {
      return(paste0("buffer|", d$path, "|", d$backing, "|", d$type, "|", paste(d$dim, collapse = "x")))
    }
    if (identical(kind, "table_buffer")) {
      col_keys <- vapply(d$columns, function(cd) {
        paste0(cd$path, "|", cd$backing, "|", cd$type, "|", paste(cd$dim, collapse = "x"))
      }, character(1))
      return(paste0("table_buffer|", paste(names(col_keys), col_keys, sep = "=", collapse = ";")))
    }
    if (identical(kind, "table_sink")) {
      return(paste0("table_sink|", d$path, "|", d$format, "|", d$mode))
    }
    paste0("unknown|", kind)
  }

  open_one_ <- function(d) {
    kind <- d$kind %||% "buffer"
    if (identical(kind, "buffer")) {
      buf <- buffer_open(
        path = d$path,
        type = d$type,
        dim = d$dim,
        backing = d$backing,
        readonly = FALSE
      )
      return(buf)
    }
    if (identical(kind, "table_buffer")) {
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
      tb <- structure(
        list(schema = d$schema, nrow = as.integer(d$nrow), backing = d$backing, columns = cols),
        class = "shard_table_buffer"
      )
      return(tb)
    }
    if (identical(kind, "table_sink")) {
      ts <- structure(
        list(schema = d$schema, mode = d$mode, path = d$path, format = d$format),
        class = "shard_table_sink"
      )
      return(ts)
    }
    stop("Unsupported out descriptor kind: ", kind, call. = FALSE)
  }

  close_one_ <- function(obj) {
    if (inherits(obj, "shard_buffer")) {
      tryCatch(buffer_close(obj), error = function(e) NULL)
      return(invisible(NULL))
    }
    if (inherits(obj, "shard_table_buffer")) {
      if (!is.null(obj$columns) && is.list(obj$columns)) {
        for (col in obj$columns) {
          if (inherits(col, "shard_buffer")) {
            tryCatch(buffer_close(col), error = function(e) NULL)
          }
        }
      }
      return(invisible(NULL))
    }
    invisible(NULL)
  }

  if (!exists(".shard_out_opened", envir = .shard_worker_env, inherits = FALSE)) {
    assign(".shard_out_opened", new.env(parent = emptyenv()), envir = .shard_worker_env)
  }
  opened <- get(".shard_out_opened", envir = .shard_worker_env)

  for (nm in names(out_desc)) {
    d <- out_desc[[nm]]
    want_key <- out_desc_key_(d)
    entry <- if (exists(nm, envir = opened, inherits = FALSE)) opened[[nm]] else NULL
    cur_key <- NULL
    cur_obj <- NULL
    if (!is.null(entry) && is.list(entry) && !is.null(entry$key) && !is.null(entry$obj)) {
      cur_key <- entry$key
      cur_obj <- entry$obj
    } else if (!is.null(entry)) {
      cur_obj <- entry
    }

    if (is.null(cur_obj) || !identical(cur_key, want_key)) {
      if (!is.null(cur_obj)) close_one_(cur_obj)
      new_obj <- open_one_(d)
      opened[[nm]] <- list(key = want_key, obj = new_obj)
    }

    out[[nm]] <- opened[[nm]]$obj
  }

  out
}

worker_shm_queue_loop_ <- function(queue_desc,
                                  worker_id,
                                  mode,
                                  n,
                                  block_size,
                                  fun,
                                  borrow_names,
                                  out_names,
                                  max_retries,
                                  error_log = FALSE,
                                  error_log_max_lines = 100L,
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

  mode <- as.character(mode %||% "scalar_n")
  block_size <- as.integer(block_size)
  n <- as.integer(n)

  error_log <- isTRUE(error_log)
  error_log_max_lines <- as.integer(error_log_max_lines)
  if (is.na(error_log_max_lines) || error_log_max_lines < 0L) error_log_max_lines <- 0L

  err_con <- NULL
  err_path <- NULL
  err_written <- 0L
  if (error_log && error_log_max_lines > 0L) {
    err_path <- file.path(tempdir(), sprintf("shard_shm_queue_errors_pid%d_w%d.log", Sys.getpid(), worker_id))
    err_con <- file(err_path, open = "a", encoding = "UTF-8")
    on.exit(try(close(err_con), silent = TRUE), add = TRUE)

    assign(".shard_shm_queue_error_log_path", err_path, envir = .shard_worker_env)
    assign(".shard_shm_queue_error_count", 0L, envir = .shard_worker_env)
  }

  repeat {
    st <- taskq_stats(seg)
    if ((st$done %||% 0L) + (st$failed %||% 0L) >= (st$n_tasks %||% 0L)) break

    task_id <- as.integer(taskq_claim(seg, worker_id))
    if (is.na(task_id) || task_id < 1L) {
      Sys.sleep(poll_sleep)
      next
    }

    shard <- NULL
    if (identical(mode, "scalar_n")) {
      start <- (task_id - 1L) * block_size + 1L
      end <- min(task_id * block_size, n)
      shard <- list(id = task_id, start = start, end = end, idx = start:end, len = end - start + 1L)
    } else if (identical(mode, "descriptor")) {
      # Fetch shard descriptors once per worker process.
      shards_key <- if (exists(".shard_shm_queue_shards_key", envir = globalenv(), inherits = FALSE)) {
        get(".shard_shm_queue_shards_key", envir = globalenv())
      } else {
        NULL
      }
      fetched_key <- if (exists(".shard_shm_queue_shards_fetched_key", envir = .shard_worker_env, inherits = FALSE)) {
        get(".shard_shm_queue_shards_fetched_key", envir = .shard_worker_env)
      } else {
        NULL
      }
      if (!exists(".shard_shm_queue_shards_fetched", envir = .shard_worker_env, inherits = FALSE) ||
          !identical(fetched_key, shards_key)) {
        shards_handle <- if (exists(".shard_shm_queue_shards", envir = globalenv(), inherits = FALSE)) {
          get(".shard_shm_queue_shards", envir = globalenv())
        } else {
          NULL
        }
        if (is.null(shards_handle)) stop("shm_queue descriptor mode missing .shard_shm_queue_shards", call. = FALSE)
        assign(".shard_shm_queue_shards_fetched", fetch(shards_handle), envir = .shard_worker_env)
        assign(".shard_shm_queue_shards_fetched_key", shards_key, envir = .shard_worker_env)
      }

      shards_list <- get(".shard_shm_queue_shards_fetched", envir = .shard_worker_env)
      shard <- shards_list[[task_id]]
      if (is.null(shard$id)) shard$id <- task_id
      if (is.null(shard$len)) {
        if (!is.null(shard$idx)) shard$len <- length(shard$idx)
        else if (!is.null(shard$start) && !is.null(shard$end)) shard$len <- as.integer(shard$end - shard$start + 1L)
      }
      if (is.null(shard$idx) && !is.null(shard$start) && !is.null(shard$end)) {
        shard$idx <- as.integer(shard$start):as.integer(shard$end)
      }
    } else {
      stop("Unsupported shm_queue mode: ", mode, call. = FALSE)
    }

    args <- list(shard)
    for (nm in borrow_names) args[[nm]] <- borrow[[nm]]
    for (nm in out_names) args[[nm]] <- out[[nm]]

    ok <- TRUE
    err_msg <- NULL
    tryCatch(
      do.call(fun, args, quote = TRUE),
      error = function(e) {
        ok <<- FALSE
        err_msg <<- conditionMessage(e)
        invisible(NULL)
      }
    )

    if (ok) {
      taskq_done(seg, task_id)
    } else {
      if (!is.null(err_con) && err_written < error_log_max_lines) {
        # Keep logs compact and parseable; avoid timestamps to keep tests stable.
        writeLines(paste0(task_id, "\t", err_msg %||% "error"), con = err_con, sep = "\n")
        err_written <- err_written + 1L
        assign(".shard_shm_queue_error_count", err_written, envir = .shard_worker_env)
      }
      taskq_error(seg, task_id, max_retries = max_retries)
    }
  }

  # Return final queue stats (small).
  taskq_stats(seg)
}

dispatch_shards_shm_queue_ <- function(n,
                                      block_size,
                                      shards = NULL,
                                      fun,
                                      borrow,
                                      out,
                                      pool,
                                      max_retries,
                                      timeout = 3600,
                                      queue_backing = c("mmap", "shm"),
                                      error_log = FALSE,
                                      error_log_max_lines = 100L) {
  n <- as.integer(n)
  block_size <- as.integer(block_size)
  if (is.na(n) || n < 1L) stop("n must be >= 1", call. = FALSE)
  if (is.na(block_size) || block_size < 1L) stop("block_size must be >= 1", call. = FALSE)
  queue_backing <- match.arg(queue_backing)
  error_log <- isTRUE(error_log)
  error_log_max_lines <- as.integer(error_log_max_lines)
  if (is.na(error_log_max_lines) || error_log_max_lines < 0L) error_log_max_lines <- 0L

  mode <- if (is.null(shards)) "scalar_n" else "descriptor"
  if (!is.null(shards) && !inherits(shards, "shard_descriptor")) {
    stop("shards must be a shard_descriptor (or NULL)", call. = FALSE)
  }

  n_tasks <- if (identical(mode, "scalar_n")) {
    as.integer(ceiling(n / block_size))
  } else {
    as.integer(shards$num_shards %||% length(shards$shards))
  }

  q <- taskq_create(n_tasks, backing = queue_backing)
  qdesc <- q$desc
  seg_master <- q$owner
  on.exit(try(segment_close(seg_master, unlink = TRUE), silent = TRUE), add = TRUE)

  borrow_names <- names(borrow)
  out_names <- names(out)

  shards_shared <- NULL
  shards_key <- NULL
  if (identical(mode, "descriptor")) {
    shards_shared <- share(shards$shards, backing = queue_backing)
    shards_key <- shards_shared$path %||% NULL
  }

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
      if (identical(mode, "descriptor")) {
        export_env <- new.env(parent = emptyenv())
        export_env$.shard_shm_queue_shards <- shards_shared
        export_env$.shard_shm_queue_shards_key <- shards_key
        tryCatch(parallel::clusterExport(w$cluster, c(".shard_shm_queue_shards", ".shard_shm_queue_shards_key"), envir = export_env),
                 error = function(e) NULL)
      }
    }

    if (identical(mode, "descriptor")) {
      export_env <- new.env(parent = emptyenv())
      export_env$.shard_shm_queue_shards <- shards_shared
      export_env$.shard_shm_queue_shards_key <- shards_key
      tryCatch(parallel::clusterExport(w$cluster, c(".shard_shm_queue_shards", ".shard_shm_queue_shards_key"), envir = export_env),
               error = function(e) NULL)
    }

    parallel_sendCall(
      w$cluster[[1]],
      fun = worker_shm_queue_loop_,
      args = list(qdesc, worker_id, mode, n, block_size, fun, borrow_names, out_names, max_retries, error_log, error_log_max_lines),
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
  error_logs <- list()

  for (i in seq_len(pool$n)) {
    w <- pool$workers[[i]]
    if (is.null(w) || !worker_is_alive(w)) next
    vals <- tryCatch(
      parallel::clusterCall(w$cluster, function() {
        list(
          view = tryCatch(view_diagnostics(), error = function(e) NULL),
          buf = tryCatch(buffer_diagnostics(), error = function(e) NULL),
          table = tryCatch(table_diagnostics(), error = function(e) NULL),
          scratch = tryCatch(scratch_diagnostics(), error = function(e) NULL),
          err_path = if (exists(".shard_shm_queue_error_log_path", envir = .shard_worker_env, inherits = FALSE)) {
            get(".shard_shm_queue_error_log_path", envir = .shard_worker_env)
          } else {
            NULL
          },
          err_count = if (exists(".shard_shm_queue_error_count", envir = .shard_worker_env, inherits = FALSE)) {
            get(".shard_shm_queue_error_count", envir = .shard_worker_env)
          } else {
            NULL
          }
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
    if (error_log && !is.null(vals$err_path) && is.character(vals$err_path) && length(vals$err_path) == 1L) {
      ec <- as.integer(vals$err_count %||% 0L)
      if (!is.na(ec) && ec > 0L) {
        error_logs[[length(error_logs) + 1L]] <- list(worker_id = i, path = vals$err_path, errors = ec)
      }
    }
  }

  # Collect final results (not gathered) + failures.
  fail <- taskq_failures(seg_master)
  failures <- list()
  if (is.list(fail) && !is.null(fail$task_id)) {
    ids <- as.integer(fail$task_id)
    rcs <- as.integer(fail$retry_count %||% rep.int(NA_integer_, length(ids)))
    for (i in seq_along(ids)) {
      id <- ids[[i]]
      if (is.na(id)) next
      failures[[as.character(id)]] <- list(
        id = id,
        last_error = "shm_queue_failed",
        retry_count = rcs[[i]] %||% NA_integer_
      )
    }
  }

  st_final <- taskq_stats(seg_master)
  total_retries <- as.integer(st_final$retries %||% 0L)

  list(
    # Fire-and-forget: do not allocate a giant placeholder list for large runs.
    # Use a lightweight results placeholder that behaves like a list of NULLs.
    results = structure(list(n = as.integer(n_tasks)), class = "shard_results_placeholder"),
    failures = failures,
    queue_status = list(
      total = n_tasks,
      pending = 0L,
      in_flight = 0L,
      completed = as.integer(st_final$done %||% 0L),
      failed = as.integer(st_final$failed %||% 0L),
      total_retries = total_retries
    ),
    diagnostics = list(
      taskq = st_final,
      view_stats = view_stats,
      copy_stats = copy_stats,
      table_stats = table_stats,
      scratch_stats = scratch_stats,
      error_logs = error_logs
    ),
    pool_stats = pool_get()$stats
  )
}
