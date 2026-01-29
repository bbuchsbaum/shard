#' @title Parallel Execution with shard_map
#' @description Core parallel execution engine with supervision, shared inputs, and output buffers.
#' @name shard_map
NULL

#' Parallel Shard Execution
#'
#' Executes a function over shards in parallel with worker supervision,
#' shared inputs, and explicit output buffers. This is the primary entry
#' point for shard's parallel execution model.
#'
#' @param shards A `shard_descriptor` from [shards()], or an integer N to
#'   auto-generate shards.
#' @param fun Function to execute per shard. Receives the shard descriptor
#'   as first argument, followed by borrowed inputs and outputs.
#' @param borrow Named list of shared inputs. These are exported to workers
#'   once and reused across shards. Treated as read-only by default.
#' @param out Named list of output buffers (from `buffer()`). Workers write
#'   results directly to these buffers.
#' @param workers Integer. Number of worker processes. If NULL, uses existing
#'   pool or creates one with `detectCores() - 1`.
#' @param chunk_size Integer. Shards to batch per worker dispatch (default 1).
#'   Higher values reduce RPC overhead but may hurt load balancing.
#' @param profile Execution profile: `"default"`, `"memory"` (aggressive recycling),
#'   or `"speed"` (minimal overhead).
#' @param mem_cap Memory cap per worker (e.g., "2GB"). Workers exceeding this
#'   are recycled.
#' @param recycle Logical or numeric. If TRUE, recycle workers on RSS drift.
#'   If numeric, specifies drift threshold (default 0.5 = 50% growth).
#' @param cow Copy-on-write policy for borrowed inputs: `"deny"` (error on mutation),
#'   `"audit"` (detect and flag), or `"allow"` (permit with tracking).
#' @param seed Integer. RNG seed for reproducibility. If NULL, no seed is set.
#' @param diagnostics Logical. Collect detailed diagnostics (default TRUE).
#' @param packages Character vector. Additional packages to load in workers.
#' @param init_expr Expression to evaluate in each worker on startup.
#' @param timeout Numeric. Seconds to wait for each shard (default 3600).
#' @param max_retries Integer. Maximum retries per shard on failure (default 3).
#' @param health_check_interval Integer. Check worker health every N shards (default 10).
#'
#' @return A `shard_result` object containing:
#'   - `results`: List of results from each shard (if fun returns values)
#'   - `failures`: Any permanently failed shards
#'   - `diagnostics`: Timing, memory, and worker statistics
#'   - `pool_stats`: Pool-level statistics
#'
#' @export
#' @examples
#' \dontrun{
#' # Simple parallel computation
#' blocks <- shards(1000, workers = 4)
#' result <- shard_map(blocks, function(shard) {
#'   sum(shard$idx^2)
#' }, workers = 4)
#'
#' # With shared inputs
#' X <- matrix(rnorm(1e6), nrow = 1000)
#' blocks <- shards(ncol(X), workers = 4)
#' result <- shard_map(blocks,
#'   borrow = list(X = X),
#'   fun = function(shard, X) {
#'     colMeans(X[, shard$idx, drop = FALSE])
#'   },
#'   workers = 4
#' )
#' }
shard_map <- function(shards,
                      fun,
                      borrow = list(),
                      out = list(),
                      workers = NULL,
                      chunk_size = 1L,
                      profile = c("default", "memory", "speed"),
                      mem_cap = "2GB",
                      recycle = TRUE,
                      cow = c("deny", "audit", "allow"),
                      seed = NULL,
                      diagnostics = TRUE,
                      packages = NULL,
                      init_expr = NULL,
                      timeout = 3600,
                      max_retries = 3L,
                      health_check_interval = 10L) {
  profile <- match.arg(profile)
  cow <- match.arg(cow)

  # Start timing
  start_time <- Sys.time()
  diag <- if (diagnostics) {
    list(
      start_time = start_time,
      health_checks = list(),
      shard_times = list(),
      worker_usage = list()
    )
  } else {
    NULL
  }

  # Convert integer to shard_descriptor if needed
  if (is.numeric(shards) && length(shards) == 1) {
    shards <- shards(as.integer(shards), workers = workers)
  }

  if (!inherits(shards, "shard_descriptor")) {
    stop("shards must be a shard_descriptor or integer", call. = FALSE)
  }

  # Determine worker count
  if (is.null(workers)) {
    pool <- pool_get()
    workers <- if (!is.null(pool)) pool$n else max(parallel::detectCores() - 1L, 1L)
  }
  workers <- max(as.integer(workers), 1L)

  # Apply profile settings
  profile_settings <- get_profile_settings(profile, mem_cap, recycle)
  mem_cap <- profile_settings$mem_cap
  rss_drift_threshold <- profile_settings$rss_drift_threshold
  health_check_interval <- profile_settings$health_check_interval

  # Ensure pool exists with correct worker count
 pool <- ensure_pool(
    workers = workers,
    mem_cap = mem_cap,
    rss_drift_threshold = rss_drift_threshold,
    packages = packages,
    init_expr = init_expr
  )

  # Validate borrowed inputs
  borrow <- validate_borrow(borrow, cow)

  # Validate output buffers
  out <- validate_out(out)

  # Set seed in workers if specified
  if (!is.null(seed)) {
    set_worker_seeds(pool, seed, shards$num_shards)
  }

  # Export borrowed inputs to workers (once, not per shard)
  export_borrow_to_workers(pool, borrow)

  # Export output buffer references if any
  if (length(out) > 0) {
    export_out_to_workers(pool, out)
  }

  # Create chunk batches if chunk_size > 1
  chunks <- create_shard_chunks(shards, chunk_size, fun, borrow, out)

  # Create self-contained executor function for workers
  chunk_executor <- make_chunk_executor()

  # Dispatch chunks to workers with supervision
  dispatch_result <- dispatch_chunks(
    chunks = chunks,
    fun = chunk_executor,
    pool = pool,
    health_check_interval = health_check_interval,
    max_retries = max_retries,
    timeout = timeout
  )

  # Collect diagnostics
  if (diagnostics) {
    diag$end_time <- Sys.time()
    diag$duration <- as.numeric(difftime(diag$end_time, diag$start_time, units = "secs"))
    diag$health_checks <- dispatch_result$diagnostics$health_checks
    diag$shards_processed <- shards$num_shards
    diag$chunks_dispatched <- length(chunks)
    diag$pool_stats <- dispatch_result$pool_stats
  }

  # Flatten results if chunk_size > 1
  results <- if (chunk_size > 1L) {
    unlist(dispatch_result$results, recursive = FALSE)
  } else {
    dispatch_result$results
  }

  # Build result object
  structure(
    list(
      results = results,
      failures = dispatch_result$failures,
      shards = shards,
      diagnostics = diag,
      queue_status = dispatch_result$queue_status,
      pool_stats = dispatch_result$pool_stats,
      cow_policy = cow,
      profile = profile
    ),
    class = "shard_result"
  )
}

#' Get Profile Settings
#'
#' Returns settings based on execution profile.
#'
#' @param profile Profile name.
#' @param mem_cap User-specified memory cap.
#' @param recycle User-specified recycle setting.
#' @return List of settings.
#' @keywords internal
get_profile_settings <- function(profile, mem_cap, recycle) {
  settings <- list(
    mem_cap = parse_bytes(mem_cap),
    rss_drift_threshold = if (is.numeric(recycle)) recycle else 0.5,
    health_check_interval = 10L
  )

  switch(profile,
    "memory" = {
      settings$rss_drift_threshold <- 0.25  # More aggressive recycling
      settings$health_check_interval <- 5L
    },
    "speed" = {
      settings$rss_drift_threshold <- 1.0  # Less recycling
      settings$health_check_interval <- 50L
    }
  )

  if (isFALSE(recycle)) {
    settings$rss_drift_threshold <- Inf  # Disable recycling
  }

  settings
}

#' Ensure Pool Exists
#'
#' Creates or validates worker pool.
#'
#' @param workers Number of workers.
#' @param mem_cap Memory cap in bytes.
#' @param rss_drift_threshold Drift threshold.
#' @param packages Packages to load.
#' @param init_expr Init expression.
#' @return Pool object.
#' @keywords internal
ensure_pool <- function(workers, mem_cap, rss_drift_threshold, packages, init_expr) {
  pool <- pool_get()

  # Check if existing pool is suitable
  if (!is.null(pool)) {
    if (pool$n == workers) {
      return(pool)
    }
    # Pool exists but wrong size - stop it
    pool_stop()
  }

  # Create new pool
  pool_create(
    n = workers,
    rss_limit = mem_cap,
    rss_drift_threshold = rss_drift_threshold,
    packages = packages,
    init_expr = init_expr
  )

  pool_get()
}

#' Validate Borrowed Inputs
#'
#' Validates and prepares borrowed inputs.
#'
#' @param borrow List of inputs.
#' @param cow COW policy.
#' @return Validated borrow list.
#' @keywords internal
validate_borrow <- function(borrow, cow) {
  if (length(borrow) == 0) return(borrow)

  if (!is.list(borrow) || is.null(names(borrow))) {
    stop("borrow must be a named list", call. = FALSE)
  }

  if (any(names(borrow) == "")) {
    stop("All borrowed inputs must be named", call. = FALSE)
  }

  # Mark inputs with COW policy
  for (name in names(borrow)) {
    attr(borrow[[name]], "shard_cow") <- cow
  }

  borrow
}

#' Validate Output Buffers
#'
#' Validates output buffer specifications.
#'
#' @param out List of output buffers.
#' @return Validated out list.
#' @keywords internal
validate_out <- function(out) {
  if (length(out) == 0) return(out)

  if (!is.list(out) || is.null(names(out))) {
    stop("out must be a named list", call. = FALSE)
  }

  if (any(names(out) == "")) {
    stop("All output buffers must be named", call. = FALSE)
  }

  out
}

#' Set Worker Seeds
#'
#' Sets reproducible RNG seeds in workers.
#'
#' @param pool Worker pool.
#' @param seed Base seed.
#' @param num_shards Number of shards for substream calculation.
#' @keywords internal
set_worker_seeds <- function(pool, seed, num_shards) {
  for (i in seq_len(pool$n)) {
    worker_seed <- seed + (i - 1L) * num_shards
    tryCatch({
      parallel::clusterCall(pool$workers[[i]]$cluster, function(s) {
        set.seed(s)
      }, worker_seed)
    }, error = function(e) NULL)
  }
}

#' Export Borrowed Inputs to Workers
#'
#' Exports borrowed data to all workers (once, reused across shards).
#'
#' @param pool Worker pool.
#' @param borrow List of borrowed inputs.
#' @keywords internal
export_borrow_to_workers <- function(pool, borrow) {
  if (length(borrow) == 0) return(invisible(NULL))

  # Create an environment with the borrowed data
  export_env <- new.env(parent = emptyenv())
  export_env$.shard_borrow <- borrow

  # Export to all workers
  for (i in seq_len(pool$n)) {
    w <- pool$workers[[i]]
    if (!is.null(w) && worker_is_alive(w)) {
      tryCatch({
        parallel::clusterExport(w$cluster, ".shard_borrow", envir = export_env)
      }, error = function(e) {
        warning("Failed to export borrow to worker ", i, ": ", conditionMessage(e))
      })
    }
  }

  invisible(NULL)
}

#' Export Output Buffers to Workers
#'
#' Exports output buffer references to workers.
#'
#' @param pool Worker pool.
#' @param out List of output buffers.
#' @keywords internal
export_out_to_workers <- function(pool, out) {
  if (length(out) == 0) return(invisible(NULL))

  export_env <- new.env(parent = emptyenv())
  export_env$.shard_out <- out

  for (i in seq_len(pool$n)) {
    w <- pool$workers[[i]]
    if (!is.null(w) && worker_is_alive(w)) {
      tryCatch({
        parallel::clusterExport(w$cluster, ".shard_out", envir = export_env)
      }, error = function(e) {
        warning("Failed to export out to worker ", i, ": ", conditionMessage(e))
      })
    }
  }

  invisible(NULL)
}

#' Create Shard Chunks
#'
#' Groups shards into chunks for dispatch. Each chunk contains a self-contained
#' executor function that can run in a worker without needing package functions.
#'
#' @param shards Shard descriptor.
#' @param chunk_size Shards per chunk.
#' @param fun User function.
#' @param borrow Borrowed inputs.
#' @param out Output buffers.
#' @return List of chunk descriptors.
#' @keywords internal
create_shard_chunks <- function(shards, chunk_size, fun, borrow, out) {
  chunk_size <- max(as.integer(chunk_size), 1L)
  num_chunks <- ceiling(shards$num_shards / chunk_size)

  chunks <- vector("list", num_chunks)

  borrow_names <- names(borrow)
  out_names <- names(out)

  for (i in seq_len(num_chunks)) {
    start_idx <- (i - 1L) * chunk_size + 1L
    end_idx <- min(i * chunk_size, shards$num_shards)

    chunk_shards <- shards$shards[start_idx:end_idx]

    chunks[[i]] <- list(
      id = i,
      shard_ids = start_idx:end_idx,
      shards = chunk_shards,
      fun = fun,
      borrow_names = borrow_names,
      out_names = out_names
    )
  }

  chunks
}

#' Create Chunk Executor Function
#'
#' Creates a self-contained function that can execute a chunk in a worker.
#' This function is passed to dispatch_chunks and runs entirely within
#' the worker process.
#'
#' @return A function that executes chunks.
#' @keywords internal
make_chunk_executor <- function() {
  # This function runs inside workers
  function(chunk) {
    # Get borrowed inputs and outputs from worker environment
    borrow <- if (exists(".shard_borrow", envir = globalenv())) {
      get(".shard_borrow", envir = globalenv())
    } else {
      list()
    }

    out <- if (exists(".shard_out", envir = globalenv())) {
      get(".shard_out", envir = globalenv())
    } else {
      list()
    }

    # Get the user function from the chunk
    fun <- chunk$fun
    borrow_names <- chunk$borrow_names
    out_names <- chunk$out_names

    # Execute for each shard in the chunk
    results <- lapply(chunk$shards, function(shard) {
      # Build args: shard first, then borrowed, then output
      args <- list(shard)

      for (name in borrow_names) {
        args[[name]] <- borrow[[name]]
      }

      for (name in out_names) {
        args[[name]] <- out[[name]]
      }

      # Execute user function
      do.call(fun, args)
    })

    results
  }
}

#' @export
print.shard_result <- function(x, ...) {
  cat("shard_map result\n")

  if (!is.null(x$diagnostics)) {
    cat("  Duration:", sprintf("%.2f seconds", x$diagnostics$duration), "\n")
    cat("  Shards:", x$diagnostics$shards_processed, "\n")
    cat("  Chunks:", x$diagnostics$chunks_dispatched, "\n")
  }

  status <- x$queue_status
  if (!is.null(status)) {
    cat("  Completed:", status$completed,
        if (status$failed > 0) paste0(" (", status$failed, " failed)") else "", "\n")
    if (status$total_retries > 0) {
      cat("  Retries:", status$total_retries, "\n")
    }
  }

  if (!is.null(x$pool_stats)) {
    cat("  Worker recycles:", x$pool_stats$total_recycles, "\n")
    cat("  Worker deaths:", x$pool_stats$total_deaths, "\n")
  }

  cat("  Profile:", x$profile, "\n")
  cat("  COW policy:", x$cow_policy, "\n")

  if (length(x$failures) > 0) {
    cat("  Failed shards:", length(x$failures), "\n")
  }

  invisible(x)
}

#' Extract Results from shard_map
#'
#' @param x A shard_result object.
#' @param flatten Logical. Flatten nested results?
#' @return List or vector of results.
#' @export
results <- function(x, flatten = TRUE) {
  if (!inherits(x, "shard_result")) {
    stop("x must be a shard_result object", call. = FALSE)
  }

  res <- x$results

  if (flatten && length(res) > 0) {
    # Try to simplify
    tryCatch(
      unlist(res, recursive = FALSE),
      error = function(e) res
    )
  } else {
    res
  }
}

#' Check if shard_map Succeeded
#'
#' @param x A shard_result object.
#' @return Logical. TRUE if no failures.
#' @export
succeeded <- function(x) {
  if (!inherits(x, "shard_result")) {
    stop("x must be a shard_result object", call. = FALSE)
  }

  length(x$failures) == 0
}
