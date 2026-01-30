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
#'   as first argument, followed by borrowed inputs and outputs. You can also
#'   select a registered kernel via `kernel=` instead of providing `fun=`.
#' @param borrow Named list of shared inputs. These are exported to workers
#'   once and reused across shards. Treated as read-only by default.
#' @param out Named list of output buffers (from `buffer()`). Workers write
#'   results directly to these buffers.
#' @param kernel Optional. Name of a registered kernel (see [list_kernels()]).
#'   If provided, `fun` must be NULL.
#' @param scheduler_policy Optional list of scheduling hints (advanced). Currently:
#'   - `max_huge_concurrency`: cap concurrent chunks whose kernel footprint is
#'     classified as `"huge"` (see [register_kernel()]).
#' @param autotune Optional. Online autotuning for scalar-N sharding (advanced).
#'   When `shards` is an integer `N`, shard_map can adjust shard block sizes over
#'   time based on observed wall time and worker RSS.
#'
#'   Accepted values:
#'   - `NULL` (default): enable online autotuning for `shard_map(N, ...)`, off for
#'     precomputed shard descriptors.
#'   - `TRUE` / `"online"`: force online autotuning (only applies when `shards` is
#'     an integer `N`).
#'   - `FALSE` / `"none"`: disable autotuning.
#'   - a list: `list(mode="online", max_rounds=..., probe_shards_per_worker=..., min_shard_time=...)`
#' @param dispatch_mode Dispatch mode (advanced). `"rpc_chunked"` is the default
#'   supervised socket-based dispatcher. `"shm_queue"` is an opt-in fast mode
#'   that uses a shared-memory task queue to reduce per-task overhead for tiny
#'   tasks. In v1, `"shm_queue"` is only supported for `shard_map(N, ...)` with
#'   `chunk_size=1` and is intended for out-buffer/sink workflows (results are
#'   not gathered).
#' @param dispatch_opts Optional list of dispatch-mode specific knobs (advanced).
#'   Currently:
#'   - For `dispatch_mode="shm_queue"`:
#'     - `block_size`: integer. If provided, overrides the default heuristic for
#'       contiguous shard block sizing.
#'     - `queue_backing`: one of `"mmap"` or `"shm"` (default `"mmap"`).
#' @param workers Integer. Number of worker processes. If NULL, uses existing
#'   pool or creates one with `detectCores() - 1`.
#' @param chunk_size Integer. Shards to batch per worker dispatch (default 1).
#'   Higher values reduce RPC overhead but may hurt load balancing.
#' @param profile Execution profile: `"default"`, `"memory"` (aggressive recycling),
#'   or `"speed"` (minimal overhead). With `profile="speed"`, shard_map will
#'   automatically enable `dispatch_mode="shm_queue"` when possible for
#'   `shard_map(N, ...)` out-buffer workflows (scalar `N`, `chunk_size=1`),
#'   unless `dispatch_mode` is explicitly specified.
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
                      fun = NULL,
                      borrow = list(),
                      out = list(),
                      kernel = NULL,
                      scheduler_policy = NULL,
                      autotune = NULL,
                      dispatch_mode = c("rpc_chunked", "shm_queue"),
                      dispatch_opts = NULL,
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
  dispatch_mode_user_provided <- !missing(dispatch_mode)
  dispatch_mode <- if (dispatch_mode_user_provided) match.arg(dispatch_mode) else "rpc_chunked"
  if (is.null(dispatch_opts)) dispatch_opts <- list()
  if (!is.list(dispatch_opts)) stop("dispatch_opts must be NULL or a list", call. = FALSE)

  kernel_meta <- NULL
  if (!is.null(kernel)) {
    kernel <- as.character(kernel)
    km <- get_kernel(kernel)
    if (is.null(km)) stop("Unknown kernel: ", kernel, call. = FALSE)
    if (!is.null(fun)) stop("Provide either fun= or kernel=, not both", call. = FALSE)
    fun <- km$impl
    kernel_meta <- km
  }
  if (!is.function(fun)) stop("fun must be a function (or specify kernel=)", call. = FALSE)

  # Start timing
  start_time <- Sys.time()
  diag <- if (diagnostics) {
    list(
      start_time = start_time,
      health_checks = list(),
      shard_times = list(),
      worker_usage = list(),
      kernel = kernel %||% NULL,
      autotune = NULL,
      dispatch_mode = dispatch_mode
    )
  } else {
    NULL
  }

  # If the user passed an integer N, we can optionally do online autotuning
  # while generating shards in phases (no up-front huge shard list required).
  shards_is_scalar_n <- is.numeric(shards) && length(shards) == 1
  n_items <- if (shards_is_scalar_n) as.integer(shards) else NA_integer_

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

  # Convert integer to shard_descriptor if needed (after worker/profile resolution).
  if (shards_is_scalar_n) {
    if (is.na(n_items) || n_items < 1L) stop("shards must be >= 1", call. = FALSE)
  } else {
    if (!inherits(shards, "shard_descriptor")) {
      stop("shards must be a shard_descriptor or integer", call. = FALSE)
    }
  }

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

  # Low-ceremony fast path: profile="speed" will automatically use shm_queue
  # for scalar-N, chunk_size=1 out-buffer workflows unless dispatch_mode was
  # explicitly set by the user.
  if (!dispatch_mode_user_provided &&
      identical(profile, "speed") &&
      shards_is_scalar_n &&
      as.integer(chunk_size) == 1L &&
      length(out) > 0 &&
      taskq_supported()) {
    dispatch_mode <- "shm_queue"
    if (diagnostics) diag$dispatch_mode <- dispatch_mode
  }

  # Set seed in workers if specified
  if (!is.null(seed)) {
    # When shards are auto-generated online, we don't know num_shards yet.
    # Use total items as a stable substream spacing.
    set_worker_seeds(pool, seed, if (shards_is_scalar_n) n_items else shards$num_shards)
  }

  # Export borrowed inputs to workers (once, not per shard)
  export_borrow_to_workers(pool, borrow)

  # Export output buffer references if any
  if (length(out) > 0) {
    export_out_to_workers(pool, out)
  }

  # shm_queue fast mode: scalar N only, chunk_size=1, fire-and-forget.
  if (identical(dispatch_mode, "shm_queue")) {
    if (!taskq_supported()) {
      warning("dispatch_mode='shm_queue' not supported on this platform; falling back to rpc_chunked", call. = FALSE)
      dispatch_mode <- "rpc_chunked"
    } else {
    if (!shards_is_scalar_n) {
      stop("dispatch_mode='shm_queue' currently requires shard_map(N, ...) with scalar N", call. = FALSE)
    }
    if (as.integer(chunk_size) != 1L) {
      stop("dispatch_mode='shm_queue' currently requires chunk_size=1", call. = FALSE)
    }
    if (length(out) == 0) {
      warning("dispatch_mode='shm_queue' does not gather results; prefer using out= buffers/sinks.", call. = FALSE)
    }

    block_size <- dispatch_opts$block_size %||% autotune_block_size(
        n = n_items,
        workers = workers,
        min_shards_per_worker = 4L,
        max_shards_per_worker = 64L
      )
    block_size <- as.integer(block_size)
    if (is.na(block_size) || block_size < 1L) stop("dispatch_opts$block_size must be >= 1", call. = FALSE)

    shards <- shards_lazy(n_items, block_size = block_size)

    dispatch_result <- dispatch_shards_shm_queue_(
      n = n_items,
      block_size = block_size,
      fun = fun,
      borrow = borrow,
      out = out,
      pool = pool,
      max_retries = max_retries,
      timeout = timeout,
      queue_backing = dispatch_opts$queue_backing %||% "mmap"
    )

    results <- dispatch_result$results

    if (diagnostics) {
      diag$end_time <- Sys.time()
      diag$duration <- as.numeric(difftime(diag$end_time, diag$start_time, units = "secs"))
      diag$health_checks <- dispatch_result$diagnostics$health_checks %||% list()
      diag$shards_processed <- shards$num_shards
      diag$chunks_dispatched <- shards$num_shards
      diag$pool_stats <- dispatch_result$pool_stats
      diag$view_stats <- dispatch_result$diagnostics$view_stats %||% NULL
      diag$copy_stats <- dispatch_result$diagnostics$copy_stats %||% NULL
      diag$table_stats <- dispatch_result$diagnostics$table_stats %||% NULL
      diag$scratch_stats <- dispatch_result$diagnostics$scratch_stats %||% NULL
      diag$shm_queue <- dispatch_result$diagnostics$taskq %||% NULL
    }

    return(structure(
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
    ))
  }
  }

  # Create self-contained executor function for workers
  chunk_executor <- make_chunk_executor()

  # Optional: online shard sizing autotune for scalar-N sharding. This is opt-in
  # by default for shard_map(N, ...) (low ceremony), and off for precomputed
  # shard descriptors.
  autotune_mode <- NULL
  autotune_cfg <- NULL
  if (is.list(autotune)) {
    autotune_mode <- as.character(autotune$mode %||% "online")
    autotune_cfg <- autotune
  } else if (!is.null(autotune)) {
    autotune_mode <- if (isTRUE(autotune)) "online" else as.character(autotune)
  } else if (shards_is_scalar_n) {
    autotune_mode <- "online"
  } else {
    autotune_mode <- "none"
  }
  if (!nzchar(autotune_mode)) autotune_mode <- "none"

  if (shards_is_scalar_n && identical(autotune_mode, "online")) {
    tuned <- shard_map_online_(
      n = n_items,
      fun = fun,
      borrow = borrow,
      out = out,
      kernel_meta = kernel_meta,
      chunk_executor = chunk_executor,
      pool = pool,
      workers = workers,
      mem_cap = mem_cap,
      chunk_size = chunk_size,
      autotune_cfg = autotune_cfg,
      profile = profile,
      diagnostics = diagnostics,
      diag = diag,
      health_check_interval = health_check_interval,
      max_retries = max_retries,
      timeout = timeout,
      scheduler_policy = scheduler_policy
    )
    shards <- tuned$shards
    dispatch_result <- tuned$dispatch_result
    if (diagnostics) diag <- tuned$diag
    results <- tuned$results
  } else {
    # Create chunk batches if chunk_size > 1
    if (shards_is_scalar_n) {
      shards <- shards(n_items, workers = workers)
    }
    chunks <- create_shard_chunks(shards, chunk_size, fun, borrow, out, kernel_meta = kernel_meta)

    # Dispatch chunks to workers with supervision
    dispatch_result <- dispatch_chunks(
      chunks = chunks,
      fun = chunk_executor,
      pool = pool,
      health_check_interval = health_check_interval,
      max_retries = max_retries,
      timeout = timeout,
      scheduler_policy = scheduler_policy
    )

    # Flatten results if chunk_size > 1
    results <- if (chunk_size > 1L) {
      unlist(dispatch_result$results, recursive = FALSE)
    } else {
      dispatch_result$results
    }

    # Collect diagnostics
    if (diagnostics) {
      diag$end_time <- Sys.time()
      diag$duration <- as.numeric(difftime(diag$end_time, diag$start_time, units = "secs"))
      diag$health_checks <- dispatch_result$diagnostics$health_checks
      diag$shards_processed <- shards$num_shards
      diag$chunks_dispatched <- length(chunks)
      diag$pool_stats <- dispatch_result$pool_stats
      diag$view_stats <- dispatch_result$diagnostics$view_stats %||% NULL
      diag$copy_stats <- dispatch_result$diagnostics$copy_stats %||% NULL
      diag$table_stats <- dispatch_result$diagnostics$table_stats %||% NULL
      diag$scratch_stats <- dispatch_result$diagnostics$scratch_stats %||% NULL
      diag$scheduler <- dispatch_result$diagnostics$scheduler %||% NULL
    }
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

# Online shard sizing for scalar-N shard_map.
#
# This runs a few small phases to pick a reasonable block_size using observed
# wall time and worker RSS, then processes the remainder with the chosen size.
shard_map_online_ <- function(n,
                              fun,
                              borrow,
                              out,
                              kernel_meta,
                              chunk_executor,
                              pool,
                              workers,
                              mem_cap,
                              chunk_size,
                              autotune_cfg = NULL,
                              profile,
                              diagnostics,
                              diag,
                              health_check_interval,
                              max_retries,
                              timeout,
                              scheduler_policy) {
  n <- as.integer(n)
  if (is.na(n) || n < 1L) stop("n must be >= 1", call. = FALSE)

  # Conservative defaults; keep user-facing ceremony low by being predictable.
  cfg <- list(
    max_rounds = 3L,
    probe_shards_per_worker = 4L,
    min_shard_time = 0.02, # seconds; below this, overhead dominates -> grow block
    grow_factor = 2.0,
    shrink_factor = 0.5,
    rss_hi = 0.85,
    rss_lo = 0.50
  )
  if (is.list(autotune_cfg)) {
    for (nm in names(cfg)) {
      if (!is.null(autotune_cfg[[nm]])) cfg[[nm]] <- autotune_cfg[[nm]]
    }
  }

  # Initial block size from the existing heuristic.
  bs <- autotune_block_size(
    n = n,
    workers = workers,
    min_shards_per_worker = 4L,
    max_shards_per_worker = 64L,
    scratch_bytes_per_item = 0,
    scratch_budget = 0
  )

  cursor <- 1L
  shard_id <- 1L
  all_shards <- list()
  all_results <- list()
  all_failures <- list()

  # Aggregate dispatch diagnostics across phases.
  agg_diag <- list(
    health_checks = list(),
    view_stats = list(created = 0L, materialized = 0L, materialized_bytes = 0, packed = 0L, packed_bytes = 0),
    copy_stats = list(borrow_exports = 0L, borrow_bytes = 0, buffer_writes = 0L, buffer_bytes = 0),
    table_stats = list(writes = 0L, rows = 0L, bytes = 0),
    scratch_stats = list(hits = 0L, misses = 0L, high_water = 0),
    scheduler = list(throttle_events = 0L),
    chunks_dispatched = 0L
  )

  hist <- list()
  rounds <- 0L

  # Use chunk_size=1 for probe phases so timing per shard is meaningful.
  probe_chunk_size <- 1L

  while (cursor <= n) {
    # Probe in early rounds; afterward, use the run's requested chunk_size.
    is_probe <- rounds < cfg$max_rounds
    use_chunk_size <- if (is_probe) probe_chunk_size else chunk_size

    # Probe only a small prefix; after tuning, take bigger bites.
    target_shards <- if (is_probe) workers * cfg$probe_shards_per_worker else workers * 32L
    phase_items <- min(n - cursor + 1L, as.integer(bs) * as.integer(max(target_shards, 1L)))
    if (phase_items < 1L) phase_items <- 1L
    phase_end <- min(cursor + phase_items - 1L, n)

    phase_shards <- create_contiguous_shards_window_(
      start = cursor,
      end = phase_end,
      block_size = bs,
      start_id = shard_id
    )
    shard_id <- shard_id + length(phase_shards)

    all_shards <- c(all_shards, phase_shards)
    phase_desc <- structure(
      list(
        n = phase_end - cursor + 1L,
        block_size = as.integer(bs),
        strategy = "contiguous",
        num_shards = length(phase_shards),
        shards = phase_shards
      ),
      class = "shard_descriptor"
    )

    chunks <- create_shard_chunks(phase_desc, use_chunk_size, fun, borrow, out, kernel_meta = kernel_meta)

    rss_before <- tryCatch(mem_report(pool)$peak_rss, error = function(e) NA_real_)
    t0 <- proc.time()[["elapsed"]]
    dr <- dispatch_chunks(
      chunks = chunks,
      fun = chunk_executor,
      pool = pool,
      health_check_interval = health_check_interval,
      max_retries = max_retries,
      timeout = timeout,
      scheduler_policy = scheduler_policy
    )
    t1 <- proc.time()[["elapsed"]]
    rss_after <- tryCatch(mem_report(pool)$peak_rss, error = function(e) NA_real_)

    # Flatten phase results into per-shard results and append.
    phase_res <- if (use_chunk_size > 1L) unlist(dr$results, recursive = FALSE) else dr$results
    all_results <- c(all_results, phase_res)
    if (length(dr$failures)) all_failures <- c(all_failures, dr$failures)

    # Update aggregate diagnostics.
    agg_diag$health_checks <- c(agg_diag$health_checks, dr$diagnostics$health_checks %||% list())
    agg_diag$chunks_dispatched <- agg_diag$chunks_dispatched + length(chunks)
    if (is.list(dr$diagnostics$view_stats)) {
      for (k in names(agg_diag$view_stats)) agg_diag$view_stats[[k]] <- (agg_diag$view_stats[[k]] %||% 0) + (dr$diagnostics$view_stats[[k]] %||% 0)
    }
    if (is.list(dr$diagnostics$copy_stats)) {
      for (k in names(agg_diag$copy_stats)) agg_diag$copy_stats[[k]] <- (agg_diag$copy_stats[[k]] %||% 0) + (dr$diagnostics$copy_stats[[k]] %||% 0)
    }
    if (is.list(dr$diagnostics$table_stats)) {
      for (k in names(agg_diag$table_stats)) agg_diag$table_stats[[k]] <- (agg_diag$table_stats[[k]] %||% 0) + (dr$diagnostics$table_stats[[k]] %||% 0)
    }
    if (is.list(dr$diagnostics$scratch_stats)) {
      agg_diag$scratch_stats$hits <- (agg_diag$scratch_stats$hits %||% 0L) + (dr$diagnostics$scratch_stats$hits %||% 0L)
      agg_diag$scratch_stats$misses <- (agg_diag$scratch_stats$misses %||% 0L) + (dr$diagnostics$scratch_stats$misses %||% 0L)
      agg_diag$scratch_stats$high_water <- max(as.double(agg_diag$scratch_stats$high_water %||% 0), as.double(dr$diagnostics$scratch_stats$high_water %||% 0))
    }
    if (is.list(dr$diagnostics$scheduler)) {
      agg_diag$scheduler$throttle_events <- (agg_diag$scheduler$throttle_events %||% 0L) + as.integer(dr$diagnostics$scheduler$throttle_events %||% 0L)
    }

    # Phase metrics
    elapsed <- as.double(t1 - t0)
    items_done <- as.integer(phase_end - cursor + 1L)
    shards_done <- length(phase_shards)
    throughput <- if (elapsed > 0) as.double(items_done) / elapsed else NA_real_
    shard_time <- if (shards_done > 0) elapsed / as.double(shards_done) else NA_real_

    rss_peak <- suppressWarnings(max(c(rss_before, rss_after), na.rm = TRUE))
    rss_frac <- if (is.finite(rss_peak) && is.finite(mem_cap) && mem_cap > 0) rss_peak / mem_cap else NA_real_

    if (is_probe) rounds <- rounds + 1L
    hist[[length(hist) + 1L]] <- list(
      round = rounds,
      start = cursor,
      end = phase_end,
      block_size = as.integer(bs),
      chunk_size = as.integer(use_chunk_size),
      elapsed_sec = elapsed,
      items = items_done,
      shards = shards_done,
      throughput_items_per_sec = throughput,
      shard_time_sec = shard_time,
      rss_peak = rss_peak,
      rss_fraction_of_mem_cap = rss_frac
    )

    cursor <- phase_end + 1L

    # Update block_size for next probe phase using simple, safe heuristics.
    if (is_probe && cursor <= n) {
      bs_next <- bs
      reason <- "keep"

      # If we get close to mem_cap, shrink.
      if (is.finite(rss_frac) && rss_frac >= cfg$rss_hi) {
        bs_next <- max(as.integer(floor(as.double(bs) * cfg$shrink_factor)), 1L)
        reason <- "shrink_rss"
      } else if (is.finite(shard_time) && shard_time < cfg$min_shard_time) {
        # If shards are too tiny (overhead dominates), grow.
        bs_next <- as.integer(ceiling(as.double(bs) * cfg$grow_factor))
        bs_next <- min(bs_next, n)
        reason <- "grow_overhead"
      } else if (is.finite(rss_frac) && rss_frac <= cfg$rss_lo && is.finite(shard_time) && shard_time < (cfg$min_shard_time * 0.5)) {
        # Extra nudge: very low RSS and very small shard time.
        bs_next <- as.integer(ceiling(as.double(bs) * cfg$grow_factor))
        bs_next <- min(bs_next, n)
        reason <- "grow_low_rss"
      }

      # Record decision.
      hist[[length(hist)]]$decision <- reason
      hist[[length(hist)]]$next_block_size <- as.integer(bs_next)
      bs <- bs_next
    }
  }

  full_desc <- structure(
    list(
      n = n,
      block_size = NA_integer_,
      strategy = "contiguous",
      num_shards = length(all_shards),
      shards = all_shards
    ),
    class = "shard_descriptor"
  )

  # Produce a unified dispatch_result-like payload.
  dispatch_result <- structure(
    list(
      results = all_results,
      failures = all_failures,
      queue_status = list(
        total = agg_diag$chunks_dispatched,
        pending = 0L,
        in_flight = 0L,
        completed = agg_diag$chunks_dispatched - length(all_failures),
        failed = length(all_failures),
        total_retries = sum(vapply(all_failures, function(x) x$retry_count %||% 0L, integer(1)), na.rm = TRUE)
      ),
      diagnostics = list(
        health_checks = agg_diag$health_checks,
        view_stats = agg_diag$view_stats,
        copy_stats = agg_diag$copy_stats,
        table_stats = agg_diag$table_stats,
        scratch_stats = agg_diag$scratch_stats,
        scheduler = agg_diag$scheduler
      ),
      pool_stats = pool_get()$stats
    ),
    class = "shard_dispatch_result"
  )

  if (diagnostics) {
    diag$end_time <- Sys.time()
    diag$duration <- as.numeric(difftime(diag$end_time, diag$start_time, units = "secs"))
    diag$health_checks <- agg_diag$health_checks
    diag$shards_processed <- full_desc$num_shards
    diag$chunks_dispatched <- agg_diag$chunks_dispatched
    diag$pool_stats <- dispatch_result$pool_stats
    diag$view_stats <- agg_diag$view_stats
    diag$copy_stats <- agg_diag$copy_stats
    diag$table_stats <- agg_diag$table_stats
    diag$scratch_stats <- agg_diag$scratch_stats
    diag$scheduler <- agg_diag$scheduler
    diag$autotune <- list(mode = "online", history = hist)
  }

  list(
    shards = full_desc,
    results = all_results,
    dispatch_result = dispatch_result,
    diag = diag
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

  # Auto-share large atomic inputs once in the main process so PSOCK workers
  # can receive a small descriptor (via ALTREP serialization) instead of full
  # data copies.
  for (name in names(borrow)) {
    x <- borrow[[name]]

    if (is.atomic(x) && !is.null(x) &&
        typeof(x) %in% c("double", "integer", "logical", "raw") &&
        !is_shared_vector(x)) {
      # Build with cow='allow' so we can attach attributes, then lock down to
      # the requested policy.
      shared <- as_shared(x, readonly = TRUE, backing = "auto", cow = "allow")

      # Preserve non-class attributes (dim, dimnames, names, tsp, etc).
      attrs <- attributes(x)
      x_class <- attr(x, "class")
      attrs$class <- NULL
      if (length(attrs)) {
        for (nm in names(attrs)) {
          attr(shared, nm) <- attrs[[nm]]
        }
      }
      class(shared) <- unique(c("shard_shared_vector", x_class))
      attr(shared, "shard_cow") <- cow

      borrow[[name]] <- shared
      x <- shared
    }

    # Best-effort tag for downstream diagnostics.
    if (!is_shared_vector(x)) {
      attr(borrow[[name]], "shard_cow") <- cow
    } else {
      existing <- attr(x, "shard_cow", exact = TRUE)
      if (!is.null(existing) && is.character(existing) &&
          length(existing) == 1L && !identical(existing, cow)) {
        warning("Borrowed input '", name, "' has shard_cow='", existing,
                "' but shard_map(cow='", cow, "') was requested. Using '",
                existing, "'.", call. = FALSE)
      }
    }
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

  bad <- vapply(out, function(x) {
    !(inherits(x, "shard_buffer") ||
        inherits(x, "shard_table_buffer") ||
        inherits(x, "shard_table_sink"))
  }, logical(1))
  if (any(bad)) {
    stop("All outputs must be shard_buffer, shard_table_buffer, or shard_table_sink objects.",
         call. = FALSE)
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

  # Export reopenable descriptors rather than shard_buffer objects. The raw
  # segment externalptr does not survive PSOCK serialization.
  describe_one <- function(obj) {
    if (inherits(obj, "shard_buffer")) {
      info <- buffer_info(obj)
      return(list(
        kind = "buffer",
        path = info$path,
        backing = info$backing,
        type = info$type,
        dim = info$dim
      ))
    }

    if (inherits(obj, "shard_table_buffer")) {
      cols <- lapply(obj$columns, function(buf) {
        info <- buffer_info(buf)
        list(path = info$path, backing = info$backing, type = info$type, dim = info$dim)
      })
      return(list(
        kind = "table_buffer",
        schema = obj$schema,
        nrow = obj$nrow,
        backing = obj$backing,
        columns = cols
      ))
    }

    if (inherits(obj, "shard_table_sink")) {
      return(list(
        kind = "table_sink",
        schema = obj$schema,
        mode = obj$mode,
        path = obj$path,
        format = obj$format
      ))
    }

    stop("Unsupported out object type", call. = FALSE)
  }

  out_desc <- lapply(out, describe_one)

  export_env <- new.env(parent = emptyenv())
  export_env$.shard_out <- out_desc

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
create_shard_chunks <- function(shards, chunk_size, fun, borrow, out, kernel_meta = NULL) {
  chunk_size <- max(as.integer(chunk_size), 1L)
  num_chunks <- ceiling(shards$num_shards / chunk_size)

  chunks <- vector("list", num_chunks)

  borrow_names <- names(borrow)
  out_names <- names(out)

  classify_bytes <- function(bytes) {
    bytes <- as.double(bytes)
    if (!is.finite(bytes) || is.na(bytes)) return("tiny")
    if (bytes >= 64 * 1024^2) return("huge")
    if (bytes >= 8 * 1024^2) return("medium")
    "tiny"
  }

  for (i in seq_len(num_chunks)) {
    start_idx <- (i - 1L) * chunk_size + 1L
    end_idx <- min(i * chunk_size, shards$num_shards)

    chunk_shards <- shards$shards[start_idx:end_idx]

    # Optional footprint hint for memory-aware scheduling.
    fp_class <- NULL
    fp_bytes <- NULL
    if (!is.null(kernel_meta) && !is.null(kernel_meta$footprint)) {
      fp <- kernel_meta$footprint
      if (is.numeric(fp) && length(fp) == 1L) {
        fp_bytes <- as.double(fp)
        fp_class <- classify_bytes(fp_bytes)
      } else if (is.function(fp)) {
        vals <- lapply(chunk_shards, function(s) {
          tryCatch(fp(s), error = function(e) NULL)
        })
        # Accept either numeric bytes or list(class=..., bytes=...).
        bytes <- vapply(vals, function(v) {
          if (is.null(v)) return(NA_real_)
          if (is.numeric(v)) return(as.double(v[[1]]))
          if (is.list(v) && !is.null(v$bytes)) return(as.double(v$bytes))
          NA_real_
        }, numeric(1))
        fp_bytes <- suppressWarnings(max(bytes, na.rm = TRUE))
        if (!is.finite(fp_bytes)) fp_bytes <- NULL
        cls <- vapply(vals, function(v) {
          if (is.list(v) && !is.null(v$class)) as.character(v$class) else NA_character_
        }, character(1))
        cls <- cls[!is.na(cls) & nzchar(cls)]
        fp_class <- if (length(cls) > 0) cls[[1]] else if (!is.null(fp_bytes)) classify_bytes(fp_bytes) else NULL
      }
    }

    chunks[[i]] <- list(
      id = i,
      shard_ids = start_idx:end_idx,
      shards = chunk_shards,
      fun = fun,
      borrow_names = borrow_names,
      out_names = out_names,
      footprint_class = fp_class,
      footprint_bytes = fp_bytes
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

    out_desc <- if (exists(".shard_out", envir = globalenv())) {
      get(".shard_out", envir = globalenv())
    } else {
      list()
    }

    # Lazily open output buffers once per worker process and cache them.
    out <- list()
    if (length(out_desc) > 0) {
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
      # `do.call()` has a sharp edge: if an argument value is a language object,
      # it will be spliced into the call and evaluated (surprising for "data"
      # being passed through borrow/out). Using quote=TRUE ensures language
      # objects are passed as values, not executed as code.
      do.call(fun, args, quote = TRUE)
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

  if (inherits(res, "shard_results_placeholder")) {
    # Avoid unlist() on a placeholder (would allocate enormous objects).
    return(res)
  }

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
  if (inherits(x, "shard_result") || inherits(x, "shard_reduce_result")) {
    return(length(x$failures) == 0)
  }
  stop("x must be a shard_result or shard_reduce_result object", call. = FALSE)
}
