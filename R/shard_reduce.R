#' @title Streaming Reductions over Shards
#' @description Reduce shard results without gathering all per-shard returns on the master.
#' @name shard_reduce
NULL

#' Reduce over shards without gathering
#'
#' `shard_reduce()` executes `map()` over shards in parallel and combines results
#' using an associative `combine()` function. Unlike `shard_map()`, it does not
#' accumulate all per-shard results on the master; it streams partials as chunks
#' complete.
#'
#' For performance and memory efficiency, reduction is performed in two stages:
#' 1) per-chunk partial reduction inside each worker, and
#' 2) streaming combine of partials on the master.
#'
#' @param shards A `shard_descriptor` from [shards()], or an integer N.
#' @param map Function executed per shard. Receives shard descriptor as first
#'   argument, followed by borrowed inputs and outputs.
#' @param combine Function `(acc, value) -> acc` used to combine results. Should
#'   be associative for deterministic behavior under chunking.
#' @param init Initial accumulator value.
#' @param borrow Named list of shared inputs (same semantics as [shard_map()]).
#' @param out Named list of output buffers/sinks (same semantics as [shard_map()]).
#' @param workers Number of worker processes.
#' @param chunk_size Shards to batch per worker dispatch (default 1).
#' @param profile Execution profile (same semantics as [shard_map()]).
#' @param mem_cap Memory cap per worker (same semantics as [shard_map()]).
#' @param recycle Worker recycling policy (same semantics as [shard_map()]).
#' @param cow Copy-on-write policy for borrowed inputs (same semantics as [shard_map()]).
#' @param seed RNG seed for reproducibility.
#' @param diagnostics Logical; collect diagnostics (default TRUE).
#' @param packages Additional packages to load in workers.
#' @param init_expr Expression to evaluate in each worker on startup.
#' @param timeout Seconds to wait for each chunk.
#' @param max_retries Maximum retries per chunk.
#' @param health_check_interval Check worker health every N completions.
#'
#' @return A `shard_reduce_result` with fields:
#'   - `value`: final accumulator
#'   - `failures`: any permanently failed chunks
#'   - `diagnostics`: run telemetry including reduction stats
#'   - `queue_status`, `pool_stats`
#' @export
#' @examples
#' \dontrun{
#' # Sum 1..1e6 without materializing 1e6 per-shard results:
#' res <- shard_reduce(
#'   shards(1e6, block_size = 1000),
#'   map = function(s) sum(s$idx),
#'   combine = function(acc, x) acc + x,
#'   init = 0,
#'   workers = 4
#' )
#' res$value
#' }
shard_reduce <- function(shards,
                         map,
                         combine,
                         init,
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

  if (!is.function(map)) stop("map must be a function", call. = FALSE)
  if (!is.function(combine)) stop("combine must be a function", call. = FALSE)

  start_time <- Sys.time()
  diag <- if (diagnostics) {
    list(
      start_time = start_time,
      kernel = NULL,
      reduce = list(partials = 0L, combine_steps = 0L, partial_max_bytes = 0)
    )
  } else NULL

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

  # Profile settings
  settings <- get_profile_settings(profile, mem_cap, recycle)

  # Ensure pool exists
  pool <- ensure_pool(workers, settings$mem_cap, settings$rss_drift_threshold, packages, init_expr)

  # Validate and export borrow/out
  borrow <- validate_borrow(borrow, cow)
  out <- validate_out(out)
  export_borrow_to_workers(pool, borrow)
  export_out_to_workers(pool, out)

  if (!is.null(seed)) {
    set_worker_seeds(pool, as.integer(seed), shards$num_shards)
  }

  # Create chunk descriptors. We reuse the same chunk format as shard_map, but
  # the executor will reduce inside the worker and return a single partial.
  chunks <- create_shard_chunks(shards, chunk_size, fun = map, borrow = borrow, out = out, kernel_meta = NULL)

  # Worker-side chunk reducer.
  chunk_reducer <- local({
    combine_fun <- combine
    init_val <- init
    function(chunk) {
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

      map_fun <- chunk$fun
      borrow_names <- chunk$borrow_names
      out_names <- chunk$out_names

      acc <- init_val
      for (shard in chunk$shards) {
        args <- list(shard)
        for (name in borrow_names) args[[name]] <- borrow[[name]]
        for (name in out_names) args[[name]] <- out[[name]]
        val <- do.call(map_fun, args, quote = TRUE)
        acc <- combine_fun(acc, val)
      }
      acc
    }
  })

  # Master-side streaming reducer: combine partials as chunks complete.
  acc <- init
  partial_bytes_max <- 0
  partial_count <- 0L
  combine_steps <- 0L

  on_partial <- function(tag, value, worker_id) {
    partial_count <<- partial_count + 1L
    combine_steps <<- combine_steps + 1L
    bs <- tryCatch(as.double(utils::object.size(value)), error = function(e) 0)
    if (is.finite(bs) && bs > partial_bytes_max) partial_bytes_max <<- bs
    acc <<- combine(acc, value)
    invisible(NULL)
  }

  dispatch_result <- dispatch_chunks(
    chunks,
    chunk_reducer,
    pool = pool,
    health_check_interval = health_check_interval,
    max_retries = max_retries,
    timeout = timeout,
    on_result = on_partial,
    store_results = FALSE,
    retain_chunks = FALSE
  )

  if (diagnostics) {
    diag$end_time <- Sys.time()
    diag$duration <- as.numeric(difftime(diag$end_time, diag$start_time, units = "secs"))
    diag$reduce$partials <- partial_count
    diag$reduce$combine_steps <- combine_steps
    diag$reduce$partial_max_bytes <- partial_bytes_max
    # Plumb runtime stats (views/scratch/scheduler/etc)
    diag$view_stats <- dispatch_result$diagnostics$view_stats %||% NULL
    diag$copy_stats <- dispatch_result$diagnostics$copy_stats %||% NULL
    diag$table_stats <- dispatch_result$diagnostics$table_stats %||% NULL
    diag$scratch_stats <- dispatch_result$diagnostics$scratch_stats %||% NULL
    diag$scheduler <- dispatch_result$diagnostics$scheduler %||% NULL
  }

  structure(
    list(
      value = acc,
      failures = dispatch_result$failures,
      shards = shards,
      diagnostics = diag,
      queue_status = dispatch_result$queue_status,
      pool_stats = dispatch_result$pool_stats,
      cow_policy = cow,
      profile = profile
    ),
    class = "shard_reduce_result"
  )
}

#' @export
print.shard_reduce_result <- function(x, ...) {
  cat("shard_reduce result\n")
  if (!is.null(x$diagnostics)) {
    cat("  Duration:", sprintf("%.2f seconds", x$diagnostics$duration %||% NA_real_), "\n")
    r <- x$diagnostics$reduce %||% list()
    cat("  Partials:", r$partials %||% 0L, "\n")
    cat("  Partial max bytes:", format_bytes(r$partial_max_bytes %||% 0), "\n")
  }
  if (length(x$failures) > 0) {
    cat("  Failed chunks:", length(x$failures), "\n")
  }
  invisible(x)
}

