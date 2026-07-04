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
#' 2) streaming combine of partials on the master, folded in chunk order.
#'
#' @section Initial value semantics:
#' `init` is combined exactly once, on the master, at the start of the final
#' fold: the result is `combine(combine(combine(init, p1), p2), ...)` where
#' `p1, p2, ...` are per-chunk partials in chunk order. Worker-side partials
#' are built without `init`: each chunk's partial starts from the chunk's
#' first mapped value. A non-neutral `init` (e.g. `init = 10` with `+`)
#' therefore contributes exactly once, regardless of `chunk_size` or
#' `workers`. This requires `combine` to be associative and able to combine
#' two mapped values (not just an accumulator with a value).
#'
#' @param shards A `shard_descriptor` from [shards()], or an integer N.
#' @param map Function executed per shard. Receives shard descriptor as first
#'   argument, followed by borrowed inputs and outputs.
#' @param combine Function `(acc, value) -> acc` used to combine results. Must
#'   be associative, and must accept two mapped values as arguments (worker
#'   partials start from a chunk's first mapped value; see
#'   *Initial value semantics*).
#' @param init Initial accumulator value, combined exactly once on the master
#'   (see *Initial value semantics*).
#' @param borrow Named list of shared inputs (same semantics as [shard_map()]).
#' @param out Named list of output buffers/sinks (same semantics as [shard_map()]).
#' @param workers Number of worker processes.
#' @param chunk_size Shards to batch per worker dispatch. The default
#'   `"auto"` targets roughly four chunks per worker,
#'   `max(1, ceiling(num_shards / (workers * 4)))`, which amortizes dispatch
#'   round trips while retaining load balance. Supply an integer to control
#'   batching explicitly.
#' @param profile Execution profile (same semantics as [shard_map()]).
#' @param mem_cap Memory cap per worker (same semantics as [shard_map()]).
#' @param recycle Worker recycling policy (same semantics as [shard_map()]).
#' @param cow Copy-on-write policy for borrowed inputs (same semantics as [shard_map()]).
#' @param seed RNG seed for reproducibility. When non-`NULL`, one independent
#'   L'Ecuyer-CMRG stream per shard is derived on the master and installed in
#'   the worker immediately before each `map()` call, so per-shard RNG draws
#'   are reproducible regardless of worker count, `chunk_size`, or dynamic
#'   shard-to-worker assignment. The master's RNG state and `RNGkind()` are
#'   left exactly as found (`seed = NULL` touches no RNG state). Note on
#'   floating-point results: partials are combined in chunk order, which is
#'   deterministic given identical chunking, so a given
#'   (`seed`, chunking, `chunk_size`) is exactly reproducible for any
#'   `workers=`; across *different* `chunk_size` values the combine order
#'   differs, so non-associative floating-point rounding may differ even
#'   though the per-shard RNG draws are identical. When `shards` is a scalar
#'   N and `seed` is set, the shard decomposition is chosen independently of
#'   the worker count so the same seed gives identical results for any
#'   `workers=`.
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
#' \donttest{
#' res <- shard_reduce(
#'   100L,
#'   map = function(s) sum(s$idx),
#'   combine = function(acc, x) acc + x,
#'   init = 0,
#'   workers = 2
#' )
#' pool_stop()
#' res$value
#' }
shard_reduce <- function(shards,
                         map,
                         combine,
                         init,
                         borrow = list(),
                         out = list(),
                         workers = NULL,
                         chunk_size = "auto",
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

  # Convert integer to shard_descriptor if needed. With seed= the
  # decomposition must not depend on the worker count, otherwise per-shard
  # RNG streams cannot give identical results across workers= (same
  # deterministic scheme as shard_map).
  if (is.numeric(shards) && length(shards) == 1) {
    n_items <- as.integer(shards)
    shards <- if (!is.null(seed)) {
      shards(n_items, block_size = seed_block_size_(n_items))
    } else {
      shards(n_items, workers = workers)
    }
  }
  if (!inherits(shards, "shard_descriptor")) {
    stop("shards must be a shard_descriptor or integer", call. = FALSE)
  }

  # Determine worker count
  if (is.null(workers)) {
    pool <- pool_get()
    workers <- if (!is.null(pool)) pool$n else .default_workers()
  }
  workers <- max(as.integer(workers), 1L)

  # Resolve chunk size. "auto" targets ~4 chunks per worker so two-stage
  # reduction actually batches (chunk_size = 1 degenerates to per-shard
  # round trips) while keeping enough chunks for load balancing.
  if (identical(chunk_size, "auto")) {
    chunk_size <- max(1L, as.integer(ceiling(shards$num_shards / (workers * 4))))
  } else {
    chunk_size <- max(as.integer(chunk_size), 1L)
  }

  # Profile settings
  settings <- get_profile_settings(profile, mem_cap, recycle)

  # Ensure pool exists
  pool <- ensure_pool(workers, settings$mem_cap, settings$rss_drift_threshold, packages, init_expr)

  # Validate and export borrow/out
  borrow <- validate_borrow(borrow, cow)
  auto_shared_names <- attr(borrow, "auto_shared")
  if (length(auto_shared_names) > 0) {
    on.exit({
      for (nm in auto_shared_names) {
        tryCatch(close(borrow[[nm]]), error = function(e) NULL)
      }
    }, add = TRUE)
  }
  out <- validate_out(out)
  export_borrow_to_workers(pool, borrow)
  export_out_to_workers(pool, out)

  # Per-shard RNG streams (7 ints each), computed once on the master. The
  # master's .Random.seed / RNGkind are saved and restored inside
  # make_shard_seed_streams_(); seed = NULL touches no RNG state.
  seed_streams <- if (!is.null(seed)) {
    make_shard_seed_streams_(seed, shards$num_shards)
  } else {
    NULL
  }

  # Create chunk descriptors. We reuse the same chunk format as shard_map, but
  # the executor will reduce inside the worker and return a single partial.
  # The map function is NOT embedded per chunk; it travels once per dispatch
  # inside the chunk_reducer closure below (.shard_dispatch_fun channel).
  chunks <- create_shard_chunks(shards, chunk_size, borrow = borrow, out = out,
                                kernel_meta = NULL, seed_streams = seed_streams)

  # Worker-side chunk reducer with a genuinely minimal environment: only the
  # map and combine functions, enclosed by the package namespace (never the
  # shard_reduce frame, which holds the full shards + chunks lists).
  chunk_reducer <- make_chunk_reducer_(map, combine)

  # Master-side streaming reducer. Partials are folded in chunk order (not
  # arrival order) so results are deterministic given identical chunking:
  # out-of-order partials are stashed until their predecessors arrive, and
  # `init` is combined exactly once at the start of the fold.
  acc <- init
  partial_bytes_max <- 0
  partial_count <- 0L
  combine_steps <- 0L
  next_chunk_id <- 1L
  pending <- new.env(parent = emptyenv())

  fold_partial <- function(value) {
    acc <<- combine(acc, value)
    combine_steps <<- combine_steps + 1L
  }

  on_partial <- function(tag, value, worker_id) {
    partial_count <<- partial_count + 1L
    bs <- tryCatch(as.double(utils::object.size(value)), error = function(e) 0)
    if (is.finite(bs) && bs > partial_bytes_max) partial_bytes_max <<- bs

    id <- suppressWarnings(as.integer(tag))
    if (is.na(id)) {
      # Untagged partial (defensive): fold in arrival order.
      fold_partial(value)
      return(invisible(NULL))
    }
    if (id == next_chunk_id) {
      fold_partial(value)
      next_chunk_id <<- next_chunk_id + 1L
      # Drain any stashed successors that are now in order.
      repeat {
        key <- as.character(next_chunk_id)
        if (!exists(key, envir = pending, inherits = FALSE)) break
        stashed <- get(key, envir = pending, inherits = FALSE)
        rm(list = key, envir = pending)
        fold_partial(stashed$value)
        next_chunk_id <<- next_chunk_id + 1L
      }
    } else {
      # Wrap so NULL partials survive storage.
      assign(as.character(id), list(value = value), envir = pending)
    }
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
    retain_chunks = FALSE,
    diagnostics = diagnostics
  )

  # If some chunks failed permanently, their successors are still stashed:
  # fold the remainder in ascending chunk order.
  leftover <- ls(pending, all.names = TRUE)
  if (length(leftover) > 0) {
    for (key in as.character(sort(as.integer(leftover)))) {
      fold_partial(get(key, envir = pending, inherits = FALSE)$value)
    }
  }

  if (diagnostics) {
    diag$end_time <- Sys.time()
    diag$duration <- as.numeric(difftime(diag$end_time, diag$start_time, units = "secs"))
    diag$reduce$partials <- partial_count
    diag$reduce$combine_steps <- combine_steps
    diag$reduce$partial_max_bytes <- partial_bytes_max
    diag$reduce$chunk_size <- chunk_size
    diag$reduce$num_chunks <- length(chunks)
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

#' Create Worker-Side Chunk Reducer
#'
#' Builds the function dispatched to workers by [shard_reduce()]. The
#' returned closure's environment is this constructor's frame, which contains
#' exactly `map_fun` and `combine_fun` and is enclosed by the package
#' namespace — it never captures the `shard_reduce()` frame (shards, chunks,
#' borrow, ...), so the serialized reducer payload is small and independent
#' of the number of shards.
#'
#' The reducer folds a chunk's mapped values WITHOUT the user's `init`: the
#' accumulator starts from the chunk's first mapped value, and `init` is
#' combined exactly once on the master. Per-shard RNG streams attached to the
#' chunk (`chunk$rng_streams`, seed= reproducibility) are installed
#' immediately before each `map_fun` call, mirroring shard_map's executor.
#'
#' @param map_fun User map function (travels to workers once per dispatch via
#'   the `.shard_dispatch_fun` channel, inside this closure).
#' @param combine_fun User combine function.
#' @return A function of one argument (`chunk`) returning the chunk partial.
#' @keywords internal
#' @noRd
make_chunk_reducer_ <- function(map_fun, combine_fun) {
  force(map_fun)
  force(combine_fun)
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
      if (!exists(".shard_out_opened", envir = .shard_worker_env, inherits = FALSE)) {
        assign(".shard_out_opened", new.env(parent = emptyenv()), envir = .shard_worker_env)
      }
      opened <- get(".shard_out_opened", envir = .shard_worker_env)
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

    borrow_names <- chunk$borrow_names
    out_names <- chunk$out_names

    # Per-shard RNG streams (seed= reproducibility). The stream for shard k
    # is installed immediately before invoking map on shard k, so RNG draws
    # do not depend on which worker runs the shard or in what order.
    rng_streams <- chunk$rng_streams

    acc <- NULL
    have_acc <- FALSE
    for (k in seq_along(chunk$shards)) {
      shard <- chunk$shards[[k]]
      args <- list(shard)
      for (name in borrow_names) args[[name]] <- borrow[[name]]
      for (name in out_names) args[[name]] <- out[[name]]

      if (!is.null(rng_streams) && k <= length(rng_streams) && !is.null(rng_streams[[k]])) {
        assign(".Random.seed", rng_streams[[k]], envir = globalenv())
      }

      val <- do.call(map_fun, args, quote = TRUE)
      if (have_acc) {
        acc <- combine_fun(acc, val)
      } else {
        acc <- val
        have_acc <- TRUE
      }
    }
    acc
  }
}

#' Print a shard_reduce_result Object
#'
#' @param x A \code{shard_reduce_result} object.
#' @param ... Further arguments (ignored).
#' @return The input \code{x}, invisibly.
#' @export
#' @examples
#' \donttest{
#' res <- shard_reduce(4L, map = function(s) sum(s$idx),
#'   combine = `+`, init = 0, workers = 2)
#' pool_stop()
#' print(res)
#' }
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
