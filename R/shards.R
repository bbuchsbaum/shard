#' @title Shard Descriptor Creation
#' @description Create shard descriptors for parallel execution with autotuning.
#' @name shards
NULL

#' Create Shard Descriptors
#'
#' Produces shard descriptors (index ranges) for use with `shard_map()`.
#' Supports autotuning based on worker count and memory constraints.
#'
#' @param n Integer. Total number of items to shard.
#' @param block_size Block size specification. Can be:
#'   - `"auto"` (default): Autotune based on worker count
#'   - Integer: Explicit number of items per shard
#'   - Character: Human-readable like `"1K"`, `"10K"`
#' @param workers Integer. Number of workers for autotuning (default: pool size or detectCores - 1).
#' @param strategy Sharding strategy: `"contiguous"` (default) or `"strided"`.
#' @param min_shards_per_worker Integer. Minimum shards per worker for load balancing (default 4).
#' @param max_shards_per_worker Integer. Maximum shards per worker to limit overhead (default 64).
#' @param scratch_bytes_per_item Numeric. Expected scratch memory per item for memory budgeting.
#' @param scratch_budget Character or numeric. Total scratch memory budget (e.g., "1GB").
#'
#' @return A `shard_descriptor` object containing:
#'   - `n`: Total items
#'   - `block_size`: Computed block size
#'   - `strategy`: Strategy used
#'   - `shards`: List of shard descriptors with `id`, `start`, `end`, `idx` fields
#'
#' @export
#' @examples
#' # Autotune for 8 workers
#' blocks <- shards(1e6, workers = 8)
#' length(blocks$shards)  # Number of shards
#'
#' # Explicit block size
#' blocks <- shards(1000, block_size = 100)
#'
#' # Access shard indices
#' blocks$shards[[1]]$idx  # Indices for first shard
shards <- function(n,
                   block_size = "auto",
                   workers = NULL,
                   strategy = c("contiguous", "strided"),
                   min_shards_per_worker = 4L,
                   max_shards_per_worker = 64L,
                   scratch_bytes_per_item = 0,
                   scratch_budget = 0) {
  # Validate n
  n <- as.integer(n)
  if (is.na(n) || n < 1L) {
    stop("shards: n must be a positive integer", call. = FALSE)
  }

  strategy <- match.arg(strategy)

  # Determine worker count for autotuning
  if (is.null(workers)) {
    pool <- pool_get()
    workers <- if (!is.null(pool)) pool$n else (parallel::detectCores() - 1L)
  }
  workers <- max(as.integer(workers), 1L)

  # Parse block_size
  if (identical(block_size, "auto")) {
    block_size <- autotune_block_size(
      n = n,
      workers = workers,
      min_shards_per_worker = min_shards_per_worker,
      max_shards_per_worker = max_shards_per_worker,
      scratch_bytes_per_item = scratch_bytes_per_item,
      scratch_budget = scratch_budget
    )
  } else if (is.character(block_size)) {
    block_size <- parse_count(block_size)
  } else {
    block_size <- as.integer(block_size)
  }

  if (is.na(block_size) || block_size < 1L) {
    stop("shards: block_size must be a positive integer", call. = FALSE)
  }

  # Create shards based on strategy
  if (strategy == "contiguous") {
    shard_list <- create_contiguous_shards(n, block_size)
  } else {
    num_shards <- ceiling(n / block_size)
    shard_list <- create_strided_shards(n, num_shards)
  }

  structure(
    list(
      n = n,
      block_size = block_size,
      strategy = strategy,
      num_shards = length(shard_list),
      shards = shard_list
    ),
    class = "shard_descriptor"
  )
}

#' Create Shards from an Explicit Index List
#'
#' Constructs a `shard_descriptor` from a user-supplied list of index vectors.
#' This is useful for non-contiguous workloads like searchlights/feature sets
#' where each shard operates on an arbitrary subset.
#'
#' @param idxs List of integer vectors (1-based indices). Each element becomes
#'   one shard with fields `id`, `idx`, and `len`.
#' @return A `shard_descriptor`.
#' @export
shards_list <- function(idxs) {
  if (!is.list(idxs)) stop("idxs must be a list", call. = FALSE)

  shards <- vector("list", length(idxs))
  for (i in seq_along(idxs)) {
    idx <- idxs[[i]]
    if (!is.numeric(idx) || is.null(idx)) stop("idxs[[", i, "]] must be a numeric/integer vector", call. = FALSE)
    idx <- as.integer(idx)
    if (length(idx) < 1L) stop("idxs[[", i, "]] must be non-empty", call. = FALSE)
    if (anyNA(idx)) stop("idxs[[", i, "]] must not contain NA", call. = FALSE)
    if (any(idx < 1L)) stop("idxs[[", i, "]] must be >= 1", call. = FALSE)

    shards[[i]] <- list(id = i, idx = idx, len = length(idx))
  }

  structure(
    list(
      n = length(shards),
      block_size = NA_integer_,
      strategy = "list",
      num_shards = length(shards),
      shards = shards
    ),
    class = "shard_descriptor"
  )
}

#' Autotune Block Size
#'
#' Determines optimal block size based on worker count and constraints.
#'
#' @param n Total items.
#' @param workers Number of workers.
#' @param min_shards_per_worker Minimum shards per worker.
#' @param max_shards_per_worker Maximum shards per worker.
#' @param scratch_bytes_per_item Scratch bytes per item.
#' @param scratch_budget Total scratch budget.
#' @return Integer block size.
#' @keywords internal
autotune_block_size <- function(n, workers,
                                min_shards_per_worker = 4L,
                                max_shards_per_worker = 64L,
                                scratch_bytes_per_item = 0,
                                scratch_budget = 0) {
  if (workers < 1L) workers <- 1L

  # Target shard counts

  min_shards <- workers * min_shards_per_worker
  max_shards <- workers * max_shards_per_worker

  # Block size from target shard counts
  block_from_min <- ceiling(n / min_shards)
  block_from_max <- max(floor(n / max_shards), 1L)

  # Start with block size for good parallelism

  block_size <- max(block_from_min, 1L)

  # Apply memory budget constraint if specified
  if (scratch_bytes_per_item > 0 && scratch_budget > 0) {
    if (is.character(scratch_budget)) {
      scratch_budget <- parse_bytes(scratch_budget)
    }
    max_items_per_budget <- scratch_budget / scratch_bytes_per_item
    memory_constrained_block <- floor(max_items_per_budget / workers)
    if (memory_constrained_block > 0 && memory_constrained_block < block_size) {
      block_size <- memory_constrained_block
    }
  }

  # Ensure shard count is in range
  num_shards <- ceiling(n / block_size)
  if (num_shards < min_shards && block_from_min > 1L) {
    block_size <- block_from_min
  }
  if (num_shards > max_shards && block_from_max > 0L) {
    block_size <- max(block_from_max, block_size)
  }

  max(as.integer(block_size), 1L)
}

#' Create Contiguous Shards
#'
#' Creates shards with consecutive indices.
#'
#' @param n Total items.
#' @param block_size Items per shard.
#' @return List of shard descriptors.
#' @keywords internal
create_contiguous_shards <- function(n, block_size) {
  num_shards <- ceiling(n / block_size)
  shards <- vector("list", num_shards)

  start <- 1L
  for (i in seq_len(num_shards)) {
    end <- min(start + block_size - 1L, n)
    shards[[i]] <- list(
      id = i,
      start = start,
      end = end,
      idx = start:end,
      len = end - start + 1L
    )
    start <- end + 1L
  }

  shards
}

create_contiguous_shards_window_ <- function(start, end, block_size, start_id = 1L) {
  start <- as.integer(start)
  end <- as.integer(end)
  block_size <- as.integer(block_size)
  start_id <- as.integer(start_id)

  if (is.na(start) || is.na(end) || start < 1L || end < start) {
    stop("Invalid shard window (start/end)", call. = FALSE)
  }
  if (is.na(block_size) || block_size < 1L) stop("block_size must be >= 1", call. = FALSE)
  if (is.na(start_id) || start_id < 1L) stop("start_id must be >= 1", call. = FALSE)

  n <- end - start + 1L
  num_shards <- ceiling(n / block_size)
  shards <- vector("list", num_shards)

  cur <- start
  id <- start_id
  for (i in seq_len(num_shards)) {
    e <- min(cur + block_size - 1L, end)
    shards[[i]] <- list(
      id = id,
      start = cur,
      end = e,
      idx = cur:e,
      len = e - cur + 1L
    )
    cur <- e + 1L
    id <- id + 1L
  }
  shards
}

#' Create Strided Shards
#'
#' Creates shards with interleaved indices.
#'
#' @param n Total items.
#' @param num_shards Number of shards.
#' @return List of shard descriptors.
#' @keywords internal
create_strided_shards <- function(n, num_shards) {
  num_shards <- max(min(num_shards, n), 1L)
  shards <- vector("list", num_shards)

  for (i in seq_len(num_shards)) {
    # Indices: i, i+num_shards, i+2*num_shards, ...
    idx <- seq(from = i, to = n, by = num_shards)
    shards[[i]] <- list(
      id = i,
      start = i,
      stride = num_shards,
      idx = idx,
      len = length(idx)
    )
  }

  shards
}

#' Parse Count String
#'
#' Parses strings like "1K", "10K", "1M" to integers.
#'
#' @param x Character string.
#' @return Integer value.
#' @keywords internal
parse_count <- function(x) {
  if (is.numeric(x)) return(as.integer(x))

  x <- toupper(trimws(x))
  match <- regexec("^([0-9.]+)\\s*(K|M|B)?$", x)
  parts <- regmatches(x, match)[[1]]

  if (length(parts) < 2) {
    stop("Cannot parse count string: ", x, call. = FALSE)
  }

  value <- as.numeric(parts[2])
  unit <- if (length(parts) >= 3) parts[3] else ""

  multiplier <- switch(
    unit,
    "K" = 1000L,
    "M" = 1000000L,
    "B" = 1000000000L,
    1L
  )

  as.integer(value * multiplier)
}

#' @export
print.shard_descriptor <- function(x, ...) {
  cat("shard descriptor\n")
  cat("  Items:", format(x$n, big.mark = ","), "\n")
  cat("  Block size:", format(x$block_size, big.mark = ","), "\n")
  cat("  Strategy:", x$strategy, "\n")
  cat("  Shards:", x$num_shards, "\n")

  # Show size distribution
  sizes <- vapply(x$shards, function(s) s$len, integer(1))
  if (length(unique(sizes)) == 1L) {
    cat("  Shard size:", sizes[1], "(uniform)\n")
  } else {
    cat("  Shard sizes:", min(sizes), "-", max(sizes), "\n")
  }

  invisible(x)
}

#' @export
length.shard_descriptor <- function(x) {
  x$num_shards
}

#' Subset Shard Descriptor
#'
#' @param x A shard_descriptor object.
#' @param i Index or indices.
#' @return The selected shard(s).
#' @export
`[.shard_descriptor` <- function(x, i) {
  x$shards[i]
}

#' Get Single Shard
#'
#' @param x A shard_descriptor object.
#' @param i Index.
#' @return The selected shard.
#' @export
`[[.shard_descriptor` <- function(x, i) {
  x$shards[[i]]
}
