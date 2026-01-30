#' CPU Affinity + mmap Advice (Advanced)
#'
#' These controls are opt-in and best-effort. On unsupported platforms, they
#' safely no-op (returning FALSE).
#'
#' @name affinity
NULL

#' Check whether CPU affinity is supported
#'
#' Currently supported on Linux only.
#'
#' @return Logical.
#' @export
affinity_supported <- function() {
  isTRUE(.Call("C_shard_affinity_supported", PACKAGE = "shard"))
}

#' Set CPU affinity for the current process
#'
#' Intended to be called inside a worker process (e.g., via `clusterCall()`).
#' On unsupported platforms, returns FALSE.
#'
#' @param cores Integer vector of 0-based CPU core ids.
#' @return Logical; TRUE on success.
#' @export
set_affinity <- function(cores) {
  cores <- as.integer(cores)
  isTRUE(.Call("C_shard_set_affinity", cores, PACKAGE = "shard"))
}

#' Pin shard workers to CPU cores
#'
#' Best-effort worker pinning to improve cache locality and reduce cross-core
#' migration. Currently supported on Linux only.
#'
#' @param pool Optional shard_pool. Defaults to current pool.
#' @param strategy "spread" assigns worker i -> core i mod ncores. "compact"
#'   assigns workers to the first cores.
#' @param cores Optional integer vector of available cores (0-based). If NULL,
#'   uses 0:(detectCores()-1).
#' @return Invisibly, a logical vector per worker indicating success.
#' @export
pin_workers <- function(pool = NULL,
                        strategy = c("spread", "compact"),
                        cores = NULL) {
  strategy <- match.arg(strategy)
  if (is.null(pool)) pool <- pool_get()
  if (is.null(pool)) stop("No active pool", call. = FALSE)

  if (!affinity_supported()) {
    return(invisible(rep(FALSE, pool$n)))
  }

  if (is.null(cores)) {
    ncores <- parallel::detectCores()
    cores <- seq.int(0L, as.integer(ncores) - 1L)
  }
  cores <- as.integer(cores)
  cores <- cores[!is.na(cores)]
  if (length(cores) < 1L) stop("cores must be non-empty", call. = FALSE)

  ok <- logical(pool$n)
  for (i in seq_len(pool$n)) {
    core <- switch(strategy,
      "spread" = cores[((i - 1L) %% length(cores)) + 1L],
      "compact" = cores[min(i, length(cores))]
    )
    ok[i] <- tryCatch(
      isTRUE(parallel::clusterCall(pool$workers[[i]]$cluster, function(c) shard::set_affinity(c), core)[[1]]),
      error = function(e) FALSE
    )
  }
  invisible(ok)
}

#' Advise OS about expected access pattern for a segment
#'
#' This calls `madvise()` on the segment mapping when available.
#'
#' @param seg A shard_segment.
#' @param advice One of "normal", "sequential", "random", "willneed", "dontneed".
#' @return Logical; TRUE if the OS accepted the hint.
#' @export
segment_advise <- function(seg,
                           advice = c("normal", "sequential", "random", "willneed", "dontneed")) {
  if (!(inherits(seg, "shard_segment") || identical(typeof(seg), "externalptr"))) {
    stop("seg must be a shard_segment or an external pointer", call. = FALSE)
  }
  advice <- match.arg(advice)
  adv_int <- switch(advice,
    "normal" = 0L,
    "sequential" = 1L,
    "random" = 2L,
    "willneed" = 3L,
    "dontneed" = 4L
  )
  ptr <- if (inherits(seg, "shard_segment")) seg$ptr else seg
  isTRUE(.Call("C_shard_segment_madvise", ptr, adv_int, PACKAGE = "shard"))
}

#' Advise access pattern for a buffer
#'
#' @param x A shard_buffer.
#' @param advice See [segment_advise()].
#' @return Logical.
#' @export
buffer_advise <- function(x,
                          advice = c("normal", "sequential", "random", "willneed", "dontneed")) {
  stopifnot(inherits(x, "shard_buffer"))
  segment_advise(x$segment, advice = advice)
}

#' Advise access pattern for a shared input vector/matrix
#'
#' @param x A shard shared vector (from [share()]).
#' @param advice See [segment_advise()].
#' @return Logical.
#' @export
shared_advise <- function(x,
                          advice = c("normal", "sequential", "random", "willneed", "dontneed")) {
  if (!is_shared_vector(x)) stop("x must be a shared vector", call. = FALSE)
  segment_advise(shared_segment(x), advice = advice)
}
