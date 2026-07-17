#' @title Chunk Queue Management
#' @description Queue management for dispatching chunks to workers with requeue support.
#' @name queue
NULL

#' Create a Chunk Queue
#'
#' Creates a queue for managing chunk dispatch with support for requeuing
#' failed chunks when workers die or are recycled.
#'
#' @param chunks List of chunk descriptors. Each chunk should have an `id` field.
#' @return A `shard_queue` object with queue management methods.
#' @keywords internal
#' @noRd
queue_create <- function(chunks) {
  # Ensure each chunk has an ID
  for (i in seq_along(chunks)) {
    if (is.null(chunks[[i]]$id)) {
      chunks[[i]]$id <- i
    }
  }

  n <- length(chunks)
  env <- new.env(parent = emptyenv())
  # `pending` is consumed via the `head` cursor (claimed slots are NULLed out);
  # it is never re-subset, so dequeue is O(1) instead of O(n) per chunk.
  env$pending <- chunks
  env$head <- 1L
  env$requeued <- list()  # retry overflow; small (failures only)
  env$n_pending <- n
  env$in_flight <- list()
  # Completed chunks are stored by original position; `done` tracks which
  # positions hold a completed chunk.
  env$completed <- vector("list", n)
  env$done <- logical(n)
  env$n_completed <- 0L
  env$failed <- list()
  env$assignment <- list()  # chunk_id -> worker_id mapping
  env$total_retries <- 0L
  env$order <- vapply(chunks, function(x) as.character(x$id), character(1))
  pos <- new.env(parent = emptyenv(), hash = TRUE, size = max(n, 29L))
  for (i in seq_len(n)) {
    assign(env$order[[i]], i, envir = pos)
  }
  env$pos <- pos

  structure(
    list(
      env = env,
      total = n
    ),
    class = "shard_queue"
  )
}

# O(1) lookup of a chunk's storage position from its id.
queue_pos_ <- function(env, chunk_id) {
  get0(chunk_id, envir = env$pos, inherits = FALSE)
}

#' Get Next Chunk from Queue
#'
#' @param queue A `shard_queue` object.
#' @param worker_id Worker requesting the chunk.
#' @return A chunk descriptor or NULL if queue is empty.
#' @keywords internal
#' @noRd
queue_next <- function(queue, worker_id) {
  env <- queue$env

  chunk <- NULL
  # Advance the cursor past claimed (NULLed) slots.
  while (env$head <= length(env$pending)) {
    chunk <- env$pending[[env$head]]
    env$pending[env$head] <- list(NULL)
    env$head <- env$head + 1L
    if (!is.null(chunk)) break
  }
  if (is.null(chunk)) {
    if (length(env$requeued) == 0) {
      return(NULL)
    }
    chunk <- env$requeued[[1L]]
    env$requeued <- env$requeued[-1L]
  }
  env$n_pending <- env$n_pending - 1L

  # Track assignment
  chunk_id <- as.character(chunk$id)
  env$in_flight[[chunk_id]] <- chunk
  env$assignment[[chunk_id]] <- worker_id

  chunk
}

queue_next_where <- function(queue, worker_id, predicate = NULL) {
  env <- queue$env
  if (env$n_pending == 0L) return(NULL)
  if (is.null(predicate)) return(queue_next(queue, worker_id))
  if (!is.function(predicate)) stop("predicate must be a function or NULL", call. = FALSE)

  chunk <- NULL
  if (env$head <= length(env$pending)) {
    for (i in env$head:length(env$pending)) {
      ch <- env$pending[[i]]
      if (is.null(ch)) next
      ok <- tryCatch(isTRUE(predicate(ch)), error = function(e) FALSE)
      if (ok) {
        chunk <- ch
        env$pending[i] <- list(NULL)
        if (i == env$head) env$head <- env$head + 1L
        break
      }
    }
  }
  if (is.null(chunk) && length(env$requeued) > 0) {
    for (i in seq_along(env$requeued)) {
      ch <- env$requeued[[i]]
      ok <- tryCatch(isTRUE(predicate(ch)), error = function(e) FALSE)
      if (ok) {
        chunk <- ch
        env$requeued <- env$requeued[-i]
        break
      }
    }
  }
  if (is.null(chunk)) return(NULL)
  env$n_pending <- env$n_pending - 1L

  chunk_id <- as.character(chunk$id)
  env$in_flight[[chunk_id]] <- chunk
  env$assignment[[chunk_id]] <- worker_id
  chunk
}

#' Mark Chunk as Completed
#'
#' @param queue A `shard_queue` object.
#' @param chunk_id Chunk identifier.
#' @param result The result from processing.
#' @return NULL (invisibly).
#' @keywords internal
#' @noRd
queue_complete <- function(queue, chunk_id, result = NULL, retain = TRUE) {
  env <- queue$env
  chunk_id <- as.character(chunk_id)

  if (!is.null(env$in_flight[[chunk_id]])) {
    chunk <- env$in_flight[[chunk_id]]
    pos <- queue_pos_(env, chunk_id)
    if (isTRUE(retain)) {
      chunk$result <- result
      chunk$completed_at <- Sys.time()
      env$completed[[pos]] <- chunk
    } else {
      # Keep only minimal completion metadata to avoid retaining large shard lists
      # when callers are doing streaming reductions.
      env$completed[[pos]] <- list(
        id = chunk$id,
        completed_at = Sys.time(),
        retry_count = chunk$retry_count %||% 0L,
        result = result
      )
    }
    if (!env$done[[pos]]) {
      env$done[[pos]] <- TRUE
      env$n_completed <- env$n_completed + 1L
    }
    env$in_flight[[chunk_id]] <- NULL
    env$assignment[[chunk_id]] <- NULL
  }

  invisible(NULL)
}

#' Mark Chunk as Failed
#'
#' @param queue A `shard_queue` object.
#' @param chunk_id Chunk identifier.
#' @param error The error that occurred.
#' @param requeue Logical. Whether to requeue for retry.
#' @return NULL (invisibly).
#' @keywords internal
#' @noRd
queue_fail <- function(queue, chunk_id, error = NULL, requeue = TRUE) {
  env <- queue$env
  chunk_id <- as.character(chunk_id)

  chunk <- env$in_flight[[chunk_id]]
  if (is.null(chunk)) {
    return(invisible(NULL))
  }

  env$in_flight[[chunk_id]] <- NULL
  env$assignment[[chunk_id]] <- NULL

  # Track retry count
  chunk$retry_count <- (chunk$retry_count %||% 0L) + 1L
  chunk$last_error <- error
  env$total_retries <- env$total_retries + 1L

  if (requeue) {
    # Put back in the (small) retry overflow queue
    env$requeued[[length(env$requeued) + 1L]] <- chunk
    env$n_pending <- env$n_pending + 1L
  } else {
    # Mark as permanently failed
    chunk$failed_at <- Sys.time()
    env$failed[[chunk_id]] <- chunk
  }

  invisible(NULL)
}

#' Requeue All Chunks for a Worker
#'
#' When a worker dies or is recycled, requeue all its in-flight chunks.
#'
#' @param queue A `shard_queue` object.
#' @param worker_id Worker identifier.
#' @return Integer. Number of chunks requeued.
#' @keywords internal
#' @noRd
queue_requeue_worker <- function(queue, worker_id) {
  env <- queue$env
  requeued <- 0L

  chunk_ids <- names(env$assignment)
  for (chunk_id in chunk_ids) {
    if (identical(env$assignment[[chunk_id]], worker_id)) {
      queue_fail(queue, chunk_id, error = "worker_recycled", requeue = TRUE)
      requeued <- requeued + 1L
    }
  }

  requeued
}

#' Check if Queue is Done
#'
#' @param queue A `shard_queue` object.
#' @return Logical. TRUE if all chunks are completed or failed.
#' @keywords internal
#' @noRd
queue_is_done <- function(queue) {
  env <- queue$env
  env$n_pending == 0L && length(env$in_flight) == 0
}

#' Check if Queue Has Work
#'
#' @param queue A `shard_queue` object.
#' @return Logical. TRUE if there are pending chunks.
#' @keywords internal
#' @noRd
queue_has_pending <- function(queue) {
  queue$env$n_pending > 0L
}

#' Get Queue Status
#'
#' @param queue A `shard_queue` object.
#' @return A list with queue statistics.
#' @keywords internal
#' @noRd
queue_status <- function(queue) {
  env <- queue$env

  list(
    total = queue$total,
    pending = env$n_pending,
    in_flight = length(env$in_flight),
    completed = env$n_completed,
    failed = length(env$failed),
    total_retries = env$total_retries
  )
}

#' Get Queue Results
#'
#' @param queue A `shard_queue` object.
#' @return List of results from completed chunks.
#' @keywords internal
#' @noRd
queue_results <- function(queue) {
  env <- queue$env
  if (env$n_completed == 0L) return(list())

  completed <- env$completed[env$done]
  ids <- env$order[env$done]
  names(completed) <- ids

  # Prefer numeric id ordering for predictable, lapply/sapply-like behavior;
  # storage order is creation order, which matches for default integer ids.
  num_ids <- suppressWarnings(as.integer(ids))
  if (!anyNA(num_ids) && is.unsorted(num_ids)) {
    completed <- completed[order(num_ids)]
  }

  lapply(completed, function(x) x$result)
}

#' Get Failed Chunks
#'
#' @param queue A `shard_queue` object.
#' @return List of failed chunk descriptors.
#' @keywords internal
#' @noRd
queue_failures <- function(queue) {
  queue$env$failed
}

#' @param x A \code{shard_queue} object.
#' @param ... Further arguments (ignored).
#' @return The input \code{x}, invisibly.
#' @export
#' @noRd
print.shard_queue <- function(x, ...) {
  status <- queue_status(x)
  cat("shard chunk queue\n")
  cat("  Total chunks:", status$total, "\n")
  cat("  Pending:", status$pending, "\n")
  cat("  In flight:", status$in_flight, "\n")
  cat("  Completed:", status$completed, "\n")
  cat("  Failed:", status$failed, "\n")
  cat("  Total retries:", status$total_retries, "\n")
  invisible(x)
}
