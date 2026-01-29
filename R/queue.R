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
queue_create <- function(chunks) {
  # Ensure each chunk has an ID
  for (i in seq_along(chunks)) {
    if (is.null(chunks[[i]]$id)) {
      chunks[[i]]$id <- i
    }
  }

  env <- new.env(parent = emptyenv())
  env$pending <- chunks
  env$in_flight <- list()
  env$completed <- list()
  env$failed <- list()
  env$assignment <- list()  # chunk_id -> worker_id mapping

  structure(
    list(
      env = env,
      total = length(chunks)
    ),
    class = "shard_queue"
  )
}

#' Get Next Chunk from Queue
#'
#' @param queue A `shard_queue` object.
#' @param worker_id Worker requesting the chunk.
#' @return A chunk descriptor or NULL if queue is empty.
#' @keywords internal
queue_next <- function(queue, worker_id) {
  env <- queue$env

  if (length(env$pending) == 0) {
    return(NULL)
  }

  chunk <- env$pending[[1]]
  env$pending <- env$pending[-1]

  # Track assignment
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
queue_complete <- function(queue, chunk_id, result = NULL) {
  env <- queue$env
  chunk_id <- as.character(chunk_id)

  if (!is.null(env$in_flight[[chunk_id]])) {
    chunk <- env$in_flight[[chunk_id]]
    chunk$result <- result
    chunk$completed_at <- Sys.time()

    env$completed[[chunk_id]] <- chunk
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

  if (requeue) {
    # Put back in pending queue for retry
    env$pending <- c(env$pending, list(chunk))
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
queue_is_done <- function(queue) {
  env <- queue$env
  length(env$pending) == 0 && length(env$in_flight) == 0
}

#' Check if Queue Has Work
#'
#' @param queue A `shard_queue` object.
#' @return Logical. TRUE if there are pending chunks.
#' @keywords internal
queue_has_pending <- function(queue) {
  length(queue$env$pending) > 0
}

#' Get Queue Status
#'
#' @param queue A `shard_queue` object.
#' @return A list with queue statistics.
#' @keywords internal
queue_status <- function(queue) {
  env <- queue$env

  # Count retries
  total_retries <- sum(vapply(
    c(env$completed, env$pending, env$in_flight, env$failed),
    function(x) x$retry_count %||% 0L,
    integer(1)
  ))

  list(
    total = queue$total,
    pending = length(env$pending),
    in_flight = length(env$in_flight),
    completed = length(env$completed),
    failed = length(env$failed),
    total_retries = total_retries
  )
}

#' Get Queue Results
#'
#' @param queue A `shard_queue` object.
#' @return List of results from completed chunks.
#' @keywords internal
queue_results <- function(queue) {
  lapply(queue$env$completed, function(x) x$result)
}

#' Get Failed Chunks
#'
#' @param queue A `shard_queue` object.
#' @return List of failed chunk descriptors.
#' @keywords internal
queue_failures <- function(queue) {
  queue$env$failed
}

#' @export
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
