#' @title Shared Memory Segment
#' @name segment
#' @description Low-level shared memory segment operations for cross-process
#'   data sharing. These functions provide the foundation for the higher-level
#'   \code{share()} and \code{buffer()} APIs.
#'
#' @details
#' Segments can be backed by:
#' \itemize{
#'   \item \code{"shm"}: POSIX shared memory (Linux/macOS) or named file
#'     mapping (Windows). Faster but may have size limitations.
#'   \item \code{"mmap"}: File-backed memory mapping. Works on all platforms
#'     and supports larger sizes.
#'   \item \code{"auto"}: Let the system choose the best option.
#' }
#'
#' All segments are created with secure permissions (0600 on Unix) and are
#' automatically cleaned up when the R object is garbage collected.
#'
#' @useDynLib shard, .registration = TRUE
NULL

#' Create a new shared memory segment
#'
#' @param size Size of the segment in bytes
#' @param backing Backing type: "auto", "mmap", or "shm"
#' @param path Optional file path for mmap backing (NULL for temp file)
#' @param readonly Create as read-only (after initial write)
#' @return An S3 object of class "shard_segment"
#' @export
#' @examples
#' \dontrun{
#' # Create a 1MB segment
#' seg <- segment_create(1024 * 1024)
#' segment_info(seg)
#' segment_close(seg)
#' }
segment_create <- function(size,
                           backing = c("auto", "mmap", "shm"),
                           path = NULL,
                           readonly = FALSE) {
    backing <- match.arg(backing)
    backing_int <- switch(backing,
        "auto" = 0L,
        "mmap" = 1L,
        "shm"  = 2L
    )

    size <- as.double(size)
    if (size <= 0) {
        stop("size must be positive")
    }

    ptr <- .Call("C_shard_segment_create", size, backing_int, path, readonly,
                 PACKAGE = "shard")

    structure(
        list(
            ptr = ptr,
            size = size,
            backing = backing,
            readonly = readonly
        ),
        class = "shard_segment"
    )
}

#' Open an existing shared memory segment
#'
#' @param path Path or shm name of the segment
#' @param backing Backing type: "mmap" or "shm"
#' @param readonly Open as read-only
#' @return An S3 object of class "shard_segment"
#' @export
segment_open <- function(path,
                         backing = c("mmap", "shm"),
                         readonly = TRUE) {
    backing <- match.arg(backing)
    backing_int <- switch(backing,
        "mmap" = 1L,
        "shm"  = 2L
    )

    ptr <- .Call("C_shard_segment_open", path, backing_int, readonly,
                 PACKAGE = "shard")
    info <- .Call("C_shard_segment_info", ptr, PACKAGE = "shard")

    structure(
        list(
            ptr = ptr,
            size = info$size,
            backing = backing,
            readonly = readonly
        ),
        class = "shard_segment"
    )
}

#' Close a shared memory segment
#'
#' @param x A shard_segment object
#' @param unlink Whether to unlink the underlying file/shm (default: FALSE for
#'   opened segments, TRUE for owned segments)
#' @return NULL (invisibly)
#' @export
segment_close <- function(x, unlink = NULL) {
    stopifnot(inherits(x, "shard_segment"))

    if (is.null(unlink)) {
        info <- .Call("C_shard_segment_info", x$ptr, PACKAGE = "shard")
        unlink <- info$owns
    }

    .Call("C_shard_segment_close", x$ptr, unlink, PACKAGE = "shard")
    invisible(NULL)
}

#' Get segment information
#'
#' @param x A shard_segment object
#' @return A list with segment properties
#' @export
segment_info <- function(x) {
    stopifnot(inherits(x, "shard_segment"))
    .Call("C_shard_segment_info", x$ptr, PACKAGE = "shard")
}

#' Get the path or name of a segment
#'
#' @param x A shard_segment object
#' @return The path string, or NULL for anonymous segments
#' @export
segment_path <- function(x) {
    stopifnot(inherits(x, "shard_segment"))
    .Call("C_shard_segment_path", x$ptr, PACKAGE = "shard")
}

#' Get the size of a segment
#'
#' @param x A shard_segment object
#' @return Size in bytes
#' @export
segment_size <- function(x) {
    stopifnot(inherits(x, "shard_segment"))
    .Call("C_shard_segment_size", x$ptr, PACKAGE = "shard")
}

#' Write data to a segment
#'
#' @param x A shard_segment object
#' @param data Data to write (raw, numeric, integer, or logical vector)
#' @param offset Byte offset to start writing (0-based)
#' @return Number of bytes written (invisibly)
#' @export
segment_write <- function(x, data, offset = 0) {
    stopifnot(inherits(x, "shard_segment"))
    bytes <- .Call("C_shard_segment_write_raw", x$ptr, data, as.double(offset),
                   PACKAGE = "shard")
    invisible(bytes)
}

#' Read raw data from a segment
#'
#' @param x A shard_segment object
#' @param offset Byte offset to start reading (0-based)
#' @param size Number of bytes to read
#' @return A raw vector
#' @export
segment_read <- function(x, offset = 0, size = NULL) {
    stopifnot(inherits(x, "shard_segment"))

    if (is.null(size)) {
        size <- segment_size(x) - offset
    }

    .Call("C_shard_segment_read_raw", x$ptr, as.double(offset), as.double(size),
          PACKAGE = "shard")
}

#' Make a segment read-only
#'
#' @param x A shard_segment object
#' @return The segment object (invisibly)
#' @export
segment_protect <- function(x) {
    stopifnot(inherits(x, "shard_segment"))
    .Call("C_shard_segment_protect", x$ptr, PACKAGE = "shard")
    x$readonly <- TRUE
    invisible(x)
}

#' @export
print.shard_segment <- function(x, ...) {
    info <- segment_info(x)
    cat("<shard_segment>\n")
    cat("  Size:", format(info$size, big.mark = ","), "bytes\n")
    cat("  Backing:", info$backing, "\n")
    if (!is.null(info$path)) {
        cat("  Path:", info$path, "\n")
    }
    cat("  Read-only:", info$readonly, "\n")
    cat("  Owns segment:", info$owns, "\n")
    invisible(x)
}

#' Check if running on Windows
#'
#' @return TRUE if on Windows, FALSE otherwise
#' @export
is_windows <- function() {
    .Call("C_shard_is_windows", PACKAGE = "shard")
}

#' Get available shared memory backing types
#'
#' @return Character vector of available backing types
#' @export
available_backings <- function() {
    .Call("C_shard_available_backings", PACKAGE = "shard")
}
