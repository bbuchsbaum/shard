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
#' @return A \code{shard_segment} object backed by shared memory.
#' @export
#' @examples
#' \donttest{
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
#' @return A \code{shard_segment} object attached to the existing segment.
#' @export
#' @examples
#' \donttest{
#' seg <- segment_create(1024, backing = "mmap")
#' path <- segment_path(seg)
#' seg2 <- segment_open(path, backing = "mmap", readonly = TRUE)
#' segment_close(seg2, unlink = FALSE)
#' segment_close(seg)
#' }
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
#' @return \code{NULL}, invisibly.
#' @export
#' @examples
#' \donttest{
#' seg <- segment_create(1024)
#' segment_close(seg)
#' }
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
#' @return A named list with segment metadata including \code{size}, \code{backing},
#'   \code{path}, \code{readonly}, and \code{owns}.
#' @export
#' @examples
#' \donttest{
#' seg <- segment_create(1024)
#' segment_info(seg)
#' segment_close(seg)
#' }
segment_info <- function(x) {
    stopifnot(inherits(x, "shard_segment"))
    .Call("C_shard_segment_info", x$ptr, PACKAGE = "shard")
}

#' Get the path or name of a segment
#'
#' @param x A shard_segment object
#' @return The path string, or \code{NULL} for anonymous segments.
#' @export
#' @examples
#' \donttest{
#' seg <- segment_create(1024, backing = "mmap")
#' segment_path(seg)
#' segment_close(seg)
#' }
segment_path <- function(x) {
    stopifnot(inherits(x, "shard_segment"))
    .Call("C_shard_segment_path", x$ptr, PACKAGE = "shard")
}

#' Get the size of a segment
#'
#' @param x A shard_segment object
#' @return Size in bytes as a numeric scalar.
#' @export
#' @examples
#' \donttest{
#' seg <- segment_create(1024)
#' segment_size(seg)
#' segment_close(seg)
#' }
segment_size <- function(x) {
    stopifnot(inherits(x, "shard_segment"))
    .Call("C_shard_segment_size", x$ptr, PACKAGE = "shard")
}

#' Write data to a segment
#'
#' @param x A shard_segment object
#' @param data Data to write (raw, numeric, integer, or logical vector)
#' @param offset Byte offset to start writing (0-based)
#' @return Number of bytes written, invisibly.
#' @export
#' @examples
#' \donttest{
#' seg <- segment_create(1024)
#' segment_write(seg, as.integer(1:10), offset = 0)
#' segment_close(seg)
#' }
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
#' @return A raw vector containing the bytes read from the segment.
#' @export
#' @examples
#' \donttest{
#' seg <- segment_create(1024)
#' segment_write(seg, as.integer(1:4), offset = 0)
#' segment_read(seg, offset = 0, size = 16)
#' segment_close(seg)
#' }
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
#' @return The \code{shard_segment} object, invisibly.
#' @export
#' @examples
#' \donttest{
#' seg <- segment_create(1024)
#' segment_protect(seg)
#' segment_close(seg)
#' }
segment_protect <- function(x) {
    stopifnot(inherits(x, "shard_segment"))
    .Call("C_shard_segment_protect", x$ptr, PACKAGE = "shard")
    x$readonly <- TRUE
    invisible(x)
}

#' Print a Shared Memory Segment
#'
#' @param x A \code{shard_segment} object.
#' @param ... Ignored.
#' @return The input \code{x}, invisibly.
#' @export
#' @examples
#' \donttest{
#' seg <- segment_create(1024)
#' print(seg)
#' segment_close(seg)
#' }
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
#' @return A logical scalar: \code{TRUE} if running on Windows, \code{FALSE} otherwise.
#' @export
#' @examples
#' is_windows()
is_windows <- function() {
    .Call("C_shard_is_windows", PACKAGE = "shard")
}

#' Get available shared memory backing types
#'
#' @return A character vector of available backing types on the current platform.
#' @export
#' @examples
#' available_backings()
available_backings <- function() {
    .Call("C_shard_available_backings", PACKAGE = "shard")
}
