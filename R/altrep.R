#' @title ALTREP Shared Vectors
#' @name altrep
#' @description ALTREP-backed zero-copy vectors for shared memory.
#'
#' @details
#' These functions create ALTREP (Alternative Representation) vectors that
#' are backed by shared memory segments. The key benefits are:
#'
#' \itemize{
#'   \item \strong{Zero-copy subsetting}: Contiguous subsets return views
#'     into the same shared memory, not copies.
#'   \item \strong{Diagnostics}: Track when data pointers are accessed or
#'     when vectors are materialized (copied to standard R vectors).
#'   \item \strong{Read-only protection}: Optionally prevent write access
#'     to protect shared data.
#' }
#'
#' Supported types: integer, double/numeric, logical, raw.
#'
#' @useDynLib shard, .registration = TRUE
NULL

#' Create a shared vector from a segment
#'
#' @param segment A shard_segment object
#' @param type Vector type: "integer", "double"/"numeric", "logical", or "raw"
#' @param offset Byte offset into segment (default: 0)
#' @param length Number of elements. If NULL, calculated from segment size.
#' @param readonly If TRUE, prevent write access via DATAPTR (default: TRUE)
#' @return An ALTREP vector backed by shared memory
#' @export
#' @examples
#' \dontrun{
#' # Create a segment and store integers
#' seg <- segment_create(400)  # 100 integers * 4 bytes
#' segment_write(seg, 1:100, offset = 0)
#'
#' # Create ALTREP view
#' x <- shared_vector(seg, "integer", length = 100)
#' x[1:10]  # Returns a view, not a copy
#'
#' # Check diagnostics
#' shared_diagnostics(x)
#' }
shared_vector <- function(segment,
                          type = c("double", "integer", "logical", "raw"),
                          offset = 0,
                          length = NULL,
                          readonly = TRUE) {
    stopifnot(inherits(segment, "shard_segment"))
    type <- match.arg(type)

    # Calculate element size
    elem_size <- switch(type,
        "integer" = 4L,
        "double"  = 8L,
        "numeric" = 8L,
        "logical" = 4L,  # R logicals are stored as int
        "raw"     = 1L
    )

    # Calculate length if not provided
    if (is.null(length)) {
        seg_size <- segment_size(segment)
        available <- seg_size - offset
        length <- floor(available / elem_size)
    }

    if (length <= 0) {
        stop("length must be positive")
    }

    .Call("C_shard_altrep_create",
          segment$ptr,
          type,
          as.double(offset),
          as.double(length),
          readonly,
          PACKAGE = "shard")
}

#' Create a view (subset) of a shared vector
#'
#' @param x A shard ALTREP vector
#' @param start Start index (1-based, like R)
#' @param length Number of elements
#' @return An ALTREP view into the same shared memory
#' @export
#' @examples
#' \dontrun{
#' seg <- segment_create(800)
#' segment_write(seg, 1:100, offset = 0)
#' x <- shared_vector(seg, "integer", length = 100)
#'
#' # Create a view of elements 10-20
#' y <- shared_view(x, start = 10, length = 11)
#' y[1]  # Same as x[10]
#' }
shared_view <- function(x, start, length) {
    if (!is_shared_vector(x)) {
        stop("x must be a shard ALTREP vector")
    }

    # Convert to 0-based
    start_0 <- as.double(start - 1)

    .Call("C_shard_altrep_view", x, start_0, as.double(length),
          PACKAGE = "shard")
}

#' Get diagnostics for a shared vector
#'
#' @param x A shard ALTREP vector
#' @return A list with diagnostic information:
#'   \describe{
#'     \item{dataptr_calls}{Number of times DATAPTR was accessed}
#'     \item{materialize_calls}{Number of times vector was copied to standard R vector}
#'     \item{length}{Number of elements}
#'     \item{offset}{Byte offset into underlying segment}
#'     \item{readonly}{Whether write access is prevented}
#'     \item{type}{R type of the vector}
#'   }
#' @export
#' @examples
#' \dontrun{
#' seg <- segment_create(400)
#' segment_write(seg, 1:100, offset = 0)
#' x <- shared_vector(seg, "integer", length = 100)
#'
#' # Access data
#' sum(x)
#'
#' # Check diagnostics
#' shared_diagnostics(x)
#' }
shared_diagnostics <- function(x) {
    if (!is_shared_vector(x)) {
        stop("x must be a shard ALTREP vector")
    }

    .Call("C_shard_altrep_diagnostics", x, PACKAGE = "shard")
}

#' Check if an object is a shared vector
#'
#' @param x Any R object
#' @return TRUE if x is a shard ALTREP vector, FALSE otherwise
#' @export
#' @examples
#' \dontrun{
#' seg <- segment_create(400)
#' segment_write(seg, 1:100, offset = 0)
#' x <- shared_vector(seg, "integer", length = 100)
#'
#' is_shared_vector(x)    # TRUE
#' is_shared_vector(1:10) # FALSE
#' }
is_shared_vector <- function(x) {
    .Call("C_is_shard_altrep", x, PACKAGE = "shard")
}

#' Get the underlying segment from a shared vector
#'
#' @param x A shard ALTREP vector
#' @return The external pointer to the underlying segment
#' @export
shared_segment <- function(x) {
    if (!is_shared_vector(x)) {
        stop("x must be a shard ALTREP vector")
    }

    .Call("C_shard_altrep_segment", x, PACKAGE = "shard")
}

#' Reset diagnostic counters for a shared vector
#'
#' @param x A shard ALTREP vector
#' @return x (invisibly)
#' @export
#' @examples
#' \dontrun{
#' seg <- segment_create(400)
#' segment_write(seg, 1:100, offset = 0)
#' x <- shared_vector(seg, "integer", length = 100)
#'
#' sum(x)
#' shared_diagnostics(x)$dataptr_calls  # > 0
#'
#' shared_reset_diagnostics(x)
#' shared_diagnostics(x)$dataptr_calls  # 0
#' }
shared_reset_diagnostics <- function(x) {
    if (!is_shared_vector(x)) {
        stop("x must be a shard ALTREP vector")
    }

    .Call("C_shard_altrep_reset_diagnostics", x, PACKAGE = "shard")
    invisible(x)
}

#' Create a shared vector from an existing R vector
#'
#' Convenience function that creates a segment, writes the data,
#' and returns an ALTREP view.
#'
#' @param x An atomic vector (integer, double, logical, or raw)
#' @param readonly If TRUE, prevent write access (default: TRUE)
#' @param backing Backing type for the segment: "auto", "mmap", or "shm"
#' @return An ALTREP vector backed by shared memory
#' @export
#' @examples
#' \dontrun{
#' # Convert existing vector to shared
#' x <- as_shared(1:100)
#' is_shared_vector(x)  # TRUE
#'
#' # Subsetting returns views
#' y <- x[1:10]
#' is_shared_vector(y)  # TRUE for contiguous subsets
#' }
as_shared <- function(x, readonly = TRUE, backing = "auto") {
    if (!is.atomic(x) || is.null(x)) {
        stop("x must be an atomic vector")
    }

    # Determine type
    type <- switch(typeof(x),
        "integer" = "integer",
        "double"  = "double",
        "logical" = "logical",
        "raw"     = "raw",
        stop("Unsupported type: ", typeof(x))
    )

    # Calculate size
    elem_size <- switch(type,
        "integer" = 4L,
        "double"  = 8L,
        "logical" = 4L,
        "raw"     = 1L
    )
    size <- length(x) * elem_size

    # Create segment and write data
    seg <- segment_create(size, backing = backing)
    segment_write(seg, x, offset = 0)

    # Optionally protect segment
    if (readonly) {
        segment_protect(seg)
    }

    # Create ALTREP view
    shared_vector(seg, type = type, offset = 0, length = length(x),
                  readonly = readonly)
}
