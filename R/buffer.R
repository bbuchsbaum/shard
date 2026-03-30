#' @title Shared Memory Buffers
#' @name buffer
#' @description Create typed writable output buffers backed by shared memory
#'   for cross-process writes during parallel execution.
#'
#' @details
#' Buffers provide an explicit output mechanism for \code{\link{shard_map}}.
#' Instead of returning results from workers (which requires serialization
#' and memory copying), workers write directly to shared buffers.
#'
#' Supported types:
#' \itemize{
#'   \item \code{"double"}: 8-byte floating point (default)
#'   \item \code{"integer"}: 4-byte signed integer
#'   \item \code{"logical"}: 4-byte logical (stored as integer)
#'   \item \code{"raw"}: 1-byte raw data
#' }
#'
#' Buffers support slice assignment using standard R indexing:
#' \code{buf[1:100] <- values}
#'
#' @seealso \code{\link{segment_create}} for low-level segment operations,
#'   \code{\link{share}} for read-only shared inputs
NULL

# Type sizes in bytes
.buffer_type_size <- function(type) {
    switch(type,
        "double"  = 8L,
        "integer" = 4L,
        "logical" = 4L,
        "raw"     = 1L,
        stop("Unsupported buffer type: ", type)
    )
}

# R type code (SEXPTYPE) for type
.buffer_sexptype <- function(type) {
    switch(type,
        "double"  = 14L,  # REALSXP
        "integer" = 13L,  # INTSXP
        "logical" = 10L,  # LGLSXP
        "raw"     = 24L,  # RAWSXP
        stop("Unsupported buffer type: ", type)
    )
}

# Buffer write diagnostics (per-process). These counters are used to attribute
# output-buffer writes to shard_map runs via per-chunk deltas in the dispatcher.
.buffer_diag_env <- new.env(parent = emptyenv())
.buffer_diag_env$writes <- 0L
.buffer_diag_env$bytes <- 0

#' Buffer Diagnostics
#'
#' Returns per-process counters for shard buffer writes. shard_map uses these
#' internally to report write volume/operations in copy_report().
#'
#' @return A list with elements \code{writes} (integer count) and \code{bytes}
#'   (total bytes written) accumulated in the current process.
#' @export
#' @examples
#' buffer_diagnostics()
buffer_diagnostics <- function() {
    list(
        writes = .buffer_diag_env$writes,
        bytes = .buffer_diag_env$bytes
    )
}

# Reset counters (internal; tests may use shard:::buffer_reset_diagnostics()).
buffer_reset_diagnostics <- function() {
    .buffer_diag_env$writes <- 0L
    .buffer_diag_env$bytes <- 0
    invisible(NULL)
}

#' Create a Shared Memory Buffer
#'
#' Creates a typed output buffer backed by shared memory that can be written
#' to by parallel workers using slice assignment.
#'
#' @param type Character. Data type: "double" (default), "integer", "logical",
#'   or "raw".
#' @param dim Integer vector. Dimensions of the buffer. For a vector, specify
#'   the length. For a matrix, specify \code{c(nrow, ncol)}. For arrays,
#'   specify all dimensions.
#' @param init Initial value to fill the buffer. Default is type-appropriate
#'   zero (\code{0}, \code{0L}, \code{FALSE}, or \code{raw(0)}).
#' @param backing Backing type for shared memory: "auto" (default), "mmap",
#'   or "shm".
#'
#' @return An S3 object of class "shard_buffer" that supports:
#' \itemize{
#'   \item Slice assignment: \code{buf[idx] <- values}
#'   \item Slice reading: \code{buf[idx]}
#'   \item Full extraction: \code{buf[]}
#'   \item Conversion to R vector: \code{as.vector(buf)}, \code{as.double(buf)}, etc.
#' }
#'
#' @export
#' @examples
#' \donttest{
#' out <- buffer("double", dim = 100)
#' out[1:10] <- rnorm(10)
#' result <- out[]
#' }
buffer <- function(type = c("double", "integer", "logical", "raw"),
                   dim,
                   init = NULL,
                   backing = c("auto", "mmap", "shm")) {
    type <- match.arg(type)
    backing <- match.arg(backing)

    # Validate and normalize dim
    dim <- as.integer(dim)
    if (any(dim <= 0)) {
        stop("All dimensions must be positive")
    }
    n <- prod(dim)

    # Calculate byte size
    elem_size <- .buffer_type_size(type)
    byte_size <- as.double(n) * elem_size

    # Create underlying segment
    seg <- segment_create(byte_size, backing = backing)

    # Initialize buffer contents
    if (is.null(init)) {
        init <- switch(type,
            "double"  = 0,
            "integer" = 0L,
            "logical" = FALSE,
            "raw"     = raw(1)
        )
    }

    # Write initial value
    init_data <- switch(type,
        "double"  = rep(as.double(init), n),
        "integer" = rep(as.integer(init), n),
        "logical" = rep(as.logical(init), n),
        "raw"     = rep(as.raw(init), n)
    )
    segment_write(seg, init_data, offset = 0)

    structure(
        list(
            segment = seg,
            type = type,
            dim = dim,
            n = n,
            elem_size = elem_size
        ),
        class = "shard_buffer"
    )
}

#' Open an Existing Buffer
#'
#' Opens a shared memory buffer that was created in another process.
#' Used by workers to attach to the parent's output buffer.
#'
#' @param path Path or shm name of the buffer's segment.
#' @param type Character. Data type of the buffer.
#' @param dim Integer vector. Dimensions of the buffer.
#' @param backing Backing type: "mmap" or "shm".
#' @param readonly Logical. Open as read-only? Default FALSE for workers.
#'
#' @return A \code{shard_buffer} object attached to the existing segment.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("double", dim = 10)
#' path <- buffer_path(buf)
#' buf2 <- buffer_open(path, type = "double", dim = 10, backing = "mmap")
#' buffer_close(buf2, unlink = FALSE)
#' buffer_close(buf)
#' }
buffer_open <- function(path, type, dim,
                        backing = c("mmap", "shm"),
                        readonly = FALSE) {
    backing <- match.arg(backing)

    dim <- as.integer(dim)
    n <- prod(dim)
    elem_size <- .buffer_type_size(type)

    seg <- segment_open(path, backing = backing, readonly = readonly)

    # Verify size matches
    expected_size <- as.double(n) * elem_size
    actual_size <- segment_size(seg)
    if (actual_size < expected_size) {
        segment_close(seg)
        stop("Segment size mismatch: expected ", expected_size,
             " bytes, got ", actual_size)
    }

    structure(
        list(
            segment = seg,
            type = type,
            dim = dim,
            n = n,
            elem_size = elem_size
        ),
        class = "shard_buffer"
    )
}

#' Get Buffer Path
#'
#' Returns the path or name of the buffer's underlying segment.
#' Use this to pass buffer location to workers.
#'
#' @param x A shard_buffer object.
#' @return A character string with the path or name of the segment, or
#'   \code{NULL} if the segment is anonymous.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("double", dim = 10)
#' buffer_path(buf)
#' buffer_close(buf)
#' }
buffer_path <- function(x) {
    stopifnot(inherits(x, "shard_buffer"))
    segment_path(x$segment)
}

#' Get Buffer Info
#'
#' Returns information about a buffer.
#'
#' @param x A shard_buffer object.
#' @return A named list with buffer properties: \code{type}, \code{dim},
#'   \code{n}, \code{bytes}, \code{backing}, \code{path}, and \code{readonly}.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("integer", dim = c(5, 5))
#' buffer_info(buf)
#' buffer_close(buf)
#' }
buffer_info <- function(x) {
    stopifnot(inherits(x, "shard_buffer"))
    seg_info <- segment_info(x$segment)
    list(
        type = x$type,
        dim = x$dim,
        n = x$n,
        bytes = as.double(x$n) * x$elem_size,
        backing = seg_info$backing,
        path = seg_info$path,
        readonly = seg_info$readonly
    )
}

#' Close a Buffer
#'
#' Closes the buffer and releases the underlying shared memory.
#'
#' @param x A shard_buffer object.
#' @param unlink Whether to unlink the underlying segment.
#' @return \code{NULL}, invisibly.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("double", dim = 10)
#' buffer_close(buf)
#' }
buffer_close <- function(x, unlink = NULL) {
    stopifnot(inherits(x, "shard_buffer"))
    segment_close(x$segment, unlink = unlink)
    invisible(NULL)
}

#' Print a Shared Memory Buffer
#'
#' @param x A \code{shard_buffer} object.
#' @param ... Ignored.
#' @return The input \code{x}, invisibly.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("double", dim = 10)
#' print(buf)
#' buffer_close(buf)
#' }
print.shard_buffer <- function(x, ...) {
    info <- buffer_info(x)
    cat("<shard_buffer>\n")
    cat("  Type:", x$type, "\n")
    if (length(x$dim) == 1) {
        cat("  Length:", format(x$n, big.mark = ","), "\n")
    } else {
        cat("  Dim:", paste(x$dim, collapse = " x "), "\n")
    }
    cat("  Size:", format(info$bytes, big.mark = ","), "bytes\n")
    cat("  Backing:", info$backing, "\n")
    if (!is.null(info$path)) {
        cat("  Path:", info$path, "\n")
    }
    invisible(x)
}

#' Length of a Shared Memory Buffer
#'
#' @param x A \code{shard_buffer} object.
#' @return An integer scalar giving the total number of elements.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("double", dim = 20)
#' length(buf)
#' buffer_close(buf)
#' }
length.shard_buffer <- function(x) {
    x$n
}

#' Dimensions of a Shared Memory Buffer
#'
#' @param x A \code{shard_buffer} object.
#' @return An integer vector of dimensions, or \code{NULL} for 1-D buffers.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("double", dim = c(4, 5))
#' dim(buf)
#' buffer_close(buf)
#' }
dim.shard_buffer <- function(x) {
    if (length(x$dim) == 1) NULL else x$dim
}

# Internal: read a range of elements from buffer
.buffer_read_range <- function(x, start, count) {
    offset <- (start - 1L) * x$elem_size
    nbytes <- count * x$elem_size

    raw_data <- segment_read(x$segment, offset = offset, size = nbytes)

    # Convert raw to appropriate type
    switch(x$type,
        "double"  = readBin(raw_data, "double", n = count),
        "integer" = readBin(raw_data, "integer", n = count),
        "logical" = as.logical(readBin(raw_data, "integer", n = count)),
        "raw"     = raw_data
    )
}

# Internal: write a range of elements to buffer
.buffer_write_range <- function(x, start, values) {
    count <- length(values)
    offset <- (start - 1L) * x$elem_size

    # Convert values to appropriate type and write
    typed_values <- switch(x$type,
        "double"  = as.double(values),
        "integer" = as.integer(values),
        "logical" = as.integer(as.logical(values)),  # Store as int
        "raw"     = as.raw(values)
    )

    segment_write(x$segment, typed_values, offset = offset)

    # Update per-process diagnostics after the write completes.
    .buffer_diag_env$writes <- .buffer_diag_env$writes + 1L
    .buffer_diag_env$bytes <- .buffer_diag_env$bytes + (count * x$elem_size)
}

# Internal: write arbitrary indices safely without read/modify/write.
# This is critical for disjoint parallel writes where indices are not one single
# contiguous run (e.g., singleton writes when block_size=1).
.buffer_write_indices <- function(x, idx, values) {
    idx <- as.integer(idx)
    if (length(idx) == 0) return(invisible(NULL))

    # If indices are strictly increasing, coalesce adjacent runs and write each
    # run as one contiguous segment write.
    if (length(idx) == 1L || (all(!is.na(idx)) && all(diff(idx) >= 0L) && !anyDuplicated(idx))) {
        run_start_pos <- 1L
        for (k in 2L:(length(idx) + 1L)) {
            is_break <- (k > length(idx)) || (idx[k] != (idx[k - 1L] + 1L))
            if (is_break) {
                start <- idx[run_start_pos]
                end_pos <- k - 1L
                .buffer_write_range(x, start, values[run_start_pos:end_pos])
                run_start_pos <- k
            }
        }
        return(invisible(NULL))
    }

    # Fallback: preserve R assignment semantics for duplicates/out-of-order by
    # performing writes in the given order (last write wins).
    for (k in seq_along(idx)) {
        .buffer_write_range(x, idx[k], values[k])
    }
    invisible(NULL)
}

#' Extract Buffer Elements
#'
#' @param x A shard_buffer object.
#' @param i Index or indices.
#' @param j Optional second index (for matrices).
#' @param ... Additional indices (for arrays).
#' @param drop Whether to drop dimensions.
#' @return A vector or array of values read from the buffer.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("double", dim = 10)
#' buf[1:5] <- 1:5
#' buf[1:3]
#' buffer_close(buf)
#' }
`[.shard_buffer` <- function(x, i, j, ..., drop = TRUE) {
    # Determine if this is matrix-style indexing by checking nargs()
    # nargs() > 2 means we have x[i, ...] style (at least one comma)
    is_matrix_style <- nargs() > 2

    # Handle empty index (return all)
    if (missing(i) && missing(j) && length(list(...)) == 0) {
        result <- .buffer_read_range(x, 1L, x$n)
        if (length(x$dim) > 1) {
            dim(result) <- x$dim
        }
        return(result)
    }

    # For 1D buffers or linear indexing (no comma used)
    if (length(x$dim) == 1 || !is_matrix_style) {
        if (missing(i)) {
            i <- seq_len(x$n)
        }
        i <- as.integer(i)

        # Check bounds
        if (any(i < 1 | i > x$n, na.rm = TRUE)) {
            stop("Index out of bounds")
        }

        # Optimize for contiguous ranges
        if (length(i) > 1 && all(diff(i) == 1L)) {
            result <- .buffer_read_range(x, i[1], length(i))
        } else {
            # Non-contiguous: read all and subset
            # TODO: optimize for sparse reads
            all_data <- .buffer_read_range(x, 1L, x$n)
            result <- all_data[i]
        }
        return(result)
    }

    # Multi-dimensional indexing
    if (length(x$dim) == 2) {
        # Matrix case
        nrow <- x$dim[1]
        ncol <- x$dim[2]

        if (missing(i)) i <- seq_len(nrow)
        if (missing(j)) j <- seq_len(ncol)

        i <- as.integer(i)
        j <- as.integer(j)

        # Convert to linear indices (column-major)
        linear_idx <- as.vector(outer(i, (j - 1L) * nrow, `+`))

        result <- .buffer_read_range(x, 1L, x$n)[linear_idx]

        if (!drop || (length(i) > 1 && length(j) > 1)) {
            dim(result) <- c(length(i), length(j))
        }
        return(result)
    }

    # General array case
    args <- c(list(i), if (!missing(j)) list(j), list(...))
    # Read all data and use standard array subsetting
    all_data <- .buffer_read_range(x, 1L, x$n)
    dim(all_data) <- x$dim
    do.call(`[`, c(list(all_data), args, list(drop = drop)))
}

#' Assign to Buffer Elements
#'
#' @param x A shard_buffer object.
#' @param i Index or indices.
#' @param j Optional second index (for matrices).
#' @param ... Additional indices (for arrays).
#' @param value Values to assign.
#' @return The modified \code{shard_buffer} object, invisibly.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("double", dim = 10)
#' buf[1:5] <- rnorm(5)
#' buffer_close(buf)
#' }
`[<-.shard_buffer` <- function(x, i, j, ..., value) {
    # Determine if this is matrix-style indexing by checking nargs()
    # nargs() > 3 means we have x[i, ...] <- value style (at least one comma)
    is_matrix_style <- nargs() > 3

    # For 1D buffers or linear indexing (no comma used)
    if (length(x$dim) == 1 || !is_matrix_style) {
        if (missing(i)) {
            i <- seq_len(x$n)
        }
        i <- as.integer(i)

        # Check bounds
        if (any(i < 1 | i > x$n, na.rm = TRUE)) {
            stop("Index out of bounds")
        }

        # Recycle value if needed
        if (length(value) != length(i)) {
            value <- rep_len(value, length(i))
        }

        # Optimize for contiguous ranges
        if (length(i) > 1 && all(diff(i) == 1L)) {
            .buffer_write_range(x, i[1], value)
        } else {
            # Non-contiguous: write indices directly to avoid read/modify/write.
            # This keeps disjoint parallel writes correct (lock-free) and avoids
            # clobbering other workers' updates.
            .buffer_write_indices(x, i, value)
        }
        return(invisible(x))
    }

    # Multi-dimensional indexing
    if (length(x$dim) == 2) {
      nrow <- x$dim[1]
      ncol <- x$dim[2]

      if (missing(i)) i <- seq_len(nrow)
      if (missing(j)) j <- seq_len(ncol)

      i <- as.integer(i)
      j <- as.integer(j)

      # Fast path for contiguous block assignment to avoid read/modify/write
      # of the entire buffer (critical for parallel disjoint writes).
      is_contig <- function(idx) length(idx) <= 1L || all(diff(idx) == 1L)

      if (length(i) > 0 && length(j) > 0 && is_contig(i) && is_contig(j) &&
          is.matrix(value) && identical(dim(value), c(length(i), length(j)))) {
        # If writing full rows for contiguous columns, write one contiguous span.
        if (identical(i, seq_len(nrow))) {
          start_lin <- (j[1] - 1L) * nrow + 1L
          .buffer_write_range(x, start_lin, as.vector(value))
          return(invisible(x))
        }

        # Otherwise write each column slice separately.
        for (k in seq_along(j)) {
          col <- j[k]
          start_lin <- (col - 1L) * nrow + i[1]
          .buffer_write_range(x, start_lin, value[, k])
        }
        return(invisible(x))
      }

      # Fallback: read all, modify, write back (slow; avoid in hot paths).
      all_data <- .buffer_read_range(x, 1L, x$n)
      dim(all_data) <- x$dim
      all_data[i, j] <- value
      dim(all_data) <- NULL
      .buffer_write_range(x, 1L, all_data)
      return(invisible(x))
    }

    # General array case
    all_data <- .buffer_read_range(x, 1L, x$n)
    dim(all_data) <- x$dim
    args <- c(list(i), if (!missing(j)) list(j), list(...))
    all_data <- do.call(`[<-`, c(list(all_data), args, list(value = value)))
    dim(all_data) <- NULL
    .buffer_write_range(x, 1L, all_data)
    invisible(x)
}

#' Coerce a Shared Memory Buffer to a Vector
#'
#' @param x A \code{shard_buffer} object.
#' @param mode Storage mode passed to \code{\link{as.vector}}.
#' @return A vector of the buffer's type (or coerced to \code{mode}).
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("double", dim = 5)
#' buf[1:5] <- 1:5
#' as.vector(buf)
#' buffer_close(buf)
#' }
as.vector.shard_buffer <- function(x, mode = "any") {
    result <- .buffer_read_range(x, 1L, x$n)
    if (mode != "any") {
        result <- as.vector(result, mode = mode)
    }
    result
}

#' Coerce a Shared Memory Buffer to Double
#'
#' @param x A \code{shard_buffer} object.
#' @param ... Ignored.
#' @return A double vector with the buffer contents.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("double", dim = 5)
#' as.double(buf)
#' buffer_close(buf)
#' }
as.double.shard_buffer <- function(x, ...) {
    as.double(.buffer_read_range(x, 1L, x$n))
}

#' Coerce a Shared Memory Buffer to Integer
#'
#' @param x A \code{shard_buffer} object.
#' @param ... Ignored.
#' @return An integer vector with the buffer contents.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("integer", dim = 5)
#' as.integer(buf)
#' buffer_close(buf)
#' }
as.integer.shard_buffer <- function(x, ...) {
    as.integer(.buffer_read_range(x, 1L, x$n))
}

#' Coerce a Shared Memory Buffer to Logical
#'
#' @param x A \code{shard_buffer} object.
#' @param ... Ignored.
#' @return A logical vector with the buffer contents.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("logical", dim = 5)
#' as.logical(buf)
#' buffer_close(buf)
#' }
as.logical.shard_buffer <- function(x, ...) {
    as.logical(.buffer_read_range(x, 1L, x$n))
}

#' Coerce a Shared Memory Buffer to Raw
#'
#' @param x A \code{shard_buffer} object.
#' @param ... Ignored.
#' @return A raw vector with the buffer contents.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("raw", dim = 5)
#' as.raw(buf)
#' buffer_close(buf)
#' }
as.raw.shard_buffer <- function(x, ...) {
    .buffer_read_range(x, 1L, x$n)
}

#' Coerce a Shared Memory Buffer to Matrix
#'
#' @param x A \code{shard_buffer} object (must be 2-dimensional).
#' @param ... Ignored.
#' @return A matrix with the buffer contents and the buffer's dimensions.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("double", dim = c(3, 4))
#' as.matrix(buf)
#' buffer_close(buf)
#' }
as.matrix.shard_buffer <- function(x, ...) {
    if (length(x$dim) != 2) {
        stop("Buffer is not 2-dimensional")
    }
    result <- .buffer_read_range(x, 1L, x$n)
    dim(result) <- x$dim
    result
}

#' Coerce a Shared Memory Buffer to Array
#'
#' @param x A \code{shard_buffer} object.
#' @param ... Ignored.
#' @return An array with the buffer contents and the buffer's dimensions, or a
#'   plain vector for 1-D buffers.
#' @export
#' @examples
#' \donttest{
#' buf <- buffer("double", dim = c(2, 3, 4))
#' as.array(buf)
#' buffer_close(buf)
#' }
as.array.shard_buffer <- function(x, ...) {
    result <- .buffer_read_range(x, 1L, x$n)
    if (length(x$dim) > 1) {
        dim(result) <- x$dim
    }
    result
}
