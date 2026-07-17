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
.buffer_diag_env$reads <- 0L
.buffer_diag_env$read_bytes <- 0
.buffer_diag_env$init_writes <- 0L
.buffer_diag_env$init_bytes <- 0

#' Buffer Diagnostics
#'
#' Returns per-process counters for shard buffer reads and writes. shard_map
#' uses write counters internally to report write volume/operations in
#' copy_report().
#'
#' @return A list with elements \code{writes} (integer count), \code{bytes}
#'   (total bytes written), \code{reads}, \code{read_bytes},
#'   \code{init_writes}, and \code{init_bytes}
#'   accumulated in the current process.
#' @export
#' @examples
#' buffer_diagnostics()
buffer_diagnostics <- function() {
    list(
        writes = .buffer_diag_env$writes,
        bytes = .buffer_diag_env$bytes,
        reads = .buffer_diag_env$reads,
        read_bytes = .buffer_diag_env$read_bytes,
        init_writes = .buffer_diag_env$init_writes,
        init_bytes = .buffer_diag_env$init_bytes
    )
}

# Reset counters (internal; tests may use shard:::buffer_reset_diagnostics()).
buffer_reset_diagnostics <- function() {
    .buffer_diag_env$writes <- 0L
    .buffer_diag_env$bytes <- 0
    .buffer_diag_env$reads <- 0L
    .buffer_diag_env$read_bytes <- 0
    .buffer_diag_env$init_writes <- 0L
    .buffer_diag_env$init_bytes <- 0
    invisible(NULL)
}

.buffer_is_zero_init <- function(type, init, init_was_null) {
    if (isTRUE(init_was_null)) return(TRUE)
    if (length(init) == 0L) return(FALSE)

    vals <- switch(type,
        "double"  = as.double(init),
        "integer" = as.integer(init),
        "logical" = as.logical(init),
        "raw"     = as.raw(init)
    )

    if (anyNA(vals)) return(FALSE)
    switch(type,
        "double"  = all(vals == 0),
        "integer" = all(vals == 0L),
        "logical" = all(!vals),
        "raw"     = all(vals == as.raw(0))
    )
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

    # Initialize buffer contents. Newly created mappings are zero-filled by the
    # OS, so NULL/type-zero initialization can skip an otherwise full-size copy.
    init_was_null <- is.null(init)
    if (init_was_null) {
        init <- switch(type,
            "double"  = 0,
            "integer" = 0L,
            "logical" = FALSE,
            "raw"     = raw(1)
        )
    }

    if (!.buffer_is_zero_init(type, init, init_was_null)) {
        init_data <- switch(type,
            "double"  = rep(as.double(init), n),
            "integer" = rep(as.integer(init), n),
            "logical" = rep(as.logical(init), n),
            "raw"     = rep(as.raw(init), n)
        )
        segment_write(seg, init_data, offset = 0)
        .buffer_diag_env$init_writes <- .buffer_diag_env$init_writes + 1L
        .buffer_diag_env$init_bytes <- .buffer_diag_env$init_bytes + byte_size
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

.buffer_empty <- function(type) {
    switch(type,
        "double"  = double(0),
        "integer" = integer(0),
        "logical" = logical(0),
        "raw"     = raw(0)
    )
}

.buffer_cast_values <- function(type, values) {
    switch(type,
        "double"  = as.double(values),
        "integer" = as.integer(values),
        "logical" = as.logical(values),
        "raw"     = as.raw(values)
    )
}

.buffer_check_range <- function(r, n) {
    if (!is_idx_range(r)) stop("Index out of bounds", call. = FALSE)
    if (r$end > n) stop("Index out of bounds", call. = FALSE)
    r
}

.buffer_range_len <- function(r) {
    as.integer(r$end - r$start + 1L)
}

.buffer_validate_index <- function(idx, n, allow_negative = FALSE) {
    idx <- as.integer(idx)
    if (anyNA(idx)) {
        stop("Index out of bounds", call. = FALSE)
    }

    if (allow_negative) {
        if (length(idx) && min(idx) < 0L) {
            if (max(idx) > 0L) {
                stop("Index out of bounds", call. = FALSE)
            }
            drop <- -idx[idx < 0L]
            if (min(drop) < 1L || max(drop) > n) {
                stop("Index out of bounds", call. = FALSE)
            }
            return(seq_len(n)[-unique(drop)])
        }
        idx <- idx[idx != 0L]
    }

    # min/max instead of `any(idx < 1L | idx > n)`: no logical temporaries, and
    # O(1) on ALTREP-compact ranges (which shard descriptors preserve).
    if (length(idx) && (min(idx) < 1L || max(idx) > n)) {
        stop("Index out of bounds", call. = FALSE)
    }
    idx
}

.buffer_is_contiguous <- function(idx) {
    n <- length(idx)
    # Span + strict sortedness == consecutive run; avoids diff(), which
    # materializes ALTREP-compact index ranges twice.
    n <= 1L ||
        (idx[[n]] - idx[[1L]] + 1L == n && !is.unsorted(idx, strictly = TRUE))
}

.buffer_selector <- function(idx, n, missing_selector = FALSE, allow_negative = FALSE) {
    n <- as.integer(n)
    if (missing_selector) {
        return(list(kind = "range", start = 1L, end = n, len = n, all = TRUE))
    }
    if (is_idx_range(idx)) {
        idx <- .buffer_check_range(idx, n)
        len <- .buffer_range_len(idx)
        return(list(
            kind = "range",
            start = idx$start,
            end = idx$end,
            len = len,
            all = identical(idx$start, 1L) && identical(idx$end, n)
        ))
    }
    idx <- .buffer_validate_index(idx, n, allow_negative = allow_negative)
    list(
        kind = "index",
        idx = idx,
        len = length(idx),
        all = length(idx) == n && identical(idx, seq_len(n))
    )
}

.buffer_selector_indices <- function(sel) {
    if (identical(sel$kind, "range")) return(sel$start:sel$end)
    sel$idx
}

.buffer_selector_first <- function(sel) {
    if (identical(sel$kind, "range")) return(sel$start)
    sel$idx[1L]
}

.buffer_selector_contiguous <- function(sel) {
    identical(sel$kind, "range") || .buffer_is_contiguous(sel$idx)
}

.buffer_matrix_linear_idx <- function(rows, cols, nrow) {
    row_idx <- .buffer_selector_indices(rows)
    col_idx <- .buffer_selector_indices(cols)
    as.integer(as.vector(outer(row_idx, (col_idx - 1L) * nrow, `+`)))
}

# Column-major linear indices for an n-dimensional (>= 3-D) selection, without
# materializing the buffer. Traversal order matches base array subsetting.
.buffer_nd_linear_idx <- function(x, args) {
    dm <- x$dim
    if (length(args) != length(dm)) {
        stop("incorrect number of dimensions", call. = FALSE)
    }
    sels <- vector("list", length(dm))
    for (d in seq_along(dm)) {
        sels[[d]] <- .buffer_selector(args[[d]], as.integer(dm[[d]]),
                                      allow_negative = TRUE)
    }
    lens <- vapply(sels, function(s) s$len, integer(1))
    linear <- as.double(.buffer_selector_indices(sels[[1L]]))
    stride <- 1
    for (d in seq_along(dm)[-1L]) {
        stride <- stride * as.double(dm[[d - 1L]])
        offs <- (as.double(.buffer_selector_indices(sels[[d]])) - 1) * stride
        linear <- as.vector(outer(linear, offs, `+`))
    }
    list(idx = linear, lens = lens)
}

.buffer_read_matrix_column_ranges <- function(x, rows, cols, nrow) {
    result <- .buffer_empty(x$type)
    length(result) <- rows$len * cols$len

    col_idx <- .buffer_selector_indices(cols)
    for (k in seq_along(col_idx)) {
        start <- (col_idx[k] - 1L) * nrow + .buffer_selector_first(rows)
        pos <- ((k - 1L) * rows$len + 1L):(k * rows$len)
        result[pos] <- .buffer_read_range(x, start, rows$len)
    }
    result
}

.buffer_write_matrix_column_ranges <- function(x, rows, cols, nrow, values) {
    col_idx <- .buffer_selector_indices(cols)
    for (k in seq_along(col_idx)) {
        start <- (col_idx[k] - 1L) * nrow + .buffer_selector_first(rows)
        pos <- ((k - 1L) * rows$len + 1L):(k * rows$len)
        .buffer_write_range(x, start, values[pos])
    }
    invisible(NULL)
}

.buffer_matrix_values <- function(type, value, nrow, ncol) {
    n <- as.numeric(nrow) * as.numeric(ncol)
    if (n == 0) return(.buffer_empty(type))

    if (is.matrix(value) && identical(dim(value), c(as.integer(nrow), as.integer(ncol)))) {
        return(.buffer_cast_values(type, as.vector(value)))
    }

    if (length(value) != n) {
        value <- rep_len(value, n)
    }
    .buffer_cast_values(type, as.vector(matrix(value, nrow = nrow, ncol = ncol)))
}

# Internal: read a range of elements from buffer
.buffer_read_range <- function(x, start, count) {
    if (count == 0L) return(.buffer_empty(x$type))

    # Double arithmetic: element offsets can exceed .Machine$integer.max bytes.
    offset <- (as.double(start) - 1) * x$elem_size
    nbytes <- as.double(count) * x$elem_size

    # Typed read straight from the mapping: one allocation + one memcpy,
    # instead of a raw intermediate plus readBin() (two full copies).
    result <- .Call(
        "C_shard_segment_read_range",
        x$segment$ptr,
        offset,
        as.double(count),
        x$type,
        PACKAGE = "shard"
    )

    .buffer_diag_env$reads <- .buffer_diag_env$reads + 1L
    .buffer_diag_env$read_bytes <- .buffer_diag_env$read_bytes + nbytes

    result
}

# Internal: read arbitrary element indices from buffer without materializing all.
.buffer_read_indices <- function(x, idx) {
    idx <- .buffer_validate_index(idx, x$n)
    n <- length(idx)
    if (n == 0L) return(.buffer_empty(x$type))

    result <- .Call(
        "C_shard_segment_gather_read",
        x$segment$ptr,
        idx,
        .buffer_sexptype(x$type),
        PACKAGE = "shard"
    )

    .buffer_diag_env$reads <- .buffer_diag_env$reads + 1L
    .buffer_diag_env$read_bytes <- .buffer_diag_env$read_bytes + (n * x$elem_size)
    result
}

# Internal: write a range of elements to buffer
.buffer_write_range <- function(x, start, values) {
    count <- length(values)
    if (count == 0L) return(invisible(NULL))

    # Double arithmetic: element offsets can exceed .Machine$integer.max bytes.
    offset <- (as.double(start) - 1) * x$elem_size

    # Convert values to appropriate type and write
    typed_values <- .buffer_cast_values(x$type, values)

    segment_write(x$segment, typed_values, offset = offset)

    # Update per-process diagnostics after the write completes.
    .buffer_diag_env$writes <- .buffer_diag_env$writes + 1L
    .buffer_diag_env$bytes <- .buffer_diag_env$bytes + (count * x$elem_size)
}

# Internal: write arbitrary indices safely without read/modify/write.
# This is critical for disjoint parallel writes where indices are not one single
# contiguous run (e.g., singleton writes when block_size=1).
.buffer_write_indices <- function(x, idx, values) {
    idx <- .buffer_validate_index(idx, x$n)
    if (length(idx) == 0) return(invisible(NULL))

    typed_values <- .buffer_cast_values(x$type, values)
    .Call(
        "C_shard_segment_scatter_write",
        x$segment$ptr,
        idx,
        typed_values,
        .buffer_sexptype(x$type),
        PACKAGE = "shard"
    )

    .buffer_diag_env$writes <- .buffer_diag_env$writes + 1L
    .buffer_diag_env$bytes <- .buffer_diag_env$bytes + (length(idx) * x$elem_size)
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
            return(.buffer_read_range(x, 1L, x$n))
        }
        if (is_idx_range(i)) {
            i <- .buffer_check_range(i, x$n)
            return(.buffer_read_range(x, i$start, .buffer_range_len(i)))
        }

        i <- .buffer_validate_index(i, x$n)
        if (length(i) == 0L) return(.buffer_empty(x$type))

        # Optimize for contiguous ranges
        if (.buffer_is_contiguous(i)) {
            result <- .buffer_read_range(x, i[1], length(i))
        } else {
            result <- .buffer_read_indices(x, i)
        }
        return(result)
    }

    # Multi-dimensional indexing
    if (length(x$dim) == 2) {
        # Matrix case
        nrow <- x$dim[1]
        ncol <- x$dim[2]

        rows <- .buffer_selector(i, nrow, missing(i), allow_negative = TRUE)
        cols <- .buffer_selector(j, ncol, missing(j), allow_negative = TRUE)

        if (rows$len == 0L || cols$len == 0L) {
            result <- .buffer_empty(x$type)
        } else if (isTRUE(rows$all) && .buffer_selector_contiguous(cols)) {
            start <- (.buffer_selector_first(cols) - 1L) * nrow + 1L
            result <- .buffer_read_range(x, start, nrow * cols$len)
        } else if (.buffer_selector_contiguous(rows) && cols$len == 1L) {
            start <- (.buffer_selector_first(cols) - 1L) * nrow + .buffer_selector_first(rows)
            result <- .buffer_read_range(x, start, rows$len)
        } else if (.buffer_selector_contiguous(rows) && .buffer_selector_contiguous(cols)) {
            result <- .buffer_read_matrix_column_ranges(x, rows, cols, nrow)
        } else {
            linear_idx <- .buffer_matrix_linear_idx(rows, cols, nrow)
            result <- .buffer_read_indices(x, linear_idx)
        }

        if (!drop || (rows$len > 1 && cols$len > 1)) {
            dim(result) <- c(rows$len, cols$len)
        }
        return(result)
    }

    # General array case: gather only the selection instead of materializing
    # the whole buffer.
    args <- c(list(i), if (!missing(j)) list(j), list(...))
    sel <- .buffer_nd_linear_idx(x, args)
    result <- if (.buffer_is_contiguous(sel$idx)) {
        .buffer_read_range(x, sel$idx[[1L]], length(sel$idx))
    } else {
        .buffer_read_indices(x, sel$idx)
    }
    dim(result) <- sel$lens
    if (drop) result <- drop(result)
    result
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
            i <- idx_range(1L, x$n)
        }

        if (is_idx_range(i)) {
            i <- .buffer_check_range(i, x$n)
            n_target <- .buffer_range_len(i)
            if (length(value) != n_target) {
                value <- rep_len(value, n_target)
            }
            .buffer_write_range(x, i$start, value)
            return(invisible(x))
        }

        i <- .buffer_validate_index(i, x$n)
        n_target <- length(i)
        if (n_target == 0L) return(invisible(x))

        # Recycle value if needed
        if (length(value) != n_target) {
            value <- rep_len(value, n_target)
        }

        # Optimize for contiguous ranges
        if (.buffer_is_contiguous(i)) {
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

      rows <- .buffer_selector(i, nrow, missing(i), allow_negative = TRUE)
      cols <- .buffer_selector(j, ncol, missing(j), allow_negative = TRUE)

      if (rows$len == 0L || cols$len == 0L) {
        return(invisible(x))
      }

      values <- .buffer_matrix_values(x$type, value, rows$len, cols$len)

      if (isTRUE(rows$all) && .buffer_selector_contiguous(cols)) {
        start_lin <- (.buffer_selector_first(cols) - 1L) * nrow + 1L
        .buffer_write_range(x, start_lin, values)
        return(invisible(x))
      }

      if (.buffer_selector_contiguous(rows) && cols$len == 1L) {
        start_lin <- (.buffer_selector_first(cols) - 1L) * nrow + .buffer_selector_first(rows)
        .buffer_write_range(x, start_lin, values)
        return(invisible(x))
      }

      if (.buffer_selector_contiguous(rows) && .buffer_selector_contiguous(cols)) {
        .buffer_write_matrix_column_ranges(x, rows, cols, nrow, values)
        return(invisible(x))
      }

      linear_idx <- .buffer_matrix_linear_idx(rows, cols, nrow)
      .buffer_write_indices(x, linear_idx, values)
      return(invisible(x))
    }

    # General array case: write only the selection. A whole-buffer
    # read-modify-write would break the lock-free disjoint parallel-write
    # guarantee the 1-D/2-D paths preserve.
    args <- c(list(i), if (!missing(j)) list(j), list(...))
    sel <- .buffer_nd_linear_idx(x, args)
    n_target <- prod(sel$lens)
    if (n_target == 0) return(invisible(x))
    values <- .buffer_cast_values(x$type, as.vector(value))
    if (length(values) != n_target) {
        values <- rep_len(values, n_target)
    }
    if (.buffer_is_contiguous(sel$idx)) {
        .buffer_write_range(x, sel$idx[[1L]], values)
    } else {
        .buffer_write_indices(x, sel$idx, values)
    }
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
