#' @title Zero-Copy Shared Objects
#' @name share
#' @description Create shared memory representations of R objects for efficient
#'   parallel access without duplication.
#'
#' @details
#' The `share()` function is the primary high-level API for creating zero-copy
#' shared inputs. When you share an object:
#'
#' \enumerate{
#'   \item The object is serialized into a shared memory segment
#'   \item The segment is marked read-only (protected)
#'   \item A lightweight handle is returned that can be passed to workers
#'   \item Workers attach to the segment and deserialize on demand
#' }
#'
#' This approach eliminates per-worker duplication of large inputs. The data
#' exists once in shared memory, and all workers read from the same location.
#'
#' \strong{Immutability Contract}: Shared objects are immutable by design.
#' Any attempt to modify shared data in a worker will fail. This guarantees
#' deterministic behavior and prevents accidental copy-on-write.
#'
#' @seealso \code{\link{segment_create}} for low-level segment operations,
#'   \code{\link{pool_create}} for worker pool management.
#'
#' @useDynLib shard, .registration = TRUE
NULL

#' Share an R Object for Parallel Access
#'
#' Creates a shared memory representation of an R object. The object is
#' serialized once and can be accessed by multiple worker processes without
#' copying.
#'
#' @param x An R object to share. Supports vectors, matrices, arrays, lists,
#'   data frames, and any object that can be serialized with \code{serialize()}.
#' @param backing Backing type: "auto" (default), "mmap", or "shm".
#'   \itemize{
#'     \item \code{"auto"}: Let the system choose the best option.
#'     \item \code{"mmap"}: File-backed memory mapping. Most portable.
#'     \item \code{"shm"}: POSIX shared memory or Windows named mapping.
#'   }
#' @param readonly Logical. If TRUE (default), the segment is protected after
#'   writing, making it read-only. Set to FALSE only if you need to modify
#'   the shared data (advanced use case).
#' @param name Optional name for the shared object. If NULL (default), a unique
#'   name is generated. Named shares can be opened by name in other processes.
#'
#' @return A \code{shard_shared} object containing:
#'   \itemize{
#'     \item \code{path}: The path or name of the shared segment
#'     \item \code{backing}: The backing type used
#'     \item \code{size}: Total size in bytes
#'     \item \code{readonly}: Whether the segment is protected
#'     \item \code{class_info}: Original class information
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Share a large matrix
#' mat <- matrix(rnorm(1e6), nrow = 1000)
#' shared_mat <- share(mat)
#'
#' # The shared object is lightweight - only metadata
#' print(object.size(shared_mat))  # Small
#'
#' # Get the data back with fetch()
#' recovered <- fetch(shared_mat)
#' identical(mat, recovered)  # TRUE
#'
#' # Use in parallel (workers access without copying)
#' pool_create(4)
#' result <- pool_lapply(1:10, function(i) {
#'   # Workers can access shared_mat efficiently
#'   data <- fetch(shared_mat)
#'   sum(data[i, ])
#' })
#'
#' # Clean up when done
#' close(shared_mat)
#' }
share <- function(x,
                  backing = c("auto", "mmap", "shm"),
                  readonly = TRUE,
                  name = NULL) {
    backing <- match.arg(backing)

    # Serialize the object
    serialized <- serialize(x, connection = NULL, xdr = FALSE)
    size <- length(serialized)

    # Create segment with enough space for the serialized data
    seg <- segment_create(size, backing = backing, path = name, readonly = FALSE)

    # Write serialized data
    segment_write(seg, serialized, offset = 0)

    # Protect if requested
    if (readonly) {
        segment_protect(seg)
    }

    # Get segment info for the handle
    info <- segment_info(seg)

    # Extract class information for better deserialization hints
    class_info <- if (is.matrix(x)) {
        list(type = "matrix", dim = dim(x), mode = mode(x))
    } else if (is.array(x)) {
        list(type = "array", dim = dim(x), mode = mode(x))
    } else if (is.data.frame(x)) {
        list(type = "data.frame", nrow = nrow(x), ncol = ncol(x))
    } else if (is.list(x)) {
        list(type = "list", length = length(x))
    } else {
        list(type = "vector", length = length(x), mode = mode(x))
    }

    structure(
        list(
            segment = seg,
            path = info$path,
            backing = info$backing,
            size = info$size,
            readonly = readonly,
            class_info = class_info
        ),
        class = "shard_shared"
    )
}

#' Fetch Data from a Shared Object
#'
#' Retrieves the R object from shared memory by deserializing it. This is the
#' primary way to access shared data in workers.
#'
#' @param x A \code{shard_shared} object.
#' @param ... Ignored.
#'
#' @return The original R object that was shared.
#'
#' @details
#' When called in the main process, this reads from the existing segment.
#' When called in a worker process, this opens the segment by path and
#' deserializes the data.
#'
#' The \code{fetch()} function is the primary way to access shared data.
#' It can also be called as \code{materialize()} for compatibility.
#'
#' @export
#' @examples
#' \dontrun{
#' x <- 1:1000
#' shared <- share(x)
#' recovered <- fetch(shared)
#' identical(x, recovered)  # TRUE
#' }
fetch <- function(x, ...) {
    UseMethod("fetch")
}

#' @export
fetch.shard_shared <- function(x, ...) {
    # If we have a valid segment pointer, read directly
    if (!is.null(x$segment) && inherits(x$segment, "shard_segment")) {
        raw_data <- segment_read(x$segment, offset = 0, size = x$size)
        return(unserialize(raw_data))
    }

    # Otherwise, open by path (worker process scenario)
    if (is.null(x$path)) {
        stop("Cannot fetch: no path available", call. = FALSE)
    }

    seg <- segment_open(x$path, backing = x$backing, readonly = TRUE)
    on.exit(segment_close(seg, unlink = FALSE))

    raw_data <- segment_read(seg, offset = 0, size = x$size)
    unserialize(raw_data)
}

#' @export
fetch.default <- function(x, ...) {
    x
}

#' Materialize Shared Object
#'
#' Alias for \code{fetch()}. Retrieves the R object from shared memory.
#'
#' @param x A \code{shard_shared} object.
#' @return The original R object.
#' @export
#' @examples
#' \dontrun{
#' shared <- share(1:100)
#' data <- materialize(shared)
#' }
materialize <- function(x) {
    UseMethod("materialize")
}

#' @export
materialize.shard_shared <- function(x) {
    fetch.shard_shared(x)
}

#' @export
materialize.default <- function(x) {
    x
}

#' Close a Shared Object
#'
#' Releases the shared memory segment. After closing, the shared object can
#' no longer be accessed.
#'
#' @param con A \code{shard_shared} object.
#' @param ... Ignored.
#'
#' @return NULL (invisibly).
#' @export
#' @method close shard_shared
close.shard_shared <- function(con, ...) {
    if (!is.null(con$segment)) {
        segment_close(con$segment, unlink = TRUE)
    }
    invisible(NULL)
}

#' Check if Object is Shared
#'
#' @param x An object to check.
#' @return TRUE if x is a \code{shard_shared} object, FALSE otherwise.
#' @export
#' @examples
#' is_shared(share(1:10))  # TRUE
#' is_shared(1:10)         # FALSE
is_shared <- function(x) {
    inherits(x, "shard_shared")
}

#' Get Information About a Shared Object
#'
#' @param x A \code{shard_shared} object.
#' @return A list with detailed information about the shared segment.
#' @export
shared_info <- function(x) {
    stopifnot(inherits(x, "shard_shared"))

    list(
        path = x$path,
        backing = x$backing,
        size = x$size,
        readonly = x$readonly,
        class_info = x$class_info,
        segment_info = if (!is.null(x$segment)) segment_info(x$segment) else NULL
    )
}

#' @export
print.shard_shared <- function(x, ...) {
    cat("<shard_shared>\n")
    cat("  Path:", x$path, "\n")
    cat("  Size:", format(x$size, big.mark = ","), "bytes\n")
    cat("  Backing:", x$backing, "\n")
    cat("  Read-only:", x$readonly, "\n")

    info <- x$class_info
    if (info$type == "matrix") {
        cat("  Original: matrix [", info$dim[1], " x ", info$dim[2], "], ",
            info$mode, "\n", sep = "")
    } else if (info$type == "array") {
        cat("  Original: array [", paste(info$dim, collapse = " x "), "], ",
            info$mode, "\n", sep = "")
    } else if (info$type == "data.frame") {
        cat("  Original: data.frame [", info$nrow, " x ", info$ncol, "]\n", sep = "")
    } else if (info$type == "list") {
        cat("  Original: list [", info$length, " elements]\n", sep = "")
    } else {
        cat("  Original: ", info$mode, " vector [", info$length, "]\n", sep = "")
    }

    invisible(x)
}

#' Open an Existing Shared Object by Path
#'
#' Opens a shared object that was created by another process. This is useful
#' for workers that need to attach to shared data without having the original
#' \code{shard_shared} object.
#'
#' @param path Path to the shared segment.
#' @param backing Backing type: "mmap" or "shm".
#' @param size Size of the segment in bytes. If NULL, attempts to detect.
#'
#' @return A \code{shard_shared} object.
#' @export
share_open <- function(path, backing = c("mmap", "shm"), size = NULL) {
    backing <- match.arg(backing)

    seg <- segment_open(path, backing = backing, readonly = TRUE)
    info <- segment_info(seg)

    if (is.null(size)) {
        size <- info$size
    }

    structure(
        list(
            segment = seg,
            path = info$path,
            backing = info$backing,
            size = size,
            readonly = TRUE,
            class_info = list(type = "unknown")
        ),
        class = "shard_shared"
    )
}
