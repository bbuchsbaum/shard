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

# Internal helper: check for non-serializable objects
# Returns NULL on success, stops with error on failure
validate_serializable <- function(x, path = "x") {
    # Check for functions (closures)
    if (is.function(x)) {
        stop("Cannot share functions (closures).\n",
             "  Found at: ", path, "\n",
             "  Hint: Extract the data you need and share that instead.",
             call. = FALSE)
    }

    # Check for external pointers
    if (typeof(x) == "externalptr") {
        stop("Cannot share external pointers.\n",
             "  Found at: ", path, "\n",
             "  External pointers reference memory that cannot be serialized.",
             call. = FALSE)
    }

    # Check for connections
    if (inherits(x, "connection")) {
        stop("Cannot share connection objects.\n",
             "  Found at: ", path, "\n",
             "  Connections cannot be serialized.",
             call. = FALSE)
    }

    # Recursively check environments
    if (is.environment(x)) {
        # Skip base/global/empty environments (these serialize fine)
        if (identical(x, baseenv()) ||
            identical(x, globalenv()) ||
            identical(x, emptyenv())) {
            return(invisible(NULL))
        }

        # Check contents of user environments
        for (nm in names(x)) {
            validate_serializable(x[[nm]], paste0(path, "$", nm))
        }
    }

    # Recursively check lists
    if (is.list(x) && !is.data.frame(x)) {
        nms <- names(x)
        for (i in seq_along(x)) {
            item_path <- if (!is.null(nms) && nzchar(nms[i])) {
                paste0(path, "$", nms[i])
            } else {
                paste0(path, "[[", i, "]]")
            }
            validate_serializable(x[[i]], item_path)
        }
    }

    # Recursively check S4 slots
    if (isS4(x)) {
        slot_names <- slotNames(x)
        for (sn in slot_names) {
            validate_serializable(slot(x, sn), paste0(path, "@", sn))
        }
    }

    invisible(NULL)
}

# Helper: get object identity (memory address) for alias detection
object_identity <- function(x) {
    # Use data.table::address() if available for more reliable address extraction
    # Fall back to lobstr::obj_addr() or manual extraction from capture.output
    if (requireNamespace("data.table", quietly = TRUE)) {
        return(data.table::address(x))
    }
    # Extract memory address from environment representation
    # This gives consistent identity for the same object
    addr <- capture.output(.Internal(inspect(x)))[1]
    # Extract the hex address from the output
    m <- regmatches(addr, regexpr("@[0-9a-f]+", addr))
    if (length(m) > 0) m else paste0("obj_", sample.int(1e9, 1))
}

# Helper: check if object is a shareable atomic type
is_shareable_atomic <- function(x) {
    # Shareable: numeric vectors (double, integer, complex), logical, raw
    # Not shareable: character, lists, environments, functions, etc.
    if (is.atomic(x) && !is.null(x)) {
        mode <- typeof(x)
        return(mode %in% c("double", "integer", "logical", "raw", "complex"))
    }
    FALSE
}

# Internal: traverse object for deep sharing with memoization
# Returns a structure with shared segments and alias information
share_deep_traverse <- function(x,
                                env,
                                path = "<root>",
                                depth = 0,
                                min_bytes = 64 * 1024 * 1024,
                                backing = "auto",
                                readonly = TRUE,
                                max_depth = Inf,
                                cycle_policy = "error") {
    # Get object identity for memoization and cycle detection
    identity <- object_identity(x)

    # Check for cycle (currently in traversal stack)
    if (exists(identity, envir = env$in_progress, inherits = FALSE)) {
        if (cycle_policy == "error") {
            stop("Cycle detected during deep sharing.\n",
                 "  Path: ", path, "\n",
                 "  Object is self-referential.\n",
                 "  Use cycle='skip' to skip cyclic references instead.",
                 call. = FALSE)
        }
        # cycle='skip': return a marker indicating cycle
        return(structure(
            list(
                type = "cycle",
                path = path,
                identity = identity
            ),
            class = "shard_deep_cycle"
        ))
    }

    # Check for alias (already seen and shared)
    if (exists(identity, envir = env$seen, inherits = FALSE)) {
        original <- get(identity, envir = env$seen, inherits = FALSE)
        return(structure(
            list(
                type = "alias",
                path = path,
                identity = identity,
                alias_of = original$path,
                alias_node_id = original$node_id,
                shared = original$shared,
                value = original$value  # For aliases to kept values
            ),
            class = "shard_deep_alias"
        ))
    }

    # Mark as in-progress for cycle detection
    assign(identity, TRUE, envir = env$in_progress)
    on.exit(rm(list = identity, envir = env$in_progress), add = TRUE)

    # Assign a node ID for tracking
    env$next_id <- env$next_id + 1
    node_id <- env$next_id

    # Determine what to do with this object
    if (is_shareable_atomic(x) && object.size(x) >= min_bytes) {
        # Share this atomic object
        shared <- share(x, backing = backing, readonly = readonly)

        # Record in seen table for alias detection
        assign(identity, list(
            path = path,
            node_id = node_id,
            shared = shared
        ), envir = env$seen)

        # Track the shared segment for cleanup
        env$segments <- c(env$segments, list(shared))

        return(structure(
            list(
                type = "shared",
                path = path,
                node_id = node_id,
                identity = identity,
                shared = shared
            ),
            class = "shard_deep_node"
        ))
    } else if (is.list(x) && !is.data.frame(x) && depth < max_depth) {
        # Recursively process list elements
        nms <- names(x)
        children <- vector("list", length(x))
        names(children) <- nms

        for (i in seq_along(x)) {
            child_path <- if (!is.null(nms) && nzchar(nms[i])) {
                paste0(path, "$", nms[i])
            } else {
                paste0(path, "[[", i, "]]")
            }
            children[[i]] <- share_deep_traverse(
                x[[i]], env, child_path, depth + 1,
                min_bytes, backing, readonly, max_depth, cycle_policy
            )
        }

        # Record in seen table
        assign(identity, list(
            path = path,
            node_id = node_id,
            shared = NULL
        ), envir = env$seen)

        return(structure(
            list(
                type = "container",
                container_type = "list",
                path = path,
                node_id = node_id,
                identity = identity,
                children = children,
                original_class = class(x),
                original_names = nms
            ),
            class = "shard_deep_container"
        ))
    } else if (is.data.frame(x) && depth < max_depth) {
        # Process data.frame columns
        children <- vector("list", ncol(x))
        names(children) <- names(x)

        for (i in seq_along(x)) {
            child_path <- paste0(path, "$", names(x)[i])
            children[[i]] <- share_deep_traverse(
                x[[i]], env, child_path, depth + 1,
                min_bytes, backing, readonly, max_depth, cycle_policy
            )
        }

        # Record in seen table
        assign(identity, list(
            path = path,
            node_id = node_id,
            shared = NULL
        ), envir = env$seen)

        return(structure(
            list(
                type = "container",
                container_type = "data.frame",
                path = path,
                node_id = node_id,
                identity = identity,
                children = children,
                original_class = class(x),
                original_names = names(x),
                original_nrow = nrow(x),
                original_row_names = attr(x, "row.names")
            ),
            class = "shard_deep_container"
        ))
    } else {
        # Keep as-is (small atomic, unshareable type, or max depth reached)
        # Record in seen table (including value for alias reconstruction)
        assign(identity, list(
            path = path,
            node_id = node_id,
            shared = NULL,
            value = x
        ), envir = env$seen)

        return(structure(
            list(
                type = "kept",
                path = path,
                node_id = node_id,
                identity = identity,
                value = x
            ),
            class = "shard_deep_kept"
        ))
    }
}

# Internal: reconstruct object from deep shared structure
fetch_deep_reconstruct <- function(node) {
    if (inherits(node, "shard_deep_node")) {
        # Shared atomic - fetch from shared segment
        return(fetch(node$shared))
    } else if (inherits(node, "shard_deep_alias")) {
        # Alias - fetch from the original shared segment or return kept value
        if (!is.null(node$shared)) {
            return(fetch(node$shared))
        } else {
            # Alias to a kept value - return the stored value
            return(node$value)
        }
    } else if (inherits(node, "shard_deep_cycle")) {
        # Cycle marker - return NULL or special marker
        # This shouldn't normally be reached if cycle='error'
        return(NULL)
    } else if (inherits(node, "shard_deep_kept")) {
        # Kept value - return as-is
        return(node$value)
    } else if (inherits(node, "shard_deep_container")) {
        # Container - reconstruct recursively
        children <- lapply(node$children, fetch_deep_reconstruct)

        if (node$container_type == "data.frame") {
            # Reconstruct data.frame
            result <- structure(
                children,
                class = node$original_class,
                names = node$original_names,
                row.names = node$original_row_names
            )
        } else {
            # Reconstruct list
            result <- children
            if (!is.null(node$original_names)) {
                names(result) <- node$original_names
            }
            if (!identical(node$original_class, "list")) {
                class(result) <- node$original_class
            }
        }
        return(result)
    } else {
        # Unknown type - return as-is
        return(node)
    }
}

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
#' @param deep Logical. If TRUE, recursively traverse lists and data.frames,
#'   sharing individual components that meet the size threshold. When FALSE
#'   (default), the entire object is serialized as one unit.
#' @param min_bytes Minimum size in bytes for an object to be shared when
#'   deep=TRUE. Objects smaller than this threshold are kept in-place.
#'   Default is 64MB (64 * 1024 * 1024).
#' @param cycle How to handle cyclic references when deep=TRUE. Either "error"
#'   (default) to stop with an error, or "skip" to skip cyclic references.
#'
#' @return A \code{shard_shared} object (when deep=FALSE) or
#'   \code{shard_deep_shared} object (when deep=TRUE) containing:
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
#'
#' # Deep sharing with alias preservation
#' big_mat <- matrix(rnorm(1e6), nrow = 1000)
#' lst <- list(a = big_mat, b = big_mat)  # Same object referenced twice
#' shared_lst <- share(lst, deep = TRUE, min_bytes = 1000)
#' # Creates only ONE shared segment - both 'a' and 'b' reference it
#' }
share <- function(x,
                  backing = c("auto", "mmap", "shm"),
                  readonly = TRUE,
                  name = NULL,
                  deep = FALSE,
                  min_bytes = 64 * 1024 * 1024,
                  cycle = c("error", "skip")) {
    backing <- match.arg(backing)
    cycle <- match.arg(cycle)

    # Validate input is serializable
    validate_serializable(x)

    # Deep sharing: traverse structure and share components individually
    if (deep) {
        # Create environment for memoization state
        env <- new.env(parent = emptyenv())
        env$seen <- new.env(parent = emptyenv())       # identity -> {path, node_id, shared}
        env$in_progress <- new.env(parent = emptyenv()) # identity -> TRUE (cycle detection)
        env$segments <- list()                          # All created segments for cleanup
        env$next_id <- 0                               # Node ID counter

        # Traverse and share
        structure_tree <- share_deep_traverse(
            x, env, "<root>", 0,
            min_bytes, backing, readonly, Inf, cycle
        )

        # Count shared vs aliased
        shared_count <- 0
        alias_count <- 0
        cycle_count <- 0
        kept_count <- 0
        total_shared_bytes <- 0

        count_nodes <- function(node) {
            if (inherits(node, "shard_deep_node")) {
                shared_count <<- shared_count + 1
                total_shared_bytes <<- total_shared_bytes + node$shared$size
            } else if (inherits(node, "shard_deep_alias")) {
                alias_count <<- alias_count + 1
            } else if (inherits(node, "shard_deep_cycle")) {
                cycle_count <<- cycle_count + 1
            } else if (inherits(node, "shard_deep_kept")) {
                kept_count <<- kept_count + 1
            } else if (inherits(node, "shard_deep_container")) {
                for (child in node$children) {
                    count_nodes(child)
                }
            }
        }
        count_nodes(structure_tree)

        return(structure(
            list(
                tree = structure_tree,
                segments = env$segments,
                backing = backing,
                readonly = readonly,
                summary = list(
                    shared_count = shared_count,
                    alias_count = alias_count,
                    cycle_count = cycle_count,
                    kept_count = kept_count,
                    total_shared_bytes = total_shared_bytes
                ),
                class_info = list(
                    type = if (is.data.frame(x)) "data.frame"
                           else if (is.list(x)) "list"
                           else "other",
                    deep = TRUE
                )
            ),
            class = "shard_deep_shared"
        ))
    }

    # Standard (non-deep) sharing: serialize entire object

    # Serialize the object (with tryCatch for edge cases)
    serialized <- tryCatch(
        serialize(x, connection = NULL, xdr = FALSE),
        error = function(e) {
            stop("Cannot share object: serialization failed.\n",
                 "Reason: ", conditionMessage(e), "\n",
                 "Objects containing closures, external pointers, or ",
                 "certain connection types cannot be shared.",
                 call. = FALSE)
        }
    )
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
    # Try using existing segment pointer (main process scenario)
    # Wrap in tryCatch because pointer becomes invalid after serialization to workers
    if (!is.null(x$segment) && inherits(x$segment, "shard_segment")) {
        result <- tryCatch({
            raw_data <- segment_read(x$segment, offset = 0, size = x$size)
            unserialize(raw_data)
        }, error = function(e) NULL)

        if (!is.null(result)) {
            return(result)
        }
        # Pointer was invalid (e.g., in worker process), fall through to reopen
    }

    # Open by path (worker process scenario)
    if (is.null(x$path)) {
        stop("Cannot fetch: no path available", call. = FALSE)
    }

    seg <- segment_open(x$path, backing = x$backing, readonly = TRUE)
    on.exit(segment_close(seg, unlink = FALSE))

    raw_data <- segment_read(seg, offset = 0, size = x$size)
    unserialize(raw_data)
}

#' @export
fetch.shard_deep_shared <- function(x, ...) {
    # Reconstruct the object from the shared structure tree
    fetch_deep_reconstruct(x$tree)
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
    # Handle shard ALTREP vectors (from shared_vector/as_shared)
    if (is_shared_vector(x)) {
        return(.Call("C_shard_altrep_materialize", x, PACKAGE = "shard"))
    }
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

#' @export
#' @method close shard_deep_shared
close.shard_deep_shared <- function(con, ...) {
    # Close all shared segments
    for (seg in con$segments) {
        tryCatch(
            close(seg),
            error = function(e) NULL
        )
    }
    invisible(NULL)
}

#' Check if Object is Shared
#'
#' @param x An object to check.
#' @return TRUE if x is a \code{shard_shared} or \code{shard_deep_shared} object,
#'   FALSE otherwise.
#' @export
#' @examples
#' is_shared(share(1:10))  # TRUE
#' is_shared(1:10)         # FALSE
is_shared <- function(x) {
    inherits(x, c("shard_shared", "shard_deep_shared"))
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

#' @export
print.shard_deep_shared <- function(x, ...) {
    cat("<shard_deep_shared>\n")
    cat("  Type:", x$class_info$type, "\n")
    cat("  Backing:", x$backing, "\n")
    cat("  Read-only:", x$readonly, "\n")
    cat("\n")
    cat("  Summary:\n")
    cat("    Shared segments:", x$summary$shared_count, "\n")
    cat("    Aliased references:", x$summary$alias_count, "\n")
    if (x$summary$cycle_count > 0) {
        cat("    Cyclic references (skipped):", x$summary$cycle_count, "\n")
    }
    cat("    Kept in-place:", x$summary$kept_count, "\n")
    cat("    Total shared bytes:",
        format(x$summary$total_shared_bytes, big.mark = ","), "\n")

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
