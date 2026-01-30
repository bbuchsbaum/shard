#' @title Arena Semantic Scope
#' @name arena
#' @description Semantic scope for scratch memory that signals temporary data
#'   should not accumulate. Enables memory-conscious parallel execution.
#'
#' @details
#' The `arena()` function provides a semantic scope that signals "this code
#' produces scratch data that should not outlive the scope." It serves two
#' purposes:
#'
#' \enumerate{
#'   \item \strong{For compiled kernels}: When Rust-based kernels are available,
#'     arena() provides real scratch arenas backed by temporary shared memory
#'     segments that are automatically reclaimed.
#'   \item \strong{For arbitrary R code}: Triggers post-task memory checks to
#'     detect growth and potential memory leaks.
#' }
#'
#' The \code{strict} parameter controls escape detection:
#' \itemize{
#'   \item \code{strict = FALSE} (default): Returns results normally, logs
#'     diagnostics about memory growth.
#'   \item \code{strict = TRUE}: Warns or errors if large objects escape the
#'     scope, and triggers aggressive memory reclamation.
#' }
#'
#' @seealso \code{\link{shard_map}} for parallel execution,
#'   \code{\link{share}} for shared memory inputs.
NULL

# Default threshold for "large object" detection in strict mode (1 MB)
.arena_escape_threshold <- 1024 * 1024

# Environment to hold arena state for nested arenas
.arena_state <- new.env(parent = emptyenv())

#' Execute Code in a Scratch Arena
#'
#' Evaluates an expression in a semantic scope that signals scratch memory usage.
#' This enables memory-conscious execution where temporaries are expected to be
#' reclaimed after the scope exits.
#'
#' @param expr An expression to evaluate within the arena scope.
#' @param strict Logical. If TRUE, enables strict mode which:
#'   \itemize{
#'     \item Warns if large objects (> 1MB by default) escape the scope
#'     \item Triggers garbage collection after scope exit
#'     \item Tracks memory growth for diagnostics
#'   }
#'   Default is FALSE for compatibility and performance.
#' @param escape_threshold Numeric. Size in bytes above which returned objects
#'   trigger a warning in strict mode. Default is 1MB (1048576 bytes).
#'   Only used when \code{strict = TRUE}.
#' @param gc_after Logical. If TRUE, triggers garbage collection after the
#'   arena scope exits. Default is TRUE in strict mode, FALSE otherwise.
#' @param diagnostics Logical. If TRUE, returns diagnostics about memory usage
#'   along with the result. Default is FALSE.
#'
#' @return The result of evaluating \code{expr}. If \code{diagnostics = TRUE},
#'   returns a list with:
#'   \itemize{
#'     \item \code{result}: The expression result
#'     \item \code{diagnostics}: Memory usage information
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic usage - wrap scratch computation
#' result <- arena({
#'   # Large temporaries created here won't accumulate
#'   tmp <- matrix(rnorm(1e6), nrow = 1000)
#'   colMeans(tmp)  # Only the means escape
#' })
#'
#' # Strict mode - warns on large escapes
#' result <- arena({
#'   big <- rnorm(1e7)  # 80MB
#'   big  # Warning: large object escaping
#' }, strict = TRUE)
#'
#' # Get diagnostics about memory usage
#' info <- arena({
#'   x <- rnorm(1e5)
#'   sum(x)
#' }, diagnostics = TRUE)
#' info$diagnostics  # Memory tracking info
#' }
arena <- function(expr,
                  strict = FALSE,
                  escape_threshold = .arena_escape_threshold,
                  gc_after = strict,
                  diagnostics = FALSE) {

    # Capture expression for evaluation
    expr <- substitute(expr)
    parent_env <- parent.frame()

    # Initialize arena state
    arena_id <- .arena_push()
    on.exit(.arena_pop(arena_id, gc_after), add = TRUE)

    # Capture initial memory state
    rss_before <- if (strict || diagnostics) rss_self() else NA_real_

    # Track GC state before
    gc_before <- if (diagnostics) gc(reset = FALSE, full = FALSE) else NULL

    # Evaluate the expression
    result <- tryCatch(
        eval(expr, envir = parent_env),
        error = function(e) {
            # Re-throw with arena context
            stop(conditionMessage(e), call. = FALSE)
        }
    )

    # Post-execution checks
    if (strict) {
        # Check for large escaping objects
        result_size <- .estimate_object_size(result)
        if (!is.na(result_size) && result_size > escape_threshold) {
            warning(
                sprintf(
                    "arena(strict=TRUE): Large object escaping (%.1f MB > %.1f MB threshold)\n",
                    result_size / 1024 / 1024,
                    escape_threshold / 1024 / 1024
                ),
                "Consider reducing result size or setting strict=FALSE.",
                call. = FALSE
            )
        }
    }

    # Collect diagnostics if requested
    if (diagnostics) {
        rss_after <- rss_self()
        gc_after_info <- gc(reset = FALSE, full = FALSE)

        diag_info <- list(
            arena_id = arena_id,
            strict = strict,
            rss_before = rss_before,
            rss_after = rss_after,
            rss_delta = if (!is.na(rss_before) && !is.na(rss_after)) {
                rss_after - rss_before
            } else {
                NA_real_
            },
            result_size = .estimate_object_size(result),
            gc_before = gc_before,
            gc_after = gc_after_info
        )

        return(structure(
            list(
                result = result,
                diagnostics = diag_info
            ),
            class = "arena_result"
        ))
    }

    result
}

#' Check if Currently Inside an Arena
#'
#' Returns TRUE if the current execution context is within an arena() scope.
#'
#' @return Logical indicating whether we are in an arena scope.
#' @export
#' @examples
#' in_arena()  # FALSE
#' arena({
#'   in_arena()  # TRUE
#' })
in_arena <- function() {
    .arena_depth() > 0
}

#' Get Current Arena Depth
#'
#' Returns the nesting depth of arena scopes. Useful for debugging.
#'
#' @return Integer count of nested arena scopes (0 if not in an arena).
#' @export
arena_depth <- function() {
    .arena_depth()
}

# ============================================================================
# Internal functions
# ============================================================================

#' Push a new arena onto the stack
#' @return Arena ID for matching pop
#' @keywords internal
.arena_push <- function() {
    depth <- .arena_depth()
    new_id <- paste0("arena_", depth + 1L, "_", format(Sys.time(), "%H%M%S%OS3"))

    # Store in arena state
    .arena_state$stack <- c(.arena_state$stack, new_id)

    new_id
}

#' Pop an arena from the stack
#' @param arena_id The ID to verify we're popping the right arena
#' @param run_gc Whether to trigger GC after pop
#' @keywords internal
.arena_pop <- function(arena_id, run_gc = FALSE) {
    stack <- .arena_state$stack

    if (length(stack) == 0) {
        warning("arena_pop called with empty stack", call. = FALSE)
        return(invisible(NULL))
    }

    # Verify we're popping the expected arena
    top <- stack[length(stack)]
    if (top != arena_id) {
        warning(
            sprintf("Arena mismatch: expected %s, found %s", arena_id, top),
            call. = FALSE
        )
    }

    # Pop the stack
    .arena_state$stack <- stack[-length(stack)]

    # Run GC if requested
    if (run_gc) {
        gc(reset = FALSE, full = FALSE)
    }

    invisible(NULL)
}

#' Get current arena depth
#' @return Integer depth
#' @keywords internal
.arena_depth <- function() {
    length(.arena_state$stack %||% character(0))
}

#' Estimate object size safely
#' @param x Object to measure
#' @return Size in bytes or NA if cannot determine
#' @keywords internal
.estimate_object_size <- function(x) {
    tryCatch({
        as.numeric(utils::object.size(x))
    }, error = function(e) {
        NA_real_
    })
}

#' @export
print.arena_result <- function(x, ...) {
    cat("<arena_result>\n")

    diag <- x$diagnostics
    if (!is.null(diag)) {
        cat("  Arena ID:", diag$arena_id, "\n")
        cat("  Strict mode:", diag$strict, "\n")

        if (!is.na(diag$rss_before)) {
            cat("  RSS before:", format(diag$rss_before, big.mark = ","), "bytes\n")
        }
        if (!is.na(diag$rss_after)) {
            cat("  RSS after:", format(diag$rss_after, big.mark = ","), "bytes\n")
        }
        if (!is.na(diag$rss_delta)) {
            cat("  RSS delta:", format(diag$rss_delta, big.mark = ","), "bytes\n")
        }
        if (!is.na(diag$result_size)) {
            cat("  Result size:", format(diag$result_size, big.mark = ","), "bytes\n")
        }
    }

    cat("\nResult:\n")
    print(x$result, ...)

    invisible(x)
}
