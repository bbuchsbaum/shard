#' @title Adapter Registry for Class-Specific Deep Sharing
#' @name adapter
#' @description Register custom traversal logic for specific classes during
#'   deep sharing operations. Adapters allow fine-grained control over how
#'   objects are decomposed and reconstructed.
#'
#' @details
#' The adapter registry provides a way to customize how specific classes are
#' handled during deep sharing. Instead of generic slot traversal for S4 objects
#' or element-wise traversal for lists, you can provide custom functions to:
#'
#' \enumerate{
#'   \item Extract the shareable children from an object (\code{children})
#'   \item Reconstruct the object from shared children (\code{replace})
#' }
#'
#' This is useful for:
#' \itemize{
#'   \item Complex S4 objects where only certain slots should be shared
#'   \item S3 objects with internal structure that differs from list structure
#'   \item Objects with accessors that should be used instead of direct slot access
#' }
#'
#' @seealso \code{\link{share}} for the main sharing function that uses adapters.
NULL

# Internal registry: environment storing adapters keyed by class name
.adapter_registry <- new.env(parent = emptyenv())

#' Validate Adapter Structure
#'
#' Internal function to validate an adapter has the required structure.
#'
#' @param adapter A list with adapter functions.
#' @param class_name The class name being registered.
#' @return TRUE if valid, throws error otherwise.
#' @keywords internal
validate_adapter <- function(adapter, class_name) {
    if (!is.list(adapter)) {
        stop("Adapter must be a list.", call. = FALSE)
    }

    # Required: class
    if (is.null(adapter$class) || !is.character(adapter$class) ||
        length(adapter$class) != 1) {
        stop("Adapter must have a 'class' element (single character string).",
             call. = FALSE)
    }

    if (adapter$class != class_name) {
        stop("Adapter class '", adapter$class, "' does not match registered class '",
             class_name, "'.", call. = FALSE)
    }

    # Required: children function
    if (is.null(adapter$children) || !is.function(adapter$children)) {
        stop("Adapter must have a 'children' function: function(x) -> named list.",
             call. = FALSE)
    }

    # Required: replace function
    if (is.null(adapter$replace) || !is.function(adapter$replace)) {
        stop("Adapter must have a 'replace' function: function(x, children) -> object.",
             call. = FALSE)
    }

    # Optional: path_prefix (character or NULL)
    if (!is.null(adapter$path_prefix) &&
        (!is.character(adapter$path_prefix) || length(adapter$path_prefix) != 1)) {
        stop("Adapter 'path_prefix' must be a single character string or NULL.",
             call. = FALSE)
    }

    invisible(TRUE)
}


#' Register an Adapter for Class-Specific Traversal
#'
#' Registers a custom adapter for a specific class. When deep sharing encounters
#' an object of this class, it will use the adapter's \code{children()} function
#' to extract shareable components instead of generic traversal.
#'
#' @param class A character string naming the class to register the adapter for.
#' @param adapter A list containing:
#'   \describe{
#'     \item{class}{Character string matching the \code{class} parameter.}
#'     \item{children}{Function taking an object and returning a named list of
#'       child objects to traverse.}
#'     \item{replace}{Function taking the original object and a named list of
#'       (potentially shared) children, returning a reconstructed object.}
#'     \item{path_prefix}{Optional character string prefix for child paths in
#'       the sharing plan (default: class name).}
#'   }
#'
#' @return Invisibly returns the previous adapter for this class (if any),
#'   or NULL if no adapter was registered.
#'
#' @export
#' @examples
#' \dontrun{
#' # Register an adapter for a custom S4 class
#' setClass("MyData", slots = c(data = "matrix", metadata = "list"))
#'
#' shard_register_adapter("MyData", list(
#'   class = "MyData",
#'   children = function(x) list(data = x@data),  # Only share the data slot
#'   replace = function(x, children) {
#'     x@data <- children$data
#'     x
#'   }
#' ))
#'
#' # Now deep sharing will only traverse the 'data' slot
#' obj <- new("MyData", data = matrix(1:1e6, nrow = 1000), metadata = list(a = 1))
#' shared <- share(obj, deep = TRUE, min_bytes = 1000)
#' }
shard_register_adapter <- function(class, adapter) {
    stopifnot(is.character(class), length(class) == 1, nzchar(class))

    validate_adapter(adapter, class)

    # Store previous adapter for return
    previous <- if (exists(class, envir = .adapter_registry, inherits = FALSE)) {
        get(class, envir = .adapter_registry, inherits = FALSE)
    } else {
        NULL
    }

    # Register the adapter
    assign(class, adapter, envir = .adapter_registry)

    invisible(previous)
}


#' Unregister an Adapter
#'
#' Removes a previously registered adapter for a class. After unregistration,
#' objects of this class will use default traversal behavior during deep sharing.
#'
#' @param class A character string naming the class to unregister.
#'
#' @return Invisibly returns the removed adapter, or NULL if no adapter was
#'   registered for this class.
#'
#' @export
#' @examples
#' \dontrun{
#' # Remove adapter for MyData class
#' old_adapter <- shard_unregister_adapter("MyData")
#' }
shard_unregister_adapter <- function(class) {
    stopifnot(is.character(class), length(class) == 1, nzchar(class))

    previous <- if (exists(class, envir = .adapter_registry, inherits = FALSE)) {
        adapter <- get(class, envir = .adapter_registry, inherits = FALSE)
        rm(list = class, envir = .adapter_registry)
        adapter
    } else {
        NULL
    }

    invisible(previous)
}


#' Get Adapter for an Object
#'
#' Retrieves the registered adapter for an object's class. Checks all classes
#' in the object's class hierarchy, returning the first matching adapter.
#'
#' @param x An R object.
#'
#' @return The adapter list if one is registered for any of the object's classes,
#'   or NULL if no adapter is registered.
#'
#' @export
#' @examples
#' \dontrun{
#' # Check if an adapter is registered for an object
#' adapter <- shard_get_adapter(my_object)
#' if (!is.null(adapter)) {
#'   children <- adapter$children(my_object)
#' }
#' }
shard_get_adapter <- function(x) {
    # Check each class in the class hierarchy
    classes <- class(x)

    for (cls in classes) {
        if (exists(cls, envir = .adapter_registry, inherits = FALSE)) {
            return(get(cls, envir = .adapter_registry, inherits = FALSE))
        }
    }

    NULL
}


#' List Registered Adapters
#'
#' Returns a character vector of all classes with registered adapters.
#'
#' @return Character vector of class names with registered adapters.
#'
#' @export
#' @examples
#' shard_list_adapters()
shard_list_adapters <- function() {
    ls(envir = .adapter_registry)
}


#' Clear All Registered Adapters
#'
#' Removes all registered adapters. Primarily useful for testing.
#'
#' @return Invisibly returns the number of adapters cleared.
#'
#' @keywords internal
shard_clear_adapters <- function() {
    n <- length(ls(envir = .adapter_registry))
    rm(list = ls(envir = .adapter_registry), envir = .adapter_registry)
    invisible(n)
}
