# Copy-on-write / immutability enforcement for shard shared vectors
#
# NOTE: We enforce cow='deny' at the R level (replacement methods), not by
# erroring on DATAPTR(writable=TRUE) in C. Some base R algorithms request a
# writable pointer even for logically read-only operations; erroring there
# would create false positives and break normal reads.

.shard_cow_policy <- function(x) {
    pol <- attr(x, "shard_cow", exact = TRUE)
    if (is.null(pol) || !is.character(pol) || length(pol) != 1L) "allow" else pol
}

.shard_stop_cow_deny <- function() {
    stop("Attempted to mutate a shared input with cow='deny'.", call. = FALSE)
}

#' Print a Shared Vector
#'
#' Print method for \code{shard_shared_vector} objects. Drops the wrapper class
#' and delegates to the underlying R print method.
#'
#' @param x A \code{shard_shared_vector}.
#' @param ... Additional arguments passed to \code{print}.
#' @return The input \code{x}, invisibly.
#' @export
print.shard_shared_vector <- function(x, ...) {
    # Drop only the wrapper class and preserve any underlying class (e.g.,
    # Date, POSIXct) so printing stays natural.
    cls <- class(x)
    cls2 <- setdiff(cls, "shard_shared_vector")
    y <- x
    class(y) <- if (length(cls2)) cls2 else NULL
    print(y, ...)
    invisible(x)
}

#' Subset-assign a Shared Vector
#'
#' Replacement method for \code{shard_shared_vector}. Raises an error if the
#' copy-on-write policy is \code{"deny"}.
#'
#' @param x A \code{shard_shared_vector}.
#' @param ... Indices.
#' @param value Replacement value.
#' @return The modified object \code{x}.
#' @export
`[<-.shard_shared_vector` <- function(x, ..., value) {
    if (identical(.shard_cow_policy(x), "deny")) .shard_stop_cow_deny()

    # NextMethod() is unreliable for primitive replacement functions when the
    # object only has a single custom class. Temporarily drop the class to force
    # base replacement semantics (while keeping shard_* attributes).
    cls <- class(x)
    class(x) <- NULL
    x <- .Primitive("[<-")(x, ..., value = value)
    class(x) <- cls
    x
}

#' Double-bracket Subset-assign a Shared Vector
#'
#' Replacement method for \code{shard_shared_vector}. Raises an error if the
#' copy-on-write policy is \code{"deny"}.
#'
#' @param x A \code{shard_shared_vector}.
#' @param ... Indices.
#' @param value Replacement value.
#' @return The modified object \code{x}.
#' @export
`[[<-.shard_shared_vector` <- function(x, ..., value) {
    if (identical(.shard_cow_policy(x), "deny")) .shard_stop_cow_deny()
    cls <- class(x)
    class(x) <- NULL
    x <- .Primitive("[[<-")(x, ..., value = value)
    class(x) <- cls
    x
}

# Attribute mutations are also considered violations under cow='deny'.

#' Set an Attribute on a Shared Vector
#'
#' Raises an error if the copy-on-write policy is \code{"deny"}.
#'
#' @param x A \code{shard_shared_vector}.
#' @param which Attribute name.
#' @param value Attribute value.
#' @return The modified object \code{x}.
#' @method attr<- shard_shared_vector
#' @export
`attr<-.shard_shared_vector` <- function(x, which, value) {
    if (identical(.shard_cow_policy(x), "deny")) .shard_stop_cow_deny()
    cls <- class(x)
    class(x) <- NULL
    x <- .Primitive("attr<-")(x, which, value)
    class(x) <- cls
    x
}

#' Set Attributes on a Shared Vector
#'
#' Raises an error if the copy-on-write policy is \code{"deny"}.
#'
#' @param x A \code{shard_shared_vector}.
#' @param value Named list of attributes.
#' @return The modified object \code{x}.
#' @method attributes<- shard_shared_vector
#' @export
`attributes<-.shard_shared_vector` <- function(x, value) {
    if (identical(.shard_cow_policy(x), "deny")) .shard_stop_cow_deny()
    cls <- class(x)
    class(x) <- NULL
    x <- .Primitive("attributes<-")(x, value)
    class(x) <- cls
    x
}

#' Set Names on a Shared Vector
#'
#' Raises an error if the copy-on-write policy is \code{"deny"}.
#'
#' @param x A \code{shard_shared_vector}.
#' @param value Character vector of names.
#' @return The modified object \code{x}.
#' @export
`names<-.shard_shared_vector` <- function(x, value) {
    if (identical(.shard_cow_policy(x), "deny")) .shard_stop_cow_deny()
    cls <- class(x)
    class(x) <- NULL
    x <- .Primitive("names<-")(x, value)
    class(x) <- cls
    x
}

#' Set dim on a Shared Vector
#'
#' Raises an error if the copy-on-write policy is \code{"deny"}.
#'
#' @param x A \code{shard_shared_vector}.
#' @param value Integer vector of dimensions.
#' @return The modified object \code{x}.
#' @export
`dim<-.shard_shared_vector` <- function(x, value) {
    if (identical(.shard_cow_policy(x), "deny")) .shard_stop_cow_deny()
    cls <- class(x)
    class(x) <- NULL
    x <- .Primitive("dim<-")(x, value)
    class(x) <- cls
    x
}

#' Set dimnames on a Shared Vector
#'
#' Raises an error if the copy-on-write policy is \code{"deny"}.
#'
#' @param x A \code{shard_shared_vector}.
#' @param value List of dimnames.
#' @return The modified object \code{x}.
#' @export
`dimnames<-.shard_shared_vector` <- function(x, value) {
    if (identical(.shard_cow_policy(x), "deny")) .shard_stop_cow_deny()
    cls <- class(x)
    class(x) <- NULL
    x <- .Primitive("dimnames<-")(x, value)
    class(x) <- cls
    x
}
