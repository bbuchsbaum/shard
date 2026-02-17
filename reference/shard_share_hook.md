# Deep Sharing Hook for Custom Classes

S3 generic that allows classes to customize deep sharing behavior.
Override this for your class to control which slots/elements are
traversed, force sharing of small objects, or transform objects before
traversal.

## Usage

``` r
shard_share_hook(x, ctx)

# Default S3 method
shard_share_hook(x, ctx)
```

## Arguments

- x:

  The object being traversed during deep sharing.

- ctx:

  A context list containing:

  path

  :   Current node path string (e.g., "\$data@cache")

  class

  :   class(x) - the object's class vector

  mode

  :   'strict' or 'balanced' - sharing mode

  min_bytes

  :   Minimum size threshold for sharing

  types

  :   Character vector of enabled types for sharing

  deep

  :   Logical, always TRUE when hook is called

## Value

A list with optional fields:

- skip_slots:

  Character vector of S4 slot names to not traverse

- skip_paths:

  Character vector of paths to not traverse

- force_share_paths:

  Character vector of paths to force share (ignore min_bytes)

- rewrite:

  Function(x) -\> x to transform object before traversal

Return an empty list for default behavior (no customization).

## Examples

``` r
if (FALSE) { # \dontrun{
# Define a hook for a class with a cache slot to skip
shard_share_hook.MyModelClass <- function(x, ctx) {
    list(
        skip_slots = "cache",  # Don't traverse the cache slot
        force_share_paths = paste0(ctx$path, "@coefficients")  # Always share coefficients
    )
}

# Define a hook that rewrites objects before sharing
shard_share_hook.LazyData <- function(x, ctx) {
    list(
        rewrite = function(obj) {
            # Materialize lazy data before sharing
            obj$data <- as.matrix(obj$data)
            obj
        }
    )
}
} # }
```
