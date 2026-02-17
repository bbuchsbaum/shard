# Arena Semantic Scope

Semantic scope for scratch memory that signals temporary data should not
accumulate. Enables memory-conscious parallel execution.

Evaluates an expression in a semantic scope that signals scratch memory
usage. This enables memory-conscious execution where temporaries are
expected to be reclaimed after the scope exits.

## Usage

``` r
arena(
  expr,
  strict = FALSE,
  escape_threshold = .arena_escape_threshold,
  gc_after = strict,
  diagnostics = FALSE
)
```

## Arguments

- expr:

  An expression to evaluate within the arena scope.

- strict:

  Logical. If TRUE, enables strict mode which:

  - Warns if large objects (\> 1MB by default) escape the scope

  - Triggers garbage collection after scope exit

  - Tracks memory growth for diagnostics

  Default is FALSE for compatibility and performance.

- escape_threshold:

  Numeric. Size in bytes above which returned objects trigger a warning
  in strict mode. Default is 1MB (1048576 bytes). Only used when
  `strict = TRUE`.

- gc_after:

  Logical. If TRUE, triggers garbage collection after the arena scope
  exits. Default is TRUE in strict mode, FALSE otherwise.

- diagnostics:

  Logical. If TRUE, returns diagnostics about memory usage along with
  the result. Default is FALSE.

## Value

The result of evaluating `expr`. If `diagnostics = TRUE`, returns a list
with:

- `result`: The expression result

- `diagnostics`: Memory usage information

## Details

The `arena()` function provides a semantic scope that signals "this code
produces scratch data that should not outlive the scope." It serves two
purposes:

1.  **For compiled kernels**: When Rust-based kernels are available,
    arena() provides real scratch arenas backed by temporary shared
    memory segments that are automatically reclaimed.

2.  **For arbitrary R code**: Triggers post-task memory checks to detect
    growth and potential memory leaks.

The `strict` parameter controls escape detection:

- `strict = FALSE` (default): Returns results normally, logs diagnostics
  about memory growth.

- `strict = TRUE`: Warns or errors if large objects escape the scope,
  and triggers aggressive memory reclamation.

## See also

[`shard_map`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
for parallel execution,
[`share`](https://bbuchsbaum.github.io/shard/reference/share.md) for
shared memory inputs.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage - wrap scratch computation
result <- arena({
  # Large temporaries created here won't accumulate
  tmp <- matrix(rnorm(1e6), nrow = 1000)
  colMeans(tmp)  # Only the means escape
})

# Strict mode - warns on large escapes
result <- arena({
  big <- rnorm(1e7)  # 80MB
  big  # Warning: large object escaping
}, strict = TRUE)

# Get diagnostics about memory usage
info <- arena({
  x <- rnorm(1e5)
  sum(x)
}, diagnostics = TRUE)
info$diagnostics  # Memory tracking info
} # }
```
