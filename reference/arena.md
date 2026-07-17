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

The result of evaluating `expr`. If `diagnostics = TRUE`, returns an
`arena_result` object with elements `result` and `diagnostics`.

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
# \donttest{
result <- arena({
  tmp <- matrix(rnorm(1e6), nrow = 1000)
  colMeans(tmp)
})

info <- arena({
  x <- rnorm(1e5)
  sum(x)
}, diagnostics = TRUE)
info$diagnostics
#> $arena_id
#> [1] "arena_1_18425454.708"
#> 
#> $strict
#> [1] FALSE
#> 
#> $rss_before
#> [1] 177160192
#> 
#> $rss_after
#> [1] 169156608
#> 
#> $rss_delta
#> [1] -8003584
#> 
#> $result_size
#> [1] 56
#> 
#> $gc_before
#>           used (Mb) gc trigger (Mb) max used (Mb)
#> Ncells 1045156 55.9    1343934 71.8  1343934 71.8
#> Vcells 3080319 23.6    8388608 64.0  5114204 39.1
#> 
#> $gc_after
#>           used (Mb) gc trigger (Mb) max used (Mb)
#> Ncells 1045193 55.9    1343934 71.8  1343934 71.8
#> Vcells 3180366 24.3    8388608 64.0  5114204 39.1
#> 
# }
```
