# Parallel sapply with Supervision

Parallel sapply with Supervision

## Usage

``` r
pool_sapply(X, FUN, ..., simplify = TRUE, pool = NULL)
```

## Arguments

- X:

  List or vector to iterate over.

- FUN:

  Function to apply.

- ...:

  Additional arguments to FUN.

- simplify:

  Logical. Simplify result to vector/matrix?

- pool:

  A `shard_pool` object. If NULL, uses current pool.

## Value

Simplified result if possible, otherwise a list.

## Examples

``` r
# \donttest{
pool_create(2)
result <- pool_sapply(1:4, function(x) x^2, pool = pool_get())
pool_stop()
# }
```
