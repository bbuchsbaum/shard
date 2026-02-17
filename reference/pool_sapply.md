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
