# Parallel Dispatch with Async Workers

An alternative dispatch that uses parallel::parLapply-style execution
but with supervision. This is a simpler interface for basic parallel
apply.

## Usage

``` r
pool_lapply(X, FUN, ..., pool = NULL, chunk_size = 1L)
```

## Arguments

- X:

  List or vector to iterate over.

- FUN:

  Function to apply to each element.

- ...:

  Additional arguments to FUN.

- pool:

  A `shard_pool` object. If NULL, uses current pool.

- chunk_size:

  Integer. Elements per chunk (default 1).

## Value

A list of results.
