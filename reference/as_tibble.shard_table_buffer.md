# Materialize a fixed table handle or buffer

Converts a `shard_table_handle` to an in-memory data.frame (or tibble if
the tibble package is installed).

## Usage

``` r
# S3 method for class 'shard_table_buffer'
as_tibble(x, max_bytes = 256 * 1024^2, ...)
```

## Arguments

- x:

  A `shard_table_handle` or `shard_table_buffer`.

- max_bytes:

  Warn if estimated payload exceeds this threshold.

- ...:

  Reserved for future extensions.

## Value

A data.frame (or tibble).
