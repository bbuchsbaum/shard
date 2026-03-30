# Collect a table handle into memory

Collect a table handle into memory

## Usage

``` r
# S3 method for class 'shard_table_handle'
collect(x, ...)
```

## Arguments

- x:

  A `shard_table_handle`.

- ...:

  Passed to
  [`as_tibble()`](https://bbuchsbaum.github.io/shard/reference/as_tibble.md).

## Value

A data.frame (or tibble if the `tibble` package is installed).
