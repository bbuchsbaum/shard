# Collect a row-groups handle into memory

Collect a row-groups handle into memory

## Usage

``` r
# S3 method for class 'shard_row_groups'
collect(x, ...)
```

## Arguments

- x:

  A `shard_row_groups` handle.

- ...:

  Passed to
  [`as_tibble()`](https://bbuchsbaum.github.io/shard/reference/as_tibble.md).

## Value

A data.frame (or tibble if the `tibble` package is installed).
