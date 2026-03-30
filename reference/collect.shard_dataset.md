# Collect a dataset handle into memory

Collect a dataset handle into memory

## Usage

``` r
# S3 method for class 'shard_dataset'
collect(x, ...)
```

## Arguments

- x:

  A `shard_dataset` handle.

- ...:

  Passed to
  [`as_tibble()`](https://bbuchsbaum.github.io/shard/reference/as_tibble.md).

## Value

A data.frame (or tibble if the `tibble` package is installed).
