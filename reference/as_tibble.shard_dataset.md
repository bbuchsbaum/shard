# Materialize a dataset handle into a data.frame/tibble

Materialize a dataset handle into a data.frame/tibble

## Usage

``` r
# S3 method for class 'shard_dataset'
as_tibble(x, max_bytes = 256 * 1024^2, ...)
```

## Arguments

- x:

  A `shard_dataset` handle.

- max_bytes:

  Accepted for API consistency.

- ...:

  Reserved for future extensions.

## Value

A data.frame (or tibble if the `tibble` package is installed).
