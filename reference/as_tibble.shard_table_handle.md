# Materialize a table handle into a data.frame/tibble

Materialize a table handle into a data.frame/tibble

## Usage

``` r
# S3 method for class 'shard_table_handle'
as_tibble(x, max_bytes = 256 * 1024^2, ...)
```

## Arguments

- x:

  A `shard_table_handle`.

- max_bytes:

  Warn if estimated payload exceeds this threshold.

- ...:

  Reserved for future extensions.

## Value

A data.frame (or tibble if the `tibble` package is installed).
