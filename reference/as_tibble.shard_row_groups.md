# Materialize a row-groups handle into a data.frame/tibble

Materialize a row-groups handle into a data.frame/tibble

## Usage

``` r
# S3 method for class 'shard_row_groups'
as_tibble(x, max_bytes = 256 * 1024^2, ...)
```

## Arguments

- x:

  A `shard_row_groups` handle.

- max_bytes:

  Accepted for API consistency; currently unused for row-groups.

- ...:

  Reserved for future extensions.

## Value

A data.frame (or tibble if the `tibble` package is installed).
