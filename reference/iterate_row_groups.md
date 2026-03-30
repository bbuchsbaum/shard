# Iterate row groups

Iterate row groups

## Usage

``` r
iterate_row_groups(x, decode = TRUE)
```

## Arguments

- x:

  A `shard_row_groups` handle.

- decode:

  Logical. If TRUE (default), native-encoded partitions are decoded to
  data.frames. If FALSE, native partitions are returned as their
  internal representation (advanced).

## Value

A zero-argument iterator function that returns the next data.frame on
each call, or `NULL` when exhausted.

## Examples

``` r
# \donttest{
s <- schema(x = float64())
sink <- table_sink(s, mode = "row_groups")
table_write(sink, 1L, data.frame(x = rnorm(5)))
rg <- table_finalize(sink)
it <- iterate_row_groups(rg)
chunk <- it()
# }
```
