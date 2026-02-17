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

An iterator function with no args that returns the next data.frame or
NULL.
