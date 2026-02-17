# Stream over row-groups/datasets and map

Applies `f()` to each partition and returns the list of per-partition
results. This is still much cheaper than collecting the full dataset
when `f()` returns a small summary per partition.

## Usage

``` r
stream_map(x, f, ...)
```

## Arguments

- x:

  A `shard_row_groups` or `shard_dataset` handle.

- f:

  Function `(chunk, ...) -> value`.

- ...:

  Passed to `f()`.

## Value

A list of per-partition values (one per file).
