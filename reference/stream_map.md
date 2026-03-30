# Stream over row-groups/datasets and map

Applies `f()` to each partition and returns the list of per-partition
results. This is still much cheaper than collecting the full dataset
when `f()` returns a small summary per partition.

## Usage

``` r
stream_map(x, f, ...)

# S3 method for class 'shard_row_groups'
stream_map(x, f, ...)

# S3 method for class 'shard_dataset'
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

A list of per-partition values, one element per row-group file.

## Examples

``` r
# \donttest{
s <- schema(x = float64())
sink <- table_sink(s, mode = "row_groups")
table_write(sink, 1L, data.frame(x = rnorm(5)))
rg <- table_finalize(sink)
nrows <- stream_map(rg, nrow)
# }
```
