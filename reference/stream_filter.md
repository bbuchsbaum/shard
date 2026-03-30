# Stream-filter a dataset/row-groups into a new partitioned dataset

Reads each partition, filters rows, and writes a new partitioned
dataset. Output is written as one partition per input partition (empty
partitions are allowed). This avoids materializing all results.

## Usage

``` r
stream_filter(x, predicate, path = NULL, ...)
```

## Arguments

- x:

  A `shard_row_groups` or `shard_dataset` handle.

- predicate:

  Function `(chunk, ...) -> logical` row mask (length == nrow(chunk)).

- path:

  Output directory. If NULL, a temp dir is created.

- ...:

  Passed to `predicate()`.

## Value

A `shard_dataset` handle pointing to the filtered partitions.

## Examples

``` r
# \donttest{
s <- schema(x = float64())
sink <- table_sink(s, mode = "row_groups")
table_write(sink, 1L, data.frame(x = c(1.0, 2.0, 3.0)))
rg <- table_finalize(sink)
filtered <- stream_filter(rg, predicate = function(chunk) chunk$x > 1.5)
# }
```
