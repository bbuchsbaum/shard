# Stream row count

Stream row count

## Usage

``` r
stream_count(x)
```

## Arguments

- x:

  A `shard_row_groups` or `shard_dataset` handle.

## Value

A single integer giving the total number of rows across all partitions.

## Examples

``` r
# \donttest{
s <- schema(x = float64())
sink <- table_sink(s, mode = "row_groups")
table_write(sink, 1L, data.frame(x = rnorm(5)))
rg <- table_finalize(sink)
stream_count(rg)
#> [1] 5
# }
```
