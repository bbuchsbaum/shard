# Stream sum of a numeric column

Computes the sum of `col` across all partitions without collecting the
full dataset. When partitions are native-encoded, this avoids decoding
string columns entirely.

## Usage

``` r
stream_sum(x, col, na_rm = TRUE)
```

## Arguments

- x:

  A `shard_row_groups` or `shard_dataset` handle.

- col:

  Column name to sum.

- na_rm:

  Logical; drop NAs (default TRUE).

## Value

A single numeric value giving the sum of the column across all
partitions.

## Examples

``` r
# \donttest{
s <- schema(x = float64())
sink <- table_sink(s, mode = "row_groups")
table_write(sink, 1L, data.frame(x = c(1.0, 2.0, 3.0)))
rg <- table_finalize(sink)
stream_sum(rg, "x")
#> [1] 6
# }
```
