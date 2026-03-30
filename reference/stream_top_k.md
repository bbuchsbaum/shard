# Stream top-k rows by a numeric column

Finds the top `k` rows by `col` without collecting the full dataset.

## Usage

``` r
stream_top_k(x, col, k = 10L, decreasing = TRUE, na_drop = TRUE)
```

## Arguments

- x:

  A `shard_row_groups` or `shard_dataset` handle.

- col:

  Column name to rank by.

- k:

  Number of rows to keep.

- decreasing:

  Logical; TRUE for largest values (default TRUE).

- na_drop:

  Logical; drop rows where `col` is NA (default TRUE).

## Value

A data.frame (or tibble if the `tibble` package is installed) with at
most `k` rows ordered by `col`.

## Details

For native-encoded partitions, this selects candidate rows using the
numeric column without decoding strings, then decodes only the chosen
rows for the returned result.

## Examples

``` r
# \donttest{
s <- schema(x = float64())
sink <- table_sink(s, mode = "row_groups")
table_write(sink, 1L, data.frame(x = c(3.0, 1.0, 2.0)))
rg <- table_finalize(sink)
stream_top_k(rg, "x", k = 2L)
#> # A tibble: 2 × 1
#>       x
#>   <dbl>
#> 1     3
#> 2     2
# }
```
