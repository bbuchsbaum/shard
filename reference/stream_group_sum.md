# Stream group-wise sum

Computes sum(value) by group across partitions without collecting. This
is optimized for factor groups
([`factor_col()`](https://bbuchsbaum.github.io/shard/reference/factor_col.md)).

## Usage

``` r
stream_group_sum(x, group, value, na_rm = TRUE)
```

## Arguments

- x:

  A `shard_row_groups` or `shard_dataset` handle.

- group:

  Group column name (recommended:
  [`factor_col()`](https://bbuchsbaum.github.io/shard/reference/factor_col.md)).

- value:

  Numeric column name to sum.

- na_rm:

  Logical; drop rows where value is NA (default TRUE).

## Value

A data.frame with columns `group` (factor) and `sum` (numeric).

## Examples

``` r
# \donttest{
s <- schema(g = factor_col(c("a", "b")), x = float64())
sink <- table_sink(s, mode = "row_groups")
table_write(sink, 1L,
  data.frame(g = factor(c("a", "b", "a"), levels = c("a", "b")), x = c(1, 2, 3)))
rg <- table_finalize(sink)
stream_group_sum(rg, "g", "x")
#>   group sum
#> 1     a   4
#> 2     b   2
# }
```
