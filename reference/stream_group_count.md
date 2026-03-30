# Stream group-wise count

Counts rows per group across partitions without collecting. Optimized
for factor groups
([`factor_col()`](https://bbuchsbaum.github.io/shard/reference/factor_col.md)).

## Usage

``` r
stream_group_count(x, group)
```

## Arguments

- x:

  A `shard_row_groups` or `shard_dataset` handle.

- group:

  Group column name (recommended:
  [`factor_col()`](https://bbuchsbaum.github.io/shard/reference/factor_col.md)).

## Value

A data.frame with columns `group` (factor) and `n` (integer).

## Examples

``` r
# \donttest{
s <- schema(g = factor_col(c("a", "b")), x = float64())
sink <- table_sink(s, mode = "row_groups")
table_write(sink, 1L,
  data.frame(g = factor(c("a", "b", "a"), levels = c("a", "b")), x = c(1, 2, 3)))
rg <- table_finalize(sink)
stream_group_count(rg, "g")
#>   group n
#> 1     a 2
#> 2     b 1
# }
```
