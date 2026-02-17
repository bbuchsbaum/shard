# Stream group-wise count

Counts rows per group across partitions without collecting. Optimized
for factor groups (factor_col()).

## Usage

``` r
stream_group_count(x, group)
```

## Arguments

- x:

  A `shard_row_groups` or `shard_dataset` handle.

- group:

  Group column name (recommended: factor_col()).

## Value

A data.frame with columns `group` and `n`.
