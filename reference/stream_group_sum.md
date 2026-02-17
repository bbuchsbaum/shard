# Stream group-wise sum

Computes sum(value) by group across partitions without collecting. This
is optimized for factor groups (factor_col()).

## Usage

``` r
stream_group_sum(x, group, value, na_rm = TRUE)
```

## Arguments

- x:

  A `shard_row_groups` or `shard_dataset` handle.

- group:

  Group column name (recommended: factor_col()).

- value:

  Numeric column name to sum.

- na_rm:

  Logical; drop rows where value is NA (default TRUE).

## Value

A data.frame with columns `group` and `sum`.
