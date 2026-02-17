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

A data.frame (or tibble if installed) with at most `k` rows.

## Details

For native-encoded partitions, this selects candidate rows using the
numeric column without decoding strings, then decodes only the chosen
rows for the returned result.
