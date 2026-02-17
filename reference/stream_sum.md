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

A single numeric sum.
