# Row layout for fixed-row table outputs

Computes disjoint row ranges for each shard via prefix-sum, enabling
lock-free writes where each shard writes to a unique region.

## Usage

``` r
row_layout(shards, rows_per_shard)
```

## Arguments

- shards:

  A `shard_descriptor`.

- rows_per_shard:

  Either a scalar integer or a function(shard)-\>integer.

## Value

A list mapping shard id (character) to `idx_range(start,end)`.
