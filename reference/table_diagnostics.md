# Table Diagnostics

Per-process counters for table writes (number of table_write calls,
rows, and bytes written). shard_map uses deltas of these counters to
produce run-level diagnostics in copy_report().

## Usage

``` r
table_diagnostics()
```

## Value

A list with `writes`, `rows`, and `bytes`.
