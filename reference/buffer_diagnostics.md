# Buffer Diagnostics

Returns per-process counters for shard buffer writes. shard_map uses
these internally to report write volume/operations in copy_report().

## Usage

``` r
buffer_diagnostics()
```

## Value

A list with `writes` and `bytes`.
