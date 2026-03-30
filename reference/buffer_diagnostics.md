# Buffer Diagnostics

Returns per-process counters for shard buffer writes. shard_map uses
these internally to report write volume/operations in copy_report().

## Usage

``` r
buffer_diagnostics()
```

## Value

A list with elements `writes` (integer count) and `bytes` (total bytes
written) accumulated in the current process.

## Examples

``` r
buffer_diagnostics()
#> $writes
#> [1] 4
#> 
#> $bytes
#> [1] 180
#> 
```
