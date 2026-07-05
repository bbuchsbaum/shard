# Buffer Diagnostics

Returns per-process counters for shard buffer reads and writes.
shard_map uses write counters internally to report write
volume/operations in copy_report().

## Usage

``` r
buffer_diagnostics()
```

## Value

A list with elements `writes` (integer count), `bytes` (total bytes
written), `reads`, `read_bytes`, `init_writes`, and `init_bytes`
accumulated in the current process.

## Examples

``` r
buffer_diagnostics()
#> $writes
#> [1] 4
#> 
#> $bytes
#> [1] 180
#> 
#> $reads
#> [1] 10
#> 
#> $read_bytes
#> [1] 1273
#> 
#> $init_writes
#> [1] 0
#> 
#> $init_bytes
#> [1] 0
#> 
```
