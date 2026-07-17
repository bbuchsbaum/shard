# Memory Usage Report

Generates a report of memory usage across all workers in the pool.

## Usage

``` r
mem_report(pool = NULL)
```

## Arguments

- pool:

  Optional. A `shard_pool` object. If NULL, uses the current pool.

## Value

An S3 object of class `shard_report` with type `"memory"` containing:

- `type`: "memory"

- `timestamp`: When the report was generated

- `pool_active`: Whether a pool exists

- `n_workers`: Number of workers

- `rss_limit`: RSS limit per worker (bytes)

- `total_rss`: Sum of RSS across all workers

- `peak_rss`: Highest RSS among workers

- `mean_rss`: Mean RSS across workers

- `workers`: Per-worker RSS details

## Examples

``` r
# \donttest{
p <- pool_create(2)
mem_report(p)
#> shard memory report
#> Generated: 2026-07-17 18:46:14 
#> 
#> Pool: 2 workers
#> RSS limit: 2.0 GB 
#> 
#> Aggregate:
#>   Total: 139.7 MB 
#>   Peak: 70.0 MB 
#>   Mean: 69.9 MB 
#> 
#> Per-worker:
#>   [+] 1: rss=70.0 MB, baseline=70.0 MB, drift=0.0%
#>   [+] 2: rss=69.8 MB, baseline=69.8 MB, drift=0.0%
pool_stop(p)
# }
```
