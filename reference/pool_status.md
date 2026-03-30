# Get Pool Status

Returns current status of all workers in the pool.

## Usage

``` r
pool_status(pool = NULL)
```

## Arguments

- pool:

  A `shard_pool` object. If NULL, uses the current pool.

## Value

A data frame with worker status information.

## Examples

``` r
# \donttest{
p <- pool_create(2)
pool_status(p)
#>   worker_id  pid status rss_bytes rss_baseline rss_drift recycle_count
#> 1         1 8556     ok  72036352     72036352         0             0
#> 2         2 8574     ok  72056832     72056832         0             0
pool_stop(p)
# }
```
