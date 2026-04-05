# Check Pool Health

Monitors all workers, recycling those with excessive RSS drift or that
have died.

## Usage

``` r
pool_health_check(pool = NULL, busy_workers = NULL)
```

## Arguments

- pool:

  A `shard_pool` object. If NULL, uses the current pool.

- busy_workers:

  Optional integer vector of worker ids that are currently running tasks
  (used internally by the dispatcher to avoid recycling a worker while a
  result is in flight).

## Value

A list with health status per worker and actions taken.

## Examples

``` r
# \donttest{
p <- pool_create(2)
pool_health_check(p)
#> Pool health check at 2026-04-05 20:45:15 
#> Stats: recycles = 0 , deaths = 0 , tasks = 0 
#> All workers healthy
pool_stop(p)
# }
```
