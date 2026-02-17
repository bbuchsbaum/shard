# Stop the Worker Pool

Terminates all worker processes and releases resources. Waits for
workers to actually terminate before returning.

## Usage

``` r
pool_stop(pool = NULL, timeout = 5)
```

## Arguments

- pool:

  A `shard_pool` object. If NULL, uses the current pool.

- timeout:

  Numeric. Seconds to wait for workers to terminate (default 5). Returns
  after timeout even if workers are still alive.

## Value

NULL (invisibly).
