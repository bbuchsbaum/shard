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
if (FALSE) { # \dontrun{
pool_create(4)
mem_report()
} # }
```
