# Get Worker RSS (Resident Set Size)

Queries the worker's memory usage via the ps package if available,
otherwise falls back to /proc on Linux or ps command.

## Usage

``` r
worker_rss(worker)
```

## Arguments

- worker:

  A `shard_worker` object.

## Value

Numeric. RSS in bytes, or NA if unavailable.
