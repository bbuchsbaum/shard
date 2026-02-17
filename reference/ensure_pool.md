# Ensure Pool Exists

Creates or validates worker pool.

## Usage

``` r
ensure_pool(workers, mem_cap, rss_drift_threshold, packages, init_expr)
```

## Arguments

- workers:

  Number of workers.

- mem_cap:

  Memory cap in bytes.

- rss_drift_threshold:

  Drift threshold.

- packages:

  Packages to load.

- init_expr:

  Init expression.

## Value

Pool object.
