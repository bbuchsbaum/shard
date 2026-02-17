# Create a Worker Pool

Spawns N R worker processes that persist across multiple
[`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
calls. Workers are supervised and recycled when RSS drift exceeds
thresholds.

## Usage

``` r
pool_create(
  n = parallel::detectCores() - 1L,
  rss_limit = "2GB",
  rss_drift_threshold = 0.5,
  heartbeat_interval = 5,
  min_recycle_interval = 1,
  init_expr = NULL,
  packages = NULL
)
```

## Arguments

- n:

  Integer. Number of worker processes to spawn.

- rss_limit:

  Numeric or character. Maximum RSS per worker before recycling. Can be
  bytes (numeric) or human-readable (e.g., "2GB"). Default is "2GB".

- rss_drift_threshold:

  Numeric. Fraction of RSS increase from baseline that triggers
  recycling (default 0.5 = 50% growth).

- heartbeat_interval:

  Numeric. Seconds between health checks (default 5).

- min_recycle_interval:

  Numeric. Minimum time in seconds between recycling the same worker
  (default 1.0). This prevents thrashing PSOCK worker creation under
  extremely tight RSS limits.

- init_expr:

  Expression to evaluate in each worker on startup.

- packages:

  Character vector. Packages to load in workers.

## Value

A `shard_pool` object (invisibly). The pool is also stored in the
package environment for reuse.
