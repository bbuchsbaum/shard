# Create a Worker Pool

Spawns N R worker processes that persist across multiple
[`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
calls. Workers are supervised and recycled when RSS drift exceeds
thresholds.

## Usage

``` r
pool_create(
  n = .default_workers(),
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

  Integer. Number of worker processes to spawn. Defaults to
  `detectCores() - 1`, capped at 2 during `R CMD check`.

- rss_limit:

  Numeric or character. Maximum RSS per worker before recycling. Can be
  bytes (numeric) or human-readable (e.g., "2GB"). Default is "2GB".

- rss_drift_threshold:

  Numeric. Fraction of RSS increase from baseline that triggers
  recycling (default 0.5 = 50% growth).

- heartbeat_interval:

  Deprecated and unused. Pool health checks are triggered per dispatched
  chunk rather than on a wall-clock interval; use the
  `health_check_interval` argument of
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
  /
  [`shard_reduce()`](https://bbuchsbaum.github.io/shard/reference/shard_reduce.md)
  to control cadence. Supplying a non-default value emits a deprecation
  warning and otherwise has no effect.

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

## Examples

``` r
# \donttest{
p <- pool_create(2)
pool_stop(p)
# }
```
