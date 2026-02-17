# Streaming Reductions over Shards

Reduce shard results without gathering all per-shard returns on the
master.

`shard_reduce()` executes `map()` over shards in parallel and combines
results using an associative `combine()` function. Unlike
[`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md),
it does not accumulate all per-shard results on the master; it streams
partials as chunks complete.

## Usage

``` r
shard_reduce(
  shards,
  map,
  combine,
  init,
  borrow = list(),
  out = list(),
  workers = NULL,
  chunk_size = 1L,
  profile = c("default", "memory", "speed"),
  mem_cap = "2GB",
  recycle = TRUE,
  cow = c("deny", "audit", "allow"),
  seed = NULL,
  diagnostics = TRUE,
  packages = NULL,
  init_expr = NULL,
  timeout = 3600,
  max_retries = 3L,
  health_check_interval = 10L
)
```

## Arguments

- shards:

  A `shard_descriptor` from
  [`shards()`](https://bbuchsbaum.github.io/shard/reference/shards.md),
  or an integer N.

- map:

  Function executed per shard. Receives shard descriptor as first
  argument, followed by borrowed inputs and outputs.

- combine:

  Function `(acc, value) -> acc` used to combine results. Should be
  associative for deterministic behavior under chunking.

- init:

  Initial accumulator value.

- borrow:

  Named list of shared inputs (same semantics as
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)).

- out:

  Named list of output buffers/sinks (same semantics as
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)).

- workers:

  Number of worker processes.

- chunk_size:

  Shards to batch per worker dispatch (default 1).

- profile:

  Execution profile (same semantics as
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)).

- mem_cap:

  Memory cap per worker (same semantics as
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)).

- recycle:

  Worker recycling policy (same semantics as
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)).

- cow:

  Copy-on-write policy for borrowed inputs (same semantics as
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)).

- seed:

  RNG seed for reproducibility.

- diagnostics:

  Logical; collect diagnostics (default TRUE).

- packages:

  Additional packages to load in workers.

- init_expr:

  Expression to evaluate in each worker on startup.

- timeout:

  Seconds to wait for each chunk.

- max_retries:

  Maximum retries per chunk.

- health_check_interval:

  Check worker health every N completions.

## Value

A `shard_reduce_result` with fields:

- `value`: final accumulator

- `failures`: any permanently failed chunks

- `diagnostics`: run telemetry including reduction stats

- `queue_status`, `pool_stats`

## Details

For performance and memory efficiency, reduction is performed in two
stages:

1.  per-chunk partial reduction inside each worker, and

2.  streaming combine of partials on the master.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sum 1..1e6 without materializing 1e6 per-shard results:
res <- shard_reduce(
  shards(1e6, block_size = 1000),
  map = function(s) sum(s$idx),
  combine = function(acc, x) acc + x,
  init = 0,
  workers = 4
)
res$value
} # }
```
