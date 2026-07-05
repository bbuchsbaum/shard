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
  chunk_size = "auto",
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

  Function `(acc, value) -> acc` used to combine results. Must be
  associative, and must accept two mapped values as arguments (worker
  partials start from a chunk's first mapped value; see *Initial value
  semantics*).

- init:

  Initial accumulator value, combined exactly once on the master (see
  *Initial value semantics*).

- borrow:

  Named list of shared inputs (same semantics as
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)).

- out:

  Named list of output buffers/sinks (same semantics as
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)).

- workers:

  Number of worker processes.

- chunk_size:

  Shards to batch per worker dispatch. The default `"auto"` targets
  roughly four chunks per worker,
  `max(1, ceiling(num_shards / (workers * 4)))`, which amortizes
  dispatch round trips while retaining load balance. Supply an integer
  to control batching explicitly.

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

  RNG seed for reproducibility. When non-`NULL`, one independent
  L'Ecuyer-CMRG stream per shard is derived on the master and installed
  in the worker immediately before each `map()` call, so per-shard RNG
  draws are reproducible regardless of worker count, `chunk_size`, or
  dynamic shard-to-worker assignment. The master's RNG state and
  [`RNGkind()`](https://rdrr.io/r/base/Random.html) are left exactly as
  found (`seed = NULL` touches no RNG state). Note on floating-point
  results: partials are combined in chunk order, which is deterministic
  given identical chunking, so a given (`seed`, chunking, `chunk_size`)
  is exactly reproducible for any `workers=`; across *different*
  `chunk_size` values the combine order differs, so non-associative
  floating-point rounding may differ even though the per-shard RNG draws
  are identical. When `shards` is a scalar N and `seed` is set, the
  shard decomposition is chosen independently of the worker count so the
  same seed gives identical results for any `workers=`.

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

2.  streaming combine of partials on the master, folded in chunk order.

## Initial value semantics

`init` is combined exactly once, on the master, at the start of the
final fold: the result is `combine(combine(combine(init, p1), p2), ...)`
where `p1, p2, ...` are per-chunk partials in chunk order. Worker-side
partials are built without `init`: each chunk's partial starts from the
chunk's first mapped value. A non-neutral `init` (e.g. `init = 10` with
`+`) therefore contributes exactly once, regardless of `chunk_size` or
`workers`. This requires `combine` to be associative and able to combine
two mapped values (not just an accumulator with a value).

## Examples

``` r
# \donttest{
res <- shard_reduce(
  100L,
  map = function(s) sum(s$idx),
  combine = function(acc, x) acc + x,
  init = 0,
  workers = 2
)
pool_stop()
res$value
#> [1] 5050
# }
```
