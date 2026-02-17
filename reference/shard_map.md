# Parallel Execution with shard_map

Core parallel execution engine with supervision, shared inputs, and
output buffers.

Executes a function over shards in parallel with worker supervision,
shared inputs, and explicit output buffers. This is the primary entry
point for shard's parallel execution model.

## Usage

``` r
shard_map(
  shards,
  fun = NULL,
  borrow = list(),
  out = list(),
  kernel = NULL,
  scheduler_policy = NULL,
  autotune = NULL,
  dispatch_mode = c("rpc_chunked", "shm_queue"),
  dispatch_opts = NULL,
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
  or an integer N to auto-generate shards.

- fun:

  Function to execute per shard. Receives the shard descriptor as first
  argument, followed by borrowed inputs and outputs. You can also select
  a registered kernel via `kernel=` instead of providing `fun=`.

- borrow:

  Named list of shared inputs. These are exported to workers once and
  reused across shards. Treated as read-only by default.

- out:

  Named list of output buffers (from
  [`buffer()`](https://bbuchsbaum.github.io/shard/reference/buffer.md)).
  Workers write results directly to these buffers.

- kernel:

  Optional. Name of a registered kernel (see
  [`list_kernels()`](https://bbuchsbaum.github.io/shard/reference/list_kernels.md)).
  If provided, `fun` must be NULL.

- scheduler_policy:

  Optional list of scheduling hints (advanced). Currently:

  - `max_huge_concurrency`: cap concurrent chunks whose kernel footprint
    is classified as `"huge"` (see
    [`register_kernel()`](https://bbuchsbaum.github.io/shard/reference/register_kernel.md)).

- autotune:

  Optional. Online autotuning for scalar-N sharding (advanced). When
  `shards` is an integer `N`, shard_map can adjust shard block sizes
  over time based on observed wall time and worker RSS.

  Accepted values:

  - `NULL` (default): enable online autotuning for `shard_map(N, ...)`,
    off for precomputed shard descriptors.

  - `TRUE` / `"online"`: force online autotuning (only applies when
    `shards` is an integer `N`).

  - `FALSE` / `"none"`: disable autotuning.

  - a list:
    `list(mode="online", max_rounds=..., probe_shards_per_worker=..., min_shard_time=...)`

- dispatch_mode:

  Dispatch mode (advanced). `"rpc_chunked"` is the default supervised
  socket-based dispatcher. `"shm_queue"` is an opt-in fast mode that
  uses a shared-memory task queue to reduce per-task overhead for tiny
  tasks. In v1, `"shm_queue"` is only supported for `shard_map(N, ...)`
  with `chunk_size=1` and is intended for out-buffer/sink workflows
  (results are not gathered).

- dispatch_opts:

  Optional list of dispatch-mode specific knobs (advanced). Currently:

  - For `dispatch_mode="rpc_chunked"`:

    - `auto_table`: logical. If TRUE, shard_map treats data.frame/tibble
      return values as row-group outputs and writes them to a table sink
      automatically (one partition per shard id). This avoids building a
      large list of tibbles and calling bind_rows() on the master.
      Requires `out=` to be empty (use explicit
      `out=list(sink=table_sink(...))` otherwise).

    - `auto_table_materialize`: `"never"`, `"auto"`, or `"always"`
      (default `"auto"`).

    - `auto_table_max_bytes`: numeric/integer. For `"auto"`, materialize
      only if estimated output size \<= this threshold (default 256MB).

    - `auto_table_mode`: `"row_groups"` (default) or `"partitioned"`.

    - `auto_table_path`: optional output directory (default tempdir()).

    - `auto_table_format`: `"auto"`, `"rds"` (default), or `"native"`.

    - `auto_table_schema`: optional `shard_schema` for validation/native
      encoding.

  - For `dispatch_mode="shm_queue"`:

    - `block_size`: integer. If provided, overrides the default
      heuristic for contiguous shard block sizing.

    - `queue_backing`: one of `"mmap"` or `"shm"` (default `"mmap"`).

    - `error_log`: logical. If TRUE, workers write a bounded per-worker
      error log to disk to aid debugging failed tasks (default FALSE).

    - `error_log_max_lines`: integer. Maximum lines per worker in the
      error log (default 100).

- workers:

  Integer. Number of worker processes. If NULL, uses existing pool or
  creates one with `detectCores() - 1`.

- chunk_size:

  Integer. Shards to batch per worker dispatch (default 1). Higher
  values reduce RPC overhead but may hurt load balancing.

- profile:

  Execution profile: `"default"`, `"memory"` (aggressive recycling), or
  `"speed"` (minimal overhead). With `profile="speed"`, shard_map will
  automatically enable `dispatch_mode="shm_queue"` when possible for
  `shard_map(N, ...)` out-buffer workflows (scalar `N`, `chunk_size=1`),
  unless `dispatch_mode` is explicitly specified.

- mem_cap:

  Memory cap per worker (e.g., "2GB"). Workers exceeding this are
  recycled.

- recycle:

  Logical or numeric. If TRUE, recycle workers on RSS drift. If numeric,
  specifies drift threshold (default 0.5 = 50% growth).

- cow:

  Copy-on-write policy for borrowed inputs: `"deny"` (error on
  mutation), `"audit"` (detect and flag), or `"allow"` (permit with
  tracking).

- seed:

  Integer. RNG seed for reproducibility. If NULL, no seed is set.

- diagnostics:

  Logical. Collect detailed diagnostics (default TRUE).

- packages:

  Character vector. Additional packages to load in workers.

- init_expr:

  Expression to evaluate in each worker on startup.

- timeout:

  Numeric. Seconds to wait for each shard (default 3600).

- max_retries:

  Integer. Maximum retries per shard on failure (default 3).

- health_check_interval:

  Integer. Check worker health every N shards (default 10).

## Value

A `shard_result` object containing:

- `results`: List of results from each shard (if fun returns values)

- `failures`: Any permanently failed shards

- `diagnostics`: Timing, memory, and worker statistics

- `pool_stats`: Pool-level statistics

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple parallel computation
blocks <- shards(1000, workers = 4)
result <- shard_map(blocks, function(shard) {
  sum(shard$idx^2)
}, workers = 4)

# With shared inputs
X <- matrix(rnorm(1e6), nrow = 1000)
blocks <- shards(ncol(X), workers = 4)
result <- shard_map(blocks,
  borrow = list(X = X),
  fun = function(shard, X) {
    colMeans(X[, shard$idx, drop = FALSE])
  },
  workers = 4
)
} # }
```
