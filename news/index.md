# Changelog

## shard 0.2.0

Internal performance and correctness refactor (Phases 1–5). No
user-facing API was added or removed; all changes improve the behavior,
speed, or robustness of the existing runtime.

### Performance

- Overhauled the task-dispatch core: the worker function and diagnostics
  setup are now exported once per dispatch instead of per task, and
  run-level diagnostics are gated behind `diagnostics = TRUE`. This
  sharply reduces dispatch wall-clock and task-payload size on
  many-shard workloads.
- The internal task queue uses an O(1) claim cursor with batched claims.
- Non-contiguous (strided) shard index sets are compacted on the wire
  and reconstructed in the worker, cutting serialization cost for sparse
  selections.
- On Linux, `backing = "auto"` now maps shared memory on tmpfs; added
  `madvise()` access hints and a cached segment base pointer.
- Buffer sparse (non-contiguous) reads and writes use typed C
  gather/scatter instead of a full read-modify-write, while bulk
  contiguous sub-block writes keep their fast path.

### Reproducibility and correctness

- `seed=` now installs an independent per-shard L’Ecuyer-CMRG RNG stream
  immediately before each shard runs, so results are identical
  regardless of `workers`, `chunk_size`, or shard-to-worker assignment,
  and are stable across worker recycling and restarts.
- `cow = "deny"` is enforced for shared inputs at the R level;
  deep-environment validation for shared objects is preserved.
- C layer: corrected long-vector handling (`XLENGTH`), Windows read-only
  segment protection, and copy/coercion accounting.
- `view_col_vars()` and the `col_vars` kernel use a numerically stable
  (Welford) computation.
- [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
  validates `borrow`/`out` names against the worker function’s formals
  before creating the pool, failing fast with a clearer message.
- [`shard_reduce()`](https://bbuchsbaum.github.io/shard/reference/shard_reduce.md):
  minimal reducer environment, per-shard seed streams, and corrected
  automatic chunking and `init` semantics.
- Worker recycling replays a bootstrap manifest so borrowed inputs and
  outputs are correctly re-exported to restarted workers.

### Autotuning and configuration

- `autotune_block_size()` now clamps correctly to
  `max_shards_per_worker` and warns when a scratch budget would require
  more shards than allowed.
- The default worker count can be overridden with
  `options(shard.workers = N)` or the `SHARD_WORKERS` environment
  variable (still capped to 2 under `R CMD check`).

### Testing

- Substantially expanded unit-test coverage of buffers, views, tables,
  kernels, and internal helpers.

## shard 0.1.0

CRAN release: 2026-04-03

- Initial CRAN release.
- Deterministic, zero-copy parallel execution runtime for R.
- Shared-memory and memory-mapped segment support for large inputs.
- Worker pool with controlled recycling to mitigate memory drift.
- Diagnostics for peak memory usage, memory return, and copy-on-write
  events.
- [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
  and
  [`shard_reduce()`](https://bbuchsbaum.github.io/shard/reference/shard_reduce.md)
  for parallel map and reduce operations.
- Arena-based pre-allocated output buffers.
- Support for POSIX shared memory and file-backed memory-mapped
  segments.
