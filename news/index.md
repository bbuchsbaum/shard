# Changelog

## shard 0.2.1

Bug-fix release addressing memory-safety and silent-wrong-answer defects
when a shared vector is accessed after its segment has been closed.

### Bug fixes

- Fixed a use-after-unmap when reading a shared vector after
  [`close()`](https://rdrr.io/r/base/connections.html) or
  [`segment_close()`](https://bbuchsbaum.github.io/shard/reference/segment_close.md).
  Shared vectors cache the resolved base pointer across reads, and
  closing a segment unmapped it without invalidating that cache, so a
  read performed before the close left a pointer into unmapped memory
  that a later read would reuse. Depending on whether the operating
  system had reused the address range, this crashed the R session or
  silently returned garbage values. Segment teardown now invalidates all
  cached pointers. Views were affected through their parent segment as
  well.
- Accessing a shared vector whose segment has been closed now raises
  `"underlying shared memory segment is no longer valid"` from every
  access path. Previously only the `DATAPTR` paths reported the error:
  element access returned `NA`, and the region-copy methods used by
  [`sum()`](https://rdrr.io/r/base/sum.html),
  [`mean()`](https://rdrr.io/r/base/mean.html) and
  [`max()`](https://rdrr.io/r/base/Extremes.html) returned a short count
  without filling the caller’s buffer, so those reductions consumed
  uninitialized memory and returned arbitrary values. Coercion and
  materialization returned zero-filled vectors.
  [`length()`](https://rdrr.io/r/base/length.html) continues to work on
  a closed vector, as it reads no shared memory.
- Fixed a bus error when
  [`segment_protect()`](https://bbuchsbaum.github.io/shard/reference/segment_protect.md)
  is called on a segment that already has a writable shared vector over
  it. Each vector captured the segment’s read-only state at creation, so
  it kept handing out writable pointers into a mapping that had since
  been made read-only, and the write faulted. Protection now replaces
  the shared mapping with a single process-private copy-on-write
  mapping. Read-only operations that ask for a writable data pointer –
  including [`range()`](https://rdrr.io/r/base/range.html),
  [`which.max()`](https://rdrr.io/r/base/which.min.html) and comparison
  operators – therefore keep their zero-copy behavior, while a real
  classed mutation is explicitly materialized and writes through an
  [`unclass()`](https://rdrr.io/r/base/class.html) bypass cannot reach
  disk or another process. Vectors created by
  [`share()`](https://bbuchsbaum.github.io/shard/reference/share.md) and
  [`as_shared()`](https://bbuchsbaum.github.io/shard/reference/as_shared.md)
  were unaffected, as those protect the segment before building the
  vector. The same stale flag was written into the serialized form of
  such a vector, so sending one to a worker re-opened the segment
  read-write and writes reached the shared bytes; serialization now
  carries the effective state.
  [`shared_diagnostics()`](https://bbuchsbaum.github.io/shard/reference/shared_diagnostics.md)
  likewise reports the effective read-only state rather than the value
  captured at creation.
- [`pool_health_check()`](https://bbuchsbaum.github.io/shard/reference/pool_health_check.md)
  no longer aborts with `"missing value where TRUE/FALSE needed"` when a
  worker’s resident-set size cannot be read. This happened when a worker
  exited between the liveness probe and the memory reading, or when a
  baseline captured at spawn was itself unavailable; such workers are
  now left for the next tick’s liveness check instead, and the health
  report says so rather than reporting all workers healthy.
  [`pool_create()`](https://bbuchsbaum.github.io/shard/reference/pool_create.md)
  now rejects an `NA` `rss_limit` or `rss_drift_threshold`, which
  reached the same comparison.
- [`segment_write()`](https://bbuchsbaum.github.io/shard/reference/segment_write.md)
  now reports the write size, offset and segment size when a write does
  not fit, instead of a bare `"Write failed"`. Writes to an unmapped
  segment are rejected with a distinct message.

## shard 0.2.0

CRAN release: 2026-07-17

Internal performance and correctness refactor. No user-facing API was
removed; apart from a new `path=` argument to
[`as_shared()`](https://bbuchsbaum.github.io/shard/reference/as_shared.md)
and the deprecation of the unused `pool_create(heartbeat_interval=)`,
all changes improve the behavior, speed, or robustness of the existing
runtime.

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
- Contiguous buffer reads (`buf[]`, `as.vector(buf)`, per-shard slice
  reads) now use a single typed C read instead of a raw intermediate
  plus [`readBin()`](https://rdrr.io/r/base/readBin.html), halving
  allocation and copy volume when materializing results.
- Master-side chunk-queue management is O(1) per chunk (cursor-based
  dequeue, preallocated completion storage); completed chunks retain
  only minimal metadata by default, releasing shard descriptors as the
  run progresses.
- Worker liveness probes reuse a `ps` handle cached at spawn time (also
  more robust against PID reuse) instead of creating one per check per
  poll tick.
- Buffer index validation and contiguity detection are allocation-free
  and preserve ALTREP-compact index ranges.
- Indexing a buffer with 3 or more dimensions now gathers/scatters only
  the selection instead of materializing the whole buffer; array
  assignment no longer breaks the lock-free disjoint parallel-write
  guarantee.
- The online autotuner and
  [`stream_map()`](https://bbuchsbaum.github.io/shard/reference/stream_map.md)
  no longer grow result lists incrementally across phases/partitions.

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
- Fixed an integer overflow that broke
  [`share()`](https://bbuchsbaum.github.io/shard/reference/share.md) for
  atomic vectors over 2GB (about 269 million doubles).
- Fixed
  [`shard_reduce()`](https://bbuchsbaum.github.io/shard/reference/shard_reduce.md)
  receiving a stale output-buffer handle when run after
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
  on the same pool with a same-named `out=` buffer.
- C layer hardening: shared-memory offset/length validation is
  overflow-safe (negative or non-finite values now error cleanly instead
  of reading or writing out of bounds), freshly created ALTREP objects
  are protected from garbage collection during construction, shard
  ALTREP methods no longer misidentify other packages’ ALTREP objects,
  and the shared task queue validates its header before use.
- [`table_write()`](https://bbuchsbaum.github.io/shard/reference/table_write.md)
  validates `shard_id`, so an NA id can no longer produce a part file
  that
  [`table_finalize()`](https://bbuchsbaum.github.io/shard/reference/table_finalize.md)
  silently drops.
- Fixed
  [`row_layout()`](https://bbuchsbaum.github.io/shard/reference/row_layout.md)
  erroring when a trailing shard has zero rows, and
  [`shard_crossprod()`](https://bbuchsbaum.github.io/shard/reference/shard_crossprod.md)
  failing on matrices with fewer than 8 columns.
- Worker liveness detection no longer reports dead workers as alive on
  systems without the `ps` package.
- On macOS, the `"shm"` backing now works: generated segment names
  previously exceeded the Darwin name-length limit, so `shm_open()`
  always failed.
- `shard_map(health_check_interval=)` is honoured when supplied
  explicitly (profile presets no longer override it), and `share(name=)`
  now applies to fast-path atomic shares.
  `pool_create(heartbeat_interval=)` is deprecated: it was never
  consulted (health checks are per-chunk, not time-based).

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

## shard 0.1.1

CRAN release: 2026-04-05

- Fixed a gcc-UBSAN report: the task-queue header used a C89 `tasks[1]`
  trailing-array idiom; changed to a C99 flexible array member.
- Default worker counts now respect `_R_CHECK_LIMIT_CORES_` and are
  capped at 2 during `R CMD check`, at all call sites
  ([`pool_create()`](https://bbuchsbaum.github.io/shard/reference/pool_create.md),
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md),
  [`shards()`](https://bbuchsbaum.github.io/shard/reference/shards.md),
  [`shard_reduce()`](https://bbuchsbaum.github.io/shard/reference/shard_reduce.md)).

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
