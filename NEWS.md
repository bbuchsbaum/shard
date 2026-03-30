# shard 0.1.0

* Initial CRAN release.
* Deterministic, zero-copy parallel execution runtime for R.
* Shared-memory and memory-mapped segment support for large inputs.
* Worker pool with controlled recycling to mitigate memory drift.
* Diagnostics for peak memory usage, memory return, and copy-on-write events.
* `shard_map()` and `shard_reduce()` for parallel map and reduce operations.
* Arena-based pre-allocated output buffers.
* Support for POSIX shared memory and file-backed memory-mapped segments.
