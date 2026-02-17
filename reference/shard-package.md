# shard: Deterministic, Zero-Copy Parallel Execution for R

Provides a parallel execution runtime for R that emphasizes
deterministic memory behavior and efficient handling of large shared
inputs. 'shard' enables zero-copy parallel reads via
shared/memory-mapped segments, encourages explicit output buffers to
avoid large result aggregation, and supervises worker processes to
mitigate memory drift via controlled recycling. Diagnostics report peak
memory usage, end-of-run memory return, and hidden copy/materialization
events to support reproducible performance benchmarking.

The shard package provides a parallel execution runtime that emphasizes
deterministic memory behavior and efficient handling of large shared
inputs.

## Core API

- [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md) -
  Primary parallel execution entry point

- [`shards()`](https://bbuchsbaum.github.io/shard/reference/shards.md) -
  Create shard descriptors with autotuning

- [`results()`](https://bbuchsbaum.github.io/shard/reference/results.md) -
  Extract results from a shard_map run

- [`succeeded()`](https://bbuchsbaum.github.io/shard/reference/succeeded.md) -
  Check if shard_map completed without failures

## Zero-Copy Shared Data

- [`share()`](https://bbuchsbaum.github.io/shard/reference/share.md) -
  Share an R object for parallel access

- [`fetch()`](https://bbuchsbaum.github.io/shard/reference/fetch.md) -
  Retrieve data from a shared object

- [`materialize()`](https://bbuchsbaum.github.io/shard/reference/materialize.md) -
  Alias for fetch()

- [`is_shared()`](https://bbuchsbaum.github.io/shard/reference/is_shared.md) -
  Check if an object is shared

- [`shared_info()`](https://bbuchsbaum.github.io/shard/reference/shared_info.md) -
  Get information about a shared object

## Output Buffers

- [`buffer()`](https://bbuchsbaum.github.io/shard/reference/buffer.md) -
  Create typed writable output buffer

- [`buffer_open()`](https://bbuchsbaum.github.io/shard/reference/buffer_open.md) -
  Open existing buffer from another process

- [`buffer_path()`](https://bbuchsbaum.github.io/shard/reference/buffer_path.md) -
  Get buffer path for cross-process sharing

- [`buffer_info()`](https://bbuchsbaum.github.io/shard/reference/buffer_info.md) -
  Get buffer information

- [`buffer_close()`](https://bbuchsbaum.github.io/shard/reference/buffer_close.md) -
  Close and release buffer

## Worker Pool Management

- [`pool_create()`](https://bbuchsbaum.github.io/shard/reference/pool_create.md) -
  Create a supervised worker pool

- [`pool_stop()`](https://bbuchsbaum.github.io/shard/reference/pool_stop.md) -
  Stop the worker pool

- [`pool_status()`](https://bbuchsbaum.github.io/shard/reference/pool_status.md) -
  Check worker status and RSS

- [`pool_health_check()`](https://bbuchsbaum.github.io/shard/reference/pool_health_check.md) -
  Monitor and recycle workers

## Task Dispatch

- [`dispatch_chunks()`](https://bbuchsbaum.github.io/shard/reference/dispatch_chunks.md) -
  Execute chunks with supervision

- [`pool_lapply()`](https://bbuchsbaum.github.io/shard/reference/pool_lapply.md) -
  Parallel lapply with supervision

- [`pool_sapply()`](https://bbuchsbaum.github.io/shard/reference/pool_sapply.md) -
  Parallel sapply with supervision

## See also

Useful links:

- <https://bbuchsbaum.github.io/shard/>

- <https://github.com/bbuchsbaum/shard>

- Report bugs at <https://github.com/bbuchsbaum/shard/issues>

Useful links:

- <https://bbuchsbaum.github.io/shard/>

- <https://github.com/bbuchsbaum/shard>

- Report bugs at <https://github.com/bbuchsbaum/shard/issues>

## Author

**Maintainer**: Bradley Buchsbaum <brad.buchsbaum@gmail.com>
