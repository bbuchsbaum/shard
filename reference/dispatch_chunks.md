# Dispatch Chunks to Worker Pool

Executes a function over chunks using the worker pool with supervision.
Handles worker death and recycling transparently by requeuing failed
chunks.

## Usage

``` r
dispatch_chunks(
  chunks,
  fun,
  ...,
  pool = NULL,
  health_check_interval = 10L,
  max_retries = 3L,
  timeout = 3600,
  scheduler_policy = NULL,
  on_result = NULL,
  store_results = TRUE,
  retain_chunks = TRUE
)
```

## Arguments

- chunks:

  List of chunk descriptors. Each chunk will be passed to `fun`.

- fun:

  Function to execute. Receives (chunk, ...) as arguments.

- ...:

  Additional arguments passed to `fun`.

- pool:

  A `shard_pool` object. If NULL, uses the current pool.

- health_check_interval:

  Integer. Check pool health every N chunks (default 10).

- max_retries:

  Integer. Maximum retries per chunk before permanent failure (default
  3).

- timeout:

  Numeric. Seconds to wait for each chunk (default 3600).

- scheduler_policy:

  Optional list of scheduling hints (advanced). Currently:

  - `max_huge_concurrency`: cap concurrent chunks with
    `footprint_class=="huge"`.

- on_result:

  Optional callback (advanced). If provided, called on the master
  process as `on_result(tag, value, worker_id)` for each successful
  chunk completion. Used by
  [`shard_reduce()`](https://bbuchsbaum.github.io/shard/reference/shard_reduce.md)
  to stream reductions.

- store_results:

  Logical (advanced). If FALSE, successful chunk values are not retained
  in the returned `results` list (streaming use cases).

- retain_chunks:

  Logical (advanced). If FALSE, completed chunk descriptors are stored
  minimally (avoids retaining large shard lists in memory).

## Value

A `shard_dispatch_result` object with results and diagnostics.
