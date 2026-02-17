# Dispatch Task to Worker

Sends a task to a specific worker and waits for the result.

## Usage

``` r
pool_dispatch(
  worker_id,
  expr,
  envir = parent.frame(),
  pool = NULL,
  timeout = 3600
)
```

## Arguments

- worker_id:

  Integer. Worker to dispatch to.

- expr:

  Expression to evaluate.

- envir:

  Environment containing variables needed by expr.

- pool:

  A `shard_pool` object. If NULL, uses the current pool.

- timeout:

  Numeric. Seconds to wait for result (default 3600).

## Value

The result of evaluating expr in the worker.
