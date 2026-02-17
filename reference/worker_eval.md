# Evaluate Expression in Worker

Sends an expression to the worker for evaluation.

## Usage

``` r
worker_eval(worker, expr, envir = parent.frame(), timeout = 3600)
```

## Arguments

- worker:

  A `shard_worker` object.

- expr:

  Expression to evaluate.

- envir:

  Environment containing variables needed by expr.

- timeout:

  Numeric. Seconds to wait for result.

## Value

The result of evaluation.
