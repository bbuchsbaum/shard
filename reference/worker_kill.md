# Kill a Worker Process

Terminates the worker process and closes connections.

## Usage

``` r
worker_kill(worker, graceful = TRUE)
```

## Arguments

- worker:

  A `shard_worker` object.

- graceful:

  Logical scalar. If `TRUE`, request an orderly cluster shutdown before
  falling back to a process kill. If `FALSE`, skip the cluster stop step
  and force termination directly.

## Value

NULL (invisibly).
