# Recycle a Worker

Kills the current worker and spawns a fresh replacement. The new worker
inherits the same ID but has a fresh R process.

## Usage

``` r
worker_recycle(worker, init_expr = NULL, packages = NULL, dev_path = NULL)
```

## Arguments

- worker:

  A `shard_worker` object.

- init_expr:

  Expression to evaluate on startup.

- packages:

  Character vector. Packages to load.

## Value

A new `shard_worker` object.
