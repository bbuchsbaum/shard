# Spawn a Worker Process

Creates a new R worker process using parallel sockets.

## Usage

``` r
worker_spawn(id, init_expr = NULL, packages = NULL, dev_path = NULL)
```

## Arguments

- id:

  Integer. Worker identifier.

- init_expr:

  Expression to evaluate on startup.

- packages:

  Character vector. Packages to load.

## Value

A `shard_worker` object.
