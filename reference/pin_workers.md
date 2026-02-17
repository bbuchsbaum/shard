# Pin shard workers to CPU cores

Best-effort worker pinning to improve cache locality and reduce
cross-core migration. Currently supported on Linux only.

## Usage

``` r
pin_workers(pool = NULL, strategy = c("spread", "compact"), cores = NULL)
```

## Arguments

- pool:

  Optional shard_pool. Defaults to current pool.

- strategy:

  "spread" assigns worker i -\> core i mod ncores. "compact" assigns
  workers to the first cores.

- cores:

  Optional integer vector of available cores (0-based). If NULL, uses
  0:(detectCores()-1).

## Value

Invisibly, a logical vector per worker indicating success.
