# Set CPU affinity for the current process

Intended to be called inside a worker process (e.g., via
`clusterCall()`). On unsupported platforms, returns FALSE.

## Usage

``` r
set_affinity(cores)
```

## Arguments

- cores:

  Integer vector of 0-based CPU core ids.

## Value

Logical; TRUE on success.
