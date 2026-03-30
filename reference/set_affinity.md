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

A logical scalar; `TRUE` on success, `FALSE` if not supported.

## Examples

``` r
affinity_supported()
#> [1] TRUE
```
