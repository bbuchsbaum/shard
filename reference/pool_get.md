# Get the Current Worker Pool

Returns the active worker pool, or NULL if none exists.

## Usage

``` r
pool_get()
```

## Value

A `shard_pool` object or NULL.

## Examples

``` r
p <- pool_get()
is.null(p)
#> [1] TRUE
```
