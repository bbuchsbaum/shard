# Print a shard_pool Object

Print a shard_pool Object

## Usage

``` r
# S3 method for class 'shard_pool'
print(x, ...)
```

## Arguments

- x:

  A `shard_pool` object.

- ...:

  Further arguments (ignored).

## Value

The input `x`, invisibly.

## Examples

``` r
# \donttest{
p <- pool_create(2)
print(p)
#> shard worker pool
#>   Workers: 2 
#>   RSS limit: 2.0 GB 
#>   Drift threshold: 50% 
#>   Created: 2026-04-05 20:45:23 
#>   Stats:
#>     Total recycles: 0 
#>     Total deaths: 0 
#>     Total tasks: 0 
pool_stop(p)
# }
```
