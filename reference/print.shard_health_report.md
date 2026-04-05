# Print a shard_health_report Object

Print a shard_health_report Object

## Usage

``` r
# S3 method for class 'shard_health_report'
print(x, ...)
```

## Arguments

- x:

  A `shard_health_report` object.

- ...:

  Further arguments (ignored).

## Value

The input `x`, invisibly.

## Examples

``` r
# \donttest{
p <- pool_create(2)
r <- pool_health_check(p)
print(r)
#> Pool health check at 2026-04-05 20:45:23 
#> Stats: recycles = 0 , deaths = 0 , tasks = 0 
#> All workers healthy
pool_stop(p)
# }
```
