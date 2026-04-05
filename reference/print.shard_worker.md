# Print a shard_worker Object

Print a shard_worker Object

## Usage

``` r
# S3 method for class 'shard_worker'
print(x, ...)
```

## Arguments

- x:

  A `shard_worker` object.

- ...:

  Further arguments (ignored).

## Value

The input `x`, invisibly.

## Examples

``` r
# \donttest{
p <- pool_create(1)
print(p$workers[[1]])
#> shard worker [1]
#>   PID: 9147 
#>   Status: alive 
#>   RSS baseline: 68.7 MB 
#>   Recycles: 0 
#>   Spawned: 2026-04-05 20:45:31 
pool_stop(p)
# }
```
