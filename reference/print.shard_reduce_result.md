# Print a shard_reduce_result Object

Print a shard_reduce_result Object

## Usage

``` r
# S3 method for class 'shard_reduce_result'
print(x, ...)
```

## Arguments

- x:

  A `shard_reduce_result` object.

- ...:

  Further arguments (ignored).

## Value

The input `x`, invisibly.

## Examples

``` r
# \donttest{
res <- shard_reduce(4L, map = function(s) sum(s$idx),
  combine = `+`, init = 0, workers = 2)
pool_stop()
print(res)
#> shard_reduce result
#>   Duration: 1.58 seconds 
#>   Partials: 4 
#>   Partial max bytes: 56 B 
# }
```
