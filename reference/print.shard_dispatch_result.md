# Print a shard_dispatch_result Object

Print a shard_dispatch_result Object

## Usage

``` r
# S3 method for class 'shard_dispatch_result'
print(x, ...)
```

## Arguments

- x:

  A `shard_dispatch_result` object.

- ...:

  Further arguments (ignored).

## Value

The input `x`, invisibly.

## Examples

``` r
# \donttest{
pool_create(2)
chunks <- list(list(id = 1L, x = 1), list(id = 2L, x = 2))
result <- dispatch_chunks(chunks, function(chunk) chunk$x, pool = pool_get())
print(result)
#> shard dispatch result
#>   Duration: 0.82 seconds 
#>   Chunks: 2 completed
#>   Pool stats: 0 recycles, 0 deaths
pool_stop()
# }
```
