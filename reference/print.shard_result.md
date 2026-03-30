# Print a shard_result Object

Print a shard_result Object

## Usage

``` r
# S3 method for class 'shard_result'
print(x, ...)
```

## Arguments

- x:

  A `shard_result` object.

- ...:

  Further arguments (ignored).

## Value

The input `x`, invisibly.

## Examples

``` r
# \donttest{
result <- shard_map(4L, function(shard) shard$idx, workers = 2)
pool_stop()
print(result)
#> shard_map result
#>   Duration: 1.67 seconds 
#>   Shards: 4 
#>   Chunks: 4 
#>   Completed: 4  
#>   Worker recycles: 0 
#>   Worker deaths: 0 
#>   Profile: default 
#>   COW policy: deny 
# }
```
