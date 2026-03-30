# Print a shard_descriptor Object

Print a shard_descriptor Object

## Usage

``` r
# S3 method for class 'shard_descriptor'
print(x, ...)
```

## Arguments

- x:

  A `shard_descriptor` object.

- ...:

  Further arguments (ignored).

## Value

The input `x`, invisibly.

## Examples

``` r
sh <- shards(100, block_size = 25)
print(sh)
#> shard descriptor
#>   Items: 100 
#>   Block size: 25 
#>   Strategy: contiguous 
#>   Shards: 4 
#>   Shard size: 25 (uniform)
```
