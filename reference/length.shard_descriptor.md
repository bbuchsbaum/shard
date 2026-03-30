# Length of a shard_descriptor Object

Length of a shard_descriptor Object

## Usage

``` r
# S3 method for class 'shard_descriptor'
length(x)
```

## Arguments

- x:

  A `shard_descriptor` object.

## Value

An integer scalar giving the number of shards.

## Examples

``` r
sh <- shards(100, block_size = 25)
length(sh)
#> [1] 4
```
