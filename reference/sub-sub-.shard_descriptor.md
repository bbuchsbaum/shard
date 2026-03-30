# Get Single Shard

Get Single Shard

## Usage

``` r
# S3 method for class 'shard_descriptor'
x[[i]]
```

## Arguments

- x:

  A shard_descriptor object.

- i:

  Index.

## Value

A subset of the object.

## Examples

``` r
sh <- shards(100, block_size = 25)
sh[[1]]
#> $id
#> [1] 1
#> 
#> $start
#> [1] 1
#> 
#> $end
#> [1] 25
#> 
#> $idx
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> 
#> $len
#> [1] 25
#> 
```
