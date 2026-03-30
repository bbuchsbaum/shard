# Subset Shard Descriptor

Subset Shard Descriptor

## Usage

``` r
# S3 method for class 'shard_descriptor'
x[i]
```

## Arguments

- x:

  A shard_descriptor object.

- i:

  Index or indices.

## Value

A subset of the object.

## Examples

``` r
sh <- shards(100, block_size = 25)
sh[1:2]
#> [[1]]
#> [[1]]$id
#> [1] 1
#> 
#> [[1]]$start
#> [1] 1
#> 
#> [[1]]$end
#> [1] 25
#> 
#> [[1]]$idx
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> 
#> [[1]]$len
#> [1] 25
#> 
#> 
#> [[2]]
#> [[2]]$id
#> [1] 2
#> 
#> [[2]]$start
#> [1] 26
#> 
#> [[2]]$end
#> [1] 50
#> 
#> [[2]]$idx
#>  [1] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
#> 
#> [[2]]$len
#> [1] 25
#> 
#> 
```
