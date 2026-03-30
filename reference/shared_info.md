# Get Information About a Shared Object

Get Information About a Shared Object

## Usage

``` r
shared_info(x)
```

## Arguments

- x:

  A `shard_shared` object.

## Value

A named list with fields `path`, `backing`, `size`, `readonly`,
`class_info`, and `segment_info`.

## Examples

``` r
# \donttest{
shared <- share(1:100)
shared_info(shared)
#> $path
#> [1] "/tmp/shard_7100_1774886049_1774886047"
#> 
#> $backing
#> [1] "mmap"
#> 
#> $size
#> [1] 400
#> 
#> $readonly
#> [1] TRUE
#> 
#> $class_info
#> $class_info$type
#> [1] "vector"
#> 
#> $class_info$length
#> [1] 100
#> 
#> $class_info$mode
#> [1] "integer"
#> 
#> 
#> $segment_info
#> $segment_info$size
#> [1] 400
#> 
#> $segment_info$backing
#> [1] "mmap"
#> 
#> $segment_info$path
#> [1] "/tmp/shard_7100_1774886049_1774886047"
#> 
#> $segment_info$readonly
#> [1] TRUE
#> 
#> $segment_info$owns
#> [1] TRUE
#> 
#> 
close(shared)
# }
```
