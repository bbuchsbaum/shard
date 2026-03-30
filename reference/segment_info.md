# Get segment information

Get segment information

## Usage

``` r
segment_info(x)
```

## Arguments

- x:

  A shard_segment object

## Value

A named list with segment metadata including `size`, `backing`, `path`,
`readonly`, and `owns`.

## Examples

``` r
# \donttest{
seg <- segment_create(1024)
segment_info(seg)
#> $size
#> [1] 1024
#> 
#> $backing
#> [1] "mmap"
#> 
#> $path
#> [1] "/tmp/shard_7127_1774894469_1774894463"
#> 
#> $readonly
#> [1] FALSE
#> 
#> $owns
#> [1] TRUE
#> 
segment_close(seg)
# }
```
