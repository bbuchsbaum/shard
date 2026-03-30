# Get Buffer Info

Returns information about a buffer.

## Usage

``` r
buffer_info(x)
```

## Arguments

- x:

  A shard_buffer object.

## Value

A named list with buffer properties: `type`, `dim`, `n`, `bytes`,
`backing`, `path`, and `readonly`.

## Examples

``` r
# \donttest{
buf <- buffer("integer", dim = c(5, 5))
buffer_info(buf)
#> $type
#> [1] "integer"
#> 
#> $dim
#> [1] 5 5
#> 
#> $n
#> [1] 25
#> 
#> $bytes
#> [1] 100
#> 
#> $backing
#> [1] "mmap"
#> 
#> $path
#> [1] "/tmp/shard_7143_1774903629_1774903639"
#> 
#> $readonly
#> [1] FALSE
#> 
buffer_close(buf)
# }
```
