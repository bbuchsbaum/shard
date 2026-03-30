# Coerce a Shared Memory Buffer to Array

Coerce a Shared Memory Buffer to Array

## Usage

``` r
# S3 method for class 'shard_buffer'
as.array(x, ...)
```

## Arguments

- x:

  A `shard_buffer` object.

- ...:

  Ignored.

## Value

An array with the buffer contents and the buffer's dimensions, or a
plain vector for 1-D buffers.

## Examples

``` r
# \donttest{
buf <- buffer("double", dim = c(2, 3, 4))
as.array(buf)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    0    0    0
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    0    0    0
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    0    0    0
#> 
#> , , 4
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    0    0    0
#> 
buffer_close(buf)
# }
```
