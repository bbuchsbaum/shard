# Coerce a Shared Memory Buffer to Matrix

Coerce a Shared Memory Buffer to Matrix

## Usage

``` r
# S3 method for class 'shard_buffer'
as.matrix(x, ...)
```

## Arguments

- x:

  A `shard_buffer` object (must be 2-dimensional).

- ...:

  Ignored.

## Value

A matrix with the buffer contents and the buffer's dimensions.

## Examples

``` r
# \donttest{
buf <- buffer("double", dim = c(3, 4))
as.matrix(buf)
#>      [,1] [,2] [,3] [,4]
#> [1,]    0    0    0    0
#> [2,]    0    0    0    0
#> [3,]    0    0    0    0
buffer_close(buf)
# }
```
