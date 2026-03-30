# Dimensions of a Shared Memory Buffer

Dimensions of a Shared Memory Buffer

## Usage

``` r
# S3 method for class 'shard_buffer'
dim(x)
```

## Arguments

- x:

  A `shard_buffer` object.

## Value

An integer vector of dimensions, or `NULL` for 1-D buffers.

## Examples

``` r
# \donttest{
buf <- buffer("double", dim = c(4, 5))
dim(buf)
#> [1] 4 5
buffer_close(buf)
# }
```
