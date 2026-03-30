# Coerce a Shared Memory Buffer to Double

Coerce a Shared Memory Buffer to Double

## Usage

``` r
# S3 method for class 'shard_buffer'
as.double(x, ...)
```

## Arguments

- x:

  A `shard_buffer` object.

- ...:

  Ignored.

## Value

A double vector with the buffer contents.

## Examples

``` r
# \donttest{
buf <- buffer("double", dim = 5)
as.double(buf)
#> [1] 0 0 0 0 0
buffer_close(buf)
# }
```
