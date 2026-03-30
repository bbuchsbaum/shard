# Coerce a Shared Memory Buffer to Raw

Coerce a Shared Memory Buffer to Raw

## Usage

``` r
# S3 method for class 'shard_buffer'
as.raw(x, ...)
```

## Arguments

- x:

  A `shard_buffer` object.

- ...:

  Ignored.

## Value

A raw vector with the buffer contents.

## Examples

``` r
# \donttest{
buf <- buffer("raw", dim = 5)
as.raw(buf)
#> [1] 00 00 00 00 00
buffer_close(buf)
# }
```
