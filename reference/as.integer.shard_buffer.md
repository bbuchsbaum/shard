# Coerce a Shared Memory Buffer to Integer

Coerce a Shared Memory Buffer to Integer

## Usage

``` r
# S3 method for class 'shard_buffer'
as.integer(x, ...)
```

## Arguments

- x:

  A `shard_buffer` object.

- ...:

  Ignored.

## Value

An integer vector with the buffer contents.

## Examples

``` r
# \donttest{
buf <- buffer("integer", dim = 5)
as.integer(buf)
#> [1] 0 0 0 0 0
buffer_close(buf)
# }
```
