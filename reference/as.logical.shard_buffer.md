# Coerce a Shared Memory Buffer to Logical

Coerce a Shared Memory Buffer to Logical

## Usage

``` r
# S3 method for class 'shard_buffer'
as.logical(x, ...)
```

## Arguments

- x:

  A `shard_buffer` object.

- ...:

  Ignored.

## Value

A logical vector with the buffer contents.

## Examples

``` r
# \donttest{
buf <- buffer("logical", dim = 5)
as.logical(buf)
#> [1] FALSE FALSE FALSE FALSE FALSE
buffer_close(buf)
# }
```
