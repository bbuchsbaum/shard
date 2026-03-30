# Coerce a Shared Memory Buffer to a Vector

Coerce a Shared Memory Buffer to a Vector

## Usage

``` r
# S3 method for class 'shard_buffer'
as.vector(x, mode = "any")
```

## Arguments

- x:

  A `shard_buffer` object.

- mode:

  Storage mode passed to
  [`as.vector`](https://rdrr.io/r/base/vector.html).

## Value

A vector of the buffer's type (or coerced to `mode`).

## Examples

``` r
# \donttest{
buf <- buffer("double", dim = 5)
buf[1:5] <- 1:5
as.vector(buf)
#> [1] 1 2 3 4 5
buffer_close(buf)
# }
```
