# Assign to Buffer Elements

Assign to Buffer Elements

## Usage

``` r
# S3 method for class 'shard_buffer'
x[i, j, ...] <- value
```

## Arguments

- x:

  A shard_buffer object.

- i:

  Index or indices.

- j:

  Optional second index (for matrices).

- ...:

  Additional indices (for arrays).

- value:

  Values to assign.

## Value

The modified `shard_buffer` object, invisibly.

## Examples

``` r
# \donttest{
buf <- buffer("double", dim = 10)
buf[1:5] <- rnorm(5)
buffer_close(buf)
# }
```
