# Extract Buffer Elements

Extract Buffer Elements

## Usage

``` r
# S3 method for class 'shard_buffer'
x[i, j, ..., drop = TRUE]
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

- drop:

  Whether to drop dimensions.

## Value

A vector or array of values read from the buffer.

## Examples

``` r
# \donttest{
buf <- buffer("double", dim = 10)
buf[1:5] <- 1:5
buf[1:3]
#> [1] 1 2 3
buffer_close(buf)
# }
```
