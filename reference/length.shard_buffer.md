# Length of a Shared Memory Buffer

Length of a Shared Memory Buffer

## Usage

``` r
# S3 method for class 'shard_buffer'
length(x)
```

## Arguments

- x:

  A `shard_buffer` object.

## Value

An integer scalar giving the total number of elements.

## Examples

``` r
# \donttest{
buf <- buffer("double", dim = 20)
length(buf)
#> [1] 20
buffer_close(buf)
# }
```
