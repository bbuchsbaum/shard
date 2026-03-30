# Print a Shared Memory Buffer

Print a Shared Memory Buffer

## Usage

``` r
# S3 method for class 'shard_buffer'
print(x, ...)
```

## Arguments

- x:

  A `shard_buffer` object.

- ...:

  Ignored.

## Value

The input `x`, invisibly.

## Examples

``` r
# \donttest{
buf <- buffer("double", dim = 10)
print(buf)
#> <shard_buffer>
#>   Type: double 
#>   Length: 10 
#>   Size: 80 bytes
#>   Backing: mmap 
#>   Path: /tmp/shard_7143_1774903647_1774903651 
buffer_close(buf)
# }
```
