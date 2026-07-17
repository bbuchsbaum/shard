# Print a Shared Memory Segment

Print a Shared Memory Segment

## Usage

``` r
# S3 method for class 'shard_segment'
print(x, ...)
```

## Arguments

- x:

  A `shard_segment` object.

- ...:

  Ignored.

## Value

The input `x`, invisibly.

## Examples

``` r
# \donttest{
seg <- segment_create(1024)
print(seg)
#> <shard_segment>
#>   Size: 1,024 bytes
#>   Backing: shm 
#>   Path: /shard_6789_1784313801_1784313803 
#>   Read-only: FALSE 
#>   Owns segment: TRUE 
segment_close(seg)
# }
```
