# Get the underlying segment from a shared vector

Get the underlying segment from a shared vector

## Usage

``` r
shared_segment(x)
```

## Arguments

- x:

  A shard ALTREP vector

## Value

A `shard_segment` S3 object wrapping the underlying segment

## Examples

``` r
# \donttest{
x <- as_shared(1:100)
shared_segment(x)
#> <shard_segment>
#>   Size: 400 bytes
#>   Backing: shm 
#>   Path: /shard_6789_1784313819_1784313826 
#>   Read-only: TRUE 
#>   Owns segment: TRUE 
# }
```
