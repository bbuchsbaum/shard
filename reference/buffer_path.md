# Get Buffer Path

Returns the path or name of the buffer's underlying segment. Use this to
pass buffer location to workers.

## Usage

``` r
buffer_path(x)
```

## Arguments

- x:

  A shard_buffer object.

## Value

A character string with the path or name of the segment, or `NULL` if
the segment is anonymous.

## Examples

``` r
# \donttest{
buf <- buffer("double", dim = 10)
buffer_path(buf)
#> [1] "/tmp/shard_7127_1774894433_1774894445"
buffer_close(buf)
# }
```
