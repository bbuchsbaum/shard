# Get the path or name of a segment

Get the path or name of a segment

## Usage

``` r
segment_path(x)
```

## Arguments

- x:

  A shard_segment object

## Value

The path string, or `NULL` for anonymous segments.

## Examples

``` r
# \donttest{
seg <- segment_create(1024, backing = "mmap")
segment_path(seg)
#> [1] "/tmp/shard_6789_1784313808_1784313810"
segment_close(seg)
# }
```
