# Get the size of a segment

Get the size of a segment

## Usage

``` r
segment_size(x)
```

## Arguments

- x:

  A shard_segment object

## Value

Size in bytes as a numeric scalar.

## Examples

``` r
# \donttest{
seg <- segment_create(1024)
segment_size(seg)
#> [1] 1024
segment_close(seg)
# }
```
