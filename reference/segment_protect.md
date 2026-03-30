# Make a segment read-only

Make a segment read-only

## Usage

``` r
segment_protect(x)
```

## Arguments

- x:

  A shard_segment object

## Value

The `shard_segment` object, invisibly.

## Examples

``` r
# \donttest{
seg <- segment_create(1024)
segment_protect(seg)
segment_close(seg)
# }
```
