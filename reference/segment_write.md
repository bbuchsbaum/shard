# Write data to a segment

Write data to a segment

## Usage

``` r
segment_write(x, data, offset = 0)
```

## Arguments

- x:

  A shard_segment object

- data:

  Data to write (raw, numeric, integer, or logical vector)

- offset:

  Byte offset to start writing (0-based)

## Value

Number of bytes written, invisibly.

## Examples

``` r
# \donttest{
seg <- segment_create(1024)
segment_write(seg, as.integer(1:10), offset = 0)
segment_close(seg)
# }
```
