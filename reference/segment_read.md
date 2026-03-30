# Read raw data from a segment

Read raw data from a segment

## Usage

``` r
segment_read(x, offset = 0, size = NULL)
```

## Arguments

- x:

  A shard_segment object

- offset:

  Byte offset to start reading (0-based)

- size:

  Number of bytes to read

## Value

A raw vector containing the bytes read from the segment.

## Examples

``` r
# \donttest{
seg <- segment_create(1024)
segment_write(seg, as.integer(1:4), offset = 0)
segment_read(seg, offset = 0, size = 16)
#>  [1] 01 00 00 00 02 00 00 00 03 00 00 00 04 00 00 00
segment_close(seg)
# }
```
