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

Number of bytes written (invisibly)
