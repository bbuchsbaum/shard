# Open an existing shared memory segment

Open an existing shared memory segment

## Usage

``` r
segment_open(path, backing = c("mmap", "shm"), readonly = TRUE)
```

## Arguments

- path:

  Path or shm name of the segment

- backing:

  Backing type: "mmap" or "shm"

- readonly:

  Open as read-only

## Value

A `shard_segment` object attached to the existing segment.

## Examples

``` r
# \donttest{
seg <- segment_create(1024, backing = "mmap")
path <- segment_path(seg)
seg2 <- segment_open(path, backing = "mmap", readonly = TRUE)
segment_close(seg2, unlink = FALSE)
segment_close(seg)
# }
```
