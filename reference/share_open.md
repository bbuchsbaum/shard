# Open an Existing Shared Object by Path

Opens a shared object that was created by another process. This is
useful for workers that need to attach to shared data without having the
original `shard_shared` object.

## Usage

``` r
share_open(path, backing = c("mmap", "shm"), size = NULL)
```

## Arguments

- path:

  Path to the shared segment.

- backing:

  Backing type: "mmap" or "shm".

- size:

  Size of the segment in bytes. If NULL, attempts to detect.

## Value

A `shard_shared` object attached to the existing segment.

## Examples

``` r
# \donttest{
shared <- share(1:50)
info <- shared_info(shared)
reopened <- share_open(info$path, backing = "mmap")
#> Error in segment_open(path, backing = backing, readonly = TRUE): Failed to open shared memory segment
close(reopened)
#> Error: object 'reopened' not found
close(shared)
# }
```
