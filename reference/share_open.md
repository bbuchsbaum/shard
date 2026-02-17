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

A `shard_shared` object.
