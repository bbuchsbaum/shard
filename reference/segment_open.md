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

An S3 object of class "shard_segment"
