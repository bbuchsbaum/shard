# Open an Existing Buffer

Opens a shared memory buffer that was created in another process. Used
by workers to attach to the parent's output buffer.

## Usage

``` r
buffer_open(path, type, dim, backing = c("mmap", "shm"), readonly = FALSE)
```

## Arguments

- path:

  Path or shm name of the buffer's segment.

- type:

  Character. Data type of the buffer.

- dim:

  Integer vector. Dimensions of the buffer.

- backing:

  Backing type: "mmap" or "shm".

- readonly:

  Logical. Open as read-only? Default FALSE for workers.

## Value

An S3 object of class "shard_buffer".
