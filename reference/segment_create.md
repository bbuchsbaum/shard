# Create a new shared memory segment

Create a new shared memory segment

## Usage

``` r
segment_create(
  size,
  backing = c("auto", "mmap", "shm"),
  path = NULL,
  readonly = FALSE
)
```

## Arguments

- size:

  Size of the segment in bytes

- backing:

  Backing type: "auto", "mmap", or "shm"

- path:

  Optional file path for mmap backing (NULL for temp file)

- readonly:

  Create as read-only (after initial write)

## Value

An S3 object of class "shard_segment"

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a 1MB segment
seg <- segment_create(1024 * 1024)
segment_info(seg)
segment_close(seg)
} # }
```
