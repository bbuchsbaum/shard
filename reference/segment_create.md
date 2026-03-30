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

A `shard_segment` object backed by shared memory.

## Examples

``` r
# \donttest{
seg <- segment_create(1024 * 1024)
segment_info(seg)
#> $size
#> [1] 1048576
#> 
#> $backing
#> [1] "mmap"
#> 
#> $path
#> [1] "/tmp/shard_7143_1774903666_1774903658"
#> 
#> $readonly
#> [1] FALSE
#> 
#> $owns
#> [1] TRUE
#> 
segment_close(seg)
# }
```
