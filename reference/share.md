# Zero-Copy Shared Objects

Create shared memory representations of R objects for efficient parallel
access without duplication.

Creates a shared memory representation of an R object. The object is
serialized once and can be accessed by multiple worker processes without
copying.

## Usage

``` r
share(
  x,
  backing = c("auto", "mmap", "shm"),
  readonly = TRUE,
  name = NULL,
  deep = FALSE,
  min_bytes = 64 * 1024 * 1024,
  cycle = c("error", "skip"),
  mode = c("balanced", "strict")
)
```

## Arguments

- x:

  An R object to share. Supports vectors, matrices, arrays, lists, data
  frames, and any object that can be serialized with
  [`serialize()`](https://rdrr.io/r/base/serialize.html).

- backing:

  Backing type: "auto" (default), "mmap", or "shm".

  - `"auto"`: Let the system choose the best option.

  - `"mmap"`: File-backed memory mapping. Most portable.

  - `"shm"`: POSIX shared memory or Windows named mapping.

- readonly:

  Logical. If TRUE (default), the segment is protected after writing,
  making it read-only. Set to FALSE only if you need to modify the
  shared data (advanced use case).

- name:

  Optional name for the shared object. If NULL (default), a unique name
  is generated. Named shares can be opened by name in other processes.

- deep:

  Logical. If TRUE, recursively traverse lists and data.frames, sharing
  individual components that meet the size threshold. When FALSE
  (default), the entire object is serialized as one unit.

- min_bytes:

  Minimum size in bytes for an object to be shared when deep=TRUE.
  Objects smaller than this threshold are kept in-place. Default is 64MB
  (64 \* 1024 \* 1024).

- cycle:

  How to handle cyclic references when deep=TRUE. Either "error"
  (default) to stop with an error, or "skip" to skip cyclic references.

- mode:

  Sharing mode when deep=TRUE. Either "balanced" (default) to continue
  on hook errors and non-shareable types, or "strict" to error.

## Value

A `shard_shared` object (when deep=FALSE) or `shard_deep_shared` object
(when deep=TRUE) containing:

- `path`: The path or name of the shared segment

- `backing`: The backing type used

- `size`: Total size in bytes

- `readonly`: Whether the segment is protected

- `class_info`: Original class information

## Details

The `share()` function is the primary high-level API for creating
zero-copy shared inputs. When you share an object:

1.  The object is serialized into a shared memory segment

2.  The segment is marked read-only (protected)

3.  A lightweight handle is returned that can be passed to workers

4.  Workers attach to the segment and deserialize on demand

This approach eliminates per-worker duplication of large inputs. The
data exists once in shared memory, and all workers read from the same
location.

**Immutability Contract**: Shared objects are immutable by design. Any
attempt to modify shared data in a worker will fail. This guarantees
deterministic behavior and prevents accidental copy-on-write.

## See also

[`segment_create`](https://bbuchsbaum.github.io/shard/reference/segment_create.md)
for low-level segment operations,
[`pool_create`](https://bbuchsbaum.github.io/shard/reference/pool_create.md)
for worker pool management.

## Examples

``` r
if (FALSE) { # \dontrun{
# Share a large matrix
mat <- matrix(rnorm(1e6), nrow = 1000)
shared_mat <- share(mat)

# The shared object is lightweight - only metadata
print(object.size(shared_mat))  # Small

# Get the data back with fetch()
recovered <- fetch(shared_mat)
identical(mat, recovered)  # TRUE

# Use in parallel (workers access without copying)
pool_create(4)
result <- pool_lapply(1:10, function(i) {
  # Workers can access shared_mat efficiently
  data <- fetch(shared_mat)
  sum(data[i, ])
})

# Clean up when done
close(shared_mat)

# Deep sharing with alias preservation
big_mat <- matrix(rnorm(1e6), nrow = 1000)
lst <- list(a = big_mat, b = big_mat)  # Same object referenced twice
shared_lst <- share(lst, deep = TRUE, min_bytes = 1000)
# Creates only ONE shared segment - both 'a' and 'b' reference it
} # }
```
