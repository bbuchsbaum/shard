# Shared Memory Buffers

Create typed writable output buffers backed by shared memory for
cross-process writes during parallel execution.

Creates a typed output buffer backed by shared memory that can be
written to by parallel workers using slice assignment.

## Usage

``` r
buffer(
  type = c("double", "integer", "logical", "raw"),
  dim,
  init = NULL,
  backing = c("auto", "mmap", "shm")
)
```

## Arguments

- type:

  Character. Data type: "double" (default), "integer", "logical", or
  "raw".

- dim:

  Integer vector. Dimensions of the buffer. For a vector, specify the
  length. For a matrix, specify `c(nrow, ncol)`. For arrays, specify all
  dimensions.

- init:

  Initial value to fill the buffer. Default is type-appropriate zero
  (`0`, `0L`, `FALSE`, or `raw(0)`).

- backing:

  Backing type for shared memory: "auto" (default), "mmap", or "shm".

## Value

An S3 object of class "shard_buffer" that supports:

- Slice assignment: `buf[idx] <- values`

- Slice reading: `buf[idx]`

- Full extraction: `buf[]`

- Conversion to R vector: `as.vector(buf)`, `as.double(buf)`, etc.

## Details

Buffers provide an explicit output mechanism for
[`shard_map`](https://bbuchsbaum.github.io/shard/reference/shard_map.md).
Instead of returning results from workers (which requires serialization
and memory copying), workers write directly to shared buffers.

Supported types:

- `"double"`: 8-byte floating point (default)

- `"integer"`: 4-byte signed integer

- `"logical"`: 4-byte logical (stored as integer)

- `"raw"`: 1-byte raw data

Buffers support slice assignment using standard R indexing:
`buf[1:100] <- values`

## See also

[`segment_create`](https://bbuchsbaum.github.io/shard/reference/segment_create.md)
for low-level segment operations,
[`share`](https://bbuchsbaum.github.io/shard/reference/share.md) for
read-only shared inputs

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a 1M element double buffer
out <- buffer("double", dim = 1e6)

# Write to slices (typically done in workers)
out[1:1000] <- rnorm(1000)
out[1001:2000] <- rnorm(1000)

# Read back results
result <- out[]  # or as.double(out)

# Matrix buffer
mat <- buffer("double", dim = c(100, 50))
mat[1:10, ] <- matrix(1:500, nrow = 10)
} # }
```
