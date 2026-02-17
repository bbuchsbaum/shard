# Create Shard Chunks

Groups shards into chunks for dispatch. Each chunk contains a
self-contained executor function that can run in a worker without
needing package functions.

## Usage

``` r
create_shard_chunks(shards, chunk_size, fun, borrow, out, kernel_meta = NULL)
```

## Arguments

- shards:

  Shard descriptor.

- chunk_size:

  Shards per chunk.

- fun:

  User function.

- borrow:

  Borrowed inputs.

- out:

  Output buffers.

## Value

List of chunk descriptors.
