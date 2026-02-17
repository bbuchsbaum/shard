# Mark Chunk as Failed

Mark Chunk as Failed

## Usage

``` r
queue_fail(queue, chunk_id, error = NULL, requeue = TRUE)
```

## Arguments

- queue:

  A `shard_queue` object.

- chunk_id:

  Chunk identifier.

- error:

  The error that occurred.

- requeue:

  Logical. Whether to requeue for retry.

## Value

NULL (invisibly).
