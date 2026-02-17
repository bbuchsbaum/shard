# Get Next Chunk from Queue

Get Next Chunk from Queue

## Usage

``` r
queue_next(queue, worker_id)
```

## Arguments

- queue:

  A `shard_queue` object.

- worker_id:

  Worker requesting the chunk.

## Value

A chunk descriptor or NULL if queue is empty.
