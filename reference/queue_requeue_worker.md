# Requeue All Chunks for a Worker

When a worker dies or is recycled, requeue all its in-flight chunks.

## Usage

``` r
queue_requeue_worker(queue, worker_id)
```

## Arguments

- queue:

  A `shard_queue` object.

- worker_id:

  Worker identifier.

## Value

Integer. Number of chunks requeued.
