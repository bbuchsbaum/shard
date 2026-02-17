# Create a Chunk Queue

Creates a queue for managing chunk dispatch with support for requeuing
failed chunks when workers die or are recycled.

## Usage

``` r
queue_create(chunks)
```

## Arguments

- chunks:

  List of chunk descriptors. Each chunk should have an `id` field.

## Value

A `shard_queue` object with queue management methods.
