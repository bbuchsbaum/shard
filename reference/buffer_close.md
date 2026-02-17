# Close a Buffer

Closes the buffer and releases the underlying shared memory.

## Usage

``` r
buffer_close(x, unlink = NULL)
```

## Arguments

- x:

  A shard_buffer object.

- unlink:

  Whether to unlink the underlying segment.

## Value

NULL (invisibly).
