# Close a shared memory segment

Close a shared memory segment

## Usage

``` r
segment_close(x, unlink = NULL)
```

## Arguments

- x:

  A shard_segment object

- unlink:

  Whether to unlink the underlying file/shm (default: FALSE for opened
  segments, TRUE for owned segments)

## Value

NULL (invisibly)
