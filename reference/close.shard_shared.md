# Close a Shared Object

Releases the shared memory segment. After closing, the shared object can
no longer be accessed.

## Usage

``` r
# S3 method for class 'shard_shared'
close(con, ...)
```

## Arguments

- con:

  A `shard_shared` object.

- ...:

  Ignored.

## Value

NULL (invisibly).
