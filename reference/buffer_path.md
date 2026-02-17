# Get Buffer Path

Returns the path or name of the buffer's underlying segment. Use this to
pass buffer location to workers.

## Usage

``` r
buffer_path(x)
```

## Arguments

- x:

  A shard_buffer object.

## Value

Character string with path/name, or NULL if anonymous.
