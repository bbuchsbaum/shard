# Write a shard's row-group output

Write a shard's row-group output

## Usage

``` r
# S3 method for class 'shard_table_sink'
table_write(target, rows_or_shard_id, data, ...)
```

## Arguments

- target:

  A `shard_table_sink`.

- rows_or_shard_id:

  Integer shard id used to name the row-group file.

- data:

  A data.frame matching the sink schema.

- ...:

  Reserved for future extensions.

## Value

NULL (invisibly).
