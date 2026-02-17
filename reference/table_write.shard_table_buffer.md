# Write into a table buffer

Write into a table buffer

## Usage

``` r
# S3 method for class 'shard_table_buffer'
table_write(target, rows_or_shard_id, data, ...)
```

## Arguments

- target:

  A `shard_table_buffer`.

- rows_or_shard_id:

  Row selector (idx_range or integer vector).

- data:

  A data.frame or named list matching the schema columns.

- ...:

  Reserved for future extensions.

## Value

NULL (invisibly).
