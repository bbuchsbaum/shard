# Write tabular results into a table buffer or sink

`table_write()` is the common write path for shard table outputs:

- For fixed-size outputs, write into a `shard_table_buffer` using a row
  selector.

- For variable-size outputs, write into a `shard_table_sink` using a
  shard id.

## Usage

``` r
table_write(target, rows_or_shard_id, data, ...)
```

## Arguments

- target:

  A `shard_table_buffer` or `shard_table_sink`.

- rows_or_shard_id:

  For buffers: row selector (idx_range or integer vector). For sinks:
  shard id (integer).

- data:

  A data.frame or named list matching the schema columns.

- ...:

  Reserved for future extensions.
