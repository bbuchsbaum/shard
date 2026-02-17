# Create a table sink for row-group or partitioned outputs

A table sink supports variable-sized outputs without returning large
data.frames to the master. Each shard writes a separate row-group file.

## Usage

``` r
table_sink(
  schema,
  mode = c("row_groups", "partitioned"),
  path = NULL,
  format = c("auto", "rds", "native")
)
```

## Arguments

- schema:

  A `shard_schema`. If NULL, a schema-less sink is created (RDS format
  only). This is primarily intended for doShard/foreach compatibility
  where output schemas may not be known in advance.

- mode:

  `"row_groups"` (temp, managed) or `"partitioned"` (persistent path).

- path:

  Directory to write row-group files. If NULL, a temp dir is created.

- format:

  Storage format for partitions: `"rds"` (data.frame RDS), `"native"`
  (columnar encoding with string offsets+bytes), or `"auto"` (selects
  `"native"` if the schema contains
  [`string_col()`](https://bbuchsbaum.github.io/shard/reference/coltypes.md);
  otherwise `"rds"`).

## Value

A `shard_table_sink`.

## Details

v1.1 implementation notes:

- Storage format is per-shard RDS (portable, CRAN-friendly).

- This guarantees bounded master memory during execution; final
  collection may still be large if you materialize.
