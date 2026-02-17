# Register a shard kernel

Registers a named kernel implementation that can be selected by
`shard_map(..., kernel = "name")`.

## Usage

``` r
register_kernel(
  name,
  impl,
  signature = NULL,
  footprint = NULL,
  supports_views = TRUE,
  description = NULL
)
```

## Arguments

- name:

  Kernel name (string).

- impl:

  Function implementing the kernel. It must accept the shard descriptor
  as its first argument.

- signature:

  Optional short signature string for documentation.

- footprint:

  Optional footprint hint. Either a constant (bytes) or a function
  `(shard, ...) -> list(class='tiny'|'medium'|'huge', bytes=...)`.

- supports_views:

  Logical. Whether the kernel is intended to operate on shard views
  without slice materialization.

- description:

  Optional human-readable description.

## Value

Invisibly, the registered kernel metadata.

## Details

A "kernel" is just a function that shard_map can call for each shard.
The registry lets shard_map attach additional metadata (footprint hints,
supports_views) for scheduling/autotuning.
