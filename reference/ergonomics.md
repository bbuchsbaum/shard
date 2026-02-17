# Ergonomic Apply/Lapply Wrappers

Convenience wrappers that provide apply/lapply-style ergonomics while
preserving shard's core contract: shared immutable inputs, supervised
execution, and diagnostics.

These functions are intentionally thin wrappers around
[`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
and related primitives.
