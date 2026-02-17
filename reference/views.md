# Zero-copy Views

Views are explicit slice descriptors over shared arrays/matrices. They
avoid creating slice-sized allocations (e.g. `Y[, a:b]`) by carrying
only metadata plus a reference to the shared backing.

## Details

This is a low-level optimization handle: arbitrary base R operations may
materialize a view; use
[`materialize()`](https://bbuchsbaum.github.io/shard/reference/materialize.md)
explicitly when you want a standard matrix/array.
