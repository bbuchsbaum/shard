# ALTREP Shared Vectors

ALTREP-backed zero-copy vectors for shared memory.

## Details

These functions create ALTREP (Alternative Representation) vectors that
are backed by shared memory segments. The key benefits are:

- **Zero-copy subsetting**: Contiguous subsets return views into the
  same shared memory, not copies.

- **Diagnostics**: Track when data pointers are accessed or when vectors
  are materialized (copied to standard R vectors).

- **Read-only protection**: Optionally prevent write access to protect
  shared data.

Supported types: integer, double/numeric, logical, raw.
