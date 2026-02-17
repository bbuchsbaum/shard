# Shared Memory Segment

Low-level shared memory segment operations for cross-process data
sharing. These functions provide the foundation for the higher-level
[`share()`](https://bbuchsbaum.github.io/shard/reference/share.md) and
[`buffer()`](https://bbuchsbaum.github.io/shard/reference/buffer.md)
APIs.

## Details

Segments can be backed by:

- `"shm"`: POSIX shared memory (Linux/macOS) or named file mapping
  (Windows). Faster but may have size limitations.

- `"mmap"`: File-backed memory mapping. Works on all platforms and
  supports larger sizes.

- `"auto"`: Let the system choose the best option.

All segments are created with secure permissions (0600 on Unix) and are
automatically cleaned up when the R object is garbage collected.
