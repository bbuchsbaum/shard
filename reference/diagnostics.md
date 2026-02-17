# Diagnostics API

Comprehensive diagnostics for shard parallel execution, providing
insights into memory usage, worker status, task execution, and shared
memory segments.

## Details

The diagnostics API provides multiple views into shard's runtime
behavior:

- [`report()`](https://bbuchsbaum.github.io/shard/reference/report.md):
  Primary entry point with configurable detail levels

- [`mem_report()`](https://bbuchsbaum.github.io/shard/reference/mem_report.md):
  Memory usage across workers

- [`cow_report()`](https://bbuchsbaum.github.io/shard/reference/cow_report.md):
  Copy-on-write policy tracking

- [`copy_report()`](https://bbuchsbaum.github.io/shard/reference/copy_report.md):
  Data transfer statistics

- [`task_report()`](https://bbuchsbaum.github.io/shard/reference/task_report.md):
  Task/chunk execution statistics

- [`segment_report()`](https://bbuchsbaum.github.io/shard/reference/segment_report.md):
  Shared memory segment information

All functions return S3 `shard_report` objects with appropriate print
methods for human-readable output.
