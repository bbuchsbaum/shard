## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS (aarch64), R 4.x
* GitHub Actions: macOS, Windows, Ubuntu (R-release, R-devel)

## Resubmission (2026-07-17)

This is a resubmission following a Debian incoming-check test failure.

The failing test created a buffer with the platform-dependent `"auto"`
backing, then reopened its path explicitly as `"mmap"`. On Linux, `"auto"`
resolves to POSIX shared memory, so the shared-memory name could not be opened
as a filesystem-backed mapping. The test now creates the buffer with
`backing = "mmap"`, matching the mechanism used to reopen it on every platform.

We also hardened `pin_workers()` to fall back to core 0 when
`parallel::detectCores()` returns `NA`, as permitted by its API.

## Comments

This is an update of shard 0.1.1. Version 0.2.0 is an internal performance
and correctness release: no user-facing API was added or removed. Highlights
(see NEWS.md for the full list):

* Overhauled task-dispatch core (per-dispatch instead of per-task exports,
  O(1) task-queue claiming, compact wire format for strided shard indices).
* Deterministic per-shard RNG streams: results are now identical regardless
  of `workers`, `chunk_size`, or shard-to-worker assignment.
* Hardened the C layer: PROTECT coverage around fresh ALTREP objects,
  overflow-safe offset/length validation on all shared-memory entry points,
  and long-vector (`XLENGTH`) correctness fixes.

Note: to dispatch tasks to its PSOCK workers, shard accesses
`parallel:::sendCall()`/`parallel:::recvResult()` via
`utils::getFromNamespace()`. These are the same unexported entry points that
packages such as 'future' rely on for asynchronous PSOCK dispatch; there is
currently no exported API in 'parallel' for non-blocking send/receive on
cluster nodes.

All parallel examples, tests, and vignettes respect the 2-core CRAN limit
(`_R_CHECK_LIMIT_CORES_` is honoured by the internal worker-count default).
