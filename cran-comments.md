## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS (aarch64), R 4.x
* GitHub Actions: macOS, Windows, Ubuntu (R-release, R-devel)

## Comments

This is a bug-fix release of shard 0.2.0. No user-facing API was added,
changed, or removed.

It fixes a memory-safety defect in the ALTREP shared-vector layer. Shared
vectors cache their resolved base pointer across reads, and closing the
backing segment unmapped it without invalidating that cache, so a read
performed before `close()` left a pointer into unmapped memory that a
subsequent read would reuse. Depending on whether the operating system had
reused the address range, this crashed the R session or silently returned
garbage.

The same release makes every access path on a closed shared vector raise an
error. Previously only the `DATAPTR` paths did so: element access returned
`NA`, and the ALTREP region-copy methods returned a short count without
filling the caller's buffer, so `sum()`, `mean()` and `max()` read
uninitialized memory.

Neither defect was reachable from the package's own tests, examples, or
vignettes, so it did not surface in CRAN checks of 0.2.0. Both are now
covered by regression tests in `tests/testthat/test-altrep-close-lifecycle.R`.

Two further defects found while fixing the above are also addressed: a bus
error when `segment_protect()` was applied to a segment that already had a
writable shared vector over it (the vector kept a creation-time copy of the
segment's read-only state), and an abort in `pool_health_check()` when a
worker's resident-set size could not be read. Both have regression tests.

Note: to dispatch tasks to its PSOCK workers, shard accesses
`parallel:::sendCall()`/`parallel:::recvResult()` via
`utils::getFromNamespace()`. These are the same unexported entry points that
packages such as 'future' rely on for asynchronous PSOCK dispatch; there is
currently no exported API in 'parallel' for non-blocking send/receive on
cluster nodes.

All parallel examples, tests, and vignettes respect the 2-core CRAN limit
(`_R_CHECK_LIMIT_CORES_` is honoured by the internal worker-count default).
