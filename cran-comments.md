## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Test environments

* local macOS (aarch64), R 4.x
* GitHub Actions: macOS, Windows, Ubuntu (R-release, R-devel)

## Resubmission notes (2026-04-05)

Two issues identified by CRAN and Prof Ripley have been corrected:

1. **gcc-UBSAN**: `shard_taskq_header_t` used a `tasks[1]` trailing array (C89
   "struct hack"), causing UBSAN to flag all `tasks[i]` accesses with `i >= 1`
   as out-of-bounds. Changed to a C99 flexible array member (`tasks[]`) in both
   the atomic and non-atomic struct branches, and updated the size calculation
   accordingly.

2. **Excess parallel processes**: Default worker counts derived from
   `parallel::detectCores() - 1` were not capped during `R CMD check`. Added a
   `.default_workers()` helper that respects the `_R_CHECK_LIMIT_CORES_`
   environment variable (set by CRAN) and caps parallelism at 2. Applied at all
   four call sites (`pool_create()`, `shard_map()`, `shards()`,
   `shard_reduce()`).
