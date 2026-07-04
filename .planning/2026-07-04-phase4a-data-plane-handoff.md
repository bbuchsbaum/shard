# Handoff: Phase 4A data-plane fixes

Date: 2026-07-04
Branch: `efficiency-phase4a-data-plane`
Base: `efficiency-phase1` at `d36dbfb`

## Scope Completed

Phase 4A was kept to the low-risk R-side batch from the review handoff:

- **4.1 Scratch pool reuse**
  - `scratch_matrix()` now stores the dim-applied matrix in the pool.
  - Reuse is exact-length for matrix shapes, avoiding the latent larger-buffer
    `dim<-` failure when a custom key is reused for a smaller shape.
  - Tests cover observable identity reuse, shape-correct misses, dimensions,
    and existing worker recycle diagnostics.
- **4.2 Lazy validation**
  - Atomic non-deep `share()` now takes the ALTREP fast path before recursive
    validation.
  - `deep=TRUE` no longer pre-walks; traversal owns validation and still fails
    functions/external pointers/connections at the traversed path.
  - Non-atomic non-deep `share()` still rejects closures with existing
    path-aware errors after serialization, preserving current public behavior.
- **4.3 Hotspot gating**
  - Expensive materialization hotspot attribution is off by default for local
    materialization and only records when `options(shard.view_hotspots = TRUE)`
    or diagnostics-enabled worker dispatch turns on the internal flag.
  - Materialization counters remain unconditional.
- **4.6 Zero-init buffer allocation**
  - `buffer()` skips the initial full-buffer write for `init = NULL` and
    type-zero initialization.
  - Nonzero initialization behavior is preserved and now recorded through
    `buffer_diagnostics()` `init_writes` / `init_bytes`.

## Verification

- Phase 3 closure before branching:
  - Exact Phase 3 native smoke command passed locally.
  - Synthetic failing assertion made the fixed smoke command exit nonzero with
    `Phase 3 native smoke: 1 test failure(s)`, then the temporary edit was
    removed.
  - Affected Phase 3 files passed.
  - Full suite on `efficiency-phase1`: `FAIL 0 | WARN 1 | SKIP 24 | PASS 1671`.
  - `R CMD check --as-cran --no-manual`: `0 errors / 2 warnings / 0 notes`.
  - Fresh GitHub Actions run `28712312476` passed on `ubuntu-latest` and
    `windows-latest`.

- Phase 4A verification:
  - Focused Phase 4A files passed:
    `test-scratch_pool.R`, `test-buffer.R`, `test-view-materialize-hotspots.R`,
    `test-share.R`, `test-deep-share.R`, `test-share-hook.R`.
  - Dispatch/view/table diagnostics files passed:
    `test-phase2-dispatch-serialization.R`,
    `test-view-kernels-shard_map.R`,
    `test-view-gather-kernel-shard_map.R`,
    `test-table_buffer-shard_map.R`,
    `test-tiled-crossprod-buffer-shard_map.R`,
    `test-diagnostics.R`.
  - Full suite on Phase 4A: `FAIL 0 | WARN 1 | SKIP 24 | PASS 1700`.
    The warning is the existing intentional forced-error path in
    `test-shard_reduce.R`.
  - State-mutation scan found cleanup `on.exit()` calls and option mentions,
    but no new top-level vignette/example state leak.
  - `R CMD check --as-cran --no-manual`: `0 errors / 2 warnings / 0 notes`.
    The warnings are the expected CRAN incoming version `0.1.1` warning and the
    Homebrew clang/R `Boolean.h` warning.

## Remaining Phase 4 Work

- **Phase 4B:** 4.4 C gather/scatter buffer I/O on its own branch.
- **Phase 4C:** 4.7/4.8 table cleanup after buffer behavior is stable.
- **Phase 4.5:** `fetch(..., materialize = FALSE)` remains a separate API
  branch. Do not flip `fetch()` laziness by default.

## Notes

- `bd ready` / `bd where` could not find an active beads workspace in this
  checkout, and the installed `bd` does not support `bd sync`; no beads issue
  state was updated.
