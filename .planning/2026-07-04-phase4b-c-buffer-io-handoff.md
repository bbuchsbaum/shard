# Handoff: Phase 4B C buffer gather/scatter I/O

Date: 2026-07-04
Current branch: `efficiency-phase4a-data-plane`
Current head: `0bf7de3` (`Preserve deep environment validation`)
Recommended next branch: `efficiency-phase4b-c-buffer-io`

## Starting State

Phase 4A is closed on this branch and pushed to origin:

- `8e5b33c` - `Implement Phase 4A data-plane fixes`
- `0bf7de3` - `Preserve deep environment validation`

The Phase 4A review found the implementation sound and merge-ready, with one
contract decision. Commit `0bf7de3` chose the safer option: balanced deep share
now rejects direct forbidden bindings inside kept environments, preserving the
old safety behavior for objects like connections.

Before starting Phase 4B, confirm you are either:

1. on `efficiency-phase4a-data-plane` at or after `0bf7de3`, or
2. on the branch where Phase 4A has been merged.

Suggested start:

```bash
git fetch origin
git switch efficiency-phase4a-data-plane
git pull --ff-only
git switch -c efficiency-phase4b-c-buffer-io
```

If Phase 4A has already been merged elsewhere, branch from that merge target
instead.

## Phase 4A Verification Baseline

Latest local evidence after `0bf7de3`:

- Focused regression files passed:
  `test-deep-share.R`, `test-scratch_pool.R`.
- Full suite passed:
  `FAIL 0 | WARN 1 | SKIP 24 | PASS 1704`.
  The warning is the existing intentional forced-error path in
  `test-shard_reduce.R`.
- Locale-pinned CRAN-like check passed:
  `0 errors / 2 warnings / 0 notes`.
  The warnings were the expected CRAN incoming version `0.1.1` warning and the
  local Homebrew clang/R `Boolean.h` warning.
- Worktree was clean and pushed after `0bf7de3`.

## Phase 4B Scope

Implement **4.4 C gather/scatter buffer I/O** only.

Goal: remove the remaining full-buffer read and per-element write costs in
`shard_buffer` indexing paths.

In scope:

1. Add typed C gather read over arbitrary element indices.
2. Add typed C scatter write over arbitrary element indices.
3. Route sparse `[` / `[<-` buffer paths through those C entry points.
4. Accept `idx_range()` selectors in buffer indexing where it avoids allocating
   `seq_len()` ranges and repeated O(n) validation.
5. Add focused tests and a small benchmark or diagnostic assertion proving the
   intended copy/write reduction.

Out of scope:

- Do not change `fetch()` laziness or start Phase 4.5.
- Do not start table string/table sink cleanup (4.7/4.8).
- Do not change Phase 4A behavior except if a Phase 4B test exposes a real
  regression.
- Do not fold in the pre-existing `buffer(init = <multi-element vector>)`
  hardening unless you split it as a separate commit/issue; it was explicitly
  out of scope for Phase 4A review.

## Current Hot Paths

Main files:

- `R/buffer.R`
  - `.buffer_read_range()` currently calls `segment_read()` and then `readBin()`.
  - Sparse 1D reads in `[.shard_buffer` read the entire buffer and subset it.
  - Matrix reads also read the entire buffer before selecting `linear_idx`.
  - `.buffer_write_indices()` coalesces sorted adjacent runs, but falls back to
    one `.buffer_write_range()` call per index for out-of-order or duplicate
    assignments.
  - Matrix assignment still falls back to full read/modify/write for many
    non-contiguous shapes.
- `src/shard_shm.c` / `src/shard_shm.h`
  - Existing registered calls are raw/range-oriented:
    `C_shard_segment_write_raw()` and `C_shard_segment_read_raw()`.
  - New gather/scatter calls should live near those wrappers.
- `src/init.c`
  - Register any new `.Call` entry points here.
- `tests/testthat/test-buffer.R`
  - Existing buffer coverage is here. Add a new focused file if the Phase 4B
    cases get too large.

Original plan target:

```text
C_shard_segment_scatter_write(seg, idx, values)
C_shard_segment_gather_read(seg, idx)
```

It is fine to add a `type` argument for gather if needed, because read result
type cannot be inferred from `seg` alone. Keep the public R API unchanged.

## Implementation Guidance

Preferred R-side shape:

- Add `.buffer_read_indices(x, idx)` for sparse reads.
- Keep `.buffer_read_range()` for contiguous ranges.
- Update `.buffer_write_indices(x, idx, values)` to use C scatter after the
  existing R coercion/recycling logic decides the final target values.
- Preserve current R indexing semantics:
  - one-based indices,
  - no out-of-bounds writes,
  - duplicate assignment is last-write-wins,
  - out-of-order reads preserve requested order,
  - zero-length indices return a zero-length vector of the buffer type,
  - logical buffers are stored as `int` but returned as logical.

C-side details to audit carefully:

- Use `XLENGTH()` / `R_xlen_t` where possible.
- Validate `NA`, `< 1`, and `> n` indices before copying.
- Compute byte offsets with overflow checks:
  `(idx - 1) * elem_size`, and ensure `offset + elem_size <= seg->size`.
- Respect `seg->readonly` for scatter writes.
- Allocate typed return vectors directly for gather:
  `REALSXP`, `INTSXP`, `LGLSXP`, or `RAWSXP`.
- For scatter, copy elements in input order so duplicates preserve R assignment
  semantics.
- Protect every allocated SEXP and avoid storing raw R pointers across
  allocations.
- Keep the existing POSIX/Windows segment abstraction intact; the new routines
  should operate on `seg->addr` and `seg->size`, not on platform-specific file
  handles.

Diagnostics:

- Existing `buffer_diagnostics()` tracks logical write operations and bytes.
  If scatter replaces many one-element writes with one C call, update
  diagnostics once for the assignment with `bytes = length(idx) * elem_size`.
  Add a test that makes this reduction observable.
- There is no read diagnostic today. A tiny benchmark script or testthat
  performance smoke can be enough, but avoid brittle wall-clock thresholds in
  CI. Prefer proving behavior structurally when possible.

## Suggested Tests

Add focused tests before or with implementation:

- Gather read for `double`, `integer`, `logical`, and `raw` buffers with sparse,
  out-of-order indices.
- Scatter write for all four types.
- Duplicate scatter indices preserve last-write-wins.
- Bounds and `NA` indices still error.
- Zero-length read/write behaves sensibly.
- Sparse read no longer reads the whole buffer path, either through a diagnostic
  assertion or a narrowly scoped benchmark.
- Sparse write of many non-contiguous indices records one logical write instead
  of one write per index, if you adopt the diagnostics shape above.
- Matrix sparse read/write cases that previously fell back to full-buffer
  materialization continue to return the same values.
- `idx_range()` works for 1D buffer read/write and, if implemented, for matrix
  row/column selectors without materializing large sequences.
- Worker/disjoint-write smoke still passes, especially:
  `test-table_buffer-shard_map.R`,
  `test-tiled-crossprod-buffer-shard_map.R`, and buffer-backed shard-map tests.

## Verification Required Before Merge

Minimum focused gate:

```bash
env NOT_CRAN=true _R_CHECK_LIMIT_CORES_=true Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-buffer.R"); testthat::test_file("tests/testthat/test-table_buffer-shard_map.R"); testthat::test_file("tests/testthat/test-tiled-crossprod-buffer-shard_map.R")'
```

Then run the full suite:

```bash
env NOT_CRAN=true _R_CHECK_LIMIT_CORES_=true Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_dir("tests/testthat")'
```

Then run CRAN-like check with a real locale. On this machine, running
`rcmdcheck` inside the sandbox can fail at `processx` load with
`Operation not permitted`, so rerun outside the sandbox if that happens:

```bash
env LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8 NOT_CRAN=true _R_CHECK_LIMIT_CORES_=true Rscript -e 'library(rcmdcheck); res <- rcmdcheck::rcmdcheck(path=".", args=c("--as-cran", "--no-manual"), build_args="--no-manual", error_on="never", quiet=FALSE); cat("\n=== SUMMARY ===\n"); cat("Errors:", length(res$errors), "\n"); cat("Warnings:", length(res$warnings), "\n"); cat("Notes:", length(res$notes), "\n")'
```

Expected final check state remains `0 errors / 2 warnings / 0 notes`, with only:

- CRAN incoming version `0.1.1` warning.
- Homebrew clang/R `Boolean.h` warning.

## Beads/Gastown Note

This checkout currently has no active beads database:

```text
bd ready --json
Error: no beads database found
```

The installed `bd` also does not support `bd sync`. Do not block Phase 4B on
beads state unless the next environment has a real `.beads` workspace.

