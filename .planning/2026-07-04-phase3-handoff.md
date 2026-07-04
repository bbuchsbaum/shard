# Handoff: shard efficiency overhaul → Phase 3

Date: 2026-07-04
Plan: `.planning/2026-07-03-efficiency-overhaul.md` (read the Phase 2 amendment block and
the Phase 1 status/follow-ups sections — they modify the original plan).
Branch: `efficiency-phase1` (poorly named now; carries Phases 1 AND 2). Not pushed.
Base for diffs: `main` is at `42c8c22` (CRAN 0.1.1 resubmission fixes).

## Current update from follow-up pass

As of the post-Phase-3 pass, `efficiency-phase1` also carries Phase 3 Branch 1
(`3.1+3.2`) and Phase 3 Branch 2 (`3.4-3.7`). Current HEAD observed:
`8e57515 Merge Phase 3 branch 2: attach registry, madvise hints, GC-native ownership,
cached base pointer`. Follow-up validation cleared the local Phase 3 gate: focused
current-HEAD tests for Phase 2 dispatch/reduce and Phase 3 taskq/ALTREP passed, the full
`testthat` suite passed (1671 pass, 0 fail, 24 expected/env skips), and
`R CMD check --as-cran --no-manual` passed with 0 errors/0 notes and only the two known
environmental warnings (CRAN incoming version 0.1.1; Homebrew clang/R `Boolean.h`
warning). Remote native smoke run `28710384759` passed on both `ubuntu-latest` and
`windows-latest`, including the Phase 3 regression files for the compile-blind C paths.
Remaining gate: fresh-context review.

## State at handoff

- **Phase 1 (correctness, items 1.1–1.11): DONE, reviewed (verdict: merge-ready).**
- **Phase 2 (dispatch serialization, items 2.1–2.5 + F1 + 5.1 pulled forward): DONE.**
  - Full suite: **1566 passed / 0 failed / 24 env-gated skips / 1 expected warning**
    (the permanent-failure warning in test-shard_reduce.R).
  - `R CMD check --as-cran`: clean except two ENVIRONMENTAL warnings that predate this
    work and recur every run — (a) "insufficient package version 0.1.1" (already in CRAN
    incoming), (b) `R_ext/Boolean.h` `-Wfixed-enum-extension` noise under Homebrew
    clang 20. Neither is ours; don't chase them.
  - **Fresh-context review of Phase 2: COMPLETE.** One MAJOR found and fixed
    (`ab89042`): strided wire expansion rebuilt `idx` as integer where the public
    descriptor is double → silent NA on worker-side integer index arithmetic past 2^31.
    Fix carries the idx storage type on the wire; kernel-visible shard is now
    `identical()` to the public descriptor (incl. field order), pinned by
    `expect_identical` round-trip tests with boundary shards. Three minors also fixed
    (test tightening, leaked `wire` marker field, stale `set_worker_seeds` doc).
    Reviewer verified everything else solid: no frame capture anywhere, stale-kernel
    recycle case clean, diagnostics gating preserves the scratch recycle safety signal,
    reduce fold drains correctly under failures, seed reproducibility exact. Phase 2 is
    merge-ready.

### Headline Phase 2 numbers (measured, installed package, 2000 tiny chunks, 2 workers)
- Dispatch wall-clock 37.78 s → 0.85 s (~44x). Per-send payload 544 KB → 7.3 KB (~74x),
  now **O(1) in chunk count** (was O(chunks) per send → O(chunks²) per run).
- Regression harness: `inst/bench/dispatch-overhead.R`; payload-size + chunk-count
  invariance pinned in `tests/testthat/test-phase2-dispatch-serialization.R`.

### What changed structurally (things Phase 3 code will touch/see)
- `shard_dispatch_wrapper_(chunk, collect_diag)` is a **namespace** function
  (R/dispatch.R); diagnostics snapshot/delta split into `shard_dispatch_diag_*_()`.
- User kernel travels ONCE per dispatch inside `.shard_dispatch_fun` (the executor from
  `make_chunk_executor(auto_table, fun)` — both args `force()`d; unforced promises
  re-drag the caller frame, this was measured). Manifest-recorded → recycled workers
  replay it. `chunk$fun` remains only as a legacy fallback in the executor.
- `create_shard_chunks()` no longer takes `fun`; chunks carry ids/descriptors/
  `rng_streams`/small metadata only.
- Strided shard descriptors are wire-compacted to `{id,start,stride,len}` by
  `shard_wire_compact_` and re-inflated worker-side by `shard_wire_expand_` (both in
  R/shard_map.R) at the top of BOTH shard_map executor branches and the shard_reduce
  reducer loop. **Wire-only**: public `shards()` objects still carry `$idx`. shm_queue
  path unaffected (ships descriptors via shared handle, with its own contiguous
  inflation in R/dispatch_shm_queue.R:206-215).
- `shard_reduce`: reducer built by `make_chunk_reducer_(map_fun, combine_fun)` with a
  minimal env (parent = namespace); per-shard L'Ecuyer-CMRG streams (same
  `make_shard_seed_streams_` as shard_map); `chunk_size="auto"` =
  `max(1, ceiling(num_shards/(workers*4)))`; **init applied exactly once** in the master
  fold; master folds partials in **chunk-id order** (out-of-order partials stashed) —
  this is what makes runif+sum reduce bit-exact across worker counts.
- Phase 1 infra you must not regress: pool bootstrap manifest
  (`pool_manifest_record_/clear_`, `pool_bootstrap_worker_` in R/pool.R) replayed on all
  5 restart/recycle paths; per-shard seed streams in chunks; `worker_is_alive()` trusts
  `pid_is_alive()` when PID known (SIGPIPE segfault fix).

## Phase 3 scope (C layer) — sequencing as amended
1. **Branch 1: 3.1 + 3.2** (taskq fetch_add cursor + batched claim; Linux AUTO→/dev/shm).
   Small, big effect. 3.1 is lock-free shm code — keep the fallback sweep for retried
   tasks; microbenchmark claims/sec (1e5 tasks, 8 workers) before/after.
2. **Branch 2: 3.4–3.7** (attach registry; madvise hints; kill O(k²) preserve-list via
   xptr tag/prot; cached base pointer w/ generation counter). 3.4 and 3.6 are the risky
   ones here: refcount/GC lifetime — write finalizer-ordering tests, valgrind locally if
   feasible.
3. **Branch 3: 3.3 alone** (streaming serialize via custom R_outpstream_t/R_inpstream_t).
   Riskiest item in the whole plan (silent-corruption class). Read side (~30 lines)
   FIRST; byte-identity round-trip stress tests across SEXP types/attributes/encodings/
   long vectors before the write side.
4. 3.8 optional, skip unless measured.

Platform gap: dev machine is macOS; 3.2/3.5 are Linux-only branches and 3.1 atomics are
platform-sensitive. Consider a GitHub Actions ubuntu job running the taskq stress test
before merging Branch 1. Windows `VirtualProtect` branch (Phase 1.10) is also
compile-blind so far — CI would cover that too.

## Deferred / decisions pending
- **F2** (low): `cow="deny"` docs promise "error on mutation" but the `unclass()` bypass
  silently writes to a private copy — C-level hard error is unimplementable (base R read
  paths request writable DATAPTR; empirically breaks identical/range/==). Decide:
  opt-in strict mode vs soften docs.
- **4.5 is an API decision** (fetch() laziness default) — put to the maintainer before
  Phase 4, do not land as a silent optimization.
- Phase 5 leftovers: 5.2–5.9 (5.1 already done). Phase 6: each feature its own project.

## Operational notes for whoever runs this
- Protocol per phase: worktree subagents implement+test+commit → integrator merges →
  full suite + `R CMD check --as-cran` → fresh-context review agent → fold MAJOR+
  findings → update plan doc status block.
- **Known harness bug to watch:** resumed/background worktree agents twice leaked edits
  into the MAIN tree instead of their worktree. Before any merge, `git status` the main
  tree; stray edits were stashed, not deleted (`git stash list`: two entries labeled
  "stray … superseded by …" — safe to drop once Phase 2 review passes; the third leak
  became commit `dee6c08` after inline review/completion).
- Tests: `NOT_CRAN=true Rscript -e 'devtools::load_all(); testthat::test_dir("tests/testthat")'`.
  Long-vector test opt-in: `SHARD_TEST_LONG_VECTORES` — actually `SHARD_TEST_LONG_VECTORS=1`
  (needs ~4 GB, passes on this 36 GB machine, ~3 s).
- `R CMD check` scratch: build the tar.gz OUTSIDE the repo (scratchpad), check with
  `_R_CHECK_LIMIT_CORES_=TRUE`.
- Commits: Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>. No pushes unless the
  user asks. No new hard deps (CRAN discipline; progressr/nanoarrow later as Suggests).
- Version currently 0.1.1 (in CRAN incoming). If CRAN accepts/rejects meanwhile,
  DESCRIPTION version may need bumping before any further submission; don't touch
  otherwise.
