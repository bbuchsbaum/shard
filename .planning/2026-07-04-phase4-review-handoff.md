# Handoff: Phase 3 review closure and Phase 4 plan

Date: 2026-07-04
Branch: `efficiency-phase1`
Current reviewed code commit: `d53ede6` (`Phase 3 review fixes: gate CI on failures, harden taskq/registry`)

## Current State

At the start of this handoff pass, the working tree was clean and the Phase 3 review-fix
code was one commit ahead of `origin/efficiency-phase1`. If this handoff is committed
afterward, there will also be a documentation-only handoff commit.

- `d53ede6` is local and unpushed in this checkout.
- `origin/efficiency-phase1` is `e03866f`.
- The previous remote smoke run `28710384759` passed, but it used the old false-green
  workflow. Treat it as compile/no-crash evidence only, not proof that Phase 3 tests fail
  the job correctly.

The latest commit says the final full suite was running when it was created. Do not assume
that final confirmation happened unless you can see the output in this checkout or rerun it.
This handoff pass locally reran the exact fixed smoke-command snippet against
`test-phase3-taskq.R` and `test-phase3-altrep-perf.R`; it exited 0.

## What The Review Found

Three fresh-context adversarial reviewers found no Phase 3 code blockers or majors:

- 3.1 taskq: merge-safe under TSan plus ASAN/UBSAN, 500 iterations with 24 oversubscribed
  workers, and 30 cross-process exactly-once runs with injected failures.
- 3.4/3.6/3.7 mmap lifetime: merge-safe under ASAN registry harness; dropping all parents
  then forcing GC repeatedly kept 200-deep view chains readable.
- 3.2/3.5 backing/madvise: Linux tmpfs branch is live where expected; cleanup and guards
  are correct.

One real blocker was in verification, not the C code: `.github/workflows/phase3-native-smoke.yaml`
was false-green because `testthat::test_file()` returns results but does not fail the R
process. Commit `d53ede6` changes the workflow to collect failures/errors and `stop()`.

Minor fixes in `d53ede6`:

- `src/shard_taskq.c`: `taskq_supported()` now requires `ATOMIC_INT_LOCK_FREE == 2`.
- `src/shard_shm.c`: degenerate `(dev, ino) == (0, 0)` attach identities bypass the registry.
- `R/dispatch_shm_queue.R`: renamed a local `n` that clobbered the function parameter.

Deferred, low risk: do not add `-lrt` to Makevars blindly for pre-glibc-2.34 Linux. Current
CI/CRAN links through libc; revisit only with a failing platform or controlled Linux proof.

## Review Instructions For The Next Agent

First review `d53ede6` itself. Do not start Phase 4 until this review is complete.

1. Confirm the branch and cleanliness:
   `git status --short --branch`
   `git log --oneline --decorate -8`
2. Inspect the commit:
   `git show --stat d53ede6`
   `git show d53ede6 -- .github/workflows/phase3-native-smoke.yaml src/shard_taskq.c src/shard_shm.c R/dispatch_shm_queue.R`
3. Re-run the exact fixed smoke command:
   `env NOT_CRAN=true _R_CHECK_LIMIT_CORES_=true Rscript -e 'pkgload::load_all(".", quiet = TRUE); files <- c("tests/testthat/test-phase3-taskq.R", "tests/testthat/test-phase3-altrep-perf.R"); bad <- 0L; for (file in files) { res <- as.data.frame(testthat::test_file(file, reporter = "summary")); failed <- sum(res$failed) + sum(res$error); if (failed > 0L) { bad <- bad + failed; cat(sprintf("FAIL: %s -> %d failed/errored\n", file, failed)); } }; if (bad > 0L) stop(sprintf("Phase 3 native smoke: %d test failure(s)", bad)); cat("Phase 3 native smoke: all regression files passed\n")'`
4. Prove the workflow can fail red before trusting it remotely. Use a temporary uncommitted
   failing test file or a throwaway branch; do not leave the synthetic failure in the tree.
5. Run affected suites at minimum:
   `test-phase3-taskq.R`, `test-phase3-altrep-perf.R`, `test-shm_queue-dispatch.R`,
   `test-segment.R`, `test-cow.R`, `test-deep-share.R`.
6. Run the full suite:
   `env NOT_CRAN=true Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_dir("tests/testthat")'`
7. Run `R CMD check --as-cran --no-manual` with `LC_ALL=C LANG=C _R_CHECK_LIMIT_CORES_=TRUE`.
   The only expected warnings are the CRAN incoming version `0.1.1` warning and the
   Homebrew clang/R `Boolean.h` warning.
8. After pushing `d53ede6`, verify the *new* `Phase 3 native smoke` run on
   `ubuntu-latest` and `windows-latest`. Do not cite run `28710384759` as post-fix CI
   evidence.

If these pass, update the plan to mark Phase 3.1/3.2 and 3.4-3.7 merge-ready. Phase 3.3
remains separate and unstarted.

## Phase 4 Decision

Do not change `fetch()` laziness by default in Phase 4.

Use this API shape when 4.5 is eventually tackled:

- `fetch(x, materialize = TRUE)` remains the default and preserves current eager behavior.
- `fetch(x, materialize = FALSE)` returns lazy ALTREP leaves where safe.
- `materialize()` stays unconditionally eager.

Reason: current callers can rely on ordinary R objects after `fetch()` for equality checks,
S4 slot assignment, and mutation expectations. Flipping the default would be an API break
hidden as a performance optimization.

## Phase 4 Execution Plan

Create a new branch after Phase 3 review is closed, preferably from the reviewed
`efficiency-phase1` head:

`git switch -c efficiency-phase4a-data-plane`

Phase 4A should be a low-risk R-side batch with no public API changes:

1. **4.1 Scratch pool reuse** (`R/scratch.R`)
   - Store the dim-applied object in the pool so hits actually reuse the object.
   - Fix the latent exact-length/dim assignment issue.
   - Tests: repeated scratch requests reuse identity/address where observable; diagnostics
     report real hits; dimensions are correct; workers do not leak reused scratch into
     user-visible results.
2. **4.2 Lazy validation** (`R/share.R`)
   - Hoist the atomic fast path above recursive `validate_serializable()`.
   - Try `serialize()` first and only walk recursively on failure.
   - For `deep=TRUE`, let traversal own validation rather than pre-walking.
   - Tests: valid atomic and deep payloads still share/fetch identically; intentionally
     unserializable inputs still fail with clear errors; validation diagnostics do not
     regress.
3. **4.3 Hotspot gating** (`R/views.R`)
   - Keep cheap counters unconditional.
   - Gate expensive `sys.calls()`/deparse hotspot attribution behind
     `getOption("shard.view_hotspots", FALSE)` and/or diagnostics-enabled dispatch.
   - Tests: hotspot list empty by default; populated when option/diagnostics enables it;
     materialization counters unchanged.
4. **4.6 Zero-init buffer allocation** (`R/buffer.R`, C only if needed for nonzero fill)
   - Skip the initial full write when `init` is `NULL` or the type zero.
   - Preserve nonzero initialization behavior.
   - Tests: zero-filled reads are correct without a recorded full-buffer write; nonzero
     init still fills correctly; large buffer creation avoids unnecessary write diagnostics.

Keep 4A as one commit series only if the changes stay small and independent. If any item
starts touching C entry points or public API, split it out.

Phase 4B should be **4.4 C gather/scatter buffer I/O** on its own branch. It needs C entry
points, direct typed reads/writes, and sparse read/write microbenchmarks. Do not mix this
with 4A.

Phase 4C should handle **4.7/4.8 table cleanup** after buffer behavior is stable.

Phase 4.5 (`fetch(..., materialize = FALSE)`) should be a separate API branch after 4A/4B
because it changes object identity and mutation semantics.

## Phase 4 Acceptance Criteria

Each Phase 4 branch needs:

- Focused regression tests for every changed behavior.
- A small benchmark or diagnostic assertion proving the intended allocation/copy reduction.
- Full `testthat` suite passing.
- `R CMD check --as-cran --no-manual` clean except known environmental warnings.
- Fresh-context review before merge.

Do not mix Phase 3.3 streaming serialization with Phase 4. It has silent-corruption risk
and must remain its own read-side-first branch with byte-identity stress tests.
