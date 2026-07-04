# shard efficiency overhaul — phased plan

Date: 2026-07-03
Status: ACTIVE — Phases 1-3.2/3.4-3.7 implemented on `efficiency-phase1`
Basis: four parallel deep audits (dispatch/pool, C layer, data plane, sharding/reduce/API).
Findings marked **[measured]** were verified empirically during the audit.

## Executive summary

shard's architecture is sound, but a small number of implementation defects silently
defeat its three headline promises:

- **Cheap dispatch** is defeated by the `dispatch_wrapper` closure serializing the entire
  dispatch frame per chunk — O(chunks²) wire volume **[measured: 1.98 MB/send where 609 B
  would do; ~4 GB traffic for a 2,000-chunk run]**.
- **Zero-copy sharing** is defeated for the common cases: deep-shared objects are fully
  materialized in every worker on fetch; Linux `AUTO` backing writes to a disk file instead
  of tmpfs; non-atomic share/fetch double-copies; the scratch pool never actually reuses.
- **Reproducibility** (`seed=`) is not delivered: per-worker MT seeds + dynamic shard
  assignment means RNG-using kernels give different answers run to run.

Plus one flat correctness landmine: `LENGTH()` int truncation in the segment write path for
long vectors (>2^31 elements) — exactly the target workload.

The plan is six phases, ordered so each lands independently, each verified by
tests + `R CMD check` before the next begins. Phases 1–2 are small and remove the majority
of measured overhead. Features (Phase 6) are separated and individually optional.

---

## Phase 1 — Correctness bugs (no behavior redesign, just fixes)

Everything here is a bug a user can hit today. Land as one commit series with regression
tests per fix.

| # | Fix | Location | Notes |
|---|-----|----------|-------|
| 1.1 | `LENGTH()` → `XLENGTH()` in segment write path | `src/shard_shm.c:723` | Audit whole file for other `LENGTH`/int-index uses on data vectors. Long-vector regression test (guarded by memory availability, skip on CI if needed). |
| 1.2 | `buf[, j] <- vector` full read-modify-write (1000x write amplification **[measured]** + silent race vs concurrent disjoint writes) | `R/buffer.R:551-579` (and array fallback `:582-589`) | Recycle/reshape vector value to matrix before the fast-path test (~3 lines). Test: byte-write counter assertion + two-worker disjoint-write race test. |
| 1.3 | Recycled workers lose `.shard_borrow` / `.shard_out` / auto-table sink / seeds → retry storms, permanent failures | 5 restart paths in `R/dispatch.R` (`:270-279, :308-314, :333-338, :372-378, :416-422`) | Introduce a **worker bootstrap manifest** stored on the pool at export time; `pool_restart_worker_`/`worker_spawn` replay it. Centralizes the 5 copy-pasted re-export blocks. Test: force a recycle mid-run (tight rss_limit), assert results correct. |
| 1.4 | `seed=` not reproducible: per-worker MT offsets + dynamic assignment | `R/shard_map.R:969-978` | Per-**shard** L'Ecuyer-CMRG streams: derive stream from `seed` advanced by `nextRNGStream` keyed on shard id; set around each shard call in the chunk executor. Deterministic regardless of worker count/assignment. Test: same seed, different `workers=`, identical results. |
| 1.5 | `.view_hotspot_key_` aborts materialization when call head deparses to length>1 (R ≥ 4.3 error in `if`) | `R/views.R:120-121` | `fn <- fn[1L]` guard (full gating of this path happens in Phase 4.3). |
| 1.6 | `parse_count` int overflow ("3B" → NA + misleading error); "B"=billions here vs bytes in `parse_bytes` | `R/shards.R:300-323` | Compute in double, validate, clear error. |
| 1.7 | `parse_bytes` returns NA silently on malformed input ("1..5GB"), NA propagates into mem_cap comparisons | `R/utils.R:38-67` | `is.na` check + error. |
| 1.8 | `unique_id()` consumes global RNG (side effect of `table_sink()` on user reproducibility) | `R/utils.R:162-173` | Use `tempfile()`-style entropy or save/restore `.Random.seed`. |
| 1.9 | `deny_write` unenforced at C level: `DATAPTR(x, writable)` on a deny vector silently copies instead of erroring | `src/shard_altrep.c:414-465` | `if (writable && info->deny_write) Rf_error(...)`. This is the whole point of `cow="deny"`. |
| 1.10 | Windows `shard_segment_protect` is a no-op flag → COW semantics silently differ cross-platform | `src/shard_shm.c:364-371` | Use `VirtualProtect(PAGE_READONLY)`; if not feasible, document loudly. |
| 1.11 | Coercion counter lies: `altrep_coerce` increments `materialize_calls` when nothing materializes | `src/shard_altrep.c:250` | Separate counter or drop increment. |

**Verify:** full test suite + `R CMD check` clean; new regression tests for 1.1–1.4.

**Status (2026-07-03): DONE and merged to `efficiency-phase1`.** 1471 tests pass, 0
failures; `R CMD check --as-cran` clean apart from two environmental warnings (0.1.1
already in CRAN incoming; R's own `Boolean.h` warning under Homebrew clang 20). Fresh-
context review verdict: merge-ready, no blockers/majors. Notable outcomes:
- 1.3/1.4 landed via a bootstrap manifest on the pool + per-shard L'Ecuyer-CMRG streams
  carried in chunks (reproducible across `workers=`/`chunk_size=`/recycle).
- 1.9 delivered by deliberate deviation: an unconditional `Rf_error` on writable
  `DATAPTR` is unimplementable (base R read paths — `identical`, `range`, `==` — request
  writable pointers), so enforcement is via readonly→COW-into-data2 + `mprotect`/
  `VirtualProtect`; the invariant (bypassed writes never reach the shared segment) is
  pinned by tests. A strict hard-error mode would need an opt-in option.
- Bonus: fixed a pre-existing master segfault (SIGPIPE writing to a dead worker's socket
  during dispatch) in `R/worker.R`.
- Review finding M1 (stale `.shard_out` manifest entry replayed to recycled workers)
  fixed + regression-tested.

### Phase 1 follow-ups (deferred, from review)
- **F1 (was M2):** `shard_reduce(seed=)` still non-reproducible — 1.4's per-shard CMRG
  streaming landed only in `shard_map`'s rpc_chunked path; `shard_reduce.R:126-127` still
  uses legacy `set_worker_seeds()` (per-worker MT + dynamic assignment). Port the same
  `make_shard_seed_streams_` scheme into the reduce chunk executor. Priority: medium.
- **F2 (was M3):** `cow="deny"` docs promise "error on mutation", but the `unclass(x)`
  bypass path silently succeeds on a private copy rather than erroring. Either add an
  opt-in strict mode or soften the documented guarantee. Priority: low.

---

## Phase 2 — Dispatch-path serialization (the single biggest win)

Two agents independently identified this as the dominant per-task cost.

**Amended 2026-07-04 (external feedback, accepted):** the "10–30 lines" estimate was
optimistic; treat Phase 2 as five sub-streams in this order: (a) namespace dispatcher
wrapper [2.1], (b) user-function export + chunk-schema cleanup [2.2], (c) reduce-path
bundle: minimal env [2.3] + per-shard seed streams for `shard_reduce` [Phase-1 F1] +
`chunk_size="auto"` fix [pulled forward from 5.1], (d) diagnostics gating [2.4],
(e) strided-shard wire compaction [2.5] — **wire-only**: `shards()` publicly promises
`$idx`, so compact descriptors exist only in the chunk payload and are inflated
worker-side before any user code runs. Other accepted amendments: 3.3 gets its own
branch with C-level stress tests; 4.5 (`fetch()` laziness) is an API decision to put to
the maintainer before Phase 4, not a silent optimization; Phase 6 items are each their
own project, not one phase.

| # | Fix | Location |
|---|-----|----------|
| 2.1 | Frame-free dispatch wrapper: make `dispatch_wrapper` a package-namespace function taking `(chunk, collect_diag)`; stop serializing the dispatch frame per send | `R/dispatch.R:85-169`, send at `:342-343` |
| 2.2 | Stop embedding user `fun` in every chunk: export once as `.shard_user_fun` (the existing `.shard_dispatch_fun` channel), chunks carry only ids/shards | `R/shard_map.R:1201-1210`; also `pool_lapply` at `R/dispatch.R:574-575` |
| 2.3 | `shard_reduce`'s `chunk_reducer` closure drags the whole shards+chunks lists in its grandparent frame → give it a minimal env (`new.env(parent = asNamespace("shard"))` containing only `combine_fun`/`init_val`) | `R/shard_reduce.R:135` |
| 2.4 | Gate diagnostics: thread `diagnostics` through `dispatch_chunks()`; skip the 8 per-chunk snapshots, delta construction, inflated result payloads, and master-side aggregation when off | `R/dispatch.R:86-101, :455-502` |
| 2.5 | Strided shards: carry `{start, stride, len}` and synthesize `idx` worker-side (contiguous shards are already ALTREP-compact **[measured: 278 B vs 400 KB]**; strided are fully materialized **[measured: 50 KB per 12.5k-element shard]**) | `R/shards.R:273-290` |

**Verify:** add a serialization-size regression test (serialize the actual `sendCall` payload
for a synthetic run; assert < 10 KB per chunk). Benchmark before/after: 2,000 tiny chunks,
expect order-of-magnitude wall-clock improvement on dispatch-bound workloads.

**Status (2026-07-04): DONE and merged to `efficiency-phase1`** (incl. 2.3+F1+5.1 reduce
bundle and wire-only 2.5). Measured: 2,000 tiny chunks 37.78 s → 0.85 s (~44x); per-send
payload 544 KB → 7.3 KB (~74x), now O(1) in chunk count. Reduce: init applied exactly
once, master folds partials in chunk-id order (bit-exact FP reduce across worker counts),
`seed=` reproducible in both shard_map and shard_reduce. Fresh-context review found one
MAJOR (strided idx reconstructed as integer vs public double → silent NA on integer index
arithmetic past 2^31) — fixed in ab89042 (idx storage type carried on the wire; kernel
shard now identical() to the public descriptor) plus three minors (expect_identical test,
leaked wire marker field, stale seed doc). Full suite 1566+ green; `R CMD check --as-cran`
clean modulo the two known environmental warnings.

---

## Phase 3 — C layer

| # | Fix | Location | Notes |
|---|-----|----------|-------|
| 3.1 | Task-queue claim O(n) CAS scan → atomic `fetch_add` head cursor + fallback sweep for retried tasks; add batched `C_shard_taskq_claim_range(seg, worker_id, k)` | `src/shard_taskq.c:131-140` | O(1) amortized claims; batching amortizes the ~10–20 µs R-level per-task overhead in `worker_shm_queue_loop_`. Pad header to 64 B (cursor design mostly obviates per-task padding). |
| 3.2 | `SHARD_BACKING_AUTO` → shm (`/dev/shm` tmpfs) on Linux; keep mmap on macOS/Windows | `src/shard_shm.c:388-393` | One `#ifdef __linux__`. Removes real disk writeback for GB-scale shares. |
| 3.3 | Streaming serialize/unserialize: custom `R_outpstream_t`/`R_inpstream_t` over the mapped region (write: counting pass + direct stream; read: in-place `InBytes`) | `R/share.R:988-1004, :1073-1098`, new C entry points | Kills the 2x peak-memory copies on share **and** the per-worker per-fetch copy. Read side (~30 lines) first — it benefits every worker immediately. |
| 3.4 | Per-process attach registry: path-keyed refcounted cache consulted by `shard_segment_open`; stops 10,000 open+fstat+mmap triples for 10,000 tasks touching the same matrix | `src/shard_altrep.c:352`, `src/shard_shm.c` | Single biggest worker-side per-task overhead. |
| 3.5 | `madvise(MADV_WILLNEED)` on readonly attach; `MADV_HUGEPAGE` for segments ≥ 2 MB (Linux) | `shard_segment_open` | Cuts page-fault storm + TLB misses on first scan. |
| 3.6 | Kill O(k²) preserve-list bookkeeping: carry segment xptr/parent in `R_MakeExternalPtr` tag/prot instead of `R_PreserveObject`/`R_ReleaseObject` per view | `src/shard_altrep.c:224-225, :556-557, :905-906` | |
| 3.7 | Cache resolved base pointer in `shard_altrep_info_t` (generation counter invalidation) — stops parent-chain walk per `Elt` | `src/shard_altrep.c:99-117, :587-593` | |
| 3.8 | Optional: `No_NA`/`Is_sorted` ALTREP methods with lazily cached flags; `Sum`/`Min`/`Max` thin wrappers | `src/shard_altrep.c:688-752` | Low priority — `Dataptr_or_null`+`Get_region` already prevent materialization. |

**Verify:** taskq microbenchmark (1e5 tasks, 8 workers: claims/sec before/after); share/fetch
peak-RSS test for a large list; `R CMD check` with valgrind locally if feasible.

**Status (2026-07-04): PARTIALLY DONE on `efficiency-phase1`; local Phase 3 gate cleared.**
Branch 1 (3.1 + 3.2) and Branch 2 (3.4–3.7) are merged. Reported isolated
outcomes: 57x task-claim throughput, 6.9x end-to-end shm_queue speedup, 10,000 repeated
worker mappings reduced to registry hits, and 2.8x faster deep-view element access.
Current local validation on the merged head: focused Phase 2/3 regression files passed,
full `testthat` suite passed (1671 pass, 0 fail, 24 expected/env skips), and
`R CMD check --as-cran --no-manual` passed with 0 errors/0 notes and only the two known
environmental warnings (CRAN incoming version 0.1.1; Homebrew clang/R `Boolean.h`
warning). Remote native smoke run `28710384759` passed on both `ubuntu-latest` and
`windows-latest`, covering the compile-blind `/dev/shm`, `madvise`, taskq atomics, and
`VirtualProtect` paths. Phase 3.3 remains intentionally unstarted and should stay on its
own branch with read-side-first byte-identity stress tests.

**Fresh-context review (2026-07-04): DONE — three parallel adversarial reviewers, each
in its own worktree with runnable evidence.** Verdicts: **3.1 taskq MERGE-SAFE** (no code
defect; ThreadSanitizer + ASAN/UBSAN clean, 500-iter × 24-worker standalone harness and
30× cross-process exactly-once with injected failures — no lost/double/hang interleaving
found); **3.4/3.6/3.7 lifetime MERGE-SAFE** (no UAF/leak/double-free; ASAN registry
harness clean, drop-all-parents-then-gc×8 keeps 200-deep views correct); **3.2/3.5/backing
PASS** (HAVE_SHM_OPEN genuinely defined so the Linux tmpfs branch is live; shm_unlink
cleanup correct; madvise properly guarded). One **BLOCKER** — the `phase3-native-smoke`
CI was structurally false-green (`testthat::test_file()` sets no non-zero exit, so a real
regression stayed green; and it was the *only* CI on the working branch) — **fixed**: the
workflow now collects results and `stop()`s on any failure, and sets
`_R_CHECK_LIMIT_CORES_`. MINORs fixed: `taskq_supported()` now gates on
`ATOMIC_INT_LOCK_FREE==2` (cross-process soundness); a degenerate `(dev,ino)==(0,0)`
identity now bypasses the attach registry (closes a theoretical same-path-recreate
wrong-hit, unreachable in practice but now provably safe) with the overclaiming comment
corrected; a parameter-clobber (`n <- w$cluster[[1]]`) in the shm_queue drain loop
renamed. Deferred (documented, low risk): `-lrt` in Makevars for pre-glibc-2.34 Linux
targets (currently links via libc; CI/CRAN fine).

---

## Phase 4 — Data plane (R side)

| # | Fix | Location | Notes |
|---|-----|----------|-------|
| 4.1 | Scratch pool actually reuse: store dim-applied object in pool, return aliased (kernels overwrite via C DATAPTR — safe for scratch semantics); fix `length >= n` vs `dim<-` exact-length latent error | `R/scratch.R:83, :107-116` | Currently duplicates full buffer per call while reporting fake hits **[verified]**. |
| 4.2 | Lazy validation: hoist atomic fast path above `validate_serializable`; attempt `serialize()` and run the recursive diagnostic walk only on failure; drop pre-validation for `deep=TRUE` (traversal handles it) | `R/share.R:32-119, :870, :961` | Validation costs 500x the serialize it guards **[measured: 1.57 s vs 3 ms]**. |
| 4.3 | Gate view hotspot attribution (`sys.calls`+deparse, ~33 µs/materialization **[measured]**) behind `getOption("shard.view_hotspots", FALSE)` or dispatcher-enabled diagnostics; keep cheap counters unconditional | `R/views.R:103-129, :473-499` | |
| 4.4 | C gather/scatter buffer I/O: `C_shard_segment_scatter_write(seg, idx, values)` + `C_shard_segment_gather_read(seg, idx)`; direct typed reads (no raw+readBin double copy); accept `idx_range` in buffer `[`/`[<-` to skip the O(n) validation triple-pass | `R/buffer.R:378-404, :449-456 (the TODO), :516-529` | Sparse read currently costs two full-buffer copies **[measured: 300 µs per 2-element read on 800 KB buffer]**; scattered writes 5–10 µs/element. |
| 4.5 | Stop materializing deep-shared nodes on fetch: return ALTREP vector with attributes restored; `fetch(x, materialize = FALSE)` default, lazy fallback only if S4 `slot<-` fails | `R/share.R:741-747` | Makes deep sharing actually zero-copy. |
| 4.6 | Skip buffer init write when init is NULL/type-zero (fresh mmap/shm is already zero-filled); C-side fill for nonzero init | `R/buffer.R:140-146` | 1 GB buffer currently allocates+writes 1 GB pointlessly. |
| 4.7 | Vectorize native string encode (`nchar(type="bytes")`, single concat + `cumsum` offsets); C helper for full-column decode | `R/tables.R:623-658, :660-710` | |
| 4.8 | Table-write residuals: pass `idx_range` through to block writes; remove `data.frame -> as.list -> as.data.frame` round trip in sink write | `R/tables.R:253-257, :805-808` | |

**Verify:** buffer read/write microbenchmarks (before/after per-op cost); scratch-reuse
address-identity test; deep-share fetch returns ALTREP (no materialization counters moved).

**Execution decision (2026-07-04):** Do not flip `fetch()` laziness by default in Phase 4.
Current `fetch(shard_deep_shared)` intentionally materializes shared leaves so it returns
ordinary R objects for equality checks, S4 slot assignment, and mutation expectations.
Implement 4.5 as an explicit opt-in first, e.g. `fetch(x, materialize = TRUE)` defaulting
to current eager behavior and `fetch(x, materialize = FALSE)` returning lazy ALTREP leaves
where safe. Keep `materialize()` always eager. Revisit the default only after downstream
examples and mutation/S4/data-frame tests pass.

**Recommended Phase 4 execution order:** first land low API-risk R-side wins (4.1 scratch
reuse, 4.2 lazy validation, 4.3 hotspot gating, 4.6 zero-init buffer allocation), then split
4.4 C gather/scatter into its own branch, then handle 4.7/4.8 table cleanup. Keep 4.5
separate because it changes object identity and mutation semantics.

---

## Phase 5 — Scheduling, reduce, autotune, ergonomics polish

| # | Fix | Location | Notes |
|---|-----|----------|-------|
| 5.1 | `shard_reduce` `chunk_size = "auto"` → `max(1, ceiling(num_shards/(workers*4)))`; document/fix non-neutral `init` applied once per chunk | `R/shard_reduce.R:64, :201-209` | Two-stage reduction currently degenerates to per-shard round-trips at default. |
| 5.2 | Fix dead max-shards clamp in `autotune_block_size` (recompute after memory constraint; warn on conflict) | `R/shards.R:190-197` | |
| 5.3 | Online autotune: pre-sized result collection (kill quadratic appends), feed tuner from per-chunk completion times inside a single dispatch instead of full phase barriers | `R/shard_map.R:498-711, :592, :624` | |
| 5.4 | RSS probing: cache page size (currently shells `getconf PAGESIZE` per call), batch one `ps` call for all workers, make health checks time-based (use the currently-dead `heartbeat_interval`) | `R/rss.R:86-89, :132-142`, `R/pool.R:272` | Windows wmic path is 50–200 ms/worker/check. |
| 5.5 | Drain all ready sockets per `socketSelect` pass (currently one result per loop iteration); stop rebuilding socklist every iteration | `R/dispatch.R:197-245, :387-394` | |
| 5.6 | Recycle cost: collapse worker_spawn's 4 sequential clusterCalls into 1; spawn replacement before killing old worker (or keep one warm spare) | `R/worker.R:17-109`, `R/pool.R:294` | |
| 5.7 | Sort chunks by descending footprint when kernel footprint hints exist (straggler mitigation) | `R/shard_map.R:1201-1210` | One-liner. |
| 5.8 | Master-side `names(formals(fun))` vs borrow/out names check → immediate clear error instead of worker runtime failure | `R/shard_map.R:1384-1393` | |
| 5.9 | `getOption("shard.workers")` / `SHARD_WORKERS` override in `.default_workers()`; consider physical cores for BLAS-heavy kernels | `R/utils.R:20-28` | Package currently has zero `getOption()` usage. |

**Verify:** full suite; dispatch-loop throughput benchmark; recycle-latency measurement.

**Recommended Phase 5 execution order (2026-07-04):** 5.1 is done in Phase 2. Next take
5.2, 5.8, and 5.9 as quick correctness/ergonomics; then 5.4 RSS probing; then 5.5 socket
draining; then 5.6 recycle spawn cost. Leave 5.3 online autotune for last because it touches
scheduling behavior across whole runs. 5.7 can ride with scheduling only if footprint hints
are already under test.

---

## Phase 6 — Feature additions (each independently optional, in priority order)

| # | Feature | Rationale |
|---|---------|-----------|
| 6.1 | **Pool ergonomics + progress**: `with_pool(n, {...})`, session finalizer (no leaked PSOCK workers), stop destroy-and-recreate on pool size mismatch (lease subsets of a max-size pool), `progressr` tick per completed chunk + `on_progress=` hook | Biggest adoption blockers vs future/mirai; the dispatch loop already observes every completion, ~30 lines for progress. |
| 6.2 | **Zero-copy data.frame sharing**: per-column ALTREP into ONE segment (column offsets, not one segment per column), strings/factors via serialized side channel, `fetch()` returns ALTREP columns without materializing | Makes the headline promise true for the most common payload in the target audience's workflows. Subsumes 4.5. Longer term: ALTSTRING class (`[n][offsets][utf8 bytes]` layout) to close the character gap fully. |
| 6.3 | **`shared_mmap(path, type, dim, offset)`**: wrap an existing binary file (raw doubles, neuroimaging volumes) as an ALTREP matrix, zero load time | C layer already does ~95% of this (`shard_segment_open` + `C_shard_altrep_create`); only the R constructor is missing. High value for the fmri/neuroim2 ecosystem. |
| 6.4 | **Segment arena for many small objects**: `arena_create(size)`, sub-allocations as offset/length ALTREP views into one segment | Collapses k segments → 1 mapping; makes deep-share `min_bytes` safely lowerable by orders of magnitude. `R/arena.R` name exists; C support does not. |
| 6.5 | **Tree-shaped streaming reduction**: worker-side pairwise combining of multiple chunk partials before returning (size/time threshold) | With shared borrows, a capability future/mirai/crew genuinely don't have. |
| 6.6 | **Arrow IPC row-group format for `table_sink`** (nanoarrow, Suggests): memory-mappable zero-copy reads, native string layout (deletes hand-rolled encoder), duckdb/polars interop | Optional; behind `format = "arrow"`. |

---

## Execution protocol

- One phase per branch/commit-series; full test suite + `R CMD check --as-cran` green before
  moving on. New regression tests accompany every fix (r-testthat-generator for coverage).
- Phases 1–2 first (correctness + biggest win). Phase 3.1–3.2 next (small C changes, big
  effect). Phases 3.3–3.7, 4, 5 can proceed in parallel work streams if desired.
- Benchmarks: add a small `inst/bench/` harness (dispatch overhead, buffer ops, share/fetch
  peak RSS, taskq claims/sec) so improvements are demonstrated, not asserted, and regressions
  are catchable later.
- Fresh-context review pass (separate subagent) after each phase before merge.
- CRAN discipline: no new hard dependencies; nanoarrow (6.6) and progressr (6.1) as Suggests
  with graceful fallback.

## Known non-problems (audited, fine as-is)

- Contiguous shard descriptors: `idx = start:end` is ALTREP-compact, ~130–280 B serialized
  regardless of length. Not the problem it looks like.
- ALTREP method coverage: `Dataptr_or_null` + `Get_region` are present and correct, so
  `sum(x)` etc. do NOT materialize. Missing Sum/Min/Max are minor.
- Dispatch load balancing is already dynamic pull-based (first-free-worker), not round-robin.
- rpc_chunked master loop uses a true blocking `socketSelect` — no sleep-bound latency floor.
- Row-group sinks: single `do.call(rbind, ...)`, no incremental-rbind antipattern.
- shm_queue worker poll (0.5 ms only on failed claim) and master poll (10 ms) are acceptable.
