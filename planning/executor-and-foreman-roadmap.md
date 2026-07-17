# shard executor + `foreman` roadmap

**Status:** planning / not started · **Date:** 2026-07-05 · **shard:** v0.2.0 (mid-CRAN-submission)

## TL;DR decision

Build the explicit, lexically-owned parallel **executor inside `shard`** (Option A) — *not*
as a separate package yet. Two adversarial design reviews (Codex council, 2 deep rounds)
concluded, high confidence: after the scope cuts, a separate package would be a wrapper
around shard's own API, not a new abstraction. The separate package (**`foreman`**, name
reserved) is created **only when a genuine second engine — `mirai` — forces a real
cross-engine contract.** Package extraction in R is cheap; premature packaging is not.

The real v0.1 deliverable is **not a package** — it is:
1. explicit **`pool=` ownership** threaded through shard (kills the "ambient pool" contradiction), proven by a concurrency test matrix;
2. a lexical **`shard_executor()`** object that owns a pool + `close()`;
3. a **benchmark gate** where `shard_executor()` must beat *raw shard* (+ future.apply / foreach / serial) on memory-honest metrics.

---

## Why (condensed from two council rounds)

The origin is a design brainstorm for an engine-agnostic R compute runtime
("executionr"), captured in `~/code/executionr/docs/design/{vision,architecture,shard-integration}.md`.
The durable thesis that survived review:

> Parallelism is an **explicit capability object you hold** (like a DB connection), not a
> global `future::plan()`. Large data moves **by reference**. Failures are **typed
> first-class results**, not broken socket reads.

**Round 1** cut the grand runtime to "shard-first": the auto-planner is cosmetic for
arbitrary closures, a unified cross-placement DataRef leaks, static contract checks
over-reach R's semantics, and — the blocking finding — the "no ambient state" thesis is
contradicted by shard's package-level current pool.

**Round 2** (reviewing the revision, knowing shard is in-house) reversed the packaging
plan to **Option A** with these decisive points:

- After the cuts, `foreman` v0.1 ≈ `shard + pool fix + lexical wrapper`. Raw shard already
  exports the entire proposed surface (`share`/`buffer`/`table_sink`/`shard_map`/
  `shard_reduce`/diagnostics/pool lifecycle). A package boundary adds release
  choreography, not value.
- **`sequential` is not a second engine** — it's a local test oracle. It exercises no
  transport/serialization/lifecycle/shared-memory/retry. So "extract from two engines"
  genuinely needs `mirai`; you cannot validate engine-agnosticism with shard+sequential.
- **In-house ownership does not dissolve the pool blocker** — it converts it into a
  CRAN-sensitive refactor (see scope below). The pool is deeply ambient today.
- **The wedge belongs to raw shard today.** foreman is value-additive only at engine #2,
  or if benchmarks prove a wrapper-specific ergonomic/diagnostic win over raw shard.
- **Co-evolution trap:** tuning shard to fit an executor *now* hardens shard concepts
  (buffers, sinks, refs, pool stats, COW) into the eventual cross-engine surface — whether
  or not that interface is "private." Guard against this until mirai exists.

Council jobs (for the record): `council-20260705-153441-677a3ad0` (round 1),
`council-20260705-162414-fa3555f0` (round 2).

---

## Revised vision

Do **not** sell a general runtime. **Win one workload class decisively**, then earn the
abstraction from working engines:

- **Target workload:** big shared read-only input · many independent slices · predeclared
  output shape. I.e. bootstrap, feature-screening, simulation, GLM/lm sweeps.
- **Win on memory-honest metrics**, not just speed: peak RSS, bytes crossing the worker
  connection, reproducibility under retry, failure legibility.
- **Extract the interface from two *real* engines** (shard + mirai), never design it up
  front. mirai is the forcing function that proves the capability model is honest.

---

## v0.1 scope (all inside `shard`)

### Deliverable A — explicit `pool=` ownership (the real first milestone)

**Problem (grounded):** shard resolves the worker pool from ambient package state. Call
sites using the `pool_get()` fallback today (from `grep`, 2026-07-05):

- `R/shard_map.R` (`pool <- pool_get()` at ~L181, L916, L936; `pool_get()$stats` at L834)
- `R/shard_reduce.R` (L140)
- `R/shards.R` (L55), `R/diagnostics.R` (L74, L263), `R/affinity.R` (L56)
- `R/dispatch.R` (L53, L187, L432) and `R/dispatch_shm_queue.R` (L532)
- Already parameterized (`pool =`, default `pool_get()`): `dispatch_chunks`, `pool_lapply`,
  `pool_sapply`.

**Task:** thread an explicit `pool =` argument through the public entry points
(`shard_map()`, `shard_reduce()`, and the diagnostics/report paths), **defaulting to
`pool_get()` for 100% back-compat**. The internal contract becomes: *if a pool is passed,
use exactly that pool for dispatch, stats, and diagnostics — never fall back to the
current pool.* Requires a first-task audit of `R/pool.R` to nail `pool_create()`
replace/stop semantics and whether `pool_get()` creates-on-demand (this determines how
invasive the change is; the council predicts this is the **first place the plan breaks**).

**Acceptance test matrix (the falsifier for "no ambient state"):**

- [ ] two simultaneous pools with different `workers` / `mem_cap` / `packages` run
      interleaved `shard_map`/`shard_reduce` without cross-contamination
- [ ] `pool_stop()` on pool A does not disturb pool B or the current pool
- [ ] `diagnostics()` / reports reflect the **passed** pool, not global state
- [ ] RNG streams + results are identical whether run via explicit pool or current pool
- [ ] existing current-pool tests still pass unchanged (back-compat)
- [ ] CRAN check clean; change is additive (no removed/renamed exports)

### Deliverable B — `shard_executor()` lexical object

A thin object that **owns** a pool created via `pool_create(...)` and exposes:
`put` · `buffer` · `table_sink` · `map_shards` · `reduce_shards` · `diagnostics` ·
`close`. `close()` calls `pool_stop()` on *its* pool. No global side effects; several
executors coexist. Keep the method set minimal and shard-native — do **not** invent an
"Engine interface" abstraction here (see cuts).

Also provide `executor_sequential()` **internal-only** as a test oracle (same method
names/result shapes) — not exported until sequential's public semantics (buffers, sinks,
diagnostics, cleanup, failure shape) are explicitly defined for both backends.

### Deliverable C — benchmark gate vs **raw shard**

The release gate. `shard_executor()` must justify itself against the thing it wraps.

- **Workloads:** (1) column-wise feature screening over a huge matrix; (2) bootstrap /
  simulation with fixed-size numeric output; (3) variable-row model summaries to
  `table_sink()`.
- **Comparators:** raw `shard` (primary), `future.apply` multisession, `foreach`+
  `doParallel`, serial base R.
- **Metrics + explicit pass/fail thresholds:** peak RSS · end RSS · bytes crossing the
  worker connection · time-to-first-result · reproducibility under retry · failure
  legibility. **Rule: if `shard_executor()` does not beat raw shard on ergonomics or
  failure legibility at no material overhead, it has no reason to ship.**

---

## Explicitly CUT / deferred (do NOT build prematurely)

These were in the original brainstorm and were cut by review. Guard against rebuilding
them before there is a concrete, engine-#2-driven reason:

- **Auto-planner / capability router** — cosmetic for arbitrary closures; only meaningful
  with declared workloads *and* ≥2 engines with different capabilities.
- **Unified cross-placement `DataRef`** (ALTREP / Arrow / DuckDB / object-store) — leaks at
  lifecycle/ownership/credentials/transfer. Keep only a local-shard ref for now.
- **Static parallel-safety Contracts** — reframe as advisory lint + runtime enforcement
  (shard COW / serialization / resource caps); no static proof over arbitrary R closures.
- **Actors, query-lowering, remote engines (Slurm/K8s)** — later experiments, each admitted
  only when a real workload demands it.
- **A public engine-agnostic interface** — stays an internal implementation detail until
  mirai forces extraction.

---

## The `foreman` package (reserved)

- **Name:** `foreman` — a held object that owns/supervises a crew of workers. CRAN-checked
  free 2026-07-05 (`harness` and `crew` are taken). Reserved, not yet created.
- **Activation trigger:** a `mirai` engine spike that cannot be expressed through the
  shard-shaped executor — i.e., the moment a second *real* engine forces a non-shard-shaped
  shared contract. **Only then** extract the executor + a genuine `Engine` interface out of
  shard into `foreman`, with shard as engine #1 and mirai as engine #2.
- **What foreman will own (later):** the engine-agnostic executor, the capability model,
  the `.executor` contract other packages accept, cross-engine failure normalization,
  structured concurrency. shard stays a pure engine and gains engine-facing hooks
  (`can_shard`, `estimate_memory`, `choose_block_size`, typed failures, ref manifests,
  replay journals) as they're proven needed.

---

## Risks / traps to watch

1. **Pool retrofit is the predicted first break.** `R/pool.R` lifecycle is ambient
   (create replaces/stops current; many paths call `pool_get()` internally). Budget for a
   real refactor, not a signature tweak. Land it *before* CRAN 0.2.0 churn settles, or
   after, but as a clearly additive minor version.
2. **CRAN back-compat.** shard is mid-submission. All executor/pool changes must be
   additive; keep `pool_get()` defaults and existing test behavior intact.
3. **Co-evolution trap.** Every shard concept you bend to fit the executor risks becoming
   the de-facto cross-engine surface. Until mirai exists, prefer shard-native shapes and
   resist generalizing.
4. **sequential ≠ engine.** Don't let a passing sequential+shard test suite masquerade as
   proof of engine-agnosticism.

---

## Task checklist (sequenced)

- [ ] **T0** Audit `R/pool.R`: document `pool_create` replace/stop semantics, `pool_get`
      create-on-demand behavior, and every internal `pool_get()` resolution. Output: a
      short scoping note appended here.
- [ ] **T1** Add `pool =` to `shard_map()` / `shard_reduce()` (+ diagnostics/report paths),
      default `pool_get()`; passed pool is authoritative end-to-end.
- [ ] **T2** Implement the Deliverable-A acceptance test matrix; make it green.
- [ ] **T3** Implement `shard_executor()` (owns pool + `close()`); internal
      `executor_sequential()` oracle.
- [ ] **T4** Build the three benchmark workloads + comparators + thresholds (Deliverable C).
- [ ] **T5** Decide go/no-ship on `shard_executor()` from T4 results (must beat raw shard).
- [ ] **T6** (gated) `mirai` engine spike → if it breaks the shard-shaped executor,
      extract `foreman` with a real Engine interface.

---

## Pointers

- Source-of-thinking design docs: `~/code/executionr/docs/design/{vision,architecture,shard-integration}.md`
  (pre-Option-A; to be revised/migrated to reflect this decision).
- shard verified API surface: `shard-integration.md` appendix (v0.2.0, ~110 exports).
- This plan supersedes the "separate `foreman` package from day 1" (Option B) direction.
