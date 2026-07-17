# shard 0.2.0 pre-CRAN review findings

**Date:** 2026-07-16 · **Method:** 4 parallel deep-review agents (R hot paths, native code,
R code quality, CRAN hygiene) + spot-verification of headline claims in source.
Ranked by priority. Items marked ✔ were verified directly in source or repro'd.

**STATUS (2026-07-16, post-fix):** FIXED — all of A (A1-A15), B1-B5 (B6: file kept,
deletion declined; B7: left as submission-time awareness), all of C (C1-C12), and D10
(macOS shm names now fit PSHMNAMLEN, so the "shm" backing genuinely works on macOS).
Full test suite: 0 failures / 2302 pass. `R CMD check --as-cran`: 0 errors, 0 warnings,
0 notes. Regression tests: tests/testthat/test-regressions-020.R and
tests/testthat/test-efficiency-020.R. REMAINING: the rest of section D (quality/
consistency follow-up pass — duplication consolidation, dead-code removal, error-style
stragglers, D9 taskq claim-race hardening, Rust prototype deletion).

---

## A. Must fix before CRAN — correctness / crash

### Native code (all small, localized patches)

- **A1 ✔ Unprotected fresh ALTREP SEXP across allocating calls** (GC use-after-free;
  rchk/gctorture will flag). `src/shard_altrep.c` — 5 sites: `altrep_duplicate` (~294),
  `altrep_unserialize` (~460), `altvec_extract_subset` (~660), `C_shard_altrep_create`
  (~941), `C_shard_altrep_view` (~1013). `R_new_altrep(...)` result used in
  `setAttrib`/`mkString`/`install` while unprotected. Fix: `PROTECT` immediately.
- **A2 ✔ Bounds-check integer overflow → OOB read/write from exported R API.**
  `src/shard_shm.c:795` (`shard_segment_write`) and `:1000` (`C_shard_segment_read_raw`):
  `off`/`len` cast from unvalidated `REAL(...)` to `size_t`; negative offset wraps and
  `off + len` overflow passes the check → segfault from `segment_read(seg, offset=-8, 16)`.
  Fix: validate `R_FINITE(d) && d >= 0`; use `off > seg->size || len > seg->size - off`.
- **A3 `get_info()` misidentifies foreign ALTREP objects** (`src/shard_altrep.c:97-101`):
  any ALTREP with an EXTPTRSXP data1 (arrow, lazyvec…) is treated as shard's and its
  payload cast to `shard_altrep_info_t*` → garbage deref in diagnostics/materialize.
  Fix: `R_altrep_inherits(x, shard_*_class)` before casting.
- **A4 Length/offset overflow in ALTREP creation** (`shard_altrep.c:909`, `:427`,
  `C_shard_altrep_view:975-984`): negative/huge `len` wraps `len * elem_size`; view path
  never checks `len >= 0`. Fix: `len >= 0`, `off <= seg_size`,
  `(size_t)len <= (seg_size - off) / elem_size`.
- **A5 Task queue trusts `hdr->n_tasks` from shared memory** (`src/shard_taskq.c`):
  no `taskq_required_size(n) <= segment_size` check before iterating; `taskq_init`
  memsets without checking `seg->readonly`. Fix: validate both.

### R-level bugs (several repro'd by reviewer)

- **A6 ✔ Integer overflow breaks `share()` for >2GB vectors — the headline use case.**
  `R/altrep.R:265`: `size <- length(x) * elem_size` is int×int; ≥269M doubles → NA.
  Fix: `as.double(length(x)) * elem_size`.
- **A7 Stale/wrong out-handle in `shard_reduce` after `shard_map` on same pool** (repro'd).
  `R/shard_reduce.R:331-375` is a drifted third copy of out-open logic sharing the
  `.shard_out_opened` cache with shard_map's executor (`R/shard_map.R:1599`) without key
  invalidation → reducer receives shard_map's `list(key=,obj=)` wrapper. Fix: delete the
  copy, call `open_out_from_desc_()` (`R/dispatch_shm_queue.R:8-116`).
- **A8 ✔ `pid_is_alive()` fallback always TRUE** (`R/worker.R:403-408`): `tools::pskill`
  returns FALSE without erroring for dead PIDs. Fix: `isTRUE(tools::pskill(pid, 0L))`.
- **A9 Silent data loss on NA `shard_id` in table sinks** (`R/tables.R:808-812, 554`):
  writes `part-NA.rds`, which `table_finalize()`'s `^part-[0-9]+\.rds$` glob (:894)
  silently drops. Fix: validate positive non-NA integer.
- **A10 `row_layout()` hard-errors on trailing zero-row shard** (repro'd)
  (`R/tables.R:204-206`): `layout[[i]] <- NULL` shortens the list; `names<-` fails.
  Fix: `next`.
- **A11 `shard_crossprod()` fails on narrow matrices with auto-tiles** (repro'd)
  (`R/kernels.R:112-125`): `ncol < 8` → empty candidates → `max(integer(0))` = -Inf.
  Clamp candidates to `min(p, ...)`.
- **A12 `shards_list()` reports shard count as item count** (`R/shards.R:135`).

### Documented arguments that silently do nothing

- **A13 `shard_map(health_check_interval=)` ignored** — `R/shard_map.R:195` overwrites it
  with `profile_settings$...`; `shard_reduce` honors it.
- **A14 `pool_create(heartbeat_interval=)`** stored, never read (`R/pool.R:22,41,94`).
- **A15 `share(name=)`** honored only on the non-deep serialize path (`R/share.R:1036`);
  atomic fast path and `deep=TRUE` drop it despite docs.

## B. Submission mechanics (fix before building the tarball)

- **B1 ✔ `planning/` leaks into the tarball** — `.Rbuildignore` has `^\.planning$` only.
  Add `^planning$`. (This very file makes it mandatory.)
- **B2 `cran-comments.md` stale** — still "new release" + 0.1.x resubmission notes.
  Rewrite as a 0.2.0 update submission. Include a note on use of
  `getFromNamespace("sendCall"/"recvResult", "parallel")` (see D6).
- **B3 `NEWS.md` excluded from tarball** — `.Rbuildignore` line 35 `^NEWS\.md$`. Remove;
  CRAN renders NEWS.md and `utils::news()` needs it.
- **B4 NEWS.md missing a 0.1.1 entry** (CRAN-SUBMISSION shows 0.1.1 was last submitted).
- **B5 Broken regex `^\\.\\.Rcheck$`** in .Rbuildignore line 9 (double-escaped). Fix or drop
  (auto-exclusion of `*.Rcheck` covers it).
- **B6 Stale `CRAN-SUBMISSION`** (0.1.1, 2026-04-05) — delete or update consciously.
- **B7 Example runtime risk**: 40 man pages' `\donttest` examples each cold-start a
  2-worker PSOCK pool; CRAN incoming runs `--run-donttest` → possible >5s NOTEs.
  Consider trimming/sharing pool startup in examples.

## C. Efficiency (R hot paths; ranked by impact)

- **C1 ✔ O(n²) queue management in dispatch loop** (`R/queue.R:54-55, 109/113`):
  `pending[-1]` copies remaining list per dequeue; named `completed[[id]] <-` grows
  per-insert. Fix: integer cursor over fixed `pending` + preallocated
  `vector("list", total)` indexed by integer chunk id.
- **C2 Per-iteration `ps` handle churn probing worker liveness**
  (`R/dispatch.R:128-144` @ :296; `R/worker.R:393-409`; also
  `R/dispatch_shm_queue.R:401-404` every 10ms/worker): fresh `requireNamespace` +
  `ps::ps_handle(pid)` per check per worker per loop tick. Fix: cache namespace flag
  package-level; cache `ps_handle` in the `shard_worker` object at spawn (also more
  correct re PID reuse); cache socket list, invalidate on restart/recycle.
- **C3 Double copy on every contiguous buffer read** (`R/buffer.R:520-538`):
  `segment_read` → raw, then `readBin` → typed (logical: 3rd pass). Backs `buf[]` /
  `as.vector(buf)` after runs (GBs, 2× peak) and worker-side slice reads. Fix: typed
  contiguous range read in C (range sibling of `C_shard_segment_gather_read`).
- **C4 Autotuner grows lists with `c()` per phase** (`R/shard_map.R:672, :705`) — O(n×P)
  recopying under the *default* online autotune. Fix: per-phase containers + one concat.
- **C5 `object.size()` per partial even with `diagnostics=FALSE`**
  (`R/shard_reduce.R:213`). Gate on the flag.
- **C6 Per-task buffer index write temporaries** (`R/buffer.R:407-434`): bounds logicals +
  `all(diff(idx)==1L)` materializes ALTREP-compact ranges twice. Fix: allocation-free
  contiguity test (`idx[n]-idx[1]+1==n && !is.unsorted(idx, strictly=TRUE)`); fold bounds
  check into C gather/scatter.
- **C7 ≥3-D buffer indexing does whole-buffer read-modify-write** (`R/buffer.R:681-685,
  782-788`) — O(total) per access and silently breaks the lock-free disjoint-write
  guarantee. Decompose into leading-dim runs, or document loudly.
- **C8 Completed chunks retain full shard descriptors + RNG streams** (`R/queue.R:106-109`,
  `retain=TRUE` default via shard_map) — second copy of the decomposition held to run end.
  Make minimal retention the default; keep full chunks only for `queue_failures()`.
- **C9 `C_shard_segment_gather_read`/`scatter_write` validate every index twice + per-element
  type switch** (`src/shard_shm.c:1011-1092`). Hoist dispatch, validate once (~2× on the
  per-element path).
- **C10 `stream_map()` grows output per partition** (`R/table_stream.R:83-90`) — minor.
- **C11 Borrow auto-share copies per `shard_map` call** (`R/shard_map.R:964-989`) — for
  iterative workflows document "pre-`share()` your inputs" prominently (or cache by identity).
- **C12 Makevars: add `PKG_CPPFLAGS += -DUSE_FC_LEN_T`** (both platforms) — `F77_CALL(dgemm)`
  with FCONE per WRE §6.6.1.

## D. Quality / consistency (follow-up pass, tests as guard)

- **D1 Consolidate out-descriptor open/close/cache** (3 copies: shard_map.R:1484-1558,
  dispatch_shm_queue.R:8-116, shard_reduce.R:331-375 — the drifted one is A7).
- **D2 Consolidate diagnostics-delta accumulation** (dispatch.R:357-396 vs
  dispatch_shm_queue.R:462-482; initializers ×3) ~80 lines. Also failure/requeue block ×3
  in dispatch.R; export_borrow/out/table_sink triplets (shard_map.R:1159-1301);
  tables.R duplications (atomic-save ×2, factor-decode ×4, schema-error ×2);
  element-size switch ×4 (use views.R:28 `elem_size_bytes`).
- **D3 Dead code removal**: unreachable S4 branch in `share_deep_traverse`
  (share.R:711-751, ~40 lines) + consequent dead `"s4"` reconstruct branch (:803-808);
  unused C entry points `C_shard_mat_crossprod_gather`, `C_shard_segment_addr`;
  test-only helpers in utils.R/rss.R/scratch.R/results_placeholder.R; assorted dead
  branches (shard_map.R:371-373, :593, :1608; dispatch.R:74, :241;
  dispatch_shm_queue.R:333-408 idle/inflight, :346-352 double re-export;
  buffer.R:167-174; shards.R:124, :200; altrep.R:44; share.R:296; `.pool_env$dev_path`).
- **D4 Inert diagnostics surfaces**: `cow_report()` reads never-written `cow_stats`;
  `copy_stats$borrow_exports/borrow_bytes` never incremented; `segment_report()`
  documents always-empty fields. Populate or re-document.
- **D5 API drift**: shard_reduce skips `validate_fun_bindings()`, doesn't reset worker
  diagnostics counters, doesn't clear auto-table-sink manifest; exported
  `as_tibble()`/`collect()` generics shadow tibble/dplyr without default methods —
  re-export tibble's generic instead; sibling NULL-handling drift
  (recommendations vs task_report/cow_report); `pool_create()` docs omit
  `shard.workers`/`SHARD_WORKERS`.
- **D6 `getFromNamespace("sendCall"/"recvResult","parallel")`** ×4 sites — centralize in
  one accessor; disclose in cran-comments; have a fallback story.
- **D7 Error-handling**: `call.=FALSE` stragglers (altrep.R 76,107,145,181,217,246,255;
  buffer.R 33,44,153,237,890; segment.R:50; warnings dispatch.R:280,317,350,
  shard_map.R:1182,1259,1295); `arena()` strips condition class (arena.R:97-103);
  worker.R:336-338 double-wraps timeout errors; consider classed conditions for
  permanent chunk failure before freezing the API.
- **D8 Fragile patterns**: RNG hygiene — `.autotune_crossprod_tiles` (kernels.R:134-135)
  and `object_identity()` fallback (share.R:149) consume `.Random.seed` without restore;
  `exists(".shard_borrow", globalenv())` without `inherits=FALSE` ×6;
  dispatch.R:398 `payload$scratch_needs_recycle` unguarded for non-list payload;
  `share()` leaks segments on error paths (share.R:1039-1044, deep path no on.exit unwind).
- **D9 Taskq claim race (low)**: CAS PENDING→CLAIMED then store `claimed_by` — death
  between the two strands the task until R-level timeout; `mark_done` double-increment
  can end dispatch early. Pack state+worker in one atomic or reclaim claimed_by==0.
- **D10 macOS "shm" backing always fails** (name > PSHMNAMLEN=31,
  `src/shard_shm.c:159-166, 1185-1207`) yet is advertised by
  `C_shard_available_backings`. Shorten the generated name or stop advertising.
- **D11 Housekeeping**: delete vestigial Rust prototype (src/*.rs, Cargo.*, vendor/ 1.8MB —
  Rbuildignored, no CRAN impact, but dead in-repo); `readonly` create param dead (L5);
  NaN passes `C_shard_segment_create` size validation (add R_FINITE).

## Verified clean (for the record)

- Dispatch design: kernel/diagnostics exported once per dispatch, strided shards
  wire-compacted, worker-side caches keyed, shm_queue per-task body minimal, taskq
  memory ordering sound (seq_cst, lock-free verified).
- CRAN: core caps (2) everywhere incl. `_R_CHECK_LIMIT_CORES_`; 35 test files use
  skip_on_cran with all >2-worker sites guarded; no library()/options() leaks; docs
  complete (107/107 exports aliased, all usage Rd have \value, zero \dontrun);
  tarball exclusions otherwise correct; no non-API C entry points; full .Call
  registration; Windows paths complete; Unix segment error paths leak-free.
- No sapply, no `:::`, on.exit stacking correct, RNG seed-stream code exemplary.
