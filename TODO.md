# Active TODO

Last condensed: 2026-03-26

This file is now the sole live backlog.
All still-unimplemented items were migrated here from the removed backlog and
roadmap trackers under `docs/plans/`.

Current actionable count: 0

## Deduce Scan-Path Allocation Guard

- [x] `src/lisp/deduce_relation_scan_helpers_more.c3`: guard `make_hashmap(...)` in `deduce_relation_materialize_row_dict(...)` so scan-range row materialization fails cleanly on OOM instead of dereferencing a null hashmap

## Scheduler Offload Reuse Regression

- [x] `src/lisp/scheduler_offload_worker.c3` / `src/lisp/tests_scheduler_boundary_worker.c3`: add a targeted regression for `QueuedOffloadWork` recycle/reuse correctness so recycled nodes are reset and free-list accounting stays bounded across a reuse cycle

## Repo Audit Follow-up Plan

- Harness-only teardown regression lane
  - [x] Confirm the harness-only teardown regression lane in `src/lisp/tests_tests.c3` stays green under `OMNI_LISP_TEARDOWN_REGRESSION`
- Method-table overwrite seam
  - [x] Fix method-table allocation/error propagation in `src/lisp/jit_jit_define_method_table.c3` (`jit_new_method_table_with_sig`, `jit_method_table_append_entry_with_sig`) so a failed redefine cannot return success semantics
  - [x] Validate stdlib-loaded redefine/replacement semantics and teardown with the focused regression in `src/lisp/tests_runtime_feature_jit_groups_more.c3`
- Environment hash-table OOM handling
  - [x] `src/lisp/value_environment.c3`: preserve the old hash table until `Env.build_hash_table()` has allocated and initialized the replacement
  - [x] `src/lisp/value_environment.c3`: guard the `Env.define()` growth allocation before copying bindings into the new buffer
  - [x] `src/lisp/value_environment.c3`: add a targeted regression for env-table rebuild/growth under allocation failure
- Interpreter init allocation failures
  - [x] `src/lisp/value_interp_init_helpers.c3`: check module-table allocation before the index fill loop
  - [x] `src/lisp/value_interp_init_helpers.c3`: check macro-table allocation before the index fill loop
  - [x] `src/lisp/value_interp_init_helpers.c3`: propagate init failure so `Interp.init()` can abort cleanly instead of dereferencing null
- Scheduler offload queue allocation failure
  - [x] `src/lisp/scheduler_offload_worker.c3`: guard `scheduler_offload_take_queued_work()` when `mem::malloc` returns null
  - [x] `src/lisp/scheduler_offload_worker.c3`: preserve slot-release and recycle behavior on queued-work allocation failure
  - [x] `src/lisp/tests_scheduler_*`: add a targeted offload regression for queued-work allocation failure and fallback accounting
- Unknown filtered test-group dispatch
  - [x] `src/lisp/tests_deduce_groups.c3`: fail or warn when `OMNI_DEDUCE_GROUP_FILTER` matches no deduce group
  - [x] `src/lisp/tests_tests.c3`: fail or warn when `OMNI_ADVANCED_GROUP_FILTER` matches no advanced group
  - [x] `src/lisp/tests_tests.c3`: add a targeted regression proving a mistyped filter does not exit green with zero executed tests


## Deduce Execution Queue

- [x] B6.3f1b Runtime persistence for the chosen rule/dependency catalog contract
- [x] B6.3f2 Durable derived freshness and ready semantics after catalog persistence
- [x] B6.4d Wider captured-call rewrite for `deduce/query` demand extraction
- [x] B6.4e1 Disjunctive union execution for demand-safe `deduce/query` branches
- [x] B6.4e2a Same-position disjunctive wrapper execution on the ephemeral query path
- [x] B6.4e2b1 Same-position disjunctive residual-conjunct execution on the ephemeral query path
- [x] B6.4e2b2 Mixed-position disjunctive branch union on the ephemeral query path
- [x] B6.4f1 Truthful fallback for recursive multi-position symbolic query demands
- [x] B6.4f2 Bounded recursive symbolic query support via single applied-position relaxation
- [x] B6.4f3 Preserve the recursive carried position across reordered multi-position query demands
- [x] B6.4f4a Truthful fallback for jointly-supported recursive permutation query demands
- [x] B6.4f4b Preserve jointly-supported same-index multi-position recursive query demands
- [x] B6.4f5a Truthful fallback when demanded positions require multiple recursive atoms
- [x] B6.4f5b Same-index mutual-recursive multi-position query support across SCC-local recursive body atoms
- [x] B6.4f5c1 Same-index mutual-recursive disjunctive multi-position query support
- [x] B6.4f5c2a Multi-hop same-index SCC multi-position query support
- [x] B6.4f5c2b1 Single-carried-position relaxation for transformed recursive SCC query demands
- [x] B6.4f5c2b2a Disjunctive single-carried-position relaxation for transformed recursive SCC query demands
- [x] B6.4f5c2b2b1 Multi-hop transformed SCC one-carried-position query support
- [x] B6.4f5c2b2b2a Truthful fallback for transformed multi-atom recursive query demands
- [x] B6.4f5c2b2b2b Retire stale transformed recursive-demand umbrella after support/fallback closure
- [x] B6.6a1 Conjunctive explain field classification closure
- [x] B6.6a2 Conjunctive analyze field classification closure
- [x] B6.6b1 Conjunctive runtime path/order truth tightening
- [x] B6.6b2 Conjunctive runtime counter truth tightening
- [x] B6.7a1 Commit/abort mutation-log contract
- [x] B6.7a2 Degraded-state and recovery contract
- [x] B6.7b1 Dirty-frontier truth contract
- [x] B6.7b2 Stats/analyze/refresh admin-truth alignment beyond tracked recompute
- [x] B6.8a1 Incremental delta substrate data model
- [x] B6.8a2 Fallback and admin boundary for incremental maintenance
- [x] B6.8b1 First extensional-source maintained-update class
- [x] B6.8b2 First derived-predicate maintained-update class
- [x] B6.9a1 Why-result payload/status baseline
- [x] B6.9a2 Public why-result seed-row surface
- [x] B6.9a3 First non-recursive derived why-result lineage
- [x] B6.9a4 One-body extensional derived why-result lineage
- [x] B6.9a5 Reconstructible extensional multi-body why-result lineage
- [x] B6.9a6 Exact-one-rule extensional search-based why-result lineage
- [x] B6.9a7 Exact-one-rule mixed-body non-recursive why-result lineage
- [x] B6.9a8 Multi-rule non-recursive derived why-result lineage
- [x] B6.9b1a First positive recursive why-result closure lineage
- [x] B6.9b1b Recursive why-result rule-step support beyond flattened fact-only closure payloads
- [x] B6.9b2a Optional why-result goal-directed read context metadata
- [x] B6.9b2b1 Exact-one goal-directed query context on matching why-result paths
- [x] B6.9b2b2a Exact-one goal-directed match context on matching why-result paths
- [x] B6.9b2b2b1 Exact-one goal-directed scan-range context on matching why-result paths
- [x] B6.9b2b2b2a Selector-scoped parity for exact-one row-read goal-directed path context
- [x] B6.9b2b2b2b1 Exact-one goal-directed scan context on matching why-result paths
- [x] B6.9b2b2b2b2a Bounded complete row-set goal-directed path context on matching why-result paths
- [x] B6.9b2b2b2b2b1 Selector-scoped parity for bounded-complete `scan` path-local goal-directed context
- [x] B6.9b2b2b2b2b2a Bounded proof-path goal-directed context for plain no-op `query` / `scan-range` reads and matching support frames
- [x] B6.9b2b2b2b2b2b1 Bounded root-path goal-directed context for plain no-op `match` reads
- [x] B6.9b2b2b2b2b2b2a Truthful selector-scoped path-local parity across shipped no-op and ephemeral row-read shapes
- [x] B6.9b2b2b2b2b2b2b1 Bounded proof-path goal-directed context for matching fact support frames
- [x] B6.9b2b2b2b2b2b2b2a Unique path-level goal-directed context propagation from matching support frames
- [x] B6.9b2b2b2b2b2b2b2b1 Common path-level goal-directed context propagation across same-relation support frames
- [x] B6.9b2b2b2b2b2b2b2b2 Path-level mixed-context goal-directed provenance lists across support-frame relations
- [x] B6.10a1 Next integrity-class selection
- [x] B6.10a2 Schema/admin payload baseline for `check` constraints
- [x] B6.10b1 Assert/write enforcement for `check` constraints
- [x] B6.10b2 Dedicated admin counters and summary truth for `check` constraints
- [x] B6.11a1 Scheduling policy contract for parallel recursion
- [x] B6.11a2 Fallback and admin-truth contract for parallel recursion
- [x] B6.11b1a Serialized component-delta payload seam for worker-scratch recursive batches
- [x] B6.11b1b1 Single-pass worker-scratch recursive delta compute over the current component snapshot
- [x] B6.11b1b2a Multi-iteration worker-scratch recursive closure for single-recursive-atom positive SCC rules
- [x] B6.11b1b2b Broader worker-scratch recursive closure beyond single-recursive-atom rule shapes
- [x] B6.11b1c Main-thread publish/apply path for worker-computed recursive deltas
- [x] B6.11b2 Runtime/admin truth for parallel execution

## Legacy Runtime and Validation Follow-up

- [x] Run a Docker-capped profiling pass for boundary-heavy workloads and record the baseline counters/trace summary.
- [x] Capture scope-chain scan pressure and hint-hit/miss ratios from the current boundary telemetry surface.
- [x] Identify which return-path outcomes dominate in practice (`reused`, destination-built, direct-promoted, spliced, disallowed).
- [x] Record the accepted regression envelope for boundary-heavy workloads in docs or scripts.
- [x] Add focused micro-bench coverage for method dispatch and typed lambda call boundaries before and after the allocation changes.
- [x] Audit `src/lisp/eval_pattern_support.c3` deep equality workspace allocation (`stack`/`seen`) under nested list/array comparisons.
- [x] Introduce a bounded scratch or inline-first workspace strategy that preserves cycle safety and current semantics.
- [x] Add a regression/benchmark slice for large nested equality comparisons so allocator churn is measurable.
- [x] Add micro-bench coverage for scheduler + async I/O/offload interaction hotspots (`queue`, completion publish, TLS/http offload).
- [x] Audit per-request heap allocation in `src/lisp/scheduler_offload_worker.c3` and classify reusable worker-local buffers versus required owned outputs.
- [x] Prototype a narrow pool/reuse strategy for offload request/completion scaffolding without changing boundary ownership semantics.
- [x] Validate that any pooling change preserves generation/task handoff correctness and offload failure cleanup.
- [x] Add benchmarks for `deduce` scan/query/count paths at corpus sizes large enough to expose regression envelopes.
- [x] Measure row materialization cost in `relation_scan_range(...)`, including per-row hashmap creation and per-column symbol-key allocation.
- [x] Reduce avoidable scan-path allocation where semantics allow (for example cached relation key values or other stable row-shape helpers).
- [x] Split the dispatch/match diagnostics formatting helpers out of `src/lisp/eval_dispatch_types.c3` into `src/lisp/eval_dispatch_match_errors.c3`.
- [x] Evaluate whether `deduce-query` should stay as full-scan + callback filtering or gain a narrower optimization path for common predicates.
- [x] Extract `deduce_why_result_*` explainability helpers and `prim_deduce_why_result` from `src/lisp/deduce_schema_query.c3` into `src/lisp/deduce_why_result.c3`.
- [x] Prioritize split candidates by size and hot-path relevance (`schema.c3`, `eval_dispatch_types.c3`, `scheduler_offload_worker.c3`).
- [x] Investigate/fix bounded `OMNI_LISP_TEST_SLICE=deduce` crash/hang on the worker-scratch recursive-delta lane (current traced run stalls before worker-scratch markers), then re-check `deduce parallel worker-scratch component pass computes serialized recursive deltas`.
- [x] Run targeted validation for the `deduce_why_result` split after the deduce worker-scratch crash/blocker is resolved.
- [x] Rename legacy names so they are visible and intentional (`memory-soak`/`syntax` aliases if still used).
- [x] Investigate/fix the remaining unfiltered `OMNI_LISP_TEST_SLICE=deduce` crash/hang starting at `deduce rule-derived reference heads validate against final recursive component snapshot`, then isolate it with a narrow validation slice.
- [x] Document the minimum required container path for boundary/lifetime changes versus allocator-benchmark work.
- [x] Stop implying that memory ownership tests are required for syntax/compiler-only changes in contributor docs.
- [x] Record the slice split and rationale in `memory/CHANGELOG.md` once the code move lands.
- [x] Add a short contributor rule: test lanes must follow ownership, not convenience bundling.
## Leak cleanup follow-up plan

- [ ] Confirm the harness-only teardown regression lane in `src/lisp/tests_tests.c3` stays green under `OMNI_LISP_TEARDOWN_REGRESSION`.
- [ ] Confirm or fix the method-table overwrite seam in `src/lisp/jit_jit_define_method_table.c3` (`jit_eval_define_typed_callable`, `jit_eval_define`) with a focused regression for redefine/replacement semantics.
