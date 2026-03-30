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

## UI Library Queue

Library-only queue. This section is about the high-level `ui` facade and its
submodules, not raw FTXUI parity work and not unrelated language/runtime work.

100% coverage target:

- complete the public `ui` facade surface called out in
  `docs/plans/ui-library-facade-plan-2026-03-27.md`,
- keep backend ABI completion in
  `docs/plans/ftxui-c-abi-shim.md`,
- do not leave partially supported facade features implicit.

- [x] Surface split: move the current single-file scaffold into a real facade + helper-module layout (`ui` facade plus internal node/effect helper modules) while preserving the canonical public `ui.*` surface
- [x] Facade module ownership: enable direct dotted library submodule imports so the helper-module split can become true `ui.nodes` / `ui.effects` / `ui.layout` / `ui.style` / `ui.runtime` / `ui.ftxui` ownership instead of file-backed flat internal helpers
- [x] Layout surface: add `ui.layout` and move layout-only helpers there (`hbox`, `vbox`, plus first-class `stack` / `spacer` if adopted), with `ui` re-exporting the canonical names
- [x] Style surface: add the first high-level `ui.style` layer with backend-agnostic `border`, `frame`, `flex`, `width`, and `height` helpers so graph/window demos can be shaped from the public library
- [x] Effect grammar: define `ui.effects` as tree-shaped declarative effect nodes with symbol tags, attribute maps, and child effect nodes, using ordinary Omni constructors first and treating macros as optional sugar over the same tree data
- [x] Runtime dispatcher: make the first real effect-dispatch path (`ui.runtime`) practical on top of the live FTXUI backend so normalized effect trees are interpreted through Omni's existing signal machinery rather than called imperatively
- [x] Backend extraction: move the current FTXUI lowering path behind a dedicated `ui.ftxui` backend module so `ui.run` delegates through the backend instead of owning concrete backend logic directly
- [x] Library validation: add focused examples and regressions for the library surface itself under `examples/libraries/ftxui/`, covering facade imports, style/layout helpers, and effect-driven usage

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
- [x] Rename old names so they are visible and intentional (`memory-soak`/`syntax` aliases if still used).
- [x] Investigate/fix the remaining unfiltered `OMNI_LISP_TEST_SLICE=deduce` crash/hang starting at `deduce rule-derived reference heads validate against final recursive component snapshot`, then isolate it with a narrow validation slice.
- [x] Document the minimum required container path for boundary/lifetime changes versus allocator-benchmark work.
- [x] Stop implying that memory ownership tests are required for syntax/compiler-only changes in contributor docs.
- [x] Record the slice split and rationale in `memory/CHANGELOG.md` once the code move lands.
- [x] Add a short contributor rule: test lanes must follow ownership, not convenience bundling.
## Leak cleanup follow-up plan

- [x] Confirm the harness-only teardown regression lane in `src/lisp/tests_tests.c3` stays green under `OMNI_LISP_TEARDOWN_REGRESSION`.
- [x] Confirm or fix the method-table overwrite seam in `src/lisp/jit_jit_define_method_table.c3` (`jit_eval_define_typed_callable`, `jit_eval_define`) with a focused regression for redefine/replacement semantics.

## 2026-03-28 Runtime Audit Follow-up

- Scope chunk allocator OOM hardening
  - [x] `src/scope_region_chunk_helpers.c3` / `src/scope_region_allocators.c3` / `src/scope_region_lifecycle.c3`: make scope chunk allocation null-safe and propagate failure instead of dereferencing a null `ScopeChunk*` during scope creation and TEMP/ESCAPE slow-path growth
  - [x] `src/scope_region_*` tests: extend the new fault-injection seam beyond `scope_create` so slow-path TEMP/ESCAPE chunk-growth failure is covered without relying on fatal assertions

- Deduce scan-path error propagation
  - [x] `src/lisp/deduce_relation_scan_helpers_more.c3`: propagate `ERROR` returns from `deduce_relation_materialize_row_dict(...)` instead of consing them into scan results as ordinary rows
  - [x] `src/lisp/deduce_relation_scan_helpers_more.c3` / `src/lisp/deduce_schema_query_execution.c3`: replace the current `nil` fallback on `deduce_relation_ensure_column_key_values(...)` failure with canonical `deduce/query-out-of-memory` failure semantics
  - [x] `src/lisp/tests_deduce_*`: add targeted regressions for scan-range / scan-all / in-txn setup and row-materialization OOM propagation

- Env write failure visibility
  - [x] `src/lisp/value_environment.c3`: make `Env.define(...)` surface binding-growth failure, preserve correctness when hash-table rebuild allocation fails, and make barrier writes fail fast instead of silently dropping bindings
  - [x] `src/lisp/deduce_schema_query_*` / `src/lisp/eval_type_*` / `src/lisp/eval_ffi_eval.c3`: thread the new direct env-define failure handling through current query-analysis and user-facing type/FFI registration paths
  - [x] `src/lisp/value_environment.c3` / `src/lisp/jit_jit_dispatch_helpers.c3`: propagate a recoverable barrier-write contract for `env_set_with_barrier(...)` helpers and JIT match-clause binding sites, returning explicit runtime errors instead of silently storing internal failure values or relying on centralized fail-fast
  - [x] `src/lisp/tests_memory_lifetime_*` / `src/lisp/tests_runtime_feature_jit_*`: add focused regressions for forced env growth/barrier failure on direct env growth and hash rebuild fallback paths so the shipped `Env.define(...)` contract is covered independently of the still-open match/bootstrap contract decision

- Boot-time method-table allocation guards
  - [x] `src/lisp/eval_init_primitives.c3` / `src/lisp/eval_dispatch_types.c3`: guard `MethodTable` and method-entry allocation before dereference during primitive/type registration
  - [x] `src/lisp/value_interp_*` / `src/lisp/eval_init_primitives.c3` / `src/lisp/eval_dispatch_types.c3`: propagate bootstrap registration failure so interpreter, entry-mode, test-harness, and AOT init paths stop before stdlib/user execution can continue with partially installed dispatch/type surfaces

## 2026-03-28 Pattern Guard Hard-Error Channel

- [x] `src/lisp/eval_pattern_support_helpers.c3` / `src/lisp/eval_pattern_matching.c3`: add a real hard-error path for `MatchResult` so guard-scope allocation/binding and guard predicate runtime failures surface as runtime errors instead of collapsing into plain "no match" semantics or assert-style fail-fast behavior

## 2026-03-28 Deduce Destructive/Admin Mutation Contamination Lane

- [x] `src/lisp/tests_deduce_groups.c3` / bounded `OMNI_LISP_TEST_SLICE=deduce`: close the apparent mutation/admin contamination lane by isolating selected Deduce blocks and fine-grained filter lanes in fresh interpreters; the previously red bounded prefix `basics,materialized,relation-attrs,core-runtime,integrity,command-void,query` now passes at `284 passed, 0 failed`, and the clean full bounded `deduce` slice is green at `375 passed, 0 failed`

## 2026-03-28 Audit Recheck Follow-up

- `--gen-e2e` truthful corpus generation
  - [x] `src/lisp/tests_e2e_generation.c3`: replace the current fixed-buffer append path with a contract that surfaces overflow/truncation explicitly for generated source, expected output, and per-case rendered values so `--gen-e2e` cannot exit green after emitting incomplete corpora

- `--gen-e2e` interpreter ownership cleanup
  - [x] `src/entry_runtime_modes.c3` / `src/lisp/tests_e2e_generation.c3`: remove the current double-bootstrap path so `run_gen_e2e_mode()` and `generate_e2e_tests(...)` share one explicit interpreter ownership contract instead of allocating and bootstrapping a second runtime internally

- `--init` fresh-target contract
  - [x] `src/entry_project_init_files.c3` / `src/entry_project_init_bind.c3`: make `--init` fail truthfully on preexisting project roots or non-directory collisions instead of treating blanket `EEXIST` as success and then overwriting scaffold files into an existing tree

- Deduce durability quiet subprocess leakage
  - [x] `src/lisp/tests_deduce_durability_groups.c3` / `src/entry_script_mode.c3`: stop restart-script success values from leaking raw `true` lines into bounded quiet Deduce runs, either by driving a quiet script-mode path or by making the durability scripts end with `nil`/`void` while preserving their assertions
