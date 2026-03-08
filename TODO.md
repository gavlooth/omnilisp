# Boundary Hardening TODO

Assumption lock (owner directive):
- [x] Keep region RC as lifetime authority (`ScopeRegion` ownership remains primary).
- [x] Do not introduce per-value reclamation for core language values.
- [x] Limit changes to boundary safety/efficiency and lifecycle guardrails.

## P0. Finalization Logic Unification (Eval + JIT)

Goal: remove duplicated boundary finalize behavior by centralizing promote/splice/copy flow.

- [x] Inventory current finalize callsites and behavior deltas.
  - [x] Eval path: `run_promote_result(...)` delegates to `boundary_finalize_scoped_result(...)`.
  - [x] JIT path: `jit_finalize_scoped_result(...)` delegates to `boundary_finalize_scoped_result(...)`.
  - [x] Shared commit policy path: `boundary_commit_escape(...)` centralizes promote/splice/reuse/fallback outcomes.
- [x] Document required shared contract for result finalization:
  - [x] Inputs: `result`, `original_result`, `saved_scope`, `active_scope`, `copy_site`.
  - [x] Outputs: finalized `Value*`, explicit child-scope release/transfer state.
  - [x] Invariants: boundary state restored, no child-scope dangling pointers.
- [x] Introduce one shared boundary helper API for finalize flow.
- [x] Migrate evaluator finalize path to shared helper.
- [x] Migrate JIT finalize path to shared helper.
- [x] Remove duplicated legacy finalize branches after migration.
- [x] Add focused regression tests for parity between Eval and JIT finalize behavior.
- [x] Add one test each for:
  - [x] promote+splice success path
  - [x] promote aborted -> fallback copy path
  - [x] non-scope-owned result fast return path

## P0. Boundary Session Guard (`defer`-safe state restore)

Goal: eliminate manual save/restore drift for `current_scope` and `releasing_scope`.

- [x] Define `BoundarySession` state object (minimal fields only).
- [x] Add `boundary_session_begin(...)` helper.
- [x] Add `boundary_session_end(...)` helper.
- [x] Ensure usage pattern enforces `defer boundary_session_end(...)`.
- [x] Convert existing manual save/restore helpers to use session guard internally (or deprecate duplicates).
- [x] Add invariants/assertions:
  - [x] begin requires non-null interp + valid scope chain
  - [x] end restores exactly captured pointers
- [x] Add negative tests:
  - [x] nested sessions restore correctly (LIFO)
  - [x] early-return path preserves state
  - [x] error/abort path preserves state

## P0. Replace Direct `env_scope.refcount++` With `scope_retain`

Goal: enforce ownership/thread guards on env-scope retain edges.

- [x] Locate all direct `env_scope.refcount++` mutations in boundary/promotion/env-copy paths.
- [x] Replace direct increments with `main::scope_retain(...)`.
- [x] Verify destructor paths still pair with `scope_release(...)`.
- [x] Add regression tests for closure/env-copy boundaries:
  - [x] copy closure payload retain/release symmetry
  - [x] nested env-copy closure retain symmetry
- [x] Run scope ownership invariants checks in debug/test mode.

## P1. Strengthen Splice Legality Preconditions

Goal: prevent misuse of `scope_splice_escapes` by hardening legality checks.

- [x] Expand `boundary_is_scope_transfer_legal(...)` preconditions:
  - [x] `parent != null` and `child != null`
  - [x] `parent != child`
  - [x] `child.parent == parent` (or explicitly allowed ancestry policy)
  - [x] `child.refcount == 1` for transfer-fast-path eligibility
  - [x] owner-thread token compatibility checks
  - [x] lane pointers/chunk list shape sanity
- [x] Return structured failure reason (enum/code) for telemetry.
- [x] Update callers to handle explicit failure reason codes.
- [x] Add negative tests:
  - [x] sibling/foreign scope splice rejected
  - [x] refcount>1 splice rejected
  - [x] owner-thread mismatch rejected

## P1. Make Splice O(1) With ESCAPE Tail Pointers

Goal: remove repeated O(n) tail walks during ESCAPE dtor/chunk list concatenation.

- [x] Extend `ScopeRegion` with:
  - [x] `escape_chunks_tail`
  - [x] `escape_dtors_tail`
- [x] Maintain tails on all relevant operations:
  - [x] ESCAPE alloc append
  - [x] ESCAPE dtor register
  - [x] reset/destroy clear
  - [x] splice transfer
- [x] Replace tail traversal loops in splice with constant-time concatenation.
- [x] Add integrity assertions for head/tail consistency.
- [x] Add tests:
  - [x] multiple splice operations preserve order and validity
  - [x] empty/non-empty concat combinations
  - [x] tail reset on full destroy/reset

## P1. Explicit `boundary_commit_escape(...)` Helper

Goal: make survivor commit policy a single callable primitive.

- [x] Define commit helper contract:
  - [x] target semantics for TEMP-owned values
  - [x] no-op semantics when already reusable in target chain
  - [x] fallback semantics when promotion context aborts
- [x] Implement helper wrappers around existing promote/copy entrypoints.
- [x] Migrate repeated callsites to helper.
- [x] Add site tagging support to preserve copy-site accounting.
- [x] Add tests:
  - [x] TEMP survivor committed
  - [x] already-safe value reused
  - [x] commit fallback copy path

## P2. Telemetry Expansion for Boundary Decisions

Goal: make optimization and safety decisions data-driven.

- [x] Add counters:
  - [x] splice attempted
  - [x] splice succeeded
  - [x] splice failed by reason code
  - [x] promotion attempted
  - [x] promotion aborted by reason
  - [x] copy fallback by reason + callsite
- [x] Add summary emission in test/diagnostic mode.
- [x] Add guard script or test assertion for required telemetry fields.
- [x] Produce baseline metrics snapshot for current suite.
- [x] Define threshold alerts for regression (copy-fallback spikes, splice success drops).

## P2. Boundary Negative and Stress Test Matrix

Goal: close boundary-state gaps with targeted tests.

- [x] Add nested boundary-session stress tests.
- [x] Add mixed event-order tests for boundary restore under failures.
- [x] Add repeated promote-abort/copy fallback stress loop.
- [x] Add splice legality fuzz cases (invalid parents/siblings/nulls).
- [x] Add closure/env boundary retain symmetry stress tests.
- [x] Add test matrix rows for:
  - [x] normal build
  - [x] ASAN build
  - [x] `OMNI_FIBER_TEMP=1`

## C3 Implementation Guidelines For This Refactor

Goal: use C3 features to make boundary hardening safer, more deterministic, and easier to maintain without changing the ownership model.

- [x] Keep `BoundaryTxn` protocol-like and single-use:
  - [x] explicit states such as `INIT`, `OPEN`, `COMMITTED`, `ABORTED`, `CLOSED`
  - [x] debug-assert invalid double-commit / double-close / use-after-close
  - [x] do not let it become a convenience bag of mutable state
- [x] Use `defer` for every boundary state restore:
  - [x] scope swaps
  - [x] releasing-scope swaps
  - [x] promotion-context begin/end symmetry
  - [x] error-path cleanup where commit fails
- [x] Add contracts on unsafe boundary helpers:
  - [x] `@require` for non-null interp/scope/value pointers
  - [x] `@require` for expected parent/child scope relation on splice paths
  - [x] `@ensure` for restored boundary state after helper returns
  - [x] `@ensure` for explicit transaction terminal state on commit/abort helpers
- [ ] Use `@inline` only for tiny predicates/accessors:
  - [x] validator-result checks
  - [x] outcome/reason classification helpers
  - [x] simple scope-chain predicates
  - [ ] ASAN-sensitive refactor: previous broad de-inline pass in `eval_boundary_api.c3` triggered deterministic ASAN stack-switch abort during advanced/JIT tests; keep current inlining until a scoped, benchmarked, ASAN-safe reduction plan is validated.
- [x] Use `@noinline` for cold failure/reporting paths:
  - [x] detailed validator failure emission
  - [x] debug graph-audit reporting
  - [x] verbose telemetry dump helpers
- [x] Use macros narrowly for instrumentation scaffolding, not policy:
  - [x] compile-time gated telemetry increments
  - [x] benchmark/debug trace insertion points
  - [x] repetitive structured event logging wrappers
  - [x] avoid macros that hide ownership transitions or boundary control flow
- [x] Add compile-time gates for instrumentation levels:
  - [x] zero-overhead default path when telemetry/debug is off
  - [x] lightweight counters mode
  - [x] verbose boundary trace mode
  - [x] focused benchmark mode
- [x] Add `$assert` checks for structural invariants:
  - [x] boundary outcome enum/table synchronization
  - [x] validator-result enum/table synchronization
  - [x] telemetry counter table length synchronization
  - [x] `BoundaryTxn` state enum/table synchronization if lookup tables are introduced
- [x] Prefer small enums/results over boolean success flags:
  - [x] validator result for splice legality
  - [x] explicit boundary outcome enum for commit result
  - [x] explicit abort reason enum for promotion-context failure
- [x] Define the debug memory-safety invariant as graph reachability:
  - [x] for any committed ESCAPE root, no reachable Omni-owned value edge may point into TEMP
  - [x] define the traversed edge set explicitly before implementing the audit
  - [x] exclude or special-case opaque non-Omni payloads (`FFI_HANDLE`, scheduler shared handles, other foreign wrappers)
- [x] Add `@test` / `@benchmark` support where practical:
  - [x] focused benchmark for promote/splice/copy decision cost
  - [x] focused benchmark for splice before/after tail-pointer optimization
  - [x] targeted protocol misuse tests for `BoundaryTxn`
  - [x] targeted graph-audit tests for TEMP reachability from ESCAPE roots
- [x] Keep macros/helpers centralized:
  - [x] one home for telemetry macros
  - [x] one home for boundary validator/result enums
  - [x] one home for graph-audit debug helpers
  - [x] avoid duplicating lightweight helper definitions across eval/JIT files
- [x] Document the C3 usage rules for this work before broad rollout:
  - [x] when to use a macro vs function
  - [x] which helpers are allowed in hot paths
  - [x] which debug/telemetry facilities must be compile-time gated
  - [x] which boundary helpers are the only legal ownership-transition entrypoints

## P0-P2. Reduce Graph Traversal In Boundary Returns

Goal: make graph traversal the rare fallback path, not the normal return path, while preserving region-centric ownership, dual-lane scope semantics, and dynamic-boundary correctness.

Migration rule:
- [ ] Do not require a single cutover to the final destination-routing architecture.
- [ ] Treat each stage as a valid intermediate state with its own build/test/ASAN closure.
- [ ] Keep fallback traversal for any boundary that is still provenance-uncertain at that stage.
- [ ] Remove a fallback only after the narrower replacement path is implemented, measured, and regression-covered.

### Stage 0. Measure The Current Traversal Tax

- [x] Add an explicit traversal-focused telemetry slice:
  - [x] `copy_to_parent` counts by `ValueTag`
  - [x] `copy_to_parent` counts by boundary site
  - [x] promoted-then-spliced counts
  - [x] promoted-then-fallback-copied counts
  - [x] average/peak cons-spine length copied in list-heavy paths
- [x] Emit the summary only behind compile-time or env-gated diagnostics.
- [x] Record one baseline in `memory/CHANGELOG.md` before changing routing policy.

C3 idioms for Stage 0:
- [x] Use small `enum` + fixed-size counter tables, not string-keyed maps, for hot-path stats.
- [x] Keep telemetry wrappers compile-time gated; default build must keep near-zero overhead.
- [x] Use `$assert` to keep enum/table sizes synchronized.
- [x] Put verbose reporters behind `@noinline` cold helpers.

### Stage 1. Centralize Provenance Classification

- [x] Define one explicit boundary classification result for return values:
  - [x] reusable in target chain
  - [x] owned by releasing TEMP lane
  - [x] owned by releasing ESCAPE lane
  - [x] mixed/uncertain provenance -> fallback copy required
- [x] Route eval and JIT finalize paths through that single classification helper.
- [x] Remove ad-hoc ownership/provenance branching from leaf callsites where equivalent boundary helpers already exist.
- [x] Keep chunk-scan membership checks behind boundary helpers only; no new direct `scope_contains(...)` callsites in hot constructors.

C3 idioms for Stage 1:
- [x] Prefer explicit result enums/structs over stacked booleans.
- [ ] Use exhaustive `switch` on `ValueTag` or route enums for reviewable control flow.
- [ ] Restrict `@inline` to tiny predicates/accessors; keep classification assembly itself in normal functions.
- [ ] Use `@require` for non-obvious scope/lane preconditions.

### Stage 2. Make List/Cons Returns Destination-Aware First

- [x] Introduce one audited path for building escaping list spines directly into the correct surviving lane/scope.
- [x] Keep iterative list-spine construction; do not reintroduce recursive cons-copy paths.
- [x] Ensure store-time barrier checks prevent TEMP pointers from being embedded into ESCAPE-allocated cons cells.
- [x] Convert the highest-volume list-return and accumulator-heavy finalize paths to use this route first.
- [x] Keep `copy_to_parent` as the fallback for uncertain provenance and dynamic surfaces.

C3 idioms for Stage 2:
- [x] Use explicit helper functions for lane-targeted cons construction; do not hide allocation policy inside macros.
- [x] Use `defer` immediately after any temporary scope/lane override.
- [x] Keep barrier helpers small and `@inline` only if they are pure, branch-light predicates.
- [x] Add contracts documenting that ESCAPE constructors must not commit TEMP child edges.

### Stage 3. Extend Destination Routing To Other Aggregates

- [x] Audit `STRING`, `ARRAY`, `HASHMAP`, `PARTIAL_PRIM`, `ITERATOR`, and wrapper-copy paths for unnecessary deep traversal.
- [x] Split wrapper-copy from payload-copy where payload identity is already safe to reuse.
- [x] Add destination-aware constructors or copy helpers only where provenance is explicit and testable.
- [x] Do not introduce per-type RC lifetimes for language values while optimizing these routes.

C3 idioms for Stage 3:
- [ ] Prefer one helper per semantic route, not one helper per type prefix unless the payload semantics actually differ.
- [ ] Use `foreach` over bindings/items when it expresses intent without index math.
- [ ] Keep shared ownership transitions in normal functions with contracts; do not bury them in convenience macros.
- [ ] Use `@nodiscard` on helper results where ignoring the returned routed value is a likely bug.

### Stage 4. Harden Env/Closure Boundaries So They Stop Forcing Copies

- [x] Audit env-copy and closure-return paths for cases where only the wrapper is transient but the captured scope is already safely retained.
- [x] Keep env-scope retention/release centralized through `scope_retain(...)` / `scope_release(...)`.
- [x] Add a single audited route for closure/env payload reuse versus wrapper relocation.
- [x] Add regression coverage for:
  - [x] function return boundary
  - [x] closure capture/env copy boundary
  - [x] destructor/release boundary

C3 idioms for Stage 4:
- [x] Use `defer` for every temporary interpreter state swap in env-copy/promotion code.
- [x] Use `@ensure` on helpers that must leave interp boundary state restored.
- [x] Keep closure/env copy helpers explicit and typed; avoid pointer-punning shortcuts.
- [x] Use cold `@noinline` failure helpers for invariant breaks and diagnostic reports.

### Stage 5. Move More Decisions To JIT/AOT, Keep Dynamic Fallbacks Explicit

- [x] Identify constructors/callsites where JIT/AOT can confidently emit destination-aware build paths.
  - [x] `src/lisp/eval_run_pipeline.c3`: `run_promote_result(...)` (`COPY_SITE_RUN_JIT_RESULT`) is a primary destination-routing candidate for top-level JIT returns.
  - [x] `src/lisp/jit_jit_eval_scopes.c3`: `jit_eval_in_single_scope(...)` (`COPY_SITE_JIT_SINGLE_SCOPE_RETURN`) is a primary destination-routing candidate for one-scope closure returns.
  - [x] `src/lisp/jit_jit_eval_scopes.c3`: `jit_eval_in_call_scope(...)` (`COPY_SITE_JIT_CALL_SCOPE_STEP2`) is a secondary candidate once single-scope route is stable.
  - [x] `src/lisp/jit_jit_eval_scopes.c3`: `copy_tco_env_chain(...)` (`COPY_SITE_JIT_TCO_FRAME_COPY`) remains a boundary-classified fallback candidate, not a direct destination-builder candidate.
  - [x] Candidate aggregate families to route first in JIT return fast-path: `CONS`, `PARTIAL_PRIM`, then `ITERATOR`/closure-wrapper cases where provenance is explicit.
- [x] Keep runtime boundary helpers as the only legal fallback for:
  - [x] runtime `eval` (`run_promote_result(...)` -> `boundary_finalize_scoped_result(...)`)
  - [x] effect/continuation boundaries (facade policy gate + boundary helper surfaces only)
  - [x] coroutine suspend/resume crossings (`prim_resume_yield_result(...)` -> `boundary_copy_to_parent_site(...)`)
  - [x] global mutation or other explicit escape boundaries (`boundary_promote_to_root(...)` callsites in collection/scheduler/IO/raise payload paths)
- [x] Do not fork ownership semantics between eval and JIT; both must terminate in the same boundary contract.

C3 idioms for Stage 5:
- [ ] Use compact opcode/route enums instead of ad-hoc flag bundles when expanding JIT routing.
- [ ] Keep compile-time feature gates at module scope with `@if` or constant mode switches, not scattered preprocessor-like branching.
- [ ] Use `$assert` to keep opcode/route decode tables synchronized.
- [ ] Preserve stack-engine opacity; no direct `ScopeRegion` policy leaks into generic stack core.

### Stage 6. Remove Redundant Traversal Only After Telemetry Proof

- [x] Identify the top `copy_to_parent` callsites still firing after destination-routing rollout.
  - [x] Boundary fallback counters (latest summary): `copy_fallback_site_jit_single=2564`, `copy_fallback_site_run_jit=2141`, `copy_fallback_site_tco=0`.
  - [x] Traversal counters (latest summary): `copy_site_jit_single=1357`, `copy_site_run_jit=308`, `copy_site_tco=0`.
  - [x] Primary narrowing targets stay `COPY_SITE_JIT_SINGLE_SCOPE_RETURN` and `COPY_SITE_RUN_JIT_RESULT` (already mapped in Stage 5 inventory).
- [x] Delete or narrow only the callsites proven redundant by telemetry + tests.
  - [x] Narrowed one fallback class by adding destination-aware `ITERATOR` survivor routing in `boundary_commit_escape(...)` with regression coverage (`boundary_commit_escape TEMP iterator survivor commit`).
  - [x] Narrowed remaining TEMP-owned survivor handling by adding direct destination ESCAPE promotion for non-specialized tags in `boundary_commit_escape(...)` with regression coverage (`boundary_commit_escape TEMP scalar direct destination commit`).
  - [x] Residual fallback volume now centers on explicit dynamic classes (`BOUNDARY_RETURN_MIXED_UNCERTAIN` and pre-aborted/budget contexts), not the narrowed redundant survivor routes.
- [x] Keep explicit fallback traversal for dynamic, provenance-uncertain crossings.
  - [x] `BOUNDARY_RETURN_MIXED_UNCERTAIN` still routes through explicit fallback copy path in `boundary_commit_escape(...)`.
  - [x] splice-rejected and pre-aborted context paths remain explicit fallback branches with reason-tagged telemetry (`copy_fallback_*` counters).
- [x] Re-evaluate `scope_contains(...)` hotness only after normal return-path copies have been reduced.
  - [x] Runtime membership checks remain centralized under boundary helper wrappers (`boundary_ptr_in_scope` / `boundary_ptr_in_scope_chain`) for boundary-return paths.
  - [x] Normalized `copy_value_if_owned_by_scope(...)` in `eval_promotion_context.c3` to use `boundary_ptr_in_scope(...)` instead of direct `main::is_in_scope(...)`.

C3 idioms for Stage 6:
- [ ] Prefer deleting branches and helpers entirely over leaving dead compatibility code in hot files.
- [ ] Keep rollback-friendly changes small and local; one semantic deletion per patch when possible.
- [ ] Add `@test` or focused regression helpers for each deleted traversal branch.
- [ ] Update `memory/CHANGELOG.md` before marking a traversal path retired.

### Stage 7. Final Fallback Removal Closure

- [x] Inventory every remaining fallback traversal site and classify why it still exists:
  - [x] dynamic `eval`
  - [x] continuation/effect boundary
  - [x] coroutine suspend/resume boundary
  - [x] global mutation / persistent store boundary
  - [x] legacy uncertain-provenance helper
  - [x] Inventory snapshot (2026-03-08):
    - dynamic eval fallback surface (active + hottest): `boundary_commit_escape(...)` in `src/lisp/eval_boundary_api.c3` via `boundary_finalize_scoped_result(...)` callsites:
      - `src/lisp/eval_run_pipeline.c3`: `run_promote_result(...)` (`COPY_SITE_RUN_JIT_RESULT`)
      - `src/lisp/jit_jit_eval_scopes.c3`: `jit_eval_in_single_scope(...)` (`COPY_SITE_JIT_SINGLE_SCOPE_RETURN`)
      - current summary counters: `copy_fallback_total=67`, `copy_fallback_site_run_jit=3`, `copy_fallback_site_jit_single=61`, `copy_fallback_site_jit_call=3`, `copy_fallback_site_tco=0`, `copy_fallback_mixed_uncertain=10`, `copy_fallback_budget=47`, `copy_fallback_splice_rejected=8`, `copy_fallback_releasing_scope=2`.
    - continuation/effect boundary fallback surface (same boundary engine, low-volume but now explicitly measured):
      - `src/lisp/jit_jit_eval_scopes.c3`: `jit_eval_in_call_scope(...)` (`COPY_SITE_JIT_CALL_SCOPE_STEP2`)
      - effect/call unwinds converge on `boundary_finalize_scoped_result(...)` -> `boundary_commit_escape(...)`; current summary shows non-zero but small pressure (`copy_fallback_site_jit_call=3`).
    - coroutine suspend/resume boundary fallback surface (now narrowed):
      - `src/lisp/primitives_iter_coroutine.c3`: `prim_resume_yield_result(...)` now routes through `boundary_copy_to_scope_site(..., COPY_SITE_PRIM_RESUME)` with explicit `yield_scope` provenance captured at `yield`.
      - legacy direct `boundary_copy_to_parent_site(...)` use in coroutine resume path has been removed; missing provenance now hard-errors (`resume: missing yield-scope provenance`).
    - global mutation / persistent store boundary fallback surface:
      - `src/lisp/eval_promotion_escape.c3`: `promote_to_root_site(...)` uses `boundary_copy_to_scope_site(...)`.
      - `src/lisp/jit_jit_closure_define_qq.c3`: root-store writes through `boundary_promote_to_root_site(...)` (`COPY_SITE_JIT_EVAL_SET`, `COPY_SITE_JIT_ENV_EXTEND_ROOT`) and one `boundary_copy_to_scope_site(...)` env-store path.
      - `src/lisp/jit_jit_define_method_table.c3`: root define via `boundary_promote_to_root_site(...)` (`COPY_SITE_JIT_EVAL_DEFINE`).
    - legacy uncertain-provenance helper surface (remaining fallback machinery):
      - `src/lisp/eval_boundary_api.c3`: `boundary_copy_to_scope_site(...)`, `boundary_copy_from_releasing_scope(...)`, `boundary_copy_value_if_owned_by_scope(...)`.
      - `src/lisp/eval_promotion_context.c3`: `copy_value_if_owned_by_scope(...)` (underlying `copy_to_parent` traversal fallback primitive).
- [ ] For each remaining class, either:
  - [ ] replace it with destination-aware construction or transfer-safe routing, or
  - [ ] redesign the boundary contract so transient-to-surviving provenance is explicit at construction time
  - [x] coroutine suspend/resume boundary: replaced legacy direct copy with transfer-safe routing that carries explicit releasing-scope provenance (`yield_scope`) from `yield` to `resume`.
  - [x] mixed/uncertain class now tries destination ESCAPE promotion before deep-copy fallback (`BOUNDARY_COMMIT_MIXED_DESTINATION_PROMOTED`) with regression coverage (`boundary_commit_escape mixed uncertain destination commit`).
  - [x] stack-overflow unwind ASAN crash closed: stack-overflow error path now survives deep recursive unwind without boundary disjoint-copy recursion crashes (root-scoped stack-overflow error + direct destination ERROR escape route), and ASAN full-suite run no longer exits `139`.
  - [x] splice-rejected fast-path experiment was rolled back after ASAN instability; splice-rejected class stays on explicit fallback copy until a safer replacement lands.
  - [x] return-boundary promotion budget was raised (`PROMOTION_CTX_BUDGET_RETURN: 4096 -> 16384`) to reduce pre-aborted finalize contexts in dynamic eval/JIT return paths.
  - [x] return-finalize boundary no longer budget-aborts: `boundary_finalize_scoped_result(...)` now uses unbounded promotion context for commit, and large-list eval/JIT parity coverage was updated accordingly (`run_memory_lifetime_finalize_abort_fallback_parity_test`).
  - [x] synthetic forced-fallback stress coverage no longer pollutes global parity telemetry: added `boundary_restore_decision_stats(...)` and wrapped budget=0 stress/parity probes so summary counters reflect non-synthetic production paths.
  - [ ] dynamic eval boundary: residual fallback pressure is no longer budget-driven; it now centers on splice-rejected/releasing-scope classes (`copy_fallback_splice_rejected=8`, `copy_fallback_releasing_scope=2`) with reduced dynamic sites (`copy_fallback_site_jit_single=7`, `copy_fallback_site_run_jit=2`, `copy_fallback_site_jit_call=1`, `copy_fallback_budget=0`).
  - [ ] continuation/effect boundary: keep on shared finalize/commit path and close once dynamic eval fallback classes are removed.
  - [ ] global mutation/persistent store boundary: still rooted in `promote_to_root_site(...)` / root-store paths using copy helper routing.
  - [ ] legacy uncertain-provenance helpers: `boundary_copy_to_scope_site(...)` / `boundary_copy_from_releasing_scope(...)` / `copy_value_if_owned_by_scope(...)` remain pending final deletion.
- [ ] Remove the last production `copy_to_parent` fallback branches from boundary return flow.
- [ ] Remove now-dead traversal telemetry branches and compatibility helpers.
- [ ] Update docs so the current runtime description no longer treats graph traversal as a live fallback mechanism.

C3 idioms for Stage 7:
- [ ] Use exhaustive `switch`es over remaining boundary classes so deletion work is mechanically reviewable.
- [ ] Prefer deleting whole helpers/modules over leaving dormant fallback code behind feature flags.
- [ ] Add focused regression tests that prove the replacement path covers the old fallback case before deleting the branch.
- [ ] Use `$assert` and small route enums to verify no retired fallback route remains referenced.

### Acceptance Criteria For This Plan

- [ ] The rollout remains staged; no TODO item assumes an all-at-once migration to the final architecture.
- [ ] Normal return paths for list-heavy code prefer reuse/splice/destination-build over deep copy.
- [ ] Intermediate stages may keep explicit fallback traversal where required for correctness.
- [ ] Final closure of this TODO removes the remaining production fallback traversal paths.
- [ ] Eval and JIT share the same boundary ownership rules.
- [ ] No stop-the-world GC, no root pinning workaround, and no per-type RC ownership model is introduced.
- [ ] `c3c build`, `c3c build --sanitize=address`, `LD_LIBRARY_PATH=/usr/local/lib ./build/main`, and `scripts/run_boundary_hardening.sh` stay green for each landed stage.

## P1-P3. Remove Non-Dispatch Fallback Pollution

Goal: remove fallback and compatibility paths that are not part of the intended language model. `MethodTable.fallback` is explicitly excluded from this plan.

Rules:
- [ ] Treat `MethodTable.fallback` as canonical dispatch behavior, not cleanup target.
- [ ] Everything else must justify its existence as either a temporary migration shim with an explicit paired replacement task in this TODO or a missing implementation to be completed by a named stage in this TODO.
- [ ] Prefer deleting fallback branches over keeping permanent compatibility residue.
- [ ] No fallback or legacy branch may remain as an open-ended "keep for now" item without a concrete end-state task.
- [ ] If a fallback stays temporarily, the replacement work must be listed immediately next to it in this TODO.

### Track A. Inventory And Classify Every Remaining Fallback

- [ ] Create one tracked inventory of live fallback surfaces, grouped by:
  - [ ] compatibility wrapper
  - [ ] parser permissive fallback
  - [ ] compiler/JIT implementation fallback
  - [ ] runtime ownership fallback
  - [ ] legacy alias / duplicate public API
- [ ] Mark each item as one of:
  - [ ] delete now
  - [ ] replace first, then delete
  - [ ] keep only until a specific staged migration in this TODO lands
- [ ] Record the inventory owner file and removal gate in `memory/CHANGELOG.md` or a focused plan doc once work starts.
- [ ] Seed the inventory with the currently confirmed live fallback/legacy surfaces:
  - [ ] `src/lisp/eval_init_primitives.c3` legacy deduce registrations
  - [ ] `src/lisp/compiler_stdlib_prelude.c3` compatibility wrappers
  - [ ] `src/lisp/parser_lexer_string_hash.c3` unknown `#` permissive fallthrough
  - [ ] `src/lisp/compiler_code_emission.c3` ignored compatibility signature
  - [ ] `src/lisp/jit_jit_compile_expr_core.c3` / `src/lisp/jit_jit_compile_effects_modules.c3` interpreter fallback
  - [ ] `src/pika/regex_cache_api.c3` compiled->simple regex fallback
  - [ ] `src/lisp/eval_promotion_copy.c3` family of runtime ownership copy fallbacks
  - [ ] `src/scope_region_reset_helpers.c3` defensive TEMP-lane repair branch
  - [ ] undeclared-effect backward-compat behavior currently exercised in `src/lisp/tests_advanced_io_effect_ffi_groups.c3`

C3 idioms for Track A:
- [ ] Use small enums for fallback class/status if instrumentation is added.
- [ ] Keep the inventory grounded in actual callsites, not comments alone.
- [ ] Use `$assert` for any enum/table tracking added to audits.

### Track B. Delete Legacy Public API Fallbacks

- [ ] Remove the legacy deduce primitive registrations once equivalent unified `deduce` call coverage and tests are in place:
  - [ ] `deduce-open`
  - [ ] `fact!`
  - [ ] `retract!`
  - [ ] `deduce-scan`
  - [ ] `deduce-query`
  - [ ] `deduce-count`
  - [ ] `deduce-match`
- [ ] Migrate tests/docs/examples that still depend on those legacy entrypoints.
- [ ] Migrate the currently known test/doc dependents:
  - [ ] `src/lisp/tests_deduce_groups.c3`
  - [ ] `src/lisp/tests_deduce_query_groups.c3`
  - [ ] `src/lisp/tests_deduce_durability_groups.c3`
  - [ ] `src/lisp/tests_deduce_isolation_groups.c3`
  - [ ] docs/examples that still mention direct legacy deduce names rather than `(deduce '...)`
- [ ] Add one rejection/error test per removed legacy public name if the parser/runtime should now reject it deterministically.

C3 idioms for Track B:
- [ ] Delete primitive table entries directly; do not leave stub aliases behind.
- [ ] Use explicit negative tests for removed names rather than silent aliasing.
- [ ] Keep public API cleanup in small patches so regressions are attributable.

### Track C. Delete Error-Model Compatibility Wrappers

- [ ] Replace `raise->message` / `try-message` usage in tests, fixtures, and stdlib callsites with canonical payload-aware handling.
- [ ] Migrate the currently known dependent surfaces:
  - [ ] `src/lisp/tests_advanced_core_unicode_groups.c3`
  - [ ] `src/lisp/tests_runtime_feature_http_groups.c3`
  - [ ] `src/lisp/tests_runtime_async_groups.c3`
  - [ ] `src/lisp/tests_advanced_io_effect_ffi_groups.c3`
  - [ ] `tests/lib/tls/server_once.omni`
- [ ] Normalize remaining string-error or message-only handler expectations onto canonical raise payloads.
- [ ] Remove the compatibility wrapper definitions from the stdlib prelude once no production/test code depends on them.
- [ ] Remove docs that present them as standard surface once the migration is complete.

C3 idioms for Track C:
- [ ] Prefer normal helper functions/macros in Omni source for canonical payload extraction where truly needed, not compatibility wrappers with degraded semantics.
- [ ] Add focused regression tests for canonical error payload shape instead of wrapper behavior.
- [ ] Keep handler rewrites explicit; do not hide payload adaptation in broad macros.

### Track D. Remove Parser Permissive Fallbacks

- [ ] Replace unknown `#` dispatch fallthrough-to-symbol behavior with explicit targeted parser errors.
- [ ] Keep `src/lisp/parser_import_export.c3` legacy-marker special cases only if they still provide materially better migration errors than normal parser failure; otherwise collapse them into standard parse error flow once old syntax removal is complete.
- [ ] Audit for any other lexer/parser “accept legacy syntax silently” fallbacks and either:
  - [ ] reject them explicitly, or
  - [ ] document them as canonical syntax if they are intentionally supported
- [ ] Add regressions that rejected legacy/permissive forms fail with deterministic messages.

C3 idioms for Track D:
- [ ] Prefer explicit token/error branches over permissive fallthrough in lexer code.
- [ ] Keep lexer branches small and exhaustive where token class is known.
- [ ] Use clear `set_error` paths close to detection, not deferred ambiguous failure later in parsing.

### Track E. Remove Compiler Compatibility Residue

- [ ] Delete ignored compatibility parameters and helper shapes that no longer affect behavior, starting with `emit_lambda_return_with_frame(..., has_frame)`.
- [ ] Collapse duplicate compiler entrypoints that only survive for old call signatures.
- [ ] Audit comments marked “API compatibility” or “intentionally ignored” and remove the underlying dead compatibility path, not just the comment.

C3 idioms for Track E:
- [ ] Prefer changing signatures and fixing callers over keeping ignored parameters.
- [ ] Use compile errors to flush stale callsites instead of compatibility shims.
- [ ] Keep one canonical helper per compiler action.

### Track F. Eliminate JIT Interpreter Fallback

- [ ] Inventory which expression groups still route through `jit_compile_fallback(...)`.
- [ ] Enumerate the current `handled = false` sources in JIT expression grouping and map each one to a concrete lowering task.
- [ ] Add telemetry or an audit mode to measure fallback frequency by expression kind.
- [ ] Implement native lowering for the remaining expression classes in descending runtime frequency.
- [ ] Explicitly land variadic-lambda JIT support so the known variadic-closure fallback path can be deleted.
- [ ] Use `src/lisp/tests_advanced_core_unicode_groups.c3` variadic coverage as the named regression gate for deleting that path.
- [ ] Convert `jit_compile_fallback(...)` from production path to guarded internal failure once all supported expression forms are lowered.
- [ ] Delete the final interpreter fallback path from JIT expression compilation.

C3 idioms for Track F:
- [ ] Use compact route enums and exhaustive `switch`es for JIT expression families.
- [ ] Keep JIT lowering grouped by semantic domain, not a growing catch-all file.
- [ ] Use `@noinline` only for cold audit/report paths; compiled-expression dispatch stays straightforward and reviewable.
- [ ] Add `$assert` synchronization for any route tables introduced during coverage expansion.

### Track G. Close Runtime Ownership Fallbacks

- [ ] Land the staged graph-traversal reduction plan above.
- [ ] Replace current `copy_to_parent(...)` boundary users with destination-aware or boundary-classified routes, starting with:
  - [ ] `src/lisp/eval_env_copy.c3`
  - [ ] `src/lisp/eval_repl.c3`
  - [ ] `src/lisp/primitives_iter_coroutine.c3`
  - [ ] `src/lisp/eval_promotion_escape.c3`
  - [ ] `src/lisp/eval_promotion_context.c3`
- [ ] Remove remaining `copy_to_parent`-style fallback branches after destination-aware replacements cover their boundary class.
- [ ] Remove compatibility telemetry or branch labels that only existed to observe now-deleted fallback paths.
- [ ] Retire `releasing_scope`-driven defensive copy logic once provenance classification and destination routing fully replace it.

C3 idioms for Track G:
- [ ] Delete ownership fallbacks only after the replacement boundary helper is the sole legal path.
- [ ] Keep state-restore and scope-restore logic `defer`-backed until the final fallback branch is gone.
- [ ] Preserve explicit contracts on ownership-transition helpers during cleanup; do not weaken them while deleting branches.

### Track H. Collapse Dual Implementations And Defensive Residue

- [ ] Regex dual-path cleanup:
  - [ ] inventory all conditions that trigger `regex_search_simple_fallback`, `regex_fullmatch_simple_fallback`, and `regex_find_all_simple_fallback`
  - [ ] split cache-bookkeeping failure from syntax/engine capability failure so regex fallback cause is explicit
  - [ ] either make compiled regex handling total for the supported syntax set or explicitly split "simple regex mode" from the normal API
  - [ ] remove the silent compiled-to-simple production fallback from the default regex API
  - [ ] migrate `src/lisp/tests_runtime_feature_pika_groups.c3` parity tests to validate the chosen post-fallback contract rather than permanent dual-path parity
- [ ] Scope reset defensive residue:
  - [ ] replace "recreate baseline TEMP chunk if scope had none" defensive branch with one shared invariant-preserving helper
  - [ ] add assertions/tests so the branch is not needed as a silent fallback path
  - [ ] delete the defensive fallback branch once invariant restoration is centralized
- [ ] Boundary provenance residue:
  - [ ] replace `in_target_scope_chain()` / `is_in_scope()` / `scope_contains()` chunk walks with one canonical provenance test path
  - [ ] remove direct closure payload ownership scans in `src/lisp/eval_promotion_copy.c3` once scope/lane provenance is explicit
  - [ ] eliminate remaining `scope_contains(...)`-driven defensive ownership checks from hot paths as destination/provenance routing lands
  - [ ] keep any temporary scan only where a named preceding stage in this TODO still depends on it

C3 idioms for Track H:
- [ ] Prefer one canonical implementation path per subsystem unless the second path is a first-class supported mode.
- [ ] Convert defensive fallback branches into explicit invariant helpers plus assertions before deletion.
- [ ] Keep replacement helpers small, typed, and contract-backed rather than adding more compatibility flags.

### Track J. Tighten Language-Surface Compatibility Drift

- [ ] Decide whether undeclared effects remain canonical language behavior or are removed as backward-compat residue.
- [ ] If undeclared effects are not canonical, enforce explicit effect declaration and migrate/remove the compatibility expectation in `src/lisp/tests_advanced_io_effect_ffi_groups.c3`.
- [ ] Audit for similar "still works for backward compat" language-surface behavior and either:
  - [ ] promote it to explicit spec-backed status, or
  - [ ] remove it with deterministic rejection tests

C3 idioms for Track J:
- [ ] Keep language-surface rules explicit in parser/eval contracts, not implied by permissive runtime behavior.
- [ ] Prefer one canonical form per feature family; reject legacy alternates rather than silently accepting them.

### Track I. Split Oversized Hotspot Files Before More Policy Changes

- [ ] Split `src/lisp/eval_boundary_api.c3` top-down before further boundary-policy growth:
  - [ ] session/txn guards
  - [ ] provenance classification + legality checks
  - [ ] commit/promote/splice/copy flow
  - [ ] graph-audit logic
  - [ ] telemetry/reporting helpers
- [ ] Split `src/lisp/scheduler_thread_tasks.c3` top-down by waiter lifecycle, completion signaling, and thread/task state transitions.
- [ ] Split `src/lisp/tls.c3` top-down by handle lifecycle, I/O operation state machine, and worker/offload boundary.
- [ ] Split `src/lisp/scheduler_primitives_tasks.c3` top-down by public primitive surface versus internal wait/join orchestration.
- [ ] Split `src/lisp/tests_memory_lifetime_boundary_groups.c3` top-down by invariant family so fallback-removal regressions stay localized.

C3 idioms for Track I:
- [ ] Always split the largest files first, per repo refactor rules.
- [ ] Move cohesive helper clusters intact before editing behavior inside them.
- [ ] Keep module surfaces small and explicit after each split; do not recreate monoliths under new names.

### Closure Criteria For This Plan

- [ ] No legacy duplicate public APIs remain solely for backward compatibility.
- [ ] No compatibility error wrappers remain on the normal language surface.
- [ ] No parser permissive fallthrough remains where the language intends explicit syntax.
- [ ] No ignored compiler compatibility signatures remain.
- [ ] JIT no longer falls back to interpreter for supported expression forms.
- [ ] Regex default APIs no longer silently switch to a second implementation path.
- [ ] Defensive invariant-repair branches have been replaced by explicit helpers/assertions or deleted.
- [ ] Language-surface backward-compat behavior is either explicitly spec-backed or removed with tests.
- [ ] Ownership/provenance hot paths no longer depend on repeated chunk-walk membership scans.
- [ ] Largest hotspot files have been split before further cleanup lands in those subsystems.
- [ ] Runtime ownership fallback cleanup is complete except for `MethodTable.fallback`, which remains canonical.

## Validation Gates (for every milestone)

- [x] `c3c build`
- [x] `c3c build --sanitize=address`
- [x] `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - [x] Resolved plain-run hang by making TLS integration fixtures opt-in via `OMNI_ENABLE_TLS_INTEGRATION=1`
- [x] Boundary guard scripts:
  - [x] `scripts/check_boundary_facade_usage.sh`
  - [x] `scripts/check_boundary_change_policy.sh`
  - [x] `scripts/run_boundary_hardening.sh`

## Documentation/Change Tracking

- [x] Update `memory/CHANGELOG.md` with each landed boundary-hardening change.
- [x] Keep `docs/areas/memory-runtime.md` status aligned with verified state.
- [x] Record any invariants/policy additions in `AGENTS.md` if behavior contract changes.

## Suggested Execution Order

- [x] Stage A: direct retain fix + boundary session guard
- [x] Stage B: finalize unification (Eval/JIT)
- [x] Stage C: splice legality hardening
- [x] Stage D: splice O(1) tail pointers
- [x] Stage E: commit helper migration
- [x] Stage F: telemetry + stress matrix closure
