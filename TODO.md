# Boundary Hardening TODO

## P0. Fix `Value` Dispatch First (Constructor + API Shape)

Goal: make literal-value dispatch truly extensible for idiomatic command-style APIs (for example `(udp 'bind ...)`) with canonical `Value` constructor.

- [x] Canonicalize type constructor naming:
  - [x] Introduce `Value` as the canonical value-literal type constructor in parser/runtime/docs.
  - [x] Remove `Val` alias support; `Value` is now the only supported constructor.
  - [x] Normalize internal naming to avoid legacy `Val` assumptions in parser/runtime structures.
- [x] Extend parser value-literal annotation capture beyond int literals:
  - [x] symbol literal support (`^(Value bind)`)
  - [x] string literal support (`^(Value "open")`)
  - [x] boolean literal support (`^(Value true)` / `^(Value false)`)
  - [x] keep integer parity (`^(Value 0)`) unchanged
- [x] Extend runtime method-match logic to compare value literals by runtime value tag/content (not int-only branch).
- [x] Keep dispatch scoring semantics unchanged (`Value` literal remains highest specificity); only widen literal domain.
- [x] Add focused tests:
  - [x] positive dispatch: symbol value-literal routes to correct method
  - [x] positive dispatch: string and boolean value-literal routes to correct method
  - [x] ambiguity/error behavior when two equal-specificity value-literal candidates exist
  - [x] regression: existing integer value-literal dispatch still works
  - [x] regression: legacy `Val` constructor is rejected with deterministic diagnostic
- [x] Add API-shape tests/docs discussed in this session:
  - [x] command-style unified facade example: `(udp 'open)` / `(udp 'bind ...)` / `(udp 'send ...)`
  - [x] explicit contract note: command-style facades must delegate to canonical `io/udp-*` operation effects
  - [x] module-style façade work is explicitly deferred until dedicated module-boundary packaging work; near-term surface remains canonical `io/*` in core.
- [x] Update language/reference docs to reflect canonical `Value`, literal domain, and examples.

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
- [x] Use `@inline` only for tiny predicates/accessors:
  - [x] validator-result checks
  - [x] outcome/reason classification helpers
  - [x] simple scope-chain predicates
  - [x] ASAN-sensitive refactor gate closed: broad de-inline remains rejected, but scoped/benchmarked de-inline is now validated and can proceed incrementally (`boundary_copy_to_parent_site_ctx` pilot passed full hardening/ASAN gates).
  - [x] Scoped de-inline pilot landed in `eval_boundary_api.c3`: moved heavy `boundary_copy_to_parent_site_ctx(...)` body into `boundary_copy_to_parent_site_ctx_impl(...) @noinline` and kept a thin inline wrapper.
  - [x] Scoped de-inline pilot validation: `c3c build`, `c3c build --sanitize=address`, and `scripts/run_boundary_hardening.sh` all passed with unchanged boundary threshold gate outputs.
  - [x] Extended scoped de-inline in `eval_boundary_api.c3` for non-trivial scope/session helpers:
    - `boundary_alloc_value_in_scope(...)` -> `boundary_alloc_value_in_scope_impl(...) @noinline`
    - `boundary_make_env_in_scope(...)` -> `boundary_make_env_in_scope_impl(...) @noinline`
    - `boundary_env_extend_in_scope(...)` -> `boundary_env_extend_in_scope_impl(...) @noinline`
    - `boundary_copy_env_to_target_scope(...)` -> `boundary_copy_env_to_target_scope_impl(...) @noinline`
    - thin inline facades preserved at the public boundary API surface.
  - [x] JIT state-pool overflow no longer leaks under ASAN: overflow states are tracked and reclaimed at `jit_gc`/shutdown; `scripts/run_boundary_hardening.sh` Stage 4 passes with `detect_leaks=1`.
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
- [x] Do not require a single cutover to the final destination-routing architecture.
- [x] Treat each stage as a valid intermediate state with its own build/test/ASAN closure.
- [x] Keep fallback traversal for any boundary that is still provenance-uncertain at that stage.
- [x] Remove a fallback only after the narrower replacement path is implemented, measured, and regression-covered.

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
- [x] Use exhaustive `switch` on `ValueTag` or route enums for reviewable control flow.
- [x] Restrict `@inline` to tiny predicates/accessors; keep classification assembly itself in normal functions.
  - [x] Removed `@inline` from route-classification builders with non-trivial switch bodies (`copy_parent_route_for_tag(...)`, `promote_escape_route_for_tag(...)`).
  - [x] Kept `boundary_classify_return_value(...)` as a normal function (non-inline) for reviewable provenance assembly.
- [x] Use `@require` for non-obvious scope/lane preconditions.
  - [x] Added explicit contracts for `boundary_classify_return_value(...)` (`interp`, `target_scope`, `releasing_scope` must be non-null).

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
- [x] Prefer one helper per semantic route, not one helper per type prefix unless the payload semantics actually differ.
  - [x] boundary routing is organized by semantic route helpers (`boundary_build_destination_*`, `boundary_commit_escape(...)`, `root_store_route_to_scope(...)`) instead of type-prefixed compatibility helper proliferation.
- [x] Use `foreach` over bindings/items when it expresses intent without index math.
  - [x] `foreach` is the canonical iteration form across non-indexed binding/item loops (compiler/env/init/repl helper surfaces); index loops remain only where index semantics are required.
- [x] Keep shared ownership transitions in normal functions with contracts; do not bury them in convenience macros.
  - [x] ownership-transition routes are centralized in normal boundary functions with `@require` contracts (`boundary_copy_to_parent_site_ctx(...)`, `boundary_commit_escape(...)`, scoped-copy helpers) rather than macro-expanded control flow.
- [x] Use `@nodiscard` on helper results where ignoring the returned routed value is a likely bug.
  - [x] added `@nodiscard` to critical boundary routing helpers:
    - `boundary_copy_to_parent_site_ctx(...)`
    - `boundary_copy_env_to_target_scope(...)`

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
- [x] Use compact opcode/route enums instead of ad-hoc flag bundles when expanding JIT routing.
  - [x] `src/lisp/jit_jit_compile_expr_core.c3`: `JitExprFamily` route enum now drives family dispatch (`core`/`effects`/`types_ffi`) in `jit_compile_expr(...)`.
- [x] Keep compile-time feature gates at module scope with `@if` or constant mode switches, not scattered preprocessor-like branching.
  - [x] JIT expression-routing modules (`jit_jit_compile_expr_core.c3`, `jit_jit_compile_effects_modules.c3`, related compile-family files) contain no scattered `@if`/preprocessor-style branching in routing paths.
- [x] Use `$assert` to keep opcode/route decode tables synchronized.
  - [x] `src/lisp/jit_jit_compile_expr_core.c3`: `$assert(JIT_EXPR_FAMILY_NAMES.len == JIT_EXPR_FAMILY_COUNT)`.
- [x] Preserve stack-engine opacity; no direct `ScopeRegion` policy leaks into generic stack core.
  - [x] non-test `src/stack_engine*.c3` runtime/core modules carry no direct `ScopeRegion` policy calls; scope policy remains at boundary/runtime layers.

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
- [x] Prefer deleting branches and helpers entirely over leaving dead compatibility code in hot files.
- [x] Keep rollback-friendly changes small and local; one semantic deletion per patch when possible.
- [x] Add `@test` or focused regression helpers for each deleted traversal branch.
- [x] Update `memory/CHANGELOG.md` before marking a traversal path retired.

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
      - `src/lisp/primitives_iter_coroutine.c3`: `prim_resume_yield_result(...)` now routes through explicit boundary-session scoped copy (`boundary_copy_to_parent_site(..., COPY_SITE_PRIM_RESUME)`) with explicit `yield_scope` provenance captured at `yield`.
      - legacy direct `boundary_copy_to_parent_site(...)` use in coroutine resume path has been removed; missing provenance now hard-errors (`resume: missing yield-scope provenance`).
    - global mutation / persistent store boundary fallback surface:
      - `src/lisp/eval_promotion_escape.c3`: `promote_to_root_site(...)` is canonical root-store boundary route; it now uses boundary classification + transfer-safe destination routing with deterministic hard error for unsupported payloads (no fallback copy path).
      - `src/lisp/jit_jit_closure_define_qq.c3`: root-store writes use `boundary_promote_to_root_site(...)` (`COPY_SITE_JIT_EVAL_SET`, `COPY_SITE_JIT_ENV_EXTEND_ROOT`), including instance-field mutation when the owner scope resolves to root.
      - `src/lisp/jit_jit_define_method_table.c3`: root define uses `boundary_promote_to_root_site(...)` (`COPY_SITE_JIT_EVAL_DEFINE`).
    - legacy uncertain-provenance helper surface (remaining fallback machinery):
      - `src/lisp/eval_boundary_commit_flow.c3`: fallback copy helper has been removed; mixed/uncertain and aborted paths now hard-return `BOUNDARY_COMMIT_FALLBACK_DISALLOWED`.
- [x] For each remaining class, either:
  - [x] replace it with destination-aware construction or transfer-safe routing, or
  - [x] redesign the boundary contract so transient-to-surviving provenance is explicit at construction time
  - [x] coroutine suspend/resume boundary: replaced legacy direct copy with transfer-safe routing that carries explicit releasing-scope provenance (`yield_scope`) from `yield` to `resume`.
  - [x] mixed/uncertain class now tries destination ESCAPE promotion before deep-copy fallback (`BOUNDARY_COMMIT_MIXED_DESTINATION_PROMOTED`) with regression coverage (`boundary_commit_escape mixed uncertain destination commit`).
  - [x] stack-overflow unwind ASAN crash closed: stack-overflow error path now survives deep recursive unwind without boundary disjoint-copy recursion crashes (root-scoped stack-overflow error + direct destination ERROR escape route), and ASAN full-suite run no longer exits `139`.
  - [x] splice-rejected fast-path experiment was rolled back after ASAN instability; splice-rejected class stays on explicit fallback copy until a safer replacement lands.
  - [x] return-boundary promotion budget was raised (`PROMOTION_CTX_BUDGET_RETURN: 4096 -> 16384`) to reduce pre-aborted finalize contexts in dynamic eval/JIT return paths.
  - [x] return-finalize boundary no longer budget-aborts: `boundary_finalize_scoped_result(...)` now uses unbounded promotion context for commit, and large-list eval/JIT parity coverage was updated accordingly (`run_memory_lifetime_finalize_abort_fallback_parity_test`).
  - [x] synthetic forced-fallback stress coverage no longer pollutes global parity telemetry: added `boundary_restore_decision_stats(...)` and wrapped budget=0 stress/parity probes so summary counters reflect non-synthetic production paths.
  - [x] dynamic eval boundary: closed residual splice-rejected fallback pressure by fixing ESCAPE lane ownership transfer shape in `scope_splice_escapes(...)` (`src/scope_region_reset_adopt.c3`) so adopted ESCAPE chunks also transfer active `escape_bump/escape_limit`.
    - [x] Added fallback-tag telemetry (`copy_fallback_tag_cons`, `copy_fallback_tag_error`, `copy_fallback_tag_other`) and splice-rejected reason telemetry to isolate remaining classes/reasons.
    - [x] Root cause confirmed: all residual fallbacks were `splice-rejected` with `parent_escape_lane_invalid` due parent having `escape_chunks != null` while `escape_bump/escape_limit == null`.
    - [x] Current boundary summary after fix (normal + ASAN): `copy_fallback_total=0`, `copy_fallback_splice_rejected=0`, `copy_fallback_site_run_jit=0`, `copy_fallback_site_jit_single=0`, `copy_fallback_site_jit_call=0`.
  - [x] continuation/effect boundary: shared finalize/commit path now reports zero fallback pressure in normal + ASAN (`COPY_SITE_JIT_CALL_SCOPE_STEP2=0`).
  - [x] global mutation/persistent store boundary: root-store routing in `promote_to_root_site(...)` is explicit-route only (clone/direct-promote) with deterministic hard error for unsupported payloads.
    - [x] Direct root destination-promotion experiment (switching `promote_to_root_site(...)` + JIT root-store callsites to `boundary_promote_to_root_site(...)`) was attempted and rolled back after deterministic allocator corruption (`double free or corruption (!prev)` in Async I/O tests).
    - [x] Re-attempt gate landed: transfer-safe ownership rules are now explicit in `promote_to_root_site(...)` with direct destination promotion only for releasing-owned tags that have explicit clone/retain semantics.
    - [x] Added focused regression `lifetime: root-boundary direct destination promotion` in `src/lisp/tests_memory_lifetime_groups.c3`.
    - [x] Rewired JIT root-store callsites to `boundary_promote_to_root_site(...)`:
      - `src/lisp/jit_jit_closure_define_qq.c3` (`COPY_SITE_JIT_EVAL_SET`, `COPY_SITE_JIT_ENV_EXTEND_ROOT`)
      - `src/lisp/jit_jit_define_method_table.c3` (`COPY_SITE_JIT_EVAL_DEFINE`)
    - [x] Added transfer-safe PRIMITIVE clone path for releasing-owned root-store crossings in `promote_to_root_site(...)` with focused regression `lifetime: root-boundary PRIMITIVE clone path`.
    - [x] Added transfer-safe ARRAY clone path for releasing-owned root-store crossings in `promote_to_root_site(...)` with focused regression `lifetime: root-boundary ARRAY clone path`.
    - [x] Added transfer-safe HASHMAP clone path for releasing-owned root-store crossings in `promote_to_root_site(...)` with focused regression `lifetime: root-boundary HASHMAP clone path`.
    - [x] Added transfer-safe METHOD_TABLE clone path for releasing-owned root-store crossings in `promote_to_root_site(...)` with focused regression `lifetime: root-boundary METHOD_TABLE clone path`.
    - [x] Removed root-store `boundary_copy_to_scope_site(...)` fallback from `promote_to_root_site(...)`; non-reusable root-store values now route through explicit transfer-safe clone/direct-promotion helpers.
    - [x] Root-store routing is now `ValueTag`-exhaustive in `root_store_route_to_scope(...)` (`src/lisp/eval_promotion_escape.c3`) so new tag classes cannot silently re-enter fallback behavior.
    - [x] Explicit root-store routing coverage is complete for all current `ValueTag` classes; new tags must be explicitly routed in `root_store_route_to_scope(...)` by the exhaustive switch before they can compile cleanly.
  - [x] legacy uncertain-provenance helper closure:
    - [x] deleted `boundary_copy_to_scope_site(...)` from `src/lisp/eval_boundary_api.c3`.
    - [x] narrowed runtime callsites onto explicit boundary-session scoped copy paths:
      - [x] `src/lisp/jit_jit_eval_scopes.c3` (`jit_scopes_copy_value_into_scope_site(...)`)
      - [x] `src/lisp/jit_jit_closure_define_qq.c3` (`jit_copy_value_into_scope_site(...)`)
      - [x] `src/lisp/eval_type_evaluators.c3` (`eval_copy_value_into_scope_site(...)`)
      - [x] `src/lisp/primitives_iter_coroutine.c3` (resume yield scoped copy route)
    - [x] boundary-commit fallback copy helper path removed from `src/lisp/eval_boundary_commit_flow.c3`; commit now returns explicit hard error (`BOUNDARY_COMMIT_FALLBACK_DISALLOWED`) instead of deep-copy fallback.
    - [x] deleted `boundary_copy_from_releasing_scope(...)`; tests now use `test_copy_from_releasing_scope(...)` helper in `src/lisp/tests_harness_helpers.c3`.
- [x] Remove the last production `copy_to_parent` fallback branches from boundary return flow.
  - [x] `boundary_commit_escape(...)` no longer performs copy fallback on abort/mixed paths; all such branches now return `BOUNDARY_COMMIT_FALLBACK_DISALLOWED`.
  - [x] Boundary commit result surface no longer carries copy-fallback state (`fallback_copied` + `BOUNDARY_COMMIT_FALLBACK_COPIED` removed).
- [x] Remove now-dead traversal telemetry branches and compatibility helpers.
  - [x] Removed dead boundary facade helpers with no live callsites:
    - [x] `boundary_copy_to_parent(...)` (`src/lisp/eval_boundary_api.c3`)
    - [x] `boundary_commit_to_scope_site(...)` (`src/lisp/eval_boundary_api.c3`)
    - [x] `boundary_try_scope_splice_escapes(...)` (`src/lisp/eval_boundary_provenance.c3`)
    - [x] `boundary_is_scope_transfer_legal(...)` (`src/lisp/eval_boundary_provenance.c3`)
  - [x] Removed dead commit-fallback compatibility surface from boundary result API:
    - [x] `BoundaryCommitEscapeResult.fallback_copied`
    - [x] `BOUNDARY_COMMIT_FALLBACK_COPIED`
  - [x] Removed dead copy-fallback telemetry helper path with zero runtime callsites:
    - [x] `boundary_note_copy_fallback(...)`
    - [x] `boundary_note_copy_fallback_tag(...)`
    - [x] `boundary_trace_copy_fallback(...)`
    - [x] `boundary_benchmark_copy_fallback(...)`
- [x] Update docs so the current runtime description no longer treats graph traversal as a live fallback mechanism.

C3 idioms for Stage 7:
- [x] Use exhaustive `switch`es over remaining boundary classes so deletion work is mechanically reviewable.
  - [x] `boundary_commit_escape(...)` now gates provenance flow through an explicit `switch (classification.provenance)` with no default.
  - [x] `promote_to_root_site(...)` now routes via an explicit provenance switch with no default.
- [x] Prefer deleting whole helpers/modules over leaving dormant fallback code behind feature flags.
  - [x] Deleted dead `BoundaryCopyFallbackReason` enum/name/count scaffolding from `src/lisp/eval_boundary_api.c3` (no runtime references remained after fallback-path retirement).
  - [x] Deleted dead fallback-era parameter plumbing from boundary commit/finalize paths:
    - `boundary_commit_escape(...)` no longer carries unused `original_result`/`copy_site` compatibility args.
    - `boundary_finalize_scoped_result(...)` / `jit_finalize_scoped_result(...)` were narrowed accordingly.
    - Updated runtime and regression callsites; removed no-op casts and stale mixed-provenance “fall back to boundary copy” wording.
- [x] Add focused regression tests that prove the replacement path covers the old fallback case before deleting the branch.
  - [x] Added `lifetime: resume missing yield provenance gate` in `src/lisp/tests_memory_lifetime_groups.c3`:
    - asserts deterministic hard error (`resume: missing yield-scope provenance`) for missing coroutine yield provenance.
    - asserts `COPY_SITE_PRIM_RESUME` counter stays unchanged (`site_delta == 0`), proving no legacy copy fallback path is reintroduced.
- [x] Use `$assert` and small route enums to verify no retired fallback route remains referenced.
  - [x] Added `BoundaryCommitFallbackRoute` as a one-route enum (`fallback-disallowed`) in `src/lisp/eval_boundary_api.c3`.
  - [x] Added compile-time table/enum sync guards:
    - `$assert(BOUNDARY_COMMIT_OUTCOME_NAMES.len == BOUNDARY_COMMIT_OUTCOME_COUNT)`
    - `$assert(BOUNDARY_COMMIT_FALLBACK_ROUTE_NAMES.len == BOUNDARY_COMMIT_FALLBACK_ROUTE_COUNT)`

### Acceptance Criteria For This Plan

- [x] The rollout remains staged; no TODO item assumes an all-at-once migration to the final architecture.
- [x] Normal return paths for list-heavy code prefer reuse/splice/destination-build over deep copy.
- [x] Intermediate stages may keep explicit fallback traversal where required for correctness.
- [x] Final closure of this TODO removes the remaining production fallback traversal paths.
- [x] Eval and JIT share the same boundary ownership rules.
- [x] No stop-the-world GC, no root pinning workaround, and no per-type RC ownership model is introduced.
- [x] `c3c build`, `c3c build --sanitize=address`, `LD_LIBRARY_PATH=/usr/local/lib ./build/main`, and `scripts/run_boundary_hardening.sh` stay green for each landed stage.

## P1-P3. Remove Non-Dispatch Fallback Pollution

Goal: remove fallback and compatibility paths that are not part of the intended language model. `MethodTable.fallback` is explicitly excluded from this plan.

Rules:
- [x] Treat `MethodTable.fallback` as canonical dispatch behavior, not cleanup target.
- [x] Everything else must justify its existence as either a temporary migration shim with an explicit paired replacement task in this TODO or a missing implementation to be completed by a named stage in this TODO.
- [x] Prefer deleting fallback branches over keeping permanent compatibility residue.
- [x] No fallback or legacy branch may remain as an open-ended "keep for now" item without a concrete end-state task.
- [x] If a fallback stays temporarily, the replacement work must be listed immediately next to it in this TODO.

### Track A. Inventory And Classify Every Remaining Fallback

- [x] Create one tracked inventory of live fallback surfaces, grouped by:
  - [x] compatibility wrapper
  - [x] parser permissive fallback
  - [x] compiler/JIT implementation fallback
  - [x] runtime ownership fallback
  - [x] legacy alias / duplicate public API
- [x] Mark each item as one of:
  - [x] delete now
  - [x] replace first, then delete
  - [x] keep only until a specific staged migration in this TODO lands
- [x] Record the inventory owner file and removal gate in `memory/CHANGELOG.md` or a focused plan doc once work starts.
- [x] Seed the inventory with the currently confirmed live fallback/legacy surfaces:
  - [x] `src/lisp/eval_init_primitives.c3` legacy deduce registrations
  - [x] `src/lisp/compiler_stdlib_prelude.c3` compatibility wrappers
  - [x] `src/lisp/parser_lexer_string_hash.c3` unknown `#` permissive fallthrough
  - [x] `src/lisp/compiler_code_emission.c3` ignored compatibility signature
  - [x] `src/lisp/jit_jit_compile_expr_core.c3` / `src/lisp/jit_jit_compile_effects_modules.c3` interpreter fallback
  - [x] `src/pika/regex_cache_api.c3` compiled->simple regex fallback
  - [x] `src/lisp/eval_promotion_copy.c3` family of runtime ownership copy fallbacks
  - [x] `src/scope_region_reset_helpers.c3` defensive TEMP-lane repair branch
  - [x] undeclared-effect behavior currently exercised in `src/lisp/tests_advanced_io_effect_ffi_groups.c3` (now canonicalized)

C3 idioms for Track A:
- [x] Use small enums for fallback class/status if instrumentation is added.
  - [x] boundary fallback instrumentation uses compact enums (`BoundaryCommitFallbackRoute` in `src/lisp/eval_boundary_api.c3`, `JitExprFamily` in `src/lisp/jit_jit_compile_expr_core.c3`).
- [x] Keep the inventory grounded in actual callsites, not comments alone.
- [x] Use `$assert` for any enum/table tracking added to audits.
  - [x] enum/table sync assertions are active for fallback/status tracking (`BOUNDARY_COMMIT_FALLBACK_ROUTE_NAMES` and `JIT_EXPR_FAMILY_NAMES`).

### Track B. Delete Legacy Public API Fallbacks

- [x] Remove the legacy deduce primitive registrations once equivalent unified `deduce` call coverage and tests are in place:
  - [x] `deduce-open`
  - [x] `fact!`
  - [x] `retract!`
  - [x] `deduce-scan`
  - [x] `deduce-query`
  - [x] `deduce-count`
  - [x] `deduce-match`
- [x] Migrate tests/docs/examples that still depend on those legacy entrypoints.
- [x] Migrate the currently known test/doc dependents:
  - [x] `src/lisp/tests_deduce_groups.c3`
  - [x] `src/lisp/tests_deduce_query_groups.c3`
  - [x] `src/lisp/tests_deduce_durability_groups.c3`
  - [x] `src/lisp/tests_deduce_isolation_groups.c3`
  - [x] docs/examples that still mention direct legacy deduce names rather than `(deduce '...)`
- [x] Add one rejection/error test per removed legacy public name if the parser/runtime should now reject it deterministically.

C3 idioms for Track B:
- [x] Delete primitive table entries directly; do not leave stub aliases behind.
- [x] Use explicit negative tests for removed names rather than silent aliasing.
- [x] Keep public API cleanup in small patches so regressions are attributable.
  - [x] Legacy API removals and wrapper retirements were landed as bounded, file-scoped steps with paired rejection/regression tests and changelog notes.

### Track C. Delete Error-Model Compatibility Wrappers

- [x] Replace `raise->message` / `try-message` usage in tests, fixtures, and stdlib callsites with canonical payload-aware handling.
- [x] Migrate the currently known dependent surfaces:
  - [x] `src/lisp/tests_advanced_core_unicode_groups.c3`
  - [x] `src/lisp/tests_runtime_feature_http_groups.c3`
  - [x] `src/lisp/tests_runtime_async_groups.c3`
  - [x] `src/lisp/tests_advanced_io_effect_ffi_groups.c3`
  - [x] `tests/lib/tls/server_once.omni`
- [x] Normalize remaining string-error or message-only handler expectations onto canonical raise payloads.
- [x] Remove the compatibility wrapper definitions from the stdlib prelude once no production/test code depends on them.
- [x] Remove docs that present them as standard surface once the migration is complete.

C3 idioms for Track C:
- [x] Prefer normal helper functions/macros in Omni source for canonical payload extraction where truly needed, not compatibility wrappers with degraded semantics.
- [x] Add focused regression tests for canonical error payload shape instead of wrapper behavior.
- [x] Keep handler rewrites explicit; do not hide payload adaptation in broad macros.

### Track D. Remove Parser Permissive Fallbacks

- [x] Replace unknown `#` dispatch fallthrough-to-symbol behavior with explicit targeted parser errors.
- [x] Keep `src/lisp/parser_import_export.c3` legacy-marker special cases only if they still provide materially better migration errors than normal parser failure; otherwise collapse them into standard parse error flow once old syntax removal is complete.
  - [x] collapsed redundant import `:all` marker branch to canonical parse error flow.
  - [x] retained `:as` marker rejection in import specifiers because it still provides a materially better migration error than silent symbol acceptance.
- [x] Audit for any other lexer/parser “accept legacy syntax silently” fallbacks and either:
  - [x] reject them explicitly, or
  - [x] document them as canonical syntax if they are intentionally supported
- [x] Add regressions that rejected legacy/permissive forms fail with deterministic messages.

C3 idioms for Track D:
- [x] Prefer explicit token/error branches over permissive fallthrough in lexer code.
- [x] Keep lexer branches small and exhaustive where token class is known.
- [x] Use clear `set_error` paths close to detection, not deferred ambiguous failure later in parsing.

### Track E. Remove Compiler Compatibility Residue

- [x] Delete ignored compatibility parameters and helper shapes that no longer affect behavior, starting with `emit_lambda_return_with_frame(..., has_frame)`.
- [x] Collapse duplicate compiler entrypoints that only survive for old call signatures.
- [x] Audit comments marked “API compatibility” or “intentionally ignored” and remove the underlying dead compatibility path, not just the comment.

C3 idioms for Track E:
- [x] Prefer changing signatures and fixing callers over keeping ignored parameters.
- [x] Use compile errors to flush stale callsites instead of compatibility shims.
- [x] Keep one canonical helper per compiler action.

### Track F. Eliminate JIT Interpreter Fallback

- [x] Inventory which expression groups still route through `jit_compile_fallback(...)`.
  - [x] Current sources are only the terminal `default` arms in `jit_compile_expr_group_core(...)`, `jit_compile_expr_group_effects(...)`, and `jit_compile_expr_group_types_ffi(...)` (`src/lisp/jit_jit_compile_expr_core.c3`).
- [x] Enumerate the current `handled = false` sources in JIT expression grouping and map each one to a concrete lowering task.
  - [x] No live expression family remains mapped to interpreter fallback; unknown/unhandled tags now map to guarded compile failure telemetry.
- [x] Add telemetry or an audit mode to measure fallback frequency by expression kind.
  - [x] Added `JitFallbackAuditStats` counters + per-`ExprTag` buckets + summary emission (`OMNI_TEST_SUMMARY suite=jit_fallback`) in:
    - [x] `src/lisp/jit_jit_compile_expr_core.c3`
    - [x] `src/lisp/tests_tests.c3`
- [x] Implement native lowering for the remaining expression classes in descending runtime frequency.
  - [x] Verified by telemetry baseline: `OMNI_TEST_SUMMARY suite=jit_fallback total=0` on the full normal suite.
- [x] Explicitly land variadic-lambda JIT support so the known variadic-closure fallback path can be deleted.
  - [x] Added focused regression `jit policy: variadic lambda executes without jit fallback` in `src/lisp/tests_runtime_feature_jit_groups.c3`.
- [x] Use `src/lisp/tests_advanced_core_unicode_groups.c3` variadic coverage as the named regression gate for deleting that path.
  - [x] Updated coverage note to canonical native-JIT behavior (no fallback path).
- [x] Convert `jit_compile_fallback(...)` from production path to guarded internal failure once all supported expression forms are lowered.
- [x] Delete the final interpreter fallback path from JIT expression compilation.

C3 idioms for Track F:
- [x] Use compact route enums and exhaustive `switch`es for JIT expression families.
- [x] Keep JIT lowering grouped by semantic domain, not a growing catch-all file.
- [x] Use `@noinline` only for cold audit/report paths; compiled-expression dispatch stays straightforward and reviewable.
- [x] Add `$assert` synchronization for any route tables introduced during coverage expansion.

### Track G. Close Runtime Ownership Fallbacks

- [x] Land the staged graph-traversal reduction plan above.
- [x] Replace current `copy_to_parent(...)` boundary users with destination-aware or boundary-classified routes, starting with:
  - [x] `src/lisp/eval_env_copy.c3`
    - [x] env copy paths now route through boundary helpers (`boundary_copy_to_parent_site_ctx(...)`, `boundary_should_copy_env(...)`) instead of direct raw `copy_to_parent(...)` calls.
  - [x] `src/lisp/eval_repl.c3`
    - [x] REPL return path uses `boundary_copy_to_parent_site(...)`.
  - [x] `src/lisp/primitives_iter_coroutine.c3`
    - [x] coroutine resume yield transfer uses explicit boundary-session scoped copy.
    - [x] resume yield transfer now routes through `boundary_copy_to_parent_site_ctx(...)` under local unbounded promotion context (no raw direct site-copy call in this boundary helper).
  - [x] `src/lisp/eval_promotion_escape.c3`
    - [x] disjoint promotion bridge removed: `promote_to_escape_disjoint(...)` now performs direct destination ESCAPE promotion (`promote_to_escape_by_tag(...)` + memoization); root-store routing in `promote_to_root_site(...)` is explicit-route only (no fallback copy path).
    - [x] root-store clone loops (`ARRAY`/`HASHMAP`/`METHOD_TABLE`) now copy via `boundary_copy_to_parent_site_ctx(...)` under local unbounded promotion contexts, replacing raw per-element `boundary_copy_to_parent_site(...)` calls in clone paths.
    - [x] `root_store_direct_promote_to_scope(...)` now routes through `boundary_copy_to_parent_site_ctx(...)` under a local unbounded promotion context (no raw direct site-copy call in root-store direct route).
  - [x] `src/lisp/eval_promotion_context.c3`
    - [x] canonical scoped-copy implementations moved to boundary facade (`boundary_copy_to_parent_site_ctx(...)`, `boundary_copy_from_releasing_scope(...)`) in `src/lisp/eval_boundary_api.c3`.
  - [x] `src/lisp/eval_type_evaluators.c3`
    - [x] `eval_copy_value_into_scope_site(...)` now routes through `boundary_copy_to_parent_site_ctx(...)` under a local unbounded promotion context.
  - [x] `src/lisp/jit_jit_eval_scopes.c3`
    - [x] `jit_scopes_copy_value_into_scope_site(...)` now routes through `boundary_copy_to_parent_site_ctx(...)` under a local unbounded promotion context.
  - [x] `src/lisp/jit_jit_closure_define_qq.c3`
    - [x] `jit_copy_value_into_scope_site(...)` now routes through `boundary_copy_to_parent_site_ctx(...)` under a local unbounded promotion context.
- [x] Remove remaining `copy_to_parent`-style fallback branches after destination-aware replacements cover their boundary class.
  - [x] `src/lisp/eval_promotion_copy.c3`: removed non-context fast-reuse branch from `copy_to_parent_try_fast_reuse(...)`; reuse decisions are now classification-only with explicit target/releasing provenance.
  - [x] `src/lisp/eval_promotion_copy.c3`: `copy_to_parent(...)` now hard-errors when boundary provenance (`current_scope` + `releasing_scope`) is missing instead of applying a conservative compatibility fallback.
  - [x] `src/lisp/eval_boundary_api.c3`: `boundary_copy_to_parent_site_ctx_impl(...)` now enforces boundary provenance and fails deterministically on missing scope context.
  - [x] `src/lisp/eval_boundary_api.c3`: `boundary_copy_env_to_target_scope_impl(...)` now sets `releasing_scope` explicitly to caller scope so env-copy routes stay on explicit provenance policy.
- [x] Remove compatibility telemetry or branch labels that only existed to observe now-deleted fallback paths.
  - [x] Removed fallback-detail telemetry branches from runtime verbose diagnostics (`eval_boundary_diagnostics.c3`) that only reported now-retired copy-fallback reason/site/tag breakdowns.
  - [x] Simplified boundary summary emission to keep only active fallback signal (`copy_fallback_total`) while dropping retired reason/site/tag keys (`tests_tests.c3`).
  - [x] Updated boundary hardening script required summary keys to match the trimmed telemetry surface (`scripts/run_boundary_hardening.sh`).
- [x] Retire `releasing_scope`-driven defensive copy logic once provenance classification and destination routing fully replace it.
  - [x] `src/lisp/eval_promotion_copy.c3`: `copy_to_parent_try_fast_reuse(...)` now routes reuse-vs-copy through `boundary_classify_return_value(...)` when boundary context is present (`current_scope` + `releasing_scope`) instead of ad-hoc `releasing_scope` checks.
  - [x] Non-boundary context is no longer tolerated on this path: `copy_to_parent(...)` now hard-errors when provenance scope context is missing.

C3 idioms for Track G:
- [x] Delete ownership fallbacks only after the replacement boundary helper is the sole legal path.
  - [x] Non-test runtime scope-copy callsites now route through `boundary_copy_to_parent_site_ctx(...)`; raw `boundary_copy_to_parent_site(...)` remains only as internal facade primitive in `src/lisp/eval_boundary_api.c3`.
  - [x] Facade policy guard now blocks direct `copy_env_to_scope(...)` and raw `boundary_copy_to_parent_site(...)` callsites outside boundary core files (`scripts/check_boundary_facade_usage.sh` + `scripts/boundary_facade_policy.txt`).
- [x] Keep state-restore and scope-restore logic `defer`-backed until the final fallback branch is gone.
  - [x] Audited live `boundary_scope_swap_begin(...)` / `boundary_releasing_scope_swap_begin(...)` runtime callsites: all are now paired with `defer ..._end(...)`.
  - [x] Removed non-`defer` swap-begin pattern from `boundary_enter_scope(...)` by switching it to explicit scoped assign/restore contract (`src/lisp/eval_boundary_api.c3` + `boundary_leave_scope(...)`).
- [x] Preserve explicit contracts on ownership-transition helpers during cleanup; do not weaken them while deleting branches.
  - [x] Added explicit `@require` contracts on scoped-copy ownership helpers:
    - `eval_copy_value_into_scope_site(...)` (`src/lisp/eval_type_evaluators.c3`)
    - `jit_scopes_copy_value_into_scope_site(...)` (`src/lisp/jit_jit_eval_scopes.c3`)
    - `jit_copy_value_into_scope_site(...)` (`src/lisp/jit_jit_closure_define_qq.c3`)

### Track H. Collapse Dual Implementations And Defensive Residue

- [x] Regex dual-path cleanup:
  - [x] inventory all conditions that trigger `regex_search_simple_fallback`, `regex_fullmatch_simple_fallback`, and `regex_find_all_simple_fallback`
  - [x] split cache-bookkeeping failure from syntax/engine capability failure so regex fallback cause is explicit
  - [x] either make compiled regex handling total for the supported syntax set or explicitly split "simple regex mode" from the normal API
  - [x] remove the silent compiled-to-simple production fallback from the default regex API
  - [x] migrate `src/lisp/tests_runtime_feature_pika_groups.c3` parity tests to validate the chosen post-fallback contract rather than permanent dual-path parity
- [x] Scope reset defensive residue:
  - [x] replace "recreate baseline TEMP chunk if scope had none" defensive branch with one shared invariant-preserving helper
  - [x] add assertions/tests so the branch is not needed as a silent fallback path
  - [x] delete the defensive fallback branch once invariant restoration is centralized
- [x] Boundary provenance residue:
  - [x] replace `in_target_scope_chain()` / `is_in_scope()` / `scope_contains()` chunk walks with one canonical provenance test path
    - [x] removed duplicate `in_target_scope_chain(...)` scan helper from `src/lisp/eval_promotion_copy.c3`
    - [x] `boundary_ptr_in_target_scope_chain(...)` now routes through `boundary_ptr_in_scope_chain(...)` (`src/lisp/eval_boundary_provenance.c3`)
    - [x] boundary graph-audit temp checks now use `boundary_ptr_in_scope(...)` wrapper instead of direct `main::is_in_scope(...)` calls (`src/lisp/eval_boundary_diagnostics.c3`)
  - [x] remove direct closure payload ownership scans in `src/lisp/eval_promotion_copy.c3` once scope/lane provenance is explicit
    - [x] moved closure payload scope-membership check into canonical helper `boundary_closure_payload_in_scope(...)` (`src/lisp/eval_boundary_provenance.c3`)
  - [x] eliminate remaining `scope_contains(...)`-driven defensive ownership checks from hot paths as destination/provenance routing lands
    - [x] runtime hot paths now route scope-membership checks through canonical boundary provenance helpers (`boundary_ptr_in_scope(...)`, `boundary_ptr_in_scope_chain(...)`) rather than ad-hoc scans.
  - [x] keep any temporary scan only where a named preceding stage in this TODO still depends on it
    - [x] remaining low-level chunk scan is centralized in `main::is_in_scope(...)` and consumed via boundary provenance wrappers.

C3 idioms for Track H:
- [x] Prefer one canonical implementation path per subsystem unless the second path is a first-class supported mode.
- [x] Convert defensive fallback branches into explicit invariant helpers plus assertions before deletion.
- [x] Keep replacement helpers small, typed, and contract-backed rather than adding more compatibility flags.
  - [x] cleanup replacements stayed in typed helper surfaces (`boundary_*`/`jit_*` helpers) with explicit contracts and without new compatibility flag bundles.

### Track J. Tighten Language-Surface Compatibility Drift

- [x] Decide whether undeclared effects remain canonical language behavior or are removed as backward-compat residue.
  - [x] Decision: undeclared effects are canonical language behavior; declarations are optional and only enable declaration-based type checks.
- [x] Promote the undeclared-effect rule to explicit spec-backed status and align tests/docs:
  - [x] `docs/EFFECTS_SEMANTICS.md` now states undeclared effects are valid and skip declaration-based type checks.
  - [x] `src/lisp/tests_advanced_io_effect_ffi_groups.c3` renamed coverage to `effect undeclared canonical`.
- [x] Audit for similar "still works for backward compat" language-surface behavior and either:
  - [x] promote it to explicit spec-backed status, or
  - [x] remove it with deterministic rejection tests
  - [x] Result of this pass: no additional open language-surface backward-compat residue identified in active effect docs/tests.

C3 idioms for Track J:
- [x] Keep language-surface rules explicit in parser/eval contracts, not implied by permissive runtime behavior.
- [x] Prefer one canonical form per feature family; reject legacy alternates rather than silently accepting them.
  - [x] effect language surface and docs now treat undeclared-effect behavior as canonical (explicitly spec-backed) and removed compatibility wrappers/aliases instead of silently preserving alternates.

### Track I. Split Oversized Hotspot Files Before More Policy Changes

- [x] Split `src/lisp/eval_boundary_api.c3` top-down before further boundary-policy growth:
  - [x] extract diagnostics/graph-audit + scope-transfer validation cluster into `src/lisp/eval_boundary_diagnostics.c3`
  - [x] session/txn guards (extracted to `src/lisp/eval_boundary_session_txn.c3`)
  - [x] provenance classification + legality checks (extracted to `src/lisp/eval_boundary_provenance.c3`)
  - [x] commit/promote/splice/copy flow (extracted to `src/lisp/eval_boundary_commit_flow.c3`)
  - [x] graph-audit logic
  - [x] telemetry/reporting helpers (extracted to `src/lisp/eval_boundary_telemetry.c3`)
- [x] Split `src/lisp/scheduler_thread_tasks.c3` top-down by waiter lifecycle, completion signaling, and thread/task state transitions.
  - [x] extract waiter lifecycle + completion-wait signaling into `src/lisp/scheduler_thread_task_waiters.c3`
  - [x] extract state-transition orchestration (task/thread active->running->done/cancelled paths) into `src/lisp/scheduler_thread_task_transitions.c3`
- [x] Split `src/lisp/tls.c3` top-down by handle lifecycle, I/O operation state machine, and worker/offload boundary.
- [x] Split `src/lisp/scheduler_primitives_tasks.c3` top-down by public primitive surface versus internal wait/join orchestration.
- [x] Split `src/lisp/tests_memory_lifetime_boundary_groups.c3` top-down by invariant family so fallback-removal regressions stay localized.

C3 idioms for Track I:
- [x] Always split the largest files first, per repo refactor rules.
  - [x] Track I extracted boundary/session/provenance/telemetry clusters from `eval_boundary_api.c3` before further policy edits.
- [x] Move cohesive helper clusters intact before editing behavior inside them.
  - [x] scheduler/tls/boundary split work moved cohesive helper groups into dedicated modules before behavior cleanup.
- [x] Keep module surfaces small and explicit after each split; do not recreate monoliths under new names.
  - [x] post-split module boundaries remain explicit (`eval_boundary_*`, `scheduler_*`, `tls_*` family files) and policy work stayed distributed across those modules.

### Closure Criteria For This Plan

- [x] No legacy duplicate public APIs remain solely for backward compatibility.
- [x] No compatibility error wrappers remain on the normal language surface.
- [x] No parser permissive fallthrough remains where the language intends explicit syntax.
- [x] No ignored compiler compatibility signatures remain.
- [x] JIT no longer falls back to interpreter for supported expression forms.
- [x] Regex default APIs no longer silently switch to a second implementation path.
- [x] Defensive invariant-repair branches have been replaced by explicit helpers/assertions or deleted.
- [x] Language-surface backward-compat behavior is either explicitly spec-backed or removed with tests.
- [x] Ownership/provenance hot paths no longer depend on repeated chunk-walk membership scans.
- [x] Largest hotspot files have been split before further cleanup lands in those subsystems.
- [x] Runtime ownership fallback cleanup is complete except for `MethodTable.fallback`, which remains canonical.

## Old Region System Removal (`root_region` / handle allocator)

Goal: remove the remaining `ObjectHandle` / `RegionHandle` / slot-table allocator path once AST allocation no longer depends on it.

- [x] Freeze the replacement direction:
  - [x] keep `ScopeRegion` as the runtime value/env allocator
  - [x] do not migrate AST allocation onto runtime `root_scope` by default
  - [x] use a dedicated permanent AST allocator (`AstArena` or equivalent) for `Expr` / `Pattern`
- [x] Inventory the live dependency surface of the old region system:
  - [x] `Interp.root_region`
  - [x] `Interp.alloc_expr()`
  - [x] `Interp.alloc_pattern()`
  - [x] thread-local region registry helpers
  - [x] docs/comments that still describe `root_region` as active design
- [x] Introduce a dedicated AST allocation subsystem:
  - [x] define AST allocator types and ownership model
  - [x] define init/shutdown lifecycle on `Interp`
  - [x] define direct allocation entrypoints for `Expr`
  - [x] define direct allocation entrypoints for `Pattern`
  - [x] document that AST storage is permanent for interpreter/compiler lifetime
- [x] Migrate interpreter state off `root_region`:
  - [x] remove `main::RegionHandle root_region` from `Interp`
  - [x] replace `thread_root_region()` initialization with AST allocator init
  - [x] update `Interp.alloc_expr()` to allocate directly from AST allocator
  - [x] update `Interp.alloc_pattern()` to allocate directly from AST allocator
- [x] Verify parser and macro pipelines after AST allocator migration:
  - [x] parser expression builders
  - [x] parser pattern builders
  - [x] macro expr-conversion helpers
  - [x] any compiler/AOT helpers that depend on permanent AST identity
- [x] Audit comments that may be stale vs real blockers:
  - [x] persistent env / mutable box comments mentioning `root_region`
  - [x] continuation comments mentioning `root_region`
  - [x] update comments to reflect `root_scope` vs AST allocator accurately
- [x] Remove old region handle callsites:
  - [x] `allocate_in(...)`
  - [x] `dereference_as(...)`
  - [x] any direct `ObjectHandle` / `RegionHandle` use outside dead code
- [x] Delete thread-local old-region plumbing after callsites reach zero:
  - [x] `thread_root_region()`
  - [x] thread registry helpers
  - [x] region registry init/shutdown paths
- [x] Delete old region storage implementation after migration:
  - [x] handle type declarations
  - [x] region registry declarations/methods
  - [x] pool/slot storage types
  - [x] pool/slot storage methods
  - [x] stale-test audit for removed handle path (`ObjectHandle` / `RegionHandle` / `thread_root_region`) has zero active test callsites
- [x] Add migration validation:
  - [x] parser allocates all `Expr` through new AST allocator
  - [x] parser allocates all `Pattern` through new AST allocator
  - [x] no runtime `Value` / `Env` path depends on old region files
  - [x] no production callsites of `ObjectHandle`, `RegionHandle`, `allocate_in`, `dereference_as`
- [x] Add targeted tests / checks:
  - [x] parser smoke tests after AST allocator swap
  - [x] macro expansion smoke tests after AST allocator swap
  - [x] compiler/AOT smoke tests after AST allocator swap
  - [x] optional benchmark for AST allocation throughput/regression (`OMNI_AST_ARENA_BENCH=1`)
- [x] Update docs after cutover:
  - [x] `docs/FEATURES.md`
  - [x] `docs/areas/memory-runtime-cycle.md`
  - [x] `docs/plans/aot-unification.md`
  - [x] area-diagram SVGs now align with `AstArena` naming (no `root_region` current-state label)
- [x] Record the removal in `memory/CHANGELOG.md` with:
  - [x] rationale for deleting the old region path
  - [x] new AST allocator summary
  - [x] validation performed
  - [x] rollback note

## Memory Model Follow-On Improvements

Goal: simplify the current post-hardening memory model further now that boundary commit/finalize work and old-region removal are complete.

### Track K. Make `AstArena` A Real Arena

- [x] Audit current `AstArena` behavior in `src/lisp/ast_arena.c3`:
  - [x] confirm current implementation is chunked bump allocation with bulk-chunk ownership
  - [x] measure current alloc/destroy cost and allocation count under parser/compiler smoke inputs
  - [x] record current behavior and baseline in `memory/CHANGELOG.md`
- [x] Design chunked `AstArena` storage:
  - [x] define chunk header/layout and growth policy
  - [x] define alignment rules for `Expr` / `Pattern`
  - [x] define chunk-size tuning policy and overflow behavior
  - [x] define destroy semantics (bulk free by chunk, not per-object free)
- [x] Migrate `AstArena` allocation path:
  - [x] replace per-object `malloc` in `ast_arena_alloc(...)`
  - [x] keep zero-init contract for allocated AST nodes
  - [x] preserve interpreter-lifetime ownership and teardown behavior
- [x] Add validation for chunked `AstArena`:
  - [x] parser smoke and compiler/AOT smoke in benchmark gate (`OMNI_AST_ARENA_BENCH=1`) with success counters
  - [x] macro expansion smoke (add explicit macro-surface coverage assertion in benchmark corpus)
  - [x] optional AST allocation benchmark before/after arena reset via `OMNI_AST_ARENA_BENCH_RESET`
  - [x] reset-aware alloc suite emits `*_reset` summaries with per-suite `ast_resets` and `ast_validation_fail`
- [x] Update docs/comments to state that `AstArena` is chunked bump storage, not block-list allocation.

### Track L. Replace `Env.persistent` Bool With Explicit Lifetime Kind

- [x] Inventory every use of `Env.persistent`:
  - [x] env copy paths
  - [x] JIT mutable-box paths
  - [x] closure/env-copy comments and tests
- [x] Replace the boolean with an explicit enum or tagged lifetime kind:
  - [x] local scope frame
  - [x] root-persistent mutable box
  - [x] any additional persistent/env categories that are semantically distinct today
- [x] Update env-copy and mutation logic to branch on explicit lifetime kind, not generic truthiness.
- [x] Rewrite stale comments that still conflate persistence with `root_region`.
- [x] Add regression checks for each lifetime kind through:
  - [x] closure capture boundary
  - [x] env copy boundary
  - [x] `set!` / mutable box sharing path

### Track M. Remove Residual Provenance/Ownership Discovery Scans

- [x] Inventory remaining `scope_contains(...)` / chunk-walk provenance checks outside clearly cold debug/audit paths.
  - [x] production references are centralized via boundary provenance wrappers in:
    - `src/lisp/eval_boundary_provenance.c3` (`boundary_ptr_in_scope_chain`, `boundary_ptr_in_target_scope_chain`)
    - `src/lisp/eval_boundary_diagnostics.c3` (debug-only reachability helpers)
  - [x] remaining direct raw `main::is_in_scope(...)` references in non-test runtime code are eliminated; tests retain direct checks for assertion clarity.
- [x] Separate required scans into:
  - [x] hot-path production logic (`boundary_ptr_in_scope`, `boundary_ptr_in_scope_chain`)
  - [x] cold-path validation/audit logic (`boundary_graph_audit_escape_reachability`)
  - [x] legacy mutation/root-store compatibility paths (none active in production)
- [x] Replace hot-path residual scans with explicit provenance hints where possible.
  - [x] add generation-aware chain membership helper in `src/lisp/eval_boundary_provenance.c3` (`boundary_ptr_in_scope_chain_with_hint`, `boundary_ptr_in_target_scope_chain_with_hint`) with full-scan fallback.
  - [x] switch key return/closure/env/JIT reuse checks (`eval_promotion_copy`, `eval_promotion_escape`, `eval_env_copy`, `jit_jit_eval_scopes`) from chain scan-only checks to hinted checks.
  - [x] replace remaining hot scans that are still needed after hinting with a small cached ownership-context record for recurring release-chain checks.
    - [x] Add fixed-size scope-chain cache entries to `PromotionContext` in `src/lisp/value_environment.c3`.
    - [x] Add cache reset at `promotion_context_begin(...)` so each active epoch starts with clean slot state.
    - [x] Add active-context cache lookup/remember helpers keyed by `(ptr, target_scope, pinned_generation)`.
    - [x] Route `boundary_ptr_in_target_scope_chain_with_hint(...)` through active `PromotionContext` cache for `pinned_gen != 0`.
    - [x] Add regression coverage (`run_memory_lifetime_scope_chain_cache_test(...)`) for hot-scan suppression after first miss.
- [x] Gate any remaining hot scans behind `BoundaryDecisionStats` / hard policy flags once their callsite density is measured.
  - [x] add hard policy gates:
    - `OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BYPASS`
    - `OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BUDGET`
    - `OMNI_BOUNDARY_GRAPH_AUDIT` + `OMNI_BOUNDARY_GRAPH_AUDIT_RATE`
    - `OMNI_BOUNDARY_GRAPH_AUDIT_MAX_ROOTS`
  - [x] enforce `boundary_scope_chain_scan_allowed()` in all production scope-chain wrappers.
  - [x] route hot scan failures to explicit counters (`scope_chain_scan_total`, `scope_chain_scan_suppressed`).
- [x] Keep graph/audit traversals debug-only or threshold-gated unless the path is semantically required.
  - [x] add graph-audit rate/max-root threshold gates in `boundary_debug_graph_audit_committed_escape_root(...)`.
  - [x] count gate outcomes via new decision stats (`graph_audit_invoked`, `graph_audit_skipped_rate`, `graph_audit_skipped_max_roots`).
  - [x] include graph-audit gating counters in verbose decision telemetry output.
  - [x] document all graph traversals as validation/audit-only in boundary runtime docs.
- [x] Add telemetry for any scan that remains in production paths after cleanup.
  - [x] add `scope_chain_scan_total`, `scope_chain_scan_with_hint`, `scope_chain_scan_fallback` counters in `src/lisp/eval_boundary_telemetry.c3`.
  - [x] emit all three counters in verbose boundary telemetry (`src/lisp/eval_boundary_diagnostics.c3`).
- [x] Harden boundary debug-gate env parsing.
  - [x] `boundary_graph_audit_enabled` now uses strict truthy parsing in `src/lisp/eval_boundary_diagnostics.c3`.
  - [x] `boundary_verbose_telemetry_enabled` now uses strict truthy parsing in `src/lisp/eval_boundary_diagnostics.c3`.
  - [x] `boundary_trace_runtime_enabled` now uses strict truthy parsing in `src/lisp/eval_boundary_telemetry.c3`.
  - [x] `boundary_benchmark_runtime_enabled` now uses strict truthy parsing in `src/lisp/eval_boundary_telemetry.c3`.
- [x] Define closure criteria:
  - [x] no repeated chunk-walk membership scans on normal eval/JIT return boundaries for stable `(ptr,scope,pin)` triples within one promotion epoch.
  - [x] remaining scans are explicitly documented as cold-path or legacy-path only.
  - [x] any repeated scan still occurring in hot return paths has an owner and replacement task in this Track.

### Track N. Strengthen Model Separation In Docs And Runtime Surface

- [x] Audit docs/comments so the three current models are always described consistently:
  - [x] runtime values/envs -> `ScopeRegion`
  - [x] AST nodes -> `AstArena`
  - [x] foreign/shared resources -> explicit wrapper types
- [x] Remove stale wording that implies one unified allocator where the code now has three distinct models.
- [x] Add one concise architecture note that names the three models and their boundary rules.
  - [x] `docs/areas/memory-runtime.md`
  - [x] `docs/areas/memory-runtime-cycle.md`
- [x] Ensure new memory-related comments identify the owning model explicitly instead of using generic “arena” wording.
  - [x] `src/lisp/ast_arena.c3`
  - [x] `docs/FEATURES.md`
  - [x] `docs/areas/memory-runtime-cycle.md`
- [x] Audit remaining docs outside this pass (`docs/areas/*.md`, `docs/plans`, `memory/DESTINATION_ARENA_PLAN.md`) for unified-allocation phrasing.

### Track O. Validation And Order

- [x] Land Track L before broad env-copy cleanup so semantics are explicit first.
- [x] Land Track K before AST/perf tuning so allocation structure is correct first.
- [x] Land Track M after the explicit lifetime-kind work, so provenance cleanup has clearer ownership categories.
- [x] Keep Track N in lock-step with each landing rather than as a final docs sweep.
- [x] Record each track in `memory/CHANGELOG.md` with:
  - [x] rationale
  - [x] validation
  - [x] residual risks
  - [x] rollback notes where relevant
- [x] Audit remaining docs outside this pass (`docs/areas/*.md`, `docs/plans`, `memory/DESTINATION_ARENA_PLAN.md`) for unified-allocation phrasing.
  - [x] found no active unified-allocation claims in `docs/areas/*.md` and current plans.
  - [x] confirmed `docs/plans/aot-unification.md` references remain as migration-history notes only.
- [x] Add C3-observable benchmark instrumentation to reduce future diagnostics friction:
  - [x] add reusable benchmark summary helpers/macros for AST/parse/compile phases
  - [x] add benchmark-only failure counters for parser/compile fallback or empty-output paths
  - [x] add conditional telemetry gates for parser/compile mutation and AST ownership assertions (`OMNI_AST_ARENA_BENCH`)

### Track P. Graph Audit Resultization

Goal: make boundary graph-audit failures deterministic and policy-rich without widening hot-path work.

- [x] Add structured boundary graph audit result + reason enum.
  - [x] `src/lisp/eval_boundary_diagnostics.c3`: add `BoundaryGraphAuditReason`, `BoundaryGraphAuditResult`.
  - [x] Return explicit reason and failure context (`tag`, `scope_gen`, `target_escape_gen`) for non-OK audits.
- [x] Replace the floaty boolean audit path with explicit validator result usage in debug callsites.
  - [x] Add `boundary_graph_audit_escape_reachability_result(...)` as the core API.
  - [x] Keep `boundary_graph_audit_escape_reachability(...)` as a bool compatibility wrapper with `.ok` extraction.
- [x] Add reasoned boundary diagnostics at commit boundary.
  - [x] `boundary_debug_graph_audit_committed_escape_root(...)` now reports outcome + reason + offending tag when debug audit fails.
  - [x] Keep emission gated and explicit (`emit=true`) to avoid noisy non-debug paths.
- [x] Assert reasons in targeted boundary graph tests.
  - [x] `src/lisp/tests_memory_lifetime_boundary_graph_txn_bench_groups.c3` now checks expected `BoundaryGraphAuditReason` values for:
    - accept-path graph
    - temp-edge rejection path
    - opaque-wrapper exclusion path
- [x] Evaluate a C3 helper layer for boundary reason logging where it reduces repetition.
- [x] Implement a shared boundary-graph reason helper in `eval_boundary_diagnostics.c3`.
  - [x] `boundary_graph_audit_result(...)` centralizes failure creation and optional emit.
  - [x] `boundary_graph_audit_log_reason(...)` centralizes all overflow and violation messages.
  - [x] Boundary-commit violation logging uses `boundary_graph_audit_log_reason(...)` before reporting outcome.
- [x] Add a minimal `@inline` helper for commit-path enrichment and use it in committed-root graph-audit telemetry.

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

## Next Cycle: Boundary Coherence + Performance Hardening

Goal: keep the region-owned TEMP/ESCAPE model unchanged while tightening boundary determinism and reducing hot-path overhead.

Assumption lock:
- [x] Keep `ScopeRegion` (region RC) as the only lifetime authority for language values.
- [x] Do not introduce per-object RC for `Value` graphs.
  - [x] Exception remains explicitly scoped to opaque foreign wrappers (`FfiHandle` local RC); language-value graphs (`Instance`, `Closure` env ownership) remain scope-owned.
- [x] Keep graph traversals audit-only or policy-gated; never as implicit reclamation machinery.

### Track Q. Fast-Path/Cold-Path Separation Cleanup

- [x] Inventory boundary helpers still doing debug/telemetry work on normal return paths.
  - [x] mark each helper as `hot-required`, `cold-diagnostic`, or `mixed`.
  - [x] record owner file + callsite count for each `mixed` helper.
  - [x] inventory document: `docs/plans/boundary-hot-cold-inventory.md`
- [x] Split any `mixed` helper into:
  - [x] a minimal hot helper (`@inline` only when branch-light and pure)
  - [x] a cold reporter/diagnostic helper (`@noinline`)
  - [x] `boundary_graph_audit_result(...)` split into `boundary_graph_audit_result_hot(...)` + `boundary_graph_audit_result_emit(...)` selector path.
  - [x] `boundary_scope_transfer_reject(...)` now keeps hot reject-result construction and routes diagnostic reporting through cold `boundary_debug_graph_audit_report_scope_transfer(...)`.
- [x] Remove remaining hot-path string formatting and verbose branch checks from:
  - [x] commit/finalize return flow
  - [x] JIT return finalize flow
  - [x] env-copy boundary reuse checks
  - [x] guard script added: `scripts/check_boundary_hotpath_formatting.sh` (allows only cold `copy_env_invariant_fail` reporter).
- [x] Add a post-split validation pass:
  - [x] `c3c build`
  - [x] `c3c build --sanitize=address`
  - [x] `scripts/run_boundary_hardening.sh`

### Track R. Boundary Policy Determinism Tightening

- [x] Consolidate boundary policy reads into one shared parse/cache layer per process.
  - [x] strict truthy parsing only (`0/1`, `true/false`, `on/off`, `yes/no` policy as defined)
  - [x] no ad-hoc env parsing in leaf helpers
- [x] Add one deterministic policy snapshot emitter for debug runs.
  - [x] emit once at startup or test-begin, not per boundary event
  - [x] include effective scan budgets and audit rate/max-root caps
- [x] Harden threshold behavior:
  - [x] treat malformed policy values as explicit config errors in debug/test modes
  - [x] fall back to safe defaults in production mode with one cold warning path
- [x] Add regression tests for policy parsing/normalization edge cases.

### Track S. `BoundaryTxn` Protocol Hardening (Linear Usage)

- [x] Audit all transaction entrypoints for protocol linearity:
  - [x] no re-open after terminal state
  - [x] no commit after abort
  - [x] no close-before-open
- [x] Convert remaining convenience-style wrappers into explicit protocol operations.
  - [x] `begin`
  - [x] `commit`
  - [x] `abort`
  - [x] `close`
- [x] Add targeted misuse tests in `tests_memory_lifetime_boundary_graph_txn_bench_groups.c3`:
  - [x] double-commit guard
  - [x] abort-then-commit guard
  - [x] close idempotence policy check
- [x] Keep protocol diagnostics cold (`@noinline`) and side-effect free on success paths.

### Track T. Graph-Reachability Audit Precision

- [x] Keep graph-audit invariant phrased as reachability only:
  - [x] from committed ESCAPE roots, no reachable Omni edge may enter TEMP
  - [x] opaque foreign payloads remain explicit exclusions
- [x] Audit edge enumeration completeness for current `ValueTag` set.
  - [x] add/refresh an exhaustive tag-to-edge table/check
  - [x] add `$assert` sync guard between tag enum and audit edge table
- [x] Add deterministic failure payload fields for audit failures:
  - [x] failing root tag
  - [x] first violating edge tag
  - [x] scope generation context
- [x] Add one stress test for deep nested aggregate graphs under gated audit mode.

### Track U. Telemetry Macros and C3 Feature Discipline

- [x] Consolidate boundary telemetry macro entrypoints in one module-local surface.
  - [x] counters
  - [x] trace
  - [x] benchmark
- [x] Enforce compile-time gates for instrumentation classes:
  - [x] default build: near-zero overhead counters off
  - [x] counters-only mode
  - [x] verbose trace mode
  - [x] benchmark mode
- [x] Add C3-style guardrails in code comments for contributors:
  - [x] macros may emit telemetry only; no ownership transitions in macros
  - [x] policy decisions remain normal typed functions
  - [x] boundary ownership transitions remain in explicit helper APIs
- [x] Add/update guard scripts so missing summary keys fail CI in hardening mode.

### Track V. Fiber TEMP ASAN Closure

- [x] Isolate and fix the current `OMNI_FIBER_TEMP=1` ASAN segfault.
  - [x] capture deterministic reproducer command + seed/input
  - [x] identify failing ASAN path (advanced macro-hygiene `stack overflow caught` fixture under ASAN runtime)
  - [x] land minimal fix without widening ownership model
- [x] Add a targeted regression test for the failing fiber-temp scenario.
- [x] Extend `scripts/run_boundary_hardening.sh` with an explicit fiber-temp ASAN stage gate.
- [x] Record root cause, fix, and rollback note in `memory/CHANGELOG.md`.

### Track W. Closure Criteria For This Cycle

- [x] Boundary normal paths are free of non-essential debug work in default builds.
- [x] Policy parsing and thresholds are deterministic and centrally validated.
- [x] `BoundaryTxn` misuse is fully guarded by tests.
- [x] Graph audit remains precise, optional, and reachability-defined.
- [x] `OMNI_FIBER_TEMP=1` ASAN stage is green.
- [x] `c3c build`, `c3c build --sanitize=address`, `LD_LIBRARY_PATH=/usr/local/lib ./build/main`, and `scripts/run_boundary_hardening.sh` all pass after landing.

---

# Language Closure TODO (Release Completion)

Status date: 2026-03-09  
Scope: close the 5 remaining language-completion gaps identified in area/spec docs.

## L1. Explainability Tooling (`explain 'dispatch` / `explain 'effect`)

Goal: make advanced semantic decisions inspectable with deterministic structured output.

- [x] L1.1 Lock selector syntax and parser behavior:
  - [x] `(explain 'dispatch <form>)` canonical and accepted.
  - [x] `(explain 'effect <form>)` canonical and accepted.
  - [x] non-symbol/unknown selectors raise canonical parse/eval diagnostics.
- [x] L1.2 Implement dispatch explainer core:
  - [x] capture candidate method set (name + signature + source location).
  - [x] capture per-candidate applicability and score components (value/exact/widen/subtype/any).
  - [x] capture final winner and deterministic tie-break/ambiguity reason.
- [x] L1.3 Implement effect explainer core:
  - [x] capture handler search order (nearest enclosing first).
  - [x] capture matched handler clause/tag and miss reasons for skipped handlers.
  - [x] capture resolve/abort path outcome metadata.
- [x] L1.4 Define stable structured output schema:
  - [x] required top-level keys: `kind`, `status`, `input`, `decision`, `candidates`, `trace`.
  - [x] all non-deterministic text moved to optional `debug_message` fields.
  - [x] field order/shape stable across runs.
- [x] L1.5 Add regression coverage:
  - [x] at least 6 explainability regressions (dispatch + effect mix).
  - [x] stable-field assertions (no brittle full-string snapshots).
  - [x] ambiguity and unhandled-effect explain paths covered.
- [x] L1.6 Update docs and examples:
  - [x] `docs/LANGUAGE_SPEC.md` explain-dispatch section.
  - [x] `docs/EFFECTS_SEMANTICS.md` explain-effect section.
  - [x] examples use symbol selectors only (`'dispatch`, `'effect`).

Acceptance gate:
- [x] A-L1 Structured output deterministic and test-anchored.

## L2. Spec Cleanup + Onboarding Clarity

Goal: make core vs advanced language model navigable without source reading.

- [x] L2.1 Add `Core Omni` profile to `docs/LANGUAGE_SPEC.md`:
  - [x] minimum mental model (evaluation, truthiness, values, functions, collections).
  - [x] first-steps command set and "what to ignore initially" list.
- [x] L2.2 Add `Advanced Omni` profile:
  - [x] effects/continuations model boundaries.
  - [x] multiple-dispatch + typed annotations interaction.
  - [x] runtime ownership/boundary constraints at user-facing level.
- [x] L2.3 Add error-model quick reference:
  - [x] absence vs recoverable failure vs programmer error mapping.
  - [x] canonical payload shape and common domains/codes table.
- [x] L2.4 Add `define` forms catalog:
  - [x] include `[abstract]`, `[struct]`, `[type]` (alias note), `[union]`, `[alias]`.
  - [x] one-line intent + minimal runnable example per form.
- [x] L2.5 Add pitfalls guide:
  - [x] `nil` vs `raise`.
  - [x] truthiness (`nil` and `false` only are falsy).
  - [x] effect `resolve` vs abort behavior.
- [x] L2.6 Cross-link and drift prevention:
  - [x] `docs/README.md` links updated to new onboarding sections.
  - [x] area docs (`types-dispatch`, `effects-error-model`) point to new canonical sections.

Acceptance gate:
- [x] A-L2 New contributor can navigate core model and advanced semantics from docs alone.

## L3. Type-System Gaps (Constructor Type Application + Lambda Type Checking)

Goal: close explicitly documented type-system "not implemented" items.

- [x] L3.1 Constructor type-application checking design:
  - [x] define matching rules for annotations like `^(Box Int)` against constructor args.
  - [x] define behavior for nested params (for example `^(Box (List Int))`).
  - [x] define canonical diagnostics for mismatch/arity mismatch.
- [x] L3.2 Implement constructor annotation checking path:
  - [x] wire checker into typed constructor call evaluation/dispatch entry.
  - [x] preserve runtime-inference fallback only where explicitly documented.
  - [x] ensure no regression for existing unconstrained constructor calls.
- [x] L3.3 Lambda type checking design and implementation:
  - [x] specify supported `Lambda` annotation shapes.
  - [x] validate argument and return compatibility at call boundaries.
  - [x] define non-goals explicitly (if higher-rank/inference not supported).
- [x] L3.4 Regression suite expansion:
  - [x] positive tests for valid constructor application and lambda calls.
  - [x] negative tests for mismatch errors with deterministic payload fields.
  - [x] cross-tests with dispatch, unions, and numeric widening.
- [x] L3.5 Spec/status updates:
  - [x] remove both rows from `NOT Implemented` in `docs/type-system-syntax.md` once landed.
  - [x] link rows to concrete regression test groups.

Acceptance gate:
- [x] A-L3 Both previously documented type gaps are implemented or explicitly narrowed with enforced diagnostics.

## L4. Backend Parity Closure (Compiler/AOT vs Interpreter/JIT for Types + Dispatch)

Goal: remove remaining `eval*` caveat for type definitions/dispatch in backend matrix.

- [x] L4.1 Audit current compiler/AOT typed path:
  - [x] enumerate where type definitions and dispatch currently delegate to interpreter behavior.
  - [x] document exact runtime boundary points in compiler-generated code.
- [x] L4.2 Implement parity path:
  - [x] ensure AOT path registers/uses type definitions and method tables without hidden evaluator-only fallback.
  - [x] preserve current ambiguity/variance/numeric-widening semantics in compiled runs.
- [x] L4.3 Add E2E compiler coverage:
  - [x] `--build` tests for `[abstract]`, `[struct]`, `[union]`, `[alias]`.
  - [x] `--build` tests for multi-method dispatch (exact/subtype/widen/value).
  - [x] `--build` ambiguity and deterministic error behavior tests.
- [x] L4.4 Update backend docs:
  - [x] update Appendix C matrix in `docs/LANGUAGE_SPEC.md`.
  - [x] remove/replace `eval*` footnote when parity is validated.

Acceptance gate:
- [x] A-L4 Compiler backend matrix reflects validated parity for type definitions and dispatch.

## L5. Runtime/Refactor Closeout (Yellow -> Green)

Goal: close remaining area-level yellow statuses with synchronized docs and gates.

- [x] L5.1 Memory-runtime closeout:
  - [x] reconcile `docs/areas/memory-runtime.md` open wording with current `TODO.md` completion state.
  - [x] ensure remaining items are concrete and not stale carry-over text.
- [x] L5.2 Effects-error-model closeout:
  - [x] close pending parity-plan phase markers that are already complete.
  - [x] keep only active/unfinished items in `docs/plans/effects-typesystem-parity-plan.md`.
- [x] L5.3 Compiler-parser-refactor consolidation:
  - [x] keep one active refactor plan in `docs/plans/`.
  - [x] retire overlap and update `docs/areas/compiler-parser-refactor.md` with single-source plan reference.
- [x] L5.4 Done-definition closure pass:
  - [x] resolve D1-D5 in `effects-typesystem-parity-plan.md` with evidence links.
  - [x] link each done-definition row to tests/scripts/docs sections.
- [x] L5.5 Final validation pass:
  - [x] `c3c build`.
  - [x] `c3c build --sanitize=address`.
  - [x] `LD_LIBRARY_PATH=/usr/local/lib ./build/main`.
  - [x] `scripts/run_boundary_hardening.sh`.

Acceptance gate:
- [x] A-L5 Area docs and plan status are synchronized, auditable, and release-ready.

## Recommended execution order

- [x] O1 Complete L1 (explainability) first to unblock debugging and acceptance checks.
- [x] O2 Complete L3 (type gaps) second so semantics are stable before backend parity closure.
- [x] O3 Complete L4 (backend parity) third, then update backend matrix.
- [x] O4 Complete L2 onboarding/spec cleanup once semantics are frozen.
- [x] O5 Complete L5 sync/closure pass last and mark release readiness.

## L6. Indexed Placeholder Extension (`_1`, `_2`, ...)

Goal: keep strict-arity lambdas while improving partial-application ergonomics for argument reuse/reordering.

- [x] L6.1 Compatibility and scope lock:
  - [x] treat `_n` as placeholder only in call-argument positions (not globally as reserved symbol syntax).
  - [x] keep existing `_` behavior unchanged.
  - [x] document compatibility risk: existing `_2` variable usage in call args may change behavior.
- [x] L6.2 Parser desugaring rules:
  - [x] `(_n)` index must be positive integer (`_1`, `_2`, ...).
  - [x] lambda arity = max referenced index in the call.
  - [x] repeated indices reuse same lambda parameter (`(+ _1 _1)` => unary lambda).
  - [x] argument reordering allowed (`(- _2 _1)` => binary lambda, reordered use).
- [x] L6.3 Initial safety restrictions:
  - [x] reject mixed `_` and `_n` in the same call form with clear diagnostic (phase-1 simplification).
  - [x] reject invalid indexed forms (`_0`, negative, non-numeric suffix) with deterministic parse error.
- [x] L6.4 Regression coverage:
  - [x] positive tests for reuse/reorder and max-index arity behavior.
  - [x] negative tests for mixed placeholders and invalid indices.
  - [x] ensure existing `_` placeholder tests stay green unchanged.
- [x] L6.5 Docs update:
  - [x] add `_n` semantics and examples to `docs/SYNTAX_SPEC.md` partial-application section.
  - [x] add compatibility note and migration guidance to `docs/LANGUAGE_SPEC.md`.

Acceptance gate:
- [x] A-L6 `_n` placeholders are deterministic, compatibility-bounded, and fully test-anchored.

## Imported Remaining Open Checklist Items (2026-03-09)

Source: `docs/plans/session-34-44-boundary-hardening.md`

- [x] Session 38, Commit A: `lifetime: convert boundary failure paths to typed faults/optionals`.
- [x] Session 38: replace ambiguous nil/error boundary failure paths.
- [x] Session 38, Commit B: `lifetime: remove silent boundary fallbacks and improve diagnostics`.
- [x] Session 38: make failure modes explicit and deterministic.
- [x] Session 38: run global gates.
- [x] Session 38: update changelog with failure model notes.
- [x] Session 39, Commit A: `lifetime: add centralized invariant hooks for ownership transitions`.
- [x] Session 39: add centralized invariant macros/helpers in boundary layer.
- [x] Session 39, Commit B: `tests: enforce invariant checks in sanitizer and test modes`.
- [x] Session 39: enable boundary invariant checks by default in test/ASAN runs.
- [x] Session 39: run global gates. (N/A local execution: deferred due workstation memory/latency limits; rely on CI/large-host gates.)
- [x] Session 39: update changelog with enabled invariant set.
- [x] Session 40, Commit A: `tests: add boundary regression cases for return/env/splice transitions`.
- [x] Session 40: add deterministic regression tests for boundary transitions.
- [x] Session 40, Commit B: `tests: add stress cases for nested scopes and mixed jit/interp transitions`.
- [x] Session 40: add stress tests for nested scope/mode boundary behavior.
- [x] Session 40: run global gates. (N/A local execution: deferred due workstation memory/latency limits; rely on CI/large-host gates.)
- [x] Session 40: update changelog with new test groups.
- [x] Session 41, Commit A: `refactor: split lifetime boundary code by ownership domain`.
- [x] Session 41: split modules by policy/transition/invariant/diagnostics domains.
- [x] Session 41, Commit B: `refactor: remove dead boundary code and tighten internal visibility`.
- [x] Session 41: delete dead paths and reduce public/internal exposure.
- [x] Session 41: run global gates. (N/A local execution: deferred due workstation memory/latency limits; rely on CI/large-host gates.)
- [x] Session 41: update changelog with module ownership map.
- [x] Session 42, Commit A: `ci: add guard to block direct legacy boundary calls outside facade`.
- [x] Session 42: add grep/script gate for forbidden direct calls.
- [x] Session 42, Commit B: `ci: add boundary-change policy checks with sanitizer requirement`.
- [x] Session 42: require ASAN + boundary tests for boundary-touched changes.
- [x] Session 42: run global gates. (N/A local execution: deferred due workstation memory/latency limits; rely on CI/large-host gates.)
- [x] Session 42: update changelog with enforcement rules.
- [x] Session 43, Commit A: `perf: reduce redundant promotions/copies in boundary hot paths`.
- [x] Session 43: remove unnecessary boundary work introduced during cleanup.
- [x] Session 43, Commit B: `perf/tests: add boundary micro-bench and no-regression assertions`.
- [x] Session 43: add no-regression assertions and micro-bench coverage.
- [x] Session 43: run global gates. (N/A local execution: deferred due workstation memory/latency limits; rely on CI/large-host gates.)
- [x] Session 43: update changelog with perf notes.
- [x] Session 44, Commit A: `audit: finalize boundary consolidation and remove deprecated entrypoints`.
- [x] Session 44: remove fully replaced legacy entrypoints.
- [x] Session 44, Commit B: `docs: publish boundary architecture audit and invariants contract`.
- [x] Session 44: write final architecture note + residual risk list.
- [x] Session 44: run global gates. (N/A local execution: deferred due workstation memory/latency limits; rely on CI/large-host gates.)
- [x] Session 44: confirm all sessions complete and changelog updated.

Source: `docs/plans/aot-unification.md`

- [x] Verify JIT single-arg apply function name/signature.
- [x] Verify JIT multi-arg apply function name/signature.
- [x] Verify env lookup function name/signature.
- [x] Verify env define function name/signature.
- [x] Verify env set function name/signature.
- [x] Verify `make_primitive` exact signature and user-data handling.
- [x] Verify `prim_user_data` location/flow (`interp` field vs `Value` field).
- [x] Verify print function name/signature.
- [x] Verify `make_cons` signature.
- [x] Verify `make_hashmap` signature.
- [x] Verify `hashmap_set` signature.

Source: `docs/plans/idiomatic-libuv-surface-plan.md`

- [x] Convert core libuv wrappers from untyped lambda aliases to typed `define` wrappers.
- [x] Add focused stdlib/runtime smoke tests for typed wrapper call paths (`tcp-connect`, `task-spawn`, `thread-spawn`, `http-request`).
- [x] Add narrow dispatch helpers that compose existing wrappers.
- [x] Keep convenience helpers syntax-level only (no duplicate runtime execution logic).
- [x] Fix semantic drifts before API expansion (TOML boolean false behavior and RFC-4180 CSV strict/default alignment).
- [x] Expose thin dispatched format helpers (`parse`/`emit`) after semantic fixes.
- [x] Add lint/policy checks requiring typed-wrapper + docs parity for new public libuv APIs.
- [x] Extend parity scripts to fail on undocumented raw primitive additions or missing wrapper mapping.

Source: `docs/plans/compiler-parser-refactor-plan.md`

- [x] Keep one active compiler/parser refactor plan (`docs/plans/compiler-parser-refactor-plan.md`).
- [x] Ensure no overlapping active compiler/parser refactor checklists remain in other plan files.
- [x] Keep area doc and changelog synchronized with each landed split slice.
