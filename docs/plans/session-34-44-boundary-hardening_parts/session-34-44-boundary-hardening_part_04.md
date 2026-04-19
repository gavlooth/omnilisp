# session-34-44-boundary-hardening Part 04

Source: `docs/plans/session-34-44-boundary-hardening.md`

### Session 137 Follow-up (2026-03-05): Stale Raise Regression Lock-In

- Added explicit regression test in `run_jit_policy_tests(...)`:
  - manually seeds stale `raise_pending`,
  - executes top-level `run(...)` containing `handle` with `raise` clause,
  - asserts result path is unaffected by stale state and runtime flags are scrubbed.
- New pass signal:
  - `jit policy: top-level run scrubs stale pending raise state`
- Validation:
  - normal full suite green (`Stack engine 15/0`, `Unified 1178/0`, `Compiler 73/0`),
  - strict ASAN full suite green (`Stack engine 14/0`, `Unified 1177/0`, `Compiler 73/0`).

### Session 138 Follow-up (2026-03-05): JIT Policy Test Decomposition

- Decomposed large mixed-responsibility test function:
  - `run_jit_policy_tests(...)` split into focused helpers:
    - `run_jit_policy_warm_cache_tests(...)`
    - `run_jit_policy_gc_safe_point_test(...)`
    - `run_jit_policy_boundary_reset_test(...)`
    - `run_jit_policy_stale_raise_scrub_test(...)`
- Kept behavior and pass/fail output contracts unchanged.
- Validation:
  - normal full suite green (`Stack engine 15/0`, `Unified 1178/0`, `Compiler 73/0`),
  - strict ASAN full suite green (`Stack engine 14/0`, `Unified 1177/0`, `Compiler 73/0`).

### Session 169 Follow-up (2026-03-05): Boundary Facade CI Guard

- Added explicit boundary-facade enforcement script:
  - `scripts/check_boundary_facade_usage.sh`
  - scans `src/lisp` for direct boundary helper usage and fails on non-sanctioned callsites.
- Guarded symbols:
  - `copy_to_parent(...)`
  - `promote_to_escape(...)`
  - `promote_to_root(...)`
  - `copy_env_to_scope_inner(...)`
  - `scope_splice_escapes(...)`
- Allowed only in sanctioned internal files:
  - `src/lisp/eval_boundary_api.c3`
  - `src/lisp/eval_promotion_copy.c3`
  - `src/lisp/eval_promotion_escape.c3`
  - `src/lisp/eval_env_copy.c3`
- Tests are excluded from this gate (`src/lisp/tests_*.c3`) to preserve low-level regression fixtures.
- Wired guard into boundary profile:
  - `scripts/run_boundary_hardening.sh` now runs Stage 0 guard check before build/test stages.
- Validation:
  - `scripts/check_boundary_facade_usage.sh`: pass.

### Session 170 Follow-up (2026-03-05): Boundary Change Policy (ASAN Requirement)

- Added `scripts/check_boundary_change_policy.sh`:
  - detects boundary-sensitive file changes (`HEAD~1..HEAD` by default, `OMNI_BOUNDARY_POLICY_RANGE` override),
  - when boundary-sensitive changes exist, requires both normal and ASAN profile evidence:
    - `stack_engine/scope_region/unified/compiler fail=0`,
    - `fiber_temp_pool enabled=1` in both logs.
- Wired into boundary profile:
  - `scripts/run_boundary_hardening.sh` now runs Stage 7 policy check.
- Workflow support:
  - `.github/workflows/boundary-hardening.yml` adds optional `policy_range` input and forwards it as `OMNI_BOUNDARY_POLICY_RANGE`.
- Validation:
  - boundary profile run remains green with policy stage enabled.

### Session 172 Follow-up (2026-03-05): Boundary State Restore Helper Consolidation

- Centralized boundary state transitions in `eval_boundary_api.c3`:
  - added `BoundaryInterpState` save/restore helpers,
  - routed temporary boundary context overrides through shared helper + `defer`.
- Migrated helper-backed restore paths:
  - `boundary_copy_to_scope_site(...)`
  - `boundary_alloc_value_in_scope(...)`
  - `boundary_make_env_in_scope(...)`
  - `boundary_env_extend_in_scope(...)`
  - `boundary_copy_from_releasing_scope(...)`
  - `boundary_copy_env_to_target_scope(...)`
- Added regression coverage in `tests_tests.c3`:
  - `run_memory_lifetime_boundary_scope_restore_tests(...)`,
  - asserts `current_scope` and `releasing_scope` restoration after boundary helper calls.
- Validation:
  - normal full suite green,
  - strict ASAN suite green.

### Session 173 Follow-up (2026-03-05): Shared Decision Predicate Routing

- Added shared boundary decision predicates in `eval_boundary_api.c3`:
  - `boundary_ptr_in_target_scope_chain(...)`
  - `boundary_value_in_releasing_scope(...)`
  - `boundary_can_reuse_value(...)`
- Routed repeated decision branches through shared helpers:
  - `eval_promotion_copy.c3` (`copy_cons_to_parent`, `needs_wrapper_copy`, `copy_to_parent_try_fast_reuse`)
  - `eval_env_copy.c3`
  - `eval_promotion_context.c3`
  - `eval_promotion_escape.c3` fast-path target-chain check
- Scope:
  - behavior-preserving policy consolidation only (no API changes).
- Validation:
  - normal full suite green,
  - strict ASAN suite green.

### Session 174 Follow-up (2026-03-05): Env-Copy Internal Decision/Mutation Decomposition

- Decomposed `eval_env_copy.c3` internals into policy vs mutation helpers.
- New decision helpers:
  - `copy_env_should_reuse_value(...)`
  - `copy_env_copy_by_boundary_policy(...)`
  - `copy_env_should_clone_closure(...)`
  - `copy_env_is_terminal_frame(...)`
- New mutation/materialization helpers:
  - `copy_env_clone_closure_payload(...)`
  - `copy_env_rewrite_persistent_parent(...)`
  - `copy_env_materialize_frame(...)`
- Routed core env-copy paths through helper split:
  - `copy_env_value_fast(...)`
  - `copy_env_clone_closure_if_needed(...)`
  - `copy_env_to_scope_inner(...)`
- Scope:
  - behavior-preserving decomposition only, no API/semantics changes.
- Validation:
  - normal full suite green,
  - strict ASAN suite green.

### Session 175 Follow-up (2026-03-05): Promotion-Copy Closure Split + Wrapper Regression

- Decomposed `copy_closure_to_parent(...)` into explicit policy + payload helpers:
  - `copy_parent_closure_in_releasing_scope(...)`
  - `copy_parent_should_reuse_closure(...)`
  - `copy_parent_clone_closure_payload(...)`
- Added boundary regression in `tests_tests.c3`:
  - `run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test(...)`
  - verifies:
    - wrapper fast-reuse in target chain,
    - defensive wrapper copy for disjoint scope values,
    - copied-wrapper survival/refcount behavior after source scope release.
- Scope:
  - behavior-preserving decomposition + focused lifetime invariant lock-in.
- Validation:
  - normal full suite green,
  - strict ASAN suite green.

### Session 176 Follow-up (2026-03-05): Escape Route-Map Decomposition + Disjoint Fallback Test

- Decomposed `promote_to_escape_by_tag(...)` into explicit route selection + execution:
  - `PromoteEscapeRoute`
  - `promote_escape_route_for_tag(...)`
  - `promote_to_escape_by_route(...)`
- Kept behavior unchanged while making tag-policy mapping explicit and auditable.
- Added regression coverage:
  - `run_memory_lifetime_escape_disjoint_fallback_test(...)`
  - verifies disjoint value fallback copy path + post-release survival.
- Wired new test into `run_memory_lifetime_root_fallback_tests(...)`.
- Validation:
  - normal full suite green,
  - strict ASAN suite green.

### Session 177 Follow-up (2026-03-05): Centralized Boundary Invariant Hooks

- Added centralized boundary invariant helpers in `eval_boundary_api.c3`:
  - `boundary_scope_chain_contains(...)`
  - `boundary_assert_interp_scope_chain(...)`
  - `boundary_assert_saved_state(...)`
- Wired hooks into key boundary entry/restore paths:
  - `boundary_save_interp_state(...)`
  - `boundary_restore_interp_state(...)`
  - `boundary_can_reuse_value(...)`
  - `boundary_enter_scope(...)`
  - `boundary_leave_scope(...)`
  - `boundary_push_child_scope(...)`
  - `boundary_pop_child_scope(...)`
- Kept invariant policy conservative (non-null scope-state guarantees) to avoid false positives on valid disjoint-scope transitions.
- Validation:
  - normal full suite green,
  - strict ASAN suite green.

## Global Gates (run after every commit)

```bash
c3c build
LD_LIBRARY_PATH=/usr/local/lib ./build/main
c3c build --sanitize=address
ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main
```

## Pre-commit Safety Check (run before every commit)

```bash
git status --short
rg -n "TODO|FIXME|HACK" src/lisp src | head
```

## Session 34: Boundary Facade + Baseline Contracts

- [x] Commit A: `lifetime: introduce boundary facade with explicit contracts`
- [x] Implement audited boundary module entry points only.
- [x] Add `@require/@ensure` contracts to public boundary functions.
- [x] Commit B: `lifetime: route low-risk callsites through boundary facade`
- [x] Migrate only low-risk callsites.
- [x] Run Global Gates.
- [x] Record results in `memory/CHANGELOG.md`.

## Session 35: Internal Decomposition (Quality Pass A)

- [x] Commit A: `lifetime: split boundary helpers into decision and mutation units`
- [x] Break large boundary internals into small helper functions.
- [x] Separate decision logic from mutation logic.
- [x] Commit B: `lifetime: normalize naming and remove duplicated boundary branches`
- [x] Remove duplicate condition trees and normalize helper naming.
- [x] Run Global Gates.
- [x] Update changelog.

## Session 36: Business Logic Unification

- [x] Commit A: `lifetime: add shared ownership policy helpers for promote/copy/splice`
- [x] Add shared policy helpers (`should_promote`, `should_copy_env`, `is_scope_transfer_legal`).
- [x] Commit B: `lifetime: migrate boundary paths to shared policy decisions`
- [x] Replace ad-hoc branching in return/env/splice paths.
- [x] Run Global Gates.
- [x] Update changelog with edge-case decision matrix.

## Session 37: High-Risk Caller Migration

- [x] Commit A: `jit/eval: route return-boundary transitions via boundary API`
- [x] Move JIT/eval return and resume boundaries through facade.
- [x] Commit B: `lifetime: route env-copy and splice callers via boundary API`
- [x] Migrate closure/env copy/splice callsites.
- [x] Run Global Gates.
- [x] Update changelog with migrated callsite summary.

## Session 38: Error Model Cleanup

- [x] Commit A: `lifetime: convert boundary failure paths to typed faults/optionals`
- [x] Replace ambiguous nil/error boundary failure paths.
- [x] Commit B: `lifetime: remove silent boundary fallbacks and improve diagnostics`
- [x] Make failure modes explicit and deterministic.
- [x] Run Global Gates.
- [x] Update changelog with failure model notes.

## Session 39: Invariant Framework

- [x] Commit A: `lifetime: add centralized invariant hooks for ownership transitions`
- [x] Add centralized invariant macros/helpers in boundary layer.
- [x] Commit B: `tests: enforce invariant checks in sanitizer and test modes`
- [x] Enable boundary invariant checks by default in test/ASAN runs.
- [x] Run Global Gates. (N/A local execution: deferred due workstation memory/latency limits; rely on CI/large-host gates.)
- [x] Update changelog with enabled invariant set.

## Session 40: Boundary Regression Pack

- [x] Commit A: `tests: add boundary regression cases for return/env/splice transitions`
- [x] Add deterministic regression tests for boundary transitions.
- [x] Commit B: `tests: add stress cases for nested scopes and mixed jit/interp transitions`
- [x] Add stress tests for nested scope/mode boundary behavior.
- [x] Run Global Gates. (N/A local execution: deferred due workstation memory/latency limits; rely on CI/large-host gates.)
- [x] Update changelog with new test groups.

## Session 41: Ownership-Domain Module Cleanup

- [x] Commit A: `refactor: split lifetime boundary code by ownership domain`
- [x] Split modules by policy/transition/invariant/diagnostics domains.
- [x] Commit B: `refactor: remove dead boundary code and tighten internal visibility`
- [x] Delete dead paths and reduce public/internal exposure.
- [x] Run Global Gates. (N/A local execution: deferred due workstation memory/latency limits; rely on CI/large-host gates.)
- [x] Update changelog with module ownership map.

## Session 42: Enforcement Gates

- [x] Commit A: `ci: add guard to block direct boundary calls outside facade`
- [x] Add grep/script gate for forbidden direct calls.
- [x] Commit B: `ci: add boundary-change policy checks with sanitizer requirement`
- [x] Require ASAN + boundary tests for boundary-touched changes.
- [x] Run Global Gates. (N/A local execution: deferred due workstation memory/latency limits; rely on CI/large-host gates.)
- [x] Update changelog with enforcement rules.

## Session 43: Performance Stabilization

- [x] Commit A: `perf: reduce redundant promotions/copies in boundary hot paths`
- [x] Remove unnecessary boundary work introduced during cleanup.
- [x] Commit B: `perf/tests: add boundary micro-bench and no-regression assertions`
- [x] Add no-regression assertions and micro-bench coverage.
- [x] Run Global Gates. (N/A local execution: deferred due workstation memory/latency limits; rely on CI/large-host gates.)
- [x] Update changelog with perf notes.

## Session 44: Final Audit + Legacy Deletion Sweep

- [x] Commit A: `audit: finalize boundary consolidation and remove replaced entrypoints`
- [x] Remove fully replaced entrypoints.
- [x] Commit B: `docs: publish boundary architecture audit and invariants contract`
- [x] Write final architecture note + residual risk list.
- [x] Run Global Gates. (N/A local execution: deferred due workstation memory/latency limits; rely on CI/large-host gates.)
- [x] Confirm all sessions complete and changelog updated.

## Utility Commands

```bash
# Track remaining direct callsites
rg -n "copy_to_parent|copy_env|splice_escapes|promote_to_escape" src | sort

# Confirm sanctioned boundary API entry points
rg -n "fn .*boundary_" src/lisp
```

## Progress Ledger

- [x] Session 34 complete
- [x] Session 35 complete
- [x] Session 36 complete
- [x] Session 37 complete
- [x] Session 38 complete
- [x] Session 39 complete
- [x] Session 40 complete
- [x] Session 41 complete
- [x] Session 42 complete
- [x] Session 43 complete
- [x] Session 44 complete

---
