  - Behavior preserved; changes are structural hardening only.
- `src/lisp/tests_tests.c3`
  - Added `run_memory_lifetime_tco_runtime_fields_restore_test(...)`.
  - Verifies after both a successful and erroring TCO run:
    - `current_scope`
    - `releasing_scope`
    - `tco_recycle_scope`
    - `tco_scope_defer_slot`
    - `tco_scope_defer_active`
    - `escape_env_mode`
    are restored to their pre-run values.
  - Wired into `run_memory_lifetime_hot_budget_tests(...)`.

### Why this matters
- Continues the boundary-hardening objective: shrink ad-hoc runtime state transitions into audited helper surfaces.
- Adds explicit regression signal for TCO runtime-field leakage, not only scope pointers.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1189 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1188 passed, 0 failed` (JIT checks disabled under ASAN)
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 182 - run_program Boundary-State Regression Coverage

### Summary
Added focused coverage to ensure top-level multi-form execution (`run_program`) preserves interpreter boundary state (`current_scope`/`releasing_scope`) across full parse+eval flow.

### What changed
- `src/lisp/tests_tests.c3`
  - Added `run_memory_lifetime_run_program_boundary_state_test(...)`.
  - Validates:
    - multi-expression program evaluates to expected result,
    - `interp.current_scope` and `interp.releasing_scope` are unchanged after `run_program(...)`.
  - Wired into `run_memory_lifetime_regression_tests(...)`.

### Why this matters
- Extends boundary hardening coverage beyond `run(...)` and JIT internals into top-level program execution.
- Prevents regressions where parse/eval orchestration leaks transient scope state across calls.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1188 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1187 passed, 0 failed` (JIT checks disabled under ASAN)
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 181 - TCO Recycle Error-Path Boundary Rollback Consolidation

### Summary
Consolidated duplicated TCO recycle error rollback logic into a single helper and added explicit invariant checks across `jit_prepare_tco_recycle(...)` paths.

### What changed
- `src/lisp/jit_jit_eval_scopes.c3`
  - Added `jit_tco_recycle_restore_on_error(...)` to centralize:
    - `current_scope`/`tco_recycle_scope` rollback,
    - fresh-scope release,
    - `jit_env` restore,
    - boundary invariant assertion,
    - error return.
  - Routed both defer-retarget error branches through the helper.
  - Added `boundary_assert_interp_scope_chain(...)` guards at:
    - `jit_prepare_tco_recycle(...)` entry,
    - fast-path return,
    - alloc-failure return,
    - success return.

### Why this matters
- Reduces duplicated, easy-to-diverge boundary rollback logic in one of the trickiest JIT lifetime paths.
- Keeps rollback behavior consistent across all TCO recycle failure exits.
- Improves auditability without changing semantics.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1187 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1186 passed, 0 failed` (JIT checks disabled under ASAN)
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 180 - TCO Env-Copy Boundary Restore Consolidation + Regression

### Summary
Replaced manual `releasing_scope` save/restore in the JIT TCO recycle env-copy helper with audited boundary-state facade calls, and added a regression to lock in interpreter boundary-state restoration through long TCO loops.

### What changed
- `src/lisp/jit_jit_eval_scopes.c3`
  - `jit_copy_tco_env_chain_for_recycle(...)` now uses:
    - `boundary_assert_interp_scope_chain(...)`
    - `boundary_save_interp_state(...)`
    - `defer boundary_restore_interp_state(...)`
  - Removed manual `saved_releasing`/restore wiring.
- `src/lisp/tests_tests.c3`
  - Added `run_memory_lifetime_tco_boundary_state_restore_test(...)`.
  - Verifies:
    - long named-let/TCO run still evaluates correctly,
    - `interp.current_scope` and `interp.releasing_scope` are exactly restored after run.
  - Wired into `run_memory_lifetime_hot_budget_tests(...)`.

### Why this matters
- Shrinks distributed boundary restore logic in one of the more subtle TCO/lifetime paths.
- Converts manual state mutation to centralized, audited boundary primitives.
- Adds direct regression coverage for "TCO recycle path must leave boundary state unchanged."

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1187 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1186 passed, 0 failed` (JIT checks disabled under ASAN)
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 179 - Boundary Invariant Hook Sweep (REPL/Macro Root-Scope Helpers)

### Summary
Completed the next low-risk boundary-hook sweep by adding explicit scope-chain invariant checks to remaining boundary transition helpers in REPL and macro expansion paths.

### What changed
- `src/lisp/eval_repl.c3`
  - `repl_eval_line(...)` now asserts `boundary_assert_interp_scope_chain(...)`:
    - at entry,
    - after `boundary_push_child_scope(...)`,
    - before return.
- `src/lisp/macros_expansion.c3`
  - `capture_template_bindings_in_root_scope(...)` now asserts `boundary_assert_interp_scope_chain(...)`:
    - at entry,
    - before return after `boundary_enter_scope(...)`/`boundary_leave_scope(...)`.

### Why this matters
- Extends invariant-coverage consistency to residual boundary transition sites outside the hot run/JIT path.
- Tightens failure locality for boundary-state corruption without changing runtime semantics.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1186 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1185 passed, 0 failed` (JIT checks disabled under ASAN)
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 178 - Boundary Invariant Hook Rollout (Run/JIT) + Interleaving Regression

### Summary
Extended centralized boundary invariant hooks into high-risk run/JIT scope transition paths and added a focused regression for nested boundary interleavings (`enter/leave` + `push/pop`) to keep scope-state restoration auditable.

### What changed
- `src/lisp/eval_run_pipeline.c3`
  - Added `boundary_assert_interp_scope_chain(...)` checks at:
    - `run_promote_result(...)` entry/exit,
    - `run(...)` entry, post-`boundary_push_child_scope(...)`, and pre-return.
- `src/lisp/jit_jit_eval_scopes.c3`
  - Added `boundary_assert_interp_scope_chain(...)` checks at:
    - `jit_finalize_scoped_result(...)` entry and all return exits,
    - `jit_eval_in_single_scope(...)` entry, post-`boundary_push_child_scope(...)`, and pre-return,
    - `jit_eval_in_call_scope(...)` entry, post-`boundary_push_child_scope(...)`, and pre-return.
- `src/lisp/tests_tests.c3`
  - Added `run_memory_lifetime_boundary_scope_interleaving_test(...)`.
  - Verifies:
    - nested boundary interleavings preserve `current_scope`/`releasing_scope`,
    - copy-from-releasing path still works after interleaving,
    - explicit save/restore round-trip remains exact.
  - Wired into `run_memory_lifetime_regression_tests(...)`.

### Why this matters
- Continues the "centralized invariants, distributed call-site coverage" hardening strategy without changing behavior.
- Narrows bug surface in the most failure-prone scope handoff paths (`run` and JIT scoped eval/finalize).
- Adds coverage for mixed boundary operation ordering, where regressions are typically subtle.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1186 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1185 passed, 0 failed` (JIT checks disabled under ASAN)
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 177 - Central Boundary Invariant Hooks (Non-Null Scope State)

### Summary
Introduced centralized boundary invariant hooks in the facade layer and wired them into key boundary transitions, with conservative non-null scope-state guarantees to avoid false positives in valid disjoint-scope flows.

### What changed
- `src/lisp/eval_boundary_api.c3`
  - Added shared invariant helpers:
    - `boundary_scope_chain_contains(...)`
    - `boundary_assert_interp_scope_chain(...)` (non-null `root_scope` + `current_scope`)
    - `boundary_assert_saved_state(...)`
  - Hooked invariant checks into key boundary paths:
    - `boundary_save_interp_state(...)`
    - `boundary_restore_interp_state(...)`
    - `boundary_can_reuse_value(...)`
    - `boundary_enter_scope(...)`
    - `boundary_leave_scope(...)`
    - `boundary_push_child_scope(...)`
    - `boundary_pop_child_scope(...)`

### Why this matters
- Centralizes recurring boundary correctness assumptions in one audited place.
- Moves toward invariant-hook coverage goals without introducing brittle constraints.
- Keeps runtime behavior stable while improving failure locality for invalid boundary state.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-05: Session 176 - Escape-Promotion Route Map + Disjoint Fallback Regression

### Summary
Decomposed escape-promotion dispatch into an explicit route map and added a regression test for disjoint-lifetime fallback-copy behavior before ESCAPE-lane promotion.

### What changed
- `src/lisp/eval_promotion_escape.c3`
  - Introduced route enum and dispatch split:
    - `enum PromoteEscapeRoute`
    - `promote_escape_route_for_tag(...)`
    - `promote_to_escape_by_route(...)`
  - `promote_to_escape_by_tag(...)` now composes route selection + route execution.
  - Behavior preserved for all value tags.
- `src/lisp/tests_tests.c3`
  - Added `run_memory_lifetime_escape_disjoint_fallback_test(...)`.
  - Verifies:
    - disjoint value does not alias source (`escaped != src`),
    - fallback copy site counter (`COPY_SITE_GENERIC`) increments as expected,
    - escaped value survives source-scope release.
  - Wired into `run_memory_lifetime_root_fallback_tests(...)`.

### Why this matters
- Continues decision-vs-mutation separation in high-risk boundary paths.
- Makes promotion policy easier to audit and less likely to regress silently.
- Adds explicit coverage for the defensive fallback path that protects against disjoint lifetime aliasing.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-05: Session 175 - Promotion-Copy Decomposition + Wrapper Boundary Regression

### Summary
Continued boundary hardening by splitting closure-copy policy from closure materialization in `copy_to_parent`, and added a focused regression test for wrapper reuse vs defensive copy behavior across scope boundaries.

### What changed
- `src/lisp/eval_promotion_copy.c3`
  - Added closure decision helpers:
    - `copy_parent_closure_in_releasing_scope(...)`
    - `copy_parent_should_reuse_closure(...)`
  - Added closure materialization helper:
    - `copy_parent_clone_closure_payload(...)`
  - Routed `copy_closure_to_parent(...)` through policy+payload helpers.
- `src/lisp/tests_tests.c3`
  - Added `run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test(...)`.
  - Covers:
    - reuse path for wrappers already in target chain,
    - defensive-copy path for disjoint wrapper values,
    - post-release refcount/liveness behavior on copied wrapper.
  - Wired into `run_memory_lifetime_regression_tests(...)`.

### Why this matters
- Further separates decision logic from mutation logic in a hot lifetime path.
- Locks in an easy-to-regress ownership invariant with explicit tests.
- Keeps progression aligned with audited boundary-hardening goals before deeper Fiber TEMP steps.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-05: Session 174 - Env-Copy Decision/Mutation Split

### Summary
Decomposed `eval_env_copy` internals into explicit decision and mutation helpers, keeping behavior unchanged while reducing boundary/lifetime bug surface.

### What changed
- `src/lisp/eval_env_copy.c3`
  - Added decision/policy helpers:
    - `copy_env_should_reuse_value(...)`
    - `copy_env_copy_by_boundary_policy(...)`
    - `copy_env_should_clone_closure(...)`
    - `copy_env_is_terminal_frame(...)`
  - Added mutation/materialization helpers:
    - `copy_env_clone_closure_payload(...)`
    - `copy_env_rewrite_persistent_parent(...)`
    - `copy_env_materialize_frame(...)`
  - Routed existing paths to the new helpers:
    - `copy_env_value_fast(...)`
    - `copy_env_clone_closure_if_needed(...)`
    - `copy_env_to_scope_inner(...)`

### Why this matters
- Separates policy decisions from mutating operations in one of the highest-risk boundary paths.
- Makes env-copy behavior easier to audit and less prone to accidental branching drift.
- Advances the boundary-hardening plan without introducing new runtime semantics.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-05: Session 173 - Shared Boundary Decision Helpers

### Summary
Reduced duplicated ownership/scope decision logic by introducing shared boundary helper predicates and routing promotion/copy/env-copy callsites through them.

### What changed
- `src/lisp/eval_boundary_api.c3`
  - Added shared decision helpers:
    - `boundary_ptr_in_target_scope_chain(...)`
    - `boundary_value_in_releasing_scope(...)`
    - `boundary_can_reuse_value(...)`
- `src/lisp/eval_promotion_copy.c3`
  - Replaced repeated reuse/copy conditions in:
    - `copy_cons_to_parent(...)`
    - `needs_wrapper_copy(...)`
    - `copy_to_parent_try_fast_reuse(...)`
  - Behavior preserved; fast-reuse/defensive-copy stats semantics unchanged.
- `src/lisp/eval_env_copy.c3`
  - Routed reusable-value checks through `boundary_can_reuse_value(...)`.
- `src/lisp/eval_promotion_context.c3`
  - Routed wrapper reuse gate through `boundary_can_reuse_value(...)`.
- `src/lisp/eval_promotion_escape.c3`
  - Routed target-chain fast-path check through `boundary_ptr_in_target_scope_chain(...)`.

### Why this matters
- Moves repeated boundary decisions toward a centralized policy surface.
- Lowers drift risk from divergent ad-hoc conditions in copy/promote/env-copy paths.
- Advances the main hardening plan item: split decision logic from mutation logic.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-05: Session 172 - Boundary State Save/Restore Centralization

### Summary
Centralized boundary interpreter state transitions (`current_scope`/`releasing_scope`) behind shared save/restore helpers, and added regression coverage for boundary state restoration invariants.

### What changed
- `src/lisp/eval_boundary_api.c3`
  - Added shared helper struct + functions:
    - `BoundaryInterpState`
    - `boundary_save_interp_state(...)`
    - `boundary_restore_interp_state(...)`
  - Migrated boundary mutators to use helper with `defer` restore:
    - `boundary_copy_to_scope_site(...)`
    - `boundary_alloc_value_in_scope(...)`
    - `boundary_make_env_in_scope(...)`
    - `boundary_env_extend_in_scope(...)`
    - `boundary_copy_from_releasing_scope(...)`
    - `boundary_copy_env_to_target_scope(...)`
- `src/lisp/tests_tests.c3`
  - Added regression:
    - `run_memory_lifetime_boundary_scope_restore_tests(...)`
  - Verifies boundary helpers restore interpreter scope/releasing state after temporary overrides.
  - Wired into `run_memory_lifetime_regression_tests(...)`.

### Why this matters
- Reduces boundary bug surface by removing repeated ad-hoc state save/restore logic.
- Makes ownership/lifetime context transitions explicit and reusable in one boundary-local place.
- Strengthens invariant coverage around interpreter boundary-state hygiene.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-05: Session 171 - Auto-Run Boundary Hardening on Boundary PRs

### Summary
Extended boundary hardening CI to run automatically on boundary-sensitive pull requests, so boundary policy enforcement is not limited to manual workflow runs.

### What changed
- `.github/workflows/boundary-hardening.yml`
  - Added `pull_request` trigger with focused `paths` filter for boundary-sensitive runtime/policy files.
  - Added `Resolve policy diff range` step:
    - auto-sets `OMNI_BOUNDARY_POLICY_RANGE=base_sha...head_sha` for PR events,
    - keeps `workflow_dispatch` override behavior via `policy_range` input.
  - Tightened PR-comment step condition to dispatch-only:
    - runs only on `workflow_dispatch` with non-empty `pr_number`.
- `docs/PROJECT_TOOLING.md`
  - Updated CI integration docs:
    - workflow now documents both `pull_request` and `workflow_dispatch` modes,
    - clarifies auto range behavior on PR runs.

### Why this matters
- Enforces boundary hardening policy automatically for boundary-touching PRs.
- Reduces reliance on manual dispatch for critical ownership/lifetime safety checks.
- Keeps main-plan focus: centralized, auditable boundary correctness gates.

### Validation
- `scripts/check_boundary_facade_usage.sh`
- `scripts/check_boundary_change_policy.sh build/boundary_hardening_normal.log build/boundary_hardening_asan.log`

## 2026-03-05: Session 170 - Boundary Change Policy Gate (ASAN Evidence)

### Summary
Added a boundary-change policy gate that requires normal+ASAN boundary profile evidence when boundary-sensitive files are touched.

### What changed
- `scripts/check_boundary_change_policy.sh`
  - New policy gate script.
  - Detects boundary-sensitive file changes (default `HEAD~1..HEAD`, or `OMNI_BOUNDARY_POLICY_RANGE` override).
  - When boundary files are changed, requires both normal and ASAN logs to contain passing suite summaries:
    - `stack_engine`, `scope_region`, `unified`, `compiler`
    - plus `fiber_temp_pool enabled=1` in both stages.
  - When no boundary-sensitive files are changed, emits a skip notice and exits success.
- `scripts/run_boundary_hardening.sh`
  - Added Stage 7 to execute policy gate after summary artifact generation.
- `.github/workflows/boundary-hardening.yml`
  - Added optional `workflow_dispatch` input:
    - `policy_range`
  - Exposes `OMNI_BOUNDARY_POLICY_RANGE` to the boundary-hardening runner.
- `docs/PROJECT_TOOLING.md`
  - Documented the new boundary policy gate and optional range controls.

### Why this matters
- Converts boundary sanitizer discipline from convention to explicit CI policy.
- Makes boundary-sensitive changes self-auditing in automation.
- Preserves current main-plan direction: hardening ownership/boundary paths before broader Fiber TEMP rollout.

### Validation
- `scripts/check_boundary_change_policy.sh build/boundary_hardening_normal.log build/boundary_hardening_asan.log`
- `scripts/run_boundary_hardening.sh`

## 2026-03-05: Session 169 - Boundary Facade CI Guard

### Summary
Added an explicit guard that fails boundary-hardening runs when direct boundary helpers are used outside sanctioned boundary implementation files.

### What changed
- `scripts/check_boundary_facade_usage.sh`
  - New CI/local guard script that scans `src/lisp` for direct calls to:
    - `copy_to_parent(...)`
    - `promote_to_escape(...)`
    - `promote_to_root(...)`
    - `copy_env_to_scope_inner(...)`
    - `scope_splice_escapes(...)`
  - Allows only sanctioned internal callsites (`eval_boundary_api.c3` + implementation modules).
  - Ignores `src/lisp/tests_*.c3` to avoid constraining low-level regression fixtures.
- `scripts/run_boundary_hardening.sh`
  - Added Stage 0 to run `scripts/check_boundary_facade_usage.sh` before build/test stages.
- `docs/PROJECT_TOOLING.md`
  - Documented the new boundary-facade guard in the boundary-hardening profile.

### Why this matters
- Enforces boundary API discipline in automation instead of relying on review memory.
- Prevents drift back to direct helper usage across runtime callsites.
- Keeps main-plan hardening focused on centralized ownership/boundary transitions.

### Validation
- `scripts/check_boundary_facade_usage.sh`
- `scripts/run_boundary_hardening.sh` (Stage 0 path exercised)

## 2026-03-05: Session 168 - Configurable Bot Login for PR Upsert

### Summary
Made boundary workflow PR-comment upsert bot identity configurable while preserving the secure default.

### What changed
- `.github/workflows/boundary-hardening.yml`
  - Added `workflow_dispatch` input:
    - `pr_comment_bot_login` (default: `github-actions[bot]`)
  - PR comment upsert matcher now uses that input for `user.login` matching, with fallback to `github-actions[bot]`.
- `docs/PROJECT_TOOLING.md`
  - Documented new `pr_comment_bot_login` input and behavior.

### Why this matters
- Supports repos/workflows that use alternate bot identities or tokens.
- Keeps deterministic upsert behavior without relaxing marker-based safety.
- Maintains main-plan focus on CI operational robustness.

### Validation
- Workflow YAML updated and remains structurally consistent.
- No runtime/test-path changes.

## 2026-03-05: Session 167 - Strict Bot Identity Match for PR Comment Upsert

### Summary
Tightened boundary workflow PR-comment upsert targeting so only comments authored by the GitHub Actions bot are eligible for marker-based updates.

### What changed
- `.github/workflows/boundary-hardening.yml`
  - PR comment lookup now requires:
    - `user.type == "Bot"`
    - `user.login == "github-actions[bot]"`
    - marker presence (`<!-- omni-boundary-hardening-summary -->`)
  - Existing pagination + marker-based upsert behavior remains unchanged.
- `docs/PROJECT_TOOLING.md`
  - Documented strict bot-identity match for upsert targeting.

### Why this matters
- Prevents accidental updates to other bot-authored comments containing similar text/markers.
- Keeps PR-comment bridge behavior deterministic and bounded to this workflow.

### Validation
- Workflow script logic updated (CI-only change).
- No runtime/test-path changes.

## 2026-03-05: Session 166 - Paginated PR Comment Upsert Lookup

### Summary
Hardened the boundary workflow PR-comment upsert path by adding paginated comment lookup, so existing marker comments are still found on PRs with large comment histories.

### What changed
- `.github/workflows/boundary-hardening.yml`
  - Updated the PR comment bridge script:
    - replaced single-page comment lookup (`per_page=100`) with paginated scan (up to 10 pages),
    - preserves marker-based upsert semantics,
    - updates existing marker comment when found; creates only when absent.
- `docs/PROJECT_TOOLING.md`
  - Documented that PR comment upsert lookup is paginated.

### Why this matters
- Prevents duplicate summary comments on long-lived/high-traffic PRs.
- Keeps CI summary publication stable as comment volume grows.
- Stays within the current plan: CI operational robustness, no runtime behavior changes.

### Validation
- Workflow logic updated and remains syntactically valid.
- No runtime/test-path changes introduced.

## 2026-03-05: Session 165 - PR Comment Upsert for Boundary Workflow

### Summary
Refined boundary workflow PR-comment behavior to upsert a single bot comment instead of posting a new comment on each run.

### What changed
- `.github/workflows/boundary-hardening.yml`
  - PR comment step now:
    - uses a stable marker (`<!-- omni-boundary-hardening-summary -->`) in comment body,
    - lists existing PR comments,
    - updates the most recent matching bot comment when present,
    - creates a new comment only when none exists.
- `docs/PROJECT_TOOLING.md`
  - Documented upsert behavior for the optional `pr_number` workflow input.

### Why this matters
- Reduces PR noise while preserving visibility of latest boundary profile status.
- Keeps CI outputs aligned to main plan operational goals without runtime changes.

### Validation
- Workflow logic updated to deterministic create-or-update flow keyed by marker.
- No runtime/test path changes; boundary profile scripts unchanged.

## 2026-03-05: Session 164 - Optional PR Comment Bridge for Boundary CI

### Summary
Extended boundary-hardening CI workflow with an optional PR-comment bridge that posts the generated boundary summary to a selected PR.

### What changed
- `.github/workflows/boundary-hardening.yml`
  - Added `workflow_dispatch` input:
    - `pr_number` (optional)
  - Added workflow permissions:
    - `contents: read`
    - `pull-requests: write`
  - Added summary markdown generation step:
    - writes `build/boundary_hardening_job_summary.md` via `scripts/emit_boundary_job_summary.sh`
  - Added PR comment step (`actions/github-script@v7`):
    - runs only when `pr_number` is provided
    - validates input and posts summary markdown as a PR comment
  - Uploads `build/boundary_hardening_job_summary.md` with other artifacts.
- `docs/PROJECT_TOOLING.md`
  - Documented optional `pr_number` input behavior.

### Why this matters
- Keeps the main plan focused on operationalizing boundary signals in CI.
- Reduces triage latency by surfacing the exact boundary summary directly in PR discussion when requested.
- Remains non-disruptive (manual dispatch + optional input).

### Validation
- Verified shell tooling remains valid:
  - `bash -n scripts/run_boundary_hardening.sh scripts/parse_boundary_summary.sh scripts/emit_boundary_job_summary.sh`
- Verified summary renderer output remains correct against current logs:
  - `scripts/emit_boundary_job_summary.sh build/boundary_hardening_normal.log build/boundary_hardening_asan.log`
  - output shows expected PASS rows and fail-field values.

## 2026-03-05: Session 163 - CI Job Summary Rendering for Boundary Profile

### Summary
Added a concise Markdown summary renderer for boundary hardening results and wired it into the GitHub workflow job summary.

### What changed
- Added script:
  - `scripts/emit_boundary_job_summary.sh`
  - Reads normal/ASAN boundary logs and emits a compact Markdown table with:
    - per-stage pass/fail status
    - `fail` fields for key suites
    - harness failure field
    - Fiber TEMP enabled field
- Updated workflow:
  - `.github/workflows/boundary-hardening.yml`
  - Added `if: always()` step to append summary output to `$GITHUB_STEP_SUMMARY` using the new script.
- Updated `docs/PROJECT_TOOLING.md` CI section to note job-summary publication.

### Why this matters
- Fast triage in CI without opening artifacts first.
- Keeps detailed logs/artifacts while surfacing the most important boundary signals inline.
- Maintains plan alignment: operationalizing boundary profile outputs rather than adding new runtime surface.

### Validation
- Local dry-run of summary renderer:
  - `scripts/emit_boundary_job_summary.sh build/boundary_hardening_normal.log build/boundary_hardening_asan.log`
  - Output table showed expected PASS + `fail=0` fields.
- Full boundary profile re-run:
  - `scripts/run_boundary_hardening.sh` passed normal + ASAN + assertions + JSON artifact emission.

## 2026-03-05: Session 162 - External CI Wiring for Boundary Profile

### Summary
Added a repository CI entrypoint for boundary hardening so the existing boundary profile can run in automation and publish artifacts.

### What changed
- Added GitHub Actions workflow:
  - `.github/workflows/boundary-hardening.yml`
  - trigger: `workflow_dispatch` (manual, non-disruptive)
  - runner: `self-hosted` Linux x64 (expects `c3c` + deps preinstalled)
  - runs `scripts/run_boundary_hardening.sh` with summary/assert/JSON enabled
  - uploads artifacts:
    - `build/boundary_hardening_normal.log`
    - `build/boundary_hardening_asan.log`
    - `build/boundary_hardening_summary.json`
- Updated `docs/PROJECT_TOOLING.md` with CI integration notes for boundary hardening profile.

### Why this matters
- Closes the remaining “external CI wiring” gap in the Fiber TEMP boundary-hardening plan.
- Makes boundary logs and machine-readable summary available as CI artifacts.
- Keeps default push/PR behavior stable by using manual dispatch only.
