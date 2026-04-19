    - take-hit behavior (`take_hits`),
    - take-miss behavior (`take_misses`).
  - Assertions use local before/after deltas to avoid brittle dependence on prior suite activity.

### Why this matters
- Increases confidence in Fiber TEMP pool mechanics without requiring global counter resets.
- Keeps default (flag-off) suite stable while adding meaningful coverage for flag-on runs.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 16/0`, `Scope region 51/0`, `Unified 1178/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Scope region 51/0`, `Unified 1177/0`, `Compiler 73/0`)
- Flagged metrics run:
  - `c3c build`
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=scope_region pass=51 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=2 misses=4 returns=8 drop_frees=0 pooled=6 peak=6 eligible_slow=2 bypass_large=0 bypass_escape=2`

## 2026-03-05: Session 144 - Fiber TEMP Test Hardening (Per-Test Metric Deltas)

### Summary
Hardened Fiber TEMP validation in stack-engine tests by asserting per-test metric deltas (not absolute global counters), reducing order sensitivity while preserving coverage of the new ESCAPE-aware bypass path.

### What changed
- `src/stack_engine.c3`
  - Extended `FiberTempScopeState` with:
    - `bypass_escape`
    - `eligible_slow`
  - `test_entry_scope_create_in_stack_ctx(...)` now captures local before/after deltas for:
    - `g_fiber_temp_pool_stats.bypass_escape_activity_allocs`
    - `g_fiber_temp_pool_stats.eligible_slow_allocs`
  - `test_stack_ctx_scope_create_in_context()` now asserts under `OMNI_FIBER_TEMP=1`:
    - bypass path exercised (`bypass_escape == 1`)
    - eligible slow-path exercised (`eligible_slow == 1`)

### Why this matters
- Keeps Fiber TEMP assertions stable across test ordering and prior suite activity.
- Verifies both positive and bypass routing behavior under real stack-context execution.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 16/0`, `Unified 1178/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1177/0`, `Compiler 73/0`)
- Flagged metrics run:
  - `c3c build`
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_engine pass=16 fail=0`
    - `OMNI_TEST_SUMMARY suite=scope_region pass=50 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=1 misses=3 returns=6 drop_frees=0 pooled=5 peak=5 eligible_slow=2 bypass_large=0 bypass_escape=2`

## 2026-03-05: Session 143 - Fiber TEMP Shape Whitelist (ESCAPE-Aware Bypass)

### Summary
Hardened Fiber TEMP routing from size-only eligibility to shape-aware eligibility:
- Fiber TEMP pool routing now bypasses scopes that have ESCAPE-lane activity.
- This keeps Fiber TEMP focused on ephemeral TEMP-heavy scopes and avoids broad adoption in mixed TEMP/ESCAPE scope shapes.

### What changed
- `src/scope_region.c3`
  - Added `fiber_temp_scope_shape_eligible(...)`.
  - `alloc_slow(...)` Fiber TEMP decision now requires:
    - scope eligible (`fiber_temp_eligible`),
    - size eligible (`<= FIBER_TEMP_ELIGIBLE_MAX_ALLOC`),
    - shape eligible (no ESCAPE chunks/dtors on the scope).
  - Added new metric counter:
    - `bypass_escape_activity_allocs`
  - Extended summary line:
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool ... bypass_escape=...`
- `src/stack_engine.c3`
  - Extended in-context scope exercise to include a mixed TEMP+ESCAPE child scope and verify `bypass_escape` behavior when `OMNI_FIBER_TEMP=1`.
- Planning/docs sync:
  - `.claude/plans/fiber-temp-session-plan.md`
  - `.claude/plans/fiber-temp-detailed-implementation-plan.md`
  - Updated stale metrics text and recorded shape-whitelist progression.

### Why this matters
- Reduces risk of accidental broad Fiber TEMP routing while preserving current gains.
- Keeps ownership semantics unchanged: ESCAPE lane remains deterministic and independent.
- Provides observability for the new bypass reason via explicit metrics.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 16/0`, `Unified 1178/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1177/0`, `Compiler 73/0`)
- Flagged metrics run:
  - `c3c build`
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_engine pass=16 fail=0`
    - `OMNI_TEST_SUMMARY suite=scope_region pass=50 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=1 misses=3 returns=6 drop_frees=0 pooled=5 peak=5 eligible_slow=2 bypass_large=0 bypass_escape=2`

## 2026-03-05: Session 142 - Fiber TEMP Eligibility Gate + In-Context Exercise

### Summary
Advanced Fiber TEMP from pure scaffold to a narrow eligibility model:
- only scopes created in active stack contexts are marked Fiber TEMP eligible,
- only TEMP lane uses pool-backed paths when eligible,
- ESCAPE lane remains unchanged.

Also added a stack-engine test that creates/releases scopes inside a stack context so Fiber TEMP pool behavior is exercised under the feature flag.

### What changed
- `src/scope_region.c3`
  - Added `ScopeRegion.fiber_temp_eligible`.
  - Added `scope_fiber_temp_context_eligible()` and used it at scope creation.
  - `scope_create(...)` now marks scopes eligible only when:
    - `OMNI_FIBER_TEMP` is enabled,
    - a stack context is active,
    - parent scope is non-null.
  - Added `scope_temp_chunk_release(...)`:
    - eligible TEMP chunks → `scope_chunk_reclaim_temp(...)`
    - non-eligible TEMP chunks → direct `mem::free(...)`
  - `alloc_slow(...)` now selects:
    - `scope_chunk_alloc_temp(...)` for eligible scopes,
    - `scope_chunk_alloc_raw(...)` otherwise.
  - TEMP reset/destroy/splice paths now use eligibility-aware release.
  - ESCAPE lane stays on raw allocation/free paths.
- `src/stack_engine.c3`
  - Added `test_stack_ctx_scope_create_in_context()` + entry helper to exercise scope create/release and TEMP slow-path overflow inside active stack context.
  - Wired into `run_stack_engine_tests()`.

### Why this matters
- This is the first meaningful routing gate for Fiber TEMP without changing ownership semantics.
- It keeps risk bounded:
  - no broad “all TEMP everywhere” enablement,
  - no ESCAPE lane interference,
  - no stack-core ownership drift.
- It provides a deterministic exercise path for metrics and regressions.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 16/0`, `Unified 1178/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1177/0`, `Compiler 73/0`)
- Flagged metrics run:
  - `c3c build`
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_engine pass=16 fail=0`
    - `OMNI_TEST_SUMMARY suite=scope_region pass=50 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=3 misses=3 returns=6 drop_frees=0 pooled=3 peak=3 eligible_slow=4 bypass_large=0`

## 2026-03-05: Session 141 - Fiber TEMP Phase 1 Skeleton (Flagged, Conservative)

### Summary
Implemented a conservative Phase 1 scaffold for Fiber TEMP in `scope_region`:
- feature flag gate (`OMNI_FIBER_TEMP`),
- chunk-pool skeleton + counters,
- no default behavior change when flag is off.

This is intentionally infrastructure-only; no broad allocation routing changes yet.

### What changed
- `src/scope_region.c3`
  - Added Fiber TEMP gate helpers:
    - `scope_fiber_temp_flag_enabled()`
    - `scope_fiber_temp_enabled()`
  - Added chunk-pool state + metrics:
    - `g_fiber_temp_chunk_pool`
    - `g_fiber_temp_chunk_pool_count`
    - `g_fiber_temp_pool_stats`
    - `FIBER_TEMP_CHUNK_POOL_MAX`
  - Added pool operations:
    - `fiber_temp_chunk_try_take(...)`
    - `scope_chunk_reclaim_temp(...)`
  - Added explicit chunk allocation split:
    - `scope_chunk_alloc_temp(...)` (TEMP lane, flagged pool path)
    - `scope_chunk_alloc_raw(...)` (raw fallback / ESCAPE lane)
  - Replaced direct frees for TEMP-lane chunk teardown paths (destroy/reset/splice) with `scope_chunk_reclaim_temp(...)`.
  - ESCAPE-lane chunk behavior remains direct-free (unchanged).
  - Extended test summary output with:
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool ...`
  - Extended `scope_freelist_cleanup()` to drain fiber-temp chunk pool.

### Guardrail notes
- Ownership authority remains region-centric; this session does not alter boundary promotion semantics.
- Stack engine remains generic; no new `ScopeRegion` logic was added there.
- Finalizer-bearing value policy unchanged.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1178/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1177/0`, `Compiler 73/0`)
- Flagged smoke (`OMNI_FIBER_TEMP=1`):
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=scope_region pass=50 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=0 misses=0 returns=0 drop_frees=0 pooled=0 peak=0`

## 2026-03-05: Session 140 - Close Item 4 (Remove Stack-Layer Scope Coupling)

### Summary
Completed the deferred item `4`: removed `ScopeRegion` touchpoints from `stack_engine` and moved suspend-lifetime scope retention fully into runtime boundary logic via the generic defer substrate.

### What changed
- `src/stack_engine.c3`
  - Removed `StackCtx.pinned_scope`.
  - Removed stack-layer scope APIs:
    - `stack_ctx_pin_scope(...)`
    - `stack_ctx_unpin_scope(...)`
  - `stack_ctx_destroy(...)` no longer performs direct scope unpin/release.
  - Stack core now manages only generic defer entries and stack lifecycle.
- Runtime suspend sites migrated to defer-backed scope guards:
  - `src/lisp/jit_jit_runtime_effects.c3`
    - `jit_shift_value(...)`
    - handler-resume suspend path in `jit_signal_try_resume_handler(...)`
  - `src/lisp/jit_jit_handle_signal.c3`
    - `jit_signal_suspend(...)`
  - `src/lisp/jit_jit_reset_shift.c3`
    - `jit_shift_impl(...)`
  - `src/lisp/primitives_iter_coroutine.c3`
    - `prim_yield_suspend_and_restore(...)` now returns `bool`
    - `prim_yield(...)` surfaces guard-registration failure as runtime error

### Why this matters
- Closes the layering violation: stack core no longer encodes Lisp scope ownership semantics.
- Preserves suspend/destroy/clone safety by using existing generic defer ownership hooks in runtime (`suspend_with_scope_guard(...)`), keeping ownership authority in boundary/runtime code.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1178/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1177/0`, `Compiler 73/0`)
- Metrics summary check:
  - `OMNI_STACK_DEFER_METRICS=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `STACK_DEFER_METRICS push=8 undefer=1 destroy_cb=9 clone_cb=1 update_arg=2 cloned_entries=2 peak_depth=3 heap_alloc=0`

## 2026-03-05: Session 139 - Fiber-Temp Checkpoint (Items 1/2/3)

### Summary
Completed the requested checkpoint scope for the fiber-temp roadmap:
- `1)` design freeze sign-off,
- `2)` ownership/guardrail checkpoint,
- `3)` defer hot-path perf sign-off,
while deferring item `4` to the next session.

### What changed
- `src/stack_engine.c3`
  - Added defer-runtime counters and summary emission for hot-path sign-off:
    - `g_stack_defer_push_count`
    - `g_stack_defer_undefer_count`
    - `g_stack_defer_destroy_callback_count`
    - `g_stack_defer_clone_callback_count`
    - `g_stack_defer_update_arg_count`
    - `g_stack_defer_entries_cloned`
    - `g_stack_defer_peak_depth`
    - `g_stack_defer_heap_alloc_count`
  - Added helpers:
    - `stack_defer_metrics_enabled()`
    - `stack_defer_metrics_reset()`
    - `emit_stack_defer_summary()`
  - Stack test runner now emits machine-readable defer summary when `OMNI_TEST_SUMMARY=1`.
- Plan/docs checkpoint updates:
  - `.claude/plans/fiber-temp-session-plan.md`
  - `.claude/plans/fiber-temp-detailed-implementation-plan.md`
  - `.claude/plans/fiber-temp-teardown-revision-summary.md`
  - `fiber-temp-teardown-revision-summary.md`
  - Added explicit checkpoint status and open carry-over item for session `4`.

### Checkpoint status
- Item `1` (design freeze): pass.
- Item `2` (ownership checkpoint): conditional pass.
  - Open gap carried forward: stack-layer direct scope touchpoints (`pinned_scope`, `stack_ctx_pin_scope`, `stack_ctx_unpin_scope`) still need removal/replacement to fully satisfy the "generic stack core" guardrail.
- Item `3` (defer perf sign-off): pass.
  - Observed metrics:
    - `STACK_DEFER_METRICS push=8 undefer=1 destroy_cb=9 clone_cb=1 update_arg=2 cloned_entries=2 peak_depth=3 heap_alloc=0`

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1177/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean`
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1177/0`, `Compiler 73/0`)
- Metrics summary run:
  - `OMNI_STACK_DEFER_METRICS=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_defer push=8 undefer=1 destroy_cb=9 clone_cb=1 update_arg=2 cloned_entries=2 peak_depth=3 heap_alloc=0`

## 2026-03-05: Session 138 - JIT Policy Test Decomposition

### Summary
Refactored a large, mixed-responsibility test function into focused helpers without changing behavior.

### What changed
- `src/lisp/tests_tests.c3`
  - Decomposed `run_jit_policy_tests(...)` into:
    - `run_jit_policy_warm_cache_tests(...)`
    - `run_jit_policy_gc_safe_point_test(...)`
    - `run_jit_policy_boundary_reset_test(...)`
    - `run_jit_policy_stale_raise_scrub_test(...)`
  - Kept public test harness flow and pass/fail semantics unchanged.
  - Main `run_jit_policy_tests(...)` now orchestrates helper calls and keeps shared setup/restore in one place.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1178/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1177/0`, `Compiler 73/0`)

## 2026-03-05: Session 137 - Stale Raise State Regression Test

### Summary
Added explicit regression coverage to lock in the ASAN stale-raise fix at top-level run boundaries.

### What changed
- `src/lisp/tests_tests.c3`
  - In `run_jit_policy_tests(...)`, added a targeted case that:
    - manually seeds `interp.flags.raise_pending` + `raise_msg`,
    - executes a top-level `run(...)` for a `handle` form,
    - asserts:
      - result is `5` (not stale raise-clause result),
      - `raise_pending` is cleared,
      - `raise_msg_len` is reset.
  - Emits:
    - `[PASS] jit policy: top-level run scrubs stale pending raise state`

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1178/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1177/0`, `Compiler 73/0`)

## 2026-03-05: Session 136 - Parser Edge Coverage Expansion (Import/Export/Type Helpers)

### Summary
Expanded focused parser edge-case coverage around recently refactored helper paths:
- import specifier list and alias parsing,
- export-from list validation,
- deftype/defunion compound helper behavior on malformed params/variants.

### What changed
- `src/lisp/tests_advanced_tests.c3`
  - Added `run_advanced_parser_import_edge_tests(...)` with targeted error-shape checks:
    - missing `:as`,
    - missing alias after `:as` (paren and symbol forms),
    - non-symbol in import list,
    - missing close paren.
  - Extended `run_advanced_parser_export_from_edge_tests(...)`:
    - nested list element error (`expected symbol in export-from list`).
  - Extended `run_advanced_parser_type_def_edge_tests(...)`:
    - non-symbol compound type params in `deftype`/`defunion`,
    - non-symbol union variant name.
  - Wired new import-edge test group into module-system advanced tests.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1177/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1176/0`, `Compiler 73/0`)

## 2026-03-05: Session 135 - Isolated Group Reset Consolidation

### Summary
Reduced harness coupling and repeated boilerplate by moving global group-boundary reset logic into the shared isolated-group runner.

### What changed
- `src/lisp/tests_tests.c3`
  - Added `run_test_global_boundary_reset(...)` for global JIT/pool reset state.
  - Updated `run_group_isolated(...)` to run global reset both before and after each isolated group.
  - Updated `run_test_group_boundary_reset(...)` to delegate global reset to shared helper and keep only per-interpreter reset fields.
  - Removed repeated `run_test_group_boundary_reset(interp)` calls between isolated group invocations in `run_lisp_tests()`.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1168/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1167/0`, `Compiler 73/0`)

## 2026-03-05: Session 134 - ASAN Raise-Pending Boundary Hardening

### Summary
Fixed an ASAN-only regression where `raise_pending` leaked across top-level `run()` boundaries and caused unrelated `handle` forms to spuriously dispatch `raise` clauses.

### Root cause
- `raise_pending` is meaningful only while unwinding inside active handlers.
- Under interpreter-only/ASAN paths, stale pending raise state could survive between independent top-level evaluations.
- This surfaced as:
  - `tco-recycle: effects in loop` failing in ASAN (`interp=FAIL`),
  - standalone `handle` repro returning `999` instead of `5`.

### What changed
- `src/lisp/eval_run_pipeline.c3`
  - Added `run_clear_stale_raise_state(...)`.
  - Guarded cleanup to top-level-safe boundary only:
    - clears state only when `handler_count == 0` and `raise_pending == true`.
  - Applied cleanup at run boundaries:
    - entry of `run_program(...)`,
    - per-expression loop in `run_program(...)`,
    - entry of `run(...)`.

### Validation
- Targeted repro:
  - ASAN build now evaluates
    - `(handle (let loop (n 5 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc 1)))) (raise msg 999))`
    - to `5` (was `999`).
- Full normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1168/0`, `Compiler 73/0`)
- Full ASAN strict:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1167/0`, `Compiler 73/0`)

## 2026-03-05: Session 133 - Diagnostic/JIT Policy Isolation

### Summary
Isolated `diagnostic` and `jit policy` unified test groups into fresh interpreters to reduce side effects from JIT-state and error-path mutations across the main shared test sequence.

### What changed
- `src/lisp/tests_tests.c3`
  - Added wrappers:
    - `run_diagnostic_tests_isolated(...)`
    - `run_jit_policy_tests_isolated(...)`
  - Updated `run_lisp_tests()` to execute both groups through isolated wrappers.
  - Wrappers use shared `run_group_isolated(...)`.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1168/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1167/0`, `Compiler 73/0`)

## 2026-03-05: Session 132 - Pika/Unicode/Compression/JSON Isolation

### Summary
Completed another order-sensitivity reduction pass by isolating four additional mid-suite groups in fresh interpreters: `pika`, `unicode`, `compression`, and `json`.

### What changed
- `src/lisp/tests_tests.c3`
  - Added wrappers:
    - `run_pika_tests_isolated(...)`
    - `run_unicode_tests_isolated(...)`
    - `run_compression_tests_isolated(...)`
    - `run_json_tests_isolated(...)`
  - Updated `run_lisp_tests()` to call isolated wrappers for these groups.
  - All wrappers use shared `run_group_isolated(...)`.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1168/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1167/0`, `Compiler 73/0`)

## 2026-03-05: Session 131 - Reader/Schema Group Isolation

### Summary
Further reduced unified-suite order sensitivity by isolating `reader dispatch` and `schema` test groups in fresh interpreter instances.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_reader_dispatch_tests_isolated(...)`
    - `run_schema_tests_isolated(...)`
  - Updated `run_lisp_tests()` to execute:
    - `run_reader_dispatch_tests_isolated(...)`
    - `run_schema_tests_isolated(...)`
  - Both wrappers route through shared `run_group_isolated(...)`.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1168/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1167/0`, `Compiler 73/0`)

## 2026-03-05: Session 130 - Deduce Isolation Routed Through Shared Helper

### Summary
Completed the isolated-group helper rollout by moving Deduce’s isolated execution path to the shared `run_group_isolated(...)` mechanism.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_deduce_group_tests(...)` (contains Deduce group body)
  - Updated:
    - `run_deduce_tests(...)` now delegates to:
      - `run_group_isolated(&run_deduce_group_tests, pass, fail)`
  - Removed duplicated local fresh-interpreter setup/teardown from `run_deduce_tests(...)`.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1168/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1167/0`, `Compiler 73/0`)

## 2026-03-05: Session 129 - Isolated Test-Group Helper Consolidation

### Summary
Refactored repeated “fresh interpreter per group” boilerplate into a single helper in the unified test harness, preserving behavior while reducing duplicated setup/teardown logic.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `alias IsolatedTestGroupFn = fn void(Interp* interp, int* pass, int* fail);`
    - `run_group_isolated(...)`
  - Updated wrappers to delegate to shared helper:
    - `run_async_tests_isolated(...)`
    - `run_http_tests_isolated(...)`
    - `run_scheduler_tests_isolated(...)`
    - `run_atomic_tests_isolated(...)`

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1168/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1167/0`, `Compiler 73/0`)

## 2026-03-05: Session 128 - Further Unified Test Isolation (Async/HTTP)

### Summary
Extended the order-sensitivity reduction pattern to additional side-effect-heavy groups by isolating `async` and `http` test execution in fresh interpreter instances.

### What changed
- `src/lisp/tests_tests.c3`
  - Added wrappers:
    - `run_async_tests_isolated(...)`
    - `run_http_tests_isolated(...)`
  - Each wrapper:
    - creates a fresh `Interp`,
    - registers primitives + stdlib,
    - runs group tests,
    - destroys/frees interpreter.
  - Updated `run_lisp_tests()` to call isolated wrappers for async/http groups.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1168/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1167/0`, `Compiler 73/0`)

## 2026-03-05: Session 127 - Test Order-Sensitivity Reduction (Scheduler/Atomic Isolation)

### Summary
Reduced shared-interpreter coupling further by isolating additional stateful unified-test groups (`scheduler`, `atomic`) into fresh interpreter instances.

### What changed
- `src/lisp/tests_tests.c3`
  - Added wrappers:
    - `run_scheduler_tests_isolated(...)`
    - `run_atomic_tests_isolated(...)`
  - Each wrapper:
    - creates a fresh `Interp`,
    - registers primitives + stdlib,
    - runs its group,
    - destroys/free the interpreter.
  - Updated `run_lisp_tests()` to call isolated wrappers instead of running those groups on the shared suite interpreter.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 15/0`, `Unified 1168/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 14/0`, `Unified 1167/0`, `Compiler 73/0`)

## 2026-03-05: Session 126 - Deduce ASAN Stability (Finalizer + Isolated Test Group)

### Summary
Addressed intermittent ASAN-only Deduce failures by hardening resource cleanup for Deduce DB handles and reducing cross-group test coupling.

### What changed
- `src/lisp/deduce.c3`
  - Added `deduce_db_finalizer(void* handle)` to deterministically close `mdb_env` (`mdb_env_close`) for `deduce-db` handles.
  - Wired `prim_deduce_open(...)` to create `deduce-db` handles with that finalizer via `make_ffi_handle_ex(...)`.
- `src/lisp/tests_tests.c3`
  - Added explicit Deduce open smoke helper:
    - `run_deduce_open_smoke_test(...)`
  - Added reopen/rebind stress helper:
    - `run_deduce_reopen_stress_test(...)` (64 sequential rebind opens)
  - Isolated Deduce test group into a fresh interpreter instance in `run_deduce_tests(...)` to avoid order-sensitive contamination from prior groups.

### Validation
- Normal:
