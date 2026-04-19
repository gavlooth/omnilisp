# fiber-temp-session-plan Part 01

Source: `docs/plans/fiber-temp-session-plan.md`

# Fiber Temp Session Plan (Execution Cadence)

Date: 2026-03-05
Source roadmap: `docs/plans/fiber-temp-detailed-implementation-plan.md`
Target cadence: 16 sessions (small, auditable commits)
Execution status: Complete for current target (all planned phases landed behind guardrails; ownership model unchanged)

## Current Checkpoint (2026-03-05)

Completed this session:
- `1)` Design freeze sign-off: **pass** (proposal/guardrail docs aligned and frozen for execution).
- `2)` Ownership/guardrail checkpoint: **conditional pass**.
  - Region-centric ownership and no per-type RC drift remain enforced.
  - Open exception to resolve in next session (`4`): `src/stack_engine.c3` still has direct scope touchpoints (`stack_ctx_pin_scope`, `stack_ctx_unpin_scope`, `pinned_scope` field).
- `3)` Defer hot-path perf sign-off: **pass** with runtime counters and zero heap-overflow allocations in current suite run.
  - `STACK_DEFER_METRICS push=8 undefer=1 destroy_cb=9 clone_cb=1 update_arg=2 cloned_entries=2 peak_depth=3 heap_alloc=0`
- `4)` Deferred by request to next session.

Validation evidence:
- Normal: `c3c build` + `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  Result: `Stack engine 14/0`, `Unified 1177/0`, `Compiler 73/0`.
- ASAN strict: `c3c clean` + `c3c build --sanitize=address` + `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  Result: `Stack engine 14/0`, `Unified 1177/0`, `Compiler 73/0`.

## Item 4 Follow-up (2026-03-05)

Completed in subsequent session:
- Removed stack-layer direct scope touchpoints from `src/stack_engine.c3`:
  - `pinned_scope` field removed
  - `stack_ctx_pin_scope(...)` removed
  - `stack_ctx_unpin_scope(...)` removed
- Migrated suspend sites to defer-backed boundary guard usage (`suspend_with_scope_guard(...)`) in:
  - `src/lisp/jit_jit_runtime_effects.c3`
  - `src/lisp/jit_jit_handle_signal.c3`
  - `src/lisp/jit_jit_reset_shift.c3`
  - `src/lisp/primitives_iter_coroutine.c3`

Validation for follow-up:
- `c3c build` + `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`: pass (`Stack engine 15/0`, `Unified 1178/0`, `Compiler 73/0`)
- `c3c clean && c3c build --sanitize=address` + strict ASAN run: pass (`Stack engine 14/0`, `Unified 1177/0`, `Compiler 73/0`)

## Fiber TEMP Phase 1 Progress (2026-03-05)

Completed:
- Added conservative `OMNI_FIBER_TEMP` scaffold in `scope_region`:
  - chunk-pool state and helpers,
  - TEMP-lane reclaim hook in destroy/reset/splice paths,
  - summary counters (`OMNI_TEST_SUMMARY suite=fiber_temp_pool ...`).
- Validation remains green in normal + ASAN with flag OFF and ON smoke run.

Next:
- Start eligibility routing (narrow whitelist) so selected TEMP allocations can use Fiber TEMP in active stack contexts.

## Fiber TEMP Phase 2 Progress (2026-03-05)

Completed:
- Introduced narrow eligibility gate:
  - scopes become Fiber TEMP eligible only when created in active stack contexts (`OMNI_FIBER_TEMP` enabled, stack context active, parent scope present).
  - TEMP lane only: ESCAPE lane remains raw alloc/free.
- Added stack-engine exercise test for in-context scope create/release to drive pool metrics under the flag.

Validation:
- Normal: `Stack engine 16/0`, `Unified 1178/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 15/0`, `Unified 1177/0`, `Compiler 73/0`.
- Flagged metrics now non-zero (`hits=3`, `misses=3`, `returns=6`, `eligible_slow=4`, `bypass_large=0`), confirming active path exercise.

Next:
- Expand allocation-shape whitelist beyond size-only checks (for example bypass pool routing once ESCAPE-lane activity appears in a scope).

## Fiber TEMP Phase 2b Progress (2026-03-05)

Completed:
- Extended allocation-shape whitelist to include ESCAPE activity:
  - Fiber TEMP slow-path routing now requires no ESCAPE chunks/dtors on the scope.
  - Added `bypass_escape` metric counter for explicit observability.
- Extended stack-engine in-context scope test to exercise both:
  - a TEMP-heavy eligible scope,
  - a mixed TEMP+ESCAPE scope that should bypass Fiber TEMP pool routing.

Validation:
- Normal: `Stack engine 16/0`, `Unified 1178/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 15/0`, `Unified 1177/0`, `Compiler 73/0`.
- Flagged metrics: `hits=1`, `misses=3`, `returns=6`, `eligible_slow=2`, `bypass_large=0`, `bypass_escape=2`.

Next:
- Add focused assertions around Fiber TEMP pool invariants (take/reclaim behavior and bypass counters) without coupling tests to suite order.

## Fiber TEMP Phase 2c Progress (2026-03-05)

Completed:
- Added focused Fiber TEMP pool invariant checks in `scope_region`:
  - reclaim path (return vs drop behavior),
  - take-hit path,
  - take-miss path,
  using local before/after deltas to avoid suite-order coupling.

Validation:
- Normal: `Stack engine 16/0`, `Scope region 51/0`, `Unified 1178/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 15/0`, `Scope region 51/0`, `Unified 1177/0`, `Compiler 73/0`.
- Flagged metrics include exercised pool counters:
  - `hits=2`, `misses=4`, `returns=8`, `eligible_slow=2`, `bypass_escape=2`.

Next:
- Begin per-fiber TEMP ownership plumbing behind the flag while preserving stack-layer genericity (no direct scope logic in stack engine).

## Fiber TEMP Enablement Substrate Progress (2026-03-05)

Completed:
- Added a generic `StackCtx` lifecycle callback channel in `stack_engine` that is independent of LIFO defer push/pop.
- Added coverage for:
  - lifecycle destroy isolation from `stack_ctx_undefer(...)`,
  - lifecycle clone hook behavior.

Why this step:
- A previous attempt to bind persistent resource state via dynamic defer registration exposed a real risk:
  non-LIFO registrations can violate call-site assumptions around `stack_ctx_undefer(...)`.
- Lifecycle channel provides the correct primitive for persistent context-owned resources.

Validation:
- Normal: `Stack engine 18/0`, `Scope region 51/0`, `Unified 1178/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 17/0`, `Scope region 51/0`, `Unified 1177/0`, `Compiler 73/0`.

Next:
- Re-attempt Fiber TEMP per-context ownership using lifecycle callbacks (not defer stack), keeping default behavior unchanged when flag is off.

## Fiber TEMP Phase 3 Progress (2026-03-05)

Completed:
- Wired per-`StackCtx` Fiber TEMP chunk caches through lifecycle hooks:
  - context-local take/reclaim path added,
  - clone-aware shared context state via lifecycle clone callback,
  - lifecycle destroy callback flushes residual chunks to global pool.
- Extended metrics and tests:
  - summary now includes `ctx_hits` and `ctx_returns`,
  - stack-context exercise test now asserts `ctx_returns` delta under the flag.

Validation:
- Normal: `Stack engine 18/0`, `Scope region 51/0`, `Unified 1178/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 17/0`, `Scope region 51/0`, `Unified 1177/0`, `Compiler 73/0`.
- Flagged: `ctx_hits=1`, `ctx_returns=6`, no assertion failures.

Next:
- Stress clone/discard permutations with Fiber TEMP enabled to verify shared-context cache behavior under multi-shot continuation patterns.

## Fiber TEMP Phase 4 Progress (2026-03-05)

Completed:
- Added clone/discard stress coverage in stack-engine tests for Fiber TEMP lifecycle paths.
- New stress test validates repeated:
  - suspend,
  - clone + discard clone,
  - resume original to completion,
  with scope create/alloc/release around suspend boundaries.
- Under `OMNI_FIBER_TEMP=1`, test asserts per-context cache return activity (`ctx_return_count` delta).

Validation:
- Normal: `Stack engine 19/0`, `Scope region 51/0`, `Unified 1178/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 18/0`, `Scope region 51/0`, `Unified 1177/0`, `Compiler 73/0`.
- Flagged metrics show substantial ctx-path exercise (`ctx_hits=33`, `ctx_returns=70`).

Next:
- Add cross-thread/offload guard tests to ensure Fiber TEMP remains confined to stack-context owner thread and global fallback remains safe under worker interactions.

## Fiber TEMP Phase 5 Progress (2026-03-05)

Completed:
- Added scheduler/offload boundary test ensuring Fiber TEMP context-cache counters stay unchanged for repeated `thread-spawn`/`thread-join` operations (no stack context).
- Coverage is active only when `OMNI_FIBER_TEMP=1`; flag-off path remains stable.

Validation:
- Normal: `Stack engine 19/0`, `Scope region 51/0`, `Unified 1179/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 18/0`, `Scope region 51/0`, `Unified 1178/0`, `Compiler 73/0`.

Next:
- Add targeted scheduler wakeup/offload interleaving stress with Fiber TEMP enabled to widen boundary-race coverage.

## Fiber TEMP Phase 5c Progress (2026-03-05)

Completed:
- Added deterministic scheduler wakeup/offload interleaving stress:
  - async offload via spawn/await,
  - wakeup enqueue/drain in-loop,
  - thread offload spawn/join.
- Added end-of-test wakeup queue drain invariant (`head == tail`).

Validation:
- Normal: `Stack engine 19/0`, `Scope region 51/0`, `Unified 1180/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 18/0`, `Scope region 51/0`, `Unified 1179/0`, `Compiler 73/0`.

Next:
- Start focused audit of Fiber TEMP behavior at scheduler cancellation/timeouts boundaries (especially destroy-before-complete paths).

## Fiber TEMP Phase 6 Progress (2026-03-05)

Completed:
- Added targeted scheduler cancellation/timeout stress under Fiber TEMP:
  - timeout-immediate and cancel+join thread paths (with context-metric invariants),
  - timeout-success control path.
- Added repeated offload-fiber cancel stress (`spawn(offload ...)`, `fiber-cancel`, `run-fibers`) to cover destroy-before-complete behavior.

Validation:
- Normal: `Stack engine 19/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 18/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`.

Next:
- Begin focused metric/telemetry hardening for Fiber TEMP clone-share lifecycle (explicit counters for lifecycle clone/destroy pool flush behavior).

## Fiber TEMP Phase 6b Progress (2026-03-05)

Completed:
- Added explicit lifecycle telemetry counters:
  - context-pool create count,
  - lifecycle clone callback count,
  - lifecycle destroy callback count,
  - deferred-destroy count (shared refcount path),
  - destroy-time chunk flush count.
- Extended summary output to include lifecycle telemetry fields.
- Strengthened clone/discard stress test assertions to require lifecycle telemetry deltas under flag.

Validation:
- Normal: `Stack engine 19/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 18/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`.
- Flagged summary includes lifecycle fields (`ctx_pools`, `lc_clone`, `lc_destroy`, `lc_defer`, `lc_flush`).

Next:
- Add a focused leakage/retention guard test around repeated create/destroy cycles to verify lifecycle flush counters and pooled-count stability over long runs.

## Fiber TEMP Phase 6c Progress (2026-03-05)

Completed:
- Added long-run retention guard for clone/discard lifecycle:
  - 128 repeated create/suspend/clone-discard/resume/destroy cycles,
  - verifies lifecycle creation/flush activity,
  - asserts bounded pooled-chunk growth.
- Integrated into stack engine suite.

Validation:
- Normal: `Stack engine 20/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 19/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`.
- Flagged telemetry confirms heavy exercised lifecycle paths with stable pooled count.

Next:
- Begin a small rollout hygiene pass: tighten docs around Fiber TEMP enablement policy (explicitly experimental/flagged) and enumerate remaining production gates.

## Fiber TEMP Phase 6d Progress (2026-03-05)

Completed:
- Hardened stack engine thread-affinity boundaries so `StackPool`/`StackCtx` ownership is explicit and enforced:
  - added owner thread tokens on pool and context structs,
  - added runtime guards for create/destroy/init/switch/suspend/resume/clone and pool shutdown paths.
- Added targeted stack-engine ownership-state test:
  - `test_stack_ctx_thread_affinity_state()` verifies pool/context ownership tokens are initialized to the current thread.

Validation:
- Normal: `Stack engine 21/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 20/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`.
- Flagged (`OMNI_FIBER_TEMP=1` + summary): pass, with stable lifecycle telemetry counters.

Next:
- Expand thread-boundary stress to explicitly cover reject/fail-fast behavior for cross-thread stack-engine misuse in a non-production test harness.

## Fiber TEMP Phase 6e Progress (2026-03-05)

Completed:
- Extended thread-affinity guards to all stack defer/lifecycle API entry points (not only top-level context lifecycle calls).
- Added owner checks for defer register/pop/update and lifecycle attach/find/clone/destroy/clear paths, closing an internal safety gap for stack-owned teardown metadata.

Validation:
- Normal: `Stack engine 21/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 20/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`.
- Flagged (`OMNI_FIBER_TEMP=1` + summary): pass with stable Fiber TEMP telemetry.

Next:
- Add an opt-in misuse harness to exercise fail-fast cross-thread stack API violations outside the default CI suite.

## Fiber TEMP Phase 6f Progress (2026-03-05)

Completed:
- Added an explicit opt-in misuse probe for stack-affinity fail-fast verification:
  - new CLI mode: `--stack-affinity-probe`,
  - intentionally corrupts stack-context owner token and calls guarded destroy path,
  - expected outcome: deterministic non-zero process termination via ownership violation.
- This keeps default tests stable while giving a concrete harness for boundary-misuse verification.

Validation:
- Normal: `Stack engine 21/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 20/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`.
- Flagged (`OMNI_FIBER_TEMP=1` + summary): pass with stable telemetry.
- Probe run: `./build/main --stack-affinity-probe` exits non-zero (`132`) with expected fail-fast backtrace.

Next:
- Decide whether to add an opt-in harness wrapper (`OMNI_STACK_AFFINITY_HARNESS=1`) that auto-runs the probe subprocess and summarizes pass/fail in test mode.

## Fiber TEMP Phase 6g Progress (2026-03-05)

Completed:
- Added `OMNI_STACK_AFFINITY_HARNESS=1` wrapper in default test mode:
  - runs full suite as before,
  - then executes `--stack-affinity-probe` as a subprocess,
  - validates non-zero exit + expected fail-fast marker in probe output,
  - emits `OMNI_TEST_SUMMARY suite=stack_affinity_harness ...`.
- Default path remains unchanged when the env flag is unset.

Validation:
- Normal default: pass (`Stack engine 21/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`).
- Normal + harness: pass + `stack_affinity_harness pass=1 fail=0`.
- ASAN default: pass (`Stack engine 20/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`).
- ASAN + harness: pass + `stack_affinity_harness pass=1 fail=0`.

Next:
- Fold this harness summary into any CI profile that already sets `OMNI_TEST_SUMMARY=1` for boundary-hardening runs.
