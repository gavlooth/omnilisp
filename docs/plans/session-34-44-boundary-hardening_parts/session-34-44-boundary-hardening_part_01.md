# session-34-44-boundary-hardening Part 01

Source: `docs/plans/session-34-44-boundary-hardening.md`

# Session 34-44 Boundary Hardening Playbook

Scope:
- Consolidate lifetime/ownership transitions into one audited boundary API.
- Improve old internals (code quality + business logic), not just wrap them.
- Keep behavior stable while reducing bug surface.

Execution policy:
- Do not advance sessions unless all gates pass.
- Keep commits small and domain-local.
- Prefer behavior-preserving refactors before semantic changes.

## Current Status (2026-03-04)

- Sessions 34-44 goals: completed in sequence (see `memory/CHANGELOG.md` and commit history).
- Continued hardening/decomposition after Session 44: Sessions 45-68 completed.
- Validation discipline held for each session:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

### Session 178 Follow-up (2026-03-05): Boundary Invariant Coverage in Run/JIT + Interleaving Regression

- Extended invariant-hook call-site coverage into top-level run/JIT high-risk transitions:
  - `run_promote_result(...)` and `run(...)` in `src/lisp/eval_run_pipeline.c3`
  - `jit_finalize_scoped_result(...)`, `jit_eval_in_single_scope(...)`, `jit_eval_in_call_scope(...)` in `src/lisp/jit_jit_eval_scopes.c3`
- Added focused regression `run_memory_lifetime_boundary_scope_interleaving_test(...)` in `src/lisp/tests_tests.c3`:
  - exercises nested `boundary_enter_scope`/`boundary_leave_scope` interleaved with `boundary_push_child_scope`/`boundary_pop_child_scope`,
  - validates exact restoration of `current_scope` + `releasing_scope`,
  - validates release-copy path remains correct post-interleaving.
- Validation:
  - normal full suite: pass (`Unified 1186/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1185/0`, `Compiler 73/0`)

### Session 179 Follow-up (2026-03-05): Boundary Hook Coverage for REPL/Macro Scope Helpers

- Completed next low-risk residual-hook sweep:
  - `repl_eval_line(...)` in `src/lisp/eval_repl.c3`
  - `capture_template_bindings_in_root_scope(...)` in `src/lisp/macros_expansion.c3`
- Added `boundary_assert_interp_scope_chain(...)` at helper entry/exit and immediately after child-scope push where applicable.
- Validation:
  - normal full suite: pass (`Unified 1186/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1185/0`, `Compiler 73/0`)

### Session 180 Follow-up (2026-03-05): TCO Env-Copy Boundary Restore Consolidation

- Consolidated manual boundary-state save/restore in:
  - `jit_copy_tco_env_chain_for_recycle(...)` (`src/lisp/jit_jit_eval_scopes.c3`)
- Migration:
  - replaced ad-hoc `releasing_scope` save/restore with
    `boundary_save_interp_state(...)` + `defer boundary_restore_interp_state(...)`.
- Added regression:
  - `run_memory_lifetime_tco_boundary_state_restore_test(...)` in `src/lisp/tests_tests.c3`
  - ensures long named-let/TCO flow restores `current_scope` and `releasing_scope` exactly.
- Validation:
  - normal full suite: pass (`Unified 1187/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1186/0`, `Compiler 73/0`)

### Session 181 Follow-up (2026-03-05): TCO Recycle Error-Path Rollback Consolidation

- Refactored duplicated rollback/error branches in `jit_prepare_tco_recycle(...)`:
  - added `jit_tco_recycle_restore_on_error(...)` in `src/lisp/jit_jit_eval_scopes.c3`.
- Unified rollback guarantees now flow through one helper:
  - restore `current_scope` + `tco_recycle_scope`,
  - release fresh scope,
  - restore `jit_env`,
  - assert boundary invariant,
  - return runtime error.
- Added invariant hooks at TCO prepare entry/return points (fast path, alloc-failure, success).
- Validation:
  - normal full suite: pass (`Unified 1187/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1186/0`, `Compiler 73/0`)

### Session 182 Follow-up (2026-03-05): run_program Boundary-State Coverage

- Added focused regression `run_memory_lifetime_run_program_boundary_state_test(...)` in `src/lisp/tests_tests.c3`.
- Coverage intent:
  - lock in boundary-state restoration for top-level multi-form execution (`run_program`),
  - confirm `current_scope` + `releasing_scope` stability across parse+eval orchestration.
- Validation:
  - normal full suite: pass (`Unified 1188/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1187/0`, `Compiler 73/0`)

### Session 183 Follow-up (2026-03-05): JIT TCO Runtime-State Helper Consolidation

- Consolidated remaining distributed TCO call-state transitions in `src/lisp/jit_jit_eval_scopes.c3`:
  - added `JitCallScopeState` save/restore helpers,
  - added recycle-scope set/activate helpers,
  - routed single/call-scope defer registration failure cleanup through `boundary_pop_child_scope(...)`.
- Added regression `run_memory_lifetime_tco_runtime_fields_restore_test(...)` in `src/lisp/tests_tests.c3`:
  - validates runtime-field restore on both successful and erroring TCO runs:
    `current_scope`, `releasing_scope`, `tco_recycle_scope`,
    `tco_scope_defer_slot`, `tco_scope_defer_active`, `escape_env_mode`.
- Validation:
  - normal full suite: pass (`Unified 1189/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1188/0`, `Compiler 73/0`)

### Session 184 Follow-up (2026-03-05): Context-Switch Invariant Hooks in jit_common

- Added boundary invariant assertions at shared context-switch helpers in `src/lisp/jit_common.c3`:
  - `save_interp_state(...)` precondition check,
  - `restore_interp_state(...)` post-restore check.
- Rationale:
  - these helpers sit on stack/effect/JIT transition seams and are high leverage for failure localization.
- Validation:
  - normal full suite: pass (`Unified 1189/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1188/0`, `Compiler 73/0`)

### Session 185 Follow-up (2026-03-05): TCO Recycle Error-Path Coverage

- Added targeted regression `run_memory_lifetime_tco_recycle_error_restore_test(...)` in `src/lisp/tests_tests.c3`.
- Directly exercises `jit_prepare_tco_recycle(...)` error rollback branch (missing active stack context while defer retargeting is required) and verifies restore invariants.
- Replaced one direct single-scope scope restore with facade helper call:
  - `jit_eval_in_single_scope(...)` now uses `boundary_leave_scope(...)`.
- Validation:
  - normal full suite: pass (`Unified 1190/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1189/0`, `Compiler 73/0`)

### Session 186 Follow-up (2026-03-05): run_program Error-Path Boundary Coverage

- Added `run_memory_lifetime_run_program_error_boundary_state_test(...)` in `src/lisp/tests_tests.c3`.
- Verifies top-level multi-form execution preserves boundary state on:
  - parse-error exit,
  - runtime-error exit.
- Validation:
  - normal full suite: pass (`Unified 1191/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1190/0`, `Compiler 73/0`)

### Session 187 Follow-up (2026-03-05): Nested Releasing-Copy Interleaving Under Abort

- Added regression `run_memory_lifetime_promotion_abort_nested_releasing_copy_test(...)` in `src/lisp/tests_tests.c3`.
- Coverage target:
  - nested `boundary_copy_from_releasing_scope(...)` calls under a budget-aborted promotion context,
  - verifies boundary state restore, expected target-scope placement, non-aliasing fallback copies, and clean promotion-context teardown.
- Validation:
  - normal full suite: pass (`Unified 1192/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1191/0`, `Compiler 73/0`)

### Session 188 Follow-up (2026-03-05): Nested Promotion-Context Stack Discipline

- Added regression `run_memory_lifetime_nested_promotion_context_stack_test(...)` in `src/lisp/tests_tests.c3`.
- Coverage target:
  - nested outer-aborted + inner-budgeted promotion contexts under releasing-scope copies,
  - verifies context `prev` linkage, active-context restoration, inner memo reuse, outer abort non-aliasing fallback, and final context teardown.
- Validation:
  - normal full suite: pass (`Unified 1193/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass after immediate rerun (`Unified 1192/0`, `Compiler 73/0`)
  - note: observed one transient order-sensitive deduce fail on first ASAN run (`deduce repeated open/rebind`), not reproduced on rerun.

### Session 189 Follow-up (2026-03-05): Deduce Reopen Flake Hardening (ASAN)

- Root-cause hardening in `src/lisp/deduce.c3`:
  - replaced truncated `%x` `(uint)(usz)db` memory-db suffix with full-width `usz` hex formatting helper (`deduce_format_usz_hex`).
- Goal:
  - reduce rare ASAN allocator-layout collisions in `deduce 'open 'memory` temp paths (`/tmp/deduce-*`), which could surface as order-sensitive `deduce repeated open/rebind` failures.
- Validation:
  - normal full suite: pass (`Unified 1193/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1192/0`, `Compiler 73/0`)
  - repeated ASAN probe (3 full reruns): all pass (`Unified 1192/0`, `Compiler 73/0` each).

### Session 190 Follow-up (2026-03-05): Deduce Reopen Stress Test Coupling Reduction

- Updated `run_deduce_reopen_stress_test(...)` in `src/lisp/tests_tests.c3`:
  - replaced separate `run(...)` + `global_env.lookup(...)` pattern with single expression:
    `(begin (define ddb-reopen (deduce 'open 'memory)) ddb-reopen)`.
- Goal:
  - preserve test semantics while reducing out-of-band env lookup timing/order coupling in stress loops.
- Validation:
  - normal full suite: pass (`Unified 1193/0`, `Compiler 73/0`)
  - strict ASAN repeated full-run probe (3 runs): all pass (`Unified 1192/0`, `Compiler 73/0` each).

### Session 191 Follow-up (2026-03-05): Deduce Memory-Open Isolation Guardrail

- Runtime hardening in `src/lisp/deduce.c3`:
  - added process-sequence entropy in `'memory` temp DB path generation (`getpid` + monotonic seq + full-width pointer suffix).
- Regression added in `src/lisp/tests_tests.c3`:
  - `run_deduce_memory_open_isolation_test(...)` verifies two memory-open handles are storage-isolated (insert into A does not appear in B).
- Validation:
  - normal full suite: pass (`Unified 1194/0`, `Compiler 73/0`)
  - strict ASAN repeated full-run probe (3 runs): all pass (`Unified 1193/0`, `Compiler 73/0` each).

### Session 192 Follow-up (2026-03-05): Scheduler Mixed Boundary-State Coverage + Top-Level TCO Transient Cleanup

- Added scheduler regression `run_scheduler_mixed_boundary_state_restore_tests(...)` in `src/lisp/tests_tests.c3`:
  - mixes success (`thread-join` + `offload`/`await`) and expected-error (`thread-cancel` + `thread-join`) paths,
  - verifies boundary/runtime fields are restored after each cycle.
- Regression initially exposed stale `jit_tco_expr` / `jit_tco_env` leakage after top-level error exits.
- Runtime fix in `src/lisp/eval_run_pipeline.c3`:
  - introduced `run_clear_stale_jit_tco_state(...)`,
  - applied at run entry and via `defer` on exit in `run(...)` and `run_program(...)`.
- Validation:
  - normal full suite: pass (`Unified 1195/0`, `Compiler 73/0`)
  - strict ASAN repeated full-run probe (3 runs): all pass (`Unified 1194/0`, `Compiler 73/0` each).

### Session 193 Follow-up (2026-03-05): Scheduler Wakeup/Offload Expected-Error Boundary Coverage

- Added scheduler regression `run_scheduler_wakeup_offload_error_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - runs repeated triplets of:
    - success spawn/await with `offload 'sleep-ms`,
    - manual wakeup queue enqueue/drain (`WAKEUP_POLL_ERROR` with out-of-range fiber id),
    - expected-error spawn/await with invalid `offload 'nope`.
  - verifies boundary/runtime field stability after every phase:
    - `current_scope`, `releasing_scope`
    - `jit_env`, `match_env`
    - `jit_tco_expr`, `jit_tco_env`
    - `tco_recycle_scope`, `tco_scope_defer_slot`, `tco_scope_defer_active`
    - `escape_env_mode`, `active_promotion_ctx`
- Validation:
  - normal full suite: pass (`Unified 1196/0`, `Compiler 73/0`)
  - normal full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1196/0`, `Compiler 73/0`)
  - strict ASAN repeated full-run probe (3 runs): all pass (`Unified 1195/0`, `Compiler 73/0` each).

### Session 194 Follow-up (2026-03-05): Wakeup Wraparound Boundary-State Regression

- Added scheduler regression `run_scheduler_wakeup_wraparound_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - repeatedly fills wakeup ring to capacity and verifies overflow enqueue rejection,
  - drains queue and verifies head/tail convergence each cycle,
  - asserts interpreter boundary/runtime field restoration after each fill+drain cycle.
- Wired into `run_scheduler_tests(...)`.
- Validation:
  - normal full suite: pass (`Unified 1197/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1196/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1196/0`, `Compiler 73/0`)

### Session 195 Follow-up (2026-03-05): Mixed Wakeup-Event Boundary Ordering

- Added scheduler regression `run_scheduler_wakeup_mixed_event_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - alternates `WAKEUP_POLL_ERROR` and `WAKEUP_TIMER_EXPIRED` ordering for the same blocked pending-read fiber,
  - verifies first-event-wins completion semantics (`error` vs `timed_out`) and `FIBER_BLOCKED -> FIBER_READY` transition,
  - verifies wakeup ring drains cleanly and interpreter boundary/runtime fields remain unchanged.
- Wired into `run_scheduler_tests(...)`.
- Validation:
  - normal full suite: pass (`Unified 1198/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1197/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1197/0`, `Compiler 73/0`)

### Session 196 Follow-up (2026-03-05): Invalid Offload Wakeup Payload Drain

- Added scheduler regression `run_scheduler_invalid_offload_wakeup_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - enqueues `WAKEUP_OFFLOAD_READY` with out-of-range fiber ids and real `OffloadCompletion*` payloads,
  - interleaves additional invalid poll-error events,
  - verifies wakeup ring drains fully and interpreter boundary/runtime fields remain unchanged.
- Wired into `run_scheduler_tests(...)`.
- Validation:
  - normal full suite: pass (`Unified 1199/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1198/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1198/0`, `Compiler 73/0`)

### Session 197 Follow-up (2026-03-05): Wakeup Ready-Barrier Ordering

- Added scheduler regression `run_scheduler_wakeup_ready_barrier_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - constructs a queue with slot-0 unready and slot-1 ready events,
  - verifies `drain_wakeups()` stops at the first unready slot (no out-of-order processing),
  - then releases slot-0 readiness and verifies ordered completion semantics and queue convergence,
  - asserts interpreter boundary/runtime fields remain unchanged across both drains.
- Wired into `run_scheduler_tests(...)`.
- Validation:
  - normal full suite: pass (`Unified 1200/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1199/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1199/0`, `Compiler 73/0`)

### Session 198 Follow-up (2026-03-05): Duplicate/Late Offload-Ready Semantics

- Added scheduler regression `run_scheduler_duplicate_offload_ready_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - drives duplicate and late `WAKEUP_OFFLOAD_READY` events for one active blocked pending offload,
  - verifies first completion payload retention and extra payload discard semantics,
  - verifies queue convergence, blocked->ready transition, and boundary/runtime state stability after both phases.
- Wired into `run_scheduler_tests(...)`.
- Validation:
  - normal full suite: pass (`Unified 1201/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1200/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1200/0`, `Compiler 73/0`)

### Session 199 Follow-up (2026-03-05): Pending-Offload Consume Path Boundary Checks

- Added scheduler regression `run_scheduler_consume_pending_offload_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - exercises direct `scheduler_consume_pending_offload(...)` pre-completion, completed, and post-reset paths,
  - alternates completion kinds (`OFFLOAD_RES_INT` and cancel/error completion),
  - verifies returned value semantics, pending-slot reset, and boundary/runtime field stability after each phase.
- Wired into `run_scheduler_tests(...)`.
- Validation:
  - normal full suite: pass (`Unified 1202/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1201/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1201/0`, `Compiler 73/0`)

### Session 200 Follow-up (2026-03-05): Pending-TCP-Read Consume Path Boundary Checks

- Added scheduler helpers in `src/lisp/tests_tests.c3`:
  - `scheduler_seed_completed_tcp_read_case(...)`
  - `scheduler_tcp_case_value_ok(...)`
- Added scheduler regression `run_scheduler_consume_pending_tcp_read_boundary_tests(...)`:
  - exercises direct `scheduler_consume_pending_tcp_read(...)` pre-completion error path,
  - exercises completed consume variants (timeout/error/empty/non-empty),
  - verifies returned value semantics and pending-read slot reset,
  - verifies post-reset consume returns expected error,
  - verifies boundary/runtime field stability after each phase.
- Wired into `run_scheduler_tests(...)`.
- Validation:
  - normal full suite: pass (`Unified 1203/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1202/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1202/0`, `Compiler 73/0`)

### Session 201 Follow-up (2026-03-05): Offload Consume Bytes Branch

- Extended `run_scheduler_consume_pending_offload_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - added `OFFLOAD_RES_BYTES` completion mode (`shared_blob_new_copy("blob-ok")`),
  - verifies consumed bytes path returns expected `STRING` payload,
  - retains boundary/runtime-state and slot-reset assertions across all consume phases.
- Validation:
  - normal full suite: pass (`Unified 1203/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1202/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1202/0`, `Compiler 73/0`)
