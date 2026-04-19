# session-34-44-boundary-hardening Part 02

Source: `docs/plans/session-34-44-boundary-hardening.md`

### Session 202 Follow-up (2026-03-05): Thread-Task Completion Ownership Paths

- Added scheduler regression `run_scheduler_thread_task_completion_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - exercises thread-task completion lifecycle (`alloc -> complete -> take`) and verifies slot teardown,
  - exercises invalid-id completion submission path for cleanup safety,
  - verifies boundary/runtime field stability across iterations.
- Wired into `run_scheduler_tests(...)`.
- Validation:
  - normal full suite: pass (`Unified 1204/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1203/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1203/0`, `Compiler 73/0`)

### Session 203 Follow-up (2026-03-05): Thread-Task Cancel State Machine

- Added scheduler regression `run_scheduler_thread_task_cancel_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - verifies cancel path for pending tasks (`already_done=false`, begin denied, completion take succeeds),
  - verifies cancel path for done tasks (`already_done=true`, completion remains retrievable),
  - verifies invalid-id cancel returns false,
  - verifies boundary/runtime field stability across iterations.
- Wired into `run_scheduler_tests(...)`.
- Validation:
  - normal full suite: pass (`Unified 1205/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1204/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1204/0`, `Compiler 73/0`)

### Session 204 Follow-up (2026-03-05): Scheduler Boundary Snapshot Consolidation

- Added shared test helper surface in `src/lisp/tests_tests.c3`:
  - `SchedulerBoundarySnapshot`
  - `scheduler_capture_runtime_boundary_snapshot(...)`
  - `scheduler_runtime_boundary_matches_snapshot(...)`
- Migrated scheduler boundary regression tests to use snapshot helper matching instead of repeated local field capture + long matcher arg lists.
- Kept drift diagnostics in mixed scheduler test, now keyed to snapshot fields.
- Validation:
  - normal full suite: pass (`Unified 1205/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1204/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1204/0`, `Compiler 73/0`)

### Session 205 Follow-up (2026-03-05): Snapshot Matcher Rollout Completion

- Completed migration of remaining scheduler boundary assertions to snapshot helper matching:
  - `scheduler_runtime_boundary_matches_snapshot(interp, &saved)`.
- Updated mixed scheduler drift diagnostics to use snapshot fields (`saved.*`) for consistency.
- Outcome: scheduler boundary regression block now uses one capture+match contract, eliminating residual expanded-arg matcher callsites.
- Validation:
  - normal full suite: pass (`Unified 1205/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1204/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1204/0`, `Compiler 73/0`)

### Session 206 Follow-up (2026-03-05): Single-Fiber Scheduler Test Helper Roll-In

- Added shared scheduler test setup/teardown helpers in `src/lisp/tests_tests.c3`:
  - `scheduler_test_prepare_single_fiber_no_async(...)`
  - `scheduler_test_prepare_blocked_pending_read(...)`
  - `scheduler_test_prepare_blocked_pending_offload(...)`
  - `scheduler_test_cleanup_single_fiber_no_async(...)`
- Migrated high-duplication pending-read-focused scheduler tests to helper usage:
  - timer wakeup, mixed wakeup-event boundary, tcp-read consume boundary, and wakeup ready-barrier boundary tests.
- Goal: reduce repeated setup/cleanup drift risk while preserving deterministic behavior.
- Validation:
  - normal full suite: pass (`Unified 1205/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1204/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1204/0`, `Compiler 73/0`)

### Session 207 Follow-up (2026-03-05): Offload Boundary Helper Migration

- Continued helper roll-in across offload-focused scheduler boundary tests in `src/lisp/tests_tests.c3`:
  - migrated duplicate-offload-ready and consume-pending-offload boundary tests to shared single-fiber setup/cleanup helpers.
- Reduced repeated reset/cleanup code while preserving phase-level boundary assertions.
- Validation:
  - normal full suite: pass (`Unified 1205/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1204/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1204/0`, `Compiler 73/0`)

### Session 208 Follow-up (2026-03-05): No-Async Cleanup Helper Consolidation

- Added shared helper `scheduler_test_cleanup_no_async(...)` in `src/lisp/tests_tests.c3`:
  - wakeup queue reset + async-handle restore.
- Migrated ring/no-async scheduler tests to use shared cleanup helper:
  - wakeup smoke, wakeup wraparound, wakeup-wraparound boundary, invalid-offload-wakeup boundary.
- Outcome: uniform no-async teardown semantics across scheduler boundary regressions.
- Validation:
  - normal full suite: pass (`Unified 1205/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1204/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1204/0`, `Compiler 73/0`)

### Session 209 Follow-up (2026-03-05): Wakeup Drop-Counter Invariant

- Added scheduler regression `run_scheduler_wakeup_drop_counter_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - fills wakeup ring, forces three overflow enqueues, verifies `wakeup_drops` delta,
  - verifies queue drain convergence and boundary/runtime field stability.
- Wired into `run_scheduler_tests(...)`.
- Validation:
  - normal full suite: pass (`Unified 1206/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1205/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1205/0`, `Compiler 73/0`)

### Session 210 Follow-up (2026-03-05): Wakeup Drop Counter Thread-Safety

- Hardened wakeup overflow telemetry in scheduler producer paths:
  - `Scheduler.wakeup_drops` moved to `types::Atomic{usz}` in `src/lisp/scheduler_state_offload.c3`.
  - ring-full path in `wakeup_enqueue(...)` now increments via atomic add (`src/lisp/scheduler_wakeup_io.c3`).
- Updated drop-counter boundary regression reads to use atomic loads:
  - `run_scheduler_wakeup_drop_counter_boundary_tests(...)` in `src/lisp/tests_tests.c3`.
- Outcome:
  - preserves existing queue semantics and test contracts,
  - removes plain-integer producer-side race from multi-producer wakeup accounting.
- Validation:
  - normal full suite: pass (`Unified 1205/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1206/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1205/0`, `Compiler 73/0`)

### Session 211 Follow-up (2026-03-05): Wakeup Full-Queue Payload Ownership

- Added scheduler regression `run_scheduler_wakeup_full_payload_ownership_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - fills wakeup ring, attempts `WAKEUP_OFFLOAD_READY` enqueue with completion payload under forced full-queue failure,
  - frees payload on failed enqueue to lock in caller-ownership contract for enqueue failure,
  - verifies queue drain convergence, `wakeup_drops` delta, and boundary/runtime field stability.
- Wired into `run_scheduler_tests(...)`.
- Outcome:
  - codifies retry-loop ownership expectations for worker enqueue failures,
  - guards against leak/double-free regressions in full-queue producer paths.
- Validation:
  - normal full suite: pass (`Unified 1205/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1207/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1206/0`, `Compiler 73/0`)

### Session 212 Follow-up (2026-03-05): Inactive Offload-Ready Payload Cleanup

- Added scheduler regression `run_scheduler_inactive_offload_ready_payload_cleanup_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - enqueues `WAKEUP_OFFLOAD_READY` with in-range fiber id targeting an inactive pending-offload slot,
  - verifies drain convergence, inactive-slot reset stability, and boundary/runtime field stability.
- Wired into `run_scheduler_tests(...)`.
- Outcome:
  - closes coverage gap for in-range inactive payload cleanup path (`scheduler_handle_wakeup_offload_ready`),
  - strengthens payload lifetime guarantees on scheduler wakeup boundaries.
- Validation:
  - normal full suite: pass (`Unified 1206/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1208/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1207/0`, `Compiler 73/0`)

### Session 213 Follow-up (2026-03-05): Worker Retry on Full Wakeup Ring

- Added scheduler regression `run_scheduler_offload_worker_retry_full_wakeup_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - exercises real offload-worker producer retry loop while ring is full,
  - observes retry via `wakeup_drops` delta,
  - drains ring, waits for in-range pending-offload completion delivery, consumes completion explicitly,
  - verifies queue convergence, pending-slot reset, and boundary/runtime state stability.
- Wired into `run_scheduler_tests(...)`.
- Outcome:
  - adds concurrent producer-path coverage (worker thread + scheduler consumer) to wakeup hardening block,
  - prevents regressions where retry/delivery teardown leaves completion payloads stranded.
- Validation:
  - normal full suite: pass (`Unified 1208/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1209/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1208/0`, `Compiler 73/0`)

### Session 214 Follow-up (2026-03-05): Worker-Cancel Thread-Task Interleave

- Added scheduler regression `run_scheduler_thread_task_worker_cancel_interleave_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - enqueues real worker offload task with explicit thread-task id,
  - immediately cancels task, then waits for completion take,
  - verifies cancel completion kind (`OFFLOAD_RES_ERROR`) + task entry cleanup,
  - verifies interpreter boundary/runtime snapshot stability.
- Wired into `run_scheduler_tests(...)`.
- Outcome:
  - extends thread-task cancel coverage from local state-machine assertions to live worker interleavings,
  - hardens completion ownership and cleanup expectations under cancel+completion races.
- Validation:
  - normal full suite: pass (`Unified 1208/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1210/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1209/0`, `Compiler 73/0`)

### Session 215 Follow-up (2026-03-05): Thread Timeout-Then-Join Boundaries

- Added scheduler regression `run_scheduler_thread_join_timeout_then_join_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - executes immediate timeout join on active worker task (expects timeout error + task persistence),
  - executes later bounded join on same task (expects completion + task removal),
  - verifies boundary/runtime snapshot stability.
- Wired into `run_scheduler_tests(...)`.
- Outcome:
  - hardens timeout path semantics against worker interleavings,
  - ensures timeout does not prematurely consume task state needed for later join.
- Validation:
  - normal full suite: pass (`Unified 1209/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1211/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1210/0`, `Compiler 73/0`)

### Session 216 Follow-up (2026-03-05): Offload-Ready Barrier Payload Path

- Added scheduler regression `run_scheduler_wakeup_offload_ready_barrier_boundary_tests(...)` in `src/lisp/tests_tests.c3`:
  - stages unready slot-0 `WAKEUP_OFFLOAD_READY` payload + ready slot-1 event,
  - verifies drain barrier stop (no premature processing),
  - releases slot-0 readiness, verifies ordered delivery and pending-offload completion,
  - consumes completion and verifies slot reset + boundary/runtime stability.
- Wired into `run_scheduler_tests(...)`.
- Outcome:
  - extends ready-barrier hardening to payload-bearing wakeup events,
  - guards against offload completion loss/free-order regressions under ready-gated drains.
- Validation:
  - normal full suite: pass (`Unified 1210/0`, `Compiler 73/0`)
  - strict ASAN full suite: pass (`Unified 1212/0`, `Compiler 73/0`)
  - strict ASAN full suite with `OMNI_FIBER_TEMP=1`: pass (`Unified 1211/0`, `Compiler 73/0`)

### Session 217 Follow-up (2026-03-05): Enforcement Config Externalization

- Externalized facade/sensitive policy data into dedicated files:
  - `scripts/boundary_facade_policy.txt`
  - `scripts/boundary_sensitive_files.txt`
- Updated policy scripts to consume those files:
  - `scripts/check_boundary_facade_usage.sh`
  - `scripts/check_boundary_change_policy.sh`
- Added lightweight CI guard workflow:
  - `.github/workflows/boundary-policy-guard.yml`
- Outcome:
  - policy now reviewed/edited as data files (reduced hardcoded drift risk),
  - fast guard path exists independent of full self-hosted hardening run.

### Session 218 Follow-up (2026-03-05): Boundary Surface Lock Audit

- Added policy-driven audit utility:
  - `scripts/audit_boundary_surface.sh`
  - strict mode (`OMNI_BOUNDARY_AUDIT_STRICT=1`) returns non-zero on violations.
- Generated checked-in audit snapshot:
  - `docs/BOUNDARY_SURFACE_AUDIT.md`
  - current status: `total=28`, `allowed=23`, `ignored=5`, `violations=0`.
- Outcome:
  - direct boundary surface is now measured by an auditable artifact.

### Session 219 Follow-up (2026-03-05): Final Handoff + Test Module Decomposition

- Split worker/interleave scheduler boundary regressions into dedicated module:
  - new file `src/lisp/tests_scheduler_boundary_worker.c3`
  - moved:
    - `run_scheduler_offload_worker_retry_full_wakeup_boundary_tests(...)`
    - `run_scheduler_wakeup_offload_ready_barrier_boundary_tests(...)`
    - `run_scheduler_thread_task_worker_cancel_interleave_boundary_tests(...)`
    - `run_scheduler_thread_join_timeout_then_join_boundary_tests(...)`
- Published boundary runtime audit handoff:
  - `docs/BOUNDARY_RUNTIME_AUDIT_2026-03-05.md`
- Validation:
  - facade guard: pass
  - strict boundary surface audit: pass (`violations=0`)
  - full normal suite: pass (`Unified 1212/0`, `Compiler 73/0`)
  - full ASAN suite: pass (`Unified 1211/0`, `Compiler 73/0`)
  - repeated ASAN+fiber-temp soak (3 runs): all pass (`Unified 1211/0`, `Compiler 73/0`)

### Post-44 Continuation Snapshot (Sessions 45-68)

- Boundary API expansion and caller migration completed across eval/jit/env/value/module paths.
- Scoped allocation/switch helpers consolidated (`enter/leave`, `push/pop child scope`, scoped env helpers).
- JIT/runtime lifetime hardening completed for:
  - closure env-copy paths
  - root promotion paths
  - scoped eval/finalization paths
  - constructor allocation failure paths
- Hotspot decomposition continued in scheduler/runtime modules:
  - scheduler await/cancel/join/spawn/offload paths
  - wakeup drain event handlers
  - JIT set-path and cache warm traversal helpers

### Session 106 Follow-up (2026-03-05): ASAN Stabilization

- Reproduced and fixed the ASAN fake-stack crash in escape-scope tests:
  - `AddressSanitizer CHECK failed (asan_thread.cpp:369)` with stack tail through:
    - `scope_chunk_alloc` -> `scope_create` -> `jit_copy_closure_env_if_needed` -> `jit_make_closure_from_expr`.
- Root cause:
  - JIT warm-cache compilation path (`jit_warm_expr_cache`/`jit_cache_expr`) bypassed eval-path JIT gating under ASAN.
- Remediation:
  - Added unified runtime JIT gate (`run_jit_enabled`) and applied it to top-level eval paths.
  - Gated warm-cache compilation behind the same runtime policy.
  - Disabled JIT cross-check execution in ASAN mode in unified test helpers.
  - Added deterministic JIT teardown at test end (`jit_global_shutdown`).
  - Kept mid-run JIT GC lightweight; skipped GC scheduling under ASAN to avoid unstable state-destroy cycles during execution.
- Validation:
  - `c3c build` + full test run: pass.
  - `c3c build --sanitize=address` + `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1` full run: pass.

### Session 107 Follow-up (2026-03-05): JIT GC Safe-Point Policy Correction

- Reproduced strict-ASAN follow-up regression:
  - leak mode: `JIT state pool full (4096)` + `2320 bytes leaked in 29 allocs` (`jit_alloc` via `jit_lookup_or_compile`)
  - crash mode: `EXIT:139` after enabling scheduling while still allowing GC call inside `jit_compile`.
- Root cause:
  - scheduling/teardown policy mismatch:
    - ASAN scheduling disabled => pool overflow + leaks
    - GC invoked from compile-time call paths => unsafe destruction context.
- Remediation:
  - moved JIT state destruction to true top-level safe points only (`run`/`run_program`/REPL `jit_gc()` calls),
  - removed `jit_gc()` call from `jit_compile()`,
  - re-enabled threshold scheduling uniformly (including ASAN),
  - kept runtime/test ASAN policy based on `main::stack_runtime_asan_enabled()`.
- Validation:
  - `c3c build` + `LD_LIBRARY_PATH=/usr/local/lib ./build/main`: pass.
  - `c3c build --sanitize=address` + strict ASAN leak run (`detect_leaks=1`): pass.

### Session 108 Follow-up (2026-03-05): ASAN Pool Pressure + Parser Edge Contracts

- Added explicit JIT execution-depth accounting around generated-code calls:
  - `jit_exec_enter/jit_exec_leave`
  - enables safe-point gating for opportunistic compile-time GC.
- Increased JIT tracked-state pool headroom (`4096 -> 16384`) to avoid false overflow warnings during nested chains where GC cannot run until unwind.
- Expanded parser edge-case contract tests for refactored helper paths:
  - `export-from` specifier/module/list errors,
  - `deftype` missing-name/missing-field-name,
  - `defunion` missing-name/missing-variant-name.
- Validation:
  - Normal full suite: pass (`Unified 1154/0`, `Compiler 73/0`).
  - Strict ASAN full suite (`detect_leaks=1`): pass (`Unified 1153/0`, `Compiler 73/0`, no pool-full warning, no leak summary failure).
