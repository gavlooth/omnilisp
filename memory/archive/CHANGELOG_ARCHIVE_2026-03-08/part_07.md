
### Why this matters
- Scheduler boundary hardening had strong wakeup/consume coverage but limited direct coverage over thread-task ownership transitions.
- This closes that seam and reinforces deterministic completion cleanup and slot lifecycle behavior under sanitizer checks.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1203 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1203 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 201 - Offload Consume Bytes-Branch Boundary Coverage

### Summary
Extended the pending-offload consume boundary regression to cover the `OFFLOAD_RES_BYTES` branch (shared-blob transfer path), while preserving existing boundary/runtime-state guarantees.

### What changed
- `src/lisp/tests_tests.c3`
  - Updated `run_scheduler_consume_pending_offload_boundary_tests(...)`:
    - expanded completion mode rotation from 2 modes to 3 modes:
      - `OFFLOAD_RES_INT`,
      - cancel/error completion,
      - `OFFLOAD_RES_BYTES` completion using `shared_blob_new_copy("blob-ok")`.
    - for bytes mode, verifies consumed value is `STRING` with expected payload.
  - Existing assertions retained:
    - pre-completion consume returns error,
    - completed consume returns correct value shape,
    - pending slot resets after consume,
    - post-reset consume returns error,
    - boundary/runtime fields remain unchanged per phase.

### Why this matters
- Session 199 covered consume-side control flow but did not exercise blob ownership transfer in `scheduler_value_from_offload_bytes(...)`.
- This closes that gap and strengthens consume-path hardening around shared-blob to Value ownership handoff.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1203 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1202 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1202 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 200 - Pending-TCP-Read Consume Boundary Regression

### Summary
Added direct consume-path scheduler regression coverage for `scheduler_consume_pending_tcp_read(...)`, asserting boundary/runtime state stability across pre-completion error, completed timeout/error/empty/non-empty cases, and post-consume repeat-error paths.

### What changed
- `src/lisp/tests_tests.c3`
  - Added helpers:
    - `scheduler_seed_completed_tcp_read_case(...)`
    - `scheduler_tcp_case_value_ok(...)`
  - Added:
    - `run_scheduler_consume_pending_tcp_read_boundary_tests(...)`
  - New coverage (looped stress):
    - Phase A: consume before completion must return `ERROR`.
    - Phase B: consume completed case, rotating across:
      - timed out (`ERROR`),
      - explicit read error (`ERROR`),
      - empty read (`STRING` length 0),
      - non-empty read (`STRING "pong"`).
    - Verifies pending slot reset (`active/completed false`, `buffer null`) after consume.
    - Phase C: consume after reset must return `ERROR` again.
  - Verifies interpreter boundary/runtime fields after each phase.
  - Includes explicit fallback cleanup if test-local buffer remains.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Prior scheduler hardening covered wakeup enqueue/drain and offload consume seams.
- This closes the symmetric tcp-read consume seam where result translation, buffer ownership release, and slot reset must remain deterministic and boundary-safe.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1203 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1202 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1202 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 199 - Pending-Offload Consume Boundary Regression

### Summary
Added direct consume-path scheduler regression coverage for `scheduler_consume_pending_offload(...)`, asserting boundary/runtime state stability across pre-completion error, completed success/error consume, and post-consume repeat-error paths.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_consume_pending_offload_boundary_tests(...)`
  - New coverage (looped stress):
    - Phase A: consume before completion (`active=false/completed=false`) must return `ERROR`.
    - Phase B: consume completed offload:
      - alternates between `OFFLOAD_RES_INT` completion and cancel-error completion,
      - verifies returned value shape (`INT` or `ERROR`) matches completion kind,
      - verifies pending slot resets to zeroed state.
    - Phase C: consume after reset must return `ERROR` again.
  - Verifies interpreter boundary/runtime fields after each phase.
  - Includes explicit test-side cleanup fallback for retained completion pointers.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Previous regressions focused on wakeup enqueue/drain behavior; this closes the consume-side boundary seam where payload translation and completion teardown occur.
- Adds deterministic protection against boundary-state drift and slot-reset regressions in offload completion consumption paths.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1202 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1201 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1201 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 198 - Duplicate/Late Offload-Ready Boundary Regression

### Summary
Added scheduler regression coverage for duplicate and late `WAKEUP_OFFLOAD_READY` delivery to the same active pending offload, asserting first-completion retention semantics, safe extra-payload discard, and boundary/runtime state restoration.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_duplicate_offload_ready_boundary_tests(...)`
  - Test behavior:
    - prepares one blocked active pending offload fiber (`fid=0`),
    - enqueues two `WAKEUP_OFFLOAD_READY` events with distinct `OffloadCompletion*` payloads,
    - verifies only first payload is retained in `pending_offloads[0].completion`,
    - verifies queue convergence (`head == tail`) and `FIBER_BLOCKED -> FIBER_READY`,
    - enqueues a third late `WAKEUP_OFFLOAD_READY` payload after completion and verifies retained payload is unchanged,
    - checks interpreter boundary/runtime fields after duplicate and late phases.
  - Includes explicit cleanup of retained completion payload in the test harness.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Existing wakeup regressions covered invalid offload wakeups and queue ordering, but not idempotent behavior for duplicate/late completion delivery on a valid pending offload.
- This closes a practical race/resend safety gap and, with ASAN, helps guard against leak/double-free regressions in offload wakeup handling.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1201 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1200 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1200 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 197 - Wakeup Ready-Barrier Boundary Regression

### Summary
Added scheduler regression coverage for wakeup ready-barrier ordering, asserting `drain_wakeups()` stops at the first unready slot, preserves FIFO processing, and leaves interpreter boundary/runtime state unchanged.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_wakeup_ready_barrier_boundary_tests(...)`
  - Test constructs a controlled queue state:
    - `wakeup_head=2`, `wakeup_tail=0`,
    - slot 0 event unready, slot 1 event ready.
  - Verifies first drain:
    - no processing occurs (`tail` stays 0, pending read remains blocked/incomplete).
  - Marks slot 0 ready and drains again; verifies ordered processing:
    - pending read transitions to completed/timed-out,
    - fiber transitions `FIBER_BLOCKED -> FIBER_READY`,
    - queue converges (`head == tail`).
  - Asserts interpreter boundary/runtime fields are unchanged before and after both drains.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Existing regressions covered wakeup ring capacity and mixed event types, but not the `wakeup_ready` ordering barrier semantics.
- This closes an ordering safety gap that protects against out-of-order event consumption under producer/consumer interleavings.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1200 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1199 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1199 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 196 - Invalid Offload Wakeup Boundary Regression

### Summary
Added scheduler regression coverage for invalid `WAKEUP_OFFLOAD_READY` events carrying real completion payloads, asserting safe drain behavior and boundary/runtime state stability.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_invalid_offload_wakeup_boundary_tests(...)`
  - New test repeatedly:
    - prepares scheduler wakeup ring in deterministic no-async mode,
    - allocates a real `OffloadCompletion*` via `scheduler_make_task_cancel_completion()`,
    - enqueues `WAKEUP_OFFLOAD_READY` with out-of-range fiber id (invalid wakeup path),
    - also enqueues a second invalid poll-error event to force mixed invalid-event drain,
    - drains wakeups and verifies queue convergence (`head == tail`),
    - validates interpreter boundary/runtime fields are unchanged.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Prior regressions covered queue mechanics and valid pending-read transitions, but not invalid offload-ready payload cleanup through `scheduler_handle_invalid_wakeup(...)`.
- This test closes that gap and gives ASAN-backed confidence against payload leak/double-free regressions on invalid wakeup paths.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1199 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1198 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1198 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 195 - Scheduler Mixed Wakeup-Event Boundary Regression

### Summary
Added a scheduler regression that exercises mixed wakeup-event ordering (`WAKEUP_POLL_ERROR` + `WAKEUP_TIMER_EXPIRED`) for a single blocked fiber and asserts both completion-order semantics and boundary/runtime state restoration.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_wakeup_mixed_event_boundary_tests(...)`
  - New coverage alternates event ordering across iterations:
    - poll-error then timer-expired,
    - timer-expired then poll-error.
  - For each cycle, the test verifies:
    - both enqueues succeed,
    - queue drains fully (`head == tail`),
    - blocked fiber transitions to `FIBER_READY`,
    - first-event-wins semantics on `PendingTcpRead`:
      - poll-first => `error_code == -17`, `timed_out == false`
      - timer-first => `timed_out == true`, `error_code == 0`
    - interpreter boundary/runtime fields remain unchanged.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Wraparound and simple wakeup behavior were covered, but mixed-event ordering for the same pending operation was not.
- This closes a subtle callback/drain interleaving gap while preserving the core boundary-hardening goal: no interpreter lifetime-state drift.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1198 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1197 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1197 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 194 - Scheduler Wakeup Wraparound Boundary-State Regression

### Summary
Added a dedicated scheduler regression that stresses wakeup-ring saturation and drain behavior while asserting interpreter boundary/runtime state restoration across repeated cycles.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_wakeup_wraparound_boundary_tests(...)`
  - New coverage loop repeatedly:
    - disables async-handle wakeups for deterministic ring-only exercise,
    - fills wakeup ring to `WAKEUP_RING_SIZE`,
    - asserts overflow enqueue is rejected,
    - drains queue and verifies `head == tail`,
    - validates boundary/runtime fields are unchanged after each cycle.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Functional wraparound behavior was already covered, but boundary-state invariants were not.
- This locks in that wakeup queue saturation/drain paths do not leak state into interpreter lifetime/ownership fields.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1197 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1196 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1196 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 193 - Scheduler Wakeup/Offload Expected-Error Boundary Regression

### Summary
Extended scheduler boundary-hardening coverage with a focused regression that combines wakeup-queue drain paths and offload expected-error flows, and verifies boundary/runtime state restoration across repeated cycles.

### What changed
- `src/lisp/tests_tests.c3`
  - Added scheduler regression:
    - `run_scheduler_wakeup_offload_error_boundary_tests(...)`
  - New coverage mixes three paths per iteration:
    - success path: spawn + `offload 'sleep-ms` + `await`
    - wakeup path: `wakeup_enqueue(...)` + `drain_wakeups()` with out-of-range fiber id
    - expected-error path: spawn + `offload 'nope` + `await`
  - Verifies boundary/runtime fields remain unchanged after each phase:
    - `current_scope`, `releasing_scope`
    - `jit_env`, `match_env`
    - `jit_tco_expr`, `jit_tco_env`
    - `tco_recycle_scope`, `tco_scope_defer_slot`, `tco_scope_defer_active`
    - `escape_env_mode`, `active_promotion_ctx`
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Expands scheduler-specific regression depth beyond mixed join/cancel flows into wakeup-queue and expected-offload-error transitions.
- Locks in boundary-state restoration guarantees on callback-driven and erroring async-adjacent paths.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1196 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1196 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- ASAN repeated full-run probe (`detect_leaks=1:halt_on_error=1:abort_on_error=1`, 3 runs):
  - all passed (`Unified 1195/0`, `Compiler 73/0` each run).

## 2026-03-05: Session 192 - Scheduler Mixed Boundary-State Regression + Top-Level JIT-TCO Cleanup

### Summary
Added mixed scheduler boundary-state regression coverage and fixed top-level transient JIT-TCO state leakage across `run` error exits.

### What changed
- `src/lisp/tests_tests.c3`
  - Added scheduler regression:
    - `run_scheduler_mixed_boundary_state_restore_tests(...)`
  - Covers mixed success + expected-error scheduler cycles:
    - success path: `thread-spawn` + `thread-join` + fiber `offload` + `await`
    - error path: `thread-cancel` followed by `thread-join` (expected error)
  - Verifies boundary/runtime fields remain unchanged across iterations:
    - `current_scope`, `releasing_scope`
    - `jit_env`, `match_env`
    - `jit_tco_expr`, `jit_tco_env`
    - `tco_recycle_scope`, `tco_scope_defer_slot`, `tco_scope_defer_active`
    - `escape_env_mode`, `active_promotion_ctx`
  - Wired into `run_scheduler_tests(...)`.
- `src/lisp/eval_run_pipeline.c3`
  - Added top-level transient cleanup hardening:
    - `run_clear_stale_jit_tco_state(...)`
    - entry clear + `defer` clear in both `run(...)` and `run_program(...)`.
  - Fixes drift where `jit_tco_expr` / `jit_tco_env` could remain set after error exits and leak into subsequent runs.

### Why this matters
- Extends boundary-hardening coverage into mixed scheduler/offload flows, including expected error paths.
- Centralizes top-level transient reset policy for JIT-TCO fields, removing a subtle cross-run state leak.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1195 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- ASAN repeated full-run probe (`detect_leaks=1:halt_on_error=1:abort_on_error=1`, 3 runs):
  - all passed (`Unified 1194/0`, `Compiler 73/0` each run).

## 2026-03-05: Session 191 - Deduce Memory-DB Isolation Regression + Path Entropy Hardening

### Summary
Strengthened deduce in-memory DB reliability with two complementary changes: higher-entropy memory path generation and explicit regression coverage proving handle/database isolation.

### What changed
- `src/lisp/deduce.c3`
  - Added robust memory-path construction helpers:
    - `deduce_append_slice(...)`
    - `deduce_append_hex(...)`
    - `deduce_build_memory_path(...)`
  - Introduced process-sequence entropy (`g_deduce_memory_open_seq` + `getpid`) in `/tmp/deduce-*` names, alongside full-width pointer suffix.
  - `prim_deduce_open(...)` now uses `deduce_build_memory_path(...)` and fails explicitly if path build fails.
- `src/lisp/tests_tests.c3`
  - Added `run_deduce_memory_open_isolation_test(...)`.
  - Opens two `'memory` databases, defines the same schema in each, inserts into one, and verifies counts diverge (`1` vs `0`) to confirm storage isolation.
  - Wired into `run_deduce_group_tests(...)`.

### Why this matters
- Reduces residual order/process-collision risk in deduce memory DB temp naming.
- Adds concrete behavior guardrail proving two memory-open handles do not alias backing storage.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1194 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- ASAN repeated full-run probe (`detect_leaks=1:halt_on_error=1:abort_on_error=1`, 3 runs):
  - all passed (`Unified 1193/0`, `Compiler 73/0` each run).

## 2026-03-05: Session 190 - Deduce Reopen Stress Test De-Flaking (Single-Expression Handle Check)

### Summary
Reduced order-sensitivity in the deduce reopen stress test by removing out-of-band global-env lookup and asserting the reopened handle through a single evaluation result.

### What changed
- `src/lisp/tests_tests.c3`
  - `run_deduce_reopen_stress_test(...)` now executes:
    - `(begin (define ddb-reopen (deduce 'open 'memory)) ddb-reopen)`
  - Validation checks the returned `EvalResult.value` directly for a live `FFI_HANDLE`.
  - Removed separate `global_env.lookup(...)` dependency from this stress path.

### Why this matters
- Keeps test intent (repeated open/rebind) while reducing external state-coupling and timing sensitivity between eval and environment lookup in stress loops.
- Complements Session 189 deduce path-suffix hardening for ASAN stability.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1193 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- ASAN repeated full-run probe (`detect_leaks=1:halt_on_error=1:abort_on_error=1`, 3 runs):
  - all passed (`Unified 1192/0`, `Compiler 73/0` each run).

## 2026-03-05: Session 189 - Deduce ASAN Flake Hardening (Full-Width Memory DB Path Suffix)

### Summary
Fixed a likely root cause for transient ASAN order-sensitive deduce reopen failures by removing 32-bit truncation in in-memory DB path generation for `deduce 'open 'memory`.

### What changed
- `src/lisp/deduce.c3`
  - Added helper:
    - `deduce_format_usz_hex(char[] out, usz value)`
  - Updated `prim_deduce_open(...)` `'memory` branch to build `/tmp/deduce-<suffix>` from full-width `usz` pointer bits.
  - Removed low-word-only formatting path (`%x` on `(uint)(usz)db`) that could collide under ASAN address layouts.

### Why this matters
- Previous suffix generation used only the low 32 bits of pointer addresses, which can collide in sanitizer-heavy allocator layouts and manifest as rare `deduce repeated open/rebind` failures.
- Full-width suffix generation materially reduces collision risk without changing user-facing API.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1193 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1192 passed, 0 failed` (JIT checks disabled under ASAN)
  - `Compiler: 73 passed, 0 failed`
- Additional ASAN stability probe (3 repeated full runs):
  - all passed (`Unified 1192/0`, `Compiler 73/0` each run).

## 2026-03-05: Session 188 - Nested Promotion-Context Stack Regression

### Summary
Added regression coverage for nested promotion-context stack behavior under releasing-scope copies, including mixed outer-aborted and inner-budgeted contexts.

### What changed
- `src/lisp/tests_tests.c3`
  - Added `run_memory_lifetime_nested_promotion_context_stack_test(...)`.
  - Scenario:
    - outer `PromotionContext` with budget `0` (forced abort),
    - nested inner `PromotionContext` with budget `4`,
    - repeated `boundary_copy_from_releasing_scope(...)` calls inside/outside inner scope.
  - Verifies:
    - context stack linkage (`inner.prev == &outer`),
    - active-context restore after `promotion_context_end(inner)`,
    - memo behavior in inner context (`inner_b == inner_a`),
    - abort fallback non-aliasing in outer context (`outer_a != outer_b`),
    - final active-context teardown (`interp.active_promotion_ctx == null`).
  - Wired into `run_memory_lifetime_promotion_context_tests(...)`.

### Why this matters
- Hardens correctness around nested promotion-context stack discipline and releasing-scope copy behavior, where implicit context leakage can produce subtle lifetime regressions.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1193 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - one transient run observed an order-sensitive deduce failure (`deduce repeated open/rebind`),
  - immediate rerun passed cleanly:
    - `Unified: 1192 passed, 0 failed` (JIT checks disabled under ASAN)
    - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 187 - Nested Releasing-Copy Interleaving Regression Under Promotion Abort

### Summary
Added targeted regression coverage for nested `boundary_copy_from_releasing_scope(...)` interleavings while promotion context is budget-aborted, verifying boundary-state restoration and non-memoized copy behavior.

### What changed
- `src/lisp/tests_tests.c3`
  - Added `run_memory_lifetime_promotion_abort_nested_releasing_copy_test(...)`.
  - Scenario:
    - starts a `PromotionContext` with budget `0` (forced abort),
    - performs two `boundary_copy_from_releasing_scope(...)` calls across nested scope interleaving (`boundary_enter_scope`/`boundary_leave_scope`),
    - validates:
      - boundary state restored after each interleaving,
      - copies are distinct (no memo aliasing under aborted context),
      - copies land in expected target scopes (parent vs sibling),
      - source-scope aliasing is not retained,
      - promotion context teardown leaves no active context.
  - Wired into `run_memory_lifetime_promotion_context_tests(...)`.

### Why this matters
- Closes a subtle interleaving gap around aborted promotion epochs and releasing-scope copy boundaries.
- Strengthens guarantees that budget-abort fallback behavior remains deterministic and state-safe across nested scope transitions.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1192 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1191 passed, 0 failed` (JIT checks disabled under ASAN)
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 186 - run_program Error-Path Boundary-State Regression

### Summary
Expanded top-level boundary coverage by adding explicit regressions that `run_program(...)` preserves scope state on parse and runtime error exits, not only success paths.

### What changed
- `src/lisp/tests_tests.c3`
  - Added `run_memory_lifetime_run_program_error_boundary_state_test(...)`.
  - Verifies `run_program(...)` preserves:
    - `interp.current_scope`
    - `interp.releasing_scope`
  across both:
    - parse-error exit (missing closing paren),
    - runtime-error exit (`(car 1)`).
  - Wired into `run_memory_lifetime_regression_tests(...)`.

### Why this matters
- Closes a boundary-coverage gap at a high-level entry point.
- Ensures orchestration-level failures do not leak transient scope state into subsequent evaluations.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1191 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1190 passed, 0 failed` (JIT checks disabled under ASAN)
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 185 - TCO Recycle Error-Path Regression + Single-Scope Boundary Leave

### Summary
Added explicit regression coverage for the TCO recycle error-restore branch and replaced one remaining direct scope restore in single-scope JIT wrapper with boundary helper usage.

### What changed
- `src/lisp/jit_jit_eval_scopes.c3`
  - In `jit_eval_in_single_scope(...)`, replaced:
    - `interp.current_scope = saved_scope`
    with:
    - `boundary_leave_scope(interp, saved_scope)`
  - Behavior unchanged; scope transition now goes through audited facade helper.
- `src/lisp/tests_tests.c3`
  - Added `run_memory_lifetime_tco_recycle_error_restore_test(...)`.
  - Drives `jit_prepare_tco_recycle(...)` through the defer-retarget error branch (`tco_scope_defer_active=true` + missing `g_current_stack_ctx`) and verifies:
    - error value returned,
    - env pointer unchanged,
    - `current_scope`, `tco_recycle_scope`, and `jit_env` restored as expected.
  - Wired into `run_memory_lifetime_hot_budget_tests(...)`.

### Why this matters
- Covers a subtle, high-risk rollback path that previously lacked direct regression coverage.
- Continues migration from ad-hoc scope assignment to boundary helper transitions.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1190 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1189 passed, 0 failed` (JIT checks disabled under ASAN)
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 184 - Context-Switch Invariant Hooks in jit_common

### Summary
Added centralized boundary invariant checks at shared interpreter context-switch save/restore points in `jit_common`, increasing coverage at a critical cross-cutting runtime seam.

### What changed
- `src/lisp/jit_common.c3`
  - `save_interp_state(...)` now asserts `boundary_assert_interp_scope_chain(interp)` before snapshotting.
  - `restore_interp_state(...)` now asserts `boundary_assert_interp_scope_chain(interp)` after restoration.

### Why this matters
- `save_interp_state` / `restore_interp_state` are reused by stack/effect/JIT transitions.
- Guarding these functions narrows failure localization for invalid scope-state transitions and complements earlier run/JIT/repl/macro boundary hook rollout.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1189 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1188 passed, 0 failed` (JIT checks disabled under ASAN)
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 183 - JIT TCO Call-State Helper Consolidation + Runtime-Field Regression

### Summary
Reduced remaining distributed TCO call-state mutations in JIT scope wrappers by introducing helper-level save/restore and recycle-state setters, and added regression coverage to ensure TCO runtime fields restore on both success and error.

### What changed
- `src/lisp/jit_jit_eval_scopes.c3`
  - Added helper abstractions:
    - `jit_set_active_recycle_scope(...)`
    - `JitCallScopeState`
    - `jit_save_call_scope_state(...)`
    - `jit_restore_call_scope_state(...)`
    - `jit_activate_call_scope_recycle(...)`
  - Routed existing paths through helpers:
    - `jit_prepare_tco_recycle(...)` fast/fallback scope retargeting.
    - `jit_eval_in_call_scope(...)` TCO/defer/escape/current scope state restore.
  - Replaced manual child-scope rollback on defer registration failure with `boundary_pop_child_scope(...)` in:
    - `jit_eval_in_single_scope(...)`
    - `jit_eval_in_call_scope(...)`
