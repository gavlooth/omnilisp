- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1224 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 221 - Unify Iterator Combinators Into Dispatched Collection API

### Summary
Removed iterator-specific `i*` combinator API surface for common transforms and
standardized on dispatched collection functions:
`map`, `filter`, `take`, `drop`, `zip`, `foldl`, `foldr`.

### What changed
- `stdlib/stdlib.lisp`
  - removed `imap`, `ifilter`, `itake`, `idrop`, `izip`, `ifoldl` definitions,
  - added `^Iterator` dispatch variants for:
    - `map`,
    - `filter`,
    - `take`,
    - `drop`,
    - `zip`,
    - `foldl`.
  - added/expanded finite collection dispatch for `List`/`Array`:
    - `reverse`,
    - `take`,
    - `drop`,
    - `zip`,
    - `foldl`,
    - `foldr`.
  - unified iterator source naming:
    - renamed `irepeat` -> `repeat`,
    - renamed `icycle` -> `cycle`.
- `src/lisp/tests_advanced_tests.c3`
  - migrated iterator/lazy tests to use unified APIs (`map/filter/take/foldl`).
  - added explicit assertion that `(filter ... (iterator ...))` returns an iterator.
  - added coverage for:
    - `repeat` / `cycle`,
    - array-dispatched `take/drop/zip/foldl`,
    - `foldr` on list and array.
- Documentation updates:
  - `docs/reference/06-effects.md`
  - `docs/reference/12-appendix-stdlib.md`
  - `docs/LANGUAGE_SPEC.md`
  - updated examples/signatures to the unified dispatched API.

### Why this matters
- Reduces cognitive load by removing duplicate iterator-specific names for
  operations that are already collection-dispatched.
- Preserves laziness semantics through dispatch:
  iterator inputs still produce lazy iterator outputs.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1213 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 220 - Documentation Closure Marker (Architecture + Fiber Temp)

### Summary
Published completion markers in architecture/runtime planning docs to reflect
the final state of this phase: boundary-hardening closed and fiber-temp
implemented as a guarded backing strategy under region-centric ownership.

### What changed
- `.claude/plans/fiber-temp-detailed-implementation-plan.md`
  - status updated to complete for current architecture target.
- `.claude/plans/fiber-temp-session-plan.md`
  - execution status updated to complete for planned phase cadence.
- `docs/BOUNDARY_RUNTIME_AUDIT_2026-03-05.md`
  - marked hardening track closed and recorded closure result.
- `memory/DESTINATION_ARENA_PLAN.md`
  - added Revision XV with explicit completion marker and architecture freeze notes.

### Why this matters
- Removes ambiguity about "complete vs incomplete" for this phase.
- Makes it explicit that fiber-temp is an optimization/backing path and not a
  separate ownership model.
- Keeps ownership and safety guardrails discoverable in primary docs.

## 2026-03-05: Session 219 - Boundary Audit Handoff + Scheduler Test Module Split

### Summary
Completed final hardening-handoff pass: split high-complexity scheduler boundary worker/interleave tests into a dedicated module and published runtime audit notes with repeated ASAN+fiber-temp soak validation.

### What changed
- `src/lisp/tests_scheduler_boundary_worker.c3` (new)
  - moved worker/interleave boundary regressions out of monolithic test file:
    - `run_scheduler_offload_worker_retry_full_wakeup_boundary_tests(...)`
    - `run_scheduler_wakeup_offload_ready_barrier_boundary_tests(...)`
    - `run_scheduler_thread_task_worker_cancel_interleave_boundary_tests(...)`
    - `run_scheduler_thread_join_timeout_then_join_boundary_tests(...)`
- `src/lisp/tests_tests.c3`
  - removed moved definitions; retained suite orchestration callsites.
- `docs/BOUNDARY_RUNTIME_AUDIT_2026-03-05.md` (new)
  - published no-drift contract, residual risk list, and closure criteria.

### Why this matters
- Reduces test hotspot pressure in `tests_tests.c3` while preserving behavior.
- Makes boundary hardening status and residual risks explicit for contributors.

### Validation
- `bash scripts/check_boundary_facade_usage.sh`
- `OMNI_BOUNDARY_AUDIT_STRICT=1 scripts/audit_boundary_surface.sh docs/BOUNDARY_SURFACE_AUDIT.md`
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1212 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1211 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- repeated soak:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_FIBER_TEMP=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` (3 consecutive runs)
  - each run: `Unified: 1211 passed, 0 failed`, `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 218 - Boundary Surface Lock (Policy-Driven Audit)

### Summary
Added policy-driven boundary surface audit tooling and generated a strict audit artifact from the current tree.

### What changed
- `scripts/audit_boundary_surface.sh` (new)
  - scans direct boundary callsites in `src/lisp`,
  - classifies each as `allowed` / `ignored` / `violation` using policy file,
  - emits markdown report and supports strict mode (`OMNI_BOUNDARY_AUDIT_STRICT=1`).
- `docs/BOUNDARY_SURFACE_AUDIT.md` (new, generated)
  - current snapshot:
    - total scanned: `28`
    - allowed: `23`
    - ignored: `5`
    - violations: `0`

### Why this matters
- Converts “surface lock” from ad-hoc grep output into an auditable, repeatable artifact.
- Gives an explicit zero-violation gate for completion tracking.

## 2026-03-05: Session 217 - CI/Policy Enforcement Closure (Config Externalization)

### Summary
Finalized enforcement plumbing by externalizing boundary policy config and adding a lightweight CI guard workflow independent from self-hosted heavy profile runs.

### What changed
- `scripts/boundary_facade_policy.txt` (new)
  - source-of-truth allowlist/ignore rules for facade guard.
- `scripts/boundary_sensitive_files.txt` (new)
  - source-of-truth list for boundary-sensitive policy checks.
- `scripts/check_boundary_facade_usage.sh`
  - now loads policy from `scripts/boundary_facade_policy.txt` (instead of hardcoded cases).
- `scripts/check_boundary_change_policy.sh`
  - now loads sensitive files from `scripts/boundary_sensitive_files.txt`.
- `.github/workflows/boundary-policy-guard.yml` (new)
  - runs facade policy guard on PR file changes relevant to boundary policy.

### Why this matters
- Removes hardcoded rule drift in enforcement scripts.
- Makes boundary policy reviewable as data, not embedded shell logic.
- Adds fast CI feedback path even when full self-hosted hardening job is not invoked.

## 2026-03-05: Session 216 - Offload-Ready Barrier Payload Regression

### Summary
Added scheduler regression coverage for wakeup ready-barrier semantics on `WAKEUP_OFFLOAD_READY` payload events, including later delivery and consume path cleanup.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_wakeup_offload_ready_barrier_boundary_tests(...)`
  - New coverage sequence:
    - prepares blocked in-range pending-offload slot,
    - manually stages two wakeup events with slot-0 `WAKEUP_OFFLOAD_READY` payload marked unready and slot-1 ready,
    - verifies first drain stops at unready slot (no premature processing),
    - releases readiness, drains again, verifies completion delivery + fiber state transition,
    - consumes pending offload (`scheduler_consume_pending_offload`) and verifies slot reset,
    - verifies interpreter boundary/runtime snapshot stability across all phases.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Existing ready-barrier coverage focused on TCP-read events.
- This locks in payload event ordering/ownership semantics under ready-barrier gating and prevents regressions that could drop or prematurely free offload completions.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1210 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1212 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1211 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 215 - Thread Timeout-Then-Join Boundary Regression

### Summary
Added scheduler regression coverage for thread-task timeout path correctness: immediate timeout must not consume task completion, and a later join must still complete and clear task state.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_thread_join_timeout_then_join_boundary_tests(...)`
  - New coverage loop:
    - allocates thread task + enqueues worker offload (`OFFLOAD_SLEEP_MS`),
    - performs immediate timeout join (`scheduler_thread_join_impl(..., 0)`), expects error and task still present,
    - performs later bounded join (`scheduler_thread_join_impl(..., 250)`), expects non-error completion and task removal,
    - verifies interpreter boundary/runtime snapshot stability.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Locks in timeout semantics across real worker execution:
  - timeout does not consume/destroy task completion state,
  - subsequent join remains valid and deterministic.
- Guards against regressions where timeout/join interleavings strand or prematurely clear thread-task entries.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1209 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1211 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1210 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 214 - Worker-Cancel Thread-Task Interleave Regression

### Summary
Added scheduler regression coverage for real worker-thread enqueue interleaved with immediate thread-task cancellation, with explicit completion ownership checks and boundary snapshot assertions.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_thread_task_worker_cancel_interleave_boundary_tests(...)`
  - New coverage loop:
    - allocates real thread task id,
    - enqueues offload work (`OFFLOAD_SLEEP_MS`) through worker path,
    - immediately cancels task (`scheduler_cancel_thread_task`),
    - waits for completion materialization (`scheduler_take_thread_task_completion`),
    - verifies cancel completion kind (`OFFLOAD_RES_ERROR`) and task-table cleanup,
    - verifies interpreter boundary/runtime snapshot stability.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Existing thread-task tests covered internal state transitions, but this locks in behavior on the real worker interleaving path.
- It guards against regressions where cancel+worker completion races leak completion ownership or leave stale task entries.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1208 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1210 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1209 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 213 - Offload Worker Retry Under Full Wakeup Ring

### Summary
Added scheduler regression coverage for real producer contention: offload worker retries `WAKEUP_OFFLOAD_READY` while the wakeup ring is full, then completes safely after drain with explicit completion consumption.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_offload_worker_retry_full_wakeup_boundary_tests(...)`
  - New coverage loop:
    - prepares blocked pending-offload slot for in-range completion delivery,
    - pre-fills wakeup ring to capacity with invalid poll-error events,
    - enqueues offload work (`OFFLOAD_SLEEP_MS`) so worker becomes concurrent producer,
    - waits for retry signal via `wakeup_drops` increase under full ring,
    - drains ring, waits for completion delivery to pending slot, consumes completion via `scheduler_consume_pending_offload(...)`,
    - verifies queue convergence, slot reset, and interpreter boundary/runtime snapshot stability.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Prior coverage validated ring semantics mostly from synthetic enqueue paths.
- This test drives the real worker retry loop (`while (!wakeup_enqueue(...))`) under contention and locks in payload lifetime correctness in that path.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1208 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1209 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1208 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 212 - Inactive Offload-Ready Payload Cleanup Regression

### Summary
Added scheduler regression coverage for in-range inactive pending-offload wakeup events, locking in payload cleanup semantics in `drain_wakeups()`.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_inactive_offload_ready_payload_cleanup_boundary_tests(...)`
  - New coverage loop:
    - prepares single-fiber no-async scheduler state with inactive `pending_offloads[0]`,
    - enqueues `WAKEUP_OFFLOAD_READY` with `fiber_id=0` and completion payload,
    - drains wakeups and verifies queue convergence (`head == tail`),
    - verifies pending-offload slot remains inactive/reset and fiber state remains `FIBER_READY`,
    - verifies interpreter boundary/runtime state snapshot remains unchanged.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Existing tests covered invalid-id cleanup and duplicate/late active-slot behavior, but not the in-range inactive-slot cleanup path.
- This closes a payload-lifetime gap and guards against leaks in a subtle producer/consumer edge path.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1206 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1208 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1207 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 211 - Wakeup Full-Queue Payload Ownership Regression

### Summary
Added scheduler regression coverage to lock in payload ownership semantics when `WAKEUP_OFFLOAD_READY` enqueue fails on a full wakeup ring.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_wakeup_full_payload_ownership_boundary_tests(...)`
  - New coverage loop:
    - fills wakeup ring to capacity,
    - attempts `WAKEUP_OFFLOAD_READY` enqueue with `OffloadCompletion*` payload (expected failure),
    - explicitly frees payload on failure to assert caller ownership on failed enqueue,
    - drains queue and verifies convergence (`head == tail`),
    - verifies `(wakeup_drops delta) == 1`,
    - verifies interpreter boundary/runtime state snapshot remains unchanged.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Worker retry paths rely on a strict contract: failed enqueue must not consume/free payload.
- This regression prevents subtle ownership regressions (leak or double-free) in full-queue producer paths.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1205 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1207 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1206 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 210 - Atomic Wakeup Drop Counter Hardening

### Summary
Hardened scheduler wakeup overflow accounting for multi-producer contexts by making `wakeup_drops` atomic and updating regression reads accordingly.

### What changed
- `src/lisp/scheduler_state_offload.c3`
  - `Scheduler.wakeup_drops` changed from `usz` to `types::Atomic{usz}`.
- `src/lisp/scheduler_wakeup_io.c3`
  - In `wakeup_enqueue(...)`, ring-full path now uses atomic increment:
    - `g_scheduler.wakeup_drops.add(1)`.
- `src/lisp/tests_tests.c3`
  - Updated `run_scheduler_wakeup_drop_counter_boundary_tests(...)` to read atomic counter via `.load()` for delta assertions.

### Why this matters
- `wakeup_enqueue(...)` can be called by producer contexts beyond the scheduler consumer loop (for example worker completion paths). A plain integer increment in that path is a race.
- This keeps overflow telemetry deterministic under concurrency without changing queue behavior or public APIs.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1205 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1206 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1205 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 209 - Wakeup Drop-Counter Boundary Regression

### Summary
Added scheduler regression coverage for wakeup-ring overflow accounting, asserting deterministic `wakeup_drops` increments under overflow and boundary/runtime state stability.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_wakeup_drop_counter_boundary_tests(...)`
  - New coverage loop:
    - fills wakeup ring to capacity,
    - attempts three additional enqueues (expected drops),
    - asserts `(wakeup_drops delta) == 3`,
    - drains queue and asserts `head == tail`,
    - verifies interpreter boundary/runtime fields remain unchanged.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Existing regressions covered overflow rejection and queue-drain behavior but did not validate drop-accounting observability.
- This locks in deterministic overflow telemetry (`wakeup_drops`) while preserving boundary hardening guarantees.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1206 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1205 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1205 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 208 - No-Async Scheduler Cleanup Helper Roll-In

### Summary
Added a shared no-async scheduler test cleanup helper and rolled it into ring-focused boundary tests to centralize wakeup-queue reset + async-handle restoration.

### What changed
- `src/lisp/tests_tests.c3`
  - Added helper:
    - `scheduler_test_cleanup_no_async(...)`
      - resets wakeup queue,
      - restores saved async handle.
  - Migrated ring/no-async test teardown callsites to this helper:
    - `run_scheduler_wakeup_smoke_test(...)`
    - `run_scheduler_wakeup_wraparound_test(...)`
    - `run_scheduler_wakeup_wraparound_boundary_tests(...)`
    - `run_scheduler_invalid_offload_wakeup_boundary_tests(...)`

### Why this matters
- The scheduler test block had repeated no-async teardown sequences with slight variation risk.
- Centralizing no-async cleanup keeps reset semantics uniform and reduces maintenance drift across boundary regressions.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1205 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 207 - Offload Boundary Tests Helper Roll-In

### Summary
Extended the single-fiber scheduler test helper migration into offload-focused boundary regressions, reducing repeated setup/teardown and cleanup boilerplate while preserving behavior.

### What changed
- `src/lisp/tests_tests.c3`
  - Migrated helper usage in offload boundary tests:
    - `run_scheduler_duplicate_offload_ready_boundary_tests(...)`
    - `run_scheduler_consume_pending_offload_boundary_tests(...)`
  - Replaced repeated manual setup/reset blocks with:
    - `scheduler_test_prepare_blocked_pending_offload(...)` or
    - `scheduler_test_prepare_single_fiber_no_async(...)`
    - `scheduler_test_cleanup_single_fiber_no_async(...)`
  - Kept completion-specific assertions and phase semantics unchanged.

### Why this matters
- Session 206 rolled helpers into pending-read-centric tests; offload tests still had duplicated teardown code.
- This keeps cleanup semantics centralized across both read/offload scheduler boundary tests and lowers future drift risk.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1205 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 206 - Scheduler Single-Fiber Test Setup/Cleanup Helper Roll-In

### Summary
Reduced scheduler boundary-test duplication by introducing shared single-fiber test setup/cleanup helpers and migrating high-duplication pending-read paths to them.

### What changed
- `src/lisp/tests_tests.c3`
  - Added shared helpers:
    - `scheduler_test_prepare_single_fiber_no_async(...)`
    - `scheduler_test_prepare_blocked_pending_read(...)`
    - `scheduler_test_prepare_blocked_pending_offload(...)`
    - `scheduler_test_cleanup_single_fiber_no_async(...)`
  - Migrated selected scheduler tests to helper usage:
    - `run_scheduler_timer_wakeup_test(...)`
    - `run_scheduler_wakeup_mixed_event_boundary_tests(...)`
    - `run_scheduler_consume_pending_tcp_read_boundary_tests(...)`
    - `run_scheduler_wakeup_ready_barrier_boundary_tests(...)`
  - Consolidated repeated local teardown logic (pending buffer/completion cleanup, fiber reset, wakeup reset, async restore) into one shared path.

### Why this matters
- Scheduler boundary tests were accumulating repeated setup/teardown blocks with subtle variation risk.
- Shared helpers centralize cleanup semantics and reduce maintenance surface while preserving deterministic test behavior.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1205 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 205 - Complete Scheduler Snapshot Matcher Rollout

### Summary
Finished rolling scheduler boundary regressions onto the snapshot-based matcher, removing remaining long-form boundary field callsites in the scheduler hardening block.

### What changed
- `src/lisp/tests_tests.c3`
  - Migrated remaining scheduler boundary assertions to:
    - `scheduler_runtime_boundary_matches_snapshot(interp, &saved)`
  - Updated mixed boundary drift diagnostics to compare directly against snapshot fields (`saved.*`), eliminating stale local-field dependencies.
  - Kept generic matcher (`scheduler_runtime_boundary_fields_match`) as the underlying primitive used by the snapshot helper.

### Why this matters
- Session 204 introduced snapshot helpers, but some scheduler callsites still used expanded field argument lists.
- Completing the rollout fully centralizes scheduler boundary assertion wiring and reduces risk of mismatched field lists in future edits.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1205 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 204 - Scheduler Boundary Snapshot Helper Consolidation

### Summary
Refactored scheduler boundary regression tests to centralize boundary-state capture/match boilerplate behind a shared snapshot helper, reducing duplication without changing behavior.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `struct SchedulerBoundarySnapshot`
    - `scheduler_capture_runtime_boundary_snapshot(...)`
    - `scheduler_runtime_boundary_matches_snapshot(...)`
  - Migrated scheduler boundary-focused tests to use snapshot capture + helper matcher instead of repeated per-test locals and long matcher argument lists.
  - Updated mixed scheduler boundary drift diagnostics to compare against snapshot fields.
  - No runtime behavior changes; this is test-code simplification/maintenance hardening.

### Why this matters
- The scheduler boundary regression section had high duplication and a growing risk of inconsistent field capture/check wiring.
- Centralizing the snapshot contract reduces future edit risk and keeps boundary hardening tests easier to evolve safely.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1205 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 203 - Thread-Task Cancel Transition Boundary Regression

### Summary
Added scheduler regression coverage for thread-task cancel transition semantics (`cancel -> try_begin -> take`) plus done-task cancel behavior, with boundary/runtime state assertions.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_thread_task_cancel_boundary_tests(...)`
  - New coverage (looped stress):
    - allocates pending task, cancels it, verifies:
      - cancel succeeds with `already_done=false`,
      - `scheduler_try_begin_thread_task(...)` returns false,
      - cancelled completion can be taken and slot clears.
    - allocates done task, verifies cancelling reports `already_done=true` and completion remains retrievable.
    - verifies invalid-id cancel returns false.
  - Verifies interpreter boundary/runtime fields remain unchanged per cycle.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Session 202 covered completion ownership transfer, but not explicit cancel transition semantics and done-task cancel reporting.
- This closes that state-machine seam and strengthens deterministic cancellation behavior under boundary hardening.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1205 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `c3c clean && c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`
- `OMNI_FIBER_TEMP=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1204 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 202 - Thread-Task Completion Boundary Regression

### Summary
Added scheduler regression coverage for thread-task completion ownership paths (`alloc -> complete -> take`) plus invalid-id completion cleanup, with boundary/runtime state assertions.

### What changed
- `src/lisp/tests_tests.c3`
  - Added:
    - `run_scheduler_thread_task_completion_boundary_tests(...)`
  - New coverage (looped stress):
    - allocates thread task slots via `scheduler_alloc_thread_task()`,
    - completes each task with a cancel/error completion payload,
    - takes completion via `scheduler_take_thread_task_completion(...)`,
    - verifies completion ownership transfer and task-slot reset (`exists` false after take),
    - exercises invalid-id completion path via `scheduler_complete_thread_task(MAX_THREAD_TASKS + k, completion)` (should self-clean completion safely).
  - Verifies interpreter boundary/runtime fields remain unchanged.
  - Wired into `run_scheduler_tests(...)`.
