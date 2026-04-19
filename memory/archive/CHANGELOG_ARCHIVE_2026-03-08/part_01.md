### Validation
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Unified tests: `1492 passed, 0 failed`
  - Compiler tests: `79 passed, 0 failed`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Unified tests: `1491 passed, 0 failed`
  - Compiler tests: `79 passed, 0 failed`

## 2026-03-07: Session 40 Slice - Generation-Safe `task-handle` Boundary

### Summary
Hardened pooled task boundaries by replacing exposed numeric task ids with
opaque `task-handle` objects carrying `(task_id, generation)` and validating
that identity at join/cancel/drop boundaries.

### What changed
- Task identity and scheduler state:
  - `src/lisp/scheduler_state_offload.c3`
    - added `SchedulerTaskHandle { task_id, generation }`
    - extended `ThreadTaskEntry` with `generation`
    - added scheduler-wide `task_generation_seed`
  - `src/lisp/scheduler_thread_tasks.c3`
    - added generation allocation/matching helpers
    - made task completion/existence/drop/cancel generation-aware
- Task primitive surface:
  - `src/lisp/scheduler_primitives.c3`
    - `task-spawn` now returns `FFI_HANDLE` named `task-handle`
    - `task-join`, `task-join-timeout`, `task-cancel` now require
      `task-handle` (not integer ids)
    - added task-handle finalizer to safely drop matching task slot
    - invalidated handle payload after successful join consume
- Stdlib/docs and tests:
  - `stdlib/stdlib.lisp`: `io/task-join` / `io/task-cancel` signatures now
    consume `task-handle`
  - `docs/reference/09-concurrency-ffi.md`: examples updated to task handles
  - `docs/plans/library-gaps-todo.md`: parity notes updated to document
    generation-safe task-handle usage
  - `src/lisp/tests_scheduler_boundary_worker.c3`: join-timeout boundary test
    adapted to generation-aware join helper signature
  - `src/lisp/tests_scheduler_boundary_worker.c3`: offload backlog boundary test
    now pumps `uv_run(..., UV_RUN_NOWAIT)` while waiting, matching
    `uv_queue_work` completion delivery semantics

### Validation
- `c3c build`
- `c3c build --sanitize=address`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Unified tests: `1492 passed, 0 failed`
  - Compiler tests: `79 passed, 0 failed`
- `scripts/run_e2e.sh`
  - Stage 3 currently fails in repo baseline with:
    `Failed to resolve src/lisp/value.c3`

## 2026-03-07: Session 39 Slice - Pooled Concurrency Surface Renamed to `task-*`

### Summary
Completed the pooled-concurrency naming cleanup so the runtime and docs now use
`task-*` for pool-backed jobs, reserving `thread-*` for future real OS-thread
primitives.

### What changed
- Runtime/effect wiring:
  - `src/lisp/eval_init_primitives.c3`
    - raw primitives switched to `__raw-task-spawn`, `__raw-task-join`,
      `__raw-task-join-timeout`, `__raw-task-cancel`
    - effect fast-path names switched to `io/task-*`
- Scheduler pooled primitives:
  - `src/lisp/scheduler_primitives.c3`
    - primitive entry points and helper labels renamed from `thread-*` to
      `task-*` for the pool-backed surface
    - error-code strings updated to `scheduler/task-*`
- Stdlib wrappers:
  - `stdlib/stdlib.lisp`
    - effect declarations and wrappers switched to `io/task-*` / `task-*`
- Tests and docs:
  - `src/lisp/tests_tests.c3`
  - `src/lisp/tests_advanced_tests.c3`
  - `src/lisp/tests_scheduler_boundary_worker.c3`
  - `docs/reference/09-concurrency-ffi.md`
  - `docs/reference/12-appendix-stdlib.md`
  - `docs/plans/library-gaps-todo.md`
    - migrated pooled API references from `thread-*` to `task-*`
    - fixed scheduler boundary runner callsite to
      `run_scheduler_task_join_timeout_then_join_boundary_tests(...)`

### Validation
- `c3c build`
- `timeout -t 300 env LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Unified tests: `1492 passed, 0 failed`
  - Compiler tests: `79 passed, 0 failed`
- `c3c build --sanitize=address`
- `timeout -t 360 env ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Unified tests: `1491 passed, 0 failed`
  - Compiler tests: `79 passed, 0 failed`

## 2026-03-07: Session 38 Slice - Explicit Splice-Failure Fallback in JIT Return Boundary

### Summary
Hardened one high-risk boundary failure path by replacing a silent splice
assumption in JIT return finalization with an explicit ownership-safe fallback.

### What changed
- `src/lisp/jit_jit_eval_scopes.c3`
  - In `jit_finalize_scoped_result(...)`, the splice branch now:
    - returns early only when `boundary_try_scope_splice_escapes(...)` succeeds,
    - otherwise explicitly falls back to
      `boundary_copy_from_releasing_scope(...)` + `scope_release(...)`.
  - This removes the previous silent assumption that a splice attempt always
    succeeds once promote eligibility is true.

### Validation
- `c3c build`
- `timeout -t 240 env LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - full suite completed: `1492 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`
- `c3c build --sanitize=address`
- `timeout -t 300 env ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - ASAN suite completed: `1491 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`

## 2026-03-07: Session 37 Closure - High-Risk Caller Migration Audit

### Summary
Closed Session 37 by auditing high-risk return/env/splice caller paths and
confirming runtime callers now route through boundary facade APIs.

### Migrated caller summary (runtime paths)
- Return-boundary transitions:
  - `src/lisp/eval_run_pipeline.c3`
  - `src/lisp/jit_jit_eval_scopes.c3`
  - both use boundary helpers for promotion/copy/splice decisions.
- Env-copy/splice callsites:
  - runtime calls route through `boundary_copy_env_to_scope(...)` in
    `src/lisp/eval_boundary_api.c3`.
  - scope splice path is centralized in:
    - `boundary_try_scope_splice_escapes(...)`
  - no direct runtime `scope_splice_escapes(...)` callsites remain outside the
    boundary facade.

### Audit evidence (grep checks)
- direct high-risk runtime calls outside boundary modules:
  - `copy_env_to_scope(...)`: no runtime callers outside
    `eval_env_copy.c3` + boundary facade
  - `scope_splice_escapes(...)`: no runtime callers outside boundary facade
  - direct promote/copy primitives in non-boundary runtime modules: not found
    (remaining direct usages are in tests/metrics).

### Validation
- Reused latest green gate run for this migration state:
  - `c3c build`
  - `timeout -t 240 env LD_LIBRARY_PATH=/usr/local/lib ./build/main`
    - `1492 passed, 0 failed`
  - `c3c build --sanitize=address`
  - `timeout -t 300 env ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
    - `1491 passed, 0 failed`

## 2026-03-07: Session 36 Slice - Shared Boundary Policy Helpers and Caller Migration

### Summary
Completed a business-logic unification slice by introducing shared policy
helpers for promote/copy/splice decisions and routing key return/env/splice
paths through those helpers.

### What changed
- `src/lisp/eval_boundary_api.c3`
  - Added shared boundary policy helpers:
    - `boundary_should_promote(...)`
    - `boundary_should_copy_env(...)`
    - `boundary_is_scope_transfer_legal(...)`
  - Updated `boundary_try_scope_splice_escapes(...)` to gate through
    `boundary_is_scope_transfer_legal(...)`.
- `src/lisp/eval_run_pipeline.c3`
  - Replaced inline promote/splice gate:
    - from direct `!ctx.aborted && scope_gen == ...` check
    - to `boundary_should_promote(...)`.
- `src/lisp/jit_jit_eval_scopes.c3`
  - Replaced inline splice eligibility branch with `boundary_should_promote(...)`.
- `src/lisp/eval_env_copy.c3`
  - Routed env-copy reuse policy through shared helper:
    - `copy_env_should_reuse_value(...)` now derives from
      `boundary_should_copy_env(...)`.
- `docs/plans/session-34-44-boundary-hardening.md`
  - Marked Session 36 checklist complete (Commit A/B, gates, changelog).

### Edge-case decision matrix (current helper semantics)
- `boundary_should_promote(v, ctx, source_scope)` -> false when any of:
  - `v == null`,
  - `ctx == null`,
  - `source_scope == null`,
  - `ctx.aborted == true`,
  - `v.scope_gen != source_scope.escape_generation`.
- `boundary_should_copy_env(v, interp)` -> false when `v == null`; otherwise
  true only when value is not reusable in the current target scope chain.
- `boundary_is_scope_transfer_legal(parent, child)` -> true only when both
  scopes are non-null and distinct.

### Validation
- `c3c build`
- `timeout -t 240 env LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - full suite completed: `1492 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`
- `c3c build --sanitize=address`
- `timeout -t 300 env ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - ASAN suite completed: `1491 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`

## 2026-03-07: Session 35 Slice B - Boundary Naming Normalization and Duplicate Condition Cleanup

### Summary
Completed Session 35 Commit B by normalizing repeated promotion-escape decision
checks into shared helpers and removing duplicated inline condition trees in the
escape promotion path.

### What changed
- `src/lisp/eval_promotion_escape.c3`
  - Added shared decision helpers:
    - `promote_escape_can_mutate(...)`
    - `promote_escape_should_iterate_cons_tail(...)`
  - Replaced repeated `promotion_context_consume(...)` guard branches across
    escape mutation helpers with the normalized guard helper.
  - Replaced duplicated inline cons-tail condition tree with
    `promote_escape_should_iterate_cons_tail(...)`.
- `docs/plans/session-34-44-boundary-hardening.md`
  - Marked Session 35 Commit B items complete and recorded gate/changelog
    closure.

### Validation
- `c3c build`
- `timeout -t 240 env LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - full suite completed: `1492 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`
- `c3c build --sanitize=address`
- `timeout -t 300 env ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - ASAN suite completed: `1491 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`

## 2026-03-07: Phase 7 Validation Refresh (Deferred Shared Reclamation)

### Summary
Re-ran the required Phase 7 validation gates after deferred shared-object
reclamation and retire-queue boundary updates; all required gates passed.

### Validation
- `c3c build`
  - linked successfully (`build/main`).
- `c3c build --sanitize=address`
  - linked successfully (`build/main`).
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - unified suite: `1491 passed, 0 failed`
  - compiler suite: `79 passed, 0 failed`

## 2026-03-07: Session 34 Slice - Boundary Facade Promotion-Context Routing

### Summary
Completed a low-risk boundary-facade hardening slice by exposing promotion
context entry/exit through audited boundary API wrappers and routing core
runtime callsites through those wrappers.

### What changed
- `src/lisp/eval_boundary_api.c3`
  - Added explicit facade entry points with contracts:
    - `boundary_promotion_context_begin(...)`
    - `boundary_promotion_context_end(...)`
  - Both wrappers enforce `interp`/`ctx` preconditions via `@require`.
- Runtime callsites migrated from direct promotion-context calls to boundary facade:
  - `src/lisp/eval_run_pipeline.c3`
  - `src/lisp/eval_env_copy.c3`
  - `src/lisp/jit_jit_eval_scopes.c3`
  - `src/lisp/eval_promotion_escape.c3`
- Planning/docs sync:
  - `docs/plans/session-34-44-boundary-hardening.md`
    - marked Session 34 items complete (`Commit A/B`, low-risk migration,
      gates, changelog record).

### Validation
- `c3c build`
- `timeout -t 240 env LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - full suite completed: `1492 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`
- `c3c build --sanitize=address`
- `timeout -t 300 env ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - ASAN suite completed: `1491 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`

## 2026-03-07: Session 35 Slice A - Copy Boundary Decision/Mutation Decomposition

### Summary
Completed Session 35 Commit A by splitting `copy_to_parent` boundary internals
into explicit decision routing and mutation execution helpers, reducing large
branch coupling in the hot boundary path.

### What changed
- `src/lisp/eval_promotion_copy.c3`
  - Added route enum + selector:
    - `CopyParentRoute`
    - `copy_parent_route_for_tag(...)`
  - Added mutation executor:
    - `copy_to_parent_by_route(...)`
  - Refactored `copy_to_parent(...)` to:
    - keep existing fast-reuse/defensive-copy guardrails,
    - dispatch through route selection + executor instead of one monolithic
      tag switch.
- `src/lisp/eval_env_copy.c3`
  - Normalized one direct chain check to boundary facade naming:
    - `in_target_scope_chain(...)` -> `boundary_ptr_in_target_scope_chain(...)`
    - in `copy_env_clone_iterator_if_needed(...)`.
- `docs/plans/session-34-44-boundary-hardening.md`
  - Marked Session 35 Commit A items complete.

### Validation
- `c3c build`
- `timeout -t 240 env LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - full suite completed: `1492 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`
- `c3c build --sanitize=address`
- `timeout -t 300 env ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - ASAN suite completed: `1491 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`

## 2026-03-07: Pipe Blocking-Parity Restore + Shared-Retire Test Consolidation

### Summary
Resolved the `pipe loopback roundtrip` regression introduced by the new libuv
pipe setup path by restoring blocking-socket parity as the default runtime
behavior, while keeping libuv wrappers as fallback integration hooks. Also
removed a duplicate shared-retire boundary test definition so the suite builds
cleanly with a single canonical implementation.

### What changed
- `csrc/uv_helpers.c`
  - Added fd-mode normalization helper for duplicated pipe descriptors:
    - clears `O_NONBLOCK` on returned fds from:
      - `omni_uv_pipe_connect_fd(...)`
      - `omni_uv_pipe_listen_fd(...)`
  - Keeps wrapper-returned descriptors compatible with blocking runtime call
    sites when fallback activates.
- `src/lisp/async.c3`
  - Updated pipe primitives to preserve language/runtime blocking semantics:
    - `prim_pipe_connect` now uses POSIX `pipe_connect_fd(...)` first, then
      falls back to `omni_uv_pipe_connect_fd(...)`.
    - `prim_pipe_listen` now uses POSIX `pipe_listen_fd(...)` first, then
      falls back to `omni_uv_pipe_listen_fd(...)`.
- `src/lisp/tests_tests.c3`
  - Removed duplicate `run_scheduler_shared_retire_queue_boundary_tests(...)`
    definition (canonical implementation remains in
    `src/lisp/tests_scheduler_boundary_worker.c3`).
- `docs/plans/library-gaps-todo.md`
  - Updated `io/pipe-connect` / `io/pipe-listen` backend notes to
    `mixed (POSIX primary + uv_pipe fallback)` so parity documentation matches
    codebase behavior.
  - Marked libuv DoD ownership gate complete:
    - `Boundary and memory ownership invariants remain RC/scope-region primary`.
  - Marked Execution Policy checklist complete for the current libuv track
    closure snapshot.
- `docs/reference/07-io-networking.md`
  - Documented pipe runtime semantics explicitly:
    - POSIX blocking path is primary,
    - libuv pipe setup is retained as fallback integration wiring.
- `docs/plans/concurrency-hybrid-memory-checklist.md`
  - Marked Immediate Snapshot `Phase 7` complete and updated current position to
    `Phase 7 complete`.
  - Added explicit full-run and ASAN validation snapshot under Validation Plan.

### Validation
- `c3c build`
- `env LD_LIBRARY_PATH=/usr/local/lib ./build/main /tmp/pipe_test.omni`
  - output: `"pipe-ok"`
- `timeout -t 240 env LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - full suite completed: `1492 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`
- `c3c build --sanitize=address`
- `timeout -t 300 env ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - ASAN suite completed: `1491 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`

## 2026-03-07: libuv Unix Pipe Setup Integration + Parity Map Expansion

### Summary
Advanced the next libuv DoD gate by routing Unix pipe setup primitives through
libuv-backed wrappers and expanding the parity map to include previously omitted
`pipe-*`, `process-*`, and `signal-*` effects.

### What changed
- `csrc/uv_helpers.c`
  - Added libuv pipe setup helpers:
    - `omni_uv_pipe_connect_fd(...)`
    - `omni_uv_pipe_listen_fd(...)`
  - Implementation details:
    - uses `uv_pipe_init`/`uv_pipe_connect` and `uv_pipe_init`/`uv_pipe_bind`/`uv_listen`,
    - duplicates underlying `uv_fileno` descriptor for runtime use,
    - drains/closes temporary loop/handles deterministically.
- `src/lisp/async.c3`
  - Added extern bindings for new helpers.
  - Updated primitives:
    - `prim_pipe_connect` now prefers `omni_uv_pipe_connect_fd`,
    - `prim_pipe_listen` now prefers `omni_uv_pipe_listen_fd`.
  - Preserved direct POSIX fallback (`pipe_connect_fd` / `pipe_listen_fd`) for
    migration when libuv setup fails.
- `docs/plans/library-gaps-todo.md`
  - Marked DoD gate complete:
    - `Unix pipes, process, and signal paths have libuv-backed APIs`.
  - Expanded backend map with:
    - `io/pipe-connect`, `io/pipe-listen`,
    - `io/process-spawn`, `io/process-wait`, `io/process-kill`,
    - `io/signal-handle`, `io/signal-unhandle`.
  - Updated snapshot counts:
    - `done-libuv`: `4/34`,
    - `partial-libuv`: `16/34`,
    - `non-libuv`: `14/34`.

### Validation
- `c3c build`
- `timeout -t 240 env LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - full suite completed: `1491 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`

## 2026-03-07: libuv TCP Write Wakeup Parity + Async Test Deadlock Fix

### Summary
Closed the stream/socket wakeup parity gap by aligning documentation with the
landed libuv writable wakeup bridge for `tcp-write`, and fixed a scheduler test
deadlock that blocked full-suite completion.

### What changed
- `src/lisp/tests_tests.c3`
  - Fixed `tcp-write in fiber via async bridge` test orchestration:
    - moved server-side read into a spawned fiber,
    - awaited read/write fibers instead of blocking main thread on `tcp-read`
      before writer fiber scheduling.
  - Result: removes false deadlock from test ordering while preserving
    scheduler-driven async coverage.
- `docs/plans/library-gaps-todo.md`
  - Marked DoD gate complete:
    - `Stream/socket async wakeups are libuv-driven and fiber-safe`.
  - Updated parity map for `io/tcp-write`:
    - backend now documented as `mixed (libuv-bridge in fiber, blocking fallback)`,
      status `partial-libuv`.
  - Updated A0 summary counts:
    - `partial-libuv` from `10/27` to `11/27`,
    - `non-libuv` from `15/27` to `14/27`.

### Validation
- `c3c build`
- `timeout -t 30 env LD_LIBRARY_PATH=/usr/local/lib ./build/main /tmp/omni_tcp_write_fiber_test.omni`
  - output: `"ping"`, `4`, `true`
- `timeout -t 240 env LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - full suite completed: `1491 passed, 0 failed` and `Compiler Tests: 79 passed, 0 failed`

## 2026-03-07: Phase 6 Domain Abstraction + Phase 7 Deferred Shared Reclamation Queue

### Summary
Advanced the concurrency/shared-memory checklist by completing the domain
abstraction layer (Phase 6) and introducing a first deferred-reclamation path
for shared-handle payloads through an explicit retire queue drained at domain
safe points (Phase 7 implementation step).

### What changed
- `src/lisp/scheduler_state_offload.c3`
  - Added domain abstraction primitives:
    - `SchedulerDomain`
    - primary/current domain helpers
    - domain affinity checks for fiber ownership
    - domain-layer publish/consume/projection wrappers over the shared bridge.
  - Added deferred shared-object reclamation plumbing for registry payloads:
    - `SHARED_RETIRED_QUEUE_CAPACITY`
    - `SharedRetiredPayload` queue state in `SharedRegistry`
    - queued retire helper under registry lock:
      - `scheduler_shared_registry_enqueue_retired_payload_locked`
    - deferred drain helper:
      - `scheduler_shared_registry_drain_retired`
    - payload free helper:
      - `scheduler_shared_registry_free_payload`
    - domain safe point hook:
      - `scheduler_domain_safe_point`
  - Updated shared destroy/retire paths to enqueue payload reclamation when a
    shared slot is retired, with immediate free fallback on queue overflow.
  - Added safe-point draining on domain bridge operations so reclaim is deferred
    to explicit boundary crossings rather than mixed into local runtime value
    ownership.
- `src/lisp/scheduler_io_fiber_core.c3`
  - Added per-fiber domain id initialization at fiber creation.
  - Added domain-fiber mismatch fail-fast guard on fiber resume.
- `src/lisp/scheduler_wakeup_io.c3`
  - Routed shared projection and sendable-int consume through domain wrappers.
- `src/lisp/scheduler_offload_worker.c3`
  - Routed sendable-int publication through domain wrapper helpers.
- `src/lisp/tests_tests.c3`
  - Added `run_scheduler_shared_retire_queue_boundary_tests(...)` coverage:
    - deferred retire-queue enqueue/drain behavior,
    - stale generation handle resolution rejection,
    - scheduler boundary/runtime snapshot preservation through retire cycles.

### Validation
- Build/test validation not re-run in this continuation checkpoint.
- Prior checkpoint builds (`c3c build` and `c3c build --sanitize=address`) were
  green before the deferred-reclamation queue patch in this entry.

## 2026-03-07: Effect File I/O Routed Through `uv_fs_*`

### Summary
Moved core effect-level file operations from `io::file::*` calls to libuv
filesystem helpers so Omni effect paths use `uv_fs_*` entry points.

### What changed
- `src/lisp/prim_io.c3`
  - Added `io_uv_read_all_file(...)` helper built on:
    - `uv_fs_open`,
    - `uv_fs_stat`,
    - `uv_fs_read`,
    - `uv_fs_close`.
  - Updated primitives to use libuv FS helpers:
    - `prim_read_file`
    - `prim_write_file`
    - `prim_file_exists`
    - `prim_read_lines`
  - Behavior remains return-compatible (`nil` on error for these primitives),
    while backend plumbing now routes through `uv_fs_*`.
- `docs/plans/library-gaps-todo.md`
  - Marked DoD gate complete for file operations using `uv_fs_*` paths.
  - Updated backend/status map rows for:
    - `io/read-file`
    - `io/write-file`
    - `io/file-exists?`
    - `io/read-lines`
  - Updated A0 summary counts to reflect new `partial-libuv` coverage.

### Validation
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-07: Deduce Named DB Exposure + Restart Durability Coverage

### Summary
Completed remaining Track F parity items by adding named LMDB database exposure
at the deduce language surface and adding restart durability regression
coverage. Fixed a file-mode bug that prevented persistent DB reopen across
processes.

### What changed
- `src/lisp/deduce.c3`
  - Added unified deduce dispatcher route for `'open-named`:
    - `(deduce 'open-named db relation-name db-name col...)`
  - Updated command guidance/unknown-command text to include `'open-named`.
  - Fixed persistent LMDB file mode for `mdb_env_open`:
    - changed `0664` (decimal) to `0o664` (octal).
  - Impact:
    - persistent DB/lock files now open with expected permissions
      (`rw-r--r--` under default umask), allowing reopen in subsequent
      processes.
- `src/lisp/deduce_schema_query.c3`
  - Added `prim_deduce_open_named(...)` implementation:
    - explicit DB-handle validation,
    - `db-name` accepts string or symbol,
    - explicit relation allocation/column collection/open flow.
  - Added helper `deduce_relation_open_dbi_with_name(...)` and reused it from
    existing relation open path.
- `src/lisp/tests_tests.c3`
  - Added `run_deduce_open_named_test(...)` coverage:
    - isolation by explicit `db-name`,
    - symbol-form `db-name` support,
    - invalid `db-name` type rejection.
  - Added `run_deduce_durability_restart_test(...)`:
    - writes a persistent relation in one Omni process,
    - reopens and verifies persisted rows in a second Omni process.
  - Added subprocess helpers for script generation/launch and cleanup.
  - Wired both tests into deduce group execution.
- `docs/reference/08-libraries.md`
  - Added `open-named` usage example.
- `docs/plans/library-gaps-todo.md`
  - Marked Track F items complete:
    - named database exposure,
    - durability/recovery tests across restart.

### Validation
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-07: Phase 5 One-Way Projection Gate for Shared Handles

### Summary
Implemented an explicit one-way projection helper so shared-handle payloads are
materialized into local `Value*` only at scheduler boundary consumption points.

### What changed
- `src/lisp/scheduler_state_offload.c3`
  - Added `scheduler_project_shared_to_local_value(SharedHandle** handle_ref, Interp* interp)`.
  - Projection now enforces a one-way conversion flow:
    - take shared bytes ownership,
    - release shared handle,
    - return local `Value*` (`make_string_owned`).
  - Handle reference is nulled after projection to prevent downstream handle reuse.
- `src/lisp/scheduler_wakeup_io.c3`
  - Routed `OFFLOAD_RES_BYTES` conversion through
    `scheduler_project_shared_to_local_value` in
    `scheduler_value_from_offload_bytes`.

### Validation
- `c3c build`
- `c3c build --sanitize=address`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - current baseline has one unrelated existing failure:
    - `[FAIL] deduce durability across restart (writer=ok reader=FAIL)`
  - scheduler/offload suites in the same run remained green.

## 2026-03-07: SharedHandle Registry (Phase 4) With Generation Validation

### Summary
Implemented a runtime-owned `SharedHandle` registry for shared byte objects,
including generation-validated resolution and explicit shared destroy/retire
entry points.

### What changed
- `src/lisp/scheduler_state_offload.c3`
  - Added runtime-owned shared registry state:
    - `SHARED_REGISTRY_CAPACITY`
    - `SharedRegistryEntry`
    - `SharedRegistry`
    - scheduler-owned `shared_registry` field
  - Extended `SharedHandle` with registry identity metadata:
    - `slot`
    - `generation`
    - `payload` (safety fallback)
  - Added registry lifecycle/validation helpers:
    - `scheduler_shared_registry_ensure_ready`
    - `scheduler_shared_registry_alloc`
    - `scheduler_shared_registry_resolve_blob`
    - `scheduler_shared_registry_destroy_slot`
    - `scheduler_shared_registry_retire_entry`
    - `scheduler_shared_registry_next_generation`
  - Added explicit shared object entry points:
    - `scheduler_shared_destroy`
    - `scheduler_shared_retire`
  - Routed blob handle creation through registry allocation and generation
    stamping.
  - Routed `shared_handle_as_blob` through generation-validated registry
    resolution.
  - Routed shared release through explicit shared destroy path.
- `src/lisp/scheduler_state_offload.c3`
  - `scheduler_init` now ensures shared registry readiness as part of scheduler
    initialization.

### Validation
- `c3c build`
- `c3c build --sanitize=address`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-07: Deduce Relation Maintenance API (`clear!` / `drop!`)

### Summary
Added explicit relation maintenance commands to deduce for bulk cleanup and
schema-level removal, with dropped-handle safety checks across query/write
paths.

### What changed
- `src/lisp/deduce.c3`
  - Added LMDB `mdb_drop` extern binding.
  - Added unified dispatch support for:
