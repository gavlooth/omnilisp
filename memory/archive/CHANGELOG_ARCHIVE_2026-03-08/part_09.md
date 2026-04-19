
### Validation
- Local boundary profile remains green:
  - `scripts/run_boundary_hardening.sh`
  - normal and ASAN stages pass, summary assertions pass, JSON artifact emitted.
- Workflow file is declarative and does not alter runtime behavior.

## 2026-03-05: Session 161 - Boundary JSON Summary Artifact

### Summary
Added a machine-readable summary artifact for the boundary-hardening profile and wired it into the runner after assertions pass.

### What changed
- `scripts/parse_boundary_summary.sh` (new, executable)
  - Parses `OMNI_TEST_SUMMARY` lines from:
    - normal log
    - ASAN log
  - Emits structured JSON summary containing:
    - `stack_engine`, `scope_region`, `unified`, `compiler`
    - `stack_affinity_harness`
    - `fiber_temp_pool` counters and enabled flag
  - Default output: `build/boundary_hardening_summary.json`
- `scripts/run_boundary_hardening.sh`
  - Added Stage 6 summary artifact generation via parser script.
  - Added toggles:
    - `OMNI_BOUNDARY_EMIT_JSON` (default `1`)
    - `OMNI_BOUNDARY_SUMMARY_JSON` (default `build/boundary_hardening_summary.json`)
  - Final output now reports summary JSON path.
- `docs/PROJECT_TOOLING.md`
  - Documented JSON artifact and new toggles.

### Why this matters
- Provides a stable artifact for CI upload and trend processing.
- Avoids manual log scraping for boundary-hardening signals.
- Complements Stage 5 assertions with reusable machine-readable output.

### Validation
- Ran `scripts/run_boundary_hardening.sh` end-to-end.
- Result:
  - normal stage pass (`stack_engine 21/0`, `scope_region 51/0`, `unified 1182/0`, `compiler 73/0`)
  - ASAN stage pass (`stack_engine 20/0`, `scope_region 51/0`, `unified 1181/0`, `compiler 73/0`)
  - Stage 5 assertions passed
  - Stage 6 emitted: `build/boundary_hardening_summary.json`
- Verified JSON content includes expected suite and fiber-temp fields for both normal and ASAN sections.

## 2026-03-05: Session 160 - Boundary Runner Summary Assertions

### Summary
Upgraded the boundary-hardening runner with machine-checkable summary assertions so it fails fast when required suite summaries are missing or report failures.

### What changed
- `scripts/run_boundary_hardening.sh`
  - Added staged log capture:
    - `build/boundary_hardening_normal.log`
    - `build/boundary_hardening_asan.log`
  - Added summary parsing/assertion helpers:
    - verifies required suites have `fail=0`:
      - `stack_engine`, `scope_region`, `unified`, `compiler`
      - `stack_affinity_harness` when affinity harness is enabled
    - verifies `fiber_temp_pool enabled=1` when Fiber TEMP is enabled
  - Added profile guard:
    - `OMNI_BOUNDARY_ASSERT_SUMMARY=1` requires `OMNI_BOUNDARY_SUMMARY=1`
  - Added Stage 5 assertion phase and explicit diagnostics on missing/bad summary lines.
- `docs/PROJECT_TOOLING.md`
  - Documented summary assertions and new toggle:
    - `OMNI_BOUNDARY_ASSERT_SUMMARY=0` to skip assertions
  - Documented boundary profile log artifacts.

### Why this matters
- Turns boundary hardening into a deterministic contract, not just a long log scan.
- Improves CI/readability by surfacing concise failure reasons for missing or bad summary lines.
- Ensures affinity harness and Fiber TEMP signals are actively validated when enabled.

### Validation
- Ran `scripts/run_boundary_hardening.sh` end-to-end.
- Result:
  - normal stage pass (`stack_engine 21/0`, `scope_region 51/0`, `unified 1182/0`, `compiler 73/0`)
  - ASAN stage pass (`stack_engine 20/0`, `scope_region 51/0`, `unified 1181/0`, `compiler 73/0`)
  - Stage 5 summary assertions passed.
  - Harness summary pass in both stages (`stack_affinity_harness pass=1 fail=0`).

## 2026-03-05: Session 159 - Boundary-Hardening Runner Script

### Summary
Added a dedicated boundary-hardening runner script that executes the full safety matrix (normal + ASAN) with Fiber TEMP and stack-affinity harness enabled by default.

### What changed
- `scripts/run_boundary_hardening.sh` (new, executable)
  - Stage 1: normal build (`c3c build`)
  - Stage 2: normal run (`./build/main`) with boundary profile env defaults
  - Stage 3: ASAN build (`c3c clean && c3c build --sanitize=address`)
  - Stage 4: ASAN run with strict `ASAN_OPTIONS`
  - Default profile env:
    - `OMNI_FIBER_TEMP=1`
    - `OMNI_STACK_AFFINITY_HARNESS=1`
    - `OMNI_TEST_QUIET=1`
    - `OMNI_TEST_SUMMARY=1`
  - Optional toggles:
    - `OMNI_BOUNDARY_ENABLE_FIBER_TEMP`
    - `OMNI_BOUNDARY_ENABLE_AFFINITY_HARNESS`
    - `OMNI_BOUNDARY_QUIET`
    - `OMNI_BOUNDARY_SUMMARY`
- `docs/PROJECT_TOOLING.md`
  - Added “Developer Test Profiles” section documenting boundary-hardening runner usage and toggles.

### Why this matters
- Provides a single repeatable command for boundary-safety verification.
- Keeps default local workflow unchanged while making hardening checks easy to run or wire into CI.
- Ensures affinity fail-fast probe and Fiber TEMP paths are exercised together in one profile.

### Validation
- Ran `scripts/run_boundary_hardening.sh`.
- Result: full profile passed:
  - normal stack engine `21/0`, scope region `51/0`, unified `1182/0`, compiler `73/0`
  - ASAN stack engine `20/0`, scope region `51/0`, unified `1181/0`, compiler `73/0`
  - harness summary: `OMNI_TEST_SUMMARY suite=stack_affinity_harness pass=1 fail=0` in both normal and ASAN stages.

## 2026-03-05: Session 158 - Optional Affinity Harness Wrapper in Test Mode

### Summary
Added an opt-in harness wrapper (`OMNI_STACK_AFFINITY_HARNESS=1`) that runs the stack-affinity misuse probe as a subprocess after the normal test suite and reports deterministic pass/fail summary output.

### What changed
- `src/entry.c3`
  - Added `run_stack_affinity_harness(self_exe)`:
    - spawns `self_exe --stack-affinity-probe`,
    - captures output to `/tmp/omni_stack_affinity_probe.log`,
    - validates expected behavior via:
      - non-zero subprocess exit,
      - output marker `"stack-engine thread-affinity violation"`.
  - Added `run_test_mode_with_self(self_exe)` wrapper:
    - runs existing full test mode unchanged,
    - when `OMNI_STACK_AFFINITY_HARNESS` is set, runs harness and emits summary.
  - `main(...)` now invokes `run_test_mode_with_self(argv[0])` in default test mode.
  - Added helper `cstr_contains(...)` for marker matching.
- Default behavior unchanged when `OMNI_STACK_AFFINITY_HARNESS` is not set.

### Why this matters
- Verifies fail-fast ownership boundaries in an automated but isolated way.
- Avoids adding crash-style tests inside the in-process default suite.
- Provides machine-readable harness summary for CI or local diagnostics.

### Validation
- Normal default:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 21/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`)
- Normal with harness:
  - `OMNI_STACK_AFFINITY_HARNESS=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ...`
  - Result: full suite pass + `OMNI_TEST_SUMMARY suite=stack_affinity_harness pass=1 fail=0`
- ASAN default:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 ...`
  - Result: pass (`Stack engine 20/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`)
- ASAN with harness:
  - same ASAN options + `OMNI_STACK_AFFINITY_HARNESS=1 OMNI_TEST_SUMMARY=1`
  - Result: pass + `OMNI_TEST_SUMMARY suite=stack_affinity_harness pass=1 fail=0`

## 2026-03-05: Session 157 - Opt-In Stack Affinity Misuse Probe

### Summary
Added an explicit, opt-in CLI probe to validate stack-engine thread-affinity fail-fast behavior without affecting default test/CI flows.

### What changed
- `src/stack_engine.c3`
  - Added `run_stack_engine_affinity_violation_probe()`.
  - Probe behavior:
    - creates a `StackPool` and `StackCtx`,
    - deliberately corrupts `StackCtx.owner_thread_token`,
    - invokes `stack_ctx_destroy(...)`, which must trigger fail-fast ownership violation.
  - Returns a non-zero error code only if the expected fail-fast path does not trigger.
- `src/entry.c3`
  - Added `--stack-affinity-probe` command-line mode:
    - `omni --stack-affinity-probe`
  - Added help text entry for the new probe mode.

### Why this matters
- Gives a concrete harness for cross-thread misuse verification while keeping the normal suite deterministic and green.
- Makes ownership-guard behavior auditable in automation and local debugging.
- Supports Fiber TEMP/thread-boundary hardening evidence without introducing flaky in-process crash tests.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 21/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 20/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`)
- Flagged summary:
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 ...`
  - Result: pass (`stack_engine pass=20 fail=0`, stable `fiber_temp_pool` telemetry)
- Probe execution:
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --stack-affinity-probe`
  - Observed fail-fast ownership violation backtrace, process exit code `132` (expected non-zero).

## 2026-03-05: Session 156 - Stack API-Level Affinity Guards

### Summary
Extended stack engine thread-affinity enforcement from top-level lifecycle functions into all defer/lifecycle API surfaces, so misuse is blocked at the first boundary call rather than only at create/resume/destroy sites.

### What changed
- `src/stack_engine.c3`
  - Added owner-thread checks to defer/lifecycle APIs:
    - `stack_ctx_defer(...)`
    - `stack_ctx_undefer(...)`
    - `stack_ctx_defer_update_arg(...)`
    - `stack_ctx_lifecycle_attach(...)`
    - `stack_ctx_find_lifecycle_arg(...)`
    - `stack_ctx_run_lifecycle_destroy(...)`
    - `stack_ctx_clear_lifecycle_storage(...)`
    - `stack_ctx_clone_lifecycle_entries(...)`
    - `stack_ctx_run_deferred_destroy(...)`
    - `stack_ctx_clear_defer_storage(...)`
  - No behavior change for valid single-thread owner paths; guards only tighten invalid cross-thread usage.

### Why this matters
- Completes the thread-affinity safety boundary for stack-owned lifetime state.
- Protects defer/lifecycle metadata integrity (critical for Fiber TEMP context caches and teardown correctness).
- Reduces chance of latent corruption from accidental internal misuse.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 21/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 20/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`)
- Flagged summary:
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 ...`
  - Result: pass (`stack_engine pass=20 fail=0`, `fiber_temp_pool` telemetry unchanged/stable).

## 2026-03-05: Session 155 - Stack Engine Thread-Affinity Hardening

### Summary
Hardened stack engine ownership boundaries by adding explicit thread-affinity guards to `StackPool` and `StackCtx` operations. This makes cross-thread misuse fail fast instead of silently corrupting stack/lifecycle state.

### What changed
- `src/stack_engine.c3`
  - Added owner token fields:
    - `StackPool.owner_thread_token`
    - `StackCtx.owner_thread_token`
  - Added thread-affinity helpers:
    - `stack_current_thread_token()`
    - `stack_require_pool_owner(...)`
    - `stack_require_ctx_owner(...)`
  - Enforced affinity checks in hot lifecycle operations:
    - `stack_pool_shutdown(...)`
    - `stack_ctx_create(...)`
    - `stack_ctx_destroy(...)`
    - `stack_ctx_init(...)`
    - `stack_ctx_switch_to(...)`
    - `stack_ctx_suspend(...)`
    - `stack_ctx_resume(...)`
    - `stack_ctx_clone(...)`
  - Added targeted test:
    - `test_stack_ctx_thread_affinity_state()` verifies pool/context ownership tokens are initialized to the current thread.
  - Wired test into `run_stack_engine_tests(...)`.

### Why this matters
- Aligns stack engine safety posture with existing `ScopeRegion` owner-thread checks.
- Protects Fiber TEMP lifecycle callbacks and stack context pooling from accidental cross-thread teardown/use.
- Reduces risk of non-deterministic memory/lifetime failures by turning ownership violations into immediate failures.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 21/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 20/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`)
- Flagged summary:
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_engine pass=20 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=322 misses=4 returns=493 drop_frees=0 pooled=6 peak=6 ctx_hits=161 ctx_returns=326 ctx_pools=161 lc_clone=160 lc_destroy=321 lc_defer=160 lc_flush=165 eligible_slow=2 bypass_large=0 bypass_escape=2`

## 2026-03-05: Session 154 - Fiber TEMP Long-Run Retention Guard

### Summary
Added a long-run retention guard test for Fiber TEMP clone/discard lifecycle paths to detect runaway pool growth or missing destroy-flush behavior across repeated cycles.

### What changed
- `src/stack_engine.c3`
  - Added `test_stack_ctx_fiber_temp_retention_guard()`.
  - Under `OMNI_FIBER_TEMP=1`, runs 128 repeated cycles of:
    - create context,
    - suspend at scope boundary,
    - clone + immediate clone destroy,
    - resume source to completion,
    - source destroy.
  - Asserts:
    - context-pool creation increases,
    - lifecycle flush count increases,
    - global pooled chunk count remains bounded (`pooled_after <= pooled_before + 64`).
  - Wired into `run_stack_engine_tests()`.

### Why this matters
- Adds a deterministic long-run retention guard for exactly the lifecycle mode most prone to subtle leaks (clone/discard loops).
- Complements earlier telemetry counters with a direct bounded-growth invariant.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 20/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 19/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`)
- Flagged summary:
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 ...`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_engine pass=20 fail=0`
    - `OMNI_TEST_SUMMARY suite=scope_region pass=51 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=322 misses=4 returns=493 drop_frees=0 pooled=6 peak=6 ctx_hits=161 ctx_returns=326 ctx_pools=161 lc_clone=160 lc_destroy=321 lc_defer=160 lc_flush=165 eligible_slow=2 bypass_large=0 bypass_escape=2`

## 2026-03-05: Session 153 - Fiber TEMP Lifecycle Telemetry Hardening

### Summary
Added explicit Fiber TEMP lifecycle telemetry counters and assertions so clone-share lifecycle behavior is directly observable (not inferred from aggregate pool counters).

### What changed
- `src/scope_region.c3`
  - Extended `FiberTempPoolStats` with:
    - `ctx_pool_created`
    - `lifecycle_clone_callbacks`
    - `lifecycle_destroy_callbacks`
    - `lifecycle_destroy_deferred`
    - `lifecycle_destroy_flush_chunks`
  - Wired counters into lifecycle callback paths:
    - clone callback increments `lifecycle_clone_callbacks`
    - destroy callback increments `lifecycle_destroy_callbacks`
    - deferred destroy branch increments `lifecycle_destroy_deferred`
    - per-chunk flush on terminal destroy increments `lifecycle_destroy_flush_chunks`
    - context-pool attach increments `ctx_pool_created`
  - Extended `OMNI_TEST_SUMMARY suite=fiber_temp_pool` output with:
    - `ctx_pools`
    - `lc_clone`
    - `lc_destroy`
    - `lc_defer`
    - `lc_flush`
- `src/stack_engine.c3`
  - Strengthened `test_stack_ctx_fiber_temp_clone_discard_stress()`:
    - under `OMNI_FIBER_TEMP=1`, now asserts deltas for:
      - lifecycle clone callbacks,
      - lifecycle destroy callbacks,
      - deferred-destroy events,
      - destroy-time chunk flushes,
      in addition to existing context-return activity.

### Why this matters
- Converts Fiber TEMP lifecycle behavior into directly testable telemetry.
- Increases confidence in clone-share correctness and destroy sequencing.
- Supports faster diagnosis if future regressions appear in suspend/clone/discard paths.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 19/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 18/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`)
- Flagged summary:
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 ...`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool ... ctx_pools=33 lc_clone=32 lc_destroy=65 lc_defer=32 lc_flush=37 ...`

## 2026-03-05: Session 152 - Cancellation/Timeout Boundary Stress (Fiber TEMP)

### Summary
Added targeted scheduler stress coverage for cancellation/timeout boundaries with Fiber TEMP enabled, including destroy-before-complete offload fiber scenarios.

### What changed
- `src/lisp/tests_tests.c3`
  - Added `run_scheduler_fiber_temp_cancel_timeout_boundary_tests(...)`:
    - repeats timeout-immediate, cancel+join, and timeout-success thread-task patterns,
    - under `OMNI_FIBER_TEMP=1`, asserts Fiber TEMP context counters (`ctx_take_hits`, `ctx_return_count`) remain unchanged for thread-only operations.
  - Added `run_scheduler_offload_cancel_boundary_tests(...)`:
    - repeats `spawn(offload 'sleep-ms ...)` + `fiber-cancel` + `run-fibers`,
    - validates destroy-before-complete style scheduler path remains stable.
  - Wired both into `run_scheduler_tests(...)`.

### Why this matters
- Directly exercises the cancellation/timeouts risk area in the Fiber TEMP roadmap.
- Confirms thread/offload boundaries do not leak context-local Fiber TEMP behavior where no stack context is active.
- Adds deterministic stress coverage for one of the trickiest lifecycle boundaries (cancel before completion).

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 19/0`, `Scope region 51/0`, `Unified 1182/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 18/0`, `Scope region 51/0`, `Unified 1181/0`, `Compiler 73/0`)
- Flagged summary:
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 ...`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_engine pass=19 fail=0`
    - `OMNI_TEST_SUMMARY suite=scope_region pass=51 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=66 misses=4 returns=109 drop_frees=0 pooled=6 peak=6 ctx_hits=33 ctx_returns=70 eligible_slow=2 bypass_large=0 bypass_escape=2`

## 2026-03-05: Session 151 - Scheduler Wakeup/Offload Interleaving Stress

### Summary
Added deterministic scheduler stress coverage that interleaves:
- async offload via fiber spawn/await,
- explicit wakeup queue enqueue/drain,
- worker-thread spawn/join.

This widens boundary-race coverage for Fiber TEMP rollout without introducing nondeterministic timing assertions.

### What changed
- `src/lisp/tests_tests.c3`
  - Added `run_scheduler_wakeup_offload_interleave_tests(...)`.
  - Runs 12 deterministic interleaving iterations and checks:
    - async offload completion success,
    - wakeup enqueue/drain success,
    - thread offload join success,
    - wakeup queue drained (`head == tail`) at the end.
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Exercises scheduler wakeup and offload interaction in one path, closer to real mixed workloads.
- Keeps assertions deterministic and stable across normal/ASAN runs.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 19/0`, `Scope region 51/0`, `Unified 1180/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 18/0`, `Scope region 51/0`, `Unified 1179/0`, `Compiler 73/0`)
- Flagged summary:
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 ...`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_engine pass=19 fail=0`
    - `OMNI_TEST_SUMMARY suite=scope_region pass=51 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=66 misses=4 returns=109 drop_frees=0 pooled=6 peak=6 ctx_hits=33 ctx_returns=70 eligible_slow=2 bypass_large=0 bypass_escape=2`

## 2026-03-05: Session 150 - Scheduler Fiber TEMP Thread-Boundary Coverage

### Summary
Added scheduler-side Fiber TEMP boundary coverage that specifically checks thread/offload operations do not mutate stack-context-local Fiber TEMP counters when no stack context is active.

### What changed
- `src/lisp/tests_tests.c3`
  - Added `run_scheduler_fiber_temp_thread_boundary_tests(...)`.
  - Under `OMNI_FIBER_TEMP=1`, captures `ctx_take_hits` / `ctx_return_count`, runs repeated `thread-spawn` + `thread-join` cycles, and asserts counters remain unchanged.
  - Wired test into `run_scheduler_tests(...)`.
  - Kept existing mixed scheduler boundary stress semantics unchanged (no incorrect “ctx counters must stay constant” constraint there).

### Why this matters
- Covers the critical boundary guarantee with a precise invariant:
  - worker/offload thread paths without stack contexts must not touch Fiber TEMP context caches.
- Avoids over-constraining mixed scheduler stress that intentionally includes stack-context activity.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 19/0`, `Scope region 51/0`, `Unified 1179/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 18/0`, `Scope region 51/0`, `Unified 1178/0`, `Compiler 73/0`)
- Flagged summary:
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 ...`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_engine pass=19 fail=0`
    - `OMNI_TEST_SUMMARY suite=scope_region pass=51 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=66 misses=4 returns=109 drop_frees=0 pooled=6 peak=6 ctx_hits=33 ctx_returns=70 eligible_slow=2 bypass_large=0 bypass_escape=2`

## 2026-03-05: Session 149 - Fiber TEMP Thread/Offload Boundary Guard Test

### Summary
Added scheduler-level boundary coverage to ensure Fiber TEMP context-cache metrics remain stack-context-local during thread/offload operations that run outside stack contexts.

### What changed
- `src/lisp/tests_tests.c3`
  - Added `run_scheduler_fiber_temp_thread_boundary_tests(...)`.
  - Under `OMNI_FIBER_TEMP=1`, the test:
    - captures `ctx_take_hits` / `ctx_return_count`,
    - runs repeated `thread-spawn` + `thread-join` offload work,
    - asserts context-cache counters do not change (no stack context involvement).
  - Wired into `run_scheduler_tests(...)`.

### Why this matters
- Validates the intended ownership boundary: Fiber TEMP per-context caches are tied to stack contexts, not generic worker/offload thread activity.
- Adds explicit regression coverage for the cross-thread boundary concern.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 19/0`, `Scope region 51/0`, `Unified 1179/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 18/0`, `Scope region 51/0`, `Unified 1178/0`, `Compiler 73/0`)
- Flagged summary:
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 ...`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_engine pass=19 fail=0`
    - `OMNI_TEST_SUMMARY suite=scope_region pass=51 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=66 misses=4 returns=109 drop_frees=0 pooled=6 peak=6 ctx_hits=33 ctx_returns=70 eligible_slow=2 bypass_large=0 bypass_escape=2`

## 2026-03-05: Session 148 - Fiber TEMP Clone/Discard Stress Coverage

### Summary
Added targeted stack-engine stress coverage for Fiber TEMP across suspended-context clone/discard cycles, validating lifecycle-backed per-context cache behavior under repeated multi-shot patterns.

### What changed
- `src/stack_engine.c3`
  - Added `test_entry_scope_yield_once(...)` to exercise scope create/alloc/release on both sides of a suspend point.
  - Added `test_stack_ctx_fiber_temp_clone_discard_stress()`:
    - repeats clone/discard on suspended contexts,
    - resumes original context to completion,
    - asserts no corruption/regression in repeated cycles,
    - under `OMNI_FIBER_TEMP=1`, asserts per-context return-path activity (`ctx_return_count` delta).
  - Wired new test into `run_stack_engine_tests()`.

### Why this matters
- Raises confidence in Fiber TEMP suspend/clone lifecycle behavior before deeper rollout phases.
- Specifically targets the risk surface that previously regressed when per-context state was attached to the wrong callback channel.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 19/0`, `Scope region 51/0`, `Unified 1178/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 18/0`, `Scope region 51/0`, `Unified 1177/0`, `Compiler 73/0`)
- Flagged summary:
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 ...`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_engine pass=19 fail=0`
    - `OMNI_TEST_SUMMARY suite=scope_region pass=51 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=66 misses=4 returns=109 drop_frees=0 pooled=6 peak=6 ctx_hits=33 ctx_returns=70 eligible_slow=2 bypass_large=0 bypass_escape=2`

## 2026-03-05: Session 147 - Fiber TEMP Per-Context Cache via Lifecycle Hooks

### Summary
Integrated Fiber TEMP per-`StackCtx` chunk caching using the new lifecycle channel (not the LIFO defer stack), preserving suspend/undefer semantics while improving ownership direction for Fiber TEMP.

### What changed
- `src/scope_region.c3`
  - Added `FiberTempCtxPool` (per-context TEMP chunk cache state with refcount for clone sharing).
  - Added lifecycle-backed helpers:
    - `fiber_temp_ctx_pool_find_current()`
    - `fiber_temp_ctx_pool_get_or_create()`
    - `fiber_temp_ctx_pool_on_clone(...)`
    - `fiber_temp_ctx_pool_on_destroy(...)`
  - Added global fallback helper:
    - `scope_chunk_reclaim_temp_global(...)`
  - `fiber_temp_chunk_try_take(...)` now attempts per-context cache first, then global pool.
  - `scope_chunk_reclaim_temp(...)` now returns chunks to per-context cache first, then global pool.
  - Extended Fiber TEMP summary metrics:
    - `ctx_hits`
    - `ctx_returns`
- `src/stack_engine.c3`
  - Extended `test_stack_ctx_scope_create_in_context()` state checks to assert per-context return-path exercise under `OMNI_FIBER_TEMP=1` (`ctx_returns` delta).

### Why this matters
- Moves Fiber TEMP ownership closer to fiber/context lifetimes without reintroducing stack-layer scope coupling.
- Avoids the earlier correctness hazard of dynamic non-LIFO defer registrations.
- Keeps fallback behavior safe: global pool remains the lower-priority path.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 18/0`, `Scope region 51/0`, `Unified 1178/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 17/0`, `Scope region 51/0`, `Unified 1177/0`, `Compiler 73/0`)
- Flagged summary:
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 ...`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_engine pass=18 fail=0`
    - `OMNI_TEST_SUMMARY suite=scope_region pass=51 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=2 misses=4 returns=13 drop_frees=0 pooled=6 peak=6 ctx_hits=1 ctx_returns=6 eligible_slow=2 bypass_large=0 bypass_escape=2`

## 2026-03-05: Session 146 - StackCtx Lifecycle Substrate (Defer-Independent)

### Summary
Added a new generic `StackCtx` lifecycle callback channel, separate from LIFO defer entries, to support persistent per-context resources without interfering with `stack_ctx_undefer(...)` semantics.

This was introduced after identifying that dynamic defer registration from non-LIFO resource paths can conflict with call-site expectations that `stack_ctx_undefer(...)` pops the latest suspend guard.

### What changed
- `src/stack_engine.c3`
  - Added lifecycle storage to `StackCtx`:
    - `lifecycle_inline`, `lifecycle_heap`, `lifecycle_count`, `lifecycle_capacity`.
  - Added lifecycle APIs:
    - `stack_ctx_lifecycle_attach(...)`
    - `stack_ctx_find_lifecycle_arg(...)`
    - lifecycle reserve/clone/destroy/clear helpers.
  - `stack_ctx_destroy(...)` now runs lifecycle callbacks after defer callbacks.
  - `stack_ctx_clone(...)` now clones lifecycle entries and invokes lifecycle clone hooks.
  - `stack_pool_shutdown(...)` now frees lifecycle overflow storage.
  - Added tests:
    - `test_stack_ctx_lifecycle_destroy_isolation()`
    - `test_stack_ctx_lifecycle_clone_hook()`
  - `run_stack_engine_tests()` now includes both lifecycle tests.

### Why this matters
- Preserves stack-engine genericity while adding the right primitive for persistent context-owned resources.
- Prevents correctness regressions caused by mixing non-LIFO resources into the LIFO defer stack.
- Unblocks safer Fiber TEMP per-context ownership work in subsequent sessions.

### Validation
- Normal:
  - `c3c build`
  - `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 18/0`, `Scope region 51/0`, `Unified 1178/0`, `Compiler 73/0`)
- ASAN strict:
  - `c3c clean && c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: pass (`Stack engine 17/0`, `Scope region 51/0`, `Unified 1177/0`, `Compiler 73/0`)
- Flagged summary:
  - `OMNI_FIBER_TEMP=1 OMNI_TEST_SUMMARY=1 ...`
  - Result includes:
    - `OMNI_TEST_SUMMARY suite=stack_engine pass=18 fail=0`
    - `OMNI_TEST_SUMMARY suite=scope_region pass=51 fail=0`
    - `OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=1 hits=2 misses=4 returns=8 drop_frees=0 pooled=6 peak=6 eligible_slow=2 bypass_large=0 bypass_escape=2`

## 2026-03-05: Session 145 - Fiber TEMP Pool Invariant Tests

### Summary
Added focused Fiber TEMP pool invariant tests in `scope_region` to lock take/reclaim behavior with order-insensitive local deltas.

### What changed
- `src/scope_region.c3`
  - Added test block `Test 16: Fiber TEMP pool invariants (flagged only)` covering:
    - reclaim behavior (`return_count` vs `drop_frees`),
