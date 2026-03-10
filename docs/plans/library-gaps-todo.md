# Library Integration TODO (libuv + Core Native Libraries)

Status: `complete`  
As of: 2026-03-09

Refactor tracking note:
- Compiler/parser modularization follow-up is now tracked in
  `docs/plans/compiler-parser-refactor-plan.md`.
- This file remains as completed integration/refactor history and is not the
  active tracker for compiler/parser split work.

Goal: drive Omni toward full async runtime integration on libuv, then complete
high-impact parity integrations for BearSSL, libdeflate, utf8proc, yyjson, and
LMDB/Deduce.

Concurrency surface target:

- Fibers remain Omni's full structured async/runtime effect model.
- `offload` remains the immediate CPU-bound offload surface.
- Pool-backed background jobs are a separate concept and should use `task-*`
  naming, not `thread-*`.
- `thread-*` should mean dedicated OS-thread lifecycle per spawn/join/cancel.
- `uv_queue_work` is the target substrate for pool-backed task execution, not
  for true OS-thread identity.

Async boundary policy target (hard-error mode):

- Effects intended as async runtime I/O (`io/tcp-*`, `io/http-*`, async file
  effects) must execute through fiber/libuv boundaries, not direct blocking I/O.
- Outside a running fiber context, these effects must raise a deterministic
  `fiber-required` error instead of silently running blocking fallback code.
- In fiber context, backend-init failure must raise deterministic runtime errors;
  do not auto-switch to blocking/offload behavior.
- Do not introduce new backend fallback branches in async primitives.
- Safety cleanup fallbacks are allowed only for teardown/resource-release
  operations and must not perform user-visible I/O.

Current parity snapshot (implementation + tests):

| Library | Status | Notes |
|---|---|---|
| libuv | Complete | Core tracks A1-A6 are landed and parity map is closed (`done-libuv: 38/38`, `partial-libuv: 0/38`, `non-libuv: 0/38`). |
| BearSSL | Complete | Track B complete. |
| libdeflate | Complete | Track C complete. |
| utf8proc | Complete | Track D complete. |
| yyjson | Baseline/advanced complete | `json-parse` / `json-emit` / `json-emit-pretty` are integrated; writer flags, precision controls, and parse-option matrix are complete. |
| LMDB / Deduce | Baseline/advanced complete | Unified `(deduce '...)` surface is integrated and tested, including explicit tx/range/db-management APIs. |
| Data formats (TOML) | Baseline/advanced complete | `toml-parse` now integrated via `tomlc17`, with timestamp/string/array/table conversion and parser option surface. |
| Data formats (CSV) | Baseline/advanced complete | `csv-parse` and `csv-emit` support delimiter/line-ending/quote options with explicit strict toggles. |

## Definition of Done: "100% libuv"

`100% libuv` means all Omni async I/O effect paths route through libuv-backed
runtime boundaries and scheduler resume points, with no blocking syscalls in
fiber execution paths.

Completion gates:

- [x] `io/async-sleep` uses `uv_timer` in async path and no longer blocks.
- [x] `io/dns-resolve` async path uses `uv_getaddrinfo`.
- [x] File operations used by Omni effects have `uv_fs_*` paths.
- [x] Stream/socket async wakeups are libuv-driven and fiber-safe.
- [x] Unix pipes, process, and signal paths have libuv-backed APIs.
- [x] Boundary and memory ownership invariants remain RC/scope-region primary.
- [x] ASAN + full test suite + boundary hardening script pass.

## Single Execution Path (Ordered)

Follow this sequence strictly. Do not start `N+1` until `N` exit criteria are met.

### 1) Core socket parity (`io/tcp-*`)
- [x] Implement libuv-native `io/tcp-connect` path (`uv_tcp_connect`) for fiber context.
- [x] Implement libuv-native `io/tcp-listen` path (`uv_tcp_bind`/`uv_listen`) for fiber context.
- [x] Implement libuv-native `io/tcp-accept` path (`uv_accept`) for fiber context.
- [x] Implement libuv-native `io/tcp-close` path aligned with libuv handle lifecycle in fiber context.
- [x] Remove fiber-path backend fallback branches (no offload/blocking failover from `tcp-*` async path; teardown-only close safety fallback allowed).
- [x] Add deterministic hard errors for non-fiber `io/tcp-*` calls (`fiber-required` policy).
- [x] Update backend map rows for `io/tcp-connect|listen|accept|close`.
- [x] Add/extend regression tests for connect/listen/accept/close across success/error/cancel paths.
- [x] Exit criteria: `io/tcp-*` fiber path is libuv-only and non-fiber path is explicit hard error (no behavioral fallback).

### 2) HTTP on top of libuv socket path
- [x] Rewire `io/http-get` to use the new libuv TCP path in fiber context.
- [x] Rewire `io/http-request` to use the new libuv TCP path in fiber context.
- [x] Replace non-fiber blocking path with deterministic hard error (`fiber-required`) for `io/http-get`/`io/http-request`.
- [x] Update backend map rows for `io/http-get` and `io/http-request`.
- [x] Add/extend HTTP fiber-path regressions (success, DNS failure, connection failure) and non-fiber hard-error behavior.
- [x] Exit criteria: HTTP paths are libuv/fiber-only and do not silently downgrade to blocking behavior.

### 3) File ops: from sync `uv_fs_*` wrappers to fiber-safe async behavior
- [x] Add async/fiber-safe path for `io/read-file` (no blocking syscall in fiber context).
- [x] Add async/fiber-safe path for `io/write-file`.
- [x] Add async/fiber-safe path for `io/read-lines`.
- [x] Add async/fiber-safe path for `io/file-exists?`.
- [x] Replace non-fiber sync fallback with deterministic hard error (`fiber-required`) for these async file effects.
- [x] Add/extend regressions for large payloads, cancellation, and error propagation.
- [x] Exit criteria: no blocking syscall in fiber context and no non-fiber behavioral fallback for these file effects.

### 4) Split pooled tasks from real threads
- [x] Rename current pool-backed task surface away from `thread-*`:
  - [x] add new raw primitives for pooled tasks (`__raw-task-spawn`, `__raw-task-join`, `__raw-task-join-timeout`, `__raw-task-cancel`)
  - [x] add new effects (`io/task-spawn`, `io/task-join`, `io/task-join-timeout`, `io/task-cancel`)
  - [x] add stdlib wrappers (`task-spawn`, `task-join`, `task-join-timeout`, `task-cancel`)
  - [x] migrate docs/tests/runtime call sites off the misleading `thread-*` name
  - [x] remove compatibility aliases; keep one canonical pooled-task name
  - [x] replace exposed numeric task ids with opaque `task-handle` values (generation-validated)
- [x] Introduce real OS-thread primitives under `thread-*`:
  - [x] define thread handle/task-id model and ownership rules
  - [x] implement `io/thread-spawn` as dedicated OS-thread creation
  - [x] implement `io/thread-join`
  - [x] implement `io/thread-join-timeout`
  - [x] implement `io/thread-cancel` with explicit semantics/limits
  - [x] add sendable boundary tests for thread return/join/cancel/destroy
- [x] Migrate pooled task backend to `uv_queue_work`:
  - [x] replace the custom pooled queue ring/worker-thread substrate entirely
  - [x] route both `io/offload` and `io/task-*` through `uv_queue_work`
  - [x] keep Omni-owned state objects:
    - [x] `OffloadWork`
    - [x] `OffloadCompletion`
    - [x] `PendingOffload`
    - [x] current task table model (`ThreadTaskEntry` -> `TaskEntry` rename when split lands)
  - [x] add a task completion wait primitive:
    - [x] shared condition variable or equivalent wait path for `task-join`
    - [x] event-driven fiber wake path for no-timeout `task-join` / `thread-join` (no 1ms sleep polling in indefinite join path)
    - [x] event-driven fiber timeout path for `task-join-timeout` / `thread-join-timeout` (single libuv timer + completion wake race, no 1ms sleep polling)
    - [x] remove busy-wait/sleep polling from join-timeout paths
    - [x] guard queued callback completion by task generation to prevent recycled-slot races
  - [x] map cancellation explicitly:
    - [x] pending task -> `uv_cancel`
    - [x] running task -> cooperative cancel flag + cancelled completion
  - [x] delete custom queue machinery once parity is green:
    - [x] queue ring storage
    - [x] queue mutex/condition variable
    - [x] dedicated worker threads
    - [x] queue-full error path tied only to local ring capacity
  - [x] keep explicit admission/backpressure policy small and documented:
    - [x] bounded in-flight/pending cap
    - [x] stable error code on saturation
    - [x] docs for fairness/backpressure expectations
- [x] Reclassify affected rows:
  - [x] `io/offload`
  - [x] `io/task-*`
  - [x] `io/thread-*`
  - [x] `io/process-*`
- [x] Exit criteria:
  - [x] pooled background jobs are no longer exposed as `thread-*`
  - [x] `thread-*` means real OS threads
  - [x] pool-backed jobs run on `uv_queue_work`
  - [x] status is no longer `partial-libuv` due to naming/policy ambiguity

### 5) Close advanced-parity status rows
- [x] LMDB/Deduce advanced parity status normalized to complete.
- [x] CSV advanced parity status normalized to complete.
- [x] Exit criteria: top parity table has no `advanced parity pending` rows.

### 6) Enforcement gate
- [x] Add CI/parity gate to fail if new `io/*` primitives bypass boundary facade paths.
- [x] Add CI/parity gate to fail on status-map regression (`done/partial/non-libuv` drift without doc update).
- [x] Add CI/policy gate to reject new async-primitive backend fallback branches (`blocking`/`offload` fallback in fiber paths).
- [x] Keep async fallback policy guard resilient to largest-first module splits by resolving function locations dynamically instead of hardcoding single source files.
- [x] Keep required validation commands mandatory for touched slices:
  - [x] `c3c build`
  - [x] `c3c build --sanitize=address`
  - [x] full suite normal run
  - [x] full suite ASAN run
- [x] Exit criteria: parity + boundary-policy drift is blocked by CI.
- Note: scheduler boundary flake pair (`scheduler worker cancel interleave boundary restore`, `scheduler timeout-then-join boundary restore`) was stabilized in Session 51 with bounded enqueue retry/pump wiring in boundary tests and additional loop pumping in task-completion polling; post-fix validation includes a 12-run full-suite stress pass plus full-suite ASAN pass.

## Codebase Modularization Path (Largest-First)

Goal: reduce cognitive load and merge friction by splitting oversized source
files into coherent modules while preserving behavior.

Rules:
- Always pick the largest eligible file first.
- Prefer extraction-only moves (no behavioral change in same slice).
- Keep one owning top-level orchestrator entry point per domain.
- Validate after each split (`c3c build` + targeted suite, then full suite cadence).

Current largest-first queue (source/runtime):
- [x] `csrc/uv_helpers.c` split into `tcp`, `pipe`, `process`, `signal` helper units.
- [x] `src/lisp/primitives_data_formats.c3` → split CSV primitives/options into dedicated module file.
- [x] `src/lisp/tests_deduce_groups.c3` → split durability/query/isolation suites.
- [x] `src/stack_engine.c3` → split remaining runtime helpers from core stack execution paths.
- [x] `src/pika/regex.c3` → continue split tokenizer/compiler cores after cache/API extraction.
- [x] `src/stack_engine_tests.c3` → split defer/clone/update clusters into dedicated test modules.
- [x] `src/lisp/tests_runtime_feature_groups.c3` → split by jit/pika/http/schema/reader domains after data/unicode extraction.
- [x] `src/scope_region.c3` → split retention/release/adopt boundary helpers after test extraction.
- [x] `src/pika/lisp_pika.c3` → split grammar compiler APIs from parse/interop glue after test/regex extraction.
- [x] `src/lisp/scheduler_state_offload.c3` → split shared-handle/sendable state pipeline from offload-state wiring.
- [x] `src/lisp/tests_advanced_tests.c3` → continue split core/unicode/macro domains after stdlib/module extraction.
- [x] `src/lisp/tests_scheduler_groups.c3` → split integration clusters by boundary/io/fairness domains.
- [x] `src/lisp/tests_compiler_tests.c3` → split compiler/JIT clusters by feature domains.
- [x] `src/lisp/scheduler_wakeup_io.c3` → split fd/io wakeup map management from wakeup dispatch.
- [x] `src/lisp/tests_advanced_type_effect_ffi_groups.c3` → split type/dispatch/effect/ffi clusters into narrower domain files.
- [x] `src/lisp/scheduler_primitives_tasks.c3` → split pooled-task orchestration from dedicated thread lifecycle helpers (dedicated thread lifecycle extracted to `src/lisp/scheduler_primitives_threads.c3`).

Execution now:
- [x] `src/lisp/tests_tests.c3` first slice complete: scheduler test block extracted to `src/lisp/tests_scheduler_groups.c3`.
- [x] `src/lisp/tests_tests.c3` second slice complete: deduce test block extracted to `src/lisp/tests_deduce_groups.c3`.
- [x] `src/lisp/tests_tests.c3` third slice complete: memory-lifetime block extracted to `src/lisp/tests_memory_lifetime_groups.c3`.
- [x] `src/lisp/tests_tests.c3` fourth slice complete: basic/core groups extracted to `src/lisp/tests_core_groups.c3`.
- [x] `src/lisp/tests_tests.c3` fifth slice complete: runtime feature groups extracted to `src/lisp/tests_runtime_feature_groups.c3`.
- [x] `src/lisp/tests_advanced_tests.c3` first slice complete: type/dispatch/effect/ffi groups extracted to `src/lisp/tests_advanced_type_effect_ffi_groups.c3`.
- [x] `src/stack_engine.c3` first slice complete: `SECTION 11: TESTS` extracted to `src/stack_engine_tests.c3`.
- [x] `src/lisp/tests_scheduler_groups.c3` first slice complete: boundary suites extracted to `src/lisp/tests_scheduler_boundary_groups.c3`.
- [x] `src/lisp/async.c3` first slice complete: process/signal/dns primitives extracted to `src/lisp/async_process_signal_dns.c3`.
- [x] `src/lisp/async.c3` second slice complete: udp/pipe primitives extracted to `src/lisp/async_udp_pipe.c3`.
- [x] `src/lisp/tests_runtime_feature_groups.c3` first slice complete: async tests extracted to `src/lisp/tests_runtime_async_groups.c3`.
- [x] `src/pika/regex.c3` first slice complete: cache/compile/public API extracted to `src/pika/regex_cache_api.c3`.
- [x] `src/lisp/tests_advanced_tests.c3` second slice complete: collections/module/stdlib groups extracted to `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- [x] `src/pika/lisp_pika.c3` first slice complete: local test helpers extracted to `src/pika/lisp_pika_tests.c3`.
- [x] `src/scope_region.c3` first slice complete: unit tests extracted to `src/scope_region_tests.c3`.
- [x] `src/lisp/tests_memory_lifetime_groups.c3` second slice complete: env-copy/tco/promotion-context suites extracted to `src/lisp/tests_memory_lifetime_env_tco_promotion_groups.c3` (later split into dedicated domain files).
- [x] `src/lisp/tests_runtime_feature_groups.c3` second slice complete: json/data/compression/unicode suites extracted to `src/lisp/tests_runtime_data_unicode_groups.c3`.
- [x] `src/pika/lisp_pika.c3` second slice complete: regex primitives extracted to `src/pika/lisp_pika_regex_primitives.c3`.
- [x] `src/lisp/scheduler_primitives.c3` first slice complete: task/thread orchestration extracted to `src/lisp/scheduler_primitives_tasks.c3` (fiber/run-loop orchestrator retained in source file).
- [x] `src/lisp/tests_scheduler_boundary_groups.c3` first slice complete: wakeup/offload boundary block extracted to `src/lisp/tests_scheduler_boundary_wakeup_offload_groups.c3`.
- [x] `src/lisp/tests_tests.c3` sixth slice complete: E2E compiler-generation block extracted to `src/lisp/tests_e2e_generation.c3`.
- [x] `src/lisp/async.c3` third slice complete: tcp transport helpers/primitives extracted to `src/lisp/async_tcp_transport_core.c3`.
- [x] `src/lisp/tests_deduce_groups.c3` split by domain:
  - [x] durability slice extracted to `src/lisp/tests_deduce_durability_groups.c3`
  - [x] query slice extracted to `src/lisp/tests_deduce_query_groups.c3`
  - [x] isolation slice extracted to `src/lisp/tests_deduce_isolation_groups.c3`
  - [x] orchestrator + shared helpers retained in `src/lisp/tests_deduce_groups.c3`
- [x] `src/stack_engine.c3` second slice complete: defer/lifecycle + create/destroy cluster extracted to `src/stack_engine_lifecycle.c3` (core switch/init/clone paths retained in source file).
- [x] `src/pika/regex.c3` second slice complete: tokenizer/token model extracted to `src/pika/regex_tokenizer.c3` (compiler/interpreter core retained in source file).
- [x] `src/stack_engine_tests.c3` first slice complete: defer/lifecycle test cluster extracted to `src/stack_engine_tests_defer_lifecycle.c3` (runner + core stack test fixtures retained in source file).
- [x] `src/lisp/primitives_data_formats.c3` first slice complete: CSV parser/emitter + option parsing extracted to `src/lisp/primitives_data_formats_csv.c3` (TOML/time primitives retained in source file).
- [x] `src/lisp/tests_runtime_feature_groups.c3` third slice complete: `jit`, `pika`, `http`, `schema`, `reader` domains extracted into dedicated group files.
- [x] `src/scope_region.c3` second slice complete: fiber-temp pool/stats and reset/adopt/cleanup helpers extracted to `src/scope_region_temp_pool.c3` and `src/scope_region_reset_adopt.c3`.
- [x] `src/pika/lisp_pika.c3` third slice complete: grammar compiler + `pika/grammar` construction path extracted to `src/pika/lisp_pika_grammar_compiler.c3`.
- [x] `src/lisp/scheduler_state_offload.c3` first slice complete: shared-handle registry/sendable projection pipeline extracted to `src/lisp/scheduler_state_shared_handles.c3` (offload-state build/init flow retained in source file).
- [x] `src/lisp/tests_advanced_tests.c3` third slice complete: core/unicode/runtime/lambda/binding groups extracted to `src/lisp/tests_advanced_core_unicode_groups.c3`, macro-hygiene groups extracted to `src/lisp/tests_advanced_macro_hygiene_groups.c3`, and shared builders + runner retained in `src/lisp/tests_advanced_tests.c3`.
- [x] `src/lisp/tests_scheduler_groups.c3` second slice complete: offload/task/thread/cancel/fairness clusters extracted to `src/lisp/tests_scheduler_io_task_groups.c3` (spawn/await/wakeup orchestration retained in `src/lisp/tests_scheduler_groups.c3`).
- [x] `src/lisp/tests_compiler_tests.c3` slice complete: compiler groups extracted to `src/lisp/tests_compiler_core_groups.c3`, compiler codegen/primitive groups extracted to `src/lisp/tests_compiler_codegen_groups.c3`, and JIT/print groups extracted to `src/lisp/tests_compiler_jit_print_groups.c3`; shared helpers + orchestrator retained in `src/lisp/tests_compiler_tests.c3`.
- [x] `src/lisp/scheduler_wakeup_io.c3` slice complete: wakeup dispatch, reliable wakeup queue plumbing, libuv wakeup callbacks, and `drain_wakeups` extracted to `src/lisp/scheduler_wakeup_dispatch.c3`; pending close/complete + consume paths retained in `src/lisp/scheduler_wakeup_io.c3`.
- [x] `src/lisp/tests_advanced_type_effect_ffi_groups.c3` slice complete: io/effect/union/ffi groups extracted to `src/lisp/tests_advanced_io_effect_ffi_groups.c3`; type/dispatch/mutation/consolidation groups retained in `src/lisp/tests_advanced_type_effect_ffi_groups.c3`.
- [x] `src/lisp/scheduler_primitives_tasks.c3` slice complete: dedicated thread lifecycle extracted to `src/lisp/scheduler_primitives_threads.c3`; pooled task orchestration retained in `src/lisp/scheduler_primitives_tasks.c3`.
- [x] `src/lisp/tests_memory_lifetime_env_tco_promotion_groups.c3` slice complete: split into `src/lisp/tests_memory_lifetime_env_copy_groups.c3`, `src/lisp/tests_memory_lifetime_tco_budget_groups.c3`, and `src/lisp/tests_memory_lifetime_promotion_context_groups.c3`.
- [x] `src/lisp/primitives_data_formats_csv.c3` slice complete: CSV parse pipeline extracted to `src/lisp/primitives_data_formats_csv_parse.c3`, CSV emit pipeline extracted to `src/lisp/primitives_data_formats_csv_emit.c3`, and option-normalization/TOML-option plumbing retained in `src/lisp/primitives_data_formats_csv.c3`.
- [x] `src/lisp/json.c3` slice complete: JSON emit conversion/serialization path extracted to `src/lisp/json_emit.c3`, while parse/options/json-pointer lookup remain in `src/lisp/json.c3`.
- [x] `src/lisp/prim_io.c3` slice complete: libc/system primitives (`shell`, `getenv`, `time*`, `exit`, `sleep`, `display`) extracted to `src/lisp/prim_system.c3`, while core I/O and libuv file primitives remain in `src/lisp/prim_io.c3`.
- [x] `src/lisp/async.c3` slice complete: process/signal handle lifecycle and process argument/ffi helper pipeline extracted to `src/lisp/async_process_signal_handles.c3`, while async core declarations/transport wrappers and `async-sleep` runtime bridge remain in `src/lisp/async.c3`.
- [x] `src/lisp/tests_e2e_generation.c3` slice complete: extracted setup fixtures to `src/lisp/tests_e2e_generation_setups.c3`, split scenarios into `src/lisp/tests_e2e_generation_cases_core.c3` and `src/lisp/tests_e2e_generation_cases_extended.c3`, and retained generation orchestration/helpers in `src/lisp/tests_e2e_generation.c3`.
- [x] `src/lisp/eval_pattern_matching.c3` slice complete: extracted support utilities (`MatchResult`, gensym table, `values_equal`, sequence/list helpers) to `src/lisp/eval_pattern_support.c3`, while retaining runtime pattern dispatch and sequence matching in `src/lisp/eval_pattern_matching.c3`.
- [x] `src/lisp/eval_repl.c3` slice complete: extracted REPL syntax-highlighting/completion callback logic and callback state/constants to `src/lisp/eval_repl_callbacks.c3`, while retaining replxx FFI/session lifecycle and eval-loop control flow in `src/lisp/eval_repl.c3`.
- [x] `src/lisp/tests_memory_lifetime_groups.c3` slice complete: extracted boundary-state/run_program/wrapper-copy regression cluster into `src/lisp/tests_memory_lifetime_boundary_groups.c3`, while retaining lane/root fallback orchestration and remaining lifetime groups in `src/lisp/tests_memory_lifetime_groups.c3`.
- [x] `src/lisp/tests_scheduler_boundary_groups.c3` slice complete: extracted thread/task-focused boundary regressions (stress, completion, cancel, mixed-state restore) to `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`, while retaining shared boundary snapshot helpers and wakeup/offload boundary suites in `src/lisp/tests_scheduler_boundary_groups.c3`.
- [x] `src/lisp/tests_core_groups.c3` slice complete: extracted let/function-parameter destructuring suites into `src/lisp/tests_core_destructure_groups.c3`, while retaining numeric/list/control/error/runtime guard groups in `src/lisp/tests_core_groups.c3`.
- [x] `src/lisp/macros_expansion.c3` slice complete: extracted macro definition/hygiene capture helpers to `src/lisp/macros_define_hygiene.c3` and template expansion/splice helpers to `src/lisp/macros_template_expansion.c3`, while retaining macro registry/expansion traversal and module lookup in `src/lisp/macros_expansion.c3`.
- [x] `src/lisp/jit_jit_module_import.c3` slice complete: extracted non-module JIT dispatch helpers (index/path/match/type/ffi wrappers and call-name tracking) into `src/lisp/jit_jit_dispatch_helpers.c3`, while retaining module/import path resolution, module table registration, and file import/eval flow in `src/lisp/jit_jit_module_import.c3`.
- [x] `src/lisp/tests_scheduler_boundary_wakeup_offload_groups.c3` slice complete: extracted offload payload ownership/cleanup and duplicate/consume regressions into `src/lisp/tests_scheduler_boundary_offload_payload_groups.c3`, while retaining wakeup queue/event-order/observability boundary regressions in `src/lisp/tests_scheduler_boundary_wakeup_offload_groups.c3`.
- [x] `src/lisp/deduce_relation_ops.c3` slice complete: extracted relation scan/range kernel helpers (`deduce_compare_*`, bound-tuple parse, cursor range scan/all) into `src/lisp/deduce_relation_scan_helpers.c3`, while retaining fact/retract/clear/drop primitives and count/scan entrypoint wiring in `src/lisp/deduce_relation_ops.c3`.
- [x] `src/lisp/scheduler_state_offload.c3` slice complete: extracted scheduler constants/enums/state structs into `src/lisp/scheduler_state_types.c3`, while retaining scheduler globals, offload work-build/error/result helpers, wakeup reset helpers, and scheduler initialization flow in `src/lisp/scheduler_state_offload.c3`.
- [x] `src/lisp/tests_advanced_stdlib_module_groups.c3` slice complete: extracted stdlib/numeric/math/array/set/string/predicate/format/introspection suites into `src/lisp/tests_advanced_stdlib_numeric_groups.c3`, while retaining collections/module/parser import-export boundary suites and orchestrators in `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- [x] `src/lisp/jit_jit_closure_define_qq.c3` slice complete: extracted JIT define/method-table dispatch helpers (`jit_eval_define*`, method-table creation/append/signature compare) into `src/lisp/jit_jit_define_method_table.c3`, while retaining TCO, closure construction, let/set path, and env-extension helpers in `src/lisp/jit_jit_closure_define_qq.c3`.
- [x] `src/lisp/tests_tests.c3` slice complete: extracted consolidated fixture/assertion helpers (`setup`, `test_*`, numeric compare helpers, and shared test utility helpers) into `src/lisp/tests_harness_helpers.c3`, while retaining top-level suite registration/orchestration and global boundary reset flow in `src/lisp/tests_tests.c3`.
- [x] `src/lisp/value_interp_state.c3` slice complete: extracted interpreter table-growth + teardown lifecycle methods (`Interp.grow_*`, `Interp.destroy`) into `src/lisp/value_interp_lifecycle.c3`, while retaining interpreter state struct, init wiring, frame guards, and allocator APIs in `src/lisp/value_interp_state.c3`.
- [x] `src/lisp/prim_io.c3` slice complete: extracted libuv fs-handle primitives (`fs-open/read/write/close/stat/readdir/rename/unlink` plus fs-handle lifetime helpers) into `src/lisp/prim_io_fs_handles.c3`, while retaining core print/newline + async file/read-lines + `load` pipeline in `src/lisp/prim_io.c3`.
- [x] `src/stack_engine_tests.c3` second slice complete: extracted thread-affinity probe + fiber-temp scope test cluster (`test_stack_ctx_thread_affinity_state`, `test_stack_ctx_scope_create_in_context`, clone/discard stress, retention guard, and affinity violation probe) into `src/stack_engine_tests_affinity_fiber_temp.c3`, while retaining core coroutine/overflow/FPU/clone runner orchestration in `src/stack_engine_tests.c3`.
- [x] `src/stack_engine.c3` third slice complete: extracted stack-pool ownership/thread-affinity subsystem (`StackPool`, owner-token helpers, `stack_pool_init`, `stack_pool_shutdown`) into `src/stack_engine_pool_ownership.c3`, while retaining stack context switch/init/suspend/resume/clone runtime core in `src/stack_engine.c3`.
- [x] `src/entry.c3` slice complete: extracted project scaffolding + FFI bind pipeline (`--init`/`--bind` helpers and command handlers) into `src/entry_project_init_bind.c3`, while retaining compile/repl/script/test/probe dispatch + `main` routing in `src/entry.c3`.
- [x] `src/stack_engine.c3` fourth slice complete: extracted coroutine bootstrap/ASAN switch bridge (`g_stack_ctx_bootstrap`, `g_main_asan_fake_stack`, `stack_ctx_bottom`, `asan_switch_out`, `asan_switch_in`, `stack_ctx_trampoline`) into `src/stack_engine_bootstrap_switch.c3`, while retaining stack region allocation, context init/suspend/resume, and clone core in `src/stack_engine.c3`.
- [x] `src/stack_engine.c3` fifth slice complete: extracted diagnostics/summary subsystem (`g_stack_quiet_output`, stack defer metrics counters/reset, summary emitters, `stack_print_pass`) into `src/stack_engine_diagnostics.c3`, while retaining stack region/runtime core and context execution flow in `src/stack_engine.c3`.
- [x] `src/stack_engine.c3` sixth slice complete: extracted stack region allocator/membership subsystem (`StackRegion`, stack-size constants, `stack_runtime_asan_enabled`, `stack_current_available_bytes`, `stack_has_apply_headroom`, `stack_region_alloc/free/contains`) into `src/stack_engine_region.c3`, while retaining stack context init/suspend/resume/clone runtime flow in `src/stack_engine.c3`.
- [x] `src/scope_region.c3` slice complete: extracted bump allocator subsystem (`ScopeRegion.alloc_slow`, `alloc_escape_slow`, `alloc`, `alloc_escape`) into `src/scope_region_allocators.c3`, while retaining scope ownership/refcount/lifecycle and chunk/destructor wiring in `src/scope_region.c3`.
- [x] `src/stack_engine_tests.c3` third slice complete: extracted coroutine runtime test cluster (context entries, suspend/resume/interleaving, overflow recovery, FPU preservation, clone multi-shot) into `src/stack_engine_tests_coroutines.c3`, while retaining stack region/pool smoke tests and `run_stack_engine_tests` orchestration in `src/stack_engine_tests.c3`.
- [x] `src/stack_engine.c3` seventh slice complete: extracted context-switch runtime subsystem (`g_current_stack_ctx`, `stack_ctx_switch_to`, `stack_ctx_suspend`, `stack_ctx_resume`) into `src/stack_engine_context_runtime.c3`, while retaining context ABI declarations, stack ctx struct/init, and clone paths in `src/stack_engine.c3`.
- [x] `src/entry.c3` slice complete: extracted test/harness command cluster (`run_test_mode`, stack-affinity harness runner, `run_test_mode_with_self`, `run_stack_affinity_probe_mode`) into `src/entry_test_modes.c3`, while retaining build/compile/repl/script command paths and CLI flag dispatch in `src/entry.c3`.
- [x] `src/scope_region.c3` second slice complete: extracted scope ownership lifecycle subsystem (`scope_create`, `scope_retain`, `scope_release`, destructor-chain runners, `scope_destroy`) into `src/scope_region_lifecycle.c3`, while retaining types/constants/global guards, chunk alloc helpers, and scope membership checks in `src/scope_region.c3`.
- [x] `src/scope_region_tests.c3` slice complete: extracted scope core test cluster (Tests 1–15 covering create/alloc/reset/RC/alignment/splice paths) into `src/scope_region_tests_core.c3` via `run_scope_region_core_tests()`, while retaining fiber-temp pool invariants + summary reporting in `src/scope_region_tests.c3`.
- [x] `src/stack_engine.c3` eighth slice complete: extracted stack cloning subsystem (`stack_ctx_clone`) into `src/stack_engine_clone.c3`, while retaining stack context ABI/struct/init declarations and section anchors in `src/stack_engine.c3`.
- [x] `src/entry.c3` second slice complete: extracted AOT build pipeline (`AOT_BUILD_PREFIX`, build output/flag/source helpers, `run_aot_backend_compile`, `run_build`) into `src/entry_build_mode.c3`, while retaining shared CLI string helpers, help/compile/repl/script handlers, and top-level argument dispatch in `src/entry.c3`.
- [x] `src/main.c3` slice complete: extracted thread-local registry subsystem (`g_thread_registry`, `thread_registry_init/shutdown`, dereference/allocate-in/root helpers) into `src/main_thread_registry.c3`, while retaining region/pool/slot data structures and registry core methods in `src/main.c3`.
- [x] `src/scope_region_tests_core.c3` slice complete: extracted reset/alignment/splice-heavy core test domain (Tests 7–15) into `src/scope_region_tests_reset_splice.c3` via `run_scope_region_reset_splice_tests()`, while retaining core setup/create/alloc/RC/freelist tests in `src/scope_region_tests_core.c3`.
- [x] `src/stack_engine_tests_coroutines.c3` slice complete: extracted coroutine edge-path test cluster (`test_stack_overflow_recovery`, `test_fpu_preservation`, `test_stack_ctx_clone_multishot` and local entry helpers) into `src/stack_engine_tests_coroutines_edges.c3`, while retaining core coroutine execution/suspend/resume/interleave tests in `src/stack_engine_tests_coroutines.c3`.
- [x] `src/stack_engine_lifecycle.c3` slice complete: extracted defer-storage subsystem (`stack_ctx_defer_entry_at`, defer reserve/push/pop/update, deferred-destroy runner, defer-storage clear) into `src/stack_engine_lifecycle_defer.c3`, while retaining lifecycle-attach/clone helpers and create/destroy orchestration in `src/stack_engine_lifecycle.c3`.
- [x] `src/stack_engine_tests_affinity_fiber_temp.c3` slice complete: extracted fiber-temp test domain (`FiberTempScopeState`, stack-scope entry helpers, `test_stack_ctx_scope_create_in_context`, clone/discard stress, retention-guard`) into `src/stack_engine_tests_fiber_temp.c3`, while retaining thread-affinity ownership smoke test and fail-fast probe harness in `src/stack_engine_tests_affinity_fiber_temp.c3`.
- [x] `src/main.c3` second slice complete: extracted pool/slot method implementations (`SparseSet.*`, `Pool.arena_alloc/allocate/destroy_all`, `SlotTable.allocate/destroy_all`) into `src/main_pool_slot_methods.c3`, while retaining region/registry core types, allocation macro, and dereference/utility helpers in `src/main.c3`.
- [x] `src/stack_engine.c3` ninth slice complete: extracted platform ABI/switch subsystem (platform constants, stack helper externs, `StackContext`, and `stack_context_switch`) into `src/stack_engine_abi_switch.c3`, while retaining coroutine status/handle metadata and `stack_ctx_init` in `src/stack_engine.c3`.
- [x] `src/stack_engine_tests_defer_lifecycle.c3` slice complete: extracted defer test domain (`test_stack_ctx_defer_destroy_lifo`, `test_stack_ctx_defer_undefer_pop`, `test_stack_ctx_defer_clone_hook`, `test_stack_ctx_defer_update_arg`, `test_stack_ctx_defer_update_arg_clone_isolation`) into `src/stack_engine_tests_defer.c3`, while retaining lifecycle destroy/clone tests in `src/stack_engine_tests_defer_lifecycle.c3`.
- [x] `src/entry_project_init_bind.c3` slice complete: extracted `--bind` command domain (path/config helpers, libclang parse pipeline, module output writer, and `run_bind`) into `src/entry_bind_mode.c3`, while retaining `--init` scaffolding helpers and `run_init` in `src/entry_project_init_bind.c3`.
- [x] `src/entry.c3` third slice complete: extracted runtime command handlers (`run_compile_mode`, `run_gen_e2e_mode`, `run_repl_mode`, `run_script_mode`) into `src/entry_runtime_modes.c3`, while retaining shared CLI helpers, help text, and top-level argument dispatch in `src/entry.c3`.
- [x] `src/stack_engine_tests_fiber_temp.c3` slice complete: extracted fiber-temp stress cluster (`test_entry_scope_yield_once`, `test_stack_ctx_fiber_temp_clone_discard_stress`, `test_stack_ctx_fiber_temp_retention_guard`) into `src/stack_engine_tests_fiber_temp_stress.c3`, while retaining scope-create-in-context state/helpers in `src/stack_engine_tests_fiber_temp.c3`.
- [x] `src/scope_region.c3` third slice complete: extracted chunk/destructor/membership helper cluster (`scope_chunk_alloc_raw`, `scope_chunk_alloc_temp`, `scope_chunk_data`, `scope_register_dtor`, `scope_register_dtor_escape`, `scope_contains`) into `src/scope_region_chunk_helpers.c3`, while retaining scope-region type definitions, global guards, and thread-affinity invariants in `src/scope_region.c3`.
- [x] `src/scope_region_temp_pool.c3` slice complete: extracted fiber-temp context-pool lifecycle cluster (`fiber_temp_ctx_pool_on_clone`, `fiber_temp_ctx_pool_on_destroy`, `fiber_temp_ctx_pool_find_current`, `fiber_temp_ctx_pool_get_or_create`, `fiber_temp_chunk_take_from_ctx_pool`) into `src/scope_region_temp_ctx_pool.c3`, while retaining global temp-pool policy, reclaim/take flow, and stat/report helpers in `src/scope_region_temp_pool.c3`.
- [x] `src/scope_region_reset_adopt.c3` slice complete: extracted reset/membership helper cluster (`scope_reset`, `scope_reset_temp_lane`, `is_in_scope`) into `src/scope_region_reset_helpers.c3`, while retaining escape-adoption (`scope_splice_escapes`) and freelist cleanup logic in `src/scope_region_reset_adopt.c3`.
- [x] `src/stack_engine_tests_defer.c3` slice complete: extracted defer-update subdomain (`test_stack_ctx_defer_update_arg`, `test_stack_ctx_defer_update_arg_clone_isolation`) into `src/stack_engine_tests_defer_update.c3`, while retaining defer destroy/undefer/clone-hook coverage in `src/stack_engine_tests_defer.c3`.
- [x] `src/scope_region_tests_reset_splice.c3` slice complete: extracted `is_in_scope` and `scope_splice_escapes` test-domain helpers into `src/scope_region_tests_splice_cases.c3`, while retaining reset/alignment/deep-chain/retain tests in `src/scope_region_tests_reset_splice.c3`.
- [x] `src/stack_engine_lifecycle.c3` second slice complete: extracted lifecycle hook storage/clone/destroy helper cluster (`stack_ctx_lifecycle_entry_at`, reserve/attach/find/clone/run/clear helpers) into `src/stack_engine_lifecycle_hooks.c3`, while retaining context create/destroy orchestration in `src/stack_engine_lifecycle.c3`.
- [x] `src/stack_engine_tests_coroutines.c3` second slice complete: extracted basic one-shot coroutine smoke cluster (`test_entry_simple`, `test_entry_100`, `test_basic_coro`, `test_stack_ctx_second`) into `src/stack_engine_tests_coroutines_basic.c3`, while retaining suspend/resume/recursion/interleave coverage in `src/stack_engine_tests_coroutines.c3`.
- [x] `src/main.c3` second slice complete: extracted `RegionRegistry` method cluster (`RegionRegistry.init`, `RegionRegistry.dereference`, `RegionRegistry.dereference_as`) into `src/main_region_registry_methods.c3`, while retaining region/pool/slot type definitions and allocation macros in `src/main.c3`.
- [x] `src/stack_engine_tests_fiber_temp_stress.c3` second slice complete: extracted fiber-temp retention guard stress test (`test_stack_ctx_fiber_temp_retention_guard`) into `src/stack_engine_tests_fiber_temp_retention.c3`, while retaining shared yield-entry helper and clone/discard stress coverage in `src/stack_engine_tests_fiber_temp_stress.c3`.
- [x] `src/scope_region_temp_pool.c3` second slice complete: extracted temp-pool stats/type/reporting cluster (stats structs/globals, eligibility predicates, `scope_print_memory_stats`) into `src/scope_region_temp_pool_stats.c3`, while retaining chunk reclaim/take/release policy logic in `src/scope_region_temp_pool.c3`.
- [x] `src/scope_region_lifecycle.c3` second slice complete: extracted destructor/destroy path cluster (`scope_run_dtor_chain`, `scope_run_dtors`, `scope_run_escape_dtors`, `scope_destroy`) into `src/scope_region_destroy.c3`, while retaining `scope_create` and retain/release APIs in `src/scope_region_lifecycle.c3`.
- [x] `src/stack_engine.c3` tenth slice complete: extracted stack context metadata/type declarations (`StackCtxStatus`, `StackCtx` callback/boot structs, inline caps, and `StackCtx` handle layout) into `src/stack_engine_types.c3`, while retaining `stack_ctx_init` context setup flow in `src/stack_engine.c3`.
- [x] `src/main.c3` third slice complete: extracted pool/slot storage type cluster (`SparseSet`, `PoolSlot`/`ArenaBlock`/`FreeListEntry`, `Pool`, `ObjectLiveInfo`, `ObjectRecord`, `SlotTable`, `SlotAllocationResult`, and related constants) into `src/main_pool_slot_types.c3`, while retaining handle aliases, `Region`, allocation/destroy helpers, and `RegionRegistry` declarations in `src/main.c3`.
- [x] `src/stack_engine_tests.c3` fourth slice complete: extracted test-suite runner/orchestration (`run_stack_engine_tests`) into `src/stack_engine_tests_runner.c3`, while retaining defer helper fixtures and stack-region/pool smoke tests in `src/stack_engine_tests.c3`.
- [x] `src/entry_bind_mode.c3` second slice complete: extracted bind path helper cluster (`resolve_bind_project_dir`, `build_bind_toml_path`, `build_bind_output_path`) into `src/entry_bind_paths.c3`, while retaining libclang parse pipeline, parsed-function lifecycle, and `run_bind` orchestration in `src/entry_bind_mode.c3`.
- [x] `src/scope_region_tests_reset_splice.c3` second slice complete: extracted topology/alignment test cluster (large allocation, deep parent-chain cascade, retain/release symmetry, and alignment gap checks) into `src/scope_region_tests_topology_alignment.c3`, while retaining reset/escape-lane/is_in_scope/splice orchestration in `src/scope_region_tests_reset_splice.c3`.
- [x] `src/scope_region_tests_alloc_lifecycle.c3` second slice complete: extracted RC/destructor/freelist lifecycle cluster (parent-child refcount invariants, destructor execution, freelist generation increment) into `src/scope_region_tests_refcount_destructor.c3`, while retaining create/alloc/escape/chunk-overflow coverage in `src/scope_region_tests_alloc_lifecycle.c3`.
- [x] `src/stack_engine_tests_coroutines_edges.c3` second slice complete: extracted overflow-recovery test domain (`test_entry_overflow`, `test_overflow_recurse`, `test_stack_overflow_recovery`) into `src/stack_engine_tests_coroutine_overflow.c3`, while retaining FPU-preservation and clone-multishot edge coverage in `src/stack_engine_tests_coroutines_edges.c3`.
- [x] `src/scope_region_tests_core.c3` second slice complete: extracted scope alloc/lifecycle test domain (Tests 1–6 covering create/alloc/escape/destructor/RC/freelist) into `src/scope_region_tests_alloc_lifecycle.c3`, while retaining core orchestrator aggregation and reset/splice composition in `src/scope_region_tests_core.c3`.
- [x] `src/entry_runtime_modes.c3` slice complete: extracted compile command-mode cluster (`run_compile_mode`) into `src/entry_compile_mode.c3`, while retaining REPL and `--gen-e2e` runtime handlers in `src/entry_runtime_modes.c3`.
- [x] `src/entry_project_init_bind.c3` slice complete: extracted `--init` scaffold path/file writer helpers (`build_project_path`, `mkdir_project_path`, `write_init_toml`, `write_init_project_json`, `write_init_main_omni`) into `src/entry_project_init_files.c3`, while retaining init command orchestration/summary in `src/entry_project_init_bind.c3`.
- [x] `src/entry_build_mode.c3` slice complete: extracted build-command argument/source/temp-file helper cluster (`find_build_output_arg`, `derive_default_output_name`, `load_build_source`, `parse_build_print_flags`, `write_aot_temp_file`) into `src/entry_build_helpers.c3`, while retaining build backend invocation and `run_build` orchestration in `src/entry_build_mode.c3`.
- [x] `src/entry_bind_mode.c3` slice complete: extracted bind parse/cleanup helper cluster (`parse_ffi_dep_headers`, `free_parsed_funcs`) into `src/entry_bind_parse_helpers.c3`, while retaining bind config loading, libclang lifecycle, and module generation orchestration in `src/entry_bind_mode.c3`.
- [x] `src/entry.c3` slice complete: extracted shared CLI helper cluster (`mem_stats_enabled`, `cstr_len`, `cstr_contains`, `find_flag_index`) into `src/entry_cli_helpers.c3`, while retaining help text and top-level flag dispatch/`main` routing in `src/entry.c3`.
- [x] `src/entry_test_modes.c3` slice complete: extracted stack-affinity probe/harness helper cluster (`run_stack_affinity_harness`, `run_stack_affinity_probe_mode`) into `src/entry_stack_affinity_mode.c3`, while retaining core test runner and harness-gating orchestration in `src/entry_test_modes.c3`.
- [x] `src/entry_project_init_files.c3` slice complete: extracted init-file writer cluster (`write_init_toml`, `write_init_project_json`, `write_init_main_omni`) into `src/entry_project_init_writers.c3`, while retaining project path builder/mkdir helpers in `src/entry_project_init_files.c3`.
- [x] `src/entry_bind_mode.c3` slice complete: extracted per-dependency bind generation cluster (header parse + output path/build + module emit + cleanup/accounting) into `src/entry_bind_dep_generation.c3`, while retaining config/libclang initialization and top-level bind orchestration in `src/entry_bind_mode.c3`.
- [x] `src/entry_build_mode.c3` slice complete: extracted backend compile command cluster (`AOT_BUILD_PREFIX`, `run_aot_backend_compile`) into `src/entry_build_backend_compile.c3`, while retaining compile pipeline orchestration in `src/entry_build_mode.c3`.
- [x] `src/entry.c3` slice complete: extracted CLI help/version text cluster (`print_help`, `print_version`) into `src/entry_cli_help_version.c3`, while retaining main argument dispatch and command routing in `src/entry.c3`.
- [x] `src/entry_script_mode.c3` slice complete: extracted script error/reporting helper cluster (`print_script_eval_error`, `print_script_result_if_non_nil`, `print_script_read_error`) into `src/entry_script_reporting.c3`, while retaining script load/interpreter lifecycle orchestration in `src/entry_script_mode.c3`.
- [x] `src/entry_bind_mode.c3` second slice complete: extracted bind runtime setup cluster (`bind_init_clang_or_report`, parsed-function buffer allocation/initialization) into `src/entry_bind_runtime_setup.c3`, while retaining config loading and bind-generation loop orchestration in `src/entry_bind_mode.c3`.
- [x] `src/entry_project_init_writers.c3` slice complete: extracted `project.json` writer cluster (`write_init_project_json`) into `src/entry_project_init_writer_project_json.c3`, while retaining `omni.toml` and `main.omni` writer helpers in `src/entry_project_init_writers.c3`.
- [x] `src/entry_test_modes.c3` slice complete: extracted test-runner setup/reporting cluster (thread-registry init, Lisp suite execution, mem-stats reporting, and shutdown summary) into `src/entry_test_runner_setup.c3`, while retaining stack/scope test orchestration and harness gating in `src/entry_test_modes.c3`.
- [x] `src/entry_compile_mode.c3` slice complete: extracted compile command success/error reporting cluster (`print_compile_usage`, `print_compile_start`, `print_compile_success`, `print_compile_write_error`, `print_compile_read_error`) into `src/entry_compile_reporting.c3`, while retaining compile load/interpreter lifecycle and output write orchestration in `src/entry_compile_mode.c3`.

## Track A: libuv First-Class Runtime Integration

### A0. Baseline inventory and migration map
- [x] Inventory every `io/*` effect and map current backend (`blocking`, `offload`, `libuv`).
- [x] Mark each primitive as one of: `done-libuv`, `partial-libuv`, `non-libuv`.
- [x] Record map in this file and mirror status summary in `memory/CHANGELOG.md`.

Backend legend:

- `blocking`: direct sync runtime/file/socket/syscall path
- `offload`: pooled background execution path (currently `uv_queue_work`)
- `libuv-bridge`: fiber async path uses libuv poll/timer/loop wake
- `mixed`: combination of the above depending on context

Status legend:

- `done-libuv`: no non-libuv path in async/fiber flow and no non-fiber behavioral fallback (hard-error outside fiber for async-only effects)
- `partial-libuv`: some libuv integration exists, but not complete
- `non-libuv`: no direct libuv backend for the effect behavior

Current `io/*` effect backend map:

| Effect | Raw primitive | Impl | Current backend | Status | Notes |
|---|---|---|---|---|---|
| `io/print` | `__raw-print` | `src/lisp/prim_io.c3` | libuv fs write (`uv_fs_write`) | done-libuv | synchronous console output now routes through libuv fs write backend |
| `io/println` | `__raw-println` | `src/lisp/prim_io.c3` | libuv fs write (`uv_fs_write`) | done-libuv | synchronous console output now routes through libuv fs write backend |
| `io/display` | `__raw-display` | `src/lisp/prim_io.c3` | libuv fs write (`uv_fs_write`) | done-libuv | display output uses libuv fs write backend without direct stdio writes |
| `io/newline` | `__raw-newline` | `src/lisp/prim_io.c3` | libuv fs write (`uv_fs_write`) | done-libuv | newline emission uses libuv fs write backend |
| `io/read-file` | `__raw-read-file` | `src/lisp/prim_io.c3` | fiber offload (`uv_queue_work` + `uv_fs_*` worker), hard-error non-fiber | done-libuv | no blocking read syscall on fiber path; non-fiber raises `io/read-file-fiber-required` |
| `io/write-file` | `__raw-write-file` | `src/lisp/prim_io.c3` | fiber offload (`uv_queue_work` + `uv_fs_*` worker), hard-error non-fiber | done-libuv | no blocking write syscall on fiber path; non-fiber raises `io/write-file-fiber-required` |
| `io/file-exists?` | `__raw-file-exists?` | `src/lisp/prim_io.c3` | fiber offload (`uv_queue_work` + `uv_fs_stat` worker), hard-error non-fiber | done-libuv | no blocking stat syscall on fiber path; non-fiber raises `io/file-exists?-fiber-required` |
| `io/read-lines` | `__raw-read-lines` | `src/lisp/prim_io.c3` | fiber offload read + local split, hard-error non-fiber | done-libuv | no blocking file read syscall on fiber path; non-fiber raises `io/read-lines-fiber-required` |
| `io/pipe-connect` | `__raw-pipe-connect` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | fiber-only (`uv_pipe_connect` on scheduler loop), hard-error non-fiber | done-libuv | non-fiber raises `io/pipe-connect-fiber-required`; async connect uses scheduler wakeup and `uv_accept`-compatible detach path |
| `io/pipe-listen` | `__raw-pipe-listen` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | fiber-only (`uv_pipe_bind`/`uv_listen`), hard-error non-fiber | done-libuv | non-fiber raises `io/pipe-listen-fiber-required`; listener keeps persistent libuv handle with deterministic close |
| `io/tcp-connect` | `__raw-tcp-connect` | `src/lisp/async.c3` | fiber-only (`uv_tcp_connect`), hard-error non-fiber | done-libuv | fiber path uses libuv async DNS + `uv_tcp_connect`; non-fiber raises `io/tcp-connect-fiber-required` |
| `io/tcp-listen` | `__raw-tcp-listen` | `src/lisp/async.c3` | fiber-only (`uv_tcp_bind`/`uv_listen`), hard-error non-fiber | done-libuv | listener setup via libuv in fiber; non-fiber raises `io/tcp-listen-fiber-required` |
| `io/tcp-accept` | `__raw-tcp-accept` | `src/lisp/async.c3` | fiber-only (`uv_accept` for TCP and pipe listeners), hard-error non-fiber | done-libuv | non-fiber raises `io/tcp-accept-fiber-required`; pipe listener accept no longer uses poll+`accept` fallback path |
| `io/tcp-read` | `__raw-tcp-read` | `src/lisp/async.c3`, `src/lisp/scheduler_tcp_async_bridge.c3` | fiber-only (`libuv-bridge`), hard-error non-fiber | done-libuv | fiber path uses libuv poll/timer bridge; non-fiber raises `io/tcp-read-fiber-required` |
| `io/tcp-write` | `__raw-tcp-write` | `src/lisp/async.c3`, `src/lisp/scheduler_tcp_async_bridge.c3` | fiber-only (`libuv-bridge`), hard-error non-fiber | done-libuv | fiber path uses libuv writable poll bridge; non-fiber raises `io/tcp-write-fiber-required` |
| `io/tcp-close` | `__raw-tcp-close` | `src/lisp/async.c3` | fiber-only (libuv close), hard-error non-fiber | done-libuv | fiber path uses `uv_tcp_open`+`uv_close`; non-fiber raises `io/tcp-close-fiber-required`; teardown-only close safety fallback retained |
| `io/dns-resolve` | `__raw-dns-resolve` | `src/lisp/async.c3` | fiber-only (`uv_getaddrinfo`), hard-error non-fiber | done-libuv | non-fiber raises `io/dns-resolve-fiber-required`; fiber path is libuv-only with no sync fallback |
| `io/async-sleep` | `__raw-async-sleep` | `src/lisp/async.c3` | fiber-only (`uv_timer_start`), hard-error non-fiber | done-libuv | non-fiber raises `io/async-sleep-fiber-required`; no offload or blocking `usleep` fallback |
| `io/offload` | `__raw-offload` | `src/lisp/scheduler_primitives.c3`, `src/lisp/scheduler_offload_worker.c3` | `uv_queue_work` (fiber + non-fiber blocking wait) | done-libuv | both fiber and non-fiber paths enqueue on `uv_queue_work`; non-fiber waits on task completion without sync worker fallback |
| `io/task-spawn` | `__raw-task-spawn` | `src/lisp/scheduler_primitives_tasks.c3` | `uv_queue_work` + scheduler wake | done-libuv | pooled task spawn on `uv_queue_work`; returns opaque generation-safe `task-handle`; admission cap is explicit |
| `io/task-join` | `__raw-task-join` | `src/lisp/scheduler_primitives_tasks.c3` | `uv_queue_work` + scheduler wake | done-libuv | generation-safe handle join supports both root and fiber contexts; no-timeout fiber join is event-driven wakeup (no sleep polling), non-fiber join pumps `uv_run(...ONCE)` (no condvar polling slices) |
| `io/task-join-timeout` | `__raw-task-join-timeout` | `src/lisp/scheduler_primitives_tasks.c3` | `uv_queue_work` + scheduler wake | done-libuv | timeout join supports root and fiber contexts with deterministic timeout semantics; fiber timeout wait uses completion wake + single libuv timer, and root timeout join uses a libuv timer + `uv_run(...ONCE)` (no 1ms polling) |
| `io/task-cancel` | `__raw-task-cancel` | `src/lisp/scheduler_primitives_tasks.c3` | `uv_queue_work` + scheduler wake | done-libuv | pooled-task cancellation via `task-handle`; pending tasks map to `uv_cancel`, running tasks use cooperative cancel |
| `io/thread-spawn` | `__raw-thread-spawn` | `src/lisp/scheduler_primitives_threads.c3`, `src/lisp/scheduler_thread_tasks.c3` | libuv thread API + scheduler wake | done-libuv | returns opaque generation-safe `thread-handle`; dedicated OS thread lifecycle now uses `uv_thread_create` (distinct from pool-backed `task-*`) |
| `io/thread-join` | `__raw-thread-join` | `src/lisp/scheduler_primitives_threads.c3`, `src/lisp/scheduler_thread_tasks.c3` | libuv thread API + scheduler wake | done-libuv | join consumes generation-safe `thread-handle` completion and joins underlying OS thread via `uv_thread_join`; no-timeout fiber join is event-driven wakeup (no sleep polling) |
| `io/thread-join-timeout` | `__raw-thread-join-timeout` | `src/lisp/scheduler_primitives_threads.c3`, `src/lisp/scheduler_thread_tasks.c3` | libuv thread API + scheduler wake | done-libuv | bounded wait over real OS-thread completion with deterministic timeout/cancel semantics; fiber timeout wait uses event-driven completion wake + single libuv timer (no 1ms polling) |
| `io/thread-cancel` | `__raw-thread-cancel` | `src/lisp/scheduler_primitives_threads.c3`, `src/lisp/scheduler_thread_tasks.c3` | libuv thread API + scheduler wake | done-libuv | cooperative cancellation request over dedicated thread entry state; active-thread drop path uses `uv_thread_detach` when non-blocking teardown is required |
| `io/process-spawn` | `__raw-process-spawn` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | libuv (`uv_spawn` + `UV_CREATE_PIPE` stdio, detached as fs-handle FDs) | done-libuv | process lifecycle and stdio provisioning are libuv-backed; stdio is returned as detached Omni fs handles |
| `io/process-wait` | `__raw-process-wait` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | fiber-only (polled `uv_run(...NOWAIT)` + async yield), hard-error non-fiber | done-libuv | non-fiber raises `io/process-wait-fiber-required`; fiber path stays non-blocking while waiting for process exit |
| `io/process-kill` | `__raw-process-kill` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | libuv (handle-scoped `uv_process_kill`) | done-libuv | signal delivery uses process-handle scoped libuv kill path |
| `io/signal-handle` | `__raw-signal-handle` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | libuv (`uv_signal`) | done-libuv | watcher install and polling are libuv-based |
| `io/signal-unhandle` | `__raw-signal-unhandle` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | libuv (`uv_signal`) | done-libuv | watcher teardown uses libuv close/stop path |
| `io/tls-connect` | `__raw-tls-connect` | `src/lisp/tls.c3` | fiber offload (`uv_queue_work` + BearSSL setup), hard-error non-fiber | done-libuv | non-fiber raises `io/tls-connect-fiber-required`; fiber path builds TLS context on libuv worker queue |
| `io/tls-read` | `__raw-tls-read` | `src/lisp/tls.c3` | fiber offload (`uv_queue_work` + BearSSL), hard-error non-fiber | done-libuv | non-fiber raises `io/tls-read-fiber-required`; fiber path runs TLS I/O on libuv worker queue |
| `io/tls-write` | `__raw-tls-write` | `src/lisp/tls.c3` | fiber offload (`uv_queue_work` + BearSSL), hard-error non-fiber | done-libuv | non-fiber raises `io/tls-write-fiber-required`; fiber path writes/flushed via libuv worker queue |
| `io/tls-close` | `__raw-tls-close` | `src/lisp/tls.c3` | fiber offload (`uv_queue_work` + BearSSL), hard-error non-fiber | done-libuv | non-fiber raises `io/tls-close-fiber-required`; TLS shutdown/final close runs on libuv worker queue |
| `io/http-get` | `__raw-http-get` | `src/lisp/http.c3` | fiber-only (`tcp-*` libuv paths), hard-error non-fiber | done-libuv | explicit `io/http-get-fiber-required`; no blocking/offload fallback |
| `io/http-request` | `__raw-http-request` | `src/lisp/http.c3` | fiber-only (`tcp-*` libuv paths), hard-error non-fiber | done-libuv | explicit `io/http-request-fiber-required`; write errors no longer ignored |

A0 summary snapshot:

- `done-libuv`: 38/38 (`print`, `println`, `display`, `newline`, `dns-resolve`, `async-sleep`, `signal-handle`, `signal-unhandle`, `tcp-connect`, `tcp-listen`, `tcp-accept`, `tcp-read`, `tcp-write`, `tcp-close`, `pipe-connect`, `pipe-listen`, `http-get`, `http-request`, `io/read-file`, `io/write-file`, `io/file-exists?`, `io/read-lines`, `process-spawn`, `process-wait`, `process-kill`, `offload`, `task-spawn`, `task-join`, `task-join-timeout`, `task-cancel`, `thread-spawn`, `thread-join`, `thread-join-timeout`, `thread-cancel`, `tls-connect`, `tls-read`, `tls-write`, `tls-close`)
- `partial-libuv`: 0/38
- `non-libuv`: 0/38

### A1. Timers and DNS
- [x] Replace `async-sleep` fiber path with direct `uv_timer_start` scheduling.
- [x] Replace non-fiber fallback behavior with deterministic hard errors (`fiber-required`) for async-only effects.
- [x] Add async DNS path via `uv_getaddrinfo` and route `io/dns-resolve` through it.
- [x] Add cancellation-safe cleanup for pending timer/DNS handles.

### A2. Async filesystem (`uv_fs_*`)
- [x] Add raw primitives:
  - `(fs-open path flags)`
  - `(fs-read handle n)`
  - `(fs-write handle data)`
  - `(fs-close handle)`
  - `(fs-stat path)`
  - `(fs-readdir path)`
  - `(fs-rename src dst)`
  - `(fs-unlink path)`
- [x] Wire stdlib wrappers and typed effects.
- [x] Ensure file handles have deterministic close/finalizer behavior.

### A3. Networking expansion
- [x] Add UDP support:
  - `(udp-socket)`
  - `(udp-bind handle host port)`
  - `(udp-send handle host port data)`
  - `(udp-recv handle)`
  - `(udp-close handle)`
- [x] Add Unix domain socket/pipe support:
  - `(pipe-connect path)`
  - `(pipe-listen path)`
- [x] Unify socket readiness and wakeup handling with scheduler barrier paths.

### A4. Process and signal integration
- [x] Add process primitives:
  - `(process-spawn cmd args env)`
  - `(process-wait handle)`
  - `(process-kill handle sig)`
- [x] Expose process stdio handles and integrate with existing read/write APIs.
- [x] Add signal primitives:
  - `(signal-handle sig callback)`
  - `(signal-unhandle handle)`

### A5. Offload / Task / Thread model alignment
- [x] Keep `offload` for CPU-bound work (compression, heavy compute).
- [x] Rename the current pooled background-job surface from `thread-*` to
  `task-*`.
- [x] Move pooled task execution backend to universal `uv_queue_work`
  substrate:
  - [x] `io/offload` -> `uv_queue_work`
  - [x] `io/task-*` -> `uv_queue_work`
  - [x] preserve Omni-owned completion and boundary handoff objects
  - [x] replace polling joins with event-driven waits:
    - [x] fiber joins via completion wake events
    - [x] root task joins via `uv_run(...ONCE)` + libuv timeout timer
    - [x] root dedicated-thread joins via predicate+condvar wait (no 1ms slices)
  - [x] eliminate missed-signal deadlocks by lock-disciplined condition-variable signaling and atomic predicate wait loops
  - [x] use `uv_cancel` for pending tasks and cooperative cancel for running
    tasks
- [x] Implement real OS-thread lifecycle under `thread-*`.
- [x] Update docs/tests so naming matches runtime semantics.

### A6. libuv completion hardening
- [x] Add stress tests for wakeup ordering, duplicate ready events, and cancellation.
- [x] Verify no fiber hangs or leaked pending ops under repeated runs.
- [x] Run:
  - `c3c build`
  - `c3c build --sanitize=address`
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `scripts/run_boundary_hardening.sh`

## Track B: BearSSL Integration

- [x] Add certificate verification and trust store support in `tls-connect`.
- [x] Add explicit CA bundle/PEM options.
- [x] Add server-side TLS (`tls-server-wrap` style API).
- [x] Add optional mutual TLS inputs (client cert/key).
- [x] Add optional session resumption policy.
- [x] Add tests for valid cert, invalid cert, hostname mismatch, and mTLS fail/pass paths.

## Track C: libdeflate Integration

- [x] Add compression level parameter support for `gzip` and `deflate`.
- [x] Add zlib format support (`zlib-compress`, `zlib-decompress`).
- [x] Add checksum primitives (`adler32`, `crc32`).
- [x] Add regression tests for round-trips and invalid/corrupt payload behavior.

## Track D: utf8proc Integration

- [x] Fix `string-upcase`/`string-downcase` truncation from fixed 1024-byte buffer.
- [x] Add `string-casefold`.
- [x] Add `string-titlecase`.
- [x] Add `char-width`.
- [x] Add `char-property`.
- [x] Add long-string and multi-byte Unicode regression tests.

## Track E: yyjson Integration (Advanced Parity Backlog)

- [x] Baseline parse/emit integration is landed:
  - `json-parse`
  - `json-emit`
  - `json-emit-pretty`
- [x] Baseline round-trip/shape tests are present in `src/lisp/tests_tests.c3`.

- [x] Add parse options:
  - [x] allow comments
  - [x] allow trailing commas
  - [x] allow `NaN`/`Inf`
- [x] Add JSON pointer query API (`json-get` style path lookup).
- [x] Add emit/format controls for numeric precision and writer flags.
- [x] Add test matrix for permissive parse flags and strict/default behavior.

## Track G: Data-Format Integration

- [x] Add TOML parser wrapper (`toml-parse`) and timestamp handling for date/time/datetime.
- [x] Add CSV parser/serializer primitives (`csv-parse`, `csv-emit`) with proper quoted field, delimiter, and multiline handling.
- [x] Add regression coverage for RFC 4180-like delimiter/quoting cases and direct row/cell access path.
- [x] Add CSV writer options (`delimiter`, `line ending`, `quote style`, nullable fields) and explicit strict-mode toggles.
- [x] Add TOML advanced control surface (documented defaults and error diagnostics for unsupported edge forms).

## Track F: LMDB / Deduce Integration (Advanced Parity Backlog)

Note: `(deduce 'open "path")` and `(deduce 'open 'memory)` are already
implemented and documented; do not track this as a missing gap.

- [x] Baseline unified `deduce` dispatcher is landed:
  - `'open`
  - `'scan`
  - `'query`
  - `'count`
  - `'match`
  - `'fact!`
  - `'retract!`
- [x] Baseline in-memory and persistent open paths are landed and tested.

- [x] Add range-scan API with explicit bounded cursor semantics.
- [x] Add explicit user transaction API (`begin/commit/abort`).
- [x] Add named database exposure where useful at language surface.
- [x] Add bulk relation drop/clear API.
- [x] Add durability and recovery tests across process restart.

## Execution Policy

- [x] Implement libuv track first (A1-A6) before broadening non-libuv features.
- [x] Keep each integration slice small and testable.
- [x] Treat fallback removal as a quality gate:
  - [x] no new async backend fallback branches in primitives.
  - [x] async effects must hard-error outside fiber context until fully migrated.
  - [x] only teardown-only safety fallbacks are permitted.
- [x] For behavior changes, update:
  - `memory/CHANGELOG.md`
  - affected reference docs in `docs/reference/`
  - this plan status checkboxes.

## Post-Integration Cleanup Queue (Append-Only)

- [x] Canonicalize fiber-join wait error domain mapping: when `task-join` / `task-join-timeout` / `thread-join` / `thread-join-timeout` fiber wait slices fail, remap surfaced errors to `scheduler/*` codes (`scheduler/task-join-wait-failed`, `scheduler/thread-join-wait-failed`) while preserving original lower-level detail in payload data.
