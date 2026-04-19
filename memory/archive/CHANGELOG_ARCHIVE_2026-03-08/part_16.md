- `src/lisp/tests_tests.c3`, `stdlib/stdlib.lisp`
- `docs/LANGUAGE_SPEC.md`, `docs/SYNTAX_SPEC.md`, `docs/FEATURES.md`, `docs/CORE_LIBS_INSPECTION.md`
- `memory/CHANGELOG.md`

### Test Count
- Unified: 1139 passed, 0 failed
- Compiler: 73 passed, 0 failed
- Stack: 10, Scope: 50

## 2026-03-03: Thread Task Timeouts/Cancellation + Variadic Lambda Parse Fix

### Summary
Extended thread task effects with timeout and cancellation semantics, and fixed a parser initialization bug that could misclassify `(lambda (.. rest) ...)` as typed.

### Changes
- **Thread task lifecycle states** (`src/lisp/scheduler.c3`):
  - Added `ThreadTaskState` (`PENDING`, `RUNNING`, `DONE`, `CANCELLED`) and `cancel_requested`.
  - Worker path now claims task state before execution (`scheduler_try_begin_thread_task`).
  - Running-task cancellation is cooperative; pending-task cancellation is immediate.
- **New task primitives/effects**:
  - Added runtime primitives:
    - `__raw-thread-join-timeout` (`prim_thread_join_timeout`)
    - `__raw-thread-cancel` (`prim_thread_cancel`)
  - Added stdlib effects/wrappers:
    - `(io/thread-join-timeout (^Any args))` + `thread-join-timeout`
    - `(io/thread-cancel (^Int task-id))` + `thread-cancel`
  - Added fast-path mappings in primitive registration for both effects.
- **Join implementation refactor** (`src/lisp/scheduler.c3`):
  - Shared join logic extracted to `scheduler_thread_join_impl(...)`.
  - `thread-join` now uses the shared path with infinite timeout.
  - `thread-join-timeout` uses bounded wait and returns timeout error deterministically.
- **Parser correctness fix** (`src/lisp/parser_parser.c3`):
  - In `(lambda (.. rest) ...)` parsing branch, explicitly initialize:
    - `e.lambda.has_typed_params = false`
  - Prevents accidental method-table creation when redefining primitive names with rest-only lambdas (for example `(define collect (lambda (.. args) args))`).
- **Tests** (`src/lisp/tests_tests.c3`):
  - Added scheduler tests:
    - `thread-join-timeout immediate timeout`
    - `thread-join-timeout success`
    - `thread-cancel causes join cancellation`
  - Existing variadic lambda tests now pass again with parser fix.

### Files Modified
- `src/lisp/scheduler.c3`
- `src/lisp/eval.c3`
- `stdlib/stdlib.lisp`
- `src/lisp/parser_parser.c3`
- `src/lisp/tests_tests.c3`

### Validation
- `c3c build` ✅
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` ✅
  - Unified tests: `1133 passed, 0 failed`

## 2026-03-03: Thread Guardrails — Scope Affinity + Minimal Task Effects

### Summary
Added runtime thread-affinity enforcement for `ScopeRegion` access and introduced a minimal thread task effect API (`io/thread-spawn`, `io/thread-join`) over sendable offload jobs.

### Changes
- **ScopeRegion owner-thread checks** (`src/scope_region.c3`):
  - Added `owner_thread_token` to `ScopeRegion`.
  - Added thread-local marker and owner check helpers.
  - Enforced owner checks in:
    - `scope_retain`, `scope_release`, `scope_destroy`
    - `alloc/alloc_slow`, `alloc_escape/alloc_escape_slow`
    - `scope_register_dtor`, `scope_register_dtor_escape`
    - `scope_reset`, `scope_reset_temp_lane`
    - `scope_splice_escapes`
  - Cross-thread region access now fails immediately (deterministic guardrail) instead of risking silent corruption.
- **Minimal threaded task effects**:
  - Added stdlib effect declarations/wrappers:
    - `(io/thread-spawn (^Any job))`
    - `(io/thread-join (^Int task-id))`
    - wrappers `thread-spawn`, `thread-join`
  - Registered runtime fast-paths:
    - `io/thread-spawn -> __raw-thread-spawn`
    - `io/thread-join -> __raw-thread-join`
  - Added primitives in scheduler:
    - `prim_thread_spawn` enqueues validated sendable job and returns integer `task-id`
    - `prim_thread_join` waits for task completion and returns value
- **Shared job parsing / sendable boundary** (`src/lisp/scheduler.c3`):
  - Added `scheduler_build_offload_work` used by both `offload` and `thread-spawn`.
  - Supported task job ops remain sendable and explicit: `sleep-ms`, `gzip`, `deflate`.
- **Task table over existing worker thread** (`src/lisp/scheduler.c3`):
  - Added bounded `thread_tasks` table with mutex protection.
  - Worker route now supports:
    - fiber completion path (`WAKEUP_OFFLOAD_READY`)
    - task completion path (`thread-spawn` / `thread-join`)
- **Tests** (`src/lisp/tests_tests.c3`):
  - Added scheduler tests:
    - `thread-spawn/thread-join gzip`
    - concurrent thread task joins
    - double join rejection

### Files Modified
- `src/scope_region.c3`
- `stdlib/stdlib.lisp`
- `src/lisp/eval.c3`
- `src/lisp/scheduler.c3`
- `src/lisp/tests_tests.c3`

### Validation
- `c3c build` ✅
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` ✅
  - Unified tests: `1130 passed, 0 failed`
  - Compiler tests: `73 passed, 0 failed`

## 2026-03-03: `io/offload` Effect + Scope Freelist Thread-Safety

### Summary
Added a minimal `io/offload` effect path for worker-thread execution with fiber suspension/resume semantics, and hardened `ScopeRegion` global freelist/generation access for concurrent threads.

### Changes
- **Effect surface (minimal exposure)**:
  - Added `(define [effect] (io/offload (^Any job)))` and stdlib wrapper:
    - `(define offload (lambda (op .. args) (signal io/offload (cons op args))))`
  - No channel/thread primitives added to Lisp surface.
- **Runtime fast-path wiring** (`src/lisp/eval.c3`):
  - Registered raw primitive `__raw-offload`.
  - Registered fast-path dispatch `io/offload -> __raw-offload`.
- **Scheduler worker offload bridge** (`src/lisp/scheduler.c3`):
  - Added pending offload slots per fiber.
  - Added worker queue + dedicated worker thread.
  - Added wakeup event type `WAKEUP_OFFLOAD_READY` with payload pointer.
  - Added internal `SharedBlob` (atomic refcount + immutable bytes) to tether offload payload lifetimes across scheduler/worker threads.
  - Offload compression now passes `SharedBlob` payloads across thread boundaries instead of raw region-owned pointers.
  - Offload byte results are transferred into runtime values with owned-byte handoff on the scheduler thread (no extra copy at completion materialization).
  - `prim_offload` behavior:
    - in fiber context: enqueue work, block fiber, resume on completion,
    - outside fiber context: synchronous fallback execution.
  - Implemented initial worker ops:
    - `sleep-ms` -> returns `nil`
    - `gzip` -> returns compressed bytes string
    - `deflate` -> returns compressed bytes string
  - All Omni value allocation remains on scheduler/main thread.
- **Wakeup drain correctness fix**:
  - `drain_wakeups()` now handles offload events independently from pending TCP-read state.
- **Owned string constructor** (`src/lisp/value.c3`):
  - Added `make_string_owned(interp, owned_chars, len)` so scheduler-side offload completion can adopt heap bytes directly while preserving scope dtor cleanup.
- **Scope-region thread-safety hardening** (`src/scope_region.c3`):
  - Added global mutex + once-init for shared freelist/generation state.
  - Guarded freelist pop/push and generation increments in:
    - `scope_create`
    - `scope_destroy`
    - `scope_splice_escapes`
    - `scope_freelist_cleanup`
- **Regression tests** (`src/lisp/tests_tests.c3`):
  - Added scheduler tests:
    - offload sync gzip roundtrip
    - offload async fiber resume
    - unsupported op rejection

### Files Modified
- `stdlib/stdlib.lisp`
- `src/lisp/eval.c3`
- `src/lisp/scheduler.c3`
- `src/lisp/tests_tests.c3`
- `src/scope_region.c3`
- `src/lisp/value.c3`

### Validation
- `c3c build` ✅
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` ✅
  - Unified tests: `1127 passed, 0 failed`
  - Compiler tests: `73 passed, 0 failed`

## 2026-03-03: Async-Safe Scheduler Wakeup Queue Boundary

### Summary
Hardened libuv callback boundaries in the fiber scheduler by introducing a thread-safe MPSC wakeup ring with `uv_async` signaling. libuv callbacks now enqueue lightweight wakeup events only; all `tcp-read` I/O and fiber state transitions execute in scheduler drain phase on the main thread.

### Changes
- **libuv async wake support** (`src/lisp/async.c3`):
  - Added `uv_async_init` / `uv_async_send` externs and `UV_HANDLE_ASYNC`.
- **Thread-safe wakeup queue + drain ownership** (`src/lisp/scheduler.c3`):
  - Added wake event type (`POLL_READABLE`, `POLL_ERROR`, `TIMER_EXPIRED`) and ring storage.
  - Added atomic queue state (`wakeup_head`, `wakeup_tail`, per-slot ready flags) with CAS-based producer reservation (`wakeup_enqueue`).
  - Added scheduler-owned `drain_wakeups()` consumer path that performs:
    - non-blocking `c_recv`,
    - `PendingTcpRead` completion updates,
    - `FIBER_BLOCKED -> FIBER_READY` transitions via shared completion path.
  - Added `uv_async_t` initialization in `scheduler_init` and noop async callback.
  - Refactored `scheduler_uv_poll_cb` / `scheduler_uv_timer_cb` to enqueue-only.
  - Wired wakeup draining into scheduler loops (`scheduler_run_until`, `scheduler_run_all`) and ring reset in idle reset path.
- **Atomic CAS correctness** (`src/lisp/threads.c3`):
  - `atomic-cas!` now uses true single-operation compare-exchange (`mem::compare_exchange`) instead of load/compare/store.
- **Regression coverage** (`src/lisp/tests_tests.c3`):
  - Added wakeup ring smoke test.
  - Added ring wraparound/overflow rejection test.
  - Added timer wakeup drain-transition test.
  - Added atomic CAS mismatch-preserves-value regression test.

### Invariants Enforced
1. libuv callbacks do not allocate Omni values or touch scope/region-owned runtime objects.
2. libuv callbacks do not perform fiber state transitions.
3. `c_recv` for async `tcp-read` is performed only in scheduler drain phase.
4. Cross-thread wake signaling uses atomics + `uv_async_send`.
5. Effect names and fast-path dispatch (`io/tcp-*`) remain unchanged.

### Files Modified
- `src/lisp/async.c3`
- `src/lisp/scheduler.c3`
- `src/lisp/threads.c3`
- `src/lisp/tests_tests.c3`

## 2026-03-03: Structured Fiber spawn/await Lifetime Model

### Summary
Implemented structured parent-child lifetimes for fibers: child tasks are scoped to the spawning fiber, in-fiber `await` suspends until child completion, and parent completion is deferred until all live children finish.

### Changes
- **Scheduler parent/child tracking** (`src/lisp/scheduler.c3`):
  - Extended `FiberEntry` with `parent_id`, `live_children`, and await/wait bookkeeping.
  - `spawn` now records parent fiber when called from a scheduler-managed running fiber.
  - Parent live-child count increments on child spawn and decrements on child completion.
- **Structured await semantics** (`src/lisp/scheduler.c3`):
  - `await` inside a running fiber now blocks/yields the current fiber until target completes.
  - In-fiber await is restricted to direct children (`await` on non-child now errors).
- **Parent completion gating** (`src/lisp/scheduler.c3`):
  - Fibers that return while children are still live transition to blocked waiting state.
  - Final result is held until last child completes, then parent is marked done.
- **Regression tests** (`src/lisp/tests_tests.c3`):
  - Added scheduler tests for:
    - direct child await,
    - parent waiting for unawaited child,
    - non-child await rejection.

### Files Modified
- `src/lisp/scheduler.c3`
- `src/lisp/tests_tests.c3`

### Validation
- `c3c build` ✅
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` ✅
  - Unified tests: `1120 passed, 0 failed`
  - Compiler tests: `73 passed, 0 failed`

## 2026-03-03: Fiber tcp-read Timeouts via libuv Timers

### Summary
Added timeout-capable async `tcp-read` for scheduler-managed fibers using `uv_timer`.

### Changes
- **Scheduler timeout wiring** (`src/lisp/scheduler.c3`):
  - Extended pending async read state with `timed_out` + `timer_handle`.
  - Added `scheduler_uv_timer_cb` to complete blocked reads on deadline expiry.
  - Shared completion path now closes both poll and timer handles and wakes blocked fibers.
  - `scheduler_try_async_tcp_read` now accepts `timeout_ms`, initializes/starts `uv_timer` when requested, and returns `"tcp-read: timeout"` on expiry.
- **Async primitive support** (`src/lisp/async.c3`):
  - Added libuv timer externs/constants already used by scheduler bridge (`uv_timer_init/start/stop`, `UV_HANDLE_TIMER`).
  - Added `(tcp-read-timeout handle timeout-ms)` primitive (`prim_tcp_read_timeout`) delegating to `tcp-read` with timeout.
- **Timeout entry points**:
  - `prim_tcp_read` now accepts optional `timeout-ms` as third arg and routes it through scheduler async bridge.
  - Added `prim_tcp_read_timeout` helper primitive implementation in `src/lisp/async.c3` (not wired into stdlib effect table yet).

### Files Modified
- `src/lisp/async.c3`
- `src/lisp/scheduler.c3`

### Validation
- `c3c build` ✅
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` (full runtime tests) ✅
  - Unified tests: `1117 passed, 0 failed`
  - Compiler tests: `73 passed, 0 failed`
- Targeted timeout smoke checks ✅
  - Silent peer + `(__raw-tcp-read h 4096 100)` produces `tcp-read: timeout`
  - Responding peer + `(__raw-tcp-read h 4096 1000)` returns data (`length = 2`)

## 2026-03-03: Fiber Suspend Lifetime Pinning + libuv tcp-read Bridge

### Summary
Hardened coroutine/fiber suspend lifetimes and wired the first real scheduler-to-libuv async I/O path for `io/tcp-read`.

### Changes
- **Suspend-point scope pinning**:
  - Added `pinned_scope` to `main::StackCtx`.
  - Added `stack_ctx_pin_scope` / `stack_ctx_unpin_scope`.
  - `stack_ctx_destroy` now always unpins retained scope references.
  - Applied pin/unpin around every coroutine suspension path used by yield/effects:
    - `prim_yield` (`src/lisp/primitives.c3`)
    - `jit_shift_impl` + stack-context effect suspend path (`src/lisp/jit_jit_helper_functions.c3`)
    - `jit_shift_value` + `jit_signal_value` (`src/lisp/jit_jit_compiler.c3`)
- **Coroutine abandonment cleanup**:
  - `COROUTINE` values now run teardown in `scope_dtor_value` (free thunk state + return `StackCtx` to pool).
  - `make_coroutine` now allocates wrapper values in `root_scope` with registered destructor to ensure cleanup even when abandoned.
- **Scheduler async bridge (MVP)**:
  - Reworked scheduler state machine to `ready/running/blocked/done`.
  - Added libuv loop initialization in scheduler bootstrap.
  - Added pending async read slots keyed by fiber id.
  - Added non-blocking read fast probe + `uv_poll` registration path for fiber-local `tcp-read`.
  - Fiber calling `tcp-read` inside scheduler now:
    1. yields and transitions to `blocked`,
    2. wakes on libuv readability callback,
    3. resumes and returns read result.
  - Callback path only manipulates C buffers/state and marks fibers ready; Omni value allocation happens on scheduler resume path.
- **Async extern plumbing**:
  - Added `uv_poll_*`, `uv_handle_size`, `uv_close`, and errno helpers in `src/lisp/async.c3`.
  - `prim_tcp_read` now attempts scheduler async path first, then falls back to blocking path outside scheduler fibers.

### Files Modified
- `src/stack_engine.c3`
- `src/lisp/primitives.c3`
- `src/lisp/jit_jit_helper_functions.c3`
- `src/lisp/jit_jit_compiler.c3`
- `src/lisp/value.c3`
- `src/lisp/async.c3`
- `src/lisp/scheduler.c3`

### Validation
- `c3c build` ✅
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` (full runtime test mode) ✅
  - Stack engine: 10 pass / 0 fail
  - Scope region: 50 pass / 0 fail
  - Unified Lisp tests: 1117 pass / 0 fail
  - Compiler tests: 73 pass / 0 fail
- Targeted smoke scripts ✅
  - fiber `spawn/await`
  - fiber `tcp-connect` + `tcp-write` + `tcp-read`

## 2026-03-03: Unified Deduce API + Destructuring in let/lambda/define

### Summary
Unified all `deduce-*` primitives into a single `(deduce 'command args...)` dispatch form. Added array and dict destructuring to `let` bindings and dict destructuring to function params (lambda/define).

### Changes
- **Unified deduce primitive**: `(deduce 'open ...)`, `(deduce 'scan ...)`, `(deduce 'query ...)`, `(deduce 'count ...)`, `(deduce 'match ...)`, `(deduce 'fact! ...)`, `(deduce 'retract! ...)`. Single primitive dispatches on first quoted symbol. Legacy primitives kept for backward compat.
- **Array destructuring in let**: `(let ([x y] [10 20]) ...)`, `(let ([head .. tail] '(1 2 3)) ...)`. Parser accepts `[` in let binding position, reuses PAT_SEQ pattern matcher.
- **PAT_SEQ now matches ARRAY values**: `match_seq_pattern` and `get_list_rest` support both CONS lists and ARRAY (contiguous) values.
- **Dict destructuring pattern (PAT_DICT)**: `{name age}` in pattern position extracts `'name` and `'age` keys from dict. Works in `let` and `match`.
- **Dict destructuring in let**: `(let ({name age} {'name "Alice" 'age 30}) ...)`.
- **Dict destructuring in function params**: `(define (f {x y} z) ...)` and `(lambda ({x y}) ...)`. Desugars to positional param + let-destruct in body. Multiple dict params allowed, mixed with positional.
- Removed debug print from `prim_define_relation`.

### Files Modified
- `src/lisp/deduce.c3` — unified `prim_deduce` dispatcher, removed debug print
- `src/lisp/eval.c3` — PAT_DICT in match_pattern, ARRAY support in match_seq_pattern/get_list_rest, registered `deduce` prim
- `src/lisp/value.c3` — PAT_DICT enum, dict_keys/dict_key_count in Pattern, pattern field in ExprLet
- `src/lisp/parser_parser.c3` — `{` in parse_pattern (PAT_DICT), `[`/`{` in parse_let, `{` in lambda/define param parsing with body desugaring
- `src/lisp/jit_jit_compiler.c3` — destructuring let fallback to interpreter helper
- `src/lisp/jit_jit_helper_functions.c3` — `jit_eval_let_destruct` helper
- `src/lisp/tests_tests.c3` — 18 updated/new tests (9 deduce, 5 let array, 4 let dict, 4 function dict params)

### Test Count
- Before: 1159 PASS, 0 failures
- After: 1172+ PASS, 0 failures

## 2026-03-03: Documentation Sync — Memory Model Completion

### Summary
Synchronized architecture docs and agent guidance with the implemented runtime.

### Changes
- Updated `memory/DESTINATION_ARENA_PLAN.md` with a new implementation-closure
  section (`Revision XIV`) that records:
  - dual-lane status,
  - shared promotion-context behavior,
  - root-boundary safety semantics,
  - env-copy memoization behavior,
  - telemetry tightening,
  - required regression gates and verification baseline.
- Updated `AGENTS.md` so memory/lifetime work explicitly references:
  - `memory/DESTINATION_ARENA_PLAN.md` for current architecture truth,
  - `memory/CHANGELOG.md` for implementation history.
- Added explicit memory-work test expectations in `AGENTS.md`:
  `c3c build`, `c3c build --sanitize=address`, and full runtime suite.

---

## 2026-03-03: Memory Polish Finalization (Dual-Lane + Promotion Context)

### Summary
Completed the remaining production-polish pass for the dual-lane scope model with promotion-context fallback. The runtime now has tighter copy telemetry, safer root-boundary promotion semantics, lower env-copy churn on shared captures, and less ambiguous escape-env control in named-let call scopes.

### Changes
- **Defensive root promotion restored** (`eval.c3`):
  - `promote_to_root_site` now routes through `copy_to_parent_site` with `releasing_scope` set, preserving defensive-copy behavior for disjoint transient lifetimes.
- **Disjoint-scope regression gate added** (`tests_tests.c3`):
  - `lifetime: root-boundary promotion defends disjoint scope`
- **Copy-site telemetry compacted** (`eval.c3`):
  - Removed dead buckets: `COPY_SITE_JIT_CALL_SCOPE_STEP1`, `COPY_SITE_JIT_TCO_ESCAPE_REFRESH`
  - Added `COPY_SITE_COUNT` bounds checks and compacted active indices.
- **Telemetry storage tightened** (`eval.c3`):
    - `site_calls` now uses `COPY_SITE_COUNT` capacity (no slack slots).
- **Env copy churn reduction** (`eval.c3`):
  - `copy_env_to_scope` now uses a shared `PromotionContext` epoch for recursive copy.
  - Shared values are memoized at boundary copy sites, reducing repeated deep copy.
  - Added target-chain fast reuse for env value copies.
  - Closure/iterator copy paths now skip deep copy when value is already in target scope chain.
- **Env copy non-regression gate added** (`tests_tests.c3`):
  - `lifetime: copy_env shared-value memo gate`
- **Escape-env control cleanup** (`jit_jit_helper_functions.c3`):
  - `jit_eval_in_call_scope` now takes explicit `enable_escape_env`.
  - Named-let now passes `!body_has_shift` directly instead of outer toggle layering.

### Verification
- `c3c build` passed.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed:
  - Unified tests: `1051 passed, 0 failed`
  - Compiler tests: `73 passed, 0 failed`
- `c3c build --sanitize=address` passed.
- `ASAN_OPTIONS=detect_leaks=0 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed with the same totals.

---

## 2026-03-02: M1.6b — Inline String Descriptor into Value Union

### Summary
Eliminated one `malloc`/`free` per string by inlining the string descriptor (chars pointer + length) directly into the Value union. Previously, each STRING/ERROR Value held a `StringVal*` pointer to a heap-allocated struct containing `chars`, `len`, `capacity`. Now `str_chars` and `str_len` live directly in the Value union, removing the indirection.

### Changes
- **Value union** (`value.c3`): Replaced `StringVal* str_val` with inline `struct { char* str_chars; usz str_len; }`
- **make_string / make_error** (`value.c3`): Removed `StringVal` malloc — write directly to `v.str_chars`/`v.str_len`
- **scope_dtor_value** (`value.c3`): Simplified STRING/ERROR case — just free `str_chars` (no struct free)
- **strval_into_value** (`primitives.c3`): Now transfers `chars`/`len` from builder, then frees the `StringVal` struct
- **prim_unsafe_free** (`primitives.c3`): Updated destructor to match new layout
- **Parser** (`parser.c3`): 5 string literal sites converted to use local `StringVal* builder` + `strval_into_value()`
- **Bulk rename across 17 files**: `.str_val.chars` -> `.str_chars`, `.str_val.len` -> `.str_len`
- **StringVal struct retained**: Still used as string builder API (`strval_new`, `strval_push`, etc.)

### Files Modified
- `src/lisp/value.c3` — Value union, make_string, make_error, scope_dtor_value
- `src/lisp/primitives.c3` — strval_into_value, prim_unsafe_free
- `src/lisp/parser.c3` — 5 string literal construction sites
- `src/lisp/eval.c3` — copy_to_parent STRING/ERROR cases
- `src/lisp/jit.c3` — all str_val.chars/len references
- `src/lisp/tests.c3`, `src/lisp/compiler.c3`, `src/lisp/async.c3`, `src/lisp/tls.c3`
- `src/lisp/http.c3`, `src/lisp/json.c3`, `src/lisp/compress.c3`, `src/lisp/unicode.c3`
- `src/lisp/schema.c3`, `src/lisp/unify.c3`, `src/lisp/deduce.c3`
- `src/pika/lisp_pika.c3`

### Test Count
- Before: 1037 unified + 73 compiler + 9 stack + 40 scope = 1159 PASS
- After: 1037 unified + 73 compiler + 9 stack + 40 scope = 1159 PASS (no change)

---

## 2026-03-02 (Session 67): Full Library Integration — 6 Libraries, 41 New Primitives

### Summary
Integrated 6 external libraries into Omni: Pika regex/grammar, utf8proc (Unicode), yyjson (JSON), libdeflate (compression), libuv (TCP/DNS/timers), BearSSL (TLS). All I/O through effects with dispatch table fast-path. Refactored hardcoded effect fast-path (14 else-if branches) into a generic dispatch table.

### Libraries Integrated
- **Pika** (src/pika/, 11 files): Regex + PEG grammar engine, copied from pika.bak/ with API fixes
- **utf8proc** (src/lisp/unicode.c3): Unicode string ops replacing ASCII-only implementations
- **yyjson** (src/lisp/json.c3 + csrc/json_helpers.c): JSON parse/emit with native Omni types
- **libdeflate** (src/lisp/compress.c3): gzip/deflate compression
- **libuv** (src/lisp/async.c3): TCP/DNS/timer with POSIX blocking fast-path
- **BearSSL** (src/lisp/tls.c3 + csrc/tls_helpers.c): TLS client connections

### Architecture: Effect Fast-Path Dispatch Table
- Replaced 14 hardcoded `else if` branches in `jit_signal_impl` with `FastPathEntry[32]` table
- `register_fast_path(interp, "io/foo", "__raw-foo")` — zero JIT changes for new effects
- Arity-based dispatch: 0→nil, 1/-1→direct, 2+→curry via cons pair
- Removed 22 individual Interp fields (8 sym_io_* + 14 raw_*), replaced with 2 fields

### Files Modified
- `project.json` — added utf8proc, deflate, yyjson, uv, bearssl to linked-libraries; json/tls c-sources
- `src/lisp/eval.c3` — register 22 new primitives, register_fast_path(), import pika
- `src/lisp/value.c3` — FastPathEntry struct, fast_path_table on Interp, removed old sym_io_*/raw_* fields
- `src/lisp/jit.c3` — replaced 2 fast-path chains with table lookups
- `src/lisp/tests.c3` — 4 new test suites (pika, unicode, compression, json, async)
- `stdlib/stdlib.lisp` — 11 new effect declarations + user-facing wrappers

### Test Count
Before: 1078 (956 unified + 73 compiler + 9 stack + 40 scope)
After: 1120 (998 unified + 73 compiler + 9 stack + 40 scope) — 42 new tests, 0 failures

---

## 2026-03-02 (Session 66): Scope Adoption at Return — O(n) → O(1) Function Return

### Summary
Replaced `copy_to_parent` at function return with conditional `scope_adopt` in both `jit_eval_in_call_scope` (Step 2) and `jit_eval_in_single_scope`. Only CONS returns use adoption (the only type where copy_to_parent is O(n)). Scalar returns use the O(1) copy + release path, freeing all temporaries.

Previous attempt (Session 65 Exploration 5) tried adding `value_rc` / deferred free to ScopeRegion, causing heap corruption during macro expansion. This new approach uses the already-proven `scope_adopt` mechanism (used at Step 1 since Session 62) with no new fields, refcounts, or lifetime semantics.

### Changes
- `jit_eval_in_call_scope` Step 2: When `result_scope.refcount == 1` AND result is CONS, adopt chunks into caller's scope (O(1)). All other returns use O(1) copy + release (frees temporaries).
- `jit_eval_in_single_scope`: Same conditional pattern — adopt only for CONS returns.
- Removed outdated comment about Exploration 5 being "too risky".

### Garbage mitigation
Adoption only triggers for CONS returns (~20% of calls). Scalar returns (~80%) use copy + release, freeing all temporaries. Dead temps from CONS-returning functions are freed at TCO bounce (scope_reset) or when caller's scope dies.

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/jit.c3` | conditional scope_adopt at Step 2 + single_scope return (~15 lines) |

### Test results
- 956 unified + 73 compiler + 9 stack + 40 scope = 1078 PASS, 0 failures

## 2026-03-01 (Session 65): Exploration 7C - Mutate-in-Place (Skipped)

### Summary
Evaluated "Exploration 7C: Mutate-in-Place" which proposed skipping `copy_to_parent` on binding values during the `copy_tco_env_chain` fast path for lambda-free loop bodies. The premise was that new iteration values would overwrite the previous iteration's values, meaning the old values could be safely freed without copying.

### Findings
This exploration was skipped because its core assumption about the TCO architecture is incorrect:
1. `copy_tco_env_chain` does NOT operate on the "old" environment from the previous iteration. It operates on the `new_env` created by `Env.extend()` in `jit_apply_value_tail`.
2. The values being processed in `copy_tco_env_chain` are the **NEW** values for the next iteration, which were evaluated inside the highly transient `call_scope`.
3. If we skip `copy_to_parent` for these bindings, the new values would be left in `call_scope`, which is immediately cleared by the TCO bounce trampoline. This would result in dangling pointers and complete evaluation failure on the very next iteration.
4. Because the `new_env` contains the new values, `copy_to_parent` is strictly required to rescue them from the dying `call_scope` into the surviving `escape_scope`.
5. True mutate-in-place would require rewriting `jit_apply_value_tail` and `Env` semantics to actually overwrite the previous environment's bindings instead of allocating a new `Env` frame, which breaks lexical scoping invariants for any captured continuations or async fibers.

The O(N) overhead of `copy_to_parent` on TCO args is already minimized by the inline bindings optimization and the fast-path liveness checks, making this complex refactoring unnecessary.

## 2026-03-01 (Session 65): Exploration 5 - Iterative O(n) Copy Optimization

### Summary
Evaluated "Exploration 5: Dual Refcount / Deferred Free" which aimed to eliminate the `O(n)` copy in `jit_eval_in_call_scope` via scope lifetime tracking (`value_rc`) or `scope_adopt`. However, modifying scope lifetimes or adopting chunks across complex TCO unwinds proved too risky, leading to non-deterministic double frees and heap corruption (`corrupted double-linked list` during macro expansion and deep unwinding).
Instead of Deferred Free, I mitigated the actual bottleneck (C stack overflow) by rewriting `copy_to_parent`'s `CONS` copying logic to be purely iterative. This safely retains deterministic memory management while preventing crashes on 50,000+ element lists.

### Changes
- Rewrote `case CONS:` in `copy_to_parent` (`src/lisp/eval.c3`) to use an iterative loop (`while (true)`) instead of tail-recursive calls, preventing C stack overflow on massive TCO loops (e.g., `range-to 50000`).
- Fixed a major memory leak in `scope_create` (`src/scope_region.c3`) where freelisted scopes would unconditionally overwrite their reused first chunk, leading to leaked chunks and potential allocator pressure.
- Documented the abandonment of Deferred Free in `jit_eval_in_call_scope` as the iterative copy solves the crash risk with zero added architectural complexity.

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/eval.c3` | Made `copy_to_parent` for `CONS` iterative, added missing `break` in `ITERATOR` case |
| `src/scope_region.c3` | Fixed `scope_create` to properly reuse chunks from the freelist |

## 2026-03-01 (Session 65): Exploration 6 - Closure Scratch Arena

### Summary
Eliminated per-closure `malloc(Closure)` by bump-allocating `Closure` structs in the scope arena. All standard closures (and their inner parameters/signatures) are now exclusively bump-allocated and freed exactly when their creating scopes die. Deep-copy semantics and robust scope tracking via `scope_contains` solve the complex lifecycle challenges of escaping closures and named-let TCO loops.

### Changes
- Replaced heap allocation of `Closure` in `make_closure`, `make_closure_no_param`, and `jit_make_closure_from_expr` with `interp.current_scope.alloc()`.
- Replaced `Closure.refcount` with `Closure.scope_gen` to enable O(1) liveness checking without RC overhead.
- Added `scope_dtor_closure` to safely release the inner `env_scope` when the bump-allocated closure dies.
- Added `scope_contains(ScopeRegion*, void*)` to `src/scope_region.c3` to correctly detect memory that was `scope_adopt`ed from a child scope.
- Fixed `copy_to_parent` for closures: it now performs a full structural deep-copy when returning closures that are trapped inside a dying or adopted scope (`scope_gen` or `scope_contains` matches `releasing_scope`).
- Fixed `copy_env_to_scope` to recursively deep-copy closures and iterators when building captured environments.
- Handled edge cases where zero-param closures need `params = null` explicitly during deep copies to avoid dangling pointers.
- Fixed `jit_eval_let_rec` memory leak/dangling pointer issues by explicitly deep-copying the loop body closure into the `env_scope` and removing dtors to prevent cycle memory leaks.
- Set `interp.releasing_scope` properly in `eval.c3` `run` loop and `jit.c3` `jit_eval_define` to ensure globals created dynamically survive TCO and execution boundaries.

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/value.c3` | Added `scope_dtor_closure`, updated `Closure` struct, modified `make_closure` |
| `src/lisp/eval.c3` | Re-wrote `copy_to_parent` closures/iterators, added scope checks to `run` and `copy_env_to_scope` |
| `src/lisp/jit.c3` | Adjusted `jit_make_closure_from_expr`, fixed `jit_eval_let_rec` TCO loops, added `releasing_scope` to env extensions |
| `src/scope_region.c3` | Added `scope_contains` |

## 2026-03-01 (Session 65): Exploration 2 - JIT Accumulator Hint

### Summary
Skipped two-scope (result_scope + call_scope) setup for non-accumulator function calls, avoiding the overhead of creating and destroying two scopes per normal function application.

### Changes
- Added `jit_eval_in_single_scope(body, env, interp)` in `src/lisp/jit.c3` which creates just ONE child scope (no result_scope, no escape_scope, no tco_recycle_scope).
- Replaced `jit_eval_in_call_scope` with `jit_eval_in_single_scope` in `jit_apply_value_impl` and `jit_apply_multi_args` for regular closure applications.
- Retained `jit_eval_in_call_scope` for `jit_eval_let_rec` (which needs the two-scope setup for TCO recycling and accumulator cell redirection).

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/jit.c3` | Added `jit_eval_in_single_scope`, modified closure application paths |

## 2026-03-01 (Session 65): Exploration 7B - Inline Bindings

### Summary
Implemented inline bindings for `Env` structs to eliminate `malloc(Binding[])` and `scope_register_dtor` for small environment frames (<=4 bindings). Since most `Env` frames are small (1-3 bindings for lambda parameters and let bindings), the vast majority of frame allocations now bypass the heap and destructor registration entirely.

### Changes
- Added `Binding inline_bindings[4]` and `bool is_inline` flag to `Env` struct in `src/lisp/value.c3`.
- Updated `ENV_INITIAL_CAPACITY` from 8 to 4.
- Modified `make_env` to set `env.bindings = &env.inline_bindings`, `env.capacity = 4`, and `env.is_inline = true`. No malloc, no dtor for bindings array.
- Updated `Env.define` to transition from inline array to heap array only when `binding_count` exceeds 4.
- Modified `scope_dtor_env` to skip `free(env.bindings)` when `is_inline` is true.
- Updated `copy_env_to_scope` in `eval.c3` to use the inline path for small copied frames.

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/value.c3` | Added inline bindings array to `Env`, updated `ENV_INITIAL_CAPACITY`, modified `make_env`, `Env.define`, and `scope_dtor_env` |
| `src/lisp/eval.c3`  | Modified `copy_env_to_scope` to respect inline size threshold |

## 2026-03-01 (Session 64): Escape-Scope Env Optimization (Unified 10+7A)

### Summary
Implemented the escape-scope env optimization from Explorations 10+7A. Named-let loop envs are now allocated in escape_scope (result_scope) so they survive TCO bounces without `copy_tco_env_chain` frame copy. Per-bounce cost drops from ~100 instructions (make_env + malloc + dtor + copies) to ~10 instructions (scope_gen check + in-place value updates).

### Changes
- **Phase 1**: Added `scope_gen` (uint) field to Env struct — stamped from `current_scope.generation` at allocation. O(1) scope membership check. Also stamped in `alloc_env()` for env copies.
- **Phase 2a**: Added `expr_contains_shift()` helper — recursive AST walker to detect E_SHIFT in named-let bodies. Lambda/Reset/Handle return false (closure/delimited boundaries). Used for Piece 6 safety (shift detection).
- **Phase 2b-f**: Added `escape_env_mode` bool to Interp. Modified `make_env` to redirect env allocation to `escape_scope` when flag is set. Set in `jit_eval_let_rec` (guarded by shift check). Save/restore at all 7 context boundary sites (reset, shift, continuation, signal, handle body, AOT resolve).
- **Phase 3**: Added fast path in `copy_tco_env_chain` — when env's `scope_gen` matches `escape_scope.generation`, skips frame copy entirely (no make_env, no malloc, no dtor, no hash table rebuild). Instead promotes binding values in-place via `copy_to_parent`.
- **Key fix during development**: `jit_eval_in_call_scope` is on the hot recursion path — adding ANY local variable (even 1 bool) increases C stack frame size enough to cause native stack overflow on 5000-deep recursion tests. Solution: `jit_eval_let_rec` saves/restores `escape_env_mode` instead.
- **Key fix**: Fast path check must be `src.scope_gen == escape_scope.generation` (env IS in escape_scope), NOT `src.scope_gen != releasing_scope.generation` (env is NOT in dying scope). The latter incorrectly catches closure env_scope envs.

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/value.c3` | `scope_gen` on Env, `escape_env_mode` on Interp, `make_env` escape redirect, `alloc_env` stamps scope_gen |
| `src/lisp/jit.c3` | `expr_contains_shift`, `copy_tco_env_chain` fast path, `jit_eval_let_rec` escape_env, 7 context boundary save/restores |
| `src/lisp/tests.c3` | 13 new escape-scope env tests |

### Test results
956 unified + 73 compiler + 9 stack + 40 scope = **1078 PASS, 0 failures** (was 1065)

---

## 2026-03-01 (Session 63 continued): Dialectic Analysis — Dual Refcount, Closure Arenas, Env Chain

### Summary
Extended dialectic reasoning analysis to three optimization areas: (1) eliminating copy_to_parent via dual refcount with selective deferred free, (2) eliminating per-closure malloc via scratch arena + RC with batch promote, (3) eliminating copy_tco_env_chain overhead via inline bindings + escape-scope env allocation. All validated through 4-round multi-provider dialectic (groq/deepseek/zai).

### Changes
- **Exploration 5** (Dual Refcount): `structural_rc` + `value_rc` on ScopeRegion. Large scopes skip copy_to_parent entirely — deferred free keeps scope alive until escaped values are released. Flat DAG alternative rejected (TCO causes unbounded memory growth).
- **Exploration 6** (Closure Scratch Arena + RC): Bump-allocate Closures in scope arena instead of malloc. Keep RC for shared ownership. Batch-scan at scope death promotes shared closures (rc>1), ephemeral closures (rc=1) freed in bulk with arena. Variant B (optimistic scratch) rejected — fatal with continuations/coroutines (dangling pointers after stack clone).
- **Exploration 7** (Inline Bindings + Escape-Scope Env): Embed small binding arrays (≤4) directly in Env struct (no malloc, no dtor). Allocate named-let env frames in escape_scope so they survive TCO bounces. Optional C: mutate-in-place for lambda-free loop bodies. Per-bounce cost drops from ~100 to ~10 instructions. Promoted to #2 priority.
- **Exploration 8** (Separated Name-Value Storage): Split Binding into immutable SymbolId[] + mutable Value*[]. Halves copy cost. High refactor surface (all env access sites).
- **Exploration 9** (Loop Register Slots): Compile named-let to pre-allocated Value* slots. Zero per-bounce allocation for lambda-free loops. Dialectic validated (4 rounds, 0.8 confidence). Multi-shot continuations are the hard edge case — need snapshot at shift time. Common case (reverse, map, fib, etc.) gets true zero-alloc.
- **Exploration 10** (Scope-Gen Stamps on Env): Add scope_gen to Env struct for O(1) skip in copy_tco_env_chain. Trivial extension of Exploration 1. No scope_adopt issue (Envs aren't adopted).
- **Unified Design (10+7A)**: Graph-of-thoughts brainstorming (25 nodes, groq/deepseek/zai) converged on "Slot-Backed Env" — Env in escape_scope with scope_gen stamp. Followed by dialectic stress-test (4 rounds). Key finding: the "value promotion problem" is NOT a new codepath — it's the existing copy_to_parent loop targeting escape_scope instead of call_scope. ~8 lines of code eliminates ~90% of per-bounce cost. **Largely obsoletes Exploration 9** (Loop Register Slots) which needed a second codepath, closure materialization, and JIT changes.
- **C1 solution** (Piece 6): Graph-of-thoughts converged on detect-and-fall-back — if shift in named-let body, don't use escape_scope for loop Env. Zero cost for common case.
- **C7 solution** (Piece 7): Graph-of-thoughts converged on continuation holds scope reference — bump escape_scope.refcount at shift, decrement when StackCtx freed. Minimal change (~6 lines).
- C1+C7 compose perfectly: loops with shift fall back to call_scope Env (no shared mutable state), while escape_scope stays alive for accumulator cons cells.
- Restructured priority order into 4 tiers. Explorations 10+7A promoted to Tier 1 (#2 priority).

### Files modified
| File | Changes |
|------|---------|
| `memory/ESCAPE_SCOPE_EXPLORATIONS.md` | Added Explorations 6-10, unified design (10+7A), 4-tier priority order |

### Test results
943 unified + 73 compiler + 9 stack + 40 scope = **1065 PASS, 0 failures** (unchanged — research only)

---

## 2026-03-01 (Session 63): Scope Generation Stamps — O(1) make_cons escape check

### Summary
Added `uint scope_gen` field to Value struct (fits in existing padding, zero size increase). Stamped at allocation with globally unique generation from monotonic counter. Replaces `is_in_scope()` chunk-list walk with O(1) `uint == uint` comparison in `make_cons` hot path. `copy_to_parent` retains `is_in_scope` because `scope_adopt` moves chunks between scopes without updating value stamps.

### Changes
- **Value struct** (`value.c3:596`): Added `uint scope_gen` field after `tag` — fits in 7-byte padding, struct stays 40 bytes
- **alloc_value/alloc_value_root** (`value.c3:2403,2414`): Stamp `scope_gen` from `current_scope.generation` / `root_scope.generation`
- **make_cons** (`value.c3:791`): Replaced `is_in_scope(cdr, escape_scope)` chunk walk with `cdr.scope_gen == escape_scope.generation` — O(1)
- **scope_region.c3**: Added `g_scope_generation_counter` global monotonic counter; `scope_create` assigns `++g_scope_generation_counter` for globally unique generations (fixes collision bug where recycled scopes shared low generation numbers)
- **copy_to_parent** (`eval.c3:1078`): Kept `is_in_scope` (chunk walk) — scope_adopt moves chunks without updating stamps, so scope_gen can't be used here
- **Dialectic reasoning analysis**: Explored 5 optimization directions (scope_gen, JIT hint, arena zero-copy, escape binding flag, lazy adoption). Wrote up in `memory/ESCAPE_SCOPE_EXPLORATIONS.md`. Fixed reasoning-tools service config (timeouts, concurrency, missing API keys).

### Bug found and fixed during implementation
