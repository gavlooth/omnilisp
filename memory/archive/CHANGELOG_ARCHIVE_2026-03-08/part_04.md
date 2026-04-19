    - added Signals section with usage snippet and semantics.
  - `docs/reference/11-appendix-primitives.md`
    - added `__raw-signal-handle` / `__raw-signal-unhandle`.
  - `docs/reference/12-appendix-stdlib.md`
    - added `signal-handle` / `signal-unhandle` to wrapper list.
  - `docs/plans/library-gaps-todo.md`
    - marked `A4 / Add signal primitives` complete.

### Validation
- Build:
  - `c3c build` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1358/0`
    - `compiler`: `76/0`
- ASAN build:
  - `c3c build --sanitize=address` succeeded.
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `20/0`
    - `scope_region`: `51/0`
    - `unified`: `1357/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 246 - A4 Process `env` Override Support

### Summary
Completed the remaining A4 process primitive gap by wiring `process-spawn` env
override through runtime argument parsing into libuv process spawn options.

### What changed
- Runtime process env plumbing
  - `src/lisp/async.c3`
    - updated `prim_process_spawn` to accept:
      - `env = nil` (inherit parent process environment), or
      - `env` as list/array of strings/symbols (`KEY=VALUE` entries).
    - added env sequence validation and conversion:
      - `io/process-spawn-env-must-be-string-sequence`
      - message: `process-spawn: env must be nil, list, or array of strings/symbols`
    - builds and passes a null-terminated `char** envp` to:
      - `omni_uv_process_spawn(file, argv, envp, ...)`.
- Regression coverage update
  - `src/lisp/tests_advanced_tests.c3`
    - replaced prior rejection test with positive behavior test:
      - `process spawn env override`
    - verifies env entry (`OMNI_PROC_TEST=ok`) is visible in child process and
      exit code is successful.
- Documentation/plan parity
  - `docs/reference/07-io-networking.md`
    - removed old env limitation note.
    - documented `env` semantics for `process-spawn` (`nil` inherit vs explicit override).
  - `docs/plans/library-gaps-todo.md`
    - marked `A4 / Add process primitives` complete.

### Validation
- Build:
  - `c3c build` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1357/0`
    - `compiler`: `76/0`
- ASAN build:
  - `c3c build --sanitize=address` succeeded.
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `20/0`
    - `scope_region`: `51/0`
    - `unified`: `1356/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 245 - A4 Process Primitive Surface (libuv-backed spawn/wait/kill + stdio handles)

### Summary
Implemented the next Track A slice by adding process primitives wired through a
libuv-backed native wrapper and exposing process stdio as existing `fs-handle`
values.

### What changed
- libuv process wrapper substrate
  - `csrc/uv_helpers.c`
    - added process wrapper API:
      - `omni_uv_process_spawn(file, argv, envp, out_process, stdin_fd, stdout_fd, stderr_fd)`
      - `omni_uv_process_wait(process, exit_status, term_signal)`
      - `omni_uv_process_kill(process, sig)`
      - `omni_uv_process_pid(process)`
      - `omni_uv_process_close(process)`
    - implementation uses `uv_spawn` + `uv_process_t` lifecycle with explicit close/wait cleanup.
- Runtime primitives
  - `src/lisp/async.c3`
    - added process-handle lifecycle (`ProcessHandle`) with deterministic finalizer close.
    - added primitives:
      - `(process-spawn cmd args env)`
      - `(process-wait handle-or-process-dict)`
      - `(process-kill handle-or-process-dict sig)`
    - `process-spawn` returns dict:
      - `'handle` process handle
      - `'pid` child pid
      - `'stdin`/`'stdout`/`'stderr` as `fs-handle` values (compatible with `fs-read`/`fs-write`/`fs-close`)
    - migration: `process-wait`/`process-kill` accept either direct process handle or the returned process dict.
    - current limitation made explicit:
      - `env` override must be `nil` for now (`io/process-spawn-env-not-supported`).
- Primitive/effect registration
  - `src/lisp/eval_init_primitives.c3`
    - registered:
      - `__raw-process-spawn`
      - `__raw-process-wait`
      - `__raw-process-kill`
    - added effect fast-path mappings:
      - `io/process-spawn -> __raw-process-spawn`
      - `io/process-wait -> __raw-process-wait`
      - `io/process-kill -> __raw-process-kill`
    - fast-path count updated: `42 -> 45`.
- Stdlib wiring
  - `stdlib/stdlib.lisp`
    - added effect declarations:
      - `io/process-spawn`, `io/process-wait`, `io/process-kill`
    - added wrappers:
      - `process-spawn`, `process-wait`, `process-kill`
    - fixed `process-kill` wrapper payload shape to packed list form.
- Regression coverage
  - `src/lisp/tests_advanced_tests.c3`
    - added:
      - `process spawn stdio + wait`
      - `process spawn env currently rejected`
      - `process kill terminates child`
- Documentation parity
  - `docs/reference/07-io-networking.md`
    - added UDP, Unix socket, and process control sections.
  - `docs/reference/11-appendix-primitives.md`
    - added `__raw-udp-*`, `__raw-pipe-*`, `__raw-process-*` entries and corrected arities.
  - `docs/reference/12-appendix-stdlib.md`
    - added `udp-*`, `pipe-*`, and `process-*` wrappers.
  - `docs/plans/library-gaps-todo.md`
    - marked `A4 / expose process stdio handles` complete.
    - left `A4 / process primitives` open due pending env override support.

### Validation
- Build:
  - `c3c build` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1357/0`
    - `compiler`: `76/0`
- ASAN build:
  - `c3c build --sanitize=address` succeeded.
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `20/0`
    - `scope_region`: `51/0`
    - `unified`: `1356/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 244 - A3 Readiness/Wakeup Unification (`udp-recv` Fiber Path)

### Summary
Completed the remaining A3 socket-readiness unification item by routing
`udp-recv` fiber execution through the same libuv poll + reliable wakeup bridge
used by `tcp-read`.

### What changed
- Scheduler async bridge reuse
  - `src/lisp/scheduler_tcp_async_bridge.c3`
    - added `scheduler_try_async_udp_recv(fd, max_bytes, interp)` as a direct
      bridge reuse over the existing async fd-read path (`scheduler_try_async_tcp_read`).
- Runtime UDP receive path
  - `src/lisp/async.c3`
    - `prim_udp_recv` now attempts scheduler async bridge first in fiber context:
      - when bridge handles the call, returns resumed async result;
      - otherwise falls back to existing blocking recv path (non-fiber / no-loop cases).
- Regression coverage
  - `src/lisp/tests_tests.c3`
    - added async regression:
      - `udp-recv in fiber via async bridge`
    - scenario:
      - bind receiver UDP socket,
      - spawn fiber blocked on `udp-recv`,
      - send datagram from sibling socket,
      - await fiber result and assert payload parity.
- Plan status
  - `docs/plans/library-gaps-todo.md`
    - marked `A3 / Unify socket readiness and wakeup handling with scheduler barrier paths` complete.

### Validation
- Build: `c3c build` succeeded.
- ASAN build: `c3c build --sanitize=address` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1354/0`
    - `compiler`: `76/0`
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `20/0`
    - `scope_region`: `51/0`
    - `unified`: `1353/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 243 - A3 Unix Pipe Surface (`pipe-connect` / `pipe-listen`)

### Summary
Completed the next A3 networking item by adding Unix domain socket primitives
and stdlib/effect wiring, reusing existing TCP accept/read/write/close flow for
stream semantics.

### What changed
- Runtime primitives
  - `src/lisp/async.c3`
    - added Unix-domain socket interop:
      - `SockaddrUn` layout
      - `pipe_connect_fd`
      - `pipe_listen_fd`
    - added primitives:
      - `(pipe-connect path)`
      - `(pipe-listen path)`
    - `pipe-connect` returns a stream `tcp-handle` (compatible with
      `tcp-read`/`tcp-write`/`tcp-close`).
    - `pipe-listen` returns a listener `tcp-handle` (compatible with
      `tcp-accept`).
    - listener setup performs best-effort stale socket-path cleanup via
      `unlink` before `bind`.
- Primitive/effect registration
  - `src/lisp/eval_init_primitives.c3`
    - registered `__raw-pipe-connect`, `__raw-pipe-listen`.
    - mapped effect fast paths:
      - `io/pipe-connect -> __raw-pipe-connect`
      - `io/pipe-listen -> __raw-pipe-listen`
    - updated fast-path registry count `40 -> 42`.
- Stdlib surface
  - `stdlib/stdlib.lisp`
    - added effect declarations:
      - `io/pipe-connect`, `io/pipe-listen`
    - added wrappers:
      - `pipe-connect`, `pipe-listen`
- Tests
  - `src/lisp/tests_advanced_tests.c3`
    - added `pipe loopback roundtrip` regression using:
      - `pipe-listen` + `pipe-connect` + `tcp-accept` + `tcp-write` + `tcp-read`.
- Plan status
  - `docs/plans/library-gaps-todo.md`
    - marked `A3 / Add Unix domain socket/pipe support` complete.

### Validation
- Build: `c3c build` succeeded.
- ASAN build: `c3c build --sanitize=address` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1353/0`
    - `compiler`: `76/0`
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `20/0`
    - `scope_region`: `51/0`
    - `unified`: `1352/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 242 - A2 Effect-Wrapper Closure + A3 UDP Primitive Surface

### Summary
Completed the next integration slice by closing A2 typed effect/wrapper parity
for filesystem operations and implementing the A3 UDP primitive surface at the
runtime + stdlib effect boundary.

### What changed
- Runtime networking primitives
  - `src/lisp/async.c3`
    - added UDP primitives:
      - `(udp-socket)`
      - `(udp-bind handle host port)`
      - `(udp-send handle host port data)`
      - `(udp-recv handle [max-bytes])`
      - `(udp-close handle)`
    - added deterministic `UdpHandle` finalizer close path.
    - added shared IPv4 sockaddr helper (`fill_sockaddr_ipv4`) and reused it
      in TCP listen setup.
    - accepted both unpacked and packed effect payload call-shapes for
      `udp-bind`/`udp-send` (variadic fast-path migration).
- Primitive registration and effect fast-path wiring
  - `src/lisp/eval_init_primitives.c3`
    - registered `__raw-udp-*` primitives.
    - added `io/udp-* -> __raw-udp-*` fast-path mapping.
    - increased fast-path table count from `35` to `40`.
- Stdlib typed effects and wrappers
  - `stdlib/stdlib.lisp`
    - added effect declarations:
      - `io/udp-socket`, `io/udp-bind`, `io/udp-send`, `io/udp-recv`,
        `io/udp-close`
    - added wrappers:
      - `udp-socket`, `udp-bind`, `udp-send`, `udp-recv`, `udp-close`
- FS wrapper migration hardening
  - `src/lisp/prim_io.c3`
    - updated `prim_fs_open` to accept packed effect-payload shape
      (`(signal io/fs-open (cons ...))`) in addition to unpacked direct args.
    - this closes the runtime mismatch introduced by A2 wrapper routing and
      keeps `fs-open` stable through the effect fast path.
- Tests
  - `src/lisp/tests_advanced_tests.c3`
    - added UDP regressions:
      - `udp socket create/close`
      - `udp loopback roundtrip`
- Plan status
  - `docs/plans/library-gaps-todo.md`
    - marked `A2 / Wire stdlib wrappers and typed effects` complete.
    - marked `A3 / Add UDP support` complete.

### Validation
- Build: `c3c build` succeeded.
- ASAN build: `c3c build --sanitize=address` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1352/0`
    - `compiler`: `76/0`
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `20/0`
    - `scope_region`: `51/0`
    - `unified`: `1351/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 241 - A2 Filesystem Primitive Surface (`uv_fs_*`) Baseline

### Summary
Continued to the next roadmap item after A1 by adding the raw filesystem
primitive surface on top of libuv FS APIs and validating deterministic file
handle close/finalizer behavior.

### What changed
- C shim additions
  - `csrc/uv_helpers.c`
    - added synchronous libuv FS wrappers:
      - `omni_uv_fs_open`, `omni_uv_fs_close`, `omni_uv_fs_read`,
        `omni_uv_fs_write`, `omni_uv_fs_stat_basic`, `omni_uv_fs_scandir_join`,
        `omni_uv_fs_rename`, `omni_uv_fs_unlink`.
- Runtime primitive wiring
  - `src/lisp/prim_io.c3`
    - added raw primitives:
      - `(fs-open path flags [mode])`
      - `(fs-read handle n)`
      - `(fs-write handle data)`
      - `(fs-close handle)`
      - `(fs-stat path)`
      - `(fs-readdir path)`
      - `(fs-rename src dst)`
      - `(fs-unlink path)`
    - added `FsHandle` FFI wrapper with finalizer-based close path to keep file
      descriptor lifecycle deterministic.
  - `src/lisp/eval_init_primitives.c3`
    - registered `fs-*` primitives.
  - `src/lisp/compiler_primitive_variable_hash_table.c3`
    - added AOT primitive hash entries for `fs-*` names.
- Validation tests
  - `src/lisp/tests_advanced_tests.c3`
    - added fs round-trip, `fs-readdir` array-shape, and `fs-stat` dict-shape
      regressions.

### Plan status updates
- `docs/plans/library-gaps-todo.md`
  - A2:
    - `Add raw primitives`: complete.
    - `Ensure file handles have deterministic close/finalizer behavior`: complete.
    - `Wire stdlib wrappers and typed effects`: still pending.

### Validation
- Build: `c3c build` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1350/0`
    - `compiler`: `76/0`
- ASAN build: `c3c build --sanitize=address` succeeded.
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `20/0`
    - `scope_region`: `51/0`
    - `unified`: `1349/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 240 - A1 libuv Timers + DNS Async Path Completion

### Summary
Completed Track A1 of the library integration roadmap by wiring `io/async-sleep`
and `io/dns-resolve` fiber paths through libuv (`uv_timer` / `uv_getaddrinfo`)
with scheduler-aware pending-op cleanup.

### What changed
- Runtime integration
  - `src/lisp/async.c3`
    - added libuv DNS async request bindings via C shim hooks.
    - routed fiber `dns-resolve` through async `uv_getaddrinfo` + fiber yield/resume.
    - preserved explicit non-fiber blocking fallback (`getaddrinfo`).
  - `src/lisp/scheduler_state_offload.c3`
    - added `PendingDnsResolve`, scheduler `pending_dns` table, and `WAKEUP_DNS_DONE`.
  - `src/lisp/scheduler_wakeup_io.c3`
    - added DNS callback (`scheduler_uv_getaddrinfo_cb`) and wakeup dispatch.
    - added pending DNS close/cancel lifecycle (`scheduler_close_pending_dns`).
    - added deterministic blocked-fiber resume on DNS completion.
  - `src/lisp/scheduler_io_fiber_core.c3`
    - initialized/reset `pending_sleeps` + `pending_dns` per fiber.
    - fiber completion now closes pending sleep + DNS state.
  - `src/lisp/scheduler_primitives.c3`
    - abort/reset paths now close pending DNS operations in addition to read/offload/sleep.
- C interop support
  - `csrc/uv_helpers.c` (new)
    - added `uv_getaddrinfo` request allocation/start/cancel helpers.
    - added req `data` accessors for callback-time host-buffer cleanup.
    - added `uv_freeaddrinfo` shim.
  - `project.json`
    - added `csrc/uv_helpers.c` to target `c-sources`.

### Plan status updates
- `docs/plans/library-gaps-todo.md`
  - A0 map mirror task marked complete.
  - `io/dns-resolve` and `io/async-sleep` reclassified to `done-libuv`.
  - A1 checklist marked complete.
  - Snapshot updated:
    - `done-libuv`: `2/27`
    - `partial-libuv`: `6/27`
    - `non-libuv`: `19/27`

### Validation
- Build: `c3c build` succeeded.
- ASAN build: `c3c build --sanitize=address` succeeded.
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `20/0`
    - `scope_region`: `51/0`
    - `unified`: `1346/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 239 - Library Integration Plan Baseline (A0 `io/*` Backend Map)

### Summary
Recorded the Track A0 baseline for the libuv-first integration plan by mapping
all `io/*` effects to current runtime backends and status tags.

### What changed
- `docs/plans/library-gaps-todo.md`
  - Reframed plan as a libuv-first integration roadmap plus other library
    tracks (BearSSL/libdeflate/utf8proc/yyjson/LMDB).
  - Added explicit A0 inventory table for all 27 `io/*` effects.
  - Added status classification snapshot:
    - `done-libuv`: `0/27`
    - `partial-libuv`: `6/27`
    - `non-libuv`: `21/27`

### Notes
- This is planning/inventory work only; no runtime behavior change in this
  session segment.
- Next implementation target is A1 (timers + DNS).

## 2026-03-07: Session 238 - ASAN Stack-Buffer-Overflow Guardrail in Symbol Interning

### Summary
Fixed a deterministic ASAN crash on the signal/type-check error path that
terminated inside `SymbolTable.intern` during payload-code interning.

Observed crash chain:
- `src/lisp/jit_jit_handle_signal.c3:77` (`runtime/effect-arg-mismatch`)
- `src/lisp/value_constructors.c3:466` (`raise_error_with_payload_names`)
- `src/lisp/value_symbol_table.c3:114` (`mem::malloc(len + 1)`)

ASAN reported stack-buffer-overflow at allocation time from an unsafe/corrupted
symbol slice reaching `intern`.

### What changed
- `src/lisp/value_symbol_table.c3`
  - Added defensive null-state checks in `SymbolTable.intern`:
    - reject null table pointers (`self`, `entries`, `hash_index`)
    - reject null `name.ptr`
  - Added explicit symbol-length guard (`name.len > 8192` => reject with
    `INVALID_SYMBOL_ID`) to block invalid ABI/corrupted slices from entering
    allocator/copy paths.
  - Normalized lookup/compare loop to reuse validated local `len`.

### Validation
- ASAN build: `c3c build --sanitize=address` succeeded.
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1347/0`
    - `compiler`: `76/0`
  - No ASAN crash.
- Normal build: `c3c build` succeeded.
- Normal full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `20/0`
    - `scope_region`: `51/0`
    - `unified`: `1346/0`
    - `compiler`: `76/0`

## 2026-03-06: Session 237 - Fix Method Table Idempotent Re-define (Ambiguity Bug)

### Summary
Fixed a bug where calling `(load ...)` multiple times caused typed function definitions to accumulate
duplicate entries in method tables, leading to "ambiguous method call" errors (ERROR values are
truthy, breaking `if` guards). Root cause: `jit_method_table_append_entry` always appended new
entries without checking for identical signatures. Fix: compare signatures before appending and
replace in-place (idempotent re-define semantics). Updated `dispatch ambiguity` test to reflect
the new behavior (second define with same sig replaces first). Removed all diagnostic code added
during investigation. Tests: 1302 → 1306 passed (3 CRUD pipeline tests now pass + 1 test updated).

### Files modified
- `src/lisp/jit_jit_closure_define_qq.c3` — added `jit_method_sigs_equal` helper; changed
  `jit_method_table_append_entry` to replace matching-signature entries instead of always appending.
- `src/lisp/tests_advanced_tests.c3` — updated "dispatch ambiguity" test: same-sig re-define now
  replaces (returns 12 = 3*4) instead of raising an ambiguity error.
- `src/lisp/tests_tests.c3` — removed all `[DIAG]` diagnostic blocks from the CRUD post/get test.

### Test counts
- Before: 1302 passed, 3 failed (`crud pipeline post/get`, `put/delete`, `duplicate-post race`) + 1 failing (`dispatch ambiguity (expected error)`)
- After: 1306 passed, 0 failed

## 2026-03-06: Session 236 - Parser/Regex Payload Normalization (`parser/*`, `regex/*`) + CRUD Example Hardening

### Summary
Continued Phase 2 migration with parser/regex coverage:
- migrated `pika` parser/regex primitive invalid-argument and grammar-failure
  paths from print+nil / nil-on-bad-args to canonical payloaded `raise`,
- added payload regressions for parser/regex domain/code behavior,
- fixed an ASAN-discovered early-return leak in grammar compilation paths,
- hardened `examples/deduce_crud_server.omni` request parsing/routing for
  malformed request-line cases used by HTTP regression tests.

### What changed
- `src/pika/lisp_pika.c3`
  - added `regex_raise(...)` and `parser_raise(...)` helpers.
  - migrated invalid-arg failures in:
    - `re-match`, `re-fullmatch`, `re-find-all`, `re-split`, `re-replace`,
      `re-match-pos`, `re-find-all-pos`
    to canonical `regex/*` payload codes.
  - removed print-and-nil parser failure flow; migrated to `parser/*` payload
    codes for:
    - grammar definition/compile errors (`pika/grammar`)
    - parser primitive arg/lookup failures (`pika/parse`, `pika/fold`,
      `pika/parse-lisp`, `pika/grammar-rules`, `pika/match-span`)
    - `pika/eval-lisp` unavailable path
  - grammar compiler now tracks first compile error (`gc_set_error`) and returns
    stable payload codes instead of logging.
  - added `defer mem::free(gc.rb.rules.ptr)` in `prim_pika_grammar` to close
    early-return leak surfaced under ASAN.
  - raised named-grammar ceiling from `16` to `256` to avoid incidental
    capacity exhaustion across isolated regression loads.
- `src/lisp/tests_advanced_tests.c3`
  - added parser/regex payload regressions:
    - regex `re-match` domain/code on invalid pattern arg
    - parser `pika/parse` domain/code on invalid grammar arg
    - parser `pika/grammar` code on invalid operator
- `examples/deduce_crud_server.omni`
  - made `server/stop` parse-safe with explicit `nil` else branch.
  - hardened `pipeline/decode-request` to avoid fragile destructuring on
    malformed request-line shapes.
  - hardened `pipeline/path-id` to avoid destructuring mismatch on malformed
    path values.
  - updated GET route behavior to return route-not-found for non-`/items`
    routes when no valid item id is present.
- `docs/ERROR_MODEL.md`
  - inventory updated for parser/regex migration state.
  - matrix updated:
    - `Pika grammar APIs`: `done`
    - `Regex match/search primitives`: remains `partial` (malformed-pattern
      signaling still pending)
  - pending P2.7 scope narrowed accordingly.
- `.claude/plans/effects-typesystem-parity-plan.md`
  - added parser progress note as completed.
  - narrowed remaining P2.7 scope to regex malformed-pattern signaling +
    residual wrapper coverage.

### Validation
- Normal build: `/opt/c3/c3c build` succeeded.
- Normal suite: `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` succeeded.
  - `stack_engine`: 21 pass / 0 fail
  - `scope_region`: 51 pass / 0 fail
  - `unified`: 1306 pass / 0 fail
  - `compiler`: 73 pass / 0 fail
- ASAN build: `/opt/c3/c3c build --sanitize=address` succeeded.
- ASAN suite: `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` succeeded.
  - `stack_engine`: 20 pass / 0 fail (overflow recovery test skipped under ASAN)
  - `scope_region`: 51 pass / 0 fail
  - `unified`: 1305 pass / 0 fail
  - `compiler`: 73 pass / 0 fail

## 2026-03-06: Session 235 - Deduce Error Payload Normalization (`deduce/*`) + Leak Fix

### Summary
Continued Phase 2 migration with the deduce-family slice:
- migrated deduce/relation/match failure paths from string-only raises to
  canonical payloaded `raise` under `deduce/*` codes,
- added deterministic payload regressions for deduce domain/code behavior,
- fixed an LMDB env cleanup leak on invalid `deduce-open` argument paths
  discovered by the new ASAN-covered regression,
- updated migration docs/plan status for remaining P2.7 scope.

### What changed
- `src/lisp/deduce.c3`
  - added `deduce_raise(...)` helper (domain fixed to `deduce`).
  - migrated `deduce-open` and top-level `deduce` dispatch errors to stable
    `deduce/*` codes.
  - fixed invalid-argument cleanup path in `deduce-open` to close LMDB env
    (`mdb_env_close`) before freeing `DeduceDb`.
- `src/lisp/deduce_relation_ops.c3`
  - migrated `fact!`, `retract!`, `deduce-count`, and `deduce-scan` failures
    to canonical `deduce/*` payload codes.
- `src/lisp/deduce_schema_query.c3`
  - migrated `__define-relation` and `deduce-query` failures to canonical
    `deduce/*` payload codes.
- `src/lisp/unify.c3`
  - migrated `deduce-match` failure paths to canonical `deduce/*` payload
    codes.
- `src/lisp/tests_advanced_tests.c3`
  - added deduce payload regressions:
    - dispatch payload domain (`deduce`)
    - dispatch command-type code
    - open invalid-path-type code
    - query arity/shape code
- `docs/ERROR_MODEL.md`
  - marked Deduce APIs as `done` with canonical payload coverage.
  - narrowed pending P2.7 note to parser/regex/residual wrappers.
- `.claude/plans/effects-typesystem-parity-plan.md`
  - added deduce progress note as completed.
  - narrowed remaining P2.7 targets to parser/regex/residual wrappers.
