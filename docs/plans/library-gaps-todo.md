# Library Integration TODO (libuv + Core Native Libraries)

Status: `active`  
As of: 2026-03-07

Goal: drive Omni toward full async runtime integration on libuv, then complete
high-impact parity integrations for BearSSL, libdeflate, utf8proc, yyjson, and
LMDB/Deduce.

Current parity snapshot (implementation + tests):

| Library | Status | Notes |
|---|---|---|
| libuv | In progress | Core tracks A1-A6 are landed; DoD gates still track remaining end-state parity. |
| BearSSL | Complete | Track B complete. |
| libdeflate | Complete | Track C complete. |
| utf8proc | Complete | Track D complete. |
| yyjson | Baseline/advanced complete | `json-parse` / `json-emit` / `json-emit-pretty` are integrated; writer flags, precision controls, and parse-option matrix are complete. |
| LMDB / Deduce | Baseline complete, advanced parity pending | Unified `(deduce '...)` surface is integrated and tested; explicit tx/range/db-management APIs remain. |
| Data formats (TOML) | Baseline/advanced complete | `toml-parse` now integrated via `tomlc17`, with timestamp/string/array/table conversion and parser option surface. |
| Data formats (CSV) | Baseline complete, advanced parity pending | `csv-parse` and `csv-emit` support delimiter/line-ending/quote options with explicit strict toggles. |

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

## Track A: libuv First-Class Runtime Integration

### A0. Baseline inventory and migration map
- [x] Inventory every `io/*` effect and map current backend (`blocking`, `offload`, `libuv`).
- [x] Mark each primitive as one of: `done-libuv`, `partial-libuv`, `non-libuv`.
- [x] Record map in this file and mirror status summary in `memory/CHANGELOG.md`.

Backend legend:

- `blocking`: direct sync runtime/file/socket/syscall path
- `offload`: scheduler worker-thread queue path
- `libuv-bridge`: fiber async path uses libuv poll/timer/loop wake
- `mixed`: combination of the above depending on context

Status legend:

- `done-libuv`: no non-libuv path in async/fiber flow for this effect
- `partial-libuv`: some libuv integration exists, but not complete
- `non-libuv`: no direct libuv backend for the effect behavior

Current `io/*` effect backend map:

| Effect | Raw primitive | Impl | Current backend | Status | Notes |
|---|---|---|---|---|---|
| `io/print` | `__raw-print` | `src/lisp/prim_io.c3` | blocking | non-libuv | synchronous console output |
| `io/println` | `__raw-println` | `src/lisp/prim_io.c3` | blocking | non-libuv | synchronous console output |
| `io/display` | `__raw-display` | `src/lisp/prim_io.c3` | blocking | non-libuv | synchronous console output |
| `io/newline` | `__raw-newline` | `src/lisp/prim_io.c3` | blocking | non-libuv | synchronous console output |
| `io/read-file` | `__raw-read-file` | `src/lisp/prim_io.c3` | blocking (`uv_fs_*` sync) | partial-libuv | read-all via `uv_fs_open`/`uv_fs_stat`/`uv_fs_read` |
| `io/write-file` | `__raw-write-file` | `src/lisp/prim_io.c3` | blocking (`uv_fs_*` sync) | partial-libuv | write path uses `uv_fs_open`/`uv_fs_write`/`uv_fs_close` |
| `io/file-exists?` | `__raw-file-exists?` | `src/lisp/prim_io.c3` | blocking (`uv_fs_stat` sync) | partial-libuv | existence check via `uv_fs_stat` |
| `io/read-lines` | `__raw-read-lines` | `src/lisp/prim_io.c3` | blocking (`uv_fs_*` sync) | partial-libuv | line split after `uv_fs` read-all |
| `io/pipe-connect` | `__raw-pipe-connect` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | mixed (POSIX primary + `uv_pipe` fallback) | partial-libuv | returns stream handle over Unix socket path; keeps blocking semantics by default |
| `io/pipe-listen` | `__raw-pipe-listen` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | mixed (POSIX primary + `uv_pipe` fallback) | partial-libuv | returns listener handle over Unix socket path; keeps blocking accept behavior by default |
| `io/tcp-connect` | `__raw-tcp-connect` | `src/lisp/async.c3` | mixed (`offload` in fiber, blocking otherwise) | non-libuv | currently no `uv_tcp_connect` |
| `io/tcp-listen` | `__raw-tcp-listen` | `src/lisp/async.c3` | blocking | non-libuv | blocking bind/listen |
| `io/tcp-accept` | `__raw-tcp-accept` | `src/lisp/async.c3` | mixed (`offload` in fiber, blocking otherwise) | non-libuv | currently no `uv_tcp` accept path |
| `io/tcp-read` | `__raw-tcp-read` | `src/lisp/async.c3`, `src/lisp/scheduler_tcp_async_bridge.c3` | mixed (`libuv-bridge` in fiber, blocking fallback) | partial-libuv | libuv poll/timer path exists in fiber |
| `io/tcp-write` | `__raw-tcp-write` | `src/lisp/async.c3`, `src/lisp/scheduler_tcp_async_bridge.c3` | mixed (`libuv-bridge` in fiber, blocking fallback) | partial-libuv | libuv writable poll path in fiber context with blocking fallback |
| `io/tcp-close` | `__raw-tcp-close` | `src/lisp/async.c3` | blocking | non-libuv | direct close |
| `io/dns-resolve` | `__raw-dns-resolve` | `src/lisp/async.c3` | mixed (`libuv-bridge` in fiber, blocking fallback) | done-libuv | fiber path uses `uv_getaddrinfo` |
| `io/async-sleep` | `__raw-async-sleep` | `src/lisp/async.c3` | mixed (`libuv-bridge` in fiber, `usleep` otherwise) | done-libuv | fiber path uses `uv_timer_start` |
| `io/offload` | `__raw-offload` | `src/lisp/scheduler_primitives.c3`, `src/lisp/scheduler_offload_worker.c3` | mixed (`offload` + libuv wake signaling) | partial-libuv | queue is custom worker threads, not `uv_queue_work` |
| `io/thread-spawn` | `__raw-thread-spawn` | `src/lisp/scheduler_primitives.c3` | mixed (`offload` + scheduler wake) | partial-libuv | thread tasks built on offload queue |
| `io/thread-join` | `__raw-thread-join` | `src/lisp/scheduler_primitives.c3` | mixed (`offload` + scheduler wake) | partial-libuv | depends on offload completion plumbing |
| `io/thread-join-timeout` | `__raw-thread-join-timeout` | `src/lisp/scheduler_primitives.c3` | mixed (`offload` + scheduler wake) | partial-libuv | timeout loop currently polling/sleep fallback |
| `io/thread-cancel` | `__raw-thread-cancel` | `src/lisp/scheduler_primitives.c3` | mixed (`offload` + scheduler wake) | partial-libuv | cancellation integrated with scheduler queue state |
| `io/process-spawn` | `__raw-process-spawn` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | mixed (`uv_spawn` + POSIX stdio pipe fd wiring) | partial-libuv | process lifecycle is libuv-backed, stdio exposed as fs handles |
| `io/process-wait` | `__raw-process-wait` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | mixed (`uv_run` loop wait) | partial-libuv | waits on per-process libuv loop |
| `io/process-kill` | `__raw-process-kill` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | mixed (`uv_kill`) | partial-libuv | signal delivery is via libuv process API |
| `io/signal-handle` | `__raw-signal-handle` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | libuv (`uv_signal`) | done-libuv | watcher install and polling are libuv-based |
| `io/signal-unhandle` | `__raw-signal-unhandle` | `src/lisp/async.c3`, `csrc/uv_helpers.c` | libuv (`uv_signal`) | done-libuv | watcher teardown uses libuv close/stop path |
| `io/tls-connect` | `__raw-tls-connect` | `src/lisp/tls.c3` | blocking | non-libuv | BearSSL over blocking socket |
| `io/tls-read` | `__raw-tls-read` | `src/lisp/tls.c3` | blocking | non-libuv | BearSSL blocking read |
| `io/tls-write` | `__raw-tls-write` | `src/lisp/tls.c3` | blocking | non-libuv | BearSSL blocking write/flush |
| `io/tls-close` | `__raw-tls-close` | `src/lisp/tls.c3` | blocking | non-libuv | BearSSL close + socket close |
| `io/http-get` | `__raw-http-get` | `src/lisp/http.c3` | mixed (`offload` in fiber, blocking otherwise) | non-libuv | uses worker for fiber context |
| `io/http-request` | `__raw-http-request` | `src/lisp/http.c3` | blocking | non-libuv | no fiber offload path currently |

A0 summary snapshot:

- `done-libuv`: 4/34 (`dns-resolve`, `async-sleep`, `signal-handle`, `signal-unhandle`)
- `partial-libuv`: 16/34 (`io/read-file`, `io/write-file`, `io/file-exists?`, `io/read-lines`, `pipe-*`, `tcp-read`, `tcp-write`, `offload`, `thread-*`, `process-*`)
- `non-libuv`: 14/34

### A1. Timers and DNS
- [x] Replace `async-sleep` fiber path with direct `uv_timer_start` scheduling.
- [x] Keep non-fiber fallback behavior explicit and documented.
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

### A5. Offload model alignment
- [x] Keep `offload` for CPU-bound work (compression, heavy compute).
- [x] Decide backend policy:
  - Decision: retain existing worker queue for CPU workloads in current cycle.
  - Rationale: preserves established wakeup/boundary behavior while libuv track
    completes around I/O paths; revisit `uv_queue_work` after A6 hardening data.
- [x] If backend changes, preserve current effect API compatibility.
  - Current state: no backend swap landed; compatibility remains verified by
    existing offload/thread/process regression coverage in `tests_tests.c3`.

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
- [x] For behavior changes, update:
  - `memory/CHANGELOG.md`
  - affected reference docs in `docs/reference/`
  - this plan status checkboxes.
