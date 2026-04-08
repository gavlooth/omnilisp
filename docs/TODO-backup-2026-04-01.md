# Active TODO

Last condensed: 2026-03-31

This file is now the sole live backlog.
All still-unimplemented items were migrated here from the removed backlog and
roadmap trackers under `docs/plans/`.

Current actionable count: 0

Completed backlog snapshots:

- `docs/TODO-backup-2026-03-26.md`
- `docs/TODO-backup-2026-03-31.md`

Use this file only for still-open work. When the live queue reaches zero,
archive the completed snapshot under `docs/` and reset this file again instead
of leaving checked-off sections in place.

## 2026-03-31 Audit Follow-up

- [x] Harden scheduler offload completion so blocked fibers cannot be stranded when reliable wakeup publication or fallback completion materialization fails.
- [x] Fix direct HTTP client response integrity so `http-get` / `http-request` stop silently truncating oversized replies and stop treating transport read failures as successful EOF.
- [x] Close direct HTTP client connection leaks on all post-connect early-return paths, including request assembly failure, write failure, and response-buffer allocation failure.
- [x] Make async `write-file` failure-safe and more specific: avoid in-place truncation data loss on partial write failure and preserve actionable write-failure causes.
- [x] Replace process-global SIGINT interruption state with a signal-safe, interpreter-correct interrupt model.
- [x] Narrow AOT `--build` backend source collection to runtime-required modules only instead of compiling test trees as part of production builds.
- [x] Harden TLS handle lifecycle and argument validation: clean up `tls-server-wrap` allocation-failure leaks and make `tls-read` reject invalid `max-bytes` arguments instead of silently defaulting.
- [x] Reconcile AOT build/tooling docs with the shipped backend contract, including generated artifacts and runtime link dependencies.
- [x] Reconcile REPL preload/server docs with the shipped `--repl-server` transport behavior for `--project` session preload.
- [x] Reconcile memory/runtime and collection-surface docs with current repo invariants, including dual-lane ownership language, `Dictionary` key support, and removal of stale RC/vector-centric guidance.
- [x] Stabilize REPL server session storage so `clone` cannot invalidate live session/interpreter pointers during session-capacity growth.
- [x] Fix REPL interrupt semantics so queued-command interrupts are not acknowledged and then cleared before execution starts.
- [x] Make non-transaction deduce mutations fail atomically at the user surface instead of returning errors after durable LMDB commits already succeeded.
- [x] Harden transactional deduce mutation tracking so failed txn-log writes cannot silently commit base facts without the corresponding dirtiness/materialization bookkeeping.
- [x] Harden OS-thread completion fallback so null-completion/OOM paths cannot strand task joins forever.
- [x] Make async signal delivery lossless or explicitly backpressured under burst load instead of dropping pending callbacks above the current 1024 cap.
- [x] Fix relation DBI open so LMDB commit failures cannot be reported as successful relation initialization.
- [x] Harden async signal/runtime lifecycle handling: release promoted callback roots correctly, revisit the fixed global signal-handle registry limit, and eliminate obvious process-global watcher bottlenecks.
- [x] Replace `process-wait` polling with an event-driven child-exit wakeup path so long-lived subprocess waits do not spin in 1 ms scheduler loops.
- [x] Make `toml-parse` option handling thread-safe so per-call UTF-8 settings do not mutate process-global parser state across concurrent parses.
- [x] Make `--bind` fail closed when parsed headers exceed the current fixed function scratch limit instead of silently generating partial FFI modules.
- [x] Make `--bind` reject overlong header paths explicitly instead of truncating them before libclang sees the path.
- [x] Harden bindgen parameter parsing so function-parameter allocation failures cannot silently emit malformed zero-argument bindings.
- [x] Make unsupported FFI type annotations fail closed instead of defaulting to raw-pointer ABI metadata.
- [x] Make `--init` scaffold creation transactional enough to avoid misleading EEXIST success on non-directories and to avoid leaving half-created project trees behind on mid-write failure.
- [x] Make `json-emit` fail closed when Omni data cannot be represented faithfully in JSON, including unsupported key/value tags and yyjson insertion failures that currently return partial or corrupted output as success.
- [x] Stabilize the JSON bridge around yyjson contracts and lifecycle boundaries by removing raw internal-layout dependence from parsing and freeing mutable emit documents on option-parse failure paths.
- [x] Make `inflate` validate arity and `original-size` strictly so it rejects ignored extra args, non-integer size hints, and non-positive sizes instead of silently falling back or casting invalid values.
- [x] Make structured REPL startup surfaces (`--repl --json` and structured `--repl-server` transports) emit machine-readable preflight failures instead of plaintext CLI usage/errors.
- [x] Reuse shared file-read classification for REPL preload and session-preload paths so open/read failures preserve their concrete cause instead of collapsing to `not found`.
- [x] Make shared CLI JSON reporting UTF-8-safe for non-UTF-8 argv/path bytes and extend smoke coverage beyond `--check --json` to `--eval --json`, `--describe --json`, REPL JSON preflight, and REPL-server startup surfaces.
- [x] Make top-level module parsing fail closed so malformed module bodies cannot escape as partial ASTs to JIT, schema, or introspection callers.
- [x] Harden import/export/module parser growth helpers so arena-allocation failure becomes a parser error instead of null writes, memory corruption, or crashes.
- [x] Reconcile `pika/parse-lisp` with the shipped Omni reader surface and make parser drift fail explicitly instead of returning `nil` for valid newer syntax.
- [x] Remove unsynchronized process-global regex cache and compile-error state so concurrent regex use cannot cross-contaminate cache bookkeeping or reported failure detail.
- [x] Make the named-grammar registry and lazy Pika parser initialization concurrency-safe so grammar installation and first-use parser setup cannot race into dangling pointers or duplicate init.
