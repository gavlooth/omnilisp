# Memory Changelog Index Part 33

Source: `memory/CHANGELOG.md`

- 2026-03-16 (editor tooling: silent clean pull diagnostics auto-refresh in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` quickfix helper paths now accept a
    `notify_empty` option in addition to the existing quickfix-open control.
  - `tooling/omni-nvim/lua/omni/init.lua` now runs automatic current-buffer and
    workspace pull-diagnostics refreshes with `notify_empty = false`, so clean
    auto-refresh results do not emit repeated `no results` notifications.
  - explicit `:OmniLspDocumentDiagnostics` and `:OmniLspWorkspaceDiagnostics`
    behavior is unchanged: manual commands still notify when the result set is
    empty.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check confirming explicit empty pull-diagnostics commands
      still notify, while event-driven empty auto-refresh stays silent.
- 2026-03-16 (editor tooling: configurable pull diagnostics auto-refresh behavior in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/init.lua` now exposes explicit config knobs for
    automatic pull-diagnostics refresh behavior:
    - `lsp.pull_diagnostics.auto_open`
    - `lsp.pull_diagnostics.auto_notify_empty`
    - `lsp.pull_diagnostics.workspace_auto_open`
    - `lsp.pull_diagnostics.workspace_auto_notify_empty`
  - the current defaults preserve the recent quiet-refresh behavior:
    - current-buffer and workspace auto-refresh keep `open = false`
    - current-buffer and workspace auto-refresh keep `notify_empty = false`
  - this makes the pull-diagnostics UX adjustable without changing the explicit
    command behavior for `:OmniLspDocumentDiagnostics` and
    `:OmniLspWorkspaceDiagnostics`.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check confirming the new config can force auto-refresh to
      reopen quickfix and emit empty-result notifications.
- 2026-03-16 (editor tooling: combined pull diagnostics snapshot in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now refactors pull-diagnostics
    collection into reusable helpers and exposes `all_diagnostics()` to build a
    single quickfix snapshot from:
    - current-buffer `textDocument/diagnostic`,
    - workspace `workspace/diagnostic`.
  - the combined snapshot deduplicates identical quickfix entries while still
    reusing the same cached pull-diagnostics result ids as the existing
    document/workspace commands.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspAllDiagnostics`,
    - default buffer-local mapping `<localleader>lA`.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for combined pull-diagnostics quickfix contents,
      cache reuse, command registration, and buffer-local mapping registration.
- 2026-03-16 (editor tooling: combined pull diagnostics reset in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now exposes
    `reset_all_diagnostics()`, which clears both:
    - current-buffer `textDocument/diagnostic` cached result ids,
    - workspace `workspace/diagnostic` cached result ids.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspAllDiagnosticsReset`.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check confirming `OmniLspAllDiagnosticsReset` clears both
      cache layers before the next combined pull-diagnostics request.
- 2026-03-16 (editor tooling: combined pull diagnostics auto-refresh in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/init.lua` now adds a third opt-in
    auto-refresh lane under `lsp.pull_diagnostics` for the merged
    `OmniLspAllDiagnostics` snapshot:
    - `all_auto_refresh`
    - `all_events`
    - `all_auto_open`
    - `all_auto_notify_empty`
  - when enabled, `omni-nvim` refreshes the combined document + workspace
    pull-diagnostics quickfix snapshot on the configured events while reusing
    the same cache-aware `all_diagnostics()` path as the explicit command.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for `OmniNvimAllPullDiagnostics` autocmd
      installation, event-driven combined refresh, and the default quiet
      combined-auto-refresh behavior.
- 2026-03-09 (L3.5 closure: type-system status/spec cleanup):
  - docs/status updates:
    - `docs/type-system-syntax.md`:
      - removed stale `NOT Implemented` carry-over rows for constructor/lambda type gaps.
      - added explicit implemented-row regression anchors to:
        - `run_advanced_type_parametric_ctor_annotation_tests`,
        - `run_advanced_type_lambda_call_boundary_tests`.
      - updated implementation-status timestamp to `2026-03-09`.
    - `docs/areas/types-dispatch.md`:
      - updated area status text to reflect `L3` gap closure and shifted remaining work references to `L4/L5`.
    - `TODO.md`:
      - marked `L3.5` complete.
      - marked acceptance gate `A-L3` complete.
- 2026-03-16 (runtime hardening: stack-pool ownership + continuation/resolve safety):
  - stack pool ownership and shutdown hardening:
    - added `StackCtx.owner_pool` and `StackPool.shutting_down`,
    - coroutine value dtors now destroy via `coroutine_val.owner_pool` instead
      of a global pool pointer, preventing cross-interpreter pool corruption,
    - `stack_ctx_destroy` now rejects owner-pool mismatch and running-context
      teardown, and short-circuits reentrant frees while pool shutdown is in
      progress,
    - `stack_ctx_create` now rejects allocations when `pool.shutting_down`,
      preventing callback-time reentry from creating unstable contexts during
      shutdown traversal,
    - `stack_pool_shutdown` now detaches `all_list` heads before callback
      teardown, rejects freeing `CTX_RUNNING`/`g_current_stack_ctx`, and
      remains reentrancy-safe while deferred callbacks run.
  - handle/resolve continuation hardening:
    - `HandleEffectState` is now heap-backed per handle activation with:
      - `continuation_refcount`,
      - `frame_active`,
      - durable captured `handler_copy` buffers,
    - both JIT and value runtime handle paths now capture durable handler copies
      for continuation resolve and retain/release state based on escaped
      continuation refs,
    - suspended handle contexts are no longer destroyed at handle-return when
      continuations still retain them (ownership transfer to continuation path),
    - applying a handler continuation as a function now routes through
      `resolve` semantics (handler snapshot reinstall + one-shot lifecycle)
      instead of raw checkpoint-style continuation resume,
    - `resolve` now requires `CTX_SUSPENDED`, restores full interpreter state
      around AOT signal suspension, and handles post-resume dead/completed
      context teardown safely when no active handle frame owns the context,
    - `resolve` resuspend-outside-handle failures now consume/invalidate the
      continuation and destroy the suspended context to avoid leaked retained
      state and stale one-shot handles.
  - build backend hardening:
    - helper script invocation now runs via `env bash` (Bash semantics preserved
      without `/bin/sh` assumptions),
    - `c3c` resolution now supports `C3C` env override first, then pinned paths,
      then `c3c` PATH fallback for non-standard toolchain installs.
  - regression validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --build /tmp/omni_build_smoke.lisp -o /tmp/omni_build_smoke_bin`
- 2026-03-16 (scheduler/async hardening: callback token guards + cancelled queue payload release):
  - async DNS/TCP connect callback ownership now uses explicit per-request
    callback context and request tokens instead of req-pointer table scans:
    - added tokened callback context structs:
      - `PendingDnsCallbackCtx`
      - `PendingConnectCallbackCtx`
    - added scheduler token seed:
      - `Scheduler.async_request_token_seed`
    - added per-pending request tokens:
      - `PendingDnsResolve.request_token`
      - `PendingTcpConnect.request_token`
    - DNS flow updates:
      - `prim_dns_resolve` allocates and binds callback context with
        `{fiber_id, request_token, host_copy}`,
      - `scheduler_uv_getaddrinfo_cb` routes by callback context fiber/token and
        rejects stale/mismatched callbacks without mutating unrelated pending
        slots,
      - DNS close/consume paths now detach/free callback context explicitly.
    - TCP/pipe connect flow updates:
      - `prim_tcp_connect` and `prim_pipe_connect` allocate and bind callback
        context with `{fiber_id, request_token, tcp_handle}`,
      - `scheduler_uv_tcp_connect_cb` validates callback context token ownership
        before touching pending slot state,
      - connect close/consume paths now detach/free callback context explicitly.
  - cancelled queued thread-task payload hardening:
    - `scheduler_uv_offload_after_cb` now releases queued `work.shared` when
      work never executed (`queued.executed == false`), preventing payload leaks
      when cancellation occurs before `work_cb`,
    - non-executed path now forces terminal task cancellation via
      `scheduler_cancel_thread_task(...)`, ensuring joiners cannot stall in
      pending state.
  - new scheduler regressions:
    - `run_scheduler_thread_task_nonexecuted_payload_release_boundary_tests`
      verifies queued payload release + terminal completion when after-work runs
      without `work_cb` execution,
    - `run_scheduler_dns_connect_request_token_guard_boundary_tests` verifies
      DNS/connect callback token guards reject req-pointer rebinding into
      unrelated pending slots.
  - regression validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `c3c build --sanitize=address` (build success)
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` under ASAN currently aborts in existing teardown path (`scope_dtor_value` UAF during isolated scheduler-suite destruction); this remains open and was not addressed in this pass.

- 2026-03-18 (boundary profiling revalidation after destination-partial hardening):
  - reran the container-capped `memory-lifetime-bench` profile on the current tree and kept the run within the accepted boundary-heavy envelope,
  - observed benchmark summaries on the rerun:
    - `boundary_decision_cost`: `iters=2048 splice_ms=3 disallowed_ms=4 reuse_ms=1`
    - `boundary_destination_routed_escape`: `iters=2048 partial_ms=2 partial_ok=2048`
    - `scope_splice_tail`: `iters=2048 escapes_per_iter=64 splice_ms=2`
  - rerun validation completed with `Unified Tests: 0 passed, 0 failed`.

- 2026-03-18 (scheduler/offload hot-path benchmark revalidation):
  - reran the container-capped `scheduler_offload_hot_path` benchmark on the current tree and kept the run green,
  - observed benchmark summary on the rerun:
    - `scheduler_offload_hot_path`: `iters=8192 queue_ms=0 queue_ok=8192 queue_pending=0 queue_enqueue_delta=8192 queue_fail_delta=0 completion_ms=2 completion_ok=8192 completion_alloc_fail=0 http_ms=2 http_ok=8192 http_shared_fail=0 tls_ms=0 tls_ok=8192`
  - rerun validation completed with `Unified Tests: 89 passed, 0 failed`.
# 2026-03-19 - Advanced iterator boundary closure

- Replaced the stdlib lazy iterator combinator path with primitive-backed iterator state in [src/lisp/primitives_iter_coroutine.c3](/home/heefoo/Documents/code/Omni/src/lisp/primitives_iter_coroutine.c3) and [stdlib/stdlib.lisp](/home/heefoo/Documents/code/Omni/stdlib/stdlib.lisp), avoiding closure env-copy churn for `map`/`filter`/`take`/`zip`/`foldl` and infinite iterator sources.
- Added a primitive list reverse fast path in [src/lisp/primitives_core.c3](/home/heefoo/Documents/code/Omni/src/lisp/primitives_core.c3) and rewired stdlib list `reverse`/`foldr` to use it.
- Fixed the remaining nested closure-backed iterator chain by allowing releasing-escape results to fall back to destination builders before hard failing on scope splice rejection in [src/lisp/eval_boundary_commit_flow.c3](/home/heefoo/Documents/code/Omni/src/lisp/eval_boundary_commit_flow.c3).
- Validation:
  - `c3c build`
  - host repros for `take`, iterator `foldl`, list `foldr`, and nested `it3`
  - bounded `advanced-unicode-iterator`: `118 passed, 0 failed`

- 2026-03-20 (Deduce join routing policy hardening):
  - join execution now has an explicit default-route policy in
    `src/lisp/deduce_relation_scan_helpers_join.c3`:
    - explicit fallback thresholds still force the historical index-first
      adaptive path,
    - the zero-threshold default now routes by supporting-index shape plus
      outer size hints, preferring index nested-loop only when the supporting
      index is fully bound and unique or the outer side is small, and choosing
      hash-first for larger non-unique / partially bound shapes,
    - the route decision is driven by explicit schema signals rather than the
      previous opaque always-index-then-maybe-fallback behavior.
  - added regression coverage for the new low-selectivity hash-first default in
    `src/lisp/tests_deduce_rule_groups_more_join.c3`.
  - added a selective-join benchmark smoke in
    `src/lisp/tests_deduce_query_bench_groups_more.c3`.
  - validation:
    - `c3c build`
    - bounded Deduce slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`78 passed, 1 failed`, remaining failure is the unrelated recursive
      iteration-limit payload regression outside the join lane)

- 2026-03-20 (Deduce aggregate metadata and validation scaffolding):
  - Deduce rule install now carries aggregate-aware head metadata through the
    normalized IR and stored rule signatures:
    - `src/lisp/deduce_rule_ops.c3` adds aggregate head-term parsing for
      `count`, `sum`, `min`, and `max`, with deferred rejection for `avg` and
      `distinct-count`,
    - `src/lisp/deduce_db_handles.c3` and
      `src/lisp/deduce_db_handles_mutation.c3` now persist/copy/release
      per-head projection kind, aggregate op, source-var ids, and aggregate
      projection counts,
    - `src/lisp/deduce_rule_eval_prims.c3` installs the aggregate metadata into
      stored rule signatures.
  - install-time aggregate validation is now explicit in
    `src/lisp/deduce_rule_eval.c3`:
    - aggregate terms are rejected in body atoms,
    - malformed / unknown aggregate projections reject deterministically,
    - aggregate inputs must be grounded by a positive body atom,
    - aggregate heads reject literal projections and mixed non-grouped shapes.
  - aggregate introspection scaffolding now exists without claiming grouped
    execution support:
    - `deduce/explain` exposes `aggregate-projection-count` plus per-head
      projection metadata in `src/lisp/deduce_rule_ops_explain.c3`,
    - `deduce/analyze` explicitly rejects aggregate-bearing rules until grouped
      execution lands in `src/lisp/deduce_rule_eval_prims.c3`.
  - regression coverage now includes aggregate install/explain/analyze
    scaffolding in:
    - `src/lisp/tests_deduce_rule_groups.c3`
    - `src/lisp/tests_deduce_rule_groups_explain.c3`
  - validation:
    - `c3c build`
    - bounded Deduce slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`90 passed, 0 failed`)

- 2026-03-26 (Deduce worker-scratch crash triage instrumentation):
  - added opt-in instrumentation in
    `src/lisp/deduce_rule_eval_exec_seminaive.c3` behind
    `OMNI_TRACE_DEDUCE_WORKER_SCRATCH=1`:
    - scratch-pass entry/exit markers,
    - per-rule dry-run begin/end markers,
    - dry-run head-fact skip/record markers,
    - scratch replay/accumulation boundary markers.
  - no semantic/runtime behavior change intended; instrumentation-only slice.
  - validation:
    - `c3c build` passed.
    - bounded deduce lane with tracing:
      `OMNI_TRACE_DEDUCE_WORKER_SCRATCH=1 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - observed state:
      - run stalled with no exit while log stayed flat at `167` lines for `30s`,
      - no `OMNI_DEDUCE_WORKER_SCRATCH` marker appeared,
      - last named boundary in log remained
        `[PASS] deduce analyze recursive limit reports stable error code`.

- 2026-03-26 (Deduce scan-range row-dict OOM guard validation):
  - hardened `src/lisp/deduce_relation_scan_helpers_more.c3` so
    `deduce_relation_materialize_row_dict(...)` returns a clean
    `deduce/query-out-of-memory` error when `make_hashmap(...)` fails.
  - added an env-gated direct helper probe in
    `src/lisp/tests_deduce_query_scan_groups.c3` behind
    `OMNI_DEDUCE_FORCE_ROW_DICT_OOM=1` so the row-dict OOM boundary is
    validated directly instead of depending on scan-path optimization shape.
  - validation:
    - `c3c build`
    - bounded deduce slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_DEDUCE_FORCE_ROW_DICT_OOM=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`1 passed, 0 failed`)

- 2026-03-26 (Omni version bump to 0.2.0):
  - updated the runtime-facing version surfaces to `0.2.0`:
    - `project.json`
    - `src/entry_cli_help_version.c3`
    - `src/entry_project_init_writer_project_json.c3`
    - `src/entry_project_init_writers.c3`
    - generated docs/man surfaces:
      - `docs/OMNI_REFERENCE.md`
      - `docs/man/omni.1`
      - `docs/man/omni-language.7`
  - rebuilt `build/main` so the binary replacement now reports `omni 0.2.0`.
  - validation:
    - `c3c build`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --version`
      (`omni 0.2.0`)

- 2026-03-30 (REPL server stdin routing + TCP transport):
  - added a real runtime input surface for language code instead of keeping REPL
    server `stdin` as a protocol placeholder:
    - `src/lisp/value_interp_state.c3` now carries an optional `input_state`
      attachment on `Interp`,
    - `src/lisp/prim_io.c3` now owns the line-buffered `InterpInputState`
      queue, the shared runtime line reader, and the new `prim_read_line`
      primitive,
    - `stdlib/stdlib.lisp` now exports `read-line` through the new
      `io/read-line` effect.
  - wired the REPL server to that runtime surface in
    `src/lisp/eval_repl_server.c3`:
    - `stdin` requests now accept `data` and optional `eof: true`,
    - session input is buffered per session and wakes blocked `read-line`
      calls,
    - interrupting a blocked `read-line` request now returns the existing
      `interrupted` terminal event.
  - widened the transport boundary:
    - added `omni --repl-server --tcp <host> <port>` in
      `src/entry_repl_server_mode.c3`,
    - updated help text in `src/entry_cli_help_version.c3`.
  - kept the runtime boundary honest:
    - sequential multi-client reuse is supported across Unix-socket/TCP
      listeners,
    - concurrent per-connection servicing is still deferred because runtime/JIT
      interpreter attachment remains owner-thread constrained.
  - compiler/runtime surface parity updates:
    - `src/lisp/eval_init_primitives.c3`
    - `src/lisp/compiler_free_vars_utils.c3`
    - `src/lisp/compiler_let_set_flat.c3`
    - `src/lisp/compiler_primitive_variable_hash_table_domains.c3`
  - validation:
    - `c3c build`
    - `printf 'abc\n' | ./build/main --eval '(read-line)' --json`
    - Python stdio REPL-server probe:
      - `clone` -> `(read-line)` -> `stdin data`
      - `clone` -> `(read-line)` -> `stdin eof`
      - `clone` -> `(read-line)` -> `interrupt`
    - Python Unix-socket probe:
      - two sequential client connections each `clone` + `eval`
    - Python TCP probe:
      - `describe` over `--repl-server --tcp 127.0.0.1 48763`

- 2026-03-30 (TCP REPL port-file discovery):
  - added nREPL-style TCP discovery for editor/plugin attachment:
    - `omni --repl-server --tcp <host> <port>` now writes
      `.omni-repl-port` in the current working directory after the listener
      binds successfully,
    - the file currently contains the bound TCP port followed by a newline.
  - implementation:
    - `src/lisp/eval_repl_server.c3` now publishes the port file with an
      atomic temp-write + rename path instead of writing it in place.
  - current contract:
    - this discovery file is emitted only for TCP REPL-server mode,
    - abrupt termination can still leave a stale `.omni-repl-port`, so
      clients should treat it as a discovery hint and still verify connect.
  - validation:
    - `c3c build`
    - `./build/main --repl-server --tcp 127.0.0.1 48763`
      writes `.omni-repl-port` containing `48763`

- 2026-03-30 (omni-nvim TCP REPL discovery):
  - updated the first-party Neovim plugin to consume the new TCP discovery
    file automatically:
    - `tooling/omni-nvim/lua/omni/repl.lua` now searches upward for
      `.omni-repl-port`, connects to `127.0.0.1:<port>` by default, clones a
      REPL-server session, and uses REPL-server `eval` requests when attached,
    - if discovery is missing or the TCP connect fails, the plugin falls back
      to the existing local `omni --repl --json` job path.
  - exposed the discovery surface in plugin defaults and docs:
    - `tooling/omni-nvim/lua/omni/init.lua`
    - `tooling/omni-nvim/README.md`
    - `tooling/omni-nvim/doc/omni.nvim.txt`
  - validation:
    - `tooling/omni-nvim/scripts/run_smoke.sh`
    - headless Neovim probe against a live
      `./build/main --repl-server --tcp 127.0.0.1 48763`
      listener with `.omni-repl-port`

- 2026-03-30 (REPL server project preload parity):
  - extended `omni --repl-server` so all shipped transports now accept
    `--project [dir]` instead of rejecting it as an unexpected argument:
    - `src/entry_repl_server_mode.c3` now parses `--project` for
      `--socket`, `--stdio`, and `--tcp`,
    - `src/entry_runtime_modes.c3` now exposes shared project-entry
      resolution so both interactive REPL preload and REPL-server preload use
      the same `omni.toml` + `src/main.omni` contract,
    - `src/lisp/eval_repl_server.c3` now preloads the resolved project entry
      for each new `clone` session before emitting the session event.
  - current contract:
    - server startup with `--project [dir]` does not preload a global shared
      interpreter,
    - each cloned session gets its own project preload,
    - preload output is emitted as normal protocol `out` events before the
      terminal `session` / `done` pair,
    - preload failure aborts the clone and returns a structured `error`
      event instead of exposing a half-initialized session.
  - validation:
    - `c3c build`
    - `./build/main --repl-server --tcp 127.0.0.1 48763 --project ./demo`
    - Python TCP probe:
      - `clone` emits `out` for `"Hello from demo!"`
      - then `session`
      - then `done`

- 2026-04-08 (syntax cleanup slice 1: fail-closed accessor removal):
  - removed leading-dot accessor shorthand from the active language surface:
    - `.name`
    - `.'key`
    - `.1`
    - `.[expr]`
    - `. [expr]`
  - removed quoted-symbol callable lookup from the active language surface:
    - `('name dict)`
  - kept access syntax centered on the existing canonical operations:
    - path access for normal member/symbol-key lookup such as `person.name`
    - postfix index access such as `expr.[key]`
    - explicit `(ref coll key)` as the semantic lookup core
  - compatibility posture:
    - this cleanup is intentionally fail-closed,
    - removed accessor surfaces now hard-error instead of remaining as legacy aliases.
  - implementation notes:
    - parser no longer treats leading-dot forms as implicit lookup lambdas,
    - runtime no longer treats bare symbol values as callable lookup shorthands.
  - validation:
    - pending during active implementation

- 2026-04-08 (syntax cleanup slice 2-3: primitive arity tightening and false/nil split):
  - removed primitive auto-partial from ordinary one-argument primitive calls:
    - bare calls like `(+ 3)` now raise arity errors,
    - partial application remains available through `_`, `|>`, and `partial`,
    - `PARTIAL_PRIM` remains a runtime value for explicit/internal partial payloads.
  - split `false` from `nil` in the active language/runtime contract:
    - `false` now binds to a distinct boolean symbol value,
    - `nil` remains the absence value,
    - both remain falsy in conditional positions.
  - implementation notes:
    - interpreter and JIT single-arg primitive apply paths now reject missing arguments instead of manufacturing partials,
    - JSON false now materializes as Omni `false`,
    - value-literal dispatch/pattern parsing no longer collapse `false` into `nil`,
    - Boolean/Nil constructors now respect the separation (`Boolean nil => false`, `Nil false` rejects).
  - validation:
    - `c3c build` unavailable in current environment; static/source-only verification performed

- 2026-04-08 (arm64 build lane repaired for current tree):
  - fixed split-file/compiler regressions that were blocking `c3c build` before
    the cleanup work could be verified:
    - restored persisted relation metadata structs after the deduce split,
    - corrected stale return/type mismatches and pointer-decay bugs in
      deduce/boundary helper files,
    - fixed allocator imports/usages for worker thread pool setup,
    - removed a stale prototype-only declaration from the boundary commit split.
  - rebuilt the static vendored dependency lane for arm64 in `deps/lib`:
    - `libutf8proc.a`
    - `libdeflate.a`
    - `libyyjson.a`
    - `libuv.a`
    - `libbearssl.a`
    - `liblmdb.a`
  - repaired local build integration around helper/link artifacts:
    - `project.json` now compiles `clib/mathutils.c` directly instead of
      pretending `mathutils` is a prebuilt library,
    - `project.json` restores the `omni_ftxui` archive link edge and embeds
      `RUNPATH=$ORIGIN` so repo-local shared libraries in `build/` resolve at
      runtime,
    - `scripts/build_omni_chelpers.sh` now passes vendored include dirs so the
      helper archives can be rebuilt from the repo tree on this host,
    - local shared libraries for GNU Lightning and replxx were built and staged
      into `build/`.
  - arm64 stack-engine note:
    - x86_64-only context switching no longer blocks the build on this host,
      but the non-x86 path is intentionally fail-closed at runtime rather than
      pretending continuations/coroutine stack switching work on arm64.
  - validation:
    - `bash deps/build_static.sh`
    - `make -C deps/src/lmdb/libraries/liblmdb clean all`
    - `bash scripts/build_omni_chelpers.sh`
    - `~/.local/bin/c3c build`
    - `readelf -d build/main`
    - `ldd build/main`
    - `./build/main --help`
- 2026-04-10 (boundary partial/iterator nested alias hardening):
  - `PARTIAL_PRIM` target-chain reuse and env-copy reuse now recurse into
    `first_arg` / `second_arg` payload graphs instead of checking only the
    direct arg wrappers.
  - shipped consequence:
    - a partial or iterator wrapper already in the target chain is no longer
      reused by identity when one arg is a target-chain `ARRAY`, `HASHMAP`,
      `SET`, or `METHOD_TABLE` wrapper whose nested child still belongs to the
      releasing/source scope.
    - those wrappers now fall back to the existing clone/promotion paths, so
      nested shared-wrapper aliasing stays fail-closed under both direct
      boundary copy and env-copy.
  - validation:
    - `c3c build`
    - targeted `memory-lifetime-smoke` regressions for target-chain wrapper
      alias policy and env-copy shared-wrapper policy
- 2026-04-10 (env-copy iterator/closure memo symmetry):
  - env-copy closure and iterator special-cases now participate in the shared
    `PromotionContext` epoch instead of bypassing memoization.
  - shipped consequence:
    - when the same source closure is bound directly and also reached through an
      iterator wrapper, env-copy now produces one shared cloned closure in the
      target scope instead of duplicating it.
    - when a source `PARTIAL_PRIM` is bound directly and also reached through an
      iterator wrapper, env-copy now reuses the same copied partial payload
      graph across both bindings.
  - validation:
    - `c3c build`
    - targeted `memory-lifetime-smoke` regressions for env-copy iterator/closure
      memo symmetry
- 2026-04-10 (JIT TCO alias-safety follow-up):
  - TCO env-copy now fails closed for target-chain `CONS` bindings whose direct
    scalar edge still belongs to the releasing scope.
  - shipped consequence:
    - the JIT TCO lane no longer decides “copy required” and then immediately
      lose that safety when the shared `copy_to_parent(...)` fast-reuse gate
      ignores a releasing-scope scalar child on a surviving `CONS` wrapper.
    - `copy_to_parent_payload_in_releasing_scope(...)` now scans `CONS` spines
      for direct releasing-scope children before admitting fast reuse, so the
      generic copy path stays consistent with the narrower JIT TCO reuse rule.
  - iterator payload alias checks now recurse into target-chain non-closure,
    non-partial payload graphs instead of relying on a shallow pointer test.
  - shipped consequence:
    - `ITERATOR` bindings carrying target-chain `ARRAY` / `HASHMAP` / `SET` /
      `CONS` style payloads now clone when those payload graphs still reach the
      releasing scope, instead of being reused by identity through the JIT TCO
      env-copy path.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` with:
      - `tco-cons-releasing-scalar-edge-copy`
      - `tco-partial-shared-wrapper-edge-copy`
      - `tco-iterator-shared-wrapper-edge-copy`
      - `tco-foreign-partial-shared-wrapper-edge-copy`
      - `tco-foreign-shared-wrapper-copy`
    - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
- 2026-04-10 (apply/promotion helper fail-closed follow-up):
  - `apply_partial(...)` now rejects impossible `PARTIAL_PRIM` execution state
    before any function-pointer call-through:
    - null/non-partial wrapper
    - null `func`
    - `remaining <= 0`
    - `remaining > 2`
    - `remaining == 2` with a prefilled `second_arg`
  - checked hashmap insertion now rejects promoted `ERROR` values from
    `boundary_promote_to_root(...)` instead of storing them as ordinary
    key/value data.
  - `fs_array_push(...)` and `csv_array_push(...)` now apply the same contract,
    so helper-level array append surfaces no longer embed promoted `ERROR`
    values into successful arrays.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=152 fail=0`
- 2026-04-10 (apply dispatch malformed-state fail-closed follow-up):
  - `apply_primitive(...)` now rejects malformed primitive wrappers before any
    primitive call-through:
    - null/non-primitive wrapper
    - null `prim_val`
    - null `prim_val.func`
  - `apply_partial(...)` now also rejects missing `first_arg` instead of
    forwarding an impossible captured-argument state into primitive dispatch.
  - `jit_apply_value_primitive(...)` now applies the same malformed primitive
    guard on the JIT helper path.
  - validation:
    - `c3c build`
    - focused `jit-policy`:
      - `invalid-primitive-state-fails-closed`
    - bounded `memory-lifetime-smoke`: `pass=152 fail=0`
- 2026-04-10 (coroutine thunk promotion fail-closed follow-up):
  - coroutine thunk publication now rejects invalid post-promotion state before
    any coroutine stack context allocation:
    - null promotion results,
    - promoted `ERROR` values,
    - non-closure thunk wrappers,
    - and closure wrappers with null `closure_val`
  - shipped consequence:
    - forced closure-wrapper promotion allocation failure no longer reaches
      `stack_ctx_make(...)` or mutates `stack_ctx_pool` counters.
    - coroutine thunk publication now matches the repo-wide fail-closed
      contract already enforced for adjacent scheduler/fiber promotion paths.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=153 fail=0`
- 2026-04-10 (data-format array promotion fail-closed follow-up):
  - JSON and TOML array assembly now reject promoted boundary `ERROR` values
    instead of storing them as successful array elements.
  - shipped consequence:
    - nested data-format array conversion under non-root scopes now preserves
      the boundary failure surface when element promotion fails.
    - TOML array element promotion is now covered through the existing
      `TIME_POINT` wrapper-copy allocation seam, so the fail-closed contract
      is pinned in bounded runtime-allocation coverage.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=154 fail=0`
- 2026-04-10 (escape cons publication fail-closed follow-up):
  - `make_cons(...)` now stages ESCAPE-lane `car` / `cdr` promotion before
    pair allocation and rejects invalid string/error promotion results instead
    of publishing an ESCAPE-lane cons that still points at TEMP-backed data.
  - shipped consequence:
    - the string/error escape-promotion seam no longer degrades into a
      successful cons carrying a non-escape field.
    - staged promoted fields are unwound if final escape-pair allocation
      fails.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=155 fail=0`
- 2026-04-10 (scheduler shared/offload projection fail-closed follow-up):
  - shared-handle projection no longer maps missing/unmaterializable shared
    payloads to empty-string success values.
  - offload path projection no longer maps missing/invalid shared path payloads
