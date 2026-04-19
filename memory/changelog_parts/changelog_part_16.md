# Memory Changelog Index Part 16

Source: `memory/CHANGELOG.md`

## 2026-04-08

- Closed arm64 language-level continuation multi-shot parity (`STACK-AARCH64-CONT-001`) after backend landing:
  - corrected arm64 GNU lightning register ID mapping in
    `src/lisp/jit_lightning_constants.c3` for `JIT_R*`, `JIT_V*`, and `JIT_FP`.
  - updated effect fast-path primitive dispatch in:
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
    - `src/lisp/jit_jit_runtime_effects_signal.c3`
  - fast-path changes now:
    - preserve primitive error payloads (domain/code/message) instead of
      rewriting them to generic runtime codes,
    - expand dotted cons payloads used by stdlib fixed-arity wrappers (for
      example `(cons host port)`) into positional primitive arguments.
  - verification evidence:
    - `c3c build`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack` ->
      `Stack engine: 23 passed, 0 failed`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))"` -> `11`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (with-continuation k (k 41))))"` -> `42`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(checkpoint (+ 1 (capture k (k 10))))"` -> `11`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` ->
      `pass=56 fail=0`
    - `OMNI_JIT_POLICY_FILTER=multishot-capture-scope-guard-clone OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` ->
      `pass=1 fail=0`

## 2026-04-07

- Hardened the automated Claude training loop so tmux-attached runs do not
  drop the window immediately on exit:
  - `scripts/claude-loop.sh` now installs an EXIT handler that pauses before
    shell termination when the loop is running inside tmux and stdout is a
    terminal.
  - The pause is opt-out via `CLAUDE_LOOP_KEEP_WINDOW=0`, so non-interactive
    automation can still terminate without blocking.
  - This keeps the training output visible after a long run or a failure
    instead of closing the tmux window as soon as the foreground process
    exits.

## 2026-04-01

- Hardened AOT closure allocation paths to fail closed across compiler emission
  and runtime bridge:
  - `src/lisp/compiler_code_emission_lambda_closures.c3` now emits an explicit
    `_closure_data == null` guard for captured closures and returns
    `aot::make_error(...)` instead of risking null-dereference on capture
    field writes; it also frees `_closure_data` when `aot::make_closure*`
    returns an error to avoid captured-payload leaks on closure-construction
    failure paths.
  - `src/lisp/aot_runtime_bridge.c3` now replaces assert-based
    `make_closure`/`make_variadic_closure` allocation handling with explicit
    error-value returns and payload cleanup on primitive allocation failure.
  - `src/lisp/aot_runtime_bridge.c3` now guards `aot_closure_apply(...)` and
    `aot_variadic_apply(...)` against missing payload/user-data state and
    returns explicit error values instead of dereferencing null closure payloads.
  - Added deterministic AOT closure allocation seams in
    `src/lisp/aot_runtime_bridge.c3`:
    - `g_aot_force_closure_data_alloc_fail`
    - `g_aot_force_closure_primitive_alloc_fail`
    - `g_aot_force_variadic_closure_data_alloc_fail`
    - `g_aot_force_variadic_closure_primitive_alloc_fail`
  - Added `aot::make_error(...)` wrapper in `src/lisp/aot.c3` so generated
    code can report closure-capture allocation failures via the AOT surface.
  - Extended `src/lisp/tests_compiler_core_groups.c3` with regression coverage
    for:
    - emitted closure-capture null-guard presence in generated C3,
    - seam-driven `aot::make_closure*` allocation failures returning
      `ValueTag.ERROR` instead of asserting,
    - direct `aot_closure_apply`/`aot_variadic_apply` missing-payload calls
      returning `ValueTag.ERROR`.
  - Validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`overall_status: pass`, compiler slice `181 passed, 0 failed`).
  - Additional closure payload hardening:
    - `src/lisp/aot_runtime_bridge.c3` now routes AOT payload validity checks
      through shared helpers (`aot_closure_payload_valid`,
      `aot_variadic_payload_valid`) and applies the same fail-closed payload
      errors in direct apply entrypoints and fast-path invoke/apply trampolines
      (`invoke_once`, `apply_multi_once`), preventing null callback
      dereference on malformed payload state.
    - `make_closure(...)` and `make_variadic_closure(...)` now fail closed when
      invoked with a null callback (`missing invoke callback`) instead of
      constructing invalid payload objects.
    - `aot_closure_apply(...)` / `aot_variadic_apply(...)` now resolve the
      active interpreter defensively (`interp` or `g_aot_interp`) before
      reading payload state, so null `interp` calls fail closed instead of
      dereferencing `interp.prim_user_data`.
    - AOT closure factory functions now resolve active interpreter state once
      and return null when no runtime interpreter is available, avoiding
      null-interpreter error-construction crashes on out-of-contract calls.
    - `src/lisp/tests_compiler_core_groups.c3` now covers null-callback closure
      construction seams for both unary and variadic AOT closure factories, and
      null-`interp` direct apply calls returning `ValueTag.ERROR`.
  - Re-validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`overall_status: pass`, `run_counts: 11 pass / 0 fail`, compiler slice
      `181 passed, 0 failed`).
  - Null-interpreter apply hardening validation:
    - `src/lisp/tests_compiler_core_groups.c3` now asserts
      `aot_closure_apply(...)` and `aot_variadic_apply(...)` fail closed with
      `ValueTag.ERROR` when called with `interp = null` and active AOT runtime
      state.
  - Variadic invoke arg-list allocation fail-close hardening:
    - `src/lisp/aot_runtime_bridge.c3` now includes deterministic seam
      `g_aot_force_invoke_variadic_arg_list_alloc_fail` and routes single-arg
      variadic invoke list construction through `aot_make_single_arg_list(...)`.
    - `invoke_once(...)` now returns explicit error
      (`aot invoke: out of memory while building variadic argument list`) when
      single-arg list construction fails, instead of passing null list payload
      into variadic closure callbacks.
    - `src/lisp/tests_compiler_core_groups.c3` now asserts this seam-driven
      invoke path returns `ValueTag.ERROR`.
    - `src/lisp/aot_runtime_bridge.c3` now also guards
      `aot_variadic_apply(...)` list construction through checked prepend helper
      `aot_prepend_arg_checked(...)` with deterministic seam
      `g_aot_force_variadic_apply_arg_list_alloc_fail`, returning explicit
      error (`aot variadic apply: out of memory while building argument list`)
      on allocation failure instead of propagating null list state.
    - `src/lisp/tests_compiler_core_groups.c3` now asserts seam-driven direct
      `aot_variadic_apply(...)` argument-list allocation failure returns
      `ValueTag.ERROR`.
    - `src/lisp/aot_runtime_bridge.c3` now guards dispatch entrypoints
      `invoke_once(...)` and `apply_multi_once(...)` with deterministic seam
      `g_aot_force_dispatch_interp_unavailable`, returning explicit errors
      (`aot invoke: runtime interpreter unavailable`,
      `aot apply-multi: runtime interpreter unavailable`) instead of routing
      null interpreter state into JIT apply surfaces.
    - `src/lisp/tests_compiler_core_groups.c3` now asserts seam-driven
      `aot::invoke(...)` and `aot::apply_multi(...)` dispatch-unavailable paths
      return `ValueTag.ERROR`.
    - additional AOT bridge failure surfaces now use
      `aot_make_error_best_effort(...)` instead of direct
      `make_error(g_aot_interp, ...)` calls in import/field-access/
      explain-dispatch paths, keeping error construction consistent with
      interpreter-availability hardening.
    - `src/lisp/aot_runtime_bridge.c3` now adds explicit bridge-runtime
      seam `g_aot_force_bridge_interp_unavailable` and fail-closed interpreter
      guards in `lookup_prim(...)`, `lookup_var(...)`, `import_module(...)`,
      `field_access(...)`, and `explain_dispatch(...)`.
    - `src/lisp/tests_compiler_core_groups.c3` now asserts seam-driven bridge
      runtime-unavailable calls for those helpers return `ValueTag.ERROR`.
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      remains green after this slice (`overall_status: pass`,
      `run_counts: 11 pass / 0 fail`, compiler `181 pass / 0 fail`).
    - extended bridge-runtime-unavailable hardening in
      `src/lisp/aot_runtime_bridge.c3` to additional direct wrappers that still
      dereferenced `g_aot_interp`:
      - tail registration entrypoints now fail closed:
        `invoke_tail(...)`, `apply_multi_tail(...)`
      - pattern/collection helpers now guard bridge interpreter access:
        `instance_field(...)`, `dict_get_symbol(...)`, `seq_nth(...)`,
        `seq_rest(...)`, `list_rest(...)`, plus constructor/type lookup checks
        in `match_var_pattern(...)` / `match_constructor(...)`
      - effect/dispatch wrappers now fail closed when bridge interpreter is
        unavailable:
        `explain_effect_signal(...)`, `explain_effect_resolve(...)`,
        `dict_from_args(...)`, `index(...)`, `compiled_handle(...)`,
        `compiled_signal(...)`, `compiled_reset(...)`, `compiled_shift(...)`,
        `compiled_resolve(...)`
      - output bridge `print_value(...)` now avoids null-interpreter symbol
        dereference by emitting a deterministic unavailable marker.
    - `src/lisp/tests_compiler_core_groups.c3` now extends the existing
      seam-driven AOT hardening regression block to assert
      `ValueTag.ERROR` for:
      - dispatch-unavailable tail registration:
        `aot::invoke_tail(...)`, `aot::apply_multi_tail(...)`
      - bridge-unavailable helper/wrapper paths:
        `aot::instance_field(...)`, `aot::dict_get_symbol(...)`,
        `aot::seq_nth(...)`, `aot::seq_rest(...)`, `aot::list_rest(...)`,
        `aot::dict_from_args(...)`, `aot::index(...)`,
        `aot::compiled_signal(...)`, `aot::compiled_handle(...)`,
        `aot::compiled_reset(...)`, `aot::compiled_shift(...)`,
        `aot::compiled_resolve(...)`, `aot::explain_effect_signal(...)`,
        `aot::explain_effect_resolve(...)`.
    - Re-validation for this extension slice:
      - `c3c build`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
        (`suite=compiler pass=183 fail=0`)
      - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
        (`overall_status: pass`, `run_counts: 11 pass / 0 fail`).

- Hardened the JSON bridge and `json-emit` fail-closed behavior:
  - `csrc/json_helpers.c` no longer exposes raw yyjson internal-layout stepping
    for parse traversal; array/object reads now use explicit yyjson iterators
    through allocation/free wrapper APIs.
  - mutable yyjson append/add wrappers now return `bool`, and
    `json-emit` now checks insertion outcomes instead of silently accepting
    partial object/array construction.
  - `json-emit` now fails explicitly for unsupported Omni root values and
    unsupported dictionary key tags instead of coercing to placeholder/null
    output, and option-parse failure now frees the mutable document.
  - fixed-precision flag handling now treats `0..15` as the valid range (with
    `0` meaning "do not force fixed precision"), matching the option parser
    contract.
- Made async `write-file` offload atomic and more specific:
  - `src/lisp/scheduler_offload_ops.c3` now writes to a temp file and renames
    on success, unlinks temp files on failure, and returns explicit
    `OFFLOAD_RES_ERROR` messages for open/write/close/rename failure stages.
  - temp-path suffix formatting now uses supported formatter directives
    (`.tmp.<pid>.<hex-id>`) instead of `%zu`, which previously caused false
    `offload write-file: temp path formatting failed` errors on normal writes.
  - `src/lisp/prim_io_file.c3` now preserves the existing user-facing
    `io/write-file-failed` contract even when the offload worker now reports
    richer internal error detail.
  - when remapping scheduler offload errors to `io/write-file-failed`, the
    primitive now clears pre-existing pending-raise state before re-raising so
    canonical I/O payloads are not shadowed by the original scheduler payload.
- Added regression coverage in
  `src/lisp/tests_runtime_data_unicode_groups.c3` for `json-emit` fail-closed
  paths (unsupported root value type and unsupported dictionary key type).
- Made TOML option parsing concurrency-safe:
  - `csrc/toml_helpers.c` now serializes TOML parse calls and temporary
    `toml_set_option(...)` mutations behind a shared libuv mutex so per-call
    `check-utf8` toggles cannot race or leak process-global parser state across
    concurrent parses.
- Hardened parser fail-closed and growth-allocation behavior for module/import
  surfaces:
  - `src/lisp/parser_module_decl.c3` now fails closed on malformed module body
    parses and on module export/body growth allocation failures instead of
    returning partial module ASTs.
  - `src/lisp/parser_import_helpers.c3`,
    `src/lisp/parser_import_helpers_specs.c3`, and
    `src/lisp/parser_export_from.c3` now convert import/export list growth and
    initial-list allocation failures into explicit parser errors instead of
    proceeding with null writes or invalid capacity state.
- Reconciled `pika/parse-lisp` with shipped reader behavior and made parser
  drift explicit:
  - `src/pika/lisp_pika.c3` now raises
    `parser/parse-lisp-grammar-drift` when the built-in Pika grammar rejects
    input that the shipped core reader can parse, instead of silently returning
    `nil`.
  - Added regression coverage in
    `src/lisp/tests_runtime_feature_pika_groups.c3` for form-comment drift
    (`#_...`) fail-closed behavior.
- Made Pika global grammar/parser state concurrency-safe:
  - `src/pika/lisp_pika.c3` now guards named-grammar registry access with a
    mutex and snapshots grammar metadata before parse/fold/match surfaces.
  - lazy global Lisp parser init now uses `thread::OnceFlag` so first-use init
    cannot race.
  - `src/pika/lisp_pika_grammar_compiler.c3` now stores named grammars through
    the synchronized registry helper.
- Removed unsynchronized regex cache/error-state behavior in the Pika regex
  bridge:
  - `src/pika/regex_cache_api.c3` now serializes cache access and resets behind
    a shared mutex, including compiled regex lookup/use paths.
  - last compile-error storage is now thread-local so concurrent regex calls do
    not overwrite each other’s error detail.
- Added module fail-closed regression coverage in
  `src/lisp/tests_advanced_stdlib_module_groups.c3` to ensure malformed module
  bodies no longer escape as partial ASTs.
- Fixed relation DBI-open commit-path reporting so commit failures cannot be
  surfaced as successful relation initialization:
  - `src/lisp/deduce_schema_query.c3` now checks `mdb_txn_commit(...)` in
    relation DBI-open paths, propagates explicit
    `deduce/*-txn-commit-failed` errors, and includes a test seam for forced
    commit-failure coverage.
  - `src/lisp/tests_deduce_groups.c3` now includes a regression that forces the
    relation-open commit-failure seam and asserts both `deduce/open-named` and
    `__define-relation` return commit-failure codes instead of success.
- Replaced `process-wait` 1 ms polling loops with event-driven scheduler wakeup:
  - `src/lisp/async_process_signal_dns_process.c3` now routes `process-wait`
    through `scheduler_execute_custom_offload_job(...)` and a blocking
    `omni_uv_process_wait(...)` worker callback instead of repeatedly polling
    `omni_uv_process_poll(...)` + `async-sleep 1`.
  - `csrc/uv_helpers_process.c` now exposes debug counters for poll/wait call
    paths, and `src/lisp/tests_advanced_io_effect_ffi_groups.c3` asserts
    `process-wait` uses the blocking wait path (`wait_calls >= 1`) with zero
    poll invocations for the same wait operation.
- Removed the async signal delivery 1024-cap drop path and added deterministic
  burst regression coverage:
  - `src/lisp/async_process_signal_handles.c3` now drains all pending signal
    callbacks without an artificial `min(pending, 1024)` cap.
  - `csrc/uv_helpers_signal.c` now provides deterministic debug pending helpers
    (`omni_uv_signal_debug_set_pending` / `omni_uv_signal_debug_pending`) used
    by tests.
  - `src/lisp/tests_advanced_io_effect_ffi_groups.c3` now forces a pending
    burst above 1024 and asserts full drain via runtime dispatch counters.
- Replaced process-global REPL SIGINT state with signal-safe interpreter routing:
  - `csrc/stack_helpers.c` now exposes thread-local SIGINT routing helpers that
    keep pending SIGINT state in `sig_atomic_t` slots and route delivery only
    to an explicitly bound interpreter slot.
  - `src/lisp/eval_signal.c3` now uses that C bridge (`omni_sigint_*`) and no
    longer keeps global `g_interrupted` process state.
  - `src/lisp/value_interp_state.c3` adds per-interpreter `sigint_requested`
    state and runtime init/lifecycle wiring.
  - `src/lisp/eval_repl.c3` now binds SIGINT routing only while evaluation is
    active and clears stale idle interrupts before each eval.
  - `src/lisp/tests_advanced_io_effect_ffi_groups.c3` adds deterministic
    coverage proving a pending SIGINT marks only the currently bound
    interpreter slot.
- Made non-transaction deduce mutation surfaces fail-atomic after durable LMDB
  commit:
  - `src/lisp/deduce_relation_ops_mutations.c3` now treats post-commit metadata
    persistence failures (`deduce_db_note_*` and relation metadata delete
    persistence in `drop!`) as best-effort follow-up work instead of returning
    user-visible failures after base data commit has already succeeded.
  - Added a forced metadata-persistence failure seam in
    `src/lisp/deduce_db_handles.c3`
    (`g_deduce_force_materialized_meta_persist_fail`) and a regression in
    `src/lisp/tests_deduce_groups.c3` proving non-transaction `fact!` preserves
    success surface while committed rows remain visible.
- Hardened transactional deduce mutation bookkeeping when txn-side mutation-log
  writes fail:
  - `src/lisp/deduce_db_handles_mutation_txn.c3` now provides a forced
    mutation-log failure seam (`g_deduce_force_txn_mutation_log_fail`) and
    marks txn mutation tracking degraded when insert/delete log writes cannot
    be recorded.
  - `src/lisp/deduce.c3` commit handling now avoids applying partial mutation
    deltas when logging degraded and instead routes post-commit bookkeeping
    through the existing untracked-write fallback
    (`deduce_db_note_untracked_write_commit`) so materialization state is
    conservatively escalated to full-recompute.
  - Added regression coverage in `src/lisp/tests_deduce_groups.c3` proving a
    forced txn-log write failure still commits base tuples while `deduce
    'analyze` reports `'full-recompute` invalidation mode.
- Hardened async signal lifecycle ownership and dispatch topology:
  - `src/lisp/async_process_signal_handles.c3` removes the fixed
    process-global `MAX_SIGNAL_HANDLES` registry and replaces it with a
    per-interpreter intrusive handle list rooted at `Interp.signal_handles_head`,
    so signal callback dispatch no longer scans unrelated interpreter watchers.
  - signal callbacks are no longer promoted to root-only lifetime with no
    teardown path; signal handles now retain the callback owner scope at
    registration and release that retained scope on unhandle/finalizer
    (`signal_handle_detach_runtime_state`), restoring balanced lifetime cleanup.
  - `src/lisp/async_process_signal_dns.c3` now routes unhandle teardown through
    the shared detach helper so explicit unhandle and finalizer cleanup follow
    the same close/unregister/release contract.
  - Added advanced regression coverage in
    `src/lisp/tests_advanced_io_effect_ffi_groups.c3` for:
    - >32 active signal handles in one interpreter without registry-cap failure,
    - owner-interpreter dispatch isolation (pending callbacks are ignored when
      dispatching on a non-owner interpreter),
    - callback owner-scope retain/release symmetry across
      `signal-handle`/`signal-unhandle`.
- Restored inline-module import no-op lowering in top-level compiler emission
  while preserving runtime bridge behavior for path/selective imports:
  - `src/lisp/compiler_program_top_level.c3` now short-circuits `(import mod)`
    with no path and no import specifiers to a static no-op when `mod` was
    already compiled inline in the same unit.
  - This preserves the no-op contract exercised by compiler regression coverage
    (`Compiler: import lowers as static no-op`) without weakening
    `aot::import_module(...)` lowering for path-based or selective import forms.
- Fixed top-level compiler import-all flag propagation and expanded import
  lowering regression coverage:
  - `src/lisp/compiler_program_top_level.c3` now threads
    `ExprImport.import_all` into emitted `aot::import_module(...)` calls for
    both module-name and path import forms instead of always emitting
    `import_all=false`.
  - `src/lisp/tests_compiler_core_groups.c3` now covers:
    - plain `(import mod 'all)` lowering with `import_all=true`,
    - path `(import \"path\" 'all)` lowering with `import_all=true`,
    - path selective import lowering with `import_all=false` plus
      `lookup_var(...)` binding,
    - plain/path/inline selective alias imports preserving alias target binding
      with source-symbol lookup (`alias = lookup_var(source)`),
    - inline `(module ...)(import mod 'all)` remaining a static no-op when the
      module was compiled inline in the same unit.
- Fixed non-top-level import lowering drift in flat expression compilation:
  - `src/lisp/compiler_tail_position_compilation_tco.c3` now routes
    `compile_import_flat(...)` through runtime import side effects
    (`aot::import_module(...)`) for non-inline import forms, threads
    `import_all`, and applies selective/alias bindings through
    `aot::define_var(alias_or_name, aot::lookup_var(source_name))`.
  - The inline no-op optimization remains for the zero-specifier inline-module
    case so existing compiled-inline import contracts stay unchanged.
  - `src/lisp/tests_compiler_core_groups.c3` now covers non-top-level import
  lowering for:
    - plain import runtime bridge side effects,
    - plain/path `'all` import side effects with `import_all=true`,
    - plain/path selective runtime binding,
    - plain/path selective alias runtime binding,
    - inline selective runtime binding (plain and alias),
    - inline-module no-op preservation (plain and `'all`) inside expression
      lowering.
- Removed duplicated inline-module import detection logic in the compiler:
  - `compiler_has_inline_compiled_module(...)` is now centralized in
    `src/lisp/compiler_program_top_level_helpers.c3` and used by both
    top-level import emission and `compile_import_flat(...)` paths, reducing
    future drift risk with no behavior change.
- Hardened compiler allocation failure paths with deterministic fail-closed
  behavior and explicit seams:
  - `src/lisp/compiler_program_pipeline.c3` now fails closed when allocating
    the combined stdlib+source program buffer fails
    (`compiler: out of memory while allocating program source buffer`) and
    short-circuits compilation on that error.
  - `src/lisp/compiler_compiler_initialization.c3` now treats compiled-module
    registry init allocation failure as a compile error instead of leaving
    null registry state unchecked.
  - `src/lisp/compiler_tail_position_compilation_tco.c3` now guards compiled
    module registry allocation/growth and per-module export-table allocation
    in `compile_module_flat(...)`, setting compile errors instead of
    dereferencing null allocations.
  - Added compiler test seams in `src/lisp/compiler_compiler_state.c3`:
    - `g_compiler_force_program_source_alloc_fail`
    - `g_compiler_force_compiled_modules_init_alloc_fail`
    - `g_compiler_force_module_registry_alloc_fail`
    - `g_compiler_force_module_exports_alloc_fail`
  - Added regression coverage in `src/lisp/tests_compiler_core_groups.c3`
    proving those seam-triggered allocation failures fail closed and surface
    stable compile error messages.
- Closed status-consistency drift after TODO/archive condensation:
  - `TODO.md` now includes the explicit zero-queue marker required by
    `scripts/check_status_consistency.sh` (`- none currently;`).
  - `docs/areas/memory-runtime.md` and `docs/areas/types-dispatch.md` now
    carry `As of: 2026-04-01`, aligned with the latest changelog entry.
  - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    now reports `overall_status: pass` with all 11 lanes green.
- Added compiler size-overflow hardening for module/program source buffers:
  - Introduced shared checked-size helper `compiler_checked_mul_usz(...)` in
    `src/lisp/compiler_compiler_state.c3` and wired compiler module registry
    allocations through it.
  - `src/lisp/compiler_compiler_initialization.c3` now fail-closes on
    compiled-module registry allocation-size overflow with
    `compiler: compiled module registry allocation size overflow`.
  - `src/lisp/compiler_tail_position_compilation_tco.c3` now fail-closes on:
    - compiled-module registry growth-capacity overflow,
    - compiled-module registry allocation-size overflow,
    - module export-table allocation-size overflow.
  - Added deterministic compiler seams in
    `src/lisp/compiler_compiler_state.c3`:
    - `g_compiler_force_compiled_modules_init_size_overflow`
    - `g_compiler_force_module_registry_growth_overflow`
    - `g_compiler_force_module_exports_size_overflow`
  - Extended `src/lisp/tests_compiler_core_groups.c3` with seam-driven
    fail-closed regressions for those overflow paths.
  - `src/lisp/compiler_program_pipeline.c3` now performs explicit unsigned
    length arithmetic in `build_program_source(...)` (`prelude_len` as `usz`)
    so the max-size guard compiles cleanly and still fail-closes with
    `compiler: program source exceeds maximum supported size`.
  - `src/lisp/compiler_top_level_compile_function.c3` now guards
    `compiler_own_result_slice(...)` against `slice.len == usz.max` before
    `+1` allocation math.
- Hardened compiler lambda-scan allocation paths to fail closed instead of
  leaving partial lambda-definition state:
  - `src/lisp/compiler_lambda_scan_lambda_defs.c3` now validates and checks
    capture/parameter table allocation sizes, fails closed on allocation and
    growth failures, and avoids null-write/partial-state capture table growth.
  - New deterministic compiler seams in
    `src/lisp/compiler_compiler_state.c3`:
    - `g_compiler_force_lambda_capture_alloc_fail`
    - `g_compiler_force_lambda_param_alloc_fail`
    - `g_compiler_force_lambda_capture_growth_alloc_fail`
    - `g_compiler_force_lambda_capture_size_overflow`
    - `g_compiler_force_lambda_param_size_overflow`
  - `src/lisp/compiler_program_pipeline.c3` now short-circuits immediately
    after program-analysis prep when `self.has_error` is set, preventing
    downstream code-emission panics when lambda-scan fails closed.
  - `src/lisp/compiler_lambda_scan.c3` now short-circuits lambda scanning when
    compiler error state is already set.
  - `src/lisp/tests_compiler_core_groups.c3` now includes fail-closed
    regressions for lambda capture alloc failure, lambda parameter alloc
    failure, lambda capture/parameter size-overflow seams, and lambda capture
    growth allocation failure.
  - Validation status summary remains green after landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=170 fail=0`.
- Hardened parser FFI function-expression allocation to fail closed:
  - `src/lisp/parser_ffi.c3` now guards `ExprFfiFn` allocation in
    `parse_ffi_fn(...)` and reports
    `out of memory while creating FFI function expression` instead of
    dereferencing a null allocation.
  - `parse_ffi_fn(...)` now cleans up partially allocated FFI parse state
    (`c_name`, parameter buffers, and struct allocation) on all parse-error
    exits before returning `null`, preventing dangling partial pointers/leaks
    in failed parse paths.
  - Added deterministic seam `g_parser_force_ffi_fn_alloc_fail` in
    `src/lisp/parser_ffi.c3` and compiler-path regression coverage in
    `src/lisp/tests_compiler_core_groups.c3` proving parse/compile fail-close
    behavior for that allocation path.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=171 fail=0`.
- Hardened FFI parameter-table growth in parser helpers:
  - `src/lisp/parser_ffi_helpers.c3` now guards FFI parameter table growth
    against size overflow and switches to atomic grow-and-swap allocation for
    parameter name/type tables, avoiding partial `realloc` state drift on
    failure paths.
  - Added deterministic parser seams in
    `src/lisp/parser_ffi_helpers.c3`:
    - `g_parser_force_ffi_param_names_alloc_fail`
    - `g_parser_force_ffi_param_types_alloc_fail`
    - `g_parser_force_ffi_param_table_size_overflow`
  - Extended compiler-path regression coverage in
    `src/lisp/tests_compiler_core_groups.c3` to prove fail-closed behavior for
    FFI parameter name alloc failure, type alloc failure, and parameter-table
    size-overflow seams.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=174 fail=0`.
- Hardened lexer token-buffer initialization fail-close path:
  - Added deterministic lexer seam in
    `src/lisp/parser_lexer_core_api.c3`:
    - `g_lexer_force_text_alloc_fail`
  - `src/lisp/compiler_program_pipeline_helpers.c3` now detects lexer
    initialization failure before parser setup and reports a stable compile
    error detail:
    `out of memory while initializing lexer token buffer`.
  - Added compiler-path regression coverage in
    `src/lisp/tests_compiler_core_groups.c3` proving lexer token-buffer
    allocation failure fails closed and surfaces the expected error text.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=175 fail=0`.
- Hardened lexer token-text growth guardrails and added deterministic growth
  failure coverage:
  - `src/lisp/parser_lexer_core_api.c3` now includes:
    - `g_lexer_force_text_growth_alloc_fail` seam for deterministic growth
      allocation failure testing,
    - explicit guards against zero/overflowed capacity requests in
      `ensure_text_capacity(...)`,
    - overflow guards in `push_current_text_char(...)` and `set_error_text(...)`
      to avoid wraparound in token-text length arithmetic.
  - `src/lisp/parser_lexer_symbol_number.c3` now routes integer overflow errors
    through `set_error_text(...)` and guards symbol-length overflow before
    `len + 1` capacity math.
  - `src/lisp/tests_compiler_core_groups.c3` now includes a deterministic
    lexer growth-allocation failure regression (long string token forcing
    growth under `g_lexer_force_text_growth_alloc_fail`), asserting fail-closed
    compile behavior with expected lexer error text.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=176 fail=0`.
- Hardened lambda-lowering lookup miss path to fail closed instead of panicking:
  - `src/lisp/compiler_native_call_compilation_flat_style.c3` now converts
    missing lambda-definition lookup from assertion panic into explicit compile
    error (`compiler: missing lambda definition during AOT lambda lowering`)
    and returns a safe nil temp.
  - Added deterministic seam `g_compiler_force_lambda_lookup_miss` in
    `src/lisp/compiler_compiler_state.c3`.
  - Added compiler-path regression in
    `src/lisp/tests_compiler_core_groups.c3` proving lookup-miss fail-close
    behavior without compiler panic.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=177 fail=0`.
- Removed remaining compiler assertion panic in lambda-closure emission:
  - `src/lisp/compiler_code_emission_lambda_closures.c3` now fail-closes
    `emit_lambda_return_common(...)` when lambda-definition lookup misses,
    setting compile error
    `compiler: missing lambda definition during closure emission` and emitting
    a safe `return aot::make_nil();` fallback in generated closure body.
  - Added deterministic seam
    `g_compiler_force_lambda_emit_lookup_miss` in
    `src/lisp/compiler_compiler_state.c3`.
  - Added compiler-path regression in
    `src/lisp/tests_compiler_core_groups.c3` proving closure-emission lookup
    miss fail-close behavior without assertion panic.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=178 fail=0`.
- Removed lexer-level panic fallback from `Lexer.expect(...)`:
  - `src/lisp/parser_lexer_core_api.c3` no longer calls `unreachable(...)`
    when token expectation fails at lexer level.
  - `Lexer.expect(...)` now fail-closes by setting lexer error text/state
    without aborting process execution.
  - Added direct regression coverage in
    `src/lisp/tests_compiler_core_groups.c3` proving lexer expect mismatch
    now fails closed (`T_ERROR`) without panic.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=179 fail=0`.
