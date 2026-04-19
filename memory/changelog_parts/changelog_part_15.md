# Memory Changelog Index Part 15

Source: `memory/CHANGELOG.md`

  ESCAPE promotion:
  - `src/lisp/eval_promotion_root_clones.c3` now owns shared cleanup helpers
    for partially materialized arrays, hashmaps/sets, and method tables, and
    those helpers unwind already-copied child ownership side effects before
    freeing the aborted heap wrapper.
  - `src/lisp/eval_promotion_copy_route_helpers.c3` and
    `src/lisp/eval_promotion_escape_structured.c3` now route array,
    hashmap/set, and method-table partial-copy failures through that cleanup
    path instead of freeing only the wrapper heap and leaking already-copied
    child retains.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now includes a
    disjoint-array regression that proves a failed second-element copy releases
    the first copied closure-env retain, and
    `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` wires that probe
    into the bounded smoke lane.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=85 fail=0`

- Closed two post-`478bc24` boundary follow-ups:
  - `src/lisp/eval_env_copy_values.c3` now routes iterator closure payloads
    through the same safe undelimited clone helper as plain closure env-copy,
    so iterators over global-env closures no longer fail closed more
    aggressively than the underlying closure contract.
  - `src/lisp/jit_jit_closure_support.c3` now exposes a scope-copy failure
    seam for method-signature cloning, and
    `src/lisp/jit_jit_closure_let_set_helpers.c3` now fails closed if detached
    env-scope recursive closure publication cannot copy its typed signature
    instead of silently publishing a downgraded closure with `type_sig = null`.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now covers
    iterator payload closure cloning for safe undelimited global-env captures.
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3` now covers recursive
    detached closure publication under forced signature-copy failure.
  - validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=rec-closure-type-sig-copy-failure ./build/main --test-suite lisp` -> `pass=1 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=84 fail=0`

## 2026-04-09

- Closed the advanced recursive-closure and generated-source regression slice:
  - `src/lisp/jit_jit_compile_let_set_helpers.c3` now treats closure-local
    mutation through collection-surface calls (`push!`, `remove!`, `set!`) as
    a mutable capture, so `let ^rec` closures box captured locals before
    recursive execution instead of silently dropping array updates.
  - `src/lisp/jit_jit_compile_let_set.c3` now threads `Interp*` into that
    mutability check, and `src/lisp/jit_jit_closure_let_set_helpers.c3` now
    publishes the patched recursive closure value explicitly and fails closed
    if the env-scope self-reference clone cannot be allocated.
  - `src/lisp/value_interp_lifecycle.c3` now rehashes macro/module tables with
    the same symbol-id slotting policy already used by runtime lookup, fixing
    table growth drift that could surface in wide advanced test setup.
  - `src/lisp/tests_advanced_tests.c3` now returns the full generated source
    slice from `advanced_test_cstr_slice(...)`, and the wide advanced builder
    callsites in `src/lisp/tests_advanced_core_semantics_groups.c3`,
    `src/lisp/tests_advanced_macro_hygiene_groups.c3`,
    `src/lisp/tests_advanced_type_dispatch_groups.c3`, and
    `src/lisp/tests_advanced_type_parametric_groups.c3` now use that helper
    consistently instead of truncating generated forms before parse.
  - `src/lisp/tests_advanced_core_semantics_groups.c3` now includes direct
    `let ^rec` regressions proving a recursively-invoked closure preserves
    captured array mutation state across `push!` updates.
  - `src/lisp/tests_harness_helpers.c3` now prints setup failure coordinates
    and the generated source text when a test fixture parse/eval step fails,
    which keeps wide generated-source regressions local to the failing setup.
  - validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_ADVANCED_GROUP_FILTER=advanced-core-semantics ./build/main --test-suite lisp` -> `pass=68 fail=0`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp` -> `pass=1164 fail=0`

- Closed the process-handle concurrency lane for `process-wait` / `process-kill` reuse:
  - `src/lisp/async_process_signal_runtime.c3` now gives each process handle a shared in-flight guard, and `src/lisp/async_process_lifecycle.c3` preserves the closed state while the handle is in use so concurrent reuse fails closed instead of racing on the same live `uv_process`.
  - `src/lisp/async_process_signal_dns_process.c3` now returns a normalized `io/process-handle-busy` error when a concurrent `process-wait` or `process-kill` attempts to reuse the same live process handle.
  - `src/lisp/tests_advanced_io_effect_ffi_scheduler_boundary.c3` and `src/lisp/tests_advanced_io_effect_ffi_groups.c3` now cover the concurrent reuse contract directly.
  - validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp` -> pass

- Closed the residual env-copy shared-wrapper hole that remained after the
  earlier return-boundary and ESCAPE-lane hardening:
  - `src/lisp/eval_env_copy_values.c3` no longer returns `ARRAY` /
    `HASHMAP` / `SET` / `METHOD_TABLE` bindings by raw pointer identity during
    env-copy. Those wrappers now route through the same boundary-copy policy
    already used by other promotion paths, so source-scope shared wrappers are
    cloned when env-copy crosses into a different target scope.
  - `src/lisp/tests_memory_lifetime_env_copy_groups.c3` now covers the shared
    wrapper env-copy contract directly, proving disjoint collection and method
    table wrappers are cloned and remain valid after source-scope teardown.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` wires that
    focused regression into the memory-lifetime env-copy smoke lane.

- Closed a post-wave fail-closed cleanup and transport follow-up:
  - `src/lisp/eval_promotion_copy_route_helpers.c3` and
    `src/lisp/eval_promotion_escape_structured.c3` now reuse the shared
    `method_table_free_partial_heap(...)` helper so method-table abort paths
    destroy already-copied heap signatures before freeing the table shell.
  - `src/lisp/aot_runtime_bridge_trampoline.c3` now rejects negative arg
    counts in `aot::apply_multi(...)`, `aot::apply_multi_tail(...)`, and the
    shared multi-arg fast path before any signed-to-unsigned conversion or
    malformed AOT closure application can proceed.
  - `src/lisp/compiler_native_call_compilation_flat_style.c3` now frees
    `_closure_data_*` when AOT closure construction returns `ERROR`, matching
    the existing safe closure-emission path.
  - `src/lisp/http.c3`, `src/lisp/http_connection.c3`, and
    `src/lisp/http_url_response.c3` now:
    - close HTTP/TCP/TLS connections on all post-connect early-return paths,
    - propagate transport read errors instead of parsing partial error output
      as a synthetic success response,
    - reject out-of-range URL ports at parse time.

- Closed the validation live-e2e evidence lane:
  - `scripts/run_validation_status_summary.sh` now runs `scripts/run_e2e.sh`
    as part of the summary bundle and refuses to report green unless the
    current run proves Stage 4 live e2e execution and a passing Stage 5 result
    in the generated logs.
  - `TODO.md` no longer tracks `AUDIT-VALIDATION-E2E-LIVE-003`; the remaining
    live items are the process concurrency, compiler diagnostic parity, and
    method-table abort-cleanup coverage lanes.

- Closed the remaining compiler diagnostic parity and method-table abort
  coverage lanes:
  - `src/lisp/jit_jit_apply_multi_prims.c3` now formats non-tail fixed-arity
    and variadic multi-arg under-application with the same canonical messages
    already used by the tail and AOT helpers.
  - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now asserts that
    compiler syntax failures report user-source coordinates after the stdlib
    prelude line offset is stripped from diagnostics.
  - `src/lisp/eval_promotion_root_clones.c3` now exposes narrow
    method-table-abort cleanup telemetry, and
    `src/lisp/jit_jit_closure_support.c3` now exposes a targeted
    heap-signature copy failure seam.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves both
    copy-to-parent and escape-promotion method-table abort paths reclaim
    already-copied heap signatures, and
    `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` includes that
    regression in the bounded smoke lane.
  - `TODO.md` is back to zero live backlog items for this audit pass.

- Closed the shared-wrapper boundary hardening lane and the integration
  blockers that surfaced while validating it:
  - `src/lisp/eval_promotion_copy.c3`,
    `src/lisp/eval_promotion_copy_route_helpers.c3`,
    `src/lisp/eval_promotion_escape_leaf.c3`, and
    `src/lisp/eval_promotion_escape_structured.c3` no longer return disjoint
    `ARRAY` / `HASHMAP` / `SET` / `METHOD_TABLE` wrappers by pointer identity
    once fast reuse is declined; they now defensively clone those wrappers and
    recurse through nested payload edges.
  - `METHOD_TABLE` shared-wrapper clones now keep copied signatures on heap
    storage so `scope_dtor_value(...)` remains compatible with the existing
    destructor contract and does not attempt to free scope-owned signature
    arrays.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now covers both
    defensive shared-wrapper cloning and the nested fail-closed boundary-copy
    path.
  - `src/lisp/tests_memory_lifetime_finalize_groups.c3` no longer reads a
    detached-scope env frame after `scope_release(detached)`, removing the
    finalize-lane UAF that was blocking bounded smoke validation.
  - Validation:
    - `rm -rf build/obj/linux-x64 build/main && c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` ->
      `pass=81 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=0:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` ->
      `pass=81 fail=0`

- Closed the validation integrity lane:
  - `scripts/check_status_consistency.sh` now accepts the current zero-item
    backlog wording.
  - `scripts/run_validation_container.sh` now serializes bounded validation
    runs with a repo-local lock.
  - `scripts/run_validation_status_summary.sh` now treats missing required
    `OMNI_TEST_SUMMARY` telemetry as a validation failure.
  - `scripts/c3c_limits.sh` now preserves quoted extra Docker args.
  - `scripts/run_e2e.sh` and `scripts/check_e2e_baseline_policy.sh` now guard
    Stage 3 compile-source parity explicitly.
  - Validation:
    - `scripts/check_status_consistency.sh`
    - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `bash -n scripts/check_status_consistency.sh scripts/run_validation_container.sh scripts/run_validation_status_summary.sh scripts/c3c_limits.sh scripts/run_e2e.sh scripts/check_e2e_baseline_policy.sh`

- Closed the focused CRUD concurrency follow-up:
  - `examples/deduce_crud_server.omni` now guards CRUD mutation entrypoints
    with a shared atomic write gate, preventing spawned overlap from surfacing
    raw runtime `ERROR` payloads.
  - the shipped application contract is now explicit:
    - one overlapping write succeeds,
    - the other resolves as a normalized application conflict
      (`crud write already in progress` or `item already exists`),
    - and only one row persists for the shared id.
  - `src/lisp/tests_runtime_feature_http_groups.c3`,
    `src/lisp/tests_tests.c3`, and `src/lisp/tests_slice_policy.c3` now move
    the spawned CRUD concurrency probe into the focused `http-crud` slice so
    the broad `http` slice remains deterministic.
  - Validation:
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` ->
      `pass=29 fail=0`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http-crud ./build/main --test-suite lisp` ->
      `pass=1 fail=0`

- Closed the compiler / JIT / AOT parity hardening lane for multi-arg apply,
  parse fail-closed behavior, and generated closure-capture OOM handling:
  - `src/lisp/jit_jit_apply_multi_prims.c3` and
    `src/lisp/jit_jit_apply_multi_prims_tail.c3` now null-guard multi-arg
    argument-buffer allocation for primitive and method-table dispatch, using
    a focused test seam so low-memory paths return an explicit error instead
    of dereferencing `null`.
  - `src/lisp/parser_top_level_parse.c3` and
    `src/lisp/compiler_program_pipeline_helpers.c3` now fail closed on parser
    error instead of returning a silently truncated prefix program to caller
    pipelines.
  - `src/lisp/aot.c3` now provides shared arg-list counting plus exact/minimum
    arity helpers, and `src/lisp/compiler_code_emission_lambda_defs.c3` now
    emits those checks into generated multi-arg lambda entrypoints so
    under-application rejects deterministically, malformed arg lists fail
    closed, and over-application keeps JIT-style chaining via
    `aot::apply_multi(...)`.
  - `src/lisp/compiler_native_call_compilation_flat_style.c3` now guards
    generated closure-capture allocation in flat expression lowering without
    emitting invalid raw `return` statements into non-`Value*` contexts.
  - focused regressions landed in:
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    - `src/lisp/tests_compiler_core_groups_fail_closed.c3`
    - `src/lisp/tests_compiler_core_groups.c3`
  - Validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp` ->
      `pass=35 fail=0`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` ->
      `pass=193 fail=0`
    - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`

- Hardened the async / REPL / FFI safety lane against partial-init and handle
  confusion defects:
  - `src/lisp/eval_repl_server_worker.c3` now refuses to start the REPL
    worker thread unless both mutex and condition-variable initialization
    succeeded, so partial-init paths fail closed before the worker can touch
    uninitialized sync state.
  - `src/lisp/async_socket_handle_runtime.c3`,
    `src/lisp/tls_handle_lifecycle.c3`, and
    `src/lisp/prim_io_fs_stream.c3` now validate exact FFI handle names before
    casting TCP/UDP/TLS/FS payloads, preventing unrelated `FFI_HANDLE` boxes
    from being reinterpreted as transport state.
  - `src/lisp/async_process_spawn.c3` now treats constructor-returned error
    values from `make_process_handle(...)` and `make_fs_handle(...)` as hard
    failures instead of packaging them into a success-shaped spawn result.
  - `src/lisp/http_url_response.c3` now rejects malformed `:port` suffixes
    with trailing garbage or missing digits, and trims HTTP response header
    slices so the exposed header string no longer retains delimiter residue.
  - focused regressions landed in:
    - `src/lisp/tests_runtime_async_repl_server_groups.c3`
    - `src/lisp/tests_advanced_io_effect_ffi_groups.c3`
    - `src/lisp/tests_runtime_feature_http_groups.c3`
  - Validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=async ./build/main --test-suite lisp` ->
      `pass=61 fail=0`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system ./build/main --test-suite lisp` ->
      `pass=42 fail=0`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_VERBOSE=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp`
      is now green after the HTTP/CRUD lane was made deterministic.
- Closed the residual broad-HTTP CRUD lane and split the remaining concurrency
  question into its own explicit backlog item:
  - `examples/deduce_crud_server.omni` now treats keyed `deduce 'fact!` as the
    atomic source of truth in `repo/create` and maps
    `deduce/integrity-key-conflict` to `Err "item already exists"`, removing
    the check-then-insert race between the pre-query and the keyed write.
  - `src/lisp/tests_runtime_feature_http_groups.c3` now keeps deterministic
    duplicate-id coverage in the broad `http` slice by issuing two POSTs with
    the same id but different payloads, asserting first-write success plus
    second-write conflict without depending on spawned concurrent mutation
    ordering.
  - The separate product/runtime question of whether spawned concurrent CRUD
    writes over one in-memory Deduce DB are supported is now tracked in
    `TODO.md` as `HTTP-CRUD-CONCURRENT-WRITES-001`.
  - Validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` -> `pass=29 fail=0`

- Nested boundary copy and ESCAPE promotion now fail closed for opaque
  primitive payloads instead of silently degrading structured wrappers:
  - `src/lisp/eval_promotion_copy.c3`,
    `src/lisp/eval_promotion_copy_route_helpers.c3`, and
    `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now thread
    `BoundaryCopyFault` through `CONS`, `PARTIAL_PRIM`, and `ITERATOR`
    boundary-copy paths, so nested primitive-copy rejection propagates as
    `BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD` rather than embedding
    `null` payload edges into copied wrappers.
  - `src/lisp/eval_promotion_copy_route_helpers.c3` also delays primitive
    wrapper allocation until after opaque-payload legality is known, so failed
    primitive-copy attempts do not allocate unreachable target-scope wrapper
    garbage before rejecting.
  - `src/lisp/eval_promotion_escape_leaf.c3` and
    `src/lisp/eval_promotion_escape_structured.c3` now propagate nested
    promotion failures through `CONS`, `PARTIAL_PRIM`, and `ITERATOR` escape
    promotion instead of returning partially rebuilt wrappers whose payloads
    were replaced with boundary-error values.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` and
    `src/lisp/tests_memory_lifetime_promotion_context_groups.c3` now cover the
    nested fail-closed regressions for boundary-copy and escape-promotion
    paths, and `src/lisp/tests_memory_lifetime_groups.c3` wires those focused
    regressions into the smoke suite.
  - Validation:
    - `c3c build`
    - `rm -rf build/obj/linux-x64 build/main && mkdir -p build/obj/linux-x64/tmp_c_compile && c3c build --sanitize=address`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` ->
      `pass=80 fail=0`

- Aligned FFI parser and AOT helper payload ownership with the repo's existing
  arena/scope lifetime model:
  - `src/lisp/parser_ffi.c3` and `src/lisp/parser_ffi_helpers.c3` now allocate
    successful `[ffi λ ...]` AST payloads (`ExprFfiFn`, copied C symbol names,
    and parameter tables) from the AST arena instead of raw `malloc`, so
    parser-owned FFI metadata is reclaimed by `ast_arena_destroy(...)` with the
    rest of the AST.
  - `src/lisp/aot_runtime_bridge_closure.c3`,
    `src/lisp/aot_runtime_bridge_ffi.c3`, and
    `src/lisp/aot_runtime_bridge_ffi_helpers.c3` now allocate AOT primitive
    payloads from `interp.root_scope` instead of raw heap ownership, keeping
    closure payloads and bound-FFI metadata on the same deterministic lifetime
    as the primitive values that reference them.
  - This closes the post-e2e AddressSanitizer leak paths that were still
    reachable from:
    - successful compiler-slice parsing of `[ffi λ ...]` forms,
    - `aot::make_closure(...)` / `aot::make_variadic_closure(...)`,
    - `aot::ffi_declare_fn(...)`.
  - Follow-up interpreter parity hardening:
    - `src/lisp/eval_ffi_eval.c3` now allocates interpreter-side declarative
      `ffi λ` bound-function payloads (`FfiBoundFn`, copied library name,
      copied symbol name, and ABI tag table) from `interp.root_scope` instead
      of raw heap ownership, matching the root-lifetime primitive values that
      retain them through `prim_val.user_data`.
    - This closes the same ownership mismatch for non-AOT declarative FFI
      bindings, so interpreter-created FFI primitives no longer depend on an
      unowned heap payload surviving past primitive teardown.
  - Root-boundary primitive promotion now fails closed for opaque primitive
    payloads:
    - `src/lisp/eval_promotion_root_clone_basic.c3` no longer raw-copies
      `Primitive.user_data` when promoting a child-owned primitive wrapper into
      root storage. If a primitive carries opaque payload state, root-store
      promotion now returns an explicit error instead of aliasing foreign
      lifetime-owned payload through a shallow struct copy.
    - `src/lisp/tests_memory_lifetime_root_boundary_groups.c3` now covers both
      the allowed plain primitive clone path and the fail-closed rejection path
      for child-owned primitives with non-null `user_data`.
  - Ordinary ESCAPE-lane primitive promotion now follows the same ownership
    rule instead of raw-copying primitive wrapper pointers:
    - `src/lisp/eval_promotion_escape_leaf.c3` now deep-clones plain
      `Primitive` headers into the destination ESCAPE lane, which avoids
      double-freeing the same heap `Primitive*` when TEMP and ESCAPE wrappers
      teardown in the same scope.
    - ESCAPE promotion now rejects child/disjoint primitive wrappers carrying
      opaque `user_data` with an explicit boundary error instead of aliasing
      foreign payload ownership through the shared-wrapper fast path.
    - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now centralizes the
      primitive opaque-payload predicate and plain-header clone helper used by
      both root-store and ESCAPE promotion paths.
    - `src/lisp/tests_memory_lifetime_groups.c3` now covers both the plain
      primitive ESCAPE clone path and the fail-closed opaque-payload rejection
      path for disjoint-scope promotion.
  - Parent-boundary and env-copy primitive handling now use the same contract:
    - `src/lisp/eval_promotion_copy.c3` no longer returns child-owned
      `PRIMITIVE` wrappers unchanged from `copy_to_parent(...)`; plain
      primitive headers are cloned into the target scope and opaque
      `user_data` payloads now surface as a typed boundary-copy fault.
    - `src/lisp/eval_env_copy_values.c3` no longer treats all primitive values
      as root/external-lifetime by default during env copy; disjoint plain
      primitives are cloned through boundary policy and opaque payloads now
      fail the env copy with `BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY`.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` and
      `src/lisp/tests_memory_lifetime_env_copy_groups.c3` now cover the plain
      primitive clone path plus opaque-payload rejection for return/env-copy
      boundaries.
  - Validation:
    - `c3c build --sanitize=address`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` ->
      `pass=189 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` ->
      `pass=75 fail=0`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite stack` ->
      `pass=22 fail=0`
    - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`
    - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(block (define [ffi lib] libc "libc.so.6") (define [ffi lambda libc] (strlen (^String s)) ^Integer) 0)'` ->
      `0`
