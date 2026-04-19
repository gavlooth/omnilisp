# Memory Changelog Index Part 14

Source: `memory/CHANGELOG.md`

  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the remaining runtime/status payload-builder slice of the internal
  collection-constructor migration:
  - `src/lisp/async_process_signal_dns_process.c3`,
    `src/lisp/async_process_spawn.c3`, and
    `src/lisp/prim_io_fs_handles.c3`
    now route process/fs status payload construction through checked
    `HASHMAP` / `ARRAY` helpers plus checked insertion instead of mutating
    unchecked constructor results.
  - `src/lisp/http_url_response.c3` now routes parsed HTTP response payload
    construction through the same checked map contract.
  - `process-spawn` now also closes its live process/fs handles if final
    success-payload construction fails, so constructor OOM no longer strands a
    half-built success result with open resources.
  - `src/lisp/eval_dispatch_error_payloads.c3` now treats dispatch payload
    dictionaries as optional under OOM, so the primary lambda/ambiguous
    typed error still returns even if payload-map construction or insertion
    fails.
  - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now applies
    that same optional-payload contract to unhandled-effect error payloads.
  - `src/lisp/primitives_meta_types_ctor_helpers.c3` now makes
    `ctor_mismatch_data(...)` fail closed by returning `null` instead of
    dereferencing unchecked hashmap payloads.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves fs,
    process-spawn, process-wait, HTTP response payload, dispatch/runtime-effect
    payload, and ctor-mismatch constructor OOM paths fail closed.
  - this closes the staged internal collection-constructor migration lane; no
    runtime/status residual remains in `TODO.md`.

- Closed the schema-explain payload-builder slice of the internal collection
  constructor migration:
  - `src/lisp/schema_explain_payload_helpers.c3` now centralizes checked map
    construction and checked `explain_dict_set*` insertion under one explicit
    `"schema explain: out of memory"` contract.
  - `src/lisp/schema_explain_helpers.c3`,
    `src/lisp/schema_explain_effect.c3`,
    `src/lisp/schema_explain_effect_result_payload.c3`,
    `src/lisp/schema_explain_effect_runtime.c3`, and
    `src/lisp/schema_explain_effect_helpers.c3`
    now route entrypoint/result/candidate/source payload maps through that
    checked contract instead of dereferencing unchecked `make_hashmap(...)`
    results.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves
    dispatch explain, effect explain, and helper payload-map construction all
    fail closed on forced hashmap allocation failure.
  - residual unchecked collection-constructor work is now narrower again:
    - remaining runtime/status payload builders only
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=134 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=134 fail=0`

- Closed the data-format bridge slice of internal collection-constructor OOM
  hardening:
  - `src/lisp/json.c3` now routes object/array construction through checked
    `ARRAY` / `HASHMAP` helpers and checked hashmap insertion.
  - `src/lisp/primitives_toml_bridge.c3` now does the same for TOML table/array
    conversion.
  - `src/lisp/primitives_data_formats_csv_parse.c3` now uses checked result-row
    and row-rotation array constructors instead of assuming `make_array(...)`
    succeeded.
  - recursive data-format conversion in JSON/TOML now propagates nested
    constructor/materialization `ERROR`s directly instead of embedding them into
    partial arrays or dicts.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves JSON,
    TOML, and CSV constructor OOM paths fail closed.
  - residual backlog is now split by real callsite family:
    - schema explain payload maps
    - remaining runtime/status payload builders
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=133 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=133 fail=0`

- Closed iterator tail error propagation so iterator OOM no longer degrades
  into silent truncation:
  - `src/lisp/primitives_iter_state.c3` now centralizes iterator pair
    construction through `iterator_make_pair_or_propagate(...)`.
  - `src/lisp/primitives_iter_sources.c3` and
    `src/lisp/primitives_iter_coroutine.c3` now use that helper whenever a
    thunk builds `(item . next)`, so any tail `ERROR` is returned directly
    instead of being wrapped in a `CONS` and later mistaken for iterator
    termination.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves both
    a source iterator thunk and a coroutine iterator thunk propagate tail
    allocation failure directly.
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=128 fail=0`

- Closed the runtime constructor OOM substrate for the live iterator/error and
  language-facing collection surfaces:
  - `src/lisp/value_core_types.c3`,
    `src/lisp/value_interp_alloc_helpers.c3`,
    `src/lisp/value_constructors_lifecycle.c3`, and
    `src/lisp/primitives_meta_types.c3`
    now track whether `STRING` / `ERROR` chars are heap-owned, so fallback
    literal-backed error values no longer flow into invalid frees.
  - `src/lisp/value_constructors.c3` now makes `make_error(...)` fail closed:
    failed message allocation returns a printable fallback `ERROR` instead of
    dereferencing a failed heap allocation.
  - `src/lisp/value_predicates_accessors_basic.c3` and
    `src/lisp/prim_collection_hashmap.c3`
    now expose checked `ARRAY` / `HASHMAP` / `SET` constructor and hashmap-grow
    helpers with deterministic OOM seams.
  - the runtime-dependent surfaces that were actually crashing on this substrate
    now use the checked path:
    - raise payload construction
    - `Dictionary`
    - `Set`
    - `to-array`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves:
    - `make_error(...)` allocation failure keeps a printable fallback error,
    - iterator ctor raises survive payload-map allocation failure,
    - `to-array` fails closed on result-array allocation failure,
    - checked collection constructor and grow failures return ordinary `ERROR`
      values instead of crashing.
  - residual scope is now narrower and explicit:
    - broad internal migration from unchecked `make_array(...)` /
      `make_hashmap(...)` callsites remains a separate follow-up lane instead
      of being hand-waved as complete.

- Closed the broader shared `StringVal` builder OOM lane so builder creation
  and growth now fail closed instead of crashing before result materialization:
  - `src/lisp/prim_string_format_helpers.c3` now:
    - returns `null` from `strval_new(...)` on builder allocation failure,
    - gives `strval_ensure(...)` an explicit `bool` failure contract with size
      overflow guards,
    - stops `strval_push(...)` / `strval_append(...)` / padding helpers from
      writing after a failed growth attempt,
    - and exposes deterministic seams for initial builder allocation and
      builder growth failure.
  - `src/lisp/prim_string_ops.c3`,
    `src/lisp/prim_string_format.c3`, and
    `src/lisp/prim_string_format_directives.c3`
    now propagate those builder failures as typed runtime OOM errors instead of
    continuing with invalid builder state.
  - parser string literal construction in:
    - `src/lisp/parser_datum_helpers.c3`
    - `src/lisp/parser_expr_atoms.c3`
    - `src/lisp/parser_patterns_values.c3`
    - `src/lisp/parser_quasiquote_datum_helpers.c3`
    now uses the same checked builder path and fails closed with parser errors.
  - `src/lisp/primitives_meta_types.c3` no longer uses unchecked `StringVal`
    allocation in the `unsafe-free` error path.
  - new regressions:
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now covers
      direct result-wrapper failure plus builder allocation/growth failure for
      runtime string helpers.
    - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now covers parser
      string-literal builder allocation failure.
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=127 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=127 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp'` -> `pass=191 fail=0`

- Closed adjacent runtime-helper allocation fail-open paths and tightened stale
  lifetime regressions exposed by ASAN:
  - `src/lisp/eval_apply.c3` now fails closed if chained partial application
    cannot allocate its next `PARTIAL_PRIM` wrapper, returning an eval error
    instead of dereferencing a null `interp.alloc_value()` result.
  - `src/lisp/primitives_iter_state.c3` and
    `src/lisp/value_predicates_accessors_basic.c3` now fail closed when
    iterator thunk or iterator wrapper allocation fails, surfacing runtime OOM
    errors instead of writing through null wrapper pointers.
  - `src/lisp/prim_string_ops.c3`, `src/lisp/prim_string_format.c3`, and
    `src/lisp/prim_string_format_helpers.c3` now route final string result
    materialization through one checked helper so forced wrapper-allocation
    failure disposes the transient `StringVal` builder and returns a typed
    runtime error.
  - `src/lisp/http_url_response.c3` now routes `status` / `headers` / `body`
    key materialization through one checked helper, so response parsing no
    longer dereferences null field-key allocations.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` adds direct smoke
    regressions for:
    - chained partial allocation failure,
    - iterator thunk and wrapper allocation failure,
    - string result-wrapper allocation failure in replace/repeat/format paths,
    - HTTP response field-key allocation failure for all three emitted keys.
  - ASAN exposed stale test-only post-release reads in:
    - `src/lisp/tests_memory_lifetime_groups.c3`
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3`
    - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3`
    Those assertions now snapshot expected error/copy state before releasing
    the relevant target scope.
  - validation:
    - `rm -rf build/obj/linux-x64 build/main && c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=126 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=126 fail=0`

- Closed remaining boundary leaf-wrapper allocation fail-open paths for
  `INSTANCE`, `FFI_HANDLE`, and `TIME_POINT` copies:
  - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now fails closed before
    `instance_retain(...)` / `ffi_handle_retain(...)` if destination wrapper
    allocation fails for `copy_instance_to_parent(...)` or
    `copy_ffi_handle_to_parent(...)`.
  - `src/lisp/eval_promotion_copy_route_helpers.c3` now returns a typed
    boundary error if `copy_time_point_to_parent(...)` cannot allocate its
    wrapper instead of dereferencing a null result.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves forced
    boundary-copy wrapper allocation failure:
    - does not retain instance owner scopes,
    - does not increment `FFI_HANDLE` refcounts or trigger early finalization,
    - and surfaces a typed error for `TIME_POINT` copies.
  - validation:
    - `rm -rf build/obj/linux-x64 build/main && c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/check_status_consistency.sh`

- Closed env-copy / return-boundary wrapper-allocation null-deref paths for
  closure and iterator special cases:
  - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now treats a null
    destination wrapper in `copy_parent_clone_closure_payload(...)` as a
    typed boundary allocation failure instead of dereferencing it, and
    `copy_closure_to_parent(...)` now supports deterministic wrapper alloc
    failure injection for regression coverage.
  - `src/lisp/eval_env_copy_values.c3` now:
    - fails closed if `copy_env_copy_time_point(...)` cannot allocate its
      wrapper,
    - routes env-copy closure wrapper allocation through the same guarded
      helper path, and
    - rolls back iterator inner payloads and returns `null` if iterator wrapper
      allocation itself fails instead of dereferencing a null wrapper.
  - `src/lisp/tests_memory_lifetime_env_copy_closure_groups.c3` now proves:
    - `copy_to_parent(...)` closure wrapper allocation failure does not retain
      detached closure env scopes or disturb later target/source teardown, and
    - env-copy closure wrapper allocation failure surfaces
      `BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY` and preserves detached
      closure env-scope ownership symmetry.
  - validation:
    - `rm -rf build/obj/linux-x64 build/main && c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/check_status_consistency.sh`

- Closed the remaining full-slice `jit-policy` crash by hardening the TCO
  recycle TEMP-graph scan:
  - `src/lisp/jit_jit_eval_scope_chain_helpers.c3` no longer allocates four
    `4096`-entry pointer arrays on the runtime stack inside
    `jit_graph_binding_reaches_temp_scope(...)`.
  - the scan state now lives in one heap-allocated `JitTempGraphScan`, so the
    TCO recycle safety gate no longer segfaults on entry when a top-level JIT
    policy case drives that helper through a smaller runtime stack.
  - this specifically restores the `stale-raise-scrub` JIT policy case, whose
    top-level run path was only exposing the stack-footprint bug in the nested
    graph scanner.
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=stale-raise-scrub ./build/main --test-suite lisp'` -> `1 passed, 0 failed`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp'` -> `pass=41 fail=0`

- Closed the normal-teardown and allocation-failure symmetry gaps for
  boundary-owned `CONTINUATION` wrappers:
  - `src/lisp/value_constructors_lifecycle.c3` now releases retained
    handle-state on normal scope teardown when a copied/promoted continuation
    wrapper carries `continuation_boundary_owned`.
  - `src/lisp/eval_promotion_copy_route_helpers.c3` and
    `src/lisp/eval_promotion_escape_leaf.c3` now:
    - register dtors for successful copied/promoted continuation wrappers that
      actually introduced the retained handle-state ref, and
    - immediately release that retained handle-state ref if wrapper
      allocation fails after the retain step.
  - `src/lisp/eval_env_copy_frame_helpers.c3` and
    `src/lisp/eval_promotion_root_clones.c3` now tombstone those copied
    wrapper dtors before manual rollback cleanup, so abandoned continuation
    wrappers unwind exactly once.
  - `src/lisp/jit_jit_runtime_effects.c3` now decrements
    `HandleEffectState.continuation_refcount` during `resolve` only when the
    resolved continuation actually owned the retained handle-state ref, so one
    unretained continuation can no longer consume another continuation’s
    shared-state retain.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves:
    - successful boundary-owned continuation wrappers release their retained
      handle-state ref on normal target-scope teardown,
    - boundary copy allocation failure after the retain step releases
      immediately, and
    - ESCAPE promotion allocation failure after the retain step releases
      immediately.
  - validation:
    - `rm -rf build/obj/linux-x64 build/main && c3c build`
    - `rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=118 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=multi-interp-lifetime,continuation-teardown,shared-handle-state-teardown,cross-interp-continuation-guard,escaped-handle-continuation-guard,side-effect-escaped-handle-continuation-guard ./build/main --test-suite lisp'` -> `6 passed, 0 failed`
    - `scripts/check_status_consistency.sh`

- Closed a continuation rollback symmetry gap in boundary/env-copy cleanup:
  - `src/lisp/eval_promotion_copy_route_helpers.c3` and
    `src/lisp/eval_promotion_escape_leaf.c3` now mark copied/promoted
    `CONTINUATION` wrappers when that specific wrapper introduced boundary
    handle-state retention.
  - `src/lisp/eval_env_copy_frame_helpers.c3` and
    `src/lisp/eval_promotion_root_clones.c3` now unwind that retention when a
    copied continuation wrapper is abandoned during env-copy rollback or
    partial boundary cleanup, instead of leaving the shared continuation
    handle-state refcount artificially elevated after failure.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now proves a
    later env-copy binding fault releases a first copied continuation retain.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves shared-
    wrapper partial cleanup releases the same continuation retain when a later
    opaque child aborts the copy.
  - validation:
    - `c3c build`
    - `c3c build --sanitize=address`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/check_status_consistency.sh`

- Closed a nested `CONS` alias blind spot in shared provenance and env-copy:
  - `src/lisp/eval_boundary_provenance.c3` now routes `CONS` reuse checks
    through an iterative cons-specific helper that validates each nested cons
    shell against the releasing scope and target scope chain before reusing the
    outer wrapper by identity.
  - this closes the case where a target-chain outer `CONS` used to be treated
    as alias-safe even though a nested `cdr` cons shell still belonged to the
    releasing/source scope and only exposed scalar leaves.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves ordinary
    boundary copy and ESCAPE promotion defensively clone that nested cons
    structure.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now proves
    env-copy clones the same target-chain outer `CONS` instead of reusing it by
    identity when the nested cons shell still belongs to the source scope.
  - validation:
    - `c3c build`
    - `c3c build --sanitize=address`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/check_status_consistency.sh`

- Closed the wrapper-slot leak on shared-wrapper and root-store partial aborts:
  - `src/lisp/eval_promotion_copy_route_helpers.c3`,
    `src/lisp/eval_promotion_root_clone_basic.c3`, and
    `src/lisp/eval_promotion_root_clones.c3` now delay destination wrapper
    allocation/registration until after fallible child-copy and payload clone
    work succeeds.
  - when wrapper allocation itself fails after payload materialization, those
    paths now route through the existing partial-cleanup helpers so copied
    child retains and heap payloads are unwound before returning.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves repeated
    failed shared-wrapper copy attempts do not monotonically grow the
    surviving target scope allocation count.
  - `src/lisp/tests_memory_lifetime_root_boundary_groups.c3` now proves the
    same invariant for repeated failed root-store method-table clone attempts
    against `root_scope`.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/check_status_consistency.sh`

- Closed a destination-commit promotion-context drift:
  - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` now routes
    direct destination escape promotion through an explicit ctx-aware helper
    instead of silently consuming `interp.active_promotion_ctx`.
  - `src/lisp/eval_boundary_commit_escape_helpers.c3` and
    `src/lisp/eval_boundary_commit_destination.c3` now use that helper for
    releasing-scope retry, mixed-destination retry, and direct destination
    promotion, so the caller-provided `PromotionContext` owns the same
    memo/budget/abort epoch across the whole destination-commit lane.
  - destination-builder teardown now restores both `memo_head` and the
    builder-local scope-chain cache snapshot, so temporary build-scope cache
    entries do not survive after the builder returns or aborts.
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
    proves:
    - repeated builder calls do not retain builder-local scope-chain cache
      state, and
    - a non-active caller `PromotionContext` still receives the abort state
      from direct destination promotion while the unrelated active context
      remains untouched.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/check_status_consistency.sh`

- Closed a direct `CONS` rollback symmetry gap in boundary copy and ESCAPE promotion:
  - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now treats iterative
    `CONS` copy as transactional: newly allocated spine cells are initialized
    to null edges, and any already-copied car/cdr payloads are unwound through
    `boundary_cleanup_materialized_value(...)` if a later cdr copy or spine
    allocation fails.
  - `src/lisp/eval_promotion_escape_structured.c3` now applies the same
    rollback rule to iterative ESCAPE-lane `CONS` promotion, so promoted car
    retains are not left live until target teardown when a later cdr promotion
    or tail-cell allocation fails.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves direct
    `CONS` boundary copy immediately unwinds a copied detached-closure car when
    the cdr fails with an opaque primitive payload fault.
  - `src/lisp/tests_memory_lifetime_groups.c3` now proves the same invariant
    for direct ESCAPE `CONS` promotion.
  - validation:
    - `c3c build`
    - `c3c build --sanitize=address`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=102 fail=0`
    - `scripts/check_status_consistency.sh`

- Closed a destination-commit promotion-context drift and a nested wrapper
  rollback symmetry gap:
  - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` now threads the
    caller `PromotionContext` through destination iterator detachment instead
    of discarding it on the detach subpath, so destination-built iterator
    copies now honor same-epoch memo reuse, budget/accounting, and context
    abort propagation like the rest of the builder lane.
  - `src/lisp/eval_promotion_root_clones.c3` now makes
    `boundary_cleanup_materialized_value(...)` recurse through copied nested
    `PARTIAL_PRIM` and `ITERATOR` payloads before cleaning the outer wrapper,
    closing the case where shared-wrapper late-failure cleanup would leave
    copied closure/env retains live when the already-materialized child was
    wrapped in a copied partial or iterator shell.
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
    proves repeated destination iterator detachment in one builder epoch
    reuses the same copied closure via the shared promotion context.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves failed
    copied `ARRAY` wrappers unwind nested copied `PARTIAL_PRIM` and `ITERATOR`
    payload retains immediately and do not destroy the original owner closure
    env scope during later target teardown.
  - validation:
    - `c3c build`
    - `c3c build --sanitize=address`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=100 fail=0`
    - `scripts/check_status_consistency.sh`

- Closed a TCO recycle TEMP-graph detection drift:
  - `src/lisp/jit_jit_eval_scope_chain_helpers.c3` no longer treats
    “graph-carrying wrapper itself is outside TEMP” as sufficient for the
    recycle fast-reset gate.
  - `env_chain_has_graph_binding_values_in_temp_scope(...)` now walks nested
    `CONS`, `ARRAY`, `HASHMAP` / `SET`, `CLOSURE`, `PARTIAL_PRIM`,
    `METHOD_TABLE`, `MODULE`, `ITERATOR`, and `INSTANCE` edges transitively,
    with bounded cycle-aware visited sets, and it fails closed by reporting
    “unsafe, do not fast-reset” on scan overflow.
  - this closes the case where `runtime_prepare_tco_recycle_env(...)` could
    call `scope_reset_temp_lane(...)` even though an owner-scope or
    target-chain wrapper binding still pointed transitively into the recycle
    scope TEMP lane.
  - `src/lisp/tests_memory_lifetime_tco_budget_groups.c3` now includes a
    focused manual regression proving a TCO recycle-scope nested `CONS` inside
    an owner-scope `ARRAY` binding forces scope replacement and env-copy
    instead of in-place TEMP reset, and
    `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` wires that probe
    into the bounded smoke lane.
  - validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed a JIT TCO nested-alias reuse hole for partial wrappers:
  - `src/lisp/jit_jit_eval_scope_chain_helpers.c3` no longer treats
    “wrapper already in the target chain” as sufficient to skip copying during
    TCO env-frame transfer.
  - target-chain bindings now still consult nested alias safety, so
    `PARTIAL_PRIM` / `ITERATOR` wrappers are copied when a shared-wrapper arg
    still contains a child from the releasing scope.
  - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now applies the same
    partial-payload alias rule in the shared fast-reuse precheck, so foreign
    `PARTIAL_PRIM` bindings with releasing-scope nested payloads no longer
    bypass TCO copy just because the wrapper itself lives outside the
    releasing scope.
  - `src/lisp/jit_jit_eval_scope_chain_helpers.c3` now also copies disjoint
    graph-carrying wrappers during TCO env-frame transfer instead of reusing
    them by identity merely because they are outside the releasing scope.
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3` now proves that TCO
    env-copy clones both target-chain and foreign partial/iterator wrappers
    instead of reusing them by identity when their nested array payload still
    points into the releasing scope.
  - the same focused JIT policy file now also proves that a foreign `ARRAY`
    binding is copied by value and remains valid after the foreign owner scope
    is released.
  - validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=tco-partial-shared-wrapper-edge-copy ./build/main --test-suite lisp`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=tco-foreign-partial-shared-wrapper-edge-copy ./build/main --test-suite lisp`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=tco-foreign-shared-wrapper-copy ./build/main --test-suite lisp`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed an env-copy rollback destructor-symmetry bug:
  - `src/scope_region_chunk_helpers.c3` now exposes
    `scope_cancel_dtor(...)` and `scope_cancel_dtor_escape(...)`, which
    tombstone the newest matching TEMP- or ESCAPE-lane destructor entry
    without mutating dtor-list topology.
  - `src/lisp/eval_env_copy_frame_helpers.c3` now cancels a copied
    target-scope value's registered dtor before manually unwinding that value
    during env-copy rollback, so rollback no longer replays
    `scope_dtor_value(...)` or `scope_dtor_closure(...)` and then lets target
    scope teardown run the same destructor again.
  - `src/lisp/eval_promotion_root_clones.c3`,
    `src/lisp/eval_promotion_copy_route_helpers.c3`,
    `src/lisp/eval_promotion_root_clone_basic.c3`, and
    `src/lisp/eval_promotion_escape_structured.c3` now use the same dtor
    tombstoning in shared-wrapper partial cleanup, so ordinary boundary copy,
    root-store clone, and ESCAPE-promotion late-failure cleanup no longer
    replay copied child dtors after manual unwind.
  - this closes the concrete double-dtor hazard for copied closures with
    retained standalone env scopes and keeps rollback transactional instead of
    relying on later target teardown to tolerate already-consumed ownership.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now proves the
    copied closure env scope is not destroyed when the abandoned target scope
    is released after a failed nested-array env-copy, and is destroyed exactly
    once when the original owner scope later releases it.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves the same
    teardown symmetry for ordinary shared-wrapper partial cleanup: failed
    copied `ARRAY` and `HASHMAP` wrappers leave the original closure env scope
    intact across target-scope release and destroy it exactly once when the
    foreign owner scope later releases it.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed a shared-wrapper cleanup symmetry follow-up:
  - `src/lisp/eval_promotion_root_clones.c3` now makes
    `boundary_cleanup_materialized_value(...)` recurse through copied nested
    `CONS`, `ARRAY`, `HASHMAP` / `SET`, and `METHOD_TABLE` children before
    destroying the outer copied wrapper.
  - ordinary boundary-copy, ESCAPE-promotion, and root-store clone partial
    cleanup paths all share that helper, so late failure in a copied shared
    wrapper no longer leaves nested copied closure retains live when the copied
    child itself was wrapped in a copied `CONS`.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` and
    `src/lisp/tests_memory_lifetime_root_boundary_groups.c3` now strengthen the
    existing partial-cleanup regressions by forcing the retained closure through
    a copied `CONS` shell before the later opaque primitive failure.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=92 fail=0`

- Closed an env-copy rollback follow-up for nested copied bindings:
  - `src/lisp/eval_env_copy_frame_helpers.c3` now recursively unwinds copied
    nested `CONS`, `ARRAY`, `HASHMAP` / `SET`, and `METHOD_TABLE` children
    during mid-frame env-copy rollback instead of only cleaning top-level
    wrappers plus iterator/partial shells.
  - failed env-copy now releases nested copied closure retains immediately at
    rollback time, rather than leaving them live until the abandoned target
    scope is eventually torn down.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now includes
    focused regressions proving nested `CONS` and `ARRAY` bindings unwind their
    copied closure-env retains immediately on rollback and remain stable across
    later target-scope teardown.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the destination-builder memo contract follow-up:
  - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3`,
    `src/lisp/eval_boundary_commit_escape_cons.c3`, and
    `src/lisp/eval_boundary_commit_escape_wrappers.c3` now make the shipped
    contract explicit: memo entries created while routing nested children
    inside temporary destination build scopes are builder-local and are
    discarded when the builder returns or aborts.
  - same-epoch alias reuse is therefore not guaranteed across repeated
    destination-builder invocations; the correctness contract is scoped to the
    committed ESCAPE result, not to transient builder memo nodes.
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now pins
    that behavior with a focused regression proving nested builder memo state
    does not survive after return and repeated builder invocations materialize
    fresh destination graphs instead of reusing transient memoized children.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=90 fail=0`

- Closed the target-chain shared-wrapper provenance follow-up:
  - `src/lisp/eval_boundary_provenance.c3` now walks nested `ARRAY`,
    `HASHMAP` / `SET`, and `METHOD_TABLE` payload edges before admitting
    fast reuse for an already-target-chain wrapper, so reuse now agrees with
    the existing graph-audit ownership model instead of only checking the
    wrapper shell.
  - target-chain wrapper reuse now fails closed back into the existing deep
    copy / ESCAPE promotion builders when any nested child still lives in the
    releasing scope or outside the surviving target chain.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now includes a focused
    regression proving target-chain `ARRAY`, `HASHMAP`, `SET`, and
    `METHOD_TABLE` wrappers clone instead of reusing pointer identity when one
    nested child is still releasing-scope owned, and
    `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` wires that probe
    into the bounded smoke lane.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=89 fail=0`

- Closed a promotion-context/env-copy follow-up in the boundary hardening lane:
  - `src/lisp/eval_promotion_context.c3` now treats memo-entry allocation
    failure as a fail-closed promotion-context abort instead of dereferencing a
    null memo node.
  - `src/lisp/eval_env_copy_values.c3` and
    `src/lisp/eval_env_copy_frame_helpers.c3` now self-clean and rollback
    iterator payload materializations when the outer iterator wrapper cannot be
    produced, including iterator payloads backed by copied `PARTIAL_PRIM`
    values.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now proves
    iterator wrapper allocation failure does not leak an already-copied inner
    payload retain.

- Closed a destination-escape commit fail-closed follow-up for nested boundary
  faults:
  - `src/lisp/eval_boundary_commit_escape_cons.c3` and
    `src/lisp/eval_boundary_commit_escape_wrappers.c3` now bubble
    boundary-generated nested child-copy errors back out as top-level commit
    errors instead of rebuilding `CONS` / `PARTIAL_PRIM` / `ITERATOR` wrappers
    with embedded `ERROR` children.
  - `src/lisp/eval_boundary_commit_destination.c3` now classifies those
    builder-returned top-level errors as destination error promotion rather
    than pretending a structured destination build succeeded.
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
    includes focused regressions proving nested opaque primitive faults fail
    closed for destination-built `CONS`, `PARTIAL_PRIM`, and `ITERATOR`
    commit paths.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=87 fail=0`
    - `scripts/check_status_consistency.sh` -> pass

- Closed the root-store shared-wrapper partial-cleanup follow-up:
  - `src/lisp/eval_promotion_root_clone_basic.c3` now routes late-failure
    `ARRAY` and `HASHMAP` / `SET` clone aborts through the same shared partial
    cleanup helpers used by ordinary boundary copy and ESCAPE promotion.
  - root-store clone rollback now unwinds already-copied child ownership side
    effects before freeing the aborted heap wrapper, instead of leaking copied
    closure-env retains on a later element/value failure.
  - `src/lisp/tests_memory_lifetime_root_boundary_groups.c3` now includes a
    focused root-boundary regression covering both `ARRAY` and `HASHMAP`
    partial-clone abort cleanup, and
    `src/lisp/tests_memory_lifetime_groups.c3` wires it into the bounded smoke
    lane.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=86 fail=0`

- Closed the shared-wrapper partial-cleanup follow-up for boundary copy and
