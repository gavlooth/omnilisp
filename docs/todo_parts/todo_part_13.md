# Active TODO Index Part 13

Source: `TODO.md`

- [x] `AUDIT-JIT-POLICY-FULL-SLICE-006` isolate and close the remaining non-continuation `jit-policy` slice crash
  - closure evidence:
    - the crash was isolated to the `stale-raise-scrub` JIT policy case, but
      the actual fault site was the TCO recycle TEMP-graph scan in
      `src/lisp/jit_eval_scope_chain_helpers.c3`, not stale raise state.
    - `jit_graph_binding_reaches_temp_scope(...)` no longer allocates four
      `4096`-entry pointer arrays on the runtime stack; it now uses one
      heap-backed `JitTempGraphScan`, closing the entry-time stack-overflow
      crash on smaller runtime stacks.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=stale-raise-scrub ./build/main --test-suite lisp'` -> `1 passed, 0 failed`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp'` -> `pass=41 fail=0`

- [x] `AUDIT-BOUNDARY-WRAPPER-SLOT-LEAK-005` eliminate pre-dtor wrapper-slot leaks on partial-abort copy/root-store paths
  - closure evidence:
    - `src/lisp/eval_promotion_copy_route_helpers.c3`,
      `src/lisp/eval_promotion_root_clone_basic.c3`, and
      `src/lisp/eval_promotion_root_clones.c3` now allocate/register the
      destination wrapper only after all fallible child-copy and payload clone
      work succeeds.
    - wrapper-allocation failure after payload success now routes through the
      existing partial-cleanup helpers, so already-copied child retains and
      heap payloads are unwound before returning.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves repeated
      failed shared-wrapper copy attempts do not grow the surviving target
      scope allocation count.
    - `src/lisp/tests_memory_lifetime_root_boundary_groups.c3` now proves the
      same invariant for repeated failed root-store method-table clone
      attempts against `root_scope`.
    - validation:
      - `c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=106 fail=0`
      - `scripts/check_status_consistency.sh` -> pass

- [x] `AUDIT-BOUNDARY-DESTINATION-CTX-005` route direct destination escape promotion through the caller promotion context
  - closure evidence:
    - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` now routes
      direct destination escape promotion through a shared ctx-aware helper
      instead of silently falling back to `interp.active_promotion_ctx`.
    - `src/lisp/eval_boundary_commit_escape_helpers.c3` and
      `src/lisp/eval_boundary_commit_destination.c3` now use that helper for
      releasing-scope retry, mixed-destination retry, and direct destination
      promotion, so destination commit stays inside the caller-owned
      memo/budget/abort epoch.
    - destination-builder teardown now restores both `memo_head` and the
      small scope-chain cache snapshot, so temporary build-scope cache entries
      cannot survive after the builder returns or aborts.
    - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
      proves:
      - repeated builder calls do not retain builder-local scope-chain cache
        state, and
      - a non-active caller `PromotionContext` still receives the abort state
        from direct destination promotion while the unrelated active context
        remains untouched.
    - validation:
      - `c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> pass
      - `scripts/check_status_consistency.sh` -> pass

- [x] `AUDIT-BOUNDARY-DESTINATION-MEMO-004` define correct promotion-context memo semantics for destination builders
  - closure evidence:
    - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` now makes the
      shipped contract explicit: memo entries remembered while routing nested
      children inside temporary destination build scopes are builder-local and
      are discarded when the builder returns or aborts.
    - `src/lisp/eval_boundary_commit_escape_cons.c3` and
      `src/lisp/eval_boundary_commit_escape_wrappers.c3` now route that
      save/restore policy through shared helpers instead of leaving it as an
      implicit per-builder pattern.
    - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
      proves repeated destination-builder calls in one promotion epoch do not
      retain child memo entries after return and therefore materialize fresh
      destination graphs instead of reusing transient builder-local memo state.
    - validation:
      - `c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=90 fail=0`

- [x] `AUDIT-BOUNDARY-PROVENANCE-WRAPPER-004` re-audit target-chain wrapper reuse for nested child-owned payloads
  - closure evidence:
    - `src/lisp/eval_boundary_provenance.c3` now walks nested `ARRAY`,
      `HASHMAP` / `SET`, and `METHOD_TABLE` payload edges before admitting
      target-chain fast reuse, so the reuse classifier now agrees with the
      existing graph-audit ownership model instead of checking only the wrapper
      shell.
    - target-chain shared wrappers now fall back into the existing copy /
      ESCAPE builders whenever any nested child still lives in the releasing
      scope or outside the surviving target chain.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now pins the exact
      target-chain-wrapper regression for `ARRAY`, `HASHMAP`, `SET`, and
      `METHOD_TABLE`, and
      `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` keeps it in the
      bounded smoke lane.
    - validation:
      - `c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=89 fail=0`

- [x] `AUDIT-COMPILER-DIAGNOSTIC-PARITY-003` unify remaining JIT/AOT diagnostic drift and cover prelude-remapped parser coordinates
  - closure evidence:
    - `src/lisp/jit_apply_multi_prims.c3` now emits the same canonical under-arity text as the tail/AOT helpers for both fixed multi-arg closures and variadic multi-arg closure application.
    - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now directly asserts that compile-time parser failures report user-source coordinates after the stdlib prelude offset is stripped.
    - validation:
      - `rm -rf build/obj/linux-x64 build/main && c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` -> `pass=194 fail=0`
      - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-BOUNDARY-METHOD-TABLE-FAILURE-TEST-003` add deterministic coverage for method-table partial-cleanup abort lanes
  - closure evidence:
    - `src/lisp/eval_promotion_root_clones.c3` now exposes narrow abort-cleanup telemetry for partial method-table reclamation, and `src/lisp/jit_closure_support.c3` now exposes a targeted heap-signature copy failure seam.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now drives both copy-to-parent and escape-promotion abort paths through that seam and proves partially copied heap signatures are reclaimed instead of leaked.
    - `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` now includes that regression in the bounded smoke lane.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=83 fail=0`

- [x] `AUDIT-ASYNC-PROCESS-CONCURRENCY-003` define and harden shared process-handle concurrency semantics
  - closure evidence:
    - `src/lisp/async_process_signal_runtime.c3` now gives each process handle a shared in-flight guard, and `src/lisp/async_process_lifecycle.c3` preserves the closed state while wait/kill activity is serialized through the same contract.
    - `src/lisp/async_process_signal_dns_process.c3` now fails closed with `io/process-handle-busy` when `process-wait` / `process-kill` reuse the same live handle concurrently.
    - focused regression coverage now lives in `src/lisp/tests_advanced_io_effect_ffi_scheduler_boundary.c3` and `src/lisp/tests_advanced_io_effect_ffi_groups.c3`.
    - validation:
      - `c3c build`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp` -> pass

- [x] `HTTP-CRUD-CONCURRENT-WRITES-001` decide and harden spawned CRUD write semantics for one in-memory Deduce DB
  - closure evidence:
    - `examples/deduce_crud_server.omni` now guards CRUD mutation entrypoints with a shared atomic write gate, preserving keyed-write duplicate-id normalization while preventing spawned overlap from surfacing raw runtime `ERROR` payloads.
    - the shipped application contract is now explicit: overlapping spawned writes over one in-memory Deduce CRUD store may resolve as either one success plus `"crud write already in progress"` or one success plus `"item already exists"`, but never as an unnormalized runtime error and never with more than one persisted row for the shared id.
    - the spawned concurrency probe now runs in the focused `http-crud` slice instead of the broad `http` slice, so deterministic HTTP regressions stay stable while the concurrency lane remains exercised.
    - validation:
      - `rm -rf build/obj/linux-x64 build/main && c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` -> `pass=29 fail=0`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http-crud ./build/main --test-suite lisp` -> `pass=1 fail=0`

- [x] `AUDIT-VALIDATION-INTEGRITY-002` repair status/validation tooling contracts
  - closure evidence:
    - `scripts/check_status_consistency.sh` now accepts the current zero-item TODO wording instead of hard-requiring one historical sentinel string.
    - `scripts/run_validation_container.sh` now serializes bounded validation runs with a repo-local lock so overlapping host/container/ASAN jobs do not corrupt the shared build tree.
    - `scripts/run_validation_status_summary.sh` now treats missing required `OMNI_TEST_SUMMARY` telemetry as a validation failure instead of trusting exit status alone.
    - `scripts/c3c_limits.sh` now preserves quoted extra Docker args instead of re-splitting them unsafely.
    - `scripts/run_e2e.sh` and `scripts/check_e2e_baseline_policy.sh` now guard Stage 3 compile-source parity explicitly against entry-build drift.
    - validation:
      - `scripts/check_status_consistency.sh` -> pass
      - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity` -> pass
      - `bash -n scripts/check_status_consistency.sh scripts/run_validation_container.sh scripts/run_validation_status_summary.sh scripts/c3c_limits.sh scripts/run_e2e.sh scripts/check_e2e_baseline_policy.sh` -> pass

- [x] `AUDIT-BOUNDARY-SHARED-WRAPPER-003` re-audit shared-wrapper boundary aliasing after fail-closed propagation landed
  - closure evidence:
    - `src/lisp/eval_promotion_copy.c3`, `src/lisp/eval_promotion_copy_route_helpers.c3`, `src/lisp/eval_promotion_escape_leaf.c3`, and `src/lisp/eval_promotion_escape_structured.c3` no longer return disjoint `ARRAY` / `HASHMAP` / `SET` / `METHOD_TABLE` wrappers by pointer identity once fast reuse is declined; they now defensively clone those wrappers and recurse through nested payload edges.
    - `METHOD_TABLE` shared-wrapper clones now keep signature arrays heap-backed so the existing value destructor contract remains sound during scope teardown.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now covers both defensive clone behavior for shared wrappers and nested fail-closed boundary-copy behavior.
    - `src/lisp/tests_memory_lifetime_finalize_groups.c3` also stops reading detached-scope env memory after release, removing the finalize-lane UAF that surfaced while validating the integrated smoke suite.
    - validation:
      - `rm -rf build/obj/linux-x64 build/main && c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=81 fail=0`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=0:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=81 fail=0`

- [x] `AUDIT-COMPILER-PARITY-002` harden JIT multi-arg allocation failures and restore AOT/JIT parity
  - closure evidence:
    - `src/lisp/jit_apply_multi_prims.c3` and `src/lisp/jit_apply_multi_prims_tail.c3` now null-guard multi-arg argument-buffer allocation for primitive and method-table dispatch, with a focused test seam that fails closed instead of dereferencing `null`.
    - `src/lisp/parser_top_level_parse.c3` and `src/lisp/compiler_program_pipeline_helpers.c3` now fail closed on malformed trailing forms; they no longer return a silently truncated prefix program after parser error.
    - `src/lisp/aot.c3` now exposes shared AOT arg-list counting and arity-error helpers, and `src/lisp/compiler_code_emission_lambda_defs.c3` uses them so generated multi-arg lambdas reject under-application, preserve JIT-style over-application chaining through `aot::apply_multi(...)`, and reject malformed arg lists explicitly.
    - `src/lisp/compiler_native_call_compilation_flat_style.c3` now guards generated closure-capture allocation in flat expression lowering without emitting invalid raw `return` statements into non-`Value*` contexts.
    - focused regressions landed in:
      - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      - `src/lisp/tests_compiler_core_groups_fail_closed.c3`
      - `src/lisp/tests_compiler_core_groups.c3`
    - validation:
      - `c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp` -> `pass=35 fail=0`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` -> `pass=193 fail=0`
      - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`

- [x] `HTTP-CRUD-DUPLICATE-POST-001` restore a deterministic green broad HTTP slice after the parser regressions landed
  - closure evidence:
    - `examples/deduce_crud_server.omni` no longer performs a check-then-insert race in `repo/create`; it now treats keyed `deduce 'fact!` as the atomic source of truth and maps `deduce/integrity-key-conflict` to the existing `"item already exists"` API result.
    - `src/lisp/tests_runtime_feature_http_groups.c3` now keeps deterministic broad-slice coverage for duplicate-id rejection using two POSTs with the same id but different payloads, rather than the order-sensitive spawned race probe.
    - the HTTP helper-method fiber smoke remains isolated from the long-lived group interpreter so the broad `http` slice no longer inherits unrelated state from earlier cases.
    - validation:
      - `c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` -> `pass=29 fail=0`

- [x] `AUDIT-BOUNDARY-FAILCLOSED-002` make nested boundary copy/promotion fail closed for opaque primitive payloads
  - closure evidence:
    - `src/lisp/eval_promotion_copy.c3`, `src/lisp/eval_promotion_copy_route_helpers.c3`, and `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now propagate `BoundaryCopyFault` through nested `CONS` / `PARTIAL_PRIM` / `ITERATOR` copy paths, so nested opaque primitive rejection aborts transitively instead of embedding null/error payloads into rebuilt wrappers.
    - `src/lisp/eval_promotion_escape_leaf.c3` and `src/lisp/eval_promotion_escape_structured.c3` now fail closed transitively through the same wrapper shapes during ESCAPE promotion.
    - primitive-copy rejection now validates opaque payload legality before allocating the destination wrapper where practical, removing the target-scope garbage-allocation path from rejected primitive copies.
    - focused regressions landed in `src/lisp/tests_memory_lifetime_boundary_groups.c3` and `src/lisp/tests_memory_lifetime_promotion_context_groups.c3`, and the smoke suite wires them through `src/lisp/tests_memory_lifetime_groups.c3`.
    - validation:
      - `c3c build` -> pass
      - `rm -rf build/obj/linux-x64 build/main && mkdir -p build/obj/linux-x64/tmp_c_compile && c3c build --sanitize=address` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=80 fail=0`
      - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-ASYNC-FFI-SAFETY-002` harden REPL worker lifecycle and handle-kind validation
  - closure evidence:
    - `src/lisp/eval_repl_server_worker.c3` now refuses to start the REPL worker thread unless both mutex and condition-variable initialization succeeded, so partial-init paths fail closed before the worker can touch uninitialized sync state.
    - `src/lisp/async_socket_handle_runtime.c3`, `src/lisp/tls_handle_lifecycle.c3`, and `src/lisp/prim_io_fs_stream.c3` now validate exact FFI handle names before casting TCP/UDP/TLS/FS payloads, closing the type-confusion path where unrelated `FFI_HANDLE` boxes could be reinterpreted as transport state.
    - `src/lisp/async_process_spawn.c3` now treats constructor-returned error values from `make_process_handle(...)` and `make_fs_handle(...)` as hard failures instead of packaging them into a success-shaped spawn result.
    - `src/lisp/http_url_response.c3` now rejects malformed `:port` suffixes with trailing garbage or missing digits, and trims HTTP response header slices so the exposed header string no longer retains delimiter residue.
    - focused regressions landed in:
      - `src/lisp/tests_runtime_async_repl_server_groups.c3`
      - `src/lisp/tests_advanced_io_effect_ffi_groups.c3`
      - `src/lisp/tests_runtime_feature_http_groups.c3`
    - validation:
      - `c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=async ./build/main --test-suite lisp` -> `pass=61 fail=0`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system ./build/main --test-suite lisp` -> `pass=42 fail=0`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_VERBOSE=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` now passes the new parser regression coverage but still reports one pre-existing unrelated failure:
        - `crud pipeline duplicate-post race (error: unexpected token in expression)`
        - tracked separately as `HTTP-CRUD-DUPLICATE-POST-001`

- [x] `REF-SPLIT-INFRA-002` continue runtime large-file decomposition (largest-first, ownership-preserving)
  - closure evidence:
    - completed largest-first structural splits in the remaining targeted runtime files:
      - `src/lisp/eval_run_pipeline.c3` (`274 -> 164`) with `src/lisp/eval_run_pipeline_helpers.c3` (`117`)
      - `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates.c3` (`275 -> 64`) with `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates_impl.c3` (`236`)
    - preserved runtime contracts by keeping coordinator entrypoints and moving internal helpers only.
    - validation:
      - `c3c build` -> pass
      - `OMNI_LISP_TEST_SLICE=schema OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=42 fail=0`

- [x] `REF-SPLIT-TESTS-002` split oversized test files by existing feature/group seams
  - closure evidence:
    - completed largest-first splits for all oversized files listed in this lane:
      - `src/lisp/tests_deduce_rule_groups_more_tail.c3` (`1902 -> 310`) + extracted seam files
      - `src/lisp/tests_deduce_query_bench_groups.c3` (`1684 -> 117`) + extracted seam files
      - `src/lisp/tests_deduce_rule_groups_explain.c3` (`1471 -> 54`) + extracted seam files
      - `src/lisp/tests_deduce_durability_groups.c3` (`1403 -> 493`) + extracted seam files
    - preserved test harness behavior by keeping coordinator runners and existing test names/filters.
    - validation:
      - `c3c build` -> pass
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=324 fail=0`

- [x] `ERR-MODEL-STRICT-001` reduce `!!` usage in non-test runtime/compiler paths per `docs/C3_STYLE.md`
  - closure evidence:
    - removed the remaining non-test `!!` sites from runtime/compiler paths, including:
      - `src/lisp/parser_callable_helpers_params.c3`
      - `src/lisp/jit_compile_let_set_helpers.c3`
      - `src/lisp/aot_runtime_bridge_helpers.c3`
      - `src/lisp/eval_dispatch_types.c3`
      - `src/lisp/prim_ui_ftxui_helpers.c3`
      - `src/lisp/primitives_meta_types_ctor.c3`
      - `src/lisp/compiler_temp_type_forms_defs_misc.c3`
      - `src/lisp/async_tcp_transport_helpers.c3`
      - `src/lisp/eval_type_evaluators.c3`
    - repo re-audit now shows zero non-test hits:
      - `rg -n "\)!!|!!;|!!," src/lisp --glob '!**/tests*'` -> no matches
    - validation:
      - `c3c build` -> pass
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=189 fail=0`
      - `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=60 fail=0`


- [x] `AUDIT-PARSER-SAFETY-001` fix parser progress/error defects and parser-surface drift
  - closure evidence:
    - lexer/parser fail-closed fixes landed in:
      - `src/lisp/parser_lexer_token_scanners_dot.c3`
      - `src/lisp/parser_lexer_whitespace.c3`
      - `src/lisp/parser_lexer.c3`
      - `src/lisp/parser_lexer_core_api.c3`
      - `src/lisp/parser_top_level_parse.c3`
      - `src/lisp/eval_run_pipeline.c3`
    - Pika/core surface parity fixes landed in:
      - `src/pika/lisp_grammar_build.c3`
      - `src/pika/lisp_grammar_scanners.c3`
    - targeted regressions added in:
      - `src/lisp/tests_compiler_core_groups_fail_closed.c3`
      - `src/lisp/tests_runtime_feature_schema_reader_groups.c3`
      - `src/lisp/tests_runtime_feature_pika_groups.c3`
    - validation:
      - `c3c build` -> pass
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=183 fail=0`
      - `OMNI_LISP_TEST_SLICE=reader-dispatch OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=12 fail=0`
      - `OMNI_LISP_TEST_SLICE=schema OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=42 fail=0`
      - `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=78 fail=0`

- [x] `VCS-JJ-CHECKOUT-ACL-001` restore `jj` checkout-state readability and bookmark workflow
  - closure evidence:
    - recovered broken checkout metadata non-destructively by rebuilding `.jj/working_copy` from readable repository state and preserving backups:
      - `.jj/working_copy.recover.bak.20260409-022334`
      - `.jj/repo/op_heads/heads.recover.bak.20260409-022441`
      - `.jj/repo/store/extra/heads.recover.bak.20260409-022625`
      - `.jj/repo/index.recover.bak.20260409-022536`
    - restored command health:
      - `jj status` -> exit `0`
      - `jj bookmark list` -> exit `0`
    - corrected bookmark workflow for current `jj 0.39.0` surface:
      - create/update by name: `jj bookmark set <name> -r <revset>`
      - move existing bookmarks: `jj bookmark move <name> --to <revset>`
      - advance closest bookmarks: `jj bookmark advance --to <revset>`
    - note:
      - `jj bookmark update` is not a valid subcommand in this CLI version.

- [x] `AUDIT-DEDUCE-SLICE-RED-001` investigate and reduce current deduce-lane failures
  - closure evidence:
    - deduce lane is now green end-to-end:
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=378 fail=0`
    - repaired high-signal root-cause clusters in materialization metadata, selector disjunctive-demand fallback, and why-result payload shape alignment:
      - `src/lisp/deduce_schema_query_metadata_schema_helpers.c3`
      - `src/lisp/deduce_schema_query_execution_goal_directed_selector_disjunction_projected.c3`
      - `src/lisp/deduce_why_result_payload.c3`
      - `src/lisp/deduce_why_result_path_payload.c3`
    - targeted admin-surface lane is also green after fixes:
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=query OMNI_DEDUCE_QUERY_FILTER=admin-surface OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=184 fail=0`

- [x] `AUDIT-ADVANCED-SLICE-RED-001` restore advanced-lane green without relaxing strict contracts
  - closure evidence:
    - fixed parser macro-pattern zero-length allocation bug that broke `syntax-match` empty-sequence clauses (`[]`) and silently prevented stdlib/test macro definitions:
      - `src/lisp/parser_patterns_values.c3`
      - avoided zero-byte AST arena allocations for empty pattern buffers in sequence/dict pattern parsing.
    - stdlib macro/predicate lane restored:
      - `stdlib/stdlib.lisp`
      - `branch` macro now loads during bootstrap again.
      - `boolean?` contract aligned to strict boolean values (`true`/`false`) while preserving existing truthiness semantics (`nil`/`false` falsy).
    - strict higher-order arity lane restored:
      - `src/lisp/primitives_meta_predicates.c3`
      - `src/lisp/eval_init_primitive_tables.c3`
      - `stdlib/stdlib.lisp`
      - introduced primitive `error?` and used it in stdlib `map` to propagate callback runtime errors instead of returning lists of embedded `ERROR` values.
    - validation:
      - `OMNI_LISP_TEST_SLICE=advanced OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=1156 fail=0`
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=182 fail=0`
