# Active TODO

Last condensed: 2026-04-09

This file is now the sole live backlog.
List only still-open items here.

Current actionable count: 1

Completed backlog snapshots:

- `docs/TODO-backup-2026-03-26.md`
- `docs/TODO-backup-2026-03-31.md`
- `docs/TODO-backup-2026-04-01.md`
- `docs/TODO-backup-2026-04-08.md`

Use this file only for still-open work.

## Live Queue

- [ ] `AUDIT-COLLECTION-CONSTRUCTOR-GUARDED-HASHMAP-CALLERS-012B` normalize the remaining already-guarded `make_hashmap(...)` caller family onto checked constructors or a shared fail-closed helper contract
  - scope:
    - deduce payload/result builders
    - deduce runtime helper state maps
    - unify/deduce row materialization helpers
  - why this remains open:
    - the direct crashable caller class is now closed separately
    - the remaining family already guards `tag` / `hashmap_val`, but it still
      depends on repeated ad-hoc raw-constructor checks instead of one checked
      constructor or shared helper contract
    - this is now strictly a normalization and consistency lane, not the same
      immediate crash class that `deduce 'match` used to have

## Recently Closed

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-RAW-HASHMAP-CRASHERS-012A` close the direct raw-hashmap caller class that still dereferenced payloads without constructor checks
  - closure evidence:
    - `src/lisp/unify_match_helpers.c3` now routes `build_result_dict(...)`
      through checked hashmap construction and checked insertion, and returns
      `deduce/match-out-of-memory` on constructor or insertion failure.
    - `src/lisp/unify_scan_helpers.c3` now propagates that builder `ERROR`
      directly instead of embedding it into a successful result list.
    - `src/lisp/tests_deduce_query_groups.c3` now proves `deduce 'match`
      propagates result-dict constructor OOM directly.
    - the residual raw-hashmap backlog is now just the already-guarded
      normalization family:
      - `AUDIT-COLLECTION-CONSTRUCTOR-GUARDED-HASHMAP-CALLERS-012B`

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-RAW-ARRAY-AOT-011A` close raw array constructor and AOT dict payload fail-closed gaps
  - closure evidence:
    - `src/lisp/value_predicates_accessors_basic.c3` now routes
      `make_array(...)` through the checked array constructor path instead of
      raw unchecked allocation.
    - `src/lisp/prim_collection_sort_array.c3` now propagates array
      constructor `ERROR`s from `array(...)` and `list->array(...)` instead of
      dereferencing a partially initialized wrapper.
    - `src/lisp/aot_runtime_bridge.c3` now routes `dict_from_args(...)`
      through checked hashmap construction and checked insertion, so bridge
      payload creation fails closed under allocation pressure.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now pins raw
      array-constructor OOM behavior directly.
    - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now pins active
      bridge-interpreter `dict_from_args(...)` hashmap-constructor OOM
      behavior directly.

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011` migrate remaining runtime/status payload builders off unchecked collection constructors
  - closure evidence:
    - `src/lisp/async_process_signal_dns_process.c3`,
      `src/lisp/async_process_spawn.c3`, and
      `src/lisp/prim_io_fs_handles.c3`
      now route runtime status payload builders through checked `HASHMAP` /
      `ARRAY` constructors and checked hashmap insertion instead of mutating
      unchecked constructor results.
    - `src/lisp/http_url_response.c3` now constructs parsed HTTP response
      payload maps through the same checked contract.
    - `process-spawn` now also closes its live process/fs handles if final
      success-payload map construction fails, so constructor OOM cannot strand
      a half-built success-shaped result with open resources.
    - `src/lisp/eval_dispatch_error_payloads.c3` now treats lambda and
      ambiguous-dispatch payload dictionaries as optional under OOM: the
      primary typed error still returns even if payload-map construction or
      insertion fails.
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now applies
      the same optional-payload contract to unhandled-effect error payloads.
    - `src/lisp/primitives_meta_types_ctor_helpers.c3` now makes
      `ctor_mismatch_data(...)` fail closed by returning `null` instead of
      dereferencing unchecked hashmap payloads.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now pins fs,
      process-spawn, process-wait, HTTP response payload, dispatch-payload,
      runtime-effect payload, and ctor-mismatch constructor OOM paths directly.
    - no same-lane residual callsites remain from the staged runtime/status
      payload-builder family.

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-SCHEMA-EXPLAIN-010` migrate schema explain payload builders onto checked collection-constructor OOM contracts
  - closure evidence:
    - `src/lisp/schema_explain_payload_helpers.c3` now centralizes checked map
      construction and checked `explain_dict_set*` insertion through one
      explicit `"schema explain: out of memory"` contract.
    - `src/lisp/schema_explain_helpers.c3`,
      `src/lisp/schema_explain_effect.c3`,
      `src/lisp/schema_explain_effect_result_payload.c3`,
      `src/lisp/schema_explain_effect_runtime.c3`, and
      `src/lisp/schema_explain_effect_helpers.c3`
      now route entrypoint/result/candidate/source payload maps through that
      checked path instead of dereferencing unchecked `make_hashmap(...)`
      results.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves:
      - dispatch explain result construction fails closed on map OOM,
      - effect explain result construction fails closed on map OOM,
      - helper payload/source maps fail closed on map OOM.
    - residual unchecked collection-constructor work is now just the separate
      runtime/status payload-builder lane:
      - `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011`
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=134 fail=0`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=134 fail=0`

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-CALLSITE-MIGRATION-009` close the data-format bridge slice of internal collection-constructor OOM hardening and split the residual backlog by real callsite family
  - closure evidence:
    - `src/lisp/json.c3` and `src/lisp/primitives_toml_bridge.c3` now use
      checked `ARRAY` / `HASHMAP` constructors plus checked hashmap insertion
      in their recursive decode paths.
    - nested conversion `ERROR`s in those files now propagate directly instead
      of being embedded into partial arrays/dicts.
    - `src/lisp/primitives_data_formats_csv_parse.c3` now uses checked row and
      result-array constructors and propagates constructor/cell materialization
      errors directly.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves JSON,
      TOML, and CSV constructor OOM paths fail closed.
    - the broad umbrella item is now split into:
      - `AUDIT-COLLECTION-CONSTRUCTOR-SCHEMA-EXPLAIN-010`
      - `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011`
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=133 fail=0`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=133 fail=0`

- [x] `AUDIT-ITERATOR-TAIL-ERROR-PROPAGATION-008` stop iterator tail-construction faults from degrading into silent truncation
  - closure evidence:
    - `src/lisp/primitives_iter_state.c3` now routes `(item . next)` iterator
      pair construction through `iterator_make_pair_or_propagate(...)`, which
      returns tail `ERROR` values directly instead of wrapping them in `CONS`.
    - source iterator thunks in `src/lisp/primitives_iter_sources.c3` and
      coroutine/transform thunks in `src/lisp/primitives_iter_coroutine.c3`
      now share that helper, so tail constructor failure no longer looks like
      normal iterator completion/truncation.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves both
      a source thunk (`repeat`) and a coroutine thunk (`take`) propagate the
      tail allocation error directly.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=128 fail=0`

- [x] `AUDIT-RUNTIME-CONSTRUCTOR-OOM-SUBSTRATE-008` harden shared runtime error/collection constructor paths that live iterator/error helpers depend on
  - closure evidence:
    - `src/lisp/value_core_types.c3`,
      `src/lisp/value_interp_alloc_helpers.c3`,
      `src/lisp/value_constructors_lifecycle.c3`, and
      `src/lisp/primitives_meta_types.c3`
      now track whether `STRING` / `ERROR` chars are heap-owned, so fallback
      literal-backed error values no longer flow into invalid frees during
      normal teardown or `unsafe-free`.
    - `src/lisp/value_constructors.c3` now makes `make_error(...)` fail closed
      when its message buffer allocation fails.
    - `src/lisp/value_predicates_accessors_basic.c3` and
      `src/lisp/prim_collection_hashmap.c3`
      now expose checked `ARRAY` / `HASHMAP` / `SET` constructor and grow
      helpers, and the live runtime-dependent surfaces now use them:
      - raise payload construction
      - `Dictionary`
      - `Set`
      - `to-array`
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now pins the
      exact constructor seams for:
      - printable `make_error(...)` fallback
      - iterator ctor raise payload-map failure
      - `to-array` result-array failure
      - checked collection constructor/grow failure
    - residual broader internal constructor migration is now split into
      `AUDIT-COLLECTION-CONSTRUCTOR-CALLSITE-MIGRATION-009` instead of being
      left implicit.

- [x] `AUDIT-STRING-BUILDER-OOM-007` harden shared `StringVal` builder creation and growth to fail closed
  - closure evidence:
    - `src/lisp/prim_string_format_helpers.c3` now gives `StringVal` an
      explicit fail-closed contract:
      - `strval_new(...)` returns `null` instead of dereferencing failed
        allocations,
      - `strval_ensure(...)` returns `bool`, guards size overflow, and marks
        builder failure on grow failure,
      - `strval_push(...)` / `strval_append(...)` / padding helpers now stop
        writing after a failed growth attempt,
      - deterministic seams were added for initial builder allocation and
        builder growth failure.
    - parser string literal construction paths in
      `src/lisp/parser_datum_helpers.c3`,
      `src/lisp/parser_expr_atoms.c3`,
      `src/lisp/parser_patterns_values.c3`, and
      `src/lisp/parser_quasiquote_datum_helpers.c3`
      now share the checked builder path and set parser errors instead of
      dereferencing a failed builder allocation.
    - runtime string helpers in
      `src/lisp/prim_string_ops.c3`,
      `src/lisp/prim_string_format.c3`, and
      `src/lisp/prim_string_format_directives.c3`
      now fail closed on builder creation/growth failure instead of writing
      through invalid builder buffers.
    - `src/lisp/primitives_meta_types.c3` no longer uses unchecked `StringVal`
      allocation in the `unsafe-free` error path.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=127 fail=0`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp'` -> `pass=191 fail=0`

- [x] `AUDIT-JIT-POLICY-FULL-SLICE-006` isolate and close the remaining non-continuation `jit-policy` slice crash
  - closure evidence:
    - the crash was isolated to the `stale-raise-scrub` JIT policy case, but
      the actual fault site was the TCO recycle TEMP-graph scan in
      `src/lisp/jit_jit_eval_scope_chain_helpers.c3`, not stale raise state.
    - `jit_graph_binding_reaches_temp_scope(...)` no longer allocates four
      `4096`-entry pointer arrays on the runtime stack; it now uses one
      heap-backed `JitTempGraphScan`, closing the entry-time stack-overflow
      crash on smaller runtime stacks.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=stale-raise-scrub ./build/main --test-suite lisp'` -> `1 passed, 0 failed`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp'` -> `pass=41 fail=0`

## Recently Closed

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
    - `src/lisp/jit_jit_apply_multi_prims.c3` now emits the same canonical under-arity text as the tail/AOT helpers for both fixed multi-arg closures and variadic multi-arg closure application.
    - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now directly asserts that compile-time parser failures report user-source coordinates after the stdlib prelude offset is stripped.
    - validation:
      - `rm -rf build/obj/linux-x64 build/main && c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` -> `pass=194 fail=0`
      - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-BOUNDARY-METHOD-TABLE-FAILURE-TEST-003` add deterministic coverage for method-table partial-cleanup abort lanes
  - closure evidence:
    - `src/lisp/eval_promotion_root_clones.c3` now exposes narrow abort-cleanup telemetry for partial method-table reclamation, and `src/lisp/jit_jit_closure_support.c3` now exposes a targeted heap-signature copy failure seam.
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
    - `src/lisp/jit_jit_apply_multi_prims.c3` and `src/lisp/jit_jit_apply_multi_prims_tail.c3` now null-guard multi-arg argument-buffer allocation for primitive and method-table dispatch, with a focused test seam that fails closed instead of dereferencing `null`.
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
      - `src/lisp/jit_jit_compile_let_set_helpers.c3`
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

- [x] `META-STATS-001` remove duplicate SCC/parallel topology planner work from `deduce/stats`
  - closure evidence:
    - removed dead duplicate topology-planning wrapper that rebuilt SCC + batch metadata independently of the stats path:
      - deleted `deduce_parallel_batch_topology_counts(...)` from `src/lisp/deduce_schema_query_metadata_parallel_topology.c3`
    - the live stats path remains on a single metadata-build lane:
      - `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
      - `src/lisp/deduce_schema_query_metadata_parallel_topology.c3`
    - no residual callsites remain for the deleted duplicate planner wrapper:
      - `rg -n "deduce_parallel_batch_topology_counts\\(" src/lisp` -> no matches
    - integration safety:
      - `c3c build` passes and links `build/main`
  - validation note:
    - the broader `OMNI_LISP_TEST_SLICE=deduce` lane is currently green in this workspace:
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=324 fail=0`

- [x] `REF-SPLIT-INFRA-REPL-001` split REPL server runtime surfaces by ownership lane
  - closure evidence:
    - split `src/lisp/eval_repl_server.c3` top-down into focused runtime surfaces without behavioral contract changes:
      - request auth/dispatch lane extracted to `src/lisp/eval_repl_server_request.c3`
      - unix/tcp listener lane extracted to `src/lisp/eval_repl_server_listeners.c3`
      - stream/session orchestration retained in `src/lisp/eval_repl_server.c3`
    - primary runtime file was reduced from `332` lines to `67` lines:
      - `wc -l src/lisp/eval_repl_server.c3 src/lisp/eval_repl_server_request.c3 src/lisp/eval_repl_server_listeners.c3`
    - integration + async REPL validation remain green after the split:
      - `c3c build`
      - `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=59 fail=0`

- [x] `AUDIT-BINDGEN-DEFERRED-001` resolve generated bindgen TODO ownership/teardown policy gaps
  - closure evidence:
    - bindgen wrappers now enforce concrete ownership/role/teardown guards and fail closed where policy is unresolved:
      - `src/lisp/bindgen.c3`
      - added explicit raise paths:
        - `bindgen/opaque-arg-role-mismatch`
        - `bindgen/opaque-arg-ownership-mismatch`
        - `bindgen/opaque-arg-unsupported-teardown`
        - `bindgen/opaque-arg-nil`
        - `bindgen/string-return-unknown-ownership`
        - `bindgen/opaque-return-manual-review`
    - staged comment marker switched from `TODO(bindgen)` to `REVIEW(bindgen)`:
      - `src/lisp/bindgen.c3`
      - `src/lisp/tests_compiler_codegen_groups_tail.c3`
    - compiler bindgen coverage remains green:
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=189 fail=0`

- [x] `AUDIT-DEDUCE-WHY-DERIVED-001` implement missing derived-subject why-result support
  - closure evidence:
    - removed explicit unsupported derived-subject error path from why-result lookup:
      - `src/lisp/deduce_why_result_lookup_derived.c3`
      - `rg -n "why-result-derived-subject-not-yet-supported|not-yet-supported" src/lisp/deduce_why_result_* src/lisp/deduce_*` returns no matches
    - derived why-result now returns structured provenance payloads instead of a legacy unsupported error for supported read shapes:
      - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(block ... (deduce/query reach (lambda (row) (= (ref row 'src) 8))) (deduce/why-result reach 8 10))"`
      - observed payload includes:
        - `kind why-result`
        - `path-kind derived`
        - `status partial`
        - `paths` with structured support frames

- [x] `AUDIT-BUILD-IMPORT-001` restore green build by fixing invalid allocator import paths
  - closure evidence:
    - updated allocator import/module references in:
      - `src/lisp/scheduler_thread_tasks.c3`
      - `src/lisp/eval_repl_server_worker.c3`
    - `c3c build` now completes and links `build/main`.

- [x] `AUDIT-REPL-SECURITY-001` lock down unauthenticated remote REPL execution surfaces
  - closure evidence:
    - TCP REPL now enforces loopback bind (`localhost`/`127.0.0.1`/`::1`) and requires `OMNI_REPL_TCP_AUTH_TOKEN` at startup:
      - `src/lisp/eval_repl_server.c3`
    - per-request authorization gate added for non-`describe` operations with `auth` token matching:
      - `src/lisp/eval_repl_server.c3`
      - `src/lisp/eval_repl_server_protocol.c3`
      - `src/lisp/eval_repl_server_protocol_parse.c3`
      - `src/lisp/eval_repl_server_state.c3`
      - `src/lisp/eval_repl_server_output.c3`
    - regression coverage added:
      - `src/lisp/tests_runtime_async_repl_server_groups.c3`
    - `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=59 fail=0`.

- [x] `AUDIT-DEDUCE-PARALLEL-RUNTIME-001` replace metadata-only parallel mode with truthful runtime state
  - closure evidence:
    - added shared runtime-truth field helper with explicit serial runtime counters:
      - `src/lisp/deduce_parallel_runtime_truth.c3`
    - updated analyze/explain/stats payloads to emit truthful runtime mode/counters:
      - `src/lisp/deduce_rule_eval_analyze_payload_fields.c3`
      - `src/lisp/deduce_rule_ops_explain_snapshot.c3`
      - `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
    - updated assertions:
      - `src/lisp/tests_deduce_query_admin_surface_tail.c3`
      - `src/lisp/tests_deduce_rule_groups_explain.c3`
    - targeted eval checks for analyze/stats runtime mode now return `true`.

- [x] `AUDIT-DEDUCE-NAIVE-FALLBACK-001` reduce recursive component fallback to naive execution
  - closure evidence:
    - seminaive recursive aggregate path is now selected directly when seminaive recursive mode is enabled:
      - `src/lisp/deduce_rule_eval_fixpoint_component_eval_non_txn.c3`
      - `src/lisp/deduce_rule_eval_fixpoint_component_eval.c3`
    - analyze/explain runtime-truth payloads remain aligned with serial execution counters.

- [x] `AUDIT-NET-IPv6-001` remove IPv4-only DNS resolution limitation
  - closure evidence:
    - DNS address rendering now supports both `AF_INET` and `AF_INET6` and reports unsupported families explicitly:
      - `src/lisp/async_tcp_transport_helpers.c3`
      - `src/lisp/async_runtime_base.c3`
      - `src/lisp/async_process_signal_dns.c3`
    - deterministic coverage added for IPv4 + IPv6 addrinfo rendering:
      - `src/lisp/tests_runtime_async_io_tls_groups.c3`
    - `OMNI_LISP_TEST_SLICE=async ... --test-suite lisp` remains green (`pass=59 fail=0`).

- [x] `STACK-AARCH64-CONT-001` arm64 language-level continuation multi-shot parity
  - closure evidence:
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack` -> `Stack engine: 23 passed, 0 failed`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))"` -> `11`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (with-continuation k (k 41))))"` -> `42`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(checkpoint (+ 1 (capture k (k 10))))"` -> `11`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(block (define replay-set-counter 0) (define replay-set-r (checkpoint (+ (capture k (+ (k 1) (k 1))) (block (set! replay-set-counter (+ replay-set-counter 1)) replay-set-counter)))) (+ (* 10 replay-set-r) replay-set-counter))"` -> `52`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=56 fail=0`
    - `OMNI_JIT_POLICY_FILTER=multishot-capture-scope-guard-clone OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=1 fail=0`
  - implementation note:
    - arm64 GNU lightning register IDs were corrected in `src/lisp/jit_lightning_constants.c3`.
    - effect fast-path primitive dispatch now preserves primitive error payloads and supports dotted cons payloads for fixed-arity wrappers in:
      - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
      - `src/lisp/jit_jit_runtime_effects_signal.c3`
