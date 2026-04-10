# Defect Audit Remediation Wave 2 (2026-04-09)

Status: completed execution note

Post-wave follow-up (2026-04-09, late pass):

- Additional follow-up fixes landed after the main wave closure:
  - env-copy now routes disjoint `ARRAY` / `HASHMAP` / `SET` / `METHOD_TABLE`
    bindings through boundary policy instead of returning source wrappers by
    identity;
  - method-table copy/escape abort paths now destroy already-copied heap
    signatures before freeing partial tables;
  - AOT multi-arg trampolines now reject negative arg counts fail-closed;
  - flat-style closure lowering now frees capture payload on closure-constructor
    `ERROR` returns;
  - HTTP client paths now close connections on all post-connect early returns,
    propagate transport read errors, and reject out-of-range URL ports.
- Process-handle concurrency lane is now closed:
  - `src/lisp/async_process_signal_runtime.c3` and
    `src/lisp/async_process_lifecycle.c3` now share a fail-closed in-flight
    guard for process handles, so concurrent `process-wait` / `process-kill`
    reuse returns a deterministic `io/process-handle-busy` error instead of
    racing on the same live handle.
  - `src/lisp/tests_advanced_io_effect_ffi_scheduler_boundary.c3` and
    `src/lisp/tests_advanced_io_effect_ffi_groups.c3` now cover the concurrent
    reuse contract directly.
  - `TODO.md` no longer tracks `AUDIT-ASYNC-PROCESS-CONCURRENCY-003`.
- Remaining follow-up lanes are now also closed:
  - `src/lisp/jit_jit_apply_multi_prims.c3` now uses the canonical
    under-arity format for non-tail fixed/variadic multi-arg closure apply,
    and `src/lisp/tests_compiler_core_groups_fail_closed.c3` now asserts
    prelude-stripped parser coordinates through the compiler pipeline.
  - `src/lisp/eval_promotion_root_clones.c3` and
    `src/lisp/jit_jit_closure_support.c3` now expose a narrow
    method-table-abort cleanup seam, and
    `src/lisp/tests_memory_lifetime_boundary_groups.c3` proves both
    copy-to-parent and escape-promotion partial-cleanup paths reclaim copied
    heap signatures on abort.
  - `TODO.md` is back to zero live items for this audit wave.

Post-wave follow-up (2026-04-10):

- The iterator coroutine cons-construction lane is now also closed:
  - `src/lisp/primitives_iter_coroutine.c3`
    now routes internal `zip` item-pair and `foldl` arg-list cons building
    through a checked iterator-local helper with a narrow nth-failure seam.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins both failure modes and proves `zip` / `foldl` return direct
    typed errors instead of embedding constructor-failed values into iterator
    data or remapping them to later apply-shape faults.
  - validation:
    - `c3c build`
    - bounded memory smoke:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- The pending-raise payload/materialization lane is now also closed:
  - `src/lisp/value_constructors.c3`
    now rejects null/error `boundary_promote_to_root(...)` results before
    publishing pending raise payload state.
  - `src/lisp/jit_jit_runtime_effects_handle.c3`
    now rejects fallback raise message-string and arg-pair constructor failure
    before handler call-through.
  - `src/lisp/jit_jit_handle_signal_handle.c3`
    now rejects fallback raise message-string materialization failure before
    handler env binding.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    and `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    now pin the fail-closed contract under forced string-allocation failure.
  - validation:
    - `c3c build`
    - targeted `jit-policy`:
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=pending-raise-string-alloc-failure ./build/main --test-suite lisp`
    - bounded memory smoke:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- The OS-thread null-completion fail-closed lane is now also closed:
  - `src/lisp/scheduler_thread_task_transition_scaffold.c3`
    now exposes a narrow transition-completion allocation fail seam for
    deterministic scheduler boundary tests.
  - `src/lisp/scheduler_thread_task_transitions.c3`
    now drops the OS-thread entry when both completion materialization and
    alloc-failure completion materialization fail, instead of returning with
    the entry still running.
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`
    now proves the double-failure seam wakes the blocked waiter, clears the
    join token, and removes the OS-thread entry.
  - `src/lisp/tests_scheduler_groups.c3`
    now wires that regression into the bounded scheduler slice.
  - validation:
    - `c3c build`
    - bounded scheduler slice:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`
    - bounded memory smoke:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- The malformed iterator-tail fail-open lane is now also closed:
  - `src/lisp/primitives_iter_state.c3`
    now exposes `iterator_tail_or_error(...)` so iterator tail validation is
    shared across terminal and coroutine helpers.
  - `src/lisp/primitives_iter_terminal.c3`
    now makes `collect` / `to-array` reject malformed iterator pairs and
    malformed iterator tails instead of silently truncating the result.
  - `src/lisp/primitives_iter_coroutine.c3`
    now makes `map`, `filter`, `take`, `zip`, and `foldl` reject malformed
    iterator tails instead of truncating or deferring broken state as normal
    completion.
  - `src/lisp/tests_advanced_core_unicode_groups.c3`
    now pins malformed-tail rejection through the surface `List`, `Array`,
    `map`, `filter`, `take`, `zip`, and `foldl` iterator pipelines.
  - validation:
    - `c3c build`
    - bounded direct evals:
      - `scripts/run_validation_container.sh bash -lc "rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --eval \"(handle (List (Iterator (lambda () (cons 1 2)))) (raise msg (ref msg 'message)))\""`
      - `scripts/run_validation_container.sh bash -lc "rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --eval \"(handle (foldl (lambda (a x) (+ a x)) 0 (Iterator (lambda () (cons 1 9)))) (raise msg (ref msg 'message)))\""`
    - bounded memory smoke:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- The scheduler wakeup publish-fallback lane is now also closed:
  - `src/lisp/scheduler_wakeup_callbacks.c3`
    now makes timer, sleep, and poll-error callbacks fall back to direct
    wakeup completion when reliable queue publish fails.
  - `src/lisp/scheduler_offload_worker.c3`
    now makes non-task worker completion fall back to
    `scheduler_handle_wakeup_offload_ready(...)` instead of freeing the live
    completion payload when publish fails.
  - `src/lisp/tests_scheduler_groups_more.c3`
    now pins the real enqueue-failure seam for timer, sleep, poll-error, and
    offload-after fallback.
  - validation:
    - `c3c build`
    - bounded scheduler slice:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`
    - bounded memory smoke:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- The pure string/list materializer follow-up is now also closed:
  - `src/lisp/prim_string_transform.c3`
    now makes `string-upcase` / `string-downcase` return string-constructor
    `ERROR`s directly and makes `string-split` propagate per-part string
    allocation failure directly.
  - `src/lisp/prim_string_ops.c3`
    now makes `string->list` fail closed on per-character string wrapper
    allocation instead of returning a list containing an `ERROR`.
  - `src/lisp/unicode.c3`
    now makes `string-graphemes` fail closed on grapheme-cluster string
    materialization instead of storing `ERROR` values in the cluster list.
  - `src/lisp/prim_io_file.c3`
    now makes `read-lines` propagate per-line string materialization failure
    directly.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins the pure string/list helper seam under forced
    `make_string(...)` allocation failure.
  - validation:
    - `c3c build`
    - bounded memory smoke:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- The string-backed payload/list materializer follow-up is now also closed:
  - `src/lisp/prim_system.c3`
    now makes `(shell cmd true)` return string-constructor failure directly
    instead of returning a success-shaped `(ERROR exit-code)` list.
  - `src/lisp/prim_io_fs_handles.c3`
    now makes `fs-readdir` return entry-name string construction failure
    directly instead of storing `ERROR` values as directory entries.
  - `src/lisp/http.c3`
    now makes `http-get` / `http-request` return host/request string
    materialization failure directly before transport setup/write.
  - `src/lisp/schema_validation.c3`
    now makes `schema-explain` return message-string construction failure
    directly instead of wrapping it in a singleton explanation list.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins `shell`, `fs-readdir`, and `schema-explain` under forced
    `make_string(...)` allocation failure.
  - validation:
    - `c3c build`
    - bounded memory smoke:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- The collection/apply array fail-open follow-up is now also closed:
  - `src/lisp/prim_collection_sort_array.c3`
    now makes `sort` / `sort-by` list rebuilding fail closed, makes
    `sort-by` propagate comparator runtime errors directly, makes `array`,
    `list->array`, array `set!`, and `push!` reject boundary-promotion
    failure instead of storing `ERROR` values as data, and makes `push!`
    fail closed on grow allocation failure.
  - `src/lisp/primitives_iter_terminal.c3`
    now makes `collect` fail closed on list construction and makes
    `to-array` reject boundary-promotion failure directly.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins array constructor/mutator and `to-array` boundary-promotion
    failure, `push!` grow failure, and `sort-by` comparator failure.
  - validation:
    - `c3c build`
    - bounded memory smoke:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- The schema-explain list-builder follow-up is now also closed:
  - `src/lisp/schema_explain_payload_helpers.c3`
    now routes list accumulation and reversal through a checked
    `explain_prepend_or_oom(...)` helper.
  - `src/lisp/schema_explain_helpers.c3`,
    `src/lisp/schema_explain_effect_helpers.c3`, and
    `src/lisp/schema_explain_effect_runtime.c3`
    now propagate the same list-builder failure for dispatch candidates,
    handler tags, and effect candidates.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins those list-builder OOM seams through a dedicated local `nth`
    fail seam.
  - validation:
    - `c3c build`
    - bounded memory smoke:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- The checked collection-mutator silent-failure lane is now also closed:
  - `src/lisp/prim_collection_hashmap.c3`
    now exposes checked-return `hashmap_set_symbol(...)`,
    `hashmap_grow(...)`, and `hashmap_set(...)` helpers instead of void
    wrappers that discarded grow/insert failure.
  - `src/lisp/prim_collection_hashmap.c3`
    now makes `set!` on dictionary targets fail closed with
    `runtime/out-of-memory` when backing-storage growth fails.
  - `src/lisp/prim_collection_generic_set.c3`
    now makes `set-add` fail closed on the same grow-failure seam.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now proves both mutators return typed errors and leave the failed key
    absent from the collection.
  - validation:
    - `c3c build`
    - bounded memory smoke:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- The direct-crash raw-hashmap caller slice is now also closed:
  - `src/lisp/unify_match_helpers.c3` now routes `build_result_dict(...)`
    through checked hashmap construction and checked insertion, and raises
    `deduce/match-out-of-memory` instead of dereferencing an unchecked raw
    constructor result.
  - `src/lisp/unify_scan_helpers.c3` now propagates that builder `ERROR`
    directly instead of embedding it into a successful result list.
  - `src/lisp/tests_deduce_query_groups.c3` now pins the
    `deduce 'match` result-dict constructor OOM seam directly.
  - the residual raw-hashmap lane is now narrower again:
    - `AUDIT-COLLECTION-CONSTRUCTOR-GUARDED-HASHMAP-CALLERS-012B`

- The remaining guarded raw-hashmap normalization lane is now also closed:
  - row materialization in `src/lisp/deduce_relation_row_materialization.c3`
    now uses checked hashmap construction plus checked insertion.
  - integrity payload builders in
    `src/lisp/deduce_relation_ops_validation_payload.c3`
    now use a checked payload-dict helper and fail closed to payload omission
    when insert-time allocation fails.
  - the deduce runtime helper state family now consistently uses checked map
    construction/insertion:
    - `src/lisp/deduce_rule_eval_exec_component_state_helpers.c3`
    - `src/lisp/deduce_rule_eval_exec_component_state.c3`
    - `src/lisp/deduce_rule_eval_exec_aggregate_state.c3`
    - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
    - `src/lisp/deduce_rule_eval_scc.c3`
    - `src/lisp/deduce_relation_scan_helpers_join.c3`
    - `src/lisp/deduce_rule_eval_analyze_setup.c3`
    - `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
  - remaining deduce explain/schema/analyze and why-result payload/result
    builders no longer use raw `make_hashmap(...)`; scoped grep over
    `src/lisp/deduce_*` and `src/lisp/unify_*` is now clean.
  - regressions now cover:
    - deduce helper-state constructor OOM,
    - integrity payload-map OOM degradation,
    - why-result path/payload OOM.
  - validation:
    - `c3c build`
    - bounded deduce slice:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'` -> `pass=328 fail=0`
  - `TODO.md` no longer tracks `AUDIT-COLLECTION-CONSTRUCTOR-GUARDED-HASHMAP-CALLERS-012B`.

- The checked-insertion follow-up for deduce payload builders is now also
  closed:
  - `src/lisp/deduce_why_result_payload.c3`,
    `src/lisp/deduce_why_result_path_payload.c3`,
    `src/lisp/deduce_why_result_lookup.c3`, and
    `src/lisp/deduce_why_result_lookup_derived.c3`
    now propagate checked `explain_dict_set*` insertion failure instead of
    returning partial why-result payloads after constructor success.
  - `src/lisp/deduce_rule_eval_analyze_payload_fields.c3`,
    `src/lisp/deduce_rule_eval_analyze_payload_tail.c3`, and
    `src/lisp/deduce_rule_eval_analyze_payload_result.c3`
    now treat insertion failure as a first-class `deduce/analyze` result
    error.
  - the remaining deduce explain/schema/stats helper family now follows the
    same contract:
    - `src/lisp/deduce_parallel_runtime_truth.c3`
    - `src/lisp/deduce_rule_ops_explain_goal_directed.c3`
    - `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3`
    - `src/lisp/deduce_rule_ops_explain_plan_payload.c3`
    - `src/lisp/deduce_rule_ops_explain_plan_steps.c3`
    - `src/lisp/deduce_rule_ops_explain_projection.c3`
    - `src/lisp/deduce_rule_ops_explain_snapshot.c3`
    - `src/lisp/deduce_rule_ops_explain_step_counters.c3`
    - `src/lisp/deduce_schema_query_metadata_schema_helpers.c3`
    - `src/lisp/deduce_schema_query_metadata_schema_payloads.c3`
    - `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
    - `src/lisp/deduce_schema_query_metadata_stats_payload.c3`
    - `src/lisp/deduce_schema_query_metadata_stats_tail.c3`
  - validation:
    - `c3c build`
    - bounded deduce slice:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'` -> `pass=328 fail=0`
  - `TODO.md` remains at zero live items.

- The raw-array constructor and AOT dict payload slice is now also closed:
  - `src/lisp/value_predicates_accessors_basic.c3` now routes `make_array(...)`
    through the checked array constructor path.
  - `src/lisp/prim_collection_sort_array.c3` now propagates raw array
    constructor `ERROR`s through `array(...)` and `list->array(...)`.
  - `src/lisp/aot_runtime_bridge.c3` now routes `dict_from_args(...)` through
    checked hashmap construction and checked insertion under the active bridge
    interpreter.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` and
    `src/lisp/tests_compiler_core_groups_fail_closed.c3`
    now pin those OOM seams directly.
  - the residual internal collection-constructor lane is now narrower again:
    - `AUDIT-COLLECTION-CONSTRUCTOR-RAW-HASHMAP-CALLERS-012`

- The staged collection-constructor migration is now fully closed:
  - `src/lisp/async_process_signal_dns_process.c3`,
    `src/lisp/async_process_spawn.c3`, and
    `src/lisp/prim_io_fs_handles.c3`
    now use checked `HASHMAP` / `ARRAY` constructors plus checked insertion
    for runtime status payloads.
  - `src/lisp/http_url_response.c3`,
    `src/lisp/eval_dispatch_error_payloads.c3`,
    `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`, and
    `src/lisp/primitives_meta_types_ctor_helpers.c3`
    now fail closed when auxiliary payload-map construction fails instead of
    dereferencing unchecked hashmap payloads.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now pins the fs,
    process-spawn, process-wait, HTTP response payload, dispatch/runtime-effect
    payload, and ctor-mismatch OOM seams directly.
  - `TODO.md` no longer tracks `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011`.

- Additional boundary/env-copy hardening landed:
  - `src/lisp/eval_promotion_context.c3` now fails closed if memo-entry
    allocation itself fails, instead of dereferencing a null `PromotionMemoEntry`.
  - `src/lisp/eval_env_copy_values.c3` and
    `src/lisp/eval_env_copy_frame_helpers.c3` now unwind iterator payload
    materialization when iterator wrapper construction fails mid-copy, including
    iterator payloads backed by copied `PARTIAL_PRIM` values.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now covers the
    iterator-wrapper failure rollback lane directly.
- Additional provenance hardening also landed:
  - `src/lisp/eval_boundary_provenance.c3` now walks nested `ARRAY`,
    `HASHMAP` / `SET`, and `METHOD_TABLE` payload edges before admitting
    target-chain fast reuse, so provenance/reuse classification now matches
    the graph-audit ownership model for shared wrappers.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now includes the exact
    missing target-chain-wrapper regression and the bounded smoke lane covers it.
- The destination-builder memo follow-up is also now closed:
  - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` now makes the
    current contract explicit: memo entries created while routing nested
    children inside temporary destination build scopes are builder-local and do
    not survive after the builder returns or aborts.
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now pins
    that contract directly.
- The destination-context follow-up is now also closed:
  - direct destination escape promotion in
    `src/lisp/eval_boundary_commit_escape_builder_helpers.c3`,
    `src/lisp/eval_boundary_commit_escape_helpers.c3`, and
    `src/lisp/eval_boundary_commit_destination.c3` now runs under the
    caller-provided `PromotionContext` rather than silently consuming ambient
    interpreter context.
  - destination-builder teardown now restores both memo state and the
    builder-local scope-chain cache snapshot.
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
    proves both the builder-local cache reset contract and the non-active
    caller-context direct-promotion contract.
- The wrapper-slot leak follow-up is now also closed:
  - `src/lisp/eval_promotion_copy_route_helpers.c3`,
    `src/lisp/eval_promotion_root_clone_basic.c3`, and
    `src/lisp/eval_promotion_root_clones.c3` now allocate destination wrapper
    values only at the commit point after fallible payload copy succeeds.
  - repeated failed shared-wrapper copy and root-store method-table clone
    attempts are now covered directly by allocation-count regressions in
    `src/lisp/tests_memory_lifetime_boundary_groups.c3` and
    `src/lisp/tests_memory_lifetime_root_boundary_groups.c3`.
- No live backlog items remain from this audit wave.
- The temporary `jit-policy` follow-up lane is now closed:
  - the remaining full-slice crash was traced to
    `src/lisp/jit_jit_eval_scope_chain_helpers.c3`, where the TCO recycle
    TEMP-graph scanner was allocating four `4096`-entry pointer arrays on the
    runtime stack.
  - the scanner now uses one heap-backed `JitTempGraphScan`, and the bounded
    full `OMNI_LISP_TEST_SLICE=jit-policy` container run is green again.

## Objective

Address the concrete defects found in the 2026-04-09 multi-lane audit without
mixing unrelated workstreams:

- boundary/runtime fail-closed regressions,
- async/REPL/FFI correctness defects,
- compiler/JIT/AOT fail-closed and parity defects,
- tooling/validation contract defects.

`memory/CHANGELOG.md` remains the implementation-truth source once behavior
changes land. This file is execution context only.

## Open Workstreams

### WS1: Boundary Fail-Closed Propagation

Defects:

- nested `copy_to_parent(...)` can silently materialize `null` payloads when a
  nested primitive copy rejects opaque `user_data`,
- nested `promote_to_escape(...)` can embed error/null-like payloads into
  `PARTIAL_PRIM` / `ITERATOR` wrappers instead of failing the boundary,
- primitive-copy rejection allocates target-scope garbage before it knows the
  copy is legal,
- shared-wrapper copy still returns original wrappers for some tags even after
  fast reuse has already been declined.

Primary files:

- `src/lisp/eval_promotion_copy_route_helpers.c3`
- `src/lisp/eval_promotion_copy_wrapper_helpers.c3`
- `src/lisp/eval_promotion_copy.c3`
- `src/lisp/eval_promotion_escape_leaf.c3`
- relevant memory-lifetime regression groups

Required closure:

- recursive boundary copy/promotion must abort transitively,
- no silently corrupted nested structures,
- focused regressions for nested opaque primitive payloads.

### WS2: Async / REPL / FFI Handle Safety

Defects:

- REPL worker startup/teardown can run with uninitialized mutex/condition
  state after partial init failure,
- TCP/UDP/TLS primitives accept arbitrary `FFI_HANDLE` boxes and reinterpret
  unrelated handle payloads,
- `process-spawn` can package constructor error values into a success-shaped
  result,
- HTTP URL/response parsing has malformed-port acceptance and header-slice
  boundary defects.

Primary files:

- `src/lisp/eval_repl_server_state.c3`
- `src/lisp/eval_repl_server_worker.c3`
- `src/lisp/async_socket_handle_runtime.c3`
- `src/lisp/async_tcp_transport_core.c3`
- `src/lisp/tls_handle_lifecycle.c3`
- `src/lisp/tls_primitives.c3`
- `src/lisp/async_process_spawn.c3`
- `src/lisp/http_url_response.c3`

Required closure:

- handle getters must validate handle kind before cast/use,
- REPL worker lifecycle must fail closed on partial init,
- process spawn must propagate constructor failure instead of returning poisoned
  success payloads,
- malformed HTTP URL/response boundaries must reject or parse cleanly.

Progress update (2026-04-09):

- shipped in the working tree:
  - REPL worker startup now refuses partial sync-init state,
  - TCP/UDP/TLS/FS handle getters validate exact handle names before cast,
  - `process-spawn` propagates handle-constructor error values instead of
    returning poisoned success payloads,
  - HTTP URL parsing rejects malformed `:port` suffixes and response header
    extraction trims delimiter residue.
- focused regressions landed for REPL worker start guards, handle-kind
  mismatch rejection, and HTTP parser edges.
- validation status:
  - async slice: green,
  - focused advanced FFI system group: green,
  - broad HTTP slice: green after the duplicate-id HTTP coverage was made deterministic.
- follow-up resolved:
  - the old broad-slice residual `HTTP-CRUD-DUPLICATE-POST-001` is closed.
  - `HTTP-CRUD-CONCURRENT-WRITES-001` is also closed:
    - the example now guards CRUD mutation entrypoints with a shared atomic
      write gate,
    - the concurrency probe moved into the focused `http-crud` slice,
    - the shipped application contract is "one success plus one normalized
      conflict" (`crud write already in progress` or `item already exists`),
      without raw runtime `ERROR` leakage.

### WS3: Compiler / JIT / AOT Fail-Closed and Parity

Defects:

- JIT multi-arg primitive/method-table paths dereference `args` without
  allocation checks,
- AOT multi-arg closure application diverges from JIT strict-arity semantics,
- generated AOT closure capture code dereferences `malloc` results without a
  null guard,
- `parse_program(...)` remains fail-open and current tests codify truncation.

Primary files:

- `src/lisp/jit_jit_apply_multi_prims.c3`
- `src/lisp/jit_jit_apply_multi_prims_tail.c3`
- `src/lisp/aot_runtime_bridge_trampoline.c3`
- `src/lisp/compiler_code_emission_lambda_defs.c3`
- `src/lisp/compiler_native_call_compilation_flat_style.c3`
- `src/lisp/parser_top_level_parse.c3`
- compiler core regressions

Required closure:

- low-memory JIT apply paths return explicit failure instead of crashing,
- AOT/JIT multi-arg closure behavior is semantically aligned,
- generated AOT capture allocation fails closed,
- `parse_program(...)` behavior is made explicit and regression-tested.

Progress update (2026-04-09):

- shipped in the working tree:
  - `src/lisp/jit_jit_apply_multi_prims.c3` and
    `src/lisp/jit_jit_apply_multi_prims_tail.c3` now null-guard multi-arg
    argument-buffer allocation for primitive and method-table dispatch, with a
    dedicated test seam for fail-closed OOM coverage.
  - `src/lisp/parser_top_level_parse.c3` and
    `src/lisp/compiler_program_pipeline_helpers.c3` now fail closed on parser
    error instead of returning a silently truncated prefix program.
  - `src/lisp/aot.c3` now provides shared arg-list counting and arity helpers,
    and `src/lisp/compiler_code_emission_lambda_defs.c3` uses them so
    generated multi-arg lambdas reject under-application, preserve JIT-style
    over-application chaining through `aot::apply_multi(...)`, and reject
    malformed arg lists explicitly.
  - `src/lisp/compiler_native_call_compilation_flat_style.c3` now guards
    closure-capture allocation in flat expression lowering without emitting
    invalid raw `return` statements into non-`Value*` generated contexts.
- focused regressions landed in:
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
  - `src/lisp/tests_compiler_core_groups_fail_closed.c3`
  - `src/lisp/tests_compiler_core_groups.c3`
- validation status:
  - `c3c build`: green
  - `OMNI_LISP_TEST_SLICE=jit-policy`: green
  - `OMNI_LISP_TEST_SLICE=compiler`: green
  - `scripts/run_e2e.sh`: green (`ALL 404 e2e compiler tests passed!`)

### WS4: Tooling / Validation Integrity

Defects:

- `check_status_consistency` disagrees with current zero-item `TODO.md`
  wording,
- validation scripts race on shared `build/obj` artifacts across concurrent
  host/container/ASAN runs,
- e2e compile-source parity is not policy-checked,
- validation status summary trusts exit codes more than emitted summary data,
- extra Docker args are expanded through unsafe shell word splitting.

Primary files:

- `scripts/check_status_consistency.sh`
- `scripts/run_validation_container.sh`
- `scripts/run_validation_status_summary.sh`
- `scripts/c3c_limits.sh`
- `scripts/run_e2e.sh`
- `scripts/check_e2e_baseline_policy.sh`

Required closure:

- zero-item backlog state passes the gate,
- concurrent validation runs cannot corrupt each other’s artifacts,
- e2e source-list drift is guarded,
- summary artifacts assert required suite telemetry,
- Docker argument handling preserves quoting.

Implementation note:

- start with the live `TODO.md` count/status wording fix and the repo-local
  validation lock, then tighten summary telemetry checks without changing the
  broader validation flow.

Progress update (2026-04-09):

- shipped in the working tree:
  - `scripts/check_status_consistency.sh` now accepts the current zero-item
    backlog wording.
  - `scripts/run_validation_container.sh` now serializes bounded validation
    runs with a repo-local lock.
  - `scripts/run_validation_status_summary.sh` now treats missing required
    `OMNI_TEST_SUMMARY` telemetry as a validation failure.
  - `scripts/c3c_limits.sh` now preserves quoted extra Docker args.
  - `scripts/run_e2e.sh` and `scripts/check_e2e_baseline_policy.sh` now guard
    Stage 3 compile-source parity explicitly.
- validation status:
  - `scripts/check_status_consistency.sh`: green
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: green
  - shell syntax checks across all touched validation scripts: green

## Execution Order

1. Land WS4 and the fail-closed portion of WS1 first.
2. Run bounded memory-lifetime validation and status/tooling checks.
3. Land WS2 safety fixes next.
4. Land WS3 parity and allocation hardening after the boundary/runtime surface
   is stable.

## Validation Baseline

- `scripts/check_status_consistency.sh`
- `c3c build`
- `c3c build --sanitize=address` for memory-sensitive lanes
- `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
- targeted async/compiler slices for WS2/WS3
- `scripts/run_e2e.sh` or `OMNI_E2E_COMPILE_ONLY=1 scripts/run_e2e.sh` where
  appropriate

## Completion

- WS1 shipped and validated through bounded `memory-lifetime-smoke`,
  including the shared-wrapper defensive clone policy and the finalize-lane
  detached-env UAF test fix that surfaced during integration validation.
- WS2 shipped and validated, including the CRUD concurrency follow-up closure.
- WS3 shipped and validated.
- WS4 shipped and validated.
- `TODO.md` actionable count returns to `0`.

## Post-Wave Follow-Up (2026-04-10)

- landed the JIT quasiquote pair-construction fail-closed follow-up:
  - `src/lisp/jit_jit_quasiquote_macros.c3`
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
- shipped behavior:
  - internal JIT quasiquote pair construction now goes through one checked
    helper with a narrow nth-failure seam.
  - nested quasiquote and list quasiquote expansion now return
    `"quasiquote: failed to allocate pair"` instead of wrapping cons
    constructor faults as successful quasiquote values.
  - direct regression landed in:
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
- validation status:
  - focused `jit-policy`: green (`quasiquote-cons-alloc-failure`)
- backlog shaping after this slice:
  - close `AUDIT-JIT-QUASIQUOTE-CONS-FAILCLOSED-033`
  - actionable backlog remains `0`

- landed the iterator tail propagation fix on top of the `StringVal` batch:
  - `src/lisp/primitives_iter_state.c3` now centralizes iterator pair
    construction through `iterator_make_pair_or_propagate(...)`
  - source iterator thunks in `src/lisp/primitives_iter_sources.c3` and
    coroutine/transform thunks in `src/lisp/primitives_iter_coroutine.c3` now
    return tail `ERROR` values directly instead of wrapping them in `CONS`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` adds direct
    regressions for both source-thunk and coroutine-thunk tail allocation
    failure
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=128 fail=0`)
- residual queue narrowed again:
  - `AUDIT-RUNTIME-CONSTRUCTOR-OOM-SUBSTRATE-008`

- landed the broader shared `StringVal` builder hardening slice after the
  runtime-helper wrapper pass:
  - `src/lisp/prim_string_format_helpers.c3` now makes builder creation/growth
    fail closed and exposes deterministic initial-alloc / grow-fail seams.
  - runtime string helpers in:
    - `src/lisp/prim_string_ops.c3`
    - `src/lisp/prim_string_format.c3`
    - `src/lisp/prim_string_format_directives.c3`
    now propagate builder OOM instead of continuing with invalid builder state.
  - parser string literal construction in:
    - `src/lisp/parser_datum_helpers.c3`
    - `src/lisp/parser_expr_atoms.c3`
    - `src/lisp/parser_patterns_values.c3`
    - `src/lisp/parser_quasiquote_datum_helpers.c3`
    now shares the checked builder path and fails closed with parser errors.
  - `src/lisp/primitives_meta_types.c3` no longer uses unchecked `StringVal`
    allocation in the `unsafe-free` error path.
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=127 fail=0`)
  - bounded ASAN `memory-lifetime-smoke`: green (`pass=127 fail=0`)
  - bounded `compiler`: green (`pass=191 fail=0`)
- closing this lane exposed the next adjacent runtime findings instead of
  leaving the queue artificially empty:
  - `AUDIT-RUNTIME-CONSTRUCTOR-OOM-SUBSTRATE-008`
  - `AUDIT-ITERATOR-TAIL-ERROR-PROPAGATION-008`

- landed adjacent runtime-helper fail-closed hardening after the boundary/JIT
  ownership wave:
  - `src/lisp/eval_apply.c3` now fails closed on chained partial wrapper
    allocation failure.
  - `src/lisp/primitives_iter_state.c3` and
    `src/lisp/value_predicates_accessors_basic.c3` now fail closed for iterator
    thunk/wrapper allocation failure.
  - `src/lisp/prim_string_ops.c3`, `src/lisp/prim_string_format.c3`, and
    `src/lisp/prim_string_format_helpers.c3` now centralize final string result
    wrapper allocation through one checked materialization helper.
  - `src/lisp/http_url_response.c3` now centralizes response field-key
    allocation through one checked helper.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` adds direct smoke
    regressions for all of the above.
- validation status:
  - `c3c build`: green
  - bounded `memory-lifetime-smoke`: green (`pass=126 fail=0`)
  - bounded ASAN `memory-lifetime-smoke`: green (`pass=126 fail=0`)
- residual adjacent runtime-helper audit item is explicit again:
  - `AUDIT-STRING-BUILDER-OOM-007`

- landed focused JIT/boundary alias-safety hardening on top of the closed wave:
  - `src/lisp/jit_jit_eval_scope_chain_helpers.c3` now treats target-chain
    `CONS` bindings with releasing-scope scalar edges as copy-required in the
    TCO env-copy lane.
  - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now makes the shared
    `copy_to_parent(...)` fast-reuse gate reject `CONS` wrappers whose direct
    children still live in the releasing scope, so the JIT TCO decision and
    the generic copy execution path stay consistent.
  - `src/lisp/eval_boundary_provenance.c3` now makes iterator alias safety
    recurse into target-chain non-closure / non-partial payload graphs instead
    of relying on a shallow payload pointer check.
  - focused regressions landed in:
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    - `src/lisp/tests_memory_lifetime_tco_budget_groups.c3`
    - `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3`
- validation status:
  - `c3c build`: green
  - targeted `OMNI_LISP_TEST_SLICE=jit-policy`: green for the focused alias
    regressions
  - bounded `memory-lifetime-smoke`: green (`pass=103 fail=0`)
- new residual audit findings are explicit in `TODO.md` again:
  - `AUDIT-BOUNDARY-DESTINATION-CTX-005`
  - `AUDIT-BOUNDARY-WRAPPER-SLOT-LEAK-005`

- landed the constructor substrate hardening slice for the runtime-dependent
  surfaces instead of pretending the whole repo had already migrated:
  - `src/lisp/value_core_types.c3`,
    `src/lisp/value_interp_alloc_helpers.c3`,
    `src/lisp/value_constructors_lifecycle.c3`, and
    `src/lisp/primitives_meta_types.c3`
    now track heap ownership for `STRING` / `ERROR` chars so best-effort
    fallback literals do not invalidate teardown.
  - `src/lisp/value_constructors.c3` now makes `make_error(...)` fail closed
    with a printable fallback value.
  - checked constructor/grow helpers landed in:
    - `src/lisp/value_predicates_accessors_basic.c3`
    - `src/lisp/prim_collection_hashmap.c3`
  - the runtime-dependent callers now using those checked helpers are:
    - raise payload construction
    - `Dictionary`
    - `Set`
    - `to-array`
  - direct regressions landed in:
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- backlog shaping after this slice:
  - close `AUDIT-RUNTIME-CONSTRUCTOR-OOM-SUBSTRATE-008`
  - open `AUDIT-COLLECTION-CONSTRUCTOR-CALLSITE-MIGRATION-009` for the broader
    internal `make_array(...)` / `make_hashmap(...)` caller migration

- landed the first internal constructor-callsite migration slice on the
  data-format bridges:
  - `src/lisp/json.c3`
  - `src/lisp/primitives_toml_bridge.c3`
  - `src/lisp/primitives_data_formats_csv_parse.c3`
- shipped behavior:
  - JSON/TOML object and array decode now use checked collection constructors
    plus checked hashmap insertion.
  - CSV parser row/result construction now uses checked array constructors.
  - nested conversion failures in JSON/TOML now propagate as ordinary `ERROR`
    values instead of being embedded into partial collections.
  - direct regressions landed in:
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=133 fail=0`)
  - bounded ASAN `memory-lifetime-smoke`: green (`pass=133 fail=0`)
- backlog shaping after this slice:
  - close `AUDIT-COLLECTION-CONSTRUCTOR-CALLSITE-MIGRATION-009`
  - open:
    - `AUDIT-COLLECTION-CONSTRUCTOR-SCHEMA-EXPLAIN-010`
    - `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011`

- landed the next collection-constructor migration slice on schema explain:
  - `src/lisp/schema_explain_payload_helpers.c3`
  - `src/lisp/schema_explain_helpers.c3`
  - `src/lisp/schema_explain_effect.c3`
  - `src/lisp/schema_explain_effect_result_payload.c3`
  - `src/lisp/schema_explain_effect_runtime.c3`
  - `src/lisp/schema_explain_effect_helpers.c3`
- shipped behavior:
  - schema explain payload maps now use checked construction and checked
    insertion through one explicit `"schema explain: out of memory"` contract.
  - dispatch explain, effect explain, and helper payload/source maps now fail
    closed instead of dereferencing unchecked `make_hashmap(...)` results.
  - direct regressions landed in:
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=134 fail=0`)
  - bounded ASAN `memory-lifetime-smoke`: green (`pass=134 fail=0`)
- backlog shaping after this slice:
  - close `AUDIT-COLLECTION-CONSTRUCTOR-SCHEMA-EXPLAIN-010`
  - leave only:
    - `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011`

- landed the adjacent apply/promotion helper fail-closed slice:
  - `src/lisp/eval_apply.c3`
  - `src/lisp/prim_collection_hashmap.c3`
  - `src/lisp/prim_io_fs_handles.c3`
  - `src/lisp/primitives_data_formats_csv_parse.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- shipped behavior:
  - `apply_partial(...)` now rejects malformed `PARTIAL_PRIM` execution state
    before call-through.
  - checked hashmap insertion now rejects promoted `ERROR` values from
    `boundary_promote_to_root(...)` instead of storing them.
  - `fs_array_push(...)` and `csv_array_push(...)` now reject promoted
    `ERROR` values instead of appending them into successful arrays.
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=152 fail=0`)
- backlog shaping after this slice:
  - close `AUDIT-APPLY-PROMOTION-FAILCLOSED-019`
  - actionable backlog returns to `0`

- landed the malformed apply/JIT primitive dispatch follow-up:
  - `src/lisp/eval_apply.c3`
  - `src/lisp/jit_jit_apply_helpers.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
- shipped behavior:
  - `apply_primitive(...)` now rejects malformed primitive wrappers before
    function-pointer call-through.
  - `apply_partial(...)` now rejects missing `first_arg` so impossible
    partially-captured state does not flow into primitive execution.
  - `jit_apply_value_primitive(...)` now applies the same malformed primitive
    guard on the JIT helper path.
- validation status:
  - focused `jit-policy`: green (`invalid-primitive-state-fails-closed`)
  - bounded `memory-lifetime-smoke`: green (`pass=152 fail=0`)
- backlog shaping after this slice:
  - close `AUDIT-APPLY-DISPATCH-FAILCLOSED-020`
  - actionable backlog remains `0`

- landed the coroutine thunk promotion follow-up:
  - `src/lisp/primitives_coroutine.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- shipped behavior:
  - coroutine thunk publication now fails closed after
    `boundary_promote_to_root(...)` if promotion returns:
    - `null`,
    - an `ERROR`,
    - or an invalid/non-closure thunk wrapper.
  - coroutine creation no longer allocates `StackCtx` state around invalid
    promoted thunk values.
  - direct regression landed in:
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=153 fail=0`)
- backlog shaping after this slice:
  - close `AUDIT-COROUTINE-THUNK-PROMOTION-FAILCLOSED-021`
  - actionable backlog remains `0`

- landed the data-format array promotion follow-up:
  - `src/lisp/json.c3`
  - `src/lisp/primitives_toml_bridge.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- shipped behavior:
  - JSON and TOML array assembly now fail closed if
    `boundary_promote_to_root(...)` returns an `ERROR` for an element.
  - promoted boundary failures are no longer published as ordinary array data.
  - direct regression landed in:
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=154 fail=0`)
- backlog shaping after this slice:
  - close `AUDIT-DATA-FORMAT-ARRAY-PROMOTION-FAILCLOSED-022`
  - actionable backlog remains `0`

- landed the escape cons publication follow-up:
  - `src/lisp/value_constructors_core.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- shipped behavior:
  - `make_cons(...)` now fails closed if string/error escape promotion cannot
    actually move the field into the ESCAPE lane.
  - staged promoted fields are unwound if final escape-pair allocation fails.
  - direct regression landed in:
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=155 fail=0`)
- backlog shaping after this slice:
  - close `AUDIT-CONS-ESCAPE-PROMOTION-FAILCLOSED-023`
  - actionable backlog remains `0`

- landed the scheduler shared/offload projection follow-up:
  - `src/lisp/scheduler_state_shared_handles.c3`
  - `src/lisp/scheduler_offload_ops.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- shipped behavior:
  - missing/unmaterializable shared payload handles now stay visible as
    scheduler errors instead of becoming empty-string success values.
  - offload `read-file` / `file-exists` path projection faults now become
    `OFFLOAD_RES_ERROR` instead of synthetic `nil` / `0` results.
  - direct regressions landed in:
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=156 fail=0`)
- backlog shaping after this slice:
  - close `AUDIT-SCHEDULER-SHARED-PROJECTION-FAILCLOSED-024`
  - actionable backlog remains `0`

- landed the scheduler batch result-list fail-closed follow-up:
  - `src/lisp/scheduler_primitives_threads.c3`
  - `src/lisp/scheduler_primitives_offload_execute.c3`
  - `src/lisp/scheduler_primitives_task_spawn.c3`
  - `src/lisp/tests_scheduler_groups_more.c3`
- shipped behavior:
  - scheduler batch primitives now route final result-list assembly through a
    checked scheduler-local prepend helper and surface typed out-of-memory
    errors instead of publishing partial result lists when final cons
    construction fails.
  - `task-spawn-batch` now drops already-spawned live thread-task entries if
    result-list publication fails after task creation.
  - direct regression landed in:
    - `src/lisp/tests_scheduler_groups_more.c3`
- validation status:
  - bounded `scheduler`: green (`pass=109 fail=0`)
- backlog shaping after this slice:
  - close `AUDIT-SCHEDULER-BATCH-RESULT-LIST-FAILCLOSED-030`
  - actionable backlog remains `0`

- landed the shared two-arg list materialization follow-up:
  - `src/lisp/value_constructors.c3`
  - `src/lisp/prim_system.c3`
  - `src/lisp/jit_jit_runtime_effects_handle.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
- shipped behavior:
  - one checked `make_list2_or_error(...)` helper now covers runtime/JIT
    helper sites that were previously nesting raw `make_cons(...)`.
  - `(shell cmd true)` now fails closed if result-list construction fails.
  - pending-raise and effect-handler arg-pair materialization now fail closed
    before handler call-through if `(k arg)` construction fails.
  - direct regressions landed in:
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
- validation status:
  - focused `jit-policy`: green (`handler-arg-list-alloc-failure`)
  - bounded `memory-lifetime-smoke`: green (`pass=160 fail=0`)
- backlog shaping after this slice:
  - close `AUDIT-TWO-ARG-LIST-MATERIALIZATION-FAILCLOSED-031`
  - actionable backlog remains `0`

- landed the JIT multi-arg list fail-closed follow-up:
  - `src/lisp/jit_jit_apply_runtime.c3`
  - `src/lisp/jit_jit_apply_multi_prims.c3`
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
- shipped behavior:
  - continuation-safe multi-arg call-list assembly now rejects
    `make_cons(...)` failure while building the arg list.
  - `jit_apply_multi_args_iterative(...)` now rejects malformed/truncated arg
    lists instead of returning partial success when the arg list breaks early.
  - direct regression landed in:
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
- validation status:
  - focused `jit-policy`: green (`multi-arg-malformed-list-fails-closed`)
- backlog shaping after this slice:
  - close `AUDIT-JIT-MULTI-ARG-LIST-FAILCLOSED-032`
  - actionable backlog remains `0`
