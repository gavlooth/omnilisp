# Memory Changelog Index Part 13

Source: `memory/CHANGELOG.md`

## 2026-04-10

- Closed the async file I/O size-guard lane:
  - `src/lisp/prim_io_file.c3`
    now checks path/content payload length arithmetic before building the
    internal async write-file offload payload.
  - `src/lisp/scheduler_offload_ops.c3`
    now checks atomic temp-path length arithmetic before allocating the
    temporary path buffer.
  - `src/lisp/prim_io_file_helpers.c3`
    now rejects a max-sized read buffer before adding the trailing byte for
    the file-read scratch buffer.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded async slice: `pass=61 fail=0`

- Closed the scheduler offload nil-completion projection lane:
  - `src/lisp/scheduler_wakeup_io.c3`
    now returns directly from each offload completion-kind projection,
    including `OFFLOAD_RES_NIL`, so async read-file/read-lines missing-path
    paths can remap the nil offload result to their I/O payload codes instead
    of leaking `scheduler/offload-invalid-completion-kind`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded async slice: `pass=61 fail=0`

- Closed the I/O/string-buffer growth hardening lane:
  - `src/lisp/prim_io_console_helpers.c3`
    now checks console capture append/copy growth arithmetic before appending
    or duplicating buffered text.
  - `src/lisp/prim_io_helpers.c3`
    now rejects overflowing input-state append lengths before mutating the
    live buffer.
  - `src/lisp/primitives_data_formats_csv_parse.c3`
    now guards CSV field and row-array growth before allocating replacement
    storage.
  - `src/lisp/eval_repl_server_state.c3`
    now rejects overflowing session-string and session-capacity growth before
    publishing replacement session state.
  - `src/lisp/unicode_case_utf8proc.c3` and
    `src/lisp/unicode_case_mapping.c3`
    now route case-mapping append growth through a checked helper so the
    UTF-8 output buffer cannot overflow or grow from invalid capacity state.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded unicode slice: `pass=25 fail=0`
    - bounded data-format slice: `pass=59 fail=0`
    - bounded async slice after scheduler nil-completion projection fix:
      `pass=61 fail=0`
    - bounded advanced unicode iterator group: `pass=129 fail=0`

- Closed the deduce direct allocation/schema mutation hardening lane:
  - `src/lisp/deduce_db_handles_mutation.c3`
    now rejects overflowing rule-signature count increments, uses subtract-form
    term-offset bounds checks, and guards direct rule metadata array
    allocations before `sizeof * count` can wrap.
  - `src/lisp/deduce_db_relation_schema_init.c3`,
    `src/lisp/deduce_db_handles.c3`, and
    `src/lisp/deduce_db_handles_register.c3`
    now reject overflowing schema column/index sizing before allocation or
    relation-schema count publication.
  - `src/lisp/deduce_db_handles_mutation_txn.c3`
    now increments transaction `inserted_count` only after the tuple delta set
    append succeeds, so failed append no longer inflates cardinality estimates.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded deduce slice: `pass=330 fail=0`

- Closed the core registry/table growth hardening lane:
  - `src/lisp/value_symbol_table.c3` and
    `src/lisp/value_type_registry.c3`
    now fail closed when init/grow allocation cannot be materialized, reject
    overflowed table/hash byte-size arithmetic before allocation, and avoid
    mutating table state until replacement allocations are confirmed.
  - `src/lisp/value_interp_init_helpers.c3`
    now routes macro/module table and hash table init sizing through checked
    multiplication before allocation.
  - `src/lisp/eval_pattern_match_support.c3`
    now guards `gensym` and match-binding growth loops against doubling and
    allocation-byte overflow before replacing backing arrays.
  - `src/lisp/prim_collection_hashmap.c3` and
    `src/lisp/prim_collection_sort_array.c3`
    now enforce power-of-two/valid capacity invariants for hashmap state and
    reject invalid/overflowing grow arithmetic before mutating collection
    backing storage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded allocator-validation slice: `pass=1 fail=0`
    - bounded advanced collections/module group: `pass=134 fail=0`
    - bounded advanced type-dispatch/mutation-chain group: `pass=236 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- Closed the audit overflow hardening batch:
  - `src/lisp/deduce_db_rule_signature_record_codec.c3`,
    `src/lisp/deduce_db_rule_signature_restore.c3`,
    `src/lisp/deduce_db_rule_signature_persistence.c3`,
    `src/lisp/deduce_db_rule_catalog_record_codec.c3`, and
    `src/lisp/deduce_db_rule_catalog_persistence.c3`
    now route persisted rule signature/catalog record sizing, byte copies, and
    restore cursor movement through checked add/mul/cursor helpers instead of
    allowing `sizeof * count` or `cursor + len` wraparound.
  - `src/lisp/ast_arena.c3`, `src/lisp/parser_import_helpers.c3`,
    `src/lisp/parser_module_decl.c3`, and
    `src/lisp/parser_export_from.c3`
    now fail closed on AST arena alignment/chunk accounting overflow and on
    parser import/module/export allocation byte-size overflow.
  - `src/lisp/jit_jit_apply_multi_prims.c3`,
    `src/lisp/jit_jit_apply_runtime.c3`,
    `src/lisp/jit_jit_runtime_effects_handle.c3`, and
    `src/lisp/jit_jit_handle_signal_helpers.c3`
    now reject oversized JIT arg buffers, handler clause arrays, and
    handle-state snapshot copies before allocation-size arithmetic can wrap.
  - `src/lisp/value_interp_lifecycle.c3`,
    `src/lisp/value_environment.c3`, and
    `src/lisp/jit_jit_define_method_table.c3`
    now guard interpreter macro/module/handler table growth, env binding
    growth, and method-table growth before doubling or allocation byte-size
    arithmetic can wrap.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded deduce slice: `pass=330 fail=0`
    - bounded allocator-validation slice: `pass=1 fail=0`
    - bounded advanced collections/module group: `pass=134 fail=0`
    - bounded advanced effect-continuation group: `pass=56 fail=0`
    - bounded advanced runtime-control group: `pass=22 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- Closed the runtime module export growth fail-closed lane:
  - `src/lisp/jit_jit_module_setup_helpers.c3`
    now routes module export table allocation/growth through a checked helper
    with a narrow failure seam, preserves the existing export table on growth
    failure, and rejects oversized export capacities before byte-size
    arithmetic can wrap.
  - `src/lisp/jit_jit_compile_effects_modules.c3` and
    `src/lisp/jit_jit_module_import_setup.c3`
    now propagate export-table growth failure instead of writing through a
    failed replacement table while re-exporting or implicitly exporting module
    definitions.
  - `src/lisp/tests_advanced_stdlib_module_groups.c3`
    now pins forced module export growth allocation failure and verifies the
    original export table remains intact.
  - validation:
    - `c3c build`
    - bounded advanced collections/module group: `pass=134 fail=0`

- Closed the scheduler offload missing-completion fail-closed lane:
  - `src/lisp/scheduler_offload_worker.c3`
    now publishes or directly handles non-task offload readiness even when both
    worker completion materialization and fallback alloc-failure completion
    materialization return `null`, so a blocked waiter is not stranded.
  - `src/lisp/scheduler_wakeup_io.c3`
    now treats an active completed offload slot with a null completion as a
    deterministic `"offload: missing completion"` error and clears the pending
    slot after consumption.
  - `src/lisp/tests_scheduler_boundary_offload_payload_groups.c3`
    now pins the completed-null wakeup path, verifies the blocked fiber becomes
    ready, and verifies consumption clears the pending offload slot.
  - validation:
    - `c3c build`
    - bounded scheduler slice: `pass=111 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- Closed the deduce/JIT capacity-growth byte-overflow lane:
  - `src/lisp/deduce_schema_query_relation_alloc.c3`,
    `src/lisp/deduce_rule_eval_exec_aggregate_groups.c3`,
    `src/lisp/deduce_rule_eval_exec_component_delta_codec.c3`,
    `src/lisp/deduce_rule_eval_exec_component_state_helpers.c3`,
    `src/lisp/deduce_schema_query_input_analysis.c3`,
    `src/lisp/deduce_db_handles_mutation_txn.c3`,
    `src/lisp/deduce_db_handles_incremental_tracking.c3`,
    `src/lisp/deduce_db_rule_signature_helpers.c3`,
    `src/lisp/deduce_db_handles.c3`, and
    `src/lisp/deduce_db_rule_catalog_record_codec.c3`
    now fail closed before `sizeof * new_cap` can overflow during relation,
    delta, aggregate, query-demand, transaction, dirty-predicate, signature,
    relation-schema, and persisted-rule-catalog growth.
  - `src/lisp/jit_jit_module_setup_helpers.c3`
    now rejects oversized source-dir vector growth and path/string length
    increments before allocation-size arithmetic can wrap.
  - `src/lisp/tests_deduce_groups_parallel.c3` and
    `src/lisp/tests_deduce_groups.c3`
    now pin the oversized-capacity fail-closed path in the bounded deduce
    parallel group.
  - validation:
    - `c3c build`
    - bounded deduce parallel group: `pass=6 fail=0`
    - bounded scheduler slice: `pass=111 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- Closed the iterator/coroutine malformed-state normalization lane:
  - `src/lisp/primitives_iter_state.c3`
    now rejects missing internal list/array iterator thunk state directly, and
    no longer treats a null recursive tail as ordinary iterator completion.
  - `src/lisp/primitives_iter_coroutine.c3`
    now rejects malformed thunk arg state for `map`, `filter`, `take`, and
    `zip` instead of normalizing it to `nil`, and `filter` no longer treats a
    non-iterator internal state transition as successful exhaustion.
  - `src/lisp/primitives_iter_terminal.c3`
    now rejects a null iterator pair from internal thunk dispatch instead of
    truncating `collect` / `to-array` as if the stream completed normally.
  - `src/lisp/primitives_coroutine_resume.c3`
    now rejects missing yielded values and missing completed results instead of
    fabricating successful `nil` returns during coroutine resume.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` and
    `src/lisp/tests_memory_lifetime_groups.c3`
    now pin malformed iterator thunk state plus missing-yield /
    missing-completion resume state directly in the bounded lifetime lanes.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the runtime effect publication fail-closed lane:
  - `src/lisp/value_constructors.c3`
    now builds handled-raise payload maps through the raw constructor path,
    and `raise_error_pending_impl(...)` now rejects payload-construction
    failure before handler binding instead of degrading to a message-only
    handled raise.
  - `src/lisp/value_interp_continuation_helpers.c3`
    now exposes a narrow continuation-allocation failure seam and returns
    `null` instead of dereferencing a failed root-scope allocation.
  - `src/lisp/jit_jit_handle_signal.c3`,
    `src/lisp/jit_jit_runtime_effects_handle.c3`,
    `src/lisp/jit_jit_reset_shift.c3`, and
    `src/lisp/jit_jit_runtime_effects_reset_shift.c3`
    now fail closed on continuation allocation failure instead of
    null-dereferencing in handled effect / capture dispatch.
  - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
    now fails closed with `"runtime effect payload: out of memory"` when
    unhandled-effect diagnostic payload construction cannot complete, instead
    of silently dropping the payload and publishing a degraded business error.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` and
    `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    now pin handled-raise payload allocation failure and handled-effect
    continuation allocation failure directly in the bounded runtime/JIT lanes.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=pending-raise-payload-alloc-failure,handle-continuation-alloc-failure ./build/main --test-suite lisp'`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the runtime helper list-materialization fail-closed lane:
  - `src/lisp/value_constructors.c3`
    now exposes a shared `make_cons_or_error(...)` helper with a narrow
    nth-failure seam for deterministic runtime list-builder tests.
  - `src/lisp/prim_string_transform.c3`
    now makes `string-split` reject internal result-list construction failure
    instead of continuing with partial/null list state.
  - `src/lisp/prim_io_file.c3`
    now makes `read-lines` reject internal result-list construction failure
    instead of continuing with partial/null list state.
  - `src/lisp/prim_collection_hashmap_key_helpers.c3`
    now makes `keys` / `values` canonical list assembly fail closed in both
    sorted and fallback paths instead of reusing raw `make_cons(...)`.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now directly pins string-split and hashmap key/value list allocation
    failure in the bounded runtime alloc lane.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the JIT helper arg-construction fail-closed lane:
  - `src/lisp/value_constructors.c3`
    now exposes one checked `make_list1_or_error(...)` helper with a narrow
    nth-failure seam for deterministic variadic-rest tests.
  - `src/lisp/jit_jit_apply_helpers.c3` and
    `src/lisp/jit_jit_apply_runtime.c3`
    now reject variadic zero-fixed-arg rest-list construction failure before
    binding the rest parameter environment.
  - `src/lisp/jit_jit_dispatch_helpers.c3`
    now routes instance `ref` dispatch arg-list construction through the
    shared checked two-item helper instead of nesting raw `make_cons(...)`.
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    now directly pins the variadic rest-list allocation failure in the
    `jit-policy` slice.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=variadic-rest-list-alloc-failure ./build/main --test-suite lisp'`

- Closed the JIT quasiquote pair-construction fail-closed lane:
  - `src/lisp/jit_jit_quasiquote_macros.c3`
    now routes all internal quasiquote pair construction through one checked
    helper with a narrow nth-failure seam and returns
    `"quasiquote: failed to allocate pair"` instead of wrapping cons
    constructor faults as successful quasiquote values.
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    now directly pins both nested quasiquote and list quasiquote pair
    construction failure in the `jit-policy` slice.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=quasiquote-cons-alloc-failure ./build/main --test-suite lisp'`

- Closed the JIT multi-arg list fail-closed lane:
  - `src/lisp/jit_jit_apply_runtime.c3`
    now rejects `make_cons(...)` failure while constructing continuation-safe
    multi-arg call lists.
  - `src/lisp/jit_jit_apply_multi_prims.c3`
    now makes `jit_apply_multi_args_iterative(...)` return
    `"arg list too short"` when the arg list breaks before all required args
    are consumed, instead of returning a partial apply result as success.
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    now pins the malformed multi-arg list case directly in the `jit-policy`
    slice.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=multi-arg-malformed-list-fails-closed ./build/main --test-suite lisp'`

- Closed the shared two-arg list materialization fail-closed lane:
  - `src/lisp/value_constructors.c3`
    now exposes one checked `make_list2_or_error(...)` helper with a narrow
    nth-failure seam for deterministic runtime/JIT tests.
  - `src/lisp/prim_system.c3`
    now makes `(shell cmd true)` fail closed with
    `"shell: failed to construct result list"` if the final two-item result
    list cannot be built.
  - `src/lisp/jit_jit_runtime_effects_handle.c3`
    now routes both pending-raise and normal effect-handler arg-pair
    construction through the same checked helper so constructor failure
    propagates before handler call-through.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=handler-arg-list-alloc-failure ./build/main --test-suite lisp'`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the scheduler batch result-list fail-closed lane:
  - `src/lisp/scheduler_primitives_threads.c3`
    now routes batch thread result-list assembly through one checked
    scheduler-local prepend helper with a narrow nth-failure seam.
  - `src/lisp/scheduler_primitives_offload_execute.c3`
    now returns a typed `"offload: out of memory"` error if final result-list
    publication fails.
  - `src/lisp/scheduler_primitives_task_spawn.c3`
    now drops already-spawned live thread-task entries if result-list
    publication fails after task creation.
  - `src/lisp/tests_scheduler_groups_more.c3`
    now proves forced result-list cons allocation failure in offload-batch,
    task-spawn-batch, and thread-spawn-batch leaves active thread-task count
    unchanged.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`

- Closed the OS-thread null-completion fail-closed lane:
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
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the malformed iterator-tail fail-open lane:
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
    - `scripts/run_validation_container.sh bash -lc "rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --eval \"(handle (List (Iterator (lambda () (cons 1 2)))) (raise msg (ref msg 'message)))\""`
    - `scripts/run_validation_container.sh bash -lc "rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --eval \"(handle (foldl (lambda (a x) (+ a x)) 0 (Iterator (lambda () (cons 1 9)))) (raise msg (ref msg 'message)))\""`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the scheduler wakeup publish-fallback lane:
  - `src/lisp/scheduler_wakeup_callbacks.c3`
    now makes timer, sleep, and poll-error callbacks fall back to the same
    direct wakeup handlers when reliable wakeup enqueue fails, instead of
    silently dropping the blocked-fiber completion.
  - `src/lisp/scheduler_offload_worker.c3`
    now makes non-task offload completion fall back to
    `scheduler_handle_wakeup_offload_ready(...)` when offload-ready publish
    fails, instead of freeing the live completion payload and stranding the
    blocked waiter.
  - `src/lisp/tests_scheduler_groups_more.c3`
    now pins publish-failure fallback for timer, sleep, poll-error, and
    offload-after paths by forcing the real
    `scheduler_publish_reliable_wakeup(...)` failure seam.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the collection/apply array fail-open lane:
  - `src/lisp/prim_collection_sort_array.c3`
    now makes `sort` / `sort-by` propagate list-construction failure
    directly, makes `sort-by` propagate comparator application errors
    instead of silently returning a partial sort, makes `array`,
    `list->array`, `set!` on arrays, and `push!` reject
    `boundary_promote_to_root(...)` failures instead of storing `ERROR`
    values as data, and makes `push!` fail closed on grow allocation
    failure instead of null-dereferencing the resized item buffer.
  - `src/lisp/primitives_iter_terminal.c3`
    now makes `collect` propagate list-construction failure directly and
    makes `to-array` reject `boundary_promote_to_root(...)` failure instead
    of returning an array populated with `ERROR` elements.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins array constructor/mutator and `to-array` boundary-promotion
    failure, `push!` grow failure, and `sort-by` comparator failure.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the string-backed runtime payload/list fail-open lane:
  - `src/lisp/prim_system.c3`
    now makes `(shell cmd true)` return the string-constructor `ERROR`
    directly instead of returning `(ERROR exit-code)`.
  - `src/lisp/prim_io_fs_handles.c3`
    now makes `fs-readdir` return entry-name string construction failure
    directly instead of storing `ERROR` entries in the returned array.
  - `src/lisp/http.c3`
    now makes `http-get` / `http-request` return host/request string
    materialization failure directly before transport setup/write.
  - `src/lisp/schema_validation.c3`
    now makes `schema-explain` return message-string construction failure
    directly instead of wrapping it inside a singleton explanation list.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins `shell`, `fs-readdir`, and `schema-explain` under forced
    string-wrapper allocation failure.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the string/list materializer fail-open lane:
  - `src/lisp/prim_string_transform.c3`
    now makes `string-upcase` and `string-downcase` return string-constructor
    `ERROR`s directly instead of mutating a non-STRING result, and
    `string-split` now propagates per-part `make_string(...)` failure instead
    of embedding it into a successful list.
  - `src/lisp/prim_string_ops.c3`
    now makes `string->list` propagate per-character `make_string(...)`
    failure directly instead of consing an `ERROR` value into a normal list.
  - `src/lisp/unicode.c3`
    now makes `string-graphemes` propagate grapheme-cluster string materialization
    failure directly instead of storing `ERROR` values into the cluster array
    and later returning a successful list containing them.
  - `src/lisp/prim_io_file.c3`
    now makes `read-lines` propagate per-line `make_string(...)` failure
    directly instead of embedding it into the intermediate line list.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins `string-upcase`, `string-downcase`, `string->list`,
    `string-split`, and `string-graphemes` under forced string-wrapper
    allocation failure.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the schema-explain list-builder fail-closed lane:
  - `src/lisp/schema_explain_payload_helpers.c3`
    now routes list accumulation and list reversal through one checked
    `explain_prepend_or_oom(...)` helper, so schema-explain list builders
    return the existing `"schema explain: out of memory"` error instead of
    hard-aborting on an internal cons allocation failure.
  - `src/lisp/schema_explain_helpers.c3`,
    `src/lisp/schema_explain_effect_helpers.c3`, and
    `src/lisp/schema_explain_effect_runtime.c3`
    now propagate that same list-builder failure for dispatch candidates,
    handler tag lists, and effect candidates.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins helper-level and top-level schema-explain list-builder OOM seams
    through a dedicated local `nth` fail seam.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the checked collection-mutator silent-failure lane:
  - `src/lisp/prim_collection_hashmap.c3`
    now makes `hashmap_set_symbol(...)`, `hashmap_grow(...)`, and
    `hashmap_set(...)` return checked `bool` results instead of discarding the
    underlying grow/insert outcome.
  - `src/lisp/prim_collection_hashmap.c3`
    now makes `set!` on dictionary targets fail closed with
    `runtime/out-of-memory` when backing-storage growth fails instead of
    returning `Void` after silently dropping the write.
  - `src/lisp/prim_collection_generic_set.c3`
    now makes `set-add` fail closed on the same backing-storage grow failure
    instead of reporting success after a no-op mutation.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins both mutator seams directly and proves the failed write does not
    materialize in the collection after the error.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the deduce explain/analyze/why-result checked-insertion follow-up:
  - the checked-constructor follow-up is now complete for the remaining deduce
    payload-map family, not just raw `make_hashmap(...)` callsites.
  - `src/lisp/deduce_why_result_payload.c3`,
    `src/lisp/deduce_why_result_path_payload.c3`,
    `src/lisp/deduce_why_result_lookup.c3`, and
    `src/lisp/deduce_why_result_lookup_derived.c3`
    now propagate the first `explain_dict_set*` insertion failure from
    context/path/payload attachment instead of silently returning a partial
    why-result payload after checked constructor success.
  - `src/lisp/deduce_rule_eval_analyze_payload_fields.c3`,
    `src/lisp/deduce_rule_eval_analyze_payload_tail.c3`, and
    `src/lisp/deduce_rule_eval_analyze_payload_result.c3`
    now fail `deduce/analyze` result-map construction closed on the first
    checked insertion failure instead of ignoring the failed write.
  - the remaining deduce explain/schema/stats helper family now follows the
    same contract too:
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
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'` -> `pass=328 fail=0`

- Closed the remaining guarded raw-hashmap normalization lane for
  `deduce_*` / `unify_*` payload and result builders:
  - `src/lisp/deduce_relation_row_materialization.c3` now routes row-dict
    materialization through checked hashmap construction plus checked
    insertion, so scan/query row payload building fails closed under
    constructor or grow pressure.
  - `src/lisp/deduce_relation_ops_validation_payload.c3` now routes integrity
    payload maps through one checked payload-dict helper and treats
    `explain_dict_set*` failure as payload omission instead of returning a
    partially populated machine-readable conflict payload.
  - `src/lisp/deduce_rule_eval_exec_component_state_helpers.c3`,
    `src/lisp/deduce_rule_eval_exec_component_state.c3`,
    `src/lisp/deduce_rule_eval_exec_aggregate_state.c3`,
    `src/lisp/deduce_rule_eval_exec_seminaive.c3`,
    `src/lisp/deduce_rule_eval_scc.c3`,
    `src/lisp/deduce_relation_scan_helpers_join.c3`,
    `src/lisp/deduce_rule_eval_analyze_setup.c3`, and
    `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
    now use checked hashmap construction and checked insertion for deduce
    runtime helper state maps instead of ad hoc raw-constructor mutation.
  - `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3`,
    `src/lisp/deduce_rule_ops_explain_plan_payload.c3`,
    `src/lisp/deduce_rule_ops_explain_plan_steps.c3`,
    `src/lisp/deduce_rule_ops_explain_projection.c3`,
    `src/lisp/deduce_rule_ops_explain_snapshot.c3`,
    `src/lisp/deduce_rule_ops_explain_step_counters.c3`,
    `src/lisp/deduce_schema_query_metadata_schema_helpers.c3`,
    `src/lisp/deduce_schema_query_metadata_schema_payloads.c3`,
    `src/lisp/deduce_schema_query_metadata_stats.c3`,
    `src/lisp/deduce_schema_query_metadata_integrity_history.c3`, and
    `src/lisp/deduce_rule_eval_analyze_payload_result.c3`
    now route the remaining already-guarded deduce explain/schema/analyze
    payload dictionaries through checked constructors instead of raw
    `make_hashmap(...)`.
  - `src/lisp/deduce_why_result_path_payload.c3`,
    `src/lisp/deduce_why_result_payload.c3`,
    `src/lisp/deduce_why_result_lookup.c3`, and
    `src/lisp/deduce_why_result_lookup_derived.c3`
    now fail closed when why-result path/payload dictionary construction
    fails, instead of treating missing path payloads as ordinary successful
    provenance snapshots.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`,
    `src/lisp/tests_deduce_groups_integrity.c3`, and
    `src/lisp/tests_deduce_query_groups.c3`
    now pin the deduce helper-state, integrity payload, and why-result OOM
    seams directly.
  - scoped verification:
    - `rg -n "make_hashmap\\(" src/lisp/deduce_* src/lisp/unify_* -S` -> no matches
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'` -> `pass=328 fail=0`

- Closed the direct-crash raw-hashmap caller slice of the internal
  collection-constructor hardening pass:
  - `src/lisp/unify_match_helpers.c3` now routes `build_result_dict(...)`
    through checked hashmap construction and checked insertion, and fails with
    `deduce/match-out-of-memory` instead of dereferencing `dict.hashmap_val`
    from an unchecked raw constructor result.
  - `src/lisp/unify_scan_helpers.c3` now propagates that builder `ERROR`
    directly instead of consing it into a successful match-result list.
  - `src/lisp/tests_deduce_query_groups.c3` now pins the
    `deduce 'match` result-dict constructor OOM seam directly.
  - the raw-hashmap backlog is now split by real risk boundary:
    - direct crashable callers are closed
    - the remaining open lane is only the already-guarded normalization family
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'`

- Closed the raw-array constructor and AOT dict payload slice of the internal
  collection-constructor hardening pass:
  - `src/lisp/value_predicates_accessors_basic.c3` now routes `make_array(...)`
    through the checked constructor contract instead of raw `mem::malloc(...)`
    with no null checks.
  - `src/lisp/prim_collection_sort_array.c3` now treats raw array-constructor
    failure as a first-class `ERROR` return in `array(...)` and
    `list->array(...)` instead of dereferencing a partially initialized wrapper.
  - `src/lisp/aot_runtime_bridge.c3` now routes `dict_from_args(...)` through
    checked hashmap construction and checked insertion, so AOT bridge payload
    building fails closed on allocator pressure instead of constructing a
    `HASHMAP` wrapper with a null payload.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves raw
    array-constructor failures propagate cleanly through `make_array(...)`,
    `array(...)`, and `list->array(...)`.
  - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now proves
    `aot::dict_from_args(...)` returns `ERROR` when bridge-side hashmap
    construction fails under an active bridge interpreter.
  - residual constructor hardening is now narrower again:
    - remaining direct `make_hashmap(...)` caller families that still depend on
      per-callsite tag/null guards instead of the checked constructor path
