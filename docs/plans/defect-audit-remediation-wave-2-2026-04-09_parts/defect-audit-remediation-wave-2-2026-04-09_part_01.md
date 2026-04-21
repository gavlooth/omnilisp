# defect-audit-remediation-wave-2-2026-04-09 Part 01

Source: `docs/plans/defect-audit-remediation-wave-2-2026-04-09.md`

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
  - `src/lisp/jit_apply_multi_prims.c3` now uses the canonical
    under-arity format for non-tail fixed/variadic multi-arg closure apply,
    and `src/lisp/tests_compiler_core_groups_fail_closed.c3` now asserts
    prelude-stripped parser coordinates through the compiler pipeline.
  - `src/lisp/eval_promotion_root_clones.c3` and
    `src/lisp/jit_closure_support.c3` now expose a narrow
    method-table-abort cleanup seam, and
    `src/lisp/tests_memory_lifetime_boundary_groups.c3` proves both
    copy-to-parent and escape-promotion partial-cleanup paths reclaim copied
    heap signatures on abort.
  - `TODO.md` is back to zero live items for this audit wave.

Post-wave follow-up (2026-04-11):

- The runtime promotion allocation-staging lane is now also closed:
  - `src/lisp/eval_promotion_copy_route_helpers.c3`,
    `src/lisp/eval_promotion_escape_structured.c3`, and
    `src/lisp/eval_promotion_root_clone_basic.c3`
    now reject overflowing array/hashmap/method-table/signature/closure
    allocation sizes before boundary copy, ESCAPE promotion, or root clone
    publication.
  - `src/lisp/eval_promotion_escape_structured.c3` now resets staged method
    signatures on dependent allocation failure and delays closure result
    wrapper publication until fallible clone/env work succeeds.
  - `src/lisp/eval_env_copy_frame_helpers.c3` now allocates non-inline binding
    storage before publishing the copied frame.
  - `src/lisp/eval_pattern_matching.c3` now rejects overflowing sequence
    element collection buffers.
  - validation:
    - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- The deduce SCC allocation-bound lane is now also closed:
  - `src/lisp/deduce_rule_eval_scc_plan.c3` and
    `src/lisp/deduce_rule_eval_validation.c3` now guard square matrix and
    fixpoint relaxation sizing.
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3`,
    `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates_impl.c3`,
    and `src/lisp/deduce_rule_eval_exec_component_delta_restore.c3`
    now guard proof-key, aggregate batch, and decoded delta-entry allocation
    sizes.
  - `src/lisp/deduce_schema_query_input_shape.c3`,
    `src/lisp/deduce_schema_query_input_roles.c3`, and
    `src/lisp/deduce_schema_query_input_constraints.c3` now reject
    overflowing `count + 1` relation capacity requests.
  - validation:
    - bounded deduce slice: `pass=330 fail=0`

- The AOT/FFI allocation-staging lane is now also closed:
  - `src/lisp/aot_type_definitions.c3` now initializes cleanup-owned type
    field and union variant state before later fallible allocation can trigger
    deferred cleanup, and checks AOT allocation sizes.
  - `src/lisp/eval_ffi_bound_call.c3` now rejects unsupported/narrowing
    runtime FFI argument counts and overflowing libffi staging buffers.
  - validation:
    - bounded compiler slice: `pass=191 fail=0`
    - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- The parser AST array allocation-helper lane is now also closed:
  - `src/lisp/parser_parser.c3` now centralizes checked parser AST array
    allocation in `Parser.alloc_ast_array_bytes(...)`.
  - Dynamic parser AST array allocations for calls, relation definitions,
    literals, patterns, type annotations, modules, lambda params, named lets,
    paths, macro clauses, blocks, and pipe rewrites now route through the
    checked helper.
  - `src/lisp/parser_set_pipe_helpers.c3` now rejects overflowing
    `arg_count + 1` before growing pipe call arguments.
  - validation:
    - bounded compiler slice: `pass=191 fail=0`
    - bounded deduce slice: `pass=330 fail=0`

- The parser AST argument size-guard lane is now also closed:
  - `src/lisp/parser_define_relation_attr_helpers.c3`
    now checks relation role/count arithmetic and `Expr*` allocation sizing
    before building `__define-relation` call arguments.
  - `src/lisp/parser_application_helpers.c3`
    now checks generic call argument `Expr*` allocation sizing before copying
    argument pointers.
  - `src/lisp/parser_ffi_helpers.c3`
    now rejects overflowing FFI parameter counts before `+ 1` capacity checks.
  - `src/lisp/parser_import_helpers_specs.c3`
    now rejects overflowing selective-import counts before `+ 1` capacity
    checks.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded compiler slice: `pass=191 fail=0`
    - bounded deduce slice: `pass=330 fail=0`

- The shared/async string size-guard lane is now also closed:
  - `src/lisp/scheduler_shared_handles_blob.c3`
    now rejects max-sized blob copies before `len + 1` allocation.
  - `src/lisp/async_process_signal_handles.c3`
    now rejects max-sized C-string copies, cons sequence count overflow, and
    argv/env `char*` table byte-size overflow before allocation.
  - `src/lisp/tls_offload_connect.c3`
    now rejects max-sized TLS duplicate strings before `len + 1` allocation.
  - `src/lisp/prim_io_fs_handles.c3`
    now rejects invalid/overflowing filesystem result array growth before
    replacing array backing storage.
  - `src/lisp/scheduler_offload_network.c3`
    now rejects max-sized compression bounds before allocating the output
    scratch buffer.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded async slice: `pass=61 fail=0`
    - bounded scheduler slice: `pass=111 fail=0`

Post-wave follow-up (2026-04-10):

- The async file I/O size-guard lane is now also closed:
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

- The scheduler offload nil-completion projection lane is now also closed:
  - `src/lisp/scheduler_wakeup_io.c3`
    now returns directly from each offload completion-kind projection,
    including `OFFLOAD_RES_NIL`, so async read-file/read-lines missing-path
    cases remap nil offload results to I/O payload codes instead of leaking
    `scheduler/offload-invalid-completion-kind`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded async slice: `pass=61 fail=0`

- The I/O/string-buffer growth hardening lane is now also closed:
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

- The deduce direct allocation/schema mutation hardening lane is now also
  closed:
  - `src/lisp/deduce_db_handles_mutation.c3`
    now guards rule-signature count increments, offset/count range checks, and
    direct metadata array allocation sizes before copying.
  - `src/lisp/deduce_db_relation_schema_init.c3`,
    `src/lisp/deduce_db_handles.c3`, and
    `src/lisp/deduce_db_handles_register.c3`
    now reject overflowing schema/index sizing before allocation or schema
    count publication.
  - `src/lisp/deduce_db_handles_mutation_txn.c3`
    now increments transaction insert counts only after tuple delta append
    succeeds.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded deduce slice: `pass=330 fail=0`

- The core registry/table growth hardening lane is now also closed:
  - `src/lisp/value_symbol_table.c3` and
    `src/lisp/value_type_registry.c3`
    now fail closed when init/grow replacement allocations are unavailable,
    and reject overflowed table/hash byte-size arithmetic before allocation.
  - `src/lisp/value_interp_init_helpers.c3`
    now checks macro/module init table/hash size multiplication before
    allocation.
  - `src/lisp/eval_pattern_match_support.c3`
    now guards `gensym` and match-binding growth loops against doubling and
    allocation-byte overflow before replacing backing arrays.
  - `src/lisp/prim_collection_hashmap.c3` and
    `src/lisp/prim_collection_sort_array.c3`
    now enforce valid grow preconditions and reject overflowing grow
    arithmetic before mutating live hashmap/array backing storage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded allocator-validation slice: `pass=1 fail=0`
    - bounded advanced collections/module group: `pass=134 fail=0`
    - bounded advanced type-dispatch/mutation-chain group: `pass=236 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- The iterator/coroutine malformed-state normalization lane is now also closed:
  - `src/lisp/primitives_iter_state.c3`
    now rejects missing list/array source thunk state directly, and
    `iterator_make_pair_or_propagate(...)` no longer maps a null recursive
    tail to successful completion.
  - `src/lisp/primitives_iter_coroutine.c3`
    now rejects malformed thunk arg state for `map`, `filter`, `take`, and
    `zip` instead of returning `nil`, and `filter` no longer treats a
    non-iterator internal state transition as valid exhaustion.
  - `src/lisp/primitives_iter_terminal.c3`
    now rejects null iterator pairs from internal thunk dispatch instead of
    truncating `collect` / `to-array`.
  - `src/lisp/primitives_coroutine_resume.c3`
    now rejects missing yielded values and missing completed results instead of
    fabricating successful `nil` resume results.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` and
    `src/lisp/tests_memory_lifetime_groups.c3`
    now pin the malformed-thunk and malformed-resume state directly.
  - validation:
    - `c3c build`
    - bounded memory smoke:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

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
  - `src/lisp/jit_runtime_effects_handle.c3`
    now rejects fallback raise message-string and arg-pair constructor failure
    before handler call-through.
  - `src/lisp/jit_handle_signal_handle.c3`
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
    now proves the float64-failure seam wakes the blocked waiter, clears the
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

- The scheduler offload missing-completion lane is now also closed:
  - `src/lisp/scheduler_offload_worker.c3`
    now still publishes or directly handles non-task offload readiness when
    both the callback completion and fallback alloc-failure completion are
    unavailable.
  - `src/lisp/scheduler_wakeup_io.c3`
    now consumes active completed-null offload slots as a typed
    `"offload: missing completion"` error and clears the pending slot.
  - `src/lisp/tests_scheduler_boundary_offload_payload_groups.c3`
    now pins the completed-null wakeup path and slot cleanup.
  - validation:
    - `c3c build`
    - bounded scheduler slice: `pass=111 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- The deduce/JIT capacity-growth byte-overflow lane is now also closed:
  - deduce relation, aggregate, delta, query-demand, transaction,
    dirty-predicate, rule-signature, relation-schema, and persisted-rule
    catalog growth helpers now guard allocation byte counts before
    `sizeof * new_cap` arithmetic can wrap.
  - JIT module source-dir vector growth now rejects oversized capacities and
    source path length increments before allocation-size arithmetic can wrap.
  - `src/lisp/tests_deduce_groups_parallel.c3` now pins the oversized-capacity
    fail-closed path under the deduce parallel group.
  - validation:
    - `c3c build`
    - bounded deduce parallel group: `pass=6 fail=0`
    - bounded scheduler slice: `pass=111 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- The runtime module export growth fail-closed lane is now also closed:
  - `src/lisp/jit_module_setup_helpers.c3`
    now uses a checked export allocation helper, preserves existing exports
    on growth failure, and rejects oversized export capacity before byte-size
    arithmetic can wrap.
  - `src/lisp/jit_compile_effects_modules.c3` and
    `src/lisp/jit_module_import_setup.c3`
    now propagate export-table growth failure during re-export and implicit
    module export paths.
  - `src/lisp/tests_advanced_stdlib_module_groups.c3`
    now pins forced module export growth allocation failure.
  - validation:
    - `c3c build`
    - bounded advanced collections/module group: `pass=134 fail=0`

- The next overflow-hardening batch is now also closed:
  - persisted deduce rule signature/catalog record encode/restore paths now
    use checked size and cursor helpers instead of raw `sizeof * count` and
    `cursor + len` arithmetic.
  - AST arena alignment/chunk accounting and parser import/module/export
    growth now reject allocation-size overflow before state mutation.
  - JIT arg-buffer, effect-handler, handle-state copy, interpreter table,
    env binding, and method-table growth paths now guard doubling and byte-size
    multiplication before allocation/mutation.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded deduce slice: `pass=330 fail=0`
    - bounded allocator-validation slice: `pass=1 fail=0`
    - bounded advanced collections/module group: `pass=134 fail=0`
    - bounded advanced effect-continuation group: `pass=56 fail=0`
    - bounded advanced runtime-control group: `pass=22 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

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
    `src/lisp/jit_handle_signal_helpers_runtime_effects.c3`, and
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
