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
  - `src/lisp/jit_jit_module_setup_helpers.c3`
    now uses a checked export allocation helper, preserves existing exports
    on growth failure, and rejects oversized export capacity before byte-size
    arithmetic can wrap.
  - `src/lisp/jit_jit_compile_effects_modules.c3` and
    `src/lisp/jit_jit_module_import_setup.c3`
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

- landed the runtime helper list-materialization fail-closed follow-up:
  - `src/lisp/value_constructors.c3`
  - `src/lisp/prim_string_transform.c3`
  - `src/lisp/prim_io_file.c3`
  - `src/lisp/prim_collection_hashmap_key_helpers.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- shipped behavior:
  - one shared `make_cons_or_error(...)` helper now covers helper-owned runtime
    list builders with a deterministic nth-failure seam.
  - `string-split`, `read-lines`, and canonical `keys` / `values` list
    assembly now fail closed instead of continuing after cons-constructor
    faults.
  - direct regressions landed in:
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=162 fail=0`)
- backlog shaping after this slice:
  - close `AUDIT-RUNTIME-LIST-MATERIALIZATION-FAILCLOSED-035`
  - actionable backlog remains `0`

- landed the JIT helper arg-construction fail-closed follow-up:
  - `src/lisp/value_constructors.c3`
  - `src/lisp/jit_jit_apply_helpers.c3`
  - `src/lisp/jit_jit_apply_runtime.c3`
  - `src/lisp/jit_jit_dispatch_helpers.c3`
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
- shipped behavior:
  - one checked `make_list1_or_error(...)` helper now covers variadic rest-list
    materialization in JIT helper-owned apply paths.
  - variadic zero-fixed-arg rest binding now fails closed instead of passing a
    raw cons-constructor fault into env binding.
  - instance `ref` dispatch now routes its two-arg helper list construction
    through the shared checked helper instead of nested raw `make_cons(...)`.
  - direct regression landed in:
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
- validation status:
  - focused `jit-policy`: green (`variadic-rest-list-alloc-failure`)
- backlog shaping after this slice:
  - close `AUDIT-JIT-HELPER-ARG-CONSTRUCTION-FAILCLOSED-034`
  - actionable backlog remains `0`

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
## 2026-04-10 follow-up

- Closed scheduler completion publication drift:
  - non-error fiber results whose root promotion faults now become explicit
    scheduler root errors in both final and pending-result publication paths
  - intentional user `ERROR` results remain allowed
- Closed scheduler offload completion decoding drift:
  - invalid `OffloadResultKind` values now fail closed as scheduler errors
    instead of returning undefined pointer state
- Closed runtime effect publication drift:
  - handled raise payload construction now fails closed before handler bind
    instead of degrading to a message-only handled raise
  - handled effect / capture dispatch now reject continuation allocation
    failure instead of null-dereferencing continuation state
  - unhandled-effect diagnostic payload construction now returns
    `"runtime effect payload: out of memory"` instead of silently dropping the
    structured payload
  - bounded validation:
    - `jit-policy` filters:
      `pending-raise-payload-alloc-failure,handle-continuation-alloc-failure`
    - `memory-lifetime-smoke`
- Closed iterator source malformed-state drift:
  - `src/lisp/primitives_iter_sources.c3`
  - `src/lisp/primitives_iter_state.c3`
  - `src/lisp/primitives_iter_coroutine.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
  - `src/lisp/tests_advanced_core_unicode_groups.c3`
- shipped behavior:
  - iterator source thunks now fail closed when internal state is missing,
    null, or malformed instead of normalizing that state to successful
    exhaustion
  - `__iterator-cycle` now rejects malformed iterator tails on both active and
    reset branches
  - `__iterator-foldl` now rejects `null` next-results as malformed iterator
    pairs instead of treating them as normal completion
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=169 fail=0`)
  - focused `advanced-unicode-iterator` still shows the unrelated pre-existing
    `Void print surface` failure
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed AOT/module/deduce/collection sentinel and rollback drift:
  - `src/lisp/aot_runtime_bridge_closure.c3`
  - `src/lisp/aot_runtime_bridge_helpers.c3`
  - `src/lisp/aot_runtime_bridge_ffi.c3`
  - `src/lisp/aot_runtime_bridge_ffi_lib.c3`
  - `src/lisp/value_environment_storage.c3`
  - `src/lisp/value_environment.c3`
  - `src/lisp/value_environment_barrier.c3`
  - `src/lisp/jit_jit_module_setup_helpers.c3`
  - `src/lisp/jit_jit_module_import_setup.c3`
  - `src/lisp/macros_expansion.c3`
  - `src/lisp/value_interp_lifecycle.c3`
  - `src/lisp/deduce_db_storage_open.c3`
  - `src/lisp/deduce_db_rule_catalog_persistence.c3`
  - `src/lisp/deduce_db_rule_signature_restore.c3`
  - `src/lisp/deduce_db_handles_storage.c3`
  - `src/lisp/eval_promotion_root_clones.c3`
  - `src/lisp/value_constructors.c3`
  - `src/lisp/prim_collection_sort_array.c3`
  - `src/lisp/prim_collection_hashmap.c3`
  - `src/lisp/prim_collection_generic_set.c3`
- shipped behavior:
  - AOT bridge intern call sites and env hash/define/set paths now reject
    `INVALID_SYMBOL_ID` before hash probing or binding mutation.
  - module import/setup now rolls back failed body/path/top-level publication,
    rebuilds module hash state, skips tombstones during module hash rebuild,
    and reacquires module entries by index after nested loads can grow the
    module table.
  - deduce restore now distinguishes missing DBIs from other LMDB open errors,
    rejects invalid restored rule-catalog symbol IDs, and rolls back
    partially restored rule signatures/schemas on mid-restore failure.
  - raise payload construction, method-table root cloning, and collection
    primitives now fail closed on invalid symbols, allocation-size overflow,
    or nullable backing storage instead of dereferencing or publishing invalid
    state.
- validation status:
  - host `c3c build --warn-deprecation=no`: green
  - bounded `advanced`: green (`pass=1183 fail=0`)
  - bounded `compiler`: green (`pass=191 fail=0`)
  - bounded `deduce`: green (`pass=330 fail=0`)
  - bounded `string-type`: green (`pass=40 fail=0`)
  - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed boundary wrapper allocation drift:
  - `src/lisp/eval_boundary_scope_env.c3`
  - `src/lisp/value_constructors.c3`
  - `src/lisp/value_predicates_accessors_basic.c3`
  - `src/lisp/prim_collection_hashmap.c3`
  - `src/lisp/primitives_coroutine.c3`
  - `src/lisp/eval_init_primitives.c3`
  - `src/lisp/eval_dispatch_types.c3`
  - `src/lisp/jit_jit_module_import.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- shipped behavior:
  - boundary alloc helpers no longer register dtors on null wrapper targets
  - wrapper constructors for arrays, dictionaries, modules, coroutines,
    primitives, and FFI handles now fail closed on wrapper allocation instead
    of dereferencing a missing root/scope wrapper
  - coroutine publication now returns its freshly created `StackCtx` to the
    pool if wrapper allocation fails after context creation
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=171 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed coroutine boundary wrapper allocation drift:
  - `src/lisp/eval_promotion_copy_route_helpers.c3`
  - `src/lisp/eval_promotion_escape_leaf.c3`
  - `src/lisp/tests_memory_lifetime_finalize_groups.c3`
- shipped behavior:
  - coroutine copy-to-parent now destroys cloned context state if destination
    wrapper allocation fails instead of leaking the clone and dereferencing the
    missing wrapper
  - coroutine escape promotion now rejects escape-wrapper allocation failure
    before mutating source ownership
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=173 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed deduce row key wrapper allocation drift:
  - `src/lisp/deduce_relation_row_materialization.c3`
  - `src/lisp/tests_deduce_query_scan_groups.c3`
- shipped behavior:
  - deduce relation row materialization now fails closed if root wrapper
    allocation fails while building cached column-key symbols
- validation status:
  - bounded `deduce`: green (`pass=329 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed destination/escape wrapper allocation drift:
  - `src/lisp/eval_boundary_scope_env.c3`
  - `src/lisp/eval_boundary_commit_escape_builders.c3`
  - `src/lisp/eval_boundary_commit_escape_cons.c3`
  - `src/lisp/eval_boundary_commit_escape_wrappers.c3`
  - `src/lisp/eval_promotion_escape_leaf.c3`
  - `src/lisp/eval_promotion_escape_structured.c3`
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3`
  - `src/lisp/tests_memory_lifetime_finalize_groups.c3`
- shipped behavior:
  - destination ESCAPE builders now fail closed when wrapper publication in the
    temporary build scope cannot allocate the destination wrapper
  - generic ESCAPE promotion now rejects or fails closed on wrapper allocation
    for string/error publication and other leaf/structured wrapper
    materializers, including closures
- validation status:
  - `c3c build`: green
  - bounded `memory-lifetime-smoke`: green (`pass=176 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed root-store clone wrapper allocation drift:
  - `src/lisp/eval_promotion_root_clone_basic.c3`
  - `src/lisp/eval_promotion_root_clones.c3`
  - `src/lisp/eval_promotion_escape_leaf.c3`
  - `src/lisp/tests_memory_lifetime_root_boundary_groups.c3`
  - `src/lisp/tests_memory_lifetime_groups.c3`
- shipped behavior:
  - root-store clone helpers now route final ESCAPE wrapper publication through
    the shared checked wrapper allocator instead of raw `alloc_value_escape()`
  - failed root-store ARRAY wrapper publication now unwinds copied nested
    closure retains immediately instead of pinning them into root lifetime
- validation status:
  - `c3c build`: green
  - bounded `memory-lifetime-smoke`: green (`pass=177 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed structured ESCAPE publication fail-open drift:
  - `src/lisp/eval_promotion_escape_leaf.c3`
  - `src/lisp/eval_promotion_escape_structured.c3`
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- shipped behavior:
  - string/error ESCAPE promotion now returns a boundary error if chars or the
    ESCAPE wrapper cannot allocate, instead of reusing the original TEMP-lane
    value
  - shared ARRAY / HASHMAP / SET / METHOD_TABLE ESCAPE promotion now delays
    final wrapper publication until payload promotion succeeds, and wrapper
    allocation failure cleans up the materialized payload before returning a
    boundary error
  - repeated ARRAY child-promotion failure now proves stable PromotionContext
    memo overhead without accumulating extra ESCAPE wrapper slots
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=179 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed core constructor publication drift:
  - `src/lisp/value_constructors_core.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- shipped behavior:
  - `make_cons(...)` ESCAPE publication now uses the checked boundary allocator
    instead of the assert-only `alloc_value_escape()` path
  - `make_closure(...)` and `make_closure_no_param(...)` now allocate closure
    payload storage before publishing/registering the wrapper, so payload OOM
    fails closed with a runtime error instead of publishing partial state
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=182 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed partial primitive and opaque-payload wrapper publication drift:
  - `src/lisp/eval_promotion_escape_leaf.c3`
  - `src/lisp/value_constructors.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
- shipped behavior:
  - `PARTIAL_PRIM` ESCAPE promotion now uses the checked boundary allocator for
    final wrapper publication instead of raw assert-only ESCAPE allocation
  - primitive and FFI-handle constructors now allocate subordinate heap payloads
    before publishing/registering root/current-scope wrappers
  - repeated primitive/FFI payload allocation failures no longer consume
    wrapper slots
  - coroutine wrapper allocation-failure diagnostics no longer inspect values
    after temporary-interpreter teardown, and the stack-pool cleanup assertion now
    handles ASAN's no-reuse mode
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=185 fail=0`)
  - bounded ASAN `memory-lifetime-smoke`: green (`pass=185 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed iterator/coroutine and list/json fail-closed drift:
  - `src/lisp/primitives_iter_terminal.c3`
  - `src/lisp/primitives_coroutine.c3`
  - `src/lisp/value_predicates_accessors_basic.c3`
  - `src/lisp/prim_string_ops.c3`
  - `src/lisp/primitives_core.c3`
  - `src/lisp/json_pointer_option_helpers.c3`
  - `src/lisp/json_pointer_options.c3`
  - `src/lisp/json_emit.c3`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
  - `src/lisp/tests_runtime_data_unicode_groups.c3`
  - `src/lisp/tests_advanced_core_unicode_groups.c3`
- shipped behavior:
  - iterator constructor and terminal gates now reject malformed `ITERATOR`
    wrappers whose thunk payload is null, `ERROR`, or otherwise not callable
  - `coroutine?` now returns `nil` for zero-argument calls instead of reading
    `args[0]`, and `make_coroutine(...)` now rejects null stack contexts
  - `string->list` and `list` now fail closed on checked cons-constructor
    failure instead of publishing malformed result lists
  - JSON pointer string-key lookup now returns the string-materialization error
    instead of falling through to symbol lookup
  - JSON emit and `list->string` now reject improper list tails instead of
    silently truncating them
- validation status:
  - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)
  - bounded ASAN `memory-lifetime-smoke`: green (`pass=189 fail=0`)
  - bounded `json`: green (`pass=39 fail=0`)
  - direct eval:
    - `(list->string (cons "a" 2))` returns
      `"list->string: expected a proper list"`
    - `(json-emit (cons 1 2))` returns
      `"json-emit: expected proper list"`
    - `(json-emit 1 (cons (list 'pretty true) 2))` returns
      `"json-emit: options list must be a proper list"`
- residual split-out item:
  - `AUDIT-FORMAT-DISPLAY-TEMP-BUILDER-FAILCLOSED-037` is now a separate TODO
    for the unrelated `%s` display formatter failure surfaced by the advanced
    unicode iterator validation group.
- backlog shaping after this slice:
  - actionable backlog is `1`
- Closed `%s` display formatter target-capacity drift:
  - `src/lisp/prim_string_format_helpers.c3`
  - `src/lisp/tests_advanced_core_unicode_groups.c3`
- shipped behavior:
  - `StringVal` target-capacity calculation now uses checked overflow addition
    for `len + needed + 1` instead of comparing against `usz.max`, which this
    C3 build lowered as a signed comparison and treated normal appends as
    overflow.
  - `(format "%s" nil)` now returns `"nil"`, `(format "%s" (Void))` returns
    `"#<void>"`, and long `%s` strings that need builder growth still render.
- validation status:
  - `c3c build`: green
  - bounded `advanced` slice with
    `OMNI_ADVANCED_GROUP_FILTER=advanced-unicode-iterator`: green
    (`pass=129 fail=0`)
  - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)
  - bounded ASAN `memory-lifetime-smoke`: green (`pass=189 fail=0`)
  - direct JSON REPL:
    - `(format "%s" nil)` returns `"nil"`
    - `(format "%s" (Void))` returns `"#<void>"`
    - long `%s` string requiring builder growth returns the source text
- backlog shaping after this slice:
  - actionable backlog returns to `0`
- Closed environment/process-spawn allocation-size drift:
  - `src/lisp/value_environment_storage.c3`
  - `src/lisp/value_environment.c3`
  - `src/lisp/eval_promotion_copy_wrapper_helpers.c3`
  - `src/lisp/async_process_spawn.c3`
- shipped behavior:
  - environment hash-table capacity and load-factor calculations now reject
    overflowing arithmetic before allocation or rebuild decisions.
  - closure wrapper parameter-copy and async process argv/env pointer-table
    staging now reject overflowing buffer sizes before allocation.
- validation status:
  - host `c3c build --warn-deprecation=no`: green
  - bounded `async`: green (`pass=61 fail=0`)
  - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed JIT effect / deduce allocation-size drift:
  - `src/lisp/jit_jit_handle_signal_handle.c3`
  - `src/lisp/jit_jit_runtime_effects_signal.c3`
  - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
  - `src/lisp/deduce_rule_eval_exec_aggregate_groups.c3`
  - `src/lisp/deduce_rule_eval_exec_aggregate_helpers.c3`
  - `src/lisp/deduce_rule_ir_helpers.c3`
  - `src/lisp/deduce_rule_ir.c3`
  - `src/lisp/deduce_relation_row_materialization.c3`
  - `src/lisp/deduce_db_goal_directed_read_tracking.c3`
- shipped behavior:
  - JIT effect handle/signal paths now check clause and signal argument buffer
    sizes before allocation.
  - deduce aggregate group state, encoded tuple staging, rule IR staging,
    relation key materialization, and read-tracking buffers now reject
    overflowing allocation arithmetic before writes.
- validation status:
  - host `c3c build --warn-deprecation=no`: green
  - bounded `deduce`: green (`pass=330 fail=0`)
  - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed AOT/JIT type-dispatch signature allocation and publication drift:
  - `src/lisp/aot_type_spec_helpers.c3`
  - `src/lisp/jit_jit_closure_support.c3`
  - `src/lisp/eval_type_declarations.c3`
  - `src/lisp/value_type_registry.c3`
  - `src/lisp/eval_dispatch_match.c3`
  - `src/lisp/eval_dispatch_match_breakdown.c3`
  - `src/lisp/schema_explain_helpers.c3`
  - `src/lisp/aot_runtime_bridge_helpers.c3`
  - `src/lisp/eval_dispatch_types.c3`
  - `src/lisp/eval_init_primitives.c3`
- shipped behavior:
  - AOT/JIT method-signature staging now checks parameter/constraint counts and
    buffer sizes before publication.
  - deftype registration now rolls back a just-added type if late constructor,
    global binding, or type-value publication fails after registry insertion.
  - type-dispatch/schema/AOT temporary buffer allocation now rejects overflowing
    sizes.
  - dispatched primitive/type-symbol bootstrap now cleans up empty method-table
    payloads when root-wrapper or global binding publication fails.
- validation status:
  - host `c3c build --warn-deprecation=no`: green
  - bounded `compiler`: green (`pass=191 fail=0`)
  - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed registry/bootstrap/unicode/collection/I/O/TLS guard drift:
  - `src/lisp/value_symbol_table.c3`
  - `src/lisp/value_type_registry.c3`
  - `src/lisp/eval_type_declarations.c3`
  - `src/lisp/value_interp_init_helpers.c3`
  - `src/lisp/value_interp_state.c3`
  - `src/lisp/unicode_case_mapping.c3`
  - `src/lisp/scheduler_offload_network.c3`
  - `src/lisp/prim_collection_hashmap.c3`
  - `src/lisp/prim_io_file_helpers.c3`
  - `src/lisp/prim_io_console_helpers.c3`
- shipped behavior:
  - symbol/type registry insertion now rejects exhausted ID spaces before
    narrowing counts, symbol probing skips stale out-of-range indices, and type
    rollback rebuilds the hash table after removing a just-added failed entry.
  - interpreter bootstrap symbols now fail fast on intern failure instead of
    publishing invalid sentinel IDs into runtime state.
  - unicode case conversion rejects strings too large for `utf8proc`'s `long`
    length API before narrowing.
  - TLS offload cleanup calls `br_sslio_close(...)` only when the SSL I/O
    context was initialized.
  - Dictionary capacity derivation now uses checked arithmetic, and hashmap
    payload allocation now precedes root-wrapper publication.
  - read-file rejects oversized `ulong` file sizes before `usz` narrowing, and
    console emit reports render/write failures as typed I/O errors.
- validation status:
  - host `c3c build --warn-deprecation=no`: green
  - bounded `compiler`: green (`pass=191 fail=0`)
  - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)
  - bounded `async`: green (`pass=61 fail=0`)
  - bounded `scheduler`: green (`pass=111 fail=0`)
  - bounded `advanced` unicode iterator: green (`pass=129 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed numeric/macro/parser/data-format/async guard drift:
  - `src/lisp/prim_math.c3`
  - `src/lisp/prim_math_core.c3`
  - `src/lisp/prim_math_arithmetic.c3`
  - `src/lisp/prim_string_format_directives.c3`
  - `src/lisp/prim_string_ops.c3`
  - `src/lisp/prim_string_transform.c3`
  - `src/lisp/prim_string_convert.c3`
  - `src/lisp/primitives_data_formats_csv.c3`
  - `src/lisp/primitives_data_formats_toml_options.c3`
  - `src/lisp/macros_template_expansion.c3`
  - `src/lisp/macros_expr_conversion_form_builders.c3`
  - `src/lisp/parser_parser.c3`
  - `src/lisp/parser_application_placeholders.c3`
  - `src/lisp/async_tcp_transport_helpers.c3`
  - `src/lisp/async_tcp_transport_core.c3`
  - `src/lisp/async_process_signal_dns.c3`
  - `src/lisp/async_tcp_transport_connect.c3`
  - `src/lisp/async_udp_pipe.c3`
  - `src/lisp/async_tcp_transport_listen.c3`
  - `src/lisp/scheduler_state_types.c3`
  - `src/lisp/scheduler_wakeup_queue.c3`
  - `src/lisp/tests_advanced_core_unicode_groups.c3`
  - `src/lisp/tests_advanced_stdlib_numeric_groups.c3`
  - `src/lisp/tests_runtime_data_unicode_groups.c3`
- shipped behavior:
  - RNG primitives now check and complete `getrandom` reads before using the
    stack buffer.
  - signed integer arithmetic and format binary rendering now reject
    `long.min`/overflow edges instead of executing undefined signed operations.
  - format width/precision parsing now rejects overflowing `int` accumulation.
  - string/data-format helpers now preserve multibyte list elements and reject
    improper list/option tails.
  - macro/parser paths now reject invalid symbol IDs and checked macro AST
    block/call allocation failures.
  - async TCP read rejects non-positive max byte counts, resumed-before-
    completion branches clean up pending state, and writable wakeup coalescing
    has separate telemetry.
- validation status:
  - host `c3c build --warn-deprecation=no`: green
  - bounded `arithmetic-comparison`: green (`pass=45 fail=0`)
  - bounded `string-type`: green (`pass=40 fail=0`)
  - bounded `data-format`: green (`pass=62 fail=0`)
  - bounded `async`: green (`pass=61 fail=0`)
  - bounded `scheduler`: green (`pass=111 fail=0`)
  - bounded `advanced-stdlib-numeric-string-predicate-format`: green
    (`pass=59 fail=0`)
  - bounded `advanced-unicode-iterator`: green (`pass=130 fail=0`)
  - bounded `advanced-macro-hygiene`: green (`pass=82 fail=0`)
- backlog shaping after this slice:
  - actionable backlog remains `0`
- Closed slice `AUDIT-AOT-PRINT-DEDUCE-SENTINELS-059`:
  - AOT generated type/type-spec and match/bridge helpers now use checked
    symbol conversion before publishing symbol IDs into type metadata,
    method signatures, match constructors, dictionary key lookup, or effect
    explain payloads.
  - Compiled list helper indexing now rejects negative indexes before `usz`
    conversion.
  - Direct and buffered value printers now handle nullable dictionary/set
    backing storage, and buffered printing rejects null/zero-capacity output
    buffers before writing.
  - Constructor type constraint diagnostics now use guarded type-registry
    lookups, while instance type inference rejects out-of-range type IDs.
  - Deduce tuple persistence now encodes full 32-bit `SymbolId` values and
    rejects invalid/out-of-range decoded IDs.
  - Deduce materialized metadata deletion now propagates real relation-meta DBI
    open errors and treats only `MDB_NOTFOUND` as a no-op delete.
  - Deduce path/name copy helpers now guard `+1` and suffix concatenation
    allocation sizes.
  - Deduce relation and rule install paths now roll back newly appended
    in-memory schemas/rule signatures when later fallible metadata, handle, or
    persistence steps fail.
  - Validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `compiler`: `pass=191 fail=0`
    - bounded `deduce`: `pass=330 fail=0`
    - bounded `advanced`: `pass=1183 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`
- Closed slice `AUDIT-PARSER-JIT-ASYNC-BOUNDARY-060`:
  - Parser surface interning now uses checked fail-closed paths for import
    string targets, path segments, type annotations and value-literal
    annotations, collection literal constructors, datum/quasiquote template
    underscore symbols, relation definition helper rewrites, explain selectors,
    and special-form comparisons.
  - Compiler effect-wrapper lambda scanning now checks synthetic AST arena
    allocation and wrapper symbols before publishing rewritten reset/shift/
    handle bodies.
  - Primitive variable hash initialization now rejects invalid symbol keys and
    exposes initialization failure to `Compiler.init`.
  - Compiler integer emission now prints signed and unsigned values without
    negating `long.min` or narrowing `usz` through `long`.
  - Macro splice append now fails on improper splice lists and recursion-limit
    exhaustion rather than silently truncating.
  - Runtime boundary promotion/destination-copy helpers now guard `len + 1`
    string/error allocation arithmetic, and boundary policy `usz` parsing now
    rejects numeric overflow.
  - JIT continuation yield-failure paths restore saved interpreter state before
    cleanup/return, pending-raise handler staging delays state clearing until
    payload/list/env construction succeeds, and runtime handle initialization
    rejects null non-empty clause arrays.
  - TLS offload yield-error paths now close pending offloads, async TCP/UDP
    ports and signal numbers are range-checked before `int` narrowing, and
    file-read close failure now fails the read operation.
  - Validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `compiler`: `pass=191 fail=0`
    - bounded `async`: `pass=61 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`
    - bounded `advanced` macro hygiene group: `pass=82 fail=0`
  - Backlog shaping after this slice:
    - actionable backlog remains `0`

- Closed slice `AUDIT-SCHEMA-LIFETIME-WIDTH-061`:
  - schema explain and deduce payload helpers now fail closed on failed symbol
    interning before dictionary publication or symbol payload construction.
  - deduce integrity payload builders now propagate the concrete `Value*` error
    from `explain_dict_set*` calls instead of returning `null`, and list payload
    builders stop on cons allocation errors.
  - deduce materialize rejects failed `"manual"` refresh-policy interning before
    persisting relation/schema state.
  - primitive name matching now guards null primitive backing pointers and
    overlong expected names before reading fixed-size primitive name storage.
  - checked array construction now stages payload allocation before publishing
    the root wrapper, matching the checked hashmap constructor discipline.
  - closure escape promotion now releases retained/detached environment scopes
    if final wrapper allocation fails.
  - external-boundary integer narrowing now has explicit guards for `exit`,
    `TimePoint`, Unicode codepoints, `fs-open`, `fs-stat`, `tcp-listen`, JSON
    pointer symbol fallback, process-handle lookup, and zlib original-size
    expansion.
  - Validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal+ASAN `data-format`: `pass=64 fail=0`
    - bounded normal+ASAN `unicode`: `pass=27 fail=0`
    - bounded normal+ASAN `compression`: `pass=27 fail=0`
    - bounded normal+ASAN `async`: `pass=65 fail=0`
    - bounded normal+ASAN `compiler`: `pass=194 fail=0`
    - bounded normal+ASAN `memory-lifetime-smoke`: `pass=190 fail=0`
    - bounded normal `advanced`: `pass=1185 fail=0`
    - bounded ASAN `advanced`: `pass=1172 fail=0`
    - bounded normal+ASAN `deduce`: `pass=330 fail=0`
  - Residual split:
    - `AUDIT-FTXUI-SMOKE-SEGFAULT-062` tracks the unrelated FTXUI
      `smoke.omni` SIGSEGV observed after targeted Lisp validation passed.

- Closed slice `AUDIT-FTXUI-SMOKE-SEGFAULT-062`:
  - The FTXUI `smoke.omni` crash was isolated to runtime boundary provenance,
    not to the FTXUI lowering/backend path: nested effect payload graphs could
    recurse through alias-safety classification until stack overflow.
  - `src/lisp/eval_boundary_provenance.c3` now uses a bounded iterative
    alias-safety worklist with visited tracking for nested arrays, dicts, sets,
    method tables, partials, iterators, and cons payloads.
  - Scalar leaves are skipped before consuming alias worklist/visited capacity,
    so wide scalar-only payloads no longer trip the fail-closed graph cap.
  - `src/lisp/tests_memory_lifetime_boundary_state_groups.c3` adds the minimal
    nested effect-payload regression plus a wide scalar payload regression, and
    `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` keeps both in the
    bounded smoke lane.
  - The FTXUI sidecar hardening slice also landed:
    `src/lisp/prim_ui_ftxui.c3` guards child pointer-array sizing,
    `src/lisp/prim_ui_ftxui_helpers.c3` guards helper-array growth and graph
    series allocation arithmetic, `src/lisp/prim_ui_ftxui_lowering.c3` rejects
    oversized menu selected-index counts before `int` narrowing, and
    `csrc/ftxui_shim.cpp` orders `keep_alive` before `component`, rejects
    nonzero child counts with null child arrays, checks table `rows * cols`
    overflow, and rejects table selector indexes outside FTXUI `int` range.
  - Closed residual slice `AUDIT-FTXUI-C-ABI-EXCEPTION-SAFETY-063`:
    status-returning FTXUI C ABI entrypoints now use a shared fail-closed
    exception guard around backend work, and deferred graph/render/event/quit
    callbacks catch callback exceptions locally before they can cross the C ABI
    or FTXUI callback frame.
  - Closed residual slice `AUDIT-FTXUI-SCREEN-LIFETIME-064`:
    `omni_ftxui_component_wrap_quit_keys(...)` retains shared ownership of the
    screen loop object in the wrapped component keep-alive list instead of
    capturing a raw screen handle.
  - Validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=192 fail=0`
  - Backlog shaping after this slice:
    - residual boundary alias visited-set performance work split to
      `AUDIT-BOUNDARY-ALIAS-WORKLIST-PERF-065`.
  - Follow-up on 2026-04-11:
    - added a shared composite cycle payload regression to the
      `memory-lifetime-smoke` lane so repeated composite aliases do not consume
      visit capacity while true graph cycles are still tracked.
    - closed `AUDIT-BOUNDARY-ALIAS-WORKLIST-PERF-065` by fronting the
      authoritative linear `seen` list with a small bounded `ushort`
      index-table accelerator for repeated composite alias checks.
    - the accelerator saturates into the existing linear scan to preserve the
      no-false-negative contract and fail-closed graph caps.
    - scalar/non-graph roots now return before entering the large traversal
      frame, and the large-array walker sits behind a small stack-headroom
      wrapper that fails closed to copy-required if the current stack context is
      too shallow.
    - larger local pointer/index-table attempts regressed FTXUI smoke with a
      `smoke.omni` boundary resolve stack overflow and were not kept; the
      landed table stays deliberately small to fit the effect/FTXUI stack
      budget.
    - validation after the fast-set and regression-test addition:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/run_ftxui_smoke.sh`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=193 fail=0`
  - Closed follow-up `AUDIT-BOUNDARY-DESTINATION-BUILD-SPLICE-FACADE-067`:
    - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` no longer
      calls `main::scope_splice_escapes(...)` directly.
    - the narrow low-level splice now lives behind
      `boundary_destination_build_scope_splice(...)` in the existing
      allowlisted boundary builder implementation file.
    - destination `cons`, `partial`, `iterator`, and `error` builders keep the
      previous build-scope commit behavior, avoiding source-wrapper fallback
      through nested effect payload return boundaries.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/check_boundary_facade_usage.sh`: green
      - host `scripts/check_boundary_change_policy.sh`: green
      - host `scripts/check_status_consistency.sh`: green
      - host `git diff --check`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=193 fail=0`
  - Closed follow-up `AUDIT-BOUNDARY-INSTANCE-MODULE-ALIAS-GRAPH-068`:
    - `src/lisp/eval_boundary_provenance.c3` now treats `INSTANCE` and
      `MODULE` as graph-bearing alias payloads, matching committed-root graph
      audit coverage.
    - the rare path uses a heap-backed value/env reachability scan instead of
      adding large local arrays to the hot alias walker stack frame.
    - the scan rejects reuse when by-value instance fields or module/env
      bindings still reach the releasing scope.
    - root-persistent env boxes are still traversed for parent/binding edges;
      they are only excluded from direct temp-frame ownership checks.
    - `src/lisp/tests_memory_lifetime_boundary_state_groups.c3` now covers an
      instance field graph retaining a releasing-scope array payload.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/check_boundary_facade_usage.sh`: green
      - host `scripts/check_status_consistency.sh`: green
      - host `git diff --check`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=194 fail=0`
  - Closed follow-up `AUDIT-JIT-MODULE-VALUE-GROWTH-069`:
    - `src/lisp/value_predicates_accessors_basic.c3` now snapshots module
      descriptors into root-scope storage for first-class `MODULE` values, so
      module values no longer point into the reallocating interpreter module
      table.
    - `src/lisp/eval_path.c3` now fails closed on invalid module descriptors
      before reading exports/envs.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now forces
      module table growth after creating a first-class module value and
      verifies exported path access still works under normal and ASAN smoke.
  - Closed follow-up `AUDIT-JIT-MUTATION-PROMOTION-NULL-GUARDS-070`:
    - `src/lisp/jit_jit_closure_let_set_helpers.c3` now rejects null
      cons-field promotion and null instance-field boundary-copy results before
      mutating storage.
    - `src/lisp/jit_jit_define_method_table.c3` now rejects null typed method
      implementations and null global define promotion results before mutating
      method-table entries or fallbacks.
    - `src/lisp/aot_type_definitions.c3` mirrors the typed-method promotion
      null guard for AOT.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/check_boundary_facade_usage.sh`: green
      - host `scripts/check_boundary_change_policy.sh`: green
      - host `scripts/check_status_consistency.sh`: green
      - host `git diff --check`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=196 fail=0`
  - Closed follow-up `AUDIT-TCO-ENV-COPY-FAIL-CLOSED-071`:
    - `src/lisp/jit_jit_eval_scope_copy.c3` now uses checked boundary-copy
      results while copying TCO env-frame bindings and rejects copy faults,
      null results, and copied `ERROR` values before binding them into a copied
      env frame.
    - Root-persistent env-box parent rewrites now fail closed if the required
      parent-chain copy fails.
  - Closed follow-up `AUDIT-TCO-RECYCLE-HOOK-FAIL-CLOSED-072`:
    - `src/lisp/runtime_backend_hooks.c3` now preserves `*env_io`, releases the
      fresh recycle scope, restores the original call scope, and returns an
      explicit error when TCO env-chain copy fails.
    - Active defer retargeting is restored back to the original call scope on
      that failure path.
    - `src/lisp/tests_memory_lifetime_tco_budget_groups.c3` now covers both the
      direct env-copy failure and recycle-hook state-restore failure paths.
    - `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` wires both
      regressions into the bounded smoke lane.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/check_boundary_facade_usage.sh`: green
      - host `scripts/check_boundary_change_policy.sh`: green
      - host `scripts/check_status_consistency.sh`: green
      - host `git diff --check`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=198 fail=0`
  - Closed follow-up `AUDIT-JIT-MUTABLE-LOCAL-NULL-BOX-073`:
    - `src/lisp/jit_jit_apply_multi_prims_tail.c3` now makes
      `jit_env_lookup_local(...)` reject null envs and missing bindings with an
      explicit `jit: missing mutable local binding` error.
    - `jit_env_reparent(...)` now returns the effective env and lets compiled
      capture setup continue with the helper result when the original env box is
      null.
    - `src/lisp/jit_jit_compile_expr_basic.c3` and
      `src/lisp/jit_jit_emit_helpers.c3` now consume those checked helper
      contracts.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3` covers both helper
      contracts in `jit-policy`.
  - Closed follow-up `AUDIT-RAISE-PAYLOAD-NESTED-PENDING-074`:
    - `src/lisp/value_constructors.c3` now builds handled raise payload
      dictionaries through the non-raising hashmap allocation helper instead
      of the raising dictionary constructor path.
    - Payload dictionary allocation failure no longer publishes a stale nested
      `raise_pending` value that the surrounding handler can accidentally
      consume as a successful handled raise.
  - Closed follow-up `AUDIT-DISPATCH-PAYLOAD-NO-PRESEED-075`:
    - `src/lisp/prim_collection_hashmap.c3` now provides
      `make_hashmap_no_raise(...)` for optional dictionary payload construction
      that must not publish allocation failures through the raise channel.
    - `src/lisp/eval_dispatch_error_payloads.c3` now uses that helper so
      dispatch diagnostic payload allocation failure cannot pre-seed
      `raise_pending` before the intended dispatch/type error is raised.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3` covers the active
      handler case directly.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/check_boundary_facade_usage.sh`: green
      - host `scripts/check_boundary_change_policy.sh`: green
      - host `scripts/check_status_consistency.sh`: green
      - host `git diff --check`: green
      - bounded normal `jit-policy` with FTXUI smoke enabled:
        `pass=51 fail=0`
      - bounded ASAN `jit-policy`: `pass=50 fail=0`
  - Closed follow-up `AUDIT-CTOR-PAYLOAD-NO-PRESEED-076`:
    - `src/lisp/primitives_meta_types_ctor_helpers.c3` now uses
      `make_hashmap_no_raise(...)` for optional constructor mismatch payloads
      and checks payload key interning before symbol construction.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` covers the
      active handler case directly and verifies constructor payload allocation
      failure leaves pending raise state clear.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/check_boundary_facade_usage.sh`: green
      - host `scripts/check_boundary_change_policy.sh`: green
      - host `scripts/check_status_consistency.sh`: green
      - host `git diff --check`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=199 fail=0`
  - Closed follow-up `AUDIT-DEDUCE-INTEGRITY-PAYLOAD-NO-PRESEED-077`:
    - `src/lisp/deduce_relation_ops_validation_payload.c3` now uses
      `make_hashmap_no_raise(...)` for optional integrity/check-context
      diagnostic payloads and routes payload field insertion through local
      no-raise setters.
    - `src/lisp/deduce_rule_eval_exec_component_state.c3` now uses the same
      non-raising helper for optional iteration-limit diagnostic payloads
      returned before the later iteration-limit raise.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` covers the
      active handler case directly and verifies deduce integrity and
      iteration-limit payload allocation failures leave pending raise state
      clear.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=200 fail=0`
  - Closed follow-up `AUDIT-DEDUCE-GOAL-DIRECTED-PAYLOAD-NO-PRESEED-078`:
    - `src/lisp/deduce_rule_eval_analyze_setup.c3` now uses
      `make_hashmap_no_raise(...)` and local checked insertion for
      goal-directed selector analysis diagnostic payload dictionaries before
      publishing the existing `deduce/analyze-out-of-memory` fallback.
    - `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
      now uses the same non-raising payload construction path for
      goal-directed selector and relation surface diagnostics before
      publishing the existing `deduce/out-of-memory` fallback.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=200 fail=0`
  - Closed follow-up `AUDIT-DEDUCE-GOAL-DIRECTED-INTERN-GUARDS-079`:
    - `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3` now
      routes goal-directed blocker and shape symbols through a checked helper
      that rejects `INVALID_SYMBOL_ID` before constructing payload symbols.
    - `src/lisp/deduce_rule_ops_explain_snapshot.c3` now uses that helper for
      goal-directed shape and execution-path payload values.
    - `src/lisp/deduce_why_result_path_payload.c3` now rejects invalid lookup
      key interning before constructing a temporary dictionary key symbol.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - bounded normal+ASAN `deduce` slice: `pass=330 fail=0`
  - Closed follow-up `AUDIT-RUNTIME-RESULT-KEY-INTERN-GUARDS-080`:
    - `src/lisp/eval_dispatch_error_payloads.c3` now rejects invalid payload
      keys and invalid symbol payload values in the optional dispatch payload
      setter.
    - `src/lisp/async_process_spawn.c3` now checks process-spawn result key
      interning before constructing dictionary key symbols and closes spawned
      resources on failure.
    - `src/lisp/http_url_response.c3` now checks HTTP response-field key
      interning before publishing response payload dictionaries.
    - `src/lisp/prim_ui_ftxui_helpers.c3` now checks UI dictionary lookup key
      interning before probing.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - bounded normal+ASAN `async` slice with FTXUI smoke enabled:
        `pass=65 fail=0`
      - bounded normal `jit-policy`: `pass=51 fail=0`
      - bounded ASAN `jit-policy`: `pass=50 fail=0`

  - Closed follow-up `AUDIT-RUNTIME-INTERN-RAISEPAYLOAD-GUARDS-081`:
    - `src/lisp/eval_init_primitive_registration.c3` now rejects failed `nil`
      symbol interning before defining the language constant.
    - `src/lisp/jit_jit_closure_runtime.c3` now treats failed promise env-tag
      interning as a non-match instead of probing environments with invalid
      symbols.
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now builds
      unhandled-effect raise payload dictionaries through the non-raising
      hashmap helper and checks payload-key interning before publication.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - bounded normal `jit-policy`: `pass=51 fail=0`
      - bounded ASAN `jit-policy`: `pass=50 fail=0`
      - bounded normal `advanced`: `pass=1185 fail=0`
      - bounded ASAN `advanced`: `pass=1172 fail=0`

  - Closed follow-up `AUDIT-CONSTRUCTOR-DISPATCH-SURFACE-083`:
    - public conversion aliases `number->string`, `symbol->string`,
      `string->symbol`, `exact->inexact`, and `inexact->exact` were removed
      in favor of canonical callable constructors `String`, `Symbol`,
      `Double`, and `Integer`.
    - `set-size` and `set->list` were removed from public primitive
      registration and compiler primitive maps; `length` now covers set
      cardinality and `List(Set ...)` materializes deterministic canonical set
      element order.
    - fast-dev-only constructor aliases `Int`/`Bool` and duplicate
      `filesystem-*` primitive aliases were removed.
    - schema validation now uses `array-of` instead of `vector-of`.
    - `lib/immer.omni` now exposes `persistent-array`,
      `persistent-dictionary`, and `persistent-set` names instead of
      `vector`, `hash-map`, and `hash-set`; the broken generic
      `count`/`conj`/`into` facade was removed because current opaque FFI
      handles have no defined persistent collection predicates.
    - residual open items were split into explicit TODO lanes for:
      - list/string constructor semantics,
      - string-to-number parse/coercion naming,
      - lowercase `list` helper policy,
      - `fs-*` versus `filesystem-*` naming,
      - typed persistent Immer wrappers or explicit non-generic API
        (later superseded by retiring the optional Immer bridge entirely),
      - repeated bounded `http` and `pika` slice failures.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - bounded `string-type` slice: `pass=40 fail=0`
      - bounded `advanced` slice: `pass=1189 fail=0`
      - bounded `compiler` slice: `pass=194 fail=0`
      - quick eval confirmed `set-size`/`set->list` are unbound,
        `length`/`List(Set ...)` behavior is correct, and `array-of` schema
        validation works.

- Closed slice `AUDIT-OFFLOAD-WIDTH-GUARDS-066`:
  - `src/lisp/scheduler_offload_network.c3` now rejects listener file
    descriptors outside `int` range before calling `tcp_accept_fd(...)`.
  - `src/lisp/eval_repl_server_state.c3` now formats REPL session IDs from a
    guarded `long` value instead of narrowing `next_session_id` through `int`.
  - `src/lisp/scheduler_offload_ops.c3` now formats atomic temp-path
    `unique_id` suffixes from a guarded `long` value instead of truncating to
    `uint`.
  - `src/lisp/scheduler_state_support_types.c3` now asserts the current
    `OffloadWork` pointer-through-`long` payload width contract at compile
    time.
  - Validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green
    - bounded normal+ASAN `async`: `pass=65 fail=0`
