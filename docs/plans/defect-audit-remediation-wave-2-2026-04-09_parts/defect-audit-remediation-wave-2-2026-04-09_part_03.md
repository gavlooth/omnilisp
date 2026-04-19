# defect-audit-remediation-wave-2-2026-04-09 Part 03

Source: `docs/plans/defect-audit-remediation-wave-2-2026-04-09.md`

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
