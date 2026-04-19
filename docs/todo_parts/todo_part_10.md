# Active TODO Index Part 10

Source: `TODO.md`

- [x] `AUDIT-PARSER-AST-ARG-SIZE-GUARDS-048` harden parser call/import/FFI
  argument sizing before AST arena allocation
  - closure evidence:
    - `src/lisp/parser_define_relation_attr_helpers.c3`
      now checks relation role/count arithmetic and `Expr*` allocation sizing
      before building `__define-relation` call arguments.
    - `src/lisp/parser_application_helpers.c3`
      now checks generic call argument `Expr*` allocation sizing before
      copying argument pointers.
    - `src/lisp/parser_ffi_helpers.c3`
      now rejects overflowing FFI parameter counts before `+ 1` capacity
      checks.
    - `src/lisp/parser_import_helpers_specs.c3`
      now rejects overflowing selective-import counts before `+ 1` capacity
      checks.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded compiler slice: `pass=191 fail=0`
      - bounded deduce slice: `pass=330 fail=0`

- [x] `AUDIT-SHARED-ASYNC-STRING-SIZE-GUARDS-047` harden shared blob,
  process/TLS string duplication, filesystem result array growth, and
  compression output allocation sizes
  - closure evidence:
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

- [x] `AUDIT-ASYNC-FILE-IO-SIZE-GUARDS-046` harden async file I/O payload,
  temp-path, and read-buffer allocation sizes against `len + 1` overflow
  - closure evidence:
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

- [x] `AUDIT-SCHEDULER-OFFLOAD-NIL-COMPLETION-PROJECTION-045` make
  scheduler offload `OFFLOAD_RES_NIL` projection terminal so async file
  missing-path failures are remapped to their I/O payload codes instead of
  leaking `scheduler/offload-invalid-completion-kind`
  - closure evidence:
    - `src/lisp/scheduler_wakeup_io.c3`
      now returns directly from each offload completion-kind projection,
      including the nil result path used by failed async read-file offloads.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded async slice: `pass=61 fail=0`

- [x] `AUDIT-IO-STRING-BUFFER-GROWTH-HARDENING-044` harden console, input,
  CSV, REPL, and Unicode string-buffer growth paths against
  overflow-before-allocation and invalid capacity state
  - closure evidence:
    - `src/lisp/prim_io_console_helpers.c3`
      now checks console capture append/copy growth arithmetic before
      appending or duplicating buffered text.
    - `src/lisp/prim_io_helpers.c3`
      now rejects overflowing input-state append lengths before mutating the
      live buffer.
    - `src/lisp/primitives_data_formats_csv_parse.c3`
      now guards CSV field and row-array growth before allocating replacement
      storage.
    - `src/lisp/eval_repl_server_state.c3`
      now rejects overflowing session-string and session-capacity growth
      before publishing the replacement session state.
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

- [x] `AUDIT-DEDUCE-DIRECT-ALLOC-SCHEMA-MUTATION-043` harden direct
  deduce schema/rule mutation allocation sites and transaction insert
  accounting
  - closure evidence:
    - `src/lisp/deduce_db_handles_mutation.c3`
      now guards rule-signature count increments, range checks, and direct
      `sizeof * count` array allocations before copying rule metadata.
    - `src/lisp/deduce_db_relation_schema_init.c3`,
      `src/lisp/deduce_db_handles.c3`, and
      `src/lisp/deduce_db_handles_register.c3`
      now reject overflowing schema/index sizing before allocation or count
      publication.
    - `src/lisp/deduce_db_handles_mutation_txn.c3`
      now increments transaction `inserted_count` only after tuple delta
      append succeeds.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded deduce slice: `pass=330 fail=0`

- [x] `AUDIT-CORE-REGISTRY-TABLE-GROWTH-HARDENING-042` harden remaining
  core registry/table growth paths against overflow-before-allocation,
  failed-allocation state corruption, and invalid hashmap capacity state
  - closure evidence:
    - `src/lisp/value_symbol_table.c3` and
      `src/lisp/value_type_registry.c3`
      now reject overflowed init/grow byte-size arithmetic and keep previous
      table state when replacement allocation cannot be materialized.
    - `src/lisp/value_interp_init_helpers.c3`
      now checks macro/module init table/hash size multiplication before
      allocation.
    - `src/lisp/eval_pattern_match_support.c3`
      now guards `gensym` / match-binding grow loops against doubling and
      allocation-byte overflow.
    - `src/lisp/prim_collection_hashmap.c3` and
      `src/lisp/prim_collection_sort_array.c3`
      now enforce valid hashmap grow preconditions and reject overflowing grow
      arithmetic before mutating live collection state.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded allocator-validation slice: `pass=1 fail=0`
      - bounded advanced collections/module group: `pass=134 fail=0`
      - bounded advanced type-dispatch/mutation-chain group: `pass=236 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- [x] `AUDIT-OVERFLOW-HARDENING-BATCH-041` close the next allocator/cursor
  overflow audit batch across deduce persistence, AST/parser allocation,
  JIT/effect staging, interpreter/env table growth, and method-table growth
  - closure evidence:
    - persisted deduce rule signature/catalog record encode/restore paths now
      use checked size and cursor helpers before allocation, copy, and slice
      movement.
    - AST arena alignment/chunk accounting and parser import/module/export
      growth now reject allocation-size overflow before state mutation.
    - JIT arg-buffer, effect-handler, and handle-state copy allocation sites
      now reject oversized element counts before byte-size multiplication.
    - interpreter macro/module/handler table growth, env binding growth, and
      method-table growth now check doubling and allocation sizes before
      mutating tables.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded deduce slice: `pass=330 fail=0`
      - bounded allocator-validation slice: `pass=1 fail=0`
      - bounded advanced collections/module group: `pass=134 fail=0`
      - bounded advanced effect-continuation group: `pass=56 fail=0`
      - bounded advanced runtime-control group: `pass=22 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- [x] `AUDIT-RUNTIME-MODULE-EXPORT-GROWTH-FAILCLOSED-040` make module
  export-table growth fail closed instead of writing through failed
  replacement allocation
  - closure evidence:
    - `src/lisp/jit_jit_module_setup_helpers.c3`
      now routes export allocation/growth through a checked helper and
      preserves existing exports on growth failure.
    - `src/lisp/jit_jit_compile_effects_modules.c3` and
      `src/lisp/jit_jit_module_import_setup.c3`
      now propagate export growth failure from re-export and implicit module
      export paths.
    - `src/lisp/tests_advanced_stdlib_module_groups.c3`
      now pins forced module export growth allocation failure.
    - validation:
      - `c3c build`
      - bounded advanced collections/module group: `pass=134 fail=0`

- [x] `AUDIT-CAPACITY-REALLOC-BYTE-OVERFLOW-039` guard deduce/JIT
  capacity growth before `sizeof * new_cap` arithmetic can wrap and
  under-allocate
  - closure evidence:
    - deduce relation, aggregate, delta, query-demand, transaction,
      dirty-predicate, rule-signature, relation-schema, and persisted-rule
      catalog growth helpers now fail closed before allocation byte counts
      overflow.
    - `src/lisp/jit_jit_module_setup_helpers.c3`
      now rejects oversized source-dir vector growth and path length
      increments before allocation-size arithmetic can wrap.
    - `src/lisp/tests_deduce_groups_parallel.c3`
      now pins the oversized-capacity fail-closed regression.
    - validation:
      - `c3c build`
      - bounded deduce parallel group: `pass=6 fail=0`
      - bounded scheduler slice: `pass=111 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- [x] `AUDIT-SCHEDULER-OFFLOAD-MISSING-COMPLETION-038` make non-task
  offload worker double-allocation failure wake and fail closed instead of
  stranding a blocked waiter
  - closure evidence:
    - `src/lisp/scheduler_offload_worker.c3`
      now publishes or directly handles readiness even when both worker
      completion and fallback alloc-failure completion are null.
    - `src/lisp/scheduler_wakeup_io.c3`
      now consumes active completed-null offload slots as
      `"offload: missing completion"` and clears the pending slot.
    - `src/lisp/tests_scheduler_boundary_offload_payload_groups.c3`
      now pins the completed-null wakeup/consume path.
    - validation:
      - `c3c build`
      - bounded scheduler slice: `pass=111 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- [x] `AUDIT-FORMAT-DISPLAY-TEMP-BUILDER-FAILCLOSED-037` fix `%s`
  formatting display path so plain `(format "%s" value)` does not fail with
  `"format: failed to grow temporary builder"` for ordinary values
  - closure evidence:
    - `src/lisp/prim_string_format_helpers.c3`
      now computes `StringVal` target capacity through checked overflow
      addition instead of comparing normal small appends against `usz.max`.
    - `src/lisp/tests_advanced_core_unicode_groups.c3`
      now pins both `(format "%s" nil)` and `(format "%s" (Void))`.
    - validation:
      - `c3c build`
      - bounded `advanced` slice with
        `OMNI_ADVANCED_GROUP_FILTER=advanced-unicode-iterator`: `pass=129 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`
      - bounded ASAN `memory-lifetime-smoke`: `pass=189 fail=0`
      - direct JSON REPL probes for `(format "%s" nil)`,
        `(format "%s" (Void))`, and a long `%s` string that requires growth

- [x] `AUDIT-RUNTIME-EFFECT-PUBLICATION-FAILCLOSED-036` make effect
  publication/dispatch fail closed when payload or continuation materialization
  fails, instead of degrading payload shape or null-dereferencing
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now makes handled raises reject payload-construction failure before
      handler bind instead of degrading to a message-only handled raise.
    - `src/lisp/value_interp_continuation_helpers.c3`
      now exposes a narrow continuation-allocation failure seam instead of
      dereferencing failed root-scope allocation.
    - `src/lisp/jit_jit_handle_signal.c3`,
      `src/lisp/jit_jit_runtime_effects_handle.c3`,
      `src/lisp/jit_jit_reset_shift.c3`, and
      `src/lisp/jit_jit_runtime_effects_reset_shift.c3`
      now fail closed on continuation allocation failure in handled effect and
      capture dispatch.
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
      now returns `"runtime effect payload: out of memory"` when
      unhandled-effect diagnostic payload construction cannot complete,
      instead of silently dropping the payload.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` and
      `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now pin both handled-raise payload construction failure and handled
      effect continuation allocation failure directly in bounded slices.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=pending-raise-payload-alloc-failure,handle-continuation-alloc-failure ./build/main --test-suite lisp'`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-RUNTIME-LIST-MATERIALIZATION-FAILCLOSED-035` make helper-owned
  list materializers fail closed instead of continuing after cons-constructor
  faults
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now exposes a shared `make_cons_or_error(...)` helper with a narrow
      nth-failure seam for deterministic runtime list-builder tests.
    - `src/lisp/prim_string_transform.c3`
      now makes `string-split` reject internal result-list construction
      failure instead of continuing with partial/null list state.
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

- [x] `AUDIT-JIT-HELPER-ARG-CONSTRUCTION-FAILCLOSED-034` make JIT helper
  arg-list and variadic rest-list construction fail closed instead of passing
  raw cons-constructor faults into normal dispatch/binding flows
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now exposes a checked `make_list1_or_error(...)` helper with a narrow
      nth-failure seam for deterministic JIT variadic-rest tests.
    - `src/lisp/jit_jit_apply_helpers.c3` and
      `src/lisp/jit_jit_apply_runtime.c3`
      now reject variadic zero-fixed-arg rest-list construction failure before
      binding the rest parameter environment.
    - `src/lisp/jit_jit_dispatch_helpers.c3`
      now routes instance `ref` dispatch arg-list materialization through the
      shared checked two-item helper instead of nesting raw `make_cons(...)`.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now directly pins the variadic rest-list allocation failure in the
      `jit-policy` slice.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=variadic-rest-list-alloc-failure ./build/main --test-suite lisp'`

- [x] `AUDIT-JIT-QUASIQUOTE-CONS-FAILCLOSED-033` make JIT quasiquote pair
  construction fail closed instead of wrapping cons-constructor faults as
  successful quasiquote values
  - closure evidence:
    - `src/lisp/jit_jit_quasiquote_macros.c3`
      now routes all internal quasiquote pair construction through one checked
      helper with a narrow nth-failure seam, and returns
      `"quasiquote: failed to allocate pair"` on allocation failure instead of
      passing raw cons-constructor faults through `eval_ok(...)`.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now directly pins both nested quasiquote and list quasiquote pair
      materialization failure in the `jit-policy` slice.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=quasiquote-cons-alloc-failure ./build/main --test-suite lisp'`
