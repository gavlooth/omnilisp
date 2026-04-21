# Active TODO Index Part 09

Source: `TODO.md`

- [x] `AUDIT-FTXUI-C-ABI-EXCEPTION-SAFETY-063` wrap exported FTXUI C ABI
  entrypoints in fail-closed exception guards
  - closure evidence:
    - `csrc/ftxui_shim.cpp` now routes status-returning backend work through
      a shared `status_try(...)` guard that maps `std::bad_alloc` to
      `OMNI_FTXUI_STATUS_OUT_OF_MEMORY` and other C++ exceptions to
      `OMNI_FTXUI_STATUS_INTERNAL_ERROR`.
    - Argument and ABI validation remains outside the guard where it was
      already fail-closed, preserving existing validation behavior.
    - Deferred graph/render/event/quit-key callback adapters catch callback
      exceptions locally and return safe fallback values rather than allowing
      callback exceptions to escape through FTXUI render/event frames.
    - A coverage check over `omni_ftxui_status` exports found no unguarded
      status-returning entrypoint except the trivial last-error accessor.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green

- [x] `AUDIT-FTXUI-SCREEN-LIFETIME-064` make quit-key wrapper lifetime explicit
  - closure evidence:
    - `csrc/ftxui_shim.cpp` now stores the underlying `ScreenInteractive` with
      shared ownership.
    - `omni_ftxui_component_wrap_quit_keys(...)` now captures the shared screen
      object instead of the raw screen handle and retains it in the wrapped
      component keep-alive list.
    - `csrc/ftxui_shim.h` and `docs/plans/ftxui-c-abi-shim.md` document that
      the quit wrapper retains the screen's underlying loop object until the
      wrapped component is destroyed.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green

- [x] `AUDIT-OFFLOAD-WIDTH-GUARDS-066` harden offload descriptor/session/temp-id
  narrowing edges
  - closure evidence:
    - `src/lisp/scheduler_offload_network.c3` now rejects listener file
      descriptors that cannot fit in the `int` `tcp_accept_fd(...)` API before
      narrowing.
    - `src/lisp/eval_repl_server_state.c3` now formats REPL session IDs from a
      guarded `long` value instead of truncating `next_session_id` through
      `int`.
    - `src/lisp/scheduler_offload_ops.c3` now formats the full guarded
      `unique_id` lane for atomic temp-path suffixes instead of truncating it to
      `uint`.
    - `src/lisp/scheduler_state_support_types.c3` now has a compile-time guard
      for the current `OffloadWork` pointer-through-`long` payload contract.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green
    - bounded normal+ASAN `async`: green (`pass=65 fail=0`)

- [x] `AUDIT-AOT-MODULE-DEDUCE-COLLECTION-SENTINELS-058` harden AOT/env
  sentinel handling, module load rollback, deduce restore failure propagation,
  and nullable collection backing guards
  - closure evidence:
    - `src/lisp/aot_runtime_bridge_closure.c3`,
      `src/lisp/aot_runtime_bridge_helpers.c3`,
      `src/lisp/aot_runtime_bridge_ffi.c3`, and
      `src/lisp/aot_runtime_bridge_ffi_lib.c3` now route AOT bridge symbol
      binding and lookup through checked intern helpers instead of allowing
      `INVALID_SYMBOL_ID` to reach environment lookup/define/set paths.
    - `src/lisp/value_environment_storage.c3`,
      `src/lisp/value_environment.c3`, and
      `src/lisp/value_environment_barrier.c3` now reject invalid symbol IDs
      before hash probing or binding mutation.
    - `src/lisp/jit_module_setup_helpers.c3`,
      `src/lisp/jit_module_import_setup.c3`,
      `src/lisp/macros_expansion.c3`, and
      `src/lisp/value_interp_lifecycle.c3` now roll back newly published
      modules on body/path/top-level load failure, rebuild module hash state,
      skip tombstones during hash rebuild, and avoid stale module pointers when
      nested loads grow the module table.
    - `src/lisp/deduce_db_storage_open.c3`,
      `src/lisp/deduce_db_rule_catalog_persistence.c3`,
      `src/lisp/deduce_db_rule_signature_restore.c3`, and
      `src/lisp/deduce_db_handles_storage.c3` now distinguish missing DBIs
      from other LMDB open failures, reject invalid interned restored symbols,
      roll back partially restored rule signatures/schemas, and fail closed on
      relation metadata policy intern failure.
    - `src/lisp/eval_promotion_root_clones.c3` now checks method-table clone
      entry allocation size before `sizeof * capacity`.
    - `src/lisp/value_constructors.c3`,
      `src/lisp/prim_collection_sort_array.c3`,
      `src/lisp/prim_collection_hashmap.c3`, and
      `src/lisp/prim_collection_generic_set.c3` now reject invalid raise
      payload symbols and nullable array/dict/set backing storage before
      dereference.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded advanced slice: `pass=1183 fail=0`
      - bounded compiler slice: `pass=191 fail=0`
      - bounded deduce slice: `pass=330 fail=0`
      - bounded string-type slice: `pass=40 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-NUMERIC-MACRO-ASYNC-TAIL-GUARDS-057` harden numeric overflow,
  macro/parser symbol allocation, data-format option tails, and async cleanup
  paths
  - closure evidence:
    - `src/lisp/prim_math.c3`, `src/lisp/prim_math_core.c3`, and
      `src/lisp/prim_math_arithmetic.c3` now fail closed on `getrandom`
      failure/partial reads and on signed `long.min` / integer overflow edges
      in arithmetic, `abs`, `gcd`, and `lcm`.
    - `src/lisp/prim_string_format_directives.c3` now rejects overflowing
      format width/precision specifiers and `%b` no longer negates
      `long.min`.
    - `src/lisp/prim_string_ops.c3`,
      `src/lisp/prim_string_transform.c3`,
      `src/lisp/primitives_data_formats_csv.c3`, and
      `src/lisp/primitives_data_formats_toml_options.c3` now preserve
      multibyte `list->string` elements and reject improper option/list tails.
    - `src/lisp/prim_string_convert.c3`,
      `src/lisp/macros_template_expansion.c3`,
      `src/lisp/macros_expr_conversion_form_builders.c3`,
      `src/lisp/parser_parser.c3`, and
      `src/lisp/parser_application_placeholders.c3` now reject intern/allocation
      failure instead of propagating invalid symbol IDs or writing through
      unchecked macro AST allocations.
    - `src/lisp/async_tcp_transport_helpers.c3`,
      `src/lisp/async_tcp_transport_core.c3`,
      `src/lisp/async_process_signal_dns.c3`,
      `src/lisp/async_tcp_transport_connect.c3`,
      `src/lisp/async_udp_pipe.c3`,
      `src/lisp/async_tcp_transport_listen.c3`,
      `src/lisp/scheduler_state_types.c3`, and
      `src/lisp/scheduler_wakeup_queue.c3` now reject negative TCP read sizes,
      close stranded pending async state on resumed-before-completion errors,
      and track writable wakeup coalesces separately.
    - regression coverage added in:
      - `src/lisp/tests_advanced_core_unicode_groups.c3`
      - `src/lisp/tests_advanced_stdlib_numeric_groups.c3`
      - `src/lisp/tests_runtime_data_unicode_groups.c3`
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded arithmetic-comparison slice: `pass=45 fail=0`
      - bounded string-type slice: `pass=40 fail=0`
      - bounded data-format slice: `pass=62 fail=0`
      - bounded async slice: `pass=61 fail=0`
      - bounded scheduler slice: `pass=111 fail=0`
      - bounded advanced numeric string/predicate/format group:
        `pass=59 fail=0`
      - bounded advanced unicode iterator group: `pass=130 fail=0`
      - bounded advanced macro hygiene group: `pass=82 fail=0`

- [x] `AUDIT-RUNTIME-REGISTRY-IO-TLS-GUARDS-056` harden registry,
  bootstrap, unicode, collection, I/O, and TLS guard paths
  - closure evidence:
    - `src/lisp/value_symbol_table.c3` and
      `src/lisp/value_type_registry.c3` now reject exhausted ID spaces before
      narrowing counts to `SymbolId` / `TypeId`; symbol probing also skips
      out-of-range stale indices, and type rollback now rebuilds the hash table
      so open-address probe chains remain sound.
    - `src/lisp/value_interp_init_helpers.c3` and
      `src/lisp/value_interp_state.c3` now fail fast if interpreter bootstrap
      symbols cannot be interned instead of publishing invalid sentinel IDs.
    - `src/lisp/unicode_case_mapping.c3` now rejects strings too large for
      `utf8proc`'s `long` length parameter before case conversion.
    - `src/lisp/scheduler_offload_network.c3` now calls `br_sslio_close(...)`
      only after `br_sslio_init(...)` has completed.
    - `src/lisp/prim_collection_hashmap.c3` now computes Dictionary initial
      capacity with checked arithmetic and allocates the hashmap payload before
      publishing the root wrapper.
    - `src/lisp/prim_io_file_helpers.c3` now rejects file sizes that cannot fit
      in `usz` before buffer allocation, and
      `src/lisp/prim_io_console_helpers.c3` now reports render/write failures
      as typed I/O errors.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded compiler slice: `pass=191 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`
      - bounded async slice: `pass=61 fail=0`
      - bounded scheduler slice: `pass=111 fail=0`
      - bounded advanced unicode iterator slice: `pass=129 fail=0`

- [x] `AUDIT-AOT-DISPATCH-SIGNATURE-ALLOC-GUARDS-055` harden AOT/JIT
  signature allocation and type publication failure paths
  - closure evidence:
    - `src/lisp/aot_type_spec_helpers.c3` now rejects overflowing type
      annotation, method-signature parameter, and method-constraint allocation
      sizes before allocation and delays count publication until staging
      succeeds.
    - `src/lisp/jit_closure_support.c3` now guards lambda method-signature
      constraint counting and parameter/constraint staging on both scope and
      heap copies.
    - `src/lisp/eval_type_declarations.c3` now checks derived type-info
      field/type-param/constraint/variant allocation sizes and rolls back a
      newly registered type if later constructor/global binding/type-value
      publication fails.
    - `src/lisp/value_type_registry.c3` now exposes a narrow
      just-added-type rollback helper for new-entry failure cleanup.
    - `src/lisp/eval_dispatch_match.c3`,
      `src/lisp/eval_dispatch_match_breakdown.c3`,
      `src/lisp/schema_explain_helpers.c3`, and
      `src/lisp/aot_runtime_bridge_helpers.c3` now reject overflowing temporary
      dispatch/schema/AOT staging buffers.
    - `src/lisp/eval_dispatch_types.c3` and
      `src/lisp/eval_init_primitives.c3` now clean up empty heap method-table
      bootstrap payloads when root-wrapper or global binding publication fails.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded compiler slice: `pass=191 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-JIT-DEDUCE-ALLOC-BOUNDS-054` harden JIT effect and deduce
  aggregate/materialization allocation arithmetic
  - closure evidence:
    - `src/lisp/jit_handle_signal_handle.c3`,
      `src/lisp/jit_runtime_effects_signal.c3`, and
      `src/lisp/jit_handle_signal_helpers_runtime_effects.c3` now reject
      overflowing effect clause and signal argument buffer sizes before
      allocation.
    - `src/lisp/deduce_rule_eval_exec_aggregate_groups.c3`,
      `src/lisp/deduce_rule_eval_exec_aggregate_helpers.c3`,
      `src/lisp/deduce_rule_ir_helpers.c3`, and
      `src/lisp/deduce_rule_ir.c3` now check aggregate group, encoded tuple,
      rule-term, and rule-atom allocation arithmetic before writing staged
      state.
    - `src/lisp/deduce_relation_row_materialization.c3` now clamps dictionary
      capacity arithmetic and rejects overflowing column-key value buffers.
    - `src/lisp/deduce_db_goal_directed_read_tracking.c3` now bounds
      read-position buffer sizing and writeback iteration from `ulong` counts.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded deduce slice: `pass=330 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-ENV-PROCESS-SPAWN-SIZE-GUARDS-053` harden environment,
  closure-wrapper, and process-spawn allocation sizing
  - closure evidence:
    - `src/lisp/value_environment_storage.c3` now rejects overflowing hash-table
      capacity and byte-size calculations before allocation.
    - `src/lisp/value_environment.c3` now guards load-factor multiplication
      before hash-table rebuild comparisons.
    - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now rejects overflowing
      closure parameter-copy allocations.
    - `src/lisp/async_process_spawn.c3` now rejects overflowing argv/env pointer
      table counts and byte sizes before process-spawn staging.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded async slice: `pass=61 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-RUNTIME-PROMOTION-ALLOC-STAGING-052` harden promotion,
  root-clone, env-copy, and pattern sequence allocation sizing and staging
  - closure evidence:
    - `src/lisp/eval_promotion_copy_route_helpers.c3`,
      `src/lisp/eval_promotion_escape_structured.c3`, and
      `src/lisp/eval_promotion_root_clone_basic.c3`
      now reject overflowing array, hashmap, method-table, signature, and
      closure-parameter allocation sizes before copying boundary-owned data.
    - `src/lisp/eval_promotion_escape_structured.c3`
      now resets staged method signatures on dependent allocation failure and
      delays closure result wrapper publication until fallible clone/env work
      succeeds.
    - `src/lisp/eval_env_copy_frame_helpers.c3`
      now allocates non-inline binding storage before publishing the copied
      frame and frees it if frame allocation fails.
    - `src/lisp/eval_pattern_matching.c3`
      now rejects overflowing sequence element collection buffers.
    - validation:
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-DEDUCE-SCC-ALLOC-BOUNDS-051` harden deduce SCC, reachability,
  proof-key, aggregate batch, component-delta, and schema-capacity arithmetic
  - closure evidence:
    - `src/lisp/deduce_rule_eval_scc_plan.c3`
      now checks SCC square matrix and stratum relaxation bound arithmetic.
    - `src/lisp/deduce_rule_eval_validation.c3`
      now checks reachability matrix square sizing before allocation.
    - `src/lisp/deduce_rule_eval_exec_seminaive.c3`,
      `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates_impl.c3`,
      and `src/lisp/deduce_rule_eval_exec_component_delta_restore.c3`
      now guard proof-key, aggregate batch, and decoded delta-entry allocation
      sizes.
    - `src/lisp/deduce_schema_query_input_shape.c3`,
      `src/lisp/deduce_schema_query_input_roles.c3`, and
      `src/lisp/deduce_schema_query_input_constraints.c3`
      now reject overflowing `count + 1` capacity requests.
    - validation:
      - bounded deduce slice: `pass=330 fail=0`

- [x] `AUDIT-AOT-FFI-ALLOC-STAGING-050` harden AOT type definition cleanup
  ownership and runtime FFI call argument staging
  - closure evidence:
    - `src/lisp/aot_type_definitions.c3`
      now initializes cleanup-owned type fields and union variants before any
      later fallible allocation can trigger deferred cleanup, and checks
      AOT field/variant/type-parameter allocation sizes before allocation.
    - `src/lisp/eval_ffi_bound_call.c3`
      now rejects unsupported/narrowing argument counts and overflowing
      libffi staging buffer sizes before preparing runtime call storage.
    - validation:
      - bounded compiler slice: `pass=191 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-PARSER-AST-ARRAY-ALLOC-HELPER-049` centralize parser AST array
  allocation overflow checks
  - closure evidence:
    - `src/lisp/parser_parser.c3`
      now exposes `Parser.alloc_ast_array_bytes(...)`, which checks
      `elem_size * count` before delegating to AST arena allocation.
    - Parser dynamic AST array allocations for calls, relation definitions,
      literals, patterns, type annotations, module bodies, lambda params,
      named-let rewrites, path expressions, macro clauses, blocks, and pipe
      rewrites now route through the checked helper.
    - `src/lisp/parser_set_pipe_helpers.c3`
      now rejects overflowing `arg_count + 1` before growing pipe call
      arguments.
    - validation:
      - bounded compiler slice: `pass=191 fail=0`
      - bounded deduce slice: `pass=330 fail=0`
