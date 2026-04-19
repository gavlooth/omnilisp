# Active TODO Index Part 12

Source: `TODO.md`

- [x] `AUDIT-STRING-PAYLOAD-MATERIALIZERS-017` make string-backed runtime
  payload/list helpers fail closed on string wrapper allocation failure
  - closure evidence:
    - `src/lisp/prim_system.c3`
      now makes `(shell cmd true)` propagate stdout string construction
      failure directly instead of returning a success-shaped `(ERROR
      exit-code)` list.
    - `src/lisp/prim_io_fs_handles.c3`
      now makes `fs-readdir` propagate entry-name string construction failure
      directly instead of storing `ERROR` values as directory entries in a
      successful array.
    - `src/lisp/http.c3`
      now makes `http-get` / `http-request` propagate host/request string
      materialization failure directly before transport setup/write.
    - `src/lisp/schema_validation.c3`
      now makes `schema-explain` propagate failure of its singleton message
      string instead of returning a one-element list containing `ERROR` as
      ordinary data.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins `shell`, `fs-readdir`, and `schema-explain` under forced
      `make_string(...)` allocation failure.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-STRING-LIST-MATERIALIZERS-016` make pure string/list helper
  surfaces fail closed on per-element string wrapper allocation failure
  - closure evidence:
    - `src/lisp/prim_string_transform.c3`
      now makes `string-upcase` / `string-downcase` return constructor
      `ERROR`s directly and makes `string-split` propagate per-part
      string-allocation failure.
    - `src/lisp/prim_string_ops.c3`
      now makes `string->list` propagate per-character string wrapper
      allocation failure instead of embedding an `ERROR` into a successful
      list.
    - `src/lisp/unicode.c3`
      now makes `string-graphemes` propagate grapheme-cluster string
      construction failure directly instead of storing `ERROR` values in the
      cluster array/list.
    - `src/lisp/prim_io_file.c3`
      now makes `read-lines` propagate per-line string-construction failure
      directly.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins `string-upcase`, `string-downcase`, `string->list`,
      `string-split`, and `string-graphemes` under forced
      `make_string(...)` allocation failure.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEMA-EXPLAIN-LIST-BUILDERS-015` make schema-explain list
  assembly fail closed instead of hard-aborting on internal cons allocation
  failure
  - closure evidence:
    - `src/lisp/schema_explain_payload_helpers.c3` now routes list
      accumulation/reversal through `explain_prepend_or_oom(...)` instead of
      raw `make_cons(...)`.
    - `src/lisp/schema_explain_helpers.c3`,
      `src/lisp/schema_explain_effect_helpers.c3`, and
      `src/lisp/schema_explain_effect_runtime.c3`
      now propagate that failure through dispatch candidates, handler tag
      lists, and effect candidates.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins helper-level and top-level schema-explain list-builder OOM
      seams through a dedicated local `nth` fail seam.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-COLLECTION-MUTATOR-CHECKED-RETURNS-014` make dictionary/set
  mutation fail closed on backing-storage grow failure instead of silently
  dropping writes behind void mutator wrappers
  - closure evidence:
    - `src/lisp/prim_collection_hashmap.c3` now makes
      `hashmap_set_symbol(...)`, `hashmap_grow(...)`, and `hashmap_set(...)`
      return checked `bool` results instead of discarding insertion/grow
      failure.
    - `src/lisp/prim_collection_hashmap.c3` now makes `set!` on dictionary
      targets return `runtime/out-of-memory` when backing-storage growth
      fails, instead of returning `Void` after a dropped write.
    - `src/lisp/prim_collection_generic_set.c3` now makes `set-add` follow
      the same checked mutator contract for `SET`.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves
      both mutators fail with typed errors and leave the failed key absent
      from the target collection after grow failure.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-DEDUCE-EXPLAIN-INSERT-FAILCLOSED-013` make deduce explain,
  analyze, schema, stats, and why-result payload builders propagate checked
  `explain_dict_set*` failures instead of returning partially populated maps
  - closure evidence:
    - `src/lisp/deduce_why_result_payload.c3`,
      `src/lisp/deduce_why_result_path_payload.c3`,
      `src/lisp/deduce_why_result_lookup.c3`, and
      `src/lisp/deduce_why_result_lookup_derived.c3`
      now return the first insertion/grow failure from path/context/payload
      attachment instead of silently dropping the failed field and returning a
      successful why-result payload.
    - `src/lisp/deduce_rule_eval_analyze_payload_fields.c3`,
      `src/lisp/deduce_rule_eval_analyze_payload_tail.c3`, and
      `src/lisp/deduce_rule_eval_analyze_payload_result.c3`
      now treat payload-field insertion failure as a first-class
      `deduce/analyze` error instead of ignoring the failed write and
      continuing with a partial result map.
    - the remaining deduce explain/schema/stats helper family now follows the
      same checked insertion contract:
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

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-GUARDED-HASHMAP-CALLERS-012B` normalize the remaining already-guarded `make_hashmap(...)` caller family onto checked constructors or a shared fail-closed helper contract
  - closure evidence:
    - `src/lisp/deduce_relation_row_materialization.c3` now uses checked
      hashmap construction plus checked insertion for row-dict materialization.
    - `src/lisp/deduce_relation_ops_validation_payload.c3` now routes
      integrity payload maps through a checked helper and drops the payload
      cleanly when insertion fails instead of returning partially populated
      dicts.
    - deduce runtime helper state maps in:
      - `src/lisp/deduce_rule_eval_exec_component_state_helpers.c3`
      - `src/lisp/deduce_rule_eval_exec_component_state.c3`
      - `src/lisp/deduce_rule_eval_exec_aggregate_state.c3`
      - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
      - `src/lisp/deduce_rule_eval_scc.c3`
      - `src/lisp/deduce_relation_scan_helpers_join.c3`
      - `src/lisp/deduce_rule_eval_analyze_setup.c3`
      - `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
      now all use checked constructor/insertion paths.
    - the remaining deduce explain/schema/analyze and why-result payload dict
      families no longer use raw `make_hashmap(...)` either:
      - `rg -n "make_hashmap\\(" src/lisp/deduce_* src/lisp/unify_* -S`
        returns no matches.
    - regressions now pin:
      - helper-state constructor OOM in
        `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      - integrity payload-map OOM degradation in
        `src/lisp/tests_deduce_groups_integrity.c3`
      - why-result payload/path OOM in
        `src/lisp/tests_deduce_query_groups.c3`
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'` -> `pass=328 fail=0`

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
    - the separate runtime/status payload-builder lane is now closed under
      `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011`.
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
