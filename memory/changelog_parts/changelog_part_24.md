# Memory Changelog Index Part 24

Source: `memory/CHANGELOG.md`

      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_eval.c3` now retains SCC planning and rule-evaluation helpers, with `deduce/analyze` / `deduce/rule!` entrypoints plus stratification validation moved to `src/lisp/deduce_rule_eval_prims.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Compiler/parser modularization batch:
  - `src/lisp/parser_define_core_helpers.c3` now retains low-level shorthand helpers, with shorthand/normal define parsing moved to `src/lisp/parser_define_core_helpers_define.c3`.
  - `src/lisp/compiler_program_pipeline.c3` now retains source assembly and final emission flow, with program parsing and analysis setup moved to `src/lisp/compiler_program_pipeline_helpers.c3`.
  - `src/lisp/compiler_temp_type_forms_defs.c3` now retains direct type-definition lowering, with abstract-type lowering moved to `src/lisp/compiler_temp_type_forms_defs_abstract.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization batch:
  - `src/lisp/parser_callable_helpers.c3` now retains body/lambda finalization helpers, with callable parameter collection moved to `src/lisp/parser_callable_helpers_params.c3`.
  - `src/lisp/parser_define_relation_attr.c3` now retains relation-column parsing and define dispatch, with relation-call assembly moved to `src/lisp/parser_define_relation_attr_helpers.c3`.
  - `src/lisp/compiler_expr_serialize_special_forms.c3` now retains core specialized serializers, with reader-side specialized serializers moved to `src/lisp/compiler_expr_serialize_special_forms_reader.c3`.
  - `src/lisp/parser_let_core.c3` now retains let parsing and binding collection, with chained let construction moved to `src/lisp/parser_let_core_helpers.c3`.
  - `src/lisp/parser_set_pipe.c3` now retains `set!` parsing, with pipe desugaring moved to `src/lisp/parser_set_pipe_helpers.c3`.
  - `src/lisp/parser_datum.c3` now retains datum/template parsing, with datum constructor helpers moved to `src/lisp/parser_datum_helpers.c3`.
  - `src/lisp/parser_let_named.c3` now retains named-let binding parsing and lambda construction, with named-let call/body assembly moved to `src/lisp/parser_let_named_helpers.c3`.
  - `src/lisp/compiler_free_vars_walk.c3` now retains free-var dispatch, with traversal helpers moved to `src/lisp/compiler_free_vars_walk_helpers.c3`.
  - `src/lisp/compiler_free_vars_utils.c3` now retains pattern-binding and primitive utilities, with quasiquote traversal moved to `src/lisp/compiler_free_vars_utils_qq.c3`.
  - `src/lisp/parser_lexer_token_scanners.c3` now retains punctuation and logic-variable scanning, with dot/underscore scanner helpers moved to `src/lisp/parser_lexer_token_scanners_dot.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization batch:
  - `src/lisp/compiler_temp_type_forms_defs_union.c3` now retains the union entrypoint, with variant-spec emission moved to `src/lisp/compiler_temp_type_forms_defs_union_helpers.c3`.
  - `src/lisp/parser_lexer_string_hash.c3` now retains `is_symbol_char(...)` and string scanning, with hash-dispatch parsing moved to `src/lisp/parser_lexer_string_hash_helpers.c3`.
  - `src/lisp/compiler_free_vars_scope_forms.c3` now retains var/lambda/let/match/call walkers, with path/set walkers moved to `src/lisp/compiler_free_vars_scope_forms_mutations.c3`.
  - `src/lisp/compiler_temp_type_forms_helpers.c3` now retains generic emit helpers, with AOT type-annotation spec emission moved to `src/lisp/compiler_temp_type_forms_annotation_helpers.c3`.
  - `src/lisp/parser_define_core.c3` now retains `parse_define(...)`, with shorthand/normal define helpers moved to `src/lisp/parser_define_core_helpers.c3`.
  - `src/lisp/parser_import_helpers.c3` now retains import state and marker helpers, with import target/spec parsing moved to `src/lisp/parser_import_helpers_specs.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_ffi.c3` now retains FFI entrypoints only.
  - `src/lisp/parser_ffi_helpers.c3` now owns FFI signature helpers:
    - `parse_ffi_fn_modifiers(...)`,
    - `set_ffi_c_name(...)`,
    - `ensure_ffi_param_capacity(...)`,
    - `parse_ffi_typed_params(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/compiler_output_helpers.c3` now retains generic output emission
    helpers only.
  - `src/lisp/compiler_output_symbol_helpers.c3` now owns symbol sanitization
    and primitive-reference helpers:
    - `is_c3_reserved(...)`,
    - `emit_symbol_name(...)`,
    - `emit_prim_global_name(...)`,
    - `record_prim_ref(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_collection_literals.c3` now retains postfix index access
    and lookup-accessor construction.
  - `src/lisp/parser_collection_literals_builders.c3` now owns dict/array
    literal builders:
    - `parse_dict_literal(...)`,
    - `parse_array_literal(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_patterns.c3` now retains `parse_pattern(...)` dispatch
    only, plus it continues to reuse the paren-pattern helpers in
    `src/lisp/parser_patterns_paren.c3`.
  - `src/lisp/parser_patterns_values.c3` now owns the string/sequence/dict/
    symbol pattern builders:
    - `parse_string_pattern_literal(...)`,
    - `parse_seq_pattern(...)`,
    - `parse_dict_pattern(...)`,
    - `parse_symbol_pattern(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/compiler_temp_type_forms_defs.c3` now retains type and abstract
    lowering.
  - `src/lisp/compiler_temp_type_forms_defs_misc.c3` now owns alias and effect
    lowering:
    - `compile_defalias_direct(...)`,
    - `compile_defeffect_direct(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/compiler_primitive_variable_hash_table_domains.c3` now retains
    the arithmetic/comparison/core/string/file/misc primitive registrations.
  - `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`
    now owns the collection/math/bitwise primitive registrations.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_expr_atoms.c3` now retains literal parsing and
    `parse_expr(...)` dispatch only.
  - `src/lisp/parser_expr_atoms_accessors.c3` now owns the dot-accessor and
    path helpers:
    - `parse_dot_prefixed_int_key(...)`,
    - `parse_dot_accessor_shorthand(...)`,
    - `parse_path_expr(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/compiler_temp_type_forms.c3` now retains typed-define handling
    and type-form dispatch only.
  - `src/lisp/compiler_temp_type_forms_defs.c3` now owns the direct
    type-definition lowerings:
    - `compile_deftype_direct(...)`,
    - `compile_defabstract_direct(...)`,
    - `compile_defunion_direct(...)`,
    - `compile_defalias_direct(...)`,
    - `compile_defeffect_direct(...)`.
  - `src/lisp/compiler_temp_type_forms_helpers.c3` remains the shared emit
    helper module for the type-form path.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/compiler_temp_type_forms.c3` now retains direct type-form
    lowering only.
  - `src/lisp/compiler_temp_type_forms_helpers.c3` now owns the shared type-
    form emit helpers:
    - `emit_bool_literal(...)`,
    - `emit_value_tag_literal(...)`,
    - `emit_target_assignment_prefix(...)`,
    - `emit_string_array_init(...)`,
    - `emit_aot_type_annotation_spec_init(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/compiler_temp_type_forms.c3` now owns direct type-form lowering
    and shared AOT annotation emit helpers.
  - `src/lisp/compiler_temp_misc_forms.c3` now retains resolve/index and
    define-bridge helpers only.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_datum.c3` now retains datum/template constructors and
    dispatch only.
  - `src/lisp/parser_datum_collections.c3` now owns the recursive list/template
    collection walkers used by `parse_datum_impl(...)`.
- validation:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    passed (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_application.c3` now retains application dispatch and
    placeholder lowering only.
  - `src/lisp/parser_application_helpers.c3` now owns shared application
    call-construction and argument-collection helpers:
    - `collect_application_args(...)`,
    - `build_call_expr(...)`,
    - `reject_nullary_accessor_shorthand_call(...)`.
  - `src/lisp/parser_set_pipe.c3` continues to reuse the shared call-builder
    helper without any behavior change.
- validation:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
    crashed in `lisp.Env.lookup` with an unaligned-access panic via
    `jit_lookup_var`.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
    surfaced a JIT mismatch on `map .1 accessor shorthand`
    (`interp=ok, jit=FAIL`).
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
    crashed in `lisp.Env.lookup` with an unaligned-access panic via
    `jit_lookup_var`.

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_type_defs.c3` now retains type-definition entrypoints
    only.
  - `src/lisp/parser_type_defs_union.c3` now retains union-definition
    entrypoints only.
  - `src/lisp/parser_type_defs_helpers.c3` now owns the shared
    copy/compound/variance helpers used by both type and union parsing.
- validation:
  - `c3c build`

## 2026-03-18

- CLI/tooling UX cleanup for help text and exit-status contract:
  - `src/entry_cli_help_version.c3` now includes an explicit exit-code summary
    for the main CLI paths.
  - `src/entry_test_modes.c3` now renders the invalid test-suite name in the
    error message instead of the raw pointer value.
  - `docs/PROJECT_TOOLING.md` now documents the common exit-status map for
    `--check`, `--eval`, `--test-suite`, and the other first-party commands.
  - `docs/man/omni.1` now distinguishes normal failure (`1`) from invalid
    test-suite selection (`2`).

- Memory-lifetime regression module identity cleanup:
  - `src/lisp/tests_memory_lifetime_boundary_graph_txn_bench_groups.c3` has
    been renamed to `src/lisp/tests_memory_lifetime_boundary_graph_txn_groups.c3`
    so boundary correctness tests are no longer labeled like benchmark-only
    modules.
  - `docs/BOUNDARY_SURFACE_AUDIT.md` now points at the renamed correctness
    module path.

- Deterministic multiple-dispatch ambiguity payload + tie metadata contract:
  - `src/lisp/eval_dispatch_match.c3`:
    - ambiguous equal-specificity dispatch now raises canonical recoverable
      payload (`type/dispatch-ambiguous`, domain `type`) instead of string-only
      error signaling.
    - ambiguity payload `data` now includes stable fields:
      - `reason` (`ambiguous-equal-specificity`)
      - `method`
      - `arg-count`
      - `arg-types`
      - `best-score`
      - `tie-count`
      - `candidate-indices`
    - `candidate-indices` are emitted in deterministic method-table index order
      (ascending), with no implicit tie-break winner.
  - regression coverage:
    - `src/lisp/tests_advanced_type_dispatch_groups.c3` now asserts:
      - stable ambiguity payload field values for equal-specificity ties,
      - stable candidate-index ordering with lower-specificity non-winner
        methods present.
  - compiler parity coverage:
    - `src/lisp/tests_e2e_generation_cases_extended.c3` adds a handled-raise
      ambiguity payload parity case so compiled output must match interpreter
      output for code/score/tie/index fields.

- Checkpoint/capture replay parity contract extension for compiled paths:
  - docs:
    - `docs/LANGUAGE_SPEC.md` now explicitly states replay-visible side-effect
      parity is execution-mode invariant (interpreter, JIT, compiled).
    - `docs/reference/06-effects.md` now mirrors the same replay parity
      requirement.
  - parity tests:
    - `src/lisp/tests_e2e_generation_cases_extended.c3` adds compiled parity
      replay cases for resumed-segment side effects:
      - `set!` mutation replay,
      - handled-effect replay.

- TODO closure:
  - `TODO.md` active queue reduced to zero by closing the four remaining
    dispatch ambiguity + replay parity items and recording completion details.

- Finwatch tutorial module landing:
  - `examples/finwatch/alerts.omni` now includes tutorial-sized price and
    analytics alert helpers built on the existing `handle`/`dispatch`
    composition surface.
  - `examples/finwatch/alerts_tutorial_smoke.omni` exercises the exported
    `alerts/tutorial-demo` helper and verifies the direct effect path under a
    log+collector-style capture.
  - `examples/README.md` now advertises the finwatch alert tutorial module and
    its dedicated smoke file as the minimal effect-handler + dispatch example.
  - `docs/plans/post-complete-backlog.md` marks the tutorial-project backlog
    item complete.

- validation:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=schema ./build/main --test-suite lisp` (`42 passed, 0 failed`)
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` (`122 passed, 0 failed`)
