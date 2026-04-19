# compiler-parser-refactor-plan Part 03

Source: `docs/plans/compiler-parser-refactor-plan.md`

### R5 Continuation Slice Z (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_expr_serialize_values.c3`:
  - moved type-annotation serialization helpers to
    `src/lisp/compiler_expr_serialize_type_annotations.c3`:
    - `serialize_quoted_text_to_buf(...)`,
    - `serialize_type_val_literal_to_buf(...)`,
    - `serialize_type_annotation_to_buf(...)`.
  - retained value-domain serialization helpers in
    `src/lisp/compiler_expr_serialize_values.c3`:
    - `serialize_value_to_buf(...)`,
    - `serialize_int_value_to_buf(...)`,
    - `serialize_double_value_to_buf(...)`,
    - `serialize_string_value_to_buf(...)`,
    - `serialize_cons_value_to_buf(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_let_set_flat.c3` (`207`) (tie with
    `src/lisp/compiler_lambda_scan.c3` (`207`)).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice AA (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_let_set_flat.c3`:
  - moved let-lowering helpers and let entrypoints to
    `src/lisp/compiler_let_compilation_flat_style.c3`:
    - `emit_let_forward_decl(...)`,
    - `emit_let_assignment(...)`,
    - `emit_recursive_closure_self_patch(...)`,
    - `compile_mutable_captured_let(...)`,
    - `compile_let_flat_common(...)`,
    - `compile_let_flat(...)`,
    - `compile_let_flat_tail(...)`.
  - retained control/set/primitive-check helpers in
    `src/lisp/compiler_let_set_flat.c3`:
    - `compile_if_flat(...)`,
    - `compile_and_flat(...)`,
    - `compile_or_flat(...)`,
    - `compile_begin_flat(...)`,
    - `compile_mutable_captured_set_flat(...)`,
    - `compile_local_set_flat(...)`,
    - `compile_set_flat(...)`,
    - `is_builtin_primitive(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_lambda_scan.c3` (`207`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice AB (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_lambda_scan.c3`:
  - moved lambda-def/capture assembly helpers and lambda-case scan to
    `src/lisp/compiler_lambda_scan_lambda_defs.c3`:
    - `collect_lambda_own_bound(...)`,
    - `collect_lambda_nested_bound(...)`,
    - `init_lambda_def_from_expr(...)`,
    - `lambda_def_push_capture(...)`,
    - `scan_lambdas_lambda(...)`.
  - retained traversal/dispatch helpers in
    `src/lisp/compiler_lambda_scan.c3`:
    - `scan_lambdas(...)`,
    - `scan_lambdas_let(...)`,
    - `scan_lambdas_call(...)`,
    - `scan_lambdas_binary_expr(...)`,
    - `scan_lambdas_if_expr(...)`,
    - `scan_lambdas_match_expr(...)`,
    - `scan_lambdas_begin_expr(...)`,
    - `scan_lambdas_module_expr(...)`,
    - `scan_lambdas_with_scope(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_expr_serialize_exprs.c3` (`204`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice AC (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_expr_serialize_exprs.c3`:
  - moved specialized expression-form helpers and specialized dispatch to
    `src/lisp/compiler_expr_serialize_special_forms.c3`:
    - `serialize_unary_expr_form(...)`,
    - `serialize_binary_expr_form(...)`,
    - `serialize_named_expr_form(...)`,
    - `serialize_symbol_and_arg_form(...)`,
    - `serialize_begin_expr_to_buf(...)`,
    - `serialize_index_expr_to_buf(...)`,
    - `serialize_path_expr_to_buf(...)`,
    - `serialize_expr_specialized_core(...)`,
    - `serialize_expr_specialized_reader(...)`,
    - `serialize_expr_specialized(...)`.
  - retained entrypoint dispatch in
    `src/lisp/compiler_expr_serialize_exprs.c3`:
    - `serialize_expr_to_buf(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_expr_serialize_definition_forms.c3` (`201`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice AD (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_expr_serialize_definition_forms.c3`:
  - moved macro/module/import/export serialization to
    `src/lisp/compiler_expr_serialize_macro_module_forms.c3`:
    - `serialize_defmacro_to_buf(...)`,
    - `serialize_module_to_buf(...)`,
    - `serialize_import_to_buf(...)`,
    - `serialize_export_from_to_buf(...)`.
  - retained type/effect definition serialization in
    `src/lisp/compiler_expr_serialize_definition_forms.c3`:
    - `serialize_deftype_to_buf(...)`,
    - `serialize_defabstract_to_buf(...)`,
    - `serialize_defunion_to_buf(...)`,
    - `serialize_defalias_to_buf(...)`,
    - `serialize_defeffect_to_buf(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/parser_import_export.c3` (`198`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice AE (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_import_export.c3`:
  - moved import helper routines to `src/lisp/parser_import_helpers.c3`:
    - `parser_token_is_legacy_marker(...)`,
    - `Parser.init_import_expr(...)`,
    - `Parser.import_ensure_capacity(...)`,
    - `Parser.parse_import_target(...)`,
    - `Parser.parse_import_symbol_spec(...)`,
    - `Parser.parse_import_paren_spec(...)`,
    - `Parser.parse_import_selective_list(...)`.
  - retained import entrypoint parsing in
    `src/lisp/parser_import_export.c3`:
    - `Parser.parse_import(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_temp_core.c3` (`198`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice AF (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_temp_core.c3`:
  - moved resolve/index/define bridge helpers to
    `src/lisp/compiler_temp_misc_forms.c3`:
    - `compile_resolve_flat(...)`,
    - `compile_index_flat(...)`,
    - `compile_eval_serialized_expr_flat(...)`,
    - `is_typed_lambda_define(...)`,
    - `compile_define_rhs_with_typed_bridge(...)`,
    - `compile_define_flat(...)`.
  - retained temp lifecycle and dispatch entrypoints in
    `src/lisp/compiler_temp_core.c3`:
    - `next_result(...)`,
    - `emit_temp_decl(...)`,
    - `emit_temp_ref(...)`,
    - `emit_nil_temp(...)`,
    - `compile_leaf_expr_to_temp(...)`,
    - `compile_to_temp_non_null(...)`,
    - `compile_to_temp(...)`,
    - `compile_to_temp_tail(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_quasiquote_flat.c3` (`198`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice AG (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_quasiquote_flat.c3`:
  - moved quasiquote `E_CALL` lowering helpers to
    `src/lisp/compiler_quasiquote_call_flat.c3`:
    - `qq_call_has_splice(...)`,
    - `qq_emit_cons_pair(...)`,
    - `qq_emit_append_pair(...)`,
    - `qq_emit_nil_result(...)`,
    - `compile_qq_call_no_splice(...)`,
    - `compile_qq_call_with_splice(...)`,
    - `compile_qq_call_flat(...)`.
  - retained non-call quasiquote lowering in
    `src/lisp/compiler_quasiquote_flat.c3`:
    - `compile_qq_marker_pair(...)`,
    - `compile_qq_var_symbol(...)`,
    - `compile_qq_app_list(...)`,
    - `compile_qq_flat(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_primitive_variable_hash_table.c3` (`194`)
    (tie with `src/lisp/compiler_native_match_compilation_flat_style.c3` (`194`)).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice AH (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_primitive_variable_hash_table.c3`:
  - moved primitive-domain population helpers to
    `src/lisp/compiler_primitive_variable_hash_table_domains.c3`:
    - `init_prim_hash_arithmetic_and_comparison(...)`,
    - `init_prim_hash_core_and_strings(...)`,
    - `init_prim_hash_files_and_misc(...)`,
    - `init_prim_hash_collections_math_bitwise(...)`.
  - retained hash table storage/probe/init orchestration in
    `src/lisp/compiler_primitive_variable_hash_table.c3`:
    - `prim_hash_insert(...)`,
    - `prim_hash_lookup(...)`,
    - `init_prim_hash(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_native_match_compilation_flat_style.c3` (`194`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice AI (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_native_match_compilation_flat_style.c3`:
  - moved pattern-binding emit helpers to
    `src/lisp/compiler_native_match_bindings_flat_style.c3`:
    - `emit_pattern_var_decl(...)`,
    - `emit_pattern_var_assign(...)`,
    - `emit_pattern_seq_elem_access(...)`,
    - `emit_pattern_seq_elem_binding(...)`,
    - `compile_pattern_bindings(...)`.
  - retained match entrypoint and pattern-check lowering in
    `src/lisp/compiler_native_match_compilation_flat_style.c3`:
    - `compile_match_flat(...)`,
    - `compile_pattern_check(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_call_flat.c3` (`185`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice AJ (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_call_flat.c3`:
  - moved call arg/list construction helpers to
    `src/lisp/compiler_call_arg_list_helpers.c3`:
    - `compile_call_arg_temps(...)`,
    - `build_arg_list_from_temps(...)`,
    - `compile_list_or_dict_call_flat(...)`,
    - `tail_call_requires_non_tail_path(...)`,
    - `compile_zero_arg_tail_call(...)`.
  - retained call/app entrypoint lowering in
    `src/lisp/compiler_call_flat.c3`:
    - `call_head_is(...)`,
    - `call_is_explain_thunked(...)`,
    - `compile_call_flat(...)`,
    - `compile_call_tail_flat(...)`,
    - `compile_app_flat(...)`,
    - `compile_app_tail_flat(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/parser_define_attrs.c3` (`184`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice AK (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_define_attrs.c3`:
  - moved relation-attribute define helpers to
    `src/lisp/parser_define_relation_attr.c3`:
    - `parse_relation_attr_options(...)`,
    - `build_define_relation_call(...)`,
    - `parse_define_relation_attr(...)`.
  - retained macro/schema/special define dispatch in
    `src/lisp/parser_define_attrs.c3`:
    - `parse_define_macro_attr(...)`,
    - `parse_define_schema_attr(...)`,
    - `parse_define_special(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/parser_control_effects.c3` (`182`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).
