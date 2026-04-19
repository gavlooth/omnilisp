# compiler-parser-refactor-plan Part 02

Source: `docs/plans/compiler-parser-refactor-plan.md`

### R5 Continuation Slice L (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_type_literals.c3`:
  - moved type-annotation parsing domain to
    `src/lisp/parser_type_annotations.c3`:
    - `parser_empty_type_annotation(...)`,
    - `parse_value_literal_annotation(...)`,
    - `type_annotation_copy_params(...)`,
    - `type_annotation_copy_meta(...)`,
    - `parse_compound_type_annotation(...)`,
    - `parse_dict_type_annotation(...)`,
    - `parse_type_annotation(...)`.
  - retained constructor type-application parsing in
    `src/lisp/parser_type_literals.c3`:
    - `parse_constructor_type_application(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/compiler_expr_serialize_values.c3` (`262`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice M (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_expr_serialize_values.c3`:
  - moved pattern serialization domain to
    `src/lisp/compiler_expr_serialize_patterns.c3`:
    - `serialize_pattern_to_buf(...)`.
  - retained value and type-annotation serialization in
    `src/lisp/compiler_expr_serialize_values.c3`:
    - `serialize_value_to_buf(...)` and value literal/string helpers,
    - `serialize_type_annotation_to_buf(...)` and quoted-text/type-literal helpers.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/parser_lexer.c3` (`260`) (tie with `src/lisp/compiler_program_pipeline.c3` (`260`)).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice N (2026-03-09)

- Performed behavior-preserving extraction on `src/lisp/parser_lexer.c3`:
  - moved whitespace/comment scanning domain to
    `src/lisp/parser_lexer_whitespace.c3`:
    - `skip_nested_block_comment(...)`,
    - `skip_whitespace(...)`.
  - retained lexer core lifecycle and token-advance dispatch in
    `src/lisp/parser_lexer.c3`:
    - text-buffer allocation/error helpers, init/destroy and position helpers,
    - literal/symbol selection, advance/error token flow, and token API (`at_end`, `match`, `expect`).
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/compiler_program_pipeline.c3` (`260`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice O (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_program_pipeline.c3`:
  - moved top-level program emission/global sync domain to
    `src/lisp/compiler_program_top_level.c3`:
    - `add_global_if_missing(...)`,
    - `collect_expr_generated_globals(...)`,
    - `collect_top_level_defines(...)`,
    - `emit_global_lookup_sync(...)`,
    - `emit_type_form_global_sync(...)`,
    - `emit_top_level_exprs(...)`.
  - retained program pipeline orchestration in
    `src/lisp/compiler_program_pipeline.c3`:
    - parse/build-source helpers,
    - analysis pass setup,
    - main-body emission orchestration and two-pass program assembly.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/parser_control_effects.c3` (`256`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice P (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_control_effects.c3`:
  - moved explain-form parsing domain to
    `src/lisp/parser_explain.c3`:
    - `parse_explain_selector_symbol(...)`,
    - `make_explain_target_thunk(...)`,
    - `parse_explain(...)`.
  - retained other control/effect parse forms in
    `src/lisp/parser_control_effects.c3`:
    - `quote`, `reset`, `shift`, `perform`, `resolve`,
      `with-continuation`, `and`, `or`, `begin`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/parser_define_core.c3` (`238`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice Q (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_define_core.c3`:
  - moved bracket-annotation `define` dispatch domain to
    `src/lisp/parser_define_annotations.c3`:
    - `parse_define_attrs(...)`,
    - `parse_define_with_annotation(...)`,
    - `parse_define_type(...)`,
    - `parse_define_ffi(...)`.
  - retained core define parsing in `src/lisp/parser_define_core.c3`:
    - shorthand/normal define parse flow,
    - shorthand lambda/body/rest parsing helpers,
    - `parse_define(...)` dispatch between annotation/shorthand/normal paths.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/parser_expr_atoms.c3` (`235`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice R (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_expr_atoms.c3`:
  - moved reader-shorthand and reader-dispatch atom helpers to
    `src/lisp/parser_expr_reader_forms.c3`:
    - `parse_backtick_shorthand(...)`,
    - `parse_unquote_shorthand(...)`,
    - `parse_unquote_splicing_shorthand(...)`,
    - `parse_form_comment_expr(...)`,
    - `parse_set_literal_expr(...)`,
    - `parse_regex_literal_expr(...)`,
    - `parse_quote_shorthand(...)`.
  - retained core atom parsing and `parse_expr(...)` dispatch in
    `src/lisp/parser_expr_atoms.c3`:
    - number/string/symbol/path/placeholder parsing,
    - collection/list dispatch and lexer error handling.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/parser_patterns.c3` (`231`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice S (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_patterns.c3`:
  - moved paren-pattern parsing domain to
    `src/lisp/parser_patterns_paren.c3`:
    - `parse_as_pattern_marker(...)`,
    - `parse_paren_pattern(...)`.
  - retained non-paren pattern parsing in
    `src/lisp/parser_patterns.c3`:
    - string/sequence/dict pattern parsing,
    - `parse_pattern(...)` token dispatch.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/parser_lexer.c3` (`226`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice T (2026-03-09)

- Performed behavior-preserving extraction on `src/lisp/parser_lexer.c3`:
  - moved lexer core lifecycle/token API helpers to
    `src/lisp/parser_lexer_core_api.c3`:
    - `ensure_text_capacity(...)`,
    - `push_current_text_char(...)`,
    - `set_error_text(...)`,
    - `init(...)`,
    - `destroy(...)`,
    - `peek(...)`,
    - `next_char(...)`,
    - `set_error_token(...)`,
    - `at_end(...)`,
    - `match(...)`,
    - `expect(...)`.
  - retained token-advance/literal-symbol scan flow in
    `src/lisp/parser_lexer.c3`:
    - `is_number_start(...)`,
    - `scan_literal_or_symbol(...)`,
    - `advance(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/parser_lambda.c3` (`220`) (tie with `src/lisp/compiler_code_emission.c3` (`220`)).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice U (2026-03-09)

- Performed behavior-preserving extraction on `src/lisp/parser_lambda.c3`:
  - moved shared callable/body helper domain to
    `src/lisp/parser_callable_helpers.c3`:
    - `parse_implicit_begin(...)`,
    - `wrap_body_with_destructuring(...)`,
    - `finalize_lambda_expr(...)`,
    - `collect_callable_params(...)`,
    - `parse_rest_param(...)`,
    - `finalize_lambda_no_fixed_params(...)`,
    - `finalize_lambda_with_params(...)`.
  - retained lambda/if entry parsing in `src/lisp/parser_lambda.c3`:
    - `parse_lambda(...)`,
    - `parse_if(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/compiler_code_emission.c3` (`220`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice V (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_code_emission.c3`:
  - moved closure-construction/lambda-return helper domain to
    `src/lisp/compiler_code_emission_lambda_closures.c3`:
    - `is_variadic_lambda(...)`,
    - `needs_arg_list(...)`,
    - `emit_make_closure_call(...)`,
    - `find_lambda_def_by_expr(...)`,
    - `emit_closure_capture_data(...)`,
    - `emit_closure_expr(...)`,
    - `emit_lambda_return_common(...)`,
    - `emit_lambda_return(...)`.
  - retained prelude and lambda-definition emission in
    `src/lisp/compiler_code_emission.c3`:
    - `emit_prelude(...)`,
    - lambda capture/signature/parameter-unpack helpers,
    - `emit_lambda_definition(...)`,
    - `emit_lambda_definitions(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/parser_type_annotations.c3` (`216`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice W (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_type_annotations.c3`:
  - moved type-annotation helper domain to
    `src/lisp/parser_type_annotation_helpers.c3`:
    - `parser_empty_type_annotation(...)`,
    - `parser_is_value_literal_ctor(...)`,
    - `parser_is_bool_symbol(...)`,
    - `parse_value_literal_annotation(...)`,
    - `type_annotation_copy_params(...)`,
    - `type_annotation_copy_meta(...)`.
  - retained type-annotation parse flow in
    `src/lisp/parser_type_annotations.c3`:
    - `parse_compound_type_annotation(...)`,
    - `parse_dict_type_annotation(...)`,
    - `parse_type_annotation(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/compiler_free_vars_scope_forms.c3` (`211`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice X (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_free_vars_scope_forms.c3`:
  - moved effect/control free-variable walkers to
    `src/lisp/compiler_free_vars_effect_forms.c3`:
    - `find_free_vars_shift(...)`,
    - `find_free_vars_resolve(...)`,
    - `find_free_vars_handle(...)`.
  - retained scope/mutation walker domain in
    `src/lisp/compiler_free_vars_scope_forms.c3`:
    - `find_free_vars_var(...)`,
    - `find_free_vars_lambda(...)`,
    - `find_free_vars_let(...)`,
    - `find_free_vars_match(...)`,
    - `find_free_vars_call(...)`,
    - `find_free_vars_path(...)`,
    - `find_free_vars_set(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_native_call_compilation_flat_style.c3` (`209`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice Y (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_native_call_compilation_flat_style.c3`:
  - moved literal/quote lowering to
    `src/lisp/compiler_native_literal_compilation_flat_style.c3`:
    - `compile_literal(...)`,
    - `compile_quote(...)`.
  - retained lambda/variable/path lowering in
    `src/lisp/compiler_native_call_compilation_flat_style.c3`:
    - `compile_lambda_flat(...)`,
    - `compile_var(...)`,
    - `compile_path(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_expr_serialize_values.c3` (`208`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).
