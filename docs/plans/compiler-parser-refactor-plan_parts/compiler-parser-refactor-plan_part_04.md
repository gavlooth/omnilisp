# compiler-parser-refactor-plan Part 04

Source: `docs/plans/compiler-parser-refactor-plan.md`

### R5 Continuation Slice AL (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_control_effects.c3`:
  - moved effect-form parse helpers to
    `src/lisp/parser_effect_forms.c3`:
    - `parse_reset(...)`,
    - `parse_shift(...)`,
    - `parse_perform(...)`,
    - `parse_resolve(...)`,
    - `parse_with_continuation(...)`.
  - retained quote/control parse helpers in
    `src/lisp/parser_control_effects.c3`:
    - `parse_quote(...)`,
    - `parse_and(...)`,
    - `parse_or(...)`,
    - `parse_begin(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/parser_application.c3` (`178`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice AM (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/parser_application.c3`:
  - moved shared call-construction and argument-collection helpers to
    `src/lisp/parser_application_helpers.c3`:
    - `collect_application_args(...)`,
    - `build_call_expr(...)`,
    - `reject_nullary_accessor_shorthand_call(...)`.
  - retained application dispatch and placeholder lowering in
    `src/lisp/parser_application.c3`:
    - `parse_application(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/parser_lexer_string_hash.c3` (`165`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
    surfaced a JIT mismatch on `map .1 accessor shorthand`
    (`interp=ok, jit=FAIL`).
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
    crashed in `lisp.Env.lookup` with an unaligned-access panic via
    `jit_lookup_var`.

### R5 Continuation Slice AN (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/parser_type_defs.c3` and `src/lisp/parser_type_defs_union.c3`:
  - moved shared copy/compound/variance helpers to
    `src/lisp/parser_type_defs_helpers.c3`:
    - `copy_symbol_list(...)`,
    - `copy_type_field_list(...)`,
    - `copy_union_variant_list(...)`,
    - `parse_compound_symbol_with_params(...)`,
    - `validate_invariant_type_params(...)`.
  - retained type-definition entrypoints in `src/lisp/parser_type_defs.c3`:
    - `init_deftype_expr(...)`,
    - `parse_deftype_name_compound(...)`,
    - `parse_deftype_name(...)`,
    - `parse_deftype_typed_field(...)`,
    - `parse_deftype_bare_field(...)`,
    - `parse_deftype(...)`.
  - retained union-definition entrypoints in `src/lisp/parser_type_defs_union.c3`:
    - `init_defunion_expr(...)`,
    - `parse_defunion_name_compound(...)`,
    - `parse_defunion_name(...)`,
    - `parse_defunion_variant_compound(...)`,
    - `parse_defunion_variant(...)`,
    - `parse_defunion(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_free_vars_walk.c3` (`170`).
- Validation evidence:
  - `c3c build` passed.

### R5 Continuation Slice AO (2026-03-19)

- Performed behavior-preserving extraction on `src/lisp/parser_datum.c3`:
  - moved recursive list/template collection walkers to
    `src/lisp/parser_datum_collections.c3`:
    - `parse_datum_collection_template(...)`,
    - `parse_datum_list(...)`.
  - retained datum/template constructors and dispatch in
    `src/lisp/parser_datum.c3`:
    - `parser_make_nil_datum(...)`,
    - `parser_make_int_datum(...)`,
    - `parser_make_symbol_datum(...)`,
    - `parser_make_string_datum(...)`,
    - `parser_make_quote_datum(...)`,
    - `parser_make_cons_datum(...)`,
    - `parse_datum_template_only(...)`,
    - `parse_datum_string(...)`,
    - `parse_datum_quote(...)`,
    - `parse_datum_impl(...)`,
    - `parse_datum(...)`,
    - `parse_template_datum(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_temp_misc_forms.c3` (`610`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    passed (`122 passed, 0 failed`).

### R5 Continuation Slice AP (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_temp_misc_forms.c3`:
  - moved direct type-form lowering and shared AOT emit helpers to
    `src/lisp/compiler_temp_type_forms.c3`:
    - `emit_bool_literal(...)`,
    - `emit_value_tag_literal(...)`,
    - `emit_target_assignment_prefix(...)`,
    - `emit_string_array_init(...)`,
    - `emit_aot_type_annotation_spec_init(...)`,
    - `compile_typed_define_direct(...)`,
    - `compile_deftype_direct(...)`,
    - `compile_defabstract_direct(...)`,
    - `compile_defunion_direct(...)`,
    - `compile_defalias_direct(...)`,
    - `compile_defeffect_direct(...)`,
    - `compile_type_form_direct(...)`.
  - retained resolve/index and define-bridge helpers in
    `src/lisp/compiler_temp_misc_forms.c3`:
    - `compile_resolve_flat(...)`,
    - `compile_index_flat(...)`,
    - `is_typed_lambda_define(...)`,
    - `compile_define_rhs_with_typed_bridge(...)`,
    - `compile_define_flat(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_temp_type_forms.c3` (`556`).
- Validation evidence:
  - `c3c build` passed.
  - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
    Docker-bounded validation wrapper:
    `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    (`122 passed, 0 failed`).

### R5 Continuation Slice AQ (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_temp_type_forms.c3`:
  - moved shared type-form emit helpers to
    `src/lisp/compiler_temp_type_forms_helpers.c3`:
    - `emit_bool_literal(...)`,
    - `emit_value_tag_literal(...)`,
    - `emit_target_assignment_prefix(...)`,
    - `emit_string_array_init(...)`,
    - `emit_aot_type_annotation_spec_init(...)`.
  - retained direct type-form lowering in
    `src/lisp/compiler_temp_type_forms.c3`:
    - `compile_typed_define_direct(...)`,
    - `compile_deftype_direct(...)`,
    - `compile_defabstract_direct(...)`,
    - `compile_defunion_direct(...)`,
    - `compile_defalias_direct(...)`,
    - `compile_defeffect_direct(...)`,
    - `compile_type_form_direct(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_temp_type_forms.c3` (`400`).
- Validation evidence:
  - `c3c build` passed.
  - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
    Docker-bounded validation wrapper:
    `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    (`122 passed, 0 failed`).

### R5 Continuation Slice AR (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_temp_type_forms.c3`:
  - moved direct type-definition lowering to
    `src/lisp/compiler_temp_type_forms_defs.c3`:
    - `compile_deftype_direct(...)`,
    - `compile_defabstract_direct(...)`,
    - `compile_defunion_direct(...)`,
    - `compile_defalias_direct(...)`,
    - `compile_defeffect_direct(...)`.
  - retained typed-define and type-form dispatch in
    `src/lisp/compiler_temp_type_forms.c3`:
    - `compile_typed_define_direct(...)`,
    - `compile_type_form_direct(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_temp_type_forms_defs.c3` (`342`).
- Validation evidence:
  - `c3c build` passed.
  - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
    Docker-bounded validation wrapper:
    `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    (`122 passed, 0 failed`).

### R5 Continuation Slice AS (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/parser_expr_atoms.c3`:
  - moved dot-accessor and path helpers to
    `src/lisp/parser_expr_atoms_accessors.c3`:
    - `parse_dot_prefixed_int_key(...)`,
    - `parse_dot_accessor_shorthand(...)`,
    - `parse_path_expr(...)`.
  - retained literal parsing and `parse_expr(...)` dispatch in
    `src/lisp/parser_expr_atoms.c3`:
    - integer/float/string/symbol/placeholder parsing,
    - top-level expression token dispatch.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/parser_define_attrs.c3` (`206`).
- Validation evidence:
  - `c3c build` passed.
  - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
    Docker-bounded validation wrapper:
    `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    (`122 passed, 0 failed`).

### R5 Continuation Slice AT (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_primitive_variable_hash_table_domains.c3`:
  - moved collection/math/bitwise primitive registrations to
    `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`.
  - retained arithmetic/comparison/core/string/file/misc primitive
    registrations in `src/lisp/compiler_primitive_variable_hash_table_domains.c3`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_temp_type_forms_defs.c3` (`182`).
- Validation evidence:
  - `c3c build` passed.
  - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
    Docker-bounded validation wrapper:
    `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    (`122 passed, 0 failed`).

### R5 Continuation Slice AU (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_temp_type_forms_defs.c3`:
  - moved alias and effect lowering to
    `src/lisp/compiler_temp_type_forms_defs_misc.c3`:
    - `compile_defalias_direct(...)`,
    - `compile_defeffect_direct(...)`.
  - retained type and abstract lowering in
    `src/lisp/compiler_temp_type_forms_defs.c3`:
    - `compile_deftype_direct(...)`,
    - `compile_defabstract_direct(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/parser_patterns.c3` (`173`).
- Validation evidence:
  - `c3c build` passed.
  - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
    Docker-bounded validation wrapper:
    `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    (`122 passed, 0 failed`).

### R5 Continuation Slice AV (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/parser_patterns.c3`:
  - moved string/sequence/dict/symbol pattern builders to
    `src/lisp/parser_patterns_values.c3`:
    - `parse_string_pattern_literal(...)`,
    - `parse_seq_pattern(...)`,
    - `parse_dict_pattern(...)`,
    - `parse_symbol_pattern(...)`.
  - retained `parse_pattern(...)` dispatch in `src/lisp/parser_patterns.c3`
    plus the existing paren-pattern helpers in
    `src/lisp/parser_patterns_paren.c3`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/parser_collection_literals.c3` (`171`).
- Validation evidence:
  - `c3c build` passed.
  - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
    Docker-bounded validation wrapper:
    `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    (`122 passed, 0 failed`).

### R5 Continuation Slice AW (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/parser_collection_literals.c3`:
  - moved dict/array literal builders to
    `src/lisp/parser_collection_literals_builders.c3`:
    - `parse_dict_literal(...)`,
    - `parse_array_literal(...)`.
  - retained postfix index access and lookup-accessor construction in
    `src/lisp/parser_collection_literals.c3`:
    - `parse_postfix_index(...)`,
    - `build_lookup_accessor(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/compiler_output_helpers.c3` (`171`).
- Validation evidence:
  - `c3c build` passed.
  - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
    Docker-bounded validation wrapper:
    `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    (`122 passed, 0 failed`).

### R5 Continuation Slice AX (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_output_helpers.c3`:
  - moved symbol sanitization and primitive-reference helpers to
    `src/lisp/compiler_output_symbol_helpers.c3`:
    - `is_c3_reserved(...)`,
    - `emit_symbol_name(...)`,
    - `emit_prim_global_name(...)`,
    - `record_prim_ref(...)`.
  - retained generic output emission helpers in
    `src/lisp/compiler_output_helpers.c3`:
    - `emit(...)`, `emit_char(...)`, `emit_escaped(...)`,
    - `emit_string_literal(...)`, `emit_line(...)`,
    - `emit_indent(...)`, `emit_newline(...)`,
    - `emit_int(...)`, `emit_usz(...)`,
    - `get_output(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected: `src/lisp/parser_ffi.c3` (`168`).
- Validation evidence:
  - `c3c build` passed.
  - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
    Docker-bounded validation wrapper:
    `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    (`122 passed, 0 failed`).
