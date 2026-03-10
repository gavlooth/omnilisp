# Compiler/Parser Refactor Consolidation Plan

Status: `active`
As of: 2026-03-09
Owner: Codex workflow

## Purpose

Keep compiler/parser modularization tracking in one canonical plan file, with
concrete extraction targets and validation gates.

This plan supersedes compiler/parser refactor tracking overlap that was
previously spread across multiple historical plan documents.

## Canonical Sources

- `memory/CHANGELOG.md` (landed slices + validation evidence)
- `docs/areas/compiler-parser-refactor.md` (area status + next-step summary)
- current `src/lisp/compiler_*.c3` and `src/lisp/parser_*.c3` file layout

## Active Queue

Post-`R5` continuation AL largest-file snapshot (`wc -l src/lisp/compiler*.c3 src/lisp/parser*.c3`, 2026-03-09):

- `src/lisp/parser_application.c3` (`178`)
- `src/lisp/parser_lexer_string_hash.c3` (`175`)
- `src/lisp/parser_type_defs.c3` (`173`)
- `src/lisp/compiler_free_vars_walk.c3` (`170`)

### Checklist

- [x] R1 Establish and document the active split threshold for this cycle (default target: modules over `500` LOC first).
- [x] R2 Split `compiler_expr_serialize_exprs.c3` by expression-family boundaries while preserving serializer parity.
- [x] R3 Split `compiler_native_effect_compilation_flat_style.c3` by effect/native call domains while preserving emitted-code parity.
- [x] R4 Re-run largest-file inventory after `R2/R3`; select next largest compiler/parser module and extract one coherent slice.
- [x] R5 Keep extraction slices behavior-preserving: no semantic rewrites in the same patch as module moves.
- [x] R6 For every landed slice, record:
  - [x] changed file ownership map in this plan,
  - [x] validation commands in `memory/CHANGELOG.md`,
  - [x] area status summary update in `docs/areas/compiler-parser-refactor.md`.

### R1 Threshold Lock (2026-03-09)

- Active split threshold for this cycle is locked to modules with more than
  `500` LOC.
- Execution order is strictly top-down by current largest-file inventory.
- First extraction targets under this lock:
  - `src/lisp/compiler_expr_serialize_exprs.c3` (`521`)
  - `src/lisp/compiler_native_effect_compilation_flat_style.c3` (`518`)
- Scope of this threshold lock is compiler/parser modularization slices only;
  behavior-preservation rule (`R5`) remains mandatory for each extraction patch.

### R2 Slice Landing (2026-03-09)

- Extracted expression-family code from `compiler_expr_serialize_exprs.c3`:
  - callable/control forms moved to
    `src/lisp/compiler_expr_serialize_callable_forms.c3`.
  - definition/module forms moved to
    `src/lisp/compiler_expr_serialize_definition_forms.c3`.
  - `src/lisp/compiler_expr_serialize_exprs.c3` retained as dispatch/entrypoint
    layer (`serialize_expr_to_buf`, specialized switches, shared small helpers).
- Ownership map update for this slice:
  - `compiler_expr_serialize_callable_forms.c3`: `lambda`, `let`, `call`,
    `handle`, `match` form serialization.
  - `compiler_expr_serialize_definition_forms.c3`: `define[type/abstract/union/alias/effect]`,
    `define[macro]`, `module`, `import`, `export-from` serialization.
  - `compiler_expr_serialize_exprs.c3`: tag dispatch and generic expression-form
    utility emitters.
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_type_literals.c3`:
  - moved collection-literal/index parsing to
    `src/lisp/parser_collection_literals.c3`:
    - `parse_postfix_index(...)`,
    - `parse_dict_literal(...)`,
    - `parse_array_literal(...)`.
  - retained type-annotation and constructor-type application parsing in
    `src/lisp/parser_type_literals.c3`.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/parser_type_defs.c3` (`356`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice B (2026-03-09)

- Performed behavior-preserving extraction on `src/lisp/parser_type_defs.c3`:
  - moved union-definition parsing domain to
    `src/lisp/parser_type_defs_union.c3`:
    - `init_defunion_expr(...)`,
    - `parse_defunion_name_compound(...)`,
    - `parse_defunion_name(...)`,
    - `parse_defunion_variant_compound(...)`,
    - `parse_defunion_variant(...)`,
    - `parse_defunion(...)`.
  - retained shared helpers and non-union type forms in
    `src/lisp/parser_type_defs.c3`:
    - symbol/field copy helpers,
    - invariant type-param validation,
    - `parse_deftype(...)`, `parse_defabstract(...)`,
    - `parse_defalias(...)`, `parse_defeffect(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/parser_control_effects.c3` (`352`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice C (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_control_effects.c3`:
  - moved handle-form parsing domain to
    `src/lisp/parser_handle_forms.c3`:
    - `consume_handle_strict_annotation(...)`,
    - `parse_handle_old_clause(...)`,
    - `parse_handle_new_clause(...)`,
    - `parse_handle_clause(...)`,
    - `parse_handle(...)`.
  - retained control/effect parse entrypoints in
    `src/lisp/parser_control_effects.c3` (`explain`, `quote`, `reset`,
    `shift`, `perform`, `resolve`, `with-continuation`, `and`, `or`,
    `begin`).
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/parser_define_core.c3` (`334`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice D (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_define_core.c3`:
  - moved named-let parsing domain to
    `src/lisp/parser_let_named.c3`:
    - `parse_named_let_bindings(...)`,
    - `build_named_let_lambda(...)`,
    - `build_named_let_call(...)`,
    - `parse_named_let(...)`.
  - retained define-family parse flow in `src/lisp/parser_define_core.c3`:
    - shorthand-define parsing and lambda finalization,
    - bracket-annotation dispatch (`type` / `ffi` / special),
    - `parse_define(...)`, `parse_define_type(...)`, `parse_define_ffi(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/compiler_temp_core.c3` (`307`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice E (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_temp_core.c3`:
  - moved temp control-flow helpers to
    `src/lisp/compiler_temp_control_flow.c3`:
    - `emit_temp_assign(...)`,
    - `compile_branch_assign(...)`, `compile_tail_branch_assign(...)`,
    - `compile_begin_prefix(...)`,
    - `compile_short_circuit(...)`, `compile_tail_short_circuit(...)`,
    - `compile_if_common(...)`, `compile_begin_common(...)`,
    - `compile_tail_if_flat(...)`, `compile_tail_begin_flat(...)`,
    - `compile_tail_and_flat(...)`, `compile_tail_or_flat(...)`.
  - retained temp dispatch and scalar/simple-form lowering in
    `src/lisp/compiler_temp_core.c3`:
    - temp id/ref primitives and leaf/define/index/resolve helpers,
    - `compile_to_temp_non_null(...)`,
    - `compile_to_temp(...)`, `compile_to_temp_tail(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/compiler_lambda_scan.c3` (`295`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice F (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_lambda_scan.c3`:
  - moved effect-wrapper lambda scan helpers to
    `src/lisp/compiler_lambda_scan_effect_wrappers.c3`:
    - `make_synthetic_lambda(...)`,
    - `make_pair_projection_app(...)`,
    - `wrap_handle_clause_body(...)`,
    - `scan_lambdas_reset(...)`,
    - `scan_lambdas_shift(...)`,
    - `scan_lambdas_handle(...)`.
  - retained lambda-scan core traversal/dispatch in
    `src/lisp/compiler_lambda_scan.c3`:
    - bound/capture collection and lambda-def initialization,
    - generic tree traversal helpers (`binary` / `if` / `match` / `begin` / `module`),
    - `scan_lambdas_with_scope(...)` tag dispatch.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/parser_import_export.c3` (`290`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice G (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_import_export.c3`:
  - moved export-from parsing domain to
    `src/lisp/parser_export_from.c3`:
    - `init_export_from_expr(...)`,
    - `parse_export_from_source_module(...)`,
    - `ensure_export_from_name_capacity(...)`,
    - `parse_export_from_name_list(...)`,
    - `parse_export_from_specifiers(...)`,
    - `parse_export_from(...)`.
  - retained import parsing domain in
    `src/lisp/parser_import_export.c3`:
    - import target/specifier parsing and selective-list helpers,
    - `parse_import(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/compiler_code_emission.c3` (`281`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice H (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_code_emission.c3`:
  - moved global/main emission domain to
    `src/lisp/compiler_code_emission_main_globals.c3`:
    - `emit_global_declarations(...)`,
    - `emit_global_value_declarations(...)`,
    - `emit_cached_primitive_declarations(...)`,
    - `emit_main_start(...)`,
    - `emit_main_end(...)`.
  - retained lambda/closure emission domain in
    `src/lisp/compiler_code_emission.c3`:
    - lambda signature/param/capture/body emitters,
    - closure construction helpers and `emit_lambda_return(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/parser_quasiquote_datum.c3` (`280`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice I (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_quasiquote_datum.c3`:
  - moved datum parsing and datum-construction helpers to
    `src/lisp/parser_datum.c3`:
    - `parser_make_*_datum(...)` helpers,
    - `parse_datum_impl(...)`,
    - `parse_datum(...)`,
    - `parse_template_datum(...)`.
  - retained quasiquote-template expression parsing in
    `src/lisp/parser_quasiquote_datum.c3`:
    - `parse_qq_*` helper forms,
    - `parse_qq_template(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/parser_type_defs.c3` (`271`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice J (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_type_defs.c3`:
  - moved non-deftype type-definition forms to
    `src/lisp/parser_type_defs_misc.c3`:
    - `parse_defabstract(...)`,
    - `parse_defalias(...)`,
    - `parse_defeffect(...)`.
  - retained shared helper and deftype domain in
    `src/lisp/parser_type_defs.c3`:
    - copy/compound helper routines,
    - `validate_invariant_type_params(...)`,
    - `parse_deftype(...)` and its sub-helpers.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/parser_pattern_match.c3` (`264`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Continuation Slice K (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/parser_pattern_match.c3`:
  - moved pattern-node parsing domain to
    `src/lisp/parser_patterns.c3`:
    - `parse_pattern(...)`,
    - sequence/dict/paren/as-marker/string pattern helpers.
  - retained `match` expression entry parsing in
    `src/lisp/parser_pattern_match.c3`:
    - `parse_match(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/parser_type_literals.c3` (`262`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

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

### R3 Slice Landing (2026-03-09)

- Split `src/lisp/compiler_native_effect_compilation_flat_style.c3` by
  effect/native-call domains:
  - retained effect-specific lowering in
    `src/lisp/compiler_native_effect_compilation_flat_style.c3`
    (`reset/shift/perform/handle` helpers + compilation flow).
  - moved native-call/literal/match-path lowering to
    `src/lisp/compiler_native_call_compilation_flat_style.c3`.
- Ownership map update for this slice:
  - `compiler_native_effect_compilation_flat_style.c3`: effect control-flow
    lowering (`compile_*_flat` for `reset`, `shift`, `perform`, `handle`).
  - `compiler_native_call_compilation_flat_style.c3`: lambda, match, literal,
    variable, quote, path lowering and related pattern helpers.
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R4 Slice Landing (2026-03-09)

- Re-ran largest-file inventory after `R2/R3`, identified
  `src/lisp/compiler_native_call_compilation_flat_style.c3` (`396`) as next
  largest compiler/parser module.
- Extracted one coherent slice (match/pattern lowering):
  - added `src/lisp/compiler_native_match_compilation_flat_style.c3`.
  - reduced `src/lisp/compiler_native_call_compilation_flat_style.c3` to
    lambda/literal/var/quote/path lowering responsibilities.
- Post-slice ownership map update:
  - `compiler_native_match_compilation_flat_style.c3`: `compile_match_flat`,
    pattern checks/bindings, and pattern helper emitters.
  - `compiler_native_call_compilation_flat_style.c3`: general native call/value
    lowering outside effect/match domains.
- Next extraction target selected from refreshed queue:
  - `src/lisp/compiler_free_vars_walk.c3` (`376`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

### R5 Slice Landing (2026-03-09)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_free_vars_walk.c3`:
  - moved scope/form-specific free-variable walkers to
    `src/lisp/compiler_free_vars_scope_forms.c3`:
    - `find_free_vars_var`, `find_free_vars_lambda`, `find_free_vars_let`,
      `find_free_vars_match`, `find_free_vars_call`, `find_free_vars_path`,
      `find_free_vars_shift`, `find_free_vars_resolve`,
      `find_free_vars_handle`, `find_free_vars_set`.
  - retained dispatch and quasiquote traversal in
    `src/lisp/compiler_free_vars_walk.c3`:
    - pair/triple/list helper walkers,
    - unary/binary/complex-form dispatch,
    - `find_free_vars(...)`, `find_free_vars_in_qq(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/parser_type_literals.c3` (`362`).
- Validation evidence:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed
    (`unified: 1697/0`, `compiler: 85/0`).

## Validation Gate Per Slice

- [x] `c3c build`
- [x] targeted compiler/parser suite for touched paths
- [x] `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main`

## Exit Criteria

- [x] One active compiler/parser refactor plan remains (`this file`).
- [x] No overlapping active compiler/parser refactor checklists remain in other plan files.
- [x] Area doc and changelog stay synchronized with each landed split slice.
