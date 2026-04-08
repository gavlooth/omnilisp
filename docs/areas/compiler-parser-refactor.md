# Compiler, Parser, and Refactor Structure

Status: `green` (major split landed; remaining compiler/parser files are below the active split threshold and this lane is no longer an active work queue)  
As of: 2026-03-19

## Canonical Sources

- `memory/CHANGELOG.md` (execution history + validated changes)
- `docs/plans/compiler-parser-refactor-plan.md` (single active plan for this area)
- current `src/lisp/*` module/file layout

## Current State

- Large monolith files have already been split into area-scoped files (`compiler_*`, `parser_*`, `jit_*`, `tests_*`, `value_*`, `eval_*`).
- The codebase structure is significantly ahead of older "god-file" descriptions.
- Historical extraction record remains centralized in `docs/plans/compiler-parser-refactor-plan.md`.
- Active cycle threshold is now locked to modules over `500` LOC, executed in strict largest-first order (`R1` complete).
- `R2` landed by splitting `compiler_expr_serialize_exprs.c3` into expression-family modules (`compiler_expr_serialize_callable_forms.c3`, `compiler_expr_serialize_definition_forms.c3`) while keeping dispatch in `compiler_expr_serialize_exprs.c3`.
- `R3` landed by splitting effect lowering from native-call lowering (`compiler_native_effect_compilation_flat_style.c3` + `compiler_native_call_compilation_flat_style.c3`).
- `R4` landed with one coherent follow-up slice: match/pattern lowering extracted to `compiler_native_match_compilation_flat_style.c3`; queue refreshed with `compiler_free_vars_walk.c3` as next target.
- `R5` landed with behavior-preserving free-variable walker extraction: scope/form walkers moved to `compiler_free_vars_scope_forms.c3`, while dispatch/qq traversal remained in `compiler_free_vars_walk.c3`.
- `R5` continuation slice landed on parser literals: collection/index parsing moved to `parser_collection_literals.c3`, leaving type-annotation/type-application parsing in `parser_type_literals.c3`.
- `R5` continuation slice B landed on parser type definitions: union parsing moved to `parser_type_defs_union.c3`, leaving shared helpers and non-union type forms in `parser_type_defs.c3`.
- `R5` continuation slice C landed on parser control/effects: handle-form parsing moved to `parser_handle_forms.c3`, while `parser_control_effects.c3` retained non-handle control/effect form entrypoints.
- `R5` continuation slice D landed on define/let boundary cleanup: named-let parsing moved to `parser_let_named.c3`, while `parser_define_core.c3` retained define-family parsing and bracket-annotation dispatch.
- `R5` continuation slice E landed on compiler temp control flow: short-circuit/if/block temp helpers moved to `compiler_temp_control_flow.c3`, while `compiler_temp_core.c3` retained temp dispatch and scalar/simple-form lowering.
- `R5` continuation slice F landed on compiler lambda scan effect wrapping: reset/shift/handle synthetic-wrapper helpers moved to `compiler_lambda_scan_effect_wrappers.c3`, while `compiler_lambda_scan.c3` retained capture/traversal dispatch logic.
- `R5` continuation slice G landed on parser import/export split: export-from parsing moved to `parser_export_from.c3`, while `parser_import_export.c3` retained import-target/specifier parsing.
- `R5` continuation slice H landed on compiler code emission split: global/main emission helpers moved to `compiler_code_emission_main_globals.c3`, while `compiler_code_emission.c3` retained lambda/closure emission logic.
- `R5` continuation slice I landed on parser datum/quasiquote split: datum parsing/construction moved to `parser_datum.c3`, while `parser_quasiquote_datum.c3` retained quasiquote-template expression parsing.
- `R5` continuation slice J landed on parser type-defs split: abstract/alias/effect parsing moved to `parser_type_defs_misc.c3`, while `parser_type_defs.c3` retained shared helpers and deftype parsing.
- `R5` continuation slice K landed on parser pattern split: pattern-node parsing moved to `parser_patterns.c3`, while `parser_pattern_match.c3` retained `parse_match(...)`.
- `R5` continuation slice L landed on parser type-annotation split: type-annotation parsing moved to `parser_type_annotations.c3`, while `parser_type_literals.c3` retained constructor type-application parsing.
- `R5` continuation slice M landed on compiler serializer split: pattern serialization moved to `compiler_expr_serialize_patterns.c3`, while `compiler_expr_serialize_values.c3` retained value/type-annotation serialization.
- `R5` continuation slice N landed on parser lexer split: whitespace/comment scanning moved to `parser_lexer_whitespace.c3`, while `parser_lexer.c3` retained lexer lifecycle and token-advance dispatch flow.
- `R5` continuation slice O landed on compiler program pipeline split: top-level define/global sync + expression emission moved to `compiler_program_top_level.c3`, while `compiler_program_pipeline.c3` retained pipeline orchestration.
- `R5` continuation slice P landed on parser control/effects split: explain-form parsing moved to `parser_explain.c3`, while `parser_control_effects.c3` retained other control/effect forms.
- `R5` continuation slice Q landed on parser define split: bracket-annotation define dispatch moved to `parser_define_annotations.c3`, while `parser_define_core.c3` retained shorthand/normal define parsing flow.
- `R5` continuation slice R landed on parser expr-atoms split: reader-shorthand/reader-dispatch helpers moved to `parser_expr_reader_forms.c3`, while `parser_expr_atoms.c3` retained core atom parsing and `parse_expr(...)` dispatch.
- `R5` continuation slice S landed on parser patterns split: paren-pattern helpers moved to `parser_patterns_paren.c3`, while `parser_patterns.c3` retained non-paren pattern parsing and token dispatch.
- `R5` continuation slice T landed on parser lexer split: lifecycle/token API helpers moved to `parser_lexer_core_api.c3`, while `parser_lexer.c3` retained literal/symbol scan and token-advance dispatch flow.
- `R5` continuation slice U landed on parser lambda split: shared callable/body helpers moved to `parser_callable_helpers.c3`, while `parser_lambda.c3` retained lambda/if entry parsing.
- `R5` continuation slice V landed on compiler code-emission split: closure-construction/lambda-return helpers moved to `compiler_code_emission_lambda_closures.c3`, while `compiler_code_emission.c3` retained prelude and lambda-definition emission flow.
- `R5` continuation slice W landed on parser type-annotation split: helper routines moved to `parser_type_annotation_helpers.c3`, while `parser_type_annotations.c3` retained compound/dict/top-level type-annotation parsing flow.
- `R5` continuation slice X landed on compiler free-vars split: effect/control walkers moved to `compiler_free_vars_effect_forms.c3`, while `compiler_free_vars_scope_forms.c3` retained scope/mutation walkers.
- `R5` continuation slice Y landed on compiler native-call split: literal/quote lowering moved to `compiler_native_literal_compilation_flat_style.c3`, while `compiler_native_call_compilation_flat_style.c3` retained lambda/var/path lowering.
- `R5` continuation slice Z landed on compiler serializer split: type-annotation serialization helpers moved to `compiler_expr_serialize_type_annotations.c3`, while `compiler_expr_serialize_values.c3` retained value-domain serialization helpers.
- `R5` continuation slice AA landed on compiler let/set split: let-lowering helpers and entrypoints moved to `compiler_let_compilation_flat_style.c3`, while `compiler_let_set_flat.c3` retained control/set/primitive-check helpers.
- `R5` continuation slice AB landed on compiler lambda-scan split: lambda-def/capture assembly helpers moved to `compiler_lambda_scan_lambda_defs.c3`, while `compiler_lambda_scan.c3` retained traversal/dispatch helpers.
- `R5` continuation slice AC landed on compiler expr-serializer split: specialized expression-form helpers moved to `compiler_expr_serialize_special_forms.c3`, while `compiler_expr_serialize_exprs.c3` retained entrypoint dispatch.
- `R5` continuation slice AD landed on compiler definition-serializer split: macro/module/import/export serializers moved to `compiler_expr_serialize_macro_module_forms.c3`, while `compiler_expr_serialize_definition_forms.c3` retained type/effect definition serializers.
- `R5` continuation slice AE landed on parser import split: import helper routines moved to `parser_import_helpers.c3`, while `parser_import_export.c3` retained `parse_import(...)` entrypoint parsing.
- `R5` continuation slice AF landed on compiler temp-core split: resolve/index/define bridge helpers moved to `compiler_temp_misc_forms.c3`, while `compiler_temp_core.c3` retained temp lifecycle and non-tail/tail dispatch entrypoints.
- `R5` continuation slice AG landed on compiler quasiquote split: quasiquote call/splice lowering helpers moved to `compiler_quasiquote_call_flat.c3`, while `compiler_quasiquote_flat.c3` retained non-call quasiquote lowering and `compile_qq_flat(...)`.
- `R5` continuation slice AH landed on primitive-hash split: primitive-domain population helpers moved to `compiler_primitive_variable_hash_table_domains.c3`, while `compiler_primitive_variable_hash_table.c3` retained hash storage/probe/init orchestration.
- `R5` continuation slice AI landed on compiler native-match split: pattern-binding emit helpers moved to `compiler_native_match_bindings_flat_style.c3`, while `compiler_native_match_compilation_flat_style.c3` retained match entrypoint and pattern-check lowering.
- `R5` continuation slice AJ landed on compiler call split: call arg/list construction helpers moved to `compiler_call_arg_list_helpers.c3`, while `compiler_call_flat.c3` retained call/app entrypoint lowering.
- `R5` continuation slice AK landed on parser define-attrs split: relation-attribute define helpers moved to `parser_define_relation_attr.c3`, while `parser_define_attrs.c3` retained macro/schema/special define dispatch.
- `R5` continuation slice AL landed on parser control/effects split: effect-form parse helpers moved to `parser_effect_forms.c3`, while `parser_control_effects.c3` retained quote/and/or/block parsing.
- `R5` continuation slice AM landed on parser application split: shared call-construction and argument-collection helpers moved to `parser_application_helpers.c3`, while `parser_application.c3` retained application dispatch and placeholder lowering.
- `R5` continuation slice AN landed on parser type-defs split: shared copy/compound/variance helpers moved to `parser_type_defs_helpers.c3`, while `parser_type_defs.c3` and `parser_type_defs_union.c3` retained the form-specific entrypoints.
- `R5` continuation slice AO landed on parser datum split: recursive list/template collection walkers moved to `parser_datum_collections.c3`, while `parser_datum.c3` retained datum/template constructors and dispatch.
- `R5` continuation slice AP landed on compiler temp type-forms split: direct AOT type lowering and annotation emit helpers moved to `compiler_temp_type_forms.c3`, while `compiler_temp_misc_forms.c3` retained resolve/index and define bridge helpers.
- `R5` continuation slice AQ landed on compiler temp type-forms split: shared emit helpers moved to `compiler_temp_type_forms_helpers.c3`, while `compiler_temp_type_forms.c3` retained direct type-form lowering.
- `R5` continuation slice AR landed on compiler temp type-forms split: direct type-definition lowering moved to `compiler_temp_type_forms_defs.c3`, while `compiler_temp_type_forms.c3` retained typed-define and type-form dispatch.
- `R5` continuation slice AS landed on parser expr-atoms split: dot-accessor and path helpers moved to `parser_expr_atoms_accessors.c3`, while `parser_expr_atoms.c3` retained literal parsing and `parse_expr(...)` dispatch.
- `R5` continuation slice AT landed on primitive-hash split: collection/math/bitwise primitive registrations moved to `compiler_primitive_variable_hash_table_domains_collections.c3`, while `compiler_primitive_variable_hash_table_domains.c3` retained the remaining primitive registrations.
- `R5` continuation slice AU landed on compiler temp type-forms split: alias/effect lowering moved to `compiler_temp_type_forms_defs_misc.c3`, while `compiler_temp_type_forms_defs.c3` retained type and abstract lowering.
- `R5` continuation slice AV landed on parser patterns split: string/sequence/dict/symbol pattern builders moved to `parser_patterns_values.c3`, while `parser_patterns.c3` retained pattern dispatch and `parser_patterns_paren.c3` retained paren-pattern helpers.
- `R5` continuation slice AW landed on parser collection-literals split: dict/array literal builders moved to `parser_collection_literals_builders.c3`, while `parser_collection_literals.c3` retained postfix index access and lookup-accessor construction.
- `R5` continuation slice AX landed on compiler output-helpers split: symbol sanitization and primitive-reference helpers moved to `compiler_output_symbol_helpers.c3`, while `compiler_output_helpers.c3` retained generic output emission helpers.
- `R5` continuation slice AY landed on parser FFI split: FFI signature helpers moved to `parser_ffi_helpers.c3`, while `parser_ffi.c3` retained FFI entrypoints.
- `R5` continuation batch BB landed three more behavior-preserving extractions:
  - shorthand/normal define parsing moved to `parser_define_core_helpers_define.c3`,
  - program parsing and analysis setup moved to `compiler_program_pipeline_helpers.c3`,
  - abstract-type lowering moved to `compiler_temp_type_forms_defs_abstract.c3`.
- `R5` continuation slice BC landed on compiler call/explain split:
  - effect-signal and resolve explain lowering moved to `compiler_call_explain_effect_helpers.c3`,
  - `compiler_call_explain_helpers.c3` now stays as the selector-dispatch and `compile_explain_flat(...)` entrypoint layer.
- `R5` continuation slice BD landed on compiler code-emission split:
  - lambda-signature, parameter-unpack, capture-binding, and body-return helpers moved to `compiler_code_emission_lambda_defs.c3`,
  - `compiler_code_emission.c3` now stays as the prelude/capture-struct and top-level lambda-definition emission layer.
- `R5` continuation slice BE landed on compiler temp-core split:
  - generic temp declaration/reference/nil/leaf helpers moved to `compiler_temp_helpers.c3`,
  - `compiler_temp_core.c3` now stays as the temp-id allocator plus non-tail and tail temp-dispatch layer.
- `R5` continuation slice BF landed on compiler native-call split:
  - variable and path lowering moved to `compiler_native_var_path_compilation_flat_style.c3`,
  - `compiler_native_call_compilation_flat_style.c3` now stays as the lambda-lowering file.
- `R5` continuation slice BG landed on compiler mutable-capture split:
  - recursive `set!` / nested-lambda capture walkers moved to `compiler_mutable_capture_detection_walk.c3`,
  - `compiler_mutable_capture_detection.c3` now stays as the `is_mutable_capture(...)` entrypoint layer.
- `R5` continuation slice BH landed on compiler callable-serializer split:
  - handle and match serialization moved to `compiler_expr_serialize_control_match_forms.c3`,
  - `compiler_expr_serialize_callable_forms.c3` now stays as the lambda/let/call serializer file.
- `R5` continuation slice BI landed on parser expr-head split:
  - symbol-head special-form helpers moved to `parser_expr_head_symbol_forms.c3`,
  - `parser_expr_head_forms.c3` now stays as the `parse_list_form(...)` dispatch file.
- `R5` continuation slice BJ landed on parser lexer symbol/number split:
  - float-literal scanning helpers moved to `parser_lexer_number_helpers.c3`,
  - `parser_lexer_symbol_number.c3` now stays as the symbol and integer scanner file.
- Compiler/parser split queue is now closed:
  - the remaining largest compiler/parser files are in the `115–121` LOC range,
  - they are below the threshold where more splitting is paying for itself,
  - further work in this area should be correctness or feature driven, not queue-driven file splitting.
- Plan-governance closure is now explicit:
  - one historical compiler/parser tracker remains (`docs/plans/compiler-parser-refactor-plan.md`),
  - historical plan files (for example `docs/plans/aot-unification.md`) are
    reference-only and not active checklist surfaces for compiler/parser
    splits.

## Known Drift

- Legacy monolith-era docs (`docs/COMPILER.md`, `docs/REFACTOR_PLAN.md`,
  `docs/C3_MIGRATION.md`) were removed and should not be recreated as active
  status artifacts.
- Historical plan files that contain prior refactor slices (for example
  `docs/plans/aot-unification.md`) remain useful as implementation history but
  are no longer active tracking surfaces for this area.

## Next Steps

1. Keep this area closed unless a future compiler/parser file grows materially again or a correctness change justifies a structural move.
2. Treat `docs/plans/compiler-parser-refactor-plan.md` as a historical execution record, not an active queue.
3. If compiler/parser structure changes materially in the future, update `memory/CHANGELOG.md` first and then refresh this area summary.
