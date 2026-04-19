# compiler-parser-refactor-plan Part 01

Source: `docs/plans/compiler-parser-refactor-plan.md`

# Compiler/Parser Refactor Consolidation Plan

Status: `complete`
As of: 2026-03-19
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

## Final Queue Snapshot

Post-closure largest-file snapshot (`wc -l src/lisp/compiler*.c3 src/lisp/parser*.c3`, 2026-03-19):

- `src/lisp/compiler_primitive_variable_hash_table_domains.c3` (`121`)
- `src/lisp/parser_patterns_values.c3` (`120`)
- `src/lisp/compiler_quasiquote_flat.c3` (`119`)

These files are retained as-is. At this size, further queue-driven splitting is
not active work.

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
- [x] R7 Close the queue once remaining compiler/parser files fall below the split-worthwhile threshold and the area is no longer structurally blocked.

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
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
    crashed in `lisp.Env.lookup` with an unaligned-access panic via
    `jit_lookup_var`.
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
