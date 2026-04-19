# compiler-parser-refactor-plan Part 05

Source: `docs/plans/compiler-parser-refactor-plan.md`

### R5 Continuation Slice AY (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/parser_ffi.c3`:
  - moved FFI signature helpers to `src/lisp/parser_ffi_helpers.c3`:
    - `parse_ffi_fn_modifiers(...)`,
    - `set_ffi_c_name(...)`,
    - `ensure_ffi_param_capacity(...)`,
    - `parse_ffi_typed_params(...)`.
  - retained FFI entrypoints in `src/lisp/parser_ffi.c3`:
    - `parse_ffi_lib(...)`,
    - `parse_ffi_fn(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_temp_type_forms_defs_union.c3` (`166`).
- Validation evidence:
  - `c3c build` passed.
  - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
    Docker-bounded validation wrapper:
    `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    (`122 passed, 0 failed`).

### R5 Continuation Batch BB (2026-03-19)

- Performed behavior-preserving extractions:
  - `src/lisp/parser_define_core_helpers.c3` -> helper `src/lisp/parser_define_core_helpers_define.c3`
    - moved shorthand/normal define parsing into the helper file.
    - retained low-level shorthand helpers in the original file.
  - `src/lisp/compiler_program_pipeline.c3` -> helper `src/lisp/compiler_program_pipeline_helpers.c3`
    - moved program parsing and analysis setup into the helper file.
    - retained source assembly and final emission flow in the original file.
  - `src/lisp/compiler_temp_type_forms_defs.c3` -> helper `src/lisp/compiler_temp_type_forms_defs_abstract.c3`
    - moved abstract-type lowering into the helper file.
    - retained direct type-definition lowering in the original file.
- Post-batch queue refresh (largest-first):
  - next extraction target selected:
    `src/lisp/compiler_call_explain_helpers.c3` (`130`).
- Validation evidence:
  - `c3c build` passed.
  - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
    Docker-bounded validation wrapper:
    `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    (`122 passed, 0 failed`).

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

### R5 Continuation Slice BC (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_call_explain_helpers.c3`:
  - moved effect explain lowering helpers to
    `src/lisp/compiler_call_explain_effect_helpers.c3`:
    - `compile_explain_effect_signal_flat(...)`,
    - `compile_explain_effect_resolve_flat(...)`.
  - retained selector dispatch and the
    `compile_explain_flat(...)` entrypoint in
    `src/lisp/compiler_call_explain_helpers.c3`.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/compiler_code_emission.c3` (`127`).
- Validation evidence:
  - `c3c build` passed.
  - Docker-bounded compiler slice passed:
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

### R5 Continuation Slice BD (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_code_emission.c3`:
  - moved lambda-signature, parameter-unpack, capture-binding, and
    body-return helpers to
    `src/lisp/compiler_code_emission_lambda_defs.c3`:
    - `emit_lambda_signature(...)`,
    - `emit_lambda_param_unpack(...)`,
    - `emit_lambda_param_unpack_list(...)`,
    - `emit_lambda_capture_bindings(...)`,
    - `emit_lambda_body_return(...)`.
  - retained prelude, zero-arg classification, capture-struct emission, and
    top-level lambda-definition/lambda-definitions emission in
    `src/lisp/compiler_code_emission.c3`.
- Post-slice queue refresh (largest-first):
  - next extraction target: `src/lisp/compiler_temp_core.c3` (`126`).
- Validation evidence:
  - `c3c build` passed.
  - Docker-bounded compiler slice passed:
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

### R5 Continuation Slice BE (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_temp_core.c3`:
  - moved generic temp helpers to `src/lisp/compiler_temp_helpers.c3`:
    - `emit_temp_decl(...)`,
    - `emit_temp_ref(...)`,
    - `emit_nil_temp(...)`,
    - `compile_leaf_expr_to_temp(...)`.
  - retained temp-id allocation (`next_result(...)`) plus non-tail and tail
    temp dispatch entrypoints in `src/lisp/compiler_temp_core.c3`.
- Post-slice queue refresh (largest-first):
  - next extraction target:
    `src/lisp/compiler_native_call_compilation_flat_style.c3` (`126`).
- Validation evidence:
  - `c3c build` passed.
  - Docker-bounded compiler slice passed:
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

### R5 Continuation Slice BF (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_native_call_compilation_flat_style.c3`:
  - moved variable and path lowering to
    `src/lisp/compiler_native_var_path_compilation_flat_style.c3`:
    - `compile_var(...)`,
    - `compile_path(...)`.
  - retained lambda lowering in
    `src/lisp/compiler_native_call_compilation_flat_style.c3`:
    - `compile_lambda_flat(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target:
    `src/lisp/compiler_mutable_capture_detection.c3` (`126`).
- Validation evidence:
  - `c3c build` passed.
  - Docker-bounded compiler slice passed:
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

### R5 Continuation Slice BG (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_mutable_capture_detection.c3`:
  - moved recursive mutable-capture walk helpers to
    `src/lisp/compiler_mutable_capture_detection_walk.c3`:
    - `has_set_on(...)`,
    - `is_captured_by_nested_lambda(...)`.
  - retained the entrypoint in
    `src/lisp/compiler_mutable_capture_detection.c3`:
    - `is_mutable_capture(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target:
    `src/lisp/compiler_expr_serialize_callable_forms.c3` (`125`).
- Validation evidence:
  - `c3c build` passed.
  - Docker-bounded compiler slice passed:
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

### R5 Continuation Slice BH (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/compiler_expr_serialize_callable_forms.c3`:
  - moved control-form serializers to
    `src/lisp/compiler_expr_serialize_control_match_forms.c3`:
    - `serialize_handle_to_buf(...)`,
    - `serialize_match_to_buf(...)`.
  - retained callable-form serializers in
    `src/lisp/compiler_expr_serialize_callable_forms.c3`:
    - `serialize_lambda_to_buf(...)`,
    - `serialize_let_to_buf(...)`,
    - `serialize_call_to_buf(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target:
    `src/lisp/parser_expr_head_forms.c3` (`122`).
- Validation evidence:
  - `c3c build` passed.
  - Docker-bounded compiler slice passed:
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

### R5 Continuation Slice BI (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/parser_expr_head_forms.c3`:
  - moved symbol-head dispatch helpers to
    `src/lisp/parser_expr_head_symbol_forms.c3`:
    - `parser_symbol_text_is(...)`,
    - `Parser.is_lambda_head_symbol(...)`,
    - `Parser.parse_quasiquote_like_form(...)`,
    - `Parser.parse_symbol_head_form(...)`.
  - retained list-form dispatch in `src/lisp/parser_expr_head_forms.c3`:
    - `Parser.parse_list_form(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target:
    `src/lisp/parser_lexer_symbol_number.c3` (`121`).
  - tied next target:
    `src/lisp/compiler_primitive_variable_hash_table_domains.c3` (`121`).
- Validation evidence:
  - `c3c build` passed.
  - Docker-bounded compiler slice passed:
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

### R5 Continuation Slice BJ (2026-03-19)

- Performed behavior-preserving extraction on
  `src/lisp/parser_lexer_symbol_number.c3`:
  - moved numeric float scanning helpers to
    `src/lisp/parser_lexer_number_helpers.c3`:
    - `Lexer.scan_number_float(...)`,
    - `Lexer.scan_number_fraction(...)`,
    - `Lexer.scan_number_exponent(...)`.
  - retained top-level symbol and integer scanning in
    `src/lisp/parser_lexer_symbol_number.c3`:
    - `Lexer.scan_symbol(...)`,
    - `Lexer.scan_number(...)`.
- Post-slice queue refresh (largest-first):
  - next extraction target:
    `src/lisp/compiler_primitive_variable_hash_table_domains.c3` (`121`).
- Validation evidence:
  - `c3c build` passed.
  - Docker-bounded compiler slice passed:
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

## Validation Gate Per Slice

- [x] `c3c build`
- [x] targeted compiler/parser suite for touched paths
- [x] `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main`

## Exit Criteria

- [x] One active compiler/parser refactor plan remains (`this file`).
- [x] No overlapping active compiler/parser refactor checklists remain in other plan files.
- [x] Area doc and changelog stay synchronized with each landed split slice.
