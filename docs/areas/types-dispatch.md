# Type System and Dispatch

Status: `green` (core parity matrix/explainability/type-gap/backend-matrix closure complete; current direct-AOT lowering state synchronized)  
As of: 2026-03-13

## Canonical Sources

- `memory/CHANGELOG.md` (primary current-state source of truth)
- `docs/type-system-syntax.md`
- `docs/LANGUAGE_SPEC.md`:
  - `#04-advanced-omni-profile` (advanced model boundaries)
  - `#06-pitfalls-guide` (truthiness and dispatch-adjacent pitfalls)
  - `#4-type-system` and `#5-multiple-dispatch` (normative surface)
  - `#55-dispatch-explainability` (dispatch explain output shape)
- `docs/plans/effects-typesystem-parity-plan.md` (Phase 4 and related items)

## Current State

- Language spec presents structural type system and multiple dispatch model.
- Runtime and parser include type/dispatch infrastructure used by current tests.
- Julia-parity matrix is explicit and currently has no `missing` rows.
- Runtime dispatch semantics are test-anchored for ambiguity, unification, union participation, invariant variance policy, and explicit numeric conversion.
- Explainability tooling is implemented with canonical selector syntax and deterministic structured output (`explain 'dispatch`, `explain 'effect`).
- Backend parity audit (`L4.1`, 2026-03-09) is complete and documented below.
- Backend parity implementation bridge (`L4.2`, 2026-03-09) is landed:
  - compiler now lowers type-definition forms and typed lambda `define` forms through direct structured AOT helpers instead of delegated `aot::eval_serialized_expr(...)`,
  - compiled `explain 'dispatch` and canonical `explain 'effect` signal/resolve forms also lower through direct AOT helpers instead of delegated eval,
  - generated call sites continue to route through `aot::invoke/apply_multi` into `jit_apply*`, preserving method-table dispatch semantics.
- Backend parity E2E coverage slice (`L4.3`, 2026-03-09) is landed:
  - `src/lisp/tests_e2e_generation_setups.c3` includes `[abstract]`, `[struct]`, `[union]`, `[alias]` fixtures and dispatch setup for exact/subtype/value-literal paths with explicit numeric conversion.
  - `src/lisp/tests_e2e_generation_cases_extended.c3` includes compiler `--build` parity cases for constructor usage, dispatch specificity tiers, alias-annotation dispatch, and explain-based dispatch diagnostics.
  - compiler path hardening added for this coverage:
    - constructor/global sync after direct type-form lowering in `src/lisp/compiler_program_pipeline.c3`,
    - explain-form direct helper lowering + canonical source capture in `src/lisp/compiler_call_flat.c3`,
    - primitive hash mapping for `explain`/`schema-explain` in `src/lisp/compiler_primitive_variable_hash_table.c3`.
- Backend matrix docs closure (`L4.4`, 2026-03-09) is landed:
  - `docs/LANGUAGE_SPEC.md` Appendix C now marks compiler support for type definitions and dispatch as `Y` with an implementation note (no `eval*` caveat row).
  - L4 acceptance gate (`A-L4`) is documentation-closed against validated parity evidence from L4.2/L4.3.

## L4.1 Audit Snapshot (2026-03-09)

- Compiler call sites in generated C3 currently route through AOT wrappers:
  - `src/lisp/compiler_call_flat.c3` emits `aot::invoke`, `aot::apply_multi`, and tail variants.
- Runtime dispatch boundary for compiled binaries is:
  - `src/lisp/aot.c3`: `invoke_once`/`apply_multi_once` route non-AOT callables to `jit_apply_value` / `jit_apply_multi_args`.
- Interpreter/JIT type-definition path is explicit evaluator delegation:
  - `src/lisp/jit_jit_compile_effects_modules.c3`: `jit_compile_deftype/defabstract/defunion/defalias/defeffect` lower via `jit_compile_3arg_helper`.
  - `src/lisp/jit_jit_dispatch_helpers.c3`: `jit_do_deftype/defabstract/defunion/defalias/defeffect` call `eval_def*`.
- Compiler/AOT gap identified by audit:
  - `src/lisp/compiler_temp_core.c3` does not lower `E_DEFTYPE/E_DEFABSTRACT/E_DEFUNION/E_DEFALIAS` in `compile_to_temp_non_null`.
  - Fallback path (`src/lisp/compiler_expression_compilation.c3`) emits `aot::make_nil() /* WARNING: unsupported expr type */`.
  - Repeated typed `define` names are not method-table merged in AOT compile path and can produce duplicate global declarations (`describe` shadowing) instead of dispatch-table extension.

## Known Gaps

- No open constructor/lambda type-gap items remain in `L3`.
- `scripts/run_e2e.sh` still has a known non-L4.3 baseline diff set (legacy continuation/closure/truthiness/dict rows). This set is now explicitly documented and unchanged relative to prior baseline snapshot.

## L5 Closeout Evidence (2026-03-09)

- `scripts/run_e2e.sh` rerun confirms the existing baseline diff set persists, with no new L4.3 parity regressions introduced.
- Baseline diff rows (line-mapped) include:
  - continuation/effect behavior rows (`reset`/`shift`/`handle` cases),
  - truthiness/predicate rows (`and` + `closure?`),
  - dict rows (`dict` read/has/keys),
  - closure introspection row (`type-of closure`).
- L4 backend parity rows remain aligned:
  - type ctor rows (`type struct ctor`, `type union ctor`),
  - dispatch rows (`dispatch exact subtype`, `dispatch parent subtype`, `dispatch numeric explicit conversion`, `dispatch value literal exact`, `dispatch value literal fallback`),
  - explainability row (`dispatch ambiguity reason`).
- Baseline stability check:
  - `build/e2e_diff.txt` is identical to prior stored baseline snapshot (`build/e2e_diff_notests.txt`).

## Next Steps

1. Keep language spec, area status, and parity matrix synchronized release-by-release.
2. If the legacy `run_e2e.sh` baseline rows change, update this area page + `memory/CHANGELOG.md` with an explicit row-level diff delta.
3. Treat any new mismatch in L4 parity rows as release-blocking regression.
