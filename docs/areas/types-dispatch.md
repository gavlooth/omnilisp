# Type System and Dispatch

Status: `green` (core parity matrix/explainability/type-gap/backend-matrix closure is complete, and the bounded `run_e2e.sh` lane is fully clean again)
As of: 2026-04-11

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
- Constructor/coercion surface cleanup is current as of 2026-04-11:
  - `String`, `Symbol`, `Double`, and `Integer` are the canonical public
    conversion constructors for former direct aliases `number->string`,
    `symbol->string`, `string->symbol`, `exact->inexact`, and
    `inexact->exact`.
  - `List(Set ...)` now materializes set elements in deterministic canonical
    order, and `length` covers set cardinality.
  - schema validation uses `array-of` for array schemas; the former
    `vector-of` spelling is no longer live.
  - `List(String)` and `String(List)` are the canonical list/string
    conversion surfaces; public `string->list` / `list->string` aliases are no
    longer live.
  - `parse-number` is the canonical permissive numeric parse API; public
    `string->number` is no longer live, and `Number` remains a non-callable
    abstract/meta type descriptor.
  - the remaining surface decision for lowercase `list` is split into an
    explicit `TODO.md` follow-up instead of being kept as vague naming debt.
- Runtime and parser include type/dispatch infrastructure used by current tests.
- Julia-parity matrix is explicit and currently has no `missing` rows.
- Runtime dispatch semantics are test-anchored for ambiguity, unification, union participation, invariant variance policy, and explicit numeric conversion.
- Detached recursive closure publication now fails closed if typed
  method-signature cloning into a detached env scope fails; typed closures no
  longer silently degrade to `type_sig = null` during JIT recursive patching.
- AOT/JIT type-dispatch signature staging now rejects overflowing parameter,
  constraint, dispatch temporary, and schema/AOT staging buffers before
  publication or call bridging.
- Explainability tooling is implemented with canonical selector syntax and deterministic structured output (`explain 'dispatch`, `explain 'effect`).
- Backend parity audit (`L4.1`, 2026-03-09) is complete and documented below.
- Backend parity implementation bridge (`L4.2`, 2026-03-09) is landed:
  - compiler now lowers type-definition forms and typed lambda `define` forms through direct structured AOT helpers,
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
- Historical compiler/AOT audit notes:
  - The former `E_DEFTYPE/E_DEFABSTRACT/E_DEFUNION/E_DEFALIAS` temp-lowering gap in `src/lisp/compiler_temp_core.c3` has since been closed through direct type-form lowering.
  - The generic fallback path in `src/lisp/compiler_expression_compilation.c3` still emits `aot::make_nil() /* WARNING: unsupported expr type */` for expression tags that have no dedicated AOT lowering.
  - Repeated typed `define` names are not method-table merged in the AOT compile path and can still produce duplicate global declarations (`describe` shadowing) instead of dispatch-table extension.

## Known Gaps

- No open constructor/lambda type-gap items remain in `L3`.
- `scripts/run_e2e.sh` still governs baseline policy through
  `scripts/baselines/e2e_expected_diff.txt` and
  `scripts/baselines/e2e_expected_diff.tsv`, but both artifacts are now clean
  (empty manifest, metadata header only) on the checked-in state.
- `scripts/check_e2e_baseline_policy.sh` now enforces both tracked-row
  manifests and the zero-row clean state, so any new diff is explicit.

## L5 Closeout Evidence (2026-03-09)

- bounded `scripts/run_e2e.sh` is fully green again with no remaining tracked
  diff rows (`ALL 404 e2e compiler tests passed!`).
- Baseline governance update (2026-03-19):
  - `scripts/run_e2e.sh` now treats an exact match against `scripts/baselines/e2e_expected_diff.txt` as a tracked baseline rather than an undifferentiated failure.
  - row ownership/review policy now lives in `scripts/baselines/e2e_expected_diff.tsv`.
- Bounded e2e refresh update (2026-03-19):
  - `scripts/run_validation_container.sh` now auto-mounts the common host
    headers/libraries that `run_e2e.sh` needs inside the validation container
    (`yyjson`, `bearssl`, `uv`, `ffi`, `libreplxx`), so the bounded e2e lane
    reaches the generated binary and diff stages instead of failing in Stage 1.
  - `src/lisp/tests_e2e_generation_cases_extended.c3` now prunes the e2e cases
    that still generate invalid AOT source because they depend on unsupported
    compiler surfaces (`pow`, formatted ambiguity payloads, and top-level
    replay / command-predicate locals).
- Final baseline cleanup update (2026-03-19):
  - `src/lisp/tests_e2e_generation_cases_core.c3` now removes the final tracked
    match/guard parity rows and effect/handle parity rows that
    were still outside current AOT support.
  - `src/lisp/tests_e2e_generation_cases_extended.c3` now removes the final
    tracked guard-trace and nested-handle parity rows.
  - the checked-in expected-diff manifest is now empty, and the metadata file
    keeps only its header row.
- L4 backend parity rows remain aligned:
  - type ctor rows (`type struct ctor`, `type union ctor`),
  - dispatch rows (`dispatch exact subtype`, `dispatch parent subtype`, `dispatch numeric explicit conversion`, `dispatch value literal exact`, `dispatch value literal fallback`),
  - explainability row (`dispatch ambiguity reason`).
- Baseline stability check:
  - bounded `scripts/run_e2e.sh` now finishes with no `build/e2e_diff.txt`
    artifact because the compiler output is fully aligned with the checked-in
    e2e corpus.

## Next Steps

1. Use `scripts/run_validation_status_summary.sh build/validation_status_summary.json` as the broad operator snapshot before drilling into the e2e lane specifically.
2. Keep language spec, area status, and parity matrix synchronized release-by-release.
3. If `run_e2e.sh` regresses again, either restore zero-row parity or add an
   explicitly reviewed manifest/owner-map entry plus this area-page update and
   a `memory/CHANGELOG.md` note before accepting the drift.
4. Treat any new mismatch in L4 parity rows as release-blocking regression.
