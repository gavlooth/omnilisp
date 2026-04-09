# Omni Documentation Contract

**Updated:** 2026-04-09

This file defines documentation authority, coverage boundaries, and verification
anchors. Use it to decide which document is authoritative for each aspect of
Omni when multiple docs overlap.

## 1. Decision Order

When two docs disagree, resolve in this order:

1. `memory/CHANGELOG.md` for current implementation and validation state.
2. Normative architecture/spec docs (`docs/LANGUAGE_SPEC.md`,
   `docs/ARCHITECTURE.md`, `docs/ERROR_MODEL.md`, `docs/SYNTAX_SPEC.md`).
3. Area status and plans (`docs/areas/*`, `docs/plans/*`).
4. Reference/cookbook material (`docs/reference/*`, `docs/OMNI_REFERENCE.md`,
   examples).

Operational truth beats stale prose. Reference docs are not normative contracts.

## 2. Coverage Matrix

| Aspect | Primary authority | Secondary context | Status | Verification source |
|---|---|---|---|---|
| Core syntax and reader forms | `docs/SYNTAX_SPEC.md` | `docs/LANGUAGE_SPEC.md` sections 1/3/6 | normative | parser/compiler tests under `src/lisp/tests_*` |
| Core evaluation, truthiness, binding, function arity | `docs/LANGUAGE_SPEC.md` | `docs/SYNTAX_SPEC.md` | normative | interpreter/JIT tests (`./build/main --test-suite lisp`) |
| Surface naming and alias policy | `docs/syntax-decision.md` | this file + `docs/SURFACE_COMPATIBILITY.md` | normative | parser diagnostics + canonical syntax tests under `tests/` |
| Access and lookup syntax (`expr.name`, `expr.[key]`, `ref`) | `docs/LANGUAGE_SPEC.md` section 6 | `docs/reference/03-collections.md` | normative | parser + runtime access tests |
| Collections and generic operations | `docs/reference/03-collections.md` | `docs/LANGUAGE_SPEC.md` sections 2/7 | operational | collection primitive tests |
| Multiple dispatch and types | `docs/LANGUAGE_SPEC.md` sections 4/5 | `docs/type-system-syntax.md`, `docs/areas/types-dispatch.md` | normative + operational | type/dispatch test groups |
| Effects and handlers | `docs/EFFECTS_SEMANTICS.md` | `docs/EFFECTS_GUIDE.md`, `docs/LANGUAGE_SPEC.md` | normative + operational | effect runtime and language tests |
| Delimited continuations | `docs/LANGUAGE_SPEC.md` section 9 | `docs/EFFECTS_SEMANTICS.md`, selected plans | normative + operational | continuation test groups |
| Error/failure contract | `docs/ARCHITECTURE.md` (ADR-2026-03-06-A) | `docs/ERROR_MODEL.md` | normative + operational | canonical `raise` payload regression tests |
| Macros and expansion model | `docs/LANGUAGE_SPEC.md` section 11 | `docs/syntax-decision.md` macro sections | normative | macro parser/expansion tests |
| Modules/import/export semantics | `docs/LANGUAGE_SPEC.md` section 12 | `docs/reference/05-macros-modules.md` | normative + operational | module/load/import tests |
| Runtime ownership and memory lanes | `docs/areas/memory-runtime.md` | `memory/DESTINATION_ARENA_PLAN.md`, `memory/CHANGELOG.md` | operational + in-progress | memory-lifetime test lanes and container gates |
| Compiler/parser refactor status | `docs/areas/compiler-parser-refactor.md` | `docs/plans/compiler-parser-refactor-plan.md` | in-progress | targeted compile/test lanes |
| Deduce query/runtime semantics | `docs/deduce-datalog-spec.md` | `docs/areas/README.md`, related plans | operational + in-progress | deduce test slices and changelog entries |
| FFI and callback model | `docs/plans/ffi-*.md` decision docs | `docs/reference/09-concurrency-ffi.md` | in-progress | targeted FFI build/test lanes |
| Tooling, CLI, REPL workflow | `docs/PROJECT_TOOLING.md` | man pages + top-level `README.md` | operational | local/tooling scripts and validation gates |
| Release and active work queue | `docs/RELEASE_STATUS.md` + `TODO.md` | `docs/areas/README.md`, `docs/plans/README.md` | operational | latest committed status/docs sweep |

## 3. Document Roles

- `docs/LANGUAGE_SPEC.md`:
  normative language/runtime contract for user-facing semantics.
- `docs/SYNTAX_SPEC.md`:
  syntax/token/AST grammar contract.
- `docs/FEATURES.md`:
  index and coverage map only, not a second normative spec.
- `docs/OMNI_REFERENCE.md`:
  navigation entry for chapter docs and appendices.
- `docs/SURFACE_COMPATIBILITY.md`:
  single source for removed/changed surface syntax and replacements.

## 4. Required Update Discipline

When changing language surface or runtime behavior:

1. Update `memory/CHANGELOG.md` with the behavioral truth first.
2. Update affected normative docs (`LANGUAGE_SPEC`, `SYNTAX_SPEC`,
   `ARCHITECTURE`, `ERROR_MODEL`) in the same change set.
3. If syntax was removed/renamed, update `docs/SURFACE_COMPATIBILITY.md`.
4. Keep `docs/FEATURES.md` as an index; do not duplicate full semantics there.

## 5. Known Drift Risks

- Legacy syntax references can survive in old plan/session docs. Those files are
  historical artifacts, not language contracts.
- `docs/reference/*` is broad and practical, but may lag normative wording after
  major surface cleanup. When in doubt, prefer `LANGUAGE_SPEC` +
  `SURFACE_COMPATIBILITY`.
