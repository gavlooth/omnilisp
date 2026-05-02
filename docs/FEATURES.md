# Omni Lisp Feature Index

**Last updated:** 2026-04-24

This file is a feature index and coverage map.
It is not a second language specification.

For normative behavior, use:
- `docs/LANGUAGE_SPEC.md`
- `docs/SYNTAX_SPEC.md`
- `docs/ARCHITECTURE.md`
- `docs/ERROR_MODEL.md`

For removed/renamed syntax, use:
- `docs/SURFACE_COMPATIBILITY.md`

## 1. Core Language

| Capability | Canonical surface | Authority |
|---|---|---|
| Function definitions | `λ`, `define` shorthand `(define (f x) ...)`; `lambda` remains accepted | `docs/LANGUAGE_SPEC.md` section 3 |
| Binding and scope | `let`, `let ^rec`, named `let` | `docs/LANGUAGE_SPEC.md` section 3 |
| Body sequencing | implicit multi-expression bodies in `λ`/`lambda`, `define`, `let`, `let ^rec`, named `let`; explicit `block` elsewhere | `docs/LANGUAGE_SPEC.md` section 3 |
| Control flow | `if`, `block`, `and`, `or`, `match` | `docs/LANGUAGE_SPEC.md` section 3 |
| Quoting/templates | `quote`, `#syntax`, `quasiquote` / `unquote` / `unquote-splicing` | `docs/LANGUAGE_SPEC.md` section 3 |
| Mutation | `set!` variable/path/generic collection update | `docs/SYNTAX_SPEC.md` + `docs/LANGUAGE_SPEC.md` |
| Truthiness | falsy: `nil`, `false`; all else truthy | `docs/LANGUAGE_SPEC.md` section 2 |
| String interpolation | `(str "hello {name}")` | `docs/LANGUAGE_SPEC.md` section 7 |
| Radix integer literals | `#xFF`, `#b1010`, `#o755` | `docs/LANGUAGE_SPEC.md` section 0 |

## 2. Data Model and Access

| Capability | Canonical surface | Authority |
|---|---|---|
| List / Array / Dictionary | literals + constructors (`List`, `Array`, `Dictionary`); dict literals auto-quote bare symbol keys | `docs/LANGUAGE_SPEC.md` sections 2/7 |
| Generic lookup | `(ref coll key)` | `docs/reference/03-collections.md` |
| Path access | `expr.name` | `docs/LANGUAGE_SPEC.md` section 6 |
| Postfix index access | `expr.[key]` | `docs/LANGUAGE_SPEC.md` section 6 |
| Generic collection ops | `length`, `map`, `filter`, `foldl`, etc. | `docs/reference/03-collections.md` |
| Data reader tags | `#hex`, `#base64`, `#json`, `#toml`, `#time`, `#uuid` | `docs/LANGUAGE_SPEC.md` sections 0/7 |

## 3. Type System and Dispatch

| Capability | Canonical surface | Authority |
|---|---|---|
| Structural and nominal typing forms | `[type]`, `[abstract]`, `[union]`, `[alias]` | `docs/LANGUAGE_SPEC.md` section 4 |
| Function/method dispatch by type/value | typed parameters and value-literal dispatch | `docs/LANGUAGE_SPEC.md` section 5 |
| Dispatch diagnostics/explainability | dispatch explain surfaces | `docs/LANGUAGE_SPEC.md` section 5 |
| Status and active work | type/dispatch area notes | `docs/areas/types-dispatch.md` |

## 4. Effects and Continuations

| Capability | Canonical surface | Authority |
|---|---|---|
| Delimited continuation boundaries | `checkpoint`, `capture` | `docs/LANGUAGE_SPEC.md` section 9 |
| Algebraic effects | `signal`, `handle`, `resolve` | `docs/EFFECTS_SEMANTICS.md` |
| Multi-shot continuation control | `with-continuation` | `docs/EFFECTS_SEMANTICS.md` + `docs/LANGUAGE_SPEC.md` |
| Practical usage guidance | effect-first patterns and caveats | `docs/EFFECTS_GUIDE.md` |

## 5. Error and Failure Contract

| Capability | Canonical surface | Authority |
|---|---|---|
| Failure class taxonomy | `absence`, `recoverable-op-failure`, `programmer-error`, `internal-runtime-error` | `docs/ARCHITECTURE.md` |
| Surface behavior mapping | `nil` vs `signal raise` vs hard runtime error | `docs/ARCHITECTURE.md` + `docs/ERROR_MODEL.md` |
| Canonical raise payload keys | `code`, `message`, `domain`, `data` in dict payloads | `docs/ERROR_MODEL.md` |

## 6. Macros and Modules

| Capability | Canonical surface | Authority |
|---|---|---|
| Macro definition model | single transformer with `syntax-match` | `docs/LANGUAGE_SPEC.md` section 11 |
| Macro hygiene and templates | `template`, `insert`, `splice`, gensym behavior | `docs/LANGUAGE_SPEC.md` + `docs/syntax-decision.md` |
| Module boundaries | `module`, `import`, grouped `import`, `export`, `export-from` | `docs/LANGUAGE_SPEC.md` section 12 |

## 7. Runtime and Memory

| Capability | Canonical surface | Authority |
|---|---|---|
| Region ownership model | deterministic scope/region ownership | `docs/areas/memory-runtime.md` |
| TEMP/ESCAPE dual-lane model | promotion/boundary semantics | `docs/areas/memory-runtime-cycle.md` |
| Current implementation truth | shipped changes and validated checkpoints | `memory/CHANGELOG.md` |

## 8. Libraries, Tooling, and Operations

| Capability | Canonical surface | Authority |
|---|---|---|
| Standard libs and primitives | reference chapters + appendices | `docs/OMNI_REFERENCE.md` and `docs/reference/*` |
| CLI/build/test workflows | `c3c`, test slices, container gates | `docs/PROJECT_TOOLING.md` |
| Release and active status | release + area status hubs | `docs/RELEASE_STATUS.md`, `docs/areas/README.md` |
| Work planning and milestones | active plans and decision docs | `docs/plans/README.md` |

## 9. Validation Anchors

Use these as practical anchors when touching features:

- Build integration: `c3c build`
- Lisp suite: `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- End-to-end checks: `scripts/run_e2e.sh`
- Memory-sensitive validation: container-bound gates in `scripts/` per
  `docs/PROJECT_TOOLING.md`

## 10. Compatibility Reminder

Do not infer supported syntax from historical plan/session artifacts.
Use:

1. `docs/SYNTAX_SPEC.md`
2. `docs/LANGUAGE_SPEC.md`
3. `docs/SURFACE_COMPATIBILITY.md`

in that order for syntax-surface truth.
