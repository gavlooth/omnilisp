# Deduce Module Boundary Decision - 2026-04-23

Status: Accepted for planning. This document closes `SURFACE-NAMING-002` from
the slash surface naming audit.

## Decision

Deduce should move out of the always-present slash-symbol primitive surface and
behind a real module/facade boundary.

The long-term canonical application-facing shape is an imported `deduce` module
whose operations are ordinary module members, for example:

```lisp
(define db (deduce.open 'memory))
(define tx (deduce.block db 'write-deferred))
(deduce.fact! tx person "Dana" 29 "dana@b.com")
(deduce.commit tx)
(deduce.analyze db)
```

Slash names such as `deduce/open`, `deduce/query`, and `deduce/refresh!` are
ordinary symbols, not module access, and should not remain the canonical public
shape for this subsystem. Deduce is large enough, independently stateful
enough, and operationally database-like enough that a module boundary is clearer
than treating `deduce/...` as a compact dense core family.

## Surface Classification

All current user-facing `deduce/...` command aliases should move behind the
module facade as module members:

| Current slash symbol | Canonical module member |
|---|---|
| `deduce/open` | `deduce.open` |
| `deduce/open-named` | `deduce.open-named` |
| `deduce/block` | `deduce.block` |
| `deduce/commit` | `deduce.commit` |
| `deduce/abort` | `deduce.abort` |
| `deduce/scan` | `deduce.scan` |
| `deduce/scan-range` | `deduce.scan-range` |
| `deduce/query` | `deduce.query` |
| `deduce/count` | `deduce.count` |
| `deduce/match` | `deduce.match` |
| `deduce/fact!` | `deduce.fact!` |
| `deduce/retract!` | `deduce.retract!` |
| `deduce/clear!` | `deduce.clear!` |
| `deduce/drop!` | `deduce.drop!` |
| `deduce/rule!` | `deduce.rule!` |
| `deduce/explain` | `deduce.explain` |
| `deduce/analyze` | `deduce.analyze` |
| `deduce/materialize!` | `deduce.materialize!` |
| `deduce/refresh!` | `deduce.refresh!` |
| `deduce/dematerialize!` | `deduce.dematerialize!` |
| `deduce/schema` | `deduce.schema` |
| `deduce/indexes` | `deduce.indexes` |
| `deduce/stats` | `deduce.stats` |
| `deduce/why-result` | `deduce.why-result` |

The non-slash relation declaration surface, for example
`(define [relation db] person ...)`, is not decided by this naming item. It
belongs to the broader Deduce language integration surface and should not be
renamed as part of the module-boundary migration unless a separate relation
declaration decision requires it.

Deduce diagnostic/error codes may continue to use stable `deduce/...` code
strings. Those strings are diagnostics, not callable language surface, and they
benefit from remaining grep-friendly and stable across the migration.

## Unified Dispatcher

The unified variadic `deduce` dispatcher should remain as a low-level escape
hatch, but it should not remain the canonical application-facing API.

The preferred landing shape is:

- `deduce.<operation>` module members are the documented API.
- `deduce.dispatch` exposes the existing command-table dispatcher for bootstrap,
  low-level diagnostics, compatibility tests, and cases that intentionally need
  dynamic command selection.
- Top-level `(deduce 'command ...)` is transitional only. It may stay during an
  explicitly approved migration window, but the final public surface should not
  require keeping a callable top-level `deduce` binding alongside the `deduce`
  module binding.

`deduce.dispatch` should be treated as an escape hatch rather than a second
spelling for every operation. User docs, examples, and normal tests should move
to direct module members.

## Migration Approach

1. Add the `deduce` module/facade with direct members for the operations listed
   above. The facade should call the existing implementation paths so the first
   behavioral migration is naming-only.
2. Update reference docs, examples, and tests from slash aliases and
   `(deduce 'command ...)` examples to `deduce.<operation>` member access.
3. Keep the unified dispatcher reachable as `deduce.dispatch` for dynamic
   command selection and low-level testing.
4. Remove public registration of the `deduce/...` command aliases unless the
   owner explicitly approves a short migration window.
5. If a migration window is approved, removed slash calls should fail closed
   after the window with deterministic diagnostics such as
   "`deduce/open` moved to `deduce.open` from the `deduce` module."

This migration should not introduce non-canonical shorthand aliases. In
particular, do not add parallel names like `db/open`, `logic/query`, or
`datalog/rule!` as part of this decision. Those would be new naming decisions,
not implementation details of the Deduce module boundary.

## Validation Implications

This decision document is documentation-only. The later implementation is a
language-surface change and needs stronger validation than a docs-only patch:

- Primitive/module registration parity: every listed `deduce.<operation>` member
  must resolve to the same behavior as the current command implementation before
  any old spelling is removed.
- Documentation parity: the primitive appendix, Deduce library reference,
  examples, and surface compatibility notes must agree on the canonical module
  surface and on any approved transitional forms.
- Targeted runtime validation: run focused Deduce command/read/write/admin
  slices after adding the facade.
- Integration validation: run `c3c build`.
- Container-bound validation: run broad or high-memory Deduce validation through
  the bounded Docker path required by repo policy.
- Removal validation: if `deduce/...` aliases are removed, add deterministic
  negative tests for representative removed forms and verify diagnostics point
  to the module-member replacements.

No behavior should be reported as migrated until the process that registers
primitives/modules has been rebuilt or restarted and the new module bindings are
active in the running binary.

## Non-Goals

- This decision does not rename Deduce data model concepts, payload fields,
  materialization policies, integrity vocabulary, or diagnostic codes.
- This decision does not make Deduce optional at runtime by itself; it only
  chooses the language-facing boundary that future optionalization or packaging
  can use.
- This decision does not decide ML, Pika, math, tensor, matrix, or IO slash
  naming.
