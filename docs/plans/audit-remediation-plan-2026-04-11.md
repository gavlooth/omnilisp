# Audit Remediation Plan - 2026-04-11

This note records the current audit plan. `TODO.md` remains the live execution
queue; this file is supporting context for why the queue is ordered this way.

## Completed Slices

- `AUDIT-HTTP-CRUD-DICT-PROMOTION-089`: fixed bounded HTTP slice failures.
- `AUDIT-PIKA-REGEX-STRESS-CACHE-090`: fixed bounded Pika stress/cache
  failures by correcting the underlying memory/promotion issue.
- Dotted cons sequence parity: `length` now matches `ref` for dotted cons
  tails.
- Primitive fallback arity: fixed-arity primitives now reject extra or missing
  arguments in the multi-argument fallback path.
- AOT constructor dispatch: `list`, `Dictionary`, and `Dict` calls now route
  through normal callable dispatch instead of name-only compiler fast paths.
- `reverse` improper-list handling: `__reverse-list` now rejects dotted tails
  instead of silently truncating them; `append` no longer silently loses
  left-side dotted-tail data through `reverse`.
- Tail multi-argument error-valued arguments: ESCAPE-lane cons promotion now
  preserves first-class `ERROR` values instead of confusing successful error
  value promotion with promotion failure, and `append` preserves the original
  improper-list error from `reverse`.
- Eval data-to-expression fail-closed arity/name slice: malformed special-form
  data for `if`, `quote`, `define`, `set!`, `checkpoint`, `capture`,
  quasiquote/unquote forms, and `signal` now reports structural conversion
  errors instead of defaulting missing operands, truncating extras, or coercing
  non-symbol names/tags to symbol id `0`.
- Eval data-to-expression block/macroexpand slice: lambda and let data forms
  now preserve parser-equivalent implicit block bodies, and `macroexpand` now
  surfaces structural conversion failures for malformed cons forms instead of
  returning the original malformed form.
- List-walker partial-success slice: a private stdlib proper-list guard now
  protects `take`, `drop`, `zip`, `any?`, `every?`, and `find`, including the
  compiler prelude variants where those functions are present.
- List-walker remaining public walkers slice: the same guard now protects
  `map`, `filter`, `foldl`, `foldr`, `append`, `for-each`, `nth`, `flatten`,
  `partition`, and `remove` via `filter`; nested improper lists in `flatten`
  also fail coherently.
- `list?` proper-list contract: the public predicate now uses the existing
  strict primitive implementation, and the broader stdlib pair alias has been
  removed so improper lists return false.
- `AUDIT-CONS-REF-SPEC-PARITY-095`: documented the existing tested cons/list
  sequence contract for `ref` and `length`, including negative indexes and
  dotted terminal tails.
- `AUDIT-STRING-GENERIC-BYTE-CODEPOINT-094`: aligned generic string `length`,
  `ref`, and postfix indexing with codepoint/character semantics;
  `string-byte-length` remains the explicit byte lane.
- `AUDIT-LIST-STRING-CONSTRUCTOR-SURFACE-084`: selected constructor-first
  list/string conversion, migrated public docs/tests to `List(String)` and
  `String(List)`, and removed public `string->list` / `list->string` aliases.
- `AUDIT-NUMBER-PARSE-SURFACE-085`: selected `parse-number` as the canonical
  permissive parse API, kept `Number` non-callable, and removed public
  `string->number` registration/compiler-map surface.
- `AUDIT-E2E-PRIMITIVE-CAPTURE-SANITIZATION-096`: repaired generated C3
  primitive capture emission, added the missing runtime primitive coverage to
  compiler primitive/free-variable/hash tables, preserved user shadowing by
  avoiding dynamic environment classification, and fixed AOT `Coroutine` thunk
  parity for `aot::make_closure` wrappers; Docker `scripts/run_e2e.sh` now
  passes all 404 generated tests.
- `AUDIT-COMPILER-PRIMITIVE-CLASSIFICATION-098`: consolidated compiler
  primitive/free-variable classification around the primitive hash table while
  preserving the existing non-value `ForeignHandle` and `__ui-ftxui-run`
  exceptions; this closes a broader drift class where hash-only runtime
  primitives could be captured as C3 locals inside lambdas.
- `AUDIT-FFI-FOREIGN-HANDLE-SURFACE-103`: replaced the public FFI pointer
  annotation surface with `^ForeignHandle`, and current-state docs/tracking
  now describe pointer ABI values as `FFI_HANDLE`/`nil` only.
- `AUDIT-FFI-FOREIGN-HANDLE-METADATA-104`: implemented the interpreter/JIT
  ForeignHandle metadata dictionary policy so `^ForeignHandle` stays the simple
  default and `^{'name File 'ownership owned 'finalizer fclose}` /
  `^{'type ForeignHandle ...}` remain the accepted FFI-local refinements;
  dictionary entries are quoted-symbol key/value pairs and Omni does not use
  colon keywords.
- `AUDIT-FFI-FOREIGN-HANDLE-AOT-POLICY-105`: implemented AOT bridge policy
  propagation for the current `ForeignHandle` metadata dictionary surface:
  parameter descriptors preserve handle family/nullability, return descriptors
  preserve handle name/ownership/finalizer, and owned handle parameter policy
  rejects fail-closed.
- `AUDIT-COMPILER-PRIMITIVE-HASH-COVERAGE-099`: completed public runtime
  primitive coverage in the compiler primitive hash table, resized the table to
  preserve a safe load factor, and added closure-capture regression coverage for
  representative math, string, and collection primitives.
- `AUDIT-FILESYSTEM-SURFACE-087`: selected `fs-*` as the canonical
  filesystem wrapper family and removed the remaining stdlib `filesystem-*`
  aliases; runtime, compiler, docs, and tests now agree on the same surface.
- `AUDIT-PROCESS-WRAPPER-PAYLOAD-FALLBACK-100`: added untyped
  `process-spawn` and `process-kill` stdlib fallbacks so invalid args preserve
  canonical `io/process-*` payload errors instead of stopping at typed dispatch.
- `AUDIT-LIST-HELPER-ALIAS-086`: kept `List` as the canonical
  constructor/conversion surface and explicitly approved lowercase `list` as
  an idiomatic public helper.
- `AUDIT-IMMER-PERSISTENT-DISPATCH-088`: wrapped optional Immer persistent
  collection handles in tagged Omni dictionaries, added persistent collection
  predicates, and made odd `persistent-dictionary` constructor arity fail
  closed instead of dropping the final key; this was later superseded by
  retiring the optional Immer bridge entirely.
- `AUDIT-LET-BRACKET-SHORTHAND-102`: removed legacy outer `let [...]`
  shorthand from live library code, while leaving canonical destructuring
  patterns such as `(let ([a b] [1 2]) ...)` intact.
- `AUDIT-IMMER-FFI-COMPAT-101`: retired the unsupported optional Immer/C++
  bridge instead of extending FFI around a non-core integration; deleted
  `lib/immer.omni`, the `lib/immer/` bridge tree, and the obsolete
  compatibility plan.

## Current Plan

1. Continue the remaining scientific-computing surface lane already in
   `TODO.md`: `LANG-TENSOR-SCIENTIFIC-SURFACE-091`.
2. Keep new implementation work sliced and committed separately after each
   audited fix, with TODO entries closed only after targeted validation.
3. Re-run broader Docker validation when a change crosses runtime memory,
   evaluator, compiler, or large stdlib boundaries.

## Validation Policy

- Run `c3c build --warn-deprecation=no` for each implementation slice.
- Run targeted direct probes for changed language behavior.
- Run bounded Docker subgroup validation for touched test slices.
- Use broader Docker validation only when the change crosses runtime memory,
  evaluator, compiler, or large stdlib boundaries.
