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

## Current Plan

1. Resolve `AUDIT-LIST-PREDICATE-CONTRACT-093` after list walker behavior is
   explicit, so `list?` can be made consistent with the selected proper-list
   contract.
2. Resolve documentation/product-contract issues that should not be guessed in
   code: `AUDIT-STRING-GENERIC-BYTE-CODEPOINT-094` and
   `AUDIT-CONS-REF-SPEC-PARITY-095`.
3. Continue constructor/dispatch cleanup decisions already in `TODO.md`:
   `AUDIT-LIST-STRING-CONSTRUCTOR-SURFACE-084`,
   `AUDIT-NUMBER-PARSE-SURFACE-085`, and
   `AUDIT-LIST-HELPER-ALIAS-086`.

## Validation Policy

- Run `c3c build --warn-deprecation=no` for each implementation slice.
- Run targeted direct probes for changed language behavior.
- Run bounded Docker subgroup validation for touched test slices.
- Use broader Docker validation only when the change crosses runtime memory,
  evaluator, compiler, or large stdlib boundaries.
