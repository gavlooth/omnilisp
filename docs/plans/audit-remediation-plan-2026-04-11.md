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

## Current Plan

1. Fix `AUDIT-EVAL-VALUE-TO-EXPR-FAIL-CLOSED-096`, starting with
   non-symbol heads/tags and exact special-form arity, then lambda/let
   multi-body parity and `macroexpand` failure surfacing.
2. Normalize list walker improper-list behavior under
   `AUDIT-LIST-WALKER-IMPROPER-LIST-092`, preferably through shared traversal
   helpers instead of one-off guards.
3. Resolve `AUDIT-LIST-PREDICATE-CONTRACT-093` after list walker behavior is
   explicit, so `list?` can be made consistent with the selected proper-list
   contract.
4. Resolve documentation/product-contract issues that should not be guessed in
   code: `AUDIT-STRING-GENERIC-BYTE-CODEPOINT-094` and
   `AUDIT-CONS-REF-SPEC-PARITY-095`.
5. Continue constructor/dispatch cleanup decisions already in `TODO.md`:
   `AUDIT-LIST-STRING-CONSTRUCTOR-SURFACE-084`,
   `AUDIT-NUMBER-PARSE-SURFACE-085`, and
   `AUDIT-LIST-HELPER-ALIAS-086`.

## Validation Policy

- Run `c3c build --warn-deprecation=no` for each implementation slice.
- Run targeted direct probes for changed language behavior.
- Run bounded Docker subgroup validation for touched test slices.
- Use broader Docker validation only when the change crosses runtime memory,
  evaluator, compiler, or large stdlib boundaries.
