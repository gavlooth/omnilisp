# Path/Ref Contract Decision (2026-04-08)

## Scope
- TODO lane: `ACCESS-UNIFY-PATH-REF-001`
- Surface under review: dot-path access `expr.name` versus canonical lookup `(ref coll key)`.

## Decision
- `expr.name` is a **distinct path operation**, not a full desugar to `ref`.
- `ref` remains the canonical dynamic collection lookup primitive.

## Why this decision
- Runtime already has explicit path-only behavior in `eval_path_step(...)` and AOT bridge `aot::field_access(...)`.
- Path semantics include module export checks and instance field access that `ref` does not model.
- Forcing desugar to `ref` would either lose current behavior or require widening `ref` into a multi-surface operation.

## Canonical contract (locked)
- `ref`:
  - list/array/string indexing (supports negative index),
  - dictionary lookup by arbitrary key type,
  - dictionary miss returns `nil`.
- `expr.name`:
  - resolves path segments via dedicated path semantics,
  - supports module export access, instance field access, dictionary symbol-key access, and cons `.car`/`.cdr`,
  - path miss/type mismatch is an error (no implicit `nil` fallback).

## Explicit parity / non-parity
- Parity case:
  - dictionary symbol-key present lookup:
    - `(let (m {'name 42}) m.name)` equals `(ref {'name 42} 'name)`.
- Non-parity cases:
  - missing dictionary symbol key:
    - `(ref {'name 42} 'age)` => `nil`
    - `(let (m {'name 42}) m.age)` => error.
  - cons named field versus ref-key:
    - `(let (p (cons 10 20)) p.car)` is valid path access,
    - `(ref (cons 10 20) 'car)` is invalid (`ref` requires integer index on lists/cons chains).

## Follow-up guidance
- Do not describe `expr.name` as “ref sugar” in docs.
- Keep docs explicit about where parity exists and where behavior intentionally diverges.
