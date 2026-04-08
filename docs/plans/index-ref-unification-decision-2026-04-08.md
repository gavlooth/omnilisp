# Index/Ref Unification Decision (2026-04-08)

## Scope
- TODO lane: `ACCESS-UNIFY-INDEX-REF-001`
- Surface under review: postfix index syntax `expr.[key]` and canonical `ref` operation.

## Decision
- Chosen seam: **shared runtime helper**.
- We are not desugaring `E_INDEX` in the parser at this stage.

## Why this seam
- It removes semantic drift between JIT `E_INDEX` and primitive `ref` without changing parser AST shape.
- It keeps AOT behavior aligned because the AOT bridge already routes through `ref`.
- It is the narrowest safe slice: implementation-only unification first, parser-desugar can still be revisited later if useful.

## Rejected/Deferred options
- Parser-level desugar of `E_INDEX -> (ref coll key)`:
  - deferred because it is a wider parser/AST/serialization change than needed for first parity.
  - it does not by itself remove runtime drift if different runtime call paths still exist.

## Canonical runtime contract (current)
- `expr.[key]` and `(ref expr key)` share one lookup implementation for:
  - list indexing (including negative index behavior),
  - array indexing (including negative index behavior),
  - dictionary key lookup,
  - string indexing.
- Unsupported collection types fail with the same type error family.
- JIT keeps an explicit instance fallback path via method-table `ref` dispatch after common lookup handling.

## First landed parity slice
- Introduce/route through shared lookup helpers:
  - `ref_try_lookup_collection(...)`
  - `ref_type_error(...)`
- Wire both:
  - `prim_ref(...)`
  - `jit_do_index(...)`
- Add parity tests that assert `ref` and `expr.[key]` agree across list/array/string/dict and negative indexing cases.

## Implementation hazard (recorded)
- JIT index lowering must not keep collection in `V1` while compiling index subexpressions.
- Integer index compilation emits helper calls (for literal construction) that can clobber caller-saved registers.
- Canonical fix: spill collection to a stack slot before compiling index, then reload before `jit_do_index(...)`.

## Next follow-up
- Review and normalize any remaining error message text that still differs by call-site context.
- Add explicit parity coverage for error paths (out-of-bounds and wrong index type) if missing.
