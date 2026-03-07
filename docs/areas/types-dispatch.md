# Type System and Dispatch

Status: `yellow` (implemented baseline; formal parity program pending completion)  
As of: 2026-03-07

## Canonical Sources

- `memory/CHANGELOG.md` (primary current-state source of truth)
- `docs/type-system-syntax.md`
- `docs/LANGUAGE_SPEC.md` (types and dispatch sections)
- `docs/plans/effects-typesystem-parity-plan.md` (Phase 4 and related items)

## Current State

- Language spec presents structural type system and multiple dispatch model.
- Runtime and parser include type/dispatch infrastructure used by current tests.
- Type/effect integration direction is defined, but parity formalization is not fully closed.

## Known Gaps

- Julia-parity matrix and status tags are still incomplete in plan tracking.
- Ambiguity resolution and specificity policy need stronger explicit coverage.
- Some "documented vs tested" mapping remains incomplete.

## Next Steps

1. Complete and publish parity matrix rows with status tags (`done/partial/missing`).
2. Add deterministic tests for specificity ordering and ambiguity tie-breaks.
3. Lock intentional differences from Julia in one explicit table.
4. Keep language spec and matrix synchronized release-by-release.
