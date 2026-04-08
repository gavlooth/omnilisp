# Effects and Error Model

Status: `green` (contract + migration baseline implemented and validated)  
As of: 2026-03-09

## Canonical Sources

- `memory/CHANGELOG.md` (primary current-state source of truth)
- `docs/LANGUAGE_SPEC.md`:
  - `#04-advanced-omni-profile` (effects/continuations boundaries)
  - `#05-error-model-quick-reference` (failure class + payload shape quick map)
  - `#06-pitfalls-guide` (`nil` vs `raise`, `resolve` vs abort)
  - `#10-effect-handlers` (language surface for effect operations)
- `docs/ARCHITECTURE.md` (effects-first failure contract)
- `docs/EFFECTS_SEMANTICS.md` (semantics and runtime obligations)
- `docs/EFFECTS_GUIDE.md` (usage-facing guide)
- `docs/plans/effects-typesystem-parity-plan.md` (execution checklist)

## Current State

- Effects-first failure contract is documented and accepted.
- Canonical error payload schema is specified in architecture docs.
- Runtime boundary/scheduler semantics are documented in effects semantics.
- Explainability tooling is closed with canonical symbol selectors and deterministic structured output (`explain 'effect`, `explain 'dispatch`).
- Conversion/compression primitive families now emit canonical payloaded raises (`type/*`, `runtime/*`) instead of string-only error paths.
- Error-model migration matrix (`docs/ERROR_MODEL.md`) now has no remaining `missing` rows.

## Known Gaps

- No open migration gaps in the current baseline.
- New runtime/stdlib API families must still default to canonical payloaded `raise` and add explicit regression anchors.

## Next Steps

1. Keep CI/lint drift gates (`effects contract` checks) in mandatory validation paths.
2. Add canonical payload code/domain regression checks when new API families are added.
3. Keep examples/docs and area snapshots aligned with canonical selector syntax (`'effect`, `'dispatch`) and onboarding quick-reference sections.
