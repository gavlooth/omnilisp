# Effects and Error Model

Status: `yellow` (core contract accepted; migration/parity still in progress)  
As of: 2026-03-07

## Canonical Sources

- `memory/CHANGELOG.md` (primary current-state source of truth)
- `docs/ARCHITECTURE.md` (effects-first failure contract)
- `docs/EFFECTS_SEMANTICS.md` (semantics and runtime obligations)
- `docs/EFFECTS_GUIDE.md` (usage-facing guide)
- `docs/plans/effects-typesystem-parity-plan.md` (execution checklist)

## Current State

- Effects-first failure contract is documented and accepted.
- Canonical error payload schema is specified in architecture docs.
- Runtime boundary/scheduler semantics are documented in effects semantics.

## Known Gaps

- Effects/typesystem parity plan still has pending phases and done-definition items.
- Explainability tooling (`explain 'effect`, `explain 'dispatch`) is not yet closed.
- Full drift-prevention gates for this area are incomplete.

## Next Steps

1. Complete pending items in parity plan Phases 3-6.
2. Finalize the parity matrix and link each done row to regression tests.
3. Add CI/lint guardrails that block drift from effects-first failure policy.
4. Keep examples/docs aligned to canonical symbol selector syntax (`'effect`, `'dispatch`).
