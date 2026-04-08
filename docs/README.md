# Omni Docs Map (Normalized)

This index is the fastest path to current, non-stale documentation.
Use this file to reduce context switching and avoid reading obsolete plans.

## Authority Order

1. `memory/CHANGELOG.md`  
   Latest validated implementation truth and test outcomes.
2. `docs/areas/README.md` and `docs/areas/*.md`  
   One operational status doc per major area with explicit next steps.
3. Specs and deep guides in `docs/` and `memory/`  
   Design intent, language rules, and rationale.

## Primary Entry Points

- Release state + gate policy: `docs/RELEASE_STATUS.md`
- Language behavior: `docs/LANGUAGE_SPEC.md`
- Deduce Datalog target semantics: `docs/deduce-datalog-spec.md`
- Onboarding (core/advanced/error/pitfalls):
  - `docs/LANGUAGE_SPEC.md#0-core-omni-profile`
  - `docs/LANGUAGE_SPEC.md#04-advanced-omni-profile`
  - `docs/LANGUAGE_SPEC.md#05-error-model-quick-reference`
  - `docs/LANGUAGE_SPEC.md#06-pitfalls-guide`
- Runtime constraints and ADRs: `docs/ARCHITECTURE.md`
- C3 implementation rules: `docs/C3_STYLE.md`
- Active implementation plans: `docs/plans/README.md`
- Area status hub: `docs/areas/README.md`
- Effects and handlers: `docs/EFFECTS_GUIDE.md`, `docs/EFFECTS_SEMANTICS.md`
- UI surface reference: `docs/UI_REFERENCE.md`
- Error payload policy: `docs/ERROR_MODEL.md`
- Type/dispatch syntax details: `docs/type-system-syntax.md`
- User-facing reference: `docs/OMNI_REFERENCE.md`
- Tooling and CLI: `docs/PROJECT_TOOLING.md`
- REPL server protocol + phase-1 status: `docs/plans/repl-server-protocol-2026-03-30.md`
- Editor integrations: `tooling/README.md`
- Syntax naming rationale: `docs/syntax-decision.md`
- Canonical feature example: `examples/finwatch/main.omni` (financial-service webserver)
- Examples catalog: `examples/README.md`

## Removed Legacy Docs

These docs were intentionally removed to reduce drift and cognitive overhead:
- `docs/COMPILER.md`
- `docs/REFACTOR_PLAN.md`
- `docs/C3_MIGRATION.md`

If you need historical details, use git history and `memory/CHANGELOG.md`.
