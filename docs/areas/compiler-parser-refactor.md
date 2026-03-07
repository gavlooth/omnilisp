# Compiler, Parser, and Refactor Structure

Status: `yellow` (major split already landed; refactor docs need consolidation)  
As of: 2026-03-07

## Canonical Sources

- `memory/CHANGELOG.md` (execution history + validated changes)
- `docs/plans/` refactor plans (active work queue)
- current `src/lisp/*` module/file layout

## Current State

- Large monolith files have already been split into area-scoped files (`compiler_*`, `parser_*`, `jit_*`, `tests_*`, `value_*`, `eval_*`).
- The codebase structure is significantly ahead of older "god-file" descriptions.

## Known Drift

- Legacy monolith-era docs (`docs/COMPILER.md`, `docs/REFACTOR_PLAN.md`,
  `docs/C3_MIGRATION.md`) were removed and should not be recreated as active
  status artifacts.
- Refactor status still spans both `docs/plans/` and changelog sessions.

## Next Steps

1. Keep one active refactor plan in `docs/plans/` and retire overlapping plan files.
2. Keep a small list of remaining oversized functions/modules and concrete extraction targets in one active plan file.
3. Tie refactor tasks to build/test/ASAN gates per change batch.
4. Record each completed refactor slice in `memory/CHANGELOG.md`.
