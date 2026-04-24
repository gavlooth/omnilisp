# Validation Status Area

## Canonical Source Of Truth

- Current implementation truth: `memory/CHANGELOG.md`
- Live queue: `TODO.md`
- Follow-up plans:
  - `docs/plans/validation-all-slice-2026-04-11.md`
  - `docs/plans/validation-all-slice-nested-let-residual-2026-04-11.md`

## Current Status

Status: `green`
As of: 2026-04-25

The latest all-slice follow-up,
`VALIDATION-ALL-SLICE-NESTED-LET-2026-04-11`, is closed.

Fixed:

- TOML option arity cluster. `toml-parse` now reaches its existing 1-2
  argument validation, and the targeted `data-format` slice passes at
  `pass=64 fail=0`.
- Diagnostics match/union cluster. Zero-length constructor-pattern AST arrays
  no longer set a parser OOM error, and the targeted `diagnostics` slice passes
  at `pass=10 fail=0`.

- Nested-let memory-stress residual. The failing fixture had two extra closing
  parentheses, so the parser rejected it before either interpreter or JIT
  evaluation. The fixture is now balanced and evaluates to `55`.

All remaining Deduce subgroups in this lane are now green after the runtime patch:

- `OMNI_DEDUCE_GROUP_FILTER=relation-attrs`: `pass=13 fail=0`
- `OMNI_DEDUCE_GROUP_FILTER=core-runtime`: `pass=6 fail=0`
- `OMNI_DEDUCE_GROUP_FILTER=rule-validation`: `pass=70 fail=0`

Latest bounded all-slice result with TLS integration intentionally skipped:

- unified: `pass=2798 fail=0`
- compiler: `pass=208 fail=0`
- remaining emitted failure: none

This is not a replacement for the TLS integration gate; the recorded broad run
used `OMNI_SKIP_TLS_INTEGRATION=1`.

## Next Steps

1. Keep this area green unless a new validation family appears.
2. Use `TODO.md` for any future validation backlog item instead of reopening
   closed residual plans.

## Validation

Use targeted host slices for narrow reproduction. Use the bounded Docker
validation path for full all-slice, memory-lifetime soak, memory-stress, or any
high-memory run.

The default bounded validation image `omni-validation:2026-03-10` includes
Valgrind as of 2026-04-25. The memory-lifetime smoke slice has passed under
bounded container Valgrind with `253 passed, 0 failed`.
