# Validation Status Area

## Canonical Source Of Truth

- Current implementation truth: `memory/CHANGELOG.md`
- Live queue: `TODO.md`
- Follow-up plans:
  - `docs/plans/validation-all-slice-2026-04-11.md`
  - `docs/plans/validation-all-slice-nested-let-residual-2026-04-11.md`

## Current Status

Status: `green`
As of: 2026-04-30

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

Latest targeted TLS/async validation:

- `scripts/run_tls_targeted.sh`: `pass=5 fail=0`
- TLS-enabled async slice with `OMNI_ENABLE_TLS_INTEGRATION=1`: `pass=104 fail=0`

Latest bounded all-slice result with TLS integration enabled and no TLS skip:

- result: passed
- command: `OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh bash -lc './scripts/build_omni_chelpers.sh && c3c build && timeout --kill-after=10s 600s env OMNI_ENABLE_TLS_INTEGRATION=1 OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=all LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --test-suite lisp'`
- summary: `OMNI_TEST_SUMMARY suite=unified pass=5532 fail=0`
- compiler summary: `OMNI_TEST_SUMMARY suite=compiler pass=390 fail=0`

TLS integration is no longer skipped, and
`VALIDATION-002-ALL-SLICE-BOUNDARY-JIT-BLOCKER` is closed in `TODO.md`.

## Next Steps

1. Use `TODO.md` for any future validation backlog item instead of reopening
   closed residual plans.

## Validation

Use targeted host slices only for narrow reproduction or debugger setup. Use the
bounded Docker validation path for validation closure on sliced Lisp runs, full
all-slice runs, memory-lifetime soak, memory-stress, or any high-memory run.

The default bounded validation image `omni-validation:2026-03-10` includes
Valgrind as of 2026-04-25. The memory-lifetime smoke slice has passed under
bounded container Valgrind with `253 passed, 0 failed`.
