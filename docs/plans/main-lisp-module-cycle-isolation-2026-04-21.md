# Main/Lisp Module Cycle Isolation Contract

Date: 2026-04-21

Backlog item: `AUDIT-2026-M5-MODULE-CYCLE`

## Decision

The `main` <-> `lisp` C3 module cycle is explicit and intentionally isolated
for the current runtime shape. It is not treated as an accidental dependency
edge to break with local file moves.

Current contract:

- `main -> lisp` is allowed only in entrypoint, CLI, test-runner, REPL, build,
  bindgen, and source-check adapter surfaces.
- `lisp -> main` is allowed for the low-level runtime services still hosted in
  `main`: scope/region ownership, stack/coroutine helpers, lifecycle hooks,
  runtime entry helpers, bindgen manifest helpers, diagnostics, and tests.
- New arbitrary `lisp -> main` imports for high-level entry behavior are not
  allowed. Add the behavior behind a narrow runtime service or move the shared
  contract before expanding the dependency.
- New arbitrary `main -> lisp` imports outside entry/test/adapter surfaces are
  not allowed. If non-entry `main` code needs Lisp runtime behavior, add a
  narrow adapter or revisit the neutral runtime module split.

This closes the audit item by documenting and validating the current
dependency boundary. It does not claim that a neutral runtime module extraction
has been completed.

## Current Map

Exact `module main` files importing `lisp` as of this decision:

- `src/entry_bind_dep_generation.c3`
- `src/entry_bind_mode.c3`
- `src/entry_bind_parse_helpers.c3`
- `src/entry_bind_runtime_setup.c3`
- `src/entry_build_mode.c3`
- `src/entry_check_mode.c3`
- `src/entry_check_reporting.c3`
- `src/entry_compile_manifest.c3`
- `src/entry_compile_runner.c3`
- `src/entry_eval_mode.c3`
- `src/entry_eval_reporting.c3`
- `src/entry_fmt_stack.c3`
- `src/entry_interp_helpers.c3`
- `src/entry_repl_server_mode.c3`
- `src/entry_runtime_modes.c3`
- `src/entry_script_mode.c3`
- `src/entry_script_reporting.c3`
- `src/entry_stack_affinity_mode.c3`
- `src/entry_test_modes.c3`
- `src/entry_test_runner_setup.c3`
- `src/main_repl_shared.c3`
- `src/scope_region_tests_splice_cases.c3`

Exact `src/lisp` files importing `main` as of this decision: 413.

The `lisp -> main` side is broad because `ScopeRegion`, stack runtime, global
scope guards, and lifecycle services live in `main`. High-signal families
include `eval_boundary_*`, `eval_env_copy_*`, `eval_promotion_*`,
`jit_eval_scope_*`, `value_*`, `runtime_backend_*`, `scheduler_*`,
`async_*`, `compiler_*`, `prim_*`, `primitives_*`, `tests_*`, and `aot.c3`.

## Future Break Boundary

If the cycle needs a real structural break, extract a neutral runtime module
first instead of moving random leaf imports. The likely starting surface is:

- `src/scope_region*.c3`
- `src/stack_engine*.c3`
- low-level lifecycle hooks and ownership helpers used by both entry code and
  Lisp runtime code

That extraction is a separate high-blast-radius runtime modularization task.
It must preserve the region-centric ownership invariant, stack/lifetime
boundary behavior, and existing source/build manifest parity.

## Validation

Use these checks when this contract is touched:

```sh
for f in $(rg -l '^module main;' src --glob '*.c3'); do
  rg -q '^\s*import\s+lisp\b' "$f" && echo "$f"
done | sort

rg -l '^\s*import\s+main\b' src/lisp --glob '*.c3' | sort

c3c build
scripts/check_e2e_baseline_policy.sh --stage3-source-parity
git diff --check
```
