# Deduce Runtime Status

## Scope

Execution-facing status for Deduce query/runtime behavior, validation signals,
and highest-priority blockers.

## Authority

When this page disagrees with another Deduce doc, resolve in this order:

1. `memory/CHANGELOG.md` (latest validated implementation truth)
2. This file (current operator status + next actions)
3. `docs/deduce-datalog-spec.md` (broader semantic surface contract)

## Current Status (2026-04-12)

- `green`: core Deduce command surfaces are implemented and exercised broadly
  (`deduce/open`, relation admin, query/read surfaces, rule install, explain/analyze payloads).
- `yellow`: aggregate and broader incremental-maintenance lanes remain partial,
  but they are observational constraints only; no live deferred queue exists
  until a fresh failing signal or owner request promotes one.
- `yellow`: legacy JIT call fastpath remains unstable under `OMNI_JIT_CALL_FASTPATH=1`,
  while the default safe call-lowering mode is stable.

## Deferred Watch Item

### `DEDUCE-JIT-CALL-FASTPATH-STABILITY-001` (yellow)

- Default path status (stable):
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=query OMNI_DEDUCE_QUERY_FILTER=admin-surface ./build/main --test-suite lisp`
  - result: `suite=unified pass=184 fail=0`
- Full-slice validation after mitigation:
  - `OMNI_LISP_TEST_SLICE=deduce` -> `pass=378 fail=0`
  - `OMNI_LISP_TEST_SLICE=jit-policy` -> `pass=33 fail=0`
  - `OMNI_LISP_TEST_SLICE=compiler` -> `pass=182 fail=0`
- Remaining experimental repro (legacy fastpath enabled):
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_JIT_CALL_FASTPATH=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=query OMNI_DEDUCE_QUERY_FILTER=admin-surface ./build/main --test-suite lisp`
  - current result: crash (`exit=-1`)
  - representative gdb signature:
    - `lisp.jit_apply_value_impl(...)` receives corrupted `func` pointer
      (`src/lisp/jit_jit_apply_runtime.c3:89`) with corrupted JIT stack frames.
- Current mitigation shipped:
  - JIT helper entry points resolve `Interp*` via active-runtime fallback
    (`jit_resolve_interp(...)`).
  - `jit_compile_call(...)` defaults to continuation-safe helper lowering.
  - legacy spill/register call fastpath is now opt-in via
    `OMNI_JIT_CALL_FASTPATH=1`.

## Operational Guidance

- Treat `OMNI_JIT_CALL_FASTPATH=1` as experimental/unstable until a live TODO
  or owner request promotes the fastpath lane back into active work.
- Keep triage focused on legacy call fastpath argument integrity and register/
  spill restoration boundaries before widening semantic work in Deduce.

## Deferred Actions

This lane is not in the live `TODO.md` queue while the default safe
call-lowering path remains stable and the legacy fastpath is opt-in only.
If the owner wants fastpath parity or default re-enable work, promote a concrete
TODO item first, then:

1. Instrument legacy fastpath call emission (`jit_compile_multi_arg_*` +
   call-site helper trampolines) to locate first corrupted function/argument
   pointer boundary under `OMNI_JIT_CALL_FASTPATH=1`.
2. Repair register/spill restoration discipline in the legacy fastpath.
3. Validate deduce/query admin-surface + full deduce slice with
   `OMNI_JIT_CALL_FASTPATH=1`.
4. Re-enable fastpath by default only after the fastpath-enabled validation lane
   is green.
