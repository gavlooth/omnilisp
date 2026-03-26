# Interp State Runtime Cleanup (2026-03-19)

Status: `complete`
As of: 2026-03-19
Owner: Codex workflow

## Purpose

Track the next runtime cleanup lane after the closeout of
the now-removed runtime/backend backlog tracker that has since been
consolidated into `TODO.md`.

This lane is narrower than the previous backlog. It is focused on the
interpreter-state surface and the files immediately coupled to it, not a broad
runtime-wide queue.

## Rationale

- `src/lisp/value_interp_state.c3` is now the largest consequential non-test
  runtime file on the interpreter side.
- It sits on a sensitive boundary:
  - interpreter state definition,
  - symbol/bootstrap initialization,
  - allocator helpers,
  - module/macro runtime wiring.
- Cleaning this surface improves auditability without reopening generic
  split-by-size work.

## Rules

1. Keep `Interp` layout and public initialization/allocator entrypoints stable
   unless a semantic change is explicitly intended.
2. Prefer moving cohesive helper blocks out behind the existing public
   entrypoints.
3. Validate every landed slice with:
   - `c3c build`
   - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
4. Record landed slices in `memory/CHANGELOG.md`.

## Queue

### 1. Reduce `value_interp_state.c3` helper ownership

Why:
- the file still mixes state definition with many setup helpers.
- symbol/bootstrap wiring is a clean first split boundary.

Acceptance:
- `src/lisp/value_interp_state.c3` keeps:
  - `Interp` / `InterpFlags`
  - public `Interp.init(...)`
  - stack/apply helpers
- helper-heavy initialization blocks move into dedicated companion files.
- allocator entrypoints may move into companion helper files as long as their
  public surface stays stable.

Current state (2026-03-19):
- complete
- split symbol/module/runtime-flag/macro initialization helpers out of
  `src/lisp/value_interp_state.c3`
  into `src/lisp/value_interp_init_helpers.c3`
- split allocator helpers out of `src/lisp/value_interp_state.c3`
  into `src/lisp/value_interp_alloc_helpers.c3`
- split private runtime bootstrap and apply-frame helpers out of
  `src/lisp/value_interp_state.c3`
  into `src/lisp/value_interp_runtime_helpers.c3`
- split continuation/resume helpers out of
  `src/lisp/value_interp_state.c3`
  into `src/lisp/value_interp_continuation_helpers.c3`
- `src/lisp/value_interp_state.c3` now keeps:
  - `InterpFlags`
  - `Interp`
  - public `Interp.init(...)`
- resulting file sizes:
  - `src/lisp/value_interp_state.c3`: `234` lines
  - `src/lisp/value_interp_init_helpers.c3`: `118` lines
  - `src/lisp/value_interp_alloc_helpers.c3`: `70` lines
  - `src/lisp/value_interp_runtime_helpers.c3`: `61` lines
  - `src/lisp/value_interp_continuation_helpers.c3`: `23` lines
- validation is green:
  - `c3c build`
  - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    (`9/9 runs passed`)

### 2. Separate allocator/bootstrap helpers from interpreter state definition

Why:
- allocation helpers are the next cohesive block still embedded in the state
  file.
- moving them leaves the main file closer to “state + public lifecycle”.

Primary targets:
- `src/lisp/value_interp_state.c3`
- new companion helper module(s) under `src/lisp/`

Acceptance:
- allocator helpers (`alloc_value*`, `alloc_env*`, `alloc_expr`, `alloc_pattern`)
  no longer live beside the full `Interp` struct definition.
- no regression in runtime bootstrap or validation summary gates.

Current state (2026-03-19):
- complete
- moved allocator helpers into `src/lisp/value_interp_alloc_helpers.c3`
- `src/lisp/value_interp_state.c3` is now focused on interpreter state,
  public initialization, resume/apply helpers, and runtime bootstrap
- validation is green:
  - `c3c build`
  - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    (`9/9 runs passed`)

## Validation

- `c3c build`
- `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
