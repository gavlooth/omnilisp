# Runtime Effects Cleanup (2026-03-19)

Status: `complete`
As of: 2026-03-19
Owner: Codex workflow

## Purpose

Track the next runtime cleanup lane after the boundary graph audit queue was
closed.

This queue is focused on the JIT/runtime effects file, which still mixes
checkpoint/capture/signal entrypoints with continuation-resume mechanics.

## Rationale

- `src/lisp/jit_jit_runtime_effects.c3` is the next large consequential
  runtime file.
- It sits on a sensitive control-flow boundary:
  - checkpoint/reset runtime,
  - signal fast-path dispatch,
  - continuation validation/resume,
  - multi-shot handler continuation cloning.
- Splitting it top-down improves auditability without changing the effect
  surface or the existing bounded validation gates.

## Rules

1. Keep the public runtime effect entrypoints stable unless a semantic change
   is explicitly intended.
2. Prefer moving continuation/resume helper blocks behind the existing public
   entrypoints.
3. Validate every landed slice with:
   - `c3c build`
   - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
4. Record landed slices in `memory/CHANGELOG.md`.

## Queue

### 1. Reduce `jit_jit_runtime_effects.c3` helper ownership

Why:
- continuation validation/resume is the largest cohesive helper block.
- checkpoint and signal entrypoints should remain easy to read.

Acceptance:
- `src/lisp/jit_jit_runtime_effects.c3` keeps the public effect entrypoints.
- continuation/resume helper logic moves into dedicated companion files.

Current state (2026-03-19):
- complete
- split continuation validation/resume helpers out of
  `src/lisp/jit_jit_runtime_effects.c3`
  into `src/lisp/jit_jit_runtime_effects_continuation.c3`
- split checkpoint/reset and capture helpers out of
  `src/lisp/jit_jit_runtime_effects.c3`
  into `src/lisp/jit_jit_runtime_effects_reset_shift.c3`
- split signal fast-path and handler-resume helpers out of
  `src/lisp/jit_jit_runtime_effects.c3`
  into `src/lisp/jit_jit_runtime_effects_signal.c3`
- `src/lisp/jit_jit_runtime_effects.c3` now keeps:
  - public resolve/continuation application entrypoints
- resulting file sizes:
  - `src/lisp/jit_jit_runtime_effects.c3`: `153` lines
  - `src/lisp/jit_jit_runtime_effects_continuation.c3`: `137` lines
  - `src/lisp/jit_jit_runtime_effects_reset_shift.c3`: `120` lines
  - `src/lisp/jit_jit_runtime_effects_signal.c3`: `84` lines
- validation is green:
  - `c3c build`
  - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    (`9/9 runs passed`)

## Validation

- `c3c build`
- `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
