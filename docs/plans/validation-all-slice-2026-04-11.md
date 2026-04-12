# Validation All-Slice Follow-Up (2026-04-11)

Status: `closed`
Owner: Codex workflow
Mode: validation triage plan
TODO lane: `VALIDATION-ALL-SLICE-2026-04-11`

## Purpose

Track the non-Tensor failures found during the bounded all-slice validation
after `TENSOR-060B`, without mixing them into the Tensor surface work.

The original bounded all-slice run:

```bash
OMNI_LISP_TEST_SLICE=all ./build/main --test-suite lisp
```

ended at `2554 passed, 222 failed`. Targeted Tensor and memory-lifetime gates
for the touched Tensor work passed, so this plan tracks the unrelated red
clusters separately.

## Current State

Fixed:

- TOML option arity cluster:
  - Root cause: `toml-parse` was registered as a one-argument primitive even
    though `prim_toml_parse` already accepts and validates one or two
    arguments.
  - Fix: register `toml-parse` as variadic so both accepted call shapes reach
    the primitive's existing validation.
  - Validation: targeted `data-format` slice now passes at `pass=64 fail=0`.

Resolved:

- Deduce relation-form metadata cluster:
  - Repro:
    `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=relation-attrs`.
  - Root cause: direct syntax for relation attributes and materialized metadata
    calls still flows through `__define-relation`, which was incorrectly
    registered as fixed arity.
  - Fix: register `__define-relation` as variadic in `eval_init_primitive_tables.c3`.
  - Validation: `pass=13 fail=0`.

- Deduce CRUD example setup:
  - Repro:
    `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=core-runtime`.
  - Validation: `pass=6 fail=0`.

- Deduce rule-validation cluster:
  - Repro:
    `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=rule-validation`.
  - Validation: `pass=70 fail=0`.

- Diagnostics match/union cluster:
  - Repro:
    `OMNI_LISP_TEST_SLICE=diagnostics`.
  - Root cause: constructor patterns with zero subpatterns called
    `alloc_ast_array_bytes(..., count=0)`, which set the parser's OOM error
    even though zero-length arrays are intentionally represented as `null`.
  - Fix: `alloc_ast_array_bytes` now returns `null` for zero count without
    setting a parser error.
  - Validation: `pass=10 fail=0`.

- Bounded all-slice rerun:
  - Validation:
    `scripts/run_validation_container.sh bash -lc 'c3c build --warn-deprecation=no && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=all ./build/main --test-suite lisp'`
  - Scope note: because this command sets `OMNI_SKIP_TLS_INTEGRATION=1`, the
    result is a bounded all-slice-without-TLS-integration baseline, not a
    replacement for the TLS integration gate.
  - Intermediate result: unified `pass=2797 fail=1`, compiler
    `pass=208 fail=0`.
  - The residual was split to
    `docs/plans/validation-all-slice-nested-let-residual-2026-04-11.md` and is
    now closed.
  - Final result: unified `pass=2798 fail=0`, compiler `pass=208 fail=0`.

## Next Steps

1. Keep this parent plan closed unless a new broad validation family appears.

## Validation Commands

```bash
c3c build --warn-deprecation=no

env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 \
  OMNI_LISP_TEST_SLICE=data-format \
  ./build/main --test-suite lisp

env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 \
  OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=relation-attrs \
  ./build/main --test-suite lisp

env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 \
  OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=core-runtime \
  ./build/main --test-suite lisp

env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 \
  OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=rule-validation \
  ./build/main --test-suite lisp
```

Use the bounded Docker validation path before re-running the full all-slice
gate.
