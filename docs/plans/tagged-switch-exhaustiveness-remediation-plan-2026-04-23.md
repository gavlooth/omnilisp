# Tagged Switch Exhaustiveness Remediation Plan - 2026-04-23

This plan records the remaining audit-class switch/default hardening queue in
`/home/christos/Omni`. `TODO.md` remains the live execution queue. This plan is
only complete when every listed switch site has been rewritten to explicit
exhaustive handling or moved behind a named fail-closed helper with tests. No
silent `default:` arms remain in the audited files.

## Current Boundary

The pane-captured audit surfaced a single class of work: hidden `default:`
branches that keep tagged switches or status switches from being explicit.
Those branches are the remaining audit surface after the broader runtime and
scientific-module work already shipped.

The remediation boundary is all-or-nothing:

- if the switch is over a closed enum/tag set, replace the `default:` with
  explicit cases for every known tag;
- if the switch is over a truly open set, move the unknown branch into a named
  helper with explicit fail-closed semantics and tests;
- do not keep silent `default:` fallthrough in the touched files;
- do not split the queue into partial “good enough” cleanups.

## Implementation Targets

The current audit list is:

- `src/lisp/compiler_mutable_capture_detection_walk.c3`
- `src/lisp/prim_tensor_capture_memory_plan.c3`
- `src/lisp/prim_ml_reduction.c3`
- `src/lisp/prim_tensor_matrix_qr_cholesky.c3`
- `src/lisp/prim_tensor_matrix_factor_primitives.c3`
- `src/lisp/prim_tensor_matrix.c3`
- `src/lisp/prim_tensor_map_eval.c3`
- `src/lisp/parser_parser.c3`
- `src/lisp/macros_define_hygiene.c3`

Expected remediation shape by file family:

1. Compiler / parser / macro walkers:
   - make every `ExprTag` / token / pattern switch explicit;
   - keep parser errors fail-closed and preserve existing diagnostics;
   - if a truly open-ended branch remains necessary, isolate it in a named
     helper and document why it cannot be exhaustive.
2. Tensor capture / reduction / matrix / map evaluation:
   - replace hidden catch-alls in payload-kind, op-status, and helper dispatch
     switches with explicit cases;
   - preserve the current runtime behavior for supported cases;
   - make unsupported or unknown variants fail closed through explicit error
     helpers rather than implicit `default:` returns.

## Work Items

- `AUDIT-SWITCH-EXHAUST-001` compiler/parser/macro exhaustiveness cleanup.
  - source: live tmux audit capture from pane 1 / window 0.
  - next: remove hidden `default:` branches from
    `compiler_mutable_capture_detection_walk.c3`, `parser_parser.c3`, and
    `macros_define_hygiene.c3`, then add focused regressions for the explicit
    fallback/error paths.

- `AUDIT-SWITCH-EXHAUST-002` tensor capture and tensor math exhaustiveness
  cleanup.
  - source: live tmux audit capture from pane 1 / window 0.
  - next: remove hidden `default:` branches from
    `prim_tensor_capture_memory_plan.c3`, `prim_ml_reduction.c3`,
    `prim_tensor_matrix_qr_cholesky.c3`, `prim_tensor_matrix_factor_primitives.c3`,
    `prim_tensor_matrix.c3`, and `prim_tensor_map_eval.c3`, then add explicit
    regressions for each fail-closed branch.

- `AUDIT-SWITCH-EXHAUST-003` validation and closure sweep.
  - source: this remediation plan.
  - next: after code changes land, run the full targeted validation sweep,
    confirm the audited files no longer contain unapproved hidden `default:`
    arms, and close the queue only when the checked-in behavior and tests
    match the new explicit cases.

## Validation Path

Minimum validation for the implementation slices:

- `c3c build --obj-out obj`
- targeted focused tests for compiler/parsing and tensor/module regressions
- `rg -n "default:" src/lisp/compiler_mutable_capture_detection_walk.c3 src/lisp/prim_tensor_capture_memory_plan.c3 src/lisp/prim_ml_reduction.c3 src/lisp/prim_tensor_matrix_qr_cholesky.c3 src/lisp/prim_tensor_matrix_factor_primitives.c3 src/lisp/prim_tensor_matrix.c3 src/lisp/prim_tensor_map_eval.c3 src/lisp/parser_parser.c3 src/lisp/macros_define_hygiene.c3`
- `git diff --check`

If any `default:` remains in those audited files, the remediation is not
complete.

## Negative-Memory Constraints

- Do not leave hidden `default:` arms in the audited switches as a temporary
  state.
- Do not convert this into a docs-only audit note; the queue is intended to
  ship code changes and regression tests.
- Do not claim closure while any audited switch still depends on a silent
  catch-all.
- Do not use broad “future cleanup” wording to defer the explicit-case work.

## Completion

- status: completed 2026-04-23.
- code landed: the audited compiler/parser/macro and tensor-matrix/reduction
  files now enumerate the current enum/status sets explicitly and no longer
  rely on hidden `default:` arms.
- validation:
  - `c3c build --obj-out obj`
  - `OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `rg -n "default:"` over the audited files
  - `git diff --check`
