# Boundary Hot/Cold Helper Inventory

Date: 2026-03-09
Owner area: boundary diagnostics / commit-finalize path

## Classification Legend

- `hot-required`: used on normal boundary return path and must stay branch-light.
- `cold-diagnostic`: reporting/formatting helpers only; no success-path side effects.
- `mixed`: helper previously combined result construction and diagnostics.

## Inventory

| Helper | File | Classification | Notes |
|---|---|---|---|
| `boundary_graph_audit_result_hot(...)` | `src/lisp/eval_boundary_diagnostics.c3` | `hot-required` | Pure result construction, no formatting/logging. |
| `boundary_graph_audit_result_emit(...)` | `src/lisp/eval_boundary_diagnostics.c3` | `cold-diagnostic` | `@noinline`, emits graph-audit reason text only. |
| `boundary_graph_audit_result(...)` | `src/lisp/eval_boundary_diagnostics.c3` | `mixed` -> split selector | Now only a lightweight selector routing to hot/cold helper. |
| `boundary_scope_transfer_reject(...)` | `src/lisp/eval_boundary_diagnostics.c3` | `mixed` -> split behavior | Hot reject result + conditional jump to cold report path when graph audit is enabled. |
| `boundary_debug_graph_audit_report_scope_transfer(...)` | `src/lisp/eval_boundary_diagnostics.c3` | `cold-diagnostic` | `@noinline`, verbose scope-lane reject report. |

## Mixed Helper Callsite Counts

Counts are from `src/lisp/eval_boundary_diagnostics.c3` and include only call sites (definition excluded).

- `boundary_graph_audit_result(...)`: 18 call sites
- `boundary_scope_transfer_reject(...)`: 9 call sites

## Outcome

- Mixed helpers were split so that formatting/reporting work is isolated in `@noinline` cold paths.
- Hot result construction now avoids string formatting and diagnostic side effects.
