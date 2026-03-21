# Boundary Graph Audit Cleanup (2026-03-19)

Status: `complete`
As of: 2026-03-19
Owner: Codex workflow

## Purpose

Track the next runtime cleanup lane after the interpreter-state surface was
reduced to state definition plus public initialization.

This queue is focused on the boundary graph audit path, which is still a large
runtime file and sits directly on the memory hardening/debug boundary.

## Rationale

- `src/lisp/eval_boundary_graph_audit.c3` was the largest consequential
  non-test runtime file after the interpreter-state cleanup.
- It mixes:
  - the public debug/telemetry entrypoints,
  - the graph reachability traversal,
  - verbose boundary telemetry dumping.
- Splitting it top-down improves auditability without changing the hardening
  surface or validation gates.

## Rules

1. Keep the public graph-audit/debug entrypoints stable unless a semantic
   change is explicitly intended.
2. Prefer moving traversal/reporting helper blocks behind the existing public
   entrypoints.
3. Validate every landed slice with:
   - `c3c build`
   - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
4. Record landed slices in `memory/CHANGELOG.md`.

## Queue

### 1. Reduce `eval_boundary_graph_audit.c3` helper ownership

Why:
- the reachability traversal is the obvious first ownership block.
- the public debug/telemetry boundary should remain easy to read.

Acceptance:
- `src/lisp/eval_boundary_graph_audit.c3` keeps:
  - public audit/debug/telemetry entrypoints
  - thin boundary flow only
- traversal-heavy reachability logic moves into dedicated companion files.

Current state (2026-03-19):
- complete
- split the reachability traversal out of
  `src/lisp/eval_boundary_graph_audit.c3`
  into `src/lisp/eval_boundary_graph_audit_reachability.c3`
- split the verbose telemetry dump helpers out of
  `src/lisp/eval_boundary_graph_audit.c3`
  into `src/lisp/eval_boundary_graph_audit_telemetry.c3`
- `src/lisp/eval_boundary_graph_audit.c3` now keeps:
  - `boundary_graph_audit_escape_reachability(...)`
  - `boundary_debug_graph_audit_committed_escape_root(...)`
- resulting file sizes:
  - `src/lisp/eval_boundary_graph_audit.c3`: `52` lines
  - `src/lisp/eval_boundary_graph_audit_reachability.c3`: `381` lines
  - `src/lisp/eval_boundary_graph_audit_telemetry.c3`: `46` lines
- validation is green:
  - `c3c build`
  - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    (`9/9 runs passed`)

## Validation

- `c3c build`
- `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
