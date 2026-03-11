# Active TODO

Last condensed: 2026-03-11

This file now tracks only active, actionable work.
Full completed history is archived at:
- `docs/plans/TODO_ARCHIVE_2026-03-11.md`

Current actionable count: 19

## Optimization Follow-Up Queue (2026-03-10)

### O1. Boundary Profiling and Evidence Capture

- [ ] Run a Docker-capped profiling pass for boundary-heavy workloads and record the baseline counters/trace summary.
- [ ] Capture scope-chain scan pressure and hint-hit/miss ratios from the current boundary telemetry surface.
- [ ] Identify which return-path outcomes dominate in practice (`reused`, destination-built, direct-promoted, spliced, disallowed).
- [ ] Record the accepted regression envelope for boundary-heavy workloads in docs or scripts.

### O2. Dispatch Hot-Path Allocation Reduction

- [ ] Add focused micro-bench coverage for method dispatch and typed lambda call boundaries before and after the allocation changes.

### O3. Structural Equality Workspace Reuse

- [ ] Audit `src/lisp/eval_pattern_support.c3` deep equality workspace allocation (`stack`/`seen`) under nested list/array comparisons.
- [ ] Introduce a bounded scratch or inline-first workspace strategy that preserves cycle safety and current semantics.
- [ ] Add a regression/benchmark slice for large nested equality comparisons so allocator churn is measurable.

### O4. Scheduler and Offload Throughput

- [ ] Add micro-bench coverage for scheduler + async I/O/offload interaction hotspots (`queue`, completion publish, TLS/http offload).
- [ ] Audit per-request heap allocation in `src/lisp/scheduler_offload_worker.c3` and classify reusable worker-local buffers versus required owned outputs.
- [ ] Prototype a narrow pool/reuse strategy for offload request/completion scaffolding without changing boundary ownership semantics.
- [ ] Validate that any pooling change preserves generation/task handoff correctness and offload failure cleanup.

### O5. Deduce Scan/Query Throughput

- [ ] Add benchmarks for `deduce` scan/query/count paths at corpus sizes large enough to expose regression envelopes.
- [ ] Measure row materialization cost in `relation_scan_range(...)`, including per-row hashmap creation and per-column symbol-key allocation.
- [ ] Reduce avoidable scan-path allocation where semantics allow (for example cached relation key values or other stable row-shape helpers).
- [ ] Evaluate whether `deduce-query` should stay as full-scan + callback filtering or gain a narrower optimization path for common predicates.

### O6. Largest-First Runtime Modularization

- [ ] Continue largest-first modularization for oversized runtime modules that still combine hot-path logic with diagnostics or explainability helpers.
- [ ] Prioritize split candidates by size and hot-path relevance (`schema.c3`, `eval_dispatch_types.c3`, `scheduler_offload_worker.c3`).
- [ ] Keep behavior unchanged while splitting, and pair each split slice with targeted validation for the touched subsystem.
