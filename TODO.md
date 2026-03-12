# Active TODO

Last condensed: 2026-03-11

This file now tracks only active, actionable work.
Full completed history is archived at:
- `docs/plans/TODO_ARCHIVE_2026-03-11.md`

Current actionable count: 6

## Language Surface Consistency (2026-03-12)

- [ ] Decide the public fate of compatibility/runtime factory aliases:
  `make-iterator`, lowercase `iterator`, lowercase `list`/`array`/`dict`/`time-point`, and similar surfaces still exist for compatibility; decide which remain public, which become deprecated, and which should disappear from docs/examples.
- [ ] Decide whether abstract/meta builtin type symbols remain type-only:
  `Any`, `Value`, `Number`, and `Collection` currently participate in type annotations and dispatch but do not have callable value-position constructor/coercion behavior.
- [ ] Decide whether the current `false`/`nil` collapse is a stable language rule:
  the new `Boolean`/`Bool` and `Nil` constructor contracts currently reflect the existing runtime truthiness model; if `Boolean` and `Nil` should become distinct runtime values later, constructor semantics and docs will need another pass.
- [ ] Finish the descriptive-name audit for remaining shorthand-heavy surfaces:
  `Ptr`, low-level namespace abbreviations like `fs-*`/`tcp-*`/`udp-*`/`tls-*`/`dns-*`, and any other public spellings that still prioritize brevity over clarity should be reviewed explicitly instead of drifting ad hoc.
- [ ] Run a Docker-capped full constructor-surface conformance pass:
  validate the constructor/type-symbol behavior across the advanced/e2e gates inside the validation container, not just the host-safe build/basic-smoke path.

## Active Failure Recovery (2026-03-11)

- [x] Fix `advanced`: `non-tail recursion exceeds former 1024 eval cap` (interp + jit parity).
- [x] Fix `advanced`: `parser matrix accepts Value bool constructor`.
- [x] Fix `advanced`: `match Some` regression (`got 0`).
- [x] Fix `advanced`: `match nested Some` regression (`got 0`).
- [x] Fix `async`: `dns-resolve localhost (fiber)` (`got='?'`).
- [x] Fix `async`: `dns-resolve returns string (fiber)`.
- [x] Re-run Docker-capped `advanced` and `async` slices and record clean pass.

## Optimization Follow-Up Queue (2026-03-10)

### O1. Boundary Profiling and Evidence Capture

- [x] Run a Docker-capped profiling pass for boundary-heavy workloads and record the baseline counters/trace summary. (`docs/plans/boundary-profiling-baseline-2026-03-11.md`)
- [x] Capture scope-chain scan pressure and hint-hit/miss ratios from the current boundary telemetry surface. (`docs/plans/boundary-profiling-baseline-2026-03-11.md`)
- [x] Identify which return-path outcomes dominate in practice (`reused`, destination-built, direct-promoted, spliced, disallowed). (`docs/plans/boundary-profiling-baseline-2026-03-11.md`)
- [x] Record the accepted regression envelope for boundary-heavy workloads in docs or scripts. (`docs/plans/boundary-profiling-baseline-2026-03-11.md`)

### O2. Dispatch Hot-Path Allocation Reduction

- [x] Add focused micro-bench coverage for method dispatch and typed lambda call boundaries before and after the allocation changes. (`docs/plans/dispatch-hot-path-benchmark-baseline-2026-03-11.md`)

### O3. Structural Equality Workspace Reuse

- [x] Audit `src/lisp/eval_pattern_support.c3` deep equality workspace allocation (`stack`/`seen`) under nested list/array comparisons. (`docs/plans/equality-workspace-audit-2026-03-11.md`)
- [x] Introduce a bounded scratch or inline-first workspace strategy that preserves cycle safety and current semantics. (`src/lisp/eval_pattern_support.c3`)
- [x] Add a regression/benchmark slice for large nested equality comparisons so allocator churn is measurable. (`docs/plans/equality-nested-benchmark-baseline-2026-03-11.md`)

### O4. Scheduler and Offload Throughput

- [x] Add micro-bench coverage for scheduler + async I/O/offload interaction hotspots (`queue`, completion publish, TLS/http offload). (`docs/plans/scheduler-offload-hot-path-benchmark-baseline-2026-03-11.md`)
- [x] Audit per-request heap allocation in `src/lisp/scheduler_offload_worker.c3` and classify reusable worker-local buffers versus required owned outputs. (`docs/plans/scheduler-offload-allocation-audit-2026-03-11.md`)
- [x] Prototype a narrow pool/reuse strategy for offload request/completion scaffolding without changing boundary ownership semantics. (`docs/plans/scheduler-offload-queued-work-pool-prototype-2026-03-11.md`)
- [x] Validate that any pooling change preserves generation/task handoff correctness and offload failure cleanup. (`docs/plans/scheduler-offload-queued-work-pool-prototype-2026-03-11.md`)

### O5. Deduce Scan/Query Throughput

- [x] Add benchmarks for `deduce` scan/query/count paths at corpus sizes large enough to expose regression envelopes. (`docs/plans/deduce-scan-query-count-benchmark-baseline-2026-03-11.md`)
- [x] Measure row materialization cost in `relation_scan_range(...)`, including per-row hashmap creation and per-column symbol-key allocation. (`docs/plans/deduce-scan-range-materialization-cost-baseline-2026-03-11.md`)
- [x] Reduce avoidable scan-path allocation where semantics allow (for example cached relation key values or other stable row-shape helpers). (`docs/plans/deduce-scan-range-key-cache-optimization-2026-03-11.md`)
- [x] Evaluate whether `deduce-query` should stay as full-scan + callback filtering or gain a narrower optimization path for common predicates. (`docs/plans/deduce-query-optimization-evaluation-2026-03-11.md`)

### O6. Largest-First Runtime Modularization

- [x] Continue largest-first modularization for oversized runtime modules that still combine hot-path logic with diagnostics or explainability helpers. (`docs/plans/runtime-modularization-split-2026-03-11.md`)
- [x] Prioritize split candidates by size and hot-path relevance (`schema.c3`, `eval_dispatch_types.c3`, `scheduler_offload_worker.c3`). (`docs/plans/runtime-modularization-split-2026-03-11.md`)
- [x] Keep behavior unchanged while splitting, and pair each split slice with targeted validation for the touched subsystem. (`docs/plans/runtime-modularization-split-2026-03-11.md`)
