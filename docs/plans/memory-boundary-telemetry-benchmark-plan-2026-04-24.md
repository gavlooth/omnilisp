# Memory Boundary Telemetry Benchmark Plan - 2026-04-24

Status: open planning lane for TODO items `MEM-BENCH-OBSERVE-003` through
`MEM-BENCH-OBSERVE-005`; `MEM-BENCH-OBSERVE-001` and
`MEM-BENCH-OBSERVE-002` are complete.

## Active Hypothesis

The memory model migration is complete enough that the next useful work is
measurement, not architecture churn. The runtime already has boundary route
telemetry, `runtime-memory-stats`, `OMNI_BOUNDARY_BENCH=1`, and the
`memory-lifetime-bench` slice. The missing piece is a structured benchmark and
counter envelope that tells us which future optimization, if any, is worth
opening.

## Current Approach

Maximize observability before changing memory behavior. Add benchmarks and
counter summaries that expose boundary route choice, copy debt, mutation drift,
scope allocator pressure, and collection shape pressure across representative
TEMP/ESCAPE workloads. Treat wall-clock timing as secondary until the counters
show a real hotspot.

## Validation Path

Primary command shape:

```bash
scripts/run_validation_container.sh env \
  LD_LIBRARY_PATH=/usr/local/lib \
  OMNI_TEST_QUIET=1 \
  OMNI_TEST_SUMMARY=1 \
  OMNI_BOUNDARY_BENCH=1 \
  OMNI_BOUNDARY_INSTR_COUNTERS=1 \
  OMNI_LISP_TEST_SLICE=memory-lifetime-bench \
  ./build/main --test-suite lisp
```

Supporting checks:

- `c3c build --obj-out obj`
- counters-enabled build with `-D OMNI_BOUNDARY_INSTR_COUNTERS`
- `scripts/check_status_consistency.sh`
- `git diff --check`
- bounded `memory-lifetime-smoke` after any runtime instrumentation change
- bounded Valgrind only after runtime memory accounting changes, not for
  documentation-only edits

## Information To Collect

Boundary route and copy information:

- planned route by route and `ValueTag`
- selected route by route and `ValueTag`
- fallback-copy count and route reason
- stable materialization success count
- stable materialization node count by root tag
- stable materialization estimated copy bytes by root tag
- scope-splice attempts, success, and failure reason
- promotion attempts and abort reason
- graph-audit invocation/skipped counts
- scope-chain scan total, hinted, fallback, and suppressed counts

Scope allocator and lifetime information:

- TEMP and ESCAPE slow-path allocation count
- requested slow-path allocation size
- selected chunk capacity
- unused bytes at scope reset/release
- peak live chunks by lane
- scope recycle/freelist hits and fresh allocations
- fiber-temp take/return/drop behavior by chunk capacity

Collection and payload shape information:

- array length/capacity at construction
- array push growth events
- dictionary requested capacity, live count, and growth events
- set/member growth events
- closure env depth and binding count at boundary copy/materialization
- string/error/big-number payload byte sizes crossing boundaries
- tensor/FFI handle wrapper crossings and release-authority classification

Workload information:

- large list return and accumulator return
- large array return and mutation-drift return
- dictionary/set return with shared children
- cyclic container publication and materialization
- closure capture/env copy return
- no-splice rollback coverage bucket
- BigInteger/String/Error payload returns
- lazy tensor metadata crossing without concrete tensor payload copies
- FFI/foreign handle wrapper crossing without Omni graph traversal

## Slices

### `MEM-BENCH-OBSERVE-001` - Existing Signal Inventory

Map current counters, bench summary lines, runtime-memory-stats fields, and
boundary profile scripts. Output a concise table of existing fields, missing
fields, source files, and whether each field is available in normal,
counters-enabled, trace, or benchmark builds.

### `MEM-BENCH-OBSERVE-002` - Counter Coverage Expansion

Add missing low-overhead counters for allocator pressure, collection shapes,
payload byte sizes, and wrapper crossings. Counters must be compile-time gated
or existing-debug-path gated so the default runtime stays zero-overhead for hot
paths.

### `MEM-BENCH-OBSERVE-003` - Workload Suite Expansion

Extend `memory-lifetime-bench` with representative TEMP/ESCAPE workloads that
exercise the boundary planner ladder and known fallback cases. Each benchmark
must print one stable `OMNI_BENCH_SUMMARY` line with correctness fields and
counter deltas.

### `MEM-BENCH-OBSERVE-004` - Baseline Capture

Run the bounded benchmark suite on the current implementation, archive the
summary output, and document the baseline interpretation. This slice should
identify the top two copy/allocator hotspots or explicitly record that no
runtime optimization is justified yet.

### `MEM-BENCH-OBSERVE-005` - Regression Envelope

Add a parser/check script that compares benchmark summaries against a checked-in
baseline. The first envelope should gate on counters and correctness, with
wide timing warnings only. Strict timing failure thresholds should wait until
the benchmark is stable across repeated container runs.

## Completed Slices

- `MEM-BENCH-OBSERVE-001`: existing signal inventory is recorded in
  `docs/plans/memory-boundary-telemetry-signal-inventory-2026-04-24.md`.
- `MEM-BENCH-OBSERVE-002`: low-overhead counter coverage is implemented and
  exposed through `runtime-memory-stats`/`OMNI_MEM_TELEMETRY` for allocator
  pressure, value-shape dimensions, wrapper authority classes, and stable
  passport invalidation reasons.

## Next Checkpoint

The next decision point is completion of `MEM-BENCH-OBSERVE-003`: extend
`memory-lifetime-bench` workloads so the new counter fields emit meaningful
`OMNI_BENCH_SUMMARY` deltas before baseline capture.

## Negative-Memory Constraints

- Do not reopen closed memory-boundary copy-debt items to chase the expected
  no-splice closure rollback coverage bucket.
- Do not weaken TEMP-edge proof rejection, mutation-epoch invalidation, or
  fail-closed planner behavior to improve benchmark numbers.
- Do not add hidden CPU fallback, root pinning, or per-language-value RC as a
  performance shortcut.
- Do not add strict wall-clock gates until repeated bounded-container runs prove
  the timing envelope is stable.
