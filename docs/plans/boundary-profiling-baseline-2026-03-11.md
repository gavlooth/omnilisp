# Boundary Profiling Baseline (2026-03-11)

Purpose: satisfy O1.1 from `TODO.md` by recording a Docker-capped baseline
for boundary-heavy workloads with current telemetry surfaces.

## Run Profile

- Date: 2026-03-11
- Workload slice: `memory-lifetime-bench`
- Runtime flags:
  - `OMNI_TEST_SUMMARY=1`
  - `OMNI_TEST_QUIET=1`
  - `OMNI_SKIP_TLS_INTEGRATION=1`
  - `OMNI_BOUNDARY_VERBOSE_TELEMETRY=1`
  - `OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1`
  - `OMNI_BOUNDARY_BENCH=1`
- Container-capped runner:
  - `scripts/run_validation_container.sh`
  - monitor log: `build/docker_validation_container_stats.log`
- Runtime log:
  - `build/boundary_profile_memory_lifetime_bench_2026-03-11.log`

Command used:

```bash
scripts/run_validation_container.sh env \
  LD_LIBRARY_PATH=/workspace/build/container-libs:/usr/local/lib \
  OMNI_TEST_SUMMARY=1 \
  OMNI_TEST_QUIET=1 \
  OMNI_SKIP_TLS_INTEGRATION=1 \
  OMNI_BOUNDARY_VERBOSE_TELEMETRY=1 \
  OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 \
  OMNI_BOUNDARY_BENCH=1 \
  OMNI_LISP_TEST_SLICE=memory-lifetime-bench \
  ./build/main
```

Note: container image runtime lacked `libreplxx.so.0`; run used
`build/container-libs/libreplxx.so.0` as a narrow runtime shim.

## Baseline Summary

From `OMNI_BENCH_SUMMARY`:

- `boundary_decision_cost`: `iters=2048 splice_ms=3 disallowed_ms=5 reuse_ms=1`
- `boundary_destination_routed_escape`: `iters=2048 partial_ms=2`
- `scope_splice_tail`: `iters=2048 escapes_per_iter=64 splice_ms=1`

From `OMNI_TEST_SUMMARY suite=boundary_decisions`:

- `splice_attempted=2048`
- `splice_succeeded=2048`
- `splice_fail_total=0`
- `promotion_attempted=2048`
- `promotion_aborted_budget=0`
- `promotion_aborted_pre_aborted=2048`
- `copy_fallback_total=0`

From verbose boundary telemetry:

- `scope_chain_scan_total=19045`
- `scope_chain_scan_with_hint=19045`
- `scope_chain_scan_fallback=12683`
- `scope_chain_scan_suppressed=0`
- `graph_audit_invoked=0`
- `graph_audit_skipped_rate=0`
- `graph_audit_skipped_max_roots=0`

From `OMNI_TEST_SUMMARY suite=boundary_traversal`:

- `copy_total=10381`
- `copy_fast_reuse=4097`
- `copy_defensive=6284`
- `promoted_then_spliced=2048`
- `promoted_then_fallback_copied=0`

## Derived Scope-Chain Ratios

Derived from:
- `scope_chain_scan_with_hint=19045`
- `scope_chain_scan_fallback=12683`
- `scope_chain_scan_total=19045`

Computed:
- Hint hit count: `6362` (`19045 - 12683`)
- Hint hit ratio: `33.41%`
- Hint miss ratio (fallback from hint path): `66.59%`
- Scan suppression ratio: `0.00%` (`scope_chain_scan_suppressed=0`)

## Return-Path Outcome Mix (Current Boundary-Bench Workload)

Observed benchmark outcome counters:
- Splice-commit success: `2048`
- Disallowed fallback outcome: `2048`
- Reuse-in-target-chain outcome: `2048`
- Destination-built partial outcome: `2048`

Interpretation for this workload:
- No single return-path outcome dominates; the benchmark lane is balanced
  across these outcome classes (`25%` each of the measured `8192` outcomes).
- `direct-promoted` is not represented as an explicit benchmark counter in this
  lane and should be added in follow-up instrumentation if dominance tracking
  for that class is required.

## Accepted Regression Envelope (Boundary-Heavy Workload)

Until a dedicated threshold checker is added for `memory-lifetime-bench`, use
the following acceptance envelope for this exact profile (`iters=2048`):

- Correctness counters:
  - `splice_ok == 2048`
  - `disallowed_ok == 2048`
  - `reuse_ok == 2048`
  - `partial_ok == 2048`
  - `copy_fallback_total == 0`
  - `splice_fail_total == 0`
- Latency counters:
  - `boundary_decision_cost.splice_ms <= 10`
  - `boundary_decision_cost.disallowed_ms <= 12`
  - `boundary_decision_cost.reuse_ms <= 6`
  - `boundary_destination_routed_escape.partial_ms <= 8`
  - `scope_splice_tail.splice_ms <= 6`
- Scan-pressure envelope:
  - `scope_chain_scan_suppressed == 0`
  - hint miss ratio (`fallback / with_hint`) should remain `<= 0.75`

Rationale:
- Keeps correctness strict (all benchmark outcomes must validate at full
  iteration count).
- Allows bounded jitter on wall-clock milliseconds while preserving a practical
  guard against large regressions.

## Status

- O1.1 baseline run and evidence capture: complete.
- O1.2 scope-chain pressure capture: complete.
- O1.3 return-path dominance capture: complete.
- O1.4 accepted regression envelope capture: complete.

## Revalidation (2026-03-18)

- Container-capped rerun on the current tree remained within the accepted
  envelope for the boundary-heavy benchmark lane.
- Observed benchmark summaries on the rerun:
  - `boundary_decision_cost`: `iters=2048 splice_ms=3 disallowed_ms=4 reuse_ms=1`
  - `boundary_destination_routed_escape`: `iters=2048 partial_ms=2 partial_ok=2048`
  - `scope_splice_tail`: `iters=2048 escapes_per_iter=64 splice_ms=2`
- The rerun completed with `Unified Tests: 0 passed, 0 failed`.
