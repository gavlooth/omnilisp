# Equality Nested Workspace Benchmark Baseline (2026-03-11)

Purpose: close O3.3 in `TODO.md` by adding a measurable benchmark lane for
large nested equality comparisons (`values_equal(...)`) and recording the
workspace churn baseline.

## Benchmark Surface

Implemented in:
- `src/lisp/tests_memory_lifetime_boundary_decision_bench_groups.c3`
  - suite: `equality_nested_workspace`
  - gate: existing `OMNI_BOUNDARY_BENCH=1`
  - slice: `OMNI_LISP_TEST_SLICE=memory-lifetime-bench`

Workload parameters:
- `iters=2048` per lane
- nested list depth: `1024`
- array length: `1024`
- lanes:
  - list equal (`true`)
  - list tail-mismatch (`false`)
  - array equal (`true`)
  - array tail-mismatch (`false`)

Instrumentation:
- `values_equal_with_workspace_stats(...)` records per-call workspace growth:
  - `stack_heap_growths`, `seen_heap_growths`
  - `stack_heap_bytes`, `seen_heap_bytes`
- benchmark emits aggregate deltas in summary output.

## Baseline Run

Command:

```bash
scripts/run_validation_container.sh env \
  LD_LIBRARY_PATH=/workspace/build/container-libs:/usr/local/lib \
  OMNI_TEST_SUMMARY=1 \
  OMNI_TEST_QUIET=1 \
  OMNI_SKIP_TLS_INTEGRATION=1 \
  OMNI_BOUNDARY_BENCH=1 \
  OMNI_LISP_TEST_SLICE=memory-lifetime-bench \
  ./build/main
```

Log artifact:
- `build/equality_nested_bench_2026-03-11.log`

Captured summary:

```text
OMNI_BENCH_SUMMARY suite=equality_nested_workspace iters=2048 fixture_ok=1 list_depth=1024 array_len=1024 list_true_ms=1304 list_true_ok=2048 list_false_ms=1300 list_false_ok=2048 array_true_ms=20 array_true_ok=2048 array_false_ms=19 array_false_ok=2048 stack_growth=4096 seen_growth=8192 stack_bytes=67108864 seen_bytes=100663296
```

## Interpretation

- Correctness checks are full-pass (`*_ok=2048` for all four lanes).
- Large list comparisons are the dominant cost here.
- Workspace churn is now directly observable in benchmark output and can be
  tracked for regression envelopes in follow-up optimization passes.
