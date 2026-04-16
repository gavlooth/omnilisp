# Dispatch Hot-Path Benchmark Baseline (2026-03-11)

Purpose: satisfy O2 in `TODO.md` by adding and recording focused micro-bench
coverage for:
- multiple dispatch hot-path calls,
- typed lambda call-boundary checks.

## Benchmark Surface

Implemented in:
- `src/lisp/tests_advanced_type_dispatch_groups.c3`
  - `run_advanced_dispatch_hot_path_benchmarks(...)`
  - env gate: `OMNI_DISPATCH_BENCH`
  - summary line: `OMNI_BENCH_SUMMARY suite=dispatch_hot_path ...`

Workload shape:
- `DISPATCH_HOT_PATH_BENCH_ITERS=32768`
- dispatch loop expression repeatedly calls `(dispatch-bench-call 7)` with
  typed method table (`^Int`, `^Float64`, fallback).
- typed-lambda loop expression repeatedly calls
  `(typed-lambda-bench-call 7)` where call boundary validates `(^Int y)`.

## Baseline Run

Command:

```bash
LD_LIBRARY_PATH=/usr/local/lib \
OMNI_LISP_TEST_SLICE=advanced \
OMNI_DISPATCH_BENCH=1 \
OMNI_SKIP_TLS_INTEGRATION=1 \
./build/main
```

Log artifact:
- `build/dispatch_hot_path_bench_2026-03-11_nosummary.log`

Captured benchmark line:

```text
OMNI_BENCH_SUMMARY suite=dispatch_hot_path iters=32768 dispatch_ms=285 dispatch_ok=1 typed_lambda_ms=348 typed_lambda_ok=1 dispatch_sum=262144 typed_lambda_sum=262144
```

## Notes

- This benchmark is env-gated and inert in default test runs.
- The baseline run exits non-zero due pre-existing unrelated advanced-suite
  failures, but benchmark emission and correctness flags are valid:
  - `dispatch_ok=1`
  - `typed_lambda_ok=1`
