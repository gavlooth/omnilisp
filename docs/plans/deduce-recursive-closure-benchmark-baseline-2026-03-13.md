# Deduce Recursive Closure Benchmark Baseline (2026-03-13)

Purpose: close O7.9 recursive benchmark TODO by adding a transitive-closure
benchmark lane that compares explicit `deduce/analyze` engines and records
speedup-tracking metrics.

## Benchmark Surface

Implemented in:
- `src/lisp/tests_deduce_query_groups.c3`
  - `run_deduce_recursive_closure_benchmarks(...)`
  - gate: `OMNI_DEDUCE_BENCH=1`
  - summary line: `OMNI_BENCH_SUMMARY suite=deduce_recursive_closure ...`

Fixture:
- chain edges: `128`
- closure relation expected rows: `8256`
- rules:
  - base: `(ancestor ?x ?y) <- (parent ?x ?y)`
  - recursive: `(ancestor ?x ?z) <- (parent ?x ?y) (ancestor ?y ?z)`

Lanes:
- naive lane:
  - analyze selector: `'naive`
  - expected execution engine: `'naive-scc`
  - iterations: `16`
- semi-naive lane:
  - analyze selector: `'semi-naive`
  - expected execution engine: `'semi-naive-scc`
  - iterations: `16`

Per-iteration assertions:
- analyze status is `ok`,
- derived closure cardinality matches expected rows,
- reported execution engine matches requested lane.

## Baseline Run

Command:

```bash
LD_LIBRARY_PATH=/usr/local/lib \
OMNI_TEST_QUIET=1 \
OMNI_SKIP_TLS_INTEGRATION=1 \
OMNI_LISP_TEST_SLICE=deduce \
OMNI_DEDUCE_BENCH=1 \
./build/main --test-suite lisp > build/deduce_incremental_mutation_bench_2026-03-13.log 2>&1
```

Log artifact:
- `build/deduce_incremental_mutation_bench_2026-03-13.log`

Captured summary:

```text
OMNI_BENCH_SUMMARY suite=deduce_recursive_closure chain_edges=128 expected_rows=8256 naive_iters=16 semi_iters=16 naive_seeded_ok=1 semi_seeded_ok=1 naive_ms=14880 naive_attempted=16 naive_ok=16 naive_engine_hit=16 naive_mode_miss=0 semi_ms=25636 semi_attempted=16 semi_ok=16 semi_engine_hit=16 semi_mode_miss=0 speedup_x1000=580 iter_scope_fail=0
```

## Notes

- Both lanes were fully valid in baseline (`*_ok=16`, `*_mode_miss=0`).
- `speedup_x1000` is reported as `naive_ms * 1000 / semi_ms`; values below
  `1000` indicate semi-naive was slower for this fixture/run profile.
- This lane is for regression tracking, not a one-time proof of dominance.
