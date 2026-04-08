# Deduce Skewed-Cardinality Benchmark Baseline (2026-03-13)

Purpose: close O7.9 skewed-cardinality benchmark TODO by adding a benchmark
lane that stresses planner/explain behavior under highly non-uniform join-key
distributions.

## Benchmark Surface

Implemented in:
- `src/lisp/tests_deduce_query_groups.c3`
  - `run_deduce_skewed_cardinality_benchmarks(...)`
  - gate: `OMNI_DEDUCE_BENCH=1`
  - summary line: `OMNI_BENCH_SUMMARY suite=deduce_skewed_cardinality ...`

Fixture:
- `bench_skew_left(key, value)`:
  - total rows: `4096`
  - heavy-key rows (`key=0`): `3584`
  - sparse-key tail rows: `512`
- `bench_skew_right(key, value)`:
  - sparse-key rows: `64` (subset of the sparse tail key range)
- rule:
  - `(bench_skew_out ?k ?v) <- (bench_skew_left ?k ?v) (bench_skew_right ?k ?w)`
- expected derived rows: `64`

Benchmark lanes:
- explain lane (`128` iterations):
  - validates explain payload counter shape stability (`rows-read`,
    `rows-emitted`, `index-hits`, `index-misses`, `counter-kind='estimated`),
  - records selectivity-order signal hits:
    - `explain_selectivity_hits` counts iterations where
      `step0_rows_read <= step1_rows_read`.
- analyze lane (`64` iterations):
  - validates `status='ok` and output cardinality remains `expected_rows`.

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
OMNI_BENCH_SUMMARY suite=deduce_skewed_cardinality left_rows=4096 heavy_rows=3584 rare_keys=64 explain_iters=128 analyze_iters=64 seeded_ok=1 expected_rows=64 explain_ms=822 explain_attempted=128 explain_ok=128 explain_mode_miss=0 explain_selectivity_hits=0 step0_rows_read=4096 step1_rows_read=64 analyze_ms=2074 analyze_ok=64 iter_scope_fail=0
```

## Notes

- Explain counter-shape assertions were stable (`explain_ok=128`,
  `explain_mode_miss=0`).
- Selectivity-order signal did not hold in this baseline
  (`explain_selectivity_hits=0`), which is now tracked as an explicit planner
  robustness metric for future tuning.
