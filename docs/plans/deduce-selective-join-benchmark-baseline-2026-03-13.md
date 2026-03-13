# Deduce Selective Join Benchmark Baseline (2026-03-13)

Purpose: close O7.9 selective-join benchmark TODO by adding an env-gated
selective-join benchmark lane with explicit explain-counter assertions and
capturing baseline output.

## Benchmark Surface

Implemented in:
- `src/lisp/tests_deduce_query_groups.c3`
  - `run_deduce_selective_join_benchmarks(...)`
  - gate: `OMNI_DEDUCE_BENCH=1`
  - summary line: `OMNI_BENCH_SUMMARY suite=deduce_selective_join ...`

Fixture:
- relations:
  - `bench_selective_src(a, b)`
  - `bench_selective_filter(a, b)`
  - `bench_selective_out(a, b)`
- rule:
  - `(bench_selective_out ?x ?y) <- (bench_selective_src ?x ?y) (bench_selective_filter 0 ?y)`
- source rows: `4096`
- filter rows: `32`
- expected derived rows: `256`

Benchmark lanes:
- explain lane:
  - iterations: `256`
  - validates explain payload contains two steps with:
    - `operator = 'index-scan` on both steps,
    - `counters.counter-kind = 'estimated`,
    - monotone selectivity signal (`step0_rows_read <= step1_rows_read`).
- analyze lane:
  - iterations: `64`
  - validates each run returns `status = 'ok` and
    `(deduce/count bench_selective_out) == expected_rows`.

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
OMNI_BENCH_SUMMARY suite=deduce_selective_join src_rows=4096 filter_rows=32 explain_iters=256 analyze_iters=64 seeded_ok=1 expected_rows=256 explain_ms=1537 explain_attempted=256 explain_ok=256 explain_mode_miss=0 step0_rows_read=32 step1_rows_read=4096 analyze_ms=1102 analyze_ok=64 iter_scope_fail=0
```

## Notes

- Explain assertions were stable in this baseline (`explain_mode_miss=0`).
- Selectivity signal remained explicit:
  - `step0_rows_read=32`
  - `step1_rows_read=4096`
- Benchmark remains inactive unless `OMNI_DEDUCE_BENCH=1` is set.
