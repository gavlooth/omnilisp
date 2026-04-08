# Deduce Scan/Query/Count Benchmark Baseline (2026-03-11)

Purpose: close O5.1 in `TODO.md` by adding explicit benchmark coverage for
`deduce` scan/query/count throughput and recording a baseline regression
envelope.

## Benchmark Surface

Implemented in the exact benchmark files:
- `src/lisp/tests_deduce_query_bench_groups.c3`
  - retained seed/assert helpers and query benchmark support used by the lane
- `src/lisp/tests_deduce_query_bench_groups_more.c3`
  - benchmark runner/reporting entrypoint: `run_deduce_scan_query_count_benchmarks(...)`
  - owns the timed `scan`, `scan-range`, `query`, and `count` lane execution
  - gate: `OMNI_DEDUCE_BENCH=1`
  - summary line: `OMNI_BENCH_SUMMARY suite=deduce_scan_query_count ...`

Benchmark-lane split:
- retained seed/assert helpers and query benchmark support in
  `src/lisp/tests_deduce_query_bench_groups.c3`
- extracted benchmark runner/reporting entrypoint into
  `src/lisp/tests_deduce_query_bench_groups_more.c3`
- `src/lisp/tests_deduce_query_groups.c3` only wires the env-gated suite call;
  it is not the benchmark lane implementation file

Workload:
- corpus rows: `4096`
- scan iterations: `16`
- scan-range iterations: `16`
- query iterations: `16`
- count iterations: `4096`
- query predicate: `(lambda (row) (> ('score row) 50))`

Benchmark notes:
- Corpus relation schema is `(id score)`.
- `scan` and `query` lanes run under per-iteration child scopes to avoid
  benchmark-only result retention inflation.
- `scan-range` runs over the full seeded key range and snapshots the dedicated
  materialization counters from `deduce_relation_scan_helpers.c3`.
- `query_expected` is computed during seeding and verified each iteration.

## Baseline Run

Command:

```bash
LD_LIBRARY_PATH=/usr/local/lib \
OMNI_TEST_QUIET=1 \
OMNI_SKIP_TLS_INTEGRATION=1 \
OMNI_LISP_TEST_SLICE=deduce \
OMNI_DEDUCE_BENCH=1 \
./build/main > build/deduce_scan_query_count_bench_2026-03-11.log 2>&1
```

Log artifact:
- `build/deduce_scan_query_count_bench_2026-03-11.log`

Captured summary:

```text
OMNI_BENCH_SUMMARY suite=deduce_scan_query_count iters_scan=16 iters_scan_range=16 iters_query=16 iters_count=4096 corpus_rows=4096 seeded_ok=1 filter_ok=1 scan_ms=655 scan_ok=16 scan_range_ms=2865 scan_range_ok=16 scan_range_calls=16 scan_range_rows=65536 scan_range_key_symbols=131072 scan_range_materialize_ms=2853 query_ms=2137 query_ok=16 query_expected=2005 count_ms=88 count_ok=4096 iter_scope_fail=0
```

## Notes

- Seeding and filter setup both succeeded (`seeded_ok=1`, `filter_ok=1`).
- All scan/query/count iterations passed expected cardinality checks.
- The current lane also emits full-range `scan-range` materialization counters
  so the Docker perf envelope can track row-dict and key-symbol churn.
- Query lane is currently the dominant cost in this benchmark surface.
