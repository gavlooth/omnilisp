# Deduce Scan-Range Materialization Cost Baseline (2026-03-11)

Purpose: close O5.2 in `TODO.md` by directly measuring row materialization work
inside `relation_scan_range(...)`, including per-row hashmap construction and
per-column symbol-key allocation.

## Measurement Surface

Implementation changes in the exact benchmark files:
- `src/lisp/deduce_relation_scan_helpers.c3`
  - added bench stats state:
    - `calls`
    - `rows_materialized`
    - `key_symbols_allocated`
    - `materialize_ms`
  - added control/snapshot helpers:
    - `deduce_scan_range_bench_set_enabled(...)`
    - `deduce_scan_range_bench_reset()`
    - `deduce_scan_range_bench_snapshot()`
  - instrumented `relation_scan_range(...)` materialization block.
- `src/lisp/tests_deduce_query_bench_groups.c3`
  - retained shared query-benchmark helper substrate used by the benchmark lane
- `src/lisp/tests_deduce_query_bench_groups_more.c3`
  - benchmark runner/reporting entrypoint now includes bounded `scan-range`
    iterations and emits the scan-range materialization counters

Benchmark-lane split:
- retained seed/assert helpers and query benchmark support in
  `src/lisp/tests_deduce_query_bench_groups.c3`
- extracted benchmark runner/reporting entrypoints, including the timed
  `scan-range` loop and summary emission, into
  `src/lisp/tests_deduce_query_bench_groups_more.c3`
- `src/lisp/tests_deduce_query_groups.c3` remains only the env-gated suite
  dispatcher

Runtime behavior:
- Instrumentation is disabled by default.
- Benchmark lane enables instrumentation only for the targeted run and disables
  it after snapshotting.
- No semantic change to normal `deduce` scan-range results.

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

- `scan_range_rows=65536` equals `4096 rows * 16 iterations`.
- `scan_range_key_symbols=131072` equals `scan_range_rows * 2 columns`.
- `scan_range_materialize_ms` is near total `scan_range_ms`, showing row
  materialization dominates scan-range wall time in this benchmark setup.
