# Deduce Scan-Range Key Cache Optimization (2026-03-11)

Purpose: close O5.3 in `TODO.md` by reducing avoidable scan-path allocations in
`relation_scan_range(...)` while preserving `deduce` scan/query semantics.

## Change Summary

Implemented:
- `src/lisp/deduce.c3`
  - extended `Relation` with cached column key values:
    - `Value** column_key_values`
  - updated relation free/finalizer paths to release the cache pointer array.
- `src/lisp/deduce_schema_query.c3`
  - initialize `rel.column_key_values = null` at relation allocation.
- `src/lisp/deduce_relation_scan_helpers.c3`
  - added `deduce_relation_ensure_column_key_values(...)` to allocate per-column
    symbol key values once and reuse them.
  - `relation_scan_range(...)` now uses cached keys instead of allocating a new
    symbol `Value` per row/column pair.
  - dictionary initial capacity now scales with relation column count to reduce
    resize pressure for wider relations.

Semantics:
- No API changes.
- Scan/query/count outputs are unchanged.
- Allocation reduction is internal to row materialization.

## Benchmark Evidence

Pre-optimization baseline (from O5.2 instrumentation run):

```text
OMNI_BENCH_SUMMARY suite=deduce_scan_query_count ... scan_range_ms=2865 ... scan_range_key_symbols=131072 ... scan_range_materialize_ms=2853 query_ms=2137 ...
```

Post-optimization run:

```bash
LD_LIBRARY_PATH=/usr/local/lib \
OMNI_TEST_QUIET=1 \
OMNI_SKIP_TLS_INTEGRATION=1 \
OMNI_LISP_TEST_SLICE=deduce \
OMNI_DEDUCE_BENCH=1 \
./build/main > build/deduce_scan_query_count_key_cache_2026-03-11.log 2>&1
```

Captured summary:

```text
OMNI_BENCH_SUMMARY suite=deduce_scan_query_count iters_scan=16 iters_scan_range=16 iters_query=16 iters_count=4096 corpus_rows=4096 seeded_ok=1 filter_ok=1 scan_ms=318 scan_ok=16 scan_range_ms=1105 scan_range_ok=16 scan_range_calls=16 scan_range_rows=65536 scan_range_key_symbols=0 scan_range_materialize_ms=1092 query_ms=994 query_ok=16 query_expected=2005 count_ms=89 count_ok=4096 iter_scope_fail=0
```

Observed impact:
- `scan_range_key_symbols`: `131072 -> 0` during measured loop.
- `scan_range_ms`: `2865 -> 1105` (~61% lower in this fixture).
- `query_ms`: `2137 -> 994` (~53% lower), consistent with query depending on
  scan materialization.

## Validation

- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_BENCH=1 ./build/main`
