# Deduce Query Optimization Evaluation (2026-03-11)

Purpose: close O5.4 in `TODO.md` by evaluating whether `deduce-query` should
remain full-scan + callback filtering or introduce a narrower optimization path
for common predicates.

## Scope Evaluated

Current query path:
- `prim_deduce_query(...)` in `src/lisp/deduce_schema_query.c3`
- row materialization from `relation_scan_all(...)` /
  `relation_scan_range(...)` in `src/lisp/deduce_relation_scan_helpers.c3`

Benchmark evidence source files:
- `src/lisp/tests_deduce_query_bench_groups.c3`
  - retained seed/assert helpers and query benchmark support for the
    `deduce_scan_query_count` lane
- `src/lisp/tests_deduce_query_bench_groups_more.c3`
  - benchmark runner/reporting entrypoint:
    `run_deduce_scan_query_count_benchmarks(...)`
  - owns the timed `scan`, `scan-range`, `query`, and `count` loops that emit
    `OMNI_BENCH_SUMMARY suite=deduce_scan_query_count ...`
- `src/lisp/tests_deduce_query_groups.c3`
  - env-gated suite wiring only; not the benchmark lane implementation file

Changes included in this evaluation pass:
- scan materialization key-cache optimization (O5.3)
- query filtering fused into a single pass (removed temporary match vector and
  second reconstruction pass)

## Evidence

Pre key-cache baseline (from O5.2 run):

```text
OMNI_BENCH_SUMMARY suite=deduce_scan_query_count ... scan_range_ms=2865 ... query_ms=2137 ...
```

Post key-cache baseline:

```text
OMNI_BENCH_SUMMARY suite=deduce_scan_query_count ... scan_range_ms=1105 ... query_ms=994 ...
```

Post fused query-filter pass:

```bash
LD_LIBRARY_PATH=/usr/local/lib \
OMNI_TEST_QUIET=1 \
OMNI_SKIP_TLS_INTEGRATION=1 \
OMNI_LISP_TEST_SLICE=deduce \
OMNI_DEDUCE_BENCH=1 \
./build/main > build/deduce_scan_query_count_query_eval_2026-03-11.log 2>&1
```

Captured summary:

```text
OMNI_BENCH_SUMMARY suite=deduce_scan_query_count iters_scan=16 iters_scan_range=16 iters_query=16 iters_count=4096 corpus_rows=4096 seeded_ok=1 filter_ok=1 scan_ms=316 scan_ok=16 scan_range_ms=1123 scan_range_ok=16 scan_range_calls=16 scan_range_rows=65536 scan_range_key_symbols=0 scan_range_materialize_ms=1114 query_ms=1021 query_ok=16 query_expected=2005 count_ms=92 count_ok=4096 iter_scope_fail=0
```

## Decision

Decision for now:
- Keep `deduce-query` on the full-scan + callback model.
- Do not add an AST-shape-specific predicate planner path in this pass.

Rationale:
- The dominant improvement came from reducing scan/materialization allocations
  (O5.3), which already lowered query cost materially.
- Additional narrowing inside callback application showed no stable benchmark
  advantage in this fixture relative to run-to-run variance.
- Predicate-shape specialization would add complexity and fragility to closure
  AST interpretation; current evidence does not justify that complexity.

Follow-up trigger:
- Revisit specialized predicate planning only if future workloads show callback
  dispatch dominating after scan/materialization costs are already controlled.

## Validation

- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_BENCH=1 ./build/main`
