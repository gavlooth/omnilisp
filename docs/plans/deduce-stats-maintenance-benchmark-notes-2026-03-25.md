# Deduce Stats Maintenance Benchmark Notes (2026-03-25)

Purpose: close the remaining S1 benchmark-harness item in
`docs/plans/deduce-analytics-extension-milestone-2026-03-24.md` with one
bounded smoke-oriented maintenance benchmark lane for `deduce/stats` and
`deduce/refresh!`.

## Benchmark Surface

Implemented in:
- `src/lisp/tests_deduce_query_bench_groups.c3`
  - `deduce_query_bench_seed_stats_maintenance_db(...)`
  - `deduce_query_bench_run_stats_maintenance_tracked_lane(...)`
  - `deduce_query_bench_run_stats_maintenance_rebuild_lane(...)`
- `src/lisp/tests_deduce_query_bench_groups_more.c3`
  - `run_deduce_stats_maintenance_benchmarks(...)`
  - gate: `OMNI_DEDUCE_BENCH=1`
  - summary line:
    `OMNI_BENCH_SUMMARY suite=deduce_stats_maintenance ...`

## Workload Shape

- seed rows: `1024`
- tracked lane iterations: `32`
- rebuild lane iterations: `8`

Seed fixture:
- `bench_stats_edge`
- `bench_stats_rebuild`
- `bench_stats_view`

Rule baseline:
- `bench_stats_view <- bench_stats_edge`

The fixture always performs an initial `deduce/materialize!` plus one targeted
`deduce/refresh!` before timing begins, so the benchmark exercises maintenance
against an already materialized and initially clean derived relation.

## Lane Semantics

Tracked lane:
- mutation pattern: insert-only into `bench_stats_edge`
- maintenance path:
  - relation-scoped `deduce/refresh! bench_stats_view`
  - post-refresh `deduce/stats bench_stats_view`
- expected contract:
  - `refresh-scope = 'relation`
  - `refresh-execution-path = 'targeted`
  - `refresh-fallback-reason = nil`
  - `materialized-refresh-count` increments once per iteration
  - post-refresh stats stay out of `full-recompute-required`

Rebuild lane:
- per iteration, seed a fresh DB fixture
- mutation pattern:
  - insert one row into `bench_stats_rebuild`
  - add a second rule head `bench_stats_view <- bench_stats_rebuild`
- maintenance path:
  - pre-refresh `deduce/stats bench_stats_view`
  - relation-scoped `deduce/refresh! bench_stats_view`
  - post-refresh `deduce/stats bench_stats_view`
- expected contract:
  - pre-refresh stats expose:
    - `materialized-last-stale-reason = 'rule-set-change`
    - `full-recompute-required = true`
  - refresh falls back through:
    - `refresh-scope = 'relation`
    - `refresh-execution-path = 'db-wide`
    - `refresh-fallback-reason = 'full-recompute-required`
  - post-refresh stats return to:
    - `materialized-last-stale-reason = nil`
    - `full-recompute-required = nil`

## Summary Fields

The current summary line emits:

- fixture shape:
  - `seed_rows`
  - `tracked_iters`
  - `rebuild_iters`
- tracked lane:
  - `tracked_seeded_ok`
  - `tracked_mutation_ms`
  - `tracked_refresh_ms`
  - `tracked_stats_ms`
  - `tracked_refresh_hits`
  - `tracked_refresh_miss`
  - `tracked_stats_hits`
  - `tracked_stats_miss`
- rebuild lane:
  - `rebuild_seeded_ok`
  - `rebuild_rule_change_ms`
  - `rebuild_refresh_ms`
  - `rebuild_stats_ms`
  - `rebuild_pre_stats_hits`
  - `rebuild_pre_stats_miss`
  - `rebuild_fallback_hits`
  - `rebuild_fallback_miss`
  - `rebuild_post_stats_hits`
  - `rebuild_post_stats_miss`
- harness stability:
  - `iter_scope_fail`

This lane is intentionally a smoke-oriented benchmark, not a hard performance
budget gate. The key contract is stable maintenance-path classification and
repeatable summary visibility under non-trivial seeded tuple counts.

## Validation

Preferred bounded validation command:

```bash
scripts/run_validation_container.sh \
  env OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_BENCH=1 \
  ./build/main --test-suite lisp
```

Host-side fallback used only for semantic smoke when Docker is unavailable:

```bash
LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "<narrow stats-maintenance smoke>"
```

The lane remains inert during normal test runs unless `OMNI_DEDUCE_BENCH=1` is
set.
