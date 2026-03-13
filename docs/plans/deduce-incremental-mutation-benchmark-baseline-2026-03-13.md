# Deduce Incremental Mutation Benchmark Baseline (2026-03-13)

Purpose: close O7.8 item 3 in `TODO.md` by adding and baseline-capturing a
mutation-heavy benchmark lane that compares tracked incremental invalidation
against conservative full-recompute invalidation.

## Benchmark Surface

Implemented in:
- `src/lisp/tests_deduce_query_groups.c3`
  - `run_deduce_incremental_mutation_benchmarks(...)`
  - gate: `OMNI_DEDUCE_BENCH=1`
  - summary line: `OMNI_BENCH_SUMMARY suite=deduce_incremental_mutation ...`

Workload shape:
- seed rows: `1024` (`bench_edge` relation)
- tracked lane iterations: `128`
- full lane iterations: `128`

Lane semantics:
- tracked lane:
  - mutation pattern: insert-only (`fact!`)
  - expected analyze mode: `tracked`
- full lane:
  - mutation pattern: retract + reinsert (`retract!` then `fact!`)
  - expected analyze mode: `full-recompute`

Both lanes:
- use the same derived-rule shape (`bench_twohop <- bench_edge, bench_edge`),
- run `deduce/analyze` after each mutation step,
- assert mode counters through emitted benchmark summary fields.

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
OMNI_BENCH_SUMMARY suite=deduce_incremental_mutation seed_rows=1024 tracked_iters=128 full_iters=128 tracked_seeded_ok=1 full_seeded_ok=1 tracked_mutation_ms=0 tracked_analyze_ms=16076 tracked_analyze_attempted=128 tracked_analyze_ok=128 tracked_mode_hits=128 tracked_mode_miss=0 full_mutation_ms=2 full_analyze_ms=14260 full_analyze_attempted=128 full_analyze_ok=128 full_mode_hits=128 full_mode_miss=0 iter_scope_fail=0
```

## Notes

- Both lanes seeded successfully (`tracked_seeded_ok=1`, `full_seeded_ok=1`).
- Mode classification was stable in this baseline run:
  - tracked lane: `tracked_mode_miss=0`
  - full lane: `full_mode_miss=0`
- The lane is intentionally benchmark-only and remains inert in default test
  runs unless `OMNI_DEDUCE_BENCH=1` is set.
