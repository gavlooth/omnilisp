# Deduce Analytics Extension Milestone (2026-03-24)

Status: `closed`

## Purpose

Create a focused follow-up milestone for Deduce analytics and maintenance verbs
that does not block current runtime-query/aggregate production work.

This milestone is intentionally small and concrete:

- keep analytics behavior explicit and deterministic
- keep maintenance verbs explicit and safe under empty-relation / malformed-schema inputs

## Track S1: Statistics Extension

### Goals

- make `deduce/stats` and related metadata handling deterministic for lifecycle
  operations
- define one explicit API shape for statistics maintenance and refresh behavior

### Task List

- [x] Publish the analytics payload baseline for maintenance mode in a single spec
  section (relation-local + DB-level fields, naming, and fallback behavior).
  targets:
  - `docs/deduce-datalog-spec.md`
  - `docs/reference/08-libraries.md`
  acceptance:
  - one shared source-of-truth list of planned analytics fields and failure
    payload keys
  completed:
  - canonical maintenance analytics field/rejection baseline published in
    `docs/deduce-datalog-spec.md#73-maintenance-analytics-payload-baseline`
  - `docs/reference/08-libraries.md` now points to that section instead of
    duplicating field inventories

- [x] Add explicit coverage for stats refresh / recompute edges in the public
  surface once API names are finalized.
  targets:
  - `src/lisp/deduce_schema_query.c3`
  - `src/lisp/tests_deduce_query_groups.c3`
  - `src/lisp/tests_deduce_durability_groups.c3`
  acceptance:
  - deterministic behavior is documented for stats operations that currently
    report empty relations, missing relations, or malformed schema
  completed:
  - added public-surface regressions for:
    - zero-row materialized refresh
    - dropped-handle stats versus refresh behavior
    - DB-wide refresh on persisted but still-unready materialized declarations
  - tightened the maintenance baseline docs to record the shipped dropped-handle
    split:
    - `deduce/stats` stays readable and reports `dropped = true`
    - `deduce/refresh!` rejects with `deduce/refresh-relation-dropped`

- [x] Add benchmark harness notes for stats-heavy relation churn and repeated
  recompute/rebuild paths at representative corpus sizes.
  targets:
  - `src/lisp/tests_deduce_query_bench_groups.c3`
  - `docs/` benchmark notes for the dedicated lane
  acceptance:
  - at least one bounded benchmark smoke path for stats maintenance under
    non-trivial tuple counts
  completed:
  - added `run_deduce_stats_maintenance_benchmarks(...)` under the existing
    `OMNI_DEDUCE_BENCH=1` Deduce benchmark gate
  - the lane now covers:
    - tracked relation-scoped refresh churn after source-fact inserts
    - forced DB-wide fallback refresh after rule-set-change invalidation
  - benchmark shape and summary fields are documented in
    `docs/plans/deduce-stats-maintenance-benchmark-notes-2026-03-25.md`

## Track S2: Data-Cleanup Verbs Extension

### Goals

- separate write-side cleanup/admin verbs from general mutation paths
- define deterministic cleanup contracts before any new verb ships

### Task List

- [x] Define a minimal cleanup verb matrix for relation maintenance and relation
  lifecycle cleanup operations.
  targets:
  - `docs/deduce-datalog-spec.md`
  - `docs/reference/08-libraries.md`
  - `src/lisp/eval_init_primitives.c3`
  - `src/lisp/deduce_relation_ops_mutations.c3`
  acceptance:
  - one canonical naming decision per verb and one explicit schema/error table
  completed:
  - published the canonical cleanup verb matrix in
    `docs/deduce-datalog-spec.md#74-cleanup-verb-matrix`
  - documented:
    - the only approved cleanup verb names
    - the current accepted call shapes
    - deterministic no-op cases
    - the current rejection-code table
  - corrected the stale implementation anchor from
    `src/lisp/deduce_schema_query.c3` to the current cleanup implementation in
    `src/lisp/deduce_relation_ops_mutations.c3`
  - tightened code-side registration/dispatch comments so the canonical
    cleanup surface is visible where the verbs are exported

- [x] Audit current cleanup paths (`deduce/retract!`, `deduce/clear!`,
  `deduce/drop!`) for deterministic empty-relation and malformed-schema
  behavior.
  targets:
  - `src/lisp/tests_deduce_durability_groups.c3`
  acceptance:
  - error payloads and success/no-op behavior are deterministic and documented
  completed:
  - extended `src/lisp/tests_deduce_durability_groups.c3` to pin:
    - `retract!` on a missing tuple as a `Void` no-op
    - repeated `drop!` on an already dropped handle as a `Void` no-op
    - `retract!` / `clear!` on a dropped relation handle as deterministic
      code-first rejections
  - the cleanup no-op and rejection matrix is now documented in
    `docs/deduce-datalog-spec.md#74-cleanup-verb-matrix`

- [x] Add dedicated admin-task tests for cleanup + schema mismatch under
  write-intent scenarios.
  targets:
  - `src/lisp/tests_deduce_durability_groups.c3`
  - `src/lisp/tests_deduce_query_groups.c3`
  acceptance:
  - deterministic failure and no-op behavior remains stable through regression
    surfaces
  completed:
  - durability-side cleanup regressions now pin:
    - no-op `retract!` on missing tuples
    - no-op repeated `drop!`
    - deterministic dropped-handle `retract!` / `clear!` rejections
  - admin-surface regressions in `src/lisp/tests_deduce_query_groups.c3` now
    pin code-first write-intent failures for:
    - `retract!` arity mismatch
    - `retract!` transaction/DB mismatch
    - `clear!` and `drop!` called with the wrong Deduce handle kind

## Notes

- This milestone is the explicit follow-up to:
  - `TODO.md`
    (analytics extension item)
- `TODO.md` follow-up items around cleanup/statistics robustness are closed.
- Validation command for this lane should remain the bounded deduce slice until the
  first implementation slice changes semantics.
