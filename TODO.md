# Active TODO

Last condensed: 2026-03-20

This file now tracks the live Deduce execution backlog.

Previous `TODO.md` state was backed up at:
- `docs/plans/TODO_BACKUP_2026-03-20.md`

Historical completed TODO archive remains at:
- `docs/plans/TODO_ARCHIVE_2026-03-11.md`

Primary planning sources:
- `docs/plans/deduce-full-roadmap-2026-03-20.md`
- `docs/plans/deduce-sophistication-plan-2026-03-20.md`
- `docs/plans/deduce-actionable-backlog-2026-03-20.md`

Current actionable count: 0
Live blocker queue:
- No live recursive aggregate execution blocker remains. Recursive grouped
  aggregate support now ships for `count` / `sum` / `min` / `max` in both
  non-recursive rules and aggregate-bearing recursive SCCs on the
  `semi-naive-scc` production lane for the current stratified rule surface.

## Deduce Actionable Backlog

### P0. Truth and Correctness

- [x] Create one authoritative Deduce capability matrix.
  targets:
  - `docs/deduce-datalog-spec.md`
  - `docs/plans/deduce-robust-datalog-roadmap.md`
  - `docs/plans/deduce-full-roadmap-2026-03-20.md`
  completed:
  - capability matrix published in `docs/deduce-datalog-spec.md#13-current-implementation-matrix`

- [x] Inventory the canonical Deduce implementation surface.
  targets:
  - `src/lisp/deduce_rule_eval*.c3`
  - `src/lisp/deduce_rule_ops*.c3`
  - `src/lisp/deduce_relation_scan_helpers*.c3`
  - `src/lisp/deduce_db_handles*.c3`
  completed:
  - canonical implementation surface inventory published in
    `docs/deduce-datalog-spec.md#14-canonical-implementation-surface-inventory`

- [x] Centralize rule validation behind one authority path.
  targets:
  - `src/lisp/deduce_rule_eval.c3`
  - `src/lisp/deduce_rule_eval_prims.c3`
  completed:
  - `deduce/rule!` now routes through `deduce_rule_validate_install_candidate` in `src/lisp/deduce_rule_eval.c3`

- [x] Add deterministic invalid-rule rejection coverage.
  targets:
  - `src/lisp/tests_deduce_rule_groups*.c3`
  completed:
  - deterministic rejection coverage now exercises arity mismatch,
    relation-not-found, unsafe head variables, unsafe negated variables, and
    negative-cycle rejection

### P1. Planner and Explain Truthfulness

- [x] Audit planner metadata versus runtime execution.
  targets:
  - `src/lisp/deduce_rule_eval_exec*.c3`
  - `src/lisp/deduce_relation_scan_helpers*.c3`
  - `src/lisp/deduce_rule_ops_explain.c3`
  completed:
  - explain payload now marks `surface-kind = 'planner-snapshot` and keeps step
    counters explicitly labeled as `estimated`

- [x] Make `deduce/explain` execution-truthful for conjunctive queries.
  targets:
  - `src/lisp/deduce_rule_ops_explain.c3`
  - `src/lisp/deduce_rule_eval_exec*.c3`
  completed:
  - explain payload no longer implies observed runtime counters where the
    current plumbing only has planner-estimate data

- [x] Add selective versus non-selective planner regressions.
  - explain-side regressions now cover selective lookup ordering, broad scan
    shape, and skew-sensitive join ordering without claiming observed runtime
    counters
  targets:
  - `src/lisp/tests_deduce_query_groups.c3`
  - `src/lisp/tests_deduce_query_bench_groups*.c3`

### P2. Recursive and Negation Closeout

- [x] Lock one canonical production recursive engine.
  targets:
  - `src/lisp/deduce_rule_eval_fixpoint.c3`
  - `src/lisp/deduce_rule_eval_exec_naive.c3`
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
  - `src/lisp/deduce_rule_eval_scc.c3`
  completed:
  - `deduce/analyze` now defaults to `semi-naive-scc`, while `naive-scc`
    remains the reference parity mode

- [x] Add recursive parity and convergence regressions.
  targets:
  - `src/lisp/tests_deduce_rule_groups*.c3`
  completed:
  - recursive closure parity now covers both `naive-scc` reference and
    `semi-naive-scc` production paths, and recursive convergence/iteration-limit
    regressions assert the documented hard-stop guard in
    `src/lisp/tests_deduce_rule_groups_more_tail.c3`

- [x] Share one dependency-graph model between validation and runtime.
  targets:
  - `src/lisp/deduce_rule_eval*.c3`
  completed:
  - validation and SCC scheduling now build from the same dependency-graph
    helper, reducing drift between rule rejection and runtime ordering

- [x] Add stratified-negation regression matrix.
  targets:
  - `src/lisp/tests_deduce_rule_groups*.c3`
  completed:
  - legal stratified negation and illegal negative-cycle rejection are both
    covered by deterministic Deduce rule tests, alongside existing stratum
    boundary analysis coverage

### P3. Join Engine and Runtime Counters

- [x] Make index nested-loop join the default for selectivity-supported shapes.
  targets:
  - `src/lisp/deduce_relation_scan_helpers_join.c3`
  - `src/lisp/deduce_rule_eval_exec*.c3`
  completed:
  - default adaptive routing now prefers index-first for selective or
    small-outer shapes, while the zero-threshold path hashes low-selectivity
    partially bound shapes directly instead of taking a fallback detour
  - fully bound unique supporting indexes still route index-first

- [x] Harden hash-join and adaptive routing thresholds.
  targets:
  - `src/lisp/deduce_relation_scan_helpers_join.c3`
  - `src/lisp/deduce_rule_eval_exec*.c3`
  completed:
  - explicit probe / miss thresholds still force the historical index-first
    adaptive path, while the default route now uses explicit schema signals
    instead of an opaque fallback-only heuristic
  - added hash-first default regression coverage plus a selective-join benchmark
    smoke
  - bounded Deduce slice now passes the join default regression after tightening
    the selected-index fully-bound check

- [x] Surface observed operator counters in `deduce/explain` and `deduce/analyze`.
  targets:
  - `src/lisp/deduce_rule_ops_explain.c3`
  - `src/lisp/deduce_rule_eval_exec*.c3`
  completed:
  - `deduce/explain` now preserves the existing per-step `counters` dictionary
    shape while sourcing values from runtime execution and reporting
    `counter-kind = 'observed`
  - per-step payload now includes `rows-read`, `rows-emitted`, `index-hits`,
    `index-misses`, and `join-probes`
  - `deduce/analyze` now mirrors the same observed step counters under
    `rule-execution`
  - bounded Deduce slice passes payload assertions for the observed explain and
    analyze surfaces

### P4. Incremental Maintenance

- [x] Publish the current incremental-state truth table.
  targets:
  - `docs/deduce-datalog-spec.md`
  - `docs/plans/deduce-incremental-delta-propagation-design-2026-03-13.md`
  completed:
  - incremental-state truth table published in
    `docs/deduce-datalog-spec.md#71-incremental-state-truth-table`

- [x] Make commit/abort mutation logs the sole source for invalidation triggers.
  targets:
  - `src/lisp/deduce_db_handles_mutation*.c3`
  - `src/lisp/deduce_rule_eval*.c3`
  completed:
  - commit-path mutation logs now drive invalidation, while abort discards the
    pending mutation log without dirtying derived-state machinery

- [x] Implement real dependency-aware invalidation for derived predicates.
  targets:
  - `src/lisp/deduce_rule_eval*.c3`
  - `src/lisp/deduce_db_handles_mutation*.c3`
  completed:
  - dependency-aware invalidation now walks rule dependents from the mutated
    predicate instead of forcing a blanket full recompute

### P5. V1.5 Aggregate Support

- [x] Lock the canonical aggregate syntax and projection semantics.
  targets:
  - `docs/deduce-datalog-spec.md`
  - `docs/plans/deduce-v15-aggregate-implementation-plan-2026-03-20.md`
  completed:
  - the Deduce spec now pins the deferred v1.5 aggregate query shape as a
    projection-driven surface where bare variables imply group-by keys and
    supported aggregate forms are `count`, `sum`, `min`, and `max`

- [x] Extend Deduce IR and installed signatures to carry aggregate metadata.
  targets:
  - `src/lisp/deduce_rule_ops.c3`
  - `src/lisp/deduce_db_handles.c3`
  - `src/lisp/deduce_db_handles_mutation.c3`
  - `src/lisp/deduce_rule_eval_prims.c3`
  completed:
  - aggregate-aware rule terms and installed signatures now persist projection
    kind, aggregate op, normalized source-var ids, and aggregate projection
    counts across install / copy / release paths

- [x] Add aggregate validation and explain/analyze payload scaffolding.
  targets:
  - `src/lisp/deduce_rule_eval.c3`
  - `src/lisp/deduce_rule_ops_planning.c3`
  - `src/lisp/deduce_rule_ops_explain.c3`
  - `src/lisp/tests_deduce_rule_groups*.c3`
  completed:
  - aggregate head validation now rejects malformed or ungrounded grouped
    projections, `deduce/explain` exposes aggregate projection count plus head
    projection metadata, and the pre-execution scaffolding landed ahead of the
    grouped runtime path

- [x] Implement grouped execution for `count`, `sum`, `min`, `max`.
  targets:
  - `src/lisp/deduce_rule_eval_exec.c3`
  - `src/lisp/deduce_rule_eval_exec_naive.c3`
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
  - `src/lisp/deduce_rule_eval_fixpoint.c3`
  - `src/lisp/tests_deduce_query_groups.c3`
  completed:
  - grouped aggregate execution now recomputes exact `count`, `sum`, `min`,
    and `max` outputs for non-recursive aggregate rules through the shared
    naive analyzer path
  - `deduce/analyze` and `deduce/explain` no longer special-case aggregate
    rules as deferred when grouped execution is supported
  - unsupported shapes are now rejected explicitly:
    - mixed aggregate/non-aggregate shared heads still reject
    - incompatible aggregate-only shared heads still reject

- [x] Add aggregate regressions and bounded benchmark smoke.
  targets:
  - `src/lisp/tests_deduce_query_groups.c3`
  - `src/lisp/tests_deduce_query_bench_groups*.c3`
  - `src/lisp/tests_deduce_rule_groups_explain.c3`
  completed:
  - deterministic grouped aggregate regressions now cover runtime
    `count`/`sum`/`min`/`max` outputs
  - explain/analyze regressions now assert that aggregate rules execute rather
    than hard-failing as deferred
  - benchmark-mode smoke now seeds and analyzes an aggregate workload in
    `src/lisp/tests_deduce_query_bench_groups_more.c3`

- [x] Support recursive aggregate rules via sound naive recompute fallback.
  targets:
  - `src/lisp/deduce_rule_eval.c3`
  - `src/lisp/deduce_rule_eval_exec.c3`
  - `src/lisp/deduce_rule_eval_exec_naive.c3`
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
  - `src/lisp/deduce_rule_eval_fixpoint.c3`
  - `src/lisp/tests_deduce_rule_groups*.c3`
  - `src/lisp/tests_deduce_query_groups.c3`
  completed:
  - recursive aggregate SCCs now fall back from the requested semi-naive
    engine to the sound naive grouped recompute path
  - aggregate-only shared heads with matching projection signatures now batch
    through one grouped finalize pass, while mixed or incompatible shapes
    still reject at install time
  - `deduce/analyze` now reports the requested execution engine, the actual
    execution engine, and whether recursive aggregate fallback was applied
  - deterministic regressions and bounded benchmark smoke now cover recursive
    aggregate convergence and explain/analyze truthfulness
  validation:
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - result: `97 passed, 0 failed`

- [x] B5.7 Build signed-delta substrate for aggregate-bearing recursive SCCs.
  targets:
  - `src/lisp/deduce_rule_eval_exec.c3`
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
  - `src/lisp/deduce_rule_eval_fixpoint.c3`
  - `src/lisp/deduce_rule_eval_prims.c3`
  - `src/lisp/tests_deduce_rule_groups*.c3`
  - `src/lisp/tests_deduce_query_groups.c3`
  acceptance:
  - seminaive recursive aggregate execution can publish signed add/remove
    deltas for aggregate-bearing recursive SCCs without losing current grouped
    result parity
  - no automatic recursive aggregate fallback remains for currently valid
    stratified recursive aggregate SCCs
  - explain/analyze remain truthful about the active engine and fallback
    status during the staged rollout
  validation:
  - `c3c build`
  - Deduce slice in the bounded container path
  - targeted recursive aggregate regressions
  completed:
  - seminaive recursive delta state now carries signed add/remove tuple slots
    in the shared execution substrate instead of a single positive-only table
  - aggregate-bearing recursive SCCs now seed tuple support tables and
    sign-aware head-materialization hooks behind the fallback gate
  - dormant seminaive aggregate finalize now publishes actual-tuple
    add/remove transitions through the same support-table zero-crossing path
    instead of direct head rewrites plus synthetic aggregate deltas
  - aggregate groups now track signed support counts internally, including
    removal-aware `count`/`sum` updates and `min`/`max` extremum recompute
    when the current support disappears
  - the dormant seminaive aggregate executor now runs mixed non-aggregate
    rules in the same recursive SCC on the same signed-delta/support-table
    substrate instead of skipping them outright
  - positive recursive aggregate SCCs now run on `semi-naive-scc`,
    including mixed aggregate/non-aggregate recursive SCCs that stay within
    positive body atoms
  - recursive aggregate SCCs with lower-stratum negated body atoms now also
    run on `semi-naive-scc`, retiring the last automatic fallback shape
  - mixed positive and mixed negated recursive aggregate regressions now pin
    seminaive exactness and truthful analyze/explain reporting
  - focused tuple-support and aggregate-group regressions now pin recursive-only,
    base-seeded, signed count-removal, and `min`-recompute behavior independently
    of the higher-level execution fixtures
  - recursive aggregate seminaive rollout is complete for the current
    stratified rule surface; remaining work is cleanup/perf, not fallback
    retirement
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `125 passed, 0 failed`
## Notes

- The detailed acceptance criteria and validation commands now live in:
  - `docs/plans/deduce-actionable-backlog-2026-03-20.md`
- Aggregate implementation seams and first patch slices are now documented in:
  - `docs/plans/deduce-v15-aggregate-implementation-plan-2026-03-20.md`
- Items beyond this queue remain in the full roadmap and are intentionally not
  promoted into `TODO.md` yet:
  - maintenance/admin: read-only `deduce/schema`, `deduce/indexes`, and
    `deduce/stats` surfaces are now shipped; refresh/recompute, compact/reset,
    and other write-side admin verbs remain deferred
- runtime hardening: the explicit `escape-scope` iterator consume regression in
  `src/lisp/tests_escape_scope_tests.c3` is fixed and the bounded
  `escape-scope` slice is green again
  - fixed cases:
    - `escape-scope: iterator consume car`
    - `escape-scope: iterator consume len`
  - follow-up retained separately: keep large named-let list-accumulator
    pressure in `memory-lifetime` budget tests rather than re-expanding the
    bounded `escape-scope` smoke slice
- aggregates: recursive aggregate seminaive support now ships for the current
  stratified surface; follow-up work is no longer blocked on fallback
  retirement
  - logic surface: `and` / `or` now ship as variadic short-circuit forms
    instead of binary-only parser forms, including macro-emitted variadic
    lowering through the normal AST path
  - `deduce/analyze` now routes explicit naive mixed recursive aggregate
    requests onto the exact seminaive lane and reports that via
    `naive-recursive-aggregate-mixed-routed-to-seminaive`
  - `deduce/explain` now has matching engine-aware routing coverage, including
    pure positive and pure negated recursive aggregate cases that stay on
    `naive-scc` and do not emit the mixed-route diagnostic
  - pure positive and pure negated recursive aggregate analyze/explain
  - declared `key` relations, declared single/composite `unique` relations,
    and declared single-column `ref` integrity constraints are now enforced
    deterministically at `fact!` time, with machine-checkable payloads and
    schema/analyze visibility for the current `B6.2` slice
  - follow-up for `B6.2` now moves to richer integrity classes beyond
    key/unique/reference relation integrity
  - provenance / why-result: payload shape is documented, implementation still
    remains deferred
  - materialized views
  - magic sets
  - parallel evaluation
