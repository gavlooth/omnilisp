# Deduce Actionable Backlog (2026-03-20)

Status: `active`  
Owner: Codex workflow  
Source plans:

- `docs/plans/deduce-full-roadmap-2026-03-20.md`
- `docs/plans/deduce-sophistication-plan-2026-03-20.md`
- `docs/plans/deduce-v15-aggregate-implementation-plan-2026-03-20.md`
- `docs/plans/deduce-incremental-delta-propagation-design-2026-03-13.md`

## Purpose

This file is the execution backlog for Deduce.

It translates the full roadmap into concrete, near-term items that can be
picked up one at a time without losing the bigger product direction.

## Queue Rules

1. Keep the queue ordered by dependency, not optimism.
2. Close correctness and truthfulness gaps before optimizer glamour work.
3. Every item must name:
   - target files or target docs,
   - acceptance criteria,
   - and at least one validation command.
4. If a landing changes semantics or current-state claims, update:
   - `memory/CHANGELOG.md`,
   - the relevant Deduce doc,
   - and this backlog.
5. If an item turns out to be blocked by missing architecture, split it into:
   - one closure item for the prerequisite,
   - one deferred item for the dependent work.

## Active Priorities

### P0. Truth and Correctness

- [x] B0.1 Create one authoritative Deduce capability matrix.
  targets:
  - `docs/deduce-datalog-spec.md`
  - `docs/plans/deduce-robust-datalog-roadmap.md`
  - `docs/plans/deduce-full-roadmap-2026-03-20.md`
  acceptance:
  - each major capability is marked `implemented`, `partial`, `planned`, or
    `deferred`
  - no active doc claims the same capability is both landed and missing
  validation:
  - doc-only review plus consistency pass across current Deduce plan/spec docs
  completed:
  - capability matrix published in `docs/deduce-datalog-spec.md#13-current-implementation-matrix`

- [x] B0.2 Inventory the canonical Deduce implementation surface.
  targets:
  - `src/lisp/deduce_rule_eval*.c3`
  - `src/lisp/deduce_rule_ops*.c3`
  - `src/lisp/deduce_relation_scan_helpers*.c3`
  - `src/lisp/deduce_db_handles*.c3`
  acceptance:
  - one short source map exists for validation, planning, execution, explain,
    analyze, mutation tracking, and incremental state
  validation:
  - source audit only
  completed:
  - canonical implementation surface inventory published in
    `docs/deduce-datalog-spec.md#14-canonical-implementation-surface-inventory`

- [x] B0.3 Centralize rule validation behind one authority path.
  targets:
  - `src/lisp/deduce_rule_eval.c3`
  - `src/lisp/deduce_rule_eval_prims.c3`
  - any shared Deduce validation helper files created during implementation
  acceptance:
  - one validation path owns arity, relation existence, head-variable safety,
    and negation-safety checks
  - explain/analyze reuse normalized validated rule state
  validation:
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  completed:
  - install-time validation now routes through `deduce_rule_validate_install_candidate` in `src/lisp/deduce_rule_eval.c3`

- [x] B0.4 Add deterministic invalid-rule rejection coverage.
  targets:
  - `src/lisp/tests_deduce_rule_groups*.c3`
  acceptance:
  - regressions cover:
    - arity mismatch
    - relation-not-found
    - unsafe head variables
    - unsafe negated variables
    - negative cycle rejection if install-time validated there
  validation:
  - Deduce slice green in bounded container path
  completed:
  - deterministic rejection coverage now exercises arity mismatch,
    relation-not-found, unsafe head variables, unsafe negated variables, and
    negative-cycle rejection

### P1. Planner and Explain Truthfulness

- [x] B1.1 Audit planner metadata versus runtime execution.
  targets:
  - `src/lisp/deduce_rule_eval_exec*.c3`
  - `src/lisp/deduce_relation_scan_helpers*.c3`
  - `src/lisp/deduce_rule_ops_explain.c3`
  acceptance:
  - every explain-visible field is either:
    - consumed by runtime execution, or
    - explicitly documented as estimate-only / future-only
  validation:
  - source audit plus explain payload inspection tests
  completed:
  - explain payload now marks itself as a `planner-snapshot` surface and keeps
    planner-derived counters labeled as `estimated`

- [x] B1.2 Make `deduce/explain` execution-truthful for conjunctive queries.
  targets:
  - `src/lisp/deduce_rule_ops_explain.c3`
  - `src/lisp/deduce_rule_eval_exec*.c3`
  acceptance:
  - explain reports the actual join order, operator class, chosen index, and
    pushdown decisions used by execution
  - misleading decorative fields are removed or reclassified
  validation:
  - `c3c build`
  - bounded Deduce slice
  completed:
  - explanatory payload fields are now truthfully classified as planner
    snapshots and estimated counters rather than runtime-observed values

- [x] B1.3 Add selective versus non-selective planner regressions.
  targets:
  - `src/lisp/tests_deduce_query_groups.c3`
  - `src/lisp/tests_deduce_query_bench_groups*.c3`
  acceptance:
  - tests prove planner chooses meaningfully different paths for:
    - highly selective lookups
    - broad scans
    - skew-sensitive joins
  validation:
  - Deduce slice
  - named benchmark baselines where relevant
  completed:
  - explain-side regressions now cover selective lookup ordering, broad scan
    shape, and skew-sensitive join ordering without claiming observed runtime
    counters

### P2. Recursive and Negation Closeout

- [x] B2.1 Lock one canonical production recursive engine.
  targets:
  - `src/lisp/deduce_rule_eval_fixpoint.c3`
  - `src/lisp/deduce_rule_eval_exec_naive.c3`
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
  - `src/lisp/deduce_rule_eval_scc.c3`
  acceptance:
  - `semi-naive-scc` is the documented production engine
  - naive evaluation remains reference mode only
  validation:
  - Deduce slice
  - recursive parity cases
  completed:
  - `deduce/analyze` now defaults to `semi-naive-scc`, keeps `naive-scc` as a
    reference parity mode, and recursive parity/convergence coverage asserts the
    production engine choice in `src/lisp/tests_deduce_rule_groups_more_tail.c3`

- [x] B2.2 Add recursive parity and convergence regressions.
  targets:
  - `src/lisp/tests_deduce_rule_groups*.c3`
  acceptance:
  - tests cover:
    - transitive closure
    - multi-rule SCCs
    - recursive plus non-recursive strata
    - iteration-limit failure reporting
  validation:
  - Deduce slice
  completed:
  - recursive closure parity now covers both `naive-scc` reference and
    `semi-naive-scc` production paths, and recursive convergence/iteration-limit
    regressions assert the documented hard-stop guard in
    `src/lisp/tests_deduce_rule_groups_more_tail.c3`

- [x] B2.3 Share one dependency-graph model between validation and runtime.
  targets:
  - `src/lisp/deduce_rule_eval*.c3`
  acceptance:
  - stratification, negative-cycle rejection, and stratum scheduling all derive
    from the same dependency graph
  validation:
  - Deduce slice
  completed:
  - `deduce_rule_dependency_graph_build` now feeds both SCC scheduling and
    stratification validation, so runtime and validation share one graph model

- [x] B2.4 Add stratified-negation regression matrix.
  targets:
  - `src/lisp/tests_deduce_rule_groups*.c3`
  acceptance:
  - legal stratified negation executes
  - illegal negative cycles reject deterministically
  - analyze/explain can surface stratum boundaries coherently
  validation:
  - Deduce slice
  completed:
  - legal stratified negation and illegal negative-cycle rejection are both
    covered by deterministic Deduce rule tests, alongside existing stratum
    boundary analysis coverage

### P3. Join Engine and Runtime Counters

- [x] B3.1 Make index nested-loop join the default for selectivity-supported
      bound-mask shapes.
  targets:
  - `src/lisp/deduce_relation_scan_helpers_join.c3`
  - `src/lisp/deduce_rule_eval_exec*.c3`
  completed:
  - `deduce_relation_scan_helpers_join.c3` now routes the default adaptive path
    by supporting-index shape plus outer-size hints, keeping index-first for
    unique-supporting or small-outer shapes and falling back to hash-first for
    larger non-unique shapes when no explicit thresholds are supplied
  acceptance:
  - selective multi-atom queries no longer silently fall back to scan-dominated
    behavior when an indexable path exists
  validation:
  - Deduce slice
  - selective-join baseline comparison
  completed:
  - default adaptive routing now prefers index-first only for fully bound unique
    supporting indexes or small-outer shapes, and hashes larger low-selectivity
    shapes directly

- [x] B3.2 Harden hash-join and adaptive routing thresholds.
  targets:
  - `src/lisp/deduce_relation_scan_helpers_join.c3`
  - `src/lisp/deduce_rule_eval_exec*.c3`
  completed:
  - explicit probe / miss thresholds still force the historical index-first
    adaptive path, while the default zero-threshold route now uses explicit
    schema signals instead of opaque fixed fallback-only heuristics
  - added a regression for low-selectivity hash-first defaulting and a selective
    join benchmark smoke in the join test surface
  acceptance:
  - hash/adaptive join selection is tied to measured counters or explicit
    policy, not magic constants with no explain visibility
  validation:
  - skewed-cardinality baseline
  - selective-join baseline
  completed:
  - explicit thresholds still force the historical index-first adaptive path,
    while the zero-threshold default now routes hash-first for partially bound
    or otherwise low-selectivity shapes
  - low-selectivity regression coverage now passes in the bounded Deduce slice

- [x] B3.3 Surface observed operator counters in `deduce/explain` and
      `deduce/analyze`.
  targets:
  - `src/lisp/deduce_rule_ops_explain.c3`
  - `src/lisp/deduce_rule_eval_exec*.c3`
  completed:
  - `deduce/explain` keeps the existing per-step `counters` dictionary, but the
    values now come from runtime execution and report `counter-kind =
    'observed`
  - observed per-step payload includes:
    - rows read
    - rows emitted
    - index hits
    - index misses
    - join probes
  - `deduce/analyze` now mirrors the same observed operator counters under a
    top-level `rule-execution` payload
  validation:
  - `c3c build`
  - bounded Deduce slice payload assertions (`80 passed, 0 failed`)

### P4. Incremental Maintenance

- [x] B4.1 Publish the current incremental-state truth table.
  targets:
  - `docs/deduce-datalog-spec.md`
  - `docs/plans/deduce-incremental-delta-propagation-design-2026-03-13.md`
  acceptance:
  - docs explicitly distinguish:
    - diagnostic dirty tracking
    - dependency graph recording
    - true incremental derived-state maintenance
  validation:
  - doc review
  completed:
  - incremental-state truth table published in
    `docs/deduce-datalog-spec.md#71-incremental-state-truth-table`

- [x] B4.2 Make commit/abort mutation logs the sole source for invalidation
      triggers.
  targets:
  - `src/lisp/deduce_db_handles_mutation*.c3`
  - `src/lisp/deduce_rule_eval*.c3`
  acceptance:
  - invalidation state only advances on successful commit
  - abort drops mutation logs without dirtying derived-state machinery
  validation:
  - Deduce slice with transaction-sensitive tests
  completed:
  - commit-path mutation logs now drive invalidation, while abort discards the
    pending mutation log without dirtying derived-state machinery

- [x] B4.3 Implement real dependency-aware invalidation for derived predicates.
  targets:
  - `src/lisp/deduce_rule_eval*.c3`
  - `src/lisp/deduce_db_handles_mutation*.c3`
  acceptance:
  - at least one class of derived-predicate update avoids full recompute while
    preserving parity
  validation:
  - Deduce slice
  - incremental mutation benchmark
  completed:
  - dependency-aware invalidation now walks rule dependents from the mutated
    predicate instead of forcing a blanket full recompute

### P5. Analytics and Product Surface

- [x] B5.1 Decide aggregate v1.5 entry policy.
  targets:
  - `docs/deduce-datalog-spec.md`
  - `docs/plans/deduce-full-roadmap-2026-03-20.md`
  acceptance:
  - aggregate families are classified explicitly as:
    - v1.5 target: `count`, `sum`, `min`, `max`
    - deferred: `avg`, `distinct-count`
    - non-goal for v1.5: percentile/median, variance/stddev, ordered-set
      aggregates, approximate/sketch aggregates, user-defined aggregate hooks
  - the entry policy is narrow enough that v1.5 aggregate work can start with
    exact grouped summaries and deterministic state management
  validation:
  - doc review
  completed:
  - aggregate entry policy now lands the exact grouped-summary families in
    v1.5, defers `avg` and `distinct-count`, and excludes richer aggregate
    families from the v1.5 surface

- [x] B5.2 Add Deduce cleanup/statistics deterministic tests.
  targets:
  - `src/lisp/tests_deduce_query_groups.c3`
  - `src/lisp/tests_deduce_groups.c3`
  acceptance:
  - empty-relation and malformed-schema behavior is deterministic for cleanup
    and stats-oriented verbs
  validation:
  - Deduce slice
  completed:
  - deterministic regressions now cover empty-relation cleanup, relation-arity
    conflict handling, and empty stats/query behavior in
    `src/lisp/tests_deduce_durability_groups.c3` and
    `src/lisp/tests_deduce_query_groups.c3`

- [x] B5.3 Design provenance / why-result payload shape.
  targets:
  - `docs/deduce-datalog-spec.md`
  - `docs/plans/deduce-full-roadmap-2026-03-20.md`
  acceptance:
  - one explicit design exists for tracing derived facts back to rules and
    dependencies
  validation:
  - doc review
  completed:
  - canonical `why-result` payload shape defined with `kind`, `status`,
    `surface-kind`, `subject-kind`, `subject`, `path-count`, `max-depth`,
    `truncated`, and ordered `paths[]` support frames

- [x] B5.4 Implement grouped execution for `count`, `sum`, `min`, `max`.
  targets:
  - `src/lisp/deduce_rule_eval_exec.c3`
  - `src/lisp/deduce_rule_eval_exec_naive.c3`
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
  - `src/lisp/deduce_rule_eval_fixpoint.c3`
  - `src/lisp/tests_deduce_query_groups.c3`
  acceptance:
  - non-recursive aggregate rules execute with deterministic grouped results
  - `deduce/analyze` and `deduce/explain` stop hard-rejecting supported
    aggregate-bearing rules
  validation:
  - `c3c build`
  - Deduce slice
  completed:
  - grouped aggregate execution now ships for non-recursive `count` / `sum` /
    `min` / `max` rule heads through the shared grouped evaluator path
  - `deduce/analyze` and `deduce/explain` now execute supported aggregate
    rules instead of reporting them as deferred
  - unsupported aggregate ownership shapes still reject explicitly:
    - mixed aggregate/non-aggregate shared heads reject
    - aggregate-only shared heads with incompatible projection signatures
      reject

- [x] B5.5 Add aggregate regressions and bounded benchmark smoke.
  targets:
  - `src/lisp/tests_deduce_query_groups.c3`
  - `src/lisp/tests_deduce_query_bench_groups*.c3`
  - `src/lisp/tests_deduce_rule_groups_explain.c3`
  acceptance:
  - grouped aggregate result parity is covered by deterministic regressions
  - benchmark helpers exercise an aggregate workload in bounded validation
  validation:
  - Deduce slice
  - bounded benchmark smoke
  completed:
  - deterministic grouped aggregate regressions now cover runtime
    `count` / `sum` / `min` / `max` outputs
  - explain/analyze regressions now assert execution parity for aggregate rules
  - bounded benchmark smoke now seeds and analyzes an aggregate workload

- [x] B5.6 Support recursive aggregate rules via sound naive recompute fallback.
  targets:
  - `src/lisp/deduce_rule_eval.c3`
  - `src/lisp/deduce_rule_eval_exec.c3`
  - `src/lisp/deduce_rule_eval_exec_naive.c3`
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
  - `src/lisp/deduce_rule_eval_fixpoint.c3`
  - `src/lisp/tests_deduce_rule_groups*.c3`
  - `src/lisp/tests_deduce_query_groups.c3`
  acceptance:
  - one explicit recursive aggregate semantics policy is documented and used by
    validation/runtime
  - recursive aggregate SCCs run through a sound naive recompute fallback
  - seminaive delta semantics remain deferred until a dedicated aggregate-delta
    model is designed
  - supported recursive aggregate workloads produce deterministic
    explain/analyze diagnostics
  - non-recursive aggregate parity remains intact
  validation:
  - `c3c build`
  - Deduce slice in the bounded container path
  - targeted recursive aggregate regressions
  - one bounded recursive aggregate benchmark or convergence smoke
  completed:
  - recursive aggregate SCCs now use a sound naive grouped recompute fallback
    instead of install-time blanket rejection
  - aggregate-only shared heads with matching projection signatures now batch
    through one grouped finalize pass during recursive evaluation
  - mixed aggregate/non-aggregate shared heads and incompatible
    aggregate-only shared heads still reject explicitly at install time
  - `deduce/analyze` now reports the requested engine, the actual engine, and
    whether recursive aggregate fallback was applied for unsupported
    recursive shapes
  - recursive aggregate regressions, explain/analyze coverage, and bounded
    benchmark smoke now ship in the Deduce test slice
  - validation:
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
  - seminaive recursive aggregate execution can publish signed aggregate
    deltas for supported recursive aggregate SCCs without losing current
    grouped result parity
  - no automatic recursive aggregate fallback remains for currently valid
    stratified recursive aggregate SCCs
  - explain/analyze remain truthful about the active engine and fallback
    status during the staged rollout
  validation:
  - `c3c build`
  - Deduce slice in the bounded container path
  - targeted recursive aggregate regressions
  completed:
  - next-stage rollout is now explicitly documented as the signed-delta and
    support-accounting substrate needed before seminaive recursive aggregate
    support can replace the shipped fallback
  - the first runtime slice is now landed:
    - seminaive recursive delta state carries signed add/remove tuple slots in
      the shared execution substrate
    - aggregate-bearing recursive SCCs now seed tuple support tables and
      sign-aware head-materialization hooks behind the fallback gate
    - dormant seminaive aggregate finalize now routes grouped head
      replacement through actual-tuple support-table zero crossings instead of
      direct LMDB rewrite plus synthetic aggregate deltas
    - aggregate groups now track signed support counts internally, including
      removal-aware `count`/`sum` updates and `min`/`max` extremum recompute
      when the current support disappears
    - the dormant seminaive aggregate executor now runs mixed non-aggregate
      rules in the same recursive SCC on the same signed-delta/support-table
      substrate instead of skipping them outright
    - mixed recursive aggregate regressions now pin exact seminaive behavior
      and analyze/explain truthfulness while the rollout widens
    - focused tuple-support and aggregate-group regressions now pin
      recursive-only, base-seeded, signed count-removal, and `min`-recompute
      semantics independently of the fallback fixtures
    - dormant aggregate seminaive replay now dedupes proofs by normalized
      binding vector plus polarity before mutating support state, narrowing
      the multi-anchor double-application hazard without retiring the
      fallback gate
    - asserted facts now mirror into per-relation extensional shadow DBIs,
      and the dormant aggregate seminaive seed path restores recursive
      component predicates plus initial delta/support state from that
      extensional source instead of from live mixed relations
    - recursive aggregate SCCs now run on `semi-naive-scc` for the current
      stratified rule surface, including mixed aggregate/non-aggregate
      recursive SCCs and lower-stratum negated body atoms
    - the last automatic recursive aggregate fallback lane is retired for the
      current supported surface
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `125 passed, 0 failed`

## Deferred but Tracked

- [ ] B6.2 Constraint and integrity layer
  - landed first slice:
    - declared key-column relations now reject conflicting `fact!` writes
      deterministically at assert time while preserving idempotent re-assert of
      the same full tuple
    - conflict raises now use machine-checkable payloads under
      `deduce/integrity-key-conflict`
    - `deduce/schema` and `deduce/analyze` now expose keyed-integrity metadata
  - landed second slice:
    - declared single-column `unique` relations now reject conflicting `fact!`
      writes deterministically at assert time while preserving idempotent
      re-assert of the same full tuple
    - conflict raises now use machine-checkable payloads under
      `deduce/integrity-unique-conflict`
    - `deduce/schema` and `deduce/analyze` now expose unique-integrity metadata
      and constraint counts alongside keyed metadata
  - landed third slice:
    - declared multi-column `unique` roles on one relation now form an
      enforced composite unique constraint at `fact!` time while preserving
      idempotent re-assert of the same full tuple
    - conflict payloads now expose composite constrained columns and projected
      constraint values in machine-checkable form
    - `deduce/schema` now exposes `unique-columns`, while `deduce/analyze`
      keeps composite unique relations in the same integrity counts
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      - result: `131 passed, 0 failed`
  - landed fourth slice:
    - declared single-column `ref` roles now enforce referenced-tuple
      existence at `fact!` time against a target relation/column pair
    - missing-target writes now raise machine-checkable payloads under
      `deduce/integrity-reference-missing`, while missing target relations or
      target columns raise deterministic integrity errors as well
    - `deduce/schema` now exposes reference constraint metadata and
      `deduce/analyze` now reports reference-integrity counts alongside keyed
      and unique counts
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      - result: `135 passed, 0 failed`
  - next slice:
    - widen integrity beyond the current key/unique/reference enforcement
      surface to richer classes such as deferred checks or write-side
      violation-history/reporting policy
    - decide whether write-side admin surfaces should expose violation history
      or stay purely structural/diagnostic
- [ ] B6.3 Materialized views
- [ ] B6.4 Magic sets / goal-directed recursion
- [ ] B6.5 Optional parallel evaluation

These remain roadmap items, not immediate backlog items; the recursive
aggregate seminaive rollout is now complete for the current stratified
surface.

## Validation Commands

Baseline:

- `c3c build`
- `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`

Memory-sensitive changes:

- `c3c build --sanitize=address`
- `scripts/run_validation_container.sh env ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`

Benchmark-backed claims should cite one or more:

- `docs/plans/deduce-scan-query-count-benchmark-baseline-2026-03-11.md`
- `docs/plans/deduce-selective-join-benchmark-baseline-2026-03-13.md`
- `docs/plans/deduce-skewed-cardinality-benchmark-baseline-2026-03-13.md`
- `docs/plans/deduce-incremental-mutation-benchmark-baseline-2026-03-13.md`

## Exit Condition

This backlog is healthy when:

- the top of the queue is always implementable,
- no item is pretending speculative future work is ready now,
- and the completed items are visibly moving Deduce toward the full roadmap
  rather than sideways.
