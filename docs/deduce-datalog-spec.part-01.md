# Deduce Datalog Spec (Draft v1)

Status: draft execution spec for robust Deduce roadmap.
Last updated: 2026-03-25.

This document defines the target Datalog semantics for Deduce.
It is implementation-facing and should be used to guide parser/runtime/planner
work.

## 1. Scope

Deduce is the Datalog subsystem for Omni.

v1 scope:

- extensional relations (EDB facts),
- intensional relations (IDB rules),
- recursive rules with fixpoint evaluation,
- stratified negation,
- deterministic query outputs by relation row order contract.

v1 non-goals (explicit defer):

- magic sets,
- parallel fixpoint evaluation,
- probabilistic/fuzzy logic extensions.

## 1.1 Delivery Tiers (Locked)

v1 required:

1. Rule install + validation pipeline (arity, safety, stratification checks).
2. Planner-backed conjunctive query execution with explicit explain output.
3. Recursive evaluation using semi-naive fixpoint with deterministic
   convergence behavior.
4. Stable migration behavior for existing `deduce/query` and
   `deduce/match` surfaces.

v1.5 deferred:

1. Incremental maintenance for derived relations under mutation-heavy workloads.
2. Advanced optimizer features (magic sets and beyond baseline heuristics).
3. Adaptive/parallel recursive execution strategies.

v1.5 aggregate entry policy:

- v1.5 aggregate work starts with exact grouped aggregates that have small,
  deterministic state and straightforward explainability:
  - `count`
  - `sum`
  - `min`
  - `max`
- `avg` and `distinct-count` are deferred until aggregate state semantics and
  empty-group behavior are explicit and tested.
- percentile/median, variance/stddev, ordered-set aggregates,
  approximate/sketch aggregates, and user-defined aggregate hooks are
  non-goals for v1.5.

v1.5 aggregate syntax target (deferred implementation, locked surface):

- aggregate queries use a projection-driven shape:

```lisp
(query [(count ?name)]
  (adult ?name _))

(query [?dept (max ?salary)]
  (employee ?name ?dept ?salary))
```

- bare variables in the projection are grouping keys
- aggregate projection terms are limited to:
  - `(count ?x)`
  - `(sum ?x)`
  - `(min ?x)`
  - `(max ?x)`
- aggregate metadata and payload conventions should be shared between
  projection-driven queries and future rule-level aggregate support

Acceptance test mapping:

- v1 acceptance must include validation, planner behavior assertions, recursive
  parity, and parity semantics checks.
- v1.5 acceptance must include incremental update benchmarks and correctness
  parity against full recompute.

## 1.2 Canonical Public Surfaces (Locked)

Canonical commands for docs/examples and future tests:

- `deduce/rule!` for rule installation,
- `deduce/query` for predicate/query evaluation,
- `deduce/match` for pattern-variable binding queries,
- `deduce/schema` for relation schema inspection,
- `deduce/indexes` for relation index inspection,
- `deduce/stats` for relation statistics inspection,
- `deduce/explain` for plan/evaluation introspection,
- `deduce/analyze` for statistics-oriented diagnostics.

Bootstrap runtime note (current implementation slice):

- `deduce/rule!` currently requires an explicit database handle first:
  `(deduce/rule! db head-atom body-atom...)`.
- this keeps predicate arity/stratification validation database-scoped while
  the broader program surface is being built.

Naming policy:

- Canonical names above define the language-facing contract.
- New feature docs and benchmarks must use canonical names.

## 1.3 Current Implementation Matrix

Status legend:

- `implemented`: present in the current tree and exercised by existing docs/tests.
- `partial`: present but still split, estimate-driven, or missing the final
  truth-preserving polish.
- `deferred`: intentionally not part of the current delivered surface.

| Capability | Current status | Current state in this tree | Source anchors |
|---|---|---|---|
| Relation declarations and fact storage | `implemented` | EDB/IDB relation storage, tuple codec, and schema query surfaces are present. | `src/lisp/deduce_db_handles.c3`, `src/lisp/deduce_tuple_codec.c3`, `src/lisp/deduce_schema_query.c3` |
| Rule install and validation | `implemented` | `deduce/rule!` now routes through one validation authority that owns arity, relation existence, head-variable safety, negation-safety, and stratification rejection over normalized rule IR. | `src/lisp/deduce_rule_eval_prims.c3`, `src/lisp/deduce_rule_eval.c3` |
| Query execution | `implemented` | `deduce/query`, `deduce/match`, `deduce/count`, `deduce/scan`, and `deduce/scan-range` are available as stable command surfaces. The relation-read surfaces now also ship relation-targeted goal-directed execution slices for eligible positive recursive closures via an optional rule-index selector, and plain reads auto-execute that same eligible dirty closure in tracked mode except for the current narrow demand-bound exceptions on bound `match`, simple equality-filter `query`, and `scan-range` positions where `lower == upper`. | `src/lisp/deduce.c3`, `src/lisp/deduce_relation_ops_query.c3`, `src/lisp/deduce_relation_ops.c3`, `src/lisp/unify.c3` |
| Planner-backed conjunctive execution | `partial` | Deterministic planning, bound-mask analysis, and operator classification exist, but execution/explain truthfulness still needs tightening. | `src/lisp/deduce_rule_ops_planning.c3`, `src/lisp/deduce_rule_eval_exec.c3`, `src/lisp/deduce_rule_ops_explain.c3` |
| Recursive evaluation | `implemented` | `semi-naive-scc` is the canonical production engine; `naive` and `naive-scc` remain reference/debug modes for explicit comparison and diagnostics. SCC planning and recursive fixpoint evaluation share one dependency graph. | `src/lisp/deduce_rule_eval_exec_naive.c3`, `src/lisp/deduce_rule_eval_exec_seminaive.c3`, `src/lisp/deduce_rule_eval_fixpoint.c3`, `src/lisp/deduce_rule_eval_scc.c3` |
| Stratified negation | `partial` | Negation safety and negative-cycle rejection exist, and the validation/runtime path now shares one dependency graph, but broader incremental maintenance remains a separate roadmap item. | `src/lisp/deduce_rule_eval_prims.c3`, `src/lisp/deduce_rule_eval.c3` |
| Explainability | `partial` | `deduce/explain` exists and emits deterministic payload skeletons, but some counters and fields are still planner-estimate driven. It now also exposes planner-side goal-directed eligibility metadata for selected recursive rule shapes without changing query execution semantics. | `src/lisp/deduce_rule_ops_explain.c3` |
| Analyze/fixpoint diagnostics | `partial` | `deduce/analyze` exposes recursive SCC/fixpoint diagnostics, mutation metadata, keyed/unique/reference integrity relation counts, DB-wide integrity violation counters, bounded recent integrity violation history, and the first selector-scoped goal-directed execution path for eligible recursive closures, but broader magic-set rewrite and incremental maintenance remain deferred. | `src/lisp/deduce_rule_eval_exec.c3`, `src/lisp/deduce_rule_eval_exec_seminaive.c3`, `src/lisp/deduce_rule_eval_fixpoint.c3`, `src/lisp/deduce_rule_eval_prims.c3` |
| Mutation logging and dirty tracking | `partial` | Commit/abort logging and dirty-state reporting exist, but they do not yet form a full incremental derived-state engine. | `src/lisp/deduce_db_handles_mutation.c3`, `src/lisp/deduce_db_handles_mutation_tracking.c3`, `src/lisp/deduce_db_handles_mutation_txn.c3` |
| Materialized views | `partial` | Explicit manual materialization is available for derived relations via `deduce 'materialize! relation` / `deduce/materialize!`, and explicit lifecycle teardown is now available through `deduce 'dematerialize! relation` / `deduce/dematerialize!`. Declaration-time materialization now ships with three accepted spellings: `[relation db materialized] rel ...`, `[relation db materialized manual] rel ...`, and `[relation db materialized on-read] rel ...`; unknown declaration-time refresh policies reject deterministically. Declaration-based materialization is lifecycle-gated: it is only refreshable once the relation actually has derived rule heads. Manual refresh is available through `deduce 'refresh! db-or-materialized-relation` / `deduce/refresh!`, while `on-read` triggers the same relation-scoped refresh surface before ordinary stored-tuple reads on stale derived materialized relations. Relation-scoped refresh now has two truthful tracked subpaths: ordinary `targeted` dependency-closure recompute and `incremental-targeted` maintained-update replay. The current incremental-targeted lane covers already-refreshed direct-copy materialized relations sourced either from one extensional predicate or from one already-refreshed ready materialized direct-copy source. Unsupported shapes, never-refreshed targets, stale derived sources, and `full-recompute-required` still fall back to the existing broader paths. DB-scoped refresh only stamps ready materialized views as refreshed. Materialized intent and refresh-history metadata now persist across reopen / `open-named` for file-backed DBs, and explicit dematerialization clears that persisted lifecycle record again without dropping the relation or its rules. A compact persisted rule/dependency catalog summary now survives reopen for admin truth, and supported persisted executable rule signatures now restore live derived execution against inferred predicate schemas, so reopened `deduce/analyze`, `deduce/refresh!`, and stale `on-read` reads regain truthful ready/fresh behavior for the current supported signature surface. Unsupported persisted signature shapes still fall back to summary-only admin truth. Rule-install invalidation is selective: only already-ready materialized views are marked stale, while declared-but-unready materialized views keep their `never-refreshed` lifecycle. Schema/stats/analyze expose refresh policy, freshness, readiness, stale reasons, and dirty-frontier metadata, while fuller durability policy for broader persisted derived-state surfaces remains deferred. | `src/lisp/deduce_schema_query.c3`, `src/lisp/deduce_db_handles.c3`, `src/lisp/deduce_db_handles_mutation_tracking.c3`, `src/lisp/deduce_rule_eval_prims.c3`, `src/lisp/deduce_rule_eval_fixpoint.c3`, `src/lisp/deduce.c3`, `src/lisp/parser_define_relation_attr.c3` |
| True incremental derived-state maintenance | `partial` | The current tree now ships two narrow maintained-update classes for ready materialized direct-copy relations: extensional-source replay and one-step derived-source replay through an already-refreshed ready materialized direct-copy source. Broader delta propagation remains a roadmap item. | `src/lisp/deduce_db_handles_mutation_tracking.c3`, `src/lisp/deduce_db_handles_mutation_txn.c3`, `src/lisp/deduce_schema_query.c3`, `docs/plans/deduce-incremental-delta-propagation-design-2026-03-13.md` |
| Maintenance/admin introspection | `implemented` | Read-only schema, index, and stats inspection are available on relation handles through `deduce/schema`, `deduce/indexes`, and `deduce/stats`. Schema payloads include keyed/unique/reference integrity constraint metadata for declared single-column and composite integrity constraints, including whether a `ref` target column is a candidate key and whether its target relation is still live, and stats payloads now expose relation-local integrity violation counters, last violation code, and bounded recent integrity violation history. | `src/lisp/deduce_schema_query.c3`, `src/lisp/deduce.c3` |
| Aggregates, constraints, provenance, magic sets, parallel recursion | `partial` | Rule-head grouped aggregates now execute for `count`/`sum`/`min`/`max` in both non-recursive rules and aggregate-bearing recursive SCCs. Recursive aggregate SCCs now run on `semi-naive-scc` for the current stratified rule surface, including mixed aggregate/non-aggregate recursive SCCs and lower-stratum negated body atoms. The current constraint slice ships extensional integrity enforcement for declared `key` relations, single/composite `unique` constraints, and single-column `ref` integrity constraints to candidate-key target columns: conflicting or missing-target `fact!` writes are rejected with machine-checkable payloads, `retract!`, `clear!`, and `drop!` reject deleting still-referenced target tuples, `deduce/stats` reports relation-local violation counters plus bounded recent violation history, `deduce/analyze` reports DB-wide violation totals plus bounded recent history, and explicit `write-deferred` transactions defer both staged `fact!` checks and final delete-side reference validation to commit-time snapshot validation. Derived writes now also enforce declared `key` / `unique` / `ref` constraints across ordinary and aggregate rule-head publish, including recursive seminaive materialization; derived `ref` heads validate against the final component transaction snapshot before commit rather than on each publish transition. The next widened integrity class is fixed as canonical `check`; aliases such as `assert`, `predicate`, and `guard` are rejected for that lane. Declared unary column `check` constraints now ship declaration/schema/admin plus write-enforcement support: relation declarations accept `(check predicate column)`, `deduce/schema` exposes `kind = 'check`, `predicate`, `columns`, and `enforced = false`, `deduce/analyze` reports DB-wide `check-constraint-count`, immediate `fact!`, derived rule-head publish, and deferred `write-deferred` commit-time validation reject bad tuples, generic integrity history surfaces expose `violation-class = 'check` plus deterministic failure codes, and relation/DB admin payloads now also expose dedicated `check-integrity-violation-count` counters. Provenance, magic sets, and parallel recursion remain follow-up work. | `src/lisp/deduce_rule_ops.c3`, `src/lisp/deduce_rule_eval.c3`, `src/lisp/deduce_rule_eval_exec.c3`, `src/lisp/deduce_rule_ops_explain.c3`, `src/lisp/deduce_relation_ops.c3`, `src/lisp/deduce_schema_query.c3`, `src/lisp/deduce_rule_eval_prims.c3`, `src/lisp/parser_define_relation_attr.c3`, `src/lisp/deduce_relation_ops_validation.c3`, `TODO.md` |

## 1.4 Canonical Implementation Surface Inventory

| Surface | Primary files | Current role | Status |
|---|---|---|---|
| Public dispatch façade | `src/lisp/deduce.c3` | Top-level `deduce` command dispatch and routing into the subsystem. | `implemented` |
| DB handle and mutation lifecycle | `src/lisp/deduce_db_handles.c3`, `src/lisp/deduce_db_handles_register.c3`, `src/lisp/deduce_db_handles_mutation.c3`, `src/lisp/deduce_db_handles_mutation_tracking.c3`, `src/lisp/deduce_db_handles_mutation_txn.c3` | Relation handles, transaction lifecycle, mutation logging, and incremental metadata capture. | `partial` |
| Relation access and query helpers | `src/lisp/deduce_relation_ops.c3`, `src/lisp/deduce_relation_ops_query.c3`, `src/lisp/deduce_relation_scan_helpers.c3`, `src/lisp/deduce_relation_scan_helpers_join.c3`, `src/lisp/deduce_relation_scan_helpers_more.c3`, `src/lisp/deduce_tuple_codec.c3`, `src/lisp/deduce_schema_query.c3` | Predicate lookup, scan/join helpers, tuple encoding, and schema-side query utilities. | `partial` |
| Rule install and validation | `src/lisp/deduce_rule_eval_prims.c3`, `src/lisp/deduce_rule_eval.c3` | `deduce/rule!` install path and centralized rule validation authority over normalized IR. | `implemented` |
| Recursive execution engines | `src/lisp/deduce_rule_eval_exec.c3`, `src/lisp/deduce_rule_eval_exec_naive.c3`, `src/lisp/deduce_rule_eval_exec_seminaive.c3`, `src/lisp/deduce_rule_eval_fixpoint.c3`, `src/lisp/deduce_rule_eval_scc.c3` | Naive, semi-naive, SCC, and fixpoint execution paths for analyze/evaluation, with `semi-naive-scc` as the production default. | `implemented` |
| Planning and explain surfaces | `src/lisp/deduce_rule_ops.c3`, `src/lisp/deduce_rule_ops_planning.c3`, `src/lisp/deduce_rule_ops_explain.c3` | Install-time planning, explain payload construction, and operator classification. | `partial` |

This inventory is intentionally narrow: it names the canonical ownership
surface for the current tree, not every helper file or historical split shard.

## 2. Core Entities

- `Fact`: concrete tuple inserted into an EDB relation.
- `Atom`: predicate application in rule body/head.
- `Rule`: head atom + conjunction body.
- `Program`: set of relation declarations + facts + rules.
- `Query`: predicate form with bound/unbound positions.

## 3. Rule Shape

Canonical logical form:

```lisp
(deduce/rule!
  (head-predicate ?x ?y)
  (body-predicate-a ?x ?z)
  (body-predicate-b ?z ?y))
```

Validation requirements:

- head predicate arity must match relation declaration.
- body atom arities must match relation declarations.
- all variables in head must appear in at least one positive body atom.
- variables in negated atoms must be bound by prior positive atoms in the same
  rule (safety rule).

## 4. Query Semantics

Canonical query form remains command-dispatched (`deduce/query`, `deduce/match`)
for the migration path, but logical semantics are:

- evaluate against current program state,
- produce bindings satisfying all body atoms,
- return `nil` when no results (absence semantics),
- command-style mutations return `Void`.

Truthiness and absence follow Omni core rules:

- only `nil` and `false` are falsy,
- no-result query is `nil` / empty relation payload according to API contract,
- operational failures are effect errors, not synthetic `nil`.

Deferred v1.5 aggregate query contract:

- aggregate queries are projection-driven rather than callback-filter driven
- projection variables without aggregate wrappers are group-by keys
- every aggregate projection must consume a grounded variable from the positive
  query body prefix
- aggregate queries remain deterministic by stable group-key ordering contract

Current implementation note:

- rule-level aggregate support is now partially landed with execution for both
  the non-recursive lane and aggregate-bearing recursive SCCs:
  - aggregate-aware head terms are normalized into installed rule signatures,
  - `deduce/rule!` accepts `count` / `sum` / `min` / `max` aggregate
    projections in rule heads and rejects malformed / ungrounded uses,
  - `deduce/rule!` also rejects unsupported aggregate ownership shapes:
    - mixed aggregate/non-aggregate shared heads reject,
    - aggregate-only shared heads with incompatible projection signatures
      reject,
  - `deduce/explain` exposes aggregate projection counts plus per-head
    projection kind and source-var metadata and can now collect observed step
    counters for supported aggregate rules,
  - `deduce/explain` now accepts an optional engine in the 3-argument form
    `(deduce/explain db selector engine)` and reports:
    - `execution-engine`,
    - `requested-execution-engine`,
    - `recursive-aggregate-naive-fallback`,
    - `naive-recursive-aggregate-mixed-routed-to-seminaive`,
  - `deduce/explain` now also reports the first goal-directed planning
    metadata slice:
    - `goal-directed-component-id`,
    - `goal-directed-eligible`,
    - `goal-directed-shape`,
    - `goal-directed-blockers`,
    - with the currently eligible shape restricted to positive recursive
      closure with no aggregates and no negated body atoms,
  - `deduce/analyze` now executes supported aggregate-bearing rules through the
    grouped evaluator path instead of rejecting them as deferred,
  - aggregate-bearing recursive SCCs now execute on `semi-naive-scc` for the
    current stratified rule surface instead of falling back to grouped naive
    recompute,
  - `deduce/analyze` reports:
    - `execution-engine`,
    - `requested-execution-engine`,
    - `recursive-aggregate-naive-fallback`,
    - `naive-recursive-aggregate-mixed-routed-to-seminaive`,
  - the staged seminaive follow-through has started in the runtime substrate:
    - recursive delta state now has signed add/remove slots,
    - aggregate-bearing recursive SCCs now seed tuple support tables and
      sign-aware head-materialization hooks behind the fallback gate,
    - dormant seminaive aggregate finalize now routes grouped head
      replacement through actual-tuple support-table zero crossings instead of
      direct rewrites plus synthetic aggregate deltas,
    - aggregate groups now track signed support counts internally, including
      removal-aware `count`/`sum` updates and `min`/`max` extremum recompute
      when the current support disappears,
    - the dormant seminaive aggregate executor now runs mixed non-aggregate
      rules in the same recursive SCC on the same signed-delta/support-table
      substrate instead of skipping them,
    - recursive aggregate SCCs now run on `semi-naive-scc`, including mixed
      aggregate/non-aggregate recursive SCCs and lower-stratum negated body
      atoms,
    - mixed recursive aggregate fixtures now verify exact seminaive behavior
      and truthful analyze/explain reporting for both positive and negated
      shapes,
    - focused tuple-support and aggregate-group regressions now pin
      recursive-only, base-seeded, signed count-removal, and `min`-recompute
      semantics independently of the higher-level execution fixtures.

## 5. Evaluation Model

Evaluation modes:

- non-recursive strata: planned join execution over EDB/IDB inputs,
- recursive strata: semi-naive SCC fixpoint by default,
- aggregate-bearing recursive SCCs now use the same seminaive SCC lane for the
  current stratified rule surface, including lower-stratum negated body atoms.

Correctness baseline:

- `semi-naive-scc` is the production correctness baseline for recursive
  aggregate workloads in the current tree.
- `naive` / `naive-scc` remain useful diagnostic modes, but mixed recursive
  aggregate shapes may materialize proof multiplicity on non-aggregate heads
  and are therefore not treated as the exactness oracle for seminaive result
  parity.
- `deduce/analyze` now reports
  `naive-recursive-aggregate-mixed-routed-to-seminaive` when an explicit
  naive request targets a recursive SCC that mixes aggregate and
  non-aggregate rules, so the exact seminaive lane remains the truth source
  for that shape.
- `deduce/explain` mirrors the same routing truth in its optional
  `(deduce/explain db selector engine)` form:
  mixed recursive aggregate selectors requested as `naive` report actual
  `semi-naive-scc`, while pure positive and pure negated recursive aggregate
  selectors stay on `naive-scc` and leave the mixed-route diagnostic unset.
- `deduce/explain` now also exposes planner-only goal-directed eligibility
  metadata for the selected rule:
  - `goal-directed-component-id`
  - `goal-directed-eligible`
  - `goal-directed-shape`
  - `goal-directed-blockers`
- `deduce/explain` now also mirrors selector-scoped analyze closure metadata
  for eligible recursive selectors:
  - `goal-directed-execution-path`
  - `goal-directed-selected-components`
  - `goal-directed-selected-predicates`
  For aggregate-bearing and negated recursive selectors those closure fields
  stay `nil` while blocker reporting remains explicit.
- `deduce/explain` stays a planner snapshot even when a runtime read has
  already happened:
  - `surface-kind = 'planner-snapshot`
  - `goal-directed-execution-path` remains the planner-side selected-closure
    classification for the chosen rule
  - `last-goal-directed-read-*` mirrors the last actual runtime read path for
    the selected head relation
  - `steps[*].counters.counter-kind = 'observed` reflects runtime-observed
    counters attached to that planner snapshot rather than schema estimates
- `deduce/explain` now also mirrors the last actual goal-directed read
  metadata for the selected head relation, including:
  - `last-goal-directed-read-execution-path`
  - `last-goal-directed-read-surface`
  - `last-goal-directed-read-fallback-reason`
  - `last-goal-directed-read-selector-rule-index`
  - `last-goal-directed-read-mutation-epoch`
  - `last-goal-directed-read-requested-bound-count`
  - `last-goal-directed-read-requested-bound-positions`
  - `last-goal-directed-read-applied-bound-count`
  - `last-goal-directed-read-applied-bound-positions`
- this current `B6.4` surface is still intentionally narrow:
  - it does not rewrite rules,
  - it does not alter `deduce/query`,
  - it only mirrors the already-shipped selector-scoped analyze closure,
  - it does not introduce magic-set rewrite semantics

Termination:

- semi-naive recursive strata converge when the delta relation for all
  predicates is empty,
- grouped aggregate fixpoint convergence is reached when no recursive
  aggregate head relation changes between seminaive SCC passes.

## 6. Negation

Negation is allowed only in stratified programs.

Rules:

- dependency graph includes positive and negative edges,
- negative cycles are rejected at validation time,
- each stratum executes after all lower strata dependencies are complete.

Rejected shape example:

```lisp
; invalid: negative cycle
(deduce/rule! (p ?x) (not (q ?x)))
(deduce/rule! (q ?x) (not (p ?x)))
```

## 7. Index and Access Semantics

Planner/executor must prefer bound-argument index access over full scans.

Execution contract:

- analyze bound-variable mask per atom,
- choose index-compatible cursor when available,
- materialize dict rows only at projection/output boundary unless required by
  API shape.

Current implementation status note:

- Deduce now persists logical index descriptors per relation schema and records
  per-body-atom bound masks plus selected logical index ids at `deduce/rule!`
  registration time.
- Deduce now builds a deterministic conjunctive-body physical plan at
  `deduce/rule!` install time, including explicit operator classification
  (`FULL_SCAN`/`INDEX_SCAN`, plus negated variants) and stable join-order
  selection.
- Planner install-time metadata now includes explicit filter/projection pushdown
  decisions per planned body step (`body_filter_pushdown`,
  `body_projection_masks`) so runtime/explain surfaces can consume consistent
  pushdown intent.
- Relation schema metadata now tracks baseline `cardinality_estimate` and
  `distinct_estimate`, and planner scoring consumes these estimates for
  tie-breaking/selectivity hints.
- Join execution helpers now include:
  - `deduce_index_nested_loop_join_count(...)` for bound/index-probe driven joins.
  - `deduce_hash_join_count(...)` for in-memory key-frequency probes, including
    duplicate-match counting and optional static-literal filtering on inner rows.
  - `deduce_adaptive_join_count(...)` for threshold-based runtime fallback from
    index-probe strategy to hash strategy when probe miss rates exceed policy.
- Operator-level join execution stats (`DeduceJoinExecStats`) now capture
  rows/probes/emission and index hit/miss counts for runtime instrumentation.
- `deduce/explain` step payload now surfaces operator counters in a deterministic
  `counters` dictionary (`rows-read`, `rows-emitted`, `index-hits`,
  `index-misses`, `counter-kind`), currently driven by planner estimate data.
- `deduce/analyze` now exposes a deterministic SCC-fixpoint execution surface:
  - `(deduce/analyze db)` and `(deduce 'analyze db)` execute installed rules
    to fixpoint over current relation state,
  - optional engine selector for recursive SCC evaluation:
    - `'semi-naive` / `'semi-naive-scc` (default),
    - `'naive` / `'naive-scc` (reference parity mode),
  - output payload includes stable run metadata (`kind`, `status`, `mode`,
    `execution-engine`, `version`, `rule-count`, `strata-count`,
    `stratum-count`,
    `recursive-strata`, `iteration-limit`, `max-component-iterations`,
    `iterations`, `derived-facts`,
    `incremental-dependency-edges`,
    `incremental-dirty-predicate-count`,
    `incremental-invalidation-mode`),
  - migration note: `mode` remains `naive-bottom-up`; engine identity is
    carried by `execution-engine` (`semi-naive-scc`).
  - execution now isolates recursive SCCs: non-recursive strata run once in
    dependency order, recursive strata run semi-naive delta fixpoint loops
    until stable (or hard-stop iteration guard).
  - validation/runtime scheduling now share one dependency-graph model so
    negative-cycle rejection and SCC ordering derive from the same edges.
  - scheduler contract: SCC components are ordered by computed stratification
    level (`stratum-count`) where positive edges allow same-stratum ordering and
    negated edges enforce strictly higher dependent strata.
  - hard-stop guard: on recursive iteration limit hit, `deduce/analyze` raises
    `deduce/analyze-iteration-limit` with payload data including
    `component-id` and `iteration-limit`.
  - incremental diagnostics are reported pre-run; successful analyze currently
    resets dirty tracking state because evaluator execution remains full
    fixpoint recompute.
  - transaction safety: write-block (`deduce 'block ... 'write`) mutations are
    now logged per relation and applied to incremental invalidation + schema
    estimate state only on successful `deduce 'commit`; `deduce 'abort` drops
    pending mutation logs.
  - committed non-destructive base writes stay on
    `incremental-invalidation-mode = 'tracked`, while committed destructive
    writes such as `retract!` are where the current surface escalates to
    `full-recompute`.
  - plain DB-level `deduce/analyze` is also the current degraded-state
    recovery boundary: its returned payload still reports the observed
    `full-recompute` frontier, but a successful analyze run clears the live
    dirty/admin state afterward because the evaluator execution was a full DB
    fixpoint.
  - explicit deferred write-blocks (`deduce 'block ... 'write-deferred`)
    postpone the current `fact!`-side key/unique/reference checks until
    `deduce 'commit`, validating touched inserted relations and final
    delete-side reference integrity against the transaction-visible snapshot
    before LMDB commit.
  - integrity enforcement is no longer purely extensional:
    `deduce/rule!` now allows declared `key` / `unique` / `ref` constrained head
    relations across both ordinary and aggregate rule heads, and the derived
    write path enforces the same key/unique checks during naive and seminaive
    materialization while deferring `ref` validation to final component
    snapshot validation before commit.
  - `ref` targets are now candidate-key references rather than loose existence
    checks: the referenced column must resolve to a declared `key` or
    single-column `unique`, and schema/analyze expose that through
    `target-unique`.
  - dropped target relations no longer appear resolved in schema/analyze:
    reference metadata now exposes `target-live`, and unresolved counts include
    refs whose target relation has been dropped.
  - delete-side reference protection is eager on `retract!`, `clear!`, and
    `drop!`: deleting a referenced target tuple raises
    `deduce/integrity-reference-target-in-use`, while the same target delete is
    allowed inside a write transaction after the blocking referencing tuple has
    already been retracted in that transaction-visible snapshot.
  - `deduce/stats` and `deduce/analyze` now both expose bounded
    `recent-integrity-violations` history, newest first, alongside their
    existing integrity counters and last-violation fields.
