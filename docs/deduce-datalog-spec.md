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

### 7.1 Incremental-State Truth Table

| State | Current behavior | Status | Source anchors |
|---|---|---|---|
| Mutation log capture | Write-block mutations are recorded per relation and only applied to invalidation/schema-estimate state on successful commit. `deduce 'abort` drops pending logs. | `implemented` | `src/lisp/deduce_db_handles_mutation.c3`, `src/lisp/deduce_db_handles_mutation_txn.c3` |
| Diagnostic dirty tracking | `deduce/analyze` reports incremental diagnostics before execution and currently resets dirty tracking after success. | `implemented` | `src/lisp/deduce_rule_eval_exec.c3`, `src/lisp/deduce_rule_eval_fixpoint.c3` |
| Dependency graph recording | Incremental dependency-edge metadata is tracked and exposed, but it is still a diagnostic input rather than a full maintenance engine. | `partial` | `src/lisp/deduce_db_handles_mutation_tracking.c3`, `src/lisp/deduce_rule_eval_exec_seminaive.c3` |
| Full recompute fallback | Successful analyze still executes the full fixpoint path today. | `implemented` | `src/lisp/deduce_rule_eval_fixpoint.c3` |
| True incremental derived-state maintenance | Delta propagation and selective invalidation are not yet the production path. | `deferred` | `docs/plans/deduce-incremental-delta-propagation-design-2026-03-13.md` |

### 7.2 Materialized View Contract

Current shipped slice:

- explicit opt-in only:
  - `(deduce 'materialize! relation)`
  - `(deduce/materialize! relation)`
- equivalent declaration syntax now ships for declaration-time materialization:
  - `[relation db materialized] rel (col ...) ...`
  - `[relation db materialized manual] rel (col ...) ...`
- declaration-time refresh policy currently ships with two accepted
  contracts:
  - `[relation db materialized]` is shorthand for
    `[relation db materialized manual]`
  - `manual` keeps explicit refresh only
  - `[relation db materialized on-read] rel ...` triggers refresh before
    ordinary stored-tuple reads when the relation is stale and currently
    refreshable
  - after file-backed reopen, stale `on-read` relations now auto-refresh
    again when their persisted executable signatures are restorable on the
    current supported surface
  - unsupported persisted signature shapes still fail those reads truthfully
    instead of pretending stale persisted snapshots are fresh
  - later approved but still unshipped trigger-shaped spellings are:
    `on-base-commit`, `on-open`, and `scheduled`
- unknown declaration-time refresh policies reject deterministically with a
  parser error instead of being silently treated as generic relation attrs
- relation must be derived; extensional-only relations reject materialization
  with `deduce/materialize-relation-not-derived`
- declaration-based materialization can be installed before rules exist, but
  neither manual refresh nor `on-read` maintenance is available until the
  relation actually has derived rule heads
- manual refresh only:
  - `(deduce 'refresh! db)`
  - `(deduce/refresh! db)`
  - `(deduce 'refresh! materialized-relation)`
  - `(deduce/refresh! materialized-relation)`
- manual dematerialization is also available:
  - `(deduce 'dematerialize! materialized-relation)`
  - `(deduce/dematerialize! materialized-relation)`
- relation-handle refresh rejects non-materialized relations with
  `deduce/refresh-relation-not-materialized`
- relation-handle dematerialize rejects non-materialized relations with
  `deduce/dematerialize-relation-not-materialized`
- relation-handle refresh rejects declared-but-unready materialized relations
  with `deduce/refresh-materialized-relation-not-derived`
- DB-handle refresh still reuses the existing analyze/fixpoint execution path
  and then records freshness metadata only for actually ready materialized
  relations
- relation-handle refresh now executes the target relation’s dependency
  closure in tracked mode and only clears stale state for the materialized
  views it actually recomputed
- materialized intent and refresh-history metadata now survive reopen /
  `open-named` on file-backed DBs, and the current supported persisted
  executable-signature slice also restores truthful ready/fresh behavior for
  reopened derived materialized relations
- the last stored materialized snapshot also remains in relation storage after
  reopen, so ordinary storage-backed reads such as `deduce/count` can still
  observe that persisted snapshot across restart
- reopened DBs now retain a compact persisted rule/dependency catalog summary
  for admin truth:
  - reopened `deduce/analyze` retains the last installed `rule-count`
  - reopened `deduce/analyze` retains the last installed
    `incremental-dependency-edges`
- supported persisted executable rule signatures now survive reopen through
  stable catalog-name remapping plus inferred predicate-schema restore, so
  relation refresh and stale `on-read` maintenance can execute again without
  reinstalling those current supported rules
- unsupported persisted signature shapes still fall back to summary-only
  admin truth and do not claim live execution support after reopen
- reopened schema/admin payloads now also retain:
  - `materialized-refresh-count`
  - `materialized-last-refresh-mutation-epoch`
- explicit dematerialization clears that persisted materialized lifecycle
  record again without dropping relation data or installed rules
- materialized stale reasons now distinguish `never-refreshed` from
  mutation-driven reasons, so declared or reopened materialized views with
  no successful refresh no longer report a blank stale reason
- installing new rules now invalidates already refreshed materialized views
  with the stale reason `rule-set-change` and forces the existing
  `full-recompute-required` fallback, rather than pretending the old targeted
  invalidation state is still sound after the rule graph changed
- if the DB is already in `full-recompute-required`, relation-handle refresh
  falls back to the DB-wide path instead of pretending selective maintenance
  is sound
- `deduce/refresh!` now makes that distinction explicit in its payload:
  - relation-scoped targeted refresh reports
    `refresh-scope = 'relation`,
    `refresh-execution-path = 'targeted`,
    and `refresh-fallback-reason = nil`
  - DB-scoped refresh reports
    `refresh-scope = 'database`,
    `refresh-execution-path = 'db-wide`,
    and `refresh-fallback-reason = nil`
  - relation-scoped refresh that escalates because
    `full-recompute-required == true` reports
    `refresh-scope = 'relation`,
    `refresh-execution-path = 'db-wide`,
    and `refresh-fallback-reason = 'full-recompute-required`
- `deduce/refresh!` now also reports refresh impact explicitly:
  - `refreshed-materialized-relations`
  - `remaining-stale-materialized-relations`
  - relation-scoped calls additionally preserve
    `requested-refresh-relation`
  This keeps relation-scoped fallback-to-DB-wide refresh truthful about all
  views it actually refreshed, not only the originally requested one.
- the invalidation frontier is now explicit instead of only count-shaped:
  - `deduce/analyze` exposes `incremental-dirty-predicates`
  - `deduce/stats` exposes `dirty-predicates` and `dirty-self`
  - `deduce/refresh!` exposes:
    - `cleared-dirty-predicates`
    - `remaining-dirty-predicates`
  Targeted relation refresh now reports exactly which predicates it cleared
  while leaving unrelated dirty predicates behind.
  Relation-local `deduce/stats` still reports the DB-global dirty frontier in
  `dirty-predicate-count` / `dirty-predicates`; `dirty-self` is the local bit
  that tells whether the requested relation is still part of that frontier.
  If a plain DB-level `deduce/analyze` has already cleared a prior
  `full-recompute` degraded mode, a later relation-scoped `deduce/refresh!`
  reports the recovered live state truthfully: it stays on the targeted path,
  while `remaining-stale-materialized-relations` and later `deduce/analyze`
  still report untouched stale peers.
- successful `deduce/analyze` remains diagnostic-only for this lane:
  - it reports `materialized-view-count`,
    `stale-materialized-view-count`, and
    `refreshed-materialized-view-count`
  - it does not implicitly refresh materialized relations
- explicit dematerialization resets the current lifecycle metadata surface back
  to the non-materialized state:
  - `materialized-view = nil`
  - `materialized-refresh-policy = nil`
  - `materialized-refresh-count = 0`
  - `materialized-last-refresh-mutation-epoch = nil`
  - `materialized-last-stale-mutation-epoch = nil`
  - `materialized-last-stale-reason = nil`
  - `materialized-stale = nil`

Current admin metadata surface:

- the canonical field baseline for relation-local `deduce/stats`,
  DB-level `deduce/analyze`, and `deduce/refresh!` now lives in
  [7.3 Maintenance Analytics Payload Baseline](#73-maintenance-analytics-payload-baseline)
  and should be the single source of truth for maintenance-mode analytics
  fields and failure payload keys
- `deduce/schema` overlaps with that maintenance surface, but it still remains
  the schema-oriented home for fields such as `materialized-refresh-policy`
  that are not currently part of the `deduce/stats` payload
- the current first slice still uses `manual` refresh policy only
- dependency-driven invalidation still records the stale reason
  `dependency-dirty`
- materialized views that have never successfully refreshed still report the
  stale reason `never-refreshed`
- rule-graph changes still report the stale reason `rule-set-change`
- any committed base mutation that dirties the dependency path still makes the
  materialized relation stale until the next manual refresh, and a successful
  refresh clears the stale reason back to `nil`
- DB-wide refresh can still leave stale materialized declarations behind when
  they are not yet ready; that remaining stale count is reported honestly

### 7.3 Maintenance Analytics Payload Baseline

This section is the canonical source of truth for the current maintenance-mode
analytics envelope. `docs/reference/08-libraries.md` should summarize this
surface and point here rather than duplicate field inventories.

#### 7.3.1 Relation-local `deduce/stats`

`deduce/stats` always returns a relation-local hashmap with:

| Category | Keys | Notes |
|---|---|---|
| Envelope | `kind`, `status`, `relation` | `kind = 'deduce-stats`, `status = 'ok` on success. |
| Cardinality estimates | `cardinality-estimate`, `distinct-estimate` | Current estimate-only counters, not persisted maintenance state. |
| Dirty frontier | `dirty-predicate-count`, `dirty-predicates`, `dirty-self`, `mutation-epoch`, `full-recompute-required` | Relation-local view over the DB dirty frontier. |
| Parallel recursive topology | `parallel-recursive-component-count`, `parallel-batched-component-count`, `parallel-batch-count`, `parallel-max-batch-size`, `parallel-batch-topology` | Mirrors the current DB-wide parallel SCC batch summary from `deduce/analyze`. |
| Parallel runtime truth | `parallel-runtime-mode` | Current public runtime truth for the parallel lane. The shipped value is `metadata-only`. |
| Integrity counters | `integrity-violation-count`, `key-integrity-violation-count`, `unique-integrity-violation-count`, `reference-integrity-violation-count`, `check-integrity-violation-count`, `last-integrity-violation-code`, `recent-integrity-violations` | Bounded violation history remains newest first. |
| Materialized lifecycle | `materialized-view`, `materialized-derived-ready`, `materialized-refresh-policy`, `materialized-refresh-count`, `materialized-last-refresh-mutation-epoch`, `materialized-last-stale-mutation-epoch`, `materialized-last-stale-reason`, `materialized-stale` | `deduce/stats` now mirrors the current configured declaration-time policy as `materialized-refresh-policy`, matching `deduce/schema` for relation-local admin truth. |
| Last goal-directed read | `last-goal-directed-read-execution-path`, `last-goal-directed-read-surface`, `last-goal-directed-read-fallback-reason`, `last-goal-directed-read-selector-rule-index`, `last-goal-directed-read-mutation-epoch`, `last-goal-directed-read-requested-bound-count`, `last-goal-directed-read-requested-bound-positions`, `last-goal-directed-read-applied-bound-count`, `last-goal-directed-read-applied-bound-positions`, `last-goal-directed-read-step-counters` | Describes the last actual runtime read path used for this relation, not the current `deduce/stats` call. |
| Handle lifecycle | `dropped` | Reports whether the current relation handle has been dropped. |

`last-goal-directed-read-execution-path` currently uses symbols such as:

- `no-op`
- `selected-component-closure`
- `full-db-fixpoint`
- `ephemeral-head-demand-match`
- `ephemeral-head-demand-query`
- `ephemeral-head-demand-scan-range`

`last-goal-directed-read-fallback-reason` is `nil` on the ordinary path and
currently uses `full-recompute-required` when a dirty read escalates to the
full DB path.

The current parallel-recursion scheduling contract is metadata-only and
deterministic:

- only recursive SCC components participate in the parallel batch summary
- batching is computed inside each recursive stratum only
- `wave = 1` means the recursive component has no same-stratum recursive
  dependency in the current SCC graph
- higher waves are one-based longest same-stratum dependency distance from
  that dependency-free boundary
- components with the same `(stratum, wave)` may batch together
- this metadata does not imply the runtime already executes those batches in
  parallel; it is the current scheduling contract for admin/explain truth
- `parallel-runtime-mode = 'metadata-only` is the explicit public runtime
  truth for this lane today
- current selector-scoped `deduce/explain` therefore still reports the
  ordinary `execution-engine = 'semi-naive-scc`,
  `goal-directed-execution-path = 'selected-component-closure`, and
  `parallel-runtime-mode = 'metadata-only` alongside the parallel batch
  metadata, rather than claiming a separate parallel runtime
- the first shipped runtime seam under that metadata is still internal-only:
  a versioned serialized component-delta payload for one SCC component's
  signed deltas, intended for scheduler byte-result handoff
- the next shipped internal slice is a single read-only seminaive scratch
  pass for positive non-aggregate recursive components:
  it evaluates one pass against the current component snapshot and returns
  serialized candidate additions without LMDB publish
- the next shipped internal closure slice now iterates that scratch path to a
  fixpoint for positive non-aggregate recursive SCC rules, including
  multi-atom recursive rule shapes via LMDB plus prior-iteration worker-visible
  overlay reads on non-anchor recursive atoms
- the next shipped internal publish slice now applies those serialized
  worker-computed component deltas on the main thread through the existing
  relation-integrity and LMDB write path
- broader worker-side recursive batch compute remains follow-up work

#### 7.3.2 DB-level `deduce/analyze`

`deduce/analyze` remains a DB-handle diagnostic surface. The maintenance-mode
baseline within its broader payload is:

| Category | Keys | Notes |
|---|---|---|
| Dirty frontier | `incremental-dependency-edges`, `incremental-dirty-predicate-count`, `incremental-dirty-predicates`, `incremental-invalidation-mode` | `incremental-invalidation-mode` is currently `tracked` or `full-recompute`. |
| Parallel recursive topology | `parallel-recursive-component-count`, `parallel-batched-component-count`, `parallel-batch-count`, `parallel-max-batch-size`, `parallel-batch-topology` | DB-wide SCC batch summary. |
| Parallel runtime truth | `parallel-runtime-mode` | Current public runtime truth for the parallel lane. The shipped value is `metadata-only`. |
| Materialized lifecycle summary | `materialized-view-count`, `ready-materialized-view-count`, `unready-materialized-view-count`, `stale-materialized-view-count`, `ready-materialized-relations`, `unready-materialized-relations`, `stale-materialized-relations`, `refreshed-materialized-view-count` | Plain `deduce/analyze` is diagnostic only, so `refreshed-materialized-view-count` remains `0` there. |
| Integrity summary | `integrity-violation-count`, `key-integrity-violation-count`, `unique-integrity-violation-count`, `reference-integrity-violation-count`, `check-integrity-violation-count`, `last-integrity-violation-code`, `last-integrity-violation-relation`, `recent-integrity-violations` | DB-wide aggregate of the relation-local integrity surfaces. |
| Goal-directed planner summary | `goal-directed-recursive-component-count`, `goal-directed-eligible-component-count`, `goal-directed-blocked-component-count`, `goal-directed-aggregate-blocked-component-count`, `goal-directed-negated-blocked-component-count`, `goal-directed-components` | Planner/eligibility metadata only; does not imply rewrite semantics. |
| Selector-scoped analyze extras | `goal-directed-selected-components`, `goal-directed-selected-predicates`, `goal-directed-execution-path`, `goal-directed-selector-rule-index`, `goal-directed-selected-component-id`, `goal-directed-demand-bound-head-positions` | Present when analyze is run in selector mode on an eligible recursive component. |
| Last goal-directed read | `last-goal-directed-read-execution-path`, `last-goal-directed-read-surface`, `last-goal-directed-read-fallback-reason`, `last-goal-directed-read-relation`, `last-goal-directed-read-selector-rule-index`, `last-goal-directed-read-mutation-epoch`, `last-goal-directed-read-requested-bound-count`, `last-goal-directed-read-requested-bound-positions`, `last-goal-directed-read-applied-bound-count`, `last-goal-directed-read-applied-bound-positions` | Reports the last actual DB-level read choice, separate from analyze's own `goal-directed-execution-path`. |

Selector-scoped `deduce/analyze` remains planner-side at the top level even
after a runtime `query` or `match` has happened:

- `goal-directed-execution-path = 'selected-component-closure` and the other
  `goal-directed-*` selector fields classify the selected planner closure
- `last-goal-directed-read-*` mirrors the last actual DB-level read choice
- `rule-execution[*].steps[*].counters.counter-kind = 'observed` reports
  runtime-observed counters attached to each analyzed rule entry

For the current parallel topology fields, `parallel-batch-topology[*]` entries
are leader entries for recursive batches, with:

- `stratum` and `wave` describing the deterministic scheduling group
- `batch-size` reporting how many recursive components share that group
- `component-predicates` describing the leader component only, not the full
  future runtime batch payload
- these fields remain topology metadata only; they do not replace the normal
  `execution-engine` / `goal-directed-execution-path` truth for the current
  runtime
- `parallel-runtime-mode = 'metadata-only` is the current explicit public
  runtime truth even though internal worker-scratch/apply seams now exist

#### 7.3.3 `deduce/refresh!` payload contract

`deduce/refresh!` keeps one naming scheme across both accepted handle shapes:

| Scope | Baseline payload shape | Required refresh keys |
|---|---|---|
| Relation handle, targeted path | relation-local `deduce/stats` envelope for the requested relation | `kind = 'deduce-refresh`, `refresh-policy = 'manual`, `refresh-scope = 'relation`, `refresh-execution-path = 'targeted`, `refresh-fallback-reason = nil`, `requested-refresh-relation`, `refreshed-relation`, `refreshed-materialized-view-count`, `refreshed-materialized-relations`, `cleared-dirty-predicates`, `remaining-dirty-predicates`, `remaining-stale-materialized-relations` |
| Relation handle, fallback DB-wide path | relation-local `deduce/stats` envelope for the requested relation | same keys as above, but `refresh-execution-path = 'db-wide` and `refresh-fallback-reason = 'full-recompute-required`; the relation-scoped call stays truthful about the DB-wide work it actually ran |
| DB handle | DB-level `deduce/analyze` envelope | `kind = 'deduce-refresh`, `refresh-policy = 'manual`, `refresh-scope = 'database`, `refresh-execution-path = 'db-wide`, `refresh-fallback-reason = nil`, `refreshed-materialized-view-count`, `refreshed-materialized-relations`, `cleared-dirty-predicates`, `remaining-dirty-predicates`, `remaining-stale-materialized-relations`; the DB-wide result also keeps `stale-materialized-view-count` honest after the refresh |

`deduce/refresh!` is explicit refresh only. Plain `deduce/analyze` never
mutates refresh history or materialized freshness state.

#### 7.3.4 Failure payload keys and rejection surface

Maintenance-mode rejections are code-first. Unless explicitly listed below,
the current shipped surface returns the error code plus message only and does
not attach a structured data payload.

| Rejection code | Trigger | Structured data keys |
|---|---|---|
| `deduce/analyze-goal-directed-selector-not-eligible` | Selector-scoped analyze targets a recursive component that is not goal-directed eligible | `rule-index`, `component-id`, `goal-directed-blockers` |
| `deduce/refresh-relation-dropped` | Relation-scoped refresh targets a dropped relation handle | none |
| `deduce/refresh-relation-not-materialized` | Relation-scoped refresh on a non-materialized relation | none |
| `deduce/refresh-materialized-relation-not-derived` | Relation-scoped refresh on a declared-but-unready materialized relation | none |
| `deduce/refresh-relation-not-found` | Relation handle no longer has a registered schema | none |
| `deduce/refresh-db-not-open` | Refresh target DB is closed | none |
| `deduce/refresh-handle-not-deduce-db-or-relation` | Refresh target is neither a Deduce DB nor relation handle | none |
| `deduce/materialize-relation-not-derived` | Materialize request targets an extensional-only relation | none |
| `deduce/dematerialize-relation-not-materialized` | Dematerialize request targets a non-materialized relation | none |

Future maintenance/admin work should extend this table first before adding new
analytics or structured rejection payloads elsewhere.

Dropped relation handles are intentionally split:

- `deduce/stats` still returns a deterministic relation-local payload and marks
  `dropped = true`
- `deduce/refresh!` rejects that same handle with
  `deduce/refresh-relation-dropped`

#### 7.3.5 Incremental Delta Substrate Baseline

The current tree already has an internal signed-delta substrate for seminaive
recursive execution. It is not yet the public incremental-maintenance engine
for ordinary DB mutations, but it is the concrete runtime model that later
maintenance work should extend rather than replace.

Current internal data model:

| Runtime piece | Current shape | Notes |
|---|---|---|
| Encoded tuple unit | `DeduceEncodedTuple { key_bytes, key_len }` | Tuple keys are stored in encoded LMDB-key form so the same identity can be replayed or support-counted without rebuilding row dictionaries. |
| Unsigned delta bucket | `DeduceDeltaTupleSet { seen_map, tuples[], count, capacity }` | Stores one deduplicated tuple stream for a predicate/iteration lane. `seen_map` is the per-pass dedupe guard. |
| Signed delta bucket | `DeduceSignedDeltaSet { additions, removals }` | Every predicate can carry both add and remove deltas; later maintenance work should keep that signed shape instead of falling back to add-only naming. |
| Support accounting | `DeduceTupleSupportTable { base_counts, recursive_counts }` | Separate support counts for already-materialized/base support and recursive support. This is the current truthful substrate for supported tuple add/remove decisions in recursive seminaive execution. |
| Iteration buffers | `current_delta_sets[]`, `next_delta_sets[]` aligned to `plan.predicates` | Component-local ping-pong buffers across seminaive iterations. |
| Executor state | `DeduceSemiNaiveRuleExec` carries the signed delta arrays, support tables, proof dedupe map, and aggregate-component mode bits | The seminaive executor is already the owner of this substrate. |

Current boundary:

- this substrate is active inside recursive seminaive execution and the
  current recursive aggregate-support path
- it is not yet the ordinary post-commit derived maintenance engine for
  `deduce/fact!` / `deduce/retract!`
- later incremental-maintenance items should reuse this signed-delta /
  support-table model and then define the fallback/admin contract on top of it

Relevant implementation anchors:

- `src/lisp/deduce_rule_eval_exec.c3`
- `src/lisp/deduce_rule_eval_exec_seminaive.c3`
- `src/lisp/deduce_rule_eval_fixpoint.c3`

#### 7.3.6 Incremental Maintenance Fallback/Admin Boundary

Future true incremental maintenance must not invent a second public degraded
state model. The current maintenance/admin surfaces already have a truthful
fallback boundary, and later maintenance work should reuse it.

Approved boundary:

- ordinary success path:
  - maintenance may keep `incremental-invalidation-mode = 'tracked`
  - dirty frontier fields may shrink to the true residual set that still needs
    work
- degraded fallback path:
  - if incremental propagation cannot remain truthful, the DB must flip to the
    existing `full-recompute` / `full-recompute-required` boundary rather than
    exposing a new partial-success mode name
  - over-invalidating is allowed; under-invalidating is not
  - dirty/admin fields may preserve a conservative superset of affected
    predicates, but they must not imply a narrower truthful frontier than the
    engine actually has

Public/admin truth requirements under fallback:

| Surface | Required behavior |
|---|---|
| `deduce/analyze` | `incremental-invalidation-mode = 'full-recompute` and the current dirty/materialized summary stays truthful for the observed pre-recovery state |
| `deduce/stats` | `full-recompute-required = true` until a recovery path has actually cleared the live state |
| relation-scoped `deduce/refresh!` | may fall back to DB-wide execution, but must report `refresh-execution-path = 'db-wide` and `refresh-fallback-reason = 'full-recompute-required` when it does |
| selector-scoped goal-directed reads | keep rejecting on the existing `*-full-recompute-required` codes while the DB is degraded |
| plain DB-level recovery paths | may clear the live degraded state only by actually running the broader DB recovery work they report |

Not approved:

- new public invalidation-mode names such as `partial`, `degraded-tracked`,
  `pending`, or `mixed`
- silently mixing targeted incremental work with unreported DB-wide recovery
- admin payloads that report a post-recovery clean state before the live DB has
  actually been recovered

This boundary is intentionally conservative so `B6.8b*` can add real
maintained-update classes without reopening the public admin vocabulary.

#### 7.3.7 First Maintained-Update Classes: Direct-Copy Incremental Refresh

The first ordinary DB-mutation maintained-update classes are now shipped, but
they are intentionally narrow.

Supported shapes:

- target relation is materialized and currently ready
- target has already completed at least one truthful refresh
- target is derived by exactly one positive non-aggregate direct-copy rule
  whose head variables match the single body atom position-for-position
- only committed tuple insertions are being replayed
- supported source families:
  - one extensional-only source predicate
  - one already-refreshed ready materialized direct-copy source predicate

Current runtime contract:

- committed insertions on the extensional source are recorded as encoded tuple
  keys in a per-target pending insertion queue
- successful incremental refresh of a supported materialized source may enqueue
  the same inserted tuple keys for one-step downstream direct-copy materialized
  targets
- relation-scoped `deduce/refresh!` on the supported target may now report
  `refresh-execution-path = 'incremental-targeted`
- stale ordinary reads on `[relation db materialized on-read]` relations reuse
  that same incremental-targeted path when the target is supported and already
  refreshed
- the per-target queue is cleared only after a successful maintained refresh
- sibling materialized targets sourced from the same base predicate keep their
  own pending queue and remain stale until they are refreshed themselves
- one-step downstream materialized targets sourced from a refreshed
  materialized relation also remain stale until they are explicitly refreshed
  themselves, but may then use the same incremental-targeted path

Current fallback boundary:

- if the target has never been refreshed, refresh stays on the older
  dependency-closure `targeted` path
- derived-source incremental replay requires the immediate source materialized
  relation to be ready and not stale at refresh time
- unsupported rule shapes, derived sources, multi-rule heads, deletes, and
  broader multi-step recursive/aggregate maintenance stay outside these classes
- if pending insertion tracking cannot remain truthful, the DB escalates to the
  existing `full-recompute-required` boundary instead of silently dropping
  updates

### 7.4 Cleanup Verb Matrix

This section is the canonical naming and contract matrix for the current
write-side cleanup/admin verbs. The shipped surface intentionally keeps one
canonical cleanup verb per operation:

- tuple delete:
  - `(deduce 'retract! ...)`
  - `(deduce/retract! ...)`
- relation contents reset:
  - `(deduce 'clear! relation)`
  - `(deduce/clear! relation)`
- relation lifecycle removal:
  - `(deduce 'drop! relation)`
  - `(deduce/drop! relation)`

No additional cleanup aliases are approved in the current surface.

#### 7.4.1 Accepted call shapes and success contract

| Verb | Canonical accepted forms | Success result | Deterministic success semantics |
|---|---|---|---|
| `retract!` | `(deduce 'retract! relation val...)`, `(deduce/retract! relation val...)`, `(deduce 'retract! txn relation val...)`, `(deduce/retract! txn relation val...)` | `Void` | Exact tuple delete by encoded tuple key. Missing tuple is a no-op success. When a live delete occurs, relation cardinality estimates and DB mutation tracking are updated. |
| `clear!` | `(deduce 'clear! relation)`, `(deduce/clear! relation)` | `Void` | Removes all stored rows from the relation and its extensional shadow but preserves the relation schema/handle for future writes and reads. Clearing an already-empty relation is a no-op success. |
| `drop!` | `(deduce 'drop! relation)`, `(deduce/drop! relation)` | `Void` | Drops the relation storage, marks the registered schema and handle as dropped, persists the dropped lifecycle flag, and invalidates later read/write use of that handle. Re-dropping an already dropped handle is a no-op success. |

Current shipped boundary:

- `retract!` is the only cleanup verb that currently accepts an explicit Deduce
  transaction handle.
- `clear!` and `drop!` are currently relation-handle-only cleanup verbs. They
  open and commit their own write transaction internally.

#### 7.4.2 Cleanup rejection matrix

Unless explicitly listed below, the current shipped surface returns the error
code plus message only and does not attach a structured data payload.

| Verb | Rejection code | Trigger |
|---|---|---|
| `retract!` | `deduce/retract-expected-relation-values` | No relation argument provided in non-transaction form. |
| `retract!` | `deduce/retract-expected-transaction-relation-values` | Transaction form omits the relation argument. |
| `retract!` | `deduce/retract-transaction-handle-closed` | Transaction handle payload is already closed. |
| `retract!` | `deduce/retract-transaction-closed` | Transaction handle is not open or its DB is closed. |
| `retract!` | `deduce/retract-transaction-read-only` | Delete requested through a read-only transaction. |
| `retract!` | `deduce/retract-relation-not-handle` | Relation argument is not an FFI handle. |
| `retract!` | `deduce/retract-relation-handle-closed` | Relation handle payload is already closed. |
| `retract!` | `deduce/retract-relation-not-deduce-relation` | Handle is not a Deduce relation handle. |
| `retract!` | `deduce/retract-database-not-open` | Target relation DB is closed. |
| `retract!` | `deduce/retract-relation-dropped` | Target relation handle is already dropped. |
| `retract!` | `deduce/retract-transaction-database-mismatch` | Transaction DB does not match relation DB. |
| `retract!` | `deduce/retract-arity-mismatch` | Tuple arity does not match relation arity. |
| `retract!` | `deduce/retract-tuple-too-large` | Encoded tuple key exceeds the fixed tuple buffer. |
| `retract!` | `deduce/integrity-reference-target-in-use` | Delete would remove a referenced target tuple. |
| `retract!` | `deduce/retract-del-failed` | LMDB delete failed for a reason other than not-found. |
| `retract!` | `deduce/retract-shadow-del-failed` | Extensional shadow delete failed. |
| `retract!` | `deduce/retract-txn-open-failed` | Internal write transaction could not be opened. |
| `retract!` | `deduce/txn-failed` | Internal write transaction commit failed. |
| `clear!` | `deduce/clear-expected-relation` | Call shape is not exactly one relation handle argument. |
| `clear!` | `deduce/clear-relation-not-handle` | Argument is not an FFI handle. |
| `clear!` | `deduce/clear-relation-handle-closed` | Relation handle payload is already closed. |
| `clear!` | `deduce/clear-relation-not-deduce-relation` | Handle is not a Deduce relation handle. |
| `clear!` | `deduce/clear-database-not-open` | Target relation DB is closed. |
| `clear!` | `deduce/clear-relation-dropped` | Target relation handle is already dropped. |
| `clear!` | `deduce/integrity-reference-target-in-use` | Relation contents still contain referenced target tuples. |
| `clear!` | `deduce/clear-txn-open-failed` | Internal write transaction could not be opened. |
| `clear!` | `deduce/clear-failed` | LMDB clear of the main relation failed. |
| `clear!` | `deduce/clear-shadow-failed` | Extensional shadow clear failed. |
| `clear!` | `deduce/txn-failed` | Internal write transaction commit failed. |
| `drop!` | `deduce/drop-expected-relation` | Call shape is not exactly one relation handle argument. |
| `drop!` | `deduce/drop-relation-not-handle` | Argument is not an FFI handle. |
| `drop!` | `deduce/drop-relation-handle-closed` | Relation handle payload is already closed. |
| `drop!` | `deduce/drop-relation-not-deduce-relation` | Handle is not a Deduce relation handle. |
| `drop!` | `deduce/drop-database-not-open` | Target relation DB is closed. |
| `drop!` | `deduce/integrity-reference-target-in-use` | Drop would remove a referenced target relation or tuple set. |
| `drop!` | `deduce/drop-txn-open-failed` | Internal write transaction could not be opened. |
| `drop!` | `deduce/drop-failed` | LMDB drop of the main relation failed. |
| `drop!` | `deduce/drop-shadow-failed` | Extensional shadow drop failed. |
| `drop!` | `deduce/txn-failed` | Internal write transaction commit failed. |

The no-op cases above are intentional contract, not unspecified behavior:

- `retract!` on a tuple that is already absent still succeeds with `Void`
- `clear!` on an empty relation still succeeds with `Void`
- `drop!` on an already dropped relation handle still succeeds with `Void`

Query-demand widening note:

- the shipped `deduce/query` demand extractor now also folds closed
  one-argument pure closure wrappers such as `id` when the wrapper body still
  collapses to the supported equality-demand subset
- the shipped `deduce/query` demand extractor now also unwraps one-argument
  captured closure wrappers around the whole filter body when they forward the
  current `row` into a body that already fits that same equality-demand subset
- the shipped `deduce/query` demand extractor now also unwraps one-argument
  captured closure wrappers on the row-column side when they forward the
  current `row` into a body that already resolves to a supported
  `(ref row 'column)` shape
- the shipped `deduce/query` demand extractor now also follows short
  forwarding chains of those one-argument captured wrappers for both the
  whole-filter and row-column cases, but it still does not claim generic
  captured-call rewrite beyond that bounded forwarding shape
- the shipped `deduce/query` demand extractor now also unwraps preserved-row
  captured wrappers with extra closed literal arguments on both the
  whole-filter and row-column sides when the wrapper body still collapses to
  that same shipped subset
- for that preserved-row literal-arg subset, the forwarded `row` does not
  need to be the first call argument; any single forwarded row position is
  accepted as long as the remaining arguments are closed literal expressions
- the shipped `deduce/query` demand extractor also preserves that same
  bounded path across short forwarding chains of those preserved-row
  literal-arg wrappers, but it still does not claim generic captured-call
  rewrite beyond that bounded chain shape
- the shipped `deduce/query` demand extractor now also rewrites captured
  comparator wrappers when exactly one argument collapses to a row-derived
  scalar such as `(ref row 'dst)`, the remaining arguments are closed literal
  expressions, and the wrapper body still collapses to the supported equality
  subset
- that same scalar-comparator subset also follows short forwarding chains of
  captured wrappers, but broader symbolic solving and generic captured-call
  rewrite still fall back
- `deduce/query` now also ships the first broader query-time rewrite
  execution slice for the same recursive subset: a filter shaped as an `or`
  of individually demand-safe branches may execute each branch as its own
  abortable ephemeral head-demand run in a separate temporary write txn,
  then reapply the original full filter over the union of rows from those
  branch-local reads
- that disjunctive union path is still intentionally narrow:
  - every disjunct must fit the already shipped demand-safe subset
  - the currently shipped union contract now accepts same-position and
    mixed-position branches together, as long as each branch individually
    reduces to the already shipped demand-safe subset
  - the only remaining broader symbolic residual is recursive migration
    work; there is no separate non-recursive goal-directed symbolic disjunction
    lane beyond the already shipped subset
- on top of that, the same disjunctive union path now also accepts
  same-position wrapper branches where each disjunct reaches that same single
  projected head position through already shipped whole-filter or row-column
  wrapper forms
- that same same-position branch family also accepts branch-local residual
  unsupported conjuncts, because each branch still mines only its supported
  equality subset and the original full disjunctive filter is reapplied over
  the union after the branch-local reads
- mixed-position disjuncts now stay on the ephemeral union path too, because
  the runtime builds a union of the branch-local requested/applied positions
  and still reapplies the original full disjunctive filter over the union
  result
- recursive multi-position symbolic demands now ship a first bounded
  migration slice on recursive subjects too:
  - a single filter or disjunctive branch shaped like `(and (= src ...) (=
    dst ...))` may stay on `ephemeral-head-demand-query` when the projected
    recursive demand is relaxed to one applied position and the original full
    filter is reapplied over the rows
  - the current shipped relaxation keeps the one recursive carried head
    position preserved by the positive self-recursive rule, so admin truth may
    report `requested-bound-positions = (0 1)` with `applied-bound-positions`
    of `(1)` for the ordinary reach order or `(0)` for reordered head shapes
  - jointly-supported recursive permutation shapes such as
    `sym(x,y) :- sym(y,x)` no longer pretend the ephemeral demand path can
    answer fully-bound symbolic queries; they now fall back truthfully to
    `selected-component-closure` instead of reporting
    `ephemeral-head-demand-query` with incomplete or empty results
  - same-index self-recursive shapes such as
    `stable(x,y) :- stable(x,y)` now also keep jointly-supported fully-bound
    multi-position query demands on `ephemeral-head-demand-query`, with admin
    truth reporting both requested and applied positions as `(0 1)`
  - same-index mutual-recursive SCC shapes such as
    `a(x,y) :- b(x,y), b(x,y) :- a(x,y)` now also keep jointly-supported
    fully-bound multi-position query demands on `ephemeral-head-demand-query`,
    as long as each recursive rule in the selected SCC has some positive
    recursive body atom in that SCC preserving all requested positions
    together at the same indices
  - that same same-index mutual-recursive SCC family now also extends to
    disjunctive pair filters when each disjunct stays in the already shipped
    jointly-supported same-index subset, with admin truth still reporting
    requested and applied positions `(0 1)`
  - that same same-index SCC support is not limited to two-relation mutual
    recursion; multi-hop positive SCC cycles that preserve all requested
    positions together at the same indices also stay on
    `ephemeral-head-demand-query`
  - transformed recursive SCC shapes now ship a narrower follow-up too:
    when the queried head predicate has some same-index positive recursive
    carrier for one requested position, fully-bound pair demands may relax to
    that one carried applied position and still run on
    `ephemeral-head-demand-query`, with the original full filter reapplied
    over the rows
  - that transformed one-carried-position relaxation is an SCC-level family,
    not just a reordered two-relation mutual-recursion case; multi-hop
    transformed SCC cycles follow the same shipped rule
  - that same one-carried-position transformed relaxation also extends to
    disjunctive pair filters when each disjunct stays inside that same
    transformed recursive SCC subset
  - transformed recursive shapes where the uncarried demand is still
    distributed across multiple recursive body atoms now stay pinned on
    truthful `selected-component-closure` fallback instead of reporting
    `ephemeral-head-demand-query` with incomplete rows
  - recursive shapes where the requested positions are distributed across
    multiple recursive atoms, such as `path(x,z) :- path(x,y), path(y,z)`,
    now stay pinned on truthful `selected-component-closure` fallback for
    `deduce/query` instead of a misleading partial demand path
  - at this boundary, transformed recursive query-time behavior is fully
    described by the shipped support/fallback families above; there is no
    separate standing transformed-residual contract beyond them
- `deduce/match` now ships the first real query-time goal-directed execution
  slice for that same currently eligible recursive shape through:
  - `(deduce/match relation pattern rule-index)`
  - `(deduce/match relation pattern rule-index 'naive)`
  - `(deduce/match relation pattern rule-index 'semi-naive)`
  This executes only the selected component dependency closure before
  matching, requires that the selected closure actually produces the requested
  relation, and rejects aggregate-bearing / negated recursive selectors plus
  relation-mismatch selectors explicitly instead of silently widening to a
  full-DB run.
  A narrower demand-bound sub-slice is now also shipped for selector-scoped
  match when the pattern binds head literals and the selected positive
  recursive shape preserves those bound positions through its self-recursive
  body:
  - the demand-bound run executes in an abortable write txn,
  - if only some requested bound head positions are preserved, the runtime
    projects the demand down to that preserved subset and still applies the
    original full match pattern afterwards,
  - it restores in-memory schema estimates after abort,
  - it leaves the dirty frontier intact after the read,
  - each demanded head position must be preserved by every positive
    self-recursive body atom in the selected rule,
  - wider recursive shapes still fall back to the existing selected-component
    closure execution instead of pretending this demand path is generic.
- `deduce/query` now ships the matching relation-targeted goal-directed
  execution slice for that same currently eligible recursive shape through:
  - `(deduce/query relation filter-fn rule-index)`
  - `(deduce/query relation filter-fn rule-index 'naive)`
  - `(deduce/query relation filter-fn rule-index 'semi-naive)`
  This executes only the selected component dependency closure before
  filtering rows, requires that the selected closure actually produces the
  requested relation, and rejects aggregate-bearing / negated recursive
  selectors plus relation-mismatch selectors explicitly instead of silently
  widening to a full-DB run.
  For the currently shipped preserved-bound subset, the demand extractor now
  accepts row-independent constants, closed numeric builtin expressions such as
  `abs`, `floor`, `ceiling`, `round`, `truncate`, `sqrt`, `exp`, `log`,
  `log10`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`, `min`,
  `max`, and `pow`,
  closed row-independent comparison guards (`=`, `<`, `<=`, `>`, `>=`),
  closed row-independent `and` / `or` guards under Omni’s ordinary
  value-returning short-circuit semantics, closed row-independent `not`
  guards under ordinary Omni truthiness semantics,
  row-independent `let` / `block` / `if` wrappers, closed literal-side
  `let` / `block` wrappers around the supported equality subset including
  wrapped `ref` column symbols, closed row-column-side `let` / `block`
  wrappers that still resolve to `(ref row 'column)` through a closed symbol
  binding, closed row-independent `if` wrappers that select between
  row-column-side `(ref row 'column)` branches, closed row-independent
  `and` / `or` wrappers around the row-column side when the selected branch
  still resolves to `(ref row 'column)`, and row-independent `or` wrappers
  with one closed falsy branch around the supported equality subset.
  The last-read admin surface now also records requested vs applied preserved
  bound-counts for projected-demand reads through:
  - `last-goal-directed-read-requested-bound-count`
  - `last-goal-directed-read-requested-bound-positions`
  - `last-goal-directed-read-applied-bound-count`
  - `last-goal-directed-read-applied-bound-positions`
- `deduce/count`, `deduce/scan`, and `deduce/scan-range` now ship the same
  relation-targeted goal-directed execution slice for that currently eligible
  recursive shape through:
  - `(deduce/count relation rule-index)`
  - `(deduce/count relation rule-index 'naive)`
  - `(deduce/count relation rule-index 'semi-naive)`
  - `(deduce/scan relation rule-index)`
  - `(deduce/scan relation rule-index 'naive)`
  - `(deduce/scan relation rule-index 'semi-naive)`
  - `(deduce/scan-range relation lower upper rule-index)`
  - `(deduce/scan-range relation lower upper rule-index 'naive)`
  - `(deduce/scan-range relation lower upper rule-index 'semi-naive)`
  `deduce/query` and equality-bound `deduce/scan-range` also ship narrower
  demand-bound sub-slices on top of that selected-closure path:
  - `deduce/query` may now harvest the supported equality subset even when
    the full filter also contains residual unsupported conjuncts, because the
    original complete filter still runs after the read,
  - equality-filter `query` may project the requested head demand down to the
    preserved subset and still apply the original full filter closure after
    the read,
  - equality-bound `scan-range` may project the requested head demand down to
    the preserved subset and still apply the original full lower/upper bounds
    after the read,
  - unsupported shapes still fall back to the ordinary selected-component
    closure execution.
  - multi-self-recursive shapes only stay on this path when every positive
    self-recursive body atom preserves the demanded head positions; otherwise
    they fall back to `selected-component-closure`, clean the target
    component, and leave unrelated dirty siblings untouched.
  For the currently shipped preserved-bound subset, positive self-recursive
  body scans now also use demanded leading-prefix probes inside the naive and
  seminaive rule-step executors, so the abortable demand path narrows the
  recursive scan itself rather than only the outer selector plan.
  These execute only the selected component dependency closure before reading
  rows, require that the selected closure actually produces the requested
  relation, and reject aggregate-bearing / negated recursive selectors plus
  relation-mismatch selectors explicitly instead of silently widening to a
  full-DB run.
  Selector-scoped preserved-bound reads now also record
  `last-goal-directed-read-step-counters` in `deduce/stats`,
  `deduce/analyze`, and `deduce/explain`, exposing observed per-step counters
  such as `rows-read`, `rows-emitted`, and `join-probes` for the selected rule
  instead of only the coarse last-read path label.
  `deduce/scan-range` now also has a narrower bounded-demand sub-slice when
  equality-bound head literals can be extracted from positions where
  `lower == upper` and the selected positive recursive shape preserves those
  bound positions through its self-recursive body:
  - the demand-bound run executes in an abortable write txn,
  - it restores in-memory schema estimates after abort,
  - it leaves the dirty frontier intact after the read,
  - each demanded head position must be preserved by every positive
    self-recursive body atom in the selected rule,
  - wider recursive shapes still fall back to the existing selected-component
    closure execution path.
- plain relation reads without a selector now also auto-execute that same
  eligible dirty positive recursive closure in tracked mode for:
  - `(deduce/match relation pattern)`
  - `(deduce/query relation filter-fn)`
  - `(deduce/count relation)`
  - `(deduce/scan relation)`
  - `(deduce/scan-range relation lower upper)`
  This automatic path is intentionally narrow:
  - it only runs when the target relation belongs to an eligible positive
    recursive closure,
  - it only runs when at least one predicate in that selected closure is
    dirty,
  - it leaves unrelated recursive components untouched,
  - in tracked mode it executes only the target closure, but once the DB has
    escalated to `full-recompute-required` a plain read of a derived target
    relation falls back to the existing full DB fixpoint path,
  - and it does not widen support to aggregate-bearing or negated recursive
    shapes.
  Plain `deduce/match` now has one narrower exception inside that shipped
  surface: bound-literal patterns may use the abortable demand-bound path for
  the current preserved-bound positive recursive subset, restoring in-memory
  schema estimates after abort and leaving the target relation dirty.
  That preserved-bound subset now includes variable-preserving recursive body
  reordering, not only same-position carry-through, and each demanded head
  position still needs to be preserved by every positive
  self-recursive body atom in the rule.
  Plain `deduce/scan-range` now has the matching narrower exception when
  equality-bound head literals can be extracted from positions where
  `lower == upper` and the current positive recursive shape preserves those
  bound positions through its self-recursive body; that path also restores
  in-memory schema estimates after abort and leaves the target relation dirty.
  The same preserved-bound widening applies there too: variable-preserving
  recursive body reordering is now accepted, not only same-position
  carry-through, and each demanded head position still needs to be preserved
  by every positive self-recursive body atom in the rule.
  Plain and selector-scoped `deduce/query` now have the matching narrower
  demand-bound exception for a small safe filter subset: conjunctions of
  row-independent literal, captured-constant, or small safe builtin
  expression equalities on `(ref row 'column)` terms for the current
  preserved-bound positive recursive subset.
  Closed numeric builtin expressions like `(+ target 0)` are now part of
  that shipped subset, and row-independent `let` / `block` / `if` wrappers
  around the same equality subset are accepted too, while unsupported
  captured call shapes still fall back to selected-closure execution.

Still deferred:

- broader declaration-policy widening beyond the explicit current
  manual-and-`on-read` contract
- the next widened integrity class is fixed as canonical `check`; aliases
  such as `assert`, `predicate`, and `guard` are rejected for that lane
- unary column checks now have a shipped declaration/schema/admin and first
  write-enforcement baseline:
  - relation declarations accept `(check predicate column)`
  - `deduce/schema` exposes `kind = 'check`, `predicate`, `columns`, and
    `enforced = false`
  - `deduce/analyze` reports DB-wide `check-constraint-count`
  - immediate `fact!`, derived rule-head publish, and deferred
    `write-deferred` commit-time snapshot validation enforce declared checks
  - failure history now surfaces `violation-class = 'check` and deterministic
    codes for failed, missing, non-callable, and raised check predicates
- dedicated per-class admin counters for `check` remain follow-up work under
  `B6.10b2`
- optimizer rewrites that rely on materialization
- persisted rule/dependency catalogs and durable derived freshness semantics
  beyond the current persisted snapshot plus intent and refresh/stale-lifecycle
  catalog

Current durability note:

- file-backed materialized relations now persist both successful refresh
  history and later invalidation history
- file-backed materialized relations also keep the last stored snapshot rows in
  relation storage across reopen; current reopen/read semantics therefore
  preserve snapshot observability even while admin surfaces remain conservative
- a compact persisted rule/dependency catalog summary now survives reopen, so
  reopened `deduce/analyze` keeps the last installed `rule-count` and
  `incremental-dependency-edges`
- executable rule state still does not persist across reopen in the current
  shipped boundary, so refresh/read paths that require live rule execution
  still need explicit rule reinstall
- reopen / `open-named` therefore preserves:
  - `materialized-refresh-count`
  - `materialized-last-refresh-mutation-epoch`
  - `materialized-last-stale-reason`
  - `materialized-last-stale-mutation-epoch`
- reopened materialized relations remain stale while that persisted stale
  reason is non-`nil`; they do not silently become fresh just because the
  live dirty frontier starts empty

## 8. Explainability Contract

`deduce/explain` output should include at minimum:

- normalized logical query/rule form,
- chosen physical operators,
- join order,
- index access per atom,
- estimated vs observed row counts (when available).

Explain output must be deterministic for deterministic stats snapshots.

Current implementation status note:

- `deduce/explain` is now available on both canonical and dispatch surfaces:
  - `(deduce/explain db [head-symbol|rule-index])`
  - `(deduce 'explain db [head-symbol|rule-index])`
- current deterministic payload skeleton includes:
  - top-level: `kind`, `status`, `rule-index`, `rule-count`, `head-predicate`,
    `join-order`, `steps`
  - per-step (`steps` list): `position`, `predicate`, `negated`, `bound-mask`,
    `selected-index`, `operator`, `filter-pushdown`, `projection-mask`,
    `counters`
- per-step `counters` dictionary keys:
  - `rows-read`, `rows-emitted`, `index-hits`, `index-misses`, `join-probes`,
    `counter-kind`
  - current `counter-kind` is `observed`; values are sourced from runtime
    execution rather than schema estimates, while top-level
    `surface-kind = 'planner-snapshot` and `goal-directed-execution-path`
    remain planner-snapshot fields.
- `deduce/analyze` exposes the same runtime-observed step counters under
  top-level `rule-execution`, where each entry contains:
  - `rule-index`
  - `head-predicate`
  - `join-order`
  - `steps`
- operator values are explicit symbols derived from planned access mode:
  `full-scan`, `index-scan`, `negated-scan`, `negated-index-probe`.
- this v1 surface remains planner-focused in operator naming/order, while the
  attached counters now reflect runtime-observed execution.
- for current conjunctive rules, `deduce/explain.steps[*]` and
  `deduce/analyze.rule-execution[*].steps[*]` share the same planner-derived
  `join-order`, `predicate`, `operator`, and `selected-index` shape; runtime
  truth is carried by the observed counters rather than a separate reordered
  execution trace.
- the current counter surfaces are intentionally distinct:
  - `deduce/stats.last-goal-directed-read-step-counters` records the last
    actual preserved-bound runtime read for that relation
  - `deduce/analyze.rule-execution[*].steps[*].counters` records the observed
    counters from the `deduce/analyze` execution itself
  - `deduce/explain.steps[*].counters` records the observed counters from the
    `deduce/explain` execution itself
  These surfaces are truthful but not interchangeable, and their observed
  `rows-read` / `rows-emitted` / `join-probes` values may differ after the
  same earlier read.

## 8.1 Provenance / Why-Result Payload Shape

This is the canonical shape for future provenance-aware Deduce surfaces.
It is designed to answer "why did this row appear?" with a deterministic
derivation trace that matches the existing `kind`/`status`/payload style used
by `deduce/explain` and `deduce/analyze`.

Canonical surface names for this payload family:

- `deduce/why-result` for selected row or fact lineage
- provenance-aware `deduce/analyze` when a caller asks for derivation tracing

Current shipped public slice:

- `(deduce/why-result relation val...)` is public today for `subject-kind = row`
  on stored tuple subjects
- extensional stored rows return `status = ok` with one deterministic `seed`
  path and one `fact` support frame
- stored rows in relations with exactly one non-negated, non-aggregate,
  extensional rule also return `status = ok` when every support tuple is fully
  reconstructible from the head row
  - current supported shapes include direct-copy, one-body variable
    projection/permutation rules, and multi-body extensional rules whose body
    variables are all bound by the head row
  - the derived payload carries one deterministic `derived` path and a `fact`
    support frame for each reconstructed supporting row
- exact-one-rule extensional derived relations also now support search-based
  non-recursive lineage when some body variables are not bound by the head row
  - if the observed DB state yields one deterministic support path, the payload
    returns `status = ok`
  - if multiple support paths exist, the payload returns `status = partial`
    with `truncated = true` and the first deterministic path
- exact-one-rule non-recursive mixed-body lineage is also now supported when
  derived body predicates are themselves already provable by the shipped
  exact-one-rule provenance helper surface
  - unique mixed-body support chains return `status = ok`
  - multiple mixed-body support chains return `status = partial` with the
    first deterministic path
- multi-rule non-recursive lineage is also now supported when one or more
  matching rules are already provable by the shipped non-recursive why-result
  helper surface
  - if exactly one support path exists across those supported matching rules,
    the payload returns `status = ok`
  - if multiple support paths exist across those supported matching rules, the
    payload returns `status = partial` with `truncated = true` and the first
    deterministic path
- first positive recursive closure lineage is also now supported when the
  recursive support chain can be proven through the shipped row-subject helper
  surface without revisiting the same `(predicate, tuple)` subject
  - a single recursive support chain returns `status = ok`
  - multiple recursive support chains return `status = partial` with
    `truncated = true` and the first deterministic path
  - recursive closure payloads now also append a `rule-step` support frame for
    each derived child subject that was used in the chosen proof path
  - recursive closure `max-depth` now reflects that deeper derived step
- missing stored rows return `status = missing`
- broader stored rows in relations with derived-rule lineage currently return
  `status = error` with code
  `deduce/why-result-derived-subject-not-yet-supported`
- `why-result` now also exposes optional top-level
  `goal-directed-read-context` metadata when the relation has last-read
  goal-directed runtime state
  - this mirrors the existing `last-goal-directed-read-*` admin truth for the
    relation, not a proof-specific derivation step
  - it may appear on `ok`, `partial`, `missing`, or `error` payloads
- `why-result` now also attaches optional path-local
  `goal-directed-read-context` metadata on the matching proof path when the
  relation’s last goal-directed `deduce/query`, `deduce/match`,
  `deduce/scan`, or `deduce/scan-range` observed a bounded complete row set
  of at most `8` rows and the traced tuple belongs to that stored subject set
  - this is currently limited to bounded-complete goal-directed `query`,
    `match`, `scan`, and `scan-range` results with no more than `8`
    matched rows
  - for `query`, `match`, and `scan-range`, that bounded-complete path-local
    slice applies in both plain and selector-scoped goal-directed reads;
    selector-scoped payloads keep their concrete `selector-rule-index`
  - `scan` now also participates in that bounded-complete path-local slice for
    selector-scoped reads; those payloads keep their concrete
    `selector-rule-index`
  - plain no-op `query` and `scan-range` reads now also store that bounded
    complete row set, so matching root proof paths can carry the same
    path-local context even when the last read stayed on `no-op`
  - plain no-op `match` reads now also participate in that bounded root-path
    slice for matching rows
  - selector-scoped valid row reads now also keep truthful path-local context
    across the current shipped shapes:
    - selector-scoped `match` and `scan` can stay on `no-op` and still carry
      bounded path-local context with their concrete `selector-rule-index`
    - selector-scoped `query` and `scan-range` can stay on their shipped
      ephemeral demand paths and still carry bounded path-local context with
      their concrete `selector-rule-index`
  - matching derived support frames inside the chosen proof path now also
    carry that same path-local context when their `(relation, tuple)` pair
    matches a bounded-complete last-read subject set from the current shipped
    `query` / `scan` / `scan-range` slices
  - matching fact support frames inside the chosen proof path now also carry
    that same path-local context when their `(relation, tuple)` pair matches
    a bounded-complete last-read subject set
  - when the root tuple itself did not match but the support frames carrying
    bounded path-local context all come from the same relation-local last-read
    state, the proof path now also inherits that same
    `goal-directed-read-context`
  - when multiple support-frame relations contribute different bounded
    path-local contexts to the chosen proof path, the path now exposes
    `goal-directed-read-contexts` as a list of those distinct contexts
    instead of forcing a fake single merged context
  - other rows on the same relation keep only the top-level relation-read
    context
- broader proof-path-integrated goal-directed provenance beyond those
  bounded-complete root/fact-frame/rule-step row-matching slices and other
  broader subject kinds,
  remain roadmap work

Top-level payload:

- `kind`: `why-result`
- `status`: `ok`, `missing`, `partial`, or `error`
- `surface-kind`: `provenance-snapshot`
- `subject-kind`: `row`, `fact`, or `binding`
- `subject`: normalized selector data for the traced result
- `path-count`: number of derivation paths included
- `max-depth`: maximum path depth included in the payload
- `truncated`: whether the trace was truncated by depth or path cap
- `paths`: ordered list of derivation paths

Status contract:

- `status = ok`
  - the subject was found
  - at least one derivation path is present
  - `path-count > 0`
  - `paths` is non-empty
- `status = missing`
  - the subject was not found in the observed DB state
  - `path-count = 0`
  - `paths = []`
  - `truncated = false`
- `status = partial`
  - the subject was found, but the returned derivation payload is
    intentionally incomplete due to path cap, depth cap, or an explicitly
    unsupported support-frame class inside an otherwise valid result
  - `path-count > 0`
  - `paths` is non-empty
  - `truncated = true`
- `status = error`
  - the request could not produce a truthful provenance snapshot at all
  - `path-count = 0`
  - `paths = []`
  - `subject` may still be echoed when normalization succeeded before the
    error boundary

Envelope requirements:

- all four statuses keep the same top-level envelope keys:
  - `kind`
  - `status`
  - `surface-kind`
  - `subject-kind`
  - `subject`
  - `path-count`
  - `max-depth`
  - `truncated`
  - `paths`
- future public surfaces may add optional diagnostic keys, but the envelope
  above stays canonical
- `paths` ordering must be deterministic for a fixed DB state even when
  `status = partial`
- current optional diagnostic keys include:
  - `goal-directed-read-context`

Subject shapes:

- `subject-kind = row`
  - `relation`
  - `tuple`
  - `bindings` when the caller supplied a binding-oriented selector
- `subject-kind = fact`
  - `predicate`
  - `tuple`
  - `fact-index` when the fact is addressable by stored row index
- `subject-kind = binding`
  - `predicate`
  - `bindings`

Path shape:

- `path-id`: stable numeric id within the payload
- `path-kind`: `derived` or `seed`
- `head-predicate`: predicate that produced the traced result
- `head-tuple`: tuple at the head of the path
- `rule-index`: rule that produced the head tuple, when `path-kind = derived`
- `support`: ordered list of support frames from leaf facts to the head
- `truncated`: whether this individual path was depth-truncated
- current optional path-local diagnostic keys include:
  - `goal-directed-read-context` for the current exact-one goal-directed
    `query` / `match` / `scan` / `scan-range` slice

Support frame shape:

- `kind`: `fact` or `rule-step`
- `frame-index`: stable numeric id within the path
- `predicate`
- `tuple`
- `rule-index`: present for rule-step frames
- `step-index`: body step within the rule, when relevant
- `selected-index`: chosen access path for the step, when relevant
- `operator`: explicit operator symbol when the frame comes from a planned step
- `bound-mask`: bound-variable mask when the frame comes from a planned step
- `bindings`: the substitution visible at this frame, when available
- `depends-on`: optional list of earlier `frame-index` values that justify this
  frame

Example shape:

```lisp
{
  kind: why-result,
  status: ok,
  surface-kind: provenance-snapshot,
  subject-kind: row,
  subject: {
    relation: ancestor,
    tuple: [alice, bob]
  },
  path-count: 1,
  max-depth: 3,
  truncated: false,
  paths: [
    {
      path-id: 0,
      path-kind: derived,
      head-predicate: ancestor,
      head-tuple: [alice, bob],
      rule-index: 4,
      support: [
        {
          kind: fact,
          frame-index: 0,
          predicate: parent,
          tuple: [alice, carol]
        },
        {
          kind: fact,
          frame-index: 1,
          predicate: parent,
          tuple: [carol, bob]
        },
        {
          kind: rule-step,
          frame-index: 2,
          predicate: ancestor,
          rule-index: 4,
          step-index: 1,
          selected-index: 0,
          operator: index-scan,
          bound-mask: [true, false],
          bindings: {?x: alice, ?y: bob}
        }
      ],
      truncated: false
    }
  ]
}
```

Design constraints:

- path ordering must be deterministic for a fixed database state
- repeated derivations of the same result should be preserved as distinct paths
- support frames should be sufficient to reconstruct a human-readable proof
  without requiring planner internals
- no hidden counters or implicit joins should be needed to interpret the payload

## 9. Error and Rejection Contract

Program construction and query planning errors must use domain `deduce` with
stable codes, including:

- `deduce/arity-mismatch`
- `deduce/relation-not-found`
- `deduce/unsafe-rule`
- `deduce/non-stratifiable-program`
- `deduce/planner-no-valid-plan`

Operational DB failures continue using existing Deduce DB/txn error surfaces.

## 10. Testing Contract

Minimum regression matrix:

1. Rule validation:
- valid rule acceptance,
- unsafe variable rejection,
- arity mismatch rejection,
- non-stratifiable negation rejection.

2. Evaluator parity:
- naive vs semi-naive equality on recursive programs.

3. Planner behavior:
- selective predicates avoid full scans, broad scans stay full-scan, and
  skew-sensitive join orderings remain stable (assert through explain payloads).

4. Runtime semantics:
- query absence remains `nil`,
- mutation success remains `Void`.

## 11. Compatibility Contract

- existing `deduce` API command surfaces remain valid.
- internal engine changes are transparent unless explicitly version-gated.
- any syntax expansion for rules/queries must preserve existing programs or
  provide a deliberate migration path.
