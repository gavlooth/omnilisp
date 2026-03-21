# Deduce Datalog Spec (Draft v1)

Status: draft execution spec for robust Deduce roadmap.  
Last updated: 2026-03-20.

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
4. Stable compatibility behavior for existing `deduce/query` and
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
  parity, and compatibility semantics checks.
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

Compatibility policy:

- legacy/non-canonical spellings may remain temporarily, but canonical names
  above are the forward language-facing contract.
- new feature docs and benchmarks must use canonical names.

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
| Query execution | `implemented` | `deduce/query` and `deduce/match` are available as stable command surfaces. | `src/lisp/deduce.c3`, `src/lisp/deduce_relation_ops_query.c3`, `src/lisp/deduce_relation_ops.c3` |
| Planner-backed conjunctive execution | `partial` | Deterministic planning, bound-mask analysis, and operator classification exist, but execution/explain truthfulness still needs tightening. | `src/lisp/deduce_rule_ops_planning.c3`, `src/lisp/deduce_rule_eval_exec.c3`, `src/lisp/deduce_rule_ops_explain.c3` |
| Recursive evaluation | `implemented` | `semi-naive-scc` is the canonical production engine; `naive` and `naive-scc` remain reference/debug modes for explicit comparison and diagnostics. SCC planning and recursive fixpoint evaluation share one dependency graph. | `src/lisp/deduce_rule_eval_exec_naive.c3`, `src/lisp/deduce_rule_eval_exec_seminaive.c3`, `src/lisp/deduce_rule_eval_fixpoint.c3`, `src/lisp/deduce_rule_eval_scc.c3` |
| Stratified negation | `partial` | Negation safety and negative-cycle rejection exist, and the validation/runtime path now shares one dependency graph, but broader incremental maintenance remains a separate roadmap item. | `src/lisp/deduce_rule_eval_prims.c3`, `src/lisp/deduce_rule_eval.c3` |
| Explainability | `partial` | `deduce/explain` exists and emits deterministic payload skeletons, but some counters and fields are still planner-estimate driven. | `src/lisp/deduce_rule_ops_explain.c3` |
| Analyze/fixpoint diagnostics | `partial` | `deduce/analyze` exposes recursive SCC/fixpoint diagnostics, mutation metadata, and keyed/unique/reference integrity relation counts, but incremental maintenance is still diagnostic-first. | `src/lisp/deduce_rule_eval_exec.c3`, `src/lisp/deduce_rule_eval_exec_seminaive.c3`, `src/lisp/deduce_rule_eval_fixpoint.c3`, `src/lisp/deduce_rule_eval_prims.c3` |
| Mutation logging and dirty tracking | `partial` | Commit/abort logging and dirty-state reporting exist, but they do not yet form a full incremental derived-state engine. | `src/lisp/deduce_db_handles_mutation.c3`, `src/lisp/deduce_db_handles_mutation_tracking.c3`, `src/lisp/deduce_db_handles_mutation_txn.c3` |
| True incremental derived-state maintenance | `deferred` | The current tree still recomputes on successful analyze; delta propagation remains a roadmap item. | `docs/plans/deduce-incremental-delta-propagation-design-2026-03-13.md` |
| Maintenance/admin introspection | `implemented` | Read-only schema, index, and stats inspection are available on relation handles through `deduce/schema`, `deduce/indexes`, and `deduce/stats`, and schema payloads now include keyed/unique/reference integrity constraint metadata for declared single-column and composite integrity constraints. | `src/lisp/deduce_schema_query.c3`, `src/lisp/deduce.c3` |
| Aggregates, constraints, provenance, magic sets, parallel recursion | `partial` | Rule-head grouped aggregates now execute for `count`/`sum`/`min`/`max` in both non-recursive rules and aggregate-bearing recursive SCCs. Recursive aggregate SCCs now run on `semi-naive-scc` for the current stratified rule surface, including mixed aggregate/non-aggregate recursive SCCs and lower-stratum negated body atoms. The current constraint slice ships declared `key` relations, single/composite `unique` constraints, and single-column `ref` integrity constraints: conflicting or missing-target `fact!` writes are rejected with machine-checkable payloads and schema/analyze visibility. Provenance, richer integrity classes beyond key/unique/reference, magic sets, and parallel recursion remain roadmap work. | `src/lisp/deduce_rule_ops.c3`, `src/lisp/deduce_rule_eval.c3`, `src/lisp/deduce_rule_eval_exec.c3`, `src/lisp/deduce_rule_ops_explain.c3`, `src/lisp/deduce_relation_ops.c3`, `src/lisp/deduce_schema_query.c3`, `src/lisp/deduce_rule_eval_prims.c3`, `docs/plans/deduce-full-roadmap-2026-03-20.md` |

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
for compatibility, but logical semantics are:

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
  - compatibility note: `mode` remains `naive-bottom-up`; engine identity is
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

### 7.1 Incremental-State Truth Table

| State | Current behavior | Status | Source anchors |
|---|---|---|---|
| Mutation log capture | Write-block mutations are recorded per relation and only applied to invalidation/schema-estimate state on successful commit. `deduce 'abort` drops pending logs. | `implemented` | `src/lisp/deduce_db_handles_mutation.c3`, `src/lisp/deduce_db_handles_mutation_txn.c3` |
| Diagnostic dirty tracking | `deduce/analyze` reports incremental diagnostics before execution and currently resets dirty tracking after success. | `implemented` | `src/lisp/deduce_rule_eval_exec.c3`, `src/lisp/deduce_rule_eval_fixpoint.c3` |
| Dependency graph recording | Incremental dependency-edge metadata is tracked and exposed, but it is still a diagnostic input rather than a full maintenance engine. | `partial` | `src/lisp/deduce_db_handles_mutation_tracking.c3`, `src/lisp/deduce_rule_eval_exec_seminaive.c3` |
| Full recompute fallback | Successful analyze still executes the full fixpoint path today. | `implemented` | `src/lisp/deduce_rule_eval_fixpoint.c3` |
| True incremental derived-state maintenance | Delta propagation and selective invalidation are not yet the production path. | `deferred` | `docs/plans/deduce-incremental-delta-propagation-design-2026-03-13.md` |

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
    execution rather than schema estimates.
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

## 8.1 Provenance / Why-Result Payload Shape

This is the canonical shape for future provenance-aware Deduce surfaces.
It is designed to answer "why did this row appear?" with a deterministic
derivation trace that matches the existing `kind`/`status`/payload style used
by `deduce/explain` and `deduce/analyze`.

Canonical surface names for this payload family:

- `deduce/why-result` for selected row or fact lineage
- provenance-aware `deduce/analyze` when a caller asks for derivation tracing

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
