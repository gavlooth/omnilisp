# Deduce Datalog Spec (Draft v1)

Status: draft execution spec for robust Deduce roadmap.  
Last updated: 2026-03-13.

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

## 5. Evaluation Model

Evaluation modes:

- non-recursive strata: planned join execution over EDB/IDB inputs,
- recursive strata: semi-naive fixpoint using delta relations.

Correctness baseline:

- naive bottom-up evaluation is reference-equivalent to semi-naive output.

Termination:

- recursive stratum converges when delta relation for all predicates is empty.

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
  - `rows-read`, `rows-emitted`, `index-hits`, `index-misses`, `counter-kind`
  - current `counter-kind` is `estimated` (planner/statistics-derived).
- operator values are explicit symbols derived from planned access mode:
  `full-scan`, `index-scan`, `negated-scan`, `negated-index-probe`.
- this v1 surface remains planner-focused; normalized logical forms and direct
  observed runtime counters remain planned follow-up work.

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
- selective predicates avoid full scans (assert through explain counters).

4. Runtime semantics:
- query absence remains `nil`,
- mutation success remains `Void`.

## 11. Compatibility Contract

- existing `deduce` API command surfaces remain valid.
- internal engine changes are transparent unless explicitly version-gated.
- any syntax expansion for rules/queries must preserve existing programs or
  provide a deliberate migration path.
