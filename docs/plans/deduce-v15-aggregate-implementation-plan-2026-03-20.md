# Deduce v1.5 Aggregate Implementation Plan (2026-03-20)

Status: `completed`
Owner: Codex workflow
Scope: promote the first v1.5 analytics lane from roadmap prose into concrete
implementation slices on the checked-in Deduce runtime

## Purpose

This plan closes the gap between the broad v1.5 roadmap and the current
codebase reality.

Deduce now has a stable v1 Datalog core, and the first aggregate lane has
already shipped on the checked-in tree for non-recursive grouped rule heads:
aggregate-aware rule terms/signatures, install-time validation, grouped
execution for exact `count` / `sum` / `min` / `max`, and explain/analyze
support now exist.

The remaining goal here is no longer generic grouped execution. Recursive
aggregate support is now landed on the checked-in tree on the seminaive SCC
production path for the current stratified rule surface; the remaining
follow-up is cleanup/perf work on that shipped runtime.

## Current Tree Constraints

The current implementation is partially aggregate-aware:

- `DeduceRuleIr` in `src/lisp/deduce_rule_ops.c3` now accepts aggregate-aware
  head terms and the installed runtime carries aggregate metadata.
- `DeduceRuleSignature` in `src/lisp/deduce_db_handles.c3` now persists
  aggregate projection metadata alongside predicate/term metadata plus planner
  masks.
- `deduce/explain` in `src/lisp/deduce_rule_ops_explain.c3` now exposes
  aggregate projection count and head-projection metadata, and supported
  aggregate rules can now collect observed counters, but plan steps still
  remain predicate scan/probe oriented:
  - `predicate`
  - `bound-mask`
  - `selected-index`
  - `operator`
  - `counters`
- `deduce/analyze` and rule execution in `src/lisp/deduce_rule_eval_exec*.c3`
  now execute grouped non-recursive aggregate rules through the shared
  evaluator/finalize path.
- The current public `deduce/query` surface in
  `src/lisp/deduce_schema_query.c3` is still relation-handle + callback
  filtering, not a projection/planner-driven query surface.
- recursive aggregate SCCs now execute on the seminaive SCC lane for the
  current stratified rule surface, including mixed aggregate/non-aggregate
  recursive SCCs and lower-stratum negated body atoms.
- the staged seminaive follow-through has started in the runtime substrate:
  - recursive delta state now carries signed add/remove slots,
  - aggregate-bearing recursive SCCs now seed tuple support tables and
    sign-aware head-materialization hooks behind the fallback gate,
  - dormant seminaive aggregate finalize now routes grouped head replacement
    through actual-tuple support-table zero crossings instead of direct
    rewrites plus synthetic aggregate deltas,
  - aggregate groups now track signed support counts internally, including
    removal-aware `count`/`sum` updates and `min`/`max` extremum recompute
    when the current support disappears,
  - the dormant seminaive aggregate executor now runs mixed non-aggregate
    rules in the same recursive SCC on the same signed-delta/support-table
    substrate instead of skipping them,
  - focused tuple-support and aggregate-group regressions now pin
    recursive-only, base-seeded, signed count-removal, and `min`-recompute
    semantics,
  - query/explain and tail regressions now pin both positive and lower-stratum
    negated recursive aggregate SCCs on the shipped seminaive lane.
  - engine-aware explain coverage now also pins the symmetric pure positive
    and pure negated recursive aggregate cases: explicit `naive` stays on
    `naive-scc` and does not emit the mixed-route diagnostic that is
    reserved for mixed recursive SCCs.

That means aggregate work must extend both:

1. the normalized IR / installed rule metadata, and
2. the explain/analyze payload model,

before the runtime can execute recursive grouped summaries truthfully.

## Landed Outcome

Recursive aggregate support is now landed for the aggregate lane baseline on
the seminaive SCC production path for the current stratified rule surface.

The shipped output is:

1. one explicit recursive aggregate semantics policy,
2. one SCC/fixpoint runtime strategy that now runs recursive aggregate SCCs
   seminaively across both positive and lower-stratum negated supported
   shapes,
3. one regression/diagnostic matrix that proves supported recursive shapes
   and truthfully reports the active engine.

Follow-up work is now narrower and non-blocking:

1. cleanup/perf work on the signed add/remove substrate,
2. optional narrowing of naive-mode proof-multiplicity behavior for mixed
   recursive aggregate debug runs,
3. broader aggregate surface work outside recursive rule-head execution.

## Canonical Syntax Target

Repo-local precedent already exists in `docs/CORE_LIBS_INSPECTION.md`:

```lisp
(query [(count ?name)]
  (adult ?name _))

(query [?dept (max ?salary)]
  (employee ?name ?dept ?salary))
```

For the v1.5 aggregate lane, lock the target shape as:

- projection-driven aggregate queries
- bare variables in the projection imply group-by keys
- supported aggregate forms are:
  - `(count ?x)`
  - `(sum ?x)`
  - `(min ?x)`
  - `(max ?x)`

Deferred from the start:

- `avg`
- `distinct-count`
- ordered-set aggregates
- approximate aggregates
- user-defined aggregate hooks

Rule-level aggregate support should reuse the same aggregate term metadata and
explain payload conventions as projection-driven queries, rather than inventing
a second incompatible representation.

## Minimal File Set

### 1. Syntax and IR ownership

- `src/lisp/deduce_rule_ops.c3`
- `src/lisp/deduce_rule_eval_prims.c3`
- `docs/deduce-datalog-spec.md`

Responsibilities:

- define aggregate-aware IR shapes
- parse aggregate terms from rule/query forms
- keep variable normalization and ownership rules explicit

### 2. Installed signature / metadata ownership

- `src/lisp/deduce_db_handles.c3`
- `src/lisp/deduce_db_handles_mutation.c3`

Responsibilities:

- persist aggregate projection metadata next to current rule signature state
- free/copy aggregate metadata with the same authority path as rule signatures

### 3. Validation and planning ownership

- `src/lisp/deduce_rule_eval.c3`
- `src/lisp/deduce_rule_ops_planning.c3`

Responsibilities:

- placement/safety validation
- group-key grounding checks
- aggregate-specific planner metadata

### 4. Execution ownership

- `src/lisp/deduce_rule_eval_exec.c3`
- `src/lisp/deduce_rule_eval_exec_naive.c3`
- `src/lisp/deduce_rule_eval_exec_seminaive.c3`
- `src/lisp/deduce_rule_eval_fixpoint.c3`

Responsibilities:

- grouped-state accumulation
- aggregate finalize step
- aggregate-specific runtime counters
- correct interaction with recursive and non-recursive evaluation

### 5. Explain/analyze ownership

- `src/lisp/deduce_rule_ops_explain.c3`
- `src/lisp/deduce_rule_eval_prims.c3`

Responsibilities:

- expose aggregate plan nodes truthfully
- report aggregate operator/state counters
- keep planner snapshot versus observed runtime semantics explicit

### 6. Test ownership

- `src/lisp/tests_deduce_rule_groups*.c3`
- `src/lisp/tests_deduce_query_groups.c3`
- `src/lisp/tests_deduce_query_bench_groups*.c3`

Responsibilities:

- syntax/validation coverage
- grouped result correctness
- explain/analyze payload checks
- bounded benchmark smoke for grouped workloads

## Concrete Seams To Extend

### S1. Rule/query IR

Current seam:

- `DeduceRuleTerm`
- `DeduceRuleAtom`
- `DeduceRuleIr`

Needed extension:

- add an explicit aggregate term/projection representation
- keep aggregate function, input variable, output binding, and group-key usage
  separate from ordinary predicate atoms

Why here:

- every later stage currently consumes the atom-only IR shape; without this
  change the rest of the runtime has nowhere principled to store grouped-state
  intent

### S2. Installed signature metadata

Current seam:

- `DeduceRuleSignature`
- `deduce_db_add_rule_signature(...)`
- `deduce_rule_signature_release_allocations(...)`

Needed extension:

- store aggregate projection count and per-projection metadata
- store which projected bindings are grouping keys versus aggregate outputs

Why here:

- explain/analyze and recursive execution consume installed signatures, not raw
  parsed forms

### S3. Validation

Current seam:

- `deduce_rule_validate_arity(...)`
- `deduce_rule_validate_head_variable_safety(...)`
- `deduce_rule_validate_negation_safety(...)`
- `deduce_rule_validate_install_candidate(...)`

Needed extension:

- reject unknown aggregate functions
- reject malformed aggregate projections
- reject aggregates whose inputs are not grounded by the positive prefix
- reject mixed grouped/non-grouped outputs that do not follow the projection
  contract

### S4. Explain/analyze payloads

Current seam:

- `deduce_explain_build_plan_steps(...)`
- `deduce_explain_build_rule_execution_entry(...)`
- `prim_deduce_explain(...)`
- `prim_deduce_analyze(...)`

Needed extension:

- add an aggregate node/operator shape
- report grouping keys and aggregate functions explicitly
- add aggregate counters without pretending they are scan/index counters

### S5. Runtime execution

Current seam:

- `deduce_collect_rule_observed_counters(...)`
- naive / semi-naive step walkers
- fixpoint orchestration

Needed extension:

- add a grouped accumulation phase after predicate bindings are produced
- finalize `count` / `sum` / `min` / `max`
- define incremental policy per aggregate:
  - initially allowed via recompute
  - true incremental maintenance can follow later

## Suggested First Patch Slice

Do not try to land parsing, execution, explain, and recursion in one patch.

First patch slice:

1. Lock the canonical aggregate syntax in `docs/deduce-datalog-spec.md` using
   the projection-driven shape already sketched in
   `docs/CORE_LIBS_INSPECTION.md`.
2. Extend `DeduceRuleIr` and `DeduceRuleSignature` with aggregate projection
   metadata, but keep runtime execution unchanged.
3. Add validation-only support for malformed aggregate syntax and placement:
   - unknown aggregate function
   - aggregate input not grounded
   - aggregate mixed into ordinary body atoms
   - invalid grouped/non-grouped projection shape
4. Extend `deduce/explain` / `deduce/analyze` data builders so aggregate-aware
   signatures have a reserved payload shape even before execution is enabled.
5. Add deterministic tests that pin the IR/validation/explain contract.

Why this slice first:

- it creates one authoritative aggregate representation
- it prevents execution work from hard-coding a second metadata shape later
- it keeps the first landing testable without needing grouped-state runtime
  plumbing immediately

## Follow-On Patch Slice

Second patch slice:

1. Implement grouped accumulation and finalize for non-recursive execution.
2. Land exact `count`, `sum`, `min`, `max`.
3. Add explain/analyze observed counters for aggregate nodes.
4. Add bounded grouped-workload regressions and benchmark smoke.

Only after that should aggregate support be threaded through recursive
semi-naive execution or incremental maintenance.

## Validation Gates

At minimum for each implementation slice:

- `c3c build`
- `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`

Aggregate execution slices should also add one bounded benchmark smoke in:

- `src/lisp/tests_deduce_query_bench_groups*.c3`

## Non-Goals For This Plan

This plan does not include:

- `avg`
- `distinct-count`
- full query special-form implementation outside the Deduce execution lane
- materialized views
- provenance execution
- parallel aggregate evaluation

Those remain separate v1.5 or v2 roadmap lanes.
