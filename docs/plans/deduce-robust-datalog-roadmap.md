# Deduce Robust Datalog Roadmap (2026-03-13)

Purpose: evolve Deduce from scan/filter-oriented relational helpers into a
robust, optimizer-backed Datalog engine while preserving Omni runtime
invariants (deterministic region ownership, boundary correctness, and
transaction safety).

## Current State Snapshot

Current behavior (validated as of 2026-03-11):

- `deduce/query` evaluates via relation scan + callback filtering.
- `deduce/match` scans rows and applies unification in memory.
- scan-path materialization has been optimized (column-key caching).
- planner metadata, explain payload skeletons, and recursive SCC/fixpoint
  execution hooks are present in the current tree, but they remain partial and
  still need truthfulness, consolidation, and regression closure before they
  count as complete.

Current truth-source pointers:

- authoritative capability matrix: `docs/deduce-datalog-spec.md#13-current-implementation-matrix`
- canonical implementation surface inventory:
  `docs/deduce-datalog-spec.md#14-canonical-implementation-surface-inventory`
- incremental-state truth table:
  `docs/deduce-datalog-spec.md#71-incremental-state-truth-table`

Evidence:

- `docs/plans/deduce-scan-query-count-benchmark-baseline-2026-03-11.md`
- `docs/plans/deduce-scan-range-materialization-cost-baseline-2026-03-11.md`
- `docs/plans/deduce-scan-range-key-cache-optimization-2026-03-11.md`
- `docs/plans/deduce-query-optimization-evaluation-2026-03-11.md`

## Success Criteria

1. Recursive rule workloads execute with semi-naive delta evaluation.
2. Join-heavy workloads use index-aware plans rather than full relation scans.
3. `deduce/explain` reports stable, interpretable physical plans.
4. Existing `deduce` API semantics remain backward compatible unless explicitly
   versioned.
5. Memory/boundary invariants remain green under ASAN + container-gated slices.

## Phase Plan

## R0. Semantics Lock and Spec Baseline

Goals:

- Freeze v1 Datalog semantics and non-goals before optimizer implementation.

Deliverables:

- `docs/deduce-datalog-spec.md` with normative syntax/semantics.
- Explicit stratified-negation constraints.
- Explicit aggregate policy (supported or deferred).

Exit gate:

- Spec reviewed and referenced by TODO and plan index.

## R1. Logical IR and Rule Validation

Goals:

- Introduce a dedicated internal rule/query IR decoupled from raw list forms.

Deliverables:

- IR model: `Predicate`, `Atom`, `Rule`, `Program`, `Binding`.
- Validation pass: arity checks, unsafe variable detection, negation safety.
- Deterministic canonical variable-id normalization.

Exit gate:

- Rule parse/normalize tests pass for valid and invalid programs.

## R2. Predicate Access and Index Surface

Goals:

- Move from scan-first access to binding-aware index access.

Deliverables:

- Relation index metadata and open/create path extensions.
- Bound-argument mask analysis for predicate lookups.
- Prefix/range tuple iterators that avoid eager dict materialization.

Exit gate:

- Explain output can show index selection decisions.

## R3. Planner v1 (Conjunctive Bodies)

Goals:

- Build cost-informed physical plans for non-recursive conjunctive queries.

Deliverables:

- Selectivity estimation baseline (row counts + distinct counts).
- Join ordering and pushdown (filter/projection).
- Plan format stable enough for regression assertions.

Exit gate:

- Benchmarks show reduced scanned rows vs scan/filter baseline on selective
  joins.

## R4. Join Engine

Goals:

- Support practical join execution strategies beyond nested scan loops.

Deliverables:

- Index nested-loop join implementation.
- Hash-join implementation for large intermediates.
- Runtime adaptive fallback thresholds for estimate misses.

Exit gate:

- Join workloads pass correctness + perf envelope gates.

## R5. Recursive Evaluation (Semi-Naive)

Goals:

- Implement recursive rule evaluation with fixpoint convergence.

Deliverables:

- SCC decomposition for recursive components.
- Naive evaluator (reference mode) + semi-naive evaluator (production mode).
- Delta relation management and convergence diagnostics.

Exit gate:

- Recursive benchmark suite validates output parity and meaningful speedup.

## R6. Negation and Stratification Runtime

Goals:

- Enforce sound negation semantics at runtime.

Deliverables:

- Stratum scheduler over validated dependency graph.
- Rejection path for non-stratifiable programs.
- Regression tests for negative and mixed recursive/negative cases.

Exit gate:

- Deterministic, test-backed stratified execution.

## R7. Incremental Maintenance (Post-v1)

Goals:

- Avoid full re-evaluation after fact mutations when safe.

Deliverables:

- Delta propagation graph for inserts/retracts.
- Dependency-aware invalidation.
- Transaction-safe derived relation updates.

Exit gate:

- Incremental benchmarks show wins over full recompute for typical update
  ratios.

## R8. Observability and Robustness

Goals:

- Make planner/evaluator behavior diagnosable and regression-resistant.

Deliverables:

- Unified Deduce counters: scans, rows touched, joins, index hits/misses, delta
  rounds.
- `deduce/explain` and `deduce/analyze` stable payload schema.
- Container-capped regression gates for correctness + performance envelopes.

Exit gate:

- CI detects both functional and major perf regressions.

## Benchmark and Validation Policy

- Keep existing baseline suites and add:
  - selective join suite,
  - recursive transitive-closure suite,
  - skewed-cardinality suite,
  - mutation-heavy incremental suite.
- Runtime-heavy and memory-stress slices run container-only per repo policy.
- Memory/lifetime sensitive changes require ASAN build + targeted Deduce slices.

## Compatibility Policy

- Keep `deduce/query` and `deduce/match` as stable front-door APIs.
- Route legacy scan/filter behavior through planner/evaluator internals
  progressively.
- New advanced features can surface behind explicit commands (`deduce/rule!`,
  `deduce/explain`, `deduce/analyze`) before becoming default behavior.
