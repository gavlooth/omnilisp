# Deduce Full Roadmap (2026-03-20)

Status: `active`  
Owner: Codex workflow  
Scope: full Deduce evolution from relational helper surface to serious Datalog
and analytics subsystem

## Purpose

This is the non-conservative roadmap for Deduce.

The intent is not merely to harden the current implementation. The intent is to
make Deduce one of the language's flagship capabilities: a deterministic,
explainable, optimizer-backed, mutation-aware Datalog and analytics layer that
fits Omni's runtime model instead of fighting it.

Use this file when deciding what Deduce should become.
Use `docs/plans/deduce-sophistication-plan-2026-03-20.md` when deciding what to
implement next on the checked-in tree.
Use `docs/plans/deduce-actionable-backlog-2026-03-20.md` for the live ordered
execution backlog.

## Product Thesis

Deduce should eventually provide all of the following:

1. A stable Datalog core for facts, rules, recursive derivation, and
   stratified negation.
2. A planner/executor that is explainable enough for debugging and efficient
   enough for practical workloads.
3. An incremental maintenance story for long-lived applications with mutable
   fact sets.
4. A transactional and analytics-oriented surface suitable for applications,
   not just tests and demos.
5. Tooling and observability strong enough that users can treat Deduce as a
   first-class subsystem rather than a hidden runtime experiment.

## Roadmap Principles

1. Determinism first.
   Deduce should prefer stable outputs and stable diagnostics over clever but
   unstable execution tricks.
2. Explainability is part of correctness.
   If a planner/executor decision cannot be surfaced coherently in
   `deduce/explain` or `deduce/analyze`, it is not ready.
3. Runtime invariants are non-negotiable.
   Deduce sophistication must preserve Omni's boundary, ownership, and
   transaction safety rules.
4. Compatibility is earned, not assumed.
   Existing `deduce/query` and `deduce/match` behavior should stay stable, but
   advanced capability may need explicit new surfaces before becoming default.
5. Internal duplication is a liability.
   There should be one authoritative validation path, one authoritative planner
   path, and one authoritative execution path per engine mode.

## Target Capability Matrix

The roadmap targets these end-state capability bands.

### A. Core Logic Engine

- relation declarations and fact storage
- rule installation and validation
- conjunctive queries
- recursive evaluation
- stratified negation
- aggregate support
- constraints and integrity checks
- provenance/introspection

### B. Optimizer and Execution

- index-aware access paths
- cost-based join ordering
- multiple join operators
- recursive SCC scheduling
- incremental maintenance
- adaptive runtime counters and feedback
- advanced rewrite rules
- optional parallel execution later

### C. Product Surface

- canonical `deduce/*` commands
- stable explain/analyze payloads
- migration-safe compatibility behavior
- deterministic error surfaces
- schema/index/statistics management
- cleanup/maintenance verbs
- benchmark and validation policy

### D. Tooling and UX

- machine-readable explain/analyze output
- docs and examples for real workloads
- debugging workflows for plan regressions
- editor/CLI surfaces for analysis and diagnostics

## Delivery Horizon

The roadmap is split into four horizons:

1. Foundation
2. v1 Datalog Core
3. v1.5 Analytics and Incremental System
4. v2 Optimizer and Scale Features

## Horizon 1: Foundation

This horizon makes Deduce internally coherent enough to support larger work.

### F0. Truth and Documentation Unification

Goals:

- remove ambiguity between docs, spec, tests, and runtime claims
- define the authoritative current-state Deduce status table

Deliverables:

- one status matrix covering:
  - installed
  - partial
  - planned
  - deferred
- one mapping from features to source files and tests
- one compatibility note for legacy aliases and legacy scan-first behavior

Exit gate:

- no active doc simultaneously treats the same feature as both shipped and
  missing

### F1. Logical IR and Validation Authority

Goals:

- build a durable internal model instead of continuing to reason from raw forms

Deliverables:

- canonical IR for:
  - predicate
  - term
  - variable
  - atom
  - rule
  - program
  - stratum
  - plan
- one validation path for:
  - arity
  - relation existence
  - unsafe variables
  - negation safety
  - aggregate placement rules
  - duplicate/conflicting declarations

Exit gate:

- install-time rule validation and explain/analyze normalization reuse the same
  IR and validation logic

### F2. Statistics and Metadata Bedrock

Goals:

- make planning and analysis depend on explicit metadata instead of guesses

Deliverables:

- relation statistics model:
  - row count
  - distinct counts
  - nullability/absence expectations if relevant
  - update timestamps
- index descriptor model:
  - key order
  - prefix capability
  - uniqueness
  - estimated selectivity
- schema metadata for rule dependency and invalidation tracking

Exit gate:

- planner and analyze payloads can consume stable metadata without recomputing
  ad hoc structure from runtime state

## Horizon 2: v1 Datalog Core

This horizon establishes Deduce as a serious Datalog system.

### V1-0. Canonical Public Surface

Goals:

- lock the front-door contract that user code and docs should depend on

Deliverables:

- canonical commands:
  - `deduce/rule!`
  - `deduce/query`
  - `deduce/match`
  - `deduce/explain`
  - `deduce/analyze`
- canonical result and error contract for each command
- compatibility bridge for any retained legacy spellings

Exit gate:

- all new docs, tests, and examples use the canonical names only

### V1-1. Planner-backed Conjunctive Queries

Goals:

- stop treating multi-atom query execution as mostly scan-and-filter

Deliverables:

- stable logical-to-physical planning path
- operator set:
  - full scan
  - index scan
  - index probe
  - filter
  - projection
  - join
  - negated probe/anti-join form
- deterministic join ordering for conjunctive bodies
- planner regression tests and baseline counters

Exit gate:

- selective queries show reduced rows touched versus scan baseline
- explain output is truthful enough to debug operator selection

### V1-2. Multiple Join Strategies

Goals:

- support practical workloads with more than one join style

Deliverables:

- index nested-loop join
- hash join
- adaptive fallback policy for estimate misses
- skew-aware routing heuristics
- per-operator runtime counters

Exit gate:

- join-heavy benchmarks show different operators winning on different query
  shapes

### V1-3. Recursive Evaluation

Goals:

- make recursive rules a real production path, not a spec ambition

Deliverables:

- SCC decomposition
- naive reference engine
- semi-naive production engine
- deterministic fixpoint convergence
- iteration-limit and divergence guardrails
- recursive parity regressions that compare `naive-scc` reference behavior
  with `semi-naive-scc` production behavior

Exit gate:

- recursive benchmark suite proves parity and meaningful speedup

### V1-4. Stratified Negation

Goals:

- support sound negation without semantic ambiguity

Deliverables:

- dependency graph with positive and negative edges
- stratification validator
- stratum-aware scheduler
- stable rejection path for non-stratifiable programs

Exit gate:

- legal stratified programs run
- negative cycles reject deterministically

### V1-5. Explainability and Analysis Baseline

Goals:

- make execution understandable enough for users and maintainers

Deliverables:

- `deduce/explain` reports:
  - normalized logical form
  - chosen physical operators
  - join order
  - index decisions
  - pushdown decisions
  - estimated counters
- `deduce/analyze` reports:
  - rule counts
  - strata
  - SCCs
  - iterations
  - derived facts
  - execution engine
  - mutation/invalidation summary when relevant

Exit gate:

- explain and analyze payloads are stable enough for regression assertions

### V1-6. Validation and Benchmark Gates

Goals:

- make Deduce capability enforceable rather than anecdotal

Deliverables:

- named benchmark suites:
  - selective joins
  - recursive transitive closure
  - skewed-cardinality joins
  - mutation-heavy workloads
- named correctness suites for:
  - rule validation
  - recursion
  - negation
  - explain/analyze payload stability
- container-capped validation commands wired into repo workflow

Exit gate:

- functional regressions and major planner regressions are easy to detect

## Horizon 3: v1.5 Analytics and Incremental System

This horizon turns Deduce from "good Datalog core" into "application-grade data
subsystem".

### V1.5-0. True Incremental Maintenance

Goals:

- avoid full recompute after every fact mutation where semantics allow

Deliverables:

- dependency graph for derived predicates
- delta propagation on insert/retract/update-equivalent mutation flows
- dirty-region or dirty-predicate invalidation
- transactional integration with commit/abort
- correctness parity against full recompute mode

Exit gate:

- benchmarked mutation-heavy workloads show real wins over full recompute

### V1.5-1. Aggregate Support

Goals:

- support practical analytics workloads that need grouped summaries

Entry policy:

- v1.5 aggregate work starts with exact grouped aggregates that have small,
  deterministic state and straightforward explainability:
  - `count`
  - `sum`
  - `min`
  - `max`
- deferred inside the aggregate lane:
  - `avg`
  - `distinct-count`
- non-goal for v1.5:
  - percentile/median
  - variance/stddev
  - ordered-set aggregates
  - approximate/sketch aggregates
  - user-defined aggregate hooks

Candidate aggregate families:

- `count` (v1.5 target)
- `sum` (v1.5 target)
- `min` (v1.5 target)
- `max` (v1.5 target)
- `avg` (deferred)
- `distinct-count` (deferred)

Deliverables:

- explicit aggregate semantics in rules and queries
- aggregate safety/placement validation
- grouped aggregate execution path
- explain visibility for aggregate nodes
- incremental semantics policy:
  - supported incrementally
  - supported via recompute
  - or deferred per aggregate

Exit gate:

- aggregate workloads are documented, tested, and explainable

### V1.5-2. Constraint and Integrity Layer

Goals:

- let Deduce enforce more than query semantics

Candidate constraints:

- uniqueness
- required arity/schema alignment
- foreign-key-like relation integrity where meaningful
- materialized relation invariants

Deliverables:

- constraint declaration/install surface
- deterministic constraint failure payloads
- transactional enforcement model

Exit gate:

- integrity checks work without undermining LMDB/runtime safety

### V1.5-3. Maintenance and Admin Verbs

Goals:

- make Deduce manageable in long-lived applications

Candidate surfaces:

- statistics refresh/recompute
- relation cleanup/compact
- stale derived-state reset
- index inspection
- schema inspection
- benchmark-friendly reset/seed helpers

Current shipped slice:

- read-only relation-handle admin/introspection surfaces are now implemented:
  - `deduce/schema`
  - `deduce/indexes`
  - `deduce/stats`
- write-side maintenance verbs beyond existing `clear!` / `drop!` remain part
  of the open v1.5 admin queue

Exit gate:

- post-complete backlog cleanup/statistics items are closed or replaced by this
  surface

### V1.5-4. Provenance and Why-Results

Goals:

- answer "why did this row appear?" and "what rule/path produced it?"

Deliverables:

- provenance-aware analyze mode
- derivation tracing for selected facts or result rows
- optional rule-path or dependency-chain payloads
- canonical `why-result` payload shape:
  - `kind = why-result`
  - `status`
  - `surface-kind = provenance-snapshot`
  - `subject-kind`
  - `subject`
  - `path-count`
  - `max-depth`
  - `truncated`
  - `paths[]` with ordered support frames

Design note:

- the payload shape is specified in `docs/deduce-datalog-spec.md`; landing the
  implementation remains a separate milestone

Exit gate:

- at least one practical debugging workflow exists for derived facts

## Horizon 4: v2 Optimizer and Scale Features

This horizon is deliberately ambitious.

### V2-0. Cost Model Upgrade

Goals:

- move beyond rule-of-thumb planning

Deliverables:

- richer selectivity estimation
- multi-column statistics
- better skew handling
- optional runtime feedback loop that feeds future plans

Exit gate:

- planner decisions improve on mixed workload corpora, not just synthetic
  microbenches

### V2-1. Advanced Rewrites

Candidate rewrites:

- predicate reordering beyond naive heuristics
- semi-join reduction
- anti-join rewrites for negation
- projection/filter pushdown across more operators
- common-subexpression reuse where safe

Exit gate:

- rewrite wins are visible in explain/analyze and justified by benchmarks

### V2-2. Magic Sets / Goal-directed Recursion

Goals:

- reduce recursive work for selective top-down-like query shapes

Deliverables:

- explicit magic-set rewrite pipeline or equivalent demand-driven transform
- explain visibility for rewritten recursive plans
- safety and compatibility rules

Exit gate:

- recursive selective queries materially improve without semantic drift

### V2-3. Materialized Views and Derived Relation Persistence

Goals:

- make some derived-state persistent and manageable

Deliverables:

- materialized derived relation declarations
- refresh policy
- invalidation policy
- persistence semantics with LMDB

Exit gate:

- materialized derivations have a coherent lifecycle and durability contract

### V2-4. Parallel and Offloaded Evaluation

Goals:

- exploit concurrency where it genuinely helps, without violating runtime
  invariants

Deliverables:

- explicit concurrency model for planner/executor
- SCC or operator-level parallelism only where determinism and ownership remain
  clear
- benchmark evidence that coordination cost is worth it

Exit gate:

- any parallel path is optional, measurable, and explainable

### V2-5. Storage and Corpus-scale Workloads

Goals:

- make Deduce reliable on larger application datasets

Deliverables:

- large-corpus benchmark suite
- memory and spill policy documentation
- relation/index management guidance
- practical envelope numbers for supported workload sizes

Exit gate:

- maintainers know where Deduce bends and where it breaks

## Cross-cutting Workstreams

These run across all horizons.

### X1. Runtime Safety

- boundary correctness
- ownership correctness
- transaction safety
- ASAN coverage for memory-sensitive paths

### X2. Documentation

- language-facing semantics
- operator-facing admin docs
- roadmap/spec/status alignment
- examples that show realistic usage, not toy facts only

### X3. Tooling

- machine-readable explain/analyze payloads
- future editor/CLI integration points
- structured diagnostics for rule install and query planning failures

### X4. Performance Governance

- benchmark baselines
- benchmark envelope policy
- acceptable noise thresholds
- release-significant regressions versus expected variance

## Suggested Major Milestones

### Milestone A: "Deduce v1"

Includes:

- Foundation complete
- planner-backed conjunctive queries
- multiple join strategies
- recursive SCC evaluation
- stratified negation
- stable explain/analyze baseline
- validation and benchmark gates

Outcome:

- Deduce is a real Datalog subsystem

Checklist:

- [ ] F0 truth and documentation unification complete
- [ ] F1 logical IR and validation authority complete
- [ ] F2 statistics and metadata bedrock complete
- [ ] V1-0 canonical public surface locked
- [ ] V1-1 planner-backed conjunctive queries landed
- [ ] V1-2 multiple join strategies landed
- [x] V1-3 recursive evaluation landed
- [x] V1-4 stratified negation landed
- [ ] V1-5 explainability and analysis baseline landed
- [ ] V1-6 validation and benchmark gates landed

### Milestone B: "Deduce v1.5"

Includes:

- true incremental maintenance
- aggregate support
- admin/maintenance verbs
- provenance tooling
- stronger stats and metadata

Outcome:

- Deduce becomes application-usable for mutable analytical workloads

Checklist:

- [ ] V1.5-0 true incremental maintenance landed
- [ ] V1.5-1 aggregate support landed
- [ ] V1.5-2 constraint and integrity layer landed
- [ ] V1.5-3 maintenance and admin verbs landed
- [ ] V1.5-4 provenance and why-result support landed

### Milestone C: "Deduce v2"

Includes:

- upgraded cost model
- advanced rewrites
- magic sets or equivalent goal-directed optimization
- materialized views
- optional parallel execution
- corpus-scale benchmark posture

Outcome:

- Deduce becomes one of Omni's defining advanced capabilities

Checklist:

- [ ] V2-0 cost model upgrade landed
- [ ] V2-1 advanced rewrites landed
- [ ] V2-2 magic sets or equivalent goal-directed optimization landed
- [ ] V2-3 materialized views and derived persistence landed
- [ ] V2-4 optional parallel and offloaded evaluation landed
- [ ] V2-5 storage and corpus-scale workload posture landed

## Milestone-to-Backlog Mapping

The roadmap defines the destination. The backlog defines the next executable
steps.

- Milestone A is primarily driven by backlog bands:
  - `P0. Truth and Correctness`
  - `P1. Planner and Explain Truthfulness`
  - `P2. Recursive and Negation Closeout`
  - `P3. Join Engine and Runtime Counters`
- Milestone B is primarily driven by:
  - `P4. Incremental Maintenance`
  - `P5. Analytics and Product Surface`
- Milestone C remains mostly roadmap-level for now and should only be promoted
  into the actionable backlog once Milestone A is materially closed and
  Milestone B has a stable semantic base.

## Recommended Next Implementation Order

For the checked-in tree, the ambitious roadmap should still start with:

1. foundation truth/IR/statistics work
2. planner truthfulness and join operator maturity
3. recursive and negation closeout
4. explain/analyze stabilization
5. incremental maintenance
6. aggregates and admin verbs
7. provenance
8. advanced optimizer work
9. optional parallel/scaling work

This ordering is not conservative. It is simply dependency-aware.
Trying to do aggregates, magic sets, or parallelism before the planner,
recursive engine, and explain surfaces are trustworthy would create a larger,
more confusing system rather than a better one.

## Success Condition

This roadmap succeeds when Deduce is no longer described as "LMDB plus some
query helpers" or "a promising Datalog sketch".

It succeeds when:

- application code can trust it,
- maintainers can debug it,
- benchmarks can measure it,
- docs can explain it,
- and future work can be framed as extensions to a serious subsystem instead of
  rescue work on an unfinished one.
