# Deduce Sophistication Plan (2026-03-20)

Status: `active`  
Owner: Codex workflow  
Scope: post-complete Deduce capability and robustness work

## Purpose

Turn Deduce from a capable relational helper surface into a more sophisticated,
predictable Datalog subsystem without destabilizing Omni's runtime ownership and
boundary model.

This plan is intentionally more execution-oriented than
`docs/plans/deduce-robust-datalog-roadmap.md`. Use the roadmap for the
long-range architecture, and use this file as the concrete work queue for the
next substantive Deduce upgrades.

## Current Baseline

As of the checked-in tree:

- canonical surfaces are already present in the spec:
  - `deduce/rule!`
  - `deduce/query`
  - `deduce/match`
  - `deduce/explain`
  - `deduce/analyze`
- install-time rule metadata, planner shape, explain payload skeleton, and
  SCC-oriented analyze/runtime hooks are partially present in
  `docs/deduce-datalog-spec.md`
- relation scan/count/query and several benchmark baselines already exist
- runtime modularization has already split much of the Deduce code into focused
  files, but major execution files remain large enough to deserve
  sophistication-driven follow-up

The implication is important: this is not a greenfield design pass. The next
phase should harden and deepen the existing planner/execution surfaces rather
than re-arguing the high-level roadmap.

## Outcomes

This plan is complete when Deduce can credibly claim all of the following:

1. Conjunctive queries use stable planner-selected execution paths rather than
   mostly scan/filter fallback behavior.
2. Recursive rule workloads are correct, deterministic, and explainable under a
   semi-naive engine.
3. Negation and stratification behavior is enforced by one authoritative
   validation/runtime path.
4. Incremental mutation handling is explicit, measurable, and no longer
   hand-wavy in status/docs.
5. `deduce/explain` and `deduce/analyze` are strong enough to debug performance
   and correctness regressions.
6. Validation policy is container-friendly, repeatable, and specific to Deduce
   risk areas.

## Non-Goals

- redesigning Deduce into a separate runtime or storage engine
- introducing parallel fixpoint execution in this phase
- broad syntax churn on the public `deduce/*` surface
- speculative optimizer work before observability and correctness are stable

## Execution Rules

1. Preserve current `deduce/query` and `deduce/match` compatibility unless a
   behavior change is explicitly documented first.
2. Prefer one authoritative implementation path over duplicate "legacy vs new"
   execution branches.
3. Any planner/runtime behavior change must land with:
   - deterministic explain/analyze visibility,
   - targeted tests,
   - and benchmark evidence where performance is part of the claim.
4. Runtime-heavy validation must use the bounded container path per repo policy.
5. Ownership and mutation safety are release-significant for Deduce work; do
   not trade boundary correctness for planner sophistication.

## Phase Queue

### D0. Baseline Closure and Doc Reality Check

Goal:
- reconcile what Deduce docs claim, what the spec says is already landed, and
  what the runtime actually guarantees today

Tasks:
- audit `docs/deduce-datalog-spec.md`,
  `docs/plans/deduce-robust-datalog-roadmap.md`, and current runtime tests for
  drift
- classify each major capability as one of:
  - `implemented`
  - `partial`
  - `planned`
- publish one short status table in the roadmap or spec so future work stops
  mixing aspirational and landed language
- identify the canonical source files for:
  - rule install/validation
  - planning
  - recursive evaluation
  - explain/analyze
  - incremental invalidation

Acceptance:
- no major Deduce capability is simultaneously described as both "not
  implemented" and "already present" across active docs
- the next phases can refer to one concrete source-of-truth table

Likely targets:
- `docs/deduce-datalog-spec.md`
- `docs/plans/deduce-robust-datalog-roadmap.md`
- `src/lisp/deduce_rule_eval*.c3`
- `src/lisp/deduce_rule_ops*.c3`
- `src/lisp/tests_deduce_*.c3`

### D1. Rule Validation Hardening

Goal:
- make rule installation reject invalid programs deterministically and early

Tasks:
- centralize arity, unsafe-variable, and negation-safety checks behind one
  validation layer
- ensure variable normalization is deterministic and shared across:
  - rule install
  - explain
  - analyze
- add explicit rejection coverage for:
  - head variables not bound by positive body atoms
  - negated-body variables introduced before binding
  - relation-not-found and arity mismatch paths
  - duplicate/conflicting predicate declarations if applicable
- make rejection payloads stable and machine-checkable

Acceptance:
- invalid rules fail at install time with stable `deduce/*` payloads
- explain/analyze never have to guess around malformed rule state

### D2. Planner Truthfulness

Goal:
- make the planner's decisions reflect the runtime's real execution behavior

Tasks:
- audit whether selected indexes, join order, filter pushdown, and projection
  masks are consumed by the executor or merely recorded as metadata
- close any gap where `deduce/explain` claims an operator that runtime
  execution does not materially honor
- stabilize a minimal physical-plan contract:
  - operator kind
  - join order
  - bound mask
  - chosen index
  - pushdown flags
- add planner regressions for selective and non-selective query shapes

Acceptance:
- `deduce/explain` is trusted as execution truth, not decorative metadata
- representative selective joins show fewer rows touched than the scan baseline

Likely targets:
- `src/lisp/deduce_relation_scan_helpers*.c3`
- `src/lisp/deduce_rule_eval_exec*.c3`
- `src/lisp/deduce_rule_ops_explain.c3`

### D3. Join Engine Maturity

Goal:
- move beyond scan-dominated execution for practical multi-atom workloads

Tasks:
- make index nested-loop joins the default when bound-mask selectivity supports
  them
- harden hash-join and adaptive-join thresholds with measured, checked
  behavior rather than static hopeful heuristics
- add benchmark-backed routing checks for:
  - selective lookup joins
  - wide fan-out joins
  - skewed-cardinality joins
- expose enough counters to distinguish planner mistakes from runtime data skew

Acceptance:
- join-heavy workloads have at least two meaningfully different operator paths
- explain/analyze can show why a join path was selected and how it behaved

### D4. Recursive Engine Closeout

Goal:
- make recursive SCC execution a first-class, production-quality path

Tasks:
- define one canonical production engine:
  - `semi-naive-scc`
- keep naive evaluation only as a correctness/reference mode
- validate deterministic convergence and derived-fact counts for:
  - transitive closure
  - multi-rule SCCs
  - recursive-plus-non-recursive mixed strata
- tighten iteration-limit failure reporting with stable component/stratum
  identifiers
- ensure recursive execution diagnostics are surfaced in `deduce/analyze`

Acceptance:
- recursive suites prove parity between naive and semi-naive outputs
- semi-naive execution demonstrates a measurable win on recursive benchmarks

Likely targets:
- `src/lisp/deduce_rule_eval_fixpoint.c3`
- `src/lisp/deduce_rule_eval_exec_seminaive.c3`
- `src/lisp/deduce_rule_eval_scc.c3`

### D5. Stratified Negation Closure

Goal:
- make negation support sound, explicit, and unsurprising

Tasks:
- ensure dependency-graph construction is shared between validation and runtime
  scheduling
- reject negative cycles with stable payloads and deterministic reporting
- add mixed positive/negative recursion tests that prove:
  - legal stratified shapes execute,
  - illegal non-stratified shapes reject,
  - explain/analyze reports stratum boundaries coherently

Acceptance:
- negation semantics are enforced by one authoritative graph/scheduler model
- docs and tests agree on exactly what "stratified" means operationally

### D6. Incremental Mutation from Diagnostics to Real Semantics

Goal:
- turn current mutation/invalidation notes into a trustworthy incremental
  maintenance story

Tasks:
- inventory what current "incremental" state actually does:
  - dirty tracking
  - dependency-edge recording
  - schema estimate updates
  - full recompute fallback points
- separate two modes explicitly:
  - `diagnostic incremental state only`
  - `true incremental derived-state maintenance`
- land dependency-aware invalidation for derived predicates only when the
  invalidation graph and commit/abort behavior are transaction-safe
- benchmark mutation-heavy workloads before and after each step

Acceptance:
- docs no longer imply true incremental maintenance where only diagnostics
  exist
- either real incremental wins are demonstrated, or the feature stays clearly
  marked deferred

Likely targets:
- `src/lisp/deduce_db_handles_mutation*.c3`
- `src/lisp/deduce_rule_eval*.c3`
- `docs/plans/deduce-incremental-delta-propagation-design-2026-03-13.md`

### D7. Observability and Operator Ergonomics

Goal:
- make Deduce debuggable by normal users and maintainers

Tasks:
- strengthen `deduce/explain` with normalized logical forms and observed
  runtime counters where feasible
- strengthen `deduce/analyze` with stable summaries for:
  - strata
  - SCCs
  - iterations
  - derived facts
  - dirty predicates
  - invalidation mode
- decide whether cleanup/statistics verbs from the post-complete backlog belong
  in this phase or in a later analytics milestone
- add deterministic empty-relation and malformed-schema tests for statistics and
  cleanup paths

Acceptance:
- a maintainer can explain a bad Deduce query plan from structured output alone
- post-complete backlog items for cleanup/statistics behavior are either closed
  or explicitly deferred into a dedicated analytics phase

### D8. Validation and Performance Gates

Goal:
- turn Deduce sophistication work into an enforceable gate rather than an
  anecdotal benchmark story

Tasks:
- keep existing benchmark baselines, then add named envelope gates for:
  - selective join
  - recursive transitive closure
  - skewed-cardinality joins
  - mutation-heavy incremental workloads
- make container-bounded validation commands explicit and repeatable
- define what counts as:
  - correctness blocker
  - planner regression
  - acceptable benchmark noise
- wire Deduce-focused summaries into the broader validation status workflow if
  the signal is useful enough

Acceptance:
- new Deduce regressions are detectable without manually interpreting raw logs
- performance claims in future changelog entries have a named benchmark source

## Recommended Execution Order

1. D0 baseline closure and doc reality check
2. D1 rule validation hardening
3. D2 planner truthfulness
4. D4 recursive engine closeout
5. D5 stratified negation closure
6. D3 join engine maturity
7. D6 incremental mutation semantics
8. D7 observability and operator ergonomics
9. D8 validation and performance gates

Reasoning:
- D0 through D2 remove ambiguity about what the current system really is.
- D4 and D5 close correctness-critical semantic gaps.
- D3, D6, and D7 are only worth doing once planner/runtime truth and recursive
  correctness are stable.
- D8 should be built continuously, but formalized after the main capability
  shape is clear.

## Validation Policy

Use the bounded container path for runtime-heavy validation.

Per substantive Deduce landing, run at minimum:

- `c3c build`
- `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`

For memory/lifetime-sensitive Deduce changes, also run:

- `c3c build --sanitize=address`
- `scripts/run_validation_container.sh env ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`

For benchmark-backed claims, reference one or more of:

- `docs/plans/deduce-scan-query-count-benchmark-baseline-2026-03-11.md`
- `docs/plans/deduce-selective-join-benchmark-baseline-2026-03-13.md`
- `docs/plans/deduce-skewed-cardinality-benchmark-baseline-2026-03-13.md`
- `docs/plans/deduce-incremental-mutation-benchmark-baseline-2026-03-13.md`

## Exit Condition

This plan can be marked complete when:

- the roadmap-level open items are either implemented or explicitly deferred
  with honest status wording,
- planner/explain/analyze semantics are trustworthy enough for regression work,
- recursive and negation behavior are test-anchored and deterministic,
- and future Deduce work can move from "sophistication bootstrap" to narrower
  feature or performance slices.
