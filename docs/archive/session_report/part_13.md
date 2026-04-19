      `query` / `match` / `scan-range` slice after confirming selector-scoped
      `scan` is not in the current goal-directed eligibility surface
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2b2b2b`
    - closed `B6.9b2b2b2b1`
    - left broader provenance integration as `B6.9b2b2b2b2`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - exact-one goal-directed `deduce/scan` now yields path-local
      `goal-directed-read-context`
    - selector-scoped `deduce/scan` stayed outside the current shipped slice
      and was left undocumented as such

- Objectives attempted
  - Close the next integrity-class naming lane so the remaining Deduce
    constraint queue targets one concrete class instead of generic “widened
    integrity” wording.
- Code/config changes made
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`, and
    `docs/reference/08-libraries.md`:
    - closed `B6.10a1`
    - fixed the next integrity class as canonical `check`
    - renamed the remaining backlog items to refer to `check` constraints
      directly
  - Added `docs/plans/deduce-next-integrity-class-decision-2026-03-25.md`:
    - recorded the naming decision, rationale, rejected aliases, and the
      remaining follow-up slices
- Validation run
  - doc/backlog shaping only:
    - confirmed `TODO.md` now closes `B6.10a1`
    - confirmed remaining `B6.10*` items now refer to `check` constraints

- Objectives attempted
  - Turn the first parallel-runtime blocker into a concrete execution-model
    decision instead of pretending the existing LMDB-writing evaluator could
    be fanned out safely.
- Code/config changes made
  - Closed `B6.9b2b2b2b2b2b2b2b2` with path-level mixed-context goal-directed
    provenance lists across support-frame relations:
    - proof paths now expose `goal-directed-read-contexts` when matching
      support-frame contexts come from different relations
    - same-relation support paths still use the singular
      `goal-directed-read-context` field instead of a plural list
  - Closed `B6.9b2b2b2b2b2b2b2b1` with common path-level goal-directed context
    propagation across same-relation support frames:
    - proof paths now inherit bounded `goal-directed-read-context` when all
      matching support-frame contexts come from the same relation-local
      last-read state
    - kept mixed-relation support paths unmerged and left that broader merge
      semantics open
  - Closed `B6.9b2b2b2b2b2b2b2a` with unique path-level goal-directed context
    propagation from matching support frames:
    - when the root tuple did not match but exactly one support frame in the
      chosen proof path carried bounded `goal-directed-read-context`, the
      proof path now inherits that same context
    - kept the broader multi-frame merge case open instead of guessing merge
      semantics
  - Closed `B6.9b2b2b2b2b2b2b1` with bounded proof-path goal-directed context
    for matching fact support frames:
    - added a regression proving `deduce/why-result` now also attaches
      path-local `goal-directed-read-context` to matching fact support frames
      inside a derived proof path
    - split the remaining provenance work to broader proof-path integration
      beyond the current root/fact-frame/rule-step row-matching surface
  - Closed `B6.11b2` with explicit runtime/admin truth for the parallel lane:
    - `deduce/analyze`, relation-local `deduce/stats`, and selector-scoped
      `deduce/explain` now expose `parallel-runtime-mode`
    - the shipped value is `metadata-only`, which makes the public boundary
      explicit instead of forcing readers to infer it from
      `execution-engine = 'semi-naive-scc`
  - Closed `B6.11b1b2b` with broader worker-scratch recursive closure for
    positive multi-atom SCC rules:
    - non-anchor recursive atoms in worker-scratch closure now read from
      `LMDB + prior-iteration worker-visible additions`
    - same-iteration worker-emitted tuples stay invisible until the
      iteration closes, preserving the seminaive boundary
    - added a direct C-level Deduce regression proving transitive
      multi-atom worker-scratch closure emits the full serialized fixpoint
      payload
  - Closed `B6.9b2b2b2b2b1` by turning shipped selector-scoped `scan`
    parity into explicit regression/docs truth:
    - selector-scoped `deduce/scan` was already using the bounded complete
      row-set path-local `goal-directed-read-context` capture path
    - added a regression and updated docs so the bounded-complete provenance
      slice no longer incorrectly claims `scan` is plain-only
  - Closed `B6.9b2b2b2b2a` with the first bounded multi-row
    proof-path-integrated goal-directed provenance slice:
    - replaced the old exact-one-only remembered subject key with a bounded
      complete row-set capture for the last goal-directed `query` / `match` /
      `scan-range` / plain `scan`
    - `deduce/why-result` path-local `goal-directed-read-context` now
      attaches when the traced row belongs to that complete captured set,
      while overflow beyond the current `8`-row limit keeps only the
      top-level relation-read context
    - added Deduce regressions for bounded-complete multi-row attachment and
      overflow fallback
  - Added `docs/plans/deduce-parallel-recursive-first-runtime-shape-decision-2026-03-25.md`:
    - chose the first runtime widening as same-wave worker-scratch compute
      for positive non-aggregate recursive SCC batches
    - kept publish and integrity validation on the main thread
    - rejected direct worker-owned LMDB publish as a misleading first slice
  - Updated `TODO.md` and `memory/CHANGELOG.md`:
    - tightened `B6.11b1` to the chosen first runtime shape
  - Closed `B6.11b1a` with the first internal handoff seam for that runtime
    shape:
    - added a versioned serialized component-delta payload for one SCC
      component's signed deltas
    - the payload fits the existing scheduler `OFFLOAD_RES_BYTES` result path
      and carries component id, predicate identity, and opaque encoded tuple
      additions/removals
    - added a direct C-level Deduce regression for payload roundtrip
    - split the remaining runtime work into worker-scratch compute
      (`B6.11b1b`) and main-thread publish/apply (`B6.11b1c`)
  - Closed `B6.11b1b1` with the first actual worker-scratch recursive compute
    slice:
    - added a read-only seminaive scratch-pass helper for positive
      non-aggregate recursive components
    - the helper seeds from the current component snapshot, evaluates one
      pass without LMDB publish, and returns serialized component deltas
    - added a direct C-level Deduce regression proving a recursive rule can
      emit current-snapshot-derived tuples into that payload
    - split the remaining worker-compute lane into multi-iteration scratch
      closure (`B6.11b1b2`) and main-thread publish/apply (`B6.11b1c`)
  - Closed `B6.11b1b2a` with the first multi-iteration worker-scratch
    recursive closure slice:
    - iterated the scratch seminaive path to a fixpoint for the current
      narrow single-recursive-atom positive SCC shape
    - added worker-local visible tuple suppression across iterations so the
      closure can accumulate serialized additions without LMDB publish
    - added a direct C-level Deduce regression proving the scratch closure
      computes a transitive `path` payload containing `(1,2)`, `(2,3)`,
      and `(1,3)`
    - split the remaining worker-closure lane into broader multi-atom
      recursive shapes (`B6.11b1b2b`) and main-thread publish/apply
      (`B6.11b1c`)
  - Closed `B6.11b1c` with the main-thread publish/apply path for
    worker-computed recursive deltas:
    - added `deduce_apply_serialized_component_delta_payload(...)`, which
      validates payload routing, reopens target relations, runs the existing
      encoded-tuple integrity checks, and publishes additions/removals through
      LMDB on the main thread
    - added a direct C-level Deduce regression proving a worker-computed
      recursive closure payload can be deserialized, committed, and observed
      as durable relation rows with updated schema estimates
    - narrowed the remaining parallel-runtime backlog to broader
      worker-scratch closure (`B6.11b1b2b`) and runtime/admin truth
      (`B6.11b2`)
  - Closed `B6.9b2b2b2b2b2a` with the next truthful proof-path provenance
    slice:
    - plain no-op `deduce/query` and `deduce/scan-range` reads now preserve
      the bounded complete subject set for later `why-result` path matching
    - `deduce/why-result` now also attaches path-local
      `goal-directed-read-context` on matching derived support frames, not
      only the root proof path, when the frame’s `(relation, tuple)` pair
      matches that bounded complete last-read subject set
    - added regressions for direct no-op `query` path attachment and derived
      support-frame attachment after a plain no-op `scan-range`
  - Closed `B6.9b2b2b2b2b2b1` with plain no-op `match` parity for bounded
    root-path goal-directed context:
    - switched the plain non-ephemeral `deduce/match` path to the txn-based
      scan helper for bounded subject-key recording, matching the already
      shipped goal-directed row-key capture path
    - added a regression proving `why-result` attaches
      `goal-directed-read-context` on the matching root proof path after a
      plain no-op `deduce/match` read
  - Closed `B6.9b2b2b2b2b2b2a` with truthful selector-scoped path-local
    parity across the shipped row-read shapes:
    - replaced the earlier misleading negative assumption with a mixed-shape
      regression
    - selector-scoped `match` and `scan` now stay truthfully visible on
      path-local `no-op` context
    - selector-scoped `query` and `scan-range` now stay truthfully visible on
      their shipped ephemeral demand paths with the same bounded path-local
      context surface
  - Pruned the non-executable roadmap markers out of `TODO.md`:
    - removed the broad `F0` through `V2-5` milestone labels from the live
      queue
    - kept only concrete actionable Deduce/runtime items in the active TODO
- Validation run
  - doc/queue shaping only
  - architecture basis confirmed from the current tree:
    - recursive fixpoint still walks `component_order` sequentially
    - component evaluators mutate LMDB transactions and shared schema/admin
      state
    - scheduler fanout helpers exist only as a future execution seam

- Objectives attempted
  - Close the parallel-recursion admin-truth slice by pinning that the
    current parallel fields are topology metadata only, not a separate
    runtime execution mode.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_rule_groups_explain.c3`:
    - strengthened the existing parallel explain regression so it now also
      proves selector-scoped `deduce/explain` still reports
      `execution-engine = 'semi-naive-scc` and
      `goal-directed-execution-path = 'selected-component-closure`
      while carrying parallel batch metadata
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.11a2`
    - documented the current fallback/admin truth for parallel topology
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - selector-scoped `deduce/explain` on the representative recursive SCC
      still reports `semi-naive-scc` plus `selected-component-closure`
      while exposing the expected parallel batch metadata

- Objectives attempted
  - Close the first parallel-recursion contract slice by documenting and
    pinning the existing SCC batch scheduling rule instead of leaving the
    topology metadata underspecified.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - strengthened the existing parallel topology regression to pin the
      current `stratum` / `wave` / `batch-size` contract on both
      `deduce/analyze` and relation-local `deduce/stats`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.11a1`
    - documented that the current parallel-recursion surface is
      metadata-only, recursive-components-only, and scheduled by
      same-stratum wave groups
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - representative recursive SCC topology still reports first-batch
      `(stratum = 0, wave = 1, batch-size = 2)` and second-batch
      `(stratum = 0, wave = 2, batch-size = 1)`

- Objectives attempted
  - Close the remaining `check`-specific admin-truth slice by exposing
    dedicated per-class violation counters instead of only generic totals and
    history.
- Code/config changes made
  - Updated `src/lisp/deduce_db_handles.c3`,
    `src/lisp/deduce_db_handles_register.c3`,
    `src/lisp/deduce_db_handles_mutation_tracking.c3`,
    `src/lisp/deduce_schema_query.c3`, and
    `src/lisp/deduce_rule_eval_prims.c3`:
    - added dedicated `check_integrity_violation_count` tracking on relation
      and DB admin state
    - `deduce/stats` and `deduce/analyze` now expose
      `check-integrity-violation-count`
  - Updated `src/lisp/tests_deduce_query_groups.c3`, `TODO.md`,
    `docs/deduce-datalog-spec.md`, `docs/reference/08-libraries.md`, and
    `memory/CHANGELOG.md`:
    - pinned the new per-class admin counters
    - closed `B6.10b2`, completing the current `check` lane
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probes:
    - existing key/unique/reference admin counters stayed unchanged
    - `check` failures now surface `check-integrity-violation-count = 1`
      on both relation-local and DB-wide admin payloads

- Objectives attempted
  - Close the first truthful write-enforcement slice for canonical `check`
    constraints and narrow the residual backlog honestly.
- Code/config changes made
  - Updated `src/lisp/deduce_relation_ops_validation.c3`,
    `src/lisp/deduce_relation_ops_mutations.c3`,
    `src/lisp/deduce_db_handles.c3`,
    `src/lisp/deduce_db_handles_mutation_tracking.c3`,
    `src/lisp/deduce_schema_query.c3`, and
    `src/lisp/tests_deduce_groups.c3` /
    `src/lisp/tests_deduce_query_groups.c3`:
    - immediate `fact!`, derived rule-head publish, and deferred
      `write-deferred` commit-time validation now enforce declared unary
      `check` constraints
    - failed checks now raise deterministic machine-checkable payloads,
      and generic integrity history/admin surfaces now record
      `violation-class = 'check`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.10b1`
    - narrowed `B6.10b2` to dedicated admin counters and summary truth for
      `check` constraints
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probes:
    - direct `fact!` check rejection returned machine-checkable
      `deduce/integrity-check-failed`
    - deferred `write-deferred` commit rejected the bad tuple and rolled
      back cleanly
    - missing check predicates rejected with
      `deduce/check-predicate-missing`
    - relation/DB history surfaced `violation-class = 'check`
    - direct `deduce/analyze` probe confirmed derived check enforcement on
      rule-head publish

- Objectives attempted
  - Close the first truthful schema/admin slice for canonical `check`
    constraints instead of leaving that lane at naming-only status.
- Code/config changes made
  - Updated `src/lisp/parser_define_relation_attr.c3`,
    `src/lisp/parser_define_relation_attr_helpers.c3`,
    `src/lisp/deduce_db_handles.c3`,
    `src/lisp/deduce_db_handles_register.c3`,
    `src/lisp/deduce_schema_query.c3`, and
    `src/lisp/deduce_rule_eval_prims.c3`:
    - relation declarations now lower unary column checks in the form
      `(check predicate column)`
    - relation schemas/admin payloads now persist and expose declared
      `check` metadata
    - `deduce/analyze` now reports DB-wide `check-constraint-count`
  - Updated `src/lisp/tests_deduce_groups.c3` and
    `src/lisp/tests_deduce_query_groups.c3`:
    - added parser/storage coverage for `(check predicate column)`
    - added public schema/analyze regression coverage for the `check`
      payload shape
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.10a2`
    - documented the shipped boundary as declaration/schema/admin baseline
      only, with enforcement still deferred to `B6.10b1`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - declared `check` constraints appear in `deduce/schema`
    - DB-wide `deduce/analyze` reports `check-constraint-count`

- Objectives attempted
  - Retire the last transformed recursive-demand umbrella item once the lane
    was fully covered by explicit shipped support/fallback slices.
- Code/config changes made
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.4f5c2b2b2b` as stale backlog drift
    - rewrote the transformed recursive query contract to say there is no
      separate standing transformed residual beyond the shipped support and
      fallback families already pinned
- Validation run
  - backlog/doc shaping only, based on the already-landed transformed SCC
    support probes plus permutation and transformed multi-atom fallback probes

- Objectives attempted
  - Close the next transformed recursive query-demand hardening slice by
    fixing the false-positive ephemeral path on transformed multi-atom
    recursive shapes.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3` and
    `src/lisp/deduce_schema_query.c3`:
    - detect head rules whose transformed recursive support still spans
      multiple recursive body atoms
    - force truthful fallback for those transformed multi-atom shapes
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for transformed recursive multi-atom
      pair and pair-disjunction demands
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2b2b2`
    - closed `B6.4f5c2b2b2a`
    - kept the broader transformed residual as `B6.4f5c2b2b2b`
- Validation run
  - host-side `./build/main --eval` probes:
    - transformed recursive multi-atom pair query moved to
      `selected-component-closure` with the correct derived row
    - transformed recursive multi-atom pair disjunction moved to
      `selected-component-closure` with the correct derived rows

- Objectives attempted
  - Close the multi-hop follow-up for the transformed recursive
    one-carried-position query-demand lane.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for transformed three-cycle recursive
      pair and pair-disjunction demands
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2b2b`
    - closed `B6.4f5c2b2b1`
    - kept the broader transformed residual as `B6.4f5c2b2b2`
- Validation run
  - host-side `./build/main --eval` probes:
    - transformed three-cycle pair query stayed on
      `ephemeral-head-demand-query` with requested `(0 1)` and applied `(0)`
    - transformed three-cycle pair disjunction stayed on
      `ephemeral-head-demand-query` with requested `(0 1)` and applied `(0)`

- Objectives attempted
  - Close the disjunctive follow-up for the transformed one-carried-position
    recursive query-demand slice.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for reordered mutual-recursive
      disjunctive pair demands
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2b2`
    - closed `B6.4f5c2b2a`
    - kept the broader transformed residual as `B6.4f5c2b2b`
- Validation run
  - host-side `./build/main --eval` probes:
    - reordered mutual-recursive disjunction stayed on
      `ephemeral-head-demand-query` with requested `(0 1)` and applied `(0)`
    - pure recursive permutation disjunction stayed on
      `selected-component-closure`

- Objectives attempted
  - Close the first transformed recursive query-demand slice without breaking
    the existing pure permutation fallback boundary.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3` and
    `src/lisp/deduce_schema_query.c3`:
    - added same-index recursive carrier detection for queried head
      positions across an SCC
    - widened recursive query-demand relaxation so transformed recursive SCC
      shapes can keep one carried applied position when that carrier exists
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for reordered mutual-recursive pair
      demands, asserting requested `(0 1)` with one applied carried position
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2b`
    - closed `B6.4f5c2b1`
    - kept the broader transformed residual as `B6.4f5c2b2`
- Validation run
  - host-side `./build/main --eval` probes:
    - reordered mutual-recursive query moved to
      `ephemeral-head-demand-query` with requested `(0 1)` and applied `(0)`
    - pure recursive permutation still stayed on `selected-component-closure`

- Objectives attempted
  - Close the next recursive query-demand slice by pinning multi-hop
    same-index SCC support instead of leaving deeper positive cycles implicit.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for fully-bound same-index
      three-relation recursive SCC queries
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2`
    - closed `B6.4f5c2a`
    - kept the remaining transformed-demand residual as `B6.4f5c2b`
- Validation run
  - host-side `./build/main --eval` probe:
    - three-relation same-index recursive SCC query stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`

- Objectives attempted
  - Close the next recursive query-demand slice by pinning same-index
    mutual-recursive disjunctive support instead of leaving it buried under
    a broader residual item.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for same-index mutual-recursive
      disjunctive pair queries
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c`
    - closed `B6.4f5c1`
    - kept the broader residual as `B6.4f5c2`
- Validation run
  - host-side `./build/main --eval` probes:
    - selector mutual-recursive disjunction stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`
    - plain mutual-recursive disjunction stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`

- Objectives attempted
  - Close the next recursive query-demand slice by widening truthful
    multi-position support from same-index self recursion to same-index
    mutual-recursive SCCs.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3`:
    - widened the same-index recursive demand gate to accept SCC-local
      positive recursive body atoms that preserve all requested positions
      together at the same indices
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for fully-bound same-index
      mutual-recursive pair queries
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.4f5b`
    - promoted the remaining residual to `B6.4f5c`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probes:
    - selector mutual-recursive pair query stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`
    - plain mutual-recursive pair query stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`

- Objectives attempted
  - Close the first goal-directed provenance slice without claiming
    proof-path-integrated planner semantics.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - `deduce/why-result` now exposes optional top-level
      `goal-directed-read-context` metadata built from the relation's existing
      last goal-directed read state
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a regression that forces a goal-directed recursive query, then
      checks that `why-result` exposes the mirrored read context even on a
      `missing` row payload
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2` into `B6.9b2a` and `B6.9b2b`
    - closed `B6.9b2a`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(missing 0 ephemeral-head-demand-query query nil 1 1 0)` for
      the goal-directed read-context slice
    - existing recursive and non-recursive why-result probes stayed intact

- Objectives attempted
  - Close the next recursive provenance slice by moving positive recursive
    why-result payloads beyond flattened fact-only support.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - recursive why-result support search now appends a `rule-step` frame when
      a derived child row is used in the chosen proof path
    - recursive why-result depth accounting now reports the deeper derived step
      in `max-depth`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - tightened the recursive why-result regression to pin `rule-step` support
      frames and `max-depth = 3` on recursive closure rows
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9b1b`
    - narrowed the remaining provenance queue to goal-directed semantics only
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probes:
    - recursive closure payload now includes a trailing `rule-step` frame with
      `(rule-index 0, step-index 1)` on the chosen child proof
    - recursive closure rows now report `max-depth = 3`
    - the existing non-recursive multi-rule why-result probe still returned
      `(ok partial true)`

- Objectives attempted
  - Close the first recursive provenance slice without widening to
    goal-directed lineage or nested recursive-step payloads.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened `deduce/why-result` support search so derived child rows recurse
      back through the shipped row-subject provenance helper surface
    - added a visited-subject guard on `(predicate, tuple)` to keep recursive
      support search finite
    - recursive closure rows now return `ok` for one support chain and
      `partial` for multiple support chains, while keeping the current
      flattened fact-support payload shape
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a dedicated recursive why-result regression for direct, unique
      recursive, and partial recursive closure rows
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split broad recursive provenance into `B6.9b1a` and `B6.9b1b`
    - closed `B6.9b1a` at the first positive recursive closure slice
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probes:
    - returned `(ok partial true)` for the existing non-recursive multi-rule
      why-result probe after the recursive helper refactor
    - returned `(ok 1 nil 1 1 2 nil)` for a unique recursive closure row
    - returned `(partial 1 true 1 1 2 nil)` for a multi-path recursive closure
      row

- Objectives attempted
  - Close the first multi-rule non-recursive provenance slice without widening
    to recursive or goal-directed lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened `deduce/why-result` to aggregate already-shipped non-recursive
      provenance support across multiple matching head rules
    - multi-rule rows with one supported path now return `ok`, and rows with
      multiple supported paths return `partial` with the first deterministic
      path
    - one-rule exact-one-rule lineage still stays on the older proven branch
      instead of going through the aggregate path
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to cover both the direct multi-rule
      `ok` case and the mixed-body multi-rule `partial` case
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a8`
    - narrowed the remaining provenance queue to recursive and goal-directed
      semantics only
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probes:
    - returned `(ok 1 nil 1 1 1 nil)` for the direct multi-rule row
    - returned `(partial 1 true 1 2 2 nil)` for the mixed-body multi-rule row

- Objectives attempted
  - Close the first mixed-body non-recursive provenance slice without widening
    to multi-rule lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened `deduce/why-result` to compose exact-one-rule non-recursive
      mixed-body lineage through already-supported exact-one-rule child
      provenance helpers
    - exact-one-rule mixed-body lineage now returns `ok` for one support chain
      and `partial` when multiple support chains exist
    - multi-rule non-recursive lineage still returns
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to include an exact-one-rule mixed-body
      supported case while keeping the multi-rule non-recursive case
      unsupported
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a7`
    - promoted the remaining multi-rule non-recursive lineage work into
      `B6.9a8`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(partial true partial 2 error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the first search-based non-recursive provenance slice for exact-one-
    rule extensional lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - added exact-one-rule extensional support search for `deduce/why-result`
      on non-recursive derived rows
    - reused the existing positive-body tuple matcher plus tuple iterators to
      search extensional supports rather than introducing a separate provenance
      matcher
    - exact-one-rule extensional lineage now returns `ok` for one deterministic
      path and `partial` with `truncated = true` when multiple support paths
      exist
    - mixed-body or multi-rule non-recursive derived lineage still returns
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to cover search-based extensional
      lineage with both `ok` and `partial` outcomes
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a6`
    - promoted the remaining mixed-body or multi-rule non-recursive lineage
      work into `B6.9a7`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(ok partial true error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the next non-recursive provenance slice for exact-one-rule
    extensional multi-body lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - generalized the public derived-lineage matcher from one-body only to
      exact-one-rule extensional derivations where every support tuple is
      reconstructible from the head row
