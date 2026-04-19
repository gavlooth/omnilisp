# Memory Changelog Index Part 21

Source: `memory/CHANGELOG.md`

## Archive

Older sessions are archived in [archive/CHANGELOG_ARCHIVE_2026-03-08.md](archive/CHANGELOG_ARCHIVE_2026-03-08.md).

## 2026-03-21

- Removed non-canonical public constructor/mutator aliases from the collection
  and lazy-sequence surface:
  - removed public lowercase constructor aliases `array`, `dict`,
    `dictionary`, `coroutine`, and `time-point`
  - removed public type-prefixed mutator aliases `array-set!` and
    `dict-set!` in favor of generic `set!`
  - removed public `iterator` as an alias of `Iterator`
  - hid the low-level iterator thunk helper behind internal `__make-iterator`
    and kept `Iterator` as the only public iterator constructor
  - removed redundant iterator forcing helpers `collect` and `to-array`; use
    `List` / `Array` constructors instead
  - kept `Dictionary` as the canonical name
  - parser/compiler lowering for array and dict literals now targets canonical
    `Array` / `Dictionary` constructors directly

- Removed the remaining binary-only ergonomics trap from the core logic
  surface:
  - `and` and `or` now parse as variadic short-circuit special forms
  - empty forms lower to the expected identities: `(and)` => `true`,
    `(or)` => `nil`
  - single-argument forms return the single argument unchanged
  - macro-emitted variadic `and` / `or` forms now validate and lower through
    the same nested binary AST path as direct source syntax
  - regression coverage now pins empty, single, multi-argument, falsy
    short-circuit, and macro-emitted variadic forms

- Shipped the current `B6.2` Deduce constraint/integrity slice and closed the
  current escape-scope regression lane:
  - declared single-column `key` relations now reject conflicting `fact!`
    writes at assert time while still treating identical full-tuple reasserts
    as idempotent
  - declared single-column `unique` relations now reject conflicting `fact!`
    writes at assert time while still treating identical full-tuple reasserts
    as idempotent
  - declared multi-column `unique` roles on one relation now form enforced
    composite unique constraints at assert time while still treating identical
    full-tuple reasserts as idempotent
  - declared single-column `ref` roles now enforce candidate-key references:
    the target relation/column pair must resolve and the target column must be
    a declared `key` or single-column `unique`
  - integrity enforcement is no longer purely extensional: non-aggregate rule
    heads on declared `key` / `unique` relations now enforce the same checks
    on derived writes, including recursive seminaive materialization
  - aggregate rule heads on declared `key` / `unique` relations now also
    publish through the same derived-write integrity checks, including
    recursive seminaive aggregate materialization
  - declared `ref` rule heads now validate against the final component
    transaction snapshot before commit, so derived ordinary and aggregate
    rule-head publish no longer relies on unsound eager per-tuple `ref`
    checks
  - conflict raises now use machine-checkable payload data under
    `deduce/integrity-key-conflict`,
    `deduce/integrity-unique-conflict`, and
    `deduce/integrity-reference-missing`, including relation, constrained
    column set / constrained value projection, referenced target metadata,
    and existing/attempted tuples where applicable
  - `deduce/schema` now exposes keyed/unique/reference integrity constraint
    metadata, including `unique-columns` for composite unique constraints, and
    `deduce/analyze` now reports keyed relation, unique relation, reference
    constraint, and integrity constraint counts
  - `deduce/stats` now reports relation-local integrity violation totals,
    per-class violation counts, and `last-integrity-violation-code`
  - `deduce/analyze` now reports DB-wide integrity violation totals,
    per-class counts, and `last-integrity-violation-code` /
    `last-integrity-violation-relation`
  - `deduce/stats` and `deduce/analyze` now both expose bounded
    `recent-integrity-violations` history, newest first, so write-side/admin
    inspection is no longer limited to counters plus last-code snapshots
  - explicit `write-deferred` transactions now defer the current `fact!`-side
    key/unique/reference checks to commit-time validation over touched
    inserted relations, with exact rollback on failed deferred commit
  - `write-deferred` transactions now also defer delete-side reference
    protection to commit-time snapshot validation, so staged target deletes can
    proceed until commit and are either rejected or accepted against the final
    transaction-visible state
  - `retract!` now rejects deleting referenced target tuples with
    `deduce/integrity-reference-target-in-use`, while write transactions allow
    the target delete after the blocking reference has already been removed in
    the same transaction-visible snapshot
  - `clear!` and `drop!` now enforce the same target-side reference protection
    as `retract!`, rejecting bulk deletion when live referencing tuples still
    exist
  - `deduce/rule!` now accepts keyed, single-column unique, and composite
    unique constrained head relations across both ordinary and aggregate rule
    heads, and the derived-write regression surface now pins recursive keyed
    materialization, aggregate keyed publish success, and machine-checkable
    key/unique conflict payloads
  - `deduce/rule!` now accepts declared `ref` constrained head relations
    across both ordinary and aggregate rule heads
  - `deduce/schema` and `deduce/analyze` now expose `target-unique` for
    reference constraints, and writes against non-unique target columns now
    raise `deduce/integrity-reference-target-not-unique`
  - dropped target relations no longer appear resolved in `deduce/schema` /
    `deduce/analyze`; reference metadata now exposes `target-live` and
    unresolved counts include dropped targets
  - bounded Deduce validation is green at `169 passed, 0 failed` after the
    final `ref` rule-head closure
  - the explicit `escape-scope: iterator consume car` and
    `escape-scope: iterator consume len` regressions are fixed, and the bounded
    `escape-scope` slice is green again after moving the larger named-let list
    accumulator pressure back into the dedicated `memory-lifetime` budget lane
  - validation for this slice:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `165 passed, 0 failed`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=escape-scope ./build/main --test-suite lisp`
    - result: `29 passed, 0 failed`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime ./build/main --test-suite lisp`
    - result: `62 passed, 0 failed`

- Removed association-list lookup from the public language surface and logged
  the remaining unrelated runtime follow-up:
  - dotted path lookup now uses real dictionaries instead of alist fallback,
    and `assoc` / `assoc-ref` are removed from the documented stdlib surface
  - the initial broad bounded sweep exposed `escape-scope` iterator consume
    failures in `src/lisp/tests_escape_scope_tests.c3`, now recorded in the
    backlog instead of being left implicit in the dirty tree:
    - `escape-scope: iterator consume car` ->
      `car: argument must be a pair`
    - `escape-scope: iterator consume len` ->
      `length: expected list, array, dict, set, or string`
  - validation for the alist-removal lane:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
    - result: `142 passed, 0 failed`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
    - result: `1082 passed, 0 failed`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    - result: `122 passed, 0 failed`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `121 passed, 0 failed`

- Deduce recursive aggregate seminaive rollout is now complete for the current
  stratified rule surface:
  - recursive aggregate SCCs with lower-stratum negated body atoms now use
    `semi-naive-scc` instead of the old grouped naive fallback
  - the last automatic `recursive-aggregate-naive-fallback` lane is retired
    for currently valid recursive aggregate workloads
  - query/explain regressions now pin exact seminaive behavior for positive
    and negated mixed aggregate SCCs, while tail routing checks assert
    seminaive exactness directly instead of treating naive proof-multiplicity
    counts as the correctness oracle for mixed non-aggregate heads
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `121 passed, 0 failed`
  - `deduce/analyze` now routes explicit naive requests for recursive SCCs
    that mix aggregate and non-aggregate rules onto the exact seminaive lane
    and reports that via
    `naive-recursive-aggregate-mixed-routed-to-seminaive`
  - `deduce/explain` now mirrors that routing truth in its optional
    3-argument engine-aware form `(deduce/explain db selector engine)`
  - explain-side regression coverage now also pins the pure positive and pure
    negated recursive aggregate cases: explicit `naive` stays on `naive-scc`
    and does not emit the mixed-route diagnostic
  - pure positive and pure negated recursive aggregate analyze/explain
    regressions now both pin that same no-route behavior

## 2026-03-20

- Deduce backlog item `B5.6` is now complete:
  - grouped aggregate execution for rule heads now ships for exact
    `count` / `sum` / `min` / `max` in both non-recursive rules and
    aggregate-bearing recursive SCCs
  - recursive aggregate SCCs request the semi-naive engine but fall back to a
    sound naive grouped recompute path until aggregate delta semantics exist
  - recursive aggregate finalize now diffs the materialized grouped head rows
    before rewrite so recursive grouped passes converge instead of churning
  - aggregate-only shared heads with matching projection signatures now batch
    through one grouped finalize pass, while mixed or incompatible shapes
    still reject at install time
  - `deduce/analyze` now reports:
    - `execution-engine`
    - `requested-execution-engine`
    - `recursive-aggregate-naive-fallback`
  - regression coverage now includes:
    - recursive aggregate install/execution acceptance
    - recursive aggregate explain/analyze truthfulness
    - naive versus semi-naive parity coverage with recursive aggregate
      fallback semantics
    - bounded recursive aggregate benchmark smoke
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `97 passed, 0 failed`

- Deduce backlog planning now promotes the next stage toward full recursive
  aggregate seminaive support:
  - the shipped recursive aggregate baseline remains the naive grouped
    recompute fallback from `B5.6`
  - the next explicit rollout stages are the signed-delta and
    support-accounting work needed to retire that fallback safely
  - active backlog/docs now spell out that staged path instead of leaving the
    remaining work as an unnamed deferred note
  - validation: doc review only

- Deduce backlog item `B5.7` has started landing its first runtime slice:
  - seminaive recursive delta state now carries signed add/remove tuple slots
    in the shared execution substrate instead of a single positive-only table
  - aggregate-bearing recursive SCCs now seed tuple support tables and
    sign-aware head-materialization hooks behind the fallback gate
  - dormant seminaive aggregate finalize now routes grouped head replacement
    through support-table zero-crossing transitions on actual materialized
    tuples instead of direct LMDB rewrite plus synthetic aggregate deltas
  - aggregate groups now track signed support counts internally, including
    removal-aware `count`/`sum` updates and `min`/`max` extremum recompute
    when the current support disappears
  - the dormant seminaive aggregate executor now runs mixed non-aggregate
    rules in the same recursive SCC on the same signed-delta/support-table
    substrate instead of skipping them outright
  - positive recursive aggregate SCCs now use the `semi-naive-scc`
    execution lane again, including mixed aggregate/non-aggregate recursive
    SCCs that stay within positive body atoms
  - mixed positive recursive aggregate regressions now pin exact seminaive
    behavior and truthful analyze/explain reporting
  - focused tuple-support and aggregate-group regressions now pin
    recursive-only, base-seeded, signed count-removal, and `min`-recompute
    behavior independently of the higher-level fallback fixtures
  - aggregate-bearing recursive SCCs no longer all share one blanket
    fallback: positive supported components now run seminaive, while
    recursive aggregate components with negated body steps still stay on the
    documented fallback until that remaining shape is validated explicitly
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `103 passed, 0 failed`
  - aggregate seminaive proof dedupe now keys off normalized variable
    bindings plus delta polarity before support application, so dormant
    multi-anchor replay no longer double-applies the same logical proof's
    support delta across anchor positions
  - asserted Deduce facts are now mirrored into per-relation extensional
    shadow DBIs so recursive aggregate substrate work can distinguish base
    tuples from derived tuples during internal reset/reseed paths
  - the dormant recursive aggregate seminaive seed path now clears component
    predicate relations, restores extensional base tuples from that shadow,
    seeds support tables from extensional state, and publishes the same base
    tuples into the first signed-delta frontier instead of reseeding from live
    mixed relations
  - focused regression coverage now pins that proof-key behavior directly
    without widening the public recursive aggregate engine contract
  - focused regression coverage now also pins extensional shadow mirror
    behavior for assert/retract/clear
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `107 passed, 0 failed`

- Deduce backlog items `B5.4` and `B5.5` are now complete:
  - grouped aggregate execution for rule heads now ships on the checked-in
    Deduce runtime for exact non-recursive:
    - `count`
    - `sum`
    - `min`
    - `max`
  - the shared naive evaluation path now accumulates grouped aggregate state,
    clears the exclusive aggregate head relation on recompute, and finalizes
    deterministic grouped rows back into LMDB
  - `deduce/analyze` no longer rejects supported aggregate-bearing rules, and
    `deduce/explain` now also collects observed step counters for aggregate
    rules through the dry-run path
  - install-time guardrails now reject unsupported aggregate shapes explicitly:
    - mixed aggregate/non-aggregate shared heads reject
    - aggregate-only shared heads with incompatible projection signatures
      reject
  - regression coverage now includes:
    - grouped runtime correctness for `count` / `sum` / `min` / `max`
    - explain/analyze parity for aggregate rules after grouped execution lands
    - explicit validation rejection for unsupported shared-head aggregate
      shapes
  - benchmark-mode smoke now seeds and analyzes an aggregate workload in the
    Deduce benchmark helpers
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `93 passed, 0 failed`

- Deduce v1.5 maintenance/admin work has started shipping on the public
  surface:
  - added read-only admin/introspection verbs:
    - `deduce/schema`
    - `deduce/indexes`
    - `deduce/stats`
  - the new surfaces are relation-handle scoped and return deterministic
    payloads built from existing Deduce metadata rather than inferred state
  - `deduce/schema` now reports:
    - relation symbol
    - columns
    - arity
    - index count
    - cardinality and distinct estimates
    - dropped/database-open flags
  - `deduce/indexes` now reports registered logical index descriptors with:
    - index id
    - ordered columns
    - column count
    - uniqueness flag
  - `deduce/stats` now reports:
    - cardinality estimate
    - distinct estimate
    - dirty predicate count
    - mutation epoch
    - full-recompute-required flag
    - dropped flag
  - deterministic regression coverage now exercises the new schema/index/stats
    surfaces in `src/lisp/tests_deduce_query_groups.c3`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `83 passed, 0 failed`

- Deduce v1.5 aggregate queue promotion is now underway:
  - the first live aggregate item is complete as a syntax/design lock
  - `docs/deduce-datalog-spec.md` now pins the deferred aggregate query shape
    as projection-driven syntax:
    - bare variables imply group-by keys
    - supported aggregate forms are `count`, `sum`, `min`, `max`
  - the concrete implementation seam map for the next patches is recorded in:
    - `docs/plans/deduce-v15-aggregate-implementation-plan-2026-03-20.md`
  - `TODO.md` now promotes the v1.5 aggregate lane into the active Deduce queue

- Deduce backlog item `B5.1` is now complete:
  - aggregate v1.5 entry policy is now explicit across the Deduce roadmap/spec:
    - v1.5 target: grouped exact aggregates with small deterministic state
      (`count`, `sum`, `min`, `max`)
    - deferred: `avg` and `distinct-count`
    - non-goal for v1.5: percentile/median, variance/stddev, ordered-set
      aggregates, approximate/sketch aggregates, and user-defined aggregate
      hooks
  - the policy is recorded in:
    - `docs/deduce-datalog-spec.md`
    - `docs/plans/deduce-full-roadmap-2026-03-20.md`
    - `docs/plans/deduce-actionable-backlog-2026-03-20.md`
  - validation: doc review only

- Deduce backlog item `B5.3` is now complete as a design item:
  - the canonical provenance / why-result payload shape is now specified in
    `docs/deduce-datalog-spec.md`
  - the payload uses:
    - `kind = why-result`
    - `status`
    - `surface-kind = provenance-snapshot`
    - `subject-kind`
    - `subject`
    - `path-count`
    - `max-depth`
    - `truncated`
    - ordered `paths[]`
  - each path is a deterministic support chain with:
    - `path-id`
    - `path-kind`
    - `head-predicate`
    - `head-tuple`
    - `rule-index`
    - `support`
  - each support frame carries:
    - `kind`
    - `frame-index`
    - `predicate`
    - `tuple`
    - `rule-index`
    - `step-index`
    - `selected-index`
    - `operator`
    - `bound-mask`
    - `bindings`
    - `depends-on`
  - roadmap and backlog notes now treat provenance / why-result as a documented
    design item while implementation remains deferred

- Deduce backlog item `B3.3` is now complete:
  - `deduce/explain` now replaces planner-estimate step counters with
    runtime-observed counters while keeping the existing planner snapshot shape
    (`steps[*].counters`) in
    `src/lisp/deduce_rule_ops_explain.c3`
  - per-step counter payload now includes:
    - `rows-read`
    - `rows-emitted`
    - `index-hits`
    - `index-misses`
    - `join-probes`
    - `counter-kind = 'observed`
  - the Deduce evaluator now accumulates per-step observed counters through the
    shared naive/semi-naive runtime paths in:
    - `src/lisp/deduce_rule_eval_exec.c3`
    - `src/lisp/deduce_rule_eval_exec_naive.c3`
    - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
    - `src/lisp/deduce_rule_eval_fixpoint.c3`
  - `deduce/analyze` now exposes runtime-observed per-rule step payloads under
    `rule-execution` in `src/lisp/deduce_rule_eval_prims.c3`
  - regression coverage now asserts observed explain/analyze counters in:
    - `src/lisp/tests_deduce_rule_groups_explain.c3`
    - `src/lisp/tests_deduce_rule_groups_more_tail.c3`
    - `src/lisp/tests_deduce_query_bench_groups.c3`
  - validation:
    - `c3c build`
    - bounded Deduce slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`80 passed, 0 failed`)
