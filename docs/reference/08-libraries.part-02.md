## 28. Deduce (Database)

Deduce is an embedded relational database backed by LMDB.

### Open a Database

```lisp
(define db (deduce 'open "app.db"))       ;; persistent
(define tmp (deduce 'open 'memory))        ;; ephemeral

;; explicit LMDB named DB exposure for relation storage
;; (relation symbol remains language-facing; db-name selects LMDB dbi name)
(define events (deduce 'open-named db 'event-log "events_v1" 'ts 'kind 'payload))
```

### Define Relations

```lisp
(define [relation db] person
  (^String name) (^Integer age) (^String email))

(define [relation db] edge (from to))
```

### Assert and Retract Facts

```lisp
(deduce 'fact! person "Alice" 30 "alice@b.com")
(deduce 'fact! person "Bob" 25 "bob@b.com")
(deduce 'fact! edge "A" "B")

(deduce 'retract! person "Alice" 30 "alice@b.com")
```

`fact!`, `retract!`, `clear!`, and `drop!` are command-style operations and
return `Void` on successful completion. Delete-side commands reject removing a
target tuple or relation contents while that target is still referenced by a
declared `ref` constraint.

### Transactions

```lisp
;; explicit write transaction
(define tx (deduce 'block db))
(deduce 'fact! tx person "Carol" 44 "carol@b.com")
(deduce 'retract! tx edge "A" "B")
(deduce 'commit tx)

;; explicit read transaction (for future read APIs that accept txn handles)
(define ro (deduce 'block db 'read))
(deduce 'abort ro)

;; explicit deferred write transaction
(define txd (deduce 'block db 'write-deferred))
(deduce 'fact! txd person "Dana" 29 "dana@b.com")
(deduce 'commit txd)
```

`commit` and `abort` return `Void` on success. `write-deferred` is a narrow
constraint mode: it defers the current `fact!`-side key/unique/reference
checks and final delete-side reference validation to commit-time snapshot
validation, rather than widening transaction semantics generally.

The next widened integrity class is fixed as canonical `check`. Relation
declarations now accept unary column checks in the form
`(check predicate column)`. The current shipped slice includes
declaration/schema/admin plus first write-enforcement support:
`deduce/schema` exposes `kind = 'check`, `predicate`, `columns`, and
`enforced = false`, `deduce/analyze` reports DB-wide
`check-constraint-count`, immediate `fact!`, derived rule-head publish, and
deferred `write-deferred` commit-time validation reject bad tuples, and the
generic integrity history surfaces now expose `violation-class = 'check` with
deterministic failure codes. Relation-local `deduce/stats` and DB-wide
`deduce/analyze` now also expose dedicated `check-integrity-violation-count`
counters. Aliases such as `assert`, `predicate`, and `guard` are not part of
the planned surface.

For the current mutation-log surface:

- write-block mutations do not touch dirty/admin state before `commit`
- `abort` drops the pending mutation log without changing row counts or
  incremental dirty metadata
- committed non-destructive base writes keep
  `incremental-invalidation-mode = 'tracked`
- committed destructive writes such as `retract!` are where the current
  surface escalates to `full-recompute`
- plain DB-level `deduce/analyze` is the current recovery boundary for that
  degraded mode: the returned payload still reports the observed
  `full-recompute` dirty frontier, but the successful analyze run clears the
  live dirty/admin state afterward

### Relation Maintenance

```lisp
;; remove all rows, keep relation schema/handle valid
(deduce 'clear! person)

;; drop relation backing store; existing relation handle becomes invalid for scans/writes
(deduce 'drop! person)
```

`clear!` and `drop!` return `Void` on success.

The canonical cleanup/admin verb matrix now lives in:

- `docs/deduce-datalog-spec.md#74-cleanup-verb-matrix`

That section is the source of truth for:

- the only approved cleanup verb names
- which verbs currently accept explicit transaction handles
- deterministic no-op behavior for missing tuples, empty relations, and
  repeated drops
- the current cleanup rejection codes

### Materialized Views

```lisp
(define db (deduce 'open 'memory))
(define [relation db] edge (src) (dst))
(define [relation db] ancestor (src) (dst))

(deduce 'fact! edge 1 2)
(deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y))

;; explicit opt-in for derived relations only
(deduce/materialize! ancestor)

;; diagnostics do not refresh the view
(deduce/analyze db)

;; refresh is explicit in the current shipped slice
(deduce/refresh! db)
(deduce/refresh! ancestor)

;; lifecycle teardown is explicit too
(deduce/dematerialize! ancestor)

(deduce/schema ancestor)
(deduce/indexes ancestor)
(deduce/stats ancestor)
```

Current shipped contract:

- only derived relations can be materialized
- `deduce/materialize!` is the explicit relation-level command for enabling a
  derived relation's materialized state immediately
- manual materialization can be declared directly with
  `[relation db materialized] rel ...`
- the same current manual policy can also be declared explicitly with
  `[relation db materialized manual] rel ...`
- declaration-time materialization also ships with
  `[relation db materialized on-read] rel ...`
- declaration-time materialization currently has two shipped policy contracts:
  `[relation db materialized]` is shorthand for
  `[relation db materialized manual]`, and `on-read` triggers refresh before
  ordinary stored-tuple reads on stale derived materialized relations
- after file-backed reopen, stale `on-read` relations now auto-refresh again
  when their persisted executable signatures are restorable on the current
  supported surface
- unsupported persisted signature shapes still fail those reads truthfully
  instead of pretending stale snapshots are fresh
- later approved but still unshipped trigger-shaped names are
  `on-base-commit`, `on-open`, and `scheduled`
- unknown declaration-time refresh policies reject deterministically
- declaration-based materialization is allowed before rules exist, but it is
  not refreshable until the relation actually has derived rule heads
- refresh is explicit and accepts either a Deduce DB handle or a materialized
  relation handle
- `deduce/dematerialize!` is the explicit relation-level teardown command that
  clears materialized intent without dropping the relation or its rules
- rule-install invalidation is selective: only already-ready materialized
  views become stale, while declared-but-unready materialized views keep
  their existing `never-refreshed` lifecycle
- the canonical maintenance analytics field matrix for:
  - relation-local `deduce/stats`
  - DB-level `deduce/analyze`
  - `deduce/refresh!`
  - selector failure payload keys
  now lives in
  `docs/deduce-datalog-spec.md#73-maintenance-analytics-payload-baseline`
  and this reference intentionally summarizes behavior instead of duplicating
  field inventories
- in the current shipped slice:
  - `deduce/schema` is the schema-oriented surface that includes
    `materialized-refresh-policy`
  - `deduce/indexes` is the relation-local index-inspection surface for key
    columns, uniqueness, and supporting index descriptors
  - `deduce/stats` is the relation-local maintenance surface for dirty
    frontier, integrity history, materialized lifecycle state, parallel SCC
    batch topology, and last goal-directed read metadata
  - `deduce/analyze` is the DB-level maintenance surface for dirty frontier,
    materialized view counts/lists, planner topology, integrity summaries, and
    last goal-directed read metadata
- DB-handle refresh still reuses the DB-wide analyze/fixpoint path
- DB-handle refresh only stamps ready materialized views as refreshed
- relation-scoped refresh now evaluates the target relation’s dependency
  closure in tracked mode and leaves unrelated stale materialized views alone
- relation-scoped refresh rejects declared-but-unready materialized relations
  with `deduce/refresh-materialized-relation-not-derived`
- if the DB has already escalated to `full-recompute-required`, relation-scoped
  refresh falls back to the DB-wide path
- refresh payloads now report which path actually ran:
  - targeted relation refresh reports
    `refresh-scope == 'relation`,
    `refresh-execution-path == 'targeted`,
    and `refresh-fallback-reason == nil`
  - DB refresh reports
    `refresh-scope == 'database`,
    `refresh-execution-path == 'db-wide`,
    and `refresh-fallback-reason == nil`
  - relation-handle refresh that escalates under
    `full-recompute-required` reports
    `refresh-scope == 'relation`,
    `refresh-execution-path == 'db-wide`,
    and `refresh-fallback-reason == 'full-recompute-required`
- refresh payloads now also report materialized-view identities, not only
  counts:
  - `refreshed-materialized-relations`
  - `remaining-stale-materialized-relations`
  - relation-scoped refresh also preserves
    `requested-refresh-relation`
- refresh payloads now also report dirty-frontier changes, not only counts:
  - `cleared-dirty-predicates`
  - `remaining-dirty-predicates`
- materialized intent and refresh-history metadata survive reopen /
  `open-named` for file-backed DBs, and the current supported persisted
  executable-signature slice now restores truthful ready/fresh behavior for
  reopened derived materialized relations
- the last stored materialized snapshot also survives reopen in relation
  storage, so `deduce/count` can still observe that snapshot across restart
- a compact persisted rule/dependency catalog summary now survives reopen, so
  reopened `deduce/analyze` keeps the last installed `rule-count` and
  `incremental-dependency-edges`
- supported persisted executable rule signatures now survive reopen through
  stable catalog-name remapping plus inferred predicate-schema restore, so
  stale `on-read` rebuilds and explicit `deduce/refresh!` can execute again
  without reinstalling those current supported rules
- unsupported persisted signature shapes still fall back to summary-only
  admin truth and do not claim live execution support after reopen
- reopened schema/admin payloads now also retain:
  - `materialized-refresh-count`
  - `materialized-last-refresh-mutation-epoch`
  - `materialized-last-stale-reason`
  - `materialized-last-stale-mutation-epoch`
- reopened materialized views now stay stale while that persisted stale
  reason is non-`nil`; they do not silently become fresh just because the
  live dirty frontier starts empty
- explicit dematerialize clears that persisted materialized lifecycle record
  again, so a later reopen does not resurrect a manually torn-down view
- materialized views that have never successfully refreshed now expose the
  stale reason `never-refreshed` instead of leaving the stale-reason field blank
- installing a new rule after a view has been refreshed invalidates that view
  with `materialized-last-stale-reason == 'rule-set-change` and forces the
  existing `full-recompute-required` fallback until the next refresh
- committed base mutations can make a materialized relation stale again until
  the next `deduce/refresh!`, with the current stale reason surfaced as
  `dependency-dirty`
- `deduce/analyze` now also exposes goal-directed planner counts for recursive
  components:
  - `goal-directed-recursive-component-count`
  - `goal-directed-eligible-component-count`
  - `goal-directed-blocked-component-count`
  - `goal-directed-aggregate-blocked-component-count`
  - `goal-directed-negated-blocked-component-count`
- `deduce/analyze` also exposes `goal-directed-components`, a recursive
  component summary list carrying component id, stratum, rule count,
  eligibility, shape, blockers, and aggregate/negation presence flags
- `deduce/analyze`, relation-local `deduce/stats`, and selector-scoped
  `deduce/explain` now also expose parallel recursive batch metadata:
  - `parallel-recursive-component-count`
  - `parallel-batched-component-count`
  - `parallel-batch-count`
  - `parallel-max-batch-size`
  - `parallel-batch-topology`
  - `parallel-runtime-mode`
  The current contract is metadata-only and deterministic:
  - only recursive SCC components participate
  - batching is computed within each recursive stratum
  - `wave = 1` means no same-stratum recursive dependency
  - higher waves are one-based longest same-stratum dependency distance from
    that boundary
  - components with the same `(stratum, wave)` may batch together
  - `parallel-runtime-mode = 'metadata-only`
  - this does not yet mean runtime recursive evaluation executes in parallel
  - the first shipped runtime seam under that metadata is internal-only:
    a versioned serialized component-delta payload for one recursive SCC
    component's signed deltas, intended for scheduler byte-result handoff
  - the next shipped internal slice is a single read-only seminaive scratch
    pass for positive non-aggregate recursive components, returning
    serialized candidate additions from the current component snapshot
  - that scratch path now also supports multi-iteration closure for positive
    non-aggregate recursive SCC rules, including multi-atom recursive rule
    shapes via LMDB plus prior-iteration worker-visible overlay reads on
    non-anchor recursive atoms
  - the next shipped internal slice now applies those serialized
    worker-computed deltas on the main thread through the existing integrity
    validation and LMDB write path
  - broader worker-side recursive batch compute is still follow-up work
  - selector-scoped `deduce/explain` therefore still reports the ordinary
    `execution-engine = 'semi-naive-scc` and
    `goal-directed-execution-path = 'selected-component-closure`
- `deduce/analyze` now also supports selector-scoped execution for the
  currently eligible recursive shape through:
  - `(deduce/analyze db rule-index)`
  - `(deduce/analyze db rule-index 'naive)`
  - `(deduce/analyze db rule-index 'semi-naive)`
  Eligible selectors execute only the selected component dependency closure
  and report:
  - `goal-directed-execution-path = 'selected-component-closure`
  - `goal-directed-selector-rule-index`
  - `goal-directed-selected-component-id`
- selector-scoped `deduce/analyze` remains planner-side at the top level even
  after a runtime read:
  - top-level `goal-directed-*` selector fields describe the selected planner
    closure
  - `last-goal-directed-read-*` mirrors the last actual DB-level read path
  - `rule-execution[*].steps[*].counters.counter-kind = 'observed` reports
    runtime-observed counters per analyzed rule entry
- `deduce/explain` now also mirrors the last actual goal-directed read
  metadata for the selected head relation:
  - `last-goal-directed-read-execution-path`
  - `last-goal-directed-read-surface`
  - `last-goal-directed-read-fallback-reason`
  - `last-goal-directed-read-selector-rule-index`
  - `last-goal-directed-read-mutation-epoch`
  - `last-goal-directed-read-requested-bound-count`
  - `last-goal-directed-read-requested-bound-positions`
  - `last-goal-directed-read-applied-bound-count`
  - `last-goal-directed-read-applied-bound-positions`
- `deduce/explain` remains a planner snapshot:
  - `surface-kind = 'planner-snapshot`
  - `goal-directed-execution-path` is the planner-side closure classification
    for the selected rule
  - `last-goal-directed-read-*` mirrors the last actual runtime read path
  - `steps[*].counters.counter-kind = 'observed` reports runtime-observed step
    counters attached to that planner snapshot
- current conjunctive rule order/operator fields stay planner-derived but are
  now pinned as aligned across both surfaces:
  - `deduce/explain.steps[*]`
  - `deduce/analyze.rule-execution[*].steps[*]`
  Both keep the same `join-order`, `predicate`, `operator`, and
  `selected-index` shape for the chosen rule.
- current observed counter surfaces are separate and truthful:
  - `deduce/stats.last-goal-directed-read-step-counters` reports the last
    actual preserved-bound runtime read
  - `deduce/analyze.rule-execution[*].steps[*].counters` reports the analyze
    execution itself
  - `deduce/explain.steps[*].counters` reports the explain execution itself
  Those observed counters can differ after the same earlier read and should
  not be treated as one shared counter source.
- aggregate-bearing and negated recursive selectors reject explicitly with
  `deduce/analyze-goal-directed-selector-not-eligible` and blocker payloads
