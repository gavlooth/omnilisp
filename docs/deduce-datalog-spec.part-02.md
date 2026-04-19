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
