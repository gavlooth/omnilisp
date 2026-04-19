### 7.4 Cleanup Verb Matrix

This section is the canonical naming and contract matrix for the current
write-side cleanup/admin verbs. The shipped surface intentionally keeps one
canonical cleanup verb per operation:

- tuple delete:
  - `(deduce 'retract! ...)`
  - `(deduce/retract! ...)`
- relation contents reset:
  - `(deduce 'clear! relation)`
  - `(deduce/clear! relation)`
- relation lifecycle removal:
  - `(deduce 'drop! relation)`
  - `(deduce/drop! relation)`

No additional cleanup aliases are approved in the current surface.

#### 7.4.1 Accepted call shapes and success contract

| Verb | Canonical accepted forms | Success result | Deterministic success semantics |
|---|---|---|---|
| `retract!` | `(deduce 'retract! relation val...)`, `(deduce/retract! relation val...)`, `(deduce 'retract! txn relation val...)`, `(deduce/retract! txn relation val...)` | `Void` | Exact tuple delete by encoded tuple key. Missing tuple is a no-op success. When a live delete occurs, relation cardinality estimates and DB mutation tracking are updated. |
| `clear!` | `(deduce 'clear! relation)`, `(deduce/clear! relation)` | `Void` | Removes all stored rows from the relation and its extensional shadow but preserves the relation schema/handle for future writes and reads. Clearing an already-empty relation is a no-op success. |
| `drop!` | `(deduce 'drop! relation)`, `(deduce/drop! relation)` | `Void` | Drops the relation storage, marks the registered schema and handle as dropped, persists the dropped lifecycle flag, and invalidates later read/write use of that handle. Re-dropping an already dropped handle is a no-op success. |

Current shipped boundary:

- `retract!` is the only cleanup verb that currently accepts an explicit Deduce
  transaction handle.
- `clear!` and `drop!` are currently relation-handle-only cleanup verbs. They
  open and commit their own write transaction internally.

#### 7.4.2 Cleanup rejection matrix

Unless explicitly listed below, the current shipped surface returns the error
code plus message only and does not attach a structured data payload.

| Verb | Rejection code | Trigger |
|---|---|---|
| `retract!` | `deduce/retract-expected-relation-values` | No relation argument provided in non-transaction form. |
| `retract!` | `deduce/retract-expected-transaction-relation-values` | Transaction form omits the relation argument. |
| `retract!` | `deduce/retract-transaction-handle-closed` | Transaction handle payload is already closed. |
| `retract!` | `deduce/retract-transaction-closed` | Transaction handle is not open or its DB is closed. |
| `retract!` | `deduce/retract-transaction-read-only` | Delete requested through a read-only transaction. |
| `retract!` | `deduce/retract-relation-not-handle` | Relation argument is not an FFI handle. |
| `retract!` | `deduce/retract-relation-handle-closed` | Relation handle payload is already closed. |
| `retract!` | `deduce/retract-relation-not-deduce-relation` | Handle is not a Deduce relation handle. |
| `retract!` | `deduce/retract-database-not-open` | Target relation DB is closed. |
| `retract!` | `deduce/retract-relation-dropped` | Target relation handle is already dropped. |
| `retract!` | `deduce/retract-transaction-database-mismatch` | Transaction DB does not match relation DB. |
| `retract!` | `deduce/retract-arity-mismatch` | Tuple arity does not match relation arity. |
| `retract!` | `deduce/retract-tuple-too-large` | Encoded tuple key exceeds the fixed tuple buffer. |
| `retract!` | `deduce/integrity-reference-target-in-use` | Delete would remove a referenced target tuple. |
| `retract!` | `deduce/retract-del-failed` | LMDB delete failed for a reason other than not-found. |
| `retract!` | `deduce/retract-shadow-del-failed` | Extensional shadow delete failed. |
| `retract!` | `deduce/retract-txn-open-failed` | Internal write transaction could not be opened. |
| `retract!` | `deduce/txn-failed` | Internal write transaction commit failed. |
| `clear!` | `deduce/clear-expected-relation` | Call shape is not exactly one relation handle argument. |
| `clear!` | `deduce/clear-relation-not-handle` | Argument is not an FFI handle. |
| `clear!` | `deduce/clear-relation-handle-closed` | Relation handle payload is already closed. |
| `clear!` | `deduce/clear-relation-not-deduce-relation` | Handle is not a Deduce relation handle. |
| `clear!` | `deduce/clear-database-not-open` | Target relation DB is closed. |
| `clear!` | `deduce/clear-relation-dropped` | Target relation handle is already dropped. |
| `clear!` | `deduce/integrity-reference-target-in-use` | Relation contents still contain referenced target tuples. |
| `clear!` | `deduce/clear-txn-open-failed` | Internal write transaction could not be opened. |
| `clear!` | `deduce/clear-failed` | LMDB clear of the main relation failed. |
| `clear!` | `deduce/clear-shadow-failed` | Extensional shadow clear failed. |
| `clear!` | `deduce/txn-failed` | Internal write transaction commit failed. |
| `drop!` | `deduce/drop-expected-relation` | Call shape is not exactly one relation handle argument. |
| `drop!` | `deduce/drop-relation-not-handle` | Argument is not an FFI handle. |
| `drop!` | `deduce/drop-relation-handle-closed` | Relation handle payload is already closed. |
| `drop!` | `deduce/drop-relation-not-deduce-relation` | Handle is not a Deduce relation handle. |
| `drop!` | `deduce/drop-database-not-open` | Target relation DB is closed. |
| `drop!` | `deduce/integrity-reference-target-in-use` | Drop would remove a referenced target relation or tuple set. |
| `drop!` | `deduce/drop-txn-open-failed` | Internal write transaction could not be opened. |
| `drop!` | `deduce/drop-failed` | LMDB drop of the main relation failed. |
| `drop!` | `deduce/drop-shadow-failed` | Extensional shadow drop failed. |
| `drop!` | `deduce/txn-failed` | Internal write transaction commit failed. |

The no-op cases above are intentional contract, not unspecified behavior:

- `retract!` on a tuple that is already absent still succeeds with `Void`
- `clear!` on an empty relation still succeeds with `Void`
- `drop!` on an already dropped relation handle still succeeds with `Void`

Query-demand widening note:

- the shipped `deduce/query` demand extractor now also folds closed
  one-argument pure closure wrappers such as `id` when the wrapper body still
  collapses to the supported equality-demand subset
- the shipped `deduce/query` demand extractor now also unwraps one-argument
  captured closure wrappers around the whole filter body when they forward the
  current `row` into a body that already fits that same equality-demand subset
- the shipped `deduce/query` demand extractor now also unwraps one-argument
  captured closure wrappers on the row-column side when they forward the
  current `row` into a body that already resolves to a supported
  `(ref row 'column)` shape
- the shipped `deduce/query` demand extractor now also follows short
  forwarding chains of those one-argument captured wrappers for both the
  whole-filter and row-column cases, but it still does not claim generic
  captured-call rewrite beyond that bounded forwarding shape
- the shipped `deduce/query` demand extractor now also unwraps preserved-row
  captured wrappers with extra closed literal arguments on both the
  whole-filter and row-column sides when the wrapper body still collapses to
  that same shipped subset
- for that preserved-row literal-arg subset, the forwarded `row` does not
  need to be the first call argument; any single forwarded row position is
  accepted as long as the remaining arguments are closed literal expressions
- the shipped `deduce/query` demand extractor also preserves that same
  bounded path across short forwarding chains of those preserved-row
  literal-arg wrappers, but it still does not claim generic captured-call
  rewrite beyond that bounded chain shape
- the shipped `deduce/query` demand extractor now also rewrites captured
  comparator wrappers when exactly one argument collapses to a row-derived
  scalar such as `(ref row 'dst)`, the remaining arguments are closed literal
  expressions, and the wrapper body still collapses to the supported equality
  subset
- that same scalar-comparator subset also follows short forwarding chains of
  captured wrappers, but broader symbolic solving and generic captured-call
  rewrite still fall back
- `deduce/query` now also ships the first broader query-time rewrite
  execution slice for the same recursive subset: a filter shaped as an `or`
  of individually demand-safe branches may execute each branch as its own
  abortable ephemeral head-demand run in a separate temporary write txn,
  then reapply the original full filter over the union of rows from those
  branch-local reads
- that disjunctive union path is still intentionally narrow:
  - every disjunct must fit the already shipped demand-safe subset
  - the currently shipped union contract now accepts same-position and
    mixed-position branches together, as long as each branch individually
    reduces to the already shipped demand-safe subset
  - the only remaining broader symbolic residual is recursive migration
    work; there is no separate non-recursive goal-directed symbolic disjunction
    lane beyond the already shipped subset
- on top of that, the same disjunctive union path now also accepts
  same-position wrapper branches where each disjunct reaches that same single
  projected head position through already shipped whole-filter or row-column
  wrapper forms
- that same same-position branch family also accepts branch-local residual
  unsupported conjuncts, because each branch still mines only its supported
  equality subset and the original full disjunctive filter is reapplied over
  the union after the branch-local reads
- mixed-position disjuncts now stay on the ephemeral union path too, because
  the runtime builds a union of the branch-local requested/applied positions
  and still reapplies the original full disjunctive filter over the union
  result
- recursive multi-position symbolic demands now ship a first bounded
  migration slice on recursive subjects too:
  - a single filter or disjunctive branch shaped like `(and (= src ...) (=
    dst ...))` may stay on `ephemeral-head-demand-query` when the projected
    recursive demand is relaxed to one applied position and the original full
    filter is reapplied over the rows
  - the current shipped relaxation keeps the one recursive carried head
    position preserved by the positive self-recursive rule, so admin truth may
    report `requested-bound-positions = (0 1)` with `applied-bound-positions`
    of `(1)` for the ordinary reach order or `(0)` for reordered head shapes
  - jointly-supported recursive permutation shapes such as
    `sym(x,y) :- sym(y,x)` no longer pretend the ephemeral demand path can
    answer fully-bound symbolic queries; they now fall back truthfully to
    `selected-component-closure` instead of reporting
    `ephemeral-head-demand-query` with incomplete or empty results
  - same-index self-recursive shapes such as
    `stable(x,y) :- stable(x,y)` now also keep jointly-supported fully-bound
    multi-position query demands on `ephemeral-head-demand-query`, with admin
    truth reporting both requested and applied positions as `(0 1)`
  - same-index mutual-recursive SCC shapes such as
    `a(x,y) :- b(x,y), b(x,y) :- a(x,y)` now also keep jointly-supported
    fully-bound multi-position query demands on `ephemeral-head-demand-query`,
    as long as each recursive rule in the selected SCC has some positive
    recursive body atom in that SCC preserving all requested positions
    together at the same indices
  - that same same-index mutual-recursive SCC family now also extends to
    disjunctive pair filters when each disjunct stays in the already shipped
    jointly-supported same-index subset, with admin truth still reporting
    requested and applied positions `(0 1)`
  - that same same-index SCC support is not limited to two-relation mutual
    recursion; multi-hop positive SCC cycles that preserve all requested
    positions together at the same indices also stay on
    `ephemeral-head-demand-query`
  - transformed recursive SCC shapes now ship a narrower follow-up too:
    when the queried head predicate has some same-index positive recursive
    carrier for one requested position, fully-bound pair demands may relax to
    that one carried applied position and still run on
    `ephemeral-head-demand-query`, with the original full filter reapplied
    over the rows
  - that transformed one-carried-position relaxation is an SCC-level family,
    not just a reordered two-relation mutual-recursion case; multi-hop
    transformed SCC cycles follow the same shipped rule
  - that same one-carried-position transformed relaxation also extends to
    disjunctive pair filters when each disjunct stays inside that same
    transformed recursive SCC subset
  - transformed recursive shapes where the uncarried demand is still
    distributed across multiple recursive body atoms now stay pinned on
    truthful `selected-component-closure` fallback instead of reporting
    `ephemeral-head-demand-query` with incomplete rows
  - recursive shapes where the requested positions are distributed across
    multiple recursive atoms, such as `path(x,z) :- path(x,y), path(y,z)`,
    now stay pinned on truthful `selected-component-closure` fallback for
    `deduce/query` instead of a misleading partial demand path
  - at this boundary, transformed recursive query-time behavior is fully
    described by the shipped support/fallback families above; there is no
    separate standing transformed-residual contract beyond them
- `deduce/match` now ships the first real query-time goal-directed execution
  slice for that same currently eligible recursive shape through:
  - `(deduce/match relation pattern rule-index)`
  - `(deduce/match relation pattern rule-index 'naive)`
  - `(deduce/match relation pattern rule-index 'semi-naive)`
  This executes only the selected component dependency closure before
  matching, requires that the selected closure actually produces the requested
  relation, and rejects aggregate-bearing / negated recursive selectors plus
  relation-mismatch selectors explicitly instead of silently widening to a
  full-DB run.
  A narrower demand-bound sub-slice is now also shipped for selector-scoped
  match when the pattern binds head literals and the selected positive
  recursive shape preserves those bound positions through its self-recursive
  body:
  - the demand-bound run executes in an abortable write txn,
  - if only some requested bound head positions are preserved, the runtime
    projects the demand down to that preserved subset and still applies the
    original full match pattern afterwards,
  - it restores in-memory schema estimates after abort,
  - it leaves the dirty frontier intact after the read,
  - each demanded head position must be preserved by every positive
    self-recursive body atom in the selected rule,
  - wider recursive shapes still fall back to the existing selected-component
    closure execution instead of pretending this demand path is generic.
- `deduce/query` now ships the matching relation-targeted goal-directed
  execution slice for that same currently eligible recursive shape through:
  - `(deduce/query relation filter-fn rule-index)`
  - `(deduce/query relation filter-fn rule-index 'naive)`
  - `(deduce/query relation filter-fn rule-index 'semi-naive)`
  This executes only the selected component dependency closure before
  filtering rows, requires that the selected closure actually produces the
  requested relation, and rejects aggregate-bearing / negated recursive
  selectors plus relation-mismatch selectors explicitly instead of silently
  widening to a full-DB run.
  For the currently shipped preserved-bound subset, the demand extractor now
  accepts row-independent constants, closed numeric builtin expressions such as
  `abs`, `floor`, `ceiling`, `round`, `truncate`, `sqrt`, `exp`, `log`,
  `log10`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`, `min`,
  `max`, and `pow`,
  closed row-independent comparison guards (`=`, `<`, `<=`, `>`, `>=`),
  closed row-independent `and` / `or` guards under Omni’s ordinary
  value-returning short-circuit semantics, closed row-independent `not`
  guards under ordinary Omni truthiness semantics,
  row-independent `let` / `block` / `if` wrappers, closed literal-side
  `let` / `block` wrappers around the supported equality subset including
  wrapped `ref` column symbols, closed row-column-side `let` / `block`
  wrappers that still resolve to `(ref row 'column)` through a closed symbol
  binding, closed row-independent `if` wrappers that select between
  row-column-side `(ref row 'column)` branches, closed row-independent
  `and` / `or` wrappers around the row-column side when the selected branch
  still resolves to `(ref row 'column)`, and row-independent `or` wrappers
  with one closed falsy branch around the supported equality subset.
  The last-read admin surface now also records requested vs applied preserved
  bound-counts for projected-demand reads through:
  - `last-goal-directed-read-requested-bound-count`
  - `last-goal-directed-read-requested-bound-positions`
  - `last-goal-directed-read-applied-bound-count`
  - `last-goal-directed-read-applied-bound-positions`
- `deduce/count`, `deduce/scan`, and `deduce/scan-range` now ship the same
  relation-targeted goal-directed execution slice for that currently eligible
  recursive shape through:
  - `(deduce/count relation rule-index)`
  - `(deduce/count relation rule-index 'naive)`
  - `(deduce/count relation rule-index 'semi-naive)`
  - `(deduce/scan relation rule-index)`
  - `(deduce/scan relation rule-index 'naive)`
  - `(deduce/scan relation rule-index 'semi-naive)`
  - `(deduce/scan-range relation lower upper rule-index)`
  - `(deduce/scan-range relation lower upper rule-index 'naive)`
  - `(deduce/scan-range relation lower upper rule-index 'semi-naive)`
  `deduce/query` and equality-bound `deduce/scan-range` also ship narrower
  demand-bound sub-slices on top of that selected-closure path:
  - `deduce/query` may now harvest the supported equality subset even when
    the full filter also contains residual unsupported conjuncts, because the
    original complete filter still runs after the read,
  - equality-filter `query` may project the requested head demand down to the
    preserved subset and still apply the original full filter closure after
    the read,
  - equality-bound `scan-range` may project the requested head demand down to
    the preserved subset and still apply the original full lower/upper bounds
    after the read,
  - unsupported shapes still fall back to the ordinary selected-component
    closure execution.
  - multi-self-recursive shapes only stay on this path when every positive
    self-recursive body atom preserves the demanded head positions; otherwise
    they fall back to `selected-component-closure`, clean the target
    component, and leave unrelated dirty siblings untouched.
  For the currently shipped preserved-bound subset, positive self-recursive
  body scans now also use demanded leading-prefix probes inside the naive and
  seminaive rule-step executors, so the abortable demand path narrows the
  recursive scan itself rather than only the outer selector plan.
  These execute only the selected component dependency closure before reading
  rows, require that the selected closure actually produces the requested
  relation, and reject aggregate-bearing / negated recursive selectors plus
  relation-mismatch selectors explicitly instead of silently widening to a
  full-DB run.
  Selector-scoped preserved-bound reads now also record
  `last-goal-directed-read-step-counters` in `deduce/stats`,
  `deduce/analyze`, and `deduce/explain`, exposing observed per-step counters
  such as `rows-read`, `rows-emitted`, and `join-probes` for the selected rule
  instead of only the coarse last-read path label.
  `deduce/scan-range` now also has a narrower bounded-demand sub-slice when
  equality-bound head literals can be extracted from positions where
  `lower == upper` and the selected positive recursive shape preserves those
  bound positions through its self-recursive body:
  - the demand-bound run executes in an abortable write txn,
  - it restores in-memory schema estimates after abort,
  - it leaves the dirty frontier intact after the read,
  - each demanded head position must be preserved by every positive
    self-recursive body atom in the selected rule,
  - wider recursive shapes still fall back to the existing selected-component
    closure execution path.
- plain relation reads without a selector now also auto-execute that same
  eligible dirty positive recursive closure in tracked mode for:
  - `(deduce/match relation pattern)`
  - `(deduce/query relation filter-fn)`
  - `(deduce/count relation)`
  - `(deduce/scan relation)`
  - `(deduce/scan-range relation lower upper)`
  This automatic path is intentionally narrow:
  - it only runs when the target relation belongs to an eligible positive
    recursive closure,
  - it only runs when at least one predicate in that selected closure is
    dirty,
  - it leaves unrelated recursive components untouched,
  - in tracked mode it executes only the target closure, but once the DB has
    escalated to `full-recompute-required` a plain read of a derived target
    relation falls back to the existing full DB fixpoint path,
  - and it does not widen support to aggregate-bearing or negated recursive
    shapes.
  Plain `deduce/match` now has one narrower exception inside that shipped
  surface: bound-literal patterns may use the abortable demand-bound path for
  the current preserved-bound positive recursive subset, restoring in-memory
  schema estimates after abort and leaving the target relation dirty.
  That preserved-bound subset now includes variable-preserving recursive body
  reordering, not only same-position carry-through, and each demanded head
  position still needs to be preserved by every positive
  self-recursive body atom in the rule.
  Plain `deduce/scan-range` now has the matching narrower exception when
  equality-bound head literals can be extracted from positions where
  `lower == upper` and the current positive recursive shape preserves those
  bound positions through its self-recursive body; that path also restores
  in-memory schema estimates after abort and leaves the target relation dirty.
  The same preserved-bound widening applies there too: variable-preserving
  recursive body reordering is now accepted, not only same-position
  carry-through, and each demanded head position still needs to be preserved
  by every positive self-recursive body atom in the rule.
  Plain and selector-scoped `deduce/query` now have the matching narrower
  demand-bound exception for a small safe filter subset: conjunctions of
  row-independent literal, captured-constant, or small safe builtin
  expression equalities on `(ref row 'column)` terms for the current
  preserved-bound positive recursive subset.
  Closed numeric builtin expressions like `(+ target 0)` are now part of
  that shipped subset, and row-independent `let` / `block` / `if` wrappers
  around the same equality subset are accepted too, while unsupported
  captured call shapes still fall back to selected-closure execution.

Still deferred:

- broader declaration-policy widening beyond the explicit current
  manual-and-`on-read` contract
- the next widened integrity class is fixed as canonical `check`; aliases
  such as `assert`, `predicate`, and `guard` are rejected for that lane
- unary column checks now have a shipped declaration/schema/admin and first
  write-enforcement baseline:
  - relation declarations accept `(check predicate column)`
  - `deduce/schema` exposes `kind = 'check`, `predicate`, `columns`, and
    `enforced = false`
  - `deduce/analyze` reports DB-wide `check-constraint-count`
  - immediate `fact!`, derived rule-head publish, and deferred
    `write-deferred` commit-time snapshot validation enforce declared checks
  - failure history now surfaces `violation-class = 'check` and deterministic
    codes for failed, missing, non-callable, and raised check predicates
- dedicated per-class admin counters for `check` remain follow-up work under
  `B6.10b2`
- optimizer rewrites that rely on materialization
- persisted rule/dependency catalogs and durable derived freshness semantics
  beyond the current persisted snapshot plus intent and refresh/stale-lifecycle
  catalog

Current durability note:

- file-backed materialized relations now persist both successful refresh
  history and later invalidation history
- file-backed materialized relations also keep the last stored snapshot rows in
  relation storage across reopen; current reopen/read semantics therefore
  preserve snapshot observability even while admin surfaces remain conservative
- a compact persisted rule/dependency catalog summary now survives reopen, so
  reopened `deduce/analyze` keeps the last installed `rule-count` and
  `incremental-dependency-edges`
- executable rule state still does not persist across reopen in the current
  shipped boundary, so refresh/read paths that require live rule execution
  still need explicit rule reinstall
- reopen / `open-named` therefore preserves:
  - `materialized-refresh-count`
  - `materialized-last-refresh-mutation-epoch`
  - `materialized-last-stale-reason`
  - `materialized-last-stale-mutation-epoch`
- reopened materialized relations remain stale while that persisted stale
  reason is non-`nil`; they do not silently become fresh just because the
  live dirty frontier starts empty
