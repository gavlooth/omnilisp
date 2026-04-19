### Query

```lisp
(deduce 'scan person)           ;; all rows
(deduce 'count person)          ;; number of rows
(deduce 'query person
  (lambda (row) (> (ref row 'age) 28)))   ;; filtered
(deduce 'match person '("Alice" _ _))     ;; pattern match
(deduce 'match ancestor '(ancestor ?x ?y) 1) ;; selected recursive closure
(deduce 'query ancestor (lambda (row) true) 1) ;; selected recursive closure
(deduce 'scan-range person '("Alice" 0 "a@x") '("Charles" 999 "zzz")) ;; bounded scan
```

For the current goal-directed recursive slice:

- selector-scoped reads remain explicit through the optional `rule-index`
  forms shown above
- plain reads without a selector now also auto-execute an eligible dirty
  positive recursive closure in tracked mode for `match`, `query`, `count`,
  `scan`, and `scan-range`
- that automatic path stays intentionally narrow:
  - it leaves unrelated recursive components untouched
  - in tracked mode it executes only the target closure, but once the DB has
    escalated to `full-recompute-required` a plain read of a derived target
    relation falls back to the existing full DB fixpoint path
  - it does not widen support to aggregate-bearing or negated recursive
    shapes

Current goal-directed query-time slice:

- plain `deduce/match`, `deduce/query`, `deduce/count`, `deduce/scan`, and
  `deduce/scan-range` keep their ordinary read API shape, but they now have
  two execution-time behaviors on dirty derived recursive targets:
  - tracked mode: target-closure-only auto-execution for eligible positive
    recursive closures
  - `full-recompute-required`: full DB fixpoint fallback for derived targets
- plain `deduce/match` has one narrower exception inside tracked mode:
  bound-literal patterns may use the abortable demand-bound path for the
  current preserved-bound positive recursive subset
  - that path restores in-memory schema estimates after abort
  - it intentionally leaves the target relation dirty
  - the preserved-bound gate now includes variable-preserving recursive body
    reordering, not only same-position carry-through
  - each demanded head position must still be preserved by at least one
    positive self-recursive body atom in the rule
  - wider recursive shapes and unbound patterns still use the ordinary tracked
    closure auto-execution path
- plain `deduce/query` has the matching narrower exception inside tracked
  mode for a small safe filter subset: conjunctions of row-independent
  literal, captured-constant, or small safe builtin expression equalities on
  `(ref row 'column)` terms for the current preserved-bound positive
  recursive subset
  - row-independent `let` / `block` / `if` wrappers around that same
    shipped equality subset are now accepted too
  - that path restores in-memory schema estimates after abort
  - it intentionally leaves the target relation dirty
  - unsupported captured call shapes and other unsupported filter shapes
    still use the ordinary tracked closure
    auto-execution path
- plain `deduce/scan-range` has the matching narrower exception inside tracked
  mode when equality-bound head literals can be extracted from positions where
  `lower == upper` and the current positive recursive shape preserves those
  bound positions through its self-recursive body
  - that path restores in-memory schema estimates after abort
  - it intentionally leaves the target relation dirty
  - the preserved-bound gate now includes variable-preserving recursive body
    reordering, not only same-position carry-through
  - each demanded head position must still be preserved by at least one
    positive self-recursive body atom in the rule
  - wider recursive shapes and non-exact ranges still use the ordinary
    tracked closure auto-execution path
- relation-level `deduce/stats` now exposes the last actual goal-directed read
  path chosen for a relation:
  - `last-goal-directed-read-execution-path`
  - `last-goal-directed-read-surface`
  - `last-goal-directed-read-fallback-reason`
  - `last-goal-directed-read-selector-rule-index`
  - `last-goal-directed-read-mutation-epoch`
  - `last-goal-directed-read-requested-bound-count`
  - `last-goal-directed-read-applied-bound-count`
- DB-level `deduce/analyze` now also exposes the last actual goal-directed
  read:
  - `last-goal-directed-read-execution-path`
  - `last-goal-directed-read-surface`
  - `last-goal-directed-read-relation`
  - `last-goal-directed-read-fallback-reason`
  - `last-goal-directed-read-selector-rule-index`
  - `last-goal-directed-read-mutation-epoch`
  - `last-goal-directed-read-requested-bound-count`
  - `last-goal-directed-read-applied-bound-count`
  Current shipped path values are:
  - `no-op`
  - `selected-component-closure`
  - `full-db-fixpoint`
  - `ephemeral-head-demand-match`
  - `ephemeral-head-demand-query`
  - `ephemeral-head-demand-scan-range`
- `deduce/query` demand extraction also folds closed one-argument pure
  closure wrappers such as `id` when the wrapper body still collapses to the
  supported equality-demand subset
- `deduce/query` demand extraction also unwraps one-argument captured closure
  wrappers around the whole filter body when they forward `row` into that
  same supported equality-demand subset
- `deduce/query` demand extraction also unwraps one-argument captured closure
  wrappers on the row-column side when they forward `row` into a supported
  `(ref row 'column)` shape
- `deduce/query` demand extraction also follows short forwarding chains of
  those one-argument captured wrappers for both the whole-filter and
  row-column cases, while wider captured-call shapes still fall back
- `deduce/query` demand extraction also unwraps preserved-row captured
  wrappers with extra closed literal arguments on both the whole-filter and
  row-column sides when the wrapper body still collapses to that same shipped
  subset
- for that preserved-row literal-arg subset, the forwarded `row` does not
  need to be the first call argument; any single forwarded row position is
  accepted as long as the remaining arguments are closed literal expressions
- `deduce/query` demand extraction also preserves that same bounded path
  across short forwarding chains of those preserved-row literal-arg wrappers,
  while wider captured-call shapes still fall back
- `deduce/query` demand extraction also rewrites captured comparator wrappers
  when exactly one call argument collapses to a row-derived scalar and the
  remaining arguments are closed literal expressions
- that same row-derived scalar comparator subset also follows short
  forwarding chains of captured wrappers, while broader symbolic solving and
  wider captured-call shapes still fall back
- `deduce/query` also now has the first broader query-time rewrite-execution
  slice for the same recursive subset: an `or` of individually demand-safe
  branches may run multiple abortable ephemeral demand executions across
  separate temporary write txns, then apply the original full filter over the
  union of branch-local rows
- that disjunctive union path is still narrow:
  - every disjunct must fit the already shipped demand-safe subset
  - the currently shipped union contract now accepts same-position and
    mixed-position branches together, as long as each branch individually
    fits that demand-safe subset
  - the only remaining broader symbolic residual is recursive migration
    work; there is no separate non-recursive goal-directed symbolic disjunction
    lane beyond the already shipped subset
- the same disjunctive union path also now accepts same-position branches
  that reach that one projected head position through already shipped
  whole-filter or row-column wrapper forms
- that same-position branch family also accepts branch-local residual
  unsupported conjuncts, because the runtime still reapplies the original
  full filter over the union after the branch-local reads
- mixed-position disjuncts now stay on the ephemeral union path too, because
  the runtime unions the branch-local requested/applied positions and then
  reapplies the original full disjunctive filter over the combined rows
- recursive multi-position symbolic demands now have a first bounded
  recursive migration slice:
  - a single filter or disjunctive branch shaped like `(and (= src ...) (=
    dst ...))` may stay on `ephemeral-head-demand-query` when the runtime
    relaxes the projected recursive demand to one applied position and then
    reapplies the original full filter over the rows
  - the current shipped relaxation keeps the one recursive carried head
    position preserved by the positive self-recursive rule, so admin truth
    may show requested positions `(0 1)` with applied positions `(1)` on the
    ordinary reach order or `(0)` on reordered head shapes
  - jointly-supported recursive permutation shapes such as
    `sym(x,y) :- sym(y,x)` now fall back truthfully to
    `selected-component-closure` instead of pretending the ephemeral demand
    path can answer fully-bound symbolic queries there
  - same-index self-recursive shapes such as
    `stable(x,y) :- stable(x,y)` now also keep jointly-supported fully-bound
    multi-position query demands on `ephemeral-head-demand-query`, with admin
    truth reporting requested and applied positions `(0 1)`
  - same-index mutual-recursive SCC shapes such as
    `a(x,y) :- b(x,y), b(x,y) :- a(x,y)` now also keep jointly-supported
    fully-bound multi-position query demands on `ephemeral-head-demand-query`
    when each recursive rule in the SCC has some positive recursive body atom
    in that SCC preserving all requested positions together at the same
    indices
  - that same same-index mutual-recursive SCC family also extends to
    disjunctive pair filters when each disjunct stays inside the already
    shipped jointly-supported same-index subset
  - the same same-index SCC support also reaches multi-hop positive cycles,
    not just two-relation mutual recursion
  - transformed recursive SCC shapes also have a narrower shipped relaxation:
    if the queried head predicate still has some same-index recursive carrier
    for one requested position, a fully-bound pair demand may relax to that
    one carried applied position and still use `ephemeral-head-demand-query`
  - that transformed one-carried-position rule is SCC-level, not limited to a
    reordered two-relation mutual-recursion shape
  - that same transformed one-carried-position relaxation also extends to
    disjunctive pair filters inside the same shipped subset
  - transformed recursive shapes where the uncarried demand is still spread
    across multiple recursive body atoms now stay on truthful
    `selected-component-closure` fallback instead of a misleading partial
    demand path
  - at this boundary, transformed recursive query-time behavior is fully
    described by those shipped support/fallback families; there is no separate
    standing transformed residual beyond them
  - recursive shapes where the requested positions are distributed across
    multiple recursive atoms, such as `path(x,z) :- path(x,y), path(y,z)`,
    now stay pinned on truthful `selected-component-closure` fallback for
    `deduce/query`
- for the currently shipped preserved-bound subset, positive body scans now
  also use demanded leading-prefix probes inside the naive and seminaive
  rule-step executors, so the abortable demand path prefix-prunes those
  positive body scans rather than only narrowing the outer selector plan
- selector-scoped preserved-bound reads now also record
  `last-goal-directed-read-step-counters` in `deduce/stats`,
  `deduce/analyze`, and `deduce/explain`, exposing observed per-step
  `rows-read` / `rows-emitted` / `join-probes` data for the selected rule
- selector-scoped match is now available for the currently eligible positive
  recursive shape:
  - `(deduce/match relation pattern rule-index)`
  - `(deduce/match relation pattern rule-index 'naive)`
  - `(deduce/match relation pattern rule-index 'semi-naive)`
- selector-scoped query is now available for that same shape:
  - `(deduce/query relation filter-fn rule-index)`
  - `(deduce/query relation filter-fn rule-index 'naive)`
  - `(deduce/query relation filter-fn rule-index 'semi-naive)`
  For the currently shipped preserved-bound subset, the demand extractor now
  also accepts row-independent constants, closed numeric builtin expressions
  such as `abs`, `floor`, `ceiling`, `round`, `truncate`, `sqrt`, `exp`,
  `log`, `log10`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`,
  `min`, `max`, and `pow`,
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
- selector-scoped count / scan / scan-range are now available for that same
  shape:
  - `(deduce/count relation rule-index)`
  - `(deduce/count relation rule-index 'naive)`
  - `(deduce/count relation rule-index 'semi-naive)`
  - `(deduce/scan relation rule-index)`
  - `(deduce/scan relation rule-index 'naive)`
  - `(deduce/scan relation rule-index 'semi-naive)`
  - `(deduce/scan-range relation lower upper rule-index)`
  - `(deduce/scan-range relation lower upper rule-index 'naive)`
  - `(deduce/scan-range relation lower upper rule-index 'semi-naive)`
- selected match executes only the chosen component dependency closure before
  scanning the requested relation
- selector-scoped `deduce/match` has a narrower demand-bound fast path when
  the pattern binds head literals and the selected positive recursive shape
  preserves those bound positions through its self-recursive body
  - if only some requested bound positions are preserved, the runtime
    projects the demand down to that preserved subset and still applies the
    original full match pattern afterwards
  - this path runs in an abortable write txn
  - it restores in-memory schema estimates after abort
  - it intentionally leaves dirty state intact after the read
  - unsupported recursive shapes fall back to the ordinary selected-closure
    execution path
- selected query executes only the chosen component dependency closure before
  filtering the requested relation
- selector-scoped `deduce/query` also has a narrower demand-bound fast path
  for that same safe filter subset: conjunctions of row-independent literal,
  captured-constant, or small safe builtin expression equalities on
  `(ref row 'column)` terms for the selected preserved-bound positive
  recursive subset
  - row-independent `let` / `block` / `if` wrappers around that same
    shipped equality subset are now accepted too
  - supported equality conjuncts can now still drive this path even when the
    filter keeps extra residual unsupported conjuncts
  - if only some equality-filter columns are preserved, the runtime projects
    the demand down to that preserved subset and still applies the original
    full filter closure afterwards
  - multi-self-recursive shapes only stay on this path when every positive
    self-recursive body atom preserves the demanded head positions; otherwise
    they fall back to `selected-component-closure`, clean the target
    component, and leave unrelated dirty siblings untouched
  - the last-read admin surface records the requested vs applied preserved
    bound-counts and bound-position lists for this projected-demand path
  - this path runs in an abortable write txn
  - it restores in-memory schema estimates after abort
  - it intentionally leaves dirty state intact after the read
  - unsupported captured call shapes and other unsupported filter shapes
    fall back to the ordinary selected-closure
    execution path
- selected count / scan / scan-range execute only that same chosen component
  dependency closure before reading rows
- selector-scoped `deduce/scan-range` also has a narrower bounded-demand fast
  path when equality-bound head literals can be extracted from positions where
  `lower == upper` and the selected positive recursive shape preserves those
  bound positions through its self-recursive body
  - if only some equality-bound positions are preserved, the runtime projects
    the demand down to that preserved subset and still applies the original
    full lower/upper bounds afterwards
  - this path runs in an abortable write txn
  - it restores in-memory schema estimates after abort
  - it intentionally leaves dirty state intact after the read
  - unsupported recursive shapes fall back to the ordinary selected-closure
    execution path
- selected relation reads reject explicitly when:
  - the chosen recursive selector is aggregate-bearing
  - the chosen recursive selector has negated body atoms
  - the selected closure does not produce the requested relation
  - the DB has already escalated to `full-recompute-required`

### Rules, Explain, Analyze

```lisp
;; install rules
(deduce 'rule! db '(ancestor ?x ?y) '(parent ?x ?y))
(deduce 'rule! db '(ancestor ?x ?z) '(parent ?x ?y) '(ancestor ?y ?z))

;; planner/execution metadata for a selected rule
(deduce 'explain db 'ancestor)

;; run SCC-fixpoint evaluator (semi-naive on recursive strata)
(deduce 'analyze db)

;; optional engine selection for recursive SCCs
(deduce 'analyze db 'semi-naive)  ;; default
(deduce 'analyze db 'naive)       ;; reference mode
```

`analyze` returns a diagnostics dict including `mode`, `execution-engine`,
`rule-count`, `strata-count`, `stratum-count`, `recursive-strata`, `iteration-limit`,
`max-component-iterations`, `iterations`, `derived-facts`,
`incremental-dependency-edges`,
`incremental-dirty-predicate-count`, and
`incremental-invalidation-mode`.

`strata-count` reflects SCC component count; `stratum-count` reflects computed
stratification levels used by runtime scheduling.

`incremental-invalidation-mode` is `tracked` for dependency-aware dirty-set
propagation and `full-recompute` when a commit cannot guarantee complete
dependency tracking (for example degraded fallback after mutation-log
allocation failure, or the current destructive committed-write path).

Relation-local `deduce/stats` keeps the DB-global dirty frontier in
`dirty-predicate-count` / `dirty-predicates`; `dirty-self` is the relation-
local bit that tells you whether that particular relation is still dirty
inside the shared frontier.

The three admin surfaces also stay phase-truthful across degraded recovery:
if a plain DB-level `deduce/analyze` has already cleared an earlier
`full-recompute` escalation, a later relation-scoped `deduce/refresh!` stays
on the targeted path, while its `remaining-stale-materialized-relations` and
later `deduce/analyze` still report any untouched stale peers.

For upcoming true incremental maintenance work, the current runtime already
has an internal signed-delta substrate:

- per-predicate `additions` and `removals` buckets
- encoded tuple keys for replay/dedup
- separate `base_counts` and `recursive_counts` support tables
- `current` / `next` delta buffers aligned to the SCC predicate order

That substrate is internal today and is used by recursive seminaive
evaluation, not by ordinary post-commit derived maintenance yet. The canonical
data-model baseline for later maintenance work is in
`docs/deduce-datalog-spec.md`, section `7.3.5 Incremental Delta Substrate Baseline`.

The fallback/admin rule for later incremental maintenance is also fixed up
front: future maintained-update paths should reuse the current
`tracked` / `full-recompute` vocabulary and the existing recovery semantics,
not invent a second public degraded-mode family. The canonical boundary is in
`docs/deduce-datalog-spec.md`, section
`7.3.6 Incremental Maintenance Fallback/Admin Boundary`.

The first ordinary maintained-update classes are now also shipped, but only
for narrow direct-copy shapes:

- already-refreshed ready materialized targets with exactly one positive
  direct-copy rule from one extensional source
- one-step downstream materialized targets with exactly one positive
  direct-copy rule from one already-refreshed ready materialized direct-copy
  source

For those classes, relation-scoped `deduce/refresh!` now reports
`refresh-execution-path = 'incremental-targeted`, and stale ordinary reads on
`[relation db materialized on-read]` reuse that same path when the target
qualifies. Unsupported shapes, stale derived sources, and never-refreshed
targets stay on the older `targeted` or `db-wide` boundaries. The canonical
contract is in `docs/deduce-datalog-spec.md`, section
`7.3.7 First Maintained-Update Classes: Direct-Copy Incremental Refresh`.

`explain` now also exposes goal-directed planner metadata for recursive rules.
For currently eligible recursive selectors it mirrors the selected dependency
closure used by selector-scoped `analyze` through:

- `goal-directed-execution-path`
- `goal-directed-selected-components`
- `goal-directed-selected-predicates`

Aggregate-bearing and negated recursive selectors keep explicit blocker
reporting and leave those selected-closure fields `nil`.

Even after a runtime `match` or `query`, `explain` stays a planner snapshot:
top-level `surface-kind` and `goal-directed-execution-path` describe the
selected rule plan, while `last-goal-directed-read-*` and
`steps[*].counters.counter-kind = 'observed` reflect the last actual runtime
read attached to that rule's head relation.

Write-transaction mutations are applied to incremental tracking only at
`(deduce 'commit tx)`; `(deduce 'abort tx)` discards pending mutation state.

The `deduce/why-result` family now has both a fixed top-level payload/status
baseline and a first public runtime slice:

- canonical statuses: `ok`, `missing`, `partial`, `error`
- canonical envelope keys:
  - `kind`
  - `status`
  - `surface-kind`
  - `subject-kind`
  - `subject`
  - `path-count`
  - `max-depth`
  - `truncated`
  - `paths`
- public today:
  - `(deduce/why-result relation val...)` for stored row subjects
  - extensional stored rows return one deterministic `seed` path
  - stored rows in exact-one-rule extensional derived relations return one
    deterministic `derived` path when every support tuple is reconstructible
    from the head row
  - current derived support covers direct-copy, one-body
    projection/permutation, and head-bound multi-body extensional rules
  - exact-one-rule extensional derived relations also support search-based
    lineage:
    - one support path returns `status = ok`
    - multiple support paths return `status = partial` with the first
      deterministic path
  - exact-one-rule mixed-body non-recursive relations also now work when the
    derived body predicates are themselves covered by the shipped exact-one-rule
    provenance helper surface
  - multi-rule non-recursive relations also now work when at least one matching
    rule is covered by the shipped non-recursive provenance helper surface
    - one support path across those matching rules returns `status = ok`
    - multiple support paths across those matching rules return `status = partial`
      with the first deterministic path
  - first positive recursive closure lineage also now works when the recursive
    support chain can be proven without revisiting the same row subject
    - one recursive support chain returns `status = ok`
    - multiple recursive support chains return `status = partial` with the first
      deterministic path
    - recursive payloads now append a `rule-step` support frame for the derived
      child row used in the chosen proof path
    - recursive closure `max-depth` now reflects that deeper derived step
  - missing rows return `status = missing`
  - broader stored derived rows that still have no shipped support path return
    `status = error` with
    `deduce/why-result-derived-subject-not-yet-supported`
  - `why-result` also now exposes optional `goal-directed-read-context`
    metadata when the relation has last goal-directed read state
    - this mirrors the existing `last-goal-directed-read-*` relation metadata
    - it is relation-level read context across all payloads
  - for the current narrower proof-integrated slice, the matching proof path
    also gets optional `goal-directed-read-context` metadata when the last
    goal-directed `deduce/query`, `deduce/match`, `deduce/scan`, or
    `deduce/scan-range` on that relation observed a bounded complete row set
    of at most `8` rows and the traced tuple belongs to that set
    - for `query`, `match`, and `scan-range`, this bounded-complete
      path-local slice applies in both plain and selector-scoped
      goal-directed reads;
      selector-scoped payloads keep their concrete `selector-rule-index`
    - `scan` now also participates in that bounded-complete path-local slice
      for selector-scoped reads; those payloads keep their concrete
      `selector-rule-index`
    - plain no-op `query` and `scan-range` reads now also preserve that
      bounded complete row set, so matching root proof paths can carry the
      same path-local context even when the last read stayed on `no-op`
    - plain no-op `match` reads now also participate in that bounded
      root-path slice for matching rows
    - selector-scoped valid row reads now also keep truthful path-local
      context across the current shipped shapes:
      `match` and `scan` can stay on `no-op`, while `query` and
      `scan-range` keep their shipped ephemeral demand paths; all four retain
      their concrete `selector-rule-index`
    - matching derived support frames now also carry that path-local context
      when their `(relation, tuple)` pair matches the current bounded
      complete last-read subject set
    - matching fact support frames now also carry that same path-local
      context when their `(relation, tuple)` pair matches the current
      bounded complete last-read subject set
    - when the root tuple itself did not match but the support frames
      carrying bounded context all come from the same relation-local last-read
      state, the proof path now also inherits that same
      `goal-directed-read-context`
    - when the chosen proof path carries multiple distinct support-frame
      contexts across different relations, the path now exposes
      `goal-directed-read-contexts` as a list instead of forcing a fake
      merged singular context
    - broader proof-path integration beyond the current
      root/fact-frame/rule-step row-matching slices remains deferred

That baseline is source-of-truth in `docs/deduce-datalog-spec.md`, section
`8.1 Provenance / Why-Result Payload Shape`.
