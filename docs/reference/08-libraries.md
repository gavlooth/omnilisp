# Regex, PEG Grammar, Compression, Unicode & Deduce

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 24. Regex

Omni uses the Pika regex engine (not PCRE). It compiles PCRE-like syntax to
Pika grammars internally and provides linear/polynomial time guarantees — no
catastrophic backtracking.

```lisp
;; Match — returns first match or nil
(re-match "[0-9]+" "abc123def")
;; => "123"

;; Full match — must match entire string
(re-fullmatch "[0-9]+" "123")     ;; => "123"
(re-fullmatch "[0-9]+" "abc123")  ;; => nil

;; Find all matches
(re-find-all "[0-9]+" "a1b22c333")
;; => ("1" "22" "333")

;; Split by pattern
(re-split "\\s+" "hello   world   foo")
;; => ("hello" "world" "foo")

;; Replace
(re-replace "[0-9]+" "abc123def456" "NUM")
;; => "abcNUMdefNUM"

;; With limit
(re-replace "[0-9]+" "a1b2c3" "X" 2)
;; => "aXbXc3"

;; Position-returning variants
(re-match-pos "[0-9]+" "abc123")
;; => (3 . 6)  — (start . end)

(re-find-all-pos "[0-9]+" "a1b22")
;; => ((1 . 2) (3 . 5))
```

### Supported Syntax

Character classes (`[a-z]`, `\d`, `\w`, `\s`), quantifiers (`*`, `+`, `?`,
`{n,m}`), alternation (`|`), groups (`(...)`), anchors (`^`, `$`),
lookahead (`(?=...)`, `(?!...)`).

**Not supported**: backreferences (`\1`) — these make matching NP-complete.
Use PEG grammar or Omni code for context-sensitive matching.

---

## 25. PEG Grammar

The Pika grammar engine handles recursive parsing, left recursion, and
structural transformations — anything beyond what regex can do.

### Define a Grammar

```lisp
(define json-grammar
  (pika/grammar
    "value"  "object | array | string | number | 'true' | 'false' | 'null'"
    "object" "'{' (pair (',' pair)*)? '}'"
    "pair"   "string ':' value"
    "array"  "'[' (value (',' value)*)? ']'"
    "string" "'\"' [^\"]* '\"'"
    "number" "[0-9]+"))
```

### Parse

```lisp
(pika/parse json-grammar "[1, 2, 3]")
;; => parse tree
```

### Fold (Transform)

```lisp
(pika/fold grammar input fold-fn)
```

### Additional Operations

```lisp
(pika/grammar-rules grammar)              ;; list rule names
(pika/match-span grammar input rule-name)  ;; match specific rule
```

---

## 26. Compression

```lisp
;; Gzip
(define compressed (gzip "hello world"))
(gunzip compressed)   ;; => "hello world"
;; Optional level: 0..12 (default 6)
(gunzip (gzip "hello world" 1))

;; Raw deflate
(define deflated (deflate "hello world"))
(inflate deflated)     ;; => "hello world"
(inflate (deflate "hello world" 12))

;; Zlib wrapper format
(define z (zlib-compress "hello world"))
(zlib-decompress z)    ;; => "hello world"

;; Checksums
(adler32 "hello world") ;; => 436929629
(crc32 "hello world")   ;; => 222957957
```

---

## 27. Unicode

```lisp
;; Case conversion (Unicode-aware via utf8proc)
(string-upcase "hello")           ;; => "HELLO"
(string-downcase "HELLO")         ;; => "hello"
(string-casefold "Straße")        ;; => "strasse"
(string-titlecase "hELLO wORLD")  ;; => "Hello World"

;; Normalization
(string-normalize "cafe\u0301" 'NFC)   ;; => "cafe" (composed)
(string-normalize "cafe" 'NFD)          ;; => decomposes accents
;; Forms: NFC, NFD, NFKC, NFKD

;; Grapheme clusters
(string-graphemes "hello")   ;; => ("h" "e" "l" "l" "o")

;; Codepoints
(string-codepoints "abc")    ;; => (97 98 99)

;; Character category
(char-category "A")    ;; => "Lu" (uppercase letter)
(char-category "a")    ;; => "Ll" (lowercase letter)
(char-category "1")    ;; => "Nd" (decimal digit)

;; Display width hint
(char-width 65)        ;; => 1

;; Property lookup
(char-property 65 'category)  ;; => "Lu"
(char-property 48 'digit?)    ;; => true
```

---

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
