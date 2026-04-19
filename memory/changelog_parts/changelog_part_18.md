# Memory Changelog Index Part 18

Source: `memory/CHANGELOG.md`

  - recursive and goal-directed provenance semantics remain open as `B6.9b1`
    and `B6.9b2`
- Closed Deduce TODO item `B6.9a7` by shipping the first exact-one-rule
  mixed-body non-recursive `deduce/why-result` lineage:
  - mixed-body non-recursive rules now compose through already-supported
    exact-one-rule child provenance helpers
  - unique support chains return `status = ok`, and multiple support chains
    return `status = partial` with the first deterministic path
  - multi-rule non-recursive derived lineage remains open as `B6.9a8`
- Closed Deduce TODO item `B6.9a6` by shipping the first exact-one-rule
  extensional search-based `deduce/why-result` lineage:
  - non-recursive extensional rules with existential/search-based support now
    return `status = ok` when one deterministic path exists
  - the same surface now returns `status = partial` with `truncated = true`
    when multiple support paths exist and only the first deterministic path is
    emitted
  - mixed-body or multi-rule non-recursive derived lineage remains open as
    `B6.9a7`
- Closed Deduce TODO item `B6.9a5` by widening non-recursive
  `deduce/why-result` lineage to reconstructible extensional multi-body rules:
  - exact-one-rule extensional derived relations now return `status = ok` when
    every support tuple is reconstructible from the head row
  - current derived support now covers direct-copy, one-body
    projection/permutation, and head-bound multi-body extensional rules
  - existential or search-based non-recursive derived lineage remains open as
    `B6.9a6`
- Closed Deduce TODO item `B6.9a4` by widening non-recursive
  `deduce/why-result` lineage to the first one-body extensional class:
  - stored rows in relations with exactly one non-negated, non-aggregate,
    one-body extensional rule now return `status = ok` when the source tuple is
    fully reconstructible from the head row
  - current derived support covers direct-copy plus one-body
    variable projection/permutation rules
  - multi-body or existential non-recursive derived lineage remains open as
    `B6.9a5`
- Closed Deduce TODO item `B6.9a3` by shipping the first non-recursive
  derived `deduce/why-result` slice:
  - stored rows in relations with exactly one non-negated, non-aggregate,
    one-body direct-copy rule from an extensional source now return
    `status = ok` with one deterministic `derived` path
  - the derived path's support names the extensional source fact predicate
  - broader non-recursive derived lineage remains open as `B6.9a4`
- Closed Deduce TODO item `B6.9a2` by shipping the first public
  `deduce/why-result` runtime slice:
  - `(deduce/why-result relation val...)` now returns deterministic row-subject
    payloads for stored extensional tuples
  - extensional hits return `status = ok` with one `seed` path and one `fact`
    support frame
  - missing tuples return `status = missing`
  - stored derived rows return `status = error` with
    `deduce/why-result-derived-subject-not-yet-supported`
  - the remaining non-recursive derived-lineage work is tracked separately as
    `B6.9a3`
- Closed Deduce TODO item `B6.9a1` by tightening the why-result baseline into
  canonical doc truth:
  - `docs/deduce-datalog-spec.md` now fixes the top-level status semantics for
    `ok`, `missing`, `partial`, and `error`
  - the canonical why-result envelope keys are now explicit before any public
    provenance runtime rollout
- Closed Deduce TODO item `B6.8b2` by shipping the first derived-predicate
  maintained-update class:
  - successful `incremental-targeted` refresh of a supported materialized
    direct-copy source can now queue the same inserted tuples for one-step
    downstream direct-copy materialized targets
  - downstream ready targets can then refresh on the same
    `incremental-targeted` path while still remaining stale until explicitly
    refreshed
- Closed Deduce TODO item `B6.8b1` by shipping the first maintained-update
  class for ordinary DB mutations:
  - committed insertions on supported extensional direct-copy sources now queue
    encoded tuple keys per materialized target
  - relation-scoped `deduce/refresh!` now reports
    `refresh-execution-path = 'incremental-targeted` for already-refreshed
    supported targets, and stale `on-read` materialized reads reuse that same
    path
  - queue-tracking failure now escalates truthfully to the existing
    `full-recompute-required` boundary instead of silently dropping deltas
- Closed Deduce TODO item `B6.8a2` by fixing the fallback/admin contract for
  future incremental maintenance:
  - the canonical docs now require later maintained-update work to reuse the
    current `tracked` / `full-recompute` public vocabulary
  - the docs now explicitly reject new public degraded-mode names and require
    existing stats/analyze/refresh/read surfaces to stay truthful under
    fallback
- Closed Deduce TODO item `B6.8a1` by lifting the current incremental
  signed-delta substrate into the canonical docs:
  - `docs/deduce-datalog-spec.md` now has an explicit data-model baseline for
    encoded tuples, signed delta sets, support tables, and current/next
    iteration buffers
  - `docs/reference/08-libraries.md` now points later maintenance work at
    that substrate instead of leaving the internal model implicit
- Closed Deduce TODO item `B6.7b2` by pinning admin-surface alignment beyond
  tracked recompute:
  - `deduce/stats`, `deduce/analyze`, and relation-scoped `deduce/refresh!`
    now have explicit regression coverage across a rule-set-change degraded
    transition
  - the current surface is now pinned so that a prior plain DB-level analyze
    clears `full-recompute`, a later relation refresh stays targeted, and the
    untouched stale peer remains visible on both refresh and later analyze
- Closed Deduce TODO item `B6.7b1` by pinning the current dirty-frontier truth
  contract:
  - relation-local `deduce/stats` now has explicit regression coverage showing
    that `dirty-predicate-count` / `dirty-predicates` stay DB-global
  - `dirty-self` is now pinned as the relation-local bit that differs between
    a cleared relation and an untouched relation after selected-component
    recovery
- Closed Deduce TODO item `B6.7a2` by pinning the current degraded-state and
  recovery contract:
  - relation-local `deduce/stats` now has explicit regression coverage showing
    `full-recompute-required` immediately after destructive writes
  - plain DB-level `deduce/analyze` now has explicit regression coverage as a
    pre-reset snapshot surface that still reports the degraded frontier it
    observed
  - a successful DB-level analyze run is now pinned as clearing the live
    `full-recompute` / dirty-admin state afterward
- Closed Deduce TODO item `B6.7a1` by pinning the current commit/abort
  mutation-log contract:
  - write-block mutations now have explicit regression coverage showing that
    dirty/admin state changes only on commit
  - `abort` drops pending mutation logs without changing row counts or dirty
    metadata
  - committed destructive writes are now pinned as the current path that
    escalates `incremental-invalidation-mode` to `full-recompute`
- Closed Deduce TODO item `B6.6b2` by pinning the real conjunctive counter
  boundary:
  - `deduce/stats.last-goal-directed-read-step-counters` now has explicit
    regression coverage as the last preserved-bound runtime read surface
  - `deduce/analyze.rule-execution[*].steps[*].counters` and
    `deduce/explain.steps[*].counters` are now pinned as separate observed
    execution surfaces, not aliases of the earlier read counters
- Closed Deduce TODO item `B6.6b1` by pinning conjunctive path/order truth at
  the current shipped boundary:
  - `deduce/explain.steps[*]` and
    `deduce/analyze.rule-execution[*].steps[*]` now have regression coverage
    proving they stay aligned on planner-derived `join-order`, `predicate`,
    `operator`, and `selected-index`
  - runtime truth for that lane remains in the attached observed counters,
    not a separate reordered execution trace
- Closed Deduce TODO item `B6.6a2` by pinning the shipped `deduce/analyze`
  classification boundary for conjunctive recursive selectors:
  - selector-scoped top-level `goal-directed-*` fields remain planner-side
  - `last-goal-directed-read-*` continues to mirror the last actual DB-level
    read
  - `rule-execution[*].steps[*].counters.counter-kind = 'observed` is now
    covered explicitly for the analyzed rule entries
- Closed Deduce TODO item `B6.6a1` by pinning the shipped `deduce/explain`
  classification boundary:
  - `deduce/explain` now has regression coverage proving it remains a
    `planner-snapshot` surface even after a runtime `deduce/query`
  - top-level `goal-directed-execution-path` stays planner-side, while
    `last-goal-directed-read-*` and `steps[*].counters.counter-kind =
    'observed` mirror the last actual runtime read
- Closed Deduce TODO item `B6.4f5a` with the next recursive symbolic safety
  slice:
  - recursive query demands whose requested positions are split across
    multiple recursive atoms, such as `path(x,z) :- path(x,y), path(y,z)`,
    now have explicit selector/plain regression coverage on truthful
    `selected-component-closure` fallback
  - the remaining recursive symbolic residual is narrower `B6.4f5b`:
    broader recursive multi-position query support beyond the current
    same-index-preserved and fallback-only shapes
- Closed Deduce TODO item `B6.4f4b` with the first real jointly-supported
  recursive multi-position query slice:
  - same-index self-recursive shapes such as `stable(x,y) :- stable(x,y)`
    now keep fully-bound multi-position selector/plain query demands on
    `ephemeral-head-demand-query`
  - admin truth for that slice now reports requested and applied positions
    `(0 1)` instead of collapsing to one carried position or a full
    selected-component closure
  - the remaining recursive symbolic residual is narrower `B6.4f5`:
    broader recursive multi-position query support beyond the current
    same-index-preserved and permutation-fallback shapes
- Closed Deduce TODO item `B6.4f4a` with the next recursive symbolic safety
  slice:
  - jointly-supported recursive permutation query demands such as
    `sym(x,y) :- sym(y,x)` no longer claim the ephemeral demand path when the
    current evaluator cannot seed the needed support tuples
  - those selector/plain single-branch and disjunctive queries now fall back
    truthfully to `selected-component-closure` with `dirty-closure`
  - the remaining recursive symbolic residual is narrower `B6.4f4b`:
    preserving jointly-supported multi-position recursive query demands
    beyond that truthful permutation fallback
- Closed Deduce TODO item `B6.4f3` with the next bounded recursive symbolic
  migration slice:
  - recursive multi-position symbolic query demands no longer hard-code the
    last requested position as the one applied recursive anchor
  - the runtime now keeps the one head position preserved by the positive
    self-recursive rule, which fixes reordered recursive head shapes where
    the carried position is not the last column
  - the remaining recursive symbolic residual is now narrower `B6.4f4`:
    jointly-supported multi-position recursive demands that should stay
    applied together on single positive self-recursive shapes
- Closed Deduce TODO item `B6.4f2` with the first bounded recursive symbolic
  migration slice:
  - recursive multi-position symbolic pair filters and disjunctive pair
    branches now stay on `ephemeral-head-demand-query` when the projected
    recursive head demand can be relaxed to one applied position
  - the current shipped relaxation keeps the trailing supported projected
    position, so admin truth now reports requested positions `(0 1)` with
    applied positions `(1)` for that slice
  - the real residual is now narrower `B6.4f3`: broader recursive symbolic
    rewrite beyond single applied-position relaxation
- Retired stale Deduce TODO item `B6.4e2c` after rechecking the remaining
  symbolic query lane against the current runtime contract:
  - there is no separate non-recursive goal-directed symbolic disjunction
    implementation lane beyond the already shipped subset, because non-recursive
    one-rule subjects are not goal-directed eligible in the current planner
  - the real open residual is entirely the recursive symbolic migration
    lane now tracked as `B6.4f2`
- Closed Deduce TODO item `B6.4f1` with the first truthful recursive-shape
  safety slice: recursive multi-position symbolic query demands, including
  single filters and disjunctive branches shaped like `(and (= src ...) (=
  dst ...))`, now fall back to `selected-component-closure` instead of
  claiming `ephemeral-head-demand-query` with incomplete results; the open
  recursive residual is broader support beyond the current single-position
  migration boundary under `B6.4f2`.
- Closed Deduce TODO item `B6.4e2b2` with the next truthful disjunctive
  rewrite slice: mixed-position demand-safe branches now stay on
  `ephemeral-head-demand-query`, with the runtime unioning the branch-local
  requested/applied positions before reapplying the original full
  disjunctive filter; the remaining open residual is broader symbolic
  multi-branch rewrite under `B6.4e2c`.
- Closed Deduce TODO item `B6.4e2b1` with the next truthful same-position
  disjunctive rewrite slice: same-position branch unions now also cover
  branch-local residual unsupported conjuncts, because each branch still
  mines only its shipped equality-demand subset and the original full filter
  is reapplied over the union after the branch-local reads; the remaining
  open residual is wider mixed-position or more symbolic disjunction under
  `B6.4e2b2`.
- Tightened the already-shipped `B6.4e1` / `B6.4e2a` disjunctive query
  rewrite boundary to the truthful runtime contract:
  - each shipped branch now executes in its own abortable temporary write
    txn before the original full filter is reapplied over the union
  - same-position wrapper disjuncts stay on the ephemeral path
  - mixed-position multi-branch disjuncts are now pinned back to
    `selected-component-closure`, which remains the open residual under
    `B6.4e2b`
- Closed Deduce TODO item `B6.4e2a` with the next truthful same-position
  multi-branch rewrite slice: the disjunctive ephemeral query path now also
  accepts same-position wrapper branches that reach the shared projected
  head-demand slot through already shipped whole-filter or row-column
  wrappers, while mixed-position disjuncts still fall back and remain open
  under `B6.4e2b`.
- Closed Deduce TODO item `B6.4e1` with the first broader query-time rewrite
  execution slice: `deduce/query` can now union multiple abortable ephemeral
  head-demand runs for an `or` of individually demand-safe branches inside
  the same temporary write txn, while wider mixed or symbolic disjunctions
  still fall back and remain open under `B6.4e2`.
- Closed Deduce TODO item `B6.4d` with the next truthful `deduce/query`
  demand-widening slice: the shipped extractor now also rewrites captured
  comparator wrappers when exactly one call argument reduces to a row-derived
  scalar and the remaining arguments are closed literals, and it preserves
  that same bounded subset across short forwarding chains without claiming
  generic captured-call rewrite or symbolic solving.
- Closed Deduce TODO item `B6.3f2` with the first truthful durable
  ready/freshness reopen slice: supported persisted executable rule
  signatures now restore live derived execution through stable catalog-name
  remapping plus inferred predicate-schema registration, so reopened
  `deduce/analyze`, `deduce/refresh!`, refreshed manual materialized views,
  and stale `on-read` reads regain truthful ready/fresh behavior on the
  current supported signature surface while unsupported signature shapes
  still fall back to summary-only admin truth.
- Closed Deduce TODO item `B6.3f1b` with the first truthful rule/dependency
  catalog persistence slice: file-backed DBs now persist a compact admin
  catalog summary across reopen, so reopened `deduce/analyze` retains the
  last installed `rule-count` and `incremental-dependency-edges` while
  executable rule state and durable ready/fresh semantics remain deferred.
- Consolidated the repo backlog into `TODO.md` and removed the parallel
  backlog/roadmap tracker files from `docs/plans/`, so there is now one live
  execution queue instead of several drifting sources.
- Shipped Deduce declaration-time `on-read` materialization as the first
  non-manual policy: `[relation db materialized on-read]` now parses,
  persists across reopen, surfaces through `deduce/schema` and `deduce/stats`,
  auto-refreshes stale derived materialized relations on ordinary stored-tuple
  reads, and fails truthfully on stale reopen cases where rule/dependency
  catalogs are still absent.
- Chose the first future non-manual declaration policy target instead of
  leaving it generic: `on-read` is now the explicit first widening after the
  current manual-only surface, while `on-base-commit`, `on-open`, and
  `scheduled` remain later approved names for different trigger families.
- Strengthened the repo agent rule from “don’t split only when naming is
  blocked” into a general naming-problem rule: whenever naming is unclear,
  drifting, or blocking progress, agents must convert that into an explicit
  decision, a concrete options note, or a direct owner question instead of
  leaving “needs naming” as the stopping point.
- Expanded the Deduce declaration-policy decision from “freeze at `manual`”
  into a full naming table for future trigger families: `on-read`,
  `on-base-commit`, `on-open`, and `scheduled` are now the approved future
  names, while vague spellings like `auto`, `eager`, `background`, and
  `on-write` are explicitly rejected.
- Closed Deduce backlog item `B6.3e1` with an explicit surface decision
  instead of leaving naming drift open-ended: declaration-time materialization
  stays frozen at `manual` until a real non-manual maintenance contract exists,
  and the decision note now records concrete rejected/deferred candidates such
  as `auto`, `eager`, `on-open`, `on-read`, and `on-base-commit`.
- Added an agent rule in `AGENTS.md` forbidding “split only” drift when work is
  blocked on missing language-facing naming or surface-contract choices:
  agents must either record the explicit current decision or add a short
  decision note with concrete candidate spellings, semantics, and a
  recommendation.
- Closed Deduce backlog item `B6.3f1a` at the honest shipped reopen contract:
  file-backed reopen preserves materialized snapshot rows and lifecycle
  metadata, but installed rules and dependency catalogs do not survive reopen,
  so reopened `deduce/analyze` resets to `rule-count = 0` and
  `incremental-dependency-edges = 0` until rules are reinstalled.
- Split Deduce backlog item `B6.3f1` at the real semantic boundary:
  `B6.3f1a` now tracks the canonical reopen contract for persisted
  rule/dependency catalogs, and `B6.3f1b` tracks any runtime/storage rollout
  that should only happen after that catalog-persistence contract is chosen.
- Split Deduce backlog item `B6.3f` at the real persistence boundary:
  `B6.3f1` now tracks persisted rule/dependency catalogs beyond the current
  snapshot contract, and `B6.3f2` tracks any later durable freshness/ready
  semantics that should only be defined after that catalog-persistence
  boundary exists.
- Split Deduce backlog item `B6.3e` at the real semantic boundary instead of
  guessing new language-facing declaration-policy names: `B6.3e1` now tracks
  the canonical policy naming/contract choice beyond `manual`, and `B6.3e2`
  tracks any parser/runtime/schema widening that should happen only after that
  naming decision exists.
- Closed Deduce backlog item `B6.3d` at the honest shipped persistence
  boundary: file-backed materialized relations now have an explicit current
  contract that persists the last stored snapshot rows across reopen in
  addition to materialized lifecycle metadata, while reopened schema/admin
  surfaces still remain conservatively unready until rules are reinstalled.
  Broader persisted rule/dependency catalogs and durable derived freshness
  semantics are promoted into follow-up item `B6.3f`.
- Closed Deduce backlog item `B6.3c` at the honest shipped declaration-policy
  boundary: declaration-time materialization is now recorded as an explicit
  manual-only contract, `[relation db materialized]` is documented as
  shorthand for `[relation db materialized manual]`, and broader
  declaration-policy widening is promoted to follow-up item `B6.3e` instead
  of leaving the shipped baseline looking perpetually partial.
- Reshaped the remaining Deduce `B6.4` backlog so the old `B6.4c` umbrella
  now closes as the shipped goal-directed execution baseline, and the true
  residual work is split explicitly into:
  - wider captured-call rewrite for `deduce/query` demand extraction
  - broader query-time magic-set / rewrite execution
  - migration and safety rules for wider recursive shapes
- Added explicit Deduce backlog items for the remaining gaps still marked
  `partial` or `deferred` in `docs/deduce-datalog-spec.md`, covering:
  - planner-backed conjunctive execution / explain truthfulness tightening
  - mutation logging / dirty-tracking contract beyond tracked recompute
  - true incremental derived-state maintenance
  - runtime provenance / why-result surface
  - richer integrity classes beyond the current key/unique/reference slice
  - parallel recursive evaluation beyond topology visibility
- Increased the backlog granularity again by splitting each of those new
  spec-derived residuals into two smaller items so contract/baseline work and
  runtime rollout work are no longer bundled together.
- Increased the backlog granularity one more time by splitting each of those
  twelve smaller residuals again into two subitems, so the open
  spec-derived Deduce queue now tracks twenty-four narrow contract/rollout
  slices rather than twelve medium ones.

- Landed the next `B6.4c` Deduce goal-directed query-demand validation slice:
  - the preserved-row literal-arg wrapper subset is now pinned explicitly
    when the forwarded `row` appears in a non-leading argument position on
    both the whole-filter side and the row-column side, including
    multi-literal-arg wrapper shapes that still collapse to the shipped
    equality-demand subset
  - added focused selector/plain regressions in
    `src/lisp/tests_deduce_query_groups.c3` so the remaining open gap stays
    "wider rewrite" rather than argument-position variants that are already
    shipped

- Landed the next `B6.4c` Deduce goal-directed query-demand slice:
  - the preserved-row wrapper helper already keeps short forwarding chains of
    extra-literal-arg wrappers on the ephemeral query path for both the
    whole-filter and row-column cases
  - added focused selector/plain regressions in
    `src/lisp/tests_deduce_query_groups.c3` to pin those chain shapes
    explicitly so the remaining open gap stays "wider rewrite" rather than
    already-shipped bounded chains

- Landed the next `B6.4c` Deduce goal-directed query-demand slice:
  - `deduce/query` demand extraction now also unwraps preserved-row captured
    wrapper calls with extra closed literal arguments on both the
    whole-filter side and the row-column side
  - this widening stays bounded to wrappers that still forward the current
    `row` directly and only bind the remaining arguments from closed literal
    expressions, so wider captured-call shapes still fall back to
    `selected-component-closure`
  - added focused selector/plain regressions in
    `src/lisp/tests_deduce_query_groups.c3` that pin the ephemeral query path
    through those preserved-row literal-arg wrapper shapes

- Landed the next `B6.4c` Deduce goal-directed query-demand slice:
  - `deduce/query` demand extraction now also follows short forwarding chains
    of one-argument captured wrappers for both the whole-filter case and the
    row-column-side case
  - this widening stays bounded rather than pretending generic captured-call
    rewrite is shipped: wider captured-call shapes outside that simple
    forwarding-chain pattern still fall back to
    `selected-component-closure`
  - added focused selector/plain regressions in
    `src/lisp/tests_deduce_query_groups.c3` that pin the ephemeral query path
    through those forwarding-chain shapes

- Landed the next `B6.4c` Deduce goal-directed query-demand slice:
  - `deduce/query` demand extraction now also unwraps one-argument captured
    closure wrappers on the row-column side when they forward the current
    `row` into a body that already resolves to a supported
    `(ref row 'column)` shape
  - this widening is still intentionally narrow and non-recursive:
    captured wrapper stacks on the row-column side still fall back to the
    existing `selected-component-closure` path instead of pretending generic
    captured-call rewrite is already shipped
  - added focused selector/plain regressions in
    `src/lisp/tests_deduce_query_groups.c3` that pin the ephemeral query path
    through that row-column captured-wrapper shape

- Landed the next `B6.4c` Deduce goal-directed query-demand slice:
  - `deduce/query` demand extraction now also unwraps one-argument captured
    closure wrappers around the whole filter body when they simply forward the
    current `row` into a body that already fits the shipped
    equality-demand subset
  - the widening is intentionally narrow and non-recursive:
    nested captured wrapper stacks still fall back to the existing
    `selected-component-closure` path instead of pretending generic
    captured-call rewrite is already shipped
  - added focused selector/plain regressions in
    `src/lisp/tests_deduce_query_groups.c3` that pin the ephemeral query path
    through that captured-wrapper shape

- Reshaped the remaining `B6.3` Deduce materialized-view backlog to match the
  real residual work:
  - closed the old `B6.3b` umbrella item as a backlog-shaping correction
    instead of leaving shipped declaration, dematerialize, and persisted
    lifecycle slices under one permanently-open heading
  - promoted the only real residuals into two explicit open items in
    `docs/plans/deduce-actionable-backlog-2026-03-20.md`:
    - `B6.3c` declaration policy beyond the current manual-only marker
    - `B6.3d` persisted derived-state durability semantics beyond the current
      lifecycle metadata
  - no runtime behavior changed in this patch; this is an active-plan truth
    correction

- Published the canonical Deduce maintenance analytics payload baseline:
  - `docs/deduce-datalog-spec.md` now has one source-of-truth section for
    relation-local `deduce/stats`, DB-level `deduce/analyze`,
    `deduce/refresh!`, and the current maintenance/admin structured rejection
    keys
  - corrected the docs to match the shipped runtime surface exactly:
    `materialized-refresh-policy` is currently a `deduce/schema` /
    `deduce/refresh!` field, not a `deduce/stats` field
  - `docs/reference/08-libraries.md` now points to that spec section instead
    of duplicating field inventories, and the first analytics-extension
    milestone item is closed in
    `docs/plans/deduce-analytics-extension-milestone-2026-03-24.md`

- Closed the next Deduce analytics-extension regression item:
  - added focused public-surface coverage in
    `src/lisp/tests_deduce_query_groups.c3` for:
    - zero-row materialized refresh staying deterministic
    - the dropped-handle split where `deduce/stats` still reports a
      deterministic payload but `deduce/refresh!` rejects with
      `deduce/refresh-relation-dropped`
  - extended `src/lisp/tests_deduce_durability_groups.c3` so persisted but
    still-unready materialized declarations also pin the DB-wide
    `deduce/refresh!` payload (`0` refreshed views, honest remaining stale
    relation list) after reopen
  - closed the matching milestone item in
    `docs/plans/deduce-analytics-extension-milestone-2026-03-24.md`

- Closed the stats-maintenance benchmark harness item for the Deduce analytics
  extension milestone:
  - added `run_deduce_stats_maintenance_benchmarks(...)` under the existing
    `OMNI_DEDUCE_BENCH=1` gate with one bounded smoke-oriented summary lane:
    - tracked fact churn over a seeded materialized relation, asserting
      targeted relation refreshes and post-refresh stats stability
    - forced rule-set-change invalidation, asserting the relation-scoped
      refresh fallback to DB-wide rebuild plus post-refresh stats recovery
  - added benchmark helper coverage in
    `src/lisp/tests_deduce_query_bench_groups.c3` and documented the emitted
    `OMNI_BENCH_SUMMARY suite=deduce_stats_maintenance ...` fields in
    `docs/plans/deduce-stats-maintenance-benchmark-notes-2026-03-25.md`

- Closed the first cleanup/admin-verb item in the Deduce analytics extension
  milestone:
  - published `docs/deduce-datalog-spec.md#74-cleanup-verb-matrix` as the
    canonical cleanup-surface contract for `deduce/retract!`,
    `deduce/clear!`, and `deduce/drop!`
  - documented:
    - the only approved cleanup verb names
    - accepted call shapes, including the current transaction-only support for
      `retract!`
    - deterministic no-op cases for absent tuples, empty relations, and
      repeated drops
    - the current rejection-code matrix
  - updated `docs/reference/08-libraries.md` to point to the spec section
    instead of carrying a second drifting cleanup contract summary
  - corrected the milestone target from the old cleanup implementation file to
    `src/lisp/deduce_relation_ops_mutations.c3`, where the cleanup verb
    handlers currently live

- Closed the cleanup-path audit item for the Deduce analytics extension
  milestone:
  - extended `src/lisp/tests_deduce_durability_groups.c3` to pin current
    cleanup edge behavior:
    - `retract!` on a missing tuple is a `Void` no-op
    - repeated `drop!` on the same dropped relation handle is a `Void` no-op
    - `retract!` and `clear!` on a dropped relation handle reject
      deterministically with `deduce/retract-relation-dropped` and
      `deduce/clear-relation-dropped`
  - that audited behavior is now aligned with the published cleanup matrix in
    `docs/deduce-datalog-spec.md#74-cleanup-verb-matrix`

- Closed the dedicated cleanup admin-task regression item for the Deduce
  analytics extension milestone:
  - extended `src/lisp/tests_deduce_query_groups.c3` with a public-surface
    write-intent regression that pins code-first cleanup failures for:
    - `retract!` tuple arity mismatch
    - `retract!` transaction/DB mismatch
    - `clear!` and `drop!` invoked with the wrong Deduce handle kind
  - together with the durability-side no-op/dropped-handle regressions, the
    current cleanup contract is now pinned across both write-intent failure and
    no-op maintenance surfaces

- Restored the shipped `deduce_scan_query_count` benchmark lane so the current
  source matches the existing benchmark notes and Docker perf-envelope script:
  - implemented `run_deduce_scan_query_count_benchmarks(...)` in
    `src/lisp/tests_deduce_query_bench_groups_more.c3` using the existing seed
    helper and scan-range materialization counters
  - the lane now emits the expected
    `OMNI_BENCH_SUMMARY suite=deduce_scan_query_count ...` fields for:
    - scan
    - full-range scan-range
    - query
    - count
    - scan-range materialization counters
  - updated `docs/plans/deduce-scan-query-count-benchmark-baseline-2026-03-11.md`
    to match the current implementation location and summary surface
  - closed the remaining open post-complete backlog items for:
    - deterministic Deduce cleanup/statistics behavior
    - Deduce scan/query/copy hotspot benchmark coverage

- Landed the formatter CLI baseline:
  - added `omni --fmt <file>` as a first-party conservative formatter path
  - default mode prints formatted source to stdout,
    `--write` rewrites in place, and `--check` returns non-zero when a file
    needs formatting changes
  - the current formatter contract is intentionally narrow and matches the
    existing LSP formatter behavior: normalize structural indentation, align
    wrapped `export` / `export-from` payloads and wrapped `let` binding-list
    continuations, trim trailing whitespace, preserve blank lines, and avoid
    aggressive intra-line rewrites
  - updated `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, and
    `docs/plans/editor-tooling-roadmap.md` to reflect the shipped CLI surface

- Landed Neovim formatter-tool integration for the formatter CLI:
  - added `tooling/omni-nvim/lua/omni/formatter.lua` plus exported helper APIs
    so `conform.nvim` can register the shipped `omni --fmt --write $FILENAME`
    formatter without ad-hoc per-user command duplication
  - added `:OmniConformSetupSpec` to print the ready-to-merge formatter setup
    table for `conform.nvim`
  - documented the Conform setup path in `tooling/omni-nvim/README.md` and
    `tooling/omni-nvim/doc/omni.nvim.txt`
  - closed the remaining formatter-tool integration roadmap item in
    `docs/plans/editor-tooling-roadmap.md`
  - reshaped the formatting roadmap so the rollout decision is explicitly
    closed as `both in phases`, leaving only the real remaining formatter
    follow-up: broader full-layout rewrites beyond the shipped conservative
    continuation-aware formatter

- Landed the next formatter semantics slice:
  - `omni --fmt` now goes beyond pure bracket-depth indentation by aligning
    wrapped `export` / `export-from` payloads and wrapped `let` binding-list
    continuations
  - `tooling/omni-lsp` mirrors the same conservative continuation rules for
    full-document formatting so the shipped CLI and editor formatting paths do
    not drift
  - added regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`
  - split the old vague formatter backlog item so the shipped continuation
    formatter is closed and only broader full-layout rewriting remains open

- Landed the next formatter preservation slice:
  - conservative formatting now preserves the existing repo-style indentation
    for multiline `if` branches instead of flattening them to shallow
    bracket-depth output
  - inline block forms inside `let` binding lists now stay aligned to their
    binding context, which preserves current `let ^rec` and similar in-tree
    multiline bindings
  - `tooling/omni-lsp` mirrors the same preservation rules so editor and CLI
    formatting stay on the same contract
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

- Landed the next generic-call formatter slice:
  - conservative formatting now preserves wrapped generic call and pipeline
    continuations by aligning them under the first argument instead of reducing
    them to shallow block indentation
  - `tooling/omni-lsp` mirrors the same generic-call continuation behavior so
    editor and CLI formatting stay aligned
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

- Landed the newline-preservation formatter slice:
  - conservative formatting now preserves the source file's existing newline
    style (`LF` vs `CRLF`) instead of rewriting line endings as a side effect
  - `tooling/omni-lsp` mirrors the same newline-preservation behavior so editor
    and CLI formatting remain on the same contract
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

- Landed the nested control-flow/effect formatter slice:
  - conservative formatting now preserves nested `if` body indentation from the
    actual `if` column when the branch form starts later in a clause line,
    which keeps current `match`-arm layouts from collapsing toward the clause
    margin
  - `when` / `unless` / `raise` / `checkpoint` now format as block-style forms
    instead of inheriting generic continuation alignment
  - `tooling/omni-lsp` mirrors the same control-flow/effect indentation rules
    so editor and CLI formatting stay on one contract
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

- Landed the higher-order lambda formatter slice:
  - conservative formatting now preserves multiline lambda bodies in current
    in-tree higher-order collection calls (`map`, `foldl`, `foldr`, `filter`,
