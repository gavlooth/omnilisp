    - current support now covers head-bound multi-body extensional rules while
      still rejecting existential/search-based lineage with
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to include a reconstructible multi-body
      extensional rule with two fact supports and to keep the existential
      non-recursive case explicitly unsupported
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a5`
    - promoted the remaining existential/search-based non-recursive lineage
      work into `B6.9a6`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(ok 2 error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the next truthful non-recursive `deduce/why-result` slice beyond
    direct-copy lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened the public one-body extensional provenance matcher so the source
      tuple can be reconstructed from head-row variable/literal bindings
    - current support now covers direct-copy plus one-body projection and
      permutation rules
    - multi-body or existential derived shapes still return
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to include a one-body projection /
      permutation derived relation in addition to direct-copy, missing, and
      unsupported multi-body cases
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a4` at the one-body extensional boundary
    - promoted the remaining multi-body or existential non-recursive lineage
      work into `B6.9a5`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(ok ok parent error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the first non-recursive derived `deduce/why-result` slice without
    overclaiming broader provenance support.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - added the first derived provenance path builder for `deduce/why-result`
    - stored rows in relations with exactly one non-negated, non-aggregate,
      one-body direct-copy rule from an extensional source now return
      `status = ok` with one deterministic `derived` path
    - broader derived shapes still return
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to cover seed `ok`, direct-copy
      derived `ok`, missing `missing`, and broader derived `error`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a3` as the first non-recursive derived provenance slice
    - promoted the remaining broader non-recursive derived lineage work into
      `B6.9a4`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(ok derived parent error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the first public `deduce/why-result` runtime slice without
    pretending derived lineage is already shipped.
- Code/config changes made
  - Updated `src/lisp/deduce.c3` and `src/lisp/eval_init_primitives.c3`:
    - wired `why-result` through unified Deduce dispatch and the
      `deduce/why-result` namespaced primitive surface
  - Updated `src/lisp/deduce_schema_query.c3`:
    - shipped `(deduce/why-result relation val...)` for stored row subjects
    - extensional stored tuples now return deterministic `status = ok`
      provenance payloads with one `seed` path and one `fact` support frame
    - missing tuples return `status = missing`
    - stored derived rows now return `status = error` with
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added regression coverage for the public seed-row `ok`, missing-row
      `missing`, and stored-derived-row `error` payloads
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a2` as the public seed-row surface
    - promoted the remaining non-recursive derived-lineage work into `B6.9a3`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(ok 1 missing 0 error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the why-result payload/status baseline before adding any public
    provenance runtime surface.
- Code/config changes made
  - Updated `docs/deduce-datalog-spec.md`:
    - fixed the canonical top-level why-result status semantics for `ok`,
      `missing`, `partial`, and `error`
    - fixed the canonical envelope keys that all future why-result surfaces
      must carry
  - Updated `docs/reference/08-libraries.md`, `TODO.md`, and
    `memory/CHANGELOG.md` so this closure lands as `B6.9a1`
- Validation run
  - doc-state inspection against the existing provenance / why-result section
    in `docs/deduce-datalog-spec.md`

- Objectives attempted
  - Close the first derived-source maintained-update slice for incremental
    materialized refresh.
- Code/config changes made
  - Updated `src/lisp/deduce_db_handles_mutation_tracking.c3`:
    - generalized the direct-copy incremental classifier
    - added the first derived-source queue path for one-step downstream
      materialized direct-copy targets
  - Updated `src/lisp/deduce_schema_query.c3`:
    - allowed `incremental-targeted` refresh when the immediate source is an
      already-refreshed ready materialized direct-copy relation
    - propagated inserted tuple keys one step downstream after a successful
      incremental refresh
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a `edge -> mid -> top` regression proving `mid` stays
      `incremental-targeted`, `top` remains stale until refreshed, and `top`
      then refreshes on the same `incremental-targeted` path
  - Updated `docs/deduce-datalog-spec.md`, `docs/reference/08-libraries.md`,
    `TODO.md`, and `memory/CHANGELOG.md` so this closure lands as `B6.8b2`
- Validation run
  - `c3c build`
  - host-side direct-copy chain probe:
    - returned `(incremental-targeted (top-chain) 1 incremental-targeted 2 2 2 nil)`

- Objectives attempted
  - Close the first ordinary maintained-update runtime slice for incremental
    materialized refresh.
- Code/config changes made
  - Updated `src/lisp/deduce_db_handles_mutation_tracking.c3` and
    `src/lisp/deduce_db_handles_mutation_txn.c3`:
    - queued committed inserted tuple keys per supported materialized target
    - escalated pending-queue tracking failure to the existing
      `full-recompute-required` boundary
  - Updated `src/lisp/deduce_schema_query.c3`:
    - added the narrow `incremental-targeted` relation refresh path for
      already-refreshed direct-copy extensional materialized targets
    - reused that path for stale `on-read` refresh
    - cleared pending incremental queue state on dematerialize
  - Updated `src/lisp/tests_deduce_query_groups.c3` and
    `src/lisp/tests_deduce_query_bench_groups.c3`:
    - pinned that sibling materialized targets keep independent pending queues
      and that supported relation refresh reports `incremental-targeted`
  - Updated `docs/deduce-datalog-spec.md`, `docs/reference/08-libraries.md`,
    `TODO.md`, and `memory/CHANGELOG.md` so this closure lands as `B6.8b1`
- Validation run
  - `c3c build`
  - host-side direct-copy sibling refresh probe:
    - returned `(incremental-targeted 2 1 2 true)`
  - host-side `on-read` direct-copy probe:
    - returned `(deduce/on-read-refresh-unavailable 1 2 2 nil)`

- Objectives attempted
  - Close the incremental-maintenance fallback/admin boundary slice before any
    maintained-update runtime work widens the surface.
- Code/config changes made
  - Updated `docs/deduce-datalog-spec.md`:
    - added `7.3.6 Incremental Maintenance Fallback/Admin Boundary`
    - fixed the approved public fallback vocabulary and truth requirements for
      analyze/stats/refresh/read surfaces
  - Updated `docs/reference/08-libraries.md`, `TODO.md`, and
    `memory/CHANGELOG.md` so this closure lands as `B6.8a2`
- Validation run
  - doc-state inspection against the already-shipped `tracked` /
    `full-recompute` admin surfaces in:
    - `src/lisp/deduce_db_handles_mutation_tracking.c3`
    - `src/lisp/deduce_rule_eval_prims.c3`
    - `src/lisp/deduce_schema_query.c3`

- Objectives attempted
  - Close the incremental delta substrate data-model slice by turning the
    already-shipped internal seminaive substrate into canonical doc truth.
- Code/config changes made
  - Updated `docs/deduce-datalog-spec.md`:
    - added `7.3.5 Incremental Delta Substrate Baseline`
    - documented the current internal signed-delta, support-table, and
      current/next iteration-buffer model
  - Updated `docs/reference/08-libraries.md`, `TODO.md`, and
    `memory/CHANGELOG.md` so this closure lands as `B6.8a1`
- Validation run
  - doc-state inspection against:
    - `src/lisp/deduce_rule_eval_exec.c3`
    - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
    - `src/lisp/deduce_rule_eval_fixpoint.c3`

- Objectives attempted
  - Close the admin-truth alignment slice by pinning how `stats`, `analyze`,
    and `refresh!` line up after degraded rule-change recovery.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused regression for the rule-set-change path where
      relation-local stats first report `full-recompute-required`
    - pinned that a plain DB-level analyze clears the degraded mode, a later
      relation-scoped refresh stays targeted, and untouched stale peers remain
      visible on refresh/analyze outputs
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.7b2`
- Validation run
  - `c3c build`
  - host-side admin-alignment probe: returned
    `(full-recompute 2 targeted nil 1 (ancestor-peer) tracked 1 (ancestor-peer))`

- Objectives attempted
  - Close the dirty-frontier truth slice by pinning the relation-local stats
    view after partial selected-component recovery.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused regression showing that two different relation stats
      payloads share the same residual dirty frontier after selected analyze
    - pinned that `dirty-self` is the local bit: false for the cleared
      relation, true for the untouched relation
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.7b1`
- Validation run
  - `c3c build`
  - host-side dirty-frontier probe: returned
    `(2 (other-src-s other-out-s) nil 2 (other-src-s other-out-s) true)`

- Objectives attempted
  - Close the degraded-state and recovery contract slice by pinning what
    `full-recompute` looks like immediately before and immediately after the
    current DB-level recovery path.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused regression showing that destructive writes first surface
      `full-recompute-required` on relation-local stats
    - pinned that plain DB-level `deduce/analyze` returns a degraded snapshot
      payload, then clears the live dirty/admin state for subsequent stats
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.7a2`
- Validation run
  - `c3c build`
  - host-side degraded analyze probe: returned
    `(true full-recompute 2 (edge-dr reach-dr) nil 1)`

- Objectives attempted
  - Close the commit/abort mutation-log contract slice by pinning when
    deferred write-block mutations actually affect dirty/admin state.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused admin-surface regression covering committed insert,
      aborted insert, and committed delete under write transactions
    - pinned that commit applies the deferred mutation log, abort drops it,
      and destructive committed writes escalate to `full-recompute`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.7a1`
- Validation run
  - `c3c build`
  - host-side commit insert probe: returned `(2 tracked (edge-txc reach-txc))`
  - host-side abort insert probe: returned `(0 tracked nil 0)`
  - host-side commit delete probe: returned
    `(2 full-recompute (edge-tdc reach-tdc) 1)`

- Objectives attempted
  - Close the conjunctive counter-truth slice by pinning the three current
    observed-counter surfaces instead of implying they are one shared source.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused preserved-bound selector regression that compares
      `deduce/stats.last-goal-directed-read-step-counters`,
      `deduce/analyze.rule-execution[*].steps[*].counters`, and
      `deduce/explain.steps[*].counters`
    - pinned the current truthful boundary where stats reports the earlier
      preserved-bound read, while analyze and explain report their own
      observed execution counters
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.6b2`
- Validation run
  - `c3c build`
  - host-side conjunctive counter probe: returned `true`

- Objectives attempted
  - Close the conjunctive path/order truth slice by pinning the current
    planner-derived join-order/operator boundary shared by `deduce/explain`
    and `deduce/analyze`.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused non-recursive conjunctive regression that compares
      `deduce/explain.steps[*]` with `deduce/analyze.rule-execution[*].steps[*]`
    - pinned shared `join-order`, `predicate`, `operator`, and
      `selected-index` fields on the current planner-derived shape
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.6b1`
- Validation run
  - `c3c build`
  - host-side conjunctive order/operator probe: returned `true`

- Objectives attempted
  - Close the missing `deduce/analyze` classification slice by pinning the
    already-shipped selector-planner versus last-runtime-read boundary for
    conjunctive recursive selectors.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused selector-scoped `deduce/analyze` regression after a real
      `deduce/query`
    - pinned top-level planner-side `goal-directed-execution-path`,
      `goal-directed-selector-rule-index`, and
      `goal-directed-selected-component-id`
    - pinned separate `last-goal-directed-read-*` runtime metadata and
      observed `rule-execution[*].steps[*].counters`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.6a2`
- Validation run
  - `c3c build`
  - host-side analyze classification probe: returned `true`

- Objectives attempted
  - Close the missing `deduce/explain` classification slice by pinning the
    already-shipped planner-snapshot versus last-runtime-read boundary for
    conjunctive recursive rules.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused `deduce/explain` regression for a two-step recursive
      conjunctive rule after an actual `deduce/query`
    - pinned `surface-kind = 'planner-snapshot`,
      `goal-directed-execution-path = 'selected-component-closure`,
      `last-goal-directed-read-execution-path = 'ephemeral-head-demand-query`,
      `last-goal-directed-read-surface = 'query`, and observed step counters
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.6a1` and the remaining explain/analyze/runtime-truth
    work stays in `B6.6a2` and `B6.6b*`
- Validation run
  - `c3c build`
  - host-side explain classification probe: returned `true`

- Objectives attempted
  - Close the next recursive symbolic safety slice by pinning the current
    fallback boundary for multi-atom recursive query demands.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain `deduce/query` regressions for
      `path(x,z) :- path(x,y), path(y,z)` with a fully-bound `(src,dst)`
      filter
    - pinned those shapes on truthful `selected-component-closure`
      fallback with zero requested/applied bound counts
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    safety slice closes as `B6.4f5a`, while the actual multi-atom recursive
    support work stays open as `B6.4f5b`
- Validation run
  - host-side selector multi-atom recursive query probe: returned
    `(({src 1 dst 4}) selected-component-closure 0 0 dirty-closure)`
  - host-side plain multi-atom recursive query probe: returned
    `(({src 1 dst 4}) selected-component-closure 0 0 dirty-closure)`

- Objectives attempted
  - Close the first true jointly-supported recursive multi-position query
    support slice beyond carried-position relaxation and permutation fallback.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3`:
    - narrowed the same-index preservation check so non-recursive seed rules
      no longer block multi-position recursive demand support
    - same-index preservation is now judged only against the positive
      self-recursive rules that actually matter for recursive carry
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain fully-bound pair regressions for
      `stable(x,y) :- edge(x,y)` plus `stable(x,y) :- stable(x,y)`
    - added selector/plain disjunctive fully-bound regressions for the same
      same-index self-recursive shape
    - pinned those shapes on `ephemeral-head-demand-query` with requested and
      applied positions `(0 1)`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so
    `B6.4f4b` closes and the honest residual is now `B6.4f5`
- Validation run
  - `c3c build`
  - host-side selector same-index recursive pair probe: returned
    `(({src 1 dst 2}) ephemeral-head-demand-query (0 1) (0 1) nil)`
  - host-side plain same-index recursive pair probe: returned
    `(({src 1 dst 2}) ephemeral-head-demand-query (0 1) (0 1) nil)`
  - host-side selector same-index recursive disjunction probe: returned
    `(({src 1 dst 2} {src 10 dst 11}) ephemeral-head-demand-query (0 1) (0 1) nil)`
  - host-side plain same-index recursive disjunction probe: returned
    `(({src 1 dst 2} {src 10 dst 11}) ephemeral-head-demand-query (0 1) (0 1) nil)`

- Objectives attempted
  - Close the next honest recursive symbolic safety slice after proving that
    jointly-supported permutation demands still cannot be answered by the
    current ephemeral evaluator.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3`:
    - added same-index recursive preservation checks so the query layer can
      distinguish “jointly supported in-place” from “jointly supported only
      through permutation”
  - Updated `src/lisp/deduce_schema_query.c3`:
    - split recursive multi-position handling into three cases:
      keep the current one-position relaxation for carried-position shapes,
      preserve all positions only when they are same-index-preserved, and
      fall back to `selected-component-closure` for jointly-supported
      permutation shapes the evaluator still cannot seed truthfully
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain symmetric recursive pair regressions
    - added selector/plain symmetric recursive disjunctive regressions
    - pinned those shapes on truthful `selected-component-closure`
      fallback instead of a false `ephemeral-head-demand-query` claim
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    safety slice closes as `B6.4f4a`, while the true joint multi-position
    recursive support stays open as `B6.4f4b`
- Validation run
  - `c3c build`
  - host-side selector symmetric recursive pair probe: returned
    `(({src 2 dst 1}) selected-component-closure 0 0 dirty-closure)`
  - host-side plain symmetric recursive pair probe: returned
    `(({src 2 dst 1}) selected-component-closure 0 0 dirty-closure)`
  - host-side selector symmetric recursive disjunction probe: returned
    `(({src 2 dst 1} {src 11 dst 10}) selected-component-closure 0 0 dirty-closure)`
  - host-side plain symmetric recursive disjunction probe: returned
    `(({src 2 dst 1} {src 11 dst 10}) selected-component-closure 0 0 dirty-closure)`

- Objectives attempted
  - Close the next honest recursive symbolic slice by fixing reordered
    recursive head shapes where the carried recursive position is not the
    last column.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - replaced the recursive demand relaxation heuristic again so it keeps
      the head position preserved by the positive self-recursive rule, rather
      than the last bound position
    - applied that preserved-position relaxation across selector/plain
      single-branch and disjunctive ephemeral query paths
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain swapped-column recursive pair regressions
    - added selector/plain swapped-column recursive disjunctive regressions
    - pinned admin truth so reordered recursive heads now keep applied
      position `(0)` instead of incorrectly narrowing to `(1)`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so
    `B6.4f3` closes and the honest residual is now `B6.4f4`
- Validation run
  - `c3c build`
  - host-side ordinary selector recursive pair smoke: returned
    `(ephemeral-head-demand-query (0 1) (1) 3)`
  - host-side ordinary plain recursive disjunction smoke: returned
    `(2 ephemeral-head-demand-query (0 1) (1))`
  - host-side swapped selector recursive pair smoke: returned
    `(({src 1 dst 3}) ephemeral-head-demand-query (0 1) (0))`
  - host-side swapped plain recursive disjunction smoke: returned
    `(({src 1 dst 3} {src 10 dst 11}) ephemeral-head-demand-query (0 1) (0))`

- Objectives attempted
  - Close the first bounded `B6.4f2` recursive symbolic slice
    instead of leaving the whole recursive query lane open.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - replaced the recursive projected-demand relaxation helper so it keeps
      the trailing supported bound position instead of the first one
    - applied that bounded relaxation across selector/plain single-branch and
      disjunctive ephemeral query paths
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - converted the recursive pair selector/plain regressions from truthful
      fallback expectations to bounded ephemeral execution expectations
    - pinned the exact admin truth for this slice:
      requested bound positions stay `(0 1)` while applied positions narrow
      to `(1)`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so
    `B6.4f2` closes and the honest residual is now `B6.4f3`
- Validation run
  - `c3c build`
  - host-side selector recursive pair bounded-ephemeral smoke: returned
    `(({src 1 dst 3}) ephemeral-head-demand-query (0 1) (1) nil)`
  - host-side plain recursive pair bounded-ephemeral smoke: returned
    `(({src 1 dst 3}) ephemeral-head-demand-query (0 1) (1) nil)`
  - host-side selector recursive disjunctive pair bounded-ephemeral smoke:
    returned
    `(({src 1 dst 3} {src 10 dst 11}) ephemeral-head-demand-query (0 1) (1) nil)`
  - host-side plain recursive disjunctive pair bounded-ephemeral smoke:
    returned
    `(({src 1 dst 3} {src 10 dst 11}) ephemeral-head-demand-query (0 1) (1) nil)`

- Objectives attempted
  - Recheck whether `B6.4e2c` is a real remaining runtime lane or just stale
    wording after the shipped symbolic/disjunctive slices.
- Code/config changes made
  - Ran direct host-side probes against a non-recursive derived one-rule
    subject and confirmed there is no separate non-recursive goal-directed
    symbolic disjunction lane in the current planner:
    - selector-scoped reads fail with `deduce/query: selected rule is not
      goal-directed eligible`
    - plain reads stay on the ordinary `no-op` path
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` to retire
    stale item `B6.4e2c` and track the real remaining symbolic work only
    under `B6.4f2`
- Validation run
  - host-side selector non-recursive symbolic disjunction probe: confirmed
    `selected rule is not goal-directed eligible`
  - host-side plain non-recursive symbolic disjunction probe: returned `no-op`

- Objectives attempted
  - Close the first honest `B6.4f` safety slice by making recursive
    multi-position symbolic query demands fall back truthfully instead of
    claiming ephemeral execution with incomplete results.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - added a shared bound-position counter for projected head demands
    - added a recursive-component guard that refuses ephemeral execution when
      a recursive selected component receives a multi-position projected
      symbolic demand
    - applied that guard to both single-branch and disjunctive selector/plain
      ephemeral query paths
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for single recursive symbolic pair
      filters
    - kept the selector/plain recursive disjunctive pair regressions on the
      truthful `selected-component-closure` fallback path
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    so this shipped safety slice closes as `B6.4f1`, while the remaining
    recursive support work stays open as `B6.4f2`
- Validation run
  - `c3c build`
  - host-side selector single recursive pair fallback smoke: returned `true`
  - host-side plain single recursive pair fallback smoke: returned `true`
  - host-side selector recursive disjunctive pair fallback smoke: returned
    `true`
  - host-side plain recursive disjunctive pair fallback smoke: returned
    `true`

- Objectives attempted
  - Close the next honest `B6.4e2b2` disjunctive rewrite slice by widening
    the shipped ephemeral union path from same-position branches to
    mixed-position demand-safe branches.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - removed the stale same-position-only guard from the disjunctive
      projected-demand executors
    - kept the runtime boundary narrow to branches that still individually
      reduce to the already shipped demand-safe subset
    - removed the now-dead helper for “same single bound position” support
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - changed the selector/plain mixed-position disjunction regressions to
      assert `ephemeral-head-demand-query`
    - aligned the expected result set to the truthful two-row union produced
      by those branch-local runs
    - flattened the test-local `let` bindings into single multi-binding
      forms
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    so this shipped slice closes as `B6.4e2b2`, while the real residual is
    now broader symbolic multi-branch rewrite under `B6.4e2c`
- Validation run
  - `c3c build`
  - host-side selector mixed-position disjunction smoke: returned `true`
  - host-side plain mixed-position disjunction smoke: returned `true`

- Objectives attempted
  - Close the next honest `B6.4e2b` residual slice by pinning that
    same-position disjunctive branches can still carry branch-local residual
    conjunct filtering on the ephemeral query path.
- Code/config changes made
  - Added focused regressions in `src/lisp/tests_deduce_query_groups.c3` for:
    - selector-scoped same-position disjunctive branches with residual
      unsupported conjuncts
    - plain same-position disjunctive branches with residual unsupported
      conjuncts
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    so this shipped slice closes as `B6.4e2b1`, while the real residual stays
    open under `B6.4e2b2`
- Validation run
  - `c3c build`
  - host-side selector same-position residual-conjunct disjunction smoke:
    returned `true`
  - host-side plain same-position residual-conjunct disjunction smoke:
    returned `true`

- Objectives attempted
  - Tighten the already-closed disjunctive `deduce/query` rewrite slices to
    the runtime boundary they can actually defend, and pin the still-open
    `B6.4e2b2` residual with fallback regressions instead of letting it drift.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - kept the shipped disjunctive union path on one shared projected bound
      position across all branches
    - changed the branch execution model to one abortable temporary write txn
      per shipped disjunctive branch instead of stacking multiple projected
      demands into one txn
    - kept same-position wrapper branches on the ephemeral path
    - forced broader mixed-position disjunctions back onto
      `selected-component-closure`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - retained the passing same-position selector/plain disjunction
      regressions
    - added selector/plain mixed-position disjunction regressions that now
      assert the fallback path instead of an unsound ephemeral result
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so the
    shipped boundary now says “separate txns plus same-position-only” rather
    than implying broader same-txn disjunctive rewrite support.
- Validation run
  - `c3c build`
  - host-side selector same-position disjunction smoke: returned `true`
  - host-side plain same-position disjunction smoke: returned `true`
  - host-side selector mixed-position disjunction fallback smoke: returned
    `true`
  - host-side plain mixed-position disjunction fallback smoke: returned
    `true`

- Objectives attempted
  - Close the next honest `B6.4e` residual slice by widening the disjunctive
    ephemeral query path to same-position wrapper branches without widening
    into mixed-position multi-branch rewrite.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - kept the disjunctive ephemeral query path constrained to one shared
      projected bound position across all branches
    - reused that boundary for both the requested and projected disjunctive
      demand cases
    - left mixed-position disjuncts on the ordinary selected-closure path
  - Added focused regressions in
    `src/lisp/tests_deduce_query_groups.c3` for:
    - selector-scoped same-position disjunctive wrapper branches
    - plain same-position disjunctive wrapper branches
    - selector/plain mixed-position disjuncts that still fall back
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    so this shipped slice closes as `B6.4e2a` and the wider residual remains
    open under `B6.4e2b`
- Validation run
  - `c3c build`
  - host-side selector same-position wrapper disjunction smoke: returned
    `true`
  - host-side plain same-position wrapper disjunction smoke: returned `true`
  - host-side selector mixed-position disjunction fallback smoke: returned
    `true`
  - host-side plain mixed-position disjunction fallback smoke: returned
    `true`

- Objectives attempted
  - Close the next live `TODO.md` execution slice after `B6.4d` by shipping
    the first honest multi-branch query-time rewrite path for `deduce/query`
    instead of another single-demand extractor widening.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
