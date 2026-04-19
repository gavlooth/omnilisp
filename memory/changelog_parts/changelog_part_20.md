# Memory Changelog Index Part 20

Source: `memory/CHANGELOG.md`

    - `(deduce/match relation pattern rule-index)`
    - `(deduce/match relation pattern rule-index 'naive)`
    - `(deduce/match relation pattern rule-index 'semi-naive)`
  - selected match executes only the chosen component dependency closure
    before matching and leaves unrelated recursive components untouched
  - selector-scoped match rejects explicitly when:
    - the selected recursive shape is aggregate-bearing
    - the selected recursive shape has negated body atoms
    - the selected closure does not produce the requested relation
    - the DB has already escalated to `full-recompute-required`
  - focused regression coverage now pins:
    - selected match execution that leaves unrelated derived components
      untouched
    - relation/selector mismatch rejection
    - aggregate recursive selector rejection
    - negated recursive selector rejection
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `189 passed, 0 failed`
- Landed the second `B6.4c` Deduce query-time goal-directed execution
  slice:
  - `deduce/query` now supports selector-scoped execution for that same
    currently eligible positive recursive shape through:
    - `(deduce/query relation filter-fn rule-index)`
    - `(deduce/query relation filter-fn rule-index 'naive)`
    - `(deduce/query relation filter-fn rule-index 'semi-naive)`
  - selected query executes only the chosen component dependency closure
    before filtering rows and leaves unrelated recursive components untouched
  - selector-scoped query rejects explicitly when:
    - the selected recursive shape is aggregate-bearing
    - the selected recursive shape has negated body atoms
    - the selected closure does not produce the requested relation
    - the DB has already escalated to `full-recompute-required`
  - focused regression coverage now pins:
    - selected query execution that leaves unrelated derived components
      untouched
    - relation/selector mismatch rejection
    - aggregate recursive selector rejection
    - negated recursive selector rejection
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `193 passed, 0 failed`
- Landed the third `B6.4c` Deduce query-time goal-directed execution
  slice:
  - `deduce/count`, `deduce/scan`, and `deduce/scan-range` now support
    selector-scoped execution for that same currently eligible positive
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
  - selector-scoped count / scan / scan-range execute only the chosen
    component dependency closure before reading rows and leave unrelated
    recursive components untouched
  - selector-scoped count / scan / scan-range reject explicitly when:
    - the selected recursive shape is aggregate-bearing
    - the selected recursive shape has negated body atoms
    - the selected closure does not produce the requested relation
    - the DB has already escalated to `full-recompute-required`
  - focused regression coverage now pins:
    - selected count / scan / scan-range execution that leaves unrelated
      derived components untouched
    - count relation/selector mismatch rejection
    - scan aggregate recursive selector rejection
    - scan-range negated recursive selector rejection
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `201 passed, 0 failed`

## 2026-03-22

- Reshaped the live Deduce backlog so completed umbrella slices stop looking
  perpetually open:
  - `B6.3a` now names the shipped materialized-view admin/refresh surface and
    is treated as closed
  - `B6.3b` now isolates the still-open declaration/durability policy work
  - `B6.4a` now covers the shipped goal-directed
    planner/admin/selected-closure visibility surface and is treated as
    closed
  - `B6.4c` now isolates the still-open query-time magic-set / goal-directed
    rewrite lane
  - `B6.5` is now treated as closed; the shipped work covers both the
    admin-visibility surface and the scheduler/runtime batch-dispatch seams
  - this was a backlog/doc-structure correction only; no runtime behavior
    changed

- Landed the first `B6.3` Deduce materialized-view slice:
  - explicit manual materialization now ships for derived relations through
    `deduce 'materialize! relation` and `deduce/materialize!`
  - manual DB-wide refresh now ships through `deduce 'refresh! db` and
    `deduce/refresh!`
  - `deduce/schema`, `deduce/stats`, and `deduce/analyze` now expose
    materialized-view freshness metadata:
    - `materialized-view`
    - `materialized-refresh-policy`
    - `materialized-refresh-count`
    - `materialized-last-refresh-mutation-epoch`
    - `materialized-stale`
  - `deduce/analyze` is now explicitly diagnostic-only for this lane:
    it reports stale materialized views but does not silently refresh them
  - focused regression coverage now pins:
    - rejecting `materialize!` on non-derived relations
    - namespaced `deduce/materialize!` / `deduce/refresh!` registration
    - stale-before-refresh, explicit manual refresh, refresh-count increment,
      and stale-again after a base mutation
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `171 passed, 0 failed`
- Landed the second `B6.3` Deduce materialized-view slice:
  - `deduce/refresh!` now accepts either a Deduce DB handle or a
    materialized relation handle
  - non-materialized relation handles now reject refresh explicitly with
    `deduce/refresh-relation-not-materialized`
  - `deduce/schema` and `deduce/stats` now expose:
    - `materialized-last-stale-mutation-epoch`
    - `materialized-last-stale-reason`
  - dependency-driven invalidation now records the stale reason
    `dependency-dirty`, and refresh clears that reason back to `nil`
  - relation-scoped refresh still reuses the existing full analyze/fixpoint
    path internally; this is a narrower admin surface, not selective
    incremental maintenance
  - focused regression coverage now pins:
    - non-materialized relation-handle refresh rejection
    - relation-scoped refresh success
    - stale-reason metadata transitions after base mutation and refresh
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `173 passed, 0 failed`
- Landed the third `B6.3` Deduce materialized-view slice:
  - relation declaration syntax now accepts the materialization marker:
    `[relation db materialized] rel ...`
  - the declaration marker sets the same current manual refresh policy as
    explicit `deduce/materialize!`
  - focused regression coverage now pins:
    - schema registration for declared materialized relations
    - rejection of unknown relation-wide attributes
    - declaration-syntax alignment with relation-scoped refresh metadata
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `178 passed, 0 failed`
- Landed the next `B6.3b` Deduce declaration-policy slice:
  - relation declaration syntax now also accepts the explicit current manual
    policy spelling:
    `[relation db materialized manual] rel ...`
  - the parser now rejects unknown declaration-time materialized refresh
    policies deterministically instead of folding them into the generic
    unknown-attribute path
  - focused regression coverage now pins:
    - explicit manual declaration syntax
    - rejection of unknown materialized refresh policies
- Landed the fourth `B6.3` Deduce materialized-view slice:
  - relation-scoped `deduce/refresh! materialized-relation` now executes the
    target relation’s dependency closure in tracked mode instead of routing
    through full DB analyze
  - targeted refresh now clears dirty state only for the predicates it
    actually recomputed and leaves unrelated stale materialized views alone
  - if the DB has already escalated to `full-recompute-required`, relation-
    scoped refresh still falls back to the DB-wide path
  - focused regression coverage now pins:
    - refreshing one materialized relation leaves a sibling materialized
      relation stale
    - `stale-materialized-view-count` remains nonzero after the targeted
      refresh when unrelated stale views still exist
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `178 passed, 0 failed`
- Landed the fifth `B6.3` Deduce materialized-view slice:
  - declaration-based materialization is now lifecycle-gated rather than
    pretending every declared materialized relation is immediately refreshable
  - `deduce/schema` / `deduce/stats` now expose
    `materialized-derived-ready`, and `deduce/analyze` now exposes
    `ready-materialized-view-count` plus
    `unready-materialized-view-count`
  - relation-scoped refresh now rejects declared-but-unready materialized
    relations with `deduce/refresh-materialized-relation-not-derived`
  - DB-scoped refresh only stamps ready materialized views as refreshed and
    leaves unready declarations stale, with honest remaining stale counts
  - focused regression coverage now pins:
    - declared materialized relations reject refresh before rule install
    - the same relation becomes refreshable after its first derived rule head
    - DB-scoped refresh can refresh one ready view while leaving an unready
      declared view stale
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `179 passed, 0 failed`
- Landed the sixth `B6.3` Deduce materialized-view slice:
  - materialized intent now persists across reopen / `open-named` for
    file-backed DBs through a narrow relation-metadata catalog
  - reopened materialized relations come back conservatively as materialized
    but unready/stale until their rules are reinstalled and refreshed, so the
    runtime no longer pretends durable derived state exists when only
    materialized intent persisted
  - dropping a declared materialized relation now clears that persisted
    materialized-intent metadata instead of leaving stale declaration state
    behind across reopen
  - materialized stale reasons now distinguish `never-refreshed` from
    mutation-driven reasons, so declared or reopened materialized views with
    no successful refresh no longer report `materialized-stale == true` with
    a blank stale reason
  - focused regression coverage now pins:
    - restart/open-named persistence of materialized intent without durable
      rule state
    - clearing persisted materialized intent on drop
    - `materialized-last-stale-reason == 'never-refreshed` before first
      refresh on declared or reopened materialized views
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `181 passed, 0 failed`
- Landed the seventh `B6.3` Deduce materialized-view slice:
  - installing a new rule now invalidates already refreshed materialized
    views with the explicit stale reason `rule-set-change`
  - rule-install invalidation also forces the existing
    `full-recompute-required` fallback, keeping the contract honest instead of
    pretending the targeted invalidation path already models rule-graph
    changes soundly
  - focused regression coverage now pins:
    - refreshed materialized views become stale after `deduce/rule!`
    - `materialized-last-stale-reason == 'rule-set-change`
    - `deduce/stats` reports `full-recompute-required == true` after the
      rule install
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `181 passed, 0 failed`
- Landed the eighth `B6.3` Deduce materialized-view slice:
  - `deduce/refresh!` payloads now report the actual refresh path:
    relation-targeted, DB-wide, or relation-scoped fallback to DB-wide under
    `full-recompute-required`
  - the admin surface now exposes that through:
    - `refresh-scope`
    - `refresh-execution-path`
    - `refresh-fallback-reason`
  - focused regression coverage now pins DB refresh payload truthfulness,
    targeted relation refresh payload truthfulness, and relation fallback
    payload truthfulness after `rule-set-change`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `182 passed, 0 failed`
- Landed the ninth `B6.3` Deduce materialized-view slice:
  - `deduce/refresh!` now reports the actual refreshed and still-stale
    materialized relation identities through:
    - `refreshed-materialized-relations`
    - `remaining-stale-materialized-relations`
    - `requested-refresh-relation` on relation-scoped calls
  - relation-scoped fallback to DB-wide refresh no longer under-reports
    refresh impact when more than one ready materialized view is refreshed
  - focused regression coverage now pins DB refresh, targeted relation
    refresh, and rule-set-change fallback payload truthfulness at the
    relation-identity level
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `184 passed, 0 failed`
- Landed the first `B6.4` Deduce goal-directed planning slice:
  - `deduce/explain` now surfaces:
    - `goal-directed-component-id`
    - `goal-directed-eligible`
    - `goal-directed-shape`
    - `goal-directed-blockers`
  - the current eligible shape is intentionally narrow:
    positive recursive closure with no aggregates and no negated body atoms
  - non-eligible recursive shapes now report explicit blockers such as
    `aggregates-present` and `negated-body-atom`
  - this slice is explain-only: there is still no magic-set rewrite, no
    goal-directed execution path, and no runtime/query/analyze behavior change
  - focused regression coverage now pins:
    - positive recursive closure eligibility
    - blocker reporting for recursive aggregate/negated shapes
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `175 passed, 0 failed`
- Landed the second `B6.4` Deduce goal-directed planning slice:
  - `deduce/analyze` now reports DB-level planner counts for recursive
    goal-directed shapes:
    - `goal-directed-recursive-component-count`
    - `goal-directed-eligible-component-count`
    - `goal-directed-blocked-component-count`
    - `goal-directed-aggregate-blocked-component-count`
    - `goal-directed-negated-blocked-component-count`
  - this remains diagnostic-only planner metadata with no execution rewrite
  - focused regression coverage now pins positive recursive closure counts
    and aggregate/negated blocker counts
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `182 passed, 0 failed`
- Landed the third `B6.4` Deduce goal-directed planning slice:
  - `deduce/analyze` now exposes `goal-directed-components`, a recursive
    component summary list carrying component id, stratum, rule count,
    eligibility, shape, blockers, and aggregate/negation presence flags
  - this remains planner/admin metadata only with no execution rewrite
  - focused regression coverage now pins positive recursive closure summaries
    and aggregate/negated blocked summaries
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `184 passed, 0 failed`
- Landed the tenth `B6.3` Deduce materialized-view slice:
  - the invalidation frontier is now explicit instead of only count-shaped:
    - `deduce/analyze` exposes `incremental-dirty-predicates`
    - `deduce/stats` exposes `dirty-predicates` and `dirty-self`
    - `deduce/refresh!` exposes:
      - `cleared-dirty-predicates`
      - `remaining-dirty-predicates`
  - targeted relation refresh now reports exactly which dirty predicates it
    cleared while leaving unrelated dirty predicates behind
  - DB-wide refresh now reports the pre-refresh dirty frontier it cleared and
    the remaining frontier after success
  - focused regression coverage now pins:
    - DB refresh dirty-frontier visibility
    - targeted refresh dirty-frontier visibility
    - rule-set-change fallback dirty-frontier truthfulness
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `187 passed, 0 failed`
- Landed the fourth `B6.4` Deduce goal-directed planning slice:
  - `deduce/analyze` now supports selector-scoped execution for the currently
    eligible recursive shape through:
    - `(deduce/analyze db rule-index)`
    - `(deduce/analyze db rule-index 'naive)`
    - `(deduce/analyze db rule-index 'semi-naive)`
  - eligible selectors are intentionally narrow:
    positive recursive closure with no aggregates and no negated body atoms
  - selected execution now evaluates only the chosen component's dependency
    closure and reports:
    - `goal-directed-execution-path = 'selected-component-closure`
    - `goal-directed-selector-rule-index`
    - `goal-directed-selected-component-id`
  - aggregate-bearing and negated recursive selectors reject explicitly with
    `deduce/analyze-goal-directed-selector-not-eligible` and blocker
    payloads instead of silently falling back to full DB execution
  - focused regression coverage now pins:
    - selected-component closure execution that leaves unrelated derived
      components untouched
    - aggregate recursive selector rejection
    - negated recursive selector rejection
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `187 passed, 0 failed`
- Landed the fifth `B6.4` Deduce goal-directed planning slice:
  - `deduce/explain` now mirrors selector-scoped analyze execution for the
    currently eligible recursive shape by exposing:
    - `goal-directed-execution-path`
    - `goal-directed-selected-components`
    - `goal-directed-selected-predicates`
  - for eligible recursive selectors this closure payload matches the same
    dependency closure used by `(deduce/analyze db rule-index [engine])`
  - aggregate-bearing and negated recursive selectors continue to expose
    blockers and now leave the selected-closure payload fields `nil`
  - focused regression coverage now pins:
    - positive recursive selector closure visibility in `deduce/explain`
    - aggregate/negated recursive selector closure omission in
      `deduce/explain`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `187 passed, 0 failed`
