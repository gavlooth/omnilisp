    - added bounded disjunctive head-demand collection for filters shaped as
      `or` trees whose branches are individually demand-safe under the
      already shipped query subset
    - added disjunctive selector/plain ephemeral execution helpers that run
      multiple projected head demands inside the same abortable write txn and
      then reapply the original full filter over the union of rows in that
      txn
    - kept the shipped boundary narrow: mixed unsupported disjuncts and
      broader symbolic disjunctions still fall back
  - Added focused regressions in
    `src/lisp/tests_deduce_query_groups.c3` for:
    - selector-scoped disjunctive union execution over two demand-safe
      branches
    - plain `deduce/query` disjunctive union execution over two demand-safe
      branches
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    so the shipped slice closes as `B6.4e1` and the residual wider
    disjunctive / rewrite space stays open under `B6.4e2`
- Validation run
  - `c3c build`
  - host-side selector disjunctive-union smoke: returned `true`
  - host-side plain `deduce/query` disjunctive-union smoke: returned `true`

- Objectives attempted
  - Close the next live `TODO.md` item, `B6.4d`, by widening the shipped
    `deduce/query` demand extractor from preserved-row wrapper rewrites to
    the next bounded captured-call family: comparator wrappers over
    row-derived scalar arguments.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - added a bounded captured-scalar-call equality extractor
    - widened `deduce/query` demand extraction so comparator wrappers like
      `(want target (pickdst row))` stay on the ephemeral demand path when
      exactly one argument reduces to a row-derived scalar and the remaining
      arguments are closed literals
    - preserved that same bounded subset across short forwarding chains
      without claiming generic captured-call rewrite or symbolic solving
  - Added focused regression coverage in
    `src/lisp/tests_deduce_query_groups.c3` for:
    - selector-scoped captured comparator wrappers over row-derived scalar
      arguments
    - plain `deduce/query` short forwarding chains of those comparator
      wrappers
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    to close `B6.4d` at that bounded shipped boundary and leave the broader
    query-time rewrite/magic-set work under `B6.4e`
- Validation run
  - `c3c build`
  - host-side selector smoke for captured comparator wrappers over a
    row-derived scalar argument: returned `true`
  - host-side plain `deduce/query` forwarding-chain smoke for that same
    scalar-comparator subset: returned `true`

- Objectives attempted
  - Close the first live `TODO.md` item, `B6.3f1b`, by persisting the
    Deduce rule/dependency catalog truth that admin surfaces can expose after
    reopen without pretending executable rule state is already durable.
- Code/config changes made
  - Added a compact persisted rule-catalog summary in
    `src/lisp/deduce_db_handles.c3`:
    - introduced a dedicated `__deduce_rule_catalog` LMDB DBI
    - rewrote that DBI from live rule signatures on each `deduce/rule!`
    - restored the compact catalog on `deduce 'open`
    - kept the restored data separate from live executable rule signatures so
      reopen truth does not overclaim runtime execution support
  - Updated `src/lisp/deduce_rule_eval_prims.c3` so `deduce/analyze` now uses
    the persisted catalog summary for `rule-count` and
    `incremental-dependency-edges` when live rules are absent.
  - Updated restart regressions in
    `src/lisp/tests_deduce_durability_groups.c3` so reopened analyze payloads
    now expect the persisted rule/dependency summary instead of zeroed values.
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    to reflect the new shipped boundary:
    - reopened admin truth keeps the compact rule/dependency summary
    - executable rule state and durable ready/fresh semantics are still
      deferred to the next item
- Validation run
  - `c3c build`
  - host-side two-process file-backed smoke:
    reopened `deduce/analyze` retained `rule-count == 1` and
    `incremental-dependency-edges == 1` while the materialized relation stayed
    conservatively unready
  - host-side two-process stale `on-read` reopen smoke:
    reads still failed truthfully with `deduce/on-read-refresh-unavailable`
    while reopened `deduce/analyze` retained the persisted rule/dependency
    summary

- Objectives attempted
  - Close the next live `TODO.md` item, `B6.3f2`, by making reopened
    materialized relations truthfully ready/fresh again for the current
    supported persisted rule-signature surface instead of keeping restart
    behavior permanently summary-only.
- Code/config changes made
  - Fixed the reopened executable-rule restore path in
    `src/lisp/deduce_db_handles.c3`:
    - stopped trusting persisted raw `SymbolId` values across processes
    - rebuilt restored rule head/body predicate identities from the persisted
      rule catalog instead
    - registered inferred predicate schemas for restored rule predicates so
      fixpoint/refresh execution has arity truth after reopen even before
      every dependency relation is explicitly reopened
  - Refactored `src/lisp/deduce_db_handles_register.c3` so relation-schema
    registration is truthful for reopen flows:
    - same-shape reopen is now idempotent instead of being treated as a hard
      conflict
    - inferred placeholder schemas can now be upgraded by a later explicit
      `deduce/open-named ...`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` to close the
    item and record the actual shipped boundary:
    - supported persisted executable signatures now restore live
      `deduce/refresh!` / `on-read` maintenance after reopen
    - unsupported persisted signature shapes still fall back to summary-only
      admin truth
- Validation run
  - `c3c build`
  - host-side two-process file-backed smoke:
    reopened manual materialized relation remained
    `materialized-derived-ready == true`, kept `rule-count == 1` and
    `incremental-dependency-edges == 1`, and returned count `1`
  - host-side two-process file-backed smoke:
    reopened stale `on-read` materialized relation auto-refreshed on read,
    advanced `materialized-refresh-count` from `1` to `2`, cleared stale
    state, and returned count `2`
  - host-side two-process file-backed smoke:
    reopened never-refreshed manual materialized relation stayed ready,
    reported `never-refreshed`, and successfully refreshed through the
    targeted relation path

- Objectives attempted
  - Reshape the remaining Deduce `B6.4` backlog so the shipped baseline and
    the true residual work are tracked as separate items rather than one long
    umbrella.
- Code/config changes made
  - Updated `docs/plans/deduce-actionable-backlog-2026-03-20.md`:
    - chose the first actual non-manual declaration-policy target:
      `on-read` is now the explicit first widening after the current
      manual-only surface
    - closed `B6.3e1` with an explicit surface decision:
      declaration-time materialization stays frozen at `manual` until a
      concrete non-manual maintenance contract exists
    - expanded that decision into an approved naming table for all visible
      non-manual trigger families:
      - `on-read`
      - `on-base-commit`
      - `on-open`
      - `scheduled`
    - retitled `B6.3e2` so it now refers directly to implementing the first
      approved non-manual policy after a real maintenance contract exists
    - closed `B6.3f1a` at the honest shipped reopen contract:
      installed rules and dependency catalogs do not survive file-backed
      reopen today, even though materialized snapshot rows and lifecycle
      metadata do
    - split `B6.3f1` at the real semantic boundary:
      - `B6.3f1a` canonical reopen contract for persisted rule/dependency
        catalogs
      - `B6.3f1b` runtime/storage rollout only after that contract is chosen
    - split `B6.3f` at the real persistence boundary:
      - `B6.3f1` persisted rule/dependency catalogs beyond snapshot durability
      - `B6.3f2` durable freshness/ready semantics only after that catalog
        persistence boundary exists
    - split `B6.3e` at the real semantic boundary:
      - `B6.3e1` canonical declaration-policy naming/contract choice beyond
        `manual`
      - `B6.3e2` runtime declaration-policy widening only after that naming
        choice exists
    - closed `B6.3c` at the honest shipped boundary:
      declaration-time materialization policy is explicitly manual-only today
    - closed `B6.3d` at the honest shipped persistence boundary:
      file-backed materialized relations currently preserve the last stored
      snapshot rows across reopen, while schema/admin surfaces stay
      conservatively unready until rules are reinstalled
    - added follow-up item:
      - `B6.3e` broader declaration-policy widening beyond manual
      - `B6.3f` persisted rule/dependency catalogs and durable derived
        freshness semantics beyond snapshot durability
    - closed `B6.4c` as the shipped goal-directed execution baseline
    - added new residual items:
      - `B6.4d` wider captured-call rewrite for `deduce/query` demand extraction
      - `B6.4e` broader query-time magic-set / rewrite execution
      - `B6.4f` recursive-shape and safety rules
      - `B6.6` planner-backed conjunctive execution and explain truthfulness
        tightening
      - `B6.7` mutation logging and dirty-tracking contract beyond tracked
        recompute
      - `B6.8` true incremental derived-state maintenance beyond tracked
        recompute
      - `B6.9` runtime provenance / why-result surface
      - `B6.10` richer integrity classes beyond current
        key/unique/reference surface
      - `B6.11` parallel recursive evaluation beyond topology visibility
    - split each of those six new residuals again into two smaller items so
      baseline/contract work and runtime rollout work are tracked separately
    - split those twelve smaller residuals one more time into paired
      subitems, so the current spec-derived Deduce queue now consists of
      twenty-four narrow slices
    - rewrote the old `remaining open work` text into a `closed state` summary
      so shipped preserved-bound behavior no longer reads as perpetually
      partial
  - Updated `memory/CHANGELOG.md` with the backlog-shaping decision.
  - Added a focused public-surface Deduce regression in
    `src/lisp/tests_deduce_groups.c3` that pins `deduce/schema` exposing the
    same `materialized-refresh-policy == 'manual` payload for both
    `[relation db materialized]` and
    `[relation db materialized manual]`.
  - Extended `src/lisp/tests_deduce_durability_groups.c3` so the restart
    materialized refresh/stale regressions now also pin persisted snapshot
    observability through `deduce/count` after reopen.
  - Extended those restart regressions again so reopened `deduce/analyze`
    explicitly pins the current no-catalog-persistence boundary with
    `rule-count == 0` and `incremental-dependency-edges == 0`.
  - Added `docs/plans/deduce-materialized-declaration-policy-decision-2026-03-25.md`
    to record concrete declaration-policy naming candidates, rejected/deferred
    directions, and the approved canonical future names for each visible
    trigger family while the surface remains frozen at `manual`.
  - Extended that decision note again so the first future widening target is
    explicit rather than generic:
    `on-read` is the chosen first non-manual contract, with `on-base-commit`,
    `on-open`, and `scheduled` left as later families.
  - Updated `AGENTS.md` with a rule that blocked language-surface naming work
    must produce an explicit decision or a concrete candidate-options note,
    and if multiple future trigger families are visible, that note must choose
    sensible canonical names for all of them instead of stopping at “later”.
  - Strengthened that into a general naming rule:
    whenever naming is unclear, drifting, or blocking progress, agents must
    turn it into an explicit decision, a concrete options note, or a direct
    owner question instead of stopping at “needs naming”.
  - Confirmed there is still no canonical wider declaration-policy spelling
    anywhere local beyond `manual`, so no parser/runtime widening was guessed
    for `B6.3e`.
  - Landed the first real non-manual declaration-time maintenance contract:
    `[relation db materialized on-read]` now parses, persists through reopened
    relation metadata, surfaces via `deduce/schema` and `deduce/stats`, and
    refreshes stale derived materialized relations on ordinary stored-tuple
    reads through the existing relation-scoped refresh machinery.
  - Kept the current reopen truth boundary explicit in both runtime and docs:
    stale `on-read` relations now fail read-time maintenance with
    `deduce/on-read-refresh-unavailable` when rule/dependency catalogs are
    absent after reopen, while fresh reopened snapshots remain readable.
  - Updated the targeted Deduce regression surface:
    - `src/lisp/tests_deduce_groups.c3` now pins `on-read` declaration
      acceptance and policy payloads
    - `src/lisp/tests_deduce_query_groups.c3` now pins unready rejection plus
      in-process `on-read` auto-refresh after rule install and dirtying
    - `src/lisp/tests_deduce_durability_groups.c3` now pins reopened fresh and
      stale `on-read` behavior across restart
  - Consolidated every remaining unchecked backlog item into `TODO.md` and
    removed the parallel backlog tracker files from `docs/plans/`.
  - Simplified `docs/plans/README.md` so it points at `TODO.md` as the sole
    live backlog, and removed backlog-specific helper scripts that only
    existed to police the deleted tracker files.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(block ... (deduce/schema ...) ...)"` for a narrow host-side materialized declaration-policy smoke
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(block ... [relation db materialized on-read] ...)'`
  - host-side two-process file-backed smokes for reopened fresh and stale
    `on-read` materialized relations
  - `rg -n "^- \\[ \\]" docs/plans --glob '*.md'`
  - key state:
    - the first non-manual declaration-time policy is now actually shipped,
      not just named: `on-read` exists as parser/runtime/docs behavior
    - `TODO.md` is now the only live backlog
    - `docs/plans/` no longer contains unchecked `- [ ]` tracker items
- Best current checkpoint/config recommendation
  - Treat `TODO.md` as the only live queue and avoid recreating area-local
    backlog trackers under `docs/plans/`.
- Unresolved issues and next actions
  - Next work should come directly from `TODO.md`.
- Signature: Codex (GPT-5)

## 2026-03-24

- Objectives attempted
  - Identify the implemented items in `TODO.md` and move the completed backlog
    slices out of the live queue.
- Code/config changes made
  - Created `docs/plans/TODO_ARCHIVE_2026-03-24.md` as a dated archive of the
    completed TODO backlog.
  - Replaced `TODO.md` with a live-queue stub that points at the archive and
    keeps only the active backlog summary.
- Experiment commands and key metrics
  - `jj status`
  - `tail -n 80 docs/SESSION_REPORT.md`
  - `sed -n '1,260p' TODO.md`
  - `sed -n '261,520p' TODO.md`
  - `sed -n '521,760p' TODO.md`
  - key state: `TODO.md` now reports `Current actionable count: 0`
- Best current checkpoint/config recommendation
  - Keep `TODO.md` as the live queue stub and use
    `docs/plans/TODO_ARCHIVE_2026-03-24.md` for the implemented backlog items.
- Unresolved issues and next actions
  - None for this slice; the remaining work is whatever new active items are
    added later.
- Follow-up slice completed
  - Objective: audit the editor-tooling roadmap/session state against shipped
    package/docs surfaces and close stale non-test backlog slices.
  - Code/config changes made
    - Updated `docs/plans/editor-tooling-roadmap.md` to reflect shipped
      evidence already present in `docs/PROJECT_TOOLING.md`,
      `tooling/tree-sitter-omni/README.md`, `tooling/omni-lsp/README.md`, and
      `tooling/omni-nvim/README.md`.
    - Marked shipped non-test items complete for:
      - Tree-sitter packaging/bootstrap commands
      - `tooling/omni-lsp` structured-diagnostics baseline plus
        symbols/completion/hover/local-first definition
      - `tooling/omni-nvim` structured eval wiring, Tree-sitter-backed
        selection with fallback, operator eval, and transcript window controls
      - indentation-first LSP range formatting
    - Narrowed the remaining roadmap queue to unresolved Tree-sitter hardening,
      LSP integration coverage, virtual-text/result-annotation work, and CLI
      formatter follow-through.
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this planning/state audit
      slice; evidence came from targeted `sed`/`rg` inspection of existing
      code/docs.
  - Best current checkpoint/config recommendation
    - Treat the structured LSP/Neovim rebases as shipped baseline and keep the
      backlog focused on the remaining hardening/testing/formatter slices.
  - Unresolved issues and next actions
    - close additional editor-tooling items only when backed by explicit code
      or docs evidence for the shipped contract.
- Signature: Codex (GPT-5)
## 2026-03-26 leak cleanup follow-up split
- Objective: turn the ASAN leak noise from the latest validation runs into a concrete backlog item instead of leaving it as vague cleanup debt.
- Findings: one explorer concluded the LMDB-looking noise is expected lifetime behavior, while the other found a fixable teardown/ownership issue around stdlib-loaded runtime objects and method-table/signature allocations.
- Change made: added a new three-slice leak cleanup follow-up plan to `TODO.md` covering harness teardown regression, method-table/signature ownership, and closure-copy/scope teardown reclamation.
- Follow-up landed: added a focused stdlib teardown redefinition regression in `src/lisp/tests_runtime_feature_jit_groups_more.c3` that loads primitives + stdlib, defines/redefines the same typed method, and tears the interpreter down through the existing shutdown path.
- Next actions: start with the harness teardown regression in `src/lisp/tests_tests.c3`; if it reproduces, the fix boundary is likely `Interp.destroy()` or `scope_dtor_value()`.
- Signature: Codex (GPT-5)

## 2026-03-26 leak follow-up narrowed again
- Objective: separate the actionable leak work from the dead-end audit slice after additional code inspection.
- Conclusion: the closure-copy / scope-teardown slice does not appear to need a code change; `scope_dtor_closure`, `scope_dtor_env`, and `scope_dtor_value` already match the allocation model in that path.
- Narrowed backlog: reduced the leak cleanup follow-up from three slices to two by removing the closure-copy audit item and keeping only the harness teardown confirmation lane plus the method-table overwrite seam in `jit_jit_define_method_table.c3`.
- Follow-up check: parallel workers confirmed the harness-only teardown regression lane already exists in `src/lisp/tests_tests.c3`, and the method-table lane still does not have enough evidence for a local ownership fix yet.
- Best current boundary: if the remaining leak suspicion is real, it now points at `jit_eval_define_typed_callable` and `jit_eval_define`, not at closure-copy helpers.
- Signature: Codex (GPT-5)

## 2026-03-26 Flatpak libcurl warning cleanup
- Objective: eliminate the `flatpak` runtime warnings caused by `/usr/local/lib/libcurl.so.4` shadowing the system curl library.
- Findings: `ldconfig -p` showed `/usr/local/lib/libcurl.so.4` and `/usr/lib/libcurl.so.4` were both cached, with the local copy winning resolution; the local artifacts were not owned by pacman.
- Config change made: moved the stray `/usr/local/lib/libcurl*` files into `/usr/local/lib/curl-disabled-20260326/` and rebuilt the loader cache with `sudo ldconfig`.
- Verification commands and key metrics:
  - `ldconfig -p | grep libcurl` now resolves `libcurl.so.4` to `/usr/lib/libcurl.so.4`
  - `flatpak --version` runs without the previous `/usr/local/lib/libcurl.so.4: no version information available` warning
- Best current checkpoint/config recommendation: keep the `/usr/local/lib` curl copies quarantined unless a local tool explicitly needs them; otherwise leave the system on the distro `libcurl`.
- Unresolved issues and next actions: if another local build expects the quarantined copy, restore it from `/usr/local/lib/curl-disabled-20260326/` rather than reintroducing it into the loader path.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce query groups split
- Objectives attempted
  - Improve the oversized `src/lisp/tests_deduce_query_groups.c3` test module by splitting the largest test families into separate C3 files while keeping the shared helpers and coordinator runner intact.
  - Apply a small idiomatic C3 cleanup by consolidating repeated truthiness/pass-fail handling behind an overloaded helper in the demand-path suite.
- Code/config changes made
  - Reduced `src/lisp/tests_deduce_query_groups.c3` to the shared helper prelude plus the top-level coordinator.
  - Split the admin-surface suite into `src/lisp/tests_deduce_query_admin_groups.c3` for the early blocks and `src/lisp/tests_deduce_query_admin_surface_tail.c3` for the remaining blocks.
  - Split the remaining demand-path admin surface blocks into `src/lisp/tests_deduce_query_admin_surface_demand_tests.c3`, `src/lisp/tests_deduce_query_admin_surface_demand_tail.c3`, and `src/lisp/tests_deduce_query_admin_surface_demand_wrapper_tests.c3`.
  - Added `deduce_query_test_eval_true(...)` and `deduce_query_test_expect_true(...)` so the repeated `run(...)`/truthiness reporting path is expressed once and reused for the cleanest demand-path checks.
  - Trimmed `src/lisp/tests_deduce_query_admin_surface_fallback_tests.c3` down to the remaining fallback coordinator body after extracting the aggregate suites into their own files.
  - Added `src/lisp/tests_deduce_query_aggregate_groups.c3` for the grouped aggregate suite.
  - Added `src/lisp/tests_deduce_query_recursive_aggregate_groups.c3` for the recursive aggregate suite.
  - Preserved the `lisp` module namespace so the moved suites can keep using the existing shared helper functions without API churn.
- Experiment commands and key metrics
  - `find src -name '*.c3' -type f -printf '%s %p\n' | sort -nr | head -20`
  - `rg -n '^fn ' src/lisp/tests_deduce_query_groups.c3`
  - `wc -l src/lisp/tests_deduce_query_groups.c3 src/lisp/tests_deduce_query_admin_groups.c3 src/lisp/tests_deduce_query_aggregate_groups.c3 src/lisp/tests_deduce_query_recursive_aggregate_groups.c3`
  - `wc -l src/lisp/tests_deduce_query_admin_groups.c3 src/lisp/tests_deduce_query_admin_surface_tail.c3`
  - `wc -l src/lisp/tests_deduce_query_admin_surface_tail.c3 src/lisp/tests_deduce_query_admin_surface_demand_tests.c3`
  - `wc -l src/lisp/tests_deduce_query_admin_surface_fallback_tests.c3`
  - `wc -l src/lisp/tests_deduce_query_admin_surface_demand_tests.c3 src/lisp/tests_deduce_query_admin_surface_demand_tail.c3 src/lisp/tests_deduce_query_admin_surface_fallback_tests.c3`
  - `wc -l src/lisp/tests_deduce_query_admin_surface_demand_tail.c3 src/lisp/tests_deduce_query_admin_surface_demand_wrapper_tests.c3`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - key file-size result after the split:
    - `src/lisp/tests_deduce_query_groups.c3`: `443` lines
    - `src/lisp/tests_deduce_query_admin_groups.c3`: `1998` lines
    - `src/lisp/tests_deduce_query_admin_surface_tail.c3`: `2598` lines
    - `src/lisp/tests_deduce_query_admin_surface_demand_tests.c3`: `1907` lines
    - `src/lisp/tests_deduce_query_admin_surface_demand_tail.c3`: `1062` lines
    - `src/lisp/tests_deduce_query_admin_surface_demand_wrapper_tests.c3`: `566` lines
    - `src/lisp/tests_deduce_query_admin_surface_fallback_tests.c3`: `1060` lines
    - `src/lisp/tests_deduce_query_aggregate_groups.c3`: `205` lines
    - `src/lisp/tests_deduce_query_recursive_aggregate_groups.c3`: `504` lines
  - build result: `Program linked to executable 'build/main'.`
  - runtime result: `build/main` launched the Lisp REPL cleanly and exited without error when not driven by a test harness.
- Best current checkpoint/config recommendation
  - Keep the shared helper prelude in `src/lisp/tests_deduce_query_groups.c3` as the coordinator entrypoint, and treat the split admin early/tail/demand/demand-tail/wrapper/fallback files plus the aggregate siblings as the canonical homes for their respective test families.
- Unresolved issues and next actions
  - No unresolved code issues remain from this split pass.
  - If a future review wants even smaller review slices, the wrapper file is the next likely split candidate.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce schema query execution split
- Objectives attempted
  - Split `src/lisp/deduce_schema_query.c3` first, before touching any of the newer test splits, by peeling off the execution-heavy query path into its own file.
- Code/config changes made
  - Reduced `src/lisp/deduce_schema_query.c3` to the query-analysis and head-demand helper half.
  - Added `src/lisp/deduce_schema_query_execution.c3` for `deduce_query_execute_goal_directed_selector_ephemeral(...)`, `deduce_query_execute_goal_directed_plain_ephemeral(...)`, the disjunction variants, and `prim_deduce_query(...)`.
  - Kept the same `lisp` module namespace and `std::core::mem` import so the moved code can keep calling the existing internal helpers without surface churn.
- Experiment commands and key metrics
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '2040,2165p'`
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '2610,2745p'`
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '2860,3145p'`
  - `wc -l src/lisp/deduce_schema_query.c3 src/lisp/deduce_schema_query_execution.c3 src/lisp/deduce_schema_query_metadata_refresh.c3`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - key file-size result after the split:
    - `src/lisp/deduce_schema_query.c3`: `2720` lines
    - `src/lisp/deduce_schema_query_execution.c3`: `1001` lines
    - `src/lisp/deduce_schema_query_metadata_refresh.c3`: `1469` lines
  - build result: `Program linked to executable 'build/main'.`
  - runtime result: `build/main` launched the Lisp REPL cleanly and exited without error when not driven by a test harness.
- Best current checkpoint/config recommendation
  - Keep `src/lisp/deduce_schema_query.c3` as the analysis/head-demand helper half, and treat `src/lisp/deduce_schema_query_execution.c3` as the canonical home for query execution and `prim_deduce_query(...)`.
- Unresolved issues and next actions
  - No unresolved code issues remain from this split pass.
  - If more reduction is needed later, the execution file is the next likely split candidate.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce schema query follow-up split
- Objectives attempted
  - Split `src/lisp/deduce_schema_query.c3` and `src/lisp/deduce_schema_query_metadata_refresh.c3` again, this time by peeling off the relation/open cluster and the refresh cluster respectively.
- Code/config changes made
  - Reduced `src/lisp/deduce_schema_query.c3` to the relation-definition/open setup cluster.
  - Added `src/lisp/deduce_schema_query_analysis.c3` for the query-expression analysis and head-demand helpers.
  - Reduced `src/lisp/deduce_schema_query_metadata_refresh.c3` to the schema/index/materialization metadata helpers.
  - Added `src/lisp/deduce_schema_query_refresh.c3` for the refresh/materialized-view invalidation path.
  - Kept the `lisp` module namespace and the shared `std::core::mem` import in the new files so the moved code can keep using the existing internal helpers.
- Experiment commands and key metrics
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '760,805p'`
  - `nl -ba src/lisp/deduce_schema_query_metadata_refresh.c3 | sed -n '820,860p'`
  - `wc -l src/lisp/deduce_schema_query.c3 src/lisp/deduce_schema_query_analysis.c3 src/lisp/deduce_schema_query_metadata_refresh.c3 src/lisp/deduce_schema_query_refresh.c3`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - key file-size result after the split:
    - `src/lisp/deduce_schema_query.c3`: `792` lines
    - `src/lisp/deduce_schema_query_analysis.c3`: `1932` lines
    - `src/lisp/deduce_schema_query_metadata_refresh.c3`: `838` lines
    - `src/lisp/deduce_schema_query_refresh.c3`: `635` lines
  - build result: `Program linked to executable 'build/main'.`
  - runtime result: `build/main` launched the Lisp REPL cleanly and exited without error when not driven by a test harness.
- Best current checkpoint/config recommendation
  - Keep `src/lisp/deduce_schema_query.c3` focused on relation/open plumbing, use `src/lisp/deduce_schema_query_analysis.c3` for query-expression and head-demand analysis, keep `src/lisp/deduce_schema_query_metadata_refresh.c3` for schema/index payload helpers, and use `src/lisp/deduce_schema_query_refresh.c3` for refresh invalidation and materialized-view rewrite logic.
- Unresolved issues and next actions
  - No unresolved code issues remain from this split pass.
  - If more reduction is needed later, `src/lisp/deduce_schema_query_analysis.c3` is the next likely split candidate.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce schema query split
- Objectives attempted
  - Split the largest remaining C3 source file, `src/lisp/deduce_schema_query.c3`, along a real API seam so the query engine stays separate from the schema/index/stats and refresh payload plumbing.
- Code/config changes made
  - Reduced `src/lisp/deduce_schema_query.c3` to the query-engine half, ending at `prim_deduce_query(...)`.
  - Added `src/lisp/deduce_schema_query_metadata_refresh.c3` for the schema/index payload helpers, the schema/index prims, and the refresh/materialization path.
  - Kept the `lisp` module namespace and the existing `std::core::mem` import so the moved code can continue reusing the same internal helpers without API churn.
- Experiment commands and key metrics
  - `find src -name '*.c3' -type f -printf '%s %p\n' | sort -nr | head -20`
  - `rg -n "^fn |^struct |^macro |^enum |^const" src/lisp/deduce_schema_query.c3`
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '3488,3525p'`
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '3708,3778p'`
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '4090,4185p'`
  - `wc -l src/lisp/deduce_schema_query.c3 src/lisp/deduce_schema_query_metadata_refresh.c3`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - key file-size result after the split:
    - `src/lisp/deduce_schema_query.c3`: `3717` lines
    - `src/lisp/deduce_schema_query_metadata_refresh.c3`: `1469` lines
  - build result: `Program linked to executable 'build/main'.`
  - runtime result: `build/main` launched the Lisp REPL cleanly and exited without error when not driven by a test harness.
- Best current checkpoint/config recommendation
  - Keep `src/lisp/deduce_schema_query.c3` focused on query construction/execution, and treat `src/lisp/deduce_schema_query_metadata_refresh.c3` as the canonical home for schema/index payload generation and refresh/materialization plumbing.
- Unresolved issues and next actions
  - No unresolved code issues remain from this split pass.
  - If more reduction is needed later, the new metadata/refresh file is the next likely split candidate.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce schema query analysis split
- Objectives attempted
  - Split the current hotspot file, `src/lisp/deduce_schema_query_analysis.c3`, at the next stable semantic boundary so the query-analysis helpers stop living with the head-demand construction and recursive-relaxation logic.
- Code/config changes made
  - Reduced `src/lisp/deduce_schema_query_analysis.c3` to the expression/literal extraction and equality-collection helpers.
  - Added `src/lisp/deduce_schema_query_head_demand.c3` for the head-demand builders, disjunctive demand handling, union-position helpers, recursive relaxation checks, reversed-list helper, and transaction scan helper.
  - Kept both files in the `lisp` module with the shared `std::core::mem` import so the moved functions continue to resolve the same internal helpers without surface churn.
- Experiment commands and key metrics
  - `rg -n "^fn |^struct |^enum |^const" src/lisp/deduce_schema_query_analysis.c3`
  - `sed -n '1,120p' src/lisp/deduce_schema_query_analysis.c3`
  - `sed -n '1220,1935p' src/lisp/deduce_schema_query_analysis.c3`
  - `wc -l src/lisp/deduce_schema_query_analysis.c3 src/lisp/deduce_schema_query_head_demand.c3 src/lisp/deduce_schema_query_metadata_refresh.c3 src/lisp/deduce_schema_query_refresh.c3`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - key file-size result after the split:
    - `src/lisp/deduce_schema_query_analysis.c3`: `1279` lines
    - `src/lisp/deduce_schema_query_head_demand.c3`: `656` lines
    - `src/lisp/deduce_schema_query_metadata_refresh.c3`: `838` lines
    - `src/lisp/deduce_schema_query_refresh.c3`: `635` lines
  - build result: `Program linked to executable 'build/main'.`
  - runtime result: `build/main` launched the Lisp REPL cleanly and exited without error when not driven by a test harness.
- Best current checkpoint/config recommendation
  - Keep `src/lisp/deduce_schema_query_analysis.c3` as the literal/equality-analysis helper file, and treat `src/lisp/deduce_schema_query_head_demand.c3` as the canonical home for head-demand construction and scan-path selection logic.
- Unresolved issues and next actions
  - No unresolved code issues remain from this split pass.
  - If more reduction is needed later, `src/lisp/deduce_schema_query_head_demand.c3` is the next likely split candidate.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce schema query equality split
- Objectives attempted
  - Split `src/lisp/deduce_schema_query_analysis.c3` one level deeper so the equality-mining logic stops living with the general literal and row-column extraction helpers.
- Code/config changes made
  - Reduced `src/lisp/deduce_schema_query_analysis.c3` to the expression/literal extraction, row-column reference extraction, and preserved-row unwrap helpers.
  - Added `src/lisp/deduce_schema_query_filter_equalities.c3` for the scalar equality collector, captured-scalar-call handling, and the general equality mining pipeline.
  - Kept both files in the `lisp` module so the moved helpers can continue calling the same internal expression-analysis routines without any surface/API churn.
- Experiment commands and key metrics
  - `rg -n "^fn |^struct |^enum |^const" src/lisp/deduce_schema_query_analysis.c3 src/lisp/deduce_schema_query_filter_equalities.c3`
  - `sed -n '940,1285p' src/lisp/deduce_schema_query_analysis.c3`
  - `tail -n 20 src/lisp/deduce_schema_query_analysis.c3`
  - `wc -l src/lisp/deduce_schema_query_analysis.c3 src/lisp/deduce_schema_query_filter_equalities.c3`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - key file-size result after the split:
    - `src/lisp/deduce_schema_query_analysis.c3`: `694` lines
    - `src/lisp/deduce_schema_query_filter_equalities.c3`: `491` lines
  - build result: `Program linked to executable 'build/main'.`
  - runtime result: `build/main` launched the Lisp REPL cleanly and exited without error when not driven by a test harness.
- Best current checkpoint/config recommendation
  - Keep `src/lisp/deduce_schema_query_analysis.c3` focused on literal/row extraction and treat `src/lisp/deduce_schema_query_filter_equalities.c3` as the canonical equality-mining layer.
- Unresolved issues and next actions
  - No unresolved code issues remain from this split pass.
  - If more reduction is needed later, the equality file is the next likely split candidate.
- Signature: Codex (GPT-5)

## 2026-03-27 FTXUI TUI wrapper implementation
- Objectives attempted
  - Bring the FTXUI TUI integration to a real buildable state using a vendored source tree and a C ABI shim.
  - Keep the Omni-facing contract stable enough to cover the practical FTXUI surface without exposing C++ ABI details.
- Code/config changes made
  - Updated `scripts/build_omni_chelpers.sh` to compile the vendored FTXUI translation units from `third_party/ftxui/src/ftxui` alongside `csrc/ftxui_shim.cpp`.
  - Extended `scripts/build_omni_chelpers.sh` to emit a dedicated `build/libomni_ftxui.a` archive so the plain project build can link the shim/backend without duplicating the existing helper C objects.
  - Updated `scripts/build_fast_dev.sh` so the helper archive rebuilds when vendored FTXUI sources or the helper build script change.
  - Removed the temporary system FTXUI link path from `scripts/run_e2e.sh`.
  - Reworked `project.json` so the direct project build links the dedicated vendored FTXUI archive instead of relying on host-installed `ftxui-*` libraries.
  - Adjusted `csrc/ftxui_shim.cpp` for FTXUI 6.1.9 API differences, including screen ownership, `FitComponent()` usage, `gridbox` shape handling, and generic support for spinner/canvas/table elements plus window/modal/collapsible/hoverable/resizable-split components.
  - Added the next DOM slice in the shim: `graph` via a C callback ABI, plus hyperlink and color-based selection decorators (`selectionStyleReset`, `selectionColor`, `selectionBackgroundColor`, `selectionForegroundColor`).
  - Added a static-series graph convenience helper to the shim/FFI boundary so the high-level `ui.graph(series)` surface can lower directly without exposing the raw callback-shaped graph ABI through the public `ui` module.
  - Added shim helpers `omni_ftxui_component_from_element` and `omni_ftxui_component_wrap_quit_keys`, and fixed component keep-alive propagation so nested widget state survives parent composition through the C ABI.
  - Bumped the FTXUI shim ABI version to `3` and extended the versioned option payloads in `csrc/ftxui_shim.h` to cover graph callbacks and hyperlink decorator arguments.
  - Replaced the stale speculative declaration surface in `src/lisp/ftxui_ffi.c3` with a direct mirror of `csrc/ftxui_shim.h`, then kept it aligned with the ABI v3 additions.
  - Added a dedicated library example subtree at `examples/libraries/ftxui/` and documented that FTXUI demos should live there instead of under `examples/finwatch/`.
  - Recorded and then validated the public wrapper naming decision: builders/helpers stay `ui.*`, real module-owned effects use `ui.open`/`ui.render`, and the runtime now resolves those qualified effect tags correctly.
  - Split the public `ui` scaffold into a real facade plus internal helper modules: `examples/libraries/ftxui/ui.omni` now re-exports from `ui_nodes.omni` and `ui_effects.omni` while keeping the canonical public `ui.*` surface unchanged.
  - Added focused compiler regressions for module-qualified effect tags so `signal ui.open` and `handle (ui.open x ...)` are covered in both direct-lowering and serializer-roundtrip compiler paths.
  - Added the first real Omni-facing interpreter slice in `examples/libraries/ftxui/ui.omni`: `ui.run` now lowers `text`, `paragraph`, `graph`, `button`, `input`, `checkbox`, `menu`, `hbox`, `vbox`, and `window` into the live FTXUI backend through a new internal `__ui-ftxui-run` primitive.
  - Updated `examples/libraries/ftxui/demo.omni` so the first live FTXUI-backed demo now exercises `ui.graph` as well as the existing widget/container subset, while keeping built-in `q` / `Esc` quit behavior.
  - Split the next planning lane explicitly: raw backend work remains in `docs/plans/ftxui-c-abi-shim.md`, while high-level library/facade work now has its own plan in `docs/plans/ui-library-facade-plan-2026-03-27.md` and its own focused section in `TODO.md`.
- Experiment commands and key metrics
  - `bash -n scripts/build_omni_chelpers.sh && bash -n scripts/build_fast_dev.sh && bash -n scripts/run_e2e.sh`
  - `jq . project.json`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `bash scripts/build_fast_dev.sh --profile`
  - `rg -n "NOT_SUPPORTED|not_supported" csrc/ftxui_shim.cpp csrc/ftxui_shim.h`
  - `OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_value_smoke.omni`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_effect_smoke.omni`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/smoke.omni`
  - `printf q | LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/demo.omni`
  - `OMNI_IN_VALIDATION_CONTAINER=1 OMNI_E2E_COMPILE_ONLY=1 bash scripts/run_e2e.sh`
  - Key results:
    - helper archive build completed successfully against the vendored FTXUI tree,
    - the direct build now links by way of `build/libomni_ftxui.a`, keeping the FTXUI C++ objects separate from the existing helper archive,
    - `c3c build` linked `build/main` successfully,
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main` started the REPL and exited cleanly on EOF,
    - the compiler slice passed all new module-qualified effect regressions; the only compiler-slice failure remained the unrelated pre-existing `Compiler: Dictionary` failure,
    - `module_value_smoke.omni`, `module_effect_smoke.omni`, and `smoke.omni` all returned `true`,
    - the first live `ui.run` demo exited cleanly when fed `q` over piped input even after adding `ui.graph` to the live path,
    - the facade split preserved the public `ui.*` contract: both the value/effect smoke paths and the live demo kept working after moving node/effect definitions into helper modules,
    - `rg -n "NOT_SUPPORTED|not_supported" ...` now finds only the status enum/name and no explicit unsupported call branches in the shim,
    - the ABI mirror now includes `graph` callbacks and hyperlink/selection decorator coverage without breaking the direct build path,
    - the repo now has a dedicated `examples/libraries/ftxui/` path reserved for FTXUI-focused examples,
    - the naming decision is now frozen in `docs/plans/ui-effects-module-naming-decision-2026-03-27.md`,
    - the e2e compile-only gate still stops on a missing host `yyjson.h` header mount in this environment.
- Best current checkpoint/config recommendation
  - Keep the vendored FTXUI source model: `project.json` + `build/libomni_ftxui.a` for the direct build, `build/libomni_chelpers.a` for AOT/e2e linkage, and `csrc/ftxui_shim.cpp` as the narrow ABI bridge.
- Unresolved issues and next actions
  - The e2e compile-only path is environment-limited by missing host headers, not by the FTXUI integration itself.
  - The first `ui.run` slice is still intentionally partial beyond the shipped graph support: full FTXUI parity remains incomplete in advanced widget option parity, arbitrary `selectionStyle(Pixel&)` callback parity, gradient decorators, richer runtime helpers, and a richer public decorator/sizing surface for making elements like graphs less minimal by default.
  - The library split is real, but true dotted submodule imports such as `ui.nodes` / `ui.effects` are not yet available because the current import surface only accepts a bare module symbol or a file path; the shipped split therefore uses file-backed flat helper modules under the public `ui` facade.
  - The next UI work should be taken from the dedicated library-only queue in `TODO.md`, not reintroduced implicitly as raw backend parity work.
- Signature: Codex (GPT-5)

## 2026-03-27 FTXUI 100% Coverage Plan Definition
- Objectives attempted
  - Turn the current FTXUI wrapper status into a concrete 100% coverage plan instead of leaving the end-state implicit.
  - Separate backend ABI completion from public `ui` facade completion so the remaining work can be closed family by family.
- Code/config changes made
  - Added a `100% Coverage Roadmap` section to `docs/plans/ftxui-c-abi-shim.md` defining the wrapper-completion rule, execution order, and exit criterion.
  - Added a `100% Facade Completion` section to `docs/plans/ui-library-facade-plan-2026-03-27.md` defining the public `ui` end-state, module ownership shape, and coverage sequence.
  - Tightened the `UI Library Queue` in `TODO.md` so the remaining facade work is broken into explicit slices instead of one broad umbrella.
  - Reframed the runtime slice around declarative Hiccup-style effect forms plus a dispatcher, instead of treating `ui.open` / `ui.render` / `ui.close` as the primary public runtime model.
- Code/config changes made
  - Added an initial `ui.effects` grammar sketch in `docs/plans/ui-library-facade-plan-2026-03-27.md` so effects are treated as stable tagged payload forms with normalization before backend dispatch.
  - Tightened the `Effect vocabulary` and `Runtime dispatcher` backlog items in `TODO.md` so the next slice is effect form shape plus interpretation, not imperative helper calls.
  - Expanded the grammar sketch so effect forms are now tree-shaped declarative nodes with symbol tags, attribute maps, child effect nodes, and explicit runtime dispatch semantics.
  - Formalized the effect submission boundary as `signal`, with effect trees preserving symbol tags through normalization and dispatch and failing explicitly on malformed or unknown tags.
  - Corrected the boundary model so UI effects reuse Omni's existing `signal` effect machinery underneath, with no competing UI-level `signal` keyword or syntax form.
  - Chose constructors as the primary public authoring model for `ui.effects`, with macros reserved as optional sugar over the same explicit tree data.
  - Implemented the first concrete tree-constructor slice in `examples/libraries/ftxui/ui_effects.omni` and surfaced it through `examples/libraries/ftxui/ui.omni`; `examples/libraries/ftxui/smoke.omni` now asserts the effect-tree shape alongside the existing live effect-handler path.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/smoke.omni`
  - Key results: the project linked successfully after adding the effect-tree constructors, and the smoke example exited successfully with the new tree-shape assertions in place.
- Best current checkpoint/config recommendation
  - Treat 100% wrapper coverage as two linked contracts: complete backend family coverage in the ABI shim, then complete the public `ui.*` facade and its focused examples/tests.
  - Keep explicit non-goals documented instead of leaving partial support ambiguous.
- Unresolved issues and next actions
  - The backend ABI still needs the deferred families called out in the shim plan, especially selection callbacks, gradient decorators, richer widget/runtime helpers, and fine-grained overload parity.
  - The public facade still needs the remaining `ui.layout`, `ui.style`, `ui.runtime`, `ui.ftxui`, and true dotted submodule ownership work.
  - Next implementation should follow the new coverage sequence rather than mixing backend parity and facade work in the same slice.
- Signature: Codex (GPT-5)

## 2026-03-27 UI Effect Tree Dispatcher
- Objectives attempted
  - Stop redesigning the UI effect model and land a concrete implementation slice.
  - Make the new tree-shaped `ui.effects` constructors usable through the public `ui` facade.
  - Add a small dispatcher that lowers the effect tree through Omni's existing effect machinery without colliding with the language-level `signal` form.
- Code/config changes made
  - Changed `examples/libraries/ftxui/ui_effects.omni` so `effect_node` now stores an explicit child list, and added `effect_tag`, `effect_attrs`, `effect_children`, `open_tree`, `render_tree`, `read_event_tree`, `invalidate_tree`, `close_tree`, and `post_event_tree`.
  - Extended `examples/libraries/ftxui/ui.omni` to re-export the new tree constructors and added `dispatch`, `dispatch_one`, and `dispatch_children` so the tree can be walked and lowered through the existing `open` / `render` / `read_event` / `invalidate` / `close` / `post_event` effect tags.
  - Updated `examples/libraries/ftxui/smoke.omni` to assert the effect tree shape and to exercise `ui.dispatch sample-effect-tree` through the live capture helper.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/smoke.omni`
  - Key results: the project linked successfully after the dispatcher landed, and the smoke example exited successfully with `true` after verifying both the constructor shape and the dispatched event order.
- Best current checkpoint/config recommendation
  - Keep `ui.effects` constructor-first and tree-shaped, with `ui.dispatch` as the bridge to Omni's existing effect system.
- Unresolved issues and next actions
  - The dispatcher is still a small bridge and not the full runtime extraction slice.
  - The next UI work should continue from the facade/runtime queue in `TODO.md`, not reopen the naming or grammar design.
- Signature: Codex (GPT-5)

## 2026-03-27 Deduce Crash Recovery + CRUD Example Restore
- Objectives attempted
  - Remove the `deduce` slice segfault that was aborting validation in `values_equal_with_workspace_stats`.
  - Restore the missing `examples/deduce_crud_server.omni` contract so the CRUD example tests can load again.
  - Re-run the deduce slice far enough to separate the crash fix from the remaining test failures.
- Code/config changes made
  - Added a symbol-key fast path in `src/lisp/prim_collection_hashmap.c3` so explain-payload dict updates no longer go through the generic key-equality path for symbol keys.
