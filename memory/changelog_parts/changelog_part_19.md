# Memory Changelog Index Part 19

Source: `memory/CHANGELOG.md`

    `find`, `partition`, `sort-by`, `for-each`) by aligning those bodies from
    the lambda's own opening column instead of flattening them to shallow
    block indentation
  - `tooling/omni-lsp` mirrors the same higher-order lambda-body alignment so
    editor and CLI formatting stay on one contract
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

- Landed the coroutine wrapper-lambda formatter slice:
  - conservative formatting now preserves multiline `Coroutine (lambda ...)`
    bodies on the current in-tree wrapper layout instead of flattening them
    toward the generic lambda indentation used for other calls
  - `tooling/omni-lsp` mirrors the same coroutine wrapper-lambda behavior so
    editor and CLI formatting stay on one contract
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

- Landed the clause/data-layout formatter slice:
  - conservative formatting now preserves multiline `match` arm payload bodies
    from their clause-opening delimiter instead of indenting them like generic
    block bodies
  - inline dict entries now keep key alignment from the `{` delimiter, and
    multiline vector payload entries that start with nested forms keep the same
    delimiter-anchored style used in current in-tree data literals
  - `tooling/omni-lsp` mirrors the same clause/data-layout behavior so editor
    and CLI formatting stay on one contract
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

## 2026-03-24

- Landed the next editor-tooling Tree-sitter query-hardening slice:
  - tightened `tooling/tree-sitter-omni/queries/highlights.scm` so canonical
    macro/module forms (`export`, `syntax-match`, `template`, `insert`,
    `splice`) receive first-party captures while removed older spellings such
    as `fn` / `begin` / `letrec` remain outside canonical keyword highlighting
  - added `tooling/tree-sitter-omni/test/check_queries.sh` and focused query
    fixtures under `tooling/tree-sitter-omni/test/queries/` covering
    highlights, injections, locals, textobjects, folds, and older
    non-highlight behavior
  - added `tooling/tree-sitter-omni/queries/indents.scm` plus an indentation
    fixture so bracketed Omni forms now expose a first-party structural
    indentation surface for editor integrations
  - reshaped `docs/plans/editor-tooling-roadmap.md` so parser-only removed syntax
    rejection no longer masquerades as a Tree-sitter grammar-corpus contract
  - validation:
    - `npx tree-sitter test --rebuild`
    - `sh ./test/check_queries.sh`

- Roadmap state audit for shipped editor tooling:
  - closed the stale `tooling/omni-lsp` integration-test roadmap item because
    `tooling/omni-lsp/tests/smoke_test.py` already drives initialize/open/change,
    hover, completion, diagnostics, and related baseline requests through a
    real stdio server subprocess
  - closed the stale `tooling/omni-nvim` result-annotation roadmap item because
    the plugin already supports optional single-form virtual-text eval
    annotations and documents the setup in `tooling/omni-nvim/README.md` and
    `tooling/omni-nvim/doc/omni.nvim.txt`

- Landed the next editor-tooling Tree-sitter hardening slice:
  - added a maintained accepted-syntax corpus at
    `tooling/tree-sitter-omni/test/corpus/coverage.txt`
  - the corpus now pins grammar coverage for:
    - type annotations
    - macro/template forms
    - regex literals
    - path plus postfix index chaining
    - effect handlers and continuations
    - module/import/export forms
    - destructuring-heavy `let` / `match` inputs
  - added package scripts for:
    - `tree-sitter parse --rebuild examples/sample.omni`
    - `tree-sitter test --rebuild`
    - `tree-sitter test --rebuild --update`
  - tightened the grammar token surface so `#r"..."` now produces
    `regex_literal` nodes instead of tokenizing as a plain `#r` symbol plus
    string literal
  - updated `docs/plans/editor-tooling-roadmap.md` to close the accepted-syntax
    grammar-coverage slice while leaving rejected corpus coverage open
  - validation:
    - `npx tree-sitter generate`
    - `npx tree-sitter test --rebuild --update`
    - `npx tree-sitter test --rebuild`

- Hardened `omni --check --json` and closed the remaining structured-check
  regression coverage slice:
  - `src/entry_check_mode.c3` now treats compiler output containing explicit
    unsupported-expression markers as a lowering failure instead of reporting a
    false clean check result
  - added `tooling/omni-lsp/tests/check_json_smoke.py` to pin:
    - parser syntax diagnostics
    - lowering diagnostics for unsupported lowered forms
    - malformed `module` / `import` diagnostics
    - deterministic top-level payload and diagnostic schema fields
  - updated `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, and
    `docs/plans/editor-tooling-roadmap.md` to match the shipped check contract
    and completed regression state
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/tests/check_json_smoke.py`
    - `c3c build` blocked by existing file-permission issue:
      `src/lisp/tests_deduce_query_groups.c3` was unreadable to the build user
    - `python3 tooling/omni-lsp/tests/check_json_smoke.py` not run because the
      rebuilt `build/main` binary could not be produced under the current
      workspace permissions

- Landed the next `runtime-modularization` slice for `eval` env-copy internals:
  - split all env-copy mechanics and helper routines out of
    `src/lisp/eval_env_copy.c3` into
    `src/lisp/eval_env_copy_helpers.c3`
  - kept behavior unchanged for closure/iterator payload handling, closure-env
    rewriting, and promotion-context orchestration across parent-chain rewrites
  - updated `docs/plans/largest-runtime-files-pass-2026-03-19.md`:
    - added landed batch entry for `eval_env_copy` split
    - removed `eval_env_copy` from the top of the next queue
  - validation: not run in this slice (build/validation to be run at the lane boundary)

- Landed the next `runtime-modularization` slice for `deduce` rule validation:
  - factored rule-install validation helpers out of
    `src/lisp/deduce_rule_eval.c3` into
    `src/lisp/deduce_rule_eval_validation.c3`
  - kept existing behavior unchanged; the orchestration path still performs, in
    order:
    - arity checks
    - head-grounding safety checks
    - negation-safety checks
    - aggregate head validation
    - aggregate migration/recursion checks
    - constrained-head signature migration checks
    - stratification checks
  - updated `docs/plans/largest-runtime-files-pass-2026-03-19.md`:
    - added landed batch entry for `deduce_rule_eval` split
    - adjusted next-queue accounting for the landed/refactored file states
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json` (`6/9 runs passed`; blockers in `status_consistency`, `jit_policy`, `deduce` slices remain)

- Landed the next `runtime-modularization` slice for `deduce` relation mutation primitives:
  - split relation mutation operations (`fact!`, `retract!`, `clear!`, `drop!`) out of
    `src/lisp/deduce_relation_ops.c3` into
    `src/lisp/deduce_relation_ops_mutations.c3`
  - kept behavior unchanged; validation and persistence paths for relation lifecycle,
    integrity checks, and extensional shadow writes/read-paths are still routed through
    the same prim entrypoints
  - updated `docs/plans/largest-runtime-files-pass-2026-03-19.md`:
    - added landed batch entry for `deduce_relation_ops` split
    - adjusted next-queue accounting for landed/refactored file states
  - validation: not run in this slice (build/validation to be run at the lane boundary)

## 2026-03-23

- Closed `B6.5` optional parallel evaluation:
  - the shipped scheduler/runtime surface now covers the admin-visibility
    slice plus the task, offload, and OS-thread batch dispatch helpers
  - the remaining queue/thread batch primitives are now all validated through
    targeted scheduler regressions
- Extended the `B6.5` scheduler/runtime seam with a plural queued-task
  execute helper:
  - added `scheduler_execute_custom_offload_task_jobs(...)` so a small batch
    of internal callback jobs can be queued and joined through the same
    task-handle path
  - added a focused scheduler regression that pins the batch execute path
    with two queued custom callbacks and validates both results arrive in
    order
- Landed the first `B6.5` parallel-evaluation admin slice:
  - relation-local `deduce/stats` now exposes the same parallel SCC batch
    topology counts that `deduce/analyze` already reports:
    - `parallel-recursive-component-count`
    - `parallel-batched-component-count`
    - `parallel-batch-count`
    - `parallel-max-batch-size`
  - `deduce/analyze`, relation-local `deduce/stats`, and `deduce/explain`
    now also expose `parallel-batch-topology` payloads for the recursive SCC
    batches
  - added regression coverage that pins those counts and topology on a
    representative recursive SCC topology
- Landed the first `B6.5` scheduler/runtime seam for future parallel dispatch:
  - the offload executor now accepts internal callback jobs via
    `OffloadWork.custom_fn` / `OffloadWork.custom_ctx`
  - added a focused scheduler regression that pins queued custom-callback
    execution through the offload executor
  - added `scheduler_execute_custom_offload_job(...)` as a dedicated queue
    and join helper for internal callback jobs
  - added `scheduler_execute_custom_offload_task_job(...)` so the same
    custom callback path can reuse a caller-supplied thread-task handle
  - added `scheduler_begin_custom_offload_task_job(...)` so the same
    callback path can fan out multiple queued task jobs before joining them
  - added `scheduler_begin_custom_offload_task_jobs(...)` so a single
    callback can queue a small fan-out of task jobs and return the caller
    supplied handles for later joins
  - added `scheduler_begin_custom_os_thread_jobs(...)` so a single callback
    can fan out multiple actual OS-thread jobs and return their caller
    supplied handles for later joins
  - added `scheduler_execute_custom_os_thread_jobs(...)` so the same
    callback fan-out can also be started and joined through one helper
  - added `scheduler_begin_os_thread_work_jobs(...)` and
    `scheduler_execute_os_thread_work_jobs(...)` so distinct offload work
    items can be dispatched and joined in parallel on real OS threads
  - added `__raw-offload-batch` so a list of raw offload job forms can be
    dispatched through the same batched queue/join path and return a result
    list in order
  - added `__raw-task-spawn-batch` so a list of raw offload job forms can be
    dispatched through the queued task path and return a task-handle list in
    order
  - added `__raw-thread-spawn-batch` so a list of raw offload job forms can
    be dispatched through the actual OS-thread spawn path and return a result
    list in order
- Tightened materialized-view rule-install invalidation:
  - only already-ready materialized views are now marked stale on
    `deduce/rule!`
  - declared-but-unready materialized views keep their existing
    `never-refreshed` lifecycle instead of being blanket-staled
  - added restart coverage for ready-vs-unready selective invalidation
- Goal-directed read admin truthfulness now also records fallback reasons:
  - `deduce/stats`, `deduce/analyze`, and `deduce/explain` now expose
    `last-goal-directed-read-fallback-reason`
  - unsupported captured-call query shapes and dirty-closure fallback reads
    now stay distinguishable from ordinary selected-closure reads in the
    admin surface
- Landed one more `B6.4c` query-demand widening:
  - closed numeric builtin wrappers such as `abs`, `floor`, `ceiling`,
    `round`, `truncate`, `sqrt`, `exp`, `log`, `log10`, `sin`, `cos`, `tan`,
    `asin`, `acos`, `atan`, `atan2`, `min`, `max`, and `pow` are now accepted
    by the shipped
    `deduce/query` demand extractor when they remain row-independent and
    still collapse to the existing equality-demand subset
  - added focused regression coverage for the closed numeric-wrapper demand
    path
- Landed another `B6.4c` query-demand widening:
  - closed one-argument pure closure wrappers such as `id` are now also
    folded by the shipped `deduce/query` demand extractor when the wrapper
    body still collapses to the supported equality-demand subset
  - added focused regression coverage for the closure-wrapper demand path
- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - for the currently shipped preserved-bound subset, positive body scans now
    also use demanded leading-prefix probes inside the naive and seminaive
    rule-step executors, so the abortable demand path prefix-prunes those
    positive body scans instead of only the outer selector plan
  - selector-scoped preserved-bound reads now also record
    `last-goal-directed-read-step-counters` in relation stats, DB analyze,
    and explain, exposing observed per-step counters such as `rows-read`,
    `rows-emitted`, and `join-probes` for the selected rule
  - the shipped `deduce/query` equality-demand subset now also accepts
    row-independent `or` wrappers with one closed falsy branch for both
    selector-scoped and plain reads
  - closed row-independent comparison guards (`=`, `<`, `<=`, `>`, `>=`) are
    now also evaluated by the shipped `deduce/query` demand extractor, so
    row-independent `if` wrappers can still keep the ephemeral query path
  - closed row-independent `and` / `or` guards now also evaluate with Omni’s
    value-returning short-circuit semantics inside the shipped
    `deduce/query` demand extractor, so row-independent boolean wrappers can
    still keep the ephemeral query path
  - closed row-independent `not` guards now also evaluate with ordinary Omni
    truthiness semantics inside the shipped `deduce/query` demand extractor,
    so row-independent negated guard wrappers can still keep the ephemeral
    query path
  - closed literal-side `let` / `block` wrappers around the shipped
    equality-demand subset are now also accepted by `deduce/query`,
    including wrapped `ref` column symbols
  - closed row-column-side `let` / `block` wrappers are now also accepted by
    that same shipped `deduce/query` equality-demand subset when they still
    resolve to `(ref row 'column)` through a closed symbol binding
  - closed row-independent `if` wrappers are now also accepted around the
    row-column side of that same shipped subset, so branch-selected
    `(ref row 'column)` forms can keep the ephemeral query path
  - closed row-independent `and` / `or` wrappers are now also accepted
    around that same row-column side when the selected branch still resolves
    to `(ref row 'column)`
  - the strict preserved-bound safety gate is now pinned again for
    multi-self-recursive `deduce/match` shapes: when every positive
    self-recursive body atom does not preserve the demanded head positions,
    the runtime falls back to `selected-component-closure`, cleans the
    target component, and leaves unrelated dirty siblings untouched
  - focused validation now pins selector-scoped preserved-bound `deduce/match`
    step counters for the selected rule under the shipped prefix-pruning path
    plus selector/plain `deduce/query` demand extraction through falsy `or`
    wrappers, closed comparison guards, closed `and` guards, closed `not`
    guards, and closed literal-side `let` / `block` wrappers
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `269 passed, 0 failed`

- Landed the next `B6.3b` Deduce materialized-view durability slice:
  - file-backed materialized relations now persist mutation-driven and
    rule-set invalidation lifecycle metadata, not only materialized intent
    and successful refresh history
  - reopened materialized relations now stay stale when their persisted
    lifecycle record still carries a non-`none` stale reason
  - focused durability validation now pins restart/open-named persistence of
    `dependency-dirty` stale state after a successful refresh
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `247 passed, 0 failed`

- Landed the next `B6.3b` Deduce materialized-view durability slice:
  - file-backed materialized relations now persist refresh-history metadata
    across reopen / `open-named`, not only the materialized-intent flag
  - the persisted lifecycle record now keeps reopened schema/admin payloads
    truthful after a successful manual refresh by carrying:
    - `materialized-refresh-count`
    - `materialized-last-refresh-mutation-epoch`
    - stored lifecycle stale metadata already tracked by the schema
  - focused durability validation now pins restart/open-named persistence of a
    successful materialized refresh
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `238 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - the demand-bound `match`, `query`, and equality-bound `scan-range` paths
    no longer fall back completely just because one requested head position is
    unsupported by the preserved-bound gate
  - the runtime now projects the requested demand down to the preserved subset,
    evaluates that narrower demand in the abortable in-txn path, and still
    applies the original full pattern / filter / range after the read
  - this widens the shipped fast path safely for mixed supported-plus-
    unsupported bound shapes such as transitive-closure reads that bind both a
    preserved destination and a non-preserved source
  - focused validation now pins projected demand execution for:
    - plain `deduce/match`
    - selector-scoped `deduce/query`
    - plain equality-bound `deduce/scan-range`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `238 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - selector-scoped and plain `deduce/query` now use the same abortable
    demand-bound path for the current preserved-bound positive recursive
    subset when the filter closure is a conjunction of row-independent
    literal, captured-constant, or small safe builtin expression equalities
    on `(ref row 'column)` terms
  - the new explain/admin field
    `goal-directed-demand-bound-head-positions` makes the preserved-bound
    positions visible directly instead of only surfacing planner-intent
    metadata
  - the read-path surface now records `ephemeral-head-demand-query`
  - wider or unsupported filter shapes still fall back to the existing
    selected-closure read path
  - captured row-independent constants, closed numeric builtin
    expressions, and row-independent `let` / `block` / `if` wrappers are now
    accepted in that shipped equality-filter subset
  - unsupported captured call shapes still fall back to selected-closure
    reads
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `251 passed, 0 failed`

- Landed the next `B6.4c` admin-truth slice for projected-demand reads:
  - relation-local `deduce/stats` and DB-level `deduce/analyze` now also
    expose:
    - `last-goal-directed-read-requested-bound-count`
    - `last-goal-directed-read-applied-bound-count`
  - projected-demand reads now make preserved-subset application visible for:
    - bound `deduce/match`
    - equality-filter `deduce/query`
    - equality-bound `deduce/scan-range`
  - focused validation now pins the requested vs applied bound-counts for:
    - plain `deduce/match`
    - selector-scoped `deduce/query`
    - plain `deduce/scan-range`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `238 passed, 0 failed`

- Landed the next `B6.4c` widening for `deduce/query` demand extraction:
  - the demand-bound `deduce/query` path no longer requires every conjunct in
    the filter to be equality-shaped
  - the runtime now harvests supported equality conjuncts for head-demand
    extraction while leaving residual unsupported conjuncts to the original
    full filter after the read
  - this widens the existing ephemeral query path without changing query
    semantics or clearing dirty state
  - focused validation now pins a selector-scoped recursive query that:
    - uses one supported equality conjunct for demand extraction
    - keeps one residual unsupported conjunct in the post-read filter
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `239 passed, 0 failed`

- Tightened selector-scoped `B6.4c` match execution truth:
  - selector-scoped bound `deduce/match` now actually threads the built
    `head_demand` into the abortable in-txn selected-component evaluator,
    instead of only using the preserved-bound gate as a selector-path label

- Landed the next `B6.4c` admin-truth slice for projected-demand reads:
  - relation-local `deduce/stats` and DB-level `deduce/analyze` now also
    expose:
    - `last-goal-directed-read-requested-bound-positions`
    - `last-goal-directed-read-applied-bound-positions`
  - the requested/applied bound-count metadata now has concrete position-list
    payloads behind it for the last goal-directed read, not only counts
  - focused validation now pins those position lists on the projected-demand
    `deduce/query` path for:
    - exact preserved demand
    - projected preserved demand after residual unsupported conjuncts
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `239 passed, 0 failed`
  - this keeps the shipped `ephemeral-head-demand-match` read-path stamp
    honest for selector-scoped bound match reads

- Landed the next `B6.4c` explain/admin-truth slice:
  - `deduce/explain` now mirrors the last actual goal-directed read metadata
    for the selected head relation instead of only surfacing planner
    eligibility and selected-closure metadata
  - the mirrored payload includes:
    - `last-goal-directed-read-execution-path`
    - `last-goal-directed-read-surface`
    - `last-goal-directed-read-selector-rule-index`
    - `last-goal-directed-read-mutation-epoch`
    - requested/applied bound counts
    - requested/applied bound-position lists
  - focused validation now pins an eligible positive recursive closure rule,
    performs a bound `deduce/match` that takes the ephemeral demand path, and
    then verifies `deduce/explain` mirrors that actual runtime choice
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `239 passed, 0 failed`

- Landed the next `B6.4c` safety/regression slice:
  - focused validation now also pins projected-demand truth for:
    - selector-scoped `deduce/match`
    - selector-scoped `deduce/scan-range`
    - plain `deduce/query`
  - this closes the most obvious remaining coverage asymmetry between the
    selector-scoped and plain demand-bound read surfaces without widening the
    public runtime contract
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `242 passed, 0 failed`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `231 passed, 0 failed`

- The preserved-bound gate remains intentionally stricter for
  multi-self-recursive shapes:
  - each demanded head position must still be preserved by every positive
    self-recursive body atom
  - mixed-position preservation across different recursive atoms falls back
    to `selected-component-closure` instead of taking an unsound ephemeral
    demand path

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - relation-local `deduce/stats` and DB-level `deduce/analyze` now expose
    the last actual goal-directed read path chosen by the runtime, instead
    of only planner-intent metadata
  - the current shipped read-path surface now records:
    - `no-op`
    - `selected-component-closure`
    - `full-db-fixpoint`
    - `ephemeral-head-demand-match`
    - `ephemeral-head-demand-scan-range`
  - focused validation now pins last-read truth for:
    - selected-component-closure
    - full-db-fixpoint
    - no-op
    - ephemeral bound `match`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `230 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - the preserved-bound demand-gate widening is now explicitly pinned for
    plain equality-bound `deduce/scan-range`, not just plain bound
    `deduce/match`
  - focused validation now covers a swapped self-recursive rule answering a
    bounded plain `deduce/scan-range` read while leaving the target relation
    dirty
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `228 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - the preserved-bound demand gate now also accepts positive recursive rules
    where demanded head variables are preserved through self-recursive
    body-variable reordering, not only same-position carry-through
  - this widens the shipped demand-bound `deduce/match` subset without
    pretending generic magic-set rewrite is already done
  - focused validation now pins a swapped self-recursive rule answering a
    bound plain `deduce/match` read while leaving the target relation dirty
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `227 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - plain `deduce/scan-range` now uses the same abortable demand-bound path
    for the current preserved-bound positive recursive subset when
    equality-bound head literals can be extracted from positions where
    `lower == upper`, instead of always persisting a tracked closure refresh
  - it restores in-memory schema estimates after abort and leaves the target
    relation dirty
  - wider recursive shapes and non-exact ranges still use the existing plain
    tracked selected-closure auto-execution path
  - focused validation now pins plain equality-bound `deduce/scan-range`
    returning the bounded rows while leaving the target relation dirty
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `226 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - plain `deduce/match` with bound literals now uses the same abortable
    demand-bound path for the current preserved-bound positive recursive
    subset instead of always persisting a tracked closure refresh
  - it restores in-memory schema estimates after abort and leaves the target
    relation dirty
  - wider recursive shapes and unbound patterns still use the existing plain
    tracked selected-closure auto-execution path
  - focused validation now pins plain bound-literal `deduce/match`
    returning the constrained rows while leaving the target relation dirty
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `225 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - selector-scoped `deduce/scan-range` now has the matching bounded-demand
    execution path when equality-bound head literals can be extracted from
    `lower == upper` positions and the selected positive recursive shape
    preserves those bound positions
  - it executes inside an abortable write txn, snapshots/restores in-memory
    schema estimates, and leaves dirty frontier state intact after the read
  - wider recursive shapes still fall back to the existing selected-component
    closure execution path instead of pretending generic demand rewrite is
    already shipped
  - focused validation now pins selector-scoped `deduce/scan-range` demand
    execution that returns the bounded rows while leaving the target relation
    dirty
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `224 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - selector-scoped `deduce/match` with bound head literals now has the first
    real demand-bound execution path for preserved-bound positive recursive
    shapes
  - it executes inside an abortable write txn, snapshots/restores in-memory
    schema estimates, and leaves dirty frontier state intact after the read
  - wider recursive shapes that do not preserve those bound positions still
    fall back to the existing selected-component closure execution path
    instead of pretending generic magic-set rewrite is already shipped
  - focused validation now pins selector-bound recursive match execution that
    returns the constrained rows while leaving the target relation dirty
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `223 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - plain relation reads without a selector now auto-execute the same
    currently eligible dirty positive recursive closure in tracked mode for:
    - `(deduce/match relation pattern)`
    - `(deduce/query relation filter-fn)`
    - `(deduce/count relation)`
    - `(deduce/scan relation)`
    - `(deduce/scan-range relation lower upper)`
  - the automatic read path is still intentionally narrow:
    - it only runs for eligible positive recursive closures,
    - it only runs when the selected closure is dirty,
    - it leaves unrelated recursive components untouched,
    - in tracked mode it executes only the target closure, and after
      `full-recompute-required` plain derived reads fall back to the existing
      full DB fixpoint path while selector-scoped reads still reject,
    - focused regressions now pin the no-op boundaries for that shipped
      plain-read path:
      - clean eligible recursive targets do not execute unrelated dirty
        closures
      - unrelated non-recursive targets do not execute dirty recursive
        closures
      - blocked negated recursive targets do not execute unrelated eligible
        closures
    - it does not widen support to aggregate-bearing or negated recursive
      shapes
  - focused regression coverage now pins:
    - plain `deduce/query` auto-execution that leaves a sibling recursive
      component dirty
    - plain `deduce/match` auto-execution for the same eligible shape
    - plain `deduce/count` / `deduce/scan` / `deduce/scan-range`
      auto-execution for the same eligible shape
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `210 passed, 0 failed`

- Landed the next `B6.3b` Deduce materialized-view lifecycle slice:
  - explicit teardown now ships through
    `deduce 'dematerialize! relation` and `deduce/dematerialize!`
  - dematerialization clears materialized intent and lifecycle metadata
    without dropping the relation or its installed rules
  - dematerialization also clears persisted materialized intent for file-backed
    DBs, so reopen / `open-named` no longer resurrects a manually torn-down
    view
  - relation-scoped `deduce/refresh!` now rejects a dematerialized relation
    through the existing `deduce/refresh-relation-not-materialized` contract
  - focused regression coverage now pins:
    - in-memory dematerialize -> refresh reject -> rematerialize lifecycle
    - restart-time absence of persisted materialized intent after dematerialize
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `189 passed, 0 failed`
- Landed the first real `B6.4c` Deduce query-time goal-directed execution
  slice:
  - `deduce/match` now supports selector-scoped execution for the currently
    eligible positive recursive shape through:
