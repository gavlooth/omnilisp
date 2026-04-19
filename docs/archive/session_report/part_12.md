    now fully reflected in `TODO.md` and `memory/CHANGELOG.md`.
  - The targeted deduce validation item remains open because the bounded deduce
    lane currently crashes (`exit 139`) before the worker-scratch serialized
    recursive-delta assertion can be confirmed green.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted:
  - harden the filtered test-group dispatch paths so unknown filters fail loudly instead of exiting green
- Code/config changes made:
  - made `OMNI_DEDUCE_GROUP_FILTER` and `OMNI_ADVANCED_GROUP_FILTER` report a failure when they match no tests
- Experiment commands and key metrics:
  - no validation run yet for this slice
- Best current checkpoint/config recommendation:
  - if this slice is validated, close the filter-dispatch TODO item and then move back to the remaining audit backlog
- Unresolved issues and next actions:
  - targeted validation still needs to confirm the filtered-dispatch failure path
  - validate this slice before marking the corresponding TODO checkbox complete
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted:
  - harden the scheduler offload queue allocation-failure path in `src/lisp/scheduler_offload_worker.c3`
- Code/config changes made:
  - returned failure immediately when queued-work allocation fails instead of resetting a null buffer
- Experiment commands and key metrics:
  - no validation run yet for this slice
- Best current checkpoint/config recommendation:
  - move on to the filtered test-group dispatch slice next
- Unresolved issues and next actions:
  - the offload path now fails safely on queued-work allocation failure, but it still needs validation
  - validate this slice before marking the corresponding TODO checkbox complete
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted:
  - harden the interpreter module/macro initialization allocation path in `src/lisp/value_interp_init_helpers.c3`
- Code/config changes made:
  - added explicit allocation assertions before the module and macro table fill loops
  - kept startup failure local and deterministic instead of allowing a later null dereference
- Experiment commands and key metrics:
  - no validation run yet for this slice
- Best current checkpoint/config recommendation:
  - continue with the scheduler offload queue slice next
- Unresolved issues and next actions:
  - the startup path still fails fast rather than recovering from OOM; that is acceptable for now but should remain documented
  - validate this slice before marking the corresponding TODO checkbox complete
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted:
  - harden the environment hash-table OOM path in `src/lisp/value_environment.c3`
- Code/config changes made:
  - kept the old environment hash table live until a replacement is fully allocated and initialized
  - skipped binding-array growth when the expansion allocation fails instead of dereferencing a null buffer
- Experiment commands and key metrics:
  - no validation run yet for this slice
- Best current checkpoint/config recommendation:
  - continue with the interpreter init allocation-failure slice next, then the scheduler offload queue slice
- Unresolved issues and next actions:
  - the env write path still lacks a propagated failure signal, so the current fix is crash-safe but not yet semantically fail-fast on OOM
  - validate this slice before marking the corresponding TODO checkbox complete
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Isolate the `deduce_why_result` validation target so the harness runs only the
    bounded why-result regression block.
  - Remove temporary scratch-pass debug scaffolding now that the worker-scratch
    blocker has been validated and closed.
- Code/config changes made
  - Added a file-wide `OMNI_DEDUCE_QUERY_FILTER` guard in
    [`src/lisp/tests_deduce_groups.c3`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3)
    so the deduce group dispatcher short-circuits to the query target instead of
    running unrelated deduce subgroups first.
  - Kept the query-side one-shot target guard in
    [`src/lisp/tests_deduce_query_groups.c3`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_query_groups.c3)
    so the targeted `why-result-bounded` helper runs exactly once.
  - Removed the temporary env-gated scratch-pass trace scaffolding from
    [`src/lisp/tests_deduce_groups.c3`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3)
    while preserving the minimal `OMNI_DEDUCE_GROUP_FILTER` isolation gate.
  - Closed the remaining `deduce_why_result` validation TODO item in
    [`TODO.md`](/home/heefoo/Documents/code/Omni/TODO.md).
- Experiment commands and key metrics
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=why-result-bounded ./build/main --test-suite lisp`
  - key metric: `=== Unified Tests: 1 passed, 0 failed ===`
- Best current checkpoint/config recommendation
  - Use `OMNI_DEDUCE_QUERY_FILTER=why-result-bounded` for the bounded why-result
    regression target; the dispatcher now isolates that path without dragging in
    unrelated deduce groups.
- Unresolved issues and next actions
  - The targeted validation slice is complete.
  - Any further deduce work should come from the separate broader runtime failure
    path rather than this closed validation item.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Re-slice the remaining unfiltered deduce crash into a dedicated reference-head
    validation boundary and verify whether the failure stays after isolation.
- Code/config changes made
  - Extracted the recursive reference-head assertion into
    [`run_deduce_rule_head_reference_snapshot_test(...)`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3)
    inside [`src/lisp/tests_deduce_groups.c3`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3).
  - Added `OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot` handling in
    [`run_deduce_group_tests(...)`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3)
    so the harness can run only that boundary.
  - Updated [`TODO.md`](/home/heefoo/Documents/code/Omni/TODO.md) to make the
    reference-head crash/hang the sole open backlog item again.
- Experiment commands and key metrics
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot ./build/main --test-suite lisp`
  - key state so far: the run reaches the isolated reference-head helper and has
    not yet produced a clean completion signal during multiple polls
- Best current checkpoint/config recommendation
  - Treat `rule_head_reference_snapshot` as the next isolation boundary; the
    broader deduce suite is no longer the right entrypoint for this issue.
- Unresolved issues and next actions
  - Determine whether the extracted `db-rule-ref-ok` helper is itself the hang/crash
    point or whether it still needs one more smaller split.
  - Keep the broader unfiltered deduce run blocked behind this narrower boundary.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Correct the mistaken extraction boundary in the deduce rule-head integrity
    tests after the helper split exposed a self-recursion bug.
- Code/config changes made
  - Restored the keyed and unique integrity checks to
    [`run_deduce_rule_head_integrity_tests(...)`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3)
    in their original order.
  - Kept [`run_deduce_rule_head_reference_snapshot_test(...)`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3)
    as the dedicated reference-head boundary helper containing only the
    `db-rule-ref-ok` regression block.
- Experiment commands and key metrics
  - no validation rerun after this correction yet
- Best current checkpoint/config recommendation
  - Rebuild before any follow-up validation, because the reference-head helper
    split has changed again since the last container run.
- Unresolved issues and next actions
  - Re-run the dedicated `OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot`
    validation after rebuild.
  - Keep the broader unfiltered deduce crash isolated behind that slice.
- Signature: Codex (GPT-5)

## 2026-03-26
- Objective
  - Isolate and fix the bounded `OMNI_LISP_TEST_SLICE=deduce` failure on the
    worker-scratch recursive-delta lane, then re-check
    `deduce parallel worker-scratch component pass computes serialized
    recursive deltas`.
- Code/config changes made
  - Added temporary `OMNI_DEDUCE_GROUP_FILTER` gating and step-by-step trace
    prints in `src/lisp/tests_deduce_groups.c3` to isolate the scratch-pass
    path.
  - Fixed the scratch-pass test fixture to use canonical `deduce/rule!`
    installs and fresh `*-scratch-pass` relation names so the test exercises a
    clean recursive component instead of reusing already materialized state.
- Experiment commands and key metrics
  - `c3c build` -> passes.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=parallel_component_scratch_pass ./build/main --test-suite lisp`
  - Before the fix, the isolated test reported `predicate-count=0` or skipped
    existing head rows and failed.
  - After the fix, the isolated test reported `predicate-count=2`,
    `entry[0]=path-parallel-scratch-pass adds=2 removes=0`, and completed with
    `1 passed, 0 failed`.
  - Enabling `OMNI_TRACE_DEDUCE_WORKER_SCRATCH=1` showed the worker path
    reached `scratch_pass_begin`, both rule-loop iterations, and
    `scratch_pass_end_before_serialize` with `a=2 b=2`.
- Best current checkpoint/config recommendation
  - Keep the fresh `parallel_component_scratch_pass` fixture as the regression
    target while revalidating the broader deduce slice.
- Unresolved issues and next actions
  - Re-run the broader bounded deduce slice to confirm no other worker-scratch
    paths remain broken.
  - Trim the temporary debug traces if they are no longer needed.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Add minimal, gated instrumentation on the deduce worker-scratch path to
    isolate where bounded deduce validation stalls/crashes.
  - Re-run build plus bounded deduce validation with tracing enabled.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_exec_seminaive.c3` with opt-in tracing
    guarded by `OMNI_TRACE_DEDUCE_WORKER_SCRATCH=1`.
  - Instrumented these boundaries:
    - `deduce_seminaive_compute_component_scratch_pass_payload(...)`
    - `deduce_seminaive_evaluate_rule_dry_run(...)`
    - `deduce_seminaive_emit_head_fact(...)`
    - scratch replay/visible-accumulation paths in seminaive step execution.
  - Added helper counters for component-local visible/additions tuple totals for
    trace messages.
- Experiment commands and key metrics
  - `c3c build` -> pass (`Program linked to executable 'build/main'`).
  - `OMNI_TRACE_DEDUCE_WORKER_SCRATCH=1 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - key observations:
    - validation run did not reach completion (stalled/hung state),
    - `/tmp/deduce_worker_scratch_trace.log` stayed at `167` lines across
      repeated checks (including a `30s` stability window),
    - log contained `0` `OMNI_DEDUCE_WORKER_SCRATCH` lines,
    - last named boundary stayed:
      `[PASS] deduce analyze recursive limit reports stable error code`.
- Best current checkpoint/config recommendation
  - Keep this instrumentation patch as a temporary triage slice and move the
    next probe earlier than the worker-scratch path, because current bounded
    deduce execution stalls before any worker-scratch marker is reached.
- Unresolved issues and next actions
  - Open blocker remains active:
    `Investigate/fix bounded OMNI_LISP_TEST_SLICE=deduce crash/hang on the
    worker-scratch recursive-delta lane`.
  - Next actionable move: add one earlier deduce-suite progress marker (before
    scratch tests are invoked) to identify the exact pre-scratch stall point.
- Signature: Codex (GPT-5)

## 2026-03-24

- Follow-up slice completed
  - Objective: close the next editor-tooling backlog item for structured
    `--check --json` diagnostics.
  - Code/config changes made:
    - Updated `src/entry_check_mode.c3` to run parser diagnostics first, then
      compiler/lowering validation (via `lisp::compile_to_c3`) before reporting
      `--check --json`, and removed an unused `getenv` declaration.
    - Updated `src/entry_check_reporting.c3` to:
      - use a dedicated compiler-lowering diagnostic code constant
      - thread the diagnostic code into `print_check_result_json`.
    - Marked `omni --check --json` complete in
      `docs/plans/editor-tooling-roadmap.md`.
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this patch-only slice;
      signature and reference checks were validated by a `rg` scan.
  - Best current checkpoint/config recommendation
    - Keep the completed `--check --json` and structured eval transport behavior in
      place and move to LSP/tooling integration rebase (`tooling/omni-lsp`,
      `tooling/omni-nvim`).
  - Unresolved issues and next actions
    - Remaining non-test/editoring backlog queue includes:
      - rebase `tooling/omni-lsp` onto structured CLI contracts
      - Tree-sitter corpus/query hardening
      - `tooling/omni-nvim` structured-eval + Tree-sitter selection upgrade
      - formatter/indentation implementation
  - Signature: Codex (GPT-5)

- Objectives attempted
  - Close the next runtime split backlog slice by splitting
    `src/lisp/deduce_rule_eval.c3` into a focused orchestration file plus a new
    validation helper file.
  - Continue the same backlog lane by splitting
    `src/lisp/eval_env_copy.c3` into `src/lisp/eval_env_copy_helpers.c3`
    with a shim-style caller module.
- Code/config changes made
  - Added `src/lisp/deduce_rule_eval_validation.c3`.
  - Reduced `src/lisp/deduce_rule_eval.c3` to orchestration calls into the new
    validation helper module.
  - Updated `docs/plans/largest-runtime-files-pass-2026-03-19.md`:
    - added the landed `deduce_rule_eval` split slice
    - refreshed next-queue counts/state
    - added the landed `eval_env_copy` split slice
    - removed `eval_env_copy` from queue head
  - Updated `memory/CHANGELOG.md` with the landed runtime split entry.
  - Updated `memory/CHANGELOG.md` with the landed `eval_env_copy` runtime split
    entry.
  - Added `src/lisp/eval_env_copy_helpers.c3`.
  - Reduced `src/lisp/eval_env_copy.c3` to a shim comment pointing at the
    helper file.
- Experiment commands and key metrics
  - no new targeted commands were run for `eval_env_copy` split in this slice
  - reused checkpoint evidence from the immediately previous landed slice:
    - `c3c build` (pass)
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9` total runs, `6` pass, `3` fail with pre-existing failures:
      `status_consistency`, `jit_policy`, `deduce`)
- Best current checkpoint/config recommendation
  - Keep `deduce_rule_eval` split landed in this pass and continue with
    `src/lisp/eval_pattern_support.c3` as the next size-driven file split
    candidate.

## 2026-03-26

- Objectives attempted
  - Validate the corrected `rule_head_reference_snapshot` deduce slice after
    fixing the helper extraction boundary in `src/lisp/tests_deduce_groups.c3`.
  - Confirm that the targeted helper now runs in isolation without dragging in
    unrelated deduce subgroups.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_groups.c3` so
    `run_deduce_rule_head_reference_snapshot_test(...)` contains only the
    `db-rule-ref-ok` reference-head regression block.
  - Restored the keyed and unique integrity checks to
    `run_deduce_rule_head_integrity_tests(...)` so the helper split no longer
    self-recurses or steals earlier integrity coverage.
  - Kept the `OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot` dispatch
    hook in `run_deduce_group_tests(...)` for the isolated target path.
- Experiment commands and key metrics
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot ./build/main --test-suite lisp`
  - key metric: `=== Unified Tests: 1 passed, 0 failed ===`
- Best current checkpoint/config recommendation
  - Use `OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot` as the narrow
    regression checkpoint for this helper boundary.
- Unresolved issues and next actions
  - The isolated reference-head slice is green.
  - Any remaining deduce failures belong to the broader unfiltered runtime
    blocker, not this extracted helper.
- Signature: Codex (GPT-5)
- Unresolved issues and next actions
  - Investigate and resolve the three status-summary failures (`status_consistency`,
    `jit_policy`, `deduce`) to restore a fully green pass gate.
- Follow-up slice completed
  - Objective: complete the next `docs`/`examples` backlog item by replacing
    removed syntax forms with canonical constructor/type spellings.
  - Changes:
    - Updated examples in `examples/deduce_crud_server.omni` and
      `examples/finwatch/*.omni` to use canonical type annotations:
      `^Integer` / `^Boolean` and canonical quoted type symbols.
    - Updated docs in:
      - `docs/reference/04-type-system.md`
      - `docs/reference/09-concurrency-ffi.md`
      - `docs/type-system-syntax.md`
      - `docs/PROJECT_TOOLING.md`
      - `docs/FEATURES.md`
      - `docs/LANGUAGE_SPEC.md`
    - Marked `docs/plans/post-complete-backlog.md` item as complete.
  - Experiment commands and key metrics
    - no new shell validation commands were run for this docs/examples cleanup slice.
  - Best current checkpoint/config recommendation
    - Proceed to the next post-complete slice; keep additional alias cleanup
      scoped to docs that explicitly present non-canonical language syntax.
  - Unresolved issues and next actions
    - Continue scanning for `Int`/`^Int` older examples in remaining docs once the
      editor tooling roadmap changes land.
- Follow-up slice started
  - Objective: begin the next post-complete backlog item by creating a dedicated
    Deduce analytics milestone for statistics + cleanup maintenance verbs.
  - Changes:
    - Added `docs/plans/deduce-analytics-extension-milestone-2026-03-24.md` with a
      minimal two-track task list:
      - statistics maintenance/refresh planning
      - data-cleanup verb expansion planning
    - Marked `Add a minimal analytics extension task list for deduce ...` as completed
      in `docs/plans/post-complete-backlog.md` and linked it to the new milestone.
    - Cross-referenced the dedicated milestone from:
      - `docs/plans/deduce-sophistication-plan-2026-03-20.md`
      - `docs/plans/README.md`
  - Experiment commands and key metrics
    - no runtime commands were run for this docs-only planning slice.
  - Best current checkpoint/config recommendation
    - Keep the item as a planning slice and begin execution slices directly from
      `docs/plans/deduce-analytics-extension-milestone-2026-03-24.md`.
  - Unresolved issues and next actions
    - Continue with the next unchecked post-complete backlog item:
      `Add tests for deterministic behavior in deduce cleanup and statistics
      operations under empty-relation and malformed-schema inputs.`
- Follow-up slice completed
  - Objective: continue the largest-first runtime modularization lane with the
    next queued file split.
  - Changes:
    - Added `src/lisp/eval_pattern_support_helpers.c3`.
    - Reduced `src/lisp/eval_pattern_support.c3` to a shim comment pointing at
      the helper file.
    - Updated `docs/plans/largest-runtime-files-pass-2026-03-19.md`:
      - added the landed `eval_pattern_support` split slice
      - refreshed next-queue ordering/counts
    - Marked `Continue largest-first modularization ...` as completed in
      `docs/plans/post-complete-backlog.md`.
  - Experiment commands and key metrics
    - no new validation/build commands were run for this split-only slice.
  - Best current checkpoint/config recommendation
    - Continue with `src/lisp/aot_runtime_bridge.c3` as the next largest queued
      runtime split target.
  - Unresolved issues and next actions
    - Run central validation (`c3c build` and status-summary gate) once the next
      split cluster is landed.
- Follow-up slice completed
  - Objective: close the public-doc migration-note item for effect-handler
    composition helper naming.
  - Changes:
    - Added migration note to `docs/EFFECTS_GUIDE.md` and documented
      `handle/chain` as the canonical helper name.
    - Added corresponding migration-note subsection in
      `docs/LANGUAGE_SPEC.md` section `10.6`.
    - Aligned helper naming in `docs/syntax-decision.md` (`handle/chain`
      canonical; historical `with-handlers` / `handle-chain` non-canonical).
    - Marked `Add a migration note for effect-handler composition helper
      names ...` as completed in `docs/plans/post-complete-backlog.md`.
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this docs-only slice.
  - Best current checkpoint/config recommendation
    - keep `handle/chain` as the only helper name used in public examples.
  - Unresolved issues and next actions
    - remaining non-test backlog item is the editor tooling roadmap execution
      lane.
- Follow-up slice completed
  - Objective: execute the editor tooling roadmap with a concrete
    bootstrap/docs integration slice.
  - Changes:
    - Added a runnable editor tooling quickstart in `README.md` under
      `Editor Tooling`.
    - Added `Editor Tooling Bootstrap (Roadmap Slice)` in
      `docs/PROJECT_TOOLING.md` with setup commands/snippets for:
      - `tooling/tree-sitter-omni`
      - `tooling/omni-lsp`
      - `tooling/omni-nvim`
    - Updated `docs/plans/editor-tooling-roadmap.md`:
      - marked bootstrap wiring for all three tooling packages complete
      - marked README editor integration example complete
      - added landed-slice note with residual queue explicitly left open
    - Updated `docs/plans/post-complete-backlog.md`:
      - marked the roadmap execution item complete for the shipped slice
      - added a new explicit residual item for remaining roadmap work
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this docs/tooling slice.
  - Best current checkpoint/config recommendation
    - use `docs/PROJECT_TOOLING.md` as the canonical bootstrap entrypoint for
      first-party editor integration.
  - Unresolved issues and next actions
    - continue with the new residual roadmap item in
      `docs/plans/post-complete-backlog.md`.
- Follow-up slice completed
  - Objective: close the `--init` docs integration sub-slice in the editor
    tooling roadmap.
  - Changes:
    - Updated `docs/PROJECT_TOOLING.md` in the `--init` section with a
      recommended post-scaffold editor/tooling setup path.
    - Updated `docs/man/omni.1` (`Init`) with the same first-step check and
      pointer to tooling docs.
    - Marked `Update --init templates or docs with recommended editor/tooling
      setup.` as completed in `docs/plans/editor-tooling-roadmap.md`.
    - Refined remaining residual roadmap wording in
      `docs/plans/post-complete-backlog.md` to remove the now-completed
      `--init` integration mention.
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this docs/tooling slice.
  - Best current checkpoint/config recommendation
    - keep `docs/PROJECT_TOOLING.md` as the canonical setup path and mirror
      key CLI guidance in `docs/man/omni.1`.
  - Unresolved issues and next actions
    - continue remaining editor-tooling roadmap slices:
      structured test output and deeper formatter/LSP/Neovim upgrades.
- Follow-up slice completed
  - Objective: close the Neovim help-tags/docs sub-slice from the editor
    tooling roadmap.
  - Changes:
    - Added first-party Neovim help doc:
      `tooling/omni-nvim/doc/omni.nvim.txt`.
    - Updated `tooling/omni-nvim/README.md` with explicit helptags generation
      and `:help omni.nvim` usage.
    - Marked `Add local docs/help tags or a concise :help omni.nvim path.`
      as completed in `docs/plans/editor-tooling-roadmap.md`.
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this docs/tooling slice.
  - Best current checkpoint/config recommendation
    - keep `tooling/omni-nvim/doc/` as the canonical in-repo help surface and
      retain README parity with the shipped help tags path.
  - Unresolved issues and next actions
    - continue remaining roadmap slices (structured test output plus deeper
      formatter/LSP/Neovim feature work).
- Follow-up slice completed
  - Objective: close the formatting-policy decision and canonical-rules
    documentation sub-slice in the editor tooling roadmap.
  - Changes:
    - Updated `docs/PROJECT_TOOLING.md` with a dedicated
      `Formatting and Indentation Policy` section:
      - decision: `both in phases` with indentation-first behavior
      - canonical formatting rules for special forms, vector/dict literals,
        `handle` clauses, type/method declarations, and macro/template blocks
    - Updated roadmap checkboxes in
      `docs/plans/editor-tooling-roadmap.md` for:
      - strategy decision (`both in phases`)
      - canonical formatting-rules definition
    - Updated roadmap landed-slice notes with this formatter-policy closure.
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this docs/policy slice.
  - Best current checkpoint/config recommendation
    - keep `docs/PROJECT_TOOLING.md` as the canonical formatting-policy source
      until `omni --fmt` semantics are implemented.
  - Unresolved issues and next actions
    - continue with remaining editor-tooling roadmap work:
      structured test output and deeper formatter/LSP/Neovim implementation
      slices.
- Follow-up slice completed
  - Objective: implement first-party machine-readable symbol help for editor
    integrations.
  - Changes:
    - Added CLI mode `--describe <symbol> [--json]`:
      - `src/entry_describe_mode.c3`
      - `src/entry_describe_reporting.c3`
      - wired in `src/entry.c3`
      - usage text updated in `src/entry_cli_help_version.c3`
    - Added docs/man coverage:
      - `docs/PROJECT_TOOLING.md`
      - `docs/man/omni.1`
    - Updated roadmap progress:
      - marked `--describe <symbol>` item complete in
        `docs/plans/editor-tooling-roadmap.md`
      - refined residual wording in
        `docs/plans/post-complete-backlog.md`
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this implementation+docs
      slice.
  - Best current checkpoint/config recommendation
    - use `omni --describe --json <symbol>` as the first-party machine-readable
      symbol-help source for editor hover/help integration.
  - Unresolved issues and next actions
    - remaining roadmap queue still includes structured test output and deeper
      formatter/LSP/Neovim feature upgrades.
- Signature: Codex (GPT-5)

## 2026-03-25

- Objectives attempted
  - Close the first truthful proof-path-integrated goal-directed provenance
    slice for `deduce/why-result` instead of leaving the whole lane as one
    broad residual item.
- Code/config changes made
  - Updated `src/lisp/deduce_db_handles.c3`,
    `src/lisp/deduce_db_handles_register.c3`,
    `src/lisp/deduce_db_handles_mutation_tracking.c3`, and
    `src/lisp/deduce_schema_query.c3`:
    - relation schemas now remember an exact-one goal-directed `query`
      subject key
    - `deduce/why-result` now attaches path-local
      `goal-directed-read-context` only when the traced tuple matches that
      exact-one query subject snapshot
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a regression proving the matching tuple gets path-local context
      while another row in the same relation does not
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2b`
    - closed `B6.9b2b1`
    - left broader provenance integration as `B6.9b2b2`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - exact-one goal-directed `query` row now yields path-local
      `goal-directed-read-context`
    - another row in the same relation keeps that path-local field `nil`

- Objectives attempted
  - Close the next truthful proof-integrated provenance slice by extending the
    exact-one path-local goal-directed context from `deduce/query` to
    `deduce/match`.
- Code/config changes made
  - Updated `src/lisp/unify.c3`:
    - goal-directed transactional `deduce/match` scans now capture the matched
      stored-row key when exactly one tuple matched
    - the relation schema stores that exact-one subject snapshot after the
      `match` read is recorded
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened path-local `goal-directed-read-context` attachment from exact-one
      `query` subjects to exact-one `query` or `match` subjects
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a regression proving the matching tuple gets path-local context
      after exact-one goal-directed `deduce-match`
    - another row in the same relation still keeps that path-local field `nil`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2b2`
    - closed `B6.9b2b2a`
    - left broader provenance integration as `B6.9b2b2b`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - exact-one goal-directed `deduce/match` row now yields path-local
      `goal-directed-read-context`
    - another row in the same relation keeps that path-local field `nil`

- Objectives attempted
  - Close the next truthful proof-integrated provenance slice by extending the
    exact-one path-local goal-directed context from `deduce/query` /
    `deduce/match` to `deduce/scan-range`.
- Code/config changes made
  - Updated `src/lisp/deduce_relation_scan_helpers_more.c3`:
    - transactional `deduce/scan-range` scans can now capture the exact-one
      stored-row key when the result set has one row
  - Updated `src/lisp/deduce_relation_ops_query.c3`:
    - goal-directed `deduce/scan-range` now records that exact-one subject
      snapshot into the relation schema after the read is recorded
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened path-local `goal-directed-read-context` attachment from exact-one
      `query` / `match` subjects to exact-one `query` / `match` /
      `scan-range` subjects
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a regression proving the matching tuple gets path-local context
      after exact-one goal-directed `deduce/scan-range`
    - another row in the same relation still keeps that path-local field `nil`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2b2b`
    - closed `B6.9b2b2b1`
    - left broader provenance integration as `B6.9b2b2b2`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - exact-one goal-directed `deduce/scan-range` row now yields path-local
      `goal-directed-read-context`
    - another row in the same relation keeps that path-local field `nil`

- Objectives attempted
  - Close the next truthful proof-integrated provenance slice by pinning
    selector-scoped parity for the shipped exact-one row-read path-local
    context surface.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a combined regression for selector-scoped exact-one
      `deduce/query`, `deduce/match`, and `deduce/scan-range`
    - matching proof paths now have explicit coverage for
      `selector-rule-index = 1`
    - non-matching rows in the same relation still keep that path-local field
      `nil`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2b2b2`
    - closed `B6.9b2b2b2a`
    - left broader provenance integration as `B6.9b2b2b2b`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - selector-scoped exact-one `query`, `match`, and `scan-range` paths all
      now have pinned path-local `goal-directed-read-context`
    - each matching path keeps `selector-rule-index = 1`

- Objectives attempted
  - Close the next truthful proof-integrated provenance slice by extending the
    exact-one path-local goal-directed context to `deduce/scan`.
- Code/config changes made
  - Updated `src/lisp/deduce_relation_scan_helpers_more.c3`:
    - full-relation `deduce/scan` can now capture the exact-one stored-row key
      when the scan result has one row
  - Updated `src/lisp/deduce_relation_ops_query.c3`:
    - `deduce/scan` now records that exact-one subject snapshot into the
      relation schema after the read completes
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened path-local `goal-directed-read-context` attachment from exact-one
      `query` / `match` / `scan-range` subjects to exact-one
      `query` / `match` / `scan` / `scan-range` subjects
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a regression for plain exact-one `deduce/scan`
    - kept selector-scoped parity scoped to the already shipped
