## 2026-03-26

- Objectives attempted
  - Seed a new backlog item from the next scan-path hotspot after the leak-cleanup queue was emptied.
- Code/config changes made
  - No code changes. Inspected `src/lisp/deduce_relation_scan_helpers_more.c3`
    and `src/lisp/deduce_rule_eval_exec_seminaive.c3` for the next actionable
    risk.
  - Added one new open TODO item for scan-path row materialization OOM
    handling in `src/lisp/deduce_relation_scan_helpers_more.c3`.
- Experiment commands and key metrics
  - `rg -n "malloc|alloc|capacity|scan_range|recursive|delta|scratch|dict" src/lisp/deduce_relation_scan_helpers_more.c3`
  - `rg -n "malloc|alloc|capacity|scan|recursive|delta|scratch|component|serialized" src/lisp/deduce_rule_eval_exec_seminaive.c3`
  - `sed -n '1,35p' src/lisp/deduce_relation_scan_helpers_more.c3`
  - `sed -n '161,210p' src/lisp/deduce_relation_scan_helpers_more.c3`
- Best current checkpoint/config recommendation
  - The next narrow fix is to guard `make_hashmap(...)` inside
    `deduce_relation_materialize_row_dict(...)` so scan-range materialization
    can fail cleanly on OOM instead of dereferencing a null hashmap.
- Unresolved issues and next actions
  - Implement and validate the new scan-path allocation guard.
- Signature: Codex (GPT-5)

## 2026-03-26 Omni version bump to 0.2.0
- Objective
  - Bump the Omni runtime/app version to `0.2.0` and replace the compiled
    binary in `build/main`.
- Code/config changes made
  - Updated the runtime-facing version surfaces to `0.2.0`:
    - `project.json`
    - `src/entry_cli_help_version.c3`
    - `src/entry_project_init_writer_project_json.c3`
    - `src/entry_project_init_writers.c3`
    - `docs/OMNI_REFERENCE.md`
    - `docs/man/omni.1`
    - `docs/man/omni-language.7`
  - Rebuilt `build/main` so the replacement binary now prints `omni 0.2.0`.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --version`
  - key metric: version output is now `omni 0.2.0`
- Best current checkpoint/config recommendation
  - Keep the version strings in the runtime entrypoint, project metadata, and
    generated docs/man pages synchronized on `0.2.0` until the next release
    bump.
- Unresolved issues and next actions
  - None for this slice.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce scan-range row-dict OOM guard validation
- Objective
  - Close the final open TODO item by proving the scan-range row-dict
    allocation guard fails cleanly under a forced OOM probe.
- Code/config changes made
  - Hardened `src/lisp/deduce_relation_scan_helpers_more.c3` so
    `deduce_relation_materialize_row_dict(...)` returns
    `deduce/query-out-of-memory` when `make_hashmap(...)` fails.
  - Added an env-gated direct helper probe in
    `src/lisp/tests_deduce_query_scan_groups.c3` behind
    `OMNI_DEDUCE_FORCE_ROW_DICT_OOM=1` so the row-dict OOM boundary is tested
    directly instead of relying on a specific `scan-range` optimization path.
  - Marked the TODO item complete in `TODO.md` and reduced
    `Current actionable count` to `0`.
  - Recorded the result in `memory/CHANGELOG.md`.
- Experiment commands and key metrics
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_DEDUCE_FORCE_ROW_DICT_OOM=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - result: `1 passed, 0 failed`
- Best current checkpoint/config recommendation
  - Keep the helper-level OOM probe in place as the regression boundary for
    this slice; it validates the production null-check without depending on the
    optimizer/demand-path shape of `scan-range`.
- Unresolved issues and next actions
  - None for this slice. The live backlog is now empty.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Check whether the remaining deduce scan row-dict OOM guard can be
    validated with an existing failure-injection or allocator-failure hook.
- Code/config changes made
  - No code changes in this inspection pass.
- Experiment commands and key metrics
  - Repo-wide search for allocator-failure and OOM hook patterns did not surface
    a reusable failure-injection seam for `deduce_relation_materialize_row_dict`.
- Best current checkpoint/config recommendation
  - Keep the deduce scan OOM guard as a code-only safety fix for now.
- Unresolved issues and next actions
  - Revisit validation only if a practical allocator-failure hook is added
    later; otherwise leave the item documented rather than forcing a fake test.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Push the scheduler offload reuse regression aggressively until it
    validated the actual reset and free-list bound behavior, instead of
    leaving it as benchmark-only coverage.
- Code/config changes made
  - Strengthened `src/lisp/tests_scheduler_boundary_worker.c3` so the existing
    scheduler boundary test directly probes a recycled `QueuedOffloadWork`
    node and asserts the reset contract plus the `queued_free_count` bound.
  - Closed the scheduler offload reuse regression item in `TODO.md` after the
    targeted scheduler slice passed.
  - Left the deduce scan-path OOM guard open as the only remaining backlog
    item.
- Experiment commands and key metrics
  - `c3c build`
    - linked successfully after the scheduler regression tightening
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
    - summary: `105 passed, 0 failed`
- Best current checkpoint/config recommendation
  - Keep the deduce scan-path OOM guard as a code-only safety item until a
    practical allocator-failure hook exists.
- Unresolved issues and next actions
  - The deduce scan-path OOM guard remains open but unvalidated.
  - If a low-friction OOM injector appears later, turn that scan guard into a
    real regression slice; otherwise keep it documented as a code-only hardening
    boundary.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Continue a fresh TODO seeding pass after the backlog had been drained, and
    separate code-only safety fixes from regression-backed slices.
- Code/config changes made
  - Added a new open TODO item for scheduler offload reuse/recycle correctness
    in `src/lisp/scheduler_offload_worker.c3` and
    `src/lisp/tests_scheduler_offload_bench_groups.c3`.
  - Kept the deduce scan-path OOM guard open in `TODO.md` as a code-only
    safety fix because no practical allocator-failure injection hook exists in
    the repo.
  - Updated `TODO.md` so `Current actionable count` is now `2`.
- Experiment commands and key metrics
  - `c3c build` had already been revalidated before this backlog update; no new
    validation run was needed for the doc-only seeding pass.
- Best current checkpoint/config recommendation
  - Treat the deduce scan-path OOM guard as shipped code with no cheap
    regression path, and use the scheduler offload reuse item as the next
    regression-backed slice.
- Unresolved issues and next actions
  - Add or locate a practical reuse-cycle regression for the scheduler offload
    queue.
  - Leave the deduce scan-path OOM guard open only as long as the backlog
    needs to remember the code-only safety boundary.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Continue the fresh hotspot-backed backlog pass after `TODO.md` was emptied
    and turn the next concrete runtime risk into a small code fix.
- Code/config changes made
  - Updated `src/lisp/deduce_relation_scan_helpers_more.c3` so scan-range row
    materialization fails through the native deduce OOM path when
    row-dict allocation is unavailable instead of dereferencing a null
    hashmap.
  - Kept the method-table hardening and isolated JIT-policy regression gate in
    place from the previous pass.
  - Left `TODO.md` with one open item for the deduce scan OOM guard, because
    no cheap allocator-failure injection hook exists for a focused regression.
  - Recorded the method-table hardening in `memory/CHANGELOG.md`.
- Experiment commands and key metrics
  - `c3c build`
    - linked successfully after the deduce scan OOM guard and the earlier
      method-table hardening
- Best current checkpoint/config recommendation
  - Keep the deduce scan OOM guard open as a code-only safety fix until there
    is a practical way to force the allocation failure path in a regression.
- Unresolved issues and next actions
  - Add a regression only if a low-friction OOM injection or allocator-failure
    hook appears later.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Harden the repo-audit follow-up items for environment OOM handling,
    interpreter init failures, scheduler offload allocation failure, and
    filtered test-group dispatch.
- Code/config changes made
  - Updated `src/lisp/value_environment.c3` to keep the old hash table alive
    until a replacement is fully built and to skip binding growth when the
    expansion buffer allocation fails.
  - Updated `src/lisp/value_interp_init_helpers.c3` to assert module and macro
    table allocations before the fill loops.
  - Updated `src/lisp/scheduler_offload_worker.c3` so queued-work allocation
    returns failure immediately when `mem::malloc` returns null.
  - Updated `src/lisp/tests_deduce_groups.c3` and `src/lisp/tests_tests.c3`
    so unknown `OMNI_DEDUCE_GROUP_FILTER` and `OMNI_ADVANCED_GROUP_FILTER`
    values fail loudly instead of exiting green.
  - Updated `TODO.md` so the audit plan items are closed after validation.
- Experiment commands and key metrics
  - `c3c build`
  - `c3c build --sanitize=address`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
    - summary: `105 passed, 0 failed`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot ./build/main --test-suite lisp`
    - summary: `1 passed, 0 failed`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=does-not-exist ./build/main --test-suite lisp`
    - summary: failed loudly as intended with `deduce group filter matched no tests`
  - ASAN runs produced leak reports dominated by pre-existing LMDB/runtime allocations; they did not surface a new functional failure in the changed slices.
- Best current checkpoint/config recommendation
  - Keep the non-ASAN behavioral results as the shipped signal for these audit slices, and treat the ASAN leak output as separate existing noise until that broader teardown work is addressed.
- Unresolved issues and next actions
  - If the leak reports need to become actionable, split that into its own teardown/cleanup audit item rather than folding it back into these OOM/harness fixes.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Triaged the live TODO queue and split it into runnable slices for parallel
    agent work.
  - Launched three worker agents for the boundary telemetry, equality/offload
    allocator, and deduce/governance tracks.
  - Recovered from an interrupted wait cycle and verified that the broad worker
    batch had not produced usable output before closing it out.
- Code/config changes made
  - Added `scripts/run_boundary_profile_regression.sh` to chain the container-
    capped boundary workload, parse step, and threshold check into one
    runnable regression command.
  - Updated `docs/plans/boundary-profiling-baseline-2026-03-11.md` to point at
    the new regression wrapper.
  - Added a `Validation Lanes` section to `docs/PROJECT_TOOLING.md` to make
    the boundary/lifetime vs allocator vs syntax/compiler validation split
    explicit.
  - Clarified the lane-ownership wording in `docs/areas/memory-runtime.md` so
    syntax/compiler-only work does not inherit a memory lane by default.
  - Updated the deduce benchmark plan docs:
    `docs/plans/deduce-scan-query-count-benchmark-baseline-2026-03-11.md`,
    `docs/plans/deduce-scan-range-materialization-cost-baseline-2026-03-11.md`,
    and `docs/plans/deduce-query-optimization-evaluation-2026-03-11.md` to
    match the current benchmark-file split.
  - Added a mixed nested equality audit case in
    `src/lisp/tests_memory_lifetime_boundary_decision_bench_groups.c3` and
    threaded the extra mixed-case timings into the existing equality benchmark
    summary.
  - Added offload allocation audit counters in
    `src/lisp/scheduler_offload_worker.c3` and exposed them through
    `src/lisp/tests_scheduler_offload_bench_groups.c3`.
  - Surfaced boundary telemetry summaries in
    `scripts/parse_boundary_profile_summary.sh` so scope-chain pressure and
    dominant return-path outcomes appear directly in the regression flow.
  - Hardened the offload pool/reuse boundary in
    `src/lisp/scheduler_offload_worker.c3` and strengthened
    `src/lisp/tests_scheduler_boundary_worker.c3` to assert the pooled reuse
    fast path.
  - Moved `prim_schema_explain(...)` into
    `src/lisp/schema_validation.c3` so validation-facing entrypoints live
    together and `src/lisp/schema.c3` stays focused on the explain selector
    dispatcher.
  - Split the dispatch/match diagnostics formatting helpers out of
    `src/lisp/eval_dispatch_types.c3` into
    `src/lisp/eval_dispatch_match_errors.c3` so the type registry and
    type-query surface stay focused in the original file.
  - Extracted the `deduce_why_result_*` explainability/path-building block and
    `prim_deduce_why_result` into `src/lisp/deduce_why_result.c3` so
    `src/lisp/deduce_schema_query.c3` can stay focused on query/execution.
  - Closed the corresponding deduce explainability extraction TODO line in
    `TODO.md`; the only remaining open item for that slice is targeted
    validation.
  - Added a tooling note that deprecated `OMNI_LISP_TEST_SLICE` aliases
    (`memory-soak`, `syntax`) are rejected by
    `src/lisp/tests_slice_policy.c3` and the explicit slice names should be
    used instead.
  - Closed the `deduce-query` optimization TODO line in `TODO.md` after
    confirming `docs/plans/deduce-query-optimization-evaluation-2026-03-11.md`
    already records the final decision to keep the current full-scan + callback
    filtering model.
  - Closed the modularization split-candidate TODO line in `TODO.md` after the
    schema extraction landed and the remaining listed candidates were already
    split in earlier slices.
  - Narrowed the remaining modularization TODO in `TODO.md` to the
    `deduce_why_result_*` extraction from `src/lisp/deduce_schema_query.c3`
    and kept the validation follow-up scoped to that same slice.
  - Added a derived `scan_range_materialize_us_per_row` metric to
    `src/lisp/tests_deduce_query_bench_groups_more.c3`.
  - Reconciled `docs/plans/dispatch-hot-path-benchmark-baseline-2026-03-11.md`
    with the actual benchmark file in
    `src/lisp/tests_advanced_type_dispatch_groups.c3`.
  - Expanded the equality inline-first workspace caps in
    `src/lisp/eval_pattern_support_helpers.c3` to keep common nested equality
    comparisons on-stack longer.
  - Hoisted the deduce in-txn scan-row dict capacity computation out of the
    per-row loop in `src/lisp/deduce_relation_scan_helpers_more.c3`.
- Experiment commands and key metrics
  - `wait_agent` on the three broad workers timed out twice and produced no
    final status.
  - Closed the stalled workers; each returned `previous_status: running`.
  - The live queue inventory surfaced 24 open items in
    `TODO.md` under `Legacy Runtime and Validation Follow-up`.
  - One deduce docs worker completed cleanly; the equality benchmark worker was
    closed after timing out without a patch.
  - Later, two tighter workers for equality and deduce benchmark code were
    also closed after timing out without a patch; the equality and offload
    slices were finished locally instead.
  - The boundary telemetry parser lane and offload pool/reuse lane both landed
    after the first report draft, so this entry now reflects the full shipped
    scope for the session.
  - The deduce per-row materialization metric and dispatch benchmark-plan
    reconciliation landed after the main queue pass, so they are now recorded
    as shipped session work as well.
  - The alias audit and modularization backlog reconciliation landed after the
    code slices, so the live queue count dropped to 2 open items.
  - The bounded deduce validation run for the `deduce_why_result` split used
    `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    and finished with `282 passed, 112 failed`.
  - The first actionable failure surfaced by that run was
    `deduce parallel worker-scratch component pass computes serialized recursive deltas`.
  - After wiring `scratch_visible_sets` through the dry-run worker-scratch call
    chain, `c3c build` now passes again.
  - A follow-up bounded deduce validation rerun
    (`scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`)
    now aborts with `exit 139` before the suite reaches a normal fail-summary
    boundary.
  - The failure set was broader than the `deduce_why_result` extraction itself,
    spanning recursive worker-scratch, why-result metadata, selector-scoped
    deduce query, stats/analyze, and aggregate/recursive aggregate coverage.
  - Promoted the recursive worker-scratch recursive-delta failure into its own
    explicit backlog blocker in `TODO.md`, and kept the `deduce_why_result`
    targeted validation item open behind it.
  - The equality workspace cap expansion and deduce scan-path hoist landed as
    the last remaining semantic cleanup items in the current runtime follow-up
    queue.
- Best current checkpoint/config recommendation
  - Treat `TODO.md` as the live execution queue and continue with narrower
    worker scopes rather than broad triage batches.
- Unresolved issues and next actions
  - Relaunch focused agents for the highest-signal slices:
    boundary telemetry, equality/offload allocator work, and deduce perf.
  - If the next worker batch stalls again, switch to direct local execution on
    one narrow backlog item instead of waiting on the whole queue.
  - Continue the offload and deduce workers only if they return cleanly; if not,
    restart them with a smaller write scope.
  - Restart the equality benchmark probe with a single-file write scope if that
    slice needs to be pushed further.
  - The equality and offload audit slices now have concrete local patches; the
    remaining code-side work is the deduce benchmark hook if that lane still
    needs a code change.
  - Keep TODO, changelog, and this report aligned if any follow-up slices land
    on top of the boundary parser or offload pool/reuse changes.
  - The deduce summary metric and dispatch plan correction are now part of the
    shipped session boundary, not residual work.
  - The equality workspace strategy and deduce scan-path allocation cleanup are
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
    deprecated syntax aliases with canonical constructor/type spellings.
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
    - Continue scanning for `Int`/`^Int` legacy examples in remaining docs once the
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
      `query` / `match` / `scan-range` slice after confirming selector-scoped
      `scan` is not in the current goal-directed eligibility surface
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2b2b2b`
    - closed `B6.9b2b2b2b1`
    - left broader provenance integration as `B6.9b2b2b2b2`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - exact-one goal-directed `deduce/scan` now yields path-local
      `goal-directed-read-context`
    - selector-scoped `deduce/scan` stayed outside the current shipped slice
      and was left undocumented as such

- Objectives attempted
  - Close the next integrity-class naming lane so the remaining Deduce
    constraint queue targets one concrete class instead of generic “widened
    integrity” wording.
- Code/config changes made
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`, and
    `docs/reference/08-libraries.md`:
    - closed `B6.10a1`
    - fixed the next integrity class as canonical `check`
    - renamed the remaining backlog items to refer to `check` constraints
      directly
  - Added `docs/plans/deduce-next-integrity-class-decision-2026-03-25.md`:
    - recorded the naming decision, rationale, rejected aliases, and the
      remaining follow-up slices
- Validation run
  - doc/backlog shaping only:
    - confirmed `TODO.md` now closes `B6.10a1`
    - confirmed remaining `B6.10*` items now refer to `check` constraints

- Objectives attempted
  - Turn the first parallel-runtime blocker into a concrete execution-model
    decision instead of pretending the existing LMDB-writing evaluator could
    be fanned out safely.
- Code/config changes made
  - Closed `B6.9b2b2b2b2b2b2b2b2` with path-level mixed-context goal-directed
    provenance lists across support-frame relations:
    - proof paths now expose `goal-directed-read-contexts` when matching
      support-frame contexts come from different relations
    - same-relation support paths still use the singular
      `goal-directed-read-context` field instead of a plural list
  - Closed `B6.9b2b2b2b2b2b2b2b1` with common path-level goal-directed context
    propagation across same-relation support frames:
    - proof paths now inherit bounded `goal-directed-read-context` when all
      matching support-frame contexts come from the same relation-local
      last-read state
    - kept mixed-relation support paths unmerged and left that broader merge
      semantics open
  - Closed `B6.9b2b2b2b2b2b2b2a` with unique path-level goal-directed context
    propagation from matching support frames:
    - when the root tuple did not match but exactly one support frame in the
      chosen proof path carried bounded `goal-directed-read-context`, the
      proof path now inherits that same context
    - kept the broader multi-frame merge case open instead of guessing merge
      semantics
  - Closed `B6.9b2b2b2b2b2b2b1` with bounded proof-path goal-directed context
    for matching fact support frames:
    - added a regression proving `deduce/why-result` now also attaches
      path-local `goal-directed-read-context` to matching fact support frames
      inside a derived proof path
    - split the remaining provenance work to broader proof-path integration
      beyond the current root/fact-frame/rule-step row-matching surface
  - Closed `B6.11b2` with explicit runtime/admin truth for the parallel lane:
    - `deduce/analyze`, relation-local `deduce/stats`, and selector-scoped
      `deduce/explain` now expose `parallel-runtime-mode`
    - the shipped value is `metadata-only`, which makes the public boundary
      explicit instead of forcing readers to infer it from
      `execution-engine = 'semi-naive-scc`
  - Closed `B6.11b1b2b` with broader worker-scratch recursive closure for
    positive multi-atom SCC rules:
    - non-anchor recursive atoms in worker-scratch closure now read from
      `LMDB + prior-iteration worker-visible additions`
    - same-iteration worker-emitted tuples stay invisible until the
      iteration closes, preserving the seminaive boundary
    - added a direct C-level Deduce regression proving transitive
      multi-atom worker-scratch closure emits the full serialized fixpoint
      payload
  - Closed `B6.9b2b2b2b2b1` by turning shipped selector-scoped `scan`
    parity into explicit regression/docs truth:
    - selector-scoped `deduce/scan` was already using the bounded complete
      row-set path-local `goal-directed-read-context` capture path
    - added a regression and updated docs so the bounded-complete provenance
      slice no longer incorrectly claims `scan` is plain-only
  - Closed `B6.9b2b2b2b2a` with the first bounded multi-row
    proof-path-integrated goal-directed provenance slice:
    - replaced the old exact-one-only remembered subject key with a bounded
      complete row-set capture for the last goal-directed `query` / `match` /
      `scan-range` / plain `scan`
    - `deduce/why-result` path-local `goal-directed-read-context` now
      attaches when the traced row belongs to that complete captured set,
      while overflow beyond the current `8`-row limit keeps only the
      top-level relation-read context
    - added Deduce regressions for bounded-complete multi-row attachment and
      overflow fallback
  - Added `docs/plans/deduce-parallel-recursive-first-runtime-shape-decision-2026-03-25.md`:
    - chose the first runtime widening as same-wave worker-scratch compute
      for positive non-aggregate recursive SCC batches
    - kept publish and integrity validation on the main thread
    - rejected direct worker-owned LMDB publish as a misleading first slice
  - Updated `TODO.md` and `memory/CHANGELOG.md`:
    - tightened `B6.11b1` to the chosen first runtime shape
  - Closed `B6.11b1a` with the first internal handoff seam for that runtime
    shape:
    - added a versioned serialized component-delta payload for one SCC
      component's signed deltas
    - the payload fits the existing scheduler `OFFLOAD_RES_BYTES` result path
      and carries component id, predicate identity, and opaque encoded tuple
      additions/removals
    - added a direct C-level Deduce regression for payload roundtrip
    - split the remaining runtime work into worker-scratch compute
      (`B6.11b1b`) and main-thread publish/apply (`B6.11b1c`)
  - Closed `B6.11b1b1` with the first actual worker-scratch recursive compute
    slice:
    - added a read-only seminaive scratch-pass helper for positive
      non-aggregate recursive components
    - the helper seeds from the current component snapshot, evaluates one
      pass without LMDB publish, and returns serialized component deltas
    - added a direct C-level Deduce regression proving a recursive rule can
      emit current-snapshot-derived tuples into that payload
    - split the remaining worker-compute lane into multi-iteration scratch
      closure (`B6.11b1b2`) and main-thread publish/apply (`B6.11b1c`)
  - Closed `B6.11b1b2a` with the first multi-iteration worker-scratch
    recursive closure slice:
    - iterated the scratch seminaive path to a fixpoint for the current
      narrow single-recursive-atom positive SCC shape
    - added worker-local visible tuple suppression across iterations so the
      closure can accumulate serialized additions without LMDB publish
    - added a direct C-level Deduce regression proving the scratch closure
      computes a transitive `path` payload containing `(1,2)`, `(2,3)`,
      and `(1,3)`
    - split the remaining worker-closure lane into broader multi-atom
      recursive shapes (`B6.11b1b2b`) and main-thread publish/apply
      (`B6.11b1c`)
  - Closed `B6.11b1c` with the main-thread publish/apply path for
    worker-computed recursive deltas:
    - added `deduce_apply_serialized_component_delta_payload(...)`, which
      validates payload routing, reopens target relations, runs the existing
      encoded-tuple integrity checks, and publishes additions/removals through
      LMDB on the main thread
    - added a direct C-level Deduce regression proving a worker-computed
      recursive closure payload can be deserialized, committed, and observed
      as durable relation rows with updated schema estimates
    - narrowed the remaining parallel-runtime backlog to broader
      worker-scratch closure (`B6.11b1b2b`) and runtime/admin truth
      (`B6.11b2`)
  - Closed `B6.9b2b2b2b2b2a` with the next truthful proof-path provenance
    slice:
    - plain no-op `deduce/query` and `deduce/scan-range` reads now preserve
      the bounded complete subject set for later `why-result` path matching
    - `deduce/why-result` now also attaches path-local
      `goal-directed-read-context` on matching derived support frames, not
      only the root proof path, when the frame’s `(relation, tuple)` pair
      matches that bounded complete last-read subject set
    - added regressions for direct no-op `query` path attachment and derived
      support-frame attachment after a plain no-op `scan-range`
  - Closed `B6.9b2b2b2b2b2b1` with plain no-op `match` parity for bounded
    root-path goal-directed context:
    - switched the plain non-ephemeral `deduce/match` path to the txn-based
      scan helper for bounded subject-key recording, matching the already
      shipped goal-directed row-key capture path
    - added a regression proving `why-result` attaches
      `goal-directed-read-context` on the matching root proof path after a
      plain no-op `deduce/match` read
  - Closed `B6.9b2b2b2b2b2b2a` with truthful selector-scoped path-local
    parity across the shipped row-read shapes:
    - replaced the earlier misleading negative assumption with a mixed-shape
      regression
    - selector-scoped `match` and `scan` now stay truthfully visible on
      path-local `no-op` context
    - selector-scoped `query` and `scan-range` now stay truthfully visible on
      their shipped ephemeral demand paths with the same bounded path-local
      context surface
  - Pruned the non-executable roadmap markers out of `TODO.md`:
    - removed the broad `F0` through `V2-5` milestone labels from the live
      queue
    - kept only concrete actionable Deduce/runtime items in the active TODO
- Validation run
  - doc/queue shaping only
  - architecture basis confirmed from the current tree:
    - recursive fixpoint still walks `component_order` sequentially
    - component evaluators mutate LMDB transactions and shared schema/admin
      state
    - scheduler fanout helpers exist only as a future execution seam

- Objectives attempted
  - Close the parallel-recursion admin-truth slice by pinning that the
    current parallel fields are topology metadata only, not a separate
    runtime execution mode.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_rule_groups_explain.c3`:
    - strengthened the existing parallel explain regression so it now also
      proves selector-scoped `deduce/explain` still reports
      `execution-engine = 'semi-naive-scc` and
      `goal-directed-execution-path = 'selected-component-closure`
      while carrying parallel batch metadata
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.11a2`
    - documented the current fallback/admin truth for parallel topology
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - selector-scoped `deduce/explain` on the representative recursive SCC
      still reports `semi-naive-scc` plus `selected-component-closure`
      while exposing the expected parallel batch metadata

- Objectives attempted
  - Close the first parallel-recursion contract slice by documenting and
    pinning the existing SCC batch scheduling rule instead of leaving the
    topology metadata underspecified.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - strengthened the existing parallel topology regression to pin the
      current `stratum` / `wave` / `batch-size` contract on both
      `deduce/analyze` and relation-local `deduce/stats`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.11a1`
    - documented that the current parallel-recursion surface is
      metadata-only, recursive-components-only, and scheduled by
      same-stratum wave groups
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - representative recursive SCC topology still reports first-batch
      `(stratum = 0, wave = 1, batch-size = 2)` and second-batch
      `(stratum = 0, wave = 2, batch-size = 1)`

- Objectives attempted
  - Close the remaining `check`-specific admin-truth slice by exposing
    dedicated per-class violation counters instead of only generic totals and
    history.
- Code/config changes made
  - Updated `src/lisp/deduce_db_handles.c3`,
    `src/lisp/deduce_db_handles_register.c3`,
    `src/lisp/deduce_db_handles_mutation_tracking.c3`,
    `src/lisp/deduce_schema_query.c3`, and
    `src/lisp/deduce_rule_eval_prims.c3`:
    - added dedicated `check_integrity_violation_count` tracking on relation
      and DB admin state
    - `deduce/stats` and `deduce/analyze` now expose
      `check-integrity-violation-count`
  - Updated `src/lisp/tests_deduce_query_groups.c3`, `TODO.md`,
    `docs/deduce-datalog-spec.md`, `docs/reference/08-libraries.md`, and
    `memory/CHANGELOG.md`:
    - pinned the new per-class admin counters
    - closed `B6.10b2`, completing the current `check` lane
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probes:
    - existing key/unique/reference admin counters stayed unchanged
    - `check` failures now surface `check-integrity-violation-count = 1`
      on both relation-local and DB-wide admin payloads

- Objectives attempted
  - Close the first truthful write-enforcement slice for canonical `check`
    constraints and narrow the residual backlog honestly.
- Code/config changes made
  - Updated `src/lisp/deduce_relation_ops_validation.c3`,
    `src/lisp/deduce_relation_ops_mutations.c3`,
    `src/lisp/deduce_db_handles.c3`,
    `src/lisp/deduce_db_handles_mutation_tracking.c3`,
    `src/lisp/deduce_schema_query.c3`, and
    `src/lisp/tests_deduce_groups.c3` /
    `src/lisp/tests_deduce_query_groups.c3`:
    - immediate `fact!`, derived rule-head publish, and deferred
      `write-deferred` commit-time validation now enforce declared unary
      `check` constraints
    - failed checks now raise deterministic machine-checkable payloads,
      and generic integrity history/admin surfaces now record
      `violation-class = 'check`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.10b1`
    - narrowed `B6.10b2` to dedicated admin counters and summary truth for
      `check` constraints
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probes:
    - direct `fact!` check rejection returned machine-checkable
      `deduce/integrity-check-failed`
    - deferred `write-deferred` commit rejected the bad tuple and rolled
      back cleanly
    - missing check predicates rejected with
      `deduce/check-predicate-missing`
    - relation/DB history surfaced `violation-class = 'check`
    - direct `deduce/analyze` probe confirmed derived check enforcement on
      rule-head publish

- Objectives attempted
  - Close the first truthful schema/admin slice for canonical `check`
    constraints instead of leaving that lane at naming-only status.
- Code/config changes made
  - Updated `src/lisp/parser_define_relation_attr.c3`,
    `src/lisp/parser_define_relation_attr_helpers.c3`,
    `src/lisp/deduce_db_handles.c3`,
    `src/lisp/deduce_db_handles_register.c3`,
    `src/lisp/deduce_schema_query.c3`, and
    `src/lisp/deduce_rule_eval_prims.c3`:
    - relation declarations now lower unary column checks in the form
      `(check predicate column)`
    - relation schemas/admin payloads now persist and expose declared
      `check` metadata
    - `deduce/analyze` now reports DB-wide `check-constraint-count`
  - Updated `src/lisp/tests_deduce_groups.c3` and
    `src/lisp/tests_deduce_query_groups.c3`:
    - added parser/storage coverage for `(check predicate column)`
    - added public schema/analyze regression coverage for the `check`
      payload shape
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.10a2`
    - documented the shipped boundary as declaration/schema/admin baseline
      only, with enforcement still deferred to `B6.10b1`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - declared `check` constraints appear in `deduce/schema`
    - DB-wide `deduce/analyze` reports `check-constraint-count`

- Objectives attempted
  - Retire the last transformed recursive-demand umbrella item once the lane
    was fully covered by explicit shipped support/fallback slices.
- Code/config changes made
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.4f5c2b2b2b` as stale backlog drift
    - rewrote the transformed recursive query contract to say there is no
      separate standing transformed residual beyond the shipped support and
      fallback families already pinned
- Validation run
  - backlog/doc shaping only, based on the already-landed transformed SCC
    support probes plus permutation and transformed multi-atom fallback probes

- Objectives attempted
  - Close the next transformed recursive query-demand hardening slice by
    fixing the false-positive ephemeral path on transformed multi-atom
    recursive shapes.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3` and
    `src/lisp/deduce_schema_query.c3`:
    - detect head rules whose transformed recursive support still spans
      multiple recursive body atoms
    - force truthful fallback for those transformed multi-atom shapes
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for transformed recursive multi-atom
      pair and pair-disjunction demands
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2b2b2`
    - closed `B6.4f5c2b2b2a`
    - kept the broader transformed residual as `B6.4f5c2b2b2b`
- Validation run
  - host-side `./build/main --eval` probes:
    - transformed recursive multi-atom pair query moved to
      `selected-component-closure` with the correct derived row
    - transformed recursive multi-atom pair disjunction moved to
      `selected-component-closure` with the correct derived rows

- Objectives attempted
  - Close the multi-hop follow-up for the transformed recursive
    one-carried-position query-demand lane.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for transformed three-cycle recursive
      pair and pair-disjunction demands
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2b2b`
    - closed `B6.4f5c2b2b1`
    - kept the broader transformed residual as `B6.4f5c2b2b2`
- Validation run
  - host-side `./build/main --eval` probes:
    - transformed three-cycle pair query stayed on
      `ephemeral-head-demand-query` with requested `(0 1)` and applied `(0)`
    - transformed three-cycle pair disjunction stayed on
      `ephemeral-head-demand-query` with requested `(0 1)` and applied `(0)`

- Objectives attempted
  - Close the disjunctive follow-up for the transformed one-carried-position
    recursive query-demand slice.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for reordered mutual-recursive
      disjunctive pair demands
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2b2`
    - closed `B6.4f5c2b2a`
    - kept the broader transformed residual as `B6.4f5c2b2b`
- Validation run
  - host-side `./build/main --eval` probes:
    - reordered mutual-recursive disjunction stayed on
      `ephemeral-head-demand-query` with requested `(0 1)` and applied `(0)`
    - pure recursive permutation disjunction stayed on
      `selected-component-closure`

- Objectives attempted
  - Close the first transformed recursive query-demand slice without breaking
    the existing pure permutation fallback boundary.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3` and
    `src/lisp/deduce_schema_query.c3`:
    - added same-index recursive carrier detection for queried head
      positions across an SCC
    - widened recursive query-demand relaxation so transformed recursive SCC
      shapes can keep one carried applied position when that carrier exists
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for reordered mutual-recursive pair
      demands, asserting requested `(0 1)` with one applied carried position
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2b`
    - closed `B6.4f5c2b1`
    - kept the broader transformed residual as `B6.4f5c2b2`
- Validation run
  - host-side `./build/main --eval` probes:
    - reordered mutual-recursive query moved to
      `ephemeral-head-demand-query` with requested `(0 1)` and applied `(0)`
    - pure recursive permutation still stayed on `selected-component-closure`

- Objectives attempted
  - Close the next recursive query-demand slice by pinning multi-hop
    same-index SCC support instead of leaving deeper positive cycles implicit.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for fully-bound same-index
      three-relation recursive SCC queries
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2`
    - closed `B6.4f5c2a`
    - kept the remaining transformed-demand residual as `B6.4f5c2b`
- Validation run
  - host-side `./build/main --eval` probe:
    - three-relation same-index recursive SCC query stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`

- Objectives attempted
  - Close the next recursive query-demand slice by pinning same-index
    mutual-recursive disjunctive support instead of leaving it buried under
    a broader residual item.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for same-index mutual-recursive
      disjunctive pair queries
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c`
    - closed `B6.4f5c1`
    - kept the broader residual as `B6.4f5c2`
- Validation run
  - host-side `./build/main --eval` probes:
    - selector mutual-recursive disjunction stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`
    - plain mutual-recursive disjunction stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`

- Objectives attempted
  - Close the next recursive query-demand slice by widening truthful
    multi-position support from same-index self recursion to same-index
    mutual-recursive SCCs.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3`:
    - widened the same-index recursive demand gate to accept SCC-local
      positive recursive body atoms that preserve all requested positions
      together at the same indices
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for fully-bound same-index
      mutual-recursive pair queries
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.4f5b`
    - promoted the remaining residual to `B6.4f5c`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probes:
    - selector mutual-recursive pair query stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`
    - plain mutual-recursive pair query stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`

- Objectives attempted
  - Close the first goal-directed provenance slice without claiming
    proof-path-integrated planner semantics.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - `deduce/why-result` now exposes optional top-level
      `goal-directed-read-context` metadata built from the relation's existing
      last goal-directed read state
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a regression that forces a goal-directed recursive query, then
      checks that `why-result` exposes the mirrored read context even on a
      `missing` row payload
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2` into `B6.9b2a` and `B6.9b2b`
    - closed `B6.9b2a`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(missing 0 ephemeral-head-demand-query query nil 1 1 0)` for
      the goal-directed read-context slice
    - existing recursive and non-recursive why-result probes stayed intact

- Objectives attempted
  - Close the next recursive provenance slice by moving positive recursive
    why-result payloads beyond flattened fact-only support.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - recursive why-result support search now appends a `rule-step` frame when
      a derived child row is used in the chosen proof path
    - recursive why-result depth accounting now reports the deeper derived step
      in `max-depth`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - tightened the recursive why-result regression to pin `rule-step` support
      frames and `max-depth = 3` on recursive closure rows
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9b1b`
    - narrowed the remaining provenance queue to goal-directed semantics only
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probes:
    - recursive closure payload now includes a trailing `rule-step` frame with
      `(rule-index 0, step-index 1)` on the chosen child proof
    - recursive closure rows now report `max-depth = 3`
    - the existing non-recursive multi-rule why-result probe still returned
      `(ok partial true)`

- Objectives attempted
  - Close the first recursive provenance slice without widening to
    goal-directed lineage or nested recursive-step payloads.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened `deduce/why-result` support search so derived child rows recurse
      back through the shipped row-subject provenance helper surface
    - added a visited-subject guard on `(predicate, tuple)` to keep recursive
      support search finite
    - recursive closure rows now return `ok` for one support chain and
      `partial` for multiple support chains, while keeping the current
      flattened fact-support payload shape
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a dedicated recursive why-result regression for direct, unique
      recursive, and partial recursive closure rows
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split broad recursive provenance into `B6.9b1a` and `B6.9b1b`
    - closed `B6.9b1a` at the first positive recursive closure slice
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probes:
    - returned `(ok partial true)` for the existing non-recursive multi-rule
      why-result probe after the recursive helper refactor
    - returned `(ok 1 nil 1 1 2 nil)` for a unique recursive closure row
    - returned `(partial 1 true 1 1 2 nil)` for a multi-path recursive closure
      row

- Objectives attempted
  - Close the first multi-rule non-recursive provenance slice without widening
    to recursive or goal-directed lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened `deduce/why-result` to aggregate already-shipped non-recursive
      provenance support across multiple matching head rules
    - multi-rule rows with one supported path now return `ok`, and rows with
      multiple supported paths return `partial` with the first deterministic
      path
    - one-rule exact-one-rule lineage still stays on the older proven branch
      instead of going through the aggregate path
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to cover both the direct multi-rule
      `ok` case and the mixed-body multi-rule `partial` case
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a8`
    - narrowed the remaining provenance queue to recursive and goal-directed
      semantics only
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probes:
    - returned `(ok 1 nil 1 1 1 nil)` for the direct multi-rule row
    - returned `(partial 1 true 1 2 2 nil)` for the mixed-body multi-rule row

- Objectives attempted
  - Close the first mixed-body non-recursive provenance slice without widening
    to multi-rule lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened `deduce/why-result` to compose exact-one-rule non-recursive
      mixed-body lineage through already-supported exact-one-rule child
      provenance helpers
    - exact-one-rule mixed-body lineage now returns `ok` for one support chain
      and `partial` when multiple support chains exist
    - multi-rule non-recursive lineage still returns
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to include an exact-one-rule mixed-body
      supported case while keeping the multi-rule non-recursive case
      unsupported
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a7`
    - promoted the remaining multi-rule non-recursive lineage work into
      `B6.9a8`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(partial true partial 2 error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the first search-based non-recursive provenance slice for exact-one-
    rule extensional lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - added exact-one-rule extensional support search for `deduce/why-result`
      on non-recursive derived rows
    - reused the existing positive-body tuple matcher plus tuple iterators to
      search extensional supports rather than introducing a separate provenance
      matcher
    - exact-one-rule extensional lineage now returns `ok` for one deterministic
      path and `partial` with `truncated = true` when multiple support paths
      exist
    - mixed-body or multi-rule non-recursive derived lineage still returns
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to cover search-based extensional
      lineage with both `ok` and `partial` outcomes
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a6`
    - promoted the remaining mixed-body or multi-rule non-recursive lineage
      work into `B6.9a7`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(ok partial true error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the next non-recursive provenance slice for exact-one-rule
    extensional multi-body lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - generalized the public derived-lineage matcher from one-body only to
      exact-one-rule extensional derivations where every support tuple is
      reconstructible from the head row
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
  - Close the first bounded `B6.4f2` recursive symbolic compatibility slice
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
      - `B6.4f` compatibility and safety rules for wider recursive shapes
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
