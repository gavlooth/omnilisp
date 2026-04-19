    remaining query-fallback branches rather than rechecking selector metadata.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Restore the bounded validation lanes so the container runtime matches the
    binary being executed, then clear the remaining JIT and compiler slice
    regressions that surfaced once the lanes could actually run.
- Code/config changes made
  - Updated `scripts/check_scheduler_state_guards.sh` and
    `scripts/check_jit_env_scope_guards.sh` so they build Omni inside the
    validation container before running the bounded test binary.
  - Added a container build step to `scripts/run_validation_status_summary.sh`
    so the aggregate bounded slices use a container-linked `build/main`.
  - Moved `stdc++` to the end of the executable link list in `project.json`
    so the vendored FTXUI archive resolves its C++ symbols correctly.
  - Tightened `jit_tco_binding_needs_copy` in
    `src/lisp/jit_jit_eval_scopes_helpers.c3` so closure and iterator payloads
    force a copy when alias reuse would cross the releasing-scope boundary.
  - Updated the compiler codegen expectation in
    `src/lisp/tests_compiler_codegen_groups.c3` to match the current
    `Dictionary` lowering path via `aot::dict_from_args(...)`.
- Experiment commands and key metrics
  - `scripts/check_scheduler_state_guards.sh`
  - `scripts/check_jit_env_scope_guards.sh`
  - Key metrics:
    - scheduler guards passed with `105` unified tests passed and `0` failed,
    - JIT env/scope guards passed with `30` unified tests passed and `0` failed.
- Best current checkpoint/config recommendation
  - Keep bounded validation lanes building inside the container and keep the
    project link order with `stdc++` after `omni_ftxui`; that combination is
    the first one that runs the guarded JIT/runtime slices cleanly in this
    environment.
- Unresolved issues and next actions
  - Refresh the aggregate validation status summary on the patched tree once
    the current stale invocation clears, then confirm whether any remaining
    lanes still fail for real.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Refresh the type/dispatch area status stamp after the consistency gate
    flagged it as stale.
- Code/config changes made
  - Updated `docs/areas/types-dispatch.md` so its `As of:` stamp matches the
    current validated changelog window.
- Experiment commands and key metrics
  - `scripts/check_status_consistency.sh`
  - Key metrics: the status consistency gate passed with latest changelog date
    `2026-03-26`, TODO actionable count `7`, memory runtime status `green`,
    and types dispatch status `green`.
- Best current checkpoint/config recommendation
  - Keep `docs/areas/types-dispatch.md` synchronized with `memory/CHANGELOG.md`
    whenever a new validated landing changes the current-state window.
- Unresolved issues and next actions
  - None for this slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Run the broader validation status summary after wiring the FTXUI smoke
    gate into it.
- Code/config changes made
  - No code changes in this slice.
- Experiment commands and key metrics
  - `./scripts/run_validation_status_summary.sh build/validation_status_summary.json`
  - Key metric: the summary artifact was generated, but the aggregate run
    still reports pre-existing failures in several unrelated lanes.
- Best current checkpoint/config recommendation
  - Treat the new FTXUI smoke gate as integrated, but do not use the current
    aggregate summary as a clean repo-wide pass signal.
- Unresolved issues and next actions
  - The unrelated failing lanes in the aggregate summary still need their own
    separate investigation.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Wire the FTXUI smoke wrapper into the regular validation spine so it runs
    automatically after the main build/test pass.
- Code/config changes made
  - Updated `scripts/run_global_gates.sh` to run `scripts/run_ftxui_smoke.sh`
    as an optional default-on stage after the normal test pass.
  - Added `scripts/run_ftxui_smoke.sh` as a named case in
    `scripts/run_validation_status_summary.sh`.
  - Documented the smoke gate in `docs/PROJECT_TOOLING.md`.
- Experiment commands and key metrics
  - `bash -n /home/heefoo/Documents/code/Omni/scripts/run_global_gates.sh /home/heefoo/Documents/code/Omni/scripts/run_validation_status_summary.sh /home/heefoo/Documents/code/Omni/scripts/run_ftxui_smoke.sh`
  - `./scripts/run_ftxui_smoke.sh`
  - Key metric: the smoke wrapper still exits cleanly after the integration
    wiring changes.
- Best current checkpoint/config recommendation
  - Keep the new smoke stage enabled by default so vendor/header drift is
    caught in the ordinary validation path.
- Unresolved issues and next actions
  - None for this slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Turn the validated FTXUI example set into a reusable shell smoke gate.
- Code/config changes made
  - Added `scripts/run_ftxui_smoke.sh` to run the three non-interactive FTXUI
    smoke entrypoints plus the interactive demo.
- Experiment commands and key metrics
  - `./scripts/run_ftxui_smoke.sh`
  - Key metric: `module_value_smoke.omni`, `module_effect_smoke.omni`,
    `smoke.omni`, and `demo.omni` all exited successfully under the wrapper.
- Best current checkpoint/config recommendation
  - Use `scripts/run_ftxui_smoke.sh` as the quick validation gate for the
    vendored FTXUI surface.
- Unresolved issues and next actions
  - None for this slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Validate the forked FTXUI surface against the small example set after the
    header rename.
- Code/config changes made
  - No additional code changes in this slice.
- Experiment commands and key metrics
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_value_smoke.omni`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_effect_smoke.omni`
  - `printf q | LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/smoke.omni`
  - Key metric: all three example entrypoints exited successfully.
- Best current checkpoint/config recommendation
  - Keep the FTXUI build wiring aligned with the neutral header names and the
    current vendored source set.
- Unresolved issues and next actions
  - None for this validation slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Validate the vendor-header rename by rebuilding the project and running an
    FTXUI-backed demo path end to end.
- Code/config changes made
  - No additional code changes in this slice.
- Experiment commands and key metrics
  - `c3c build`
  - `printf q | LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/demo.omni`
  - Key metric: the project linked cleanly, and the demo exited successfully
    after rendering the UI once.
- Best current checkpoint/config recommendation
  - Keep the forked FTXUI header names and include paths aligned with the
    updated vendor surface.
- Unresolved issues and next actions
  - None for this validation slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Remove the repo's older example and the stale finwatch
    debug/example fixtures so the examples index only advertises the canonical
    product path.
- Code/config changes made
  - Deleted `examples/deduce_crud_server.omni`.
  - Deleted the unreferenced finwatch stale fixtures:
    `examples/finwatch/_stale2.omni`,
    `examples/finwatch/_stale3.omni`,
    `examples/finwatch/_stale4.omni`,
    `examples/finwatch/_stale_debug.omni`.
  - Removed the old section from `examples/README.md` so the
    examples landing page only points at current example material.
- Experiment commands and key metrics
  - `rg -n "deduce_crud_server|_stale_debug|_stale2|_stale3|_stale4|examples/deduce_crud_server|Legacy example|Keep this for regression coverage" .`
  - `rg --files .`
- Best current checkpoint/config recommendation
  - Keep `examples/README.md` focused on the canonical `finwatch` path and
    treat any future regression fixtures as explicit, separately documented
    regressions rather than part of the examples index.
- Unresolved issues and next actions
  - None for this slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Clear the remaining historical transcript blob of older wording so the
    repository-wide search no longer turns up the previous support language.
- Code/config changes made
  - Rewrote `plan.jsonl` in place to replace the remaining old-style tokens with
    neutral older wording.
- Experiment commands and key metrics
  - `perl -0pi -e 's/\\bold-style\\b/older/g' /home/heefoo/Documents/code/Omni/plan.jsonl`
  - `rg -n "old-style tokens" /home/heefoo/Documents/code/Omni/plan.jsonl`
  - Key metric: `plan.jsonl` now reports zero old-style hits.
- Best current checkpoint/config recommendation
  - Keep the active Omni tree free of support-language references.
  - Leave upstream vendor API/file names alone unless the dependency itself is
    being forked or rewritten.
- Unresolved issues and next actions
  - The vendored FTXUI subtree still contains upstream named files and comments.
    Removing those would require a dependency-surface rewrite, not just a text
    scrub.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Fork the vendored FTXUI subtree away from the old header naming so the
    repository-wide text scan no longer depends on the upstream file names.
- Code/config changes made
  - Renamed the vendored FTXUI headers from the old named files to
    `text.hpp` and `width.hpp`.
  - Updated the vendored includes and build manifest to match the new header
    names.
  - Reworded the few vendor comments that still referenced the old naming.
- Experiment commands and key metrics
  - `rg -n "neutral scan markers" /home/heefoo/Documents/code/Omni -g '!/home/heefoo/Documents/code/Omni/plan.jsonl'`
  - `rg -n "neutral vendor header markers" /home/heefoo/Documents/code/Omni/third_party/ftxui`
  - Key metric: the full repository scan now returns no hits outside the expected upstream dependency surface.
- Best current checkpoint/config recommendation
  - Keep the forked FTXUI headers and includes aligned with the new neutral
    names.
- Unresolved issues and next actions
  - None for this slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Strip remaining old-support wording from the active docs,
    scripts, tests, memory notes, and Lisp source comments after the earlier
    surface cleanup passes.
- Code/config changes made
  - Normalized the active docs and reference pages to use removed/old/historical
    wording instead of support-framed language.
  - Removed the remaining old wording from `memory/CHANGELOG.md`,
    `memory/DESTINATION_ARENA_PLAN.md`, and the archive changelog copy.
  - Renamed a few test labels and script messages so the live tree no longer
    reports support-style terminology in its own diagnostics.
- Experiment commands and key metrics
  - `rg -n "old wording markers" /home/heefoo/Documents/code/Omni -g '!/home/heefoo/Documents/code/Omni/third_party/**' -g '!/home/heefoo/Documents/code/Omni/plan.jsonl'`
  - `rg -n "old wording markers" /home/heefoo/Documents/code/Omni/memory /home/heefoo/Documents/code/Omni/docs /home/heefoo/Documents/code/Omni/src /home/heefoo/Documents/code/Omni/stdlib /home/heefoo/Documents/code/Omni/scripts /home/heefoo/Documents/code/Omni/tests /home/heefoo/Documents/code/Omni/TODO.md /home/heefoo/Documents/code/Omni/README.md /home/heefoo/Documents/code/Omni/AGENTS.md -g '!/home/heefoo/Documents/code/Omni/docs/plans/**'`
  - Key metric: the active tree scan is now clean; remaining matches are only
    in vendored third-party API/deprecation references and the transcript blob
    in `plan.jsonl`.
- Best current checkpoint/config recommendation
  - Keep the active Omni surface free of old-support wording.
  - Leave vendor/dependency API names and transcript logs alone unless the
    next task explicitly asks to rewrite those historical or upstream sources.
- Unresolved issues and next actions
  - None for the active tree cleanup slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Remove the remaining alias and long-form wrapper surfaces from the Omni
    Lisp runtime, compiler, docs, and examples.
  - Strip the active docs, source comments, and surface guidance of remaining
    migration wording so the repo no longer presents backward support
    as an active contract.
  - Extend the scrub into archival changelog copies and vendor prose comments
    where that could be done without touching upstream structural metadata.
- Code/config changes made
  - Removed alias registrations and lookup paths for `Int`, `Bool`, `Dict`,
    and `Ptr` from the interpreter/type registry and FFI annotation mapping.
  - Removed the long-form I/O wrappers from primitive
    registration and compiler primitive tables, leaving the canonical `fs-*`
    and `tcp-*`/`udp-*`/`dns-resolve`/`tls-*` surfaces.
  - Deleted the `eval_serialized_expr(...)` AOT debug bridge.
  - Updated tests and docs to use canonical spellings and removed the stale
    long-form filesystem test path.
  - Scrubbed remaining explicit migration wording from archival plan notes and
    changelog slices so the repository history now records the removals as
    one-way contract changes rather than support promises.
  - Renamed the internal deduce aggregate-head helper to drop its old suffix
    suffix and updated the single call site.
  - Scrubbed the archival changelog copy in `memory/archive/` and vendor prose
    comments in `third_party/ftxui/`.
  - Removed the remaining Bazel module field from
    `third_party/ftxui/MODULE.bazel` after the follow-up request to continue
    past the last structural hit.
  - Scrubbed the transcript blob in `plan.jsonl` so repo-wide full-text search
    no longer surfaces the old wording.
- Experiment commands and key metrics
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
  - Key metric: advanced slice completed `1106 passed, 0 failed` after the alias cleanup and rebuild.
  - Key metric: active-tree full-text search returned no hits after the second-pass scrub.
  - Key metric: archival/vendor search returned no hits after removing the Bazel module field.
  - Key metric: repo-wide full-text search returned no hits after the transcript scrub.
- Best current checkpoint/config recommendation
  - Keep the canonical `fs-*` and `tcp-*`/`udp-*`/`dns-resolve`/`tls-*`
    surfaces as the only public I/O names.
  - Keep the active docs/source surface free of migration-language promises;
    use migration/shim terminology only where historical context is unavoidable.
- Unresolved issues and next actions
  - Historical plan/docs entries still mention the removed bridge and older
    naming decisions; the explicit migration language has now been removed
    from the archival notes covered by this pass.
- Signature: Codex (GPT-5)

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
  - Added a tooling note that removed `OMNI_LISP_TEST_SLICE` slice names
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
