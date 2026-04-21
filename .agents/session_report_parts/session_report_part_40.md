# Session Report Part 40

Source: `.agents/SESSION_REPORT.md`

## 2026-04-22 02:05 CEST - ML Validation Benchmark And Availability Gate

Objective attempted:
- Continue `ML-VK-090` by converting the remaining bounded-container and
  benchmark-fixture residual into an executable validation entrypoint, then
  close the Vulkan availability host-path residual with an operation-family
  availability regression.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
- `scripts/run_ml_validation_slice.sh`
- `docs/todo_parts/todo_part_14.md`
- `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`

Code or configuration changes made:
- Added opt-in `OMNI_ML_BENCH=1` benchmark summaries to the advanced
  collections ML/Tensor test group.
- The fixture emits `ml_inference_oracle` for repeated CPU `ml/linear`
  inference and `ml_training_step_oracle` for repeated CPU
  `ml/optimizer-step` training updates, with scope recycle and
  copy-to-parent reuse deltas on the training summary.
- Added `scripts/run_ml_validation_slice.sh`, a Docker-bound focused
  advanced collections ML validation script that asserts the benchmark
  summary iteration counts.
- Closed `ML-VK-090-003` in the TODO for the bounded validation and benchmark
  fixture.
- Expanded `ML-VK-090-004` into an operation-family availability-gate
  regression that checks both host sides: Vulkan-visible hosts tie Float32 ML
  capability bits to actual Float32 placement across linear, activation,
  reduction, normalization, attention, convolution/pooling, optimizer, and
  gradient clipping families and execute a device-resident `ml/linear` smoke,
  while Vulkan-unavailable hosts report false ML Vulkan capability bits and
  fail closed with `tensor/backend-unavailable` for `to-device 'vulkan`.
- Closed the parent `ML-VK-090`; future broader validation expansion should be
  tracked as a new item for the specific operation family or benchmark
  dimension it adds.

Commands run:
- `c3c build --obj-out obj`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_ML_BENCH=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ./build/main --test-suite lisp`
- `scripts/run_ml_validation_slice.sh`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(let (vulkan (ref (tensor-backends) 3)) ...)"`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ./build/main --test-suite lisp`
- `scripts/check_file_size_gate.sh`
- `git diff --check -- src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3 docs/todo_parts/todo_part_14.md docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md .agents/plan_parts/plan_part_05.md .agents/PLAN.md memory/changelog_parts/changelog_part_37.md .agents/session_report_parts/session_report_part_40.md .agents/SESSION_REPORT.md`

Key results:
- Build passed.
- Local focused advanced collections probe passed with `pass=1900 fail=0`,
  `ml_inference_oracle inference_ok=128`, and
  `ml_training_step_oracle train_ok=64`.
- Bounded focused validation passed with `pass=1884 fail=0`,
  `ml_inference_oracle inference_ok=128`, and
  `ml_training_step_oracle train_ok=64`.
- The direct expanded ML Vulkan availability-gate expression returned `true`
  on this Vulkan-visible host and in the bounded container where Vulkan is
  unavailable.
- The focused advanced collections slice with the committed availability-gate
  regression passed with `pass=1901 fail=0`.
- File-size gate passed with no tracked code files above 1000 LOC.
- Targeted diff whitespace check passed.

Invalidated assumptions or failed approaches worth preserving:
- The first bounded script run failed because the validation image's bundled
  `/usr/local/bin/c3c` produced `Exec format error` on this host. The script
  now performs the build before entering the container and runs only the
  focused test/bench workload inside the bounded validation container.
- The first benchmark expressions tried to embed detailed numeric assertions
  inside the timing loops and were malformed. Keep semantic assertions in the
  normal regression tests; benchmark loops should measure successful repeated
  public operations and assert iteration counts.
- The first unavailable-host version of the availability-gate test expected
  `tensor/backend-unsupported`; bounded validation showed the runtime's
  specific missing-backend diagnostic is `tensor/backend-unavailable`, which is
  now the asserted contract for unavailable Vulkan.

Unresolved issues:
- No residual remains under `ML-VK-090` for the current validation gate.

Next actions:
- Move to another open roadmap item with a concrete failing signal. Future ML
  validation additions should be filed as new operation-family or benchmark
  dimension items, not reopened under `ML-VK-090`.

Signature: GPT-5 Codex

## 2026-04-22 18:05 CEST - UI FTXUI Session Runtime

Objective attempted:
- Continue the audit/remediation queue by closing
  `UI-LIB-RUNTIME-SESSION-001`, the explicit session-owned UI
  read/update/render lifecycle residual after the one-shot blocking FTXUI loop.

Relevant workspace or target:
- `/home/christos/Omni`
- `csrc/ftxui_shim.h`
- `csrc/ftxui_shim.cpp`
- `csrc/ftxui_shim_runtime.inc`
- `src/lisp/ftxui_ffi.c3`
- `src/lisp/ftxui_ffi_constants.c3`
- `src/lisp/prim_ui_ftxui.c3`
- `examples/libraries/ftxui/`
- `docs/UI_REFERENCE.md`
- `docs/plans/ui-library-facade-plan-2026-03-27.md`
- `docs/todo_parts/todo_part_14.md`

Code or configuration changes made:
- Added `omni_ftxui_session_t` to the native shim as an owned FTXUI custom
  `Loop` wrapper with create/destroy, `run_once`, `run_once_blocking`, and
  `has_quitted` ABI functions.
- Added C3 externs and a `ui-ftxui-session` owned `ForeignHandle` wrapper with
  a finalizer that destroys the native session loop before retained
  components, heap state, screen, and context.
- Added hidden primitives and public wrappers for `open_session`,
  `update_session`, `render_session`, `read_event_session`,
  `invalidate_session`, `post_event_session`, and `close_session` across the
  canonical dotted modules, flat compatibility modules, and `ui` facade.
- Added `module_session_smoke.omni`; expanded direct/value FTXUI smokes and
  `scripts/run_ftxui_smoke.sh`.
- Updated the UI reference, UI facade plan, TODO, changelog, and active plan.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build --obj-out obj`
- `LD_LIBRARY_PATH=build:/usr/local/lib ./build/main examples/libraries/ftxui/module_session_smoke.omni`
- `LD_LIBRARY_PATH=build:/usr/local/lib ./build/main examples/libraries/ftxui/module_value_smoke.omni`
- `LD_LIBRARY_PATH=build:/usr/local/lib ./build/main examples/libraries/ftxui/module_direct_smoke.omni`
- `OMNI_FTXUI_RUNTIME_LD_LIBRARY_PATH=build:/usr/local/lib scripts/run_ftxui_smoke.sh`

Key results:
- Build passed after rebuilding `libomni_ftxui.a` with the new shim symbols.
- The new session smoke passed and verifies handle metadata, update,
  invalidate, post-event, non-blocking render, idempotent close, and
  closed-handle rejection. The event-read placeholder from this lifecycle slice
  was superseded by the later event-payload entry below.
- The full FTXUI smoke script passed.
- `UI-LIB-RUNTIME-SESSION-001` is closed for session-owned lifecycle control.

Invalidated assumptions or failed approaches worth preserving:
- Do not treat FTXUI `RunOnceBlocking` as an Omni event value. It can step the
  loop, but raw event reads need an explicit ABI capture/read contract.

Unresolved issues:
- None for this session lifecycle slice after the later event-payload entry
  below closed `UI-LIB-RUNTIME-SESSION-EVENT-PAYLOAD-001`.

Next actions:
- Continue one of the remaining explicit open items:
  `ML-VK-040-FUSED-ATTENTION-001`, `ML-VK-050-VK-BWD-001`,
  or `ML-VK-060-FUSED-CUDA-001`.

Signature: GPT-5 Codex

## 2026-04-22 05:54 CEST - UI FTXUI Session Event Payload Reads

Objective attempted:
- Close `UI-LIB-RUNTIME-SESSION-EVENT-PAYLOAD-001` by making
  `ui.read_event_session` return real session-owned event payload values rather
  than a fail-closed placeholder.

Relevant workspace or target:
- `/home/christos/Omni`
- `csrc/ftxui_shim.h`
- `csrc/ftxui_shim.cpp`
- `csrc/ftxui_shim_runtime.inc`
- `src/lisp/ftxui_ffi.c3`
- `src/lisp/ftxui_ffi_types.c3`
- `src/lisp/prim_ui_ftxui.c3`
- `examples/libraries/ftxui/module_session_smoke.omni`

Code or configuration changes made:
- Added a native `omni_ftxui_event_read_result` ABI struct and
  `omni_ftxui_session_take_event` drain function.
- Wrapped session roots with `ftxui::CatchEvent` so loop steps record the last
  observed event payload without consuming downstream component handling.
- Converted captured events into Omni dictionaries with `kind` and optional
  `text` fields for character events, known special keys, custom events, and
  fallback special-key input text.
- Updated the session smoke to post a character event and assert
  `ui.read_event_session` returns `{'kind 'character 'text "x"}`.
- Updated the UI reference, UI facade plan, TODO, changelog, and active plan to
  close the residual item and preserve the one-shot `read_event_tree`
  fail-closed boundary.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build --obj-out obj`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_session_smoke.omni`
- `LD_LIBRARY_PATH=build:/usr/local/lib ./build/main examples/libraries/ftxui/module_session_smoke.omni`
- `OMNI_FTXUI_RUNTIME_LD_LIBRARY_PATH=build:/usr/local/lib scripts/run_ftxui_smoke.sh`
- `scripts/check_build_config_parity.sh`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Native helper and C3 builds passed.
- The direct session smoke passed with a posted character event and a returned
  event dictionary.
- The full FTXUI smoke script, build-config parity check, file-size gate, and
  whitespace diff check passed.
- `UI-LIB-RUNTIME-SESSION-EVENT-PAYLOAD-001` is closed.

Invalidated assumptions or failed approaches worth preserving:
- Do not keep assuming session event payload reads are impossible because
  FTXUI `Loop` has no direct event return value. Treat `RunOnceBlocking` as a
  loop-step primitive only; the valid bridge is an explicit `CatchEvent`
  capture/read ABI around the session root.

Unresolved issues:
- FTXUI `Event::Custom` is payload-free, so custom session events round-trip as
  `{'kind 'custom}` rather than preserving caller-supplied text.

Next actions:
- Continue one of the remaining explicit open items:
  `ML-VK-040-FUSED-ATTENTION-001`, `ML-VK-050-VK-BWD-001`, or
  `ML-VK-060-FUSED-CUDA-001`.

Signature: GPT-5 Codex

## 2026-04-22 02:30 CEST - ML-VK-020 Leaky ReLU Closure

Objective attempted:
- Continue the broad audit/repair pass by closing the remaining concrete
  `ML-VK-020` mismatch: the scoped activation list still named
  `leaky-relu`, but the runtime only exposed `ml/relu`, `ml/sigmoid`,
  `ml/tanh`, and `ml/gelu`.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_ml_activation.c3`
- `src/lisp/prim_tensor_backend_ops.c3`
- `src/lisp/eval_init_primitive_tables.c3`
- `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
- `docs/todo_parts/todo_part_14.md`
- `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- `docs/LANGUAGE_SPEC.part-01b.md`
- `docs/reference/11-appendix-primitives.md`
- `memory/changelog_parts/changelog_part_37.md`
- `.agents/plan_parts/plan_part_05.md`

Code or configuration changes made:
- Added canonical `ml/leaky-relu(input [negative-slope])` with default slope
  `0.01`.
- Implemented the operation as `max(input, 0) + slope * min(input, 0)` over
  existing Tensor map primitives so Float64/Float32 CPU/CUDA/Vulkan placement
  follows the existing Tensor map contract without hidden transfers.
- Added non-negative finite slope validation and Float32 representability
  validation for Float32 tensor execution.
- Registered `ml/leaky-relu` for interpreter and AOT primitive lookup.
- Added `ml-neural-leaky-relu-float64` and
  `ml-neural-leaky-relu-float32` capability keys to `tensor-backends`.
- Added CPU, Vulkan, invalid-slope, unsupported-dtype, and capability
  regressions.
- Updated the language spec, primitive reference, roadmap, TODO, changelog, and
  active agent plan to mark `ML-VK-020` closed for the current scoped contract.

Commands run:
- `c3c build --obj-out obj`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_file_size_gate.sh`
- `git diff --check`
- `scripts/run_ml_validation_slice.sh`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_file_size_gate.sh`
- `git diff --check -- src/lisp/prim_ml_activation.c3 src/lisp/prim_tensor_backend_ops.c3 src/lisp/eval_init_primitive_tables.c3 src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3 docs/LANGUAGE_SPEC.part-01b.md docs/reference/11-appendix-primitives.md docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md docs/todo_parts/todo_part_14.md`

Key results:
- Build passed.
- Focused advanced collections passed with `pass=1908 fail=0`.
- Bounded ML validation passed with `pass=1891 fail=0`,
  `inference_ok=128`, and `train_ok=64`.
- Compiler slice passed with `pass=290 fail=0`.
- Primitive docs parity, file-size gate, and targeted diff whitespace checks
  passed.

Invalidated assumptions or failed approaches worth preserving:
- None new. The implementation intentionally reuses existing Tensor map
  placement semantics; it does not add a hidden Vulkan-specific fallback path.

Unresolved issues:
- Broad `ml-neural-map` remains false. Future activation expansion should land
  as a new operation-specific item with its own capability/test boundary rather
  than reopening `ML-VK-020`.

Next actions:
- Continue with `ML-VK-050` autograd expansion or the remaining `ML-VK-080`
  graph/runtime work. Side-audit results also identified FFI async return and
  context-lifetime issues as high-signal future repair targets.

Signature: GPT-5 Codex

## 2026-04-22 01:32 CEST - Runtime Ownership Fallback Closure Guard

Objective attempted:
- Close `FALLBACK-RUNTIME-OWNERSHIP-001` once the remaining runtime ownership
  fallback cleanup surfaces were guard-backed rather than left as a broad open
  umbrella.

Relevant workspace or target:
- `/home/christos/Omni`
- `scripts/check_boundary_facade_usage.sh`
- `scripts/boundary_facade_policy.txt`
- `src/lisp/value_tensor_expr_edges.c3`
- `docs/plans/fallback-inventory.md`
- `docs/todo_parts/todo_part_14.md`

Code or configuration changes made:
- Extended `scripts/check_boundary_facade_usage.sh` to reject production
  `copy_to_parent_by_route(...)`, `boundary_copy_to_parent_site_ctx(...)`, and
  uncached `boundary_ptr_in_scope_chain_with_hint(...)` call sites outside
  sanctioned boundary implementation files.
- Added narrow policy allow entries for the internal implementation files that
  still own these low-level helpers.
- Replaced a direct `promote_to_escape(...)` call in tensor expression-edge
  escape promotion with `boundary_promote_to_escape(...)`.
- Marked `FALLBACK-RUNTIME-OWNERSHIP-001` closed in the TODO and fallback
  inventory. The invalidated global scope-chain scan deletion remains rejected.

Commands run:
- `bash scripts/check_boundary_facade_usage.sh`
- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh bash -lc 'LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'`
- `git diff --check -- src/lisp/value_tensor_expr_edges.c3 scripts/check_boundary_facade_usage.sh scripts/boundary_facade_policy.txt src/lisp/eval_boundary_scope_chain.c3 src/lisp/eval_boundary_provenance.c3 src/lisp/eval_boundary_commit_escape_wrappers.c3 src/lisp/tests_memory_lifetime_boundary_state_groups.c3 src/lisp/tests_memory_lifetime_smoke_suite_groups.c3`
- `scripts/check_file_size_gate.sh`

Key results:
- Boundary facade guard passed and now covers the fallback cleanup regression
  surfaces that caused repeated churn.
- Build passed.
- Bounded `memory-lifetime-smoke` passed with `pass=237 fail=0`.
- File-size gate passed with no tracked code files above 1000 LOC.

Invalidated assumptions or failed approaches worth preserving:
- Do not reopen `FALLBACK-RUNTIME-OWNERSHIP-001` to retry global
  `OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BYPASS=1`; that deletion path was already
  falsified by bounded smoke failures. Future ownership work needs a new
  concrete failing signal or route-specific item.

Unresolved issues:
- No seeded runtime ownership fallback surface remains open in
  `docs/plans/fallback-inventory.md`.
- Full suite and ASAN were not run in this closure slice.

Next actions:
- Continue with the next open backlog area, such as `ML-VK-090` validation,
  broader Vulkan ML work, or another concrete audit finding with a live failing
  signal.

Signature: GPT-5 Codex

## 2026-04-22 01:15 CEST - Runtime Ownership Explicit Target Scope Cache

Objective attempted:
- Continue `FALLBACK-RUNTIME-OWNERSHIP-001` by removing another
  route-specific scope-chain cache bypass from explicit target-scope boundary
  decisions.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/eval_boundary_scope_chain.c3`
- `src/lisp/eval_boundary_provenance.c3`
- `src/lisp/eval_boundary_commit_escape_wrappers.c3`
- `src/lisp/tests_memory_lifetime_boundary_state_groups.c3`
- `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3`

Code or configuration changes made:
- Added `boundary_ptr_in_scope_chain_with_hint_cached(...)`, a cache-aware
  explicit-target scope-chain lookup helper backed by the active
  `PromotionContext` route cache.
- Routed `boundary_ptr_in_target_scope_chain_with_hint(...)` through the new
  helper to keep the current-scope path and explicit-target path aligned.
- Routed `boundary_classify_return_value(...)` and destination partial child
  reuse checks through the explicit-target cache.
- Added `run_memory_lifetime_boundary_explicit_target_scope_cache_test(...)`
  and wired it into the memory-lifetime smoke suite.

Commands run:
- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh bash -lc 'LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'`
- `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`
- `scripts/run_validation_container.sh bash -lc 'LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'`
- `c3c build --obj-out obj`
- `rg -n "boundary_ptr_in_scope_chain_with_hint\\(|boundary_ptr_in_scope_chain_with_hint_cached\\(" src/lisp -g '*.c3'`
- `git diff --check -- src/lisp/eval_boundary_scope_chain.c3 src/lisp/eval_boundary_provenance.c3 src/lisp/eval_boundary_commit_escape_wrappers.c3 src/lisp/tests_memory_lifetime_boundary_state_groups.c3 src/lisp/tests_memory_lifetime_smoke_suite_groups.c3`
- `scripts/check_file_size_gate.sh`

Key results:
- Normal build passed.
- Bounded `memory-lifetime-smoke` passed with `pass=237 fail=0`.
- Counters build passed. The compiler emitted the existing feature-switch
  unreachable-code warning in `eval_boundary_telemetry.c3`.
- Counters-enabled bounded `memory-lifetime-smoke` passed with
  `pass=237 fail=0`.
- Rebuilt the normal binary after the counters-enabled validation.
- File-size gate passed with no tracked code files above 1000 LOC.

Invalidated assumptions or failed approaches worth preserving:
- None in this slice. The global `OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BYPASS=1`
  deletion path remains invalidated; this change keeps cleanup route-specific.

Unresolved issues:
- `FALLBACK-RUNTIME-OWNERSHIP-001` remains open. The remaining work is final
  inventory/closure of route-specific traversal fallback surfaces, not broad
  scope-chain scan deletion.

Next actions:
- Continue `FALLBACK-RUNTIME-OWNERSHIP-001` by auditing remaining direct
  target-chain/reuse decisions for uncached explicit target-scope lookups or
  close the runtime ownership fallback item if the next inventory finds no
  remaining runtime surfaces.

Signature: GPT-5 Codex

## 2026-04-22 01:01 CEST - Runtime Ownership Env Barrier Checked Copy

Objective attempted:
- Continue `FALLBACK-RUNTIME-OWNERSHIP-001` by removing a remaining direct
  lower-level route-copy bypass from detached environment write promotion.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/value_environment_barrier.c3`
- `docs/todo_parts/todo_part_14.md`
- `.agents/plan_parts/plan_part_05.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Replaced the env write barrier's direct `copy_to_parent_by_route(...)` call
  with `copy_to_parent_with_fault(...)`.
- The env barrier now converts typed `BoundaryCopyFault` results into explicit
  error values at the env write boundary.
- Live inventory now reports `copy_to_parent_by_route(...)` only inside
  `src/lisp/eval_promotion_copy.c3`: the implementation and its two internal
  checked-copy call sites.

Commands run:
- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh bash -lc 'LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'`
- `rg -n 'copy_to_parent_by_route\(' src/lisp -g '*.c3'`
- `rg -n '[^A-Za-z0-9_]copy_to_parent\(|boundary_copy_to_parent_site_ctx\b|copy_to_parent_site\b' src/lisp -g '*.c3'`

Key results:
- Build passed.
- Bounded `memory-lifetime-smoke` passed with `pass=236 fail=0`.
- The unchecked wrapper inventory still reports no live
  `copy_to_parent(...)`, `boundary_copy_to_parent_site_ctx`, or
  `copy_to_parent_site` symbols under `src/lisp`.
- The only remaining `copy_to_parent_by_route(...)` references are internal to
  `eval_promotion_copy.c3`.

Invalidated assumptions or failed approaches worth preserving:
- None in this slice. The invalidated global scope-chain-scan bypass remains
  unchanged and must not be retried as a broad deletion.

Unresolved issues:
- `FALLBACK-RUNTIME-OWNERSHIP-001` remains open for route-specific traversal
  fallback cleanup. Global `OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BYPASS=1` remains a
  known-bad deletion path.

Next actions:
- Continue with route-specific fallback surfaces under
  `FALLBACK-RUNTIME-OWNERSHIP-001`, or return to `ML-VK-090` validation and
  benchmark coverage if ML validation is prioritized.

Signature: GPT-5 Codex

## 2026-04-22 00:39 CEST - ML Gradient Finite-Difference Validation

Objective attempted:
- Advance `ML-VK-090` by adding execution-backed finite-difference validation
  for shipped `ml/grad` contracts.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
- `docs/todo_parts/todo_part_14.md`
- `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- `memory/changelog_parts/changelog_part_37.md`
- `.agents/plan_parts/plan_part_05.md`

Code or configuration changes made:
- Added two central finite-difference regressions beside the existing analytic
  `ml/grad` tests.
- The new tests perturb public loss expressions through `ml/linear` plus
  `ml/mean-squared-error` or `ml/cross-entropy`, then compare the numeric
  gradient to the analytic `ml/grad` input/weight gradient.
- Closed `ML-VK-090-001` as the CPU finite-difference validation slice and
  left the broader Vulkan fallback and benchmark validation work explicit.
- Tightened stale Vulkan fallback diagnostics in `ml/clip-gradients` and
  `ml/optimizer-step` so mixed/unsupported Vulkan contracts no longer claim
  that Vulkan kernels are globally unimplemented.
- Added `ML-VK-090-002` coverage requiring mixed Vulkan/CPU
  `ml/clip-gradients` and `ml/optimizer-step` failures to expose the
  no-hidden-CPU-fallback placement contract in their error messages.

Commands run:
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval ...` for both candidate
  finite-difference expressions before patching.
- `c3c build --obj-out obj`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `rg -n "Vulkan gradient clipping kernels are not implemented|Vulkan optimizer kernels are not implemented" src/lisp docs memory .agents`
- `scripts/check_file_size_gate.sh`
- targeted `git diff --check`

Key results:
- Build passed.
- Focused advanced collections module passed with `pass=1900 fail=0`.
- The stale fallback diagnostic scan returned no matches.
- File-size gate passed.

Invalidated assumptions or failed approaches worth preserving:
- None in this slice. The finite-difference expressions were verified before
  landing and did not require a runtime implementation change.

Unresolved issues:
- At this checkpoint, `ML-VK-090` still needed broader no-hidden-fallback
  instrumentation where applicable, availability-gated Vulkan host paths,
  bounded-container ML slices, and benchmark fixtures. Later `ML-VK-090-003`
  closed the bounded/benchmark fixture residual, so the current remaining
  parent residual is broader availability-gated Vulkan host-path coverage.
- Broader `ML-VK-050`/`ML-VK-070` autograd expansion remains separate from
  this validation-only slice.

Next actions:
- Continue `ML-VK-090` with broader availability-gated Vulkan host-path
  coverage, or move to the runtime autograd expansion lane if execution
  semantics are prioritized.

Signature: GPT-5 Codex

## 2026-04-21 23:39 CEST - Runtime Ownership Unchecked Copy Wrapper Deletion

Objective attempted:
- Continue `FALLBACK-RUNTIME-OWNERSHIP-001` through a route-specific fallback
  cleanup without using the invalidated global scope-chain scan bypass.

Relevant workspace or target:
- `src/lisp/eval_boundary_api.c3`
- `src/lisp/eval_promotion_copy.c3`
- memory-lifetime test helpers and focused boundary tests

Code or configuration changes made:
- Removed dead unchecked `boundary_copy_to_parent_site_ctx(...)`.
- Removed dead unchecked `copy_to_parent_site(...)`.
- Removed plain unchecked `copy_to_parent(...)`.
- Moved remaining test-only callers to
  `boundary_copy_to_parent_site_ctx_checked(...)` with explicit
  `BoundaryCopyResult` handling, `test_copy_to_parent(...)`, or direct
  `copy_to_parent_with_fault(...)` assertions.

Commands run:
- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh bash -lc 'LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'`
- targeted `git diff --check`
- `scripts/check_file_size_gate.sh`

Key results:
- Build passed.
- Bounded `memory-lifetime-smoke` passed with `pass=236 fail=0`.
- Live inventory for `copy_to_parent(...)`, `boundary_copy_to_parent_site_ctx`,
  and `copy_to_parent_site` under `src/lisp` returned no symbols.

Unresolved issues:
- `FALLBACK-RUNTIME-OWNERSHIP-001` remains open. Global scope-chain scan
  deletion is still invalidated; remaining cleanup must stay route-specific.

Signature: GPT-5 Codex

## 2026-04-22 00:18 CEST - ML SGD Step Device Reuse

Objective attempted:
- Continue codebase audit/repair work by converting a stale ML device
  fail-closed path into real behavior.

Relevant workspace or target:
- `/home/christos/Omni`
- `ML-VK-060`
- `src/lisp/prim_ml_autograd.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`

Code or configuration changes made:
- Routed `ml/sgd-step(parameters gradients learning-rate)` through the existing
  stateless CUDA/Vulkan dense row-major `Float32` SGD optimizer kernels before
  falling back to CPU materialization.
- Kept mixed CPU/device leaves fail-closed with `tensor/backend-unsupported`;
  no hidden tensor transfer was added.
- Added CUDA and Vulkan placement-preservation regressions guarded by
  `ml-optimizer-sgd-float32` capability bits.
- Updated `docs/todo_parts/todo_part_14.md`,
  `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`, and
  `memory/changelog_parts/changelog_part_37.md`.

Commands run:
- `c3c build --obj-out obj`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Build passed.
- Focused advanced collections/module slice passed with `pass=1897 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- File-size gate passed with no tracked code files above 1000 LOC.
- Whitespace check passed.

Invalidated assumptions or failed approaches worth preserving:
- Do not treat `ml/sgd-step` as CPU-only now that the shared SGD optimizer
  kernels exist. The correct narrow contract is all-device CUDA/Vulkan Float32
  execution with mixed placement rejected.

Unresolved issues:
- Broader reverse-mode autograd remains the blocker for full training coverage
  beyond the current data-oriented gradient specs.
- Fused CUDA optimizer kernels beyond current map-backed implementations remain
  open.

Next actions:
- Continue `ML-VK-050` autograd expansion or the remaining `ML-VK-090`
  validation/benchmark suite work, rather than reopening `ml/sgd-step` device
  placement.

Signature: GPT-5 Codex

## 2026-04-21 23:59 CEST - FTXUI ABI Wrapper Contract Freeze

Objective attempted:
- Close the residual `FTXUI-C-ABI-WRAPPERS-001` item by either implementing
  the next wrapper family or freezing deferred wrapper families explicitly.

Relevant workspace or target:
- FTXUI C ABI shim plan and focused ABI regressions.

Code or configuration changes made:
- Added fail-closed ABI regression coverage for the unsupported piped-input
  runtime-helper path in
  `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3`.
- Marked the remaining advanced upstream wrapper families as explicit non-goals
  for the current generic-builder-first ABI contract in
  `docs/plans/ftxui-c-abi-shim.md` and `docs/todo_parts/todo_part_14.md`.

Commands run:
- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh bash -lc 'LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'`
- `scripts/run_ftxui_smoke.sh`
- targeted `git diff --check`
- `scripts/check_file_size_gate.sh`

Key result:
- `FTXUI-C-ABI-WRAPPERS-001` is closed for the current contract. The next UI
  work should stay in the Omni-facing runtime loop item unless a concrete
  feature proves a new dedicated ABI wrapper is required.
- Bounded focused advanced FFI group passed with `pass=105 fail=0`.

Unresolved issues:
- `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001` remains open for blocking/session-based
  read/update/render behavior.

Signature: GPT-5 Codex

## 2026-04-21 23:55 CEST - UI Runtime FTXUI Backend Dispatch

Objective attempted:
- Continue codebase audit/repair by implementing the split
  `UI-LIB-RUNTIME-BACKEND-001` runtime-to-backend lifecycle path.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_ui_ftxui.c3`
- `examples/libraries/ftxui/lib/ui/runtime.omni`
- `examples/libraries/ftxui/lib/ui/ftxui.omni`
- `examples/libraries/ftxui/module_backend_smoke.omni`

Code or configuration changes made:
- Added hidden primitive `__ui-ftxui-dispatch`.
- Registered the primitive and added it to the compiler builtin exclusion list
  alongside `__ui-ftxui-run`.
- Added `ui.runtime.dispatch_to`.
- Added `ui.ftxui.dispatch` as the public backend handoff.
- Added `module_backend_smoke.omni` and included it in
  `scripts/run_ftxui_smoke.sh`.
- Updated TODO, plan, UI reference, changelog, and active agent plan.

Commands run:
- `c3c build --obj-out obj`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_backend_smoke.omni`
- `scripts/run_ftxui_smoke.sh`
- `scripts/run_validation_container.sh bash -lc 'LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'`
- targeted `git diff --check`
- `scripts/check_file_size_gate.sh`

Key results:
- Build passed.
- Backend smoke returned `true`.
- FTXUI smoke passed with `module_backend_smoke`.
- Bounded advanced FFI system surface passed with `pass=102 fail=0`.
- Whitespace and file-size gates passed.

Invalidated assumptions or failed approaches worth preserving:
- Do not keep treating UI runtime backend execution as unimplemented. The
  non-interactive open/render/invalidate/post/close backend lifecycle is now
  shipped.
- Do not model `read_event_tree` as part of the non-interactive dispatcher. It
  intentionally fails closed pending an explicit interactive/session loop
  contract.

Unresolved issues:
- `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001` remains open for blocking/session-based
  read/update/render behavior.

Next actions:
- Design the interactive loop/session contract before adding blocking terminal
  reads to the UI runtime surface.

Signature: GPT-5 Codex

## 2026-04-21 23:22 CEST - UI Facade Dotted Module Closure

Objective attempted:
- Continue codebase audit/repair by closing or narrowing the stale
  `UI-LIB-FACADE-001` backlog item.

Relevant workspace or target:
- `/home/christos/Omni`
- `examples/libraries/ftxui/`
- `docs/plans/ui-library-facade-plan-2026-03-27.md`
- `docs/todo_parts/todo_part_14.md`
- `docs/UI_REFERENCE.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Rewired `examples/libraries/ftxui/ui.omni` to load the dotted `lib/ui/*`
  modules as the canonical implementation source instead of the flat
  compatibility helper files.
- Fixed sibling imports in `lib/ui/layout.omni`, `lib/ui/style.omni`,
  `lib/ui/runtime.omni`, and `lib/ui/evaluate.omni` so default dotted module
  paths load cleanly.
- Added `examples/libraries/ftxui/module_direct_smoke.omni` and added it to
  `scripts/run_ftxui_smoke.sh`.
- Removed the stale static evaluator copy from `ui.runtime`; `lib/ui/evaluate`
  remains the isolated evaluator owner.
- Closed `UI-LIB-FACADE-001` and split the remaining backend lifecycle work into
  `UI-LIB-RUNTIME-BACKEND-001`.

Commands run:
- `c3c build --obj-out obj`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/lib/ui/runtime.omni`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/lib/ui/evaluate.omni`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_direct_smoke.omni`
- `scripts/run_ftxui_smoke.sh`

Key results:
- Build passed.
- Direct `lib/ui/runtime.omni` load returned the dispatch module value.
- Direct `lib/ui/evaluate.omni` load returned the evaluator function.
- Full FTXUI smoke passed, including the new direct dotted-module smoke and
  the live demo quit path.

Invalidated assumptions or failed approaches worth preserving:
- Do not keep treating `UI-LIB-FACADE-001` as missing module ownership. The
  dotted helper modules are now the facade implementation source and have direct
  import smoke coverage.
- Do not treat `ui.runtime` as the owner of static tree evaluation. Runtime owns
  dispatch; static evaluation belongs in `lib/ui/evaluate.omni`.

Unresolved issues:
- Superseded by the later `2026-04-21 23:55 CEST - UI Runtime FTXUI Backend
  Dispatch` entry: `UI-LIB-RUNTIME-BACKEND-001` is now closed for the
  non-interactive backend lifecycle. Interactive read/update/render loop work is
  tracked separately as `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001`.

Next actions:
- Continue with `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001` if an interactive,
  blocking or session-based read/update/render loop is required.

Signature: GPT-5 Codex

## 2026-04-21 12:03 CEST - MEMORY-P2-TCO-LANE-RESET Verification

Objective attempted:
- Close `MEMORY-P2-TCO-LANE-RESET` by verifying whether TCO call-scope recycle
  now resets only TEMP while preserving ESCAPE bindings.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/scope_region_reset_helpers.c3`
- `src/lisp/runtime_backend_hooks.c3`
- `src/lisp/jit_eval_scopes.c3`
- `src/lisp/tests_memory_lifetime_tco_budget_groups.c3`
- `src/lisp/tests_memory_lifetime_tco_budget_failure_guard_groups.c3`

Code or configuration changes made:
- No additional runtime code was needed for this item in this pass. The current
  implementation already includes `scope_reset_temp_lane(ScopeRegion*)` and
  routes eligible TCO bounces through `runtime_prepare_tco_recycle_env`.
- Marked `MEMORY-P2-TCO-LANE-RESET` complete and recorded the validation and
  local container-toolchain requirement.

Commands run:
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/run_validation_container.sh bash -lc 'c3c build && env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
- `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh bash -lc 'which c3c; c3c --version; c3c build && env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite scope`
- `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=tco-recycling ./build/main --test-suite lisp'`

Key results:
- Host-side memory-lifetime smoke refused as expected under the container-only
  validation policy.
- Full JIT policy slice passed with `pass=52 fail=0`.
- The first bounded container attempt failed because the validation image's
  default `/usr/local/bin/c3c` is x86-64 and this host is aarch64.
- Bounded container memory-lifetime smoke passed with the local aarch64
  toolchain mounted: `pass=233 fail=0`.
- Scope suite passed with `63 passed, 0 failed`.
- Bounded container `tco-recycling` slice passed with `pass=11 fail=0`.

Invalidated assumptions or failed approaches worth preserving:
- Do not assume the validation image's default toolchain is executable on this
  host. For container-bound validation here, mount `/home/christos/.local` via
  `OMNI_VALIDATION_TOOLCHAIN_ROOT`.

Unresolved issues:
- None for TEMP-lane reset routing. Remaining TCO work is the separate
  `MEMORY-P2-TCO-ENV-REUSE` item.

Next actions:
- Continue with `MEMORY-P2-TCO-ENV-REUSE`, focused on reducing remaining
  hot-path env-copy churn rather than reopening TEMP-lane reset.

Signature: GPT-5 Codex

## 2026-04-21 22:40 CEST - Runtime Ownership Zero-Hint Scope Cache

Objective attempted:
- Continue auditing and repairing open runtime ownership fallback work under
  `FALLBACK-RUNTIME-OWNERSHIP-001`.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/eval_boundary_scope_chain.c3`
- `src/lisp/tests_memory_lifetime_boundary_state_groups.c3`
- `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3`
- `docs/plans/fallback-inventory.md`
- `docs/todo_parts/todo_part_14.md`

Code or configuration changes made:
- Removed the special-case early return that made
  `boundary_ptr_in_target_scope_chain_with_hint(...)` skip the active
  promotion-context route cache whenever `pinned_gen == 0`.
- Removed dead non-hinted scope-chain helper surfaces after verifying all live
  callers use the hinted/cache-aware route.
- Added a focused memory-lifetime smoke regression proving repeated
  zero-generation hinted target-chain lookups scan once under one active
  promotion context.
- Updated the fallback inventory, TODO, changelog, and active plan checkpoint
  to record this as route-specific fallback cleanup rather than global scan
  deletion.

Commands run:
- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh bash -lc 'OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp'`
- `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`
- `scripts/run_validation_container.sh bash -lc 'OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp'`
- `c3c build --obj-out obj`
- `rg -n 'boundary_ptr_in_scope_chain_scan\\(|boundary_ptr_in_scope_chain\\(|boundary_ptr_in_target_scope_chain\\(' src/lisp -g '*.c3'`

Key results:
- Normal build passed.
- Bounded memory-lifetime smoke passed with `pass=236 fail=0`.
- Counters build passed; C3 emitted the expected unreachable-code warning in
  `eval_boundary_telemetry.c3` because the instrumentation feature branch
  returns before the default mode.
- Counters-enabled bounded memory-lifetime smoke passed with `pass=236 fail=0`.
- The workspace was rebuilt back to the normal non-counters variant.

Invalidated assumptions or failed approaches worth preserving:
- The global scope-chain scan bypass remains invalidated. Continue deleting or
  reducing scan fallback only through route-specific replacement semantics.

Unresolved issues:
- `FALLBACK-RUNTIME-OWNERSHIP-001` remains open for any remaining explicit
  boundary route replacements and final closure of staged fallback wording.

Next actions:
- Continue with another route-specific scan/copy fallback if one is identifiable
  from telemetry or code inventory; otherwise move to the next open backlog item
  rather than forcing the known-bad global bypass.

Signature: GPT-5 Codex

## 2026-04-21 22:26 CEST - FTXUI C ABI Focused Wrapper Coverage

Objective attempted:
- Continue auditing and repairing open codebase TODOs by advancing the active
  `FTXUI-C-ABI-WRAPPERS-001` item.

Relevant workspace or target:
- `/home/christos/Omni`
- `csrc/ftxui_shim.h`
- `csrc/ftxui_shim_component.inc`
- `src/lisp/ftxui_ffi.c3`
- `src/lisp/ftxui_ffi_types.c3`
- `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3`
- `docs/plans/ftxui-c-abi-shim.md`
- `docs/todo_parts/todo_part_14.md`

Code or configuration changes made:
- Added `omni_ftxui_component_handle_event(...)` to the C ABI and C3 extern
  surface for deterministic component event dispatch outside an interactive
  screen loop.
- Added the previously documented-but-missing
  `omni_ftxui_component_wrap_action(...)` action callback wrapper.
- Tightened `component_handle_event` so callback-side context failures are
  returned instead of being cleared after `OnEvent`.
- Corrected the C3 FTXUI callback option mirror so callback fields use
  function-pointer aliases directly.
- Added focused advanced FFI regressions for FTXUI context/error lifecycle,
  screen lifecycle/control/post-event/exit, table selection/render conversion,
  canvas draw/render conversion, event-handler callback delivery, and action
  callback delivery.
- Updated the FTXUI ABI plan, TODO entry, changelog, and active plan checkpoint
  to separate landed focused coverage from remaining advanced wrapper-family
  work.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh bash -lc 'OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp'`
- `scripts/run_ftxui_smoke.sh`

Key results:
- Build linked successfully.
- Bounded focused advanced FFI group passed with `pass=102 fail=0`.
- Existing FTXUI smoke script passed for `module_value_smoke`,
  `module_effect_smoke`, `smoke.omni`, and `demo.omni`.

Invalidated assumptions or failed approaches worth preserving:
- Do not model C3 callback fields as `CallbackAlias*`; C3 function aliases
  already represent the callable slot shape used by the C ABI. The previous
  pointer-to-function-pointer mirror only went unnoticed because no test was
  assigning those fields.

Unresolved issues:
- Superseded by the later `2026-04-21 23:59 CEST - FTXUI ABI Wrapper Contract
  Freeze` entry: `FTXUI-C-ABI-WRAPPERS-001` is now closed for the chosen
  generic-builder-first ABI contract.

Next actions:
- Continue through the Omni-facing UI runtime loop item unless a concrete
  feature proves a new dedicated ABI wrapper is required.

Signature: GPT-5 Codex

## 2026-04-21 21:41 CEST - Runtime Ownership Fallback Checkpoint

Objective attempted:
- Continue `FALLBACK-RUNTIME-OWNERSHIP-001` by auditing remaining
  `copy_to_parent` and scope-chain traversal fallback surfaces.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/plans/fallback-inventory.md`
- `docs/todo_parts/todo_part_14.md`
- `src/lisp/eval_promotion_copy.c3`
- `src/lisp/eval_boundary_scope_chain.c3`
- `src/lisp/eval_boundary_provenance.c3`
- `src/lisp/tests_memory_lifetime_boundary_state_groups.c3`

Code or configuration changes made:
- Hardened checked `copy_to_parent_with_fault(...)` missing-provenance behavior:
  checked calls now return `null` and set
  `BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE` instead of returning an
  `ERROR` value through the typed helper surface.
- Extended the boundary copy fault test to cover the direct checked helper in
  addition to `boundary_copy_to_parent_site_ctx_checked(...)`.
- Split the scope-chain raw scan loop from its policy gate, so a hinted lookup
  that falls back to raw scope-chain traversal consumes scan budget once per
  logical lookup.
- Split the target-chain reuse predicate into
  `boundary_can_reuse_value_with_target_chain(...)` so
  `boundary_classify_return_value(...)` can reuse the target-chain membership
  it already computed instead of performing a second target-chain scan in the
  same classification decision.
- Added a memory-lifetime smoke regression for the scope-chain scan-budget
  edge.
- Updated the fallback inventory and active TODO to record that global
  scope-chain scan deletion is currently invalidated.

Commands run:
- `c3c build --obj-out obj`
- `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`
- `scripts/run_validation_container.sh bash -lc 'LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'`
- `scripts/run_validation_container.sh bash -lc 'LD_LIBRARY_PATH=/usr/local/lib OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BYPASS=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'`
- `scripts/run_validation_container.sh bash -lc 'LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'` after the counters build
- `scripts/check_file_size_gate.sh`
- targeted `git diff --check`

Key results:
- Build passed.
- Normal bounded `memory-lifetime-smoke` passed with `pass=235 fail=0`.
- Counters-enabled bounded `memory-lifetime-smoke` also passed with
  `pass=235 fail=0`, covering the scan-budget-sensitive path.
- The workspace was rebuilt back to the normal non-counters binary after that
  counters validation.
- The scan-bypass experiment failed with `pass=215 fail=9`, proving the broad
  deletion path is not yet semantically covered by explicit routes.

Invalidated assumptions or failed approaches worth preserving:
- Do not globally bypass/delete scope-chain scans as the next cleanup step.
  Current reuse/classification invariants still require them.

Unresolved issues:
- `FALLBACK-RUNTIME-OWNERSHIP-001` remains open. Remaining cleanup needs
  further route-specific replacement of scope-chain scan dependents, not a
  global traversal deletion. The duplicate target-chain scan inside return
  classification is now removed.

Next actions:
- Continue auditing boundary routes that still depend on scope-chain scans and
  replace them one semantic family at a time when tests can prove parity.

Signature: GPT-5 Codex

## 2026-04-21 22:06 CEST - Runtime Ownership Destination Checked Copy

Objective attempted:
- Continue `FALLBACK-RUNTIME-OWNERSHIP-001` by replacing live unchecked
  destination-builder copy-to-parent routes with the checked copy facade.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/eval_boundary_commit_escape_cons.c3`
- `src/lisp/eval_boundary_commit_escape_builder_helpers.c3`
- `src/lisp/eval_boundary_commit_escape_wrappers.c3`
- `docs/todo_parts/todo_part_14.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Converted destination cons, partial, and iterator child-copy calls from
  `boundary_copy_to_parent_site_ctx(...)` to
  `boundary_copy_to_parent_site_ctx_checked(...)`.
- Kept checked-copy child faults as promotable error children instead of
  aborting the promotion context before
  `boundary_destination_bubble_child_fault(...)` can build the target-scope
  error value.

Commands run:
- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh bash -lc 'OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp'`

Key results:
- Build passed.
- Bounded `memory-lifetime-smoke` passed with `pass=235 fail=0`.
- The smoke slice still prints the expected graph-audit violation from the
  nested destination fault test before proving the fail-closed path; the final
  unified summary is green.

Invalidated assumptions or failed approaches worth preserving:
- Do not mark the active destination-promotion context aborted immediately for
  ordinary checked child-copy faults on this route. That suppresses the
  destination error-bubbling contract and regresses the nested iterator fault
  test.

Unresolved issues:
- `FALLBACK-RUNTIME-OWNERSHIP-001` remains open. Other commit/build/root clone
  call sites still need checked-copy conversion and route-specific validation.

Next actions:
- Continue with the remaining unchecked copy-to-parent call sites in boundary
  commit/build/root clone paths, one route family at a time.

Signature: GPT-5 Codex

## 2026-04-21 22:15 CEST - Runtime Ownership Root Checked Copy

Objective attempted:
- Finish the unchecked `boundary_copy_to_parent_site_ctx(...)` callsite cleanup
  slice under `FALLBACK-RUNTIME-OWNERSHIP-001`.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/eval_promotion_root_store.c3`
- `src/lisp/eval_promotion_root_clone_basic.c3`
- `src/lisp/eval_promotion_root_clones.c3`
- `docs/todo_parts/todo_part_14.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Converted root-store direct promotion and root clone child-copy routes to
  `boundary_copy_to_parent_site_ctx_checked(...)`.
- Preserved root clone fail-closed semantics by converting typed copy faults to
  error values at the existing clone boundary and using the existing partial
  cleanup paths for arrays, hashmaps, and method tables.

Commands run:
- `rg -n "boundary_copy_to_parent_site_ctx\\(" src/lisp/eval_boundary_* src/lisp/eval_promotion_* src/lisp/value_* src/lisp/jit_*`
- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh bash -lc 'OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp'`

Key results:
- The unchecked-wrapper inventory now reports only the wrapper definition in
  `src/lisp/eval_boundary_api.c3`.
- Build passed.
- Bounded `memory-lifetime-smoke` passed with `pass=235 fail=0`.

Unresolved issues:
- `FALLBACK-RUNTIME-OWNERSHIP-001` remains open for route-specific scope-chain
  traversal replacement. Global scan deletion remains invalidated by the prior
  `OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BYPASS=1` failure.

Next actions:
- Audit remaining scope-chain traversal consumers and replace them only where a
  route-specific ownership/provenance check can prove parity.

Signature: GPT-5 Codex

## 2026-04-21 22:23 CEST - Runtime Ownership Reuse Precheck

Objective attempted:
- Continue route-specific `FALLBACK-RUNTIME-OWNERSHIP-001` cleanup after
  invalidating broad scope-chain scan deletion.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/eval_boundary_provenance.c3`

Code or configuration changes made:
- Reordered `boundary_can_reuse_value(...)` to compute target-chain membership
  once before alias-safety traversal and pass it into
  `boundary_can_reuse_value_with_target_chain(...)`.

Commands run:
- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh bash -lc 'OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp'`

Key results:
- Build passed.
- Bounded `memory-lifetime-smoke` passed with `pass=235 fail=0`.

Unresolved issues:
- This is a scan-reduction cleanup, not global scan deletion. The route-specific
  replacement queue remains open under `FALLBACK-RUNTIME-OWNERSHIP-001`.

Next actions:
- Continue auditing remaining scope-chain consumers with explicit route
  semantics and avoid reopening the invalidated global bypass approach.

Signature: GPT-5 Codex

## 2026-04-21 21:24 CEST - REPL Server Bounded Request Queue

Objective attempted:
- Finish the residual same-stream queueing work split out of
  `REPL-SERVER-MULTICLIENT-001`.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/plans/repl-server-protocol-2026-03-30.md`
- `docs/todo_parts/todo_part_14.md`
- `src/lisp/eval_repl_server_state.c3`
- `src/lisp/eval_repl_server_worker.c3`
- `src/lisp/eval_repl_server_worker_helpers.c3`
- `src/lisp/tests_runtime_async_repl_server_groups.c3`

Code or configuration changes made:
- Replaced the REPL worker's single pending-command slot with an 8-entry FIFO
  command queue.
- Preserved one active runtime command per connection.
- Changed `protocol/server-busy` to queue-full backpressure for runtime work.
- Kept interrupts active-request-only.
- Extended stdin matching across queued `eval` / `load-file` request ids.
- Deferred queued `clone` session allocation to worker execution so the
  request reader cannot reallocate the connection session table while the worker
  may hold active session pointers.
- Added teardown cleanup for queued commands.
- Added regressions for multiple pending commands, FIFO order, queued stdin
  targeting, active-only interrupt behavior, queue-full backpressure, and
  deferred clone session allocation.

Commands run:
- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh bash -lc 'LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'`

Key results:
- Build passed.
- Bounded `async` slice passed with `pass=69 fail=0`.

Unresolved issues:
- No residual REPL phase item remains open in this TODO group. Future REPL work
  should be shaped as new protocol capability items, not kept under the closed
  transport/queueing umbrella.

Next actions:
- Continue with the remaining open audit queue; likely next choices are
  `FALLBACK-RUNTIME-OWNERSHIP-001`, `FTXUI-C-ABI-WRAPPERS-001`, or a narrower
  ML-VK residual.

Signature: GPT-5 Codex

## 2026-04-21 21:24 CEST - REPL Server Concurrent Client Handoff

Objective attempted:
- Continue the REPL server protocol backlog by shipping the concurrent
  accepted-client transport boundary while preserving current per-connection
  single-worker semantics.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/plans/repl-server-protocol-2026-03-30.md`
- `docs/todo_parts/todo_part_14.md`
- `src/lisp/eval_repl_server_listeners.c3`
- `src/lisp/async_runtime_base.c3`
- `src/lisp/tests_runtime_async_repl_server_groups.c3`

Code or configuration changes made:
- Added detached REPL client-handler thread context and start helper.
- Changed Unix socket and TCP listeners to hand accepted client fds to handler
  threads instead of processing each stream synchronously inside the listener
  loop.
- Added structured start-failure rejection for accepted clients:
  `runtime/client-handler-start-failed`.
- Added `socketpair` extern/constants for local handler-thread regression
  coverage.
- Added a regression that starts two independent handler streams over
  socketpairs and verifies both return `describe` replies with the expected
  transport names.
- Updated the protocol plan and TODO so request queueing is no longer hidden
  under the completed multi-client transport item.

Commands run:
- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh bash -lc 'LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'`
- targeted `git diff --check`
- `scripts/check_file_size_gate.sh`

Key results:
- Build passed.
- Bounded `async` slice passed with `pass=66 fail=0`.
- File-size gate passed with no tracked code files over 1000 LOC.

Unresolved issues:
- `REPL-SERVER-REQUEST-QUEUE-001` remains open. Same-stream runtime work still
  uses the existing one-active-operation `protocol/server-busy` contract.

Next actions:
- Design bounded per-stream request queueing with explicit interrupt/stdin
  semantics and backpressure before changing existing busy behavior.

Signature: GPT-5 Codex

## 2026-04-21 21:24 CEST - FTXUI C ABI Contract Decisions

Objective attempted:
- Close the contract-decision half of the FTXUI ABI backlog before expanding
  wrapper families.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/plans/ftxui-c-abi-shim.md`
- `docs/todo_parts/todo_part_14.md`
- `csrc/ftxui_shim.h`
- `csrc/ftxui_shim.cpp`
- `src/lisp/ftxui_ffi.c3`
- `src/lisp/prim_ui_ftxui.c3`

Code or configuration changes made:
- Updated `docs/plans/ftxui-c-abi-shim.md` to turn the remaining open
  interface questions into explicit contract decisions.
- Marked `FTXUI-C-ABI-CONTRACT-001` complete in the active TODO.

Key decisions:
- Inbound `const char*` fields are borrowed only for the ABI call. The shim
  must eagerly copy any string that FTXUI state can retain after return.
- `omni_ftxui_status_name(...)` returns static storage.
- `omni_ftxui_context_last_error_message(...)` returns context-owned storage
  valid only until the next error mutation, clear, or destroy.
- The ABI remains generic-builder-first. Dedicated entry points are reserved
  for lifetime, callback, or state contracts that generic builders cannot
  express cleanly.
- `OMNI_FTXUI_EVENT_CUSTOM` remains payload-free; richer payloads belong in
  callback `user_data` or the Omni-facing effect/runtime layer.

Commands run:
- targeted source/doc inspection with `rg` and `sed`
- targeted `git diff --check`

Unresolved issues:
- Superseded by the later `2026-04-21 23:59 CEST - FTXUI ABI Wrapper Contract
  Freeze` entry: deferred wrapper families are now frozen as explicit
  non-goals for the current ABI contract.

Next actions:
- Add focused ABI regressions for context/screen lifecycle, event
  create/post/destroy, widget callbacks, and table/canvas conversion before
  landing more wrapper families.

Signature: GPT-5 Codex

## 2026-04-21 21:24 CEST - UV Callback Handle Closure

Objective attempted:
- Audit and close `FFI-CALLBACK-UV-001`, then harden any remaining lifetime
  gaps in the explicit `uv` callback-handle prototype.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/async_uv_timer_callback_handle.c3`
- `src/lisp/async_uv_timer_callback.c3`
- `csrc/uv_helpers.c`
- `src/lisp/tests_advanced_io_effect_ffi_scheduler_boundary.c3`
- `src/lisp/tests_advanced_io_effect_ffi_groups.c3`
- `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3`
- `docs/todo_parts/todo_part_14.md`

Code or configuration changes made:
- Confirmed the shipped callback lane is limited to the explicit `uv` timer
  callback shim and does not expose generic closure-to-native-function-pointer
  coercion.
- Added uv-specific regression coverage for wrapper box/value allocation
  failure after C shim registration. The test verifies the shared FFI
  constructor/finalizer path releases the retained callback owner scope.
- Added uv-specific regression coverage for finalizer-driven teardown when a
  local wrapper scope is destroyed without explicit unhandle.
- Closed `FFI-CALLBACK-UV-001` in the TODO part and updated the active plan and
  changelog with the shipped contract.

Commands run:
- `c3c build --obj-out obj`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- targeted `git diff --check`

Key results:
- Build passed.
- Focused FFI advanced surface passed with `pass=72 fail=0`.
- The prior backlog statement that the shim-only prototype was not shipped is
  now stale and has been closed.

Invalidated assumptions or failed approaches worth preserving:
- Do not treat a `make_ffi_handle_ex(...)` null return as a uv-specific leak by
  default; ownership is intentionally transferred to the shared FFI constructor
  once the finalizer and payload are passed in.

Unresolved issues:
- None for the scoped `FFI-CALLBACK-UV-001` prototype.
- Broader generic callback handles remain intentionally unshipped.

Next actions:
- Continue with `FALLBACK-RUNTIME-OWNERSHIP-001`, the highest-risk remaining
  memory/lifetime cleanup item from the active TODO queue.

Signature: GPT-5 Codex

## 2026-04-21 18:45 CEST - FFI Async and ML-VK Fail-Closed Hardening

Objective attempted:
- Continue repository audit/repair after subagent findings, focusing on
  concrete memory/lifetime and malformed-graph issues.

Relevant workspace or target:
- `/home/christos/Omni`
- FFI async/offload scheduler ownership
- FFI Float32 return ABI
- ML-VK custom Kernel and Tensor graph replay validation

Code or configuration changes made:
- Added optional owned custom-context release to `OffloadWork` via
  `custom_free` and `scheduler_release_custom_ctx(...)`.
- Routed owned custom-context cleanup through worker execution, worker
  cancellation, admission/backend errors, pending fiber errors, and OS-thread
  failure/finish paths.
- Changed `ffi-async-call` to transfer its heap call context to the scheduler
  instead of freeing it immediately after enqueue.
- Fixed sync and async FFI Float32 return storage so libffi writes into a
  `float` slot, not a `long` slot.
- Reused the sync variadic C ABI inference helper from `ffi-async-call` so
  async variadic extras keep their real Integer/Float/String/ForeignHandle
  ABI shape instead of collapsing to pointer arguments.
- Routed async FFI string returns through scheduler shared bytes and made
  pointer-like async returns fail closed instead of materializing raw addresses
  as integers.
- Added `sinf(Float32)` FFI surface tests for non-zero Float32 return payloads,
  async variadic `snprintf`, async string/null `strchr` returns, and fail-closed
  async pointer-like returns.
- Hardened ML-VK graph/custom Kernel validation:
  - `Kernel` construction rejects source-backed non-`main` entries;
  - bare symbolic `source 'map-unary-f32` now validates and executes like the
    dictionary `builtin-spirv` unary source form;
  - `tensor/run(graph)` rejects both left/right scalar operands on one binary
    map node;
  - `tensor/capture` validates map operands before appending graph nodes.
- Updated TODO and changelog artifacts for the closed hardening boundaries.

Commands run:
- `c3c build --obj-out obj`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- Direct `./build/main --eval` probes for non-`main` Kernel rejection and
  both-scalar graph replay rejection.
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `scripts/check_file_size_gate.sh`
- Targeted `git diff --check`

Key results:
- Build passed.
- Focused FFI advanced surface passed: `pass=109 fail=0`.
- Direct fail-closed ML-VK probes returned `true`.
- Advanced collections module passed: `pass=1895 fail=0`.
- File-size gate passed with no tracked code files above 1000 LOC.
- Targeted whitespace check passed.

Observed failure modes:
- Basic slice still has the previously observed unrelated tail multi-arg
  failures when run without selecting the advanced slice.

Invalidated assumptions or failed approaches worth preserving:
- Do not free heap custom offload contexts at the primitive call site once
  `OffloadWork` has been queued or copied; queued worker/task records may still
  dereference `custom_ctx`.
- Do not treat `FFI_TYPE_FLOAT32` returns as integer-lane storage. Libffi must
  receive a real `float*` return slot.
- Do not accept malformed graph map nodes and infer the intended operand side
  during replay; ambiguous scalar-side metadata must fail closed.

Unresolved issues:
- Generic FFI callback-to-native-pointer support remains open under
  `FFI-CALLBACK-UV-001`; this pass only fixed async FFI call offload context
  ownership and Float32 returns.
- ML-VK-090 validation/benchmark harness remains open.
- The unrelated advanced collections failures need their own repair lane.

Next actions:
- Continue audit/repair from the remaining open TODOs, with the next high-value
  runtime lane being the advanced collections realize/tail failures or the
  explicit `FFI-CALLBACK-UV-001` callback-handle prototype.

Signature: GPT-5 Codex

## 2026-04-21 20:49 CEST - JIT Tail Error Args and Tensor Contract Alias Repair

Objective attempted:
- Continue audit/repair from the red focused collections gate identified after
  the FFI async and ML-VK hardening pass.

Relevant workspace or target:
- `/home/christos/Omni`
- JIT tail multi-argument call argument-list construction
- Tensor `realize` destination alias checks for lazy contract expressions

Code or configuration changes made:
- Updated `jit_cons_escape(...)` so promoted `ERROR` values in the cons car
  are valid closure-call argument data when they satisfy the same promoted-error
  contract as `make_cons(...)`.
- Added `TensorVal.alias_origin` to preserve logical tensor storage lineage
  across boundary clones.
- Initialized fresh tensor payload alias origins in `tensor_alloc_payload_base_checked(...)`.
- Made transpose views inherit their source tensor alias origin.
- Extended tensor storage alias checks to compare alias origins before raw
  backing-storage pointers.
- Updated TODO and changelog artifacts for the closed repair slices.

Commands run:
- `c3c build --obj-out obj`
- Direct `./build/main --eval` probes for:
  - named-let tail call ignoring an error-valued unused argument,
  - named-let tail call preserving the original error-valued argument when
    returned,
  - `realize` source-error propagation,
  - direct and nested contract-source destination alias rejection,
  - allowed map-in-place realization.
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `scripts/check_file_size_gate.sh`
- Targeted `git diff --check`

Key results:
- Build passed.
- Basic Lisp slice passed: `pass=166 fail=0`.
- Focused advanced collections module passed: `pass=1894 fail=0`.
- Contract alias probes returned `"contract: destination aliases source tensor"`.
- Map-in-place realization remained allowed and returned `5.0`.
- Code file-size gate passed with no tracked code file above 1000 LOC.
- Targeted whitespace check passed.

Invalidated assumptions or failed approaches worth preserving:
- Do not treat every `ERROR` value returned by escape promotion as a helper
  failure when constructing closure argument lists. In the cons car position,
  error values can be ordinary data until the callee observes them.
- Do not rely only on concrete tensor `data` pointer equality for contract
  destination alias checks. Boundary cloning can deep-copy storage while the
  source expression still descends from the same logical tensor.

Unresolved issues:
- ASAN was not run for this slice.
- The broader repository still has many unrelated dirty changes and open audit
  backlog items; this pass closed only the tail-error and tensor-contract-alias
  regressions that were governing the focused collections failure.

Next actions:
- Continue auditing from the remaining open TODO queue; prioritize runtime
  ownership, callback-handle, fallback, and ML-VK validation/benchmark items
  rather than reopening the closed collections regressions.

Signature: GPT-5 Codex

## 2026-04-21 18:10 CEST - Runtime Split Queue Superseded

Objective attempted:
- Continue audit/repair work while honoring the owner rule: do not split files
  unless they exceed 1000 LOC.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/plans/largest-runtime-files-pass-2026-03-19.md`
- `docs/todo_parts/todo_part_14.md`
- `AGENTS.md`

Code or configuration changes made:
- Updated `AGENTS.md` to persist the 1000 LOC file-splitting threshold.
- Updated `scripts/check_file_size_gate.sh` so validation enforces the same
  1000 LOC code-file threshold.
- Marked `docs/plans/largest-runtime-files-pass-2026-03-19.md` as
  superseded instead of active.
- Closed `RUNTIME-FILE-SPLIT-QUEUE-001` without splitting code.
- Recorded negative memory in the changelog so future agents do not reopen
  the stale 2026-03-19 split queue as active churn.
- Marked `AUDIT_REPORT_2026-04-21.md` and
  `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` as historical inputs rather than
  live backlog sources; the live tracking is in `TODO.md` parts.

Commands run:
- `jj status`
- `rg -n "^- \\[ \\]" TODO.md docs/todo_parts .agents/PLAN.md .agents/plan_parts`
- `wc -l` inventory over current `src/` and `csrc/` source files.
- Targeted inspections of `AUDIT_REPORT_2026-04-21.md`,
  `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md`, `AGENTS.md`, and the split-plan
  artifacts.

Key results:
- The former runtime split queue is stale: the old queue heads are now
  126, 126, 284, 196, 169, and 59 LOC.
- Current source inventory has no `src/` or `csrc/` code file above 1000 LOC;
  the largest observed source file is 762 LOC.

Invalidated assumptions or failed approaches worth preserving:
- Do not treat the 2026-03-19 largest-runtime-files plan as an active work
  driver under the current owner policy.
- Do not spend more cycles on file-splitting churn below 1000 LOC unless a
  semantic ownership boundary independently requires it.

Unresolved issues:
- The remaining open backlog items are implementation/contract work in Vulkan
  ML, UI/FTXUI, REPL multi-client handling, explicit `uv` callback handles,
  and fallback runtime ownership cleanup.

Next actions:
- Continue with a real implementation or contract item from the remaining
  open TODO queue; do not re-enter structural split work from this superseded
  plan.

Signature: GPT-5 Codex

## 2026-04-21 17:50 CEST - ACCESS-UNIFY-INDEX-REF-002 Closed

Objective attempted:
- Finish and close index/ref error-path parity between `(ref coll key)` and
  postfix `expr.[key]`.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_collection_generic_set.c3`
- `src/lisp/jit_dispatch_helpers.c3`
- `src/lisp/jit_compile_effects_modules.c3`
- `src/lisp/tests_core_groups.c3`
- `docs/todo_parts/todo_part_14.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Added direct parity tests in `src/lisp/tests_core_groups.c3` for:
  - array/list/string out-of-bounds errors;
  - array/list/string wrong-index-type errors;
  - dictionary missing-key `nil` behavior;
  - unsupported collection errors.
- Added canonical postfix error-message assertions for array/list/string
  out-of-bounds and wrong-index-type errors.
- Marked `ACCESS-UNIFY-INDEX-REF-002` complete in the TODO backlog and updated
  changelog/plan artifacts.

Commands run:
- Static implementation scans over `ref_try_lookup_collection(...)`,
  `prim_ref(...)`, `jit_do_index(...)`, and `jit_compile_index(...)`.
- Two scoped explorer subagents audited tests and implementation.
- Direct rebuilt-binary probes for representative canonical postfix errors.
- `c3c build`
- `c3c build --obj-out obj`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`

Key results:
- Implementation already shared the runtime negative path:
  `prim_ref(...)` and `jit_do_index(...)` call
  `ref_try_lookup_collection(...)`, and unsupported collection errors use
  shared `ref_type_error(...)`.
- JIT postfix indexing still preserves the prior clobber fix by spilling the
  collection before compiling the index expression.
- `c3c build --obj-out obj` passed and linked `build/main`.
- Direct canonical postfix probes returned `true`.
- The broad basic slice ran the new index tests, but remains red on two
  unrelated tail multi-arg failures: `pass=164 fail=2`.

Invalidated assumptions or failed approaches worth preserving:
- Do not keep `ACCESS-UNIFY-INDEX-REF-002` open solely because dictionary lacks
  out-of-bounds or wrong-index-type errors. Dictionary lookup accepts arbitrary
  key values by contract; the applicable parity case is missing-key `nil`.
- Do not use plain `c3c build` as the recovery build path in the current
  damaged object state. It failed looking for missing `build/obj/linux-aarch64`
  objects; `c3c build --obj-out obj` passed.

Unresolved issues:
- The basic Lisp slice still has unrelated tail multi-arg failures. This pass
  did not investigate or modify that surface.

Next actions:
- Continue from the remaining open TODOs. Good non-Vulkan candidates are
  `FALLBACK-RUNTIME-OWNERSHIP-001`, `REPL-SERVER-MULTICLIENT-001`, or
  `RUNTIME-FILE-SPLIT-QUEUE-001`.

Signature: GPT-5 Codex

## 2026-04-21 12:55 CEST - OWNERSHIP-HARDENING-001 Closed

Objective attempted:
- Codify the completed TEMP/ESCAPE ownership model in project-facing docs and
  close the ownership-hardening parent item.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/ARCHITECTURE.md`
- `docs/C3_STYLE.md`
- `docs/todo_parts/todo_part_01.md`
- `memory/changelog_parts/changelog_part_37.md`
- `.agents/plan_parts/plan_part_05.md`

Code or configuration changes made:
- Added accepted ADR `ADR-2026-04-21-A: Scope/Region Ownership Contract` to
  `docs/ARCHITECTURE.md`.
- Added the ownership-boundary checklist, durable graph/handle policy, and
  static allocation-routing rule to `docs/C3_STYLE.md`.
- Marked all deferred subitems under `OWNERSHIP-HARDENING-001` closed in
  `docs/todo_parts/todo_part_01.md`.
- Updated changelog and active-plan handoff notes.

Commands run:
- Read-only inspections with `rg`, `sed`, and `wc`.

Key results:
- The accepted docs now state `ScopeRegion` ownership, TEMP/ESCAPE lanes, the
  committed ESCAPE no-TEMP-edge invariant, shared boundary promotion context,
  retired `scope_adopt` return flow, no per-type RC for ordinary values,
  opaque-resource exceptions, durable graph policy, handle/ID policy, static
  allocation as optimization, boundary vocabulary, and concrete examples.
- Runtime stats visibility subitem is considered satisfied by the already
  landed `runtime-memory-stats` primitive and `OMNI_MEM_TELEMETRY=1` teardown
  JSON.

Invalidated assumptions or failed approaches worth preserving:
- Do not force future agents to reconstruct the ownership model from memory
  history. `docs/ARCHITECTURE.md` and `docs/C3_STYLE.md` are now the review
  entry points for new graph-carrying runtime shapes.

Unresolved issues:
- None for `OWNERSHIP-HARDENING-001`.

Next actions:
- Continue the remaining open TODO queue, primarily Vulkan/ML roadmap parents
  and `META-PLAN-TODO-BACKFILL-001`.

Signature: GPT-5 Codex

## 2026-04-21 12:38 CEST - Memory Tier 3 Evaluation Closed

Objective attempted:
- Evaluate and close the remaining Tier 3 memory proposal TODO items:
  inline small collections, TEMP lane scope paging, and per-thread scope pools.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/todo_parts/todo_part_15.md`
- `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md`
- `docs/plans/memory-tier3-evaluation-2026-04-21.md`
- `memory/changelog_parts/changelog_part_37.md`
- `.agents/plan_parts/plan_part_05.md`

Code or configuration changes made:
- Added `docs/plans/memory-tier3-evaluation-2026-04-21.md`.
- Updated `docs/plans/README.md` with the new decision note.
- Marked the three Tier 3 `MEMORY-P3-*` TODO items closed as evaluated.
- Added changelog and active-plan handoff notes.

Commands run:
- Read-only inspections with `rg`, `sed`, `nl`, and side-agent review.

Key results:
- Inline small collections: direct inline storage in `Value` is not approved
  without benchmark evidence because it would break the 56-byte `Value` ABI and
  pointer-backed collection payload assumptions. Future code should start, if
  justified, with an `Array` payload-level small-buffer pilot that keeps
  `Value` unchanged.
- Scope paging: 4KB/16KB/64KB TEMP size classes are mechanically plausible, but
  not approved as a tuning policy until class-level telemetry captures chunk
  capacity distribution, release/reset waste, and fiber-temp behavior by
  capacity.
- Per-thread scope pools: same-owner-thread local recycle caches are feasible
  later, but cross-thread scope mobility is blocked by the current scope owner
  invariant and scheduler contract until an explicit transfer policy exists.

Invalidated assumptions or failed approaches worth preserving:
- Do not treat Tier 3 proposal sketches as immediate code instructions. They
  are measurement-gated architecture work.
- Do not increase `Value.sizeof` for inline collection storage without a
  measured bottleneck and a coordinated ABI/JIT/boundary update.
- Do not bypass `scope_guard_owner` or JIT owner-thread checks for per-thread
  scope pools.

Unresolved issues:
- No open `MEMORY-*` proposal TODO items remain in
  `docs/todo_parts/todo_part_15.md`.
- Future memory performance work should begin with telemetry/benchmarks, not
  direct representation or threading rewrites.

Next actions:
- Open new implementation TODOs only after one of these measurements identifies
  a concrete bottleneck: collection length/capacity allocation telemetry,
  chunk-class fragmentation telemetry, or scope global-lock contention
  telemetry.

Signature: GPT-5 Codex

## 2026-04-21 12:21 CEST - MEMORY-P2-TCO-ENV-REUSE Closed

Objective attempted:
- Reduce env-copy churn by reusing target/root-owned env frames in generic
  boundary env-copy and JIT TCO env-copy paths where ownership invariants allow.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/todo_parts/todo_part_15.md`
- `memory/changelog_parts/changelog_part_37.md`
- `.agents/plan_parts/plan_part_05.md`

Code or configuration changes made:
- Added shared env-frame reuse guards in
  `src/lisp/eval_env_copy_frame_helpers.c3`.
- Routed generic boundary env-copy through the reuse guard in
  `src/lisp/eval_env_copy_helpers.c3`.
- Routed JIT TCO env-chain copy through the same reuse guard in
  `src/lisp/jit_eval_scope_copy.c3`.
- Added a memory-lifetime regression that verifies target-owned env frames are
  reused by both `boundary_copy_env_to_target_scope` and `copy_tco_env_chain`
  without increasing the generic or TCO copy-site counters.
- Closed the backlog item and updated the changelog/plan handoff notes.

Commands run:
- `c3c build`
- `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
- `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=tco-recycling ./build/main --test-suite lisp'`

Key results:
- Build passed.
- Bounded container memory-lifetime smoke passed with `pass=234 fail=0`.
- Bounded container TCO recycling passed with `pass=11 fail=0`.

Invalidated assumptions or failed approaches worth preserving:
- Do not treat TCO env reuse as permission to skip binding value copies.
  Releasing-scope frames or binding graphs must still materialize replacements
  and route copied values through the existing copy sites. The closed path is
  whole-frame reuse only for target/root-owned frames whose parent and bindings
  are already reusable.
- A boundary env-copy test must start from the releasing scope. Starting with
  `interp.current_scope` already set to the target makes the wrapper classify
  the target as the releasing scope and correctly blocks target-owned binding
  reuse.

Unresolved issues:
- No Tier 2 residuals remain. Remaining proposal items are Tier 3
  evaluation/design lanes: inline small collections, scope paging, and
  per-thread scope pools.

Next actions:
- Continue with the Tier 3 memory proposal items only if the owner wants to
  proceed beyond the now-closed P0/P1/P2 remediation set.

Signature: GPT-5 Codex

## 2026-04-21 11:48 CEST - MEMORY-P2-JIT-ESCAPE-OPCODES Tail Constructors

Objective attempted:
- Close `MEMORY-P2-JIT-ESCAPE-OPCODES` by adding lane-specific JIT allocation
  helpers and emitting them for proven tail-position return construction.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/jit_apply_helpers.c3`
- `src/lisp/jit_compile_expr_basic.c3`
- `src/lisp/jit_compile_expr_core.c3`
- `src/lisp/jit_compile_expr_dispatch.c3`
- `src/lisp/tests_runtime_feature_jit_groups_failures.c3`
- `src/lisp/tests_runtime_feature_jit_groups_more.c3`

Code or configuration changes made:
- Added JIT ESCAPE allocation helpers for nil, cons, array, and string literal
  return construction, with lightweight helper counters for regression checks.
- Lowered tail `cons`, `List`/`list`, and `Array` calls when the callee is the
  unshadowed global primitive.
- Lowered tail string literals to copy into ESCAPE-owned string storage.
- Preserved normal apply semantics for shadowed constructor names and non-tail
  constructor calls.
- Added a focused JIT policy regression that verifies returned values and that
  the generated code hit the ESCAPE helper counters with zero JIT fallback.
- Marked `MEMORY-P2-JIT-ESCAPE-OPCODES` complete in part 15 and recorded the
  closure in changelog and plan artifacts.

Commands run:
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=tail-constructor-escape-opcode LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(runtime-memory-stats)'`

Key results:
- Build passed.
- Focused JIT policy regression passed with `pass=1 fail=0` and
  `jit_fallback total=0`.
- Full JIT policy slice passed with `pass=52 fail=0`.
- `runtime-memory-stats` still returned the structured telemetry dictionary
  after the JIT changes.

Invalidated assumptions or failed approaches worth preserving:
- Treating tail-call argument-list allocation as sufficient for this item was
  too narrow. The item specifically requires tail-position return construction,
  so the implementation now recognizes constructor calls in `jit_compile_call`
  and lowers those constructors directly.

Unresolved issues:
- None for the proposal-scoped cons/array/string tail-constructor surface.

Next actions:
- Continue with `MEMORY-P2-TCO-LANE-RESET`: add TEMP-lane reset support for
  eligible TCO call scopes while preserving ESCAPE bindings.

Signature: GPT-5 Codex

## 2026-04-21 14:08 CEST - Audit and Memory Proposal TODO Backfill

Objective attempted:
- Read `AUDIT_REPORT_2026-04-21.md`,
  `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md`, and `AGENTS.md`, then populate the
  active TODO backlog with concrete checkbox items.

Relevant workspace or target:
- `/home/christos/Omni`
- `TODO.md`
- `docs/todo_parts/todo_part_15.md`
- `AUDIT_REPORT_2026-04-21.md`
- `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md`
- `AGENTS.md`

Code or configuration changes made:
- Added `docs/todo_parts/todo_part_15.md` as a new backlog part for the
  2026-04-21 audit remediation and memory architecture improvement items.
- Updated `TODO.md` to index Part 15.
- Deduplicated overlapping audit/proposal items:
  - multi-resource `defer` cleanup is tracked as
    `AUDIT-MEM-P0-DEFER-MULTI-RESOURCE`;
  - struct-size assertions are tracked as
    `AUDIT-MEM-P1-STRUCT-SIZE-ASSERTS`;
  - splice reachability validation is tracked as
    `AUDIT-MEM-P1-SPLICE-REACHABILITY`.
- Added open checkbox items for the audit critical/high/medium/low findings
  and memory P0/P1/P2/P3 improvement lanes, with source references, files,
  concrete next steps, constraints, and validation guidance.

Commands run:
- `sed -n '1,260p' AUDIT_REPORT_2026-04-21.md`
- `sed -n '260,620p' AUDIT_REPORT_2026-04-21.md`
- `sed -n '1,320p' memory/MEMORY_IMPROVEMENTS_PROPOSAL.md`
- `sed -n '1,360p' AGENTS.md`
- `rg -n "AUDIT|MEMORY|ML-VK-080|kernel-source-layout|selected-region-plan|memory improvement|churn|negative memory|TODO" TODO.md docs/todo_parts/todo_part_14.md .agents/PLAN.md`
- `rg -n "AUDIT-2026|AUDIT-MEM|MEMORY-P" docs/todo_parts/todo_part_15.md`
- `wc -l docs/todo_parts/todo_part_15.md TODO.md`
- `git diff --check`

Key results:
- Part 15 now contains 29 open backlog items:
  - 21 audit remediation items;
  - 8 memory architecture improvement items.
- `git diff --check` passed after the TODO/index edits.

Unresolved issues:
- The TODO items are backlog population only; no remediation code was changed
  in this step.
- The existing `META-PLAN-TODO-BACKFILL-001` remains open for a full
  `docs/plans/` checkbox-tracking audit.

Next actions:
- Start with the highest-risk items:
  `AUDIT-2026-C1-ADDRINFO-ABI`,
  `AUDIT-2026-C2-RELATION-VALUE-LIFETIME`,
  `MEMORY-P0-PROMOTION-LEAKS`, and
  `MEMORY-P0-SPLICE-SCOPE-GEN`.

Signature: GPT-5 Codex

## 2026-04-21 11:20 CEST - MEMORY-P1-TELEMETRY Runtime Memory Stats

Objective attempted:
- Close `MEMORY-P1-TELEMETRY` by turning existing runtime memory counters into
  a user-visible telemetry surface and adding the missing cumulative counters
  called out by the memory improvement proposal.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/todo_parts/todo_part_15.md`
- `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md`
- `src/scope_region_*.c3`
- `src/lisp/eval*.c3`
- `src/lisp/prim_runtime_memory_stats.c3`

Code or configuration changes made:
- Added `ScopeMemoryTelemetryStats` in the scope layer with TEMP/ESCAPE aligned
  allocation bytes, chunk acquire/free bytes, and scope create/release/destroy
  and recycle counts.
- Wired scope telemetry through allocation, release, destroy, reset, and splice
  paths without changing ownership semantics.
- Added outermost copy-to-parent allocation-byte accounting to
  `CopyToParentStats`.
- Added `runtime-memory-stats`, returning a dictionary with `scope`,
  `scope-transfer`, `fiber-temp`, and `copy-to-parent` sections.
- Added `OMNI_MEM_TELEMETRY=1` JSON teardown output for runtime entry paths and
  the Lisp test runner.
- Added basic/native regressions covering the structured primitive and direct
  scope telemetry counter updates.
- Marked `MEMORY-P1-TELEMETRY` complete in part 15 and recorded the wave in the
  changelog and plan artifacts.

Commands run:
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(runtime-memory-stats)'`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_MEM_TELEMETRY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '1'`

Key results:
- Build passed.
- `runtime-memory-stats` returned a structured dictionary with the expected
  telemetry sections.
- Stage 3 source parity passed after adding the new primitive file to the AOT
  runtime manifest.
- Basic Lisp slice passed with `pass=152 fail=0`.
- `OMNI_MEM_TELEMETRY=1` emitted a teardown JSON object after `--eval '1'`.

Invalidated assumptions or failed approaches worth preserving:
- Existing textual `OMNI_MEM_STATS` output was not enough to close this item:
  it did not provide a user-consumable structured primitive, did not activate
  on `OMNI_MEM_TELEMETRY`, and did not track the proposal's cumulative
  scope-create/recycle or copy-byte counters.

Unresolved issues:
- Six part 15 memory proposal items remain open after this wave:
  `MEMORY-P2-JIT-ESCAPE-OPCODES`, `MEMORY-P2-TCO-LANE-RESET`,
  `MEMORY-P2-TCO-ENV-REUSE`, `MEMORY-P3-INLINE-SMALL-COLLECTIONS`,
  `MEMORY-P3-SCOPE-PAGING`, and `MEMORY-P3-PER-THREAD-SCOPE-POOLS`.

Next actions:
- Continue with `MEMORY-P2-JIT-ESCAPE-OPCODES` or `MEMORY-P2-TCO-LANE-RESET`.
  Use `runtime-memory-stats` before and after copy-reduction work to avoid
  repeating low-signal churn on the same bottleneck.

Signature: GPT-5 Codex

## 2026-04-21 11:05 CEST - Audit Remediation Wave 13

Objective attempted:
- Close `AUDIT-2026-L4-MAGIC-NUMBERS` by naming the audit-listed raw numeric
  values and validating touched areas.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_math.c3`
- `src/lisp/prim_nn_init.c3`
- `src/stack_engine_backend_contract.c3`
- `src/lisp/prim_collection_hashmap_key_helpers.c3`
- `src/lisp/async_io_shared.c3`
- `docs/todo_parts/todo_part_15.md`
- `memory/changelog_parts/changelog_part_37.md`
- `.agents/plan_parts/plan_part_05.md`

Code or configuration changes made:
- Added named constants for the random mantissa mask and `2^52` scale in
  `src/lisp/prim_math.c3`.
- Added named constants for the neural initializer LCG multiplier, increment,
  and `2^32` scale in `src/lisp/prim_nn_init.c3`.
- Added named constants for x86 MXCSR and x87 control-word defaults in
  `src/stack_engine_backend_contract.c3`.
- Added a named time-zone offset hash-bias constant in
  `src/lisp/prim_collection_hashmap_key_helpers.c3`.
- Confirmed the audit-listed async TCP and HTTP buffer sizes were already moved
  to named constants in `src/lisp/async_io_shared.c3`.

Commands run:
- audit-listed raw numeric value scan
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack`
- `git diff --check`

Key results:
- Build passed.
- Basic slice passed with `pass=150 fail=0`.
- Async slice passed with `pass=65 fail=0`.
- Focused advanced collections passed with `pass=1892 fail=0`.
- Stack suite passed with `pass=24 fail=0`.
- Whitespace check passed.

Invalidated assumptions or failed approaches worth preserving:
- None.

Unresolved issues:
- Seven part 15 audit/proposal items remain open after wave 13.

Next actions:
- Continue with `MEMORY-P1-TELEMETRY`, `MEMORY-P2-JIT-ESCAPE-OPCODES`, or
  `MEMORY-P2-TCO-LANE-RESET`.

Signature: GPT-5 Codex

## 2026-04-21 10:45 CEST - Audit Remediation Wave 12

Objective attempted:
- Close `AUDIT-2026-L5-DEBUG-BUILD-SIDE-EFFECTS` and resolve the boundary
  hot-path formatting failure discovered during wave 11.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/jit_compiler_compile.c3`
- `src/lisp/runtime_backend_hooks_cache.c3`
- `src/lisp/jit_eval_scopes.c3`
- `src/lisp/jit_eval_scope_trace.c3`
- `scripts/check_debug_build_side_effects.sh`
- `scripts/check_boundary_hotpath_formatting.sh`
- `src/entry_build_runtime_manifest_lisp_part2.c3`

Code or configuration changes made:
- Verified that the audit-listed `DEBUG_BUILD` blocks only gate diagnostics.
  Required JIT pool-pressure GC scheduling, runtime cache clear/retry/commit,
  and warning-state mutations remain in release code.
- Added `scripts/check_debug_build_side_effects.sh`, which fails if the
  audit-listed JIT/cache `DEBUG_BUILD` blocks contain non-diagnostic
  statements.
- Moved opt-in TCO scope trace formatting out of hot
  `src/lisp/jit_eval_scopes.c3` into `src/lisp/jit_eval_scope_trace.c3`.
- Added `src/lisp/jit_eval_scope_trace.c3` to the guarded AOT Lisp runtime
  manifest.
- Marked `AUDIT-2026-L5-DEBUG-BUILD-SIDE-EFFECTS` complete and updated the
  changelog and active plan.

Commands run:
- `scripts/check_boundary_hotpath_formatting.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_debug_build_side_effects.sh`
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `git diff --check`

Key results:
- Boundary hot-path formatting check passed.
- Debug-build side-effect guard passed.
- Stage 3 source parity passed.
- Build passed.
- JIT policy slice passed with `pass=51 fail=0`.
- Whitespace check passed.

Invalidated assumptions or failed approaches worth preserving:
- The audit-listed `DEBUG_BUILD` blocks were diagnostic-only; the real failing
  local signal was hot-path trace formatting in `jit_eval_scopes.c3`, now moved
  behind helper functions in `jit_eval_scope_trace.c3`.

Unresolved issues:
- Eight part 15 audit/proposal items remain open after wave 12.

Next actions:
- Continue with `AUDIT-2026-L4-MAGIC-NUMBERS`,
  `MEMORY-P1-TELEMETRY`, or `MEMORY-P2-JIT-ESCAPE-OPCODES`.

Signature: GPT-5 Codex

## 2026-04-21 10:20 CEST - Audit Remediation Wave 11

Objective attempted:
- Close `AUDIT-2026-L2-JIT-FILE-NAMES` as a dedicated mechanical JIT file
  naming cleanup.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/jit_*.c3`
- `src/entry_build_runtime_manifest_lisp_part2.c3`
- `scripts/check_effects_contract_policy.sh`
- `scripts/check_boundary_hotpath_formatting.sh`
- `docs/todo_parts/todo_part_15.md`
- `memory/changelog_parts/changelog_part_37.md`
- `.agents/plan_parts/plan_part_05.md`

Code or configuration changes made:
- Renamed every active double-prefix JIT C3 file under `src/lisp/` to the
  single-prefix `jit_*.c3` form.
- Updated active source manifests, scripts, docs/plans, TODOs, audit/proposal
  references, and session/plan artifacts so active source/script/planning
  surfaces no longer reference the redundant JIT file prefix.
- Updated Stage 3 AOT source parity manifest entries in
  `src/entry_build_runtime_manifest_lisp_part2.c3`.
- Converted two remaining legacy `make_error` calls in migrated JIT effect
  surfaces to canonical payloaded runtime errors:
  `src/lisp/jit_handle_signal.c3` and `src/lisp/jit_reset_shift.c3`.

Commands run:
- `for f in src/lisp/jit_jit_*.c3; do mv "$f" "${f/jit_jit_/jit_}"; done`
- active redundant JIT-prefix reference scan
- `c3c build`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `scripts/check_effects_contract_policy.sh`
- `scripts/check_boundary_hotpath_formatting.sh`
- `git diff --check`

Key results:
- No active source/script/planning surfaces scanned still reference the old
  redundant JIT file prefix.
- Build passed.
- Stage 3 source parity passed.
- Compiler slice passed with `pass=290 fail=0`.
- JIT policy slice passed with `pass=51 fail=0`.
- Effects-contract policy passed after the two JIT allocation errors were
  converted to payloaded runtime errors.
- Whitespace check passed.

Invalidated assumptions or failed approaches worth preserving:
- Do not reintroduce double-prefix JIT filenames in active source, scripts,
  manifests, or planning artifacts. The active prefix is `src/lisp/jit_*.c3`.

Unresolved issues:
- `scripts/check_boundary_hotpath_formatting.sh` still fails on debug
  `io::printfn` calls in `src/lisp/jit_eval_scopes.c3`; this is the sharp
  signal for the open `AUDIT-2026-L5-DEBUG-BUILD-SIDE-EFFECTS` item.
- Nine part 15 audit/proposal items remain open after wave 11.

Next actions:
- Continue with `AUDIT-2026-L4-MAGIC-NUMBERS`,
  `AUDIT-2026-L5-DEBUG-BUILD-SIDE-EFFECTS`, or `MEMORY-P1-TELEMETRY`.

Signature: GPT-5 Codex

## 2026-04-21 09:45 CEST - Audit Remediation Waves 9-10

Objective attempted:
- Continue the audit/proposal backlog from `docs/todo_parts/todo_part_15.md`
  after wave 8, focusing on `AUDIT-2026-M5-MODULE-CYCLE` and
  `AUDIT-2026-M6-BUILD-CONFIG`.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/todo_parts/todo_part_15.md`
- `docs/plans/main-lisp-module-cycle-isolation-2026-04-21.md`
- `project.json`
- `scripts/run_e2e.sh`
- `scripts/check_build_config_parity.sh`
- `src/entry_build_backend_compile.c3`
- `src/entry_build_runtime_manifest_lisp_part0.c3`
- `src/entry_build_runtime_manifest_lisp_part3.c3`
- `docs/PROJECT_TOOLING.part-02.md`
- `memory/changelog_parts/changelog_part_37.md`
- `.agents/plan_parts/plan_part_05.md`

Code or configuration changes made:
- Closed `AUDIT-2026-M5-MODULE-CYCLE` by adding an explicit isolation contract
  for the current `main` <-> `lisp` module cycle. The current contract permits
  `main -> lisp` only in entry, CLI, REPL, build, bindgen, source-check, and
  test adapter surfaces, and permits `lisp -> main` only for low-level runtime
  services still hosted in `main`.
- Recorded the future physical break boundary as a neutral runtime module
  extraction around `scope_region*`, `stack_engine*`, and related lifecycle and
  ownership helpers.
- Restored AOT Lisp runtime manifest parity by adding
  `src/lisp/async_io_shared.c3` and `src/lisp/prim_ml_constants.c3` to the
  guarded runtime source manifests.
- Closed `AUDIT-2026-M6-BUILD-CONFIG` by adding FTXUI include roots and missing
  helper C/Vulkan ML sources to `project.json`.
- Added `scripts/check_build_config_parity.sh` to guard JSON parseability,
  FTXUI include-dir coverage, helper-source parity with
  `scripts/build_omni_chelpers.sh`, and e2e/AOT library-path override hooks.
- Replaced hardcoded e2e/AOT host library search paths with
  `OMNI_RUNTIME_TOOLCHAIN_LIB_PATH` and `OMNI_AOT_LINK_LIBRARY_PATH` overrides,
  while preserving `/usr/local/lib` fallback behavior.
- Documented the new runtime/AOT library path overrides in
  `docs/PROJECT_TOOLING.part-02.md`.
- Marked both backlog items complete and updated changelog and active plan
  artifacts.

Commands run:
- `for f in $(rg -l '^module main;' src --glob '*.c3'); do rg -q '^\s*import\s+lisp\b' "$f" && echo "$f"; done | sort`
- `rg -l '^\s*import\s+main\b' src/lisp --glob '*.c3' | sort | wc -l`
- `c3c build`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `python3 -m json.tool project.json`
- `scripts/check_build_config_parity.sh`
- `scripts/build_omni_chelpers.sh`
- `scripts/run_ftxui_smoke.sh`
- `LD_LIBRARY_PATH="${OMNI_RUNTIME_TOOLCHAIN_LIB_PATH:-/usr/local/lib}" ./build/main --help`
- `git diff --check`
- `OMNI_E2E_COMPILE_ONLY=1 scripts/run_validation_container.sh scripts/run_e2e.sh`

Key results:
- Importer scan found 22 `module main` files importing `lisp`.
- Lisp-side scan found 413 `src/lisp` files importing `main`.
- Build passed.
- Stage 3 source parity passed after adding the two missing Lisp runtime
  manifest entries.
- JSON parsing, build-config parity, helper archive build, FTXUI smoke, CLI
  help smoke, and whitespace checks passed.
- Container e2e compile-only did not reach repo build; the validation image
  failed while executing `/usr/local/bin/c3c` with `Exec format error`.

Invalidated assumptions or failed approaches worth preserving:
- Do not try to break the `main` <-> `lisp` cycle by moving random leaf imports.
  The current closure is an explicit isolation contract; a physical break must
  start with a neutral runtime module for shared scope/stack/lifecycle services.
- Do not let `project.json` drift from `scripts/build_omni_chelpers.sh` when
  adding helper C sources or FTXUI include dependencies. Run
  `scripts/check_build_config_parity.sh`.

Unresolved issues:
- Ten part 15 audit/proposal items remain open after wave 10.
- Container e2e compile-only needs a compatible validation `c3c` toolchain or
  image fix before it can serve as a local verification signal here.

Next actions:
- Continue with `AUDIT-2026-L2-JIT-FILE-NAMES`,
  `AUDIT-2026-L4-MAGIC-NUMBERS`, or `AUDIT-2026-L5-DEBUG-BUILD-SIDE-EFFECTS`.

Signature: GPT-5 Codex

## 2026-04-21 10:45 CEST - Audit and Memory Remediation Wave 3

Objective attempted:
- Continue `AUDIT_REPORT_2026-04-21.md` and
  `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` remediation by closing independent
  TODO items from `docs/todo_parts/todo_part_15.md` with parallel agents.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/todo_parts/todo_part_15.md`
- evaluator/boundary provenance, kernel-source, checkpoint, symbol-table,
  type-registry, interp lifecycle, and memory-lifetime boundary tests.

Code or configuration changes made:
- Closed `AUDIT-2026-H2-VALUETAG-EXHAUSTIVE` by making audit-listed
  `ValueTag` switch fallback behavior explicit in evaluator, boundary,
  checkpoint, and JIT literal comparison code.
- Closed `AUDIT-2026-H4-REQUIRE-CONTRACTS` by adding focused `@require`
  contracts to high-risk helper dereferences while leaving intentionally
  null-tolerant boundary probes fail-closed.
- Closed `AUDIT-2026-M7-FAULT-INJECTION-GLOBALS` by gating deterministic
  test-only allocation-failure globals behind `OMNI_TEST_FAULT_INJECTION` and
  keeping normal builds free of those exported symbols.
- Closed `MEMORY-P0-SPLICE-SCOPE-GEN` by adding a root-aware escape splice
  helper that rewrites reachable child ESCAPE generation stamps and owner-scope
  metadata before adoption; commit paths now use that root-aware helper.
- Added a memory-lifetime smoke regression for root/leaf/tail generation stamps
  after root-aware splice adoption.
- Updated TODO, plan, changelog, and this session report with the wave-3
  checkpoint.

Commands run:
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
- `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
- `c3c -D OMNI_TEST_FAULT_INJECTION --obj-out obj-fi build`
- feature-gated basic slice with `OMNI_LISP_TEST_SLICE=basic`
- normal-binary `nm build/main | rg "g_(symbol_table|type_registry|interp)_force"`
- scoped ValueTag default scan over the audit-listed files
- `git diff --check`

Key results:
- Normal build passed and was restored after the feature-gated build.
- Normal basic slice passed with `pass=150 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- Feature-gated basic slice passed with `pass=161 fail=0`.
- Bounded container `memory-lifetime-smoke` passed with `pass=232 fail=0`.
- Normal binary has no exported gated fault-injection globals.
- Scoped ValueTag default scan found no remaining `.tag` switch defaults in
  the audit-listed files.
- Part-15 open item count is now 17.

Invalidated assumptions or failed approaches worth preserving:
- Do not attempt raw ESCAPE chunk generation rewrites for splice adoption:
  escape chunks do not provide per-object metadata. The working approach is a
  root graph walk before adoption, with traversal failure rejecting the splice.
- Do not require contracts on boundary helpers that are intentionally
  null-tolerant test probes; those paths encode fail-closed behavior.

Unresolved issues:
- ASAN was not run in this wave.
- `AUDIT-MEM-P1-SPLICE-REACHABILITY` remains open for debug validation inside
  `scope_splice_escapes`.
- Remaining open part-15 items should continue from the updated plan targets.

Next actions:
- Continue with `AUDIT-MEM-P1-SPLICE-REACHABILITY`,
  `AUDIT-2026-M1-DUPLICATION-CONSTANTS`, and
  `AUDIT-MEM-P0-DEFER-MULTI-RESOURCE`.

Signature: GPT-5 Codex

## 2026-04-21 11:20 CEST - Audit and Memory Remediation Wave 4

Objective attempted:
- Close the next memory hardening item,
  `AUDIT-MEM-P1-SPLICE-REACHABILITY`, without moving Lisp graph knowledge into
  the raw `main` scope allocator layer.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/eval_boundary_graph_audit.c3`
- `src/lisp/eval_boundary_provenance.c3`
- `src/lisp/eval_boundary_commit_escape_helpers.c3`
- `src/lisp/tests_memory_lifetime_boundary_groups.c3`
- `docs/todo_parts/todo_part_15.md`

Code or configuration changes made:
- Added `boundary_debug_graph_audit_pre_splice_escape_root`, a policy-gated
  reachability audit for root-aware splice adoption.
- Root-aware splice now runs the pre-splice audit before stamp rewrite and raw
  `scope_splice_escapes`; releasing-scope TEMP edges are rejected with
  `BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID`.
- Commit paths pass `interp.global_env` through to the root-aware splice helper
  so graph traversal can terminate correctly at known global context.
- Added a memory-lifetime smoke regression that enables graph audit, constructs
  a releasing ESCAPE root with a releasing TEMP edge, and verifies rejection
  before adoption.
- Closed the TODO item and updated plan/changelog counts.

Commands run:
- `c3c build`
- `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`

Key results:
- Build passed.
- Bounded container `memory-lifetime-smoke` passed with `pass=233 fail=0`.
- The new regression emitted the expected pre-splice graph-audit violation and
  still passed.
- Part-15 open item count is now 16.

Invalidated assumptions or failed approaches worth preserving:
- Do not put Lisp committed-root reachability validation directly inside
  `main::scope_splice_escapes`. That layer can move chunks and run TEMP
  teardown, but it cannot traverse Lisp roots or global-env terminals.

Unresolved issues:
- ASAN was not run in this wave.

Next actions:
- Continue with `AUDIT-2026-M1-DUPLICATION-CONSTANTS`,
  `AUDIT-MEM-P0-DEFER-MULTI-RESOURCE`, and `AUDIT-2026-M3-FOREACH-LOOPS`.

Signature: GPT-5 Codex

## 2026-04-21 11:55 CEST - Audit and Memory Remediation Wave 5

Objective attempted:
- Close `AUDIT-2026-M1-DUPLICATION-CONSTANTS` by extracting the audit-listed
  constants and duplicate helper bodies where they affect behavior or clear
  drift.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_ml_activation.c3`
- `src/lisp/prim_ml_autograd.c3`
- `src/lisp/prim_ml_optimizer.c3`
- `src/lisp/prim_ml_optimizer_checkpoint.c3`
- `src/lisp/async_tcp_transport_helpers.c3`
- `src/lisp/http.c3`
- `src/lisp/eval.c3`
- `src/lisp/deduce_tuple_codec.c3`
- `src/lisp/deduce_db_rule_signature_record_codec.c3`

Code or configuration changes made:
- Added `prim_ml_constants.c3` for shared GELU and optimizer default
  constants.
- Added `async_io_shared.c3` for shared TCP/HTTP buffer limits and duplicated
  fiber/loop error raiser bodies.
- Updated TCP/HTTP code to use shared constants/helpers while preserving public
  error codes and messages.
- Merged `eval_error` and `eval_error_expr` through a single
  `eval_error_with_location` constructor.
- Moved tuple encoded-size ownership to `deduce_tuple_codec.c3`; corrected
  symbol tuple sizing to 5 bytes so it matches the encoder's tag plus 4-byte
  symbol id.
- Closed the TODO item and updated plan/changelog counts.

Commands run:
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=async ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=query ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- `git diff --check`

Key results:
- Build passed.
- Basic slice passed with `pass=150 fail=0`.
- Async slice passed with `pass=65 fail=0`.
- Deduce query slice passed with `pass=214 fail=0`.
- Focused advanced collections passed with `pass=1892 fail=0`.
- Whitespace check passed.
- Part-15 open item count is now 15.

Invalidated assumptions or failed approaches worth preserving:
- The old rule-signature tuple size helper undercounted symbol literals as
  3 bytes. Symbol tuple entries must be counted as 5 bytes, matching
  `encode_tuple`.

Unresolved issues:
- Cosmetic helper dedupe outside the audit-listed families was left untouched.

Next actions:
- Continue with `AUDIT-MEM-P0-DEFER-MULTI-RESOURCE`,
  `AUDIT-2026-M3-FOREACH-LOOPS`, and
  `AUDIT-2026-M4-GLOBAL-STATE-CONTRACTS`.

Signature: GPT-5 Codex

## 2026-04-21 14:45 CEST - Audit and Memory Remediation Wave 2

Objective attempted:
- Continue addressing part 15 audit/proposal TODO items with multiple
  subagents on disjoint files plus local H3 integration.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/todo_parts/todo_part_15.md`
- `src/lisp/value_interp_init_helpers.c3`
- `src/lisp/value_environment.c3`
- `src/lisp/value_constructors.c3`
- `src/lisp/eval_repl_server_state.c3`
- `src/lisp/prim_tensor_construct.c3`
- `src/lisp/prim_tensor_construct_aux.c3`
- `src/lisp/async_tcp_transport_helpers.c3`
- `src/entry_runtime_project_paths.c3`
- `src/scope_region_global_guards.c3`
- `src/lisp/value_core_types.c3`
- `src/lisp/value_runtime_types.c3`
- `src/lisp/value_environment_storage.c3`
- `src/lisp/value_interp_state.c3`
- `src/scope_region.c3`

Code or configuration changes made:
- Closed `AUDIT-2026-H3-IGNORED-RETURNS` by removing the audit-listed ignored
  non-void returns and making each outcome explicit:
  - interp macro/module init allocation failure leaves recoverable empty
    storage;
  - env hash table allocation/rebuild failure is documented as a linear lookup
    fallback;
  - FFI handle and REPL session cleanup failures now log warnings;
  - Tensor default dtype inference fails closed on invalid inferred data;
  - TCP listen setup fails and closes the socket when `SO_REUSEADDR` setup
    fails.
- Closed `AUDIT-2026-H6-BOUNDED-CSTR` by bounding project-path and REPL-server
  C string scans with fixed buffer/line limits and fail-closed behavior for
  unterminated inputs.
- Closed `AUDIT-2026-H7-OPTIONAL-UNWRAP` by replacing the global scope mutex
  init force unwrap with an explicit catch path and named invariant abort.
- Closed `AUDIT-MEM-P1-STRUCT-SIZE-ASSERTS` by adding compile-time `$assert`s
  for measured `Value`, `ScopeRegion`, `Closure`, `TensorVal`, `HashMap`,
  `Array`, `Env`, `Interp`, `Primitive`, `SymbolTable`, and `MethodTable`
  sizes.

Commands run:
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=async ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- `timeout 10s env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl --project demo </dev/null`
- `printf '%s\n' '{"id":"1","op":"describe"}' | timeout 10s env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl-server --stdio`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Build passed and linked `build/main`.
- Basic slice passed with `pass=161 fail=0`.
- Async slice passed with `pass=65 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- Focused advanced collections slice passed with `pass=1892 fail=0`.
- Project-path smoke and REPL-server stdio smoke passed.
- Code file-size gate and whitespace checks passed.

Invalidated assumptions or failed approaches worth preserving:
- Do not convert interp macro/module initial allocation failures to assertions:
  existing storage-hardening tests intentionally force those failures and
  expect recoverable empty storage.
- If `c3c build` reports missing incremental object files, rerun after the
  separate-object build path has restored object state; this was an artifact
  issue, not a source compile failure.

Unresolved issues:
- Synthetic unterminated-pointer regressions were not added for H6.
- ASAN was not run for this wave.

Next actions:
- Continue the remaining open part 15 items. The next high-value targets are
  still `AUDIT-2026-H2-VALUETAG-EXHAUSTIVE`,
  `AUDIT-2026-H4-REQUIRE-CONTRACTS`, and
  `MEMORY-P0-SPLICE-SCOPE-GEN`.

Signature: GPT-5 Codex

## 2026-04-21 14:22 CEST - Audit and Memory Remediation Wave 1

Objective attempted:
- Address the first concrete batch of open audit/proposal TODO items using
  parallel subagents and local integration.

Relevant workspace or target:
- `/home/christos/Omni`
- `AUDIT_REPORT_2026-04-21.md`
- `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md`
- `docs/todo_parts/todo_part_15.md`
- `src/lisp/async_tcp_transport_helpers.c3`
- `csrc/addrinfo_helpers.c`
- `src/lisp/deduce_runtime_open.c3`
- `src/lisp/deduce_relation_row_materialization.c3`
- `src/lisp/eval_promotion_copy_wrapper_helpers.c3`
- `src/lisp/eval_promotion_escape_structured.c3`
- `src/lisp/eval_boundary_commit_escape_cons.c3`

Code or configuration changes made:
- Closed `AUDIT-2026-C1-ADDRINFO-ABI` by routing addrinfo connect/render
  behavior through typed C helpers instead of C3 hardcoded struct offsets.
- Closed `AUDIT-2026-C2-RELATION-VALUE-LIFETIME` by changing relation
  column-key caches from long-lived `Value*` pointers to `SymbolId` values and
  constructing root-backed dictionary keys per materialized row.
- Closed `AUDIT-2026-H1-FORMAT-STRINGS` across the audit-listed files with
  C3-native `%d`/`%x` formats. The compiler rejected C-style width spellings
  such as `%ld`, `%zu`, and `%u`.
- Closed `AUDIT-2026-L1-INVALID-SYMBOL-ID` for the audit-listed compiler and
  JIT closure helper files by replacing raw symbol sentinel checks with
  `INVALID_SYMBOL_ID`.
- Closed `MEMORY-P0-PROMOTION-LEAKS` by adding cleanup for partial closure
  clone/promotion failures, cons promotion aborts, destination cons escape
  aborts, and retained env-scope failure paths.
- Updated `docs/todo_parts/todo_part_15.md`,
  `memory/changelog_parts/changelog_part_37.md`, and session reports.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `scripts/check_async_fallback_policy.sh`
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=query ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=async ./build/main --test-suite lisp`
- `OMNI_VALIDATION_TIMEOUT_SEC=180 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Helper rebuild passed.
- Build passed.
- Async fallback policy guard passed.
- Compiler slice passed with `pass=290 fail=0`.
- Deduce query slice passed with `pass=214 fail=0`.
- Async slice passed with `pass=65 fail=0`.
- Bounded container memory-lifetime smoke passed with `pass=231 fail=0`.
- Code file-size gate and whitespace checks passed.

Invalidated assumptions or failed approaches worth preserving:
- Do not mechanically apply C printf width spellings to C3 `io::printf`;
  `%ld`, `%zu`, and `%u` are rejected by the compiler. Use C3-supported
  format grammar and validate with `c3c build`.
- Host-local `memory-lifetime-smoke` remains disallowed by repository policy;
  use the bounded container path for that slice.

Unresolved issues:
- ASAN validation was not run for this remediation wave.
- Raw `0xFFFFFFFF` values outside the audit-listed symbol-id sentinel scope
  remain as future cleanup only if they are proven to be symbol ids.
- The other 25 items in `docs/todo_parts/todo_part_15.md` remain open.

Next actions:
- Continue from the remaining open part 15 items. Highest-value next options
  are `AUDIT-2026-H2-VALUETAG-EXHAUSTIVE`,
  `AUDIT-2026-H3-IGNORED-RETURNS`, or `MEMORY-P0-SPLICE-SCOPE-GEN`.

Signature: GPT-5 Codex

## 2026-04-21 13:46 CEST - ML-VK-080-036 Shared Source Layout and Selected-Region Planner

Objective attempted:
- Close the consolidated high-churn `ML-VK-080-036` work item by defining one
  shared Kernel source/layout contract and one selected-region planner output
  consumed by Tensor command lowering.

Relevant workspace or target:
- `/home/christos/Omni`
- Kernel source validation and runtime dispatch.
- Tensor graph capture, selected-region planning, and graph-run native lowering.
- `docs/todo_parts/todo_part_14.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Added checked `kernel-source-layout` metadata dictionaries for direct
  scale/unary/binary SPIR-V Kernel sources. Construction and `kernel/run`
  runtime dispatch now both validate ABI, descriptor layout, dtype,
  input/output counts, and optional push-layout metadata; the legacy checked
  binary layout symbol remains accepted only for the binary ABI.
- Added `tensor/capture(source)` selected-region planner metadata. Captured
  graphs now include top-level `selected-region-plan` data for native
  scalar-map, tensor-map, direct-view, contract, and concrete mixed
  view/dense-source lowering candidates.
- Added `tensor/run(graph)` selected-region plan consumption. Native
  selected-region executors now require a matching selected-region candidate
  plus validated command-batch metadata; missing or corrupted selected-region
  plan data skips the native path and falls back to serial Vulkan graph replay.
- Updated tests, primitive docs, backlog state, changelog, and active plan.
  `ML-VK-080-036` is closed as the shared planner/source-contract boundary.

Commands run:
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`
- Open-child TODO scan for unchecked `ML-VK-080-0xx` items in
  `docs/todo_parts/todo_part_14.md`

Key results:
- Focused advanced collections passed with `pass=1892 fail=0`.
- Primitive docs parity, Stage 3 source parity, code file-size gate, and diff
  whitespace checks passed.
- The open `ML-VK-080-0xx` TODO scan returned no unchecked child items.
- The remaining source/custom Kernel, selected-region runtime reuse, and
  command-buffer lowering residuals are no longer tracked as separate
  `ML-VK-080` child items. The shared contract and planner are now the
  executable boundary for the selected native region paths already present.

Invalidated assumptions or failed approaches worth preserving:
- Repeating one-off ABI, graph-family, or command-lowering cases for this lane
  was the churn source. Future work should extend the shared source-layout and
  selected-region-plan contracts instead of reopening case-specific residuals.
- Do not inline large direct source-binary execution fixtures into the long
  advanced collections harness; keep constructor/fail-closed coverage there and
  use standalone probes or a non-variadic fixture builder for that runtime path.

Unresolved issues:
- Source-language parsing/compilation, reflection, arbitrary direct-SPIR-V
  descriptor schemas beyond checked scale/unary/binary ABIs,
  memory-plan-backed runtime reuse, arbitrary mixed schedules, and fused
  dispatch execution remain future capability boundaries, not active
  `ML-VK-080` residual children.

Next actions:
- Treat source-language parsing/compilation, arbitrary direct-SPIR-V descriptor
  schemas beyond checked scale/unary/binary ABIs, memory-plan-backed runtime
  reuse, arbitrary mixed schedules, and fused dispatch execution as new future
  capability work, not as unfinished `ML-VK-080-036` residuals.

Signature: GPT-5 Codex

## 2026-04-21 06:58 CEST - ML-VK-080-012 Tensor Command-Batch Planning Metadata

Objective attempted:
- Implement `ML-VK-080-012` as non-executing command-batch planning metadata
  for captured Tensor graphs.

Relevant workspace or target:
- `/home/christos/Omni`
- `.agents/PLAN.md`
- `.agents/SESSION_REPORT.md`
- `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- `docs/todo_parts/todo_part_14.md`
- `docs/LANGUAGE_SPEC.part-01b.md`
- `docs/reference/03-collections.part-01.md`
- `docs/reference/11-appendix-primitives.md`

Code or configuration changes made:
- Updated the Vulkan ML roadmap and TODO slice to split `ML-VK-080-011`
  schedule metadata from `ML-VK-080-012` command-batch planning metadata.
- Extended `tensor/capture(source)` graph plans with `command-batching`,
  `command-batch-count`, `command-batches`, `dispatch-count`, `barrier-count`,
  `batching-policy`, `submission`, and `requires-runtime-support`.
- Added one serial `command-buffer-candidate` batch descriptor for launchable
  `direct-helper` schedule steps. The descriptor records launch node ids,
  schedule steps, external dependencies, dispatch metadata, barrier policy, and
  non-submitted status.
- Hardened capture metadata generation with checked `usz` to `Integer`
  conversion and a fail-closed contract-axis guard.
- Updated focused capture tests, language spec, reference docs, active plan,
  and session report summary to reflect the new slice.

Commands run:
- `sed -n '466,476p' docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- `sed -n '796,812p' docs/todo_parts/todo_part_14.md`
- `sed -n '516,525p' docs/LANGUAGE_SPEC.part-01b.md`
- `sed -n '467,476p' docs/reference/03-collections.part-01.md`
- `sed -n '249,264p' docs/reference/11-appendix-primitives.md`
- `sed -n '1061,1080p' .agents/PLAN.md`
- `sed -n '884,900p' .agents/SESSION_REPORT.md`
- `rg -n "ML-VK-080-012|command-batching 'none|command batching" docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md docs/todo_parts/todo_part_14.md docs/LANGUAGE_SPEC.part-01b.md docs/reference/03-collections.part-01.md docs/reference/11-appendix-primitives.md .agents/PLAN.md .agents/SESSION_REPORT.md`
- `jj status`
- `jj diff --stat`
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- `ML-VK-080-012` now exposes descriptive-only command-batch planning metadata.
- Launchable graphs report `command-batching 'metadata`; non-launching graphs
  report `command-batching 'none`.
- The report and plan now distinguish the schedule-metadata slice from the
  command-batch-metadata slice.
- Validation passed: `c3c build`, focused advanced collections
  `pass=1850 fail=0`, compiler slice `pass=289 fail=0`, basic Lisp
  `pass=161 fail=0`, primitive docs parity, Stage 3 source parity, code
  file-size gate, and `git diff --check`.

Invalidated assumptions or failed approaches worth preserving:
- Treating `command-batching 'none` as part of the earlier schedule-metadata
  slice was too coarse; it needed its own narrower documentation slice.
- The initial subagent patch emitted one pseudo-batch per launch node and
  included malformed C3 string/capacity edits. That approach was discarded in
  favor of a single serial metadata batch descriptor.

Unresolved issues:
- Runtime command-buffer batching, fusion, source compilation, buffer reuse,
  and broader invalidation/capability planning remain open.

Next actions:
- Keep following the existing `ML-VK-080` roadmap with the remaining open
  runtime work once the planning docs are consumed.

Signature: GPT-5 Codex

## 2026-04-21 12:59 CEST - ML-VK-080-033/034/035 Source-Binary ABI and Mixed DAG Lowering

Objective attempted:
- Close the checked source-binary direct-SPIR-V Kernel ABI slice and the
  concrete mixed DAG runtime/command-buffer lowering slices without leaving the
  broader source-language, arbitrary layout, memory-plan reuse, or fused
  lowering work hidden under the same items.

Relevant workspace or target:
- `/home/christos/Omni`
- `.agents/PLAN.md`
- `.agents/SESSION_REPORT.md`
- `docs/todo_parts/todo_part_14.md`
- `memory/changelog_parts/changelog_part_37.md`
- `docs/reference/03-collections.part-01.md`
- `docs/reference/11-appendix-primitives.md`
- Vulkan Tensor graph-run and Kernel source runtime code.

Code or configuration changes made:
- Added checked direct SPIR-V `source-binary-f32-v1` Kernel source validation
  for storage2-output1 `Float32` shaders. `kernel/run` now validates entry,
  ABI, optional `storage2-output1-f32-v1` metadata, Vulkan input placement,
  input/output shapes, and empty push dictionaries before dispatching supplied
  words through a native Vulkan helper.
- Added native selected-region reuse for the concrete mixed graph family:
  source -> transpose-view plus dense source -> tensor-map -> scalar-map*.
  The native helper owns intermediates and returns one final dense Vulkan
  output handle.
- Added command-batch metadata lowering validation for that same mixed graph
  family. Invalid metadata skips the native mixed path and falls back to serial
  graph replay.
- Updated Kernel/Tensor tests, primitive docs, backlog items, changelog, and
  active plan state. Closed `ML-VK-080-033`, `ML-VK-080-034`, and
  `ML-VK-080-035`.
- Churn correction after review: the remaining source/custom Kernel,
  selected-region runtime reuse, and command-buffer lowering work had become
  three mechanism-specific residuals for one broader missing abstraction. The
  plan now consolidates them under `ML-VK-080-036` as one semantic closure
  boundary.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- Standalone `./build/main --eval` probe for optimized binary-add
  `source-binary-f32-v1` direct SPIR-V.

Key results:
- Helper build passed.
- `c3c build` passed.
- Focused advanced collections passed with `pass=1886 fail=0`.
- Standalone source-binary direct-SPIR-V runtime probe returned
  `[vulkan 9.0 16.0]`.
- Source-binary constructor/fail-closed coverage is in the advanced suite;
  the positive runtime execution check is currently a standalone probe.

Invalidated assumptions or failed approaches worth preserving:
- Do not inline large source-binary direct-SPIR-V execution fixtures in the long
  advanced collections harness. Standalone `--eval` execution of the optimized
  389-word binary-add shader returned `[vulkan 9.0 16.0]`, but placing that
  fixture after the direct scale/unary SPIR-V tests corrupted the JIT/apply
  path. Keep constructor/fail-closed coverage in the suite and use a standalone
  probe or a future non-variadic fixture builder for direct source-binary
  runtime validation.

Unresolved issues:
- `ML-VK-080-036`: consolidated source/layout contract plus selected-region
  planner output that command lowering can consume. Sub-boundaries are:
  source-language or arbitrary direct-SPIR-V descriptor layouts beyond checked
  scale/unary/binary ABIs; selected-region runtime reuse beyond the concrete
  transpose-view/dense-source family including memory-plan-backed reuse; and
  command-buffer lowering beyond the concrete mixed region including broader
  mixed schedules and fused dispatch plans.
- Broader invalidation/capability planning remains open.

Next actions:
- Run final broader local gates for this checkpoint: helper build, `c3c build`,
  focused advanced collections, compiler/basic slices, primitive docs parity,
  Stage 3 e2e source parity, file-size gate, and diff whitespace check.
- Continue from the consolidated `ML-VK-080-036` planner/source-contract
  boundary rather than reopening `ML-VK-080-033`, `ML-VK-080-034`, or
  `ML-VK-080-035`, and rather than creating another case-specific residual.

Signature: GPT-5 Codex

### 2026-04-21 13:07 CEST Addendum - Churn Consolidation Correction

- Owner review identified high churn: about 18 case-specific cycles across the
  source/custom Kernel, selected-region runtime reuse, and command-buffer
  lowering lanes.
- Updated `AGENTS.md` with a hard churn recognition and consolidation rule.
- Updated `docs/todo_parts/todo_part_14.md`, `.agents/PLAN.md`,
  `.agents/plan_parts/plan_part_05.md`, and
  `memory/changelog_parts/changelog_part_37.md` so the remaining `ML-VK-080`
  child work is one open consolidation item, `ML-VK-080-036`. Former
  `ML-VK-080-037` and `ML-VK-080-038` are absorbed as sub-boundaries, not
  independent residual work queues.
- Validation after this documentation/planning correction:
  - Open-child TODO scan shows only `ML-VK-080-036` open under `ML-VK-080`.
  - `git diff --check` passed.
- Runtime validation note: a final `c3c build` run after the earlier code work
  was interrupted by owner redirect before completion. No code was changed by
  this churn-consolidation addendum.

Signature: GPT-5 Codex

## 2026-04-21 11:02 CEST - ML-VK-080-030 Source Unary SPIR-V ABI

Objective attempted:
- Close `ML-VK-080-030` by adding a second executable checked direct-SPIR-V
  ABI beyond `source-scale-f32-v1`, then split still-broader source-language
  compilation work into a concrete residual item.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_kernel_source.c3`
- `src/lisp/tensor_vulkan_backend.c3`
- `csrc/tensor_vulkan_helpers_ml_clip.c`
- `csrc/tensor_vulkan_helpers_api_decls.h`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `docs/todo_parts/todo_part_14.md`
- `docs/reference/03-collections.part-01.md`
- `docs/reference/11-appendix-primitives.md`
- `memory/changelog_parts/changelog_part_37.md`
- `.agents/plan_parts/plan_part_05.md`

Code or configuration changes made:
- Added C3 validation and runtime dispatch for checked unary source kernels:
  `source-unary-f32-v1`.
- Added native Vulkan helper entrypoints for registered builtin unary SPIR-V
  and direct word-array unary SPIR-V execution.
- Extended `Kernel.source` validation to accept registered builtin
  `map-unary-f32` and direct `abi 'source-unary-f32-v1`.
- Added focused constructor/runtime coverage for:
  - direct unary ABI validation,
  - registered `map-unary-f32` source execution,
  - direct word-array `source-unary-f32-v1` execution.
- Closed `ML-VK-080-030` in the TODO and split remaining source-language or
  arbitrary descriptor-layout work into `ML-VK-080-033`.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Helper rebuild passed.
- `c3c build` passed.
- Focused advanced collections module passed with `pass=1875 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- Basic Lisp slice passed with `pass=161 fail=0`.
- Primitive docs parity, Stage 3 source parity, code file-size gate, and
  whitespace checks passed.
- Touched code files remain under the 700 LOC code gate:
  - `prim_kernel_source.c3`: 345 LOC
  - `tensor_vulkan_helpers_api_decls.h`: 692 LOC
  - `tensor_vulkan_backend.c3`: 675 LOC
  - `tensor_vulkan_helpers_ml_clip.c`: 232 LOC
  - `tests_advanced_stdlib_module_groups_generic_ops_part9.c3`: 247 LOC

Invalidated assumptions or failed approaches worth preserving:
- Do not inline the full generated all-ops `map-unary-f32` SPIR-V word array
  in a Lisp regression. That 1,499-word test expression caused the advanced
  collections run to segfault after the direct scale test. The direct unary
  regression now uses a smaller generated sqrt shader with the same descriptor
  and push-constant ABI.
- Do not treat `source-unary-f32-v1` as arbitrary custom source compilation.
  It is only a checked one-input/one-output Vulkan `Float32` unary ABI.

Unresolved issues:
- A full unfiltered host `OMNI_LISP_TEST_SLICE=advanced` run still exited
  abnormally before producing a summary in this dirty workspace. The focused
  touched group is green and was used as the governing validation for this
  slice.
- `ML-VK-080-031` remains open for direct-view consumers, mixed DAGs, or
  memory-plan-backed selected-region runtime reuse.
- `ML-VK-080-032` remains open for direct-view consumers, mixed regions, or
  fused command-buffer lowering.
- `ML-VK-080-033` remains open for source-language custom Kernel compilation
  or arbitrary direct-SPIR-V descriptor-layout schemas.

Next actions:
- Continue with `ML-VK-080-031` if the priority is runtime reuse over
  view-consuming or mixed graph regions.
- Continue with `ML-VK-080-032` if the priority is command-buffer lowering for
  view/mixed/fused regions.
- Continue with `ML-VK-080-033` if the priority is text source compilation or
  arbitrary descriptor-layout custom kernels.

Signature: GPT-5 Codex

## 2026-04-21 11:34 CEST - ML-VK-080-031/032 Direct-View Runtime Reuse and Lowering

Objective attempted:
- Close `ML-VK-080-031` and `ML-VK-080-032` for the concrete direct-view
  consumer family: captured all-Vulkan `Float32` source -> direct
  transpose-view -> scalar-map* graphs.

Relevant workspace or target:
- `/home/christos/Omni`
- `csrc/tensor_vulkan_helpers_dispatch_view.c`
- `csrc/tensor_vulkan_helpers_api_decls.h`
- `scripts/build_omni_chelpers.sh`
- `src/lisp/tensor_vulkan_backend_batch.c3`
- `src/lisp/prim_tensor_graph_run.c3`
- `src/lisp/prim_tensor_graph_run_chains.c3`
- `src/lisp/prim_tensor_backend_expr.c3`
- `src/lisp/prim_tensor_map.c3`
- `src/lisp/prim_tensor_device_copy.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `docs/todo_parts/todo_part_14.md`
- `memory/changelog_parts/changelog_part_37.md`
- `.agents/plan_parts/plan_part_05.md`

Code or configuration changes made:
- Added `omni_tensor_backend_vulkan_map_view_scalar_chain_f32`, a native
  helper for transpose-view input plus scalar-map tails. The helper records the
  scalar dispatches into one command buffer, uses view strides for the first
  tensor operand, dense strides for scratch/output tensors, owns scratch
  buffers in C, and transfers one final dense output handle.
- Added graph-run detection and command-batch metadata validation for captured
  source -> transpose-view -> scalar-map* regions.
- Preserved Vulkan transpose-view graph structure in explicit `to-device`
  graph copy and kept Vulkan-touching maps over non-concrete operands lazy
  until that copy path.
- Extended serial graph replay so invalid command-batch metadata can fall back
  through Vulkan transpose-view inputs without hidden CPU fallback.
- Closed `ML-VK-080-031` and `ML-VK-080-032` in the TODO, then split mixed
  DAG/memory-plan runtime reuse into `ML-VK-080-034` and mixed/fused
  command-buffer lowering into `ML-VK-080-035`.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`
- `wc -l csrc/tensor_vulkan_helpers_dispatch_view.c csrc/tensor_vulkan_helpers_api_decls.h src/lisp/prim_tensor_graph_run.c3 src/lisp/prim_tensor_graph_run_chains.c3 src/lisp/prim_tensor_map.c3 src/lisp/prim_tensor_backend_expr.c3 src/lisp/prim_tensor_device_copy.c3 src/lisp/tensor_vulkan_backend_batch.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3 scripts/build_omni_chelpers.sh`

Key results:
- Helper rebuild passed.
- `c3c build` passed.
- Focused advanced collections module passed with `pass=1879 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- Basic Lisp slice passed with `pass=161 fail=0`.
- Primitive docs parity and Stage 3 source parity passed.
- Code file-size gate and `git diff --check` passed.
- Focused regressions confirmed native reuse for valid direct-view scalar
  regions and serial fallback for invalid command-batch metadata.
- Touched code files remain under the 700 LOC code gate; the closest file is
  `csrc/tensor_vulkan_helpers_api_decls.h` at 694 LOC.

Invalidated assumptions or failed approaches worth preserving:
- Do not reuse `omni_tensor_backend_vulkan_map_scalar_chain_f32` for
  transpose-view consumers. It uses one dense metadata shape for output and
  operands, while view consumers need view strides for the first tensor input
  and dense strides for scratch/output.
- Do not set intermediate lazy map expressions over Vulkan views to
  `TENSOR_DEVICE_VULKAN` during construction. That triggers the boundary
  promotion guard on return; preserve the lazy expression structurally until
  explicit `to-device` graph copy.
- Do not assume `to-device 'vulkan` may realize views before capture for this
  family. The graph copy path must preserve transpose-view metadata.

Unresolved issues:
- Mixed DAG selected-region runtime reuse and memory-plan-backed reuse remain
  open under `ML-VK-080-034`.
- Selected mixed-region and fused command-buffer lowering remain open under
  `ML-VK-080-035`.
- Arbitrary source-language custom Kernel compilation and arbitrary direct
  SPIR-V descriptor-layout schemas remain open under `ML-VK-080-033`.

Next actions:
- Continue with `ML-VK-080-034` for mixed DAG or memory-plan-backed runtime
  reuse.
- Continue with `ML-VK-080-035` for selected mixed-region or fused
  command-buffer lowering.
- Continue with `ML-VK-080-033` for arbitrary source-language or descriptor
  schema work.

Signature: GPT-5 Codex

## 2026-04-21 09:01 CEST - ML-VK-080-016/017/018 Runtime Graph Progress

Objective attempted:
- Continue the `ML-VK-080` graph capture/fusion/runtime backlog aggressively
  after session recovery, covering custom source-backed Kernel dispatch,
  contracted reuse planning, and captured Tensor graph execution.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_kernel.c3`
- `src/lisp/prim_kernel_source.c3`
- `src/lisp/prim_tensor_capture.c3`
- `src/lisp/prim_tensor_capture_memory_plan.c3`
- `src/lisp/prim_tensor_graph_run.c3`
- `csrc/tensor_vulkan_helpers_ml_clip.c`
- `docs/todo_parts/todo_part_14.md`

Code or configuration changes made:
- Added a registered-source `Kernel` execution path:
  `source-scale-f32` with `source 'ml-clip-scale-f32` and `entry 'main`
  dispatches through a new native Vulkan source helper.
- Added `tensor/run(graph)` and compiler/runtime primitive registration. The
  executor consumes captured all-Vulkan `Float32` source/map graph dictionaries
  and replays source and map nodes through existing Vulkan helper dispatch.
- Changed capture graph source handling after discovering ordinary dictionaries
  cannot store non-concrete Tensor expression values safely; capture now stores
  executable concrete tensors on `tensor-source` nodes as `source-value`.
- Split capture memory planning into `prim_tensor_capture_memory_plan.c3` and
  extended the nested memory-plan contract to version 2 with metadata-only
  transient reuse groups and explicit `executes-reuse false`.
- Updated primitive docs, collections reference text, TODO, plan, and changelog
  to close the shipped slices and split residuals into `ML-VK-080-019`,
  `ML-VK-080-020`, and `ML-VK-080-021`.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval ...` probes for
  `tensor/capture` and `tensor/run`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `wc -l` on touched C3 source/test files

Key results:
- Build passed after rebuilding `libomni_chelpers`.
- Focused advanced collections slice passed with `pass=1857 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- Primitive docs parity and Stage 3 source parity passed.
- Touched C3 files are below 700 LOC; largest touched C3 file is
  `tensor_vulkan_backend.c3` at 647 LOC.

Invalidated assumptions or failed approaches worth preserving:
- Do not store non-concrete Tensor expression values directly inside ordinary
  graph dictionaries. A probe with `(Dictionary 'source y)` where `y` was a
  Vulkan map expression failed with `Dictionary: out of memory while growing
  backing storage`. Store executable concrete source tensors on source nodes
  and replay from node data instead.
- Do not implement runtime buffer reuse by aliasing or mutating
  `TensorVal.device_handle` in isolation; real reuse must be designed with
  ownership/finalizer semantics.

Unresolved issues:
- Arbitrary source text compilation and user-provided SPIR-V arrays remain
  open as `ML-VK-080-019`.
- Real runtime buffer reuse/ownership transfer remains open as
  `ML-VK-080-020`.
- Contract/view graph execution and command-buffer lowering from capture
  metadata remain open as `ML-VK-080-021`.

Next actions:
- Implement `ML-VK-080-021` if the next priority is broader `tensor/run`
  semantics, starting with contract node replay using existing Vulkan contract
  helpers.
- Implement `ML-VK-080-019` if the next priority is true custom source input,
  choosing either backend text compilation or a checked SPIR-V word-array
  language contract.

Signature: GPT-5 Codex

## 2026-04-21 09:18 CEST - ML-VK-080-019/021 Parallel Continuation

Objective attempted:
- Continue `ML-VK-080` in parallel after recovering the tmux context, using
  subagent findings for source validation, runtime reuse ownership, and broader
  captured graph execution.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_kernel.c3`
- `src/lisp/prim_kernel_source.c3`
- `src/lisp/prim_tensor_graph_run.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `docs/todo_parts/todo_part_14.md`
- `.agents/PLAN.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Extended `Kernel.source` validation so registered-source dictionaries and
  direct SPIR-V dictionaries are accepted as data after strict validation.
  Direct SPIR-V word arrays validate magic, version, id bound, and schema, then
  fail closed for runtime execution until a backend compile/pipeline entrypoint
  exists.
- Extended `tensor/run(graph)` to replay captured all-Vulkan `Float32`
  contract nodes through the existing Vulkan contract helper.
- Extended `tensor/run(graph)` to replay captured direct
  `matrix/transpose-view` nodes as view metadata through the existing
  transpose-view construction path.
- Added focused advanced tests for Vulkan Float32 graph contract replay,
  transpose-view replay, direct SPIR-V source validation, and fail-closed
  direct SPIR-V execution.
- Updated reference docs, TODO, plan, and changelog to close
  `ML-VK-080-019` as validation-only and `ML-VK-080-021` as contract/direct
  view replay, while splitting executable direct SPIR-V/source compilation into
  `ML-VK-080-022` and command-buffer graph lowering into `ML-VK-080-023`.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval ...` probes for
  contract replay and transpose-view replay
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`
- `wc -l` on touched C3 source/test files

Key results:
- Native helper rebuild and `c3c build` passed.
- Focused advanced collections passed with `pass=1861 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- Basic Lisp slice passed with `pass=161 fail=0`.
- Primitive docs parity, Stage 3 source parity, code file-size gate, and
  whitespace checks passed.
- Touched C3 files remain below 700 LOC; largest touched code file checked was
  `tensor_vulkan_backend.c3` at 647 LOC.

Invalidated assumptions or failed approaches worth preserving:
- Do not treat direct SPIR-V source validation as executable custom source
  support. The data contract is now checked, but runtime execution still needs
  a backend compile/pipeline entrypoint.
- Do not implement runtime reuse by mutating C3 `TensorVal.device_handle`
  fields or aliasing handles. The preferred next boundary is an opaque native
  Vulkan executor that owns scratch buffers and transfers only the final output
  handle to a fresh Tensor.
- Do not replay direct transpose-view nodes by materializing a dense transpose
  inside `tensor/run`; direct view replay should preserve view metadata.

Unresolved issues:
- `ML-VK-080-020`: real runtime buffer reuse/ownership transfer remains open.
- `ML-VK-080-022`: executable direct SPIR-V or source-language custom Kernel
  compilation remains open.
- `ML-VK-080-023`: command-buffer lowering for captured Tensor graph metadata
  remains open, including any required view-consuming map/contract semantics.

Next actions:
- Implement `ML-VK-080-020` through an opaque native map-chain or graph
  executor if buffer reuse is the next priority.
- Implement `ML-VK-080-022` only after adding a backend compile/pipeline
  entrypoint for validated direct SPIR-V or an explicit source-language
  compiler contract.
- Implement `ML-VK-080-023` by lowering validated schedule/command-batch
  records to a runtime executor without hidden CPU fallback.

Signature: GPT-5 Codex

## 2026-04-21 09:40 CEST - ML-VK-080-020 Tensor Run Native Reuse Boundary

Objective attempted:
- Continue from the validated `ML-VK-080-019/021` checkpoint and implement the
  next safe runtime reuse slice without crossing into C3 handle aliasing or
  general buffer pooling.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_tensor_graph_run.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `docs/todo_parts/todo_part_14.md`
- `.agents/PLAN.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Added a `tensor/run(graph)` fast path for captured all-Vulkan `Float32`
  source -> scalar map -> scalar map graphs.
- The fast path validates the graph shape and scalar map metadata, then calls
  the native Vulkan two-map command-buffer batch helper. Native code borrows
  the source handle, owns the intermediate scratch buffer, and transfers one
  final output handle to a fresh Tensor.
- Added a focused advanced test proving numeric correctness, source
  immutability, Vulkan placement, and native reuse-path execution via the
  existing map-chain dispatch counter.
- Updated reference docs, TODO, plan, and changelog to close `ML-VK-080-020`
  as the initial native reuse boundary and split broader reuse into
  `ML-VK-080-024`.

Commands run:
- `c3c build`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`
- `wc -l` on touched C3/C files

Key results:
- Build passed.
- Focused advanced collections passed with `pass=1863 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- Basic Lisp slice passed with `pass=161 fail=0`.
- Primitive docs parity, Stage 3 source parity, code file-size gate, and
  whitespace checks passed.
- Touched code files remain below 700 LOC; `prim_tensor_graph_run.c3` is 480
  LOC and the test shard is 175 LOC.

Invalidated assumptions or failed approaches worth preserving:
- Do not represent this as general memory-plan execution. The capture
  `memory-plan` remains metadata-only and still reports `executes-reuse false`.
- Do not generalize the current C3 fast path into handle aliasing. Broader
  reuse still needs a native opaque executor boundary with explicit scratch
  ownership and cleanup counters.

Unresolved issues:
- `ML-VK-080-022`: executable direct SPIR-V or source-language custom Kernel
  compilation remains open.
- `ML-VK-080-023`: general command-buffer lowering for captured Tensor graph
  metadata remains open.
- `ML-VK-080-024`: generalized runtime reuse beyond the initial two-scalar-map
  graph boundary remains open.

Next actions:
- If continuing reuse, implement `ML-VK-080-024` as a native opaque executor
  for longer linear map chains or selected graph regions, with failure cleanup
  counters and no C3 `TensorVal.device_handle` mutation.
- If continuing command-buffer work, implement `ML-VK-080-023` by lowering
  validated schedule/command-batch records rather than special-casing graph
  shapes one by one.

Signature: GPT-5 Codex

## 2026-04-21 10:03 CEST - ML-VK-080-024 Linear Scalar-Map Runtime Reuse

Objective attempted:
- Continue runtime reuse after `ML-VK-080-020` by replacing the two-map graph
  special case with a native scalar-map-chain executor for longer captured
  linear map chains.

Relevant workspace or target:
- `/home/christos/Omni`
- `csrc/tensor_vulkan_helpers_dispatch_batch.c`
- `csrc/tensor_vulkan_helpers_api_decls.h`
- `src/lisp/tensor_vulkan_backend_batch.c3`
- `src/lisp/prim_tensor_graph_run.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`

Code or configuration changes made:
- Added `omni_tensor_backend_vulkan_map_scalar_chain_f32`, a native Vulkan
  scalar-chain executor that accepts arrays of scalar values, scalar modes, and
  map op ids.
- The native executor records all scalar-map dispatches into one command
  buffer, inserts barriers between dispatches, destroys all intermediate
  scratch buffers, and transfers one final output handle.
- Reworked `tensor/run(graph)` scalar-map reuse detection to accept captured
  all-Vulkan `Float32` source -> scalar-map* graphs with two or more map nodes.
- Added a focused longer-chain test that verifies native scalar-chain counter
  use, numeric correctness, Vulkan placement, and captured source immutability.
- Updated references, TODO, plan, and changelog to close `ML-VK-080-024` and
  split broader reuse into `ML-VK-080-025`.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`
- `wc -l` on touched C/C3 files

Key results:
- Native helper rebuild passed.
- Build passed.
- Focused advanced collections passed with `pass=1865 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- Basic Lisp slice passed with `pass=161 fail=0`.
- Primitive docs parity, Stage 3 source parity, code file-size gate, and
  whitespace checks passed.
- Touched code files remain below 700 LOC; `csrc/tensor_vulkan_helpers_api_decls.h`
  is 608 LOC, `prim_tensor_graph_run.c3` is 475 LOC, and
  `tensor_vulkan_helpers_dispatch_batch.c` is 348 LOC.

Invalidated assumptions or failed approaches worth preserving:
- The two-map graph-run special case is no longer the right default boundary.
  Prefer the native scalar-chain executor for captured linear scalar-map
  graphs and keep C3 responsible only for validating graph shape and metadata.
- Do not treat this as selected-region graph reuse. Tensor/tensor maps,
  contract/view consumers, and mixed graph regions still need separate native
  ownership and cleanup design.

Unresolved issues:
- `ML-VK-080-022`: executable direct SPIR-V or source-language custom Kernel
  compilation remains open.
- `ML-VK-080-023`: command-buffer lowering for captured Tensor graph metadata
  remains open.
- `ML-VK-080-025`: generalized runtime reuse beyond linear scalar-map chains
  remains open.

Next actions:
- Continue with `ML-VK-080-023` for command-batch metadata lowering, or
  `ML-VK-080-025` for native selected-region reuse.

Signature: GPT-5 Codex

## 2026-04-21 10:16 CEST - ML-VK-080-023 Scalar-Map Command-Batch Metadata Lowering

Objective attempted:
- Continue from the linear scalar-map reuse checkpoint by making the native
  `tensor/run` scalar-chain path consume captured command-batch metadata
  instead of relying on node shape alone.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_tensor_graph_run.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `docs/todo_parts/todo_part_14.md`
- `.agents/PLAN.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Added command-batch validation for the scalar-chain `tensor/run` lowering
  path. The executor now requires one `command-buffer-candidate` batch with
  matching launch/dispatch counts, barrier policy, batch node ids, dispatch
  node ids, dispatch operations, and the external source dependency.
- If captured command-batch metadata is missing or invalid, `tensor/run`
  skips the native scalar-chain command-buffer path and falls back to serial
  helper replay.
- Added a focused test that mutates the captured batch execution marker,
  verifies numeric fallback still succeeds, and asserts the native scalar-chain
  dispatch counter is unchanged.
- Updated references, TODO, plan, and changelog to close `ML-VK-080-023` for
  scalar-map command-batch metadata lowering and split broader command-buffer
  lowering into `ML-VK-080-026`.

Commands run:
- `c3c build`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Build passed.
- Focused advanced collections passed with `pass=1867 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- Basic Lisp slice passed with `pass=161 fail=0`.
- Primitive docs parity, Stage 3 source parity, code file-size gate, and
  whitespace checks passed.
- Touched code files remain below 700 LOC; `prim_tensor_graph_run.c3` is 523
  LOC and the test shard is 207 LOC after this slice.

Invalidated assumptions or failed approaches worth preserving:
- Do not infer command-buffer execution from graph node shape alone. For the
  scalar-map path, captured `command-buffer-candidate` metadata must validate
  before lowering to the native command-buffer executor.
- Invalid command-batch metadata should not break numerically supported serial
  replay when the graph nodes themselves remain executable.

Unresolved issues:
- `ML-VK-080-022`: executable direct SPIR-V or source-language custom Kernel
  compilation remains open.
- `ML-VK-080-025`: selected-region runtime reuse beyond linear scalar-map
  chains remains open.
- `ML-VK-080-026`: command-buffer lowering beyond scalar-map command-batch
  metadata remains open.

Next actions:
- Continue with `ML-VK-080-026` if the next priority is broader
  command-buffer lowering, or `ML-VK-080-025` for selected-region reuse.

Signature: GPT-5 Codex

## 2026-04-21 09:56 CEST - ML-VK-080-025 Tensor-Map Selected-Region Runtime Reuse

Objective attempted:
- Continue from scalar-map command-batch lowering by extending
  `tensor/run(graph)` runtime reuse beyond linear scalar-map chains to a
  selected tensor/tensor map region followed by scalar maps.

Relevant workspace or target:
- `/home/christos/Omni`
- `csrc/tensor_vulkan_helpers_dispatch_batch.c`
- `csrc/tensor_vulkan_helpers_api_decls.h`
- `src/lisp/tensor_vulkan_backend_batch.c3`
- `src/lisp/prim_tensor_graph_run.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `docs/todo_parts/todo_part_14.md`
- `.agents/PLAN.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Added `omni_tensor_backend_vulkan_map_tensor_scalar_chain_f32`, a native
  Vulkan selected-region executor for dense same-shape `Float32` tensor/tensor
  map -> scalar-map* regions with two concrete all-Vulkan source tensors.
- The native executor records the tensor/tensor map and following scalar maps
  into one command buffer, inserts barriers between dispatches, destroys
  intermediate scratch buffers, and transfers one final output handle to C3.
- Added `tensor/run(graph)` detection and metadata validation for the selected
  region. It requires matching `command-buffer-candidate` dispatch metadata and
  falls back to serial helper replay when metadata is invalid but the nodes are
  otherwise supported.
- Added a focused regression that verifies native counter movement, numeric
  correctness, Vulkan placement, and captured source immutability.
- Updated references, TODO, plan, and changelog to close `ML-VK-080-025` for
  dense tensor/tensor map plus scalar tails and split broader selected-region
  reuse into `ML-VK-080-027`.
- Closed `ML-VK-080-026` for the same concrete command-buffer metadata
  lowering boundary, because `tensor/run(graph)` now validates and lowers
  dense tensor/tensor map -> scalar-map* `command-buffer-candidate` batches.
  Broader command-buffer lowering moved to `ML-VK-080-028`.
- Split native chain-detection helpers from `prim_tensor_graph_run.c3` into
  `prim_tensor_graph_run_chains.c3` and added the new file to the AOT runtime
  manifest. This leaves room for the next runtime-reuse slice under the hard
  code-file gate.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`
- `wc -l` on touched C/C3 files
- After the chain-helper split: `c3c build`
- After the chain-helper split: focused advanced collections slice
- After the chain-helper split: `scripts/check_file_size_gate.sh`
- After the chain-helper split: `git diff --check`

Key results:
- Native helper rebuild passed.
- Build passed.
- Focused advanced collections passed with `pass=1869 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- Basic Lisp slice passed with `pass=161 fail=0`.
- Primitive docs parity, Stage 3 source parity, code file-size gate, and
  whitespace checks passed.
- Touched code files remain below 700 LOC; `prim_tensor_graph_run.c3` is 676
  LOC and `tensor_vulkan_helpers_dispatch_batch.c` is 541 LOC after this slice.
- After the chain-helper split, `prim_tensor_graph_run.c3` is 533 LOC and
  `prim_tensor_graph_run_chains.c3` is 146 LOC. Rebuild, focused advanced
  collections `pass=1869 fail=0`, file-size gate, and whitespace checks passed.

Invalidated assumptions or failed approaches worth preserving:
- Do not treat linear scalar-map reuse as the neutral selected-region boundary.
  Dense tensor/tensor map plus scalar tails now have their own native opaque
  executor with native scratch ownership.
- Do not generalize this result to contract/view-consuming regions, arbitrary
  multi-source DAGs, or memory-plan-backed reuse. Those remain separate
  ownership and cleanup problems under `ML-VK-080-027`.
- A captured contract -> scalar-map graph does expose a single command-batch
  candidate, but lowering that whole region correctly requires a
  contract-aware native executor. Do not count serial contract materialization
  plus scalar-tail reuse as full selected-region command-buffer execution.

Unresolved issues:
- `ML-VK-080-029`: source-language custom Kernel compilation or broader
  direct-SPIR-V ABI contracts remain open.
- `ML-VK-080-027`: selected-region runtime reuse beyond dense tensor/tensor
  map plus scalar-map tails remains open.
- `ML-VK-080-028`: command-buffer lowering beyond dense tensor/tensor map
  selected regions remains open.

Next actions:
- Continue with `ML-VK-080-028` or `ML-VK-080-027` depending on whether the
  next priority is command-buffer lowering breadth or runtime reuse breadth.

Signature: GPT-5 Codex

## 2026-04-21 10:16 CEST - ML-VK-080-022 Direct SPIR-V Kernel Execution

Objective attempted:
- Continue custom Kernel source execution by making validated direct SPIR-V
  word arrays executable for the checked Vulkan `source-scale-f32` ABI.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_kernel_source.c3`
- `src/lisp/tensor_vulkan_backend.c3`
- `csrc/tensor_vulkan_helpers_ml_clip.c`
- `csrc/tensor_vulkan_helpers_api_decls.h`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `docs/todo_parts/todo_part_14.md`
- `.agents/PLAN.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Added `omni_tensor_backend_vulkan_kernel_source_scale_f32_spirv`, a native
  Vulkan helper that validates a direct SPIR-V header, builds a compute
  pipeline from the supplied words, and dispatches the existing two-buffer
  scale ABI.
- Added the C3 extern and updated `kernel/run` source handling so direct
  `Kernel.source.words` arrays are copied into temporary native `uint32_t`
  scratch before dispatch.
- Removed the old direct-SPIR-V pre-execution rejection for `source-scale-f32`.
- Added a focused regression that passes the built-in scale shader as a direct
  SPIR-V word array and verifies Vulkan placement plus numeric results.
- Updated references, TODO, plan, and changelog to close `ML-VK-080-022` for
  checked direct-SPIR-V execution and split broader source-language or
  multi-layout ABI work into `ML-VK-080-029`.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `git diff --check`
- `wc -l` on touched C/C3 files
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`

Key results:
- Native helper rebuild passed.
- Build passed.
- Focused advanced collections passed with `pass=1870 fail=0`.
- Whitespace check passed.
- Touched code files remain below 700 LOC; `tensor_vulkan_backend.c3` is 657
  LOC, `prim_kernel_source.c3` is 233 LOC, and
  `tensor_vulkan_helpers_api_decls.h` is 633 LOC.
- Compiler slice passed with `pass=290 fail=0`.
- Basic Lisp slice passed with `pass=161 fail=0`.
- Primitive docs parity, Stage 3 source parity, and code file-size gate passed.

Invalidated assumptions or failed approaches worth preserving:
- Direct SPIR-V word-array source validation is no longer only data-level for
  the checked `source-scale-f32` ABI. Treat it as executable when the Kernel
  shape, descriptor, push, placement, and entry constraints validate.
- Do not generalize this to arbitrary source languages, descriptor layouts, or
  custom operations. Those still need an explicit ABI/source contract under
  `ML-VK-080-029`.

Unresolved issues:
- `ML-VK-080-027`: selected-region runtime reuse beyond dense tensor/tensor
  map plus scalar-map tails remains open.
- `ML-VK-080-028`: command-buffer lowering beyond dense tensor/tensor map
  selected regions remains open.
- `ML-VK-080-029`: source-language custom Kernel compilation or broader
  direct-SPIR-V ABI contracts remain open.

Next actions:
- Continue with `ML-VK-080-027`, `ML-VK-080-028`, or `ML-VK-080-029`
  depending on whether the next priority is selected-region reuse, graph
  command-buffer lowering, or broader custom Kernel source semantics.

Signature: GPT-5 Codex

## 2026-04-21 10:37 CEST - ML-VK-080-027/028/029 Closure

Objective attempted:
- Address the remaining open graph-capture/source work items:
  `ML-VK-080-027`, `ML-VK-080-028`, and `ML-VK-080-029`.

Relevant workspace or target:
- `/home/christos/Omni`
- `csrc/tensor_vulkan_helpers_contract_region.c`
- `csrc/tensor_vulkan_helpers_dispatch_batch.c`
- `csrc/tensor_vulkan_helpers_api_decls.h`
- `scripts/build_omni_chelpers.sh`
- `src/lisp/prim_tensor_graph_run.c3`
- `src/lisp/prim_tensor_graph_run_chains.c3`
- `src/lisp/prim_kernel_source.c3`
- `src/lisp/tensor_vulkan_backend_batch.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `docs/todo_parts/todo_part_14.md`
- `.agents/PLAN.md`
- `.agents/plan_parts/plan_part_05.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Added a native Vulkan contract -> scalar-map* selected-region executor for
  captured all-Vulkan `Float32` graphs.
- Replaced the temporary two-helper composition with one command-buffer
  recording: contract dispatch, shader-write/read barrier, scalar-tail
  dispatches, and scalar-tail barriers in one queue submission.
- Added C3 graph-run detection and command-batch metadata validation for the
  contract-scalar selected region before routing to the native helper.
- Added explicit direct-SPIR-V source ABI validation:
  `abi 'source-scale-f32-v1` is accepted, omitted ABI remains compatible with
  that same checked scale ABI, and other ABI values fail closed.
- Added focused regression coverage for contract-scalar selected-region
  runtime dispatch and explicit direct-SPIR-V ABI validation.
- Closed `ML-VK-080-027`, `ML-VK-080-028`, and `ML-VK-080-029` on their
  shipped boundaries and split broader residual work into `ML-VK-080-030`,
  `ML-VK-080-031`, and `ML-VK-080-032`.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`
- `wc -l` on touched C/C3 files

Key results:
- Native helper rebuild passed.
- Build passed.
- Focused advanced collections passed with `pass=1873 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- Basic Lisp slice passed with `pass=161 fail=0`.
- Primitive docs parity passed.
- Stage 3 source parity passed.
- File-size gate passed.
- `git diff --check` passed after removing trailing spaces from the existing
  added `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` artifact.
- Touched code files remain below 700 LOC; the largest newly changed C helper
  is `csrc/tensor_vulkan_helpers_contract_region.c` at 474 LOC.

Unresolved issues:
- Source-language compilation and direct-SPIR-V descriptor layouts beyond
  `source-scale-f32-v1` remain open under `ML-VK-080-030`.
- Direct-view consumers, mixed DAGs, and memory-plan-backed selected-region
  runtime reuse remain open under `ML-VK-080-031`.
- Direct-view, mixed-region, and fused command-buffer lowering remain open
  under `ML-VK-080-032`.

Next actions:
- Start from `ML-VK-080-030`, `ML-VK-080-031`, or `ML-VK-080-032` depending
  on whether source ABI, runtime reuse, or broader command lowering is the next
  priority. Do not reopen `ML-VK-080-027/028/029` unless their shipped
  contracts regress.

Signature: GPT-5 Codex

## 2026-04-21 08:05 CEST - ML-VK-080-015 Vulkan Map Chain Command-Buffer Batch

Objective attempted:
- Implement `ML-VK-080-015` as the first executable Tensor command-buffer
  batching slice for Vulkan `Float32` map expressions.

Relevant workspace or target:
- `/home/christos/Omni`
- `csrc/tensor_vulkan_helpers_dispatch_batch.c`
- `src/lisp/prim_tensor_vulkan_map_batch.c3`
- `src/lisp/prim_tensor_cuda_map_expr.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `docs/todo_parts/todo_part_14.md`
- `.agents/PLAN.md`

Code or configuration changes made:
- Added a native Vulkan helper for two scalar Float32 map dispatches recorded
  into one command buffer with one intermediate shader-write/read barrier and
  one queue submission.
- Added C3 extern wiring and a runtime map-chain detector used by
  `tensor_map_try_vulkan_value` before the older one-helper-per-op path.
- Added build-manifest entries for the new C helper and C3 runtime files.
- Added focused coverage that checks numeric correctness and asserts the new
  chain dispatch counter increments when Vulkan Float32 is available.
- Closed `ML-VK-080-015` and split general captured graph execution into
  `ML-VK-080-018`.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Build passed.
- Focused advanced collections passed with `pass=1854 fail=0`.
- Compiler slice passed with `pass=289 fail=0`.
- Basic Lisp passed with `pass=161 fail=0`.
- Primitive docs parity, Stage 3 source parity, code file-size gate, and
  whitespace checks passed.

Invalidated assumptions or failed approaches worth preserving:
- Starting from an already concrete Vulkan tensor does not preserve a two-map
  expression for batching; existing `map` direct dispatch eagerly materializes
  one operation at a time. The executable batching path is an explicit
  CPU-expression to `to-device 'vulkan` graph route followed by `realize`.

Unresolved issues:
- `tensor/capture` graph dictionaries remain metadata-only and are not
  executable.
- General captured Tensor graph execution is now tracked as `ML-VK-080-018`.
- Source-backed custom Kernel compilation/dispatch and contracted runtime
  buffer reuse remain open.

Next actions:
- Continue `ML-VK-080-018` if the next priority is arbitrary captured graph
  execution, or `ML-VK-080-016` if source-backed custom Kernel compilation is
  the priority.

Signature: GPT-5 Codex

## 2026-04-21 07:46 CEST - ML-VK-080-014 Tensor Fusion Eligibility Metadata

Objective attempted:
- Implement `ML-VK-080-014` as metadata-only fusion eligibility planning for
  captured Tensor graphs, without claiming fused execution.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_tensor_capture.c3`
- `src/lisp/prim_tensor_capture_fusion.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `.agents/PLAN.md`
- `.agents/SESSION_REPORT.md`
- `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- `docs/todo_parts/todo_part_14.md`
- `docs/LANGUAGE_SPEC.part-01b.md`
- `docs/reference/03-collections.part-01.md`
- `docs/reference/11-appendix-primitives.md`

Code or configuration changes made:
- Added `prim_tensor_capture_fusion.c3` to classify captured Tensor graph fusion
  eligibility without growing `prim_tensor_capture.c3` past the code-size gate.
- Added nested metadata-only `fusion-plan` dictionaries to
  `tensor/capture(source)` graph plans.
- Marked direct Vulkan Float32 map chains as eligibility-only `map-chain`
  candidates when all launchable nodes are map nodes.
- Marked contract and direct transpose-view nodes as hard fusion barriers with
  `contract-boundary` and `view-boundary` reasons.
- Updated focused capture tests and ML-VK-080 roadmap/TODO/spec/reference docs.

Commands run:
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Build passed.
- Focused advanced collections passed with `pass=1852 fail=0`.
- Compiler slice passed with `pass=289 fail=0`.
- Basic Lisp passed with `pass=161 fail=0`.
- Primitive docs parity, Stage 3 source parity, code file-size gate, and
  whitespace checks passed.
- `prim_tensor_capture.c3` is 668 LOC and the new
  `prim_tensor_capture_fusion.c3` is 110 LOC, keeping the code-only 700-line
  rule intact.

Invalidated assumptions or failed approaches worth preserving:
- Do not present fusion metadata as fused execution. The current Vulkan helper
  layer still dispatches one helper-backed op at a time; fused shaders and
  fused dispatch need a new runtime lowering path.
- Executable Tensor command-buffer batching is not available from capture
  metadata alone; it needs a native/runtime executor bridge that consumes
  schedule/command-batch records.
- Source-backed custom Kernel compilation remains blocked on a backend compile
  and pipeline entrypoint.
- The current memory plan is descriptive only; real buffer reuse must move
  together with Tensor ownership/finalizer and backend allocation semantics.

Unresolved issues:
- Runtime fused execution, executable command-buffer batching, source-backed
  Kernel compilation/dispatch, and contracted buffer reuse/lifetime planning
  remain open.

Next actions:
- Continue the remaining `ML-VK-080` runtime work from the roadmap: executable
  Tensor command-buffer batching, source-backed custom Kernel compilation,
  runtime fused dispatch, and contracted buffer reuse/lifetime planning.

Signature: GPT-5 Codex

## 2026-04-21 15:45 CEST - Audit Remediation Wave 6

Objective attempted:
- Close `AUDIT-MEM-P0-DEFER-MULTI-RESOURCE` from part 15 by replacing
  branch-local multi-resource cleanup with scoped `defer` guards.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/todo_parts/todo_part_15.md`
- `src/lisp/value_tensor_clone.c3`
- `src/lisp/eval_promotion_copy_route_helpers.c3`
- `src/lisp/eval_promotion_escape_structured.c3`

Code or configuration changes made:
- Added a commit-guarded cleanup path to `tensor_clone_payload`, covering view,
  Vulkan-device, CUDA-device, concrete, map-scalar, and contract-axis clone
  returns.
- Replaced repeated manual partial cleanup calls in shared wrapper parent-copy
  paths with deferred guards that track copied array items, processed hashmap
  slots, copied method-table entries, and fallback ownership.
- Applied the same guard pattern to escape-promotion wrappers.
- Corrected method-table abort cleanup state so implementation copy/promotion
  failures after a successful signature copy destroy the current copied
  signature during rollback.
- Marked `AUDIT-MEM-P0-DEFER-MULTI-RESOURCE` complete in part 15 and recorded
  the wave in changelog and plan artifacts.

Commands run:
- `c3c build`
- `scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=memory-lifetime-smoke LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `c3c build --sanitize=address`
- `git diff --check`
- `jj status`

Key results:
- Normal build passed.
- Bounded container memory-lifetime smoke passed with `pass=233 fail=0`,
  including tensor clone, root-boundary shared-wrapper clone, escape promotion,
  method-table abort cleanup, and wrapper partial cleanup coverage.
- Whitespace check passed.
- ASAN did not run: the local `c3c` toolchain rejected sanitizer mode before
  building with `Address sanitizer is only supported on Linux, FreeBSD, NetBSD, Darwin and Windows.`

Invalidated assumptions or failed approaches worth preserving:
- Do not treat method-table implementation-copy failure as if no current entry
  needs cleanup. Once a signature copy succeeds, the current entry must be
  included in partial cleanup even if the implementation value has not been
  installed yet.

Unresolved issues:
- ASAN validation remains unavailable in this local toolchain invocation.
- Fourteen part 15 audit/proposal items remain open after this wave.

Next actions:
- Continue with `AUDIT-2026-M3-FOREACH-LOOPS`,
  `AUDIT-2026-M4-GLOBAL-STATE-CONTRACTS`, or
  `AUDIT-2026-M5-MODULE-CYCLE` as the next small remediation wave.

Signature: GPT-5 Codex

## 2026-04-21 16:08 CEST - Audit Remediation Wave 7

Objective attempted:
- Close `AUDIT-2026-M3-FOREACH-LOOPS` by converting practical manual index
  loops to `foreach` without changing loops whose indices carry semantics.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/todo_parts/todo_part_15.md`
- `src/lisp/value_print_buf.c3`
- `src/lisp/prim_kernel_source.c3`
- `src/lisp/prim_tensor_capture.c3`
- `src/lisp/prim_nn_checkpoint.c3`
- `src/lisp/eval_env_copy_values.c3`

Code or configuration changes made:
- Converted print-buffer string and hashmap/set entry traversal to `foreach`.
- Converted Kernel source SPIR-V validation/source-dict traversal and word copy
  loops to `foreach`.
- Converted Tensor capture metadata array and membership scans to `foreach`.
- Converted NN checkpoint array encode/decode, dictionary encode, and
  JSON-source whitespace scans to `foreach`.
- Converted closure env self-reference binding scan to `foreach`.
- Left paired-array, reverse-order, shape/stride, presentation-separator, and
  offset-sensitive loops indexed.
- Marked `AUDIT-2026-M3-FOREACH-LOOPS` complete in the TODO and recorded the
  wave in changelog and plan artifacts.

Commands run:
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(let (model (nn/load (nn/save (nn/init (nn/dense 2 1 (Dictionary 'kernel-init (Dictionary 'kind 'ones) 'bias-init (Dictionary 'kind 'zeros))) (Dictionary 'dtype 'Float32 'device 'cpu 'seed 1)))) out (nn/predict model (Tensor Float32 [2] [1 2]))) (and (= (ref model 'kind) 'model) (= (ref model 'device) 'cpu) (= (ref out [0]) (Float32 3.0))))"`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `git diff --check`

Key results:
- Build passed.
- Model checkpoint round-trip probe returned `true`.
- Basic slice passed with `pass=150 fail=0`.
- Focused advanced collections module passed with `pass=1892 fail=0`.
- Whitespace check passed.

Invalidated assumptions or failed approaches worth preserving:
- Do not use `[..len]` as a count-style pointer slice in these C3 foreach
  conversions. It included one extra element in the NN checkpoint array path,
  causing restored models to fail validation. Use `[0:len]` for exact
  count-style pointer iteration.

Unresolved issues:
- Thirteen part 15 audit/proposal items remain open after this wave.

Next actions:
- Continue with `AUDIT-2026-M4-GLOBAL-STATE-CONTRACTS`,
  `AUDIT-2026-M5-MODULE-CYCLE`, or `AUDIT-2026-M6-BUILD-CONFIG`.

Signature: GPT-5 Codex

## 2026-04-21 16:27 CEST - Audit Remediation Wave 8

Objective attempted:
- Close `AUDIT-2026-M4-GLOBAL-STATE-CONTRACTS` by making the scheduler/JIT
  global-state contracts explicit and centralizing JIT compile nonce mutation.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/todo_parts/todo_part_15.md`
- `src/lisp/scheduler_state_offload.c3`
- `src/lisp/jit_compiler.c3`
- `src/lisp/jit_compiler_runtime_identity.c3`
- `src/lisp/jit_compiler_compile.c3`
- `src/lisp/jit_compiler_state_pool.c3`
- `src/lisp/value_symbol_table.c3`
- `src/lisp/value_type_registry.c3`
- `src/lisp/value_interp_lifecycle.c3`

Code or configuration changes made:
- Documented scheduler globals as single-runtime-thread fiber/uv-loop state,
  with cross-thread offload completion limited to existing mutex/CV/wakeup
  handoff paths.
- Documented JIT runtime globals and state-pool/spill-list globals as
  owner-thread-only state protected by `jit_require_owner_thread`.
- Added `runtime_backend_next_compile_nonce` and routed
  `jit_track_compiled_state` through it, preserving nonzero nonce wrapping under
  the owner-thread guard.
- Documented symbol-table, type-registry, and interpreter allocation
  fault-injection globals as feature-gated test-only controls.
- Marked `AUDIT-2026-M4-GLOBAL-STATE-CONTRACTS` complete in part 15 and
  recorded the wave in changelog and plan artifacts.

Commands run:
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=scheduler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `scripts/check_jit_env_scope_guards.sh`
- `scripts/check_scheduler_state_guards.sh`
- `git diff --check`

Key results:
- Build passed.
- Compiler slice passed with `pass=290 fail=0`.
- Scheduler slice passed with `pass=113 fail=0`.
- JIT env/scope guard script passed.
- Scheduler state guard script passed.
- Whitespace check passed.

Invalidated assumptions or failed approaches worth preserving:
- Do not treat `g_jit_compile_nonce_counter` as an unguarded live data race in
  the current runtime. The JIT runtime contract is owner-thread affinity; adding
  atomics or broad mutexes would widen the contract and should be handled as a
  separate multi-thread JIT design change.

Unresolved issues:
- Twelve part 15 audit/proposal items remain open after this wave.

Next actions:
- Continue with `AUDIT-2026-M5-MODULE-CYCLE`,
  `AUDIT-2026-M6-BUILD-CONFIG`, or `AUDIT-2026-L2-JIT-FILE-NAMES`.

Signature: GPT-5 Codex

## 2026-04-21 07:30 CEST - ML-VK-080-013 Tensor Memory-Plan Metadata

Objective attempted:
- Implement `ML-VK-080-013` as metadata-only memory planning for captured
  Tensor graphs without changing the top-level `tensor-graph` capture contract.

Relevant workspace or target:
- `/home/christos/Omni`
- `.agents/PLAN.md`
- `.agents/SESSION_REPORT.md`
- `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- `docs/todo_parts/todo_part_14.md`
- `docs/LANGUAGE_SPEC.part-01b.md`
- `docs/reference/03-collections.part-01.md`
- `docs/reference/11-appendix-primitives.md`

Code or configuration changes made:
- Added per-node layout/allocation metadata to captured Tensor graph nodes:
  `element-count`, `byte-length`, `storage-offset`, `storage-elements`,
  `storage-bytes`, `allocation`, `owner`, and `write-policy`.
- Added a nested metadata-only `memory-plan` dictionary to
  `tensor/capture(source)` graph plans. The nested plan has kind
  `tensor-memory-plan`, version/backend/dtype/policy fields, external and
  transient byte totals, and `node-memory`.
- Preserved the existing top-level `tensor-graph` plan fields: schedule,
  command-batch planning, execution state, fusion metadata, shape, and
  invalidation key.
- Updated focused tests, language spec, reference docs, roadmap, TODO, active
  plan, and session summary for the nested `memory-plan` contract.

Commands run:
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Build passed.
- Focused advanced collections passed with `pass=1852 fail=0`.
- Compiler slice passed with `pass=289 fail=0`.
- Basic Lisp passed with `pass=161 fail=0`.
- Primitive docs parity, Stage 3 source parity, code file-size gate, and
  whitespace checks passed.
- `prim_tensor_capture.c3` is 665 LOC, below the code-only 700-line limit.

Invalidated assumptions or failed approaches worth preserving:
- Do not make `tensor/capture(source)` return top-level
  `kind 'tensor-memory-plan`; that regresses the graph contract and drops
  schedule/command-batch metadata. Memory planning belongs under nested
  `memory-plan` while the top-level result remains `kind 'tensor-graph`.

Unresolved issues:
- Runtime buffer allocation, handle retention, buffer reuse, executable command
  batching, fusion, source compilation, and broader invalidation/capability
  planning remain open.

Next actions:
- Continue the remaining `ML-VK-080` runtime work from the roadmap: executable
  command buffers, fusion, source compilation, buffer reuse/lifetime planning,
  and broader invalidation/capability planning.

Signature: GPT-5 Codex

## 2026-04-22 03:15 CEST - ML Backlog Reshape And Leaky ReLU Backward

Objective attempted:
- Continue broad audit/repair work by closing stale ML umbrella backlog items,
  filling the new leaky-ReLU forward/autograd/NN surface gap, freezing the
  first reusable `ml/grad` tape metadata contract, and closing the UI blocking
  FTXUI loop slice.

Relevant workspace or target:
- `/home/christos/Omni`
- Vulkan ML roadmap/TODO lane
- `ml/grad`
- Omni Neural DataSpec activation constructors and lowering
- `ui.loop` / `ui.ftxui.loop`
- FTXUI smoke examples

Code or configuration changes made:
- Added `leaky-relu` activation backward support to CPU `ml/grad`
  `linear-activation-mean-squared-error` for `Float64` and `Float32`.
- Added direct `ml/grad` `negative-slope` handling for `leaky-relu`, including
  default `0.01`, non-negative finite validation, and Float32
  representability checks.
- Added default-slope `nn/leaky-relu` DataSpec support: constructor,
  validation symbol, interpreter/AOT registration, and `nn/apply`/`nn/predict`
  lowering through `ml/leaky-relu`.
- Closed `ML-VK-050-005` by adding a versioned metadata-only `gradient-tape`
  dictionary to shipped `ml/grad` results. The tape is ordinary scope-region
  data, records dtype/source/activation and node metadata, sets
  `retains-handles` false, and does not own native handles or executor state.
- Closed `ML-VK-050-006` by adding CPU dense row-major Tensor-expression MSE
  and softmax-cross-entropy gradient specs to `ml/grad`. Reverse accumulation
  now walks supported `map` and `contract` expression nodes, computes MSE and
  softmax-CE upstream gradients, and fails closed for ambiguous `wrt` leaves or
  unsupported map backward rules.
- Closed `ML-VK-050-007` by moving non-CPU Tensor-expression backward
  preflight before `wrt` leaf matching. Vulkan forward tensors without matching
  backward kernels now fail closed under `tensor/backend-unsupported` instead
  of reporting ambiguous/unreachable CPU `wrt` diagnostics.
- Reshaped the ML TODO queue so shipped umbrella parents
  `ML-VK-001`, `ML-VK-040`, `ML-VK-050`, `ML-VK-060`, `ML-VK-070`, and `ML-VK-080` are
  closed. Remaining work is now explicit under
  `ML-VK-040-TRAIN-BN-001`, `ML-VK-040-FUSED-ATTENTION-001`,
  `ML-VK-050-VK-BWD-001`, and `ML-VK-060-FUSED-CUDA-001`.
- Closed `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001` for the one-shot blocking
  effect-tree loop by adding `__ui-ftxui-loop`, `ui.ftxui.loop`, `ui.loop`, and
  `ui.runtime.loop_to`, including matching flat compatibility module exports in
  `ui_ftxui.omni` and `ui_runtime.omni`.
- Added `module_interactive_loop_smoke.omni` and wired it into
  `scripts/run_ftxui_smoke.sh`. The smoke uses piped `q` for the real blocking
  loop and separately covers explicit pre-loop `close_tree`, fail-closed
  `read_event_tree`, and double-render rejection.
- Split the remaining session-owned external read/update/render work to
  `UI-LIB-RUNTIME-SESSION-001`.
- Updated language/reference docs, the Vulkan ML roadmap, Omni Neural DataSpec
  plan, UI docs/README, TODO part 14, active plan, and changelog.

Commands run:
- `c3c build --obj-out obj`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh`
- `scripts/check_file_size_gate.sh`
- `git diff --check`
- `timeout 20s bash -lc 'printf q | LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_interactive_loop_smoke.omni'`
- `OMNI_FTXUI_RUNTIME_LD_LIBRARY_PATH=build:/usr/local/lib timeout 60s scripts/run_ftxui_smoke.sh`
- `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ./build/main --test-suite lisp`

Key results:
- Build passed.
- Focused advanced collections passed with `pass=1919 fail=0` after the
  ML-VK-050-007 fail-closed preflight fix.
- AOT runtime manifest source parity, primitive docs parity, file-size gate, and
  `git diff --check` passed.
- The new FTXUI blocking-loop smoke passed with piped `q`, and the full FTXUI
  smoke script passed.
- Focused advanced FFI/FTXUI surface validation passed with `pass=109 fail=0`.
- Code files touched remain below the user-requested 1000 LOC split threshold.

Invalidated assumptions or failed approaches worth preserving:
- Do not keep `ML-VK-001`, `ML-VK-040`, `ML-VK-060`, `ML-VK-070`, or
  `ML-VK-080` open as umbrella placeholders. `ML-VK-050` is now in the same
  category for the current CPU reverse-mode plus explicit device fail-closed
  contract. Future work should use explicit split items such as
  `ML-VK-050-VK-BWD-001`.
- Do not assume the lower CPU resolver is sufficient for Tensor-expression
  device backward fail-closed semantics. Copied Dictionary specs can break
  `wrt` identity for Vulkan tensors, so device preflight must happen before
  leaf matching.
- Do not treat pre-loop `post_event_tree` as equivalent to live keyboard input
  for deterministic loop exit. The smoke uses piped input for the real blocking
  loop and `close_tree` for explicit pre-loop close semantics.

Unresolved issues:
- Native Vulkan backward kernels remain open under `ML-VK-050-VK-BWD-001`.
- Training-mode batch normalization, fused attention, and fused CUDA optimizer
  kernels are split future work, not hidden residuals under closed parents.
- Session-owned UI read/update/render state remains open under
  `UI-LIB-RUNTIME-SESSION-001`; `read_event_tree` intentionally remains
  fail-closed in the one-shot blocking helper.

Next actions:
- Continue with `ML-VK-050-VK-BWD-001` if native Vulkan backward kernels are the
  priority.
- Alternatively, work the non-ML open item
  `UI-LIB-RUNTIME-SESSION-001`.

Signature: GPT-5 Codex

## 2026-04-21 17:41 CEST - META Plan TODO Backfill

Objective attempted:
- Backfill checkbox TODO tracking for `docs/plans/` documents with live or
  in-flight work, then close `META-PLAN-TODO-BACKFILL-001`.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/plans/`
- `docs/todo_parts/todo_part_14.md`
- `memory/changelog_parts/changelog_part_37.md`
- `.agents/plan_parts/plan_part_05.md`
- `.agents/session_report_parts/session_report_part_40.md`

Code or configuration changes made:
- Audited 113 Markdown plan files with local scans and three parallel subagent
  review slices.
- Closed `META-PLAN-TODO-BACKFILL-001` with evidence.
- Added live TODO entries for:
  - `RUNTIME-FILE-SPLIT-QUEUE-001`
  - `UI-LIB-FACADE-001`
  - `FTXUI-C-ABI-CONTRACT-001`
  - `FTXUI-C-ABI-WRAPPERS-001`
  - `REPL-SERVER-MULTICLIENT-001`
  - `FFI-CALLBACK-UV-001`
  - `FALLBACK-RUNTIME-OWNERSHIP-001`
  - `ACCESS-UNIFY-INDEX-REF-002`
- Added closed backfill entry `STACK-AARCH64-001` because the stack aarch64
  plan already records runtime parity evidence.

Commands run:
- `find docs/plans -type f -name '*.md' | wc -l`
- `rg` scans for unchecked boxes, open/future/deferred markers, TODO lane
  references, and existing TODO coverage.
- Three parallel subagent review slices over disjoint `docs/plans/` ranges.
- Targeted `sed`/`rg` inspections for candidate plan files.
- `wc -l` for updated index counts.
- `git diff --check`

Key results:
- `docs/todo_parts/todo_part_14.md` now contains explicit TODO-backed
  tracking for the previously implicit live plan residuals.
- Closed/decision-only plans and already TODO-backed plans were not duplicated.
- Fixed-width complex tensor residuals remain tracked under the existing
  `TENSOR-100H-*` entries.

Invalidated assumptions or failed approaches worth preserving:
- Do not treat every unreferenced plan file as a live backlog item. Several
  unreferenced files are closed decision notes or historical implementation
  records. Backfill only plans with explicit residual work or active status.
- Do not reopen stack/aarch64 as active work from the delivery-slice wording
  alone; the same plan records runtime arm64 parity evidence.

Unresolved issues:
- The new backfilled live entries remain open and should be worked by their
  individual IDs, not under the now-closed META item.

Next actions:
- Pick the next TODO by priority. The backfilled items add non-Vulkan options
  such as `FALLBACK-RUNTIME-OWNERSHIP-001`,
  `REPL-SERVER-MULTICLIENT-001`, and `RUNTIME-FILE-SPLIT-QUEUE-001`, while the
  existing Vulkan ML queue remains open.

Signature: GPT-5 Codex

## 2026-04-22 16:20 CEST - ML-VK-040 Train Batch Normalization DataSpec

Objective attempted:
- Continue the codebase audit/remediation queue by closing the explicit training-mode/current-batch-stat batch-normalization residual without introducing hidden mutable state or hidden device fallback.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_nn_dataspec.c3`
- `src/lisp/prim_nn_init.c3`
- `src/lisp/prim_nn_apply.c3`
- `src/lisp/prim_nn_training.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `docs/todo_parts/todo_part_14.md`
- `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- `docs/plans/omni-neural-dataspec-plan-2026-04-20.md`

Code or configuration changes made:
- Added `nn/batch-normalization(channels channel-axis [options])` as a validated Omni Neural DataSpec layer constructor.
- `nn/init` now creates explicit BN `scale`/`bias` parameter tensors and `running-mean`/`running-variance` state tensors.
- `nn/apply` lowers BN inference/eval through `ml/batch-normalization` using running state.
- `nn/forward` now has a real state-threading path for train-mode BN: CPU dense row-major current-batch stats are computed, output is normalized with current stats, and updated running stats are returned as ordinary `nn-forward` data and updated model data without mutating the original model.
- Explicit forward arity accepts `(Dictionary 'mode 'train)` for state-threading standalone/sequential specs.
- Added regression tests for init state, immutable train forward state update, sequential explicit state threading, invalid options, and Vulkan train-mode fail-closed behavior.

Commands run:
- `c3c build --obj-out obj`
- `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Build passed.
- Focused advanced collections module passed with `pass=1923 fail=0`.
- Compiler slice passed with `pass=290 fail=0`.
- Primitive docs parity, e2e baseline policy, code file-size gate, and
  whitespace diff checks passed.
- `ML-VK-040-TRAIN-BN-001` is closed for the explicit CPU train-state DataSpec boundary.

Unresolved issues:
- Native CUDA/Vulkan current-batch BN training and BN backward kernels remain unimplemented and fail closed; future work should live under device-backward/state-kernel items rather than reopening the explicit CPU DataSpec slice.

Next actions:
- Continue with remaining explicit open items: `ML-VK-040-FUSED-ATTENTION-001`,
  `ML-VK-050-VK-BWD-001`, `ML-VK-060-FUSED-CUDA-001`, or
  `UI-LIB-RUNTIME-SESSION-EVENT-PAYLOAD-001`.

Signature: GPT-5 Codex

## 2026-04-22 06:47 CEST - ML-VK-050 First Native Vulkan MSE Backward Kernel

Objective attempted:
- Continue the codebase audit/remediation queue by closing the first native
  Vulkan backward-kernel item without regressing the improved FFI callback
  bridge.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_ml_autograd_tensor_expr.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
- `src/lisp/prim_ffi_callback.c3`
- `csrc/ffi_helpers.c`
- `docs/todo_parts/todo_part_14.md`
- `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`

Code or configuration changes made:
- Added the first native Vulkan `ml/grad` backward slice:
  dense row-major `Float32` `tensor-mean-squared-error` now computes loss and
  prediction gradient on Vulkan when `wrt` is the concrete prediction tensor.
- Implemented the Vulkan gradient as native map kernels:
  `(prediction - targets) * (2 / element-count)`.
- Extended tensor identity for concrete non-CPU tensors to recognize shared
  device storage handles, which fixes interpreter specs where dictionary
  evaluation preserves the same device allocation through distinct wrappers.
- Allowed the internal backward map primitive lookup to accept dispatched
  method tables, matching the current arithmetic primitive registration model.
- Kept unsupported Vulkan graph/map-expression backward fail-closed and split
  that residual to `ML-VK-050-VK-MAP-BWD-001`.
- Preserved the improved FFI callback design by aligning the C helper comments
  and link path with `omni_ffi_callback_dispatch`; the stale
  `omni_ffi_closure_trampoline` path was not restored.

Commands run:
- `c3c build --obj-out obj`
- `LD_LIBRARY_PATH=build:/usr/local/lib ./build/main --eval ...` direct
  Vulkan MSE gradient probe
- `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ./build/main --test-suite lisp`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Build passed.
- Direct Vulkan probe returned loss `5.0`, gradient `[1.0, 3.0]`, and Vulkan
  placement for loss, gradient, and tape metadata.
- Focused advanced collections module passed with `pass=1924 fail=0`.
- Focused advanced FFI system surface passed with `pass=115 fail=0`.
- Code file-size gate passed with no tracked code files above 1000 LOC.
- `git diff --check` passed.
- `ML-VK-050-VK-BWD-001` is closed.

Invalidated assumptions or failed approaches worth preserving:
- Do not assume Vulkan map-expression backward can be added by only extending
  the existing reverse graph walk. Normal Vulkan `map` eagerly materializes
  concrete device tensors and drops expression edges.
- Do not globally make Vulkan `map` lazy to solve autograd; use an explicit
  autograd capture/tape mode or graph-preserving `ml/grad` map path.
- Do not restore `omni_ffi_closure_trampoline`; the improved FFI callback ABI is
  `omni_ffi_callback_dispatch`.

Unresolved issues:
- Recursive Vulkan map-expression backward for `map +`/`map *` under MSE is
  still open as `ML-VK-050-VK-MAP-BWD-001`.
- Fused attention and fused CUDA optimizer kernels remain separate open ML
  items.

Next actions:
- Continue with `ML-VK-050-VK-MAP-BWD-001`,
  `ML-VK-040-FUSED-ATTENTION-001`, or `ML-VK-060-FUSED-CUDA-001`.

Signature: GPT-5 Codex
