# Session Report Part 40

Source: `.agents/SESSION_REPORT.md`

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
