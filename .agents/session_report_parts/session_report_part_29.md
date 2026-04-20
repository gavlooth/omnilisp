# Session Report Part 29

## 2026-04-20 03:48 CEST - Vulkan ML Max Checkpoint

- Objective attempted:
  - Continue the strict audit/TODO loop after Vulkan `ml/sum`/`ml/mean`/
    `ml/variance` by adding the next concrete Vulkan stable-reduction
    prerequisite.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `csrc/tensor_vulkan_helpers_ml_reduction.c`
  - `csrc/tensor_vulkan_ml_reduction_f32.comp`
  - `csrc/tensor_vulkan_ml_reduction_f64.comp`
  - `src/lisp/prim_ml_reduction.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  - `docs/todo_parts/todo_part_14.md`
- Code or configuration changes made:
  - Added and closed `ML-VK-020-006-VK-MAX` in TODO after audit found the
    Vulkan `ml/max` follow-up was implicit instead of concrete.
  - Extended the existing Vulkan ML reduction helper/shaders with `op == 3`
    for axis maximum.
  - Routed Vulkan `ml/max` through the helper from `prim_ml_reduction.c3`,
    preserving Float64/Float32 dtype and Vulkan placement.
  - Added guarded Vulkan parity coverage for Float64/Float32 `ml/max` and kept
    Vulkan `ml/logsumexp`/`ml/softmax` fail-closed.
  - Updated roadmap/spec/reference text so current Vulkan `ml/max` support is
    no longer documented as CPU-only.
- Commands run:
  - Fast audit subagents for reduction implementation and TODO/docs/tests.
  - Fast worker subagent for the initial C helper/shader max opcode patch.
  - `glslangValidator` and `spirv-val` for both updated reduction shaders.
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - Direct REPL smoke for Vulkan Float64/Float32 `ml/max` and Vulkan
    `ml/logsumexp` fail-closed behavior.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Direct smokes returned `true` for Vulkan Float64 max, Vulkan Float32 max,
    and preserved Vulkan logsumexp fail-closed behavior.
  - Focused advanced collections suite passed with `pass=1695 fail=0`.
  - Basic Lisp slice passed with `pass=160 fail=0`.
  - File-size gate passed with no tracked text file over 700 lines.
- Current best recommendation / checkpoint:
  - Vulkan `ml/max` is now implemented. The next Vulkan ML work should add
    exp/log-backed stable kernels before claiming Vulkan `ml/logsumexp`,
    `ml/softmax`, cross-entropy, or MSE.
- Unresolved issues:
  - Full host `advanced` slice was not rerun in this pass because the previous
    checkpoint reproduced an unrelated segfault after TCO tests before ML
    part8.
  - Full bounded-container suite was not run.
- Signature: Codex GPT-5.4

## 2026-04-20 03:40 CEST - Vulkan ML Axis Reduction Checkpoint

- Objective attempted:
  - Continue the TODO/audit loop by implementing the open
    `ML-VK-020-006-VK` Vulkan axis-reduction substrate and reconciling the
    reviewer findings into TODO/test artifacts.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `csrc/tensor_vulkan_helpers_ml_reduction.c`
  - `csrc/tensor_vulkan_ml_reduction_f32.comp`
  - `csrc/tensor_vulkan_ml_reduction_f64.comp`
  - `src/lisp/prim_ml_reduction.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  - `docs/todo_parts/todo_part_14.md`
- Code or configuration changes made:
  - Added a real one-input Vulkan reduction helper for Float64/Float32
    `ml/sum`, `ml/mean`, and population `ml/variance`.
  - Added Float32/Float64 GLSL compute shaders plus generated SPIR-V C blobs
    and build wiring.
  - Routed Vulkan tensors through the new reduction helper without hidden CPU
    fallback, while keeping `ml/max`, `ml/logsumexp`, and `ml/softmax`
    backend-unsupported on Vulkan.
  - Updated `tensor-backends` so Vulkan `ml-reduction-float64` and
    `ml-reduction-float32` track the backend Float64/Float32 capability bits.
  - Added rank-3 middle-axis CPU regression coverage for shared reduction
    indexing and guarded Vulkan parity tests that assert results stay on
    Vulkan and match CPU after explicit copy-back.
  - Closed `ML-VK-020-006-VK` in TODO and updated the roadmap/spec/reference
    docs.
- Commands run:
  - Fast explorer subagent audit for Vulkan reduction helper surface.
  - Fast worker subagent for TODO/test planning artifact updates.
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_ml_reduction_f32.comp -o /tmp/omni_ml_reduction_f32.spv`
  - `spirv-val --target-env vulkan1.0 /tmp/omni_ml_reduction_f32.spv`
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_ml_reduction_f64.comp -o /tmp/omni_ml_reduction_f64.spv`
  - `spirv-val --target-env vulkan1.0 /tmp/omni_ml_reduction_f64.spv`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main < /tmp/omni-ml-reduction-check.omni`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
- Key results:
  - Direct Vulkan smokes returned `true` for Float64 reductions, Float32
    reductions, and rank-3 middle-axis CPU/stable reductions; `ml/max` on
    Vulkan returned `tensor/backend-unsupported` as intended.
  - Basic Lisp slice passed with `pass=160 fail=0`.
  - File-size gate passed with no tracked text file over 700 lines.
- Invalidated assumptions or failed approaches worth preserving:
  - The static audit claim that `ml_reduction_result_index` multiplies output
    strides through reduced axes was false against the current code; the helper
    skips reduced axes before advancing `result_stride`. Keep the new rank-3
    middle-axis regression as proof instead of reworking the indexer.
  - Do not claim Vulkan `ml/max`, `ml/logsumexp`, `ml/softmax`, or loss support
    from the sum/mean/variance helper alone.
- Unresolved issues:
  - Full `advanced` host slice currently segfaults after the TCO tests before
    reaching the ML part8 tests; this was not resolved in this slice.
  - Full bounded-container suite was not run.
- Next actions:
  - Implement Vulkan `ml/max` and then exp/log-backed stable kernels before
    enabling Vulkan `ml/logsumexp`, `ml/softmax`, or losses.
- Signature: Codex GPT-5.4

## 2026-04-20 02:45 CEST - ML Stable Reduction And Softmax Checkpoint

- Objective attempted:
  - Continue the strict TODO audit/implementation loop after CPU ML axis
    reductions, using fast subagents for audit support and then implementing
    the next coherent TODO slice.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_ml_reduction.c3`
  - `src/lisp/prim_ml_stable_reduction.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  - `docs/todo_parts/todo_part_14.md`
  - `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- Code or configuration changes made:
  - Added CPU `ml/max` as the canonical axis maximum reducer for Float64 and
    Float32 tensors.
  - Changed Float32 reduction output narrowing failures from
    `tensor/backend-unsupported` to `tensor/numeric-overflow`.
  - Added CPU `ml/logsumexp(input axes)` with max-shifted multi-axis reduction
    semantics.
  - Added CPU `ml/softmax(input axis)` with max-shifted single-axis
    normalization and input-shape preservation.
  - Split stable reduction/softmax code into
    `src/lisp/prim_ml_stable_reduction.c3` so all tracked files remain under
    the 700-line repository limit.
  - Registered the new primitives in runtime and AOT lookup tables.
  - Updated TODO, roadmap, language spec, primitive appendix, and collections
    reference docs. Cross-entropy and mean-squared-error are now separate open
    TODO items rather than hidden under a completed softmax/logsumexp slice.
- Commands run:
  - Fast subagent audits for reducer diagnostics/coverage, Vulkan reduction
    feasibility, and `ML-VK-020-007` surface/edge cases.
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
- Key results:
  - Focused advanced collections suite passed with `pass=1681 fail=0`.
  - File-size gate passed with no tracked text file over 700 lines.
- Invalidated assumptions or failed approaches worth preserving:
  - A stable softmax/logsumexp implementation cannot be built on sum-only
    reducers without a per-slice max path.
  - Cross-entropy remains underdefined until the target contract is chosen;
    do not expose a primitive that accepts both probability/one-hot tensors and
    class-index tensors by implication.
- Current best recommendation / checkpoint:
  - Continue with `ML-VK-020-007-B` only after deciding the cross-entropy target
    contract, or implement `ML-VK-020-007-C` mean-squared-error first.
  - Implement `ML-VK-020-006-VK` before claiming any Vulkan reduction,
    logsumexp, or softmax capability.
- Unresolved issues:
  - Full bounded-container suite was not run.
  - CUDA/Vulkan reductions, logsumexp, and softmax intentionally fail closed.
  - Cross-entropy and mean-squared-error remain open TODO items.
- Signature: Codex GPT-5.4

## 2026-04-20 03:02 CEST - ML Cross Entropy Checkpoint

- Objective attempted:
  - Finish the remaining `ML-VK-020-007` CPU loss primitive by recording a
    concrete target contract instead of leaving cross-entropy ambiguous.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_ml_stable_reduction.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  - `docs/todo_parts/todo_part_14.md`
- Code or configuration changes made:
  - Added CPU `ml/cross-entropy(logits targets axis)`.
  - Froze targets as same-shape probability/one-hot tensors, not class-index
    tensors.
  - Implemented max-shifted log-softmax over one explicit class axis and
    averaged losses over non-class positions.
  - Added tests for large-logit stability, Float32 dtype preservation, non-last
    class axes, soft targets, invalid probability sums, mismatch diagnostics,
    and Vulkan fail-closed behavior.
  - Closed `ML-VK-020-007-B` in TODO and docs.
- Commands run:
  - Fast cross-entropy subagent audit.
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
- Key results:
  - Focused advanced collections suite passed with `pass=1692 fail=0`.
  - File-size gate passed with no tracked text file over 700 lines.
- Current best recommendation / checkpoint:
  - `ML-VK-020-007` CPU surface is closed. Do not claim Vulkan support until
    `ML-VK-020-006-VK` adds real backend reductions.
- Unresolved issues:
  - Full bounded-container suite was not run.
  - CUDA/Vulkan cross-entropy intentionally fails closed.
- Signature: Codex GPT-5.4

## 2026-04-20 02:55 CEST - ML Mean Squared Error Checkpoint

- Objective attempted:
  - Continue the TODO implementation loop after the stable softmax/logsumexp
    checkpoint by choosing the unambiguous remaining loss item.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_ml_stable_reduction.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  - `docs/todo_parts/todo_part_14.md`
- Code or configuration changes made:
  - Added CPU `ml/mean-squared-error(predictions targets)` as a scalar Tensor
    loss for same-shape, same-dtype Float64/Float32 tensors.
  - Registered the primitive in runtime and AOT lookup tables.
  - Added Float64, Float32 dtype-preservation, lazy CPU input, mismatch, and
    Vulkan fail-closed regressions.
  - Closed `ML-VK-020-007-C` in TODO and docs.
- Commands run:
  - Fast MSE subagent audit.
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
- Key results:
  - Focused advanced collections suite passed with `pass=1686 fail=0`.
  - File-size gate passed with no tracked text file over 700 lines.
- Current best recommendation / checkpoint:
  - The only remaining `ML-VK-020-007` public primitive is cross-entropy.
    Decide target semantics before implementing it.
- Unresolved issues:
  - Full bounded-container suite was not run.
  - CUDA/Vulkan MSE intentionally fails closed.
- Signature: Codex GPT-5.4
