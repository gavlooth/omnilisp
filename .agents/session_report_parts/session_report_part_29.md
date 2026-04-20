# Session Report Part 29

## 2026-04-20 13:09 CEST - Vulkan ML Mean Squared Error Checkpoint

- Objective attempted:
  - Continue the Vulkan ML suite after Float32 softmax by adding the first
    Vulkan loss kernel.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `csrc/tensor_vulkan_helpers_ml_loss.c`
  - `csrc/tensor_vulkan_ml_mse_f64.comp`
  - `csrc/tensor_vulkan_ml_mse_f32.comp`
  - `src/lisp/prim_ml_stable_reduction.c3`
  - `src/lisp/prim_ml_vulkan_losses.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  - `src/entry_build_backend_compile.c3`
  - `src/entry_build_runtime_manifest_lisp_part*.c3`
  - `scripts/check_e2e_baseline_policy.sh`
  - `docs/todo_parts/todo_part_14.md`
- Code or configuration changes made:
  - Added and closed `ML-VK-020-007-VK-MSE`.
  - Added dedicated Vulkan Float64/Float32 MSE shaders, generated SPIR-V
    objects, and C helper wiring.
  - Routed Vulkan `ml/mean-squared-error(predictions targets)` without hidden
    CPU fallback, preserving dtype and Vulkan placement for the scalar result.
  - Added guarded Vulkan Float64/Float32 parity coverage and mixed CPU/Vulkan
    fail-closed coverage.
  - Updated roadmap/spec/reference/TODO text for the new Vulkan MSE contract.
  - Regenerated the AOT non-test Lisp runtime manifests into four under-700
    parts after audit found the manifest omitted tensor/ML runtime files.
  - Strengthened the Stage 3 parity checker to compare the full non-test
    `src/lisp/*.c3` set against the AOT manifest parts.
  - Replaced a production deduce OOM-test env hook's dependency on
    `test_c_getenv` with the production boundary getenv wrapper so AOT builds
    do not require test-only sources.
  - Added a Vulkan Float32 MSE overflow check that reads back the scalar status
    value from the device result, rejects non-finite output, and maps it to
    `tensor/numeric-overflow` to match the CPU Float32 loss contract.
- Commands run:
  - Medium subagent audit for MSE implementation surface.
  - Fast docs/TODO audit subagent.
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_ml_mse_f32.comp -o /tmp/omni_ml_mse_f32.spv`
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_ml_mse_f64.comp -o /tmp/omni_ml_mse_f64.spv`
  - `spirv-val --target-env vulkan1.0 /tmp/omni_ml_mse_f32.spv`
  - `spirv-val --target-env vulkan1.0 /tmp/omni_ml_mse_f64.spv`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - Direct REPL smokes for Vulkan Float64/Float32 MSE and mixed CPU/Vulkan
    fail-closed behavior.
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `git diff --check`
  - `scripts/check_file_size_gate.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_e2e_baseline_policy.sh`
  - Direct Vulkan/CPU Float32 overflow smokes for `ml/mean-squared-error`.
  - AOT smoke: `LD_LIBRARY_PATH=build:/usr/local/lib:deps/lib ./build/main --build /tmp/.../mse.omni -o /tmp/.../mse_bin` and run output `1.66666666666667`.
- Key results:
  - Focused advanced collections passed with `pass=1701 fail=0`.
  - Basic Lisp slice passed with `pass=160 fail=0`.
  - Direct smokes returned `true` for Vulkan MSE support and mixed-device
    fail-closed behavior.
  - Direct CPU and Vulkan Float32 overflow smokes both returned
    `tensor/numeric-overflow`.
  - AOT manifest parity passed and an AOT binary using
    `ml/mean-squared-error` compiled, linked, and ran.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat the old `append_matching_sources(...)` Stage 3 checker shape
    as authoritative; the AOT build path now uses explicit runtime manifests.
  - Do not use `test_c_getenv` in non-test runtime files because production AOT
    builds intentionally exclude `src/lisp/tests_*.c3`.
  - Put `build` before `/usr/local/lib` in `LD_LIBRARY_PATH` when validating
    freshly rebuilt C helpers; otherwise the process can load stale installed
    helper libraries.
- Current best recommendation / checkpoint:
  - Commit and push the non-artifact source/docs/session changes.
- Unresolved issues:
  - Full bounded-container suite was not run.
  - Vulkan `ml/cross-entropy` remains open.
  - Vulkan Float64 `ml/logsumexp` and `ml/softmax` remain blocked on a
    validated Float64 exp/log policy.
- Signature: Codex GPT-5.4

## 2026-04-20 12:52 CEST - Vulkan ML Softmax Float32 Checkpoint

- Objective attempted:
  - Continue the Vulkan ML suite after Float32 `ml/logsumexp` by adding a real
    same-shape backend softmax slice with parallel audit support.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `csrc/tensor_vulkan_helpers_ml_softmax.c`
  - `csrc/tensor_vulkan_ml_softmax_f32.comp`
  - `src/lisp/prim_ml_stable_reduction.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  - `docs/todo_parts/todo_part_14.md`
- Code or configuration changes made:
  - Added and closed `ML-VK-020-007-VK-SM-F32`.
  - Added a dedicated Vulkan Float32 softmax helper/shader/SPIR-V object for
    max-shifted same-shape axis normalization.
  - Routed Vulkan Float32 `ml/softmax(input axis)` without hidden CPU fallback
    and kept Vulkan Float64 `ml/softmax` fail-closed.
  - Added guarded Vulkan softmax coverage for device/dtype preservation and
    row normalization.
  - Updated roadmap/spec/reference/TODO text for the new Float32-only Vulkan
    `ml/softmax` contract.
- Commands run:
  - Fast subagent audit for Vulkan softmax implementation surface.
  - Fast docs worker; no files changed by the worker.
  - Medium post-design audit subagent for final-diff risks.
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_ml_softmax_f32.comp -o /tmp/omni_ml_softmax_f32.spv`
  - `spirv-val --target-env vulkan1.0 /tmp/omni_ml_softmax_f32.spv`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - Direct REPL smokes for Vulkan Float32 `ml/softmax` and Vulkan Float64
    fail-closed behavior.
- Key results:
  - Direct smokes returned `true` for Vulkan Float32 softmax support and
    Vulkan Float64 fail-closed behavior.
  - Focused advanced collections passed with `pass=1699 fail=0`.
  - Basic Lisp slice passed with `pass=160 fail=0`.
  - Primitive docs parity, `git diff --check`, and file-size gate passed.
- Current best recommendation / checkpoint:
  - Commit and push the non-artifact source/docs/session changes.
- Unresolved issues:
  - Full bounded-container suite was not run.
  - Vulkan Float64 stable exp/log-backed ML kernels remain unsupported.
  - Vulkan loss kernels remain open.
- Signature: Codex GPT-5.4

## 2026-04-20 12:34 CEST - Vulkan ML Logsumexp Float32 Checkpoint

- Objective attempted:
  - Continue the strict audit/TODO loop after Vulkan `ml/max` by adding the
    first exp/log-backed stable reduction slice.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `csrc/tensor_vulkan_helpers_ml_reduction.c`
  - `csrc/tensor_vulkan_ml_reduction_f32.comp`
  - `src/lisp/prim_ml_stable_reduction.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  - `docs/todo_parts/todo_part_14.md`
- Code or configuration changes made:
  - Added and closed `ML-VK-020-007-VK-LSE-F32` in TODO after audit found
    Vulkan stable reductions lacked a concrete follow-up item.
  - Extended the Vulkan Float32 ML reduction shader/helper with `op == 4` for
    max-shifted `ml/logsumexp(input axes)`.
  - Routed Vulkan Float32 `ml/logsumexp` through the reduction helper while
    keeping Vulkan Float64 `ml/logsumexp` fail-closed.
  - Added guarded Vulkan Float32 row and all-axis parity coverage, and kept
    Vulkan `ml/softmax` fail-closed until a same-shape normalization shader
    exists.
  - Updated roadmap/spec/reference text for the new Float32-only Vulkan
    `ml/logsumexp` contract.
- Commands run:
  - Fast worker subagent for the C helper/shader/SPIR-V Float32 opcode patch.
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_ml_reduction_f32.comp -o /tmp/omni_ml_reduction_f32.spv`
  - `spirv-val --target-env vulkan1.0 /tmp/omni_ml_reduction_f32.spv`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - Direct REPL smoke for Vulkan Float32 `ml/logsumexp`, Vulkan Float64
    `ml/logsumexp` fail-closed behavior, and Vulkan `ml/softmax`
    fail-closed behavior.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- Key results:
  - Direct smokes returned `true` for the supported/unsupported boundaries.
  - Focused advanced collections suite passed with `pass=1696 fail=0`.
- Current best recommendation / checkpoint:
  - Vulkan Float32 `ml/logsumexp` is implemented. Next Vulkan ML work should
    add a same-shape `ml/softmax` axis-normalization shader or a validated
    Float64 exp/log policy before broadening stable reductions or losses.
- Unresolved issues:
  - Full host `advanced` slice was not rerun because the previous checkpoint
    reproduced an unrelated segfault after TCO tests before ML part8.
  - Full bounded-container suite was not run.
- Signature: Codex GPT-5.4

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
