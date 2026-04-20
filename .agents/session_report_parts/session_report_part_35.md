# 2026-04-20 17:06 CEST - Omni Neural DataSpec Inference

- Objective attempted:
  - Continue `ML-VK-070` by implementing `ML-VK-070-003`, inference
    application and inspection for Omni Neural DataSpec model bundles.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - Vulkan ML roadmap item `ML-VK-070-003`.
- Code or configuration changes made:
  - Added public `nn/apply`, `nn/predict`, `nn/summary`, `nn/spec`,
    `nn/parameters`, `nn/state`, and `nn/mode` primitives.
  - Wired runtime primitive registration, AOT primitive lookup, and runtime
    manifest inclusion for `src/lisp/prim_nn_apply.c3`.
  - Implemented model-bundle inference via `(nn/apply model input)` and
    explicit data inference via `(nn/apply spec params state input [options])`.
  - `nn/predict` now requires model mode `'eval`.
  - `nn/summary` reports inspectable model metadata and parameter counts.
  - Lowering covers dense, conv1d, conv2d, max/avg pool2d, activation,
    softmax, sequential, and CPU flatten paths through the existing `ml/*`
    primitive layer.
  - Tightened the explicit `nn/apply` options contract so only an empty
    reserved options dictionary is accepted; non-empty options fail closed.
  - Tightened `nn/summary` parameter-tree errors so malformed trees report
    `nn/invalid-spec` instead of generic count failure.
  - Added focused tests and updated language spec, primitive appendix, TODO,
    plan, and changelog artifacts.
- Commands run:
  - Fast subagent contract audit for inference/accessor surface.
  - Fast subagent docs/backlog update.
  - Fast subagent test implementation.
  - Fast subagent review-only audit for `nn/apply` and docs/tests.
  - `c3c build`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - `c3c build` passed.
  - Focused advanced collections passed with `pass=1748 fail=0`.
  - Basic Lisp passed with `pass=160 fail=0`.
  - Primitive docs parity, file-size gate, and whitespace checks passed.
  - `docs/LANGUAGE_SPEC.part-01b.md` was split top-down again by moving the
    tail backend notes into `docs/LANGUAGE_SPEC.part-01c.md`; all tracked text
    files remain under 700 lines.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not accept non-empty `nn/apply` options as a no-op. Until real options
    exist, accepting them silently is a broken contract.
  - Do not collapse malformed `nn/summary` parameter trees into generic count
    failures; invalid model data should remain `nn/invalid-spec`.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run in this
    slice.
  - `ML-VK-070-004` checkpoint round-trip support remains open.
- Next actions:
  - Continue `ML-VK-070-004`: checkpoint serialization/restore for transparent
    model bundles.
  - Keep full training facade work under `ML-VK-070-005` until autograd and
    optimizer contracts are ready.
- Signature: Codex GPT-5.4

# 2026-04-20 17:35 CEST - Omni Neural DataSpec Checkpoints

- Objective attempted:
  - Continue `ML-VK-070` by implementing `ML-VK-070-004`, checkpoint
    serialization and restore for Omni Neural DataSpec specs and model bundles.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - Vulkan ML roadmap item `ML-VK-070-004`.
- Code or configuration changes made:
  - Added public `nn/save-spec`, `nn/load-spec`, `nn/save`, and `nn/load`
    primitives.
  - Wired runtime primitive registration, AOT primitive lookup, and runtime
    manifest inclusion for `src/lisp/prim_nn_checkpoint.c3`.
  - Implemented checkpoint JSON strings and path-based save/load.
  - Encoded model bundle data explicitly: spec, params, state, mode, dtype,
    device, metadata, tensor dtype, tensor shape, flat tensor data, and recorded
    tensor placement.
  - Restored non-CPU tensors through explicit `to-device` routes.
  - Added focused round-trip and payload-family mismatch tests.
  - Updated language spec, primitive appendix, Omni Neural DataSpec plan, TODO,
    active plan, and memory changelog artifacts.
- Commands run:
  - Fast read-only subagent pass for test and docs insertion points.
  - `c3c build`
  - Direct `--eval` smoke checks for string and path checkpoint round trips.
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - `c3c build` passed.
  - Direct string and path checkpoint smokes returned `true`.
  - Focused advanced collections passed with `pass=1753 fail=0`.
  - Basic Lisp passed with `pass=160 fail=0`.
  - Primitive docs parity, file-size gate, and whitespace checks passed.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run in this
    slice.
  - `ML-VK-070-005` remains blocked on the lower-level autograd and optimizer
    roadmap lanes.
- Next actions:
  - Continue Vulkan ML prerequisites for `ML-VK-050` autograd and `ML-VK-060`
    optimizers before training facade work.
- Signature: Codex GPT-5.4

# 2026-04-20 18:21 CEST - Vulkan ML Layer Normalization

- Objective attempted:
  - Continue `ML-VK-040` by implementing the first normalization primitive for
    CPU and Vulkan ML execution.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - Vulkan ML roadmap item `ML-VK-040-001`.
- Code or configuration changes made:
  - Added public `ml/layer-normalization(input axis [epsilon])`.
  - Implemented CPU `Float64`/`Float32` normalization and direct Vulkan
    `Float32` normalization.
  - Added dedicated C helper, GLSL compute shader, and embedded SPIR-V source
    for same-shape Vulkan layer normalization.
  - Wired runtime primitive registration, AOT primitive lookup, runtime source
    manifest inclusion, and Vulkan backend externs.
  - Added `ml-layer-normalization-float64`,
    `ml-layer-normalization-float32`, and broad `ml-normalization` backend
    capability reporting.
  - Added focused CPU/Vulkan/fail-closed tests.
  - Updated language spec, primitive appendix, collections reference,
    Vulkan ML roadmap, TODO, plan, and memory changelog artifacts.
- Commands run:
  - Fast read-only subagent implementation-surface probe.
  - Fast read-only subagent Vulkan helper/shader probe.
  - `glslangValidator -V csrc/tensor_vulkan_ml_layer_norm_f32.comp`
  - `spirv-val` on the generated layer-normalization SPIR-V.
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - Direct CPU and Vulkan `--eval` smokes.
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - `c3c build` passed.
  - Direct CPU and Vulkan layer-normalization smokes returned `true`.
  - Focused advanced collections passed with `pass=1758 fail=0`.
  - Basic Lisp passed with `pass=160 fail=0`.
  - Primitive docs parity, file-size gate, and whitespace checks passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat the existing rank-reducing Vulkan ML reduction helper as a
    layer-normalization implementation. Layer normalization preserves input
    shape and needs a dedicated same-shape helper/shader path.
  - Do not satisfy Vulkan layer normalization by realizing to CPU. Unsupported
    placements must fail closed until a validated backend kernel exists.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run in this
    slice.
  - Affine gamma/beta layer normalization is not implemented; it should be a
    separate multi-buffer Vulkan kernel slice rather than an implicit CPU path.
- Next actions:
  - Continue `ML-VK-040` with batch normalization or scaled dot-product
    attention.
  - If affine layer normalization is needed first, add explicit scale/bias
    tensor operands and a matching Vulkan dispatch path.
- Signature: Codex GPT-5.4

# 2026-04-20 18:58 CEST - Vulkan ML Batch Normalization

- Objective attempted:
  - Continue `ML-VK-040` by implementing explicit-stat batch normalization for
    CPU and Vulkan ML execution.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - Vulkan ML roadmap item `ML-VK-040-002`.
- Code or configuration changes made:
  - Added public
    `ml/batch-normalization(input scale bias mean variance channel-axis [epsilon])`.
  - Implemented CPU `Float64`/`Float32` and direct Vulkan dense row-major
    `Float32` execution.
  - Added a dedicated Vulkan C helper, GLSL compute shader, and embedded
    SPIR-V source with seven storage-buffer bindings.
  - Wired runtime registration, AOT primitive lookup, Vulkan externs, helper
    declarations, and helper build input.
  - Added `ml-batch-normalization-float64`,
    `ml-batch-normalization-float32`, and broad `ml-normalization` capability
    reporting.
  - Added focused CPU/Vulkan/fail-closed tests.
  - Updated language spec, primitive appendix, collections reference,
    Vulkan ML roadmap, TODO, plan, and memory changelog artifacts.
- Commands run:
  - Fast read-only subagent implementation-surface probes.
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_ml_batch_norm_f32.comp`
  - `spirv-val` on the generated batch-normalization SPIR-V.
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - Direct CPU and Vulkan `--eval` smokes.
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - `c3c build` passed.
  - Direct CPU and Vulkan batch-normalization smokes returned `true`.
  - Focused advanced collections passed with `pass=1764 fail=0`.
  - Basic Lisp passed with `pass=160 fail=0`.
  - Primitive docs parity, file-size gate, and whitespace checks passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not model the shipped batch-normalization primitive as hidden mutable
    training state. The accepted contract is data-oriented and uses explicit
    scale, bias, mean, and variance tensors.
  - Do not satisfy mixed Vulkan batch-normalization operands by realizing them
    to CPU. Unsupported placements must fail closed until a validated backend
    path exists.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run in this
    slice.
  - Training-mode/current-batch-stat batch normalization remains deferred until
    state/autograd contracts are ready.
- Next actions:
  - Continue `ML-VK-040` with scaled dot-product attention, or continue the
    lower-level autograd/optimizer prerequisites needed for training-mode
    normalization.
- Signature: Codex GPT-5.4

## 2026-04-20 - Vulkan ML Scaled Dot-Product Attention

- Objective attempted:
  - Continue `ML-VK-040` by implementing a backend-neutral scaled dot-product
    attention primitive with direct Vulkan inference support and no hidden CPU
    fallback for Vulkan operands.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - ML Tensor primitives, Vulkan helper/shader path, capability reporting,
    tests, public docs, TODO, and operational plan/changelog artifacts.
- Code or configuration changes made:
  - Added `ml/scaled-dot-product-attention(query key value [mask] [scale])`.
  - Implemented CPU `Float64`/`Float32` max-shifted attention with optional
    additive `[Q K]` or batched masks and default scale `1 / sqrt(head-dim)`.
  - Implemented direct dense Vulkan `Float32` attention through
    `csrc/tensor_vulkan_helpers_ml_attention.c`,
    `csrc/tensor_vulkan_ml_attention_f32.comp`, and the generated SPIR-V C
    embedding.
  - Wired runtime and AOT primitive registration, runtime source manifest,
    helper build manifest, and Vulkan C ABI declarations.
  - Added `ml-scaled-dot-product-attention-float64`,
    `ml-scaled-dot-product-attention-float32`, and broad `ml-attention`
    backend capability reporting.
  - Updated tests, language/reference docs, the Vulkan ML roadmap, and TODO.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_ml_attention_f32.comp`
  - `spirv-val`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - direct CPU and Vulkan `--eval` smokes with `LD_LIBRARY_PATH=build:/usr/local/lib`
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=build:/usr/local/lib ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=build:/usr/local/lib ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Focused advanced collections passed: `pass=1769 fail=0`.
  - Basic Lisp slice passed: `pass=160 fail=0`.
  - Helper build, C3 build, primitive docs parity, Stage 3 source parity,
    file-size gate, shader validation, and diff whitespace checks passed.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run.
  - Fused attention/dropout/matmul and training/backward kernels remain future
    `ML-VK-050`/graph-fusion work, not part of the shipped primitive contract.
- Next actions:
  - Prefer `ML-VK-050` autograd prerequisites unless the owner wants fused
    attention performance work first.
- Signature: Codex GPT-5.4
