# 2026-04-20 16:18 CEST - Vulkan ML Dense 2D Convolution

- Objective attempted:
  - Continue `ML-VK-030` after dense `ml/conv1d` by auditing and implementing
    the next explicit convolution slice with multiple agents.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - Vulkan ML roadmap item `ML-VK-030-002`.
- Code or configuration changes made:
  - Added public `ml/conv2d` with arity 9:
    `input kernel stride-height stride-width padding-height padding-width
    dilation-height dilation-width groups`.
  - Implemented dense row-major NCHW input and OIHW kernel validation,
    grouped CPU `Float64`/`Float32` execution, direct dense Vulkan `Float32`
    execution, and fail-closed mixed-device behavior.
  - Added the Vulkan compute shader, generated SPIR-V wrapper, helper API
    wiring, primitive/AOT/runtime-manifest registration, capability fields,
    focused tests, docs, TODO, roadmap, and changelog updates.
- Commands run:
  - `glslangValidator -V csrc/tensor_vulkan_ml_conv2d_f32.comp -o build/generated/tensor_vulkan_ml_conv2d_f32.spv`
  - `spirv-val build/generated/tensor_vulkan_ml_conv2d_f32.spv`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `git diff --check`
  - `scripts/check_file_size_gate.sh`
- Key results:
  - Focused advanced collections passed with `pass=1721 fail=0`.
  - Basic Lisp slice passed with `pass=160 fail=0`.
  - File-size gate passed; all touched text files remain under 700 LOC.
  - Broad `ml-convolution` remains false because max/average pooling is not yet
    implemented.
- Unresolved issues:
  - `ML-VK-030` still needs max and average pooling before broad convolution
    family capability can become true.
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run in this slice.
- Next actions:
  - Implement explicit dense pooling, preferably `ml/max-pool2d` and
    `ml/avg-pool2d`, with CPU `Float64`/`Float32`, Vulkan `Float32`,
    fail-closed mixed placement, and narrow capability keys before flipping
    broad `ml-convolution`.
- Signature: Codex GPT-5.4
