# 2026-04-20 16:47 CEST - Vulkan ML Dense 2D Pooling

- Objective attempted:
  - Continue implementation after the Omni Neural DataSpec decision by closing
    the low-level pooling gap required for the first ergonomic ConvNet
    inference surface.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - Vulkan ML roadmap item `ML-VK-030-003`.
- Code or configuration changes made:
  - Added public `ml/max-pool2d` and `ml/avg-pool2d` with arity 7:
    `input window-height window-width stride-height stride-width
    padding-height padding-width`.
  - Implemented dense row-major NCHW validation, CPU `Float64`/`Float32`
    execution, direct dense Vulkan `Float32` execution, and fail-closed lazy
    mixed CPU/Vulkan behavior.
  - Added the Vulkan compute shader, generated SPIR-V wrapper, helper API
    wiring, primitive/AOT/runtime-manifest registration, capability fields,
    focused tests, docs, TODO, roadmap, and changelog updates.
- Commands run:
  - `glslangValidator -V csrc/tensor_vulkan_ml_pool2d_f32.comp -o build/generated/tensor_vulkan_ml_pool2d_f32.spv`
  - `spirv-val build/generated/tensor_vulkan_ml_pool2d_f32.spv`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- Key results:
  - Focused advanced collections passed with `pass=1726 fail=0`.
  - Broad `ml-convolution` is now true for CPU and for Vulkan when Float32 is
    available, scoped to the explicit dense convolution/pooling family.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run in this slice.
  - Convolution/pooling backward kernels remain owned by `ML-VK-050` autograd.
- Next actions:
  - Run remaining hygiene checks and commit/push.
  - Start `ML-VK-070-001` Omni Neural DataSpec schemas and `nn/validate`.
- Signature: Codex GPT-5.4
