# Session Report Part 36

## 2026-04-20 22:42 CEST - ML-VK-060-007 Vulkan Float32 SGD Optimizer

- Objective attempted:
  - Continue `ML-VK-060` by adding the first Vulkan optimizer kernel path for
    Omni's data-oriented optimizer suite.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - Vulkan ML helper C/SPIR-V files, `ml/optimizer-step`, backend capability
    reporting, advanced collection tests, public ML docs, TODO, roadmap, and
    operational planning artifacts.
- Code or configuration changes made:
  - Added Vulkan `Float32` SGD shaders for stateless SGD, initial momentum
    velocity creation, and momentum velocity consumption.
  - Added `csrc/tensor_vulkan_helpers_ml_optimizer.c` and SPIR-V declarations.
  - Added `src/lisp/prim_ml_optimizer_vulkan.c3` and a pre-CPU optimizer hook
    for all-Vulkan SGD leaves.
  - Added narrow `tensor-backends` capability keys
    `ml-optimizer-sgd-float64` and `ml-optimizer-sgd-float32`; CPU reports both
    true and Vulkan reports `ml-optimizer-sgd-float32` with Vulkan Float32
    availability. Broad `ml-optimizer` remains false.
  - Updated tests so Adam/RMSProp Vulkan paths remain fail-closed while Vulkan
    Float32 SGD succeeds and preserves device placement.
  - Recorded the custom `Kernel` design decision: `Kernel` is a real type/value;
    optional `(define [kernel] ...)` sugar must desugar to
    `(define name (Kernel spec))`; access uses Omni path/index syntax such as
    `k.inputs.[0].name`; execution stays explicit through `kernel/run`.
- Commands run so far:
  - `glslangValidator -V --target-env vulkan1.0` for all three SGD shaders
  - `spirv-val --target-env vulkan1.0` for all generated SGD SPIR-V binaries
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - Direct Vulkan `ml/optimizer-step` smokes for stateless SGD and momentum
  - Direct Adam Vulkan fail-closed smoke
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=build:/usr/local/lib ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=build:/usr/local/lib ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=build:/usr/local/lib ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Direct stateless Vulkan SGD returned Vulkan output with CPU copyback values
    approximately `[1.93, 4.06]`.
  - Direct momentum initialization returned Vulkan parameter and velocity
    tensors with copyback values approximately `[0.95]` and `[0.5]`.
  - Focused advanced collections passed: `pass=1803 fail=0`.
  - Basic Lisp passed: `pass=160 fail=0`.
  - Compiler slice passed: `pass=281 fail=0`.
  - No code file is above the 700 LOC cap.
- Unresolved issues:
  - Vulkan Adam, AdamW, RMSProp, and clipping kernels remain open.
  - CUDA optimizer kernels remain open.
  - `nn/train-step` remains blocked on the remaining autograd and optimizer
    surfaces.
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` has not been run.
- Next actions:
  - Commit and push this slice.
  - Continue `ML-VK-060` with Vulkan Adam/AdamW/RMSProp or move back to
    `ML-VK-050` autograd prerequisites if training facade work is still blocked.
- Signature: Codex GPT-5.4
