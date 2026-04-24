# TODO Part 17

## Vulkan/CUDA/ML Audit Residuals — 2026-04-24

Source: `AUDIT_REPORT_VULKAN_CUDA_ML_2026-04-23.md`.

- [x] `VKCUDA-AUDIT-M1` keep batch-normalization train-mode running state on the model/device placement contract.
  - classification: runtime behavior, targeted backend placement fix.
  - done 2026-04-24: train-mode `nn/forward` batch normalization now fails
    closed when input, affine params, or running state are non-CPU, preventing
    hidden CPU updates after touching Vulkan state.
  - validation: C3 diagnostics; `c3c build --obj-out obj`; bounded
    `advanced-collections-module` (`2067 passed, 0 failed`).
  - negative-memory constraint: do not add hidden CPU fallback that silently
    changes model state placement.

- [x] `VKCUDA-AUDIT-M2` add CUDA scientific unary domain status checks for invalid real-valued domains.
  - classification: runtime behavior, targeted CUDA kernel status fix.
  - done 2026-04-24: CUDA scientific unary kernels now check `log`, `log10`,
    `asin`, and `acos` domains explicitly, use priority status writes, and the
    generated scientific PTX includes `atom.global.max.u32` status writes.
  - validation: `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75` for
    `csrc/tensor_cuda_scientific_unary.cu`; C3 diagnostics; `c3c build
    --obj-out obj`; bounded `advanced-collections-module`
    (`2067 passed, 0 failed`).
  - negative-memory constraint: do not rely on NaN propagation as an error
    signal.

- [x] `VKCUDA-AUDIT-M3` guard CPU softmax cross-entropy backward against zero `exp_sums`.
  - classification: runtime behavior, targeted numerical stability fix.
  - done 2026-04-24: CPU tensor softmax-cross-entropy backward now rejects
    zero or non-finite softmax denominators before forming gradients.
  - validation: C3 diagnostics; `c3c build --obj-out obj`; bounded
    `advanced-collections-module` (`2067 passed, 0 failed`).
  - negative-memory constraint: do not mask division-by-zero with a fallback
    that returns a plausible but unverified gradient.

- [x] `VKCUDA-AUDIT-M4` decide and implement Vulkan cross-entropy Float64 coverage or explicit unsupported contract.
  - classification: runtime behavior, targeted backend coverage decision.
  - done 2026-04-24: Vulkan cross-entropy Float64 remains explicitly
    unsupported; diagnostics now name the Float32-only capability contract.
  - validation: C3 diagnostics; `c3c build --obj-out obj`; bounded
    `advanced-collections-module` (`2067 passed, 0 failed`).
  - negative-memory constraint: do not advertise Float64 Vulkan loss support
    unless kernels and tests exist.

- [x] `VKCUDA-AUDIT-M5` reconcile generic Vulkan autograd resolution with existing specific Vulkan backward implementations.
  - classification: runtime behavior, structural backend routing fix.
  - done 2026-04-24: generic linear `ml/grad` paths reject Vulkan operands
    explicitly, while implemented tensor-expression Vulkan backward paths stay
    separate and fail closed for unsupported Float64 softmax cross-entropy.
  - validation: C3 diagnostics; `c3c build --obj-out obj`; bounded
    `advanced-collections-module` (`2067 passed, 0 failed`).
  - negative-memory constraint: do not bypass the explicit unsupported fallback
    for expressions without implemented Vulkan backward semantics.

- [x] `VKCUDA-AUDIT-M6-L5` add null guards for Vulkan physical-device feature/memory-property function pointers.
  - classification: runtime behavior, targeted dynamic-loader hardening.
  - done 2026-04-24: Vulkan physical-device selection, probe/context feature
    reads, and host-visible memory-type lookup now fail closed if the feature
    or memory-property query functions are unavailable.
  - validation: C3 diagnostics; `c3c build --obj-out obj`; bounded
    `advanced-collections-module` (`2067 passed, 0 failed`).
  - negative-memory constraint: do not assume partial dynamic symbol resolution
    is impossible.

- [x] `VKCUDA-AUDIT-L1` harden division backward for subnormal denominator squaring.
  - classification: runtime behavior, targeted autograd numerical fix.
  - done 2026-04-24: CPU tensor-expression division backward validates the
    denominator and `b * b` before right-side gradient division, rejecting
    subnormal or non-finite squares.
  - validation: C3 diagnostics; `c3c build --obj-out obj`; bounded
    `advanced-collections-module` (`2067 passed, 0 failed`).
  - negative-memory constraint: do not treat `b != 0.0` as sufficient for the
    squared denominator.

- [x] `VKCUDA-AUDIT-L2` decide Box-Muller clamp policy for `nn_rng_normal`.
  - classification: runtime behavior, targeted initializer distribution decision.
  - done 2026-04-24: `nn_rng_normal` now makes the Box-Muller lower-bound
    policy explicit with `NN_INIT_BOX_MULLER_MIN_U1`, preserving the existing
    midpoint-sampled lower bound while preventing accidental `log(0)`.
  - validation: C3 diagnostics; `c3c build --obj-out obj`; bounded
    `advanced-collections-module` (`2067 passed, 0 failed`).
  - negative-memory constraint: do not change initializer distribution
    silently without recording the policy.

- [x] `VKCUDA-AUDIT-L3` prioritize CUDA rounding error status deterministically.
  - classification: runtime behavior, targeted CUDA status reporting fix.
  - done 2026-04-24: CUDA rounding kernels use priority status writes, the
    generated rounding PTX contains `atom.global.max.u32`, and the host launcher
    maps status `2` to `OMNI_TENSOR_CUDA_INVALID_ARGUMENT`.
  - validation: `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75` for
    `csrc/tensor_cuda_rounding_i64.cu`; C3 diagnostics; `c3c build
    --obj-out obj`; bounded `advanced-collections-module`
    (`2067 passed, 0 failed`).
  - negative-memory constraint: do not hand-edit generated PTX instead of
    regenerating from `.cu`.

- [x] `VKCUDA-AUDIT-L4` define negative-zero derivative behavior for sqrt.
  - classification: runtime behavior, targeted autograd numerical policy.
  - done 2026-04-24: CPU tensor-expression sqrt backward treats signed zero,
    including `-0.0`, as undefined and raises `tensor/invalid-argument`; method
    table fallback primitives are recognized when matching unary derivatives.
  - validation: C3 diagnostics; `c3c build --obj-out obj`; bounded
    `advanced-collections-module` (`2067 passed, 0 failed`).
  - negative-memory constraint: do not rely on ordinary `x > 0.0` if
    signed-zero behavior matters.

- [x] `SCOPED-MODULE-AOT-001` add AOT lowering for scoped module open.
  - classification: runtime behavior, structural compiler/runtime parity.
  - blocker/task: `(with mod body...)` is implemented for JIT/runtime but AOT
    currently emits an explicit unsupported diagnostic for `E_WITH_MODULE`.
  - why: a correct AOT implementation must preserve child-env scoping and must
    not emulate scoped open by importing exports into `global_env`.
  - concrete next step: design a compiler/runtime bridge that can enter a
    temporary import child environment while compiling body expressions, then
    add compiler slice tests for unqualified exports, shadowing, and no-leak
    behavior.
  - prerequisites: decide how AOT symbol references inside the `with` body map
    to dynamically opened module exports without turning them into permanent C
    globals.
  - negative-memory constraint: do not implement AOT scoped open as
    `import 'all` plus global cleanup; that would be scope-observable and
    unsafe around errors.
  - done 2026-04-25: `AUDIT-048` closed after AOT lowering gained scoped
    module open support with generated local aliases, tail-position lowering,
    alias-shadowing preservation, and compile-mode runtime bootstrap for module
    export resolution.
  - validation: compiler slice and scoped-module open compiler regressions
    recorded in `AUDIT.md`.
