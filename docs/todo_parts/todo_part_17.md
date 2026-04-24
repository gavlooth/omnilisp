# TODO Part 17

## Vulkan/CUDA/ML Audit Residuals — 2026-04-24

Source: `AUDIT_REPORT_VULKAN_CUDA_ML_2026-04-23.md`.

- [ ] `VKCUDA-AUDIT-M1` keep batch-normalization train-mode running state on the model/device placement contract.
  - classification: runtime behavior, targeted backend placement fix.
  - why: Critical/High audit items were closed first; this Medium item needs a design choice for state migration semantics.
  - next step: update `src/lisp/prim_nn_apply.c3` batch-normalization training forward to either keep Vulkan Float32 running stats on Vulkan or fail closed with a placement diagnostic.
  - prerequisites: run the advanced collections/module ML and NN tests after patching.
  - negative-memory constraint: do not add hidden CPU fallback that silently changes model state placement.

- [ ] `VKCUDA-AUDIT-M2` add CUDA scientific unary domain status checks for invalid real-valued domains.
  - classification: runtime behavior, targeted CUDA kernel status fix.
  - why: left after Critical/High remediation to avoid mixing status semantics with dispatch/race fixes.
  - next step: update `csrc/tensor_cuda_scientific_unary.cu`, regenerate the scientific PTX include, and add tests for `log`, `asin`, and `acos` invalid domains.
  - prerequisites: `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75`.
  - negative-memory constraint: do not rely on NaN propagation as an error signal.

- [ ] `VKCUDA-AUDIT-M3` guard CPU softmax cross-entropy backward against zero `exp_sums`.
  - classification: runtime behavior, targeted numerical stability fix.
  - why: open Medium numerical issue outside the dispatch/optimizer patch family.
  - next step: patch `src/lisp/prim_ml_autograd_tensor_expr_losses.c3` so zero or non-finite softmax denominators fail closed or use a stable fallback path, then add a regression with extreme underflow logits.
  - prerequisites: C3 diagnostics and advanced ML gradient tests.
  - negative-memory constraint: do not mask division-by-zero with a fallback that returns a plausible but unverified gradient.

- [ ] `VKCUDA-AUDIT-M4` decide and implement Vulkan cross-entropy Float64 coverage or explicit unsupported contract.
  - classification: runtime behavior, targeted backend coverage decision.
  - why: dtype coverage mismatch remains after high-severity numerical fixes.
  - next step: inspect `src/lisp/prim_ml_vulkan_losses.c3`; either add Float64 Vulkan cross-entropy or document/fail closed consistently with backend capability reporting.
  - prerequisites: owner-approved dtype capability contract if behavior changes.
  - negative-memory constraint: do not advertise Float64 Vulkan loss support unless kernels and tests exist.

- [ ] `VKCUDA-AUDIT-M5` reconcile generic Vulkan autograd resolution with existing specific Vulkan backward implementations.
  - classification: runtime behavior, structural backend routing fix.
  - why: generic resolver coverage remains inconsistent with specific Vulkan backward files.
  - next step: map `src/lisp/prim_ml_autograd.c3` against `src/lisp/prim_ml_autograd_vulkan_*.c3` and route supported expressions without CPU fallback.
  - prerequisites: advanced Vulkan autograd tests.
  - negative-memory constraint: do not bypass the explicit unsupported fallback for expressions without implemented Vulkan backward semantics.

- [ ] `VKCUDA-AUDIT-M6-L5` add null guards for Vulkan physical-device feature/memory-property function pointers.
  - classification: runtime behavior, targeted dynamic-loader hardening.
  - why: lower-severity null-deref hardening remains after shared-context race fix.
  - next step: guard `omni_vulkan_get_physical_device_features` and `omni_vulkan_get_physical_device_memory_properties` in `csrc/tensor_vulkan_helpers_core.c` probe/selection/memory-type paths.
  - prerequisites: C build and Vulkan backend capability tests.
  - negative-memory constraint: do not assume partial dynamic symbol resolution is impossible.

- [ ] `VKCUDA-AUDIT-L1` harden division backward for subnormal denominator squaring.
  - classification: runtime behavior, targeted autograd numerical fix.
  - why: Low autograd numerical issue remains after Adam correction hardening.
  - next step: patch `src/lisp/prim_ml_autograd_tensor_expr.c3` so `b * b` underflow is checked before division.
  - prerequisites: CPU and Vulkan division-gradient regressions where applicable.
  - negative-memory constraint: do not treat `b != 0.0` as sufficient for the squared denominator.

- [ ] `VKCUDA-AUDIT-L2` decide Box-Muller clamp policy for `nn_rng_normal`.
  - classification: runtime behavior, targeted initializer distribution decision.
  - why: owner/product-level numerical policy remains open.
  - next step: add a short decision note or patch `src/lisp/prim_nn_init.c3` to clamp uniform samples to the chosen lower bound, then update initializer tests.
  - prerequisites: decide whether Omni wants conventional normal tails or current wider deterministic tails.
  - negative-memory constraint: do not change initializer distribution silently without recording the policy.

- [ ] `VKCUDA-AUDIT-L3` prioritize CUDA rounding error status deterministically.
  - classification: runtime behavior, targeted CUDA status reporting fix.
  - why: lower-priority debugging determinism remains open.
  - next step: change `csrc/tensor_cuda_rounding_i64.cu` status writes from first-error wins to priority ordering, regenerate PTX, and add a multi-error regression.
  - prerequisites: `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75`.
  - negative-memory constraint: do not hand-edit generated PTX instead of regenerating from `.cu`.

- [ ] `VKCUDA-AUDIT-L4` define negative-zero derivative behavior for sqrt.
  - classification: runtime behavior, targeted autograd numerical policy.
  - why: Low edge-case behavior remains open.
  - next step: decide whether `-0.0` follows zero or negative-domain behavior in `src/lisp/prim_ml_autograd_tensor_expr.c3`, then add a regression.
  - prerequisites: numerical policy decision for signed zero.
  - negative-memory constraint: do not rely on ordinary `x > 0.0` if signed-zero behavior matters.
