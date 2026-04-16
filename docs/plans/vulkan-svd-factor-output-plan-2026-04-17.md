# Vulkan SVD Factor Output Plan

Date: 2026-04-17
Status: Direct Vulkan `matrix/svd` factor-output slice implemented for dense
row-major `Float64` inputs, with the original `k <= 64` private-array cap
replaced by storage-buffer Gram scratch; residual boundaries remain below.

## Decision

Direct Vulkan `matrix/svd` uses the existing backend-neutral `matrix/svd`
public surface. It does not add `vulkan/svd`, `matrix/vulkan-svd`,
backend-specific aliases, hidden CPU/LAPACK fallback, or dtype downcast.

The Vulkan singular-value helper is useful prior art, including for larger-`k`
singular-value extraction, but it is not by itself the complete `matrix/svd`
implementation because the public SVD contract requires reduced factor
outputs, not only singular values.

## Public Contract To Preserve

- Input is a rank-2 dense row-major Vulkan-placed `Float64` Tensor.
- The output is a dictionary with keys exactly `u`, `s`, and `v`.
- `k = min(rows, columns)`.
- `u` has shape `[rows k]`.
- `s` has shape `[k]`, sorted descending.
- `v` has shape `[columns k]`.
- The decomposition is reduced and reconstructs the input as
  `u * diag(s) * transpose(v)`.
- Outputs remain Vulkan-placed `Float64` tensors. CPU inspection requires an
  explicit `(to-device result 'cpu)` copyback for each Tensor output.
- Empty axes preserve the existing CPU shape contract.
- Rank-deficient inputs are supported; zero singular values remain in `s`.
- Unsupported dtype, layout, shader index space, mixed-device input, missing Vulkan
  capability, or unsupported lazy operand state raises `tensor/backend-unsupported`.
- Shader non-convergence raises `tensor/no-convergence`.

## Implementation Shape

The shipped slice uses a dedicated factor-output helper,
`omni_tensor_backend_vulkan_svd_f64`, instead of extending scalar norm readback
or pretending the singular-value-only helper can satisfy `matrix/svd`.

The first implementation slice has landed and:

- stays within dense row-major `Float64`;
- reuses checked-in GLSL plus generated SPIR-V C artifacts;
- stores Gram scratch in Vulkan storage-buffer payload space instead of fixed
  private arrays, removing the original `k <= 64` helper bound while preserving
  32-bit shader index guards;
- returns separate Vulkan buffers for `u`, `s`, and `v`;
- uses a typed status payload for non-convergence and backend execution status;
- normalizes factor ordering and representative vector signs to match the
  existing CPU oracle tolerance;
- keeps CPU LAPACK and pure CPU SVD as semantic oracles only, not runtime
  fallback for Vulkan operands.

The algorithm can start from the existing Gram/Jacobi singular-value path, but
the shipped boundary must include factor construction and orientation. A helper
that only computes `s` does not close this plan.

## Implemented Checkpoint

The landed slice added:

- `csrc/tensor_vulkan_svd_f64.comp` and generated
  `csrc/tensor_vulkan_svd_f64_spv.c`;
- `omni_tensor_backend_vulkan_svd_f64` with one input and three Vulkan output
  buffers;
- C3 route wiring so concrete and lazy Vulkan `matrix/svd` inputs execute
  before the CPU resolver;
- availability-gated tests for square, wide, empty-axis, `k > 64`,
  wrong-rank, output-device, and no-LAPACK-counter behavior.

One implementation detail is now part of the operational constraint: do not
restore a second private `double[4096]` eigenvector array in the shader. That
approach compiled but failed at runtime on this Vulkan stack with
`tensor/backend-execution-failed`. The current shader stores the driving
eigenvectors in the eventual output buffer (`V` for tall/square inputs, `U`
for wide inputs) and keeps only the Gram matrix plus eigenvalues in private
storage.

## Validation Gate

For this implemented slice, the validation run was:

- shader compile and `spirv-val`;
- `./scripts/build_omni_chelpers.sh`;
- `c3c build --obj-out obj`;
- focused public smokes for square, wide, and empty matrices;
- representative factor-value checks after explicit CPU copyback of `u`, `s`,
  and `v`;
- device-placement checks proving all three outputs are Vulkan tensors;
- no-LAPACK counter checks for concrete and lazy Vulkan operands;
- `k > 64` value, placement, and no-LAPACK checks;
- fail-closed checks for wrong rank and unsupported oversized `k`;
- focused `advanced-collections-module`;
- `git diff --check`.

The latest focused runtime validation passed
`advanced-collections-module` with `pass=999 fail=0`.

Supersession: Vulkan `Float32` direct SVD has since landed, and the
large-dense dense all-ones / rank-deficient `65x65` robustness blocker was
closed by scale-aware eigenvalue tolerance plus orthonormal completion in the
`Float32` singular-values/SVD shaders.

## Deferred Boundaries

These remain residual boundaries after the first direct Vulkan `matrix/svd`
slice:

- `TENSOR-100H-SVD-FACTORS`: fixed-width complex `matrix/svd` factor output.
  Closure requires reduced `u`/`s`/`v` semantics for `Complex128` and
  `Complex64`: complex `u`/`v`, component-width real `s`, reconstruction
  checks, rank-deficient and empty-axis behavior, and no substitution with
  singular-value-only helpers;
- strided or view-backed inputs;
- full SVD or economy/full selector options beyond the current reduced public
  contract;
- performance-oriented tiled or multi-dispatch large-`k` algorithms beyond the
  current correctness-first storage-backed single-dispatch helper.

## Negative Constraints

- Do not satisfy `matrix/svd` by copying Vulkan input to CPU and calling LAPACK.
- Do not introduce backend-named public SVD surfaces.
- Do not downcast `Float64` input to `Float32`.
- Do not use scalar norm readback as a substitute for factor-output semantics.
