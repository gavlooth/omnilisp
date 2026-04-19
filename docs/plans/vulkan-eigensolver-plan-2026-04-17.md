# Vulkan Eigensolver Plan

Date: 2026-04-17
Status: Symmetric real `Float64` Vulkan `matrix/eigenvalues` and
`matrix/eigenvectors` support dense row-major square inputs, including `n > 64`
through storage-buffer scratch within helper resource limits, 32-bit shader
index guards, and the Jacobi iteration guard; residual boundaries remain below.

## Decision

Direct Vulkan eigen execution should extend the existing backend-neutral
`matrix/eigenvalues`, `matrix/eigenvectors`, and `matrix/eigenpairs` surfaces.
Do not add backend-specific public names, hidden CPU/LAPACK fallback, or Big*
lowering.

The eigensolver work has two distinct phases:

1. symmetric real `Float64` eigenvalues/eigenvectors on Vulkan;
2. general nonsymmetric eigenpairs only after Omni has a native fixed-width
   complex Tensor dtype or an explicitly approved replacement for the current
   `BigComplex` output contract.

## Symmetric Real Contract

The first Vulkan slice covers:

- `matrix/eigenvalues`;
- `matrix/eigenvectors`.

Public contract to preserve:

- Input is a square dense row-major Vulkan-placed `Float64` Tensor, including
  `n > 64` within helper resource limits, 32-bit shader index guards, and the
  Jacobi iteration guard.
- `matrix/eigenvalues` returns a Vulkan-placed `Float64` Tensor with shape `[n]`.
- `matrix/eigenvectors` returns a dictionary with:
  - `values`: a Vulkan-placed `Float64` Tensor with shape `[n]`;
  - `vectors`: a Vulkan-placed `Float64` Tensor with shape `[n n]`, where each
    column is the normalized eigenvector aligned with the same-position value.
- Eigenvalues are sorted descending.
- Eigenvector signs are normalized to match the existing CPU oracle tolerance.
- Empty square input preserves the existing CPU shape contract.
- Nonsymmetric Vulkan input raises `tensor/not-symmetric` only after the Vulkan
  path has accepted the operand as an otherwise supported Vulkan matrix.
- Unsupported dtype, layout, non-square shape, resource bounds, mixed-device
  input, missing Vulkan/Float64 capability, or unsupported lazy operand state raises
  `tensor/backend-unsupported`.
- Shader non-convergence raises `tensor/no-convergence`.

## Implemented Checkpoint

The landed symmetric real slice added:

- `csrc/tensor_vulkan_symmetric_eigen_f64.comp` and generated
  `csrc/tensor_vulkan_symmetric_eigen_f64_spv.c`;
- `omni_tensor_backend_vulkan_symmetric_eigen_f64` with one input buffer and
  separate Vulkan output buffers for values and vectors;
- C3 route wiring so concrete and lazy Vulkan `matrix/eigenvalues` /
  `matrix/eigenvectors` execute before the CPU resolver;
- status mapping for Vulkan nonsymmetric input to `tensor/not-symmetric`;
- availability-gated tests for value/vector copyback, output device placement,
  lazy Vulkan input realization, nonsymmetric diagnostics, large-`n`
  storage-buffer scratch execution, resource-bound fail-closed behavior, and
  no-LAPACK-counter behavior.

The helper uses storage-buffer scratch behind the public output buffers, so it
is no longer capped by the old `n <= 64` private-array strategy. Performance
measurement, tiling, and staged execution remain future optimization
boundaries, not CPU fallbacks.

## General Eigenpair Contract

Native fixed-width complex Tensor dtypes now exist, so the remaining blocker is
not storage availability. The remaining blocker is the public result contract
for complex eigen outputs.

`TENSOR-100H-COMPLEX-EIGEN` must decide and implement:

- whether complex symmetric/Hermitian cases extend `matrix/eigenvalues` and
  `matrix/eigenvectors`, or stay under `matrix/eigenpairs`;
- whether general nonsymmetric fixed-width complex eigenpairs return
  `Complex128`/`Complex64` values and vector tensors, and how that relates to
  the existing CPU `BigComplex` `matrix/eigenpairs` contract;
- sorting, phase normalization, residual tolerance, repeated-value behavior,
  and dtype promotion rules;
- backend eligibility and fail-closed behavior for CUDA/Vulkan.

The current recommendation is to keep existing public names and migrate
fixed-width numeric `matrix/eigenpairs` to fixed-width complex output:
`Complex128` for `Float64`/`Complex128` inputs and `Complex64` for
`Float32`/`Complex64` inputs. Do not report Vulkan or CUDA general
`matrix/eigenpairs` support while the public output contract remains
pointer-backed `BigComplex`.

## Implementation Shape

The symmetric real slice should add a dedicated helper, tentatively
`omni_tensor_backend_vulkan_symmetric_eigen_f64`, with separate output buffers
for values and vectors.

The helper should:

- stay within dense row-major `Float64`;
- use checked-in GLSL plus generated SPIR-V C artifacts;
- perform or explicitly validate the symmetry check for Vulkan operands before
  factor iteration;
- keep CPU LAPACK and pure CPU Jacobi eigensolver as semantic oracles only, not
  runtime fallback for Vulkan operands;
- return typed status for unsupported input, not-symmetric input, and
  non-convergence;
- sort values and vectors together before returning.

## Validation Gate

For the implemented direct Vulkan `matrix/eigenvalues` / `matrix/eigenvectors`
slice, the validation gate is:

- shader compile and `spirv-val`;
- `./scripts/build_omni_chelpers.sh`;
- `c3c build --obj-out obj`;
- focused public smokes for diagonal, symmetric dense, repeated-value,
  nonsymmetric, singular, and empty square matrices;
- vector residual checks after explicit CPU copyback;
- device-placement checks proving returned values and vectors are Vulkan tensors;
- no-LAPACK counter checks for concrete and lazy Vulkan operands;
- diagnostic-order checks proving unsupported Vulkan operands fail with Tensor
  backend diagnostics before CPU-only shape or storage errors;
- focused `advanced-collections-module`;
- `./scripts/check_primitive_docs_parity.sh`;
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`;
- `git diff --check`.

## Deferred Boundaries

These are not part of the first symmetric real Vulkan eigensolver slice:

- performance-oriented tiling or staged execution for large symmetric
  eigensolvers;
- `matrix/eigenpairs` for general nonsymmetric matrices;
- `Float32` eigen solvers;
- fixed-width complex eigen solvers;
- strided or view-backed inputs;
- hidden CPU construction of `BigComplex` outputs.

## Negative Constraints

- Do not copy Vulkan input to CPU and call LAPACK.
- Do not introduce backend-named public eigen surfaces.
- Do not lower `BigComplex` into Vulkan fixed-width storage.
- Do not report general `matrix/eigenpairs` as Vulkan-supported while the public
  output contract remains pointer-backed `BigComplex`.
