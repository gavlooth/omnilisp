# Vulkan Eigensolver Plan

Date: 2026-04-17
Status: Symmetric real `Float64` and Hermitian fixed-width complex Vulkan
`matrix/eigenvalues` / `matrix/eigenvectors` support dense row-major square
inputs within helper resource limits, 32-bit shader index guards, and the
Jacobi iteration guard. General non-Hermitian Vulkan `matrix/eigenpairs` now
routes native `Float64`, `Float32`, `Complex128`, and `Complex64` tensors to
backend-native complex-valued values/vectors, including exact 2x2
complex-shift spectra and mixed-block spectra with deflated trailing scalars.
The durable live queue is closed as of 2026-04-22.

## Decision

Direct Vulkan eigen execution should extend the existing backend-neutral
`matrix/eigenvalues`, `matrix/eigenvectors`, and `matrix/eigenpairs` surfaces.
Do not add backend-specific public names, hidden CPU/LAPACK fallback, or Big*
lowering.

The eigensolver work has three distinct phases:

1. symmetric real `Float64` eigenvalues/eigenvectors on Vulkan;
2. Hermitian fixed-width `Complex128`/`Complex64` eigenvalues/eigenvectors on
   Vulkan;
3. general nonsymmetric eigenpairs only after a native non-Hermitian complex
   solver design is recorded.

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

Native fixed-width complex Tensor dtypes, CPU fixed-width general
`matrix/eigenpairs`, and Vulkan fixed-width complex general `matrix/eigenpairs`
now exist. The remaining Vulkan eigensolver work is numerical hardening and
performance staging, not storage availability or public result dtype.

Closed `TENSOR-100H-COMPLEX-EIGEN` decisions:

- complex Hermitian cases extend `matrix/eigenvalues` and
  `matrix/eigenvectors`;
- fixed-width numeric `matrix/eigenpairs` returns `Complex128` for
  `Float64`/`Complex128` inputs and `Complex64` for `Float32`/`Complex64`
  inputs;
- Vulkan Hermitian `Complex128`/`Complex64` eigenvalues/eigenvectors are
  implemented with native Jacobi shaders;
- Vulkan `Complex128`/`Complex64` general `matrix/eigenpairs` are implemented
  with native serial shifted-QR shaders and no hidden CPU/LAPACK fallback;
- Vulkan `Float64`/`Float32` general `matrix/eigenpairs` are implemented with
  native real-input shaders that return public `Complex128`/`Complex64`
  values/vectors on Vulkan;
- exact 2x2 complex-shift spectra are handled by a direct analytic eigenpair
  path in all four general Vulkan shaders, avoiding singular shifted-QR steps;
- mixed-block spectra with negligible trailing subdiagonal coupling are handled
  by active-submatrix deflation, scalar trailing-block solve, and leading 2x2
  analytic solve through the accumulated basis;
- CUDA general `matrix/eigenpairs` capability bits remain false.

Closed `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL` solver boundary:

- The shipped first native solver uses a serial shifted complex QR iteration
  over dense row-major buffers, with public fixed-width complex values/vectors,
  private scratch embedded after the public vector payload, and a private status
  buffer.
- The helper returns `tensor/no-convergence` through the normal Vulkan status
  mapping when QR convergence or eigenvector reconstruction fails.
- The public route is enabled for `Float64`, `Float32`, `Complex128`, and
  `Complex64`; real inputs return complex-valued Vulkan result tensors following
  the CPU fixed-width result-dtype contract.
- Staged Hessenberg reduction/multi-kernel QR remains the recommended future
  performance path if larger difficult spectra prove the serial helper is a
  measured bottleneck, but active-submatrix deflation is no longer an open
  correctness blocker for the named mixed-block fixtures.

## Implementation Shape

The symmetric real slice added a dedicated helper,
`omni_tensor_backend_vulkan_symmetric_eigen_f64`, with separate output buffers
for values and vectors. The Hermitian complex slice added
`omni_tensor_backend_vulkan_hermitian_eigen_complex128` and
`omni_tensor_backend_vulkan_hermitian_eigen_complex64` with the same public
values/vectors shape and placement contract.

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

## General Complex Vulkan Design Boundary

Decision recorded 2026-04-22 for
`TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL`: do not ship a 2x2-only or
triangular-only shortcut as the next step. The native Vulkan route must target
one shared non-Hermitian solver boundary for `Complex128` and `Complex64`.

Chosen solver family:

- staged complex Hessenberg reduction plus implicit shifted QR;
- right-eigenvector back-accumulation from the same transformations used by the
  Schur/eigenvalue iteration;
- final per-eigenvalue vector refinement/residual check before public routing.

Rejected/deferred families:

- direct dense QR without Hessenberg reduction: too weak as the shared boundary
  because it spends unnecessary work per iteration and gives poor convergence
  control for clustered spectra;
- realification of complex matrices: invalid as the primary contract because it
  loses the direct complex eigenpair pairing semantics already rejected in TODO
  and memory;
- Hermitian Jacobi reuse: only valid for Hermitian inputs and already shipped
  under `matrix/eigenvalues` / `matrix/eigenvectors`, not general
  `matrix/eigenpairs`;
- hidden CPU/LAPACK fallback: prohibited for Vulkan operands.

Helper ABI target:

- `omni_tensor_backend_vulkan_general_eigen_complex128(input, byte_len, n,
  &out_values, &out_vectors)` and the matching `complex64` helper;
- output `values` is a Vulkan `Complex128`/`Complex64` rank-1 tensor with `n`
  complex elements;
- output `vectors` is a Vulkan `Complex128`/`Complex64` rank-2 tensor with
  shape `[n n]`, columns aligned with `values`;
- the C helper owns all scratch/status buffers and returns only public output
  handles on success;
- status slots must distinguish invalid shape/byte length, unsupported resource
  bounds, non-convergence, and allocation/dispatch failure;
- helper validation must reject `n*n` and byte-count overflow, `n > UINT32_MAX`,
  and any scratch size that cannot be represented in Vulkan descriptor ranges.

Scratch layout target:

- matrix workspace `H` in complex component storage, initialized from input;
- accumulated right-vector matrix `Q`, initialized to identity and updated by
  the same Householder/Givens transformations as `H`;
- per-column Householder/vector scratch and per-step shift/status scratch;
- private status buffer for convergence, deflation count, and first failing
  iteration;
- no public output buffer may contain status sentinels. Unlike symmetric
  eigenvalues, general complex values are a dense complex tensor and status must
  live in a private buffer that is read and destroyed before success returns.

Implementation sub-boundaries:

1. Add helper ABI declarations and fail-closed C entry points with validation,
   no shader routing, and no capability flip.
2. Add checked-in GLSL/SPIR-V for a serial correctness-first prototype covering
   small dense matrices through the same ABI, with private status and output
   buffers. This may be one invocation/workgroup, but it must be the same
   Hessenberg/QR boundary, not a special case.
3. Add C3 routing only after residual tests pass for both `Complex128` and
   `Complex64`, `tensor-backends` capability bits reflect helper availability,
   and LAPACK `zgeev`/`cgeev` counters prove no CPU fallback for Vulkan inputs.
4. Treat larger staged/multi-dispatch performance as a follow-up only after the
   correctness-first native ABI is green.

Validation additions required before capability bits flip:

- Vulkan `Complex128` and `Complex64` `matrix/eigenpairs` values/vectors remain
  Vulkan-placed with result dtype matching CPU fixed-width contract;
- residual checks validate `A * v ~= lambda * v` after explicit CPU copyback;
- triangular, diagonal, non-normal, repeated-value, and non-Hermitian examples;
- no hidden CPU/LAPACK fallback: `zgeev` and `cgeev` counters unchanged for
  Vulkan inputs;
- unsupported layout/view/mixed-device inputs fail closed before helper
  dispatch;
- shader compile, `spirv-val`, helper rebuild, `c3c build --obj-out obj`,
  focused host and bounded-container `advanced-collections-module`, file-size
  gate, and status consistency gate.

## Deferred Boundaries

Open follow-up risks are tracked as explicit TODO items instead of reopening the
closed complex parent:

- none currently; the durable live queue is closed as of 2026-04-22.

Closed follow-up risks:

- `TENSOR-100H-VK-GENERAL-EIGEN-DEFLATION-001`: active-submatrix deflation now
  handles mixed-block Vulkan general eigen spectra for `Float64`, `Float32`,
  `Complex128`, and `Complex64`, including 3x3 fixtures with a 2x2 rotation
  block plus trailing scalar.
- `TENSOR-100H-VK-REAL-GENERAL-EIGEN-001`: native Vulkan real-valued general
  `matrix/eigenpairs` for dense row-major `Float64` and `Float32` now routes
  publicly and returns Vulkan `Complex128`/`Complex64` values/vectors with
  no-LAPACK validation.
- `TENSOR-100H-VK-COMPLEX-GENERAL-EIGEN-HARDENING-001`: exact 2x2 complex-shift
  hardening is implemented, named real/complex general eigenpair probes are
  recorded, and the remaining mixed-block deflation gap is split into its own
  semantic closure item.

Still out of scope for these follow-up items unless explicitly promoted:

- strided or view-backed inputs;
- hidden CPU construction of fixed-width complex outputs;
- backend-named public eigen surfaces.

## Negative Constraints

- Do not copy Vulkan input to CPU and call LAPACK.
- Do not introduce backend-named public eigen surfaces.
- Do not reuse the Hermitian Jacobi shader as a general non-Hermitian complex
  eigensolver.
- Do not regress the active-submatrix deflation path by writing reconstructed
  leading 2x2 output columns directly over the accumulated basis before all
  columns are computed; use scratch output staging and copy back afterward.
- Do not reopen the closed fixed-width complex general eigen parent for
  performance hardening; use the explicit hardening TODO and measurement gate.
