# Vulkan Dtype And Layout Policy

Date: 2026-04-17
Status: Decision note for `TENSOR-100F` Vulkan math-library work after the
closed `TENSOR-100E` baseline

## Decision

Vulkan backend kernels accelerate Omni Tensor operations only when the
language-visible dtype and layout can be preserved exactly. The backend must
not introduce a backend-specific Tensor type, backend-named math API, hidden
CPU/GPU transfer, or silent dtype conversion.

The current Vulkan public contract remains:

- explicit placement through `(to-device tensor 'vulkan)`,
- explicit copyback through `(to-device tensor 'cpu)`,
- capability discovery through `tensor-backends`,
- backend-neutral execution through `map`, `contract`, and later eligible
  `matrix/*` surfaces.

## Dtype Policy

Current native Tensor dtypes are `Float64`, `Float32`, `BigInteger`,
`BigFloat`, and `BigComplex`.

Vulkan kernels may target a dtype only when all of these are true:

1. The dtype has concrete, homogeneous, fixed-width device storage.
2. The shader can preserve the dtype's language semantics without loss.
3. `tensor-backends` reports the required device capability.
4. Unsupported devices fail closed with Tensor backend diagnostics.

Therefore:

- `Float64` remains the first Vulkan dtype because it is the only current
  Vulkan-enabled fixed-width Tensor dtype and can be guarded by Vulkan
  `shaderFloat64`.
- `Float32` is not a Vulkan fallback for `Float64`. It is a separate native
  Tensor dtype on CPU, and Vulkan `Float32` placement/copyback, destination
  `realize`, dense row-major `map`, unary helpers, direct `min`/`max`, rank-N
  `contract`, structural matrix kernels (`transpose`, `diagonal`,
  `diagonal-matrix`, `trace`), direct `matrix/rank`, and direct `matrix/norm`
  selectors default/`'frobenius`, `'one`, `'infinity`, and `'max`, SVD-backed
  outputs, serial factor/solve surfaces, and staged parallel solve have landed
  behind explicit capability reporting. Large-dense SVD robustness has landed
  for dense row-major inputs through scale-aware eigenvalue tolerance plus
  orthonormal completion. Remaining layout/dtype boundaries are recorded in
  `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`.
- `BigInteger`, `BigFloat`, and `BigComplex` remain CPU/exact or
  high-precision Tensor dtypes. Vulkan must not lower them to fixed-width
  approximations.
- Fixed-width complex CPU scalar/Tensor support exists as `Complex128` and
  `Complex64`. Vulkan raw storage, elementwise map, contract, structural
  matrix acceleration, and the landed numerical subset use explicit complex
  backend capability bits and must not reuse pointer-backed `BigComplex`. The
  landed Vulkan numerical subset covers LU/determinant/solve/inverse/rank/norm/
  singular-values/QR/Cholesky for dense row-major zero-offset fixed-width
  complex tensors.

Recommended dtype expansion order:

1. Keep extending `Float64` kernels under the existing capability gate.
2. Add Vulkan `Float32` only through
   `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`, after
   real device kernels and copy paths preserve the landed CPU Tensor dtype
   semantics.
3. Treat fixed-width complex Vulkan numerical matrix support as
   operation-family gated. The landed subset covers `matrix/lu`,
   `matrix/determinant`, `matrix/solve`, `matrix/inverse`, `matrix/rank`,
   `matrix/norm`, `matrix/singular-values`, `matrix/qr`, and
   `matrix/cholesky`; continue additional complex numerical families only with
   their own helper ABI, capability bits, status mapping, CPU oracle coverage,
   and kernel validation.
4. Keep `BigInteger`, `BigFloat`, and `BigComplex` on CPU paths unless a future
   exact arbitrary-precision device representation is designed explicitly.

### Real Vs Complex

Real fixed-width Vulkan support and complex Vulkan support are separate
language contracts.

Real support can grow one native dtype at a time:

- `Float64` is already native Tensor storage and remains the active Vulkan
  dtype.
- `Float32` has CPU Tensor constructor/storage semantics; Vulkan still
  requires a `tensor-backends` capability bit plus real `Float32` kernels and
  copy paths before any device dispatch accepts it.

Complex support uses native fixed-width Tensor dtypes rather than reuse of
`BigComplex`. `Complex128` and `Complex64` define the scalar names, component
precision, CPU Tensor oracle for construction, component helpers,
equality/hash, `map`, and `contract`, plus explicit CUDA/Vulkan storage
round-trips for concrete zero-offset dense row-major tensors when
`tensor-backends` reports `complex128` or `complex64`. Vulkan additionally
supports dense row-major `Complex128`/`Complex64` elementwise map, contract, and
structural matrix kernels when their operation capability bits are reported.
Vulkan complex LU/determinant/solve is landed as the first numerical family
behind `matrix-numerical-complex128` and `matrix-numerical-complex64`, with
capability reporting, status mapping, and validation recorded.

Before additional Vulkan complex numerical matrix kernel families are eligible,
Omni must define:

- which CPU oracle operations are implemented first on device;
- pivot magnitude and tolerance rules for complex matrix algorithms;
- separate `tensor-backends` operation capability reporting for each
  fixed-width complex dtype. Storage capability is already separate from
  kernel capability and must not be treated as permission to run complex
  Vulkan kernels.

`BigComplex` remains the high-precision pointer-backed complex dtype. It must
not be silently lowered to fixed-width complex Vulkan storage.

## Layout Policy

Current Vulkan kernels target owned dense row-major contiguous Tensor storage.

Vulkan kernels may target a layout only when all of these are true:

1. The operation can map the logical Tensor shape to device addresses exactly.
2. The backend can validate rank, axes, strides, and aliasing before dispatch.
3. The result shape and dtype match the CPU Tensor oracle.
4. Unsupported layouts fail closed rather than copying or reordering silently.

Therefore:

- Dense row-major contiguous rank-N layouts are now the active shipped Vulkan
  layout contract for the supported `Float64` map and multi-axis contract
  kernels.
- Zero-size contracted axes and zero-size free dimensions must preserve the CPU
  Tensor oracle.
- Zero-axis contractions are part of the active dense row-major Vulkan contract.
- Strided views, transposed views, broadcast views, packed triangular storage,
  and sparse layouts require separate layout metadata and tests before Vulkan
  dispatch may target them.
- Vulkan transpose-view materialization is landed as a narrower boundary than
  general strided dispatch. It can materialize a direct rank-2 transpose view
  into dense Vulkan storage at explicit placement/realization/copyback
  boundaries, but it does not make arbitrary view-backed Vulkan kernels
  eligible without a separate ABI and validation plan.
- A future Vulkan stride-aware descriptor should represent shape, strides,
  offset, dtype, rank, and device buffer explicitly. Until that exists,
  non-contiguous layouts stay unsupported on Vulkan.

Recommended layout expansion order:

1. Extend the existing dense row-major `Float64` kernel family.
2. Add more rank-N `Float64` operations that use the same contiguous layout
   contract.
3. Add stride-aware view dispatch only after view metadata and aliasing rules
   are explicit in the runtime.

## Next Implementation Boundary

The next useful Vulkan implementation slice should stay within the current
`Float64` dense row-major policy and broaden kernel coverage, not dtype/layout
semantics.

Completed follow-up slice:

- add a small policy-backed `Float64` Vulkan `map` kernel family for exact-shape
  dense row-major Tensor/scalar and Tensor/Tensor elementwise arithmetic, still
  through public `map`;
- add rank-polymorphic dense row-major metadata dispatch for right-aligned
  singleton-axis Tensor/Tensor `map` broadcasting;
- add rank-polymorphic dense row-major metadata dispatch for public
  single-axis `contract`, preserving free-left-then-free-right output axis
  order and zero-size Tensor oracle semantics;
- add rank-polymorphic dense row-major metadata dispatch for public multi-axis
  `contract`, preserving free-left-then-free-right output axis order and
  zero-size Tensor oracle semantics;
- add a dense row-major `Float64` rank-2 `matrix/transpose` kernel through the
  public backend-neutral matrix surface;
- add dense row-major `Float64` `matrix/diagonal` and
  `matrix/diagonal-matrix` structural kernels through the public
  backend-neutral matrix surface;
- add a dense row-major `Float64` `matrix/trace` reducer through the public
  backend-neutral matrix surface, computing on Vulkan and copying back only
  the scalar result required by the public scalar contract;
- add a dense row-major `Float64` `matrix/rank` reducer through the public
  backend-neutral matrix surface, computing on Vulkan and copying back only
  the scalar `Integer` result required by the public scalar contract;
- add dense row-major `Float64` `matrix/lu` through the public backend-neutral
  matrix surface, returning the combined `lu` factor as a Vulkan-placed Tensor
  and copying back only the `pivots` plus `swap-count` metadata required by
  the public dictionary contract;
- add a dense row-major `Float64` `matrix/determinant` reducer through the
  public backend-neutral matrix surface, computing on Vulkan and copying back
  only the scalar `Float64` result required by the public scalar contract;
- add dense row-major `Float64` `matrix/inverse` through the public
  backend-neutral matrix surface, computing on Vulkan, returning a
  Vulkan-placed inverse Tensor, and raising `tensor/singular-matrix` for
  singular inputs;
- add dense row-major `Float64` `matrix/solve` through the public
  backend-neutral matrix surface, accepting Vulkan-placed rank-1 or rank-2
  right-hand tensors, returning Vulkan-placed solution tensors, and raising
  `tensor/singular-matrix` for singular systems;
- add dense row-major `Float64` `matrix/cholesky` through the public
  backend-neutral matrix surface, returning a Vulkan-placed lower factor
  Tensor and raising `tensor/not-positive-definite` for nonsymmetric or
  non-SPD inputs;
- add dense row-major `Float64` `matrix/qr` through the public
  backend-neutral matrix surface, returning Vulkan-placed reduced `q` and `r`
  factor Tensors and raising `tensor/singular-matrix` for rank-deficient
  inputs;
- add dense row-major `Float64` `matrix/norm` reducers for
  default/`'frobenius`, `'one`, `'infinity`, `'max`, `'spectral`, and
  `'nuclear`, computing on Vulkan and copying back only the scalar result
  required by the public scalar contract. The SVD-backed spectral and nuclear
  selectors route through the storage-backed Vulkan singular-values helper,
  including `k = min(rows, columns) > 64`, without hidden CPU/LAPACK fallback;
- keep unsupported callables, unsupported dtypes, mixed devices, and unsupported
  layouts fail-closed;
- update `tensor-backends` only if additional capability bits are needed.

Recommended next slice:

- follow `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`: continue
  shared Vulkan helper factoring and add the next eligible dense row-major
  `Float64` kernel family; keep stride-aware views blocked until layout and
  aliasing metadata is explicit; treat the measured thresholded parallel
  solver as a closed baseline unless later measurements justify a separate
  performance item.

Do not implement arbitrary-rank strided layout or Big* Vulkan lowering in the
next slice. Fixed-width complex raw storage, map, contract, structural kernels,
and the landed Vulkan numerical subset are already capability-gated; remaining
complex work must stay operation-specific: full complex SVD factor output, CUDA
complex singular-values/norm/SVD, and complex eigen result contracts.
`Float32` Tensor CPU storage plus Vulkan placement/map/unary/minmax, rank-N
contract, structural matrix, direct reducer, SVD-backed, serial factor/solve,
staged parallel solve, and large-dense SVD robustness slices have landed.
Future `Float32` SVD work is performance-oriented tiled/multi-dispatch
execution; stride/view-backed layouts and unsupported dtype families remain
outside the current Vulkan contract.

## Validation

Any dtype/layout expansion must run:

- `./scripts/build_omni_chelpers.sh`,
- `c3c build --obj-out obj`,
- direct Vulkan-visible smokes for each new supported operation,
- focused `advanced-collections-module`,
- bounded-container focused `advanced-collections-module`,
- `scripts/check_primitive_docs_parity.sh`,
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`,
- `git diff --check`.
