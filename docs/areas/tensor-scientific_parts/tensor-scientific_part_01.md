# tensor-scientific Part 01

Source: `docs/areas/tensor-scientific.md`

# Tensor Scientific Area

## Canonical Source Of Truth

- Current implementation truth: `memory/CHANGELOG.md`
- Normative language surface: `docs/LANGUAGE_SPEC.md`
- User reference: `docs/reference/03-collections.md`
- Active plan: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`

## Current Status

Status: `yellow`
As of: 2026-04-18

The canonical user-facing surface is:

- `Tensor` for rank-polymorphic scientific numeric values,
- tensor-dispatched `map` for elementwise work,
- `contract` for summed-axis tensor contraction, with paired-axis arrays
  such as `[1 0]` canonical for the common rank-2 case,
- constructor dispatch as the public materialization boundary:
  `Array`, `List`, `Dictionary`, and `Tensor` consume iterators and normalize
  compatible values through their constructors.

`realize` remains available as a low-level Tensor storage primitive for
exact-shape/dtype destination writes, but it is no longer the conceptual
counterpart to lazy iteration. Omni does not need a public Clojure-style
`delay` / `force` pair for iterator or Tensor laziness.

Implemented slices:

- `TENSOR-010`: native Tensor runtime payload, descriptor, print surface,
  lifetime copy/promotion paths, and introspection primitives.
- `TENSOR-020`: `(Tensor Float64 shape data-or-scalar)` and
  `(ref tensor index-array)`.
- `TENSOR-030`: concrete tensor realization and destination copy/fill.
- `TENSOR-040`: tensor-dispatched elementwise `map` for unary, tensor-scalar,
  scalar-tensor, and exact-shape tensor-tensor `Float64` cases.
- `TENSOR-050`: pure `Float64` `contract`, including paired-axis array
  shorthand and explicit left/right axis-list inputs.
- `TENSOR-060A`: destination-fusion audit.
- `TENSOR-060B`: lazy Tensor expression payloads under the existing `Tensor`
  value and destination realization into exact-shape/dtype tensors.
- `TENSOR-060C`: platform-safe shape dimension parsing and zero-size
  contracted-axis identity realization.
- `TENSOR-060D`: realize edge coverage for rank-0 concrete tensors,
  zero-size destination/realization paths, aliased elementwise `map`
  destination realization, and duplicate-axis checks after negative-axis
  normalization.
- `TENSOR-060E`: tensor boundary rollback cleanup for already-copied/promoted
  expression child values, plus generic realized-value cleanup through
  tensor expression edges.
- `TENSOR-060F`: tensor boundary/realize fail-closed hardening for
  non-unique destination retry promotion and malformed concrete source storage.
- `TENSOR-060G`: lazy Tensor `ref` cancels and destroys the temporary concrete
  realization after extracting the scalar result.
- `TENSOR-060H`: nested lazy Tensor `realize` cleans temporary child
  tensors created while resolving nested `map`/`contract` expression operands.
- `TENSOR-060I`: failed lazy Tensor realization cleans the fresh concrete
  result tensor through the generic fresh-value path, with nested `contract`
  realization coverage.
- `TENSOR-060J`: lazy Tensor `realize` into an explicit destination stages
  expression evaluation into a temporary Tensor and commits only after success,
  preserving recursive contract alias rejection while allowing zero-size
  non-aliasing destinations and cleaning failed constructor results.
- `TENSOR-060K`: lazy Tensor expression edge rollback has deterministic
  fail-counter coverage, and root promotion of lazy Tensor expression operands
  keeps tensor edges out of the released child TEMP lane.
- `TENSOR-070`: tensor-tensor `map` supports right-aligned singleton-axis
  broadcasting, including rank-0 tensor scalar broadcasting and deterministic
  `tensor/shape-mismatch` failures for incompatible axes.
- `TENSOR-075`: `(Array tensor)` and `(List tensor)` now materialize Tensor
  expressions and expose flat row-major element values through the canonical
  collection constructor/conversion surfaces.
- `TENSOR-076`: `(Tensor data)`, `(Tensor data Float64)`, and
  `(Tensor Float64 data)` infer `Float64` tensor shape from real numeric scalars
  or rectangular nested arrays/proper lists whose leaves can narrow to finite
  `Float64`.
- `TENSOR-076B`: `(Tensor iterator)`, `(Tensor iterator dtype)`, and
  `(Tensor dtype shape iterator)` consume finite numeric iterators through the
  Tensor constructor path.
- `TENSOR-076C`: `Iterator(^Tensor)` now yields flat row-major Tensor elements
  through the canonical iterator constructor surface. Lazy CPU Tensor
  expressions are realized once into Tensor storage for iteration, and non-CPU
  device tensors fail closed until explicitly copied with `to-device 'cpu`.
- `TENSOR-077`: native `BigFloat` concrete Tensor storage supports
  constructor, `dtype`, `ref`, flat `(Array tensor)` / `(List tensor)`
  conversion, and concrete `realize`.
- `TENSOR-078`: tensor-dispatched `map` now supports native `BigFloat`
  tensors for unary, tensor-scalar, scalar-tensor, exact-shape tensor-tensor,
  and right-aligned singleton-axis broadcast cases. BigFloat lazy map payloads
  clone scalar handles across function-return/closure-capture boundaries.
- `TENSOR-079`: tensor-dispatched `contract` now supports native `BigFloat`
  tensors through the pure C3 contraction fallback, including vector dot,
  rank-2 matrix product, zero-size contracted-axis identity, explicit
  destination realization, and lazy expression boundary survival. BLAS fast
  paths remain `Float64`-only.
- `TENSOR-081`: native `BigInteger` Tensor storage and kernels support
  constructor, `dtype`, `ref`, flat collection conversion, concrete
  `realize`, tensor-dispatched `map`, and pure C3 `contract` for exact
  integer tensor work. BigInteger Tensor data must be exact integers; inexact
  floating data fails closed.
- `TENSOR-082`: native `BigComplex` Tensor storage and kernels support
  constructor, `dtype`, `ref`, flat collection conversion, concrete
  `realize`, tensor-dispatched `map`, and pure C3 `contract` for complex
  tensor work. Real numeric leaves promote to zero-imaginary BigComplex
  elements; BLAS fast paths remain `Float64`-only.
- `TENSOR-083`: BigComplex Tensor component kernels support elementwise
  `real-part`, `imag-part`, and `conjugate`. Component extraction realizes
  lazy BigComplex Tensor sources and returns native BigFloat tensors; conjugate
  returns native BigComplex tensors.
- `TENSOR-084`: real Tensor component semantics are dtype-preserving:
  `real-part` and `conjugate` copy `Float64`, `BigInteger`, and `BigFloat`
  tensors, while `imag-part` returns same-shape zero tensors in the same dtype.
- `TENSOR-085`: Tensor `abs` supports elementwise magnitude for all native
  tensor dtypes. Real Tensor dtypes preserve dtype and shape; `BigComplex`
  Tensor magnitudes return same-shape native `BigFloat` tensors.
- `TENSOR-086`: Tensor `sqrt` supports elementwise square root for all native
  tensor dtypes. `Float64` and `BigInteger` Tensor inputs return `Float64`
  tensors; `BigFloat` and `BigComplex` Tensor inputs preserve dtype.
- `TENSOR-086B`: Tensor unary `+` is an identity operation that preserves the
  Tensor value's current placement. On Vulkan-placed dense row-major `Float64`
  tensors, `(map + tensor)` routes through the separate unary helper identity
  opcode and returns a Vulkan-placed Tensor.
- `TENSOR-087`: Tensor unary scientific math supports `sin`, `cos`, `tan`,
  `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, and `log10`
  through a shared elementwise helper; `math.erf` and `math.erfc` are now
  included for CPU Tensor inputs. `Float64` and `BigInteger` Tensor inputs
  return `Float64` tensors except `Float32` inputs preserve `Float32` for
  `math.erf`/`math.erfc`; `BigFloat` Tensor inputs preserve dtype, and
  `BigComplex` Tensor inputs preserve dtype for the elementary scientific
  family but fail closed for `math.erf`/`math.erfc` until a complex
  error-function contract exists. Dense row-major Vulkan `Float32` tensors now
  route the `sin` through `log10` family, `math.erf`, `math.erfc`, and
  `stats.normal-cdf` through the dedicated `Float32` unary helper for public
  `map` and direct Tensor unary math. Dense row-major Vulkan `Float64` tensors
  route `stats.normal-cdf`
  through a separate arithmetic polynomial approximation in the Float64 unary
  helper; other `Float64` scientific unary operations remain fail-closed rather
  than downcasting because the current Vulkan 1.0 GLSL validation path rejected
  double transcendental builtins.
- `TENSOR-087B`: Tensor `stats.normal-cdf` applies elementwise for CPU Tensor
  inputs, dense row-major CUDA `Float64`/`Float32` tensors, and dense row-major
  Vulkan `Float64`/`Float32` tensors through fixed distribution opcode `19`.
  `Float64` and `Float32` preserve float dtype, `BigInteger` returns `Float64`,
  `BigFloat` preserves dtype, and `BigComplex` fails closed until a complex
  distribution contract exists. Vulkan `Float64` uses a piecewise polynomial
  double approximation without hidden CPU fallback or Float32 downcast.
- `TENSOR-087C`: CPU, CUDA, and Vulkan `Float64`/`Float32` Tensor
  `stats.normal-quantile` apply elementwise
  with a whole-operation failure on the first invalid probability outside
  `0 < p < 1`. `Float64` and `Float32` preserve float dtype, `BigFloat`
  preserves dtype, `BigComplex` fails closed, and non-empty `BigInteger`
  probability tensors fail because integer probabilities cannot satisfy the
  open interval. CUDA dense row-major `Float64`/`Float32` uses generated CUDA
  C/libdevice PTX op `20` plus a probability-status word so invalid elements
  fail before result exposure. Vulkan dense row-major `Float64`/`Float32` uses
  dedicated status-bearing inverse-CDF shader/helpers with the same
  diagnostics. The advanced stdlib module tests cover direct/map Vulkan
  placement, dtype preservation, representative `0.025`/`0.5`/`0.975` values,
  and invalid-probability/non-finite diagnostics for Vulkan `Float64`
  quantile.
- `TENSOR-088`: Tensor `pow` supports tensor-scalar, scalar-tensor, and
  broadcast tensor-tensor powers. `BigComplex` wins the result dtype if either
  input is complex, `BigFloat` wins if either input is BigFloat, and remaining
  real/exact inputs return `Float64` tensors.
- `TENSOR-089`: Tensor `atan2` supports tensor-scalar, scalar-tensor, and
  broadcast tensor-tensor real-plane arctangent. `BigFloat` inputs preserve
  dtype, remaining real/exact inputs return `Float64` tensors, and complex
  operands fail closed.
- `TENSOR-090`: Tensor `floor`, `ceiling`, `round`, and `truncate` return
  same-shape native `BigInteger` Tensor results for real Tensor inputs.
  BigFloat tensors use the existing exact BigFloat rounding path; BigComplex
  tensors fail closed. CUDA support is exposed through the
  `rounding-big-integer` backend capability for dense row-major
  `Float64`/`Float32` inputs and materializes native CPU `BigInteger` Tensor
  results after the CUDA rounding kernel copies fixed-width integer results
  back from device scratch. Vulkan support uses the same backend
  `rounding-big-integer` capability for dense row-major `Float64`/`Float32`
  inputs when `shaderInt64` is available, then materializes CPU
  `BigInteger` Tensor results after checked device integer copyback. Vulkan
  rounding is not implied by generic `available`, `float64`, or `float32`
  capability.
- `TENSOR-091`: Tensor `min` and `max` support tensor-scalar, scalar-tensor,
  and broadcast tensor-tensor real comparison. `BigFloat` wins if either
  input is BigFloat, `Float64` wins if either input is Float64, otherwise the
  result is native `BigInteger` Tensor storage. Complex operands fail closed.
- `TENSOR-092`: Tensor `gcd` and `lcm` support tensor-scalar, scalar-tensor,
  and broadcast tensor-tensor exact integer inputs. Tensor operands must be
  native `BigInteger` tensors; inexact and complex Tensor dtypes fail closed.
  Results are native `BigInteger` Tensor storage.
- `TENSOR-080`: optional backend boundary contract is closed as a design-only
  slice; BLAS/LAPACK/CUDA/cuBLAS work stays optional behind the pure `Tensor`
  fallback. Ordinary Tensor storage remains native/scoped; truly opaque
  foreign backend resources still require explicit ownership/finalizer policy.
- `TENSOR-090A`: dense rank-2 `Float64` contraction equivalent to
  `(contract a b [1 0])` now has an optional native BLAS `dgemm` fast path
  behind the Tensor evaluator, with unsupported cases falling back to the pure
  C3 contraction kernel.
- `TENSOR-090B`: the optional native BLAS `dgemm` fast path now covers every
  contiguous row-major rank-2 single-axis contract layout by passing transpose
  flags for `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`.
- `TENSOR-090C`: the optional native BLAS `dgemv` fast path now covers
  contiguous row-major rank-2/rank-1 and rank-1/rank-2 single-axis `Float64`
  contracts, including transposed matrix-vector and vector-matrix layouts.
- `TENSOR-090D`: the optional native BLAS `ddot` fast path now covers
  contiguous rank-1/rank-1 single-axis `Float64` vector dot contractions.
- `TENSOR-090E`: the optional native BLAS `dger` fast path now covers
  contiguous rank-1/rank-1 zero-axis `Float64` outer-product contractions.
- `TENSOR-090F`: `matrix/solve` is the first public rank-2 Tensor solver
  surface. It solves square `Float64` coefficient systems with rank-1 or
  rank-2 `Float64` right-hand sides through a pure runtime fallback; singular
  systems raise `tensor/singular-matrix`. LAPACK/LAPACKE acceleration remains
  an optional backend follow-on behind the same surface.
- `TENSOR-090G`: `matrix/solve` now probes `LAPACKE_dgesv` at runtime and uses
  it when available, while missing LAPACKE or unsupported backend cases retain
  the pure runtime solver fallback.
- `TENSOR-090AM`: `matrix/solve` and `matrix/inverse` now have forced pure
  fallback coverage through a private test-only `dgesv` disable hook; the
  `dgesv` backend path no longer depends on the unrelated `dgeev` disable
  state.
- `TENSOR-090H`: `matrix/lu` is the first public rank-2 Tensor decomposition
  surface. It returns combined partial-pivot LU factors plus final 0-based
  pivot row order and swap count metadata; singular inputs raise
  `tensor/singular-matrix`.
- `TENSOR-090I`: `matrix/determinant` is the first decomposition consumer. It
  reuses the pure partial-pivot LU kernel and returns a `Float64` scalar,
  including `0.0` for singular matrices and `1.0` for empty square matrices.
- `TENSOR-090T`: `matrix/transpose` transposes rank-2 Tensor values while
  preserving native dtype across `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex`. Concrete Tensor inputs materialize a transposed Tensor, while
  transpose-view inputs compose structurally back to the source orientation.
- `TENSOR-090T2`: `matrix/transpose-view` constructs a read-only rank-2
  transpose view over a Tensor source. The view swaps logical shape and
  strides, keeps the source Tensor as owner, reports `tensor-layout` payload
  `view`, owner `view-source`, `owns-storage` false, and write-policy
  `read-only-view`, and supports CPU `ref`, flat `(Array view)` /
  `(List view)`, and CPU `realize` materialization. Writes through the view
  fail closed. CUDA/Vulkan placement, raw copy helpers, and device kernels
  remain fail-closed for view payloads until explicit view-aware backend ABIs
  land.
- `TENSOR-090T3`: Vulkan transpose-view materialization is landed. The shipped
  boundary is explicit materialization of direct rank-2 transpose views into
  dense Vulkan storage where placement/realization/copyback requires it, not
  arbitrary stride-aware Vulkan kernel execution. Runtime validation records
  CPU oracle parity, unsupported-view diagnostics, and host/container
  test results.
- `TENSOR-090V`: `matrix/diagonal` extracts the main diagonal from rank-2
  Tensor values into a rank-1 Tensor of length `min(rows, columns)`, preserving
  native dtype and cloning owned high-precision element handles.
- `TENSOR-090W`: `matrix/diagonal-matrix` builds a square rank-2 Tensor from
  a rank-1 Tensor, preserving native dtype, cloning owned high-precision
  diagonal handles, and filling off-diagonal cells with the dtype's zero value.
- `TENSOR-090X`: `matrix/identity` builds a square identity Tensor from a
  non-negative integer size, defaults to `Float64`, and supports optional
  `Float64`, `BigInteger`, `BigFloat`, and `BigComplex` dtype selection.
- `TENSOR-090U`: `matrix/trace` sums the diagonal of square rank-2 Tensor
  values and returns a scalar in the Tensor's native numeric family: `Double`
  for `Float64`, or `BigInteger`, `BigFloat`, and `BigComplex` for the
  matching high-precision Tensor dtypes.
- `TENSOR-090Y`: `matrix/rank` returns the numerical rank of rectangular
  rank-2 `Float64` Tensor values as an `Integer`, with default tolerance
  `1e-12` and optional non-negative finite tolerance selection.
- `TENSOR-090Z`: `matrix/rank` now uses optional runtime-loaded
  `LAPACKE_dgesvd` acceleration when available, counting returned singular
  values above tolerance while retaining the pure row-echelon fallback.
- `TENSOR-090AH`: `matrix/norm` returns `Float64` norms for rank-2 `Float64`
  Tensor values, defaulting to Frobenius and accepting the initial
  `'frobenius`, `'one`, `'infinity`, or `'max` selectors.
- `TENSOR-090AI`: `matrix/norm` also accepts `'spectral` for the largest
  singular value and `'nuclear` for the sum of singular values. CPU inputs use
  the existing singular-value path, while eligible Vulkan inputs route through
  the Vulkan singular-value helper and read back only the scalar norm result.
- `TENSOR-090AJ`: `matrix/norm` spectral and nuclear selectors now have
  explicit backend and forced-fallback coverage through a private test-only
  `dgesvd` disable hook.
- `TENSOR-090AK`: `matrix/rank`, `matrix/singular-values`, and `matrix/svd`
  now have forced pure fallback coverage through the same private `dgesvd`
  disable hook.
- `TENSOR-090AA`: `matrix/singular-values` returns descending singular values
  for rank-2 `Float64`, `Float32`, `Complex128`, and `Complex64` Tensor
  values as rank-1 component-width real tensors. CPU real inputs use the
  existing runtime LAPACK or pure fallback path; CPU complex inputs use
  realification and collapse duplicate real singular-value pairs. Eligible
  Vulkan inputs route through checked-in Gram/Jacobi helpers with CPU semantics
  as the oracle and no hidden CPU/LAPACK fallback.
- `TENSOR-090AB`: `matrix/eigenpairs` returns general nonsymmetric eigenpairs
  for square rank-2 `Float64` Tensor values as `BigComplex` `values` and
  aligned `BigComplex` `vectors` tensors. It accepts lazy inputs and empty
  square matrices.
- `TENSOR-090AC`: `matrix/eigenpairs` now has a pure fallback for hosts without
  runtime `LAPACKE_dgeev`, preserving the same `BigComplex` values/vectors
  contract.
- `TENSOR-090AD`: forced no-`dgeev` fallback coverage now includes 3x3
  diagonal, upper-triangular, and real-plus-complex-block matrices, plus
  residual checks that representative vector columns satisfy
  `A*v ~= lambda*v`.
- `TENSOR-090AE`: `LAPACKE_dgeev` accelerated `matrix/eigenpairs` coverage now
  includes representative real and complex residual checks for returned vector
  columns when the backend is available.
- `TENSOR-090AF`: both accelerated and forced-fallback `matrix/eigenpairs`
  paths now include a non-normal upper-triangular residual check for a
  non-basis returned vector column.
- `TENSOR-090AG`: the advanced collections/module suite now has reusable 3x3
  `matrix/eigenpairs` residual helpers and all-column backend/fallback harness
  checks for non-normal and real-plus-complex-block matrices.
- `TENSOR-090AQ`: the pure no-`dgeev` `matrix/eigenpairs` fallback now treats
  isolated 2x2 real-Schur blocks as converged, fixing a bounded-container-only
  corruption of the trailing real eigenvalue in a 3x3
  real-plus-complex-block matrix.
- `TENSOR-090S`: `matrix/inverse` is the first solve-derived matrix
  transform surface. It returns a same-shape `Float64` Tensor inverse for
  nonsingular square rank-2 `Float64` tensors and raises
  `tensor/singular-matrix` for singular inputs.
- `TENSOR-090J`: `matrix/qr` is the first non-LU decomposition surface. It
  returns reduced `q` and `r` factors for full-column-rank rank-2 `Float64`
  tensors with rows greater than or equal to columns.
- `TENSOR-090K`: `matrix/cholesky` is the first symmetric positive-definite
  decomposition surface. It returns a lower-triangular `Float64` Tensor factor
  for square symmetric positive-definite rank-2 `Float64` tensors and raises
  `tensor/not-positive-definite` for nonsymmetric or non-positive-definite
  inputs.
- `TENSOR-090L`: `matrix/svd` is the first rectangular decomposition surface.
  It returns reduced `u`, `s`, and `v` factors for rank-2 `Float64` tensors,
  where `u` has shape `[rows k]`, `s` has shape `[k]`, `v` has shape
  `[columns k]`, and `k = min(rows, columns)`.
- `TENSOR-090M`: `matrix/eigenvalues` and `matrix/eigenvectors` are the first
  eigen surfaces. They use a symmetric-real contract for square rank-2
  `Float64` tensors; nonsymmetric inputs raise `tensor/not-symmetric`.
  `matrix/eigenvectors` returns aligned `values` and `vectors` columns.
- `TENSOR-090N`: `matrix/lu` and `matrix/determinant` now use optional
  runtime-loaded `LAPACKE_dgetrf` acceleration when available. Missing
  LAPACKE, missing symbols, unsupported LAPACK ABI widths, and unavailable
  backend paths retain the pure runtime LU fallback and existing public
  contracts.
- `TENSOR-090AN`: `matrix/lu` and `matrix/determinant` now have forced pure
  fallback coverage through a private test-only `dgetrf` disable hook.
- `TENSOR-090O`: `matrix/cholesky` now uses optional runtime-loaded
  `LAPACKE_dpotrf` acceleration when available. The runtime keeps the existing
  exact symmetry precheck, lower-triangular output shape, upper-zeroing, and
  `tensor/not-positive-definite` failure contract.
- `TENSOR-090AO`: `matrix/cholesky` now has forced pure fallback coverage
  through a private test-only `dpotrf` disable hook.
- `TENSOR-090P`: `matrix/qr` now uses optional runtime-loaded
  `LAPACKE_dgeqrf` plus `LAPACKE_dorgqr` acceleration when available. The
  backend forms reduced `q`/`r`, sign-normalizes factors to preserve existing
  orientation, and keeps rank-deficient inputs mapped to
  `tensor/singular-matrix`.
- `TENSOR-090AP`: `matrix/qr` now has forced pure fallback coverage through a
  private test-only QR backend disable hook; the existing tolerance-based
  LAPACK QR rank guard remains in place.
- `TENSOR-090Q`: `matrix/eigenvalues` and `matrix/eigenvectors` now use
  optional runtime-loaded `LAPACKE_dsyev` acceleration when available. The
  runtime keeps the exact symmetry precheck, descending eigenvalue order,
  aligned vector columns, and the current symmetric-real-only contract.
- `TENSOR-090AL`: `matrix/eigenvalues` and `matrix/eigenvectors` now have
  forced pure fallback coverage through a private test-only `dsyev` disable
  hook.
- `TENSOR-090R`: `matrix/svd` now uses optional runtime-loaded
  `LAPACKE_dgesvd` acceleration when available. The runtime keeps the reduced
  `u`/`s`/`v` factor shapes, descending singular values, rank-deficient input
  support, and the pure Gram/Jacobi SVD fallback as the semantic oracle.
- `TENSOR-100`: explicit-device CUDA/cuBLAS design is closed. Future GPU
  support stays behind `Tensor`, uses `to-device`, `device`, and
  `tensor-backends`, rejects `GpuTensor`, `CudaTensor`, backend-flavored math
  names, and implicit CPU/GPU transfer, and requires deterministic ownership
  for CUDA buffers and cuBLAS handles.
- `TENSOR-100A`: CPU-only placement/introspection surface is implemented.
  `device` reports `'cpu` for current Tensor values; `to-device` with target
  `'cpu` realizes Tensor expressions to CPU Tensor storage; `to-device` with
  target `'cuda` fails closed with `tensor/backend-unavailable`.
- `TENSOR-100B`: placement metadata/backend inventory is implemented.
  `TensorVal` stores placement and an opaque device handle/finalizer slot,
  `tensor-backends` reports structured CPU/CUDA availability, CPU kernels
  reject non-CPU storage, and opaque device-handle destruction is covered by
  memory-lifetime tests.
- `TENSOR-100B2`: `tensor-layout` is implemented as the public Tensor layout
  metadata query. It returns a `Dictionary` with dtype, device, payload kind,
  logical shape, physical strides, rank, element count, logical byte length,
  storage offset, concrete backing storage element/byte extent, view flags,
  owner, and write policy. Lazy expression payloads report `0` storage extent
  until realization. Current payload values are `concrete`, `map`, `contract`,
  and `view`;
  current layout values are `dense-row-major` and `strided`; current owner
  values are `self`, `view-source`, and `expression`; current write policies
  are `mutable`, `immutable`, `mutable-view`, and `read-only-view`. The first
  public view constructor is `matrix/transpose-view`; CPU/CUDA/Vulkan kernels
  still accept only the layouts they explicitly support. Dense GPU kernels and
  raw copy helpers continue to require zero-offset dense row-major storage.
- `TENSOR-100F-complex-numerical-subset`: Vulkan fixed-width complex
  `matrix/lu`, `matrix/determinant`, `matrix/solve`, `matrix/inverse`,
  `matrix/rank`, direct `matrix/norm` reducers, `matrix/qr`, and
  `matrix/cholesky` are landed for dense row-major zero-offset
  `Complex128`/`Complex64` tensors behind
  `matrix-numerical-complex128` and `matrix-numerical-complex64`. Validation
  covers capability reporting, shader/helper status mapping, CPU oracle parity,
  no-hidden-CPU-fallback behavior, singular/tolerance diagnostics where
  applicable, placement/copyback checks, and host/container validation.
- `TENSOR-100C`: optional CUDA copy support is implemented without a required
  CUDA link dependency. `to-device 'cuda` copies concrete `Float64` or
  `Float32` CPU Tensor storage to CUDA when runtime-loaded libcudart is usable;
  `to-device 'cpu` copies CUDA storage back to native CPU Tensor storage. CUDA
  availability is gated by symbol resolution, device count, and allocation/free
  probing.
- `TENSOR-100D`: optional cuBLAS rank-2 contract support is implemented.
  CUDA-placed dense row-major matching `Float64` or matching `Float32`
  rank-2/rank-2 single-axis contractions route to cuBLAS GEMM for `[1 0]`,
  `[0 0]`, `[1 1]`, and `[0 1]`; rank-2/rank-1, rank-1/rank-2, and
  rank-1/rank-1 dot single-axis contractions route to cuBLAS GEMV. `Float32`
  capability is probed through separately resolved `cublasSgemm_v2` /
  `cublasSgemv_v2` symbols without weakening existing `Float64` cuBLAS
  availability. Results are CUDA-placed Tensors; callers copy back explicitly
  with `to-device 'cpu`. Focused coverage includes available execution for all
  GEMM/GEMV layouts, unsupported CUDA contract diagnostics, zero-size CUDA
  identity/fill behavior, and forced cuBLAS unavailable reporting for nonzero
  contractions.
- `TENSOR-100F`: CUDA elementwise `map` support is implemented for dense
  row-major `Float64` and `Float32` tensors through CUDA Driver API kernels.
  Binary operations cover `+`, `-`, `*`, `/`, `min`, and `max` over
  tensor/scalar, scalar/tensor, exact-shape tensor/tensor, and right-aligned
  singleton-axis tensor/tensor broadcasting. Arithmetic/component unary
  operations cover unary `+`, `abs`, unary `-`, `sqrt`, `real-part`,
  `imag-part`, and `conjugate`; scientific unary operations cover `sin`, `cos`,
  `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, `log10`,
  `math.erf`, `math.erfc`, `stats.normal-cdf`, and
  `stats.normal-quantile` when the generated CUDA C/libdevice PTX module
  loads. CUDA quantile propagates probability-domain status before returning an
  output tensor. Direct public
  `map` on CUDA tensors, lazy map realization to CUDA, CUDA destination
  realization from lazy CUDA maps, and direct Tensor arithmetic/component and
  scientific unary primitives preserve CUDA placement. `tensor-backends`
  reports `elementwise-map-float64`, `elementwise-map-float32`,
  `scientific-map-float64`, `scientific-map-float32`,
  `math-elementary-float64`, `math-elementary-float32`,
  `math-error-function-float64`, `math-error-function-float32`,
  `stats-distribution-float64`, `stats-distribution-float32`, and
  CUDA `rounding-big-integer` capability keys. There
  is no hidden CPU fallback; unsupported callables, mixed CPU/CUDA operands,
  mixed dtype/device operands, and unsupported layouts remain explicit
  backend-unsupported/fail-closed cases. Mixed CPU/CUDA `map` operands fail with
  an explicit CUDA-placement diagnostic, and mixed CUDA Tensor dtypes fail with
  `tensor/dtype-mismatch`; unsupported mixed CPU/CUDA lazy operands are rejected
  before CPU lazy materialization can run.
- CUDA destination-form `realize` supports existing dense row-major `Float64`
  or `Float32` CUDA destinations for matching CPU sources, CUDA sources,
  supported lazy CUDA map or contract results, and scalar fills. CPU destinations
  still require explicit `to-device 'cpu` before accepting CUDA sources, and
  unsupported CUDA lazy expressions fail closed with `tensor/backend-unsupported`.
  CUDA concrete Tensor clone/copy supports Omni-owned CUDA payloads and valid
  foreign CUDA concrete payloads. Foreign CUDA clones allocate fresh Omni-owned
  CUDA storage, copy device-to-device, and install `tensor_cuda_device_finalizer`
  on the clone; fake or invalid CUDA handles still fail closed.
- `TENSOR-100E` plus subsequent `TENSOR-100F` Vulkan extensions: portable
  Vulkan placement, SPIR-V map kernels, broadcasting, a separate unary `abs` /
  unary `-` / unary `sqrt` / real-component helper,
  generic dense row-major rank-N `Float64`
  multi-axis contract, Vulkan `matrix/transpose`, Vulkan `matrix/diagonal` /
  `matrix/diagonal-matrix`, Vulkan `matrix/trace`, Vulkan `matrix/rank`,
  Vulkan `matrix/lu`, Vulkan `matrix/solve`, Vulkan `matrix/determinant`,
  Vulkan `matrix/inverse`, Vulkan `matrix/cholesky`, Vulkan `matrix/qr`, and
  Vulkan `matrix/norm` are implemented.
  Vulkan is a runtime-optional
  explicit GPU backend behind
  `Tensor`; `tensor-backends` reports a structured `vulkan` entry with explicit
  `Float64` kernel capability. `to-device 'vulkan` copies concrete `Float64`
  CPU Tensor storage into opaque Vulkan storage when runtime-loaded Vulkan
  support is usable, and `to-device 'cpu` copies Vulkan storage back to native
  CPU storage. Destination-form `realize` supports matching dense row-major
  `Float64` CPU sources, Vulkan sources, lazy Vulkan results, and scalar fills
  into existing Vulkan destinations; CPU destinations still require explicit
  `to-device 'cpu` before accepting device sources. `map` supports dense
  row-major Vulkan `Float64` elementwise
  arithmetic `+`, `-`, `*`, and `/` for Tensor/scalar, scalar/Tensor,
  exact-shape Tensor/Tensor inputs, and right-aligned singleton-axis
  Tensor/Tensor broadcasting through embedded SPIR-V kernels, returning
  Vulkan-placed tensors. The binary map helper also supports `min` and `max`
  for dense row-major Vulkan `Float64` tensor/scalar, scalar/Tensor, and
  Tensor/Tensor broadcast cases, and direct public Tensor `min` / `max` route
  through it for eligible Vulkan operands. `map +` routes through the unary
  identity opcode for eligible Vulkan operands, and direct Tensor unary `+`
  preserves the Tensor's current placement. `map abs`, direct Tensor `abs`, `map -`, direct
  Tensor unary `-`, `map sqrt`, direct Tensor `sqrt`, `map real-part`,
  `map imag-part`, `map conjugate`, and direct Tensor `real-part` /
  `imag-part` / `conjugate` support Vulkan-placed dense row-major `Float64`
  tensors through a separate two-buffer unary helper; unsupported unary
  callables still fail closed instead of using the invalidated generic unary
  map mode. CPU direct Tensor unary `-` preserves
  native Tensor dtype for `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex`; scalar unary `-` is reachable through the documented
  one-argument public surface. Direct Tensor unary math on Vulkan fails closed
  for unsupported operations instead of materializing hidden CPU results.
  `contract` supports Vulkan-placed
  dense row-major rank-N `Float64` tensors with zero or more contracted axis
  pairs, including zero-axis outer products and rank-0 scalar products; output
  axes are ordered as free left axes followed by free right axes. Results
  remain Vulkan-placed tensors. Missing Vulkan fails with
  `tensor/backend-unavailable`;
  the generic contract shader is generated from checked-in GLSL and uses
  rank/shape/stride/axis-list metadata-buffer dispatch.
  `matrix/transpose` supports Vulkan-placed dense row-major `Float64`,
  `Float32`, `Complex128`, or `Complex64` rank-2 concrete tensors and returns a
  Vulkan-placed materialized transposed Tensor that preserves input dtype.
  `matrix/transpose-view` remains CPU-readable and now also supports explicit
  Vulkan materialization for direct rank-2 transpose views over dense
  zero-offset Vulkan storage. This is not general stride-aware Vulkan kernel
  execution; broader view-backed kernels still fail closed.
  `matrix/diagonal` and `matrix/diagonal-matrix` support Vulkan-placed dense
  row-major `Float64` or `Float32` inputs and return Vulkan-placed Tensor
  results that preserve input dtype.
  `matrix/trace` supports Vulkan-placed dense row-major square `Float64` or
  `Float32` inputs, computes on Vulkan, and reads back only the scalar result
  required by the scalar return contract.
  `matrix/rank` supports Vulkan-placed dense row-major `Float64`, `Float32`,
  `Complex128`, or `Complex64` rank-2 inputs, computes on Vulkan, and reads
  back only the scalar `Integer` result required by the scalar return
  contract. Complex rank uses magnitude-based pivoting and tolerance.
  `matrix/lu` supports Vulkan-placed dense row-major square `Float64` or
  `Float32` inputs, computes partial-pivot LU on Vulkan, returns the combined
  `lu` factor as a Vulkan-placed Tensor preserving input dtype, and reads back
  only `pivots` plus `swap-count` dictionary metadata required by the public
  contract.
  `matrix/solve` supports Vulkan-placed dense row-major square `Float64` or
  `Float32` coefficient tensors with matching Vulkan-placed rank-1 or rank-2
  right-hand tensors, computes Gaussian elimination on Vulkan, returns a
  Vulkan-placed solution Tensor preserving RHS dtype, and raises
  `tensor/singular-matrix` for singular systems. `Float64` and `Float32`
  systems with `n >= 65` route through staged workgroup-parallel Vulkan solve
  helpers; the `Float32` threshold is parity-backed on this stack rather than
  a decisive speedup claim.
  `matrix/determinant` supports Vulkan-placed dense row-major square
  `Float64`, `Float32`, `Complex128`, or `Complex64` inputs, computes on
  Vulkan, and reads back only the scalar result required by the scalar return
  contract.
  `matrix/inverse` supports CPU and Vulkan-placed dense row-major square
  `Float64`, `Float32`, `Complex128`, or `Complex64` inputs, computes
  Gauss-Jordan elimination, returns a Vulkan-placed inverse Tensor preserving
  input dtype for Vulkan inputs, and raises `tensor/singular-matrix` for
  singular inputs.
  `matrix/cholesky` supports Vulkan-placed dense row-major square `Float64` or
  `Float32` inputs, computes the lower-triangular factor on Vulkan, returns a
  Vulkan-placed factor Tensor preserving input dtype, and raises
  `tensor/not-positive-definite` for nonsymmetric or non-SPD inputs.
  `matrix/qr` supports Vulkan-placed dense row-major rank-2 `Float64` or
  `Float32` inputs with rows greater than or equal to columns, computes
  reduced QR on Vulkan, returns Vulkan-placed `q` and `r` factor Tensors
  preserving input dtype, and raises `tensor/singular-matrix` for
  rank-deficient inputs.
  `matrix/norm` supports Vulkan-placed dense row-major `Float64`, `Float32`,
  `Complex128`, and `Complex64` inputs for default/`'frobenius`, `'one`,
  `'infinity`, `'max`, `'spectral`, and `'nuclear`. Complex direct selectors
  use complex magnitudes; complex spectral/nuclear selectors use native Vulkan
  complex singular-value helpers. Supported paths compute on Vulkan and read
  back only the scalar result required by the scalar return contract.
  `matrix/singular-values` supports Vulkan-placed dense row-major rank-2
  `Float64`, `Float32`, `Complex128`, and `Complex64` inputs, computes on
  Vulkan with storage-buffer Gram scratch, returns a Vulkan-placed rank-1
  component-width real Tensor, supports `k = min(rows, columns) > 64`, and
  requires explicit `to-device 'cpu` for CPU inspection. Shader
  non-convergence raises
  `tensor/no-convergence`. Unsupported dtypes, unsupported Vulkan map callables,
  incompatible map broadcast shapes, mixed CPU/Vulkan operands, and unsupported
  Vulkan layouts fail closed with `tensor/backend-unsupported`. Vulkan `map`
  and map-backed `min` / `max` preflight checks run before concrete lazy
  realization, so fail-closed mixed-device cases do not materialize CPU
  fallbacks first.
  Direct `matrix/svd` supports Vulkan-placed dense row-major rank-2 `Float64`
  and `Float32` inputs, computes reduced SVD factors on Vulkan with
  storage-buffer Gram scratch, returns Vulkan-placed `u`, `s`, and `v` tensors
  with the input float dtype and shapes `[rows k]`, `[k]`, and `[columns k]`,
  and requires explicit `to-device 'cpu` for CPU inspection. Shader
  non-convergence raises `tensor/no-convergence`.
  Direct `matrix/eigenvalues` and `matrix/eigenvectors` now support
  Vulkan-placed dense row-major square symmetric `Float64` inputs, including
  `n > 64` through storage-buffer scratch within helper resource limits,
  32-bit shader index guards, and the Jacobi iteration guard. They compute
  values and aligned vector columns on Vulkan, return Vulkan-placed Tensor
  outputs, and require explicit `to-device 'cpu` for CPU inspection.
  Nonsymmetric Vulkan inputs raise `tensor/not-symmetric`; resource-bound,
  unsupported shape/layout/dtype, and missing Vulkan/Float64 capability cases
  fail closed with Tensor backend diagnostics; shader non-convergence raises
  `tensor/no-convergence`. The Vulkan path has no hidden CPU/LAPACK fallback.
  Direct `matrix/eigenpairs` remains CPU-only and
  fail-closed on Vulkan inputs while its public output contract is
  pointer-backed `BigComplex`.
  The current SVD design record is
  `docs/plans/vulkan-svd-factor-output-plan-2026-04-17.md`; the current eigen
  plan is `docs/plans/vulkan-eigensolver-plan-2026-04-17.md`. Vulkan
  `Float32` execution now supports placement/copyback, destination `realize`,
  dense row-major elementwise `map`, unary helpers, direct `min`/`max`, rank-N
  dense row-major `contract`, structural matrix kernels (`transpose`,
  `diagonal`, `diagonal-matrix`, `trace`), direct `matrix/rank`, direct
  `matrix/norm` selectors, direct `matrix/singular-values`, direct
  `matrix/svd`, and serial factor/solve surfaces (`matrix/determinant`,
  `matrix/lu`, `matrix/solve`, `matrix/inverse`, `matrix/cholesky`, and
  `matrix/qr`) under explicit `tensor-backends` capability reporting, plus
  large-dense SVD robustness for dense row-major inputs;
  CPU/runtime `Tensor Float32` storage exists and follows
  `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`.
  CPU `Float32` factor/SVD oracles now cover `matrix/determinant`,
  `matrix/lu`, `matrix/solve`, `matrix/inverse`, `matrix/cholesky`,
  `matrix/qr`, `matrix/singular-values`, `matrix/svd`, and SVD-backed
  `matrix/norm` selectors without hidden Float64 LAPACK routing. Scalar
  `Float32` runtime values and CUDA `Float32` placement/copyback plus eligible
  cuBLAS contract routing are landed, including rank-1/rank-1 dot; supported
  CUDA zero-size contractions now preserve zero-length outputs or fill additive
  identity on CUDA. CUDA elementwise `map` is landed for dense row-major
  `Float64`/`Float32` binary scalar/exact-shape/right-aligned broadcast
  operands, arithmetic/component unary ops, and generated-PTX scientific unary
  ops. Valid foreign CUDA concrete payload clone now deep-copies into
  Omni-owned CUDA storage. CPU fixed-width complex scalar/Tensor storage has
  landed for `Complex128`/`Complex64`; CUDA/Vulkan raw complex storage, dense
  row-major complex elementwise `map`, dense row-major complex `contract`, and
  CUDA/Vulkan dense row-major complex structural matrix kernels are
  capability-gated and landed. Vulkan complex `matrix/lu`,
  `matrix/determinant`, `matrix/solve`, `matrix/inverse`, `matrix/rank`,
  `matrix/norm`, `matrix/singular-values`, `matrix/qr`, and
  `matrix/cholesky` are also capability-gated and landed. Remaining
  unvalidated CUDA/Vulkan complex numerical matrix families, including complex
  SVD factor output, CUDA complex singular-values/norm selectors, and complex
  eigenvector result contracts, remain
  fail-closed/deferred until their explicit helper ABIs, capability bits, and
  validation plans land. The landed `tensor-layout` primitive only exposes
  metadata and does not create views or relax dense row-major backend
  preconditions.
