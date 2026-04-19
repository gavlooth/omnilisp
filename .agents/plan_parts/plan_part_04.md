# Agent Plan Index Part 04

Source: `.agents/PLAN.md`

## Scalar Scientific Backend Direction

Default scalar numerics direction from the owner remains:

- Avoid GSL as the baseline scientific stack.
- Prefer Boost.Multiprecision for exact/high-precision scalar values.
- Prefer Boost.Math for scalar scientific functions behind an owned C++ shim
  with a stable C ABI.
- Keep GNU precision libraries only as possible later performance backends if
  the owner explicitly accepts that dependency path.
- Keep user-facing scalar names backend-neutral: `Integer`, `BigInteger`,
  `Float64`, `BigFloat`, optional `Complex`, and `BigComplex`.

Minimal scalar-first targets:

- `BigInteger` through Boost.Multiprecision `cpp_int` is now the first landed
  scalar slice: constructor, printing/String conversion, `Number` identity,
  `+`/`-`/`*` overflow promotion, `/`, `%`, ordering comparisons,
  `abs`, `min`, `max`, `gcd`, `lcm`, bitwise operations, equality/hash
  support, `parse-number` decimal overflow promotion, and scope-boundary
  cloning are implemented.
- `BigFloat` through Boost.Multiprecision `cpp_dec_float_50` is now the first
  high-precision decimal slice: constructor, printing/String conversion,
  `Number` identity, `Float64`/`Integer` narrowing, `+`, `-`, `*`, `/`,
  comparisons, `abs`, `min`, `max`, equality/hash support, scope-boundary
  cloning, `parse-number` floating overflow promotion, BigFloat-preserving
  scalar math for trig, inverse trig, hyperbolic, exponential/logarithmic,
  power/root, gamma/error-function, standard-normal helpers, and exact
  BigFloat `floor`/`ceiling`/`round`/`truncate` to `Integer` or `BigInteger`
  are implemented. Precision-control APIs remain a separate follow-up.
- `BigComplex` is now the first high-precision complex scalar slice:
  constructor, printing/String conversion, `Number` identity, `+`, `-`, `*`,
  `/`, unary `-`, equality/hash support, `zero?`, `abs` to `BigFloat`,
  scope-boundary cloning/promotion, fail-closed ordered operations, and
  BigComplex-preserving `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`,
  `cosh`, `tanh`, `exp`, `log`, `log10`, `sqrt`, and `pow` are implemented.
  `real-part`, `imag-part`, and `conjugate` are implemented as numeric
  primitives. Broader special functions and distributions remain separate
  follow-ups.
- `math/lgamma`, `math/erf`, `math/erfc`, `stats/normal-cdf`, and
  `stats/normal-quantile` are now validated Boost.Math wrapper slices: C++ shim
  status codes, finite-input/range/domain error mapping, primitive
  registration, AOT lookup, focused numeric tests, and direct runtime smokes
  are implemented.
- `math/erf`, `math/erfc`, and `stats/normal-cdf` are no longer scalar-only
  for Tensor inputs: CPU Tensor elementwise support and CUDA dense row-major
  `Float64`/`Float32` opcode support are implemented. Rounding primitives
  remain dtype-changing `BigInteger` Tensor operations, so they are not CUDA
  same-dtype map opcodes.
- `stats/normal-cdf` and `stats/normal-quantile` are intentionally
  one-argument standard-normal helpers. Mean/stddev parameters or other
  distribution families need separate surface decisions.

## Tensor Backend Direction

OpenBLAS/LAPACK/LAPACKE should integrate as optional backend acceleration, not
as a new public Tensor surface:

- The pure C3 Tensor fallback remains the semantic oracle.
- Ordinary Tensor storage remains C3-native and scoped; do not replace normal
  Tensor storage with `ForeignHandle`.
- BLAS rank-2 `Float64` GEMM now optimizes all contiguous row-major single-axis
  rank-2 contract layouts behind `realize` when dtype, rank, layout, device,
  and aliasing match the backend contract: `[1 0]`, `[0 0]`, `[1 1]`, and
  `[0 1]`.
- BLAS `Float64` GEMV now optimizes contiguous row-major rank-2/rank-1 and
  rank-1/rank-2 single-axis contract layouts behind the same backend boundary.
- BLAS `Float64` DDOT now optimizes contiguous rank-1/rank-1 vector dot
  contractions behind the same backend boundary.
- BLAS `Float64` DGER now optimizes contiguous rank-1/rank-1 outer-product
  contractions behind the same backend boundary.
- `matrix/solve` is the first public rank-2 Tensor solver surface. It uses a
  pure Float64 runtime solver for square coefficient tensors and rank-1/rank-2
  RHS tensors; singular systems raise `tensor/singular-matrix`.
- `matrix/solve` now has optional runtime-loaded `LAPACKE_dgesv` acceleration
  when `liblapacke` is available; missing backend support retains the pure
  solver fallback.
- `matrix/solve` and `matrix/inverse` now have forced-fallback coverage
  through a private test-only `dgesv` disable hook. The `dgesv` backend path
  has its own disable state and no longer depends on the unrelated `dgeev`
  test hook.
- `matrix/lu` is the first public rank-2 Tensor decomposition surface. It uses
  pure partial-pivot LU factorization for square `Float64` tensors, returns
  combined factor, final row-order pivots, and swap-count metadata, and raises
  `tensor/singular-matrix` for singular inputs.
- `matrix/determinant` is the first decomposition consumer. It reuses the pure
  LU kernel, returns a `Float64` scalar, returns `0.0` for singular matrices,
  and preserves the empty square determinant convention `1.0`.
- `matrix/transpose` is the first dtype-preserving structural matrix transform.
  It transposes rank-2 Tensor values, returns shape `[columns rows]`, and
  preserves native dtype across `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex`.
- `matrix/diagonal` is the second dtype-preserving structural matrix transform.
  It extracts the main diagonal from rank-2 Tensor values into a rank-1 Tensor
  of length `min(rows, columns)`, preserving native dtype across `Float64`,
  `BigInteger`, `BigFloat`, and `BigComplex`.
- `matrix/diagonal-matrix` is the first dtype-preserving structural matrix
  constructor. It builds a square rank-2 Tensor from rank-1 Tensor values,
  clones high-precision diagonal handles, and fills off-diagonal cells with
  the dtype's zero value.
- `matrix/identity` is the first size-driven structural matrix constructor. It
  builds square identity Tensors from non-negative integer sizes, defaults to
  `Float64`, and supports optional `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex` dtype selection.
- `matrix/trace` is the first dtype-preserving structural matrix reduction. It
  sums the diagonal of square rank-2 Tensor values and returns a scalar in the
  Tensor's native numeric family: `Double` for `Float64`, or `BigInteger`,
  `BigFloat`, and `BigComplex` for matching high-precision Tensor dtypes.
- `matrix/rank` is the first numerical structural matrix reduction. It
  computes rectangular rank-2 `Float64` Tensor numerical rank as an `Integer`,
  realizes lazy inputs, supports empty axes, and accepts an optional
  non-negative finite tolerance.
- `matrix/rank` now uses optional runtime-loaded `LAPACKE_dgesvd`
  acceleration when available, counting singular values above tolerance while
  retaining the pure row-echelon fallback.
- `matrix/norm` is the first general matrix norm reducer. It returns
  `Float64` norms for rank-2 `Float64` Tensor values, realizes lazy inputs,
  supports empty axes, defaults to Frobenius, and accepts explicit
  `'frobenius`, `'one`, `'infinity`, `'max`, `'spectral`, and `'nuclear`
  selectors. The spectral and nuclear selectors reuse the existing SVD
  pure/LAPACK machinery.
- `matrix/norm` spectral and nuclear selectors now have explicit backend and
  forced-fallback coverage through a private test-only `dgesvd` disable hook,
  preserving the same public results whether runtime LAPACK is available or
  the pure SVD fallback is used.
- `matrix/rank`, `matrix/singular-values`, and `matrix/svd` now also have
  forced-fallback coverage through the private `dgesvd` disable hook, so the
  SVD-backed matrix lane validates both accelerated and pure fallback
  semantics for shipped public contracts.
- `matrix/inverse` is the first solve-derived matrix transform surface. It
  returns a same-shape `Float64` Tensor inverse for nonsingular square rank-2
  `Float64` tensors, raises `tensor/singular-matrix` for singular inputs, and
  uses optional runtime-loaded `LAPACKE_dgesv` acceleration when available.
- `matrix/lu` and `matrix/determinant` now have optional runtime-loaded
  `LAPACKE_dgetrf` acceleration when `liblapacke` is available; missing backend
  support retains the pure LU fallback and existing public contracts.
- `matrix/lu` and `matrix/determinant` now also have forced-fallback coverage
  through a private test-only `dgetrf` disable hook, preserving public results
  through the pure partial-pivot LU fallback when runtime `dgetrf` is disabled.
- `matrix/qr` is the first non-LU decomposition surface. It computes reduced
  QR for full-column-rank rank-2 `Float64` tensors with rows greater than or
  equal to columns and returns `q`/`r` factors.
- `matrix/qr` now has optional runtime-loaded `LAPACKE_dgeqrf` plus
  `LAPACKE_dorgqr` acceleration when `liblapacke` is available; missing
  backend support retains the pure QR fallback and existing reduced-factor
  orientation.
- `matrix/qr` now also has forced-fallback coverage through a private test-only
  QR backend disable hook, preserving public results through the pure reduced
  QR fallback when runtime QR LAPACK is disabled. The existing tolerance-based
  LAPACK QR rank guard remains in place.
- `matrix/cholesky` is the first symmetric positive-definite decomposition
  surface. It computes a lower-triangular `Float64` factor for square
  symmetric positive-definite rank-2 `Float64` tensors and raises
  `tensor/not-positive-definite` for nonsymmetric or non-positive-definite
  inputs.
- `matrix/cholesky` now has optional runtime-loaded `LAPACKE_dpotrf`
  acceleration when `liblapacke` is available; missing backend support retains
  the pure Cholesky fallback and the exact symmetry precheck.
- `matrix/cholesky` now also has forced-fallback coverage through a private
  test-only `dpotrf` disable hook, preserving public results through the pure
  lower-triangular Cholesky fallback when runtime `dpotrf` is disabled.
- `matrix/svd` is the first rectangular decomposition surface. It computes a
  reduced SVD for rank-2 `Float64` tensors, supports rank-deficient inputs, and
  returns `u` shape `[rows k]`, `s` shape `[k]`, and `v` shape `[columns k]`,
  where `k = min(rows, columns)`.
- `matrix/svd` now has optional runtime-loaded `LAPACKE_dgesvd` acceleration
  when `liblapacke` is available; missing backend support retains the pure
  Gram/Jacobi SVD fallback and existing reduced-factor contract.
- `matrix/singular-values` exposes the `s` part of the reduced SVD contract
  directly as a rank-1 `Float64` Tensor and uses the same optional
  `LAPACKE_dgesvd` acceleration and pure fallback as `matrix/svd`.
- `matrix/eigenvalues` and `matrix/eigenvectors` are the first eigen surfaces.
  They intentionally use a symmetric-real contract for square rank-2
  `Float64` tensors. Nonsymmetric matrices raise `tensor/not-symmetric`.
- `matrix/eigenvalues` and `matrix/eigenvectors` now have optional
  runtime-loaded `LAPACKE_dsyev` acceleration when `liblapacke` is available;
  missing backend support retains the pure symmetric Jacobi fallback and the
  exact symmetry precheck.
- `matrix/eigenvalues` and `matrix/eigenvectors` now have forced-fallback
  coverage through a private test-only `dsyev` disable hook, preserving the
  same symmetric-real public contract when runtime LAPACK is disabled.
- `matrix/eigenpairs` is the chosen general nonsymmetric eigen surface. It
  returns aligned `BigComplex` `values` and `vectors` tensors for square
  rank-2 `Float64` input, realizes lazy input, supports empty square matrices,
  and uses runtime `LAPACKE_dgeev` when available with a pure fallback
  otherwise. The pure no-`dgeev` fallback is covered on 2x2 real/complex
  cases and targeted 3x3 diagonal, upper-triangular, and
  real-plus-complex-block cases, including representative residual checks for
  `A*v ~= lambda*v`. The accelerated `LAPACKE_dgeev` path is also covered by
  representative real and complex residual checks when the backend is
  available. Both backend and forced-fallback paths include a non-normal
  upper-triangular residual check for a non-basis vector column. The focused
  advanced collections/module suite also has reusable 3x3 residual helpers
  that validate all returned columns for representative non-normal and
  real-plus-complex-block matrices under backend and forced-fallback execution.
  The pure fallback now treats isolated 2x2 real-Schur blocks as converged,
  fixing a bounded-container-only trailing-real-eigenvalue corruption in the
  3x3 real-plus-complex-block case.
- `(Array tensor)` and `(List tensor)` are implemented as explicit Tensor
  collection conversions that materialize lazy Tensor expressions and return flat
  row-major values.
- `(Tensor iterator)`, `(Tensor iterator dtype)`, and
  `(Tensor dtype shape iterator)` are now the priority constructor-dispatch
  materialization path for finite numeric iterators; this replaces treating
  `realize` or a Clojure-style `force` as the public lazy boundary.
- `(Iterator tensor)` now exposes flat row-major Tensor elements through the
  canonical iterator constructor surface, matching `(Array tensor)` and
  `(List tensor)`. Non-CPU device tensors still require explicit
  `to-device 'cpu` before iteration.
- `(Tensor data)`, `(Tensor data Float64)`, and `(Tensor Float64 data)` now infer
  native `Float64` tensor shape from real numeric scalars or rectangular nested
  arrays/proper lists whose leaves can narrow to finite `Float64`, while
  preserving the explicit `(Tensor Float64 shape data-or-scalar)` constructor.
- Native `BigInteger` Tensor storage now supports constructor, `dtype`, `ref`,
  flat `(Array tensor)` / `(List tensor)` conversion, concrete `realize`,
  tensor-dispatched `map`, and pure C3 `contract` kernels. BigInteger Tensor
  data must be exact integers.
- Native `BigComplex` Tensor storage now supports constructor, `dtype`, `ref`,
  flat `(Array tensor)` / `(List tensor)` conversion, concrete `realize`,
  tensor-dispatched `map`, and pure C3 `contract` kernels. Real numeric leaves
  promote to zero-imaginary BigComplex elements.
- BigComplex Tensor component kernels now support `real-part`, `imag-part`,
  and `conjugate`. Component extraction returns BigFloat tensors; conjugate
  returns BigComplex tensors.
- Real Tensor component semantics are dtype-preserving: `real-part` and
  `conjugate` copy Float64, BigInteger, and BigFloat tensors; `imag-part`
  returns same-shape zero tensors in the same dtype.
- Tensor `abs` now supports elementwise magnitude for all native Tensor dtypes:
  real Tensor dtypes preserve dtype and shape, while BigComplex tensors return
  same-shape BigFloat magnitude tensors.
- Tensor `sqrt` now supports elementwise square root for all native Tensor
  dtypes: Float64 and BigInteger tensors return same-shape Float64 tensors,
  while BigFloat and BigComplex tensors preserve dtype.
- Tensor unary scientific math now supports elementwise `sin`, `cos`, `tan`,
  `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, and `log10`
  for all native Tensor dtypes. Float64 and BigInteger tensors return
  same-shape Float64 tensors, while BigFloat and BigComplex tensors preserve
  dtype.
- Tensor `pow` now supports tensor-scalar, scalar-tensor, and broadcast
  tensor-tensor powers. BigComplex wins result dtype, then BigFloat, otherwise
  the result is a Float64 tensor.
- Tensor `atan2` now supports tensor-scalar, scalar-tensor, and broadcast
  tensor-tensor real-plane arctangent. BigFloat preserves dtype, other
  real/exact inputs return Float64 tensors, and complex operands fail closed.
- Tensor `floor`, `ceiling`, `round`, and `truncate` now return BigInteger
  Tensor results for real Tensor inputs. BigFloat tensors use exact rounding;
  BigComplex tensors fail closed.
- CUDA exposes direct dense row-major `Float64`/`Float32` Tensor rounding
  through `rounding-big-integer` and CPU `Tensor BigInteger` materialization.
  Vulkan rounding is not implied by Vulkan Float64/Float32 support; tests are
  staged to require a Vulkan `rounding-big-integer` capability before accepting
  BigInteger results and otherwise keep direct Vulkan rounding fail-closed.
- Native `BigFloat` concrete Tensor storage now supports constructor, `dtype`,
  `ref`, flat `(Array tensor)` / `(List tensor)` conversion, and concrete
  `realize`.
- Tensor-dispatched `map` now supports native `BigFloat` tensors for unary,
  tensor-scalar, scalar-tensor, exact-shape tensor-tensor, and right-aligned
  singleton-axis broadcast cases.
- Tensor-dispatched `contract` now supports native `BigFloat` tensors through
  the pure C3 contraction fallback. BLAS fast paths remain `Float64`-only.
- Tensor `min` and `max` now support native Tensor inputs with tensor-scalar,
  scalar-tensor, and broadcast tensor-tensor real comparison. BigFloat wins
  if either input is BigFloat, Float64 wins if either input is Float64, and the
  remaining exact inputs return native `BigInteger` Tensor results.
- Tensor `gcd` and `lcm` now support native Tensor inputs with tensor-scalar,
  scalar-tensor, and broadcast tensor-tensor exact integer semantics. Tensor
  operands must be native `BigInteger` Tensor storage; results are native
  `BigInteger` tensors. The working fast path borrows Tensor element handles
  and uses existing `i64`/borrowed BigInteger C ABI helpers for scalars; the
  manufactured scalar-handle variant was invalidated by corrupted
  tensor-scalar results.
- Unsupported strides, dtypes, aliasing, device placement, or missing libraries
  must fall back or fail deterministically without changing Tensor semantics.
- Direct native backend calls are preferred for hot Tensor kernels. User-facing
  FFI handles are not part of the normal Tensor object model.
- Backend modules should sit behind names such as `tensor/blas` and
  `tensor/lapack`; only genuinely opaque backend resources need explicit
  ownership/finalizer policy.
- `TENSOR-100` is closed as the explicit-device CUDA/cuBLAS design slice:
  future GPU support stays behind the existing `Tensor` value, uses
  `to-device`, `device`, and `tensor-backends`, rejects `GpuTensor`,
  `CudaTensor`, backend-flavored math names, and implicit CPU/GPU transfers,
  and requires deterministic ownership for CUDA buffers and cuBLAS handles.
- `TENSOR-100A` is implemented as the CPU-only placement/introspection slice:
  `device` reports `'cpu`; `to-device` with target `'cpu` realizes Tensor
  expressions to CPU Tensor storage; `to-device` with target `'cuda` fails
  closed with `tensor/backend-unavailable`.
- `TENSOR-100B` is implemented as the placement metadata/backend inventory
  slice: `TensorVal` stores placement and an opaque device handle/finalizer
  slot, `tensor-backends` reports structured CPU/CUDA availability, CPU kernels
  reject non-CPU storage, boundary clones refuse opaque non-CPU payloads until
  explicit copy semantics land, and `tensor_free_payload` owns opaque device
  handle release.
- `TENSOR-100C` is implemented as the optional CUDA copy slice:
  runtime-loaded `libcudart` support copies concrete `Float64` or `Float32`
  CPU Tensor storage to CUDA through `to-device 'cuda` and copies CUDA Tensor
  storage back through `to-device 'cpu`. CUDA availability requires a successful
  allocation/free probe, not just device-count success.
- `TENSOR-100D` is implemented as the cuBLAS rank-2 CUDA contract slice:
  runtime-loaded cuBLAS handle lifecycle is per operation, `tensor-backends`
  reports `cublas` with dtype capability bits, CUDA-placed dense row-major
  matching `Float64` or matching `Float32` rank-2/rank-2 single-axis
  contractions return CUDA-placed Tensors through dtype-specific GEMM for
  `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`, and rank-2/rank-1 plus rank-1/rank-2
  contractions route through dtype-specific GEMV. `Float32` cuBLAS capability
  is probed separately from `Float64` availability. Focused tests cover
  available paths, mixed-device and unsupported multi-axis diagnostics, forced
  cuBLAS-unavailable reporting, and fail-closed zero-size CUDA dimensions.
- `TENSOR-100E` is complete as the portable Vulkan backend baseline:
  `docs/plans/vulkan-backend-decision-2026-04-16.md` records the policy,
  `tensor-backends` reports a structured `vulkan` entry with explicit
  `float64` kernel capability, `to-device 'vulkan` copies concrete `Float64`
  CPU Tensor storage into opaque host-visible coherent Vulkan buffers when available,
  and `to-device 'cpu` copies Vulkan storage back to native CPU Tensor storage.
  Vulkan `map` now supports dense row-major `Float64` elementwise arithmetic
  `+`, `-`, `*`, and `/` for Tensor/scalar, scalar/Tensor, exact-shape
  Tensor/Tensor inputs, and right-aligned singleton-axis Tensor/Tensor
  broadcasting through public `map`, returning Vulkan-placed tensors.
  The same binary map helper supports `min` and `max` for dense row-major
  Vulkan `Float64` tensor/scalar, scalar/Tensor, and Tensor/Tensor broadcast
  cases, and direct public Tensor `min` / `max` route through it for eligible
  Vulkan operands.
  Vulkan `map` also supports real-valued component operations
  `real-part`, `imag-part`, and `conjugate` for dense row-major `Float64`
  inputs through the separate unary helper; direct Tensor component operations
  on Vulkan return Vulkan-placed `Float64` tensors.
  Public `contract` now supports Vulkan-placed dense row-major rank-N
  `Float64` tensors with zero or more explicit contracted axis pairs. Output
  axes are ordered as free left axes followed by free right axes. Results
  remain Vulkan-placed tensors. The generic contract kernel is generated from
  checked-in GLSL and uses rank/shape/stride/axis-list metadata-buffer dispatch.
  Public `matrix/transpose` supports Vulkan-placed dense row-major `Float64`
  rank-2 tensors and returns a Vulkan-placed transposed Tensor.
  Public `matrix/diagonal` and `matrix/diagonal-matrix` support Vulkan-placed
  dense row-major `Float64` inputs and return Vulkan-placed Tensor results.
  Public `matrix/trace` supports Vulkan-placed dense row-major square
  `Float64` inputs, computes on Vulkan, and reads back only the scalar result
  required by the scalar return contract.
  Public `matrix/rank` supports Vulkan-placed dense row-major `Float64` or
  `Float32` rank-2 inputs, computes on Vulkan, and reads back only the scalar
  `Integer` result required by the scalar return contract.
  Public `matrix/lu` supports Vulkan-placed dense row-major square `Float64`
  inputs, computes partial-pivot LU on Vulkan, returns the combined `lu`
  factor as a Vulkan-placed Tensor, and reads back only `pivots` plus
  `swap-count` dictionary metadata required by the public contract.
  Public `matrix/determinant` supports Vulkan-placed dense row-major square
  `Float64` inputs, computes on Vulkan, and reads back only the scalar
  `Float64` result required by the scalar return contract.
  Public `matrix/solve` supports Vulkan-placed dense row-major square
  `Float64` coefficient tensors with Vulkan-placed rank-1 or rank-2 `Float64`
  right-hand tensors, computes Gaussian elimination on Vulkan, returns
  Vulkan-placed solution tensors, and raises `tensor/singular-matrix` for
  singular systems.
  Public `matrix/inverse` supports CPU and Vulkan-placed dense row-major
  square `Float64`, `Float32`, `Complex128`, or `Complex64` inputs, computes
  Gauss-Jordan elimination, returns an inverse Tensor preserving input dtype
  and Vulkan placement for Vulkan inputs, and raises `tensor/singular-matrix`
  for singular inputs.
  Public `matrix/norm` supports Vulkan-placed dense row-major `Float64` and
  `Float32` inputs for default/`'frobenius`, `'one`, `'infinity`, `'max`,
  `'spectral`, and `'nuclear`. Public `matrix/singular-values`
  supports Vulkan-placed dense row-major rank-2 `Float64` and `Float32`
  inputs, computes on Vulkan with storage-buffer Gram scratch, returns a
  Vulkan-placed rank-1 Tensor preserving the input float dtype for
  `k = min(rows, columns)`, and keeps explicit CPU copyback as the inspection
  boundary. The singular-value shader maps backend non-convergence
  to `tensor/no-convergence`; the helper rejects logical matrices whose element
  count or scratch payload exceeds the shader's 32-bit index space before
  dispatch.
  Public `matrix/svd` supports Vulkan-placed dense row-major rank-2 `Float64`
  and `Float32` inputs, including lazy Vulkan inputs, computes reduced factor
  outputs on Vulkan with storage-buffer Gram scratch, returns Vulkan-placed
  `u`, `s`, and `v` tensors with the input float dtype and shapes
  `[rows k]`, `[k]`, and `[cols k]`, and keeps explicit CPU copyback as the
  inspection boundary.
  Direct destination `realize` is not an implicit GPU-to-CPU copy boundary:
  concrete or lazy Vulkan sources must be copied with explicit `to-device 'cpu`
  first and otherwise fail closed with `tensor/backend-unsupported`.
  Unsupported Vulkan dtypes, unsupported Vulkan map callables,
  incompatible map broadcast shapes, mixed CPU/Vulkan operands, and unsupported
  Vulkan layouts fail closed with Tensor backend diagnostics.
  Zero-size contracted axes in supported Vulkan layouts produce
  additive-identity output. The dtype/layout policy is now explicit: keep
  fixed-width dtype paths explicit, do not downcast between `Float64` and
  `Float32`, do not lower pointer-backed Big* Tensor dtypes to Vulkan, keep
  landed fixed-width complex backend support behind explicit capability bits,
  and defer full complex SVD factor output, CUDA complex singular-values/norm/SVD,
  complex eigen result contracts, and stride-aware layouts until their
  contracts exist.
- `dot` / `outer` helpers may be considered later as library conveniences over
  `contract`, but they should not replace the locked first Tensor surface.
