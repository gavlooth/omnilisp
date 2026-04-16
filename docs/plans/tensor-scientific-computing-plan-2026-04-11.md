# Tensor Scientific Computing Plan (2026-04-11)

Status: historical integrated plan; `TODO.md` is the sole live backlog, and
the current live Tensor/Vulkan lane is `TENSOR-100F`
Owner: language owner + Codex workflow
Mode: implementation plan and progress log
Historical lanes: `LANG-TENSOR-SCIENTIFIC-SURFACE-091`,
`LANG-TENSOR-BACKEND-BOUNDARY-092`, and `LANG-TENSOR-BROADCASTING-093` closed

## Purpose

Define Omni's first coherent scientific/ML numeric surface without copying
Julia syntax wholesale and without weakening Omni's existing language
invariants.

The chosen direction is:

- `Tensor` is the canonical rank-polymorphic numeric aggregate.
- `Array` remains the generic boxed collection.
- `map` is extended through dispatch for elementwise tensor operations and
  scalar broadcast.
- `contract` is the canonical tensor contraction operation.
- Constructor dispatch is the canonical public materialization boundary:
  `Array`, `List`, `Dictionary`, and `Tensor` consume iterators or normalize
  compatible values through their constructors.
- `realize` remains only a low-level Tensor storage primitive for exact
  destination writes and compatibility with shipped tensor-storage tests; it is
  not Omni's general lazy-computation primitive.

This plan records the naming decisions, semantic contracts, rollout slices,
test gates, deferred work, and implementation progress.

## Current Context

Omni already has the right language substrate for a scientific layer:

- multiple dispatch is a core extension mechanism,
- typed method dispatch is deterministic and test-anchored,
- parametric types and constructor type-application checking exist,
- numeric conversion in dispatch is explicit by design,
- generic collection operations already exist (`map`, `filter`, `foldl`,
  `ref`, `length`),
- deterministic scope/region memory ownership is a language invariant.

The missing piece is the numerical substrate. Current `Array` is a mutable
dynamic collection of boxed Omni values. It is useful as a general collection,
but it is not a high-performance homogeneous numeric storage type.

`examples/scicomp_demo.omni` now uses canonical `Tensor`, `map`,
`contract`, and `realize` forms. The older `vec-*`, `mat-*`, and `mat-mul`
prototype vocabulary is no longer part of the shipped surface.

Name collision note:

- Deduce already has the namespaced command `deduce/materialize!` and the
  relation attribute `[relation ... materialized]`.
- Tensor `realize` is a different concept: it writes a tensor expression,
  concrete tensor, or scalar into concrete tensor storage, optionally into an
  existing destination tensor. Public materialization should prefer constructor
  dispatch such as `(Tensor iterator)`, `(Array iterator)`, and
  `(List iterator)`.
- Do not rename or fold the Deduce materialized-view command into this tensor
  operation.

## Implementation Progress

- `TENSOR-001` is complete: the surface is frozen around `Tensor`, tensor
  dispatch through `map`, `contract`, and `realize`.
- `TENSOR-010` is complete: native `Tensor` runtime payloads, destructor,
  printing, type identity, boundary copy/promotion support, and `tensor?`,
  `dtype`, `shape`, `rank`, and `length` are implemented and tested.
- `TENSOR-020` is complete: `(Tensor Float64 shape data-or-scalar)` constructs
  native Float64 tensors from scalar fills or exact-length array/proper-list
  data, and `(ref tensor index-array)` indexes tensors with per-axis negative
  index support.
- `TENSOR-076` is complete: `(Tensor data)`, `(Tensor data Float64)`, and
  `(Tensor Float64 data)` infer native `Float64` tensor shape from real numeric
  scalars or rectangular nested arrays/proper lists whose leaves can narrow to
  finite `Float64` while preserving the explicit shape/data constructor.
- `TENSOR-077` is complete: `BigFloat` concrete Tensor storage supports
  constructor, `dtype`, `ref`, flat collection conversion, and concrete
  `realize`.
- `TENSOR-078` is complete: tensor-dispatched `map` supports native
  `BigFloat` tensors for unary, tensor-scalar, scalar-tensor, exact-shape
  tensor-tensor, and right-aligned singleton-axis broadcast cases, with
  BigFloat scalar operands cloned into lazy map payloads for boundary-safe
  function-return and closure-capture behavior.
- `TENSOR-079` is complete: tensor-dispatched `contract` supports native
  `BigFloat` tensors through the pure C3 contraction fallback, including vector
  dot, rank-2 matrix product, zero-size contracted-axis identity, explicit
  destination realization, and lazy expression boundary survival. BLAS fast
  paths remain `Float64`-only.
- `TENSOR-081` is complete: native `BigInteger` Tensor storage supports
  constructor, `dtype`, `ref`, flat collection conversion, concrete
  `realize`, tensor-dispatched `map`, and pure C3 `contract` kernels for exact
  integer tensor work. BigInteger Tensor data must be exact `Integer` or
  `BigInteger` values; inexact `Float64` data fails closed.
- `TENSOR-082` is complete: native `BigComplex` Tensor storage supports
  constructor, `dtype`, `ref`, flat collection conversion, concrete
  `realize`, tensor-dispatched `map`, and pure C3 `contract` kernels for
  complex tensor work. Integer, Float64, BigInteger, and BigFloat leaves promote
  to zero-imaginary BigComplex elements; BigComplex leaves preserve their
  complex components.
- `TENSOR-083` is complete: BigComplex Tensor component kernels support
  elementwise `real-part`, `imag-part`, and `conjugate`. Component extraction
  returns BigFloat tensors, conjugate returns BigComplex tensors, and lazy
  BigComplex Tensor sources are realized before the component kernel runs.
- `TENSOR-084` is complete: real Tensor component semantics are now
  dtype-preserving. `real-part` and `conjugate` copy Float64, BigInteger, and
  BigFloat tensors; `imag-part` returns same-shape zero tensors in the same
  dtype.
- `TENSOR-085` is complete: Tensor `abs` applies elementwise magnitude for all
  native tensor dtypes. Real tensor dtypes preserve dtype and shape;
  BigComplex tensors return same-shape native BigFloat magnitude tensors.
- `TENSOR-086` is complete: Tensor `sqrt` applies elementwise square root for
  all native tensor dtypes. Float64 and BigInteger tensor inputs return
  same-shape Float64 tensors; BigFloat and BigComplex tensor inputs preserve
  dtype.
- `TENSOR-087` is complete: Tensor unary scientific math applies elementwise
  `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`,
  `log`, and `log10` for all native tensor dtypes. Follow-on support adds
  elementwise CPU Tensor `math/erf`, `math/erfc`, `stats/normal-cdf`, and
  `stats/normal-quantile`;
  `Float64`/`Float32` preserve float dtype, `BigInteger` returns same-shape
  `Float64`, `BigFloat` preserves dtype, and `BigComplex` fails closed for
  these error-function/distribution primitives. CUDA dense row-major
  `Float64`/`Float32` also supports `math/erf`, `math/erfc`,
  `stats/normal-cdf`, and `stats/normal-quantile` through fixed scientific
  unary opcodes; CUDA quantile uses a probability-status word so invalid
  probabilities fail before result exposure. Vulkan `Float32` supports
  `stats/normal-cdf` and `stats/normal-quantile` through dedicated helpers;
  Vulkan quantile uses a status-bearing inverse-CDF shader so invalid
  probabilities fail before result exposure. Vulkan `Float64` distribution
  support remains deferred behind explicit double approximation/status policy.
- `TENSOR-088` is complete: Tensor `pow` applies elementwise power for
  tensor-scalar, scalar-tensor, and broadcast tensor-tensor inputs. BigComplex
  wins the result dtype if either input is complex, BigFloat wins if either
  input is BigFloat, and remaining Float64/BigInteger inputs return Float64
  tensors.
- `TENSOR-089` is complete: Tensor `atan2` applies elementwise real-plane
  two-argument arctangent for tensor-scalar, scalar-tensor, and broadcast
  tensor-tensor inputs. BigFloat inputs preserve dtype, remaining real/exact
  inputs return Float64 tensors, and complex operands fail closed.
- `TENSOR-090` is complete: Tensor `floor`, `ceiling`, `round`, and
  `truncate` apply elementwise rounding for real Tensor inputs and return
  native BigInteger Tensor results. BigFloat inputs use the exact scalar
  BigFloat rounding path; complex operands fail closed.
- `TENSOR-090T` is complete: `matrix/transpose` transposes rank-2 Tensor
  values while preserving native dtype across `Float64`, `BigInteger`,
  `BigFloat`, and `BigComplex`. High-precision element handles are cloned into
  the transposed result rather than shallow-copied.
- `TENSOR-090V` is complete: `matrix/diagonal` extracts the main diagonal from
  rank-2 Tensor values into a rank-1 Tensor of length `min(rows, columns)`,
  preserving native dtype and cloning owned high-precision element handles.
- `TENSOR-090W` is complete: `matrix/diagonal-matrix` builds a square rank-2
  Tensor from a rank-1 Tensor, preserving native dtype, cloning owned
  high-precision diagonal handles, and filling off-diagonal cells with the
  dtype's zero value.
- `TENSOR-090X` is complete: `matrix/identity` builds a square identity Tensor
  from a non-negative integer size, defaults to `Float64`, and supports
  optional `Float64`, `BigInteger`, `BigFloat`, and `BigComplex` dtype
  selection.
- `TENSOR-090U` is complete: `matrix/trace` sums the diagonal of square
  rank-2 Tensor values and returns a scalar in the Tensor's native numeric
  family: `Double` for `Float64`, or `BigInteger`, `BigFloat`, and
  `BigComplex` for the matching high-precision Tensor dtypes.
- `TENSOR-090Y` is complete: `matrix/rank` returns the numerical rank of
  rectangular rank-2 `Float64` Tensor values as an `Integer`, with default
  tolerance `1e-12` and optional non-negative finite tolerance selection.
- `TENSOR-090Z` is complete: `matrix/rank` now uses optional runtime-loaded
  `LAPACKE_dgesvd` acceleration when available, counting singular values
  above tolerance and retaining the pure row-echelon fallback.
- `TENSOR-090AH` is complete: `matrix/norm` returns `Float64` norms for
  rank-2 `Float64` Tensor values, defaulting to Frobenius and accepting the
  initial `'frobenius`, `'one`, `'infinity`, or `'max` selectors.
- `TENSOR-090AI` is complete: `matrix/norm` also accepts `'spectral` for the
  largest singular value and `'nuclear` for the sum of singular values,
  reusing the existing pure/LAPACK SVD machinery.
- `TENSOR-090AJ` is complete: `matrix/norm` spectral and nuclear selectors now
  have explicit backend and forced-fallback coverage through a private
  test-only `dgesvd` disable hook.
- `TENSOR-090AK` is complete: `matrix/rank`, `matrix/singular-values`, and
  `matrix/svd` now have forced pure fallback coverage through the same private
  `dgesvd` disable hook.
- `TENSOR-090AA` is complete: `matrix/singular-values` returns descending
  singular values for rank-2 `Float64` Tensor values as a rank-1 `Float64`
  Tensor, reusing the existing pure/LAPACK reduced SVD machinery.
- `TENSOR-090AB` is complete: `matrix/eigenpairs` returns general
  nonsymmetric eigenpairs for square rank-2 `Float64` Tensor values as
  `BigComplex` `values` and aligned `BigComplex` `vectors` tensors. It
  realizes lazy inputs, supports empty square matrices, sorts eigenpairs
  deterministically, and may use runtime `LAPACKE_dgeev` when available.
- `TENSOR-090AC` is complete: `matrix/eigenpairs` now has a pure fallback for
  hosts without runtime `LAPACKE_dgeev`, preserving the same sorted
  `BigComplex` values/vectors contract.
- `TENSOR-090AD` is complete: forced no-`dgeev` fallback tests now cover 3x3
  diagonal, upper-triangular, and real-plus-complex-block matrices, plus
  residual checks that representative vector columns satisfy
  `A*v ~= lambda*v`.
- `TENSOR-090AE` is complete: accelerated `LAPACKE_dgeev`
  `matrix/eigenpairs` tests now validate representative real and complex
  vector columns satisfy `A*v ~= lambda*v` when the backend is available.
- `TENSOR-090AL` is complete: `matrix/eigenvalues` and
  `matrix/eigenvectors` now have forced pure fallback coverage through a
  private test-only `dsyev` disable hook.
- `TENSOR-090AF` is complete: both accelerated and forced-fallback
  `matrix/eigenpairs` tests now validate a non-normal upper-triangular
  residual for a non-basis returned vector column.
- `TENSOR-090AG` is complete: reusable 3x3 residual helpers now let backend
  and forced-fallback `matrix/eigenpairs` tests validate every returned vector
  column for representative non-normal and real-plus-complex-block matrices.
- `TENSOR-091` is complete: Tensor `min` and `max` apply tensor-scalar,
  scalar-tensor, and broadcast tensor-tensor real comparison. BigFloat wins if
  either input is BigFloat, Float64 wins if either input is Float64, and the
  remaining exact inputs return native BigInteger Tensor results. Complex
  operands fail closed.
- `TENSOR-092` is complete: Tensor `gcd` and `lcm` apply tensor-scalar,
  scalar-tensor, and broadcast tensor-tensor exact integer operations.
  Tensor operands must use native BigInteger Tensor storage; inexact or
  complex Tensor dtypes fail closed. Results are native BigInteger Tensor
  values.
- `TENSOR-030` is complete for concrete tensor realization: concrete
  tensors satisfy the tensor-expression boundary, `(realize tensor)`
  returns the already-concrete tensor, `(realize tensor out)` copies into a
  mutable exact-shape/dtype destination, and `(realize scalar out)` fills
  the destination.
- `TENSOR-040` is complete for elementwise `map`: unary tensor, tensor-scalar,
  scalar-tensor, and exact-shape tensor-tensor `Float64` inputs route through
  the existing generic `map` dispatch.
- `TENSOR-070` is complete: Tensor `map` now supports NumPy-style trailing
  singleton-axis broadcasting for tensor-tensor inputs, with rank-0 tensors
  treated as broadcast-compatible tensor scalars and deterministic
  `tensor/shape-mismatch` failures for incompatible shapes.
- `TENSOR-075` is complete: the canonical collection constructor/conversion
  surfaces now accept Tensor values. `(Array tensor)` and `(List tensor)`
  materialize lazy Tensor expressions if needed and expose flat row-major
  element values; shape/rank metadata remains available through `shape` and
  `rank` rather than nested collection conversion.
- `TENSOR-076B` is complete: the Tensor constructor consumes finite numeric
  iterators through dynamic dispatch, so `(Tensor iterator)`,
  `(Tensor iterator dtype)`, and `(Tensor dtype shape iterator)` are public
  constructor-materialization forms instead of requiring a separate
  force/realize step.
- `TENSOR-076C` is complete: `Iterator(^Tensor)` exposes Tensor values as a
  flat row-major iterator, matching `(Array tensor)` and `(List tensor)`.
  Lazy CPU Tensor expressions are realized once into Tensor storage before
  iteration; non-CPU device tensors fail closed until the caller explicitly
  copies with `to-device 'cpu`.
- `TENSOR-050` is complete for pure `Float64` `contract`: paired-axis arrays
  and explicit axis-list contraction support one or more axis pairs, rank-0
  scalar results, multi-axis contractions, per-axis negative normalization,
  and deterministic axis/dimension diagnostics.
- `TENSOR-060A` is complete as a fusion-boundary audit: true
  `(realize (map ...) out)` / `(realize (contract ...) out)` fusion
  requires a lazy tensor-expression value or a macro/special-form rewrite,
  because ordinary eager evaluation realizes the source before
  `realize` receives it.
- `TENSOR-060B-prep` is complete: eager `map` and `contract` now execute
  through internal destination-ready pure Tensor kernels, so a later lazy
  expression value can realize through staged destination storage without
  reimplementing the elementwise or contraction loops.
- `TENSOR-060B` is complete: Tensor-dispatched `map` and `contract` now return
  lazy expression payloads under the existing `Tensor` value, and
  `realize` can allocate a concrete result or stage evaluation before
  copying into a destination tensor after success.
- `TENSOR-060C` is complete: valid non-negative Tensor dimensions no longer
  trip the platform-size overflow guard on 64-bit builds, and contraction over
  zero-size axes realizes the additive identity instead of entering the
  contracted-index divide/modulo path.
- `TENSOR-060D` is complete: rank-0 concrete destination realization,
  zero-size destination/realization paths, aliased elementwise `map`
  destination realization, and duplicate-axis checks after negative-axis
  normalization are covered.
- `TENSOR-060E` is complete: tensor expression child values copied or promoted
  across boundary paths are now rolled back if a later tensor copy/promotion or
  wrapper allocation step fails. Generic realized-value cleanup also
  recurses through tensor expression edges before freeing the copied tensor
  payload.
- `TENSOR-060F` is complete: tensor values now use the non-unique destination
  retry promotion path at return boundaries, and concrete `realize` fast
  paths validate source backing storage before returning or copying malformed
  tensors.
- `TENSOR-060G` is complete: lazy Tensor `ref` cancels and destroys the
  temporary concrete realization after extracting the scalar result, with
  destructor-count regression coverage in the memory-lifetime smoke slice.
- `TENSOR-060H` is complete: nested lazy Tensor `realize` now cleans
  temporary child tensors created while resolving nested `map`/`contract`
  expression operands, leaving only the top-level realized result live
  until caller cleanup.
- `TENSOR-060I` is complete: failed lazy Tensor realization now cleans the
  fresh concrete result tensor through the generic fresh-value cleanup path,
  with nested `contract` realization and failed `map` realization
  destructor-count coverage in the memory-lifetime smoke slice.
- `TENSOR-060J` is complete: lazy Tensor `realize` into an explicit
  destination now stages expression evaluation into a temporary Tensor and
  commits to the destination only after success. The contract destination/source
  aliasing rejection walks lazy expression operands while ignoring zero-byte
  tensor storage, and failed Tensor constructor data validation cleans the
  unreturned tensor wrapper.
- `TENSOR-060K` is complete: lazy Tensor expression edge rollback now has
  deterministic copy/promotion fail counters and regression coverage for a
  mid-edge copy failure after a prior child tensor copy has committed. Root
  promotion of lazy Tensor expressions now promotes tensor operand edges into
  the destination ESCAPE lane, while graph-audit traversal treats shared
  dispatch method tables as callable descriptors rather than Tensor-owned
  expression data.
- `TENSOR-110` is complete: the Tensor cleanup surface lane is closed with a
  canonical example in `examples/scicomp_demo.omni` and newly added lazy
  `map`/`contract` return and closure-capture regression coverage.
  Metadata examples retain `Array`-based shape/axes payload forms where that
  transport format was already canonical.
- Current next slices are optional backend follow-ons after the closed
  `TENSOR-100` explicit CUDA/cuBLAS design: concrete CUDA placement metadata,
  fail-closed `to-device` diagnostics, CUDA buffer ownership, explicit
  CPU/CUDA copies, cuBLAS handle lifecycle, and CUDA-placed `Float64`
  rank-2 contraction.
- `TENSOR-090A` is complete: dense rank-2 `Float64` contraction equivalent to
  `(contract a b [1 0])` now has a direct native BLAS `dgemm` fast path at the
  Tensor evaluation boundary. The fast path is optional at runtime, uses the
  existing scoped native `TensorVal` storage, does not introduce a public
  `TensorHandle`, and falls back to the pure C3 contraction kernel when no
  compatible BLAS library/symbol is available or when dtype/rank/layout/axis
  conditions do not match.
- `TENSOR-090B` is complete: that private `dgemm` backend now accepts
  transpose flags and covers all contiguous row-major rank-2 single-axis
  `Float64` contractions, including `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`,
  while still using the pure C3 contraction kernel as the semantic fallback.
- `TENSOR-090C` is complete: the same private BLAS backend now resolves
  `cblas_dgemv` and covers contiguous row-major rank-2/rank-1 and rank-1/rank-2
  single-axis `Float64` contractions, including transposed matrix-vector and
  vector-matrix layouts. Unsupported rank/layout/symbol cases still fall back
  to the pure C3 contraction kernel.
- `TENSOR-090D` is complete: the private BLAS backend now resolves
  `cblas_ddot` and covers contiguous rank-1/rank-1 `Float64` vector dot
  contractions, with unsupported cases still falling back to the pure C3
  contraction kernel.
- `TENSOR-090E` is complete: the private BLAS backend now resolves
  `cblas_dger` and covers contiguous rank-1/rank-1 `Float64` zero-axis
  contractions as outer products, with unsupported cases still falling back to
  the pure C3 contraction kernel.
- `TENSOR-090F` is complete: the first public solver surface is
  `matrix/solve`, not bare `solve`, `linalg/solve`, `tensor/solve`, or a
  backend-flavored `tensor/lapack/solve`. The first implementation uses a pure
  Float64 Gaussian-elimination fallback for square rank-2 coefficient tensors
  and rank-1/rank-2 RHS tensors. LAPACK/LAPACKE may later accelerate the same
  surface, but the pure solver is the semantic oracle.
- `TENSOR-090G` is complete: `matrix/solve` now has optional runtime-loaded
  `LAPACKE_dgesv` acceleration behind the same public surface. Missing
  `liblapacke`, missing symbols, unsupported LAPACK ABI widths, or unavailable
  backend paths retain the pure runtime solver fallback. LAPACK singular
  results map to the same `tensor/singular-matrix` contract.
- `TENSOR-090AM` is complete: `matrix/solve` and `matrix/inverse` now have
  forced pure fallback coverage through a private test-only `dgesv` disable
  hook. The `dgesv` backend path has its own disable state and no longer
  depends on the unrelated `dgeev` test hook.
- `TENSOR-090H` is complete: `matrix/lu` is the first rank-2 decomposition
  surface. It realizes lazy square `Float64` Tensor inputs, computes pure C3
  partial-pivot LU factors, raises `tensor/singular-matrix` for singular
  inputs, and returns a dictionary with `lu`, final 0-based `pivots`, and
  `swap-count` metadata. Optional LAPACK `dgetrf` acceleration is a backend
  path behind this same public dictionary contract.
- `TENSOR-090I` is complete: `matrix/determinant` is the first decomposition
  consumer. It realizes lazy square `Float64` Tensor inputs, reuses the pure
  partial-pivot LU kernel, returns a `Float64` scalar determinant, treats
  singular matrices as determinant `0.0`, and uses the empty square convention
  `1.0`.
- `TENSOR-090T` is complete: `matrix/transpose` is the first dtype-preserving
  structural matrix transform. It realizes lazy rank-2 Tensor inputs, returns
  shape `[columns rows]`, and preserves native Tensor dtype for `Float64`,
  `BigInteger`, `BigFloat`, and `BigComplex`.
- `TENSOR-090V` is complete: `matrix/diagonal` is the second dtype-preserving
  structural matrix transform. It realizes lazy rank-2 Tensor inputs, returns
  a rank-1 Tensor with length `min(rows, columns)`, supports rectangular and
  empty matrices, and preserves native Tensor dtype for `Float64`,
  `BigInteger`, `BigFloat`, and `BigComplex`.
- `TENSOR-090W` is complete: `matrix/diagonal-matrix` is the first
  dtype-preserving structural matrix constructor. It realizes lazy rank-1
  Tensor inputs, returns a square rank-2 Tensor with shape `[n n]`, supports
  empty vectors, preserves native Tensor dtype for `Float64`, `BigInteger`,
  `BigFloat`, and `BigComplex`, and fills off-diagonal entries with the dtype's
  zero value.
- `TENSOR-090X` is complete: `matrix/identity` is the first size-driven
  structural matrix constructor. It returns a square rank-2 Tensor with shape
  `[n n]`, defaults to `Float64`, supports optional `Float64`, `BigInteger`,
  `BigFloat`, and `BigComplex` dtype selection, and fills off-diagonal entries
  with the dtype's zero value.
- `TENSOR-090U` is complete: `matrix/trace` is the first dtype-preserving
  structural matrix reduction. It realizes lazy square rank-2 Tensor inputs,
  sums diagonal entries, returns `0.0` or the high-precision zero for empty
  square matrices, and returns a scalar in the Tensor's native numeric family.
- `TENSOR-090Y` is complete: `matrix/rank` is the first numerical structural
  matrix reduction. It realizes lazy rectangular rank-2 `Float64` Tensor
  inputs, returns an `Integer`, supports empty axes, and accepts an optional
  non-negative finite tolerance.
- `TENSOR-090Z` is complete: `matrix/rank` now has optional
  `LAPACKE_dgesvd` acceleration when available. The backend path counts
  singular values above tolerance and the pure row-echelon rank counter remains
  the fallback.
- `TENSOR-090AH` is complete: `matrix/norm` is the first general matrix norm
  reducer. It realizes lazy rank-2 `Float64` Tensor inputs, supports empty
  axes, returns a `Float64`, defaults to Frobenius, and accepts explicit
  `'frobenius`, `'one`, `'infinity`, and `'max` selectors.
- `TENSOR-090AI` is complete: `matrix/norm` now covers singular-value norms
  with `'spectral` and `'nuclear` selectors, implemented through the existing
  pure/LAPACK SVD path.
- `TENSOR-090AJ` is complete: focused tests now prove the SVD-backed
  `matrix/norm` selectors use runtime `LAPACKE_dgesvd` when available and keep
  the same results when `dgesvd` is disabled for forced pure fallback
  validation.
- `TENSOR-090AK` is complete: focused tests now force `dgesvd` off for
  `matrix/rank`, `matrix/singular-values`, and `matrix/svd`, proving those
  already-shipped public contracts preserve results through pure fallback
  paths.
- `TENSOR-090AA` is complete: `matrix/singular-values` exposes the `s` part of
  the existing reduced SVD contract directly. It realizes lazy rank-2
  `Float64` Tensor inputs, supports rectangular and rank-deficient matrices,
  returns shape `[min(rows, columns)]`, and uses the same optional
  `LAPACKE_dgesvd` acceleration and pure SVD fallback as `matrix/svd`.
- `TENSOR-090AB` is complete: `matrix/eigenpairs` is the general
  nonsymmetric eigen surface. It realizes lazy square `Float64` Tensor inputs
  and returns a dictionary with `values` as `BigComplex` shape `[n]` and
  `vectors` as `BigComplex` shape `[n n]`, with vector columns aligned to the
  same-position values. Eigenpairs are sorted by descending magnitude with
  deterministic real/imaginary tie-breakers. Empty square input returns empty
  `[0]` values and `[0 0]` vectors. The implementation uses runtime-loaded
  `LAPACKE_dgeev` when available and retains a pure QR/nullspace fallback
  otherwise.
- `TENSOR-090AD` is complete: the no-`dgeev` fallback path is now covered on
  3x3 diagonal, upper-triangular, and real-plus-complex-block matrices in the
  focused advanced collections/module suite, including residual checks that
  representative vector columns satisfy `A*v ~= lambda*v`.
- `TENSOR-090AE` is complete: the accelerated `LAPACKE_dgeev` path now has
  focused residual checks for representative real and complex vector columns,
  preserving the same `A*v ~= lambda*v` contract under backend execution.
- `TENSOR-090AF` is complete: backend and forced-fallback `matrix/eigenpairs`
  tests now include a non-normal upper-triangular residual check, proving a
  non-basis middle vector column still satisfies `A*v ~= lambda*v`.
- `TENSOR-090AG` is complete: the fixed residual assertions are now
  complemented by reusable 3x3 all-column residual helpers that validate
  representative non-normal and real-plus-complex-block matrices under both
  backend and forced-fallback execution.
- `TENSOR-090AQ` is complete: the pure no-`dgeev` `matrix/eigenpairs` fallback
  now treats isolated 2x2 real-Schur blocks as converged, fixing a
  bounded-container-only corruption of the trailing real eigenvalue in a 3x3
  real-plus-complex-block matrix.
- `TENSOR-090S` is complete: `matrix/inverse` is the first solve-derived
  matrix transform surface. It realizes lazy square `Float64` Tensor inputs,
  solves against an identity RHS through optional `LAPACKE_dgesv` or the pure
  Gaussian fallback, returns a same-shape `Float64` Tensor inverse, and raises
  `tensor/singular-matrix` for singular inputs.
- `TENSOR-090AM` is complete: focused tests now force `dgesv` off for
  `matrix/solve` and `matrix/inverse`, proving both shipped solver contracts
  preserve results through the pure Gaussian fallback.
- `TENSOR-090J` is complete: `matrix/qr` is the first non-LU decomposition
  surface. It realizes lazy rank-2 `Float64` Tensor inputs with rows greater
  than or equal to columns, computes reduced QR for full-column-rank matrices,
  returns a dictionary with `q` shape `[rows columns]` and `r` shape
  `[columns columns]`, and raises `tensor/singular-matrix` for rank-deficient
  inputs.
- `TENSOR-090K` is complete: `matrix/cholesky` is the first symmetric
  positive-definite decomposition surface. It realizes lazy square `Float64`
  Tensor inputs, computes a pure lower-triangular Cholesky factor, returns a
  same-shape `Float64` Tensor with zero entries above the diagonal, and raises
  `tensor/not-positive-definite` for nonsymmetric or non-positive-definite
  inputs.
- `TENSOR-090L` is complete: `matrix/svd` is the first rectangular
  decomposition surface. It realizes lazy rank-2 `Float64` Tensor inputs,
  computes a pure reduced SVD through a symmetric Jacobi eigensolver on the
  smaller Gram matrix, supports rank-deficient inputs, and returns a dictionary
  with `u` shape `[rows k]`, `s` shape `[k]`, and `v` shape `[columns k]`,
  where `k = min(rows, columns)`.
- `TENSOR-090M` is complete: `matrix/eigenvalues` and `matrix/eigenvectors`
  are the first eigen surfaces. They realize lazy square symmetric `Float64`
  Tensor inputs, use the pure symmetric Jacobi eigensolver, raise
  `tensor/not-symmetric` for nonsymmetric inputs, and return descending
  eigenvalues. `matrix/eigenvectors` returns aligned `values` and `vectors`
  columns.
- `TENSOR-090N` is complete: `matrix/lu` and `matrix/determinant` now have
  optional runtime-loaded `LAPACKE_dgetrf` acceleration behind the same public
  surfaces. Missing `liblapacke`, missing symbols, unsupported LAPACK ABI
  widths, or unavailable backend paths retain the pure runtime LU fallback.
  LAPACK singular results preserve the existing contracts: `matrix/lu` raises
  `tensor/singular-matrix`, while `matrix/determinant` returns `0.0`.
- `TENSOR-090AN` is complete: `matrix/lu` and `matrix/determinant` now have
  forced pure fallback coverage through a private test-only `dgetrf` disable
  hook.
- `TENSOR-090O` is complete: `matrix/cholesky` now has optional
  runtime-loaded `LAPACKE_dpotrf` acceleration behind the same public surface.
  The runtime still exact-checks symmetry before calling LAPACK, zeros the
  upper triangle after a lower-factor backend result, and maps non-positive
  definite results to the existing `tensor/not-positive-definite` contract.
- `TENSOR-090AO` is complete: `matrix/cholesky` now has forced pure fallback
  coverage through a private test-only `dpotrf` disable hook.
- `TENSOR-090P` is complete: `matrix/qr` now has optional runtime-loaded
  `LAPACKE_dgeqrf` plus `LAPACKE_dorgqr` acceleration behind the same public
  reduced-QR surface. The backend extracts the reduced `R`, forms reduced `Q`,
  sign-normalizes factor columns to preserve the existing positive-diagonal
  orientation, and maps rank-deficient inputs to `tensor/singular-matrix`.
- `TENSOR-090AP` is complete: `matrix/qr` now has forced pure fallback
  coverage through a private test-only QR backend disable hook. The existing
  tolerance-based LAPACK QR rank guard remains in place.
- `TENSOR-090Q` is complete: `matrix/eigenvalues` and `matrix/eigenvectors`
  now have optional runtime-loaded `LAPACKE_dsyev` acceleration behind the
  same symmetric-real public surfaces. The runtime still exact-checks symmetry
  before the backend call, sorts LAPACK's ascending eigenpairs into descending
  order, normalizes eigenvector signs for stable output, and keeps general
  nonsymmetric eigenpairs as a separate contract decision.
- `TENSOR-090AL` is complete: focused tests now force `dsyev` off for
  `matrix/eigenvalues` and `matrix/eigenvectors`, proving those shipped
  symmetric-real contracts preserve values and representative vector columns
  through the pure Jacobi fallback.
- `TENSOR-090R` is complete: `matrix/svd` now has optional runtime-loaded
  `LAPACKE_dgesvd` acceleration behind the same reduced SVD public surface.
  The backend requests reduced factors, transposes LAPACK's `vt` into Omni's
  `v` column-factor layout, normalizes signs for stable output, and keeps the
  pure Gram/Jacobi SVD implementation as the semantic oracle.
- `TENSOR-100` is complete as an explicit-device CUDA/cuBLAS design slice:
  future GPU support stays behind the existing `Tensor` value, uses
  `to-device`, `device`, and `tensor-backends` as the first public placement
  and introspection surface, rejects `GpuTensor`, `CudaTensor`, and
  backend-flavored math names, and forbids implicit CPU/GPU transfers inside
  ordinary Tensor operations.
- `TENSOR-100A` is complete as the CPU-only placement/introspection surface:
  `device` reports `'cpu` for current Tensor values, `to-device` with target
  `'cpu` realizes Tensor expressions to CPU Tensor storage, `to-device` with
  target `'cuda` fails closed with `tensor/backend-unavailable`, and the
  primitives are registered for interpreter and AOT lookup without adding a
  CUDA dependency.
- `TENSOR-100E` is complete as the correctness-first Vulkan baseline: Vulkan is the portable explicit GPU backend
  direction behind the same `Tensor` surface. The landed slices add
  runtime-loaded `libvulkan` probing, structured `tensor-backends` inventory
  with explicit `Float64` and `Float32` kernel capability, explicit
  `to-device 'vulkan` placement for concrete `Float64` or `Float32` CPU
  Tensor storage, `to-device 'cpu` copyback from Vulkan storage, dense
  row-major `Float64` and `Float32` `map` arithmetic `+`, `-`, `*`, `/`,
  `min`, `max`, unary helpers, direct `min`/`max`, rank-N `contract`,
  structural matrix kernels, direct reducers, SVD-backed surfaces, and serial
  factor/solve surfaces for eligible dense row-major tensors. Contract output
  axes are ordered as free left axes followed by free right axes.
  Results are Vulkan-placed tensors; callers copy back explicitly with
  `to-device 'cpu`. The generic contract shader is generated from checked-in
  GLSL and uses rank/shape/stride/axis-list metadata-buffer dispatch. Unsupported Vulkan
  contract cases, including mixed-device and unsupported layout/dtype, fail
  closed. Zero-axis contractions and zero-size contracted axes in supported
  Vulkan layouts preserve the CPU Tensor oracle. The Vulkan dtype/layout
  policy is recorded in `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`:
  keep fixed-width dtype paths explicit, do not downcast between `Float64` and
  `Float32`, do not lower pointer-backed Big* Tensor dtypes to Vulkan, keep
  landed fixed-width complex backend support behind explicit capability bits,
  and defer full complex SVD factor output, CUDA complex singular-values/norm/SVD,
  complex eigen result contracts, and stride-aware layouts until their
  contracts exist.
  The broader Vulkan math-library direction, including helper factoring,
  real/fixed-width-complex dtype prerequisites, stride-aware layout
  prerequisites, and the measured parallel solver baseline, is now recorded in
  `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`; remaining live
  Vulkan work continues under `TENSOR-100F`.
- Solver/decomposition namespace checkpoint: future rank-2 decomposition
  conveniences should use `matrix/` names such as `matrix/lu`, `matrix/qr`,
  `matrix/cholesky`, `matrix/svd`, `matrix/eigenvalues`, and
  `matrix/eigenvectors` unless the owner explicitly changes the public
  namespace. `matrix/cholesky`, `matrix/svd`, `matrix/eigenvalues`, and
  `matrix/eigenvectors` are now shipped for their documented contracts.
  `matrix/eigenpairs` is the chosen broader nonsymmetric eigenpair surface and
  ships with both optional `LAPACKE_dgeev` acceleration and a pure fallback
  that treats isolated 2x2 real-Schur blocks as settled.

## Locked Naming Decisions

### `Tensor`

Use `Tensor` as the canonical public name for the n-dimensional homogeneous
numeric aggregate.

Rationale:

- It matches the practical ML/scientific meaning used by NumPy, PyTorch, JAX,
  and similar ecosystems.
- It avoids exposing storage representation in the public type name.
- It covers rank 0, rank 1, rank 2, and rank n without introducing `Vector`
  terminology.
- It keeps `Matrix` free for a possible rank-2 convenience layer later.

Contract:

- `Tensor` is computational, not a promise of full coordinate-transform tensor
  algebra.
- The first surface means: dtype, shape, rank, homogeneous numeric storage,
  indexing, elementwise expressions, contraction expressions, and explicit
  realization into storage. Reductions remain planned library conveniences,
  not part of the shipped first surface.

### `Array`

Keep `Array` as the generic boxed collection.

Rules:

- Do not make `Array` the scientific numeric aggregate.
- Do not rename `Array` to `Tensor`.
- Do not introduce `Vector` as a public generic collection term.

### `map`

Extend `map` through dispatch as the elementwise tensor operation.

Canonical forms:

```lisp
(map sqrt x)
(map + x y)
(map * x 2.0)
```

Contract:

- For tensors, `map` produces a tensor-shaped expression.
- The current implementation represents that expression as a lazy `Tensor`
  payload and realizes it only at `realize` or `ref`.
- Tensor `map` currently accepts one or two tensor/scalar inputs.
- Scalar arguments broadcast over tensor arguments.
- First implementation supports scalar broadcast and exact-shape tensor-tensor
  inputs.
- `TENSOR-070` adds concrete trailing singleton expansion for tensor-tensor `map`.
  Exact-shape matching remains the exact fast path.
- Elementwise multiplication is `(map * a b)`, not `(* a b)`.

`TENSOR-070` broadcast rule for tensor-tensor `map`:

- Align shapes by trailing axes (right-align dimensions).
- Treat missing leading axes of a shorter shape as `1`.
- For each aligned axis pair, dimensions are compatible when equal or either is `1`.
- If all axes are compatible, result axis size is `max(axis_a, axis_b)`.
- If any axis pair is incompatible, raise `tensor/shape-mismatch`.
- Do not add backend-specific tensor types or map names in this dispatch path.

### `contract`

Use `contract` as the canonical summed-index tensor algebra operation.

Canonical form:

```lisp
(contract a b [1 0])
```

Meaning:

- contract axis `1` of `a` with axis `0` of `b`.
- for rank-2 shapes `[m k]` and `[k n]`, result shape is `[m n]`.

Multi-axis form:

```lisp
(contract a b [[1 0] [2 3]])
```

Rules:

- `contract` produces a tensor-shaped expression.
- paired-axis arrays are canonical; the explicit
  `(contract a b left-axes right-axes)` form remains accepted.
- explicit axis lists must have equal length.
- each axis must be in range for its tensor.
- dimensions for paired axes must match.
- result shape is all non-contracted axes from the left tensor followed by all
  non-contracted axes from the right tensor, preserving source order.
- no implicit default contraction is provided for general tensors.

### `realize`

Use constructor dispatch as the canonical public materialization boundary:
`(Array iterator)`, `(List iterator)`, `(Dictionary iterator)` where pair shape
applies, and `(Tensor iterator)` consume finite iterators. `realize` remains a
low-level Tensor storage primitive for destination reuse and compatibility with
shipped Tensor storage tests.

Low-level storage forms:

```lisp
(realize (map + a b))
(realize (map sqrt x) x)
(realize (contract a b [1 0]) c)
(realize 0.0 x)
(realize src out)
```

Contract:

- `(realize expr)` returns a concrete `Tensor`.
- `(realize expr out)` writes the expression into existing mutable tensor
  `out` and returns `out`.
- A concrete `Tensor` satisfies the tensor-expression protocol.
- If `expr` is already a concrete tensor and no destination is provided,
  `realize` may return the tensor unchanged.
- If `expr` is a scalar, a destination tensor is required; every element of the
  destination is set to that scalar, subject to dtype policy.
- If `expr` is a tensor and a destination is provided, shape and dtype
  compatibility are required.
- If `expr` is a lazy tensor expression, the first implementation may evaluate
  it into a temporary tensor and copy it into the destination.
- Later compiler/JIT optimization may fuse recognized `map` and `contract`
  expressions into destination storage, but fusion is an optimization, not the
  user-visible semantic contract.
- Unsafe aliasing must either use a scratch tensor or raise a deterministic
  diagnostic.

## Rejected Names And Non-Goals

Rejected as canonical:

- `DenseArray`: names storage representation, not the mathematical object.
- `Matrix`: too rank-2 in normal mathematical/programming use; do not use as
  the rank-polymorphic aggregate name.
- `Vector`: conflicts with the repo's collection terminology guardrail and
  should not be introduced for the first tensor surface.
- `matmul`: programming jargon and rank-2 biased; use `contract`.
- `map!`: compact but hides destination and aliasing semantics.
- `map-into`: technically clear, but awkward to read and too tied to `map`.
- `compute`: too broad.
- `assign`: too vague and too close to binding/update semantics.
- `copy` / `fill` as peers to `realize`: too close to each other; they may
  exist later only as convenience wrappers over `realize`.

Non-goals for the first implementation:

- no Julia dotted operator syntax,
- no implicit numeric widening in method dispatch,
- no automatic `(* tensor tensor)` semantics for general tensors,
- no GPU execution or storage backend in the core MVP,
- no autodiff,
- no sparse tensor backend,
- no view mutation semantics until slicing/view policy is explicit,
- no new shorthand aliases unless the owner explicitly approves them.

## Syntax Target

Use value-position type descriptors for dtype arguments, not `^` annotations in
constructor argument position.

Preferred first surface:

```lisp
(define x
  (Tensor Float64 [2 3]
    [1.0 2.0 3.0
     4.0 5.0 6.0]))

(dtype x)       ; => 'Float64
(shape x)       ; => [2 3]
(rank x)        ; => 2
(length x)      ; => 6
(ref x [1 2])   ; => 6.0
```

Rationale:

- `Float64` already exists as a value-position type descriptor/coercion
  surface.
- `^Float64` should remain annotation syntax unless a separate type-literal
  expression surface is deliberately added.
- `(ref x [i j])` keeps `ref` as a two-argument operation instead of forcing a
  variadic `ref` decision in the first slice.

Optional constructors after the base constructor is stable:

```lisp
(Tensor Float64 [2 3] 0.0)  ; scalar fill constructor
(Tensor Integer [4] [1 2 3 4])
```

## SPARC Plan

### Specification

Requirements:

- `Tensor` values are homogeneous.
- Every tensor has:
  - dtype,
  - shape,
  - rank,
  - element count,
  - contiguous or strided numeric storage metadata.
- Initial dtype target is `Float64`.
- `Integer` can follow after the `Float64` path is validated.
- `Boolean`, `Complex`, `Float32`, `Float64`, and fixed-width integer dtypes
  are deferred.
- Shape vectors use zero-based axis numbering, matching Omni's existing index
  examples.
- Rank 0 must remain design-compatible, even if not implemented in the first
  slice.
- Tensor construction validates data length against shape product.
- Tensor shape product arithmetic must be overflow-checked before allocation.
- Tensor operations report deterministic payloaded `raise` failures.

Diagnostic target:

- Domain: `tensor`.
- Representative codes:
  - `tensor/shape-mismatch`
  - `tensor/rank-mismatch`
  - `tensor/dtype-mismatch`
  - `tensor/axis-out-of-range`
  - `tensor/contract-dimension-mismatch`
  - `tensor/constructor-data-length`
  - `tensor/alias-unsafe`
  - `tensor/out-of-memory`

Acceptance:

- `Tensor` constructor success and failure behavior is documented.
- `map`, `contract`, and `realize` have deterministic shape/dtype/aliasing
  rules.
- The old `vec-*` / `mat-*` exploratory example is not presented as the
  canonical surface.

### Pseudocode

Tensor construction:

```text
Tensor(dtype_descriptor, shape_array, data_or_scalar):
  validate dtype descriptor
  validate shape is an Array of non-negative Integers
  compute element_count with overflow checks
  allocate TensorVal metadata and numeric storage
  if data_or_scalar is scalar:
    coerce scalar to dtype
    fill storage
  else:
    validate data_or_scalar is finite Array or list with element_count items
    coerce every element to dtype or raise dtype mismatch
    write unboxed storage
  return Tensor wrapper
```

Tensor `map`:

```text
map(f, inputs...):
  classify inputs as tensors or scalars
  if tensor inputs are exact-shape equal:
    result_shape = that shared shape
  else:
    align trailing axes between tensor shapes (missing leading axes => 1)
    if every aligned axis satisfies equality or contains 1:
      result_shape = axis-wise max
    else:
      raise tensor/shape-mismatch
  create a tensor expression that records:
    function f
    input expressions/scalars
    result shape
    result dtype policy
  return tensor expression
```

Tensor `contract`:

```text
contract(a, b, left_axes, right_axes):
  validate a and b are tensors
  validate axes are unique, in range, and same count
  validate paired contracted dimensions match
  compute result shape from non-contracted axes
  create a tensor expression that records:
    left expression
    right expression
    contraction axes
    result shape
    result dtype policy
  return tensor expression
```

`realize`:

```text
realize(source, maybe_dest):
  if maybe_dest is omitted:
    if source is concrete Tensor:
      return source
    if source is tensor expression:
      allocate a concrete Tensor for source shape/dtype
      evaluate expression into the new Tensor
      return Tensor
    otherwise:
      raise tensor/dtype-mismatch or type/arg-mismatch

  validate maybe_dest is mutable Tensor
  if source is scalar:
    coerce scalar to maybe_dest dtype
    fill maybe_dest storage
    return maybe_dest
  if source is Tensor or tensor expression:
    validate shape compatibility
    validate dtype compatibility or explicit conversion policy
    handle aliasing
    evaluate/copy source into maybe_dest
    return maybe_dest
  raise tensor/dtype-mismatch or type/arg-mismatch
```

### Architecture

Runtime value model:

- Add a native `Tensor` runtime value representation rather than modeling
  tensors as `Array` of boxed values.
- Add a tensor-expression protocol. A concrete `Tensor` participates in that
  protocol; lazy `map`/`contract` expression values can be added behind the
  same surface.
- Prefer a heap-backed `TensorVal` payload owned by the current scope/region
  wrapper destructor path.
- Keep the ownership authority in scope/region retain/release machinery.
- Do not introduce a per-type refcount for tensors.
- Storage metadata should include:
  - dtype enum or interned type symbol,
  - rank,
  - shape array,
  - strides array,
  - element count,
  - byte length,
  - data pointer,
  - mutability flag,
  - view/owner flag reserved for later slicing.

Likely code ownership:

- value/tag plumbing:
  - `src/lisp/value_types*.c3`
  - `src/lisp/value_predicates_accessors_basic.c3`
  - `src/lisp/value_print_helpers.c3`
  - `src/lisp/primitives_meta_types.c3`
- primitive implementation:
  - new `src/lisp/prim_tensor*.c3` files
  - primitive registration maps
  - compiler primitive variable/hash maps where needed
- dispatch/type integration:
  - builtin type registry setup
  - `type-of`, `is?`, `type-args`, and method dispatch applicability
- collection integration:
  - `length`
  - `ref`
  - `(set! tensor [i j] value)` only after indexing semantics are stable
  - `map`
- compiler/JIT/AOT:
  - direct primitive calls first,
  - compiler parity cases after interpreter behavior is green,
  - fusion for `(realize (map ...) out)` and `(realize (contract ...) out)`
    only after allocation semantics are correct.

Storage and backend policy:

- MVP uses native contiguous CPU `Float64` storage; the first follow-up storage
  dtype is concrete native `BigFloat`.
- Strides should be present in the metadata early, even if all MVP tensors are
  contiguous. This avoids painting slicing/views into a corner.
- The core implementation owns the semantic fallback for `map`, `contract`,
  and `realize`; it must not require BLAS, LAPACK, CUDA, or cuBLAS to be
  present for correctness.
- BLAS/LAPACK/CUDA/cuBLAS integrations are optional backend optimizations, not
  separate first-surface types.
- Optional means both build-optional and run-optional:
  - a normal Omni build and test run must succeed without BLAS/LAPACK/CUDA
    headers, shared libraries, drivers, or GPUs,
  - backend modules may be compiled or loaded only when their dependencies are
    present,
  - missing backends must fail or fall back deterministically rather than
    changing tensor semantics.
- Native library and context resources must use the FFI handle policy:
  - `dlopen`/backend library handles and cuBLAS context handles are
    `ForeignHandle`-style opaque resources, not raw integer pointers,
  - ownership and finalizers are explicit at the backend boundary,
  - backend handles must not own Omni `Value` graphs.
- Sparse and GPU storage strategies are deferred behind `Tensor`; they must not
  introduce user-facing `SparseTensor` or `GpuTensor` as the first canonical
  surface.

### `TENSOR-080` Backend boundary decision

Status: design/contract slice only. No backend runtime code lands here.

This slice closes the semantic contract for later BLAS/cuBLAS work without
adding a new user-facing runtime surface.

Decision points:

- The pure `Tensor` fallback remains the semantic source of truth for
  `map`, `contract`, and `realize`.
- Optional backend modules may accelerate evaluation only when their capability
  contract matches the operation.
- Backend selection is capability-driven and deterministic.
- Missing backend dependencies must fall back to the pure implementation rather
  than altering tensor semantics.
- Ordinary Tensor storage remains native and scope/region owned.
- Backend resources that are truly opaque foreign resources use explicit
  ownership/finalizer policy and must not own Omni `Value` graphs.
- GPU/device movement is explicit; ordinary tensor operations never imply
  implicit CPU/GPU transfers.
- No public backend-flavored mathematical names are introduced as the normal
  surface.

Capability / fallback contract:

| Operation class | Pure fallback | Optional backend module | Dispatch contract |
| --- | --- | --- | --- |
| Dense rank-2 `Float64` contraction | pure `contract` kernel | `tensor/blas` | only contiguous `Float64`, exact rank-2 layout, no aliasing, no hidden transfer |
| Decomposition and solve routines | pure `matrix/solve` fallback, pure `matrix/lu` factorization, pure `matrix/determinant` via LU, pure `matrix/inverse` via identity solve, pure `matrix/qr` reduced factorization, pure `matrix/cholesky` factorization, pure `matrix/singular-values` extraction, pure `matrix/svd` reduced factorization, pure symmetric `matrix/eigenvalues` and `matrix/eigenvectors`, pure general `matrix/eigenpairs` fallback, pure dtype-preserving `matrix/diagonal` transform, pure dtype-preserving `matrix/diagonal-matrix` constructor, pure dtype-selectable `matrix/identity` constructor, pure dtype-preserving `matrix/trace` reduction, pure `matrix/rank` numerical reduction, pure `matrix/norm` reduction, optional `dgesv` for solving/inversion, optional `dgetrf` for LU/determinant factorization, optional `dgeqrf`/`dorgqr` for QR factorization, optional `dpotrf` for Cholesky factorization, optional `dgesvd` for singular values, reduced SVD, rank singular-value counting, and spectral/nuclear norm selectors, optional `dsyev` for symmetric eigenpairs, optional `dgeev` for general eigenpairs | `tensor/lapack` | only when the operation can preserve existing `Tensor` semantics |
| Explicit host/device movement | none; CPU tensors stay CPU by default | `tensor/cuda` | move only when the caller requests it |
| Dense GPU contraction | pure `contract` kernel on host | `tensor/cublas` | requires explicit CUDA placement and compatible dtype/layout |

Module layering:

- Core runtime owns:
  - `Tensor` value representation,
  - dtype, shape, rank, total length, and `ref`,
  - tensor-expression protocol,
  - `map`, `contract`, and `realize` semantics,
  - pure CPU fallback kernels,
  - deterministic diagnostics and scope/region lifetime behavior.
- A later `tensor` library may own conveniences such as `sum`, `mean`,
  `reshape`, `transpose`, `permute`, `dot`, and `outer`.
- Optional backend modules may provide acceleration:
  - `tensor/blas` for BLAS-backed dense CPU kernels,
  - `tensor/lapack` for decomposition and solve routines,
  - `tensor/cuda` for explicit CUDA device storage/movement policy,
  - `tensor/cublas` for cuBLAS-backed dense GPU kernels.
- Backend modules own discovery and capability checks:
  - detect candidate shared libraries at runtime when possible,
  - expose availability through a small capability table,
  - pick accelerated kernels only when dtype, rank, layout, aliasing, and
    device placement match the kernel contract,
  - otherwise route to the pure fallback.
- Backend modules optimize execution; they do not change `Tensor`, `map`,
  `contract`, or `realize` semantics.
- The first CPU BLAS integration is a native runtime shim over shared-library
  discovery for `cblas_dgemm`; it is not routed through user-visible FFI
  `ForeignHandle` values.
- Solver/decomposition conveniences use the `matrix/` namespace for rank-2
  Tensor operations. Do not publish bare `solve`, `linalg/solve`,
  `tensor/solve`, or backend-flavored public solver names.
- The canonical user-facing contraction stays `contract`. Do not expose
  `blas-matmul`, `cublas-contract`, or similar backend-flavored names as the
  normal mathematical surface.
- Backend dispatch should happen at the realization/evaluation boundary,
  especially for `(realize (contract ...) out)` and
  `(realize (map ...) out)`.
- GPU/CUDA support requires explicit device movement in the first design.
  Do not introduce implicit CPU-GPU transfer in ordinary `map`, `contract`, or
  `realize`.
- BLAS first target:
  - optimize only contiguous `Float64` rank-2 contraction equivalent to
    `(contract a b [1 0])`,
  - preserve the C3 fallback as the validation oracle,
  - route unsupported strides, dtypes, aliasing, or shapes back to fallback.
  - Status: implemented 2026-04-14 as `TENSOR-090A` through a private
    `dlopen`/`dlsym` C shim for `cblas_dgemm`, with path-sensitive regression
    coverage when a compatible BLAS library is available.
  - Extended 2026-04-15 as `TENSOR-090C` to rank-2/rank-1 and rank-1/rank-2
    `cblas_dgemv` contraction layouts through the same private backend seam.
- cuBLAS first target:
  - require explicit CUDA device placement before invoking cuBLAS,
  - keep host tensors on CPU fallback unless explicitly moved,
  - keep host/device synchronization and error propagation deterministic.

Locked future device/backend introspection forms from `TENSOR-100`:

```lisp
(to-device x 'cuda)
(to-device x 'cpu)
(device x)

(tensor-backends)
```

`tensor-use-backend!` and backend-specific math names are rejected as the first
GPU control surface. Backend selection remains capability-driven, but GPU
placement is explicit through `to-device`.

### Refinement

Rollout slices:

1. `TENSOR-001` Documentation and surface freeze
   - Land this plan and TODO pointer.
   - Add a short note to future language docs only when implementation starts.

2. `TENSOR-010` Runtime representation
   - Add native Tensor payload, destructor, printing, `tensor?`, `dtype`,
     `shape`, `rank`, and `length`.
   - No `map`, `contract`, or `realize` yet.
   - Status: implemented 2026-04-11. Payloads are current-scope owned,
     payload copy/promotion paths deep-clone shape/stride/data storage, and
     the boundary graph audit treats tensors as leaf payloads.

3. `TENSOR-020` Constructor and indexing
   - Implement `(Tensor Float64 shape data-or-scalar)`.
   - Implement `(ref tensor index-array)`.
   - Keep variadic tensor `ref` deferred.
   - Status: implemented 2026-04-11. The constructor accepts `Float64` only,
     shape as an array or proper list of non-negative integers, and data as
     a numeric scalar fill or exact-length array/proper list. Tensor `ref`
     accepts array/proper-list indices and supports negative indexing per
     axis. Inferred-shape constructor overloads are implemented as
     `TENSOR-076`.
     axis.

4. `TENSOR-030` Tensor-expression protocol and `realize` — complete for
   concrete tensor/scalar sources
   - Treat concrete `Tensor` as a tensor expression.
   - Implement `(realize expr)`.
   - Implement `(realize expr out)`.
   - Implement scalar-to-destination fill through `(realize scalar out)`.
   - No lazy expression fusion is required yet.

5. `TENSOR-040` Elementwise `map`
   - Implement unary tensor `map`.
   - Implement tensor-scalar `map`.
   - Implement exact-shape tensor-tensor `map`.
   - Defer singleton-axis broadcast until after exact-shape coverage is green.
   - Status: implemented 2026-04-11. The runtime exposes fixed-arity typed
     `map` overloads over an internal `__tensor-map` primitive for unary,
     tensor-scalar, scalar-tensor, and exact-shape tensor-tensor `Float64`
     inputs. The later `TENSOR-060B` slice changed the returned payload to a
     lazy Tensor expression under the same public `Tensor` value.

6. `TENSOR-050` `contract`
   - Implement pure C3 fallback contraction for `Float64`.
   - Support one or more axis pairs.
   - Add rank-2 contraction examples through `contract`.
   - Status: implemented 2026-04-11. The runtime exposes public `contract`
     with pure `Float64` fallback, paired-axis shorthand, exact axis-list arity
     validation, duplicate-axis checks, paired-dimension checks, rank-0 scalar
     result support, and lazy Tensor expression realization through
     `TENSOR-060B`.

7. `TENSOR-060A` Destination realization and expression fusion audit
   - Status: implemented 2026-04-11 as a split/decision slice.
   - Current eager `map` and `contract` already make
     `(realize (map ...) out)` and `(realize (contract ...) out)`
     semantically correct, but they allocate a source tensor before
     `realize` sees it.
   - True fusion requires either a native lazy tensor-expression value that
     `map` and `contract` can return, or making `realize` a macro/special
     form that rewrites recognized source expressions before ordinary argument
     evaluation.
   - Choose the lazy tensor-expression value path for the next implementation
     slice. It keeps `realize` as a runtime storage boundary and gives
     backend dispatch a real expression node to inspect later.
   - Do not add public `map-into`, `matmul`, or backend-flavored escape names
     to compensate for the eager temporary.

8. `TENSOR-060B` Tensor-expression protocol and destination fusion
   - Optimize `(realize (map ...) out)` and
     `(realize (contract ...) out)` when the expression representation
     exists.
   - Implementation prep landed 2026-04-11: the first pure `map` and
     `contract` paths were split through destination-ready internal kernels
     before lazy payloads were wired on top.
   - Status: implemented 2026-04-11. Tensor-dispatched `map` and `contract`
     now return lazy expression payloads under the existing `Tensor` value,
     with `realize` forcing them into allocated concrete storage or staging
     evaluation before copying into an exact-shape/dtype destination tensor
     after success.
   - `TENSOR-060C` follow-up status: implemented 2026-04-12. Shape parsing now
     checks the signed-to-`usz` boundary through an unsigned comparison, and
     zero-size contracted axes realize to the additive identity.
   - The implementation keeps expression nodes internal to native `Tensor`
     payloads; no public `TensorExpr` type, macro rewrite, `map-into`, or
     backend-specific operation name was added.
   - Keep ordinary eager-source copy semantics as the fallback.
   - Aliasing policy: elementwise `map` may safely realize into an input
     tensor; `contract` rejects destinations that alias either source tensor.

9. `TENSOR-070` Broadcasting extension
   - Concrete trailing singleton rule for tensor-tensor `map`:
     - align shapes by trailing axes, treating missing leading axes as `1`;
     - axis pair is valid when equal or one side is `1`;
     - result axis is axis-wise max;
     - incompatible axes raise `tensor/shape-mismatch`.
   - Keep shape errors deterministic and payloaded.
   - Keep map dispatch backend-neutral: no backend-specific tensor types in
     public surface signatures.
   - Status: implemented 2026-04-12. Tensor-tensor `map` now right-aligns
     shapes, treats missing leading axes as `1`, expands singleton axes to the
     result max dimension, broadcasts rank-0 tensors as tensor scalars, and
     raises deterministic `tensor/shape-mismatch` errors for incompatible
     axes.

10. `TENSOR-080` Tensor library facade and backend boundary
   - Status: design/contract slice only; no backend runtime code lands here.
   - Decide the import/module path for the high-level tensor library.
   - Keep backend-specific names out of the public semantic surface.
   - Document that `contract` and `realize` are the semantic hooks that
     backend optimizations must preserve.
   - Define the optional backend capability table and fallback routing
     contract.
   - Split the backend boundary into a pure fallback facade plus optional
     `tensor/blas`, `tensor/lapack`, `tensor/cuda`, and `tensor/cublas`
     modules, without making those backends required for core correctness.
   - Record the no-implicit-transfer rule for CUDA-style movement and the
     explicit ownership/finalizer boundary for genuinely opaque native backend
     resources.
   - Acceptance criteria:
     - the pure fallback remains valid for every covered tensor operation,
     - backend selection is capability-driven and deterministic,
     - missing backend dependencies fall back without changing semantics,
     - native handles stay outside Omni `Value` ownership,
     - no public `TensorExpr`, backend-flavored math name, or implicit
       host/device transfer surface is introduced.
   - Status: closed 2026-04-12 as a design/contract slice.

11. `TENSOR-090` BLAS/LAPACK backend design and rank-2 contraction
    - Use FFI/native library paths only after the pure fallback behavior is
      stable.
    - Keep fallback available for validation and portability.
    - Treat BLAS-backed rank-2 contraction as an optimization of
      `(realize (contract a b [1 0]) out)`, not as a new canonical
      contraction operation.
    - Keep ordinary Tensor storage native/scoped; gate genuinely opaque native
      handles through explicit ownership and finalizer policy.

12. `TENSOR-100` CUDA/cuBLAS backend design
    - Require explicit host/device movement in the design.
    - Keep GPU storage behind `Tensor` rather than introducing a first-surface
      GPU tensor type.
    - Keep GPU-heavy validation on the bounded container path.
    - Treat cuBLAS handles as owned foreign handles with deterministic
      creation/destruction and no Omni value ownership.
    - Status: closed 2026-04-16. The canonical future device surface is
      `to-device`, `device`, and `tensor-backends`; `GpuTensor`,
      `CudaTensor`, backend-flavored math names, `tensor-use-backend!` as the
      first control surface, and implicit CPU/GPU transfer are rejected.
    - `TENSOR-100A` status: implemented 2026-04-16. `device` and `to-device`
      are registered for CPU-only placement semantics, with CUDA movement
      failing closed until a CUDA backend lands.
    - `TENSOR-100B` status: implemented 2026-04-16. `TensorVal` stores
      placement metadata and an opaque device handle/finalizer slot,
      `tensor-backends` reports structured CPU/CUDA availability, CPU kernels
      reject non-CPU storage, and fake-device destruction is covered by
      memory-lifetime tests. The next CUDA/cuBLAS slice is explicit
      CPU<->CUDA copy semantics for concrete `Float64` Tensor storage.
    - `TENSOR-100C` status: implemented 2026-04-16. `to-device 'cuda` copies
      concrete `Float64` CPU Tensor storage to CUDA when runtime-loaded
      libcudart is usable, `to-device 'cpu` copies CUDA storage back to native
      CPU storage, and CUDA availability requires symbol resolution, device
      count, and allocation/free probing. The next slice is cuBLAS handle
      lifecycle and CUDA-placed rank-2 `Float64` contract execution.
    - `TENSOR-100D` status: implemented 2026-04-16. Runtime-loaded cuBLAS
      handle lifecycle is per operation, `tensor-backends` reports `cublas`,
      CUDA-placed dense row-major `Float64` rank-2/rank-2 single-axis
      contractions execute through `cublasDgemm_v2` for `[1 0]`, `[0 0]`,
      `[1 1]`, and `[0 1]`, and rank-2/rank-1 plus rank-1/rank-2
      contractions execute through `cublasDgemv_v2`, returning CUDA-placed
      Tensor storage. Focused regressions cover available execution for all
      GEMM/GEMV layouts, unsupported CUDA contract diagnostics, zero-size
      fail-closed behavior, and forced cuBLAS unavailable reporting.
    - `TENSOR-100E` status: completed correctness-first baseline 2026-04-17.
      Vulkan backend probing is
      runtime-loaded and optional, `tensor-backends` reports `vulkan` with
      explicit `Float64` kernel capability, `to-device 'vulkan` copies
      concrete `Float64` CPU Tensor storage into opaque Vulkan buffers when
      available, `to-device 'cpu` copies Vulkan storage back to native CPU
      Tensor storage, `map` supports dense row-major Vulkan `Float64`
      arithmetic `+`, `-`, `*`, and `/` for Tensor/scalar, scalar/Tensor,
      exact-shape Tensor/Tensor inputs, and right-aligned singleton-axis
      Tensor/Tensor broadcasting, `matrix/transpose` supports Vulkan-placed
      dense row-major `Float64` rank-2 tensors, `matrix/diagonal` and
      `matrix/diagonal-matrix` support Vulkan-placed dense row-major `Float64`
      inputs, `matrix/trace` supports Vulkan-placed dense row-major square
      `Float64` inputs, `matrix/rank` supports Vulkan-placed dense row-major
      `Float64` rank-2 inputs, `matrix/determinant` supports Vulkan-placed
      dense row-major square `Float64` inputs, `matrix/solve` supports
      Vulkan-placed dense row-major square `Float64` coefficient tensors with
      Vulkan-placed rank-1 or rank-2 `Float64` right-hand tensors and routes
      larger systems through the first thresholded parallel solve helper,
      `matrix/inverse` supports Vulkan-placed dense row-major square
      `Float64` inputs, `matrix/cholesky` supports Vulkan-placed dense
      row-major square `Float64` inputs, `matrix/qr` supports Vulkan-placed
      dense row-major rank-2 `Float64` inputs with rows greater than or equal
      to columns, `matrix/norm` supports
      Vulkan-placed dense row-major
      `Float64` inputs for
      default/`'frobenius`, `'one`, `'infinity`, and `'max`, and public
      `contract` supports Vulkan-placed dense row-major
      rank-N `Float64` tensors with one or more
      explicit contracted axis pairs. Contract output axes are ordered as free
      left axes followed by free right axes. Results remain Vulkan-placed. The
      generic contract shader is generated from checked-in GLSL and uses
      rank/shape/stride/axis-list metadata-buffer dispatch.
      Unsupported Vulkan contract cases fail closed with backend diagnostics
      rather than silently copying to CPU; zero-size contracted axes in
      supported Vulkan layouts produce additive-identity output.
      The Vulkan dtype/layout policy is recorded in
      `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`: keep fixed-width
      dtype paths explicit, do not downcast between `Float64` and `Float32`,
      do not lower pointer-backed Big* Tensor dtypes to Vulkan, keep landed
      fixed-width complex backend support behind explicit capability bits, and
      defer full complex SVD factor output, CUDA complex singular-values/norm/SVD,
      complex eigen result contracts, and stride-aware layouts until their
      contracts exist.

13. `TENSOR-110` Example migration - complete
    - `examples/scicomp_demo.omni` now uses canonical `Tensor`, `map`,
      `contract`, and `realize`.
    - `vec-*`, `mat-*`, and `mat-mul` are removed from the canonical example
      surface.
    - Lazy `map`/`contract` return and closure-capture regressions now cover
      the cleanup lane.

14. `TENSOR-120` Autodiff design note
    - Design AD around differentiable tensor operations, not arbitrary Omni
      execution.
    - Treat `map` and `contract` as the first differentiable primitive family.

First implementation decisions:

- `Tensor` constructor accepts value-position dtype descriptors like `Float64`
  and `BigFloat` as the canonical dtype argument. Quoted dtype symbols such as
  `'Float64` and `'BigFloat` are accepted compatibility inputs, but docs should
  prefer value-position descriptors.
- Tensor `map` result dtype is the dtype of the first tensor input for the
  `Float64` kernel slice. Explicit conversion can be expressed as another
  mapped operation once non-Float64 kernels exist.
- Destination `realize` requires exact dtype compatibility in the first
  slice. Explicit conversion must happen before realization.
- `TENSOR-070` concrete singleton-axis broadcasting is implemented:
  right-align
  shapes, treat missing leading axes as `1`, require compatibility when either
  axis is `1` or equal, and set result axes with max. Exact-shape path stays the
  primary fast path.
- Map dispatch stays backend-neutral: only canonical `Tensor` surface types
  participate; no backend-specific tensor types or names are introduced.
- `length` for `Tensor` means total element count.

### Completion

Documentation gates:

- `docs/LANGUAGE_SPEC.md` documents:
  - `Tensor`,
  - `dtype`, `shape`, `rank`,
  - `length` as tensor element count,
  - tensor `ref`,
  - concrete tensor/scalar/expression `realize`,
  - lazy tensor `map` and `contract` expression payloads.
  - remaining gates for later slices: optional backend routing and diagnostic
    code refinements.
- `docs/type-system-syntax.md` documents how `Tensor` participates in type
  annotations and dispatch, and that `(Tensor data)` plus
  `(Tensor Float64 shape data-or-scalar)` are the constructor surfaces,
  including Tensor-specific `map`, `contract`, and `realize` behavior.
- `docs/SURFACE_COMPATIBILITY.md` records any scicomp prototype surface
  removal if old names are removed.
- `examples/scicomp_demo.omni` is updated to the canonical `Tensor`, `map`,
  `contract`, and `realize` surface.
- `memory/CHANGELOG.md` records behavior only after code lands and validation
  has concrete evidence.

Test gates:

- Constructor:
  - valid `Float64` tensor construction,
  - scalar-fill construction,
  - data length mismatch,
  - invalid shape,
  - shape product overflow,
  - dtype mismatch.
- Introspection:
  - `type-of`,
  - `tensor?`,
  - `dtype`,
  - `shape`,
  - `rank`,
  - `length`.
- Indexing:
  - `(ref tensor [i ...])`,
  - wrong rank index,
  - out-of-range index,
  - negative index policy, either accepted by explicit rule or rejected.
- `map`:
  - unary tensor,
  - tensor-scalar,
  - exact-shape tensor-tensor,
  - singleton-axis tensor-tensor expansion using trailing-axis alignment,
  - shape mismatch,
  - function result dtype mismatch.
- `contract`:
  - rank-2 contraction via `(contract a b [1 0])`,
  - rank-0 scalar result for vector dot product,
  - zero-axis outer product,
  - multi-axis contraction,
  - paired-axis array shorthand plus array and proper-list axis inputs,
  - negative axis normalization,
  - axis length mismatch,
  - axis out of range,
  - duplicate contracted axis,
  - contracted dimension mismatch,
  - non-tensor operands,
  - malformed axis containers,
  - non-integer axes.
- `realize`:
  - concrete tensor source without destination returns the already-concrete
    tensor,
  - allocate concrete tensor from `map`,
  - allocate concrete tensor from `contract`,
  - realize scalar into destination,
  - realize tensor into destination,
  - realize `map` into destination,
  - realize `contract` into destination,
  - destination shape mismatch,
  - dtype mismatch,
  - safe self-update,
  - unsafe alias handling.
- Memory/lifetime:
  - tensor returned across a function boundary,
  - tensor captured in closure env,
  - tensor realized across scope boundaries,
  - tensor destruction path under normal and ASAN builds,
  - no hidden per-type refcount ownership path.
- Execution parity:
  - interpreter behavior first,
  - JIT behavior after interpreter is green,
  - AOT/compiler e2e rows after primitive/codegen hooks exist.
- Optional backend parity, when those backends land:
  - BLAS rank-2 contraction matches pure fallback,
  - LAPACK library routines document their pure fallback or dependency policy,
  - CUDA/cuBLAS paths require explicit device movement,
  - disabled or missing backend dependencies fail or fall back deterministically.

Validation guidance:

- Run targeted tensor tests after each slice.
- Run `c3c build` for integration safety after code slices.
- Run `c3c build --sanitize=address` for tensor storage/lifetime slices.
- Use bounded container validation for broad or high-memory tensor test slices.
- Keep BLAS/GPU/heavy scientific benchmarks container-bound.

Latest targeted validation for the tensor boundary cleanup audit:

- `c3c build --warn-deprecation=no`
- Host targeted `advanced-collections-module` group: `pass=212 fail=0`
- Bounded container `memory-lifetime-smoke` slice: `pass=222 fail=0`

## Deferred Future Work

- `Matrix` rank-2 convenience constructor or alias, only if owner explicitly
  wants it after `Tensor` is stable.
- `dot` and `outer` as library conveniences over `contract`.
- `trace`, `transpose`, `permute`, `reshape`, `sum`, `mean`, and axis
  reductions.
- Slicing and views with explicit copy/view mutation policy.
- `Complex` dtype.
- fixed-width numeric dtypes.
- BLAS/LAPACK backend modules and kernel selection.
- CUDA/cuBLAS storage/backend modules with explicit host/device movement.
- autodiff over differentiable tensor primitives.

## First User-Facing Example Target

```lisp
(define a
  (Tensor Float64 [2 3]
    [1.0 2.0 3.0
     4.0 5.0 6.0]))

(define b
  (Tensor Float64 [3 2]
    [7.0  8.0
     9.0  10.0
     11.0 12.0]))

(define c (Tensor Float64 [2 2] 0.0))

(realize (contract a b [1 0]) c)

(define shifted (Tensor (map + c 1.0)))
(define squared (Tensor (map * shifted shifted)))
(define from-iterator (Tensor (map (lambda (x) (+ x 1)) (Iterator [1 2 3]))))
```

This is the intended mental model:

- `Tensor` is the object.
- `map` is elementwise/broadcast computation.
- `contract` is tensor algebra.
- Constructors are the public materialization boundary.
- `realize` is a low-level destination-storage operation with optional
  destination reuse, not a general force/delay counterpart.
