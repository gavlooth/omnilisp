# tensor-scientific-computing-plan-2026-04-11 Part 01

Source: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`

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
