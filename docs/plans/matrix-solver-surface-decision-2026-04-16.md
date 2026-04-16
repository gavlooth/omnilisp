# Matrix Solver Surface Decision (2026-04-16)

Status: current decision for Tensor matrix solver and decomposition
conveniences.

## Decision

Use `matrix/` as the public namespace for rank-2 Tensor structural, solver,
and decomposition conveniences.

The first structural matrix transform is:

```lisp
(matrix/transpose coefficients)
(matrix/diagonal coefficients)
(matrix/diagonal-matrix diagonal)
(matrix/identity size [dtype])
```

Contract:

- `matrix/transpose` accepts rank-2 Tensor inputs and returns shape
  `[columns rows]`.
- `matrix/diagonal` accepts rank-2 Tensor inputs and returns a rank-1 Tensor
  with length `min(rows, columns)`.
- `matrix/diagonal-matrix` accepts rank-1 Tensor inputs and returns a square
  rank-2 Tensor with shape `[n n]`.
- `matrix/identity` accepts a non-negative integer size and optional dtype,
  defaulting to `Float64`, and returns a square rank-2 Tensor with shape
  `[size size]`.
- Lazy Tensor operands are realized before structural transformation.
- Native Tensor dtype is preserved for `Float64`, `BigInteger`, `BigFloat`,
  and `BigComplex`.
- Owned high-precision element handles are cloned into the result, and
  `matrix/diagonal-matrix` and `matrix/identity` fill off-diagonal cells with
  the dtype's zero value.
- The canonical public name is `matrix/transpose`; do not add abbreviated
  aliases unless the owner explicitly approves them.
- The canonical public name is `matrix/diagonal`; do not add abbreviated
  aliases unless the owner explicitly approves them.
- The canonical public name is `matrix/diagonal-matrix`; do not add abbreviated
  aliases unless the owner explicitly approves them.
- The canonical public name is `matrix/identity`; do not add abbreviated
  aliases unless the owner explicitly approves them.

The first structural matrix reduction is:

```lisp
(matrix/trace coefficients)
(matrix/rank coefficients [tolerance])
(matrix/norm coefficients [selector])
```

Contract:

- `matrix/trace` requires `coefficients` to be a square rank-2 Tensor.
- `matrix/trace` realizes lazy Tensor operands before summing the diagonal.
- `matrix/trace` returns a scalar in the Tensor's native numeric family:
  `Double` for `Float64`, or `BigInteger`, `BigFloat`, and `BigComplex` for
  matching high-precision Tensor dtypes.
- `matrix/trace` returns the dtype's zero scalar for empty square matrices.
- `matrix/rank` accepts rectangular rank-2 `Float64` Tensor inputs and returns
  the numerical rank as an `Integer`.
- `matrix/rank` realizes lazy Tensor operands before rank counting, supports
  empty axes, uses default tolerance `1e-12`, and accepts an optional
  non-negative finite numeric tolerance.
- `matrix/rank` may use optional runtime-loaded `LAPACKE_dgesvd` acceleration
  to count singular values above tolerance, but the pure row-echelon rank
  counter remains the fallback.
- `matrix/norm` accepts rectangular rank-2 `Float64` Tensor inputs and returns
  a `Float64` scalar norm.
- `matrix/norm` realizes lazy Tensor operands before norm reduction, supports
  empty axes as `0.0`, defaults to Frobenius, and accepts explicit
  `'frobenius`, `'one`, `'infinity`, `'max`, `'spectral`, and `'nuclear`
  selectors.
- The canonical public name is `matrix/trace`; do not add abbreviated aliases
  unless the owner explicitly approves them.
- The canonical public name is `matrix/rank`; do not add abbreviated aliases
  unless the owner explicitly approves them.
- The canonical public name is `matrix/norm`; do not add abbreviated aliases
  or selector aliases such as `'inf` or `'two`.

The first shipped solver surface is:

```lisp
(matrix/solve coefficients rhs)
```

Contract:

- `coefficients` must be a square rank-2 `Float64` Tensor.
- `rhs` must be a rank-1 `Float64` Tensor with length `n` or a rank-2
  `Float64` Tensor with leading dimension `n`.
- The result preserves the RHS rank: rank-1 RHS returns shape `[n]`; rank-2
  RHS returns shape `[n k]`.
- Lazy Tensor operands are realized before solving.
- Singular systems raise `tensor/singular-matrix`.
- LAPACK/LAPACKE may accelerate this operation when `LAPACKE_dgesv` is
  available at runtime, but the pure runtime solver is the semantic oracle.

Parallel solver follow-up:

- Future parallel solvers must stay behind the same `matrix/solve`,
  `matrix/lu`, `matrix/inverse`, and related `matrix/*` public surfaces.
- Do not add `parallel/solve`, `vulkan/solve`, `tensor/vulkan/solve`, or other
  backend-flavored public solver names.
- Backend execution remains an implementation policy: CPU pure/LAPACK paths
  are the semantic oracle, Vulkan/CUDA/etc. may execute only when operands are
  explicitly placed on that backend, and unsupported backend cases must fail
  closed instead of silently transferring to CPU.
- Deterministic singular/error behavior and RHS-rank-preserving result shapes
  are part of the public contract regardless of whether the backend uses a
  serial or parallel algorithm.

The first shipped decomposition surface is:

```lisp
(matrix/lu coefficients)
```

Contract:

- `coefficients` must be a square rank-2 `Float64` Tensor.
- Lazy Tensor operands are realized before factorization.
- The result is a dictionary with:
  - `lu`: a square `Float64` Tensor containing L below the diagonal with an
    implicit unit diagonal and U on/above the diagonal.
  - `pivots`: an array containing the final 0-based row order after partial
    pivoting.
  - `swap-count`: the number of row swaps, suitable for determinant parity.
- Singular inputs raise `tensor/singular-matrix`.
- LAPACK/LAPACKE may accelerate this operation when `LAPACKE_dgetrf` is
  available at runtime, but the pure runtime LU factorization is the semantic
  oracle and the public dictionary contract is unchanged.

The first shipped decomposition consumer is:

```lisp
(matrix/determinant coefficients)
```

Contract:

- `coefficients` must be a square rank-2 `Float64` Tensor.
- Lazy Tensor operands are realized before determinant computation.
- The result is a `Float64` scalar value.
- Partial-pivot LU semantics define the determinant value and row-swap parity.
- Singular matrices return `0.0`.
- LAPACK/LAPACKE may accelerate the underlying factorization when
  `LAPACKE_dgetrf` is available at runtime, but the pure runtime LU
  factorization is the semantic oracle.
- The canonical public name is `matrix/determinant`; do not add an abbreviated
  `matrix/det` alias unless the owner explicitly approves it.

The first solve-derived matrix transform is:

```lisp
(matrix/inverse coefficients)
```

Contract:

- `coefficients` must be a square rank-2 `Float64` Tensor.
- Lazy Tensor operands are realized before inversion.
- The result is a same-shape `Float64` Tensor inverse.
- Singular matrices raise `tensor/singular-matrix`.
- LAPACK/LAPACKE may accelerate the identity solve when `LAPACKE_dgesv` is
  available at runtime, but the pure runtime Gaussian solve is the semantic
  oracle.
- The canonical public name is `matrix/inverse`; do not add abbreviated
  aliases unless the owner explicitly approves them.

The first non-LU decomposition surface is:

```lisp
(matrix/qr coefficients)
```

Contract:

- `coefficients` must be a rank-2 `Float64` Tensor with rows greater than or
  equal to columns.
- Lazy Tensor operands are realized before factorization.
- The result is a dictionary with:
  - `q`: a `Float64` Tensor with shape `[rows columns]`.
  - `r`: a `Float64` Tensor with shape `[columns columns]`.
- This is a reduced QR decomposition for full-column-rank inputs.
- Rank-deficient inputs raise `tensor/singular-matrix`.
- LAPACK/LAPACKE may accelerate this operation when both `LAPACKE_dgeqrf` and
  `LAPACKE_dorgqr` are available at runtime. The backend forms reduced `q`
  and `r` and sign-normalizes factors to preserve the existing
  positive-diagonal orientation; the pure runtime QR factorization is the
  semantic oracle.
- The canonical public name is `matrix/qr`; do not add bare `qr` or
  backend-flavored names.

The first symmetric positive-definite decomposition surface is:

```lisp
(matrix/cholesky coefficients)
```

Contract:

- `coefficients` must be a square symmetric positive-definite rank-2
  `Float64` Tensor.
- Lazy Tensor operands are realized before factorization.
- The result is a lower-triangular `Float64` Tensor with the same shape as the
  input. Entries above the diagonal are zero.
- Nonsymmetric or non-positive-definite inputs raise
  `tensor/not-positive-definite`.
- LAPACK/LAPACKE may accelerate this operation when `LAPACKE_dpotrf` is
  available at runtime. The runtime still exact-checks symmetry before calling
  LAPACK, and the pure runtime Cholesky factorization is the semantic oracle.
- The canonical public name is `matrix/cholesky`; do not add bare `cholesky`
  or backend-flavored names.

The first rectangular decomposition surface is:

```lisp
(matrix/singular-values coefficients)
(matrix/svd coefficients)
```

Contract:

- `coefficients` must be a rank-2 `Float64` Tensor.
- Lazy Tensor operands are realized before factorization.
- `matrix/singular-values` returns a rank-1 `Float64` Tensor with shape
  `[k]`, where `k = min(rows, columns)`.
- Singular values are sorted descending.
- The result is a dictionary with:
  - `u`: a `Float64` Tensor with shape `[rows k]`.
  - `s`: a `Float64` Tensor with shape `[k]`, sorted descending.
  - `v`: a `Float64` Tensor with shape `[columns k]`.
- `k = min(rows, columns)`.
- The decomposition is reduced: the input reconstructs as
  `u * diag(s) * transpose(v)`.
- Rank-deficient inputs are supported; zero singular values remain in `s`.
- LAPACK/LAPACKE may accelerate this operation when `LAPACKE_dgesvd` is
  available at runtime. The backend must preserve the reduced `u`/`s`/`v`
  shape contract and factor orientation for `matrix/svd`; the pure runtime
  Gram/Jacobi SVD is the semantic oracle.
- The canonical public name is `matrix/singular-values`; do not add
  abbreviated aliases such as `matrix/svals` unless the owner explicitly
  approves them.
- The canonical public name is `matrix/svd`; do not add bare `svd` or
  backend-flavored names.

The first eigen surfaces include symmetric-real and general nonsymmetric paths:

```lisp
(matrix/eigenvalues coefficients)
(matrix/eigenvectors coefficients)
(matrix/eigenpairs coefficients)
```

Contract:

- `coefficients` must be a square symmetric rank-2 `Float64` Tensor.
- Lazy Tensor operands are realized before factorization.
- `matrix/eigenvalues` returns a `Float64` Tensor with shape `[n]`, sorted
  descending.
- `matrix/eigenvectors` returns a dictionary with:
  - `values`: a `Float64` Tensor with shape `[n]`, sorted descending.
  - `vectors`: a `Float64` Tensor with shape `[n n]`; each column is the
    normalized eigenvector aligned with the same-position value.
- Nonsymmetric inputs raise `tensor/not-symmetric`.
- LAPACK/LAPACKE may accelerate these operations when `LAPACKE_dsyev` is
  available at runtime. The runtime still exact-checks symmetry before calling
  LAPACK, sorts eigenpairs descending, normalizes eigenvector signs for stable
  output, and keeps the pure symmetric Jacobi eigensolver as the semantic
  oracle.
- General nonsymmetric real eigenpairs, including complex-valued output, are
  represented by `matrix/eigenpairs`.
- The canonical public names are `matrix/eigenvalues` and
  `matrix/eigenvectors`; do not add bare `eigenvalues`, bare `eigenvectors`,
  or backend-flavored names.

General eigenpair contract:

- `coefficients` must be a square rank-2 `Float64` Tensor.
- Lazy Tensor operands are realized before factorization.
- `matrix/eigenpairs` returns a dictionary with:
  - `values`: a `BigComplex` Tensor with shape `[n]`.
  - `vectors`: a `BigComplex` Tensor with shape `[n n]`; each column is the
    normalized right eigenvector aligned with the same-position value.
- Real and complex eigenvalues use the same `BigComplex` output type.
- Eigenpairs are sorted by descending eigenvalue magnitude with deterministic
  real/imaginary tie-breakers.
- Empty square input returns empty `[0]` values and `[0 0]` vectors.
- Runtime `LAPACKE_dgeev` may accelerate this operation when available. Missing
  backend support retains the pure QR/nullspace fallback under the same public
  contract.
- The canonical public name is `matrix/eigenpairs`; do not add bare
  `eigenpairs` or backend-flavored names.

## Candidate Names

- `matrix/solve`: chosen. It is explicit about the rank-2 matrix contract,
  avoids a bare global `solve`, and does not expose backend ownership.
- `linear/solve`: rejected for the first slice. It is reasonable but broader
  than the current rank-2 Tensor contract.
- `linalg/solve`: deferred/rejected for now. `linalg` is abbreviated and was
  not accepted as the base namespace.
- `tensor/solve`: rejected. It implies a rank-polymorphic Tensor operation,
  which is misleading for matrix system solving.
- `tensor/lapack/solve`: rejected for the normal public surface. Backend names
  describe implementation ownership, not user-facing math.
- bare `solve`: rejected by prior owner direction.

## Future Names

Use the same `matrix/` namespace for rank-2 decomposition conveniences unless
the owner explicitly changes the public library namespace:

- `matrix/lu`: shipped as the first decomposition surface.
- `matrix/determinant`: shipped as the first decomposition consumer.
- `matrix/transpose`: shipped as the first structural matrix transform.
- `matrix/diagonal`: shipped as the second structural matrix transform.
- `matrix/diagonal-matrix`: shipped as the first structural matrix constructor.
- `matrix/identity`: shipped as the first size-driven structural matrix
  constructor.
- `matrix/trace`: shipped as the first structural matrix reduction.
- `matrix/rank`: shipped as the first numerical structural matrix reduction.
- `matrix/norm`: shipped as the first general matrix norm reducer.
- `matrix/inverse`: shipped as the first solve-derived matrix transform.
- `matrix/qr`: shipped as the first non-LU decomposition surface.
- `matrix/cholesky`: shipped as the first symmetric positive-definite
  decomposition surface.
- `matrix/singular-values`: shipped as the direct singular-value extraction
  surface.
- `matrix/svd`: shipped as the first rectangular decomposition surface.
- `matrix/eigenvalues`: shipped for square symmetric `Float64` matrices.
- `matrix/eigenvectors`: shipped for square symmetric `Float64` matrices,
  returning aligned values and vector columns.
- `matrix/eigenpairs`: shipped for general square `Float64` matrices,
  returning aligned `BigComplex` values and vector columns.

Conveniences should return explicit structured values when they expose multiple
outputs; do not introduce ambiguous tuple-like payloads as part of this naming
decision.
