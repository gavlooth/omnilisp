# Memory Changelog Index Part 06

Source: `memory/CHANGELOG.md`

  - Both the accelerated `LAPACKE_dgeev` path and forced pure fallback now
    validate the middle returned vector column satisfies `A*v ~= lambda*v`.
  - This exercises a non-basis eigenvector alignment case beyond diagonal and
    simple rotation examples.
  - validation:
    - direct backend and forced-fallback non-normal residual smokes -> `true`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `578 passed, 0 failed`

- Completed `TENSOR-090AE` accelerated eigenpairs residual coverage:
  - Added focused `LAPACKE_dgeev`-path residual checks for
    `matrix/eigenpairs` when the runtime backend is available.
  - The accelerated path now validates representative real and complex vector
    columns satisfy `A*v ~= lambda*v`, complementing the pure fallback residual
    checks from `TENSOR-090AD`.
  - validation:
    - direct backend residual smokes for real diagonal and complex rotation
      matrices -> `true`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `576 passed, 0 failed`

- Completed `TENSOR-090AD` broader pure eigenpairs fallback coverage:
  - Added focused regressions for forced no-`dgeev` fallback on:
    - 3x3 diagonal matrices with three real eigenvalues.
    - 3x3 upper-triangular matrices where eigenvalues are the diagonal.
    - 3x3 real matrices containing a 2x2 complex-conjugate block plus one
      real eigenvalue.
  - These tests harden the `TENSOR-090AC` pure QR/nullspace fallback beyond
    the initial 2x2 real/complex cases while preserving the same public
    `matrix/eigenpairs` contract.
  - Added forced-fallback residual checks that validate representative
    returned vector columns satisfy `A*v ~= lambda*v`.
  - validation:
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `574 passed, 0 failed`

- Completed `TENSOR-090AC` pure fallback for general matrix eigenpairs:
  - `matrix/eigenpairs` now treats runtime `LAPACKE_dgeev` as optional
    acceleration rather than a hard dependency.
  - When `dgeev` is unavailable or explicitly disabled for validation, the
    runtime uses a pure QR eigenvalue pass plus complex nullspace solving for
    aligned right eigenvectors.
  - The fallback preserves the public contract from `TENSOR-090AB`:
    `BigComplex` `values` shape `[n]`, `BigComplex` `vectors` shape `[n n]`,
    deterministic value sorting, and aligned vector columns.
  - Added a test-only `dgeev` disable hook and
    `OMNI_TENSOR_DISABLE_LAPACK_DGEEV` CLI validation switch.
  - Updated docs, TODO, and agent handoff artifacts to remove the
    backend-required status.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `568 passed, 0 failed`
    - forced no-`dgeev` complex eigenvalue smoke -> `"0+1i"`
    - forced no-`dgeev` vector dtype smoke -> `"BigComplex"`
    - forced no-`dgeev` real diagonal smoke -> `true`

- Completed `TENSOR-090AB` general matrix eigenpairs surface:
  - Added public `matrix/eigenpairs` for square rank-2 `Float64` Tensor inputs.
  - The implementation realizes lazy inputs, calls runtime-loaded
    `LAPACKE_dgeev`, and returns a dictionary with:
    - `values`: a rank-1 `BigComplex` Tensor with shape `[n]`.
    - `vectors`: a rank-2 `BigComplex` Tensor with shape `[n n]`; vector
      columns align with the same-position eigenvalue.
  - Real and complex eigenvalues share the same `BigComplex` output type.
  - Eigenpairs are sorted by descending eigenvalue magnitude with deterministic
    real/imaginary tie-breakers.
  - Right eigenvector columns are phase-normalized for stable display.
  - Empty square matrices return empty `[0]` values and `[0 0]` vectors without
    requiring a backend call.
  - Hosts without `LAPACKE_dgeev` now retain the pure fallback shipped in
    `TENSOR-090AC`.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Added focused advanced collection/module coverage for complex eigenvalues,
    real diagonal eigenpairs, vector dtype/shape, lazy input realization, empty
    shape, backend call-count coverage, rank validation, and dtype validation.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, matrix surface decision note, plan index, live TODO, and
    agent handoff plan/report.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `564 passed, 0 failed`
    - direct complex eigenvalue smoke -> `"0+1i"`
    - direct vector dtype smoke -> `"BigComplex"`
    - direct empty-vector-shape smoke -> `0`
    - direct dtype validation smoke ->
      `matrix/eigenpairs: expected a square rank-2 Float64 Tensor`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090AA` matrix singular-values surface:
  - Added public `matrix/singular-values` for rank-2 `Float64` Tensor inputs.
  - The implementation realizes lazy inputs, computes the existing reduced SVD
    workspaces, and returns the descending singular values as a rank-1
    `Float64` Tensor with shape `[min(rows, columns)]`.
  - Rectangular and rank-deficient matrices are supported.
  - Empty axes return an empty rank-1 Tensor.
  - Optional runtime-loaded `LAPACKE_dgesvd` acceleration is reused when
    available; missing backend support retains the pure Gram/Jacobi fallback.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Added focused backend-path coverage that asserts the `dgesvd` call counter
    advances when `liblapacke` is available and otherwise accepts fallback
    retention.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, matrix surface decision note, plan index, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `550 passed, 0 failed`
    - direct leading singular value smoke -> `3.0`
    - direct wide-shape smoke -> `2`
    - direct empty-shape smoke -> `0`
    - direct lazy-input smoke -> `2.0`
    - direct dtype validation smoke ->
      `matrix/singular-values: expected a rank-2 Float64 Tensor`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090Z` matrix rank LAPACK coverage:
  - Routed public `matrix/rank` through optional runtime-loaded
    `LAPACKE_dgesvd` when available.
  - The backend path copies the input, computes singular values, and counts
    values above the caller/default tolerance.
  - The pure partial-pivot row-echelon rank counter remains the fallback when
    LAPACK is unavailable, rejects the input, or fails to converge.
  - The public `matrix/rank` contract is unchanged: rectangular rank-2
    `Float64` input, lazy realization, optional non-negative finite tolerance,
    empty axes returning `0`, and `Integer` result.
  - Added focused backend-path coverage that asserts the `dgesvd` call counter
    advances when `liblapacke` is available and otherwise accepts fallback
    retention.
  - Updated Tensor scientific plan, Tensor area page, matrix surface decision
    note, plan index, and agent handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `540 passed, 0 failed`

- Completed `TENSOR-090Y` matrix rank surface:
  - Added public `matrix/rank` for rectangular rank-2 `Float64` Tensor inputs.
  - The implementation realizes lazy inputs and computes numerical rank with
    partial-pivot row-echelon elimination over a copied scratch buffer.
  - The default tolerance is `1e-12`.
  - Optional tolerance selection accepts any numeric value that can narrow to a
    non-negative finite `Float64`.
  - Empty axes return rank `0`.
  - The result is an `Integer`.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, matrix surface decision note, plan index, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `538 passed, 0 failed`
    - direct `matrix/rank` full rectangular smoke -> `2`
    - direct `matrix/rank` deficient rectangular smoke -> `1`
    - direct `matrix/rank` empty-axis smoke -> `0`
    - direct tolerance-suppressed small pivot smoke -> `1`
    - direct zero-tolerance small pivot smoke -> `2`
    - direct lazy-input smoke -> `2`
    - direct dtype validation smoke ->
      `matrix/rank: expected a rank-2 Float64 Tensor`
    - direct negative-tolerance validation smoke ->
      `matrix/rank: tolerance must be a non-negative finite number`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090X` matrix identity surface:
  - Added public `matrix/identity` for non-negative integer sizes.
  - The implementation builds a square rank-2 Tensor with shape `[n n]`.
  - The default dtype is `Float64`.
  - Optional dtype selection supports `Float64`, `BigInteger`, `BigFloat`, and
    `BigComplex`, matching the existing Tensor dtype parser.
  - Diagonal cells are filled with the dtype's one value; off-diagonal cells
    are filled with the dtype's zero value.
  - Empty size `0` returns an empty square Tensor with shape `[0 0]`.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, matrix surface decision note, plan index, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `529 passed, 0 failed`
    - direct `matrix/identity` default Float64 diagonal smoke -> `1.0`
    - direct `matrix/identity` default Float64 off-diagonal smoke -> `0.0`
    - direct `matrix/identity` empty shape smoke -> `[0 0]`
    - direct `matrix/identity` BigInteger one smoke -> `"1"`
    - direct `matrix/identity` BigInteger zero smoke -> `"0"`
    - direct `matrix/identity` BigFloat one smoke -> `"1"`
    - direct `matrix/identity` BigComplex one smoke -> `"1+0i"`
    - direct dtype preservation smoke -> `"BigComplex"`
    - direct negative-size validation smoke ->
      `matrix/identity: size must be a non-negative Integer`
    - direct invalid-dtype validation smoke ->
      `matrix/identity: dtype must be Float64, BigInteger, BigFloat, or BigComplex`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090W` matrix diagonal-matrix surface:
  - Added public `matrix/diagonal-matrix` for rank-1 Tensor inputs.
  - The implementation realizes lazy inputs and builds a square rank-2 Tensor
    with shape `[n n]`.
  - Native dtype is preserved for `Float64`, `BigInteger`, `BigFloat`, and
    `BigComplex`.
  - High-precision diagonal element handles are cloned into the result instead
    of being shallow-copied.
  - Off-diagonal cells are filled with the dtype's zero value.
  - Empty rank-1 inputs return an empty square Tensor with shape `[0 0]`.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, matrix surface decision note, plan index, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `517 passed, 0 failed`
    - direct `matrix/diagonal-matrix` Float64 diagonal smoke -> `3.0`
    - direct `matrix/diagonal-matrix` Float64 off-diagonal smoke -> `0.0`
    - direct `matrix/diagonal-matrix` empty shape smoke -> `[0 0]`
    - direct `matrix/diagonal-matrix` BigInteger clone smoke ->
      `"9223372036854775808"`
    - direct `matrix/diagonal-matrix` BigInteger off-diagonal zero smoke ->
      `"0"`
    - direct `matrix/diagonal-matrix` BigFloat clone smoke -> `"2.5"`
    - direct `matrix/diagonal-matrix` BigComplex clone smoke -> `"3+4i"`
    - direct dtype preservation smoke -> `"BigComplex"`
    - direct lazy-input smoke -> `2.0`
    - direct rank validation smoke ->
      `matrix/diagonal-matrix: expected a rank-1 Tensor`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090V` matrix diagonal surface:
  - Added public `matrix/diagonal` for rank-2 Tensor inputs.
  - The implementation realizes lazy inputs and extracts the main diagonal
    into a rank-1 Tensor with length `min(rows, columns)`.
  - Native dtype is preserved for `Float64`, `BigInteger`, `BigFloat`, and
    `BigComplex`.
  - High-precision Tensor element handles are cloned into the diagonal result
    instead of being shallow-copied, preserving Tensor ownership invariants.
  - Empty rectangular inputs return an empty rank-1 Tensor with shape `[0]`.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, matrix surface decision note, plan index, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `505 passed, 0 failed`
    - direct `matrix/diagonal` rectangular Float64 smoke -> `5.0`
    - direct `matrix/diagonal` empty shape smoke -> `[0]`
    - direct `matrix/diagonal` BigInteger clone smoke ->
      `"9223372036854775808"`
    - direct `matrix/diagonal` BigFloat clone smoke -> `"2.5"`
    - direct `matrix/diagonal` BigComplex clone smoke -> `"3+4i"`
    - direct dtype preservation smoke -> `"BigComplex"`
    - direct lazy-input smoke -> `4.0`
    - direct rank validation smoke ->
      `matrix/diagonal: expected a rank-2 Tensor`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090U` matrix trace surface:
  - Added public `matrix/trace` for square rank-2 Tensor inputs.
  - The implementation realizes lazy inputs and sums the diagonal.
  - The result is a scalar in the Tensor's native numeric family: `Double`
    for `Float64`, or `BigInteger`, `BigFloat`, and `BigComplex` for matching
    high-precision Tensor dtypes.
  - Empty square matrices return the dtype's zero scalar.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, matrix surface decision note, plan index, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `496 passed, 0 failed`
    - direct `matrix/trace` Float64 smoke -> `5.0`
    - direct `matrix/trace` empty square smoke -> `0.0`
    - direct `matrix/trace` BigInteger smoke -> `"9223372036854775812"`
    - direct `matrix/trace` BigFloat smoke -> `"3.75"`
    - direct `matrix/trace` BigComplex smoke -> `"4+6i"`
    - direct lazy-input smoke -> `5.0`
    - direct rectangular validation smoke ->
      `matrix/trace: expected a square rank-2 Tensor`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090T` matrix transpose surface:
  - Added public `matrix/transpose` for rank-2 Tensor inputs.
  - The implementation realizes lazy inputs and returns a Tensor with shape
    `[columns rows]`.
  - Native dtype is preserved for `Float64`, `BigInteger`, `BigFloat`, and
    `BigComplex`.
  - High-precision Tensor element handles are cloned into the transposed result
    instead of being shallow-copied, preserving Tensor ownership invariants.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, matrix surface decision note, plan index, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `488 passed, 0 failed`
    - direct `matrix/transpose` Float64 value smoke -> `6.0`
    - direct `matrix/transpose` shape smoke -> `[3 2]`
    - direct `matrix/transpose` BigInteger clone smoke ->
      `"9223372036854775808"`
    - direct `matrix/transpose` BigFloat clone smoke -> `"2.5"`
    - direct `matrix/transpose` BigComplex clone smoke -> `"3+4i"`
    - direct rank validation smoke ->
      `matrix/transpose: expected a rank-2 Tensor`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090S` matrix inverse surface:
  - Added public `matrix/inverse` for nonsingular square rank-2 `Float64`
    Tensor inputs.
  - The implementation realizes lazy inputs, solves against an identity RHS,
    and returns a same-shape `Float64` Tensor inverse.
  - `matrix/inverse` uses optional runtime-loaded `LAPACKE_dgesv` acceleration
    when available, while the pure Gaussian solve remains the semantic oracle.
  - Singular inputs raise `tensor/singular-matrix`; non-square and non-Float64
    Tensor inputs raise the existing Tensor shape/type diagnostics.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, matrix surface decision note, plan index, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `479 passed, 0 failed`
    - direct `matrix/inverse` first-entry smoke -> `0.6`
    - direct `matrix/inverse` off-diagonal smoke -> `-0.7`
    - direct inverse product identity smoke -> `1.0`
    - direct singular inverse handler smoke ->
      `matrix/inverse: input matrix is singular`
    - direct empty square inverse shape smoke -> `0`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090R` optional LAPACK SVD acceleration:
  - Installed `liblapacke-dev` on the host so runtime-loaded LAPACKE paths can
    be validated live instead of only through fallback retention.
  - Extended the private runtime-loaded LAPACK helper to resolve
    `LAPACKE_dgesvd`, with separate availability and call-count probes.
  - Routed `matrix/svd` through the LAPACK helper when `liblapacke` provides
    `LAPACKE_dgesvd`.
  - Preserved the existing reduced SVD public contract: `u` shape `[rows k]`,
    `s` shape `[k]`, `v` shape `[columns k]`, descending singular values, and
    rank-deficient inputs supported with zero singular values.
  - The backend uses reduced `jobu='S'`/`jobvt='S'`, transposes LAPACK's `vt`
    into Omni's `v` column-factor layout, and normalizes signs for stable
    factor output.
  - Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or
    unavailable backend paths keep the pure C3 Gram/Jacobi SVD fallback as the
    semantic oracle.
  - Live LAPACKE validation also exposed a previous QR backend bug: exact-zero
    diagonal rank checks are too weak after `LAPACKE_dgeqrf`; the QR helper now
    rejects diagonals with absolute value at or below `1e-12`.
  - validation:
    - `sudo apt-get update && sudo apt-get install -y liblapacke-dev`
    - `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `469 passed, 0 failed`
    - direct `matrix/svd` leading singular value smoke -> `3.0`
    - direct `matrix/svd` U-column smoke -> `1.0`
    - direct `matrix/svd` V-column smoke -> `1.0`
    - direct wide `matrix/svd` V-column smoke -> `1.0`
    - direct rank-deficient `matrix/svd` singular value smoke -> `0.0`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090Q` optional LAPACK symmetric eigen acceleration:
  - Extended the private runtime-loaded LAPACK helper to resolve
    `LAPACKE_dsyev`, with separate availability and call-count probes.
  - Routed `matrix/eigenvalues` and `matrix/eigenvectors` through the LAPACK
    helper when `liblapacke` provides `LAPACKE_dsyev`.
  - Preserved the existing symmetric-real public contract: exact symmetry is
    checked before the backend call, eigenvalues are descending, eigenvector
    columns align with values, and nonsymmetric inputs raise
    `tensor/not-symmetric`.
  - The backend sorts LAPACK's ascending eigenpairs into descending order,
    normalizes eigenvector signs for stable output, and maps LAPACK
    non-convergence to the existing `tensor/no-convergence` failure path.
  - Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or
    unavailable backend paths keep the pure C3 symmetric Jacobi fallback as the
    semantic oracle.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - direct `matrix/eigenvalues` diagonal smoke -> `3.0`
    - direct `matrix/eigenvalues` symmetric off-diagonal smoke -> `3.0`
    - direct `matrix/eigenvectors` aligned-value smoke -> `3.0`
    - direct `matrix/eigenvectors` vector-column smoke -> `1.0`
    - direct nonsymmetric eigen handler smoke ->
      `matrix/eigenvalues: input matrix is not symmetric`
    - focused advanced collections/module group on host
      -> `467 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`
    - local `ldconfig -p` showed `liblapack.so.3` and BLAS libraries but no
      `liblapacke`, so the focused LAPACK path regressions validated fallback
      retention on this host.

- Completed `TENSOR-090P` optional LAPACK QR acceleration:
  - Extended the private runtime-loaded LAPACK helper to resolve
    `LAPACKE_dgeqrf` and `LAPACKE_dorgqr` as a paired QR backend capability,
    with separate availability and call-count probes.
  - Routed `matrix/qr` through the LAPACK helper when `liblapacke` provides
    both symbols.
  - Preserved the existing reduced QR public contract: `q` shape
    `[rows columns]`, `r` shape `[columns columns]`, full-column-rank inputs
    only, and `tensor/singular-matrix` for rank-deficient inputs.
  - The backend extracts reduced `R`, forms reduced `Q`, and sign-normalizes
    factor columns so the existing positive-diagonal orientation is preserved.
  - Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or
    unavailable backend paths keep the pure C3 QR fallback as the semantic
    oracle.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - direct `matrix/qr` Q first-column smoke -> `1.0`
    - direct `matrix/qr` Q second-column smoke -> `1.0`
    - direct `matrix/qr` R projection smoke -> `1.0`
    - direct `matrix/qr` shape smoke -> `3`
    - direct rank-deficient QR handler smoke ->
      `matrix/qr: input matrix is rank deficient`
    - focused advanced collections/module group on host
      -> `463 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`
    - local `ldconfig -p` showed `liblapack.so.3` and BLAS libraries but no
      `liblapacke`, so the focused LAPACK path regression validated fallback
      retention on this host.

- Completed `TENSOR-090O` optional LAPACK `dpotrf` Cholesky acceleration:
  - Extended the private runtime-loaded LAPACK helper to resolve
    `LAPACKE_dpotrf`, with separate availability and call-count probes.
  - Routed `matrix/cholesky` through the LAPACK helper when `liblapacke` and
    `LAPACKE_dpotrf` are available.
  - Preserved the existing public contract: exact symmetry is checked before
    the backend call, the result is a lower-triangular `Float64` Tensor with
    zero entries above the diagonal, and nonsymmetric or non-positive-definite
    inputs raise `tensor/not-positive-definite`.
  - Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or
    unavailable backend paths keep the pure C3 Cholesky fallback as the
    semantic oracle.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - direct `matrix/cholesky` diagonal smoke -> `2.0`
    - direct `matrix/cholesky` lower-entry smoke -> `1.0`
    - direct `matrix/cholesky` upper-zero smoke -> `0.0`
    - direct nonsymmetric Cholesky handler smoke ->
      `matrix/cholesky: input matrix is not symmetric positive definite`
    - direct indefinite Cholesky handler smoke ->
      `matrix/cholesky: input matrix is not symmetric positive definite`
    - focused advanced collections/module group on host
      -> `461 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`
    - local `ldconfig -p` showed `liblapack.so.3` and BLAS libraries but no
      `liblapacke`, so the focused LAPACK path regression validated fallback
      retention on this host.

- Completed `TENSOR-090N` optional LAPACK `dgetrf` LU/determinant
  acceleration:
  - Extended the private runtime-loaded LAPACK helper to resolve
    `LAPACKE_dgetrf` alongside `LAPACKE_dgesv`, with separate availability and
    call-count probes.
  - Routed `matrix/lu` through the LAPACK helper when `liblapacke` and
    `LAPACKE_dgetrf` are available, preserving the public dictionary contract:
    combined `lu` factor tensor, final 0-based pivot row order, and
    `swap-count`.
  - Routed `matrix/determinant` through the same optional `dgetrf`
    factorization path when available, preserving determinant parity and the
    existing singular `0.0` contract.
  - Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or
    unavailable backend paths keep the pure C3 LU fallback as the semantic
    oracle.
  - LAPACK singular results still map to the existing public contracts:
    `matrix/lu` raises `tensor/singular-matrix`, while
    `matrix/determinant` returns `0.0`.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - direct `matrix/lu` factor smoke -> `0.5`
    - direct `matrix/determinant` smoke -> `-2.0`
    - direct singular `matrix/lu` handler smoke ->
      `matrix/lu: coefficient matrix is singular`
    - direct singular `matrix/determinant` smoke -> `0.0`
    - focused advanced collections/module group on host
      -> `459 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`
    - local `ldconfig -p` showed `liblapack.so.3` and BLAS libraries but no
      `liblapacke`, so the focused LAPACK path regression validated fallback
      retention on this host.

- Completed `TENSOR-090M` first symmetric-real Matrix eigen surfaces:
  - Added public `matrix/eigenvalues` and `matrix/eigenvectors` for square
    symmetric rank-2 `Float64` Tensor inputs.
  - The implementation realizes lazy inputs and reuses the pure symmetric
    Jacobi eigensolver.
  - `matrix/eigenvalues` returns a descending rank-1 `Float64` Tensor.
  - `matrix/eigenvectors` returns a dictionary with:
    - `values`: descending rank-1 `Float64` Tensor.
    - `vectors`: rank-2 `Float64` Tensor with normalized eigenvectors stored
      as columns aligned with `values`.
  - Nonsymmetric inputs raise `tensor/not-symmetric`.
  - General nonsymmetric eigenpairs with complex-valued output remain a
    separate contract decision.
  - Registered the primitives in interpreter and AOT primitive lookup tables.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, plan index, matrix surface decision note, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - direct symmetric `matrix/eigenvalues` smoke -> `3.0`
    - direct `matrix/eigenvectors` vector-column smoke -> `1.0`
    - direct nonsymmetric eigen handler smoke -> `tensor/not-symmetric`
    - focused advanced collections/module group on host
      -> `455 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090L` first rectangular Matrix decomposition surface:
  - Added public `matrix/svd` for rank-2 `Float64` Tensor inputs.
  - The implementation realizes lazy inputs and computes a pure reduced SVD
    through a symmetric Jacobi eigensolver on the smaller Gram matrix.
  - The result is a dictionary with:
    - `u`: a `Float64` Tensor with shape `[rows k]`.
    - `s`: a `Float64` Tensor with shape `[k]`, sorted descending.
    - `v`: a `Float64` Tensor with shape `[columns k]`.
  - `k = min(rows, columns)`, and rank-deficient inputs are supported with
    zero singular values.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, plan index, matrix surface decision note, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - direct `matrix/svd` leading singular value smoke -> `3.0`
    - direct wide `matrix/svd` V-column smoke -> `1.0`
    - direct rank-deficient `matrix/svd` singular value smoke -> `0.0`
    - focused advanced collections/module group on host
      -> `445 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090K` first symmetric positive-definite Matrix
  decomposition surface:
  - Added public `matrix/cholesky` for square symmetric positive-definite
    rank-2 `Float64` Tensor inputs.
  - The implementation realizes lazy inputs and computes a pure
    lower-triangular Cholesky factor.
  - The result is a same-shape `Float64` Tensor, with entries above the
    diagonal set to zero.
  - Nonsymmetric or non-positive-definite inputs raise
    `tensor/not-positive-definite`.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, plan index, matrix surface decision note, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - direct `matrix/cholesky` diagonal smoke -> `2.0`
    - direct `matrix/cholesky` lower-entry smoke -> `1.0`
    - direct indefinite Cholesky handler smoke ->
      `tensor/not-positive-definite`
    - focused advanced collections/module group on host
      -> `435 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090J` first non-LU Matrix decomposition surface:
  - Added public `matrix/qr` for full-column-rank rank-2 `Float64` Tensor
    inputs with rows greater than or equal to columns.
  - The implementation realizes lazy inputs and computes a pure reduced QR
    factorization.
  - The result is a dictionary with:
    - `q`: a `Float64` Tensor with shape `[rows columns]`.
    - `r`: a `Float64` Tensor with shape `[columns columns]`.
  - Rank-deficient inputs raise `tensor/singular-matrix`.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, plan index, matrix surface decision note, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - direct `matrix/qr` R projection smoke -> `1.0`
