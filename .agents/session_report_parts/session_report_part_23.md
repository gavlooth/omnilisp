# Session Report Index Part 23

Source: `.agents/SESSION_REPORT.md`

## 2026-04-16 14:20 CEST - LAPACK DPOTRF Behind Cholesky

Objective attempted:
- Continue backend acceleration coverage behind the existing Tensor matrix
  surfaces by adding optional LAPACK Cholesky support without changing the
  public `matrix/cholesky` contract.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Extended `csrc/tensor_lapack_helpers.c` to resolve `LAPACKE_dpotrf` at
  runtime, with independent availability and call-count probes.
- Added C3 externs for the `dpotrf` helper in
  `src/lisp/tensor_lapack_backend.c3`.
- Routed `matrix/cholesky` through optional `LAPACKE_dpotrf` acceleration in
  `src/lisp/prim_tensor_matrix.c3`.
- Added a symmetry-preserving lower-triangle copy helper so nonsymmetric inputs
  are rejected before any LAPACK one-triangle factorization path can run.
- Added a conditional focused regression for the Cholesky `dpotrf` backend path
  in `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`, and
  `.agents/PLAN.md`.

Key results:
- Public surface remains unchanged: no backend-flavored names were added.
- `matrix/cholesky` still returns a lower-triangular `Float64` Tensor with the
  same shape as the input and zero entries above the diagonal.
- Nonsymmetric or non-positive-definite inputs still raise
  `tensor/not-positive-definite`.
- Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or backend
  unavailability retain the pure C3 Cholesky fallback as the semantic oracle.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct Cholesky diagonal smoke:
  `(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [0 0])`
  returned `2.0`.
- Direct Cholesky lower-entry smoke:
  `(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [1 0])`
  returned `1.0`.
- Direct Cholesky upper-zero smoke:
  `(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [0 1])`
  returned `0.0`.
- Direct nonsymmetric and indefinite Cholesky smokes returned
  `matrix/cholesky: input matrix is not symmetric positive definite`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `461 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`: showed
  `liblapack.so.3` and BLAS libraries but no `liblapacke`, so the local run
  validated fallback retention rather than live accelerated `dpotrf`.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` by broadening LAPACK/LAPACKE coverage behind QR, SVD,
  or symmetric eigen surfaces, or by making the broader nonsymmetric eigenpair
  contract decision.
- Keep public names backend-neutral under `matrix/`; do not add bare
  decomposition aliases or backend-flavored public names.

Unresolved issues:
- Live `LAPACKE_dpotrf` execution was not validated on this host because
  `liblapacke` is unavailable.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 14:13 CEST - LAPACK DGETRF Behind LU And Determinant

Objective attempted:
- Continue the Tensor scientific matrix lane by broadening backend coverage
  behind already-shipped public surfaces instead of adding new public names.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Extended `csrc/tensor_lapack_helpers.c` to resolve `LAPACKE_dgetrf` at
  runtime alongside `LAPACKE_dgesv`, with independent availability and
  call-count probes.
- Added C3 externs for the `dgetrf` helper in
  `src/lisp/tensor_lapack_backend.c3`.
- Routed `matrix/lu` and `matrix/determinant` through optional
  `LAPACKE_dgetrf` acceleration in `src/lisp/prim_tensor_matrix.c3`.
- Added conditional focused regressions for the LU and determinant `dgetrf`
  backend paths in `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`, and
  `.agents/PLAN.md`.

Key results:
- Public surface remains unchanged: no backend-flavored names were added.
- `matrix/lu` still returns combined `lu`, final 0-based `pivots`, and
  `swap-count`; singular inputs still raise `tensor/singular-matrix`.
- `matrix/determinant` still returns a `Float64` scalar and returns `0.0` for
  singular matrices.
- Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or backend
  unavailability retain the pure C3 LU fallback as the semantic oracle.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct LU factor smoke:
  `(ref (ref (matrix/lu (Tensor Float64 [2 2] [4 3 2 1])) 'lu) [1 0])`
  returned `0.5`.
- Direct determinant smoke:
  `(matrix/determinant (Tensor Float64 [2 2] [4 3 2 1]))` returned `-2.0`.
- Direct singular LU smoke returned
  `matrix/lu: coefficient matrix is singular`.
- Direct singular determinant smoke returned `0.0`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `459 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`: showed
  `liblapack.so.3` and BLAS libraries but no `liblapacke`, so the local run
  validated fallback retention rather than live accelerated `dgetrf`.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` by broadening LAPACK/LAPACKE coverage behind the
  remaining shipped surfaces such as QR, Cholesky, SVD, or symmetric eigen, or
  by making the broader nonsymmetric eigenpair contract decision.
- Keep public names backend-neutral under `matrix/`; do not add bare
  decomposition aliases or backend-flavored public names.

Unresolved issues:
- Live `LAPACKE_dgetrf` execution was not validated on this host because
  `liblapacke` is unavailable.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 13:00 CEST - Matrix Solver And Decomposition Surfaces

Objective attempted:
- Continue the Tensor scientific lane after BLAS `dgemm`/`dgemv`/`ddot`/`dger`
  slices by resolving the solver/decomposition naming blocker and landing the
  first useful matrix solver and decomposition behavior.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `src/lisp/prim_tensor_matrix.c3` with the first `matrix/solve`
  implementation.
- Added private runtime-loaded LAPACK helper wiring in
  `csrc/tensor_lapack_helpers.c` and `src/lisp/tensor_lapack_backend.c3`.
- Added the LAPACK helper source to `project.json` and
  `scripts/build_omni_chelpers.sh`.
- Registered `matrix/solve` in interpreter primitives and AOT primitive
  lookup.
- Added `matrix/lu` in `src/lisp/prim_tensor_matrix.c3` and registered it in
  interpreter primitives and AOT primitive lookup.
- Added `matrix/determinant` in `src/lisp/prim_tensor_matrix.c3` and
  registered it in interpreter primitives and AOT primitive lookup.
- Added `matrix/qr` in `src/lisp/prim_tensor_matrix.c3` and registered it in
  interpreter primitives and AOT primitive lookup.
- Added `matrix/cholesky` in `src/lisp/prim_tensor_matrix.c3` and registered
  it in interpreter primitives and AOT primitive lookup.
- Added `matrix/svd` in `src/lisp/prim_tensor_matrix.c3` and registered it in
  interpreter primitives and AOT primitive lookup.
- Added `matrix/eigenvalues` and `matrix/eigenvectors` in
  `src/lisp/prim_tensor_matrix.c3` and registered them in interpreter
  primitives and AOT primitive lookup.
- Added focused advanced collections/module regressions for vector RHS,
  matrix RHS, lazy coefficient realization, dtype, singular matrices,
  mismatched RHS shape, unsupported complex dtype, and conditional LAPACK path
  use when `LAPACKE_dgesv` is available.
- Added focused `matrix/lu` regressions for combined lower/upper factors,
  pivot row order, swap count, lazy realization, result dtype, singular input,
  non-square input, and unsupported complex dtype.
- Added focused `matrix/determinant` regressions for LU-derived determinant,
  pivot parity, singular-zero behavior, lazy realization, empty-square identity,
  non-square input, and unsupported complex dtype.
- Added focused `matrix/qr` regressions for reduced `q`/`r` factor contents,
  output shapes, lazy realization, wide input rejection, rank-deficient input,
  and unsupported complex dtype.
- Added focused `matrix/cholesky` regressions for lower factor contents, upper
  zeroing, lazy realization, output shape, non-square input, nonsymmetric
  input, indefinite input, and unsupported complex dtype.
- Added focused `matrix/svd` regressions for reduced singular values, `u`/`v`
  factor contents, tall and wide output shapes, lazy realization,
  rank-deficient input, and unsupported complex dtype.
- Added focused `matrix/eigenvalues` and `matrix/eigenvectors` regressions for
  diagonal and off-diagonal symmetric eigenvalues, aligned eigenvector
  payloads, lazy realization, nonsymmetric rejection, and unsupported complex
  dtype.
- Added `docs/plans/matrix-solver-surface-decision-2026-04-16.md` and updated
  the Tensor language spec, collection reference, Tensor area page, Tensor
  plan, plan index, `.agents/PLAN.md`, and `memory/CHANGELOG.md`.

Key results:
- Public solver namespace is now `matrix/`, with `matrix/solve` as the first
  shipped operation.
- Rejected first-surface names: bare `solve`, `linalg/solve`, `tensor/solve`,
  and backend-flavored `tensor/lapack/solve`.
- `matrix/solve` accepts a square rank-2 `Float64` coefficient Tensor and a
  rank-1 or rank-2 `Float64` RHS Tensor, realizes lazy operands, preserves RHS
  rank in the result, and raises `tensor/singular-matrix` for singular systems.
- LAPACK/LAPACKE is now optionally probed through `LAPACKE_dgesv` behind the
  same surface; missing backend support keeps the pure runtime solver fallback,
  which remains the semantic oracle.
- `matrix/lu` is the first shipped rank-2 decomposition surface. It accepts a
  square rank-2 `Float64` Tensor, realizes lazy operands, returns a dictionary
  with combined `lu` factors, final 0-based `pivots`, and `swap-count`, and
  raises `tensor/singular-matrix` for singular inputs.
- `matrix/determinant` is the first shipped decomposition consumer. It reuses
  the partial-pivot LU kernel, returns a `Float64` scalar, returns `0.0` for
  singular matrices, and uses `1.0` for empty square matrices.
- `matrix/qr` is the first shipped non-LU decomposition surface. It computes a
  reduced QR decomposition for full-column-rank rank-2 `Float64` tensors with
  rows greater than or equal to columns, returning `q` and `r` tensors.
- `matrix/cholesky` is the first shipped symmetric positive-definite
  decomposition surface. It computes a lower-triangular `Float64` factor for
  square symmetric positive-definite rank-2 `Float64` tensors, returns a
  same-shape Tensor, zeroes entries above the diagonal, and raises
  `tensor/not-positive-definite` for nonsymmetric or non-positive-definite
  inputs.
- `matrix/svd` is the first shipped rectangular decomposition surface. It
  computes reduced SVD for rank-2 `Float64` tensors, supports rank-deficient
  inputs, and returns a dictionary with `u` shape `[rows k]`, `s` shape `[k]`,
  and `v` shape `[columns k]`, where `k = min(rows, columns)`.
- `matrix/eigenvalues` and `matrix/eigenvectors` are the first shipped eigen
  surfaces. They intentionally use a symmetric-real contract for square
  rank-2 `Float64` tensors. `matrix/eigenvalues` returns descending
  eigenvalues; `matrix/eigenvectors` returns aligned `values` and `vectors`
  columns; nonsymmetric inputs raise `tensor/not-symmetric`.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`: showed
  `liblapack.so.3` and BLAS libraries but no `liblapacke`, so local runtime
  validation exercised the fallback path.
- Direct vector RHS smoke:
  `(ref (matrix/solve (Tensor Float64 [2 2] [2 1 1 3]) (Tensor Float64 [2] [1 2])) [1])`
  returned `0.6`.
- Direct matrix RHS smoke:
  `(ref (matrix/solve (Tensor Float64 [2 2] [2 1 1 3]) (Tensor Float64 [2 2] [1 2 2 1])) [0 1])`
  returned `1.0`.
- Direct singular handler smoke returned `tensor/singular-matrix`.
- Direct LU factor smoke:
  `(ref (ref (matrix/lu (Tensor Float64 [2 2] [4 3 2 1])) 'lu) [1 0])`
  returned `0.5`.
- Direct LU pivot smoke:
  `(ref (ref (matrix/lu (Tensor Float64 [2 2] [0 2 1 3])) 'pivots) 0)`
  returned `1`.
- Direct singular LU handler smoke returned `tensor/singular-matrix`.
- Direct determinant smoke:
  `(matrix/determinant (Tensor Float64 [2 2] [4 3 2 1]))`
  returned `-2.0`.
- Direct singular determinant smoke returned `0.0`.
- Direct empty-square determinant smoke returned `1.0`.
- Direct QR R projection smoke:
  `(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'r) [0 1])`
  returned `1.0`.
- Direct QR Q column smoke:
  `(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'q) [1 1])`
  returned `1.0`.
- Direct rank-deficient QR handler smoke returned `tensor/singular-matrix`.
- Direct Cholesky diagonal smoke:
  `(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [0 0])`
  returned `2.0`.
- Direct Cholesky lower-entry smoke:
  `(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [1 0])`
  returned `1.0`.
- Direct indefinite Cholesky handler smoke returned
  `tensor/not-positive-definite`.
- Direct SVD leading singular value smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 's) [0])`
  returned `3.0`.
- Direct wide SVD V-column smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 3] [3 0 0 0 2 0])) 'v) [1 1])`
  returned `1.0`.
- Direct rank-deficient SVD singular value smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 2] [0 0 0 0])) 's) [0])`
  returned `0.0`.
- Direct symmetric eigenvalue smoke:
  `(ref (matrix/eigenvalues (Tensor Float64 [2 2] [2 1 1 2])) [0])`
  returned `3.0`.
- Direct eigenvector column smoke:
  `(ref (ref (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])) 'vectors) [0 0])`
  returned `1.0`.
- Direct nonsymmetric eigen handler smoke returned `tensor/not-symmetric`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed after the pure solver slice, `pass=399 fail=0`.
- After adding optional LAPACK probing, the same focused group passed,
  `pass=401 fail=0`.
- After adding `matrix/lu`, the same focused group passed, `pass=410 fail=0`.
- After adding `matrix/determinant`, the same focused group passed,
  `pass=417 fail=0`.
- After adding `matrix/qr`, the same focused group passed, `pass=426 fail=0`.
- After adding `matrix/cholesky`, the same focused group passed,
  `pass=435 fail=0`.
- After adding `matrix/svd`, the same focused group passed,
  `pass=445 fail=0`.
- After adding `matrix/eigenvalues` and `matrix/eigenvectors`, the same
  focused group passed, `pass=455 fail=0`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` by adding the next rank-2 decomposition routine under
  `matrix/`, or broaden LAPACK/LAPACKE coverage behind the existing
  `matrix/solve`, `matrix/lu`, `matrix/determinant`, `matrix/qr`, and
  `matrix/cholesky`, `matrix/svd`, `matrix/eigenvalues`, and
  `matrix/eigenvectors` surfaces.
- Keep `matrix/solve` as the user-facing contract; do not add bare `solve`,
  `qr`, `cholesky`, `svd`, `eigenvalues`, `eigenvectors`, `linalg/solve`,
  `tensor/solve`, or backend-flavored public names.
- Do not add abbreviated aliases such as `matrix/det` unless the owner
  explicitly approves them.

Unresolved issues:
- This host has no `liblapacke`, so the optional `dgesv` accelerated branch
  compiled but was not live-executed locally; the conditional test validated
  fallback retention.
- The existing added `out` file contains unrelated reverse-SSH text
  and was left untouched.

Signature: Codex GPT-5
