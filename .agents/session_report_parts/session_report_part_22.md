# Session Report Index Part 22

Source: `.agents/SESSION_REPORT.md`

## 2026-04-16 15:12 CEST - Matrix Transpose Surface

Objective attempted:
- Continue the Tensor matrix lane with a deterministic structural matrix
  transform that preserves native Tensor dtypes.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/transpose` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/transpose` in interpreter and AOT primitive lookup
  tables.
- Added focused advanced collection/module regressions for Float64 value
  transposition, result shape, lazy input realization, BigInteger/BigFloat/
  BigComplex clone behavior, dtype preservation, and rank validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/transpose` accepts rank-2 Tensor values.
- Lazy inputs are realized before transposition.
- The result shape is `[columns rows]`.
- Native dtype is preserved for `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex`.
- High-precision Tensor element handles are cloned into the result rather than
  shallow-copied, preserving deterministic Tensor ownership.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- First focused suite run caught two stale shape assertions that compared
  integer shape entries through `test_eq_double`; the implementation output
  was correct (`[3 2]`). The assertions were changed to string checks.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `488 passed, 0 failed`.
- Direct Float64 transpose smoke:
  `(ref (matrix/transpose (Tensor Float64 [2 3] [1 2 3 4 5 6])) [2 1])`
  returned `6.0`.
- Direct shape smoke returned `[3 2]`.
- Direct BigInteger clone smoke returned `"9223372036854775808"`.
- Direct BigFloat clone smoke returned `"2.5"`.
- Direct BigComplex clone smoke returned `"3+4i"`.
- Direct rank validation smoke returned
  `matrix/transpose: expected a rank-2 Tensor`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces that have pure fallbacks, or
  make the product-level decision for general nonsymmetric eigenpairs and
  complex-valued output.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/t` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 15:02 CEST - Matrix Inverse Surface

Objective attempted:
- Continue the Tensor matrix lane with a deterministic user-facing matrix
  surface that does not make new semantics depend on LAPACK availability.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/inverse` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/inverse` in interpreter and AOT primitive lookup tables.
- Added focused advanced collection/module regressions for inverse values,
  product identity, lazy input, empty square shape, singular input, non-square
  input, complex dtype rejection, and live/fallback `LAPACKE_dgesv` dispatch.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/inverse` accepts nonsingular square rank-2 `Float64` Tensor inputs.
- Lazy inputs are realized before inversion.
- The result is a same-shape `Float64` Tensor.
- Singular inputs raise `tensor/singular-matrix`.
- The implementation uses optional runtime-loaded `LAPACKE_dgesv` when
  available by solving against an identity RHS; the pure Gaussian solver
  remains the semantic oracle and fallback.
- General nonsymmetric eigenpairs remain a separate contract decision because
  they need a complex-valued result policy and deterministic fallback.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `479 passed, 0 failed`.
- Direct inverse first-entry smoke:
  `(ref (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [0 0])`
  returned `0.6`.
- Direct inverse off-diagonal smoke:
  `(ref (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [0 1])`
  returned `-0.7`.
- Direct inverse product identity smoke:
  `(ref (realize (contract (Tensor Float64 [2 2] [4 7 2 6]) (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [1 0])) [1 1])`
  returned `1.0`.
- Direct singular inverse smoke returned
  `matrix/inverse: input matrix is singular`.
- Direct empty square inverse shape smoke returned `0`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` either by deciding the general nonsymmetric eigenpair
  contract, or by adding another deterministic `matrix/` surface with a pure
  fallback and optional backend acceleration.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/inv` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 14:51 CEST - LAPACK DGESVD Behind SVD

Objective attempted:
- Continue backend acceleration coverage behind existing Tensor matrix
  surfaces by adding optional LAPACK SVD support, after installing LAPACKE
  locally for live backend validation.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Installed host package `liblapacke-dev` with `apt`, making `liblapacke.so`
  available to the runtime-loaded LAPACK helper.
- Extended `csrc/tensor_lapack_helpers.c` to resolve `LAPACKE_dgesvd` at
  runtime, with independent availability and call-count probes.
- Routed `matrix/svd` through optional `LAPACKE_dgesvd` acceleration in
  `src/lisp/prim_tensor_matrix.c3`.
- Added reduced SVD result adaptation in the C helper: LAPACK `vt` is
  transposed into Omni's `v` column-factor layout, and factor signs are
  normalized for stable output.
- Added a conditional focused regression for the SVD `dgesvd` backend path in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Tightened the LAPACK QR backend rank check from exact-zero diagonal testing
  to `abs(diagonal) <= 1e-12` after live LAPACKE validation exposed a
  rank-deficient QR false success.
- Updated `memory/CHANGELOG.md`, `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`, and
  `.agents/PLAN.md`.

Key results:
- Public surface remains unchanged: no backend-flavored names were added.
- `matrix/svd` still returns reduced `u`, `s`, and `v` factors with shapes
  `[rows k]`, `[k]`, and `[columns k]`.
- Rank-deficient inputs still succeed with zero singular values.
- Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or backend
  unavailability retain the pure Gram/Jacobi SVD fallback as the semantic
  oracle.
- The host now validates live LAPACKE-backed paths, not just fallback
  retention.

Commands run and key results:
- `sudo apt-get update && sudo apt-get install -y liblapacke-dev`: passed.
- `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`: showed
  `liblapacke.so`, `liblapacke.so.3`, `liblapack.so`, and BLAS libraries.
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Initial live focused advanced collections/module group exposed
  `matrix qr rejects rank deficient input` as a false success; the QR backend
  tolerance fix corrected it.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `469 passed, 0 failed`.
- Direct SVD leading singular value smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 's) [0])`
  returned `3.0`.
- Direct SVD U-column smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 'u) [0 0])`
  returned `1.0`.
- Direct SVD V-column smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 'v) [1 1])`
  returned `1.0`.
- Direct wide SVD V-column smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 3] [3 0 0 0 2 0])) 'v) [1 1])`
  returned `1.0`.
- Direct rank-deficient SVD singular value smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 2] [0 0 0 0])) 's) [0])`
  returned `0.0`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- Final `./scripts/build_omni_chelpers.sh`: passed.
- Final `c3c build --obj-out obj`: passed with existing entry deprecation
  warnings.
- Final focused advanced collections/module group:
  `469 passed, 0 failed`.

Invalidated assumptions / failed approaches:
- Do not keep assuming LAPACK QR rank detection can use exact zero pivots.
  `LAPACKE_dgeqrf` can produce tiny nonzero diagonals for rank-deficient input;
  use a tolerance-based check at this backend boundary.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` by choosing the broader general nonsymmetric eigenpair
  contract, or by adding additional LAPACK backend coverage only where it
  preserves an already-shipped public matrix contract.
- Keep public names backend-neutral under `matrix/`; do not add bare
  decomposition aliases or backend-flavored public names.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated to this
  slice.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 14:40 CEST - LAPACK DSYEV Behind Symmetric Eigen

Objective attempted:
- Continue backend acceleration coverage behind existing Tensor matrix
  surfaces by adding optional LAPACK symmetric eigen support without broadening
  the public eigen contract.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Extended `csrc/tensor_lapack_helpers.c` to resolve `LAPACKE_dsyev` at
  runtime, with independent availability and call-count probes.
- Added a LAPACK no-convergence status code in
  `src/lisp/tensor_lapack_backend.c3`.
- Added C3 externs for the `dsyev` helper in
  `src/lisp/tensor_lapack_backend.c3`.
- Routed `matrix/eigenvalues` and `matrix/eigenvectors` through optional
  `LAPACKE_dsyev` acceleration in `src/lisp/prim_tensor_matrix.c3`.
- Added backend-side descending eigenpair sorting and eigenvector sign
  normalization so output remains aligned and stable.
- Added conditional focused regressions for the eigenvalue and eigenvector
  `dsyev` backend paths in `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`, and
  `.agents/PLAN.md`.

Key results:
- Public surface remains unchanged: no backend-flavored names were added.
- `matrix/eigenvalues` still returns descending rank-1 `Float64` eigenvalues.
- `matrix/eigenvectors` still returns aligned `values` and `vectors` columns.
- Nonsymmetric inputs still raise `tensor/not-symmetric`.
- General nonsymmetric eigenpairs remain a separate product contract decision.
- Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or backend
  unavailability retain the pure symmetric Jacobi fallback as the semantic
  oracle.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct eigenvalues diagonal smoke:
  `(ref (matrix/eigenvalues (Tensor Float64 [2 2] [3 0 0 2])) [0])`
  returned `3.0`.
- Direct eigenvalues symmetric off-diagonal smoke:
  `(ref (matrix/eigenvalues (Tensor Float64 [2 2] [2 1 1 2])) [0])`
  returned `3.0`.
- Direct eigenvectors aligned-value smoke:
  `(ref (ref (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])) 'values) [0])`
  returned `3.0`.
- Direct eigenvectors vector-column smoke:
  `(ref (ref (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])) 'vectors) [0 0])`
  returned `1.0`.
- Direct nonsymmetric eigen smoke returned
  `matrix/eigenvalues: input matrix is not symmetric`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `467 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`: showed
  `liblapack.so.3` and BLAS libraries but no `liblapacke`, so the local run
  validated fallback retention rather than live accelerated `dsyev`.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` by broadening LAPACK/LAPACKE coverage behind SVD, or
  by making the broader nonsymmetric eigenpair contract decision.
- Keep public names backend-neutral under `matrix/`; do not add bare
  decomposition aliases or backend-flavored public names.

Unresolved issues:
- Live `LAPACKE_dsyev` execution was not validated on this host because
  `liblapacke` is unavailable.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 14:27 CEST - LAPACK DGEQRF DORGQR Behind QR

Objective attempted:
- Continue backend acceleration coverage behind the existing Tensor matrix
  surfaces by adding optional LAPACK reduced-QR support without changing the
  public `matrix/qr` contract.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Extended `csrc/tensor_lapack_helpers.c` to resolve paired
  `LAPACKE_dgeqrf` and `LAPACKE_dorgqr` runtime symbols, with independent
  availability and call-count probes.
- Added C3 externs for the QR helper in `src/lisp/tensor_lapack_backend.c3`.
- Routed `matrix/qr` through optional LAPACK acceleration in
  `src/lisp/prim_tensor_matrix.c3`.
- Added backend-side reduced `R` extraction, reduced `Q` formation, and sign
  normalization so the existing positive-diagonal factor orientation is
  preserved when LAPACK is available.
- Added a conditional focused regression for the QR LAPACK backend path in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`, and
  `.agents/PLAN.md`.

Key results:
- Public surface remains unchanged: no backend-flavored names were added.
- `matrix/qr` still returns a dictionary with reduced `q` shape
  `[rows columns]` and `r` shape `[columns columns]`.
- Rank-deficient inputs still raise `tensor/singular-matrix`.
- Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or backend
  unavailability retain the pure C3 QR fallback as the semantic oracle.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct QR Q first-column smoke:
  `(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'q) [0 0])`
  returned `1.0`.
- Direct QR Q second-column smoke:
  `(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'q) [1 1])`
  returned `1.0`.
- Direct QR R projection smoke:
  `(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'r) [0 1])`
  returned `1.0`.
- Direct QR shape smoke returned `3`.
- Direct rank-deficient QR smoke returned
  `matrix/qr: input matrix is rank deficient`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `463 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`: showed
  `liblapack.so.3` and BLAS libraries but no `liblapacke`, so the local run
  validated fallback retention rather than live accelerated QR.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` by broadening LAPACK/LAPACKE coverage behind SVD or
  symmetric eigen surfaces, or by making the broader nonsymmetric eigenpair
  contract decision.
- Keep public names backend-neutral under `matrix/`; do not add bare
  decomposition aliases or backend-flavored public names.

Unresolved issues:
- Live `LAPACKE_dgeqrf`/`LAPACKE_dorgqr` execution was not validated on this
  host because `liblapacke` is unavailable.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5
