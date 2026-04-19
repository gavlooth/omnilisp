# Session Report Index Part 20

Source: `.agents/SESSION_REPORT.md`

## 2026-04-16 17:24 CEST - General Matrix Eigenpairs Backend Residual Coverage

Objective attempted:
- Continue `matrix/eigenpairs` validation by adding accelerated-path residual
  coverage after closing the pure fallback residual slice.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `LAPACKE_dgeev`-path residual regressions in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/areas/tensor-scientific.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, `TODO.md`,
  `.agents/PLAN.md`, and this session report.

Key results:
- When the runtime backend is available, `matrix/eigenpairs` now validates
  representative real and complex returned vector columns satisfy
  `A*v ~= lambda*v`.
- This complements the forced no-`dgeev` fallback residual checks from
  `TENSOR-090AD`.

Commands run and key results:
- Direct backend residual smoke for a real diagonal matrix: `true`.
- Direct backend residual smoke for a complex rotation matrix: `true`.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `576 passed, 0 failed`.

Current best recommendation / checkpoint:
- `matrix/eigenpairs` now has value, dtype/shape, backend call-count, pure
  fallback residual, and accelerated-path residual coverage in the focused
  advanced collections/module suite.
- Future work should move to broader stress/property validation or a different
  scientific-numerics surface instead of repeating fixed eigenpair examples.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 17:14 CEST - General Matrix Eigenpairs Fallback Coverage

Objective attempted:
- Continue hardening `matrix/eigenpairs` after closing the no-LAPACK fallback
  gap.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added forced no-`dgeev` fallback regressions in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/areas/tensor-scientific.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, `TODO.md`,
  `.agents/PLAN.md`, and this session report.

Key results:
- The pure `matrix/eigenpairs` fallback is now covered beyond the initial 2x2
  cases.
- Added 3x3 fallback coverage for diagonal real eigenvalues, upper-triangular
  real eigenvalues, and a real-plus-complex-conjugate block.
- Added representative residual checks that validate returned vector columns
  satisfy `A*v ~= lambda*v` in the forced fallback path.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `574 passed, 0 failed`.

Current best recommendation / checkpoint:
- `matrix/eigenpairs` fallback coverage is now reasonable for 2x2 and targeted
  3x3 cases. Future work can add broader property tests or larger stress
  cases, preferably as a distinct validation slice.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 17:03 CEST - General Matrix Eigenpairs Pure Fallback

Objective attempted:
- Continue `matrix/eigenpairs` by closing the backend-required gap from the
  first implementation.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a pure fallback for `matrix/eigenpairs` in
  `src/lisp/prim_tensor_matrix.c3`.
- Added a `dgeev` disable hook and `OMNI_TENSOR_DISABLE_LAPACK_DGEEV` switch
  in `csrc/tensor_lapack_helpers.c` so fallback behavior can be validated on
  LAPACKE-enabled hosts.
- Added focused fallback tests to
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated docs, `TODO.md`, `memory/CHANGELOG.md`, and `.agents/PLAN.md` so
  `matrix/eigenpairs` is no longer described as backend-required.

Key results:
- `matrix/eigenpairs` now uses runtime `LAPACKE_dgeev` when available and
  otherwise computes eigenpairs through a pure QR/nullspace path.
- The fallback preserves the same public contract: `BigComplex` values,
  aligned `BigComplex` vector columns, deterministic sorting, lazy input
  realization, and empty square support.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `568 passed, 0 failed`.
- `OMNI_TENSOR_DISABLE_LAPACK_DGEEV=1 ... --eval '(String ... values [0])'`:
  returned `"0+1i"`.
- `OMNI_TENSOR_DISABLE_LAPACK_DGEEV=1 ... --eval '(String (dtype ... vectors))'`:
  returned `"BigComplex"`.
- `OMNI_TENSOR_DISABLE_LAPACK_DGEEV=1 ... --eval '(< ... diagonal value ...)'`:
  returned `true`.

Current best recommendation / checkpoint:
- `TENSOR-090AC` is closed. Future matrix work can add backend coverage for
  shipped surfaces or broaden numerical fallback tests, but `matrix/eigenpairs`
  now has a real no-LAPACK path.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 16:42 CEST - General Matrix Eigenpairs Surface

Objective attempted:
- Continue the Tensor matrix lane after the owner selected the flexible
  language-aligned real-vs-complex contract for general eigenpairs.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added dynamic `LAPACKE_dgeev` discovery, call-count telemetry, and C3
  externs in the Tensor LAPACK helper path.
- Added `omni_big_complex_from_doubles` to the BigComplex helper ABI so
  backend `double` real/imaginary parts can populate `BigComplex` Tensor
  storage directly.
- Added public `matrix/eigenpairs` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/eigenpairs` in interpreter and AOT primitive lookup
  tables.
- Added focused advanced collection/module regressions for complex eigenvalues,
  real diagonal eigenpairs, vector dtype/shape, lazy input realization, empty
  shape, `dgeev` backend call-count coverage, rank validation, and dtype
  validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, `TODO.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/eigenpairs` accepts square rank-2 `Float64` Tensor values.
- Lazy inputs are realized before factorization.
- The result is a dictionary with `values` as `BigComplex` shape `[n]` and
  `vectors` as `BigComplex` shape `[n n]`.
- Real and complex eigenvalues use the same output representation.
- Eigenpairs are sorted by descending eigenvalue magnitude with deterministic
  real/imaginary tie-breakers.
- Right eigenvector columns are aligned with values and phase-normalized.
- Empty square input returns empty `[0]` values and `[0 0]` vectors without a
  backend call.
- Superseded by the 17:03 CEST entry: the first implementation required
  runtime `LAPACKE_dgeev`, but `matrix/eigenpairs` now has a pure fallback.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- An incorrect focused test invocation using `--test` failed because the binary
  does not expose that flag; the correct runner is `--test-suite lisp` plus the
  slice/group environment variables.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `564 passed, 0 failed`.
- Direct complex eigenvalue smoke returned `"0+1i"`.
- Direct vector dtype smoke returned `"BigComplex"`.
- Direct empty-vector-shape smoke returned `0`.
- Direct dtype validation smoke returned
  `matrix/eigenpairs: expected a square rank-2 Float64 Tensor`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Invalidated assumptions / failed approaches:
- Do not use `./build/main --test ...`; this binary uses
  `--test-suite lisp` with `OMNI_LISP_TEST_SLICE` and
  `OMNI_ADVANCED_GROUP_FILTER` for focused Lisp groups.

Current best recommendation / checkpoint:
- `matrix/eigenpairs` is the canonical public surface for general square
  eigenpairs.
- Superseded by the 17:03 CEST entry: `TENSOR-090AC` is closed.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 16:13 CEST - Matrix Singular-Values Surface

Objective attempted:
- Continue the Tensor matrix lane with a direct singular-value extraction
  surface that reuses the existing SVD contract and avoids the unresolved
  general nonsymmetric eigenpair output contract.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/singular-values` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/singular-values` in interpreter and AOT primitive lookup
  tables.
- Added focused advanced collection/module regressions for leading and second
  singular values, wide matrix shape, empty-axis shape, lazy input realization,
  rank-deficient zero singular value, backend `dgesvd` call-count coverage,
  rank validation, and dtype validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/singular-values` accepts rank-2 `Float64` Tensor values.
- Lazy inputs are realized before factorization.
- The result is a rank-1 `Float64` Tensor with shape
  `[min(rows, columns)]`.
- Singular values are sorted descending by the existing SVD machinery.
- Rectangular and rank-deficient matrices are supported.
- Empty axes return an empty rank-1 Tensor.
- Optional `LAPACKE_dgesvd` acceleration is reused when available, with the
  pure Gram/Jacobi SVD path as fallback.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Initial focused test run failed because the new rank-1 Tensor `ref`
  assertions used scalar indices (`0`, `1`) instead of Tensor index arrays
  (`[0]`, `[1]`); tests were corrected and the binary rebuilt.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `550 passed, 0 failed`.
- Direct leading singular value smoke returned `3.0`.
- Direct wide-shape smoke returned `2`.
- Direct empty-shape smoke returned `0`.
- Direct lazy-input smoke returned `2.0`.
- Direct dtype validation smoke returned
  `matrix/singular-values: expected a rank-2 Float64 Tensor`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Invalidated assumptions / failed approaches:
- Do not write rank-1 Tensor `ref` tests as `(ref tensor 0)` or
  `(ref tensor 1)`; use index arrays such as `(ref tensor [0])`.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces or backend coverage that
  preserves shipped contracts.
- General nonsymmetric eigenpairs still need a real-vs-complex output contract
  decision before becoming a public surface.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/svals` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 16:07 CEST - Matrix Rank LAPACK Coverage

Objective attempted:
- Continue the Tensor matrix lane by adding backend coverage for an
  already-shipped public contract instead of opening the unresolved general
  nonsymmetric eigenpair surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Routed `matrix/rank` in `src/lisp/prim_tensor_matrix.c3` through optional
  runtime-loaded `LAPACKE_dgesvd` when available.
- Kept the pure partial-pivot row-echelon rank counter as the fallback path.
- Added focused advanced collection/module coverage that asserts the
  `dgesvd` call counter advances when the backend is available and otherwise
  treats fallback retention as the expected result.
- Updated `memory/CHANGELOG.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- The public `matrix/rank` contract is unchanged.
- Backend rank counting uses singular values from `LAPACKE_dgesvd` and counts
  values above the caller/default tolerance.
- Pure row-echelon rank counting remains active when LAPACK is unavailable,
  rejects the input, or fails to converge.
- Local focused validation now proves the `dgesvd` rank path on this host
  because `liblapacke` is installed.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `540 passed, 0 failed`.

Current best recommendation / checkpoint:
- Continue with backend coverage for shipped `matrix/` contracts or make a
  product-level decision for general nonsymmetric eigenpairs before exposing
  them.
- Keep public names backend-neutral under `matrix/`; backend names remain
  implementation details.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5
