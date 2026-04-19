# Session Report Index Part 21

Source: `.agents/SESSION_REPORT.md`

## 2026-04-16 15:57 CEST - Matrix Rank Surface

Objective attempted:
- Continue the Tensor matrix lane with a numerical structural reduction that
  does not depend on the unresolved nonsymmetric eigenpair output contract.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/rank` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/rank` in interpreter and AOT primitive lookup tables.
- Added focused advanced collection/module regressions for full rectangular
  rank, deficient rectangular rank, empty axes, lazy input realization,
  tolerance suppression, zero-tolerance small pivots, vector validation,
  dtype validation, and negative tolerance validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/rank` accepts rectangular rank-2 `Float64` Tensor values.
- Lazy inputs are realized before rank counting.
- Rank is computed with partial-pivot row-echelon elimination over a copied
  scratch buffer.
- The default tolerance is `1e-12`.
- Optional tolerance selection accepts numeric values that narrow to a
  non-negative finite `Float64`.
- Empty axes return `0`.
- The result is an `Integer`.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `538 passed, 0 failed`.
- Direct full rectangular smoke returned `2`.
- Direct deficient rectangular smoke returned `1`.
- Direct empty-axis smoke returned `0`.
- Direct tolerance-suppressed small-pivot smoke returned `1`.
- Direct zero-tolerance small-pivot smoke returned `2`.
- Direct lazy-input smoke returned `2`.
- Direct dtype validation smoke returned
  `matrix/rank: expected a rank-2 Float64 Tensor`.
- Direct negative-tolerance validation smoke returned
  `matrix/rank: tolerance must be a non-negative finite number`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces or backend coverage that
  preserves shipped contracts.
- General nonsymmetric eigenpairs still need a real-vs-complex output contract
  decision before becoming a public surface.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/rank?`, `matrix/num-rank`, or backend-flavored names
  unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 15:51 CEST - Matrix Identity Surface

Objective attempted:
- Continue the Tensor matrix lane with a size-driven structural constructor
  that supports the existing Tensor dtype families.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/identity` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/identity` in interpreter and AOT primitive lookup tables.
- Added focused advanced collection/module regressions for default Float64
  diagonal/off-diagonal values, shape, empty shape, BigInteger/BigFloat/
  BigComplex identity values, dtype preservation, negative size validation,
  and invalid dtype validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/identity` accepts non-negative integer sizes.
- The result is a square rank-2 Tensor with shape `[n n]`.
- The default dtype is `Float64`.
- Optional dtype selection supports `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex`, using the existing Tensor dtype parser.
- Diagonal cells are filled with the dtype's one value.
- Off-diagonal cells are filled with the dtype's zero value.
- Empty size `0` returns an empty square Tensor with shape `[0 0]`.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `529 passed, 0 failed`.
- Direct default Float64 diagonal smoke returned `1.0`.
- Direct default Float64 off-diagonal smoke returned `0.0`.
- Direct empty shape smoke returned `[0 0]`.
- Direct BigInteger one smoke returned `"1"`.
- Direct BigInteger zero smoke returned `"0"`.
- Direct BigFloat one smoke returned `"1"`.
- Direct BigComplex one smoke returned `"1+0i"`.
- Direct dtype preservation smoke returned `"BigComplex"`.
- Direct negative-size validation smoke returned
  `matrix/identity: size must be a non-negative Integer`.
- Direct invalid-dtype validation smoke returned
  `matrix/identity: dtype must be Float64, BigInteger, BigFloat, or BigComplex`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces or backend coverage that
  preserves shipped contracts.
- General nonsymmetric eigenpairs still need a real-vs-complex output contract
  decision before becoming a public surface.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/eye` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 15:43 CEST - Matrix Diagonal-Matrix Surface

Objective attempted:
- Continue the Tensor matrix lane with the constructor counterpart to
  `matrix/diagonal`, preserving native dtype ownership.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/diagonal-matrix` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/diagonal-matrix` in interpreter and AOT primitive lookup
  tables.
- Added focused advanced collection/module regressions for Float64 diagonal
  and off-diagonal values, shape, lazy input realization, empty square shape,
  BigInteger/BigFloat/BigComplex clone behavior, BigInteger off-diagonal zero,
  dtype preservation, and rank validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/diagonal-matrix` accepts rank-1 Tensor values.
- Lazy inputs are realized before matrix construction.
- The result is a square rank-2 Tensor with shape `[n n]`.
- Native dtype is preserved for `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex`.
- High-precision diagonal element handles are cloned into the result rather
  than shallow-copied.
- Off-diagonal cells are filled with the dtype's zero value.
- Empty rank-1 inputs return an empty square Tensor with shape `[0 0]`.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `517 passed, 0 failed`.
- Direct Float64 diagonal smoke returned `3.0`.
- Direct Float64 off-diagonal smoke returned `0.0`.
- Direct empty square shape smoke returned `[0 0]`.
- Direct BigInteger diagonal smoke returned `"9223372036854775808"`.
- Direct BigInteger off-diagonal zero smoke returned `"0"`.
- Direct BigFloat diagonal smoke returned `"2.5"`.
- Direct BigComplex diagonal smoke returned `"3+4i"`.
- Direct dtype preservation smoke returned `"BigComplex"`.
- Direct lazy-input diagonal smoke returned `2.0`.
- Direct rank validation smoke returned
  `matrix/diagonal-matrix: expected a rank-1 Tensor`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces or backend coverage that
  preserves shipped contracts.
- General nonsymmetric eigenpairs still need a real-vs-complex output contract
  decision before becoming a public surface.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/diag` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 15:31 CEST - Matrix Diagonal Surface

Objective attempted:
- Continue the Tensor matrix lane with a deterministic structural matrix
  transform that returns a Tensor and preserves native dtype ownership.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/diagonal` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/diagonal` in interpreter and AOT primitive lookup tables.
- Added focused advanced collection/module regressions for rectangular
  Float64 extraction, result shape, lazy input realization, empty diagonal
  shape, BigInteger/BigFloat/BigComplex clone behavior, dtype preservation,
  and rank validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/diagonal` accepts rank-2 Tensor values, including rectangular
  matrices.
- Lazy inputs are realized before extraction.
- The result is a rank-1 Tensor with length `min(rows, columns)`.
- Native dtype is preserved for `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex`.
- High-precision Tensor element handles are cloned into the result rather than
  shallow-copied.
- Empty rectangular inputs return an empty rank-1 Tensor with shape `[0]`.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
  A new unreachable-code warning from an exhaustive dtype switch was removed,
  then the command was rerun successfully.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `505 passed, 0 failed`.
- Direct rectangular Float64 diagonal smoke returned `5.0`.
- Direct empty diagonal shape smoke returned `[0]`.
- Direct BigInteger diagonal smoke returned `"9223372036854775808"`.
- Direct BigFloat diagonal smoke returned `"2.5"`.
- Direct BigComplex diagonal smoke returned `"3+4i"`.
- Direct dtype preservation smoke returned `"BigComplex"`.
- Direct lazy-input diagonal smoke returned `4.0`.
- Direct rank validation smoke returned
  `matrix/diagonal: expected a rank-2 Tensor`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces or backend coverage that
  preserves shipped contracts.
- General nonsymmetric eigenpairs still need a real-vs-complex output contract
  decision before becoming a public surface.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/diag` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 15:23 CEST - Matrix Trace Surface

Objective attempted:
- Continue the Tensor matrix lane with a deterministic structural matrix
  reduction that keeps native Tensor numeric families intact.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/trace` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/trace` in interpreter and AOT primitive lookup tables.
- Added focused advanced collection/module regressions for Float64 trace,
  lazy input realization, empty square zero, BigInteger/BigFloat/BigComplex
  scalar results, vector rejection, and non-square rejection.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/trace` accepts square rank-2 Tensor values.
- Lazy inputs are realized before diagonal summation.
- The result is a scalar in the Tensor's native numeric family: `Double` for
  `Float64`, or `BigInteger`, `BigFloat`, and `BigComplex` for matching
  high-precision Tensor dtypes.
- Empty square matrices return the dtype's zero scalar.
- Rectangular and non-rank-2 inputs raise
  `matrix/trace: expected a square rank-2 Tensor`.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `496 passed, 0 failed`.
- Direct Float64 trace smoke returned `5.0`.
- Direct empty square trace smoke returned `0.0`.
- Direct BigInteger trace smoke returned `"9223372036854775812"`.
- Direct BigFloat trace smoke returned `"3.75"`.
- Direct BigComplex trace smoke returned `"4+6i"`.
- Direct lazy-input trace smoke returned `5.0`.
- Direct rectangular validation smoke returned
  `matrix/trace: expected a square rank-2 Tensor`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces or backend coverage that
  preserves shipped contracts.
- General nonsymmetric eigenpairs still need a real-vs-complex output contract
  decision before becoming a public surface.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/tr` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5
