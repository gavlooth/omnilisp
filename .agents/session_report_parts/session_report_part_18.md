# Session Report Index Part 18

Source: `.agents/SESSION_REPORT.md`

## 2026-04-16 18:58 CEST - TENSOR-100 CUDA/cuBLAS Backend Design Closure

Objective attempted:
- Continue the Tensor plan after LAPACK validation by closing the next named
  design lane: explicit-device CUDA/cuBLAS backend policy.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `docs/plans/cuda-cublas-backend-decision-2026-04-16.md`.
- Updated `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
  `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`, `TODO.md`,
  `memory/CHANGELOG.md`, `.agents/PLAN.md`, and this session report.

Key results:
- Locked future GPU support behind the existing `Tensor` value.
- Chose `to-device`, `device`, and `tensor-backends` as the first future
  placement/introspection surface.
- Rejected public `GpuTensor`, `CudaTensor`, backend-flavored math names,
  `tensor-use-backend!` as the first control surface, and implicit CPU/GPU
  transfer inside ordinary Tensor operations.
- Defined the next implementation boundary: Tensor placement metadata,
  CPU-only `device`, fail-closed `to-device` diagnostics when CUDA is
  unavailable, then opaque CUDA buffer ownership tests before cuBLAS execution.

Validation:
- Documentation/reference grep checks.
- `git diff --check`.

Unresolved issues:
- No CUDA runtime code was added in this design slice.
- Future GPU-heavy validation must use the bounded container path.
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 18:48 CEST - Advanced Macro-Hygiene Stack Headroom Calibration

Objective attempted:
- Continue broader bounded validation after the Tensor matrix fallback lane and
  fix the full `advanced` slice crash exposed by that validation.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Updated `src/lisp/tests_advanced_macro_hygiene_groups.c3`.
- Replaced the ASAN/non-ASAN non-tail recursion headroom split at depths
  `896`/`1200` with one portable depth, `512`.
- Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`, and this
  session report.

Key results:
- The crash narrowed to the
  `advanced-macro-hygiene-string-number` non-tail recursion headroom probe.
- `parse-number` and String round-trip tests were not the failing surface.
- The old `896`/`1200` thresholds were environment-specific calibration values,
  not portable runtime contracts.
- Full bounded `advanced` validation now passes.

Commands run and key results:
- Pre-fix bounded full `advanced` slice: crashed with exit `139`.
- Split bounded advanced groups: groups through
  `advanced-binding-mutation` passed; `advanced-macro-hygiene` crashed.
- `advanced-macro-hygiene-stdlib-migration`: `13 passed, 0 failed`.
- Pre-fix `advanced-macro-hygiene-string-number`: crashed with exit `139`.
- Calibration probes:
  - bounded container `(f 896)`: passed.
  - bounded container `(f 1200)`: crashed.
  - host `(f 512)`: passed.
  - host `(f 640)`, `(f 768)`, `(f 896)`, and `(f 1200)`: crashed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Host `advanced-macro-hygiene-string-number`: `9 passed, 0 failed`.
- Bounded container `advanced-macro-hygiene-string-number`: `9 passed, 0 failed`.
- Host `advanced-macro-hygiene`: `83 passed, 0 failed`.
- Bounded container `advanced-macro-hygiene`: `83 passed, 0 failed`.
- Bounded container full `advanced` slice: `1928 passed, 0 failed`.
- `ldconfig -p | rg -i 'lapacke|openblas|lapack'`: `liblapacke.so` and
  `liblapacke.so.3` are now visible under `/lib/aarch64-linux-gnu`.
- Host `advanced-collections-module` with
  `LD_LIBRARY_PATH=/usr/local/lib:/usr/lib/aarch64-linux-gnu`:
  `624 passed, 0 failed`.
- Bounded container `advanced-collections-module` with mounted aarch64
  toolchain and LAPACK library path: `611 passed, 0 failed`.
- Bounded container `memory-lifetime-smoke` with the same runtime library path:
  `225 passed, 0 failed`.

Current best recommendation / checkpoint:
- Keep this test as a portable non-tail recursion smoke at depth `512`.
- Do not reintroduce the old high thresholds without an explicit
  architecture/stack-limit detector and bounded validation on this host.

Unresolved issues:
- The validation image still has an x86-64 `/opt/c3/c3c`; bounded validation on
  this host needs `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 20:42 CEST - Pure Eigenpairs Real-Schur Fallback Stabilization

Objective attempted:
- Broaden validation after the LAPACK forced-fallback coverage lane and fix the
  bounded-container-only pure `matrix/eigenpairs` fallback failure it exposed.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Updated the pure QR eigenvalue convergence check in
  `src/lisp/prim_tensor_matrix.c3`.
- The convergence scan now treats isolated subdiagonal 2x2 real-Schur blocks
  as settled instead of applying shifted QR iteration to an already-settled
  complex block.
- Updated `TODO.md`, `memory/CHANGELOG.md`, Tensor area/plan docs,
  `.agents/PLAN.md`, and this session report.

Key results:
- Fixed a bounded-container-only corruption where forced no-`dgeev`
  `matrix/eigenpairs` returned a trailing real eigenvalue like `6146+0i`
  instead of `2+0i` for a 3x3 real-plus-complex-block matrix.
- Host focused validation stayed green.
- Bounded container focused validation now passes with the mounted aarch64 host
  toolchain.
- The existing tolerance-based LAPACK QR rank guard was preserved.

Commands run and key results:
- Initial bounded container focused run with the container's default `c3c`:
  blocked by `Exec format error` because `/opt/c3/c3c` in the image is x86-64
  while the container is `aarch64`.
- Retried bounded validation with
  `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- Pre-fix bounded container focused run: `608 passed, 3 failed`.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `624 passed, 0 failed`.
- `OMNI_VALIDATION_TIMEOUT_SEC=900 OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh bash -lc 'c3c build --obj-out obj && env LD_LIBRARY_PATH=/opt/omni-host-toolchain/lib:/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp'`:
  `611 passed, 0 failed`.
- `OMNI_TENSOR_DISABLE_LAPACK_DGEEV=1 ./build/main --eval ...` direct value
  smoke for the 3x3 real-plus-complex block: `"2+0i"`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The LAPACK forced-fallback coverage lane is now complete for the shipped
  Tensor matrix backend routines, and the bounded container validation no
  longer exposes the pure eigenpairs fallback corruption.

Unresolved issues:
- The validation image still has an x86-64 `/opt/c3/c3c`; bounded validation on
  this host needs `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 20:24 CEST - QR Forced Fallback Coverage

Objective attempted:
- Continue `TENSOR-090` backend coverage by adding forced pure fallback
  validation for the `dgeqrf`/`dorgqr`-backed QR surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a private test-only QR backend disable hook in
  `csrc/tensor_lapack_helpers.c` and its C3 extern in
  `src/lisp/tensor_lapack_backend.c3`.
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- The new test forces the runtime QR LAPACK backend off for `matrix/qr`, then
  verifies an expected public result through the pure reduced QR fallback.
- Preserved the existing tolerance-based LAPACK QR rank guard.
- Updated `TODO.md`, `memory/CHANGELOG.md`, Tensor area/plan docs,
  `.agents/PLAN.md`, and this session report.

Key results:
- The public matrix API is unchanged.
- `matrix/qr` now has explicit forced pure fallback coverage.
- QR backend control is independent and follows the same test-only disable
  pattern as the other LAPACK routines.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `624 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The QR lane now validates accelerated candidates and forced pure fallback
  behavior for `matrix/qr`.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 20:10 CEST - DPOTRF Cholesky Forced Fallback Coverage

Objective attempted:
- Continue `TENSOR-090` backend coverage by adding forced pure fallback
  validation for the `dpotrf`-backed Cholesky surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a private test-only `LAPACKE_dpotrf` disable hook in
  `csrc/tensor_lapack_helpers.c` and its C3 extern in
  `src/lisp/tensor_lapack_backend.c3`.
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- The new test forces runtime `dpotrf` off for `matrix/cholesky`, then verifies
  an expected public result through the pure lower-triangular Cholesky
  fallback.
- Updated `TODO.md`, `memory/CHANGELOG.md`, Tensor area/plan docs,
  `.agents/PLAN.md`, and this session report.

Key results:
- The public matrix API is unchanged.
- `matrix/cholesky` now has explicit forced pure fallback coverage.
- `dpotrf` backend control is independent and follows the same test-only
  disable pattern as the other LAPACK routines.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `622 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The `dpotrf` Cholesky lane now validates accelerated candidates and forced
  pure fallback behavior for `matrix/cholesky`.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 19:55 CEST - DGETRF LU And Determinant Forced Fallback Coverage

Objective attempted:
- Continue `TENSOR-090` backend coverage by adding forced pure fallback
  validation for the `dgetrf`-backed LU surfaces.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a private test-only `LAPACKE_dgetrf` disable hook in
  `csrc/tensor_lapack_helpers.c` and its C3 extern in
  `src/lisp/tensor_lapack_backend.c3`.
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- The new tests force runtime `dgetrf` off for `matrix/lu` and
  `matrix/determinant`, then verify expected public results through the pure
  partial-pivot LU fallback.
- Updated `TODO.md`, `memory/CHANGELOG.md`, Tensor area/plan docs,
  `.agents/PLAN.md`, and this session report.

Key results:
- The public matrix APIs are unchanged.
- `matrix/lu` now has explicit forced pure fallback coverage.
- `matrix/determinant` now has explicit forced pure fallback coverage.
- `dgetrf` backend control is independent and follows the same test-only
  disable pattern as the other LAPACK routines.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `620 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The `dgetrf` LU lane now validates accelerated candidates and forced pure
  fallback behavior for `matrix/lu` and `matrix/determinant`.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 19:31 CEST - DGESV Solve And Inverse Forced Fallback Coverage

Objective attempted:
- Continue `TENSOR-090` backend coverage by adding forced pure fallback
  validation for the `dgesv`-backed solver surfaces.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a private test-only `LAPACKE_dgesv` disable hook in
  `csrc/tensor_lapack_helpers.c` and its C3 extern in
  `src/lisp/tensor_lapack_backend.c3`.
- Decoupled the `dgesv` helper from the unrelated `dgeev` disable state.
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- The new tests force runtime `dgesv` off for `matrix/solve` and
  `matrix/inverse`, then verify expected public results through the pure
  Gaussian solver fallback.
- Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`, and this
  session report.

Key results:
- The public matrix APIs are unchanged.
- `matrix/solve` now has explicit forced pure fallback coverage.
- `matrix/inverse` now has explicit forced pure fallback coverage.
- `dgesv` backend control is no longer tied to the `dgeev` test hook.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `616 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The `dgesv` solver lane now validates accelerated candidates and forced pure
  fallback behavior for `matrix/solve` and `matrix/inverse`.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5
