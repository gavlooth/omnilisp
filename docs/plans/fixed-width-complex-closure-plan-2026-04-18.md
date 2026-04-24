# Fixed-Width Complex Closure Plan

Date: 2026-04-18
Status: closed closure record for fixed-width complex tensor numerical work.
The final Vulkan general eigenpair residuals closed on 2026-04-23 with a
passing bounded global gate.

## Objective

The original objective was to close the remaining fixed-width complex tensor
matrix work without keeping one umbrella item open across unrelated
implementation classes. That work was split into three `TENSOR-100H` lanes with
separate result contracts, backend ABIs, validation gates, and closure criteria;
all three lanes are now closed.

## Deployed Planning Agents

- `TENSOR-100H-SVD-FACTORS`: GPT-5.4 high-reasoning agent inspected the full
  fixed-width complex `matrix/svd` factor-output lane.
- `TENSOR-100H-CUDA-SVD-NORMS`: GPT-5.4 high-reasoning agent inspected CUDA
  fixed-width complex singular-values, spectral/nuclear norms, and SVD.
- `TENSOR-100H-COMPLEX-EIGEN`: GPT-5.4 high-reasoning agent inspected
  fixed-width complex eigen/eigenpair contracts across CPU/Vulkan/CUDA.
- Docs/backlog lane: GPT-5.4 medium-reasoning agent inspected planning
  artifacts and recommended the three-lane backlog split.

Ruflo task lanes:

- `task-1776535656686-symqsu`: `TENSOR-100H-SVD-FACTORS`.
- `task-1776535656710-clzj23`: `TENSOR-100H-CUDA-SVD-NORMS`.
- `task-1776535656878-ncxig9`: `TENSOR-100H-COMPLEX-EIGEN`.

## Lane 1: TENSOR-100H-SVD-FACTORS

Close full fixed-width complex `matrix/svd` factor output. CPU factor output is
closed as `TENSOR-100H-SVD-FACTORS-CPU`; Vulkan factor output is closed as
`TENSOR-100H-SVD-FACTORS-VULKAN`.

Contract:

- Public surface remains `matrix/svd`.
- Result remains a dictionary with exactly `u`, `s`, and `v`.
- For input shape `[rows cols]`, `k = min(rows, cols)`.
- `Complex128` input returns:
  - `u`: `Tensor Complex128 [rows k]`
  - `s`: `Tensor Float64 [k]`
  - `v`: `Tensor Complex128 [cols k]`
- `Complex64` input returns:
  - `u`: `Tensor Complex64 [rows k]`
  - `s`: `Tensor Float32 [k]`
  - `v`: `Tensor Complex64 [cols k]`
- Reconstruction is `A ~= U * diag(S) * V^H`.
- `u` and `v` columns are unitary under the Hermitian inner product.
- Phase normalization must make factor tests stable: rotate paired singular
  vectors by a unit phase so the largest-magnitude pivot component is real and
  nonnegative.

Implementation sequence:

1. Closed 2026-04-22: CPU first. Added `Complex128`/`Complex64` oracle
   behavior in `src/lisp/prim_tensor_matrix_svd_primitives.c3` and
   `src/lisp/prim_tensor_matrix_lu_svd_core_b.c3`.
2. Closed 2026-04-22: Use a native complex factor algorithm, not realified
   `U`/`V`.
   Recommended CPU path: Hermitian Gram/Jacobi, with optional runtime-loaded
   `LAPACKE_zgesvd` / `LAPACKE_cgesvd` acceleration plus pure fallback.
   The landed CPU path uses a native complex Hermitian Gram/eigenvector oracle
   with deterministic phase normalization. Complex64 widens through the
   Complex128 oracle and narrows at the output boundary.
3. Closed 2026-04-22: Extend `prim_matrix_svd` to allocate mixed result dtypes.
4. Closed 2026-04-22: Add CPU tests for dtype, shape, reconstruction,
   tall/wide, rank-deficient zero, empty-axis, no-LAPACK behavior, and
   `BigComplex` rejection.
5. Closed 2026-04-22: Vulkan second. Added complex SVD helper ABI and shaders:
   `csrc/tensor_vulkan_svd_complex128.comp`,
   `csrc/tensor_vulkan_svd_complex64.comp`, generated SPIR-V C blobs,
   helper exports, C3 externs, routing, and tests.
6. Keep CUDA out of this lane.

Validation gate:

- `glslangValidator` and `spirv-val` for new Vulkan shaders, when Vulkan lands.
- `./scripts/build_omni_chelpers.sh`
- `c3c build --obj-out obj`
- focused host `advanced-collections-module`
- bounded-container focused `advanced-collections-module`
- primitive docs parity
- Stage 3 source parity
- targeted `git diff --check`

Negative constraints:

- Do not use realified SVD factors for public complex `u`/`v`.
- Do not substitute `matrix/singular-values` for factor-output semantics.
- Do not copy Vulkan input to CPU/LAPACK.
- Do not add backend-named public SVD aliases.

## Lane 2: TENSOR-100H-CUDA-SVD-NORMS

Close CUDA fixed-width complex singular-values, spectral/nuclear norms, and
SVD-family execution.

Decision:

- Use runtime-loaded cuSOLVER DN for the numerical SVD algorithm.
- Use `cusolverDnZgesvd` for `Complex128` and `cusolverDnCgesvd` for
  `Complex64`.
- Use custom CUDA/PTX only for deterministic layout adapters:
  Omni row-major to cuSOLVER column-major, optional adjoint input for wide
  matrices, and cuSOLVER `VT = V^H` to Omni row-major `v`.
- Do not flip broad CUDA `matrix-numerical-complex128` /
  `matrix-numerical-complex64` solely for this lane. Prefer operation-specific
  capability bits such as `matrix-singular-values-complex128`,
  `matrix-singular-values-complex64`, `matrix-svd-complex128`, and
  `matrix-svd-complex64`.

Implementation sequence:

1. [x] Add cuSOLVER dynamic resolution in `csrc/tensor_cuda_helpers.c`.
2. [x] Add availability probes, disable-for-tests hook, and `gesvd` call
   counter.
   - Closed 2026-04-22: dynamic `libcusolver` loading now resolves
     `cusolverDnZgesvd`/`cusolverDnCgesvd`, probes handle creation/destruction,
     exposes `cusolver` / `cusolver-complex-svd` loader capabilities, and keeps
     operation-specific CUDA complex SVD capability fields false until routing
     lands.
3. [x] Add generated adapter PTX source, for example
   `csrc/tensor_cuda_complex_svd.cu` and
   `csrc/tensor_cuda_complex_svd_ptx.inc`.
   - Closed 2026-04-22 under `TENSOR-100H-CUDA-SVD-NORMS-ADAPTERS`:
     row-major Omni to column-major cuSOLVER input adapters, wide-matrix
     adjoint preparation, cuSOLVER `U` to public row-major `u`, and cuSOLVER
     `VT = V^H` to public row-major `v` are shipped in the CUDA complex matrix
     PTX/helper module.
4. [x] Implement CUDA complex singular-values helpers and route
   `matrix/singular-values`.
5. [x] Implement CUDA spectral/nuclear norm helpers. Norms read back only the
   public `Float64` scalar.
6. [x] Implement CUDA complex `matrix/svd` factor helpers returning CUDA-placed
   `u`, `s`, and `v`.
7. [x] Add tests for capability reporting, unavailable/fail-closed behavior,
   values, dtype, device placement, lazy CUDA inputs, empty axes, wide inputs,
   reconstruction, call counters, and no CPU/LAPACK fallback.
   - Closed 2026-04-22 under `TENSOR-100H-CUDA-SVD-NORMS-EXEC`: CUDA
     Complex128/Complex64 `matrix/singular-values`, spectral/nuclear
     `matrix/norm`, and reduced-factor `matrix/svd` route through dynamically
     loaded cuSOLVER `gesvd` plus the shipped adapter ABI. Regression coverage
     includes capability truth, fail-closed unavailable behavior, dtype/device
     placement, lazy CUDA inputs, empty axes, wide SVD factors, reconstruction,
     and cuSOLVER counter movement.

Closure state:

- cuSOLVER is an isolated optional CUDA SVD provider behind dynamic loading.
  Public operations route only after shape, byte-length, layout-adapter, and
  capability checks pass.
- No active checkpoint remains under this lane. Reopen future CUDA SVD work only
  as a new measurement- or capability-led item.

Validation gate:

- CUDA PTX generation and `ptxas` for adapter kernels.
  - 2026-04-22 local note: `nvcc` and `ptxas` were not available on `PATH`,
    so `TENSOR-100H-CUDA-SVD-NORMS-ADAPTERS` could not generate or validate
    checked-in PTX in that host session.
- C helper syntax/object compile and exported symbol checks.
- `./scripts/build_omni_chelpers.sh`
- `c3c build --obj-out obj`
- direct CUDA smokes when CUDA + cuSOLVER are present
- focused host `advanced-collections-module`
- bounded-container focused `advanced-collections-module`
- primitive docs parity
- Stage 3 source parity
- targeted `git diff --check`

Negative constraints:

- Do not implement CUDA complex SVD by realifying into doubled real matrices.
- Do not return cuSOLVER `VT` as public `v`; convert `V^H` to `V`.
- Do not make CUDA/cuSOLVER a link-time dependency.
- Do not claim full CUDA complex numerical matrix support beyond this family.

## Lane 3: TENSOR-100H-COMPLEX-EIGEN

Close fixed-width complex eigen/eigenpair contracts before claiming backend
execution.

Recommended product contract:

- Keep existing public names; do not add dtype- or backend-named eigen
  aliases.
- `matrix/eigenvalues` remains the symmetric/Hermitian value-only surface.
  It should accept square rank-2 `Float64`, `Float32`, `Complex128`, and
  `Complex64` tensors. Hermitian complex values are component-width real.
- `matrix/eigenvectors` remains the symmetric/Hermitian factor surface and
  returns `{ values, vectors }`. Values are component-width real; vectors
  preserve input dtype.
- `matrix/eigenpairs` becomes the general fixed-width eigenpair surface for
  square `Float64`, `Float32`, `Complex128`, and `Complex64` tensors. Values
  and vectors are fixed-width complex: `Complex128` for `Float64`/`Complex128`
  input and `Complex64` for `Float32`/`Complex64` input.
- The current CPU `Float64` `matrix/eigenpairs` `BigComplex` output should be
  migrated, not preserved as the backend-neutral fixed-width result.
  `BigComplex` remains a high-precision family, not a hidden GPU result type.

Implementation sequence:

1. Freeze the contract in docs and tests before adding backend claims.
2. Add shared fixed-width eigen result builders, sorting, phase normalization,
   and residual-oriented helpers.
3. [x] Add CPU Hermitian `Complex128`/`Complex64` `matrix/eigenvalues` and
   `matrix/eigenvectors`.
   - Closed 2026-04-22: CPU Hermitian fixed-width complex eigenvalues and
     eigenvectors now use an exact Hermitian gate and pure Complex128 Jacobi
     factorization. Values are component-real (`Float64` for `Complex128`,
     `Float32` for `Complex64`), vectors preserve input dtype, and Complex64
     widens/narrows around the Complex128 oracle. Optional `zheev`/`cheev`
     acceleration remains a future performance path only; it is not required
     for the shipped CPU contract.
4. [x] Add CPU general fixed-width `matrix/eigenpairs` for real and complex
   fixed-width inputs. Migrate current `Float64` output from `BigComplex` to
   `Complex128` under the fixed-width contract.
   - 2026-04-22 partial closure: existing CPU `Float64`
     `matrix/eigenpairs` now returns `Complex128` `values` and `vectors` for
     both LAPACK and pure fallback paths. Float32/Complex128/Complex64 general
     eigenpair inputs remain open.
   - 2026-04-22 closure: CPU general `matrix/eigenpairs` now accepts
     `Float32`, `Complex128`, and `Complex64`; Float32/Complex64 return
     `Complex64` result tensors and Complex128 returns `Complex128`.
5. [x] Add operation-specific capability bits:
   `matrix-hermitian-eigen-complex128`,
   `matrix-hermitian-eigen-complex64`,
   `matrix-eigenpairs-complex128`,
   `matrix-eigenpairs-complex64`.
   - 2026-04-22 partial closure: `matrix-hermitian-eigen-complex128` and
     `matrix-hermitian-eigen-complex64` now report true only on CPU, matching
     the shipped CPU Hermitian oracle. `matrix-eigenpairs-complex128` and
     `matrix-eigenpairs-complex64` remain false on every backend until the
     general fixed-width eigenpair contract ships.
   - 2026-04-22 closure: CPU `matrix-eigenpairs-complex128` and
     `matrix-eigenpairs-complex64` now follow LAPACK `zgeev`/`cgeev`
     availability; CUDA and cuBLAS remain false.
   - 2026-04-23 closure: Vulkan `matrix-eigenpairs-complex128` and
     `matrix-eigenpairs-complex64` now report native fixed-width general
     eigenpair support when Vulkan complex execution is available.
6. [x] Add Vulkan Hermitian complex eigenvalues/eigenvectors after CPU oracle
   behavior is stable.
   - Closed 2026-04-22: Vulkan `Complex128`/`Complex64` Hermitian
     `matrix/eigenvalues` and `matrix/eigenvectors` now use native Jacobi
     shaders and helper ABIs. Values stay Vulkan-placed and component-real;
     vectors stay Vulkan-placed and preserve input complex dtype. Non-Hermitian
     inputs fail closed with `tensor/not-symmetric`.
7. [x] Add Vulkan general fixed-width eigenpairs after recording the
   non-Hermitian complex solver boundary.
   - Closed 2026-04-23: Vulkan `Float64`, `Float32`, `Complex128`, and
     `Complex64` general `matrix/eigenpairs` now return Vulkan-placed
     fixed-width complex `values` and `vectors`. Exact 2x2 complex-shift cases
     use direct analytic handling, and active-submatrix deflation handles the
     mixed-block 3x3 residuals that blocked final closure.
8. Keep CUDA eigen false unless a cuSOLVER eigen lane is explicitly opened.

Active hypothesis:

- The fixed-width eigen contract is closed for CPU Float64/Float32 general
  eigenpairs, CPU Complex128/Complex64 Hermitian and LAPACK-backed general
  eigenpairs, Vulkan Hermitian complex execution, and native Vulkan general
  `matrix/eigenpairs` for Float64/Float32/Complex128/Complex64.

Current approach:

- Keep this plan as a closure record. CPU `matrix/eigenpairs`, Vulkan
  Hermitian `matrix/eigenvalues`/`matrix/eigenvectors`, Vulkan general
  `matrix/eigenpairs`, residual tests, and operation-specific capability bits
  are shipped.
- Treat forced no-LAPACK fallback tests as execution-counter tests, not only
  capability-probe tests; `dgeev` now has a verified disable gate at the helper
  execution boundary.

Next checkpoint:

- None currently under this plan. Reopen future work only as a new named
  measurement- or capability-led item.

Validation gate:

- residual checks `A * v ~= lambda * v`
- dtype/shape/device/capability truth-table checks
- Hermitian and general nonsymmetric cases
- repeated/defective-tolerant tests that avoid assuming unique columns
- no-hidden-fallback and LAPACK counter tests for backend paths
- shader validation when Vulkan lands
- `./scripts/build_omni_chelpers.sh`
- `c3c build --obj-out obj`
- focused host and bounded-container advanced tests
- primitive docs parity
- Stage 3 source parity
- targeted `git diff --check`

Negative constraints:

- Do not preserve `BigComplex` as the backend-neutral result for fixed-width
  numeric eigenpairs if Vulkan/CUDA support is claimed.
- Do not lower pointer-backed `BigComplex` into fixed-width backend storage.
- Do not report eigen capability through broad complex numerical bits alone.
- Do not include CUDA eigen work unless cuSOLVER eigen support is explicitly
  designed and validated.
- Do not rely on LAPACK `available()` false alone as proof that pure fallback
  executed; disabled LAPACK routines must also leave routine-specific counters
  unchanged or otherwise prove no backend call occurred.
- Do not use realification as a pure general complex eigensolver; it cannot
  distinguish a complex eigenvalue from its conjugate for arbitrary complex
  matrices.
- Do not treat the shipped Vulkan Hermitian Jacobi kernel as a general complex
  eigensolver.

## Execution Order

1. Closed: `TENSOR-100H-SVD-FACTORS-CPU` CPU oracle.
2. Closed: `TENSOR-100H-SVD-FACTORS-VULKAN` Vulkan complex factor output.
3. Closed: `TENSOR-100H-CUDA-SVD-NORMS-LOADER` cuSOLVER loader and capability
   probes.
4. Closed: `TENSOR-100H-CUDA-SVD-NORMS-ADAPTERS` CUDA complex SVD layout
   adapters and checked-in PTX.
5. Closed: `TENSOR-100H-CUDA-SVD-NORMS-EXEC` public CUDA
   `matrix/singular-values`, `matrix/norm`, and `matrix/svd` routing.
6. Closed: `TENSOR-100H-COMPLEX-EIGEN` contract freeze and CPU migration.
7. Closed: `TENSOR-100H-COMPLEX-EIGEN` Vulkan Hermitian execution.
8. Closed: `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL`, including real Vulkan
   general eigen routes, exact-shift hardening, and active-submatrix
   deflation.

This order makes CPU oracles explicit before backend claims, keeps CUDA's
cuSOLVER dependency isolated, and avoids blocking SVD closure on the larger
eigen product-contract migration.
