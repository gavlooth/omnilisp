# Fixed-Width Complex Closure Plan

Date: 2026-04-18
Status: active closure plan for the remaining fixed-width complex tensor
numerical work after CPU/Vulkan `matrix/singular-values` and spectral/nuclear
`matrix/norm` landed.

## Objective

Close the remaining fixed-width complex tensor matrix work without keeping one
umbrella item open across unrelated implementation classes. The remaining work
is split into three `TENSOR-100H` lanes with separate result contracts,
backend ABIs, validation gates, and closure criteria.

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

Close full fixed-width complex `matrix/svd` factor output. This is not closed
by singular-value support.

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

1. CPU first. Add `Complex128`/`Complex64` oracle behavior in
   `src/lisp/prim_tensor_matrix.c3`.
2. Use a native complex factor algorithm, not realified `U`/`V`.
   Recommended CPU path: Hermitian Gram/Jacobi, with optional runtime-loaded
   `LAPACKE_zgesvd` / `LAPACKE_cgesvd` acceleration plus pure fallback.
3. Extend `prim_matrix_svd` to allocate mixed result dtypes.
4. Add CPU tests for dtype, shape, reconstruction, tall/wide, rank-deficient,
   empty-axis, lazy input, and `BigComplex` rejection.
5. Vulkan second. Add complex SVD helper ABI and shaders:
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

1. Add cuSOLVER dynamic resolution in `csrc/tensor_cuda_helpers.c`.
2. Add availability probes, disable-for-tests hook, and `gesvd` call counter.
3. Add generated adapter PTX source, for example
   `csrc/tensor_cuda_complex_svd.cu` and
   `csrc/tensor_cuda_complex_svd_ptx.inc`.
4. Implement CUDA complex singular-values helpers and route
   `matrix/singular-values`.
5. Implement CUDA spectral/nuclear norm helpers. Norms read back only the
   public `Float64` scalar.
6. Implement CUDA complex `matrix/svd` factor helpers returning CUDA-placed
   `u`, `s`, and `v`.
7. Add tests for capability reporting, unavailable/fail-closed behavior,
   values, dtype, device placement, lazy CUDA inputs, empty axes, wide inputs,
   reconstruction, call counters, and no CPU/LAPACK fallback.

Validation gate:

- CUDA PTX generation and `ptxas` for adapter kernels.
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
3. Add CPU Hermitian `Complex128`/`Complex64` `matrix/eigenvalues` and
   `matrix/eigenvectors`, using optional `zheev`/`cheev` acceleration plus pure
   fallback.
4. Add CPU general fixed-width `matrix/eigenpairs` for real and complex
   fixed-width inputs. Migrate current `Float64` output from `BigComplex` to
   `Complex128` under the fixed-width contract.
5. Add operation-specific capability bits:
   `matrix-hermitian-eigen-complex128`,
   `matrix-hermitian-eigen-complex64`,
   `matrix-eigenpairs-complex128`,
   `matrix-eigenpairs-complex64`.
6. Add Vulkan Hermitian complex eigenvalues/eigenvectors after CPU oracle
   behavior is stable.
7. Add Vulkan general fixed-width eigenpairs only after the result contract and
   CPU oracle tests are stable.
8. Keep CUDA eigen false unless a cuSOLVER eigen lane is explicitly opened.

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

## Execution Order

1. `TENSOR-100H-SVD-FACTORS` CPU oracle.
2. `TENSOR-100H-SVD-FACTORS` Vulkan complex factor output.
3. `TENSOR-100H-CUDA-SVD-NORMS` cuSOLVER loader, capabilities, and
   singular-values/norms.
4. `TENSOR-100H-CUDA-SVD-NORMS` CUDA factor output.
5. `TENSOR-100H-COMPLEX-EIGEN` contract freeze and CPU migration.
6. `TENSOR-100H-COMPLEX-EIGEN` Vulkan Hermitian/general execution.

This order makes CPU oracles explicit before backend claims, keeps CUDA's
cuSOLVER dependency isolated, and avoids blocking SVD closure on the larger
eigen product-contract migration.
