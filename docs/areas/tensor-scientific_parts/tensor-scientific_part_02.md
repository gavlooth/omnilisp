# tensor-scientific Part 02

Source: `docs/areas/tensor-scientific.md`

  Zero-size contracted axes in supported Vulkan layouts also produce
  additive-identity output, matching CPU Tensor semantics. The Vulkan
  dtype/layout policy is recorded in
  `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`: keep extending
  fixed-width `Float64` dense row-major kernels first, do not downcast to
  `Float32`, do not lower pointer-backed Big* Tensor dtypes to Vulkan, and
  require explicit fixed-width complex backend ABIs plus stride-aware layout
  contracts before extending Vulkan beyond the current real fixed-width paths.
  Broader Vulkan math-library, real/complex dtype, layout, and staged/tiled
  parallel solver policy is tracked in
  `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
- `TENSOR-110`: cleanup surface closure; examples should use canonical
  `Tensor`, `map`, `contract`, constructor materialization, and only use
  `realize` for explicit destination-storage writes.

Deferred by design:

- Exact-shape tensor-tensor `map` remains the first matching/fallback path.
- No backend-specific tensor types participate in core `map` dispatch; backend
  behavior is resolved at constructor materialization, explicit device
  movement, destination storage, and execution layers.
- BLAS/LAPACK/CUDA/cuBLAS acceleration is optional backend work behind the
  pure `Tensor` fallback; the current BLAS slices cover dense rank-2/rank-2
  `dgemm`, rank-2/rank-1 and rank-1/rank-2 `dgemv`, rank-1/rank-1 `ddot`,
  and rank-1/rank-1 zero-axis `dger` `Float64` contractions. `matrix/solve`
  uses optional `LAPACKE_dgesv` acceleration when available, with the pure
  runtime solver as the semantic oracle, with forced fallback coverage through
  a private `dgesv` disable hook. `matrix/lu` and
  `matrix/determinant` use optional `LAPACKE_dgetrf` acceleration when
  available, with the pure runtime LU factorization as the semantic oracle and
  forced fallback coverage through a private `dgetrf` disable hook.
  `matrix/inverse` uses optional `LAPACKE_dgesv` acceleration over an identity
  RHS when available, with the pure runtime Gaussian solve as its semantic
  oracle and forced fallback coverage. `matrix/cholesky` uses optional
  `LAPACKE_dpotrf` acceleration when available, with the pure
  lower-triangular factorization as its semantic oracle and forced fallback
  coverage through a private `dpotrf` disable hook.
  `matrix/qr` uses optional `LAPACKE_dgeqrf`/`LAPACKE_dorgqr` acceleration
  when available, with the pure reduced QR factorization as its semantic
  oracle and forced fallback coverage through a private QR backend disable
  hook. `matrix/singular-values`, `matrix/svd`, and the spectral/nuclear
  `matrix/norm` selectors use optional `LAPACKE_dgesvd` acceleration when
  available, with the pure reduced Gram/Jacobi SVD as their semantic oracle.
  `matrix/rank` uses optional `LAPACKE_dgesvd` acceleration when available,
  with pure row-echelon rank counting as its fallback.
  `matrix/eigenvalues` and
  `matrix/eigenvectors` use optional `LAPACKE_dsyev` acceleration when
  available, with the pure symmetric Jacobi eigensolver as the semantic oracle
  for square symmetric matrices.
  `matrix/eigenpairs` uses runtime-loaded `LAPACKE_dgeev` for general
  nonsymmetric eigenpairs when available and retains the pure fallback
  otherwise; its pure QR eigenvalue fallback treats isolated 2x2 real-Schur
  blocks as settled.

## Next Steps

1. Keep the pure C3 fallback as the validation oracle.
2. Continue the Vulkan lane through
   `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`: factor the helper
   plumbing, add the next dense row-major real matrix kernel where still
   missing, keep further backend dtype expansion gated on explicit backend
   capability bits, device layout, and validation contracts, and treat the
   measured thresholded `matrix/solve` parallel route as shipped unless later
   measurements justify a separate performance item. `Complex128`/`Complex64`
   now have CPU semantics plus CUDA/Vulkan storage/map/contract support,
   CUDA/Vulkan structural matrix kernels, and Vulkan
   LU/determinant/solve/inverse;
   remaining CUDA/Vulkan numerical complex matrix kernels remain explicit
   future work.
3. Continue matrix backend coverage only when it preserves an already-shipped
   public `matrix/*` contract. Do not expose bare `solve`, `cholesky`, `svd`,
   `eigenvalues`, `eigenvectors`, `linalg/solve`, `tensor/solve`,
   `parallel/solve`, `vulkan/solve`, or backend-flavored public solver names.
4. Continue precision Tensor work only for a concrete next precision contract,
   such as configurable precision policy, mixed precision promotion rules, or
   complex-specific scientific kernels.
5. Keep ordinary Tensor storage native/scoped; gate only genuinely opaque
   backend resources through explicit ownership/finalizer policy.
6. Continue CUDA map broadening only from explicit residual gates: unsupported
   layouts/views, explicit mixed-device execution support, and callable
   extension beyond the fixed op-id table remain separate work and must not use
   hidden CPU materialization as a fallback.
7. CUDA dtype-changing rounding uses an explicit `Tensor BigInteger` result
   path. Direct `floor`, `ceiling`, `round`, and `truncate` over CUDA-placed
   dense row-major `Float64`/`Float32` inputs are gated by the CUDA backend
   `rounding-big-integer` capability, return `BigInteger` dtype, and validate
   values after explicit CPU copyback; this must not be implemented as
   same-dtype CUDA float map output.
8. Vulkan Tensor rounding uses a separate dtype-changing contract. Do not
   infer it from generic Vulkan `available`, `float64`, `float32`, or
   same-dtype map/scientific capabilities; direct Vulkan rounding must report
   `rounding-big-integer` and return CPU `Tensor BigInteger` results, or fail
   closed on devices without that feature.
9. Do not add a public `TensorExpr`, `matmul`, backend-specific tensor type, or
   implicit CPU/GPU transfer surface in the first backend slice.

## Validation

Recent targeted validation recorded in `memory/CHANGELOG.md`:

- `c3c build --warn-deprecation=no`
- Latest precision Tensor validation:
  - host `c3c build main --output-dir build --build-dir build/obj2`: passed
    with existing deprecation warnings.
  - host `c3c build`: passed with existing deprecation warnings.
  - host targeted `advanced-collections-module` group: `pass=321 fail=0`.
  - host targeted `advanced-collections-module` group after BigComplex
    component kernels: `pass=327 fail=0`.
  - host targeted `advanced-collections-module` group after real Tensor
    component semantics: `pass=330 fail=0`.
  - host targeted `advanced-collections-module` group after Tensor `abs`
    semantics: `pass=335 fail=0`.
  - host targeted `advanced-collections-module` group after Tensor `sqrt`
    semantics: `pass=341 fail=0`.
  - host targeted `advanced-collections-module` group after Tensor unary
    scientific math semantics: `pass=353 fail=0`.
  - host targeted `advanced-collections-module` group after Tensor `pow`
    semantics: `pass=361 fail=0`.
  - host targeted `advanced-collections-module` group after Tensor `atan2`
    semantics: `pass=369 fail=0`.
  - host targeted `advanced-collections-module` group after Tensor rounding
    semantics: `pass=379 fail=0`.
  - host targeted `advanced-collections-module` group after Tensor min/max
    semantics: `pass=387 fail=0`.
  - host targeted `advanced-collections-module` group after Tensor gcd/lcm
    semantics: `pass=392 fail=0`.
  - bounded container `advanced-collections-module` group after real Tensor
    component semantics: `pass=330 fail=0`.
  - bounded container `advanced-collections-module` group after Tensor `abs`
    semantics: `pass=335 fail=0`.
  - bounded container `advanced-collections-module` group after Tensor `sqrt`
    semantics: `pass=341 fail=0`.
  - bounded container `advanced-collections-module` group after Tensor unary
    scientific math semantics: `pass=353 fail=0`.
  - bounded container `advanced-collections-module` group after Tensor `pow`
    semantics: `pass=361 fail=0`.
  - bounded container `advanced-collections-module` group after Tensor `atan2`
    semantics: `pass=369 fail=0`.
  - bounded container `advanced-collections-module` group after Tensor rounding
    semantics: `pass=379 fail=0`.
  - bounded container `advanced-collections-module` group after Tensor min/max
    semantics: `pass=387 fail=0`.
  - bounded container `advanced-collections-module` group after Tensor gcd/lcm
    semantics: `pass=392 fail=0`.
  - bounded container `memory-lifetime-smoke`: `pass=225 fail=0`.
  - bounded container `advanced-collections-module` group after BigComplex
    component kernels: `pass=327 fail=0`.
  - bounded container `memory-lifetime-smoke`: `pass=225 fail=0`.
- Latest BLAS-backed contract validation:
  - host `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - host targeted `advanced-collections-module` group: `pass=221 fail=0`.
  - direct `(ref (realize (contract a b [1 0])) [1 1])`: returned `154.0`.
  - direct transpose-backed rank-2 contract smokes returned `84.0` for
    `[0 0]`, `68.0` for `[1 1]`, and `123.0` for `[0 1]`.
- Latest transactional destination realization validation:
  - host targeted `advanced-collections-module` group: `pass=212 fail=0`
- Latest lazy Tensor edge/root-promotion validation:
  - Docker-bounded `memory-lifetime-smoke` slice: `pass=222 fail=0` after
    adding lazy closure-env promotion coverage for closure-captured Tensor
    callables, copy-to-parent detached env-scope parent chains, and
    commit-scoped graph-audit releasing-child/global-env coverage.
  - Docker-bounded ASAN `memory-lifetime-smoke` slice: `pass=222 fail=0`
- Latest Vulkan Tensor backend validation:
  - host `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - direct Vulkan `matrix/transpose` smokes returned `6.0`, `vulkan`,
    `vulkan`, and zero-size length `0`.
  - direct Vulkan `matrix/diagonal` smokes returned `5.0`, `vulkan`,
    `vulkan`, and zero-size length `0`.
  - direct Vulkan `matrix/diagonal-matrix` smokes returned `3.0`, `0.0`,
    `vulkan`, `vulkan`, and zero-size length `0`.
  - direct Vulkan `matrix/trace` smokes returned `5.0`, `5.0`, and `0.0`.
  - durable Vulkan `matrix/singular-values` regressions cover supported and
    unsupported cases including `k == 64`, `k > 64`, rectangular `2x65`, empty `0x65`,
    destination `realize` fail-closed behavior, status-payload mapping, and
    oversized index validation; `k > 64` returns Vulkan results without hidden
    CPU/LAPACK fallback.
  - direct Vulkan `matrix/norm` smokes returned `5.0`, `9.53939201416946`,
    `9.0`, `15.0`, `6.0`, `5.0`, `0.0`, and large-`k` spectral/nuclear
    results through the singular-value helper.
  - direct Vulkan `matrix/rank` smokes returned `2`, `1`, `2`, `1`, `2`,
    `0`, `0`, and `tensor/shape-mismatch`.
  - direct Vulkan `Float32` SVD-backed smokes include the previously failing
    dense all-ones / rank-deficient `65x65` cases after scale-aware eigenvalue
    tolerance plus orthonormal completion; outputs preserve Vulkan placement
    and `Float32` Tensor dtype without hidden CPU/LAPACK fallback.
  - direct Vulkan `Float32` factor/solve smokes returned `-2.0` for
    determinant, `0.199999988079071` for solve, `"Float32"` for inverse output
    dtype, `0.5` for LU, `2.0` for Cholesky, and `1.0` for QR.
  - direct Vulkan `matrix/lu` smokes returned `0.5`, `-0.5`, `1`, `1`,
    `vulkan`, `3.0`, `vulkan`, `0`, `tensor/singular-matrix`, and
    `tensor/shape-mismatch`.
  - direct Vulkan `matrix/solve` smokes returned `0.2`, `0.6`, `vulkan`,
    `1.0`, `0.2`, `0.6`, `tensor/singular-matrix`,
    `tensor/shape-mismatch`, `tensor/backend-unsupported`, and
    `tensor/singular-matrix`.
  - direct Vulkan `matrix/determinant` smokes returned `-2.0`, `-2.0`,
    `0.0`, `-2.0`, `1.0`, and `tensor/shape-mismatch`.
  - direct Vulkan `matrix/inverse` smokes returned `0.6`, `-0.7`, `vulkan`,
    `tensor/singular-matrix`, `0.4`, `vulkan`, `tensor/shape-mismatch`, and
    `1.0`.
  - direct Vulkan `matrix/cholesky` smokes returned `1.0`, `2.0`, `0.0`,
    `vulkan`, `vulkan`, zero-size shape `0`, `tensor/not-positive-definite`,
    `tensor/not-positive-definite`, and `tensor/shape-mismatch`.
  - direct Vulkan `matrix/qr` smokes returned `1.0`, `1.0`, `vulkan`,
    `vulkan`, zero-size shape `0`, and `tensor/singular-matrix`.
  - direct Vulkan multi-axis contract smokes returned `70.0` and `210.0`.
  - direct Vulkan rank-N contract smokes returned `460.0`, `139.0`, `3340.0`,
    `vulkan`, and `0.0`.
  - direct Vulkan generic contract smokes returned `154.0`, `60.0`, `167.0`,
    `69.0`, `122.0`, `69.0`, `46.0`, `43.0`, `vulkan`, and
    `tensor/backend-unsupported`.
  - direct unary `+` / `-` / `sqrt` / component smokes returned `vulkan`,
    `-1`, `2.5`, `2.0`, `vulkan`, `-1.5`, `3.0`, `vulkan`, `2.0`,
    `vulkan`, `-2.5`, `0.0`, `vulkan`, and
    `tensor/backend-unsupported` across scalar, CPU Tensor, and Vulkan Tensor
    paths.
  - direct Vulkan `Float32` scientific unary smokes returned `(0.0 1.0 1.0
    0.0)` for `sin`/`cos`/`exp`/`log`, `1.99999988079071` for `log10 100`,
    `0.841344714164734` for `stats.normal-cdf 1.0`, and
    `tensor/backend-unsupported` for Vulkan `Float64` `map sin`.
  - direct and mapped Vulkan `Float64` `stats.normal-cdf` smokes returned
    Vulkan-placed `Float64` tensors matching the CPU double oracle within
    `1e-6` at `-1.0`, `0.0`, and `1.0`; Vulkan `Float64`
    `stats.normal-quantile` remained fail-closed.
  - direct Vulkan `min` / `max` smokes returned `-2.0`, `0.0`, `vulkan`, and
    `tensor/backend-unsupported` across map, direct, lazy, and mixed-device
    paths.
  - Vulkan destination `realize` smokes returned `2.0`, `3.0`, `4.0`, and
    `1.0` for CPU source, Vulkan source, lazy Vulkan source, and scalar fill
    into existing Vulkan destinations, while Vulkan source into CPU
    destination still returned `tensor/backend-unsupported`.
  - host targeted `advanced-collections-module` group after Vulkan
    singular-values and destination-realize hardening: `pass=907 fail=0`.
  - host targeted `advanced-collections-module` group after Vulkan
    destination-form `realize` support: `pass=949 fail=0`.
  - bounded-container targeted `advanced-collections-module` group after
    Vulkan destination-form `realize` support: `pass=936 fail=0`.
  - CUDA destination `realize` smokes returned `2.0`, `3.0`, `4.0`, `1.0`,
    `154.0`, `tensor/backend-unsupported`, and
    `tensor/backend-unsupported` across CPU source, CUDA source, lazy CPU
    source, scalar fill, lazy cuBLAS contract, CUDA source into CPU
    destination, and unsupported CUDA map construction.
  - host targeted `advanced-collections-module` group after CUDA
    destination-form `realize` support and unsupported CUDA map fail-closed
    hardening: `pass=959 fail=0`.
  - host targeted `advanced-stdlib-numeric` group after CPU-only numeric
    Vulkan guard hardening: `pass=411 fail=0`.
  - primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check` passed for the hardening slice.
  - earlier host targeted `advanced-collections-module` group: `pass=824 fail=0`.
  - host `basic` slice: `pass=144 fail=0`.
  - host `compiler` slice: `pass=277 fail=0`.
  - earlier bounded container `advanced-collections-module` group before the
    later singular-values hardening: `pass=811 fail=0`.
  - bounded container full `advanced` slice: `pass=1972 fail=0`.
  - bounded container `memory-lifetime-smoke`: `pass=227 fail=0`.
- `git diff --check`
