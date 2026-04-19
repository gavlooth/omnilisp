    `src/lisp/prim_tensor_matrix.c3`: `n >= 3` uses the new parallel helper,
    while `n < 3` keeps the serial Vulkan helper.
  - Added availability-gated parallel solve tests in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`, and Vulkan
    roadmap/reference docs.
- Key results:
  - The first `TENSOR-100G` slice is a 64-invocation single-workgroup
    partial-pivot Gaussian elimination shader with parallel pivot reduction,
    row swaps, elimination updates, and RHS-column back-substitution.
  - The parallel path preserves the existing public `matrix/solve` contract:
    rank-1 RHS returns rank 1, rank-2 RHS returns rank 2 with matching shape,
    results remain Vulkan-placed, singular systems raise
    `tensor/singular-matrix`, and LAPACK is not called.
  - The parallel helper uses a private typed `uint` status buffer instead of
    extending the serial helper's trailing `double` status sentinel.
  - While validating, a transient bug removed the serial solver's private
    trailing status slot and broke existing 2x2 Vulkan solve tests with
    `tensor/backend-invalid-state`; the slot was restored and the focused
    suite returned to green.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_solve_parallel_f64.comp -o /tmp/omni_tensor_vulkan_solve_parallel_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_solve_parallel_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned serial solve value `0.2`, parallel solve
    value `1.0`, vector RHS rank `1`, and `tensor/singular-matrix` for a
    dependent 3x3 system.
  - Host focused `advanced-collections-module`: `pass=793 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=780 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Extend `TENSOR-100G` from the current single-workgroup parallel solver to
    a staged/tiled multi-kernel helper with explicit Vulkan memory barriers.
  - Replace the fixed `n >= 3` bring-up threshold with a measurement-backed
    threshold once the staged helper exists.

Signature: Codex GPT-5

## 2026-04-17 05:29 CEST - TENSOR-100F1 Vulkan Matrix QR
- Objective attempted:
  - Continue the Vulkan math roadmap by landing dense row-major `Float64`
    Vulkan QR through the existing backend-neutral `matrix/qr` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_qr_f64.comp` and generated
    `csrc/tensor_vulkan_qr_f64_spv.c`.
  - Wired the generated SPIR-V source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_qr_f64` and a one-input/two-output
    dispatch helper in `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Routed public `matrix/qr` in `src/lisp/prim_tensor_matrix.c3` for
    concrete or realized Vulkan `Float64` inputs before the CPU-only path.
  - Added availability-gated Vulkan QR tests and CPU empty QR shape coverage
    in `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, and Vulkan/Tensor docs.
- Key results:
  - Public `matrix/qr` now supports Vulkan-placed dense row-major rank-2
    `Float64` inputs with rows greater than or equal to columns.
  - The returned dictionary keeps `q` and `r` as Vulkan-placed Tensor values.
  - Rank-deficient Vulkan inputs raise `tensor/singular-matrix`; wide inputs
    keep the existing `tensor/shape-mismatch` contract.
  - Vulkan QR does not call LAPACK and does not copy the input to CPU.
  - This is a correctness-first serial QR kernel, not the future parallel
    solver/factorization path.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_qr_f64.comp -o /tmp/omni_tensor_vulkan_qr_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_qr_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned representative QR values `1.0` and `1.0`,
    output devices `vulkan` and `vulkan`, empty shape `0`, and
    `tensor/singular-matrix` for the dependent-column case.
  - Host focused `advanced-collections-module`: `pass=787 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=774 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Continue `TENSOR-100F` helper factoring with the next eligible dense
    row-major `Float64` matrix kernel or move to `TENSOR-100G`.
  - `TENSOR-100G` must remain a separate thresholded parallel solver helper;
    do not present QR or the current serial solve/LU/inverse kernels as the
    performance path.

Signature: Codex GPT-5

## 2026-04-17 05:09 CEST - TENSOR-100F1 Vulkan Matrix Cholesky
- Objective attempted:
  - Continue the Vulkan math roadmap by landing the next dense row-major
    `Float64` matrix kernel through the existing backend-neutral
    `matrix/cholesky` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_cholesky_f64.comp` and generated
    `csrc/tensor_vulkan_cholesky_f64_spv.c`.
  - Wired the generated SPIR-V source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_cholesky_f64` and a shared
    trailing-status copyback helper in `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Routed public `matrix/cholesky` in `src/lisp/prim_tensor_matrix.c3` for
    concrete or realized Vulkan `Float64` inputs before the CPU-only path.
  - Fixed the CPU pure Cholesky fallback so empty square tensors with null
    zero-size data pointers succeed.
  - Added availability-gated Vulkan Cholesky tests in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, and Vulkan/Tensor docs.
- Key results:
  - Public `matrix/cholesky` now supports Vulkan-placed dense row-major square
    `Float64` inputs and returns a Vulkan-placed lower factor Tensor.
  - Nonsymmetric or non-SPD Vulkan inputs raise
    `tensor/not-positive-definite`; non-square inputs keep the existing
    `tensor/shape-mismatch` contract.
  - Vulkan Cholesky does not call LAPACK and does not copy the input to CPU.
  - The broader helper/pipeline factoring and parallel solver remain open.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_cholesky_f64.comp -o /tmp/omni_tensor_vulkan_cholesky_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_cholesky_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned representative factor values `1.0`, `2.0`,
    upper-zero `0.0`, result device `vulkan`, lazy-input device `vulkan`,
    empty shape `0`, two `tensor/not-positive-definite` diagnostics, and
    `tensor/shape-mismatch`.
  - Direct CPU empty Cholesky shape smoke returned `0`.
  - Host focused `advanced-collections-module`: `pass=775 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=762 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Continue `TENSOR-100F` helper factoring with the next eligible dense
    row-major `Float64` matrix kernel.
  - Start `TENSOR-100G` as a separate thresholded parallel solver helper; do
    not mutate the current serial `matrix/solve` shader in place.

Signature: Codex GPT-5

## 2026-04-17 04:47 CEST - Vulkan Math Library And Parallel Solver Plan
- Objective attempted:
  - Record a durable plan for continuing the Vulkan math backend beyond the
    shipped correctness-first `TENSOR-100E` kernels, including real/complex
    dtype policy, layout prerequisites, and a real parallel solver track.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
  - Updated `TODO.md` with `TENSOR-100F` / `TENSOR-100G` as the live roadmap
    and parallel solver follow-up.
  - Updated `docs/plans/README.md`,
    `docs/plans/vulkan-backend-decision-2026-04-16.md`,
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`,
    `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
    `docs/areas/tensor-scientific.md`, and `.agents/PLAN.md`.
- Key results:
  - The plan keeps Vulkan behind backend-neutral `Tensor`, `map`, `contract`,
    `matrix/*`, `to-device`, `device`, and `tensor-backends` surfaces.
  - `Float64` dense row-major remains the first Vulkan library tier.
    `Float32` and fixed-width complex are deferred until native Tensor storage
    and public dtype semantics exist; `BigInteger`, `BigFloat`, and
    `BigComplex` must not be lowered to Vulkan.
  - The current serial Vulkan `matrix/solve`, `matrix/lu`, and
    `matrix/inverse` shaders are now explicitly recorded as
    correctness-preserving small-system/backend bring-up paths, not the
    performance solver target.
  - `TENSOR-100G` requires a separate thresholded parallel solver helper with a
    typed buffer/status contract, staged helper shape, parallel pivot search,
    row swaps, elimination, and RHS-column back-substitution.
- Commands run and key results:
  - Read-only inspection of current plan docs, TODO, Vulkan solve shader,
    Vulkan helper, C3 matrix primitive, and session reports.
  - Parallel read-only agents inspected planning placement and current solver
    implementation constraints.
- Unresolved issues / next actions:
  - This was a planning pass; no new Vulkan solver code was implemented.
  - Next implementation checkpoint is `TENSOR-100F1`: factor shared Vulkan
    helper plumbing plus one additional dense row-major `Float64` matrix
    kernel, then begin `TENSOR-100G` with a separate parallel solve helper.

Signature: Codex GPT-5

## 2026-04-17 04:36 CEST - TENSOR-100E Vulkan Matrix Solve
- Objective attempted:
  - Continue `TENSOR-100E` by routing public `matrix/solve` to Vulkan for
    eligible dense row-major square `Float64` coefficient tensors and matching
    rank-1 or rank-2 `Float64` right-hand tensors without changing the
    backend-neutral matrix surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_solve_f64.comp` and generated
    `csrc/tensor_vulkan_solve_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_solve_f64` in
    `csrc/tensor_vulkan_helpers.c`, including a shared three-buffer dispatch
    helper and status-only host copyback for singular detection.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/solve` handles
    concrete or realized Vulkan coefficient/RHS tensors before the CPU-only
    storage check.
  - Added availability-gated Vulkan solve regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, and Vulkan/Tensor docs.
- Key results:
  - Public `matrix/solve` now supports Vulkan-placed dense row-major square
    `Float64` coefficient tensors with Vulkan-placed rank-1 or rank-2
    `Float64` right-hand tensors.
  - The returned solution remains a Vulkan-placed Tensor; CPU inspection still
    requires explicit `to-device 'cpu`.
  - Singular Vulkan systems raise `tensor/singular-matrix`; mixed CPU/Vulkan
    operands fail closed with `tensor/backend-unsupported`.
  - The input tensors are not copied to CPU and the Vulkan path does not call
    LAPACK; the helper computes on Vulkan and reads back only status metadata.
  - CPU Tensor `matrix/solve` behavior remains on the existing
    dgesv-then-pure implementation.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_solve_f64.comp -o /tmp/omni_tensor_vulkan_solve_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_solve_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan solve smokes returned `0.2`, `0.6`, `vulkan`, `1.0`,
    `0.2`, `0.6`, `tensor/singular-matrix`, `tensor/shape-mismatch`,
    `tensor/backend-unsupported`, and `tensor/singular-matrix`.
  - Host focused `advanced-collections-module`: `pass=763 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=750 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Vulkan solve currently uses a serial single-invocation Gaussian-elimination
    shader. It is a real Vulkan execution path and preserves the public
    contract, but it is not a high-performance parallel solver. Stride-aware
    views, non-contiguous layouts, non-`Float64` Vulkan dtypes, and mixed-device
    solve transfers remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 04:17 CEST - TENSOR-100E Vulkan Matrix Inverse
- Objective attempted:
  - Continue `TENSOR-100E` by routing public `matrix/inverse` to Vulkan for
    eligible dense row-major square `Float64` inputs without changing the
    backend-neutral matrix surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_inverse_f64.comp` and generated
    `csrc/tensor_vulkan_inverse_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_inverse_f64` in
    `csrc/tensor_vulkan_helpers.c`, including status-only host copyback for
    singular detection.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/inverse` handles
    concrete or realized Vulkan tensors before the CPU-only storage check.
  - Added availability-gated Vulkan inverse regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, and Vulkan/Tensor docs.
- Key results:
  - Public `matrix/inverse` now supports Vulkan-placed dense row-major square
    `Float64` inputs.
  - The returned inverse remains a Vulkan-placed Tensor; CPU inspection still
    requires explicit `to-device 'cpu`.
  - Singular Vulkan inputs raise `tensor/singular-matrix`.
  - The input tensor is not copied to CPU and the Vulkan path does not call
    LAPACK; the helper computes on Vulkan and reads back only status metadata.
  - CPU Tensor `matrix/inverse` behavior remains on the existing
    dgesv-then-pure implementation.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_inverse_f64.comp -o /tmp/omni_tensor_vulkan_inverse_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_inverse_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan inverse smokes returned `0.6`, `-0.7`, `vulkan`,
    `tensor/singular-matrix`, `0.4`, `vulkan`, `tensor/shape-mismatch`, and
    `1.0`.
  - Host focused `advanced-collections-module`: `pass=751 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=738 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Vulkan inverse currently uses a serial single-invocation Gauss-Jordan
    shader. It is a real Vulkan execution path and preserves the public
    contract, but it is not a high-performance parallel inverse. Stride-aware
    views, non-contiguous layouts, non-`Float64` Vulkan dtypes, and
    solve-style Vulkan RHS APIs remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 04:04 CEST - TENSOR-100E Vulkan Matrix LU
- Objective attempted:
  - Continue `TENSOR-100E` by routing public `matrix/lu` to Vulkan for
    eligible dense row-major square `Float64` inputs without changing the
    public dictionary contract.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_lu_f64.comp` and generated
    `csrc/tensor_vulkan_lu_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_lu_f64` in
    `csrc/tensor_vulkan_helpers.c`, including metadata-only host copyback for
    pivots and swap count.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/lu` handles concrete
    Vulkan tensors before the CPU-only storage check.
  - Added availability-gated Vulkan LU regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/lu` now supports Vulkan-placed dense row-major square
    `Float64` inputs.
  - The returned dictionary still contains `lu`, `pivots`, and `swap-count`.
    For Vulkan input, `lu` remains Vulkan-placed while the small metadata
    values are ordinary host values.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the dictionary metadata required by the public contract.
  - CPU Tensor `matrix/lu` behavior remains on the existing LAPACK-then-pure
    implementation.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_lu_f64.comp -o /tmp/omni_tensor_vulkan_lu_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_lu_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan LU smokes returned `0.5`, `-0.5`, `1`, `1`, `vulkan`,
    `3.0`, `vulkan`, `0`, `tensor/singular-matrix`, and
    `tensor/shape-mismatch`.
  - Host focused `advanced-collections-module`: `pass=740 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=727 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Vulkan LU currently uses a serial single-invocation partial-pivot shader.
    It is a real Vulkan execution path and preserves the public contract, but
    it is not a high-performance parallel factorization. Stride-aware views,
    non-contiguous layouts, non-`Float64` Vulkan dtypes, and broader
    solve/inverse Vulkan algorithms remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 03:48 CEST - TENSOR-100E Vulkan Matrix Determinant
- Objective attempted:
  - Continue `TENSOR-100E` with a backend-neutral `Float64` scalar reducer by
    routing public `matrix/determinant` to Vulkan for eligible dense row-major
    square inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_determinant_f64.comp` and generated
    `csrc/tensor_vulkan_determinant_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_determinant_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/determinant` handles
    concrete Vulkan tensors before the CPU-only storage check.
  - Added availability-gated Vulkan determinant regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/determinant` now supports Vulkan-placed dense row-major
    square `Float64` inputs and returns the scalar `Float64` determinant.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the scalar determinant required by the public contract.
  - CPU Tensor `matrix/determinant` behavior remains on the existing
    LAPACK-then-pure implementation.
  - Vulkan determinant tests include a no-LAPACK check to guard against hidden
    CPU fallback.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_determinant_f64.comp -o /tmp/omni_tensor_vulkan_determinant_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_determinant_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan determinant smokes returned `-2.0`, `-2.0`, `0.0`,
    `-2.0`, `1.0`, and `tensor/shape-mismatch`.
  - Direct CPU determinant smoke returned `-2.0`.
  - Direct Vulkan transpose regression smoke returned `6.0`.
  - Host focused `advanced-collections-module`: `pass=730 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=717 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Vulkan determinant currently uses a serial single-invocation partial-pivot
    LU shader. It is a real Vulkan execution path, but not a parallel
    high-performance determinant algorithm. Stride-aware views, non-contiguous
    layouts, non-`Float64` Vulkan dtypes, and larger decomposition-backed
    Vulkan algorithms remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 03:27 CEST - TENSOR-100E Vulkan Matrix Rank
- Objective attempted:
  - Continue `TENSOR-100E` with a backend-neutral `Float64` scalar reducer by
    routing public `matrix/rank` to Vulkan for eligible dense row-major rank-2
    inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_rank_f64.comp` and generated
    `csrc/tensor_vulkan_rank_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_rank_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/rank` handles
    concrete Vulkan tensors before the CPU-only storage check.
  - Added availability-gated Vulkan rank regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/rank` now supports Vulkan-placed dense row-major `Float64`
    rank-2 inputs and returns the scalar `Integer` rank.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the scalar rank result required by the public contract.
  - CPU Tensor `matrix/rank` behavior remains on the existing LAPACK-then-pure
    implementation.
  - Vulkan rank tests include a no-LAPACK check to guard against hidden CPU
    fallback.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_rank_f64.comp -o /tmp/omni_tensor_vulkan_rank_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_rank_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan rank smokes returned `2`, `1`, `2`, `1`, `2`, `0`, `0`,
    and `tensor/shape-mismatch`.
  - Direct CPU rank smoke returned `2`.
  - Host focused `advanced-collections-module`: `pass=722 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=709 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Vulkan rank currently mirrors the existing pure partial-pivot elimination
    fallback, not LAPACK/SVD rank semantics. Stride-aware views,
    non-contiguous layouts, non-`Float64` Vulkan dtypes, and larger
    decomposition-backed Vulkan algorithms remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 03:06 CEST - TENSOR-100E Vulkan Matrix Norm
- Objective attempted:
  - Continue `TENSOR-100E` with backend-neutral `Float64` scalar reducers by
    routing public `matrix/norm` to Vulkan for eligible dense row-major rank-2
    inputs and direct norm selectors.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_norm_f64.comp` and generated
    `csrc/tensor_vulkan_norm_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_norm_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/norm` handles
    concrete Vulkan tensors before the CPU-only storage check.
  - Added availability-gated Vulkan norm regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/norm` now supports Vulkan-placed dense row-major `Float64`
    rank-2 inputs for default/`'frobenius`, `'one`, `'infinity`, and `'max`.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the scalar result required by the public return contract.
  - At this checkpoint, SVD-backed `'spectral` and `'nuclear` failed closed on
    Vulkan with `tensor/backend-unsupported`; this was superseded later on
    2026-04-17 by the Vulkan singular-value helper slice.
  - CPU Tensor `matrix/norm` behavior, including LAPACK/fallback coverage for
    spectral/nuclear selectors, remains on the existing CPU implementation.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_norm_f64.comp -o /tmp/omni_tensor_vulkan_norm_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_norm_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan norm smokes returned `5.0`, `9.53939201416946`, `9.0`,
    `15.0`, `6.0`, `5.0`, `0.0`, and `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `pass=712 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=699 fail=0`.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    non-`Float64` Vulkan dtypes, SVD-backed Vulkan norm selectors, and Vulkan
    unary scientific `map` remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 02:53 CEST - TENSOR-100E Vulkan Matrix Trace
- Objective attempted:
  - Continue `TENSOR-100E` with a backend-neutral `Float64` reducer by routing
    public `matrix/trace` to Vulkan for eligible dense row-major square inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_trace_f64.comp` and generated
    `csrc/tensor_vulkan_trace_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_trace_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/trace` handles
    concrete Vulkan tensors before the CPU-only storage check and returns the
    scalar result after a one-scalar Vulkan result readback.
  - Added availability-gated Vulkan trace regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/trace` now supports Vulkan-placed dense row-major square
    `Float64` inputs.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the scalar result required by the public return contract.
  - CPU Tensor and Big* Tensor trace behavior remains on the existing CPU
    implementation.
  - Unsupported GPU/device cases fail closed rather than silently copying to CPU.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_trace_f64.comp -o /tmp/omni_tensor_vulkan_trace_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_trace_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan trace smokes returned `5.0`, `5.0`, and `0.0`.
  - Direct CPU trace smoke returned `5.0`.
  - Direct non-rank-2 trace error smoke preserved the existing shape diagnostic.
  - Host focused `advanced-collections-module`: `pass=705 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=692 fail=0`.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    non-`Float64` Vulkan dtypes, broader scalar reducers, and Vulkan unary
    scientific `map` remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 02:42 CEST - TENSOR-100E Vulkan Matrix Diagonal Pair
- Objective attempted:
  - Continue `TENSOR-100E` with the next backend-neutral `Float64` structural
    matrix family by routing public `matrix/diagonal` and
    `matrix/diagonal-matrix` to Vulkan for eligible dense row-major inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_diagonal_f64.comp`,
    `csrc/tensor_vulkan_diagonal_matrix_f64.comp`, and generated SPIR-V C
    sources for both kernels.
  - Wired the new SPIR-V C sources into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_diagonal_f64` and
    `omni_tensor_backend_vulkan_diagonal_matrix_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added C3 externs in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/diagonal` and
    `matrix/diagonal-matrix` route Vulkan `Float64` dense row-major inputs
    before the CPU-only storage check and return Vulkan-placed Tensor results.
  - Added availability-gated Vulkan regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/diagonal` and `matrix/diagonal-matrix` now support
    Vulkan-placed dense row-major `Float64` inputs.
  - Results remain Vulkan-placed tensors; CPU inspection still requires
    explicit `to-device 'cpu`.
  - CPU Tensor and Big* Tensor behavior remains on the existing CPU
    implementation.
  - Unsupported GPU/device cases fail closed rather than silently copying to CPU.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0` for both new shaders: passed.
  - `spirv-val --target-env vulkan1.0` for both generated SPIR-V files: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `matrix/diagonal` smokes returned `5.0`, `vulkan`,
    `vulkan`, and zero-size length `0`.
  - Direct Vulkan `matrix/diagonal-matrix` smokes returned `3.0`, `0.0`,
    `vulkan`, `vulkan`, and zero-size length `0`.
  - Direct CPU diagonal/diagonal-matrix smokes returned `5.0`, `3.0`, and
    `0.0`; rank-error smokes preserved the existing shape diagnostics.
  - Host focused `advanced-collections-module`: `pass=702 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=689 fail=0`.
- Unresolved issues / next actions:
  - Superseded by the later matrix trace entry for `matrix/trace`.
    Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    non-`Float64` Vulkan dtypes, and Vulkan unary scientific `map` remain
    unsupported.

Signature: Codex GPT-5

## 2026-04-17 03:05 CEST - TENSOR-100E Vulkan Matrix Transpose
- Objective attempted:
  - Continue `TENSOR-100E` with the next backend-neutral `Float64` kernel family
    by routing public `matrix/transpose` to Vulkan for eligible dense row-major
    rank-2 tensors.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_transpose_f64.comp` and generated
    `csrc/tensor_vulkan_transpose_f64_spv.c`.
  - Added `omni_tensor_backend_vulkan_transpose_f64` in
    `csrc/tensor_vulkan_helpers.c` and wired the SPIR-V source into
    `project.json` plus `scripts/build_omni_chelpers.sh`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/transpose` handles
    concrete Vulkan tensors before the CPU-only storage check and returns a
    Vulkan-placed result for dense row-major `Float64` rank-2 inputs.
  - Added availability-gated Vulkan transpose regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/transpose` now supports Vulkan-placed dense row-major
    `Float64` rank-2 tensors.
  - Results remain Vulkan-placed tensors; CPU inspection still requires
    explicit `to-device 'cpu`.
  - CPU Tensor and Big* Tensor transpose behavior remains on the existing CPU
    implementation.
  - Unsupported GPU/device cases fail closed rather than silently copying to CPU.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_transpose_f64.comp -o /tmp/omni_tensor_vulkan_transpose_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_transpose_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan transpose smokes returned `6.0`, `vulkan`, `vulkan`, and
    zero-size length `0`.
  - Direct CPU transpose smoke returned `6.0`.
  - Direct non-rank-2 transpose error smoke returned `tensor/shape-mismatch`.
  - Host focused `advanced-collections-module`: `pass=693 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=680 fail=0`.
