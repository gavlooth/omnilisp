# Session Report Index Part 14

Source: `.agents/SESSION_REPORT.md`

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
