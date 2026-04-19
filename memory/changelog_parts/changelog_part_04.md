# Memory Changelog Index Part 04

Source: `memory/CHANGELOG.md`

      passed

- Advanced `TENSOR-100G` tiled LU staging for Vulkan `Float64` solve:
  - Added `csrc/tensor_vulkan_solve_multi_factor_f64.comp` plus generated
    `csrc/tensor_vulkan_solve_multi_factor_f64_spv.c`, and wired the generated
    shader through `project.json` and `scripts/build_omni_chelpers.sh`.
  - Expanded the private multi-dispatch solve ABI with a factor scratch buffer
    at descriptor binding `5`, a factor pipeline, and four-buffer barriers
    covering output, typed status, pivot scratch, and factor scratch.
  - The command buffer now runs a per-pivot panel-factor dispatch after row
    swap and before elimination. The factor stage computes `L[row,pivot]`,
    stores it in factor scratch and the lower-triangular coefficient workspace,
    then elimination consumes the staged factor instead of recomputing it for
    every coefficient/RHS update.
  - Added `omni_tensor_backend_vulkan_solve_factor_stage_call_count` as a
    private test/diagnostic counter and a dense 65x65 `I + J` solve regression
    proving the factor stage executes without LAPACK.
  - Remaining `TENSOR-100G` work is measurement-backed threshold routing. The
    solver now has staged panel factors, cross-workgroup pivot selection, and
    multi-workgroup elimination/backsolve, but it is still not a fully blocked
    trailing-update LU.
  - validation:
    - all solve-multi pivot/reduce/commit/swap/factor/eliminate/backsolve
      shaders passed `glslangValidator -V --target-env vulkan1.0`
    - all affected solve-multi shaders passed `spirv-val --target-env
      vulkan1.0`
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/solve` smokes returned dense 9x9 value `1.0`,
      65x65 identity value `1.0`, dense 65x65 `I + J` value `1.0`, and
      `tensor/singular-matrix`
    - host focused `advanced-collections-module` -> `801 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `788 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100G` cross-workgroup pivot Vulkan `Float64` solve:
  - Split parallel pivot handling into scan, reduce, commit, and row-swap
    stages. `csrc/tensor_vulkan_solve_multi_pivot_f64.comp` now scans pivot
    candidates into scratch instead of doing one-workgroup scan/swap, and new
    `csrc/tensor_vulkan_solve_multi_pivot_reduce_f64.comp`,
    `csrc/tensor_vulkan_solve_multi_pivot_commit_f64.comp`, and
    `csrc/tensor_vulkan_solve_multi_row_swap_f64.comp` handle recursive
    candidate reduction, final pivot commit, and row swaps.
  - Added generated SPIR-V C sources for the new shader stages and wired them
    through `project.json` and `scripts/build_omni_chelpers.sh`.
  - Reworked the private multi-dispatch solve helper to allocate a private
    `double` pivot-magnitude scratch buffer and expanded typed `uint` status
    buffer with ping-pong candidate row lanes.
  - The command buffer now scans pivot candidates across workgroups, recursively
    reduces them through ping-pong scratch offsets with explicit barriers,
    commits the selected pivot row, runs row swap as a separate dispatch, then
    continues through elimination and backsolve.
  - Added `omni_tensor_backend_vulkan_solve_pivot_reduce_call_count` as a
    private test/diagnostic counter and a 65x65 Vulkan solve regression proving
    the cross-workgroup pivot-reduction path executes without LAPACK.
  - Negative memory: do not zero `A[row,pivot]` inside the same elimination
    dispatch that reads it to compute row factors. That in-dispatch zeroing
    raced the coefficient/RHS update invocations and changed the dense 9x9
    `J + I` solve result from `1.0` to `1.35`. If explicit cleanup is needed,
    it must be a separate dispatch after a barrier.
  - At this checkpoint, remaining `TENSOR-100G` work was tiled LU
    scratch-buffer staging and a measurement-backed threshold. Cross-workgroup
    pivot selection was landed, but the solver was still a straightforward
    Gaussian-elimination path rather than a blocked/tiled LU factorization.
  - validation:
    - all solve-multi pivot/reduce/commit/swap/eliminate/backsolve shaders
      passed `glslangValidator -V --target-env vulkan1.0`
    - all affected solve-multi shaders passed `spirv-val --target-env
      vulkan1.0`
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/solve` smokes returned `0.2`, `3.0`, `1.0` for the
      65x65 cross-workgroup case, and `tensor/singular-matrix`
    - an initial host focused run failed the dense 9x9 test after the
      in-dispatch zeroing attempt; removing that zeroing restored the result
    - host focused `advanced-collections-module` -> `800 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `787 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100G` multi-dispatch Vulkan `Float64` solve:
  - Added `csrc/tensor_vulkan_solve_multi_pivot_f64.comp`,
    `csrc/tensor_vulkan_solve_multi_eliminate_f64.comp`, and
    `csrc/tensor_vulkan_solve_multi_backsolve_f64.comp`, plus generated
    SPIR-V C sources for all three kernels.
  - Wired the new generated shader sources through `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Reworked the private parallel solve helper so
    `omni_tensor_backend_vulkan_solve_parallel_f64` now records a
    multi-dispatch command buffer: init/copy, per-pivot pivot/swap,
    per-pivot multi-workgroup elimination, and descending per-row backsolve.
  - Kept the public `matrix/solve` and C3 helper surface unchanged. The serial
    `n < 3` Vulkan helper still uses its private trailing `double` status
    sentinel; the parallel helper still uses private typed `uint` status.
  - Added `omni_tensor_backend_vulkan_solve_multi_dispatch_call_count` as a
    private test/diagnostic counter and wired it through the C3 Vulkan backend
    externs.
  - Added tests proving the serial 2x2 threshold stays off both parallel and
    multi-dispatch counters, the parallel path increments the multi-dispatch
    counter without LAPACK, and a dense 9x9 `J + I` system solves through the
    multi-workgroup elimination path.
  - Remaining `TENSOR-100G` work is full cross-workgroup pivot reduction,
    scratch-buffered/tiled LU, and measurement-backed thresholds. This slice
    parallelizes elimination and RHS backsolve dispatches across workgroups,
    but pivot selection/swap remains one workgroup for now.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed for pivot,
      elimination, and backsolve shaders
    - `spirv-val --target-env vulkan1.0` passed for pivot, elimination, and
      backsolve shaders
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/solve` smokes returned `0.2`, `3.0`, `18.0`,
      dense 9x9 value `1.0`, and `tensor/singular-matrix`
    - host focused `advanced-collections-module` -> `799 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `786 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100G` staged Vulkan `Float64` parallel matrix solve:
  - Added `csrc/tensor_vulkan_solve_parallel_init_f64.comp` and generated
    `csrc/tensor_vulkan_solve_parallel_init_f64_spv.c`, then wired the new
    SPIR-V source through `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Split the parallel solve path into two Vulkan compute dispatches recorded
    in one command buffer: an init/copy stage that fills
    `[solution/RHS][coefficient workspace]` plus a private typed status buffer,
    followed by the existing workgroup-parallel factor/back-solve stage.
  - Added a Vulkan 1.0 `vkCmdPipelineBarrier` path using buffer-memory
    barriers on the output workspace and private `uint` status buffer between
    the two dispatches. The helper uses `COMPUTE_SHADER` pipeline-stage masks
    and `SHADER_WRITE -> SHADER_READ|SHADER_WRITE` access masks.
  - Kept public `matrix/solve` routing unchanged: Vulkan dense row-major
    `Float64` systems with `n >= 3` use the parallel helper; `n < 3` remains
    on the serial Vulkan helper and still uses its private trailing `double`
    status sentinel.
  - Updated tests so the 2x2 serial threshold does not increment the parallel
    helper counter, staged pivoting is exercised, a 9x9 identity solve copies
    more than one 64-lane workgroup of coefficient data, post-elimination
    singular systems raise `tensor/singular-matrix`, and LAPACK remains unused
    on the Vulkan parallel path.
  - Remaining `TENSOR-100G` work is now tiled/multi-workgroup solver design and
    measurement-backed thresholds; the staged barrier helper is landed but is
    still a single-workgroup factor/back-solve algorithm.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed for init and
      factor/solve shaders
    - `spirv-val --target-env vulkan1.0` passed for init and factor/solve
      shaders
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/solve` smokes returned serial value `0.2`,
      staged value `1.0`, staged 9x9 multi-RHS value `18.0`, and
      `tensor/singular-matrix`
    - host focused `advanced-collections-module` -> `798 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `785 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100G` first Vulkan `Float64` parallel matrix solve slice:
  - Added `csrc/tensor_vulkan_solve_parallel_f64.comp` and generated
    `csrc/tensor_vulkan_solve_parallel_f64_spv.c`, then wired the generated
    SPIR-V source through `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_solve_parallel_f64` plus
    `omni_tensor_backend_vulkan_solve_parallel_call_count` in the
    runtime-loaded Vulkan helper layer, and exposed them through
    `src/lisp/tensor_vulkan_backend.c3`.
  - Added a two-input/two-output Vulkan dispatch helper for kernels that bind
    coefficient input, RHS input, result/workspace output, and a private status
    output. The parallel solve path uses an explicit private `uint` status
    buffer instead of extending the serial solver's trailing `double` status
    sentinel.
  - Routed public `matrix/solve` for Vulkan dense row-major `Float64` systems
    with `n >= 3` through the new parallel helper. The existing serial Vulkan
    solve helper remains the small-size path for `n < 3`.
  - Implemented a 64-invocation single-workgroup partial-pivot Gaussian
    elimination shader. It parallelizes pivot reduction, row swaps,
    elimination updates, and RHS-column back-substitution while preserving the
    existing public `matrix/solve` contract.
  - Added availability-gated tests for the parallel Vulkan vector RHS path,
    rank-1 preservation, matrix RHS path, rank-2/shape preservation, singular
    diagnostics, helper call-count movement, and unchanged LAPACK `dgesv`
    counter.
  - The remaining `TENSOR-100G` boundary is a staged/tiled multi-kernel solver
    helper with explicit Vulkan memory barriers and a measurement-backed
    threshold. Do not treat this first single-workgroup slice as the final
    large-system solver.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/solve` smokes returned serial value `0.2`,
      parallel value `1.0`, and `tensor/singular-matrix`
    - host focused `advanced-collections-module` -> `793 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `780 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100F1` Vulkan `Float64` matrix QR:
  - Added `csrc/tensor_vulkan_qr_f64.comp` and generated
    `csrc/tensor_vulkan_qr_f64_spv.c`, then wired the SPIR-V source through
    `project.json` and `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_qr_f64` in the runtime-loaded Vulkan
    helper plus a one-input/two-output dispatch helper so QR can return
    independent Vulkan buffers for `q` and `r`.
  - The Vulkan helper computes reduced modified Gram-Schmidt QR on Vulkan for
    dense row-major rank-2 `Float64` input with rows greater than or equal to
    columns. `q` is stored in a Vulkan buffer of shape `[rows columns]`; `r`
    is stored in a Vulkan buffer of shape `[columns columns]` with a private
    trailing status word that is not exposed through the public Tensor shape.
  - Routed public `matrix/qr` for concrete or realized Vulkan-placed dense
    row-major `Float64` input through the new helper before the CPU-only
    storage check. CPU tensors keep the existing dgeqrf-then-pure
    implementation; Vulkan inputs do not silently copy to CPU or call LAPACK.
  - Added a near-zero QR diagonal guard in the shader to preserve the existing
    public rank-deficient diagnostic for the canonical dependent-column test.
  - Added availability-gated regressions for Vulkan QR factor values, output
    placement, output shapes, lazy Vulkan input, empty result placement,
    wide-input diagnostics, rank-deficient diagnostics, and no-LAPACK path
    usage. Added CPU empty QR shape coverage.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/qr` smokes returned `1.0`, `1.0`, `vulkan`,
      `vulkan`, zero-size shape `0`, and `tensor/singular-matrix`
    - host focused `advanced-collections-module` -> `787 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `774 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100F1` Vulkan `Float64` matrix Cholesky:
  - Added `csrc/tensor_vulkan_cholesky_f64.comp` and generated
    `csrc/tensor_vulkan_cholesky_f64_spv.c`, then wired the SPIR-V source
    through `project.json` and `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_cholesky_f64` in the runtime-loaded
    Vulkan helper. The helper computes a lower-triangular Cholesky factor on
    Vulkan for dense row-major square `Float64` input, keeps the factor
    payload in a Vulkan buffer, and copies back only trailing status metadata
    for nonsymmetric or non-SPD detection.
  - Factored shared trailing-status copyback for serial Vulkan solve, inverse,
    and Cholesky helpers.
  - Routed public `matrix/cholesky` for concrete or realized Vulkan-placed
    dense row-major square `Float64` input through the new helper before the
    CPU-only storage check. CPU tensors keep the existing dpotrf-then-pure
    implementation; Vulkan inputs do not silently copy to CPU or call LAPACK.
  - Fixed the CPU pure Cholesky fallback to accept empty square tensors whose
    zero-size backing data pointer is null.
  - Added availability-gated regressions for Vulkan Cholesky factor values,
    upper-zeroing, Vulkan result placement, lazy Vulkan input, empty square
    result placement, non-square diagnostics, nonsymmetric and indefinite
    diagnostics, and no-LAPACK path usage.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/cholesky` smokes returned `1.0`, `2.0`, `0.0`,
      `vulkan`, `vulkan`, zero-size shape `0`,
      `tensor/not-positive-definite`, `tensor/not-positive-definite`, and
      `tensor/shape-mismatch`
    - direct CPU empty Cholesky shape smoke returned `0`
    - host focused `advanced-collections-module` -> `775 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `762 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100E` Vulkan `Float64` matrix solve:
  - Added `csrc/tensor_vulkan_solve_f64.comp` and generated
    `csrc/tensor_vulkan_solve_f64_spv.c`, then wired the SPIR-V source
    through `project.json` and `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_solve_f64` in the runtime-loaded Vulkan
    helper. The helper performs Gaussian elimination with partial pivoting on
    Vulkan for square coefficient tensors and rank-1 or rank-2 right-hand
    tensors, keeps the solution payload in a Vulkan buffer, and copies back
    only trailing status metadata for singular detection.
  - Routed public `matrix/solve` for concrete or realized Vulkan-placed dense
    row-major square `Float64` coefficient tensors and Vulkan-placed matching
    `Float64` right-hand tensors through the new helper before the CPU-only
    storage check. CPU tensors keep the existing dgesv-then-pure
    implementation; Vulkan inputs do not silently copy to CPU or call LAPACK.
  - The public result shape is unchanged: rank-1 RHS returns a rank-1
    solution Tensor and rank-2 RHS returns a rank-2 solution Tensor. For
    Vulkan inputs, the returned solution remains Vulkan-placed and must be
    copied back explicitly with `to-device 'cpu` for CPU inspection.
  - Added availability-gated regressions for Vulkan solve vector RHS, matrix
    RHS, Vulkan result placement, lazy Vulkan coefficients, lazy Vulkan RHS,
    singular diagnostics, mismatched RHS diagnostics, mixed-device
    fail-closed diagnostics, empty-system singular behavior, and no-LAPACK
    path usage.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/solve` smokes returned `0.2`, `0.6`, `vulkan`,
      `1.0`, `0.2`, `0.6`, `tensor/singular-matrix`,
      `tensor/shape-mismatch`, `tensor/backend-unsupported`, and
      `tensor/singular-matrix`
    - host focused `advanced-collections-module` -> `763 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `750 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100E` Vulkan `Float64` matrix inverse:
  - Added `csrc/tensor_vulkan_inverse_f64.comp` and generated
    `csrc/tensor_vulkan_inverse_f64_spv.c`, then wired the SPIR-V source
    through `project.json` and `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_inverse_f64` in the runtime-loaded
    Vulkan helper. The helper performs Gauss-Jordan elimination with partial
    pivoting on Vulkan, keeps the inverse payload in a Vulkan buffer, and
    copies back only trailing status metadata for singular detection.
  - Routed public `matrix/inverse` for concrete or realized Vulkan-placed
    dense row-major square `Float64` inputs through the new helper before the
    CPU-only storage check. CPU tensors keep the existing dgesv-then-pure
    implementation; Vulkan inputs do not silently copy to CPU or call LAPACK.
  - The public result shape is unchanged: a same-shape `Float64` Tensor
    inverse for nonsingular input. For Vulkan input, the returned inverse
    remains Vulkan-placed and must be copied back explicitly with
    `to-device 'cpu` for CPU inspection.
  - Added availability-gated regressions for Vulkan inverse values, product
    identity, Vulkan result placement, lazy Vulkan input, empty square
    results, singular diagnostics, non-square diagnostics, and no-LAPACK path
    usage.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/inverse` smokes returned `0.6`, `-0.7`,
      `vulkan`, `tensor/singular-matrix`, `0.4`, `vulkan`,
      `tensor/shape-mismatch`, and `1.0`
    - host focused `advanced-collections-module` -> `751 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `738 passed, 0 failed`

- Advanced `TENSOR-100E` Vulkan `Float64` matrix LU factorization:
  - Added `csrc/tensor_vulkan_lu_f64.comp` and generated
    `csrc/tensor_vulkan_lu_f64_spv.c`, then wired the SPIR-V source through
    `project.json` and `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_lu_f64` in the runtime-loaded Vulkan
    helper. The helper performs partial-pivot LU on Vulkan, returns the
    combined LU factor as a Vulkan buffer, and copies back only the `pivots`
    plus `swap-count` metadata required by the public dictionary contract.
  - Routed public `matrix/lu` for concrete or realized Vulkan-placed dense
    row-major square `Float64` inputs through the new helper before the
    CPU-only storage check. CPU tensors keep the existing LAPACK-then-pure
    implementation; Vulkan inputs do not silently copy to CPU or call LAPACK.
  - The public result shape is unchanged: a dictionary with `lu`, `pivots`,
    and `swap-count`. For Vulkan input, `lu` remains a Vulkan-placed Tensor;
    metadata values are host-side `Array`/`Integer` values.
  - Added availability-gated regressions for Vulkan LU factor values, pivot
    order, swap count, Vulkan factor placement, lazy Vulkan input, empty square
    factors, singular diagnostics, non-square diagnostics, and no-LAPACK path
    usage.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/lu` smokes returned `0.5`, `-0.5`, `1`, `1`,
      `vulkan`, `3.0`, `vulkan`, `0`, `tensor/singular-matrix`, and
      `tensor/shape-mismatch`
    - host focused `advanced-collections-module` -> `740 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `727 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100E` Vulkan `Float64` matrix determinant reducer:
  - Added `csrc/tensor_vulkan_determinant_f64.comp` and generated
    `csrc/tensor_vulkan_determinant_f64_spv.c`, then wired the SPIR-V source
    through `project.json` and `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_determinant_f64` in the runtime-loaded
    Vulkan helper. The helper performs partial-pivot LU on a Vulkan work
    buffer and copies back only the scalar determinant payload required by the
    public `matrix/determinant` contract.
  - Routed public `matrix/determinant` for concrete or realized Vulkan-placed
    dense row-major square `Float64` inputs through the new helper before the
    CPU-only storage check. CPU tensors keep the existing LAPACK-then-pure
    implementation; Vulkan inputs do not silently copy to CPU or call LAPACK.
  - Preserved CPU determinant edge semantics on Vulkan: singular matrices
    return `0.0`, empty square matrices return `1.0`, and non-finite
    determinant payloads are not rejected after scalar readback.
  - Added availability-gated regressions for Vulkan determinant values,
    pivot-parity behavior, singular matrices, lazy Vulkan input, empty square
    matrices, non-square diagnostics, and no-LAPACK path usage.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/determinant` smokes returned `-2.0`, `-2.0`,
      `0.0`, `-2.0`, `1.0`, and `tensor/shape-mismatch`
    - direct CPU determinant smoke returned `-2.0`
    - direct Vulkan transpose regression smoke returned `6.0`
    - host focused `advanced-collections-module` -> `730 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `717 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100E` Vulkan `Float64` matrix rank reducer:
  - Added `csrc/tensor_vulkan_rank_f64.comp` and generated
    `csrc/tensor_vulkan_rank_f64_spv.c`, then wired the SPIR-V source through
    `project.json` and `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_rank_f64` in the runtime-loaded Vulkan
    helper. The helper computes a partial-pivot row-elimination rank on Vulkan
    and copies back only the scalar rank payload required by the public
    `matrix/rank` contract.
  - Routed public `matrix/rank` for concrete or realized Vulkan-placed dense
    row-major `Float64` rank-2 inputs through the new helper before the
    CPU-only storage check. CPU tensors keep the existing LAPACK-then-pure
    implementation; Vulkan inputs do not silently copy to CPU or call LAPACK.
  - Added availability-gated regressions for Vulkan full/deficient rank,
    lazy Vulkan input, tolerance behavior, empty rows/columns, rank-2 shape
    diagnostics, and no-LAPACK path usage.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/rank` smokes returned `2`, `1`, `2`, `1`, `2`,
      `0`, `0`, and `tensor/shape-mismatch`
    - direct CPU rank smoke returned `2`
    - host focused `advanced-collections-module` -> `722 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `709 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100E` Vulkan `Float64` matrix norm reducers:
  - Added `csrc/tensor_vulkan_norm_f64.comp` as the checked-in GLSL source for
    dense row-major Vulkan `Float64` `matrix/norm` direct reducers.
  - Added `csrc/tensor_vulkan_norm_f64_spv.c`, generated from that GLSL
    source, and wired it through `project.json` plus
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_norm_f64` in the runtime-loaded Vulkan
    helper. The helper computes on Vulkan and copies back only the single
    `Float64` scalar result required by the public `matrix/norm` contract.
  - Routed public `matrix/norm` for concrete or realized Vulkan-placed dense
    row-major `Float64` rank-2 inputs through the new helper before the
    CPU-only storage check.
  - At this checkpoint, Vulkan supported default/`'frobenius`, `'one`,
    `'infinity`, and `'max`; a later `TENSOR-100F1` slice extended
    `'spectral` and `'nuclear` through the Vulkan singular-value helper.
  - Kept the Frobenius reduction numerically aligned with the CPU shape by
    using a scaled sum-of-squares payload in the shader and applying the final
    square root after scalar readback.
  - Added availability-gated regressions for Vulkan norm selectors, lazy
    Vulkan input realization, empty input, and SVD-selector fail-closed
    behavior.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/norm` smokes returned `5.0`,
      `9.53939201416946`, `9.0`, `15.0`, `6.0`, `5.0`, `0.0`, and
      `tensor/backend-unsupported`
    - host focused `advanced-collections-module` -> `712 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `699 passed, 0 failed`

- Advanced `TENSOR-100E` Vulkan `Float64` matrix trace:
  - Added `csrc/tensor_vulkan_trace_f64.comp` as the checked-in GLSL source
    for dense row-major Vulkan `Float64` square-matrix trace.
  - Added `csrc/tensor_vulkan_trace_f64_spv.c`, generated from that GLSL
    source, and wired it through `project.json` plus
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_trace_f64` in the runtime-loaded Vulkan
    helper. The helper computes on Vulkan and copies back only the single
    `Float64` scalar result required by the public `matrix/trace` contract.
  - Routed public `matrix/trace` for concrete or realized Vulkan-placed dense
    row-major square `Float64` inputs through the new helper before the CPU-only
    storage check. CPU tensors and Big* tensors keep the existing CPU scalar
    implementation; unsupported GPU/device cases fail closed instead of
    copying the input tensor silently.
  - Added availability-gated regressions for Vulkan trace scalar result, lazy
    Vulkan input realization, and empty-square trace.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/trace` smokes returned `5.0`, `5.0`, and `0.0`
    - direct CPU trace smoke still returned `5.0`
    - direct non-rank-2 trace error smoke preserved the existing shape
      diagnostic
    - host focused `advanced-collections-module` -> `705 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `692 passed, 0 failed`

- Advanced `TENSOR-100E` Vulkan `Float64` matrix diagonal structural pair:
  - Added `csrc/tensor_vulkan_diagonal_f64.comp` and
    `csrc/tensor_vulkan_diagonal_matrix_f64.comp` as checked-in GLSL sources
    for dense row-major Vulkan `Float64` diagonal extraction and diagonal
    matrix construction.
  - Added generated SPIR-V C sources for both kernels and wired them through
    `project.json` plus `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_diagonal_f64` and
    `omni_tensor_backend_vulkan_diagonal_matrix_f64` in the runtime-loaded
    Vulkan helper, using the input tensor's existing Vulkan context and
    probing Vulkan/Float64 support before zero-size success.
  - Routed public `matrix/diagonal` and `matrix/diagonal-matrix` for concrete
    or realized Vulkan-placed dense row-major `Float64` inputs through the new
    helpers. CPU tensors and Big* tensors keep the existing CPU implementation;
    unsupported GPU/device cases fail closed instead of copying silently.
  - Added availability-gated regressions for Vulkan diagonal roundtrip,
    placement preservation, lazy Vulkan input realization, off-diagonal zero
    fill, and zero-size copyback.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed for both shaders
    - `spirv-val --target-env vulkan1.0` passed for both shaders
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/diagonal` smokes returned `5.0`, `vulkan`,
      `vulkan`, and zero-size length `0`
    - direct Vulkan `matrix/diagonal-matrix` smokes returned `3.0`, `0.0`,
      `vulkan`, `vulkan`, and zero-size length `0`
    - direct CPU diagonal/diagonal-matrix smokes still returned `5.0`, `3.0`,
      and `0.0`
    - direct rank-error smokes preserved the existing shape diagnostics
    - host focused `advanced-collections-module` -> `702 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `689 passed, 0 failed`

- Advanced `TENSOR-100E` Vulkan `Float64` matrix transpose:
  - Added `csrc/tensor_vulkan_transpose_f64.comp` as the checked-in GLSL source
    for dense row-major Vulkan `Float64` rank-2 transpose.
  - Added `csrc/tensor_vulkan_transpose_f64_spv.c`, generated from that GLSL
    source, and wired it through `project.json` plus
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_transpose_f64` in the runtime-loaded
    Vulkan helper with a two-buffer SPIR-V dispatch on the input tensor's
    existing Vulkan context.
  - Routed public `matrix/transpose` for concrete Vulkan-placed dense row-major
    `Float64` rank-2 tensors through the new helper. CPU tensors and Big*
    tensors keep the existing CPU implementation; unsupported GPU/device cases
    fail closed instead of copying silently.
  - Added availability-gated regressions for Vulkan transpose roundtrip,
    placement preservation, lazy Vulkan input realization, and zero-size
    transpose copyback.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan transpose smokes returned `6.0`, `vulkan`, `vulkan`, and
      zero-size length `0`
    - direct CPU transpose smoke still returned `6.0`
    - direct non-rank-2 transpose error smoke returned `tensor/shape-mismatch`
    - host focused `advanced-collections-module` -> `693 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `680 passed, 0 failed`
    - primitive docs parity and Stage 3 source parity passed
    - `git diff --check` passed

- Advanced `TENSOR-100E` Vulkan rank-N multi-axis contract:
  - Extended the Vulkan `Float64` dense row-major `contract` path from exactly
    one contracted axis pair to one or more explicit contracted axis pairs.
  - Updated `csrc/tensor_vulkan_contract_f64.comp` so the metadata-buffer shader
    stores left/right contracted-axis lists and flattens the full contracted
    coordinate space inside each output element.
  - Regenerated `csrc/tensor_vulkan_contract_f64_spv.c`.
  - Updated `omni_tensor_backend_vulkan_contract_f64`, the C3 Vulkan extern, and
    the public Vulkan `contract` gate to pass axis-list metadata while preserving
    free-left-then-free-right output axis order.
  - Added availability-gated regressions for scalar and rank-3 multi-axis
    Vulkan contractions.
  - At this checkpoint, unsupported dtypes, mixed CPU/Vulkan operands,
    zero-axis contractions, and unsupported layouts still failed closed with
    Tensor backend diagnostics. The zero-axis restriction was superseded by
    the later Vulkan zero-axis contract slice on 2026-04-17.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan smokes returned `70.0` for a scalar multi-axis contraction,
      `210.0` for a rank-3 multi-axis contraction, `460.0` for the existing
      rank-N single-axis contraction, `8.0` for binary `map +`, and
      `tensor/backend-unsupported` for unsupported Vulkan `map sqrt`
    - host focused `advanced-collections-module` -> `689 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `676 passed, 0 failed`

- Advanced `TENSOR-100E` Vulkan rank-N single-axis contract:
  - Replaced the fixed rank-1/rank-2 Vulkan `Float64` contract shader with a
    rank/shape/stride metadata-buffer shader for dense row-major tensors.
  - Regenerated `csrc/tensor_vulkan_contract_f64_spv.c` from the checked-in
    `csrc/tensor_vulkan_contract_f64.comp` source.
  - Extended `omni_tensor_backend_vulkan_contract_f64` and the C3 Vulkan
    extern to pass operand/output rank, shape, and stride metadata.
  - Relaxed the public Vulkan `contract` dispatch gate to accept dense
    row-major `Float64` tensors of arbitrary rank with exactly one contracted
    axis, preserving output order as free left axes followed by free right
    axes.
  - Fixed the generic contract descriptor pool size to match the four-buffer
    descriptor layout used by left, right, output, and metadata buffers.
  - Multi-axis contractions, unsupported layouts/dtypes, and mixed CPU/Vulkan
    operands still fail closed with Tensor backend diagnostics.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan rank-N contract smokes returned `460.0`, `139.0`,
