# Session Report Index Part 13

Source: `.agents/SESSION_REPORT.md`

## 2026-04-17 07:12 CEST - TENSOR-100G Tiled-LU Solve Staging
- Objective attempted:
  - Add scratch-buffered per-pivot panel-factor staging to the parallel Vulkan
    `matrix/solve` path.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_solve_multi_factor_f64.comp` and generated
    `csrc/tensor_vulkan_solve_multi_factor_f64_spv.c`.
  - Wired the generated factor shader through `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Expanded `csrc/tensor_vulkan_helpers.c` so the multi-dispatch solve helper
    allocates a private factor scratch buffer, binds descriptor `5`, creates a
    factor pipeline, and uses four-buffer barriers.
  - Inserted a factor dispatch after row swap and before elimination. It
    stores `L[row,pivot]` in factor scratch and in the lower-triangular
    workspace; elimination now consumes staged factors.
  - Added `omni_tensor_backend_vulkan_solve_factor_stage_call_count` plus the
    matching C3 extern and focused counter assertion.
  - Added a dense 65x65 `I + J` Vulkan solve regression that exercises
    nonzero staged factors without LAPACK.
- Key results:
  - Public `matrix/solve` remains unchanged.
  - The serial `n < 3` Vulkan helper still uses its trailing `double` status
    sentinel; the parallel helper still uses private typed `uint` status.
  - Tiled-LU staging is landed as a `tile_width = 1` panel-factor stage.
    Remaining `TENSOR-100G` work is measurement-backed threshold routing and,
    later, full blocked trailing-update LU if measurements justify it.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0` passed for all affected
    solve-multi shaders.
  - `spirv-val --target-env vulkan1.0` passed for all affected solve-multi
    shaders.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned dense 9x9 value `1.0`, 65x65 identity value
    `1.0`, dense 65x65 `I + J` value `1.0`, and `tensor/singular-matrix`.
  - Host focused `advanced-collections-module`: `pass=801 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=788 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Replace the fixed `n >= 3` bring-up threshold with measurement-backed
    routing for serial and parallel staged Vulkan solve paths.

Signature: Codex GPT-5

## 2026-04-17 06:55 CEST - TENSOR-100G Cross-Workgroup Vulkan Pivot Reduction
- Objective attempted:
  - Remove the remaining one-workgroup pivot-selection boundary from the
    parallel Vulkan `matrix/solve` path.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Reworked `csrc/tensor_vulkan_solve_multi_pivot_f64.comp` into a pivot
    scan stage that writes per-workgroup candidates into scratch.
  - Added `csrc/tensor_vulkan_solve_multi_pivot_reduce_f64.comp`,
    `csrc/tensor_vulkan_solve_multi_pivot_commit_f64.comp`, and
    `csrc/tensor_vulkan_solve_multi_row_swap_f64.comp`, plus generated SPIR-V
    C sources.
  - Expanded the private multi-dispatch solve ABI in
    `csrc/tensor_vulkan_helpers.c` to bind a fifth descriptor for pivot
    magnitude scratch, allocate ping-pong scratch lanes, recursively reduce
    pivot candidates, commit the selected row, and run row swaps as their own
    dispatch before elimination.
  - Added `omni_tensor_backend_vulkan_solve_pivot_reduce_call_count` and the
    matching C3 extern.
  - Added a 65x65 Vulkan solve regression that proves the cross-workgroup
    pivot-reduction stage executes without LAPACK.
- Key results:
  - Public `matrix/solve` remains unchanged.
  - The serial `n < 3` Vulkan helper still uses its trailing `double` status
    sentinel; the parallel helper still uses private typed `uint` status.
  - Pivot selection now spans multiple workgroups. Remaining `TENSOR-100G`
    work is tiled LU staging plus measurement-backed thresholds.
  - An attempted in-dispatch cleanup of `A[row,pivot]` was removed because it
    raced elimination invocations that still read that column for row factors.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0` passed for all affected
    solve-multi shaders.
  - `spirv-val --target-env vulkan1.0` passed for all affected solve-multi
    shaders.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `0.2`, `3.0`, 65x65 value `1.0`, and
    `tensor/singular-matrix`.
  - First host focused `advanced-collections-module` run failed one dense 9x9
    test after the in-dispatch zeroing attempt changed the result to `1.35`.
    Removing that zeroing restored the suite.
  - Host focused `advanced-collections-module`: `pass=800 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=787 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Add tiled LU scratch-buffer staging for larger systems.
  - Replace the fixed `n >= 3` bring-up threshold with measured routing after
    the tiled path exists.

Signature: Codex GPT-5

## 2026-04-17 06:24 CEST - TENSOR-100G Multi-Dispatch Vulkan Solve
- Objective attempted:
  - Advance the parallel solver track from a staged two-dispatch helper to a
    multi-dispatch Vulkan solve path where elimination and RHS backsolve can
    span multiple workgroups.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_solve_multi_pivot_f64.comp`,
    `csrc/tensor_vulkan_solve_multi_eliminate_f64.comp`, and
    `csrc/tensor_vulkan_solve_multi_backsolve_f64.comp`, plus generated
    SPIR-V C sources for all three shaders.
  - Wired the generated SPIR-V sources into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Reworked the private Vulkan parallel solve helper in
    `csrc/tensor_vulkan_helpers.c` to record init/copy, per-pivot
    pivot/swap, per-pivot elimination, and descending per-row backsolve
    dispatches in one command buffer with buffer-memory barriers between
    stages.
  - Added `omni_tensor_backend_vulkan_solve_multi_dispatch_call_count` and the
    matching C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated advanced module tests so the 2x2 serial threshold keeps both
    parallel counters unchanged, the Vulkan parallel path increments the
    multi-dispatch counter without LAPACK, and a dense 9x9 `J + I` system
    exercises the multi-workgroup elimination path.
- Key results:
  - The public `matrix/solve` surface and routing contract remain unchanged.
  - The serial Vulkan `n < 3` helper still uses its private trailing `double`
    status sentinel; the parallel helper still uses private typed `uint`
    status storage.
  - Elimination and RHS backsolve now scale across workgroups. Pivot
    selection/swap is still a one-workgroup stage, so the next real solver
    boundary is cross-workgroup pivot reduction plus scratch-buffered/tiled LU.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0` passed for pivot,
    elimination, and backsolve shaders.
  - `spirv-val --target-env vulkan1.0` passed for pivot, elimination, and
    backsolve shaders.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned serial value `0.2`, pivoting value `3.0`,
    9x9 multi-RHS identity value `18.0`, dense 9x9 `J + I` value `1.0`, and
    `tensor/singular-matrix`.
  - Host focused `advanced-collections-module`: `pass=799 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=786 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Add cross-workgroup pivot reduction. The likely next ABI needs pivot
    magnitude and status scratch buffers instead of trying to dispatch the
    current one-workgroup pivot shader wider.
  - Add scratch-buffered/tiled LU and replace the fixed `n >= 3` bring-up
    threshold with measurements after the tiled path exists.

Signature: Codex GPT-5

## 2026-04-17 06:09 CEST - TENSOR-100G Staged Vulkan Parallel Solve
- Objective attempted:
  - Advance the parallel solver track from one monolithic compute shader to a
    staged Vulkan helper with an explicit inter-dispatch memory dependency.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_solve_parallel_init_f64.comp` and generated
    `csrc/tensor_vulkan_solve_parallel_init_f64_spv.c`.
  - Wired the new generated SPIR-V source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Updated the parallel solve helper in `csrc/tensor_vulkan_helpers.c` to
    create two shader modules and two pipelines, record init/copy and
    factor/solve dispatches into one command buffer, and issue a Vulkan 1.0
    buffer-memory barrier between them.
  - Kept the public C3 `matrix/solve` surface and routing unchanged: Vulkan
    `Float64` systems with `n >= 3` use the parallel helper, while `n < 3`
    remains on the serial Vulkan helper.
  - Added staged-path tests in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` for the 2x2 serial
    threshold counter, pivoting, 9x9 multi-RHS copy beyond one workgroup, and
    post-elimination singular diagnostics.
- Key results:
  - The staged helper now initializes the result/workspace and private `uint`
    status buffer in a separate dispatch before factor/back-solve begins.
  - The inter-dispatch barrier uses `COMPUTE_SHADER` pipeline-stage masks and
    `SHADER_WRITE -> SHADER_READ|SHADER_WRITE` access masks on the output and
    status buffers.
  - The serial solver's trailing `double` status sentinel remains separate and
    untouched.
  - The remaining solver work is tiled or multi-workgroup LU-style solving plus
    measurement-backed thresholds; the factor/back-solve stage is still a
    single-workgroup algorithm.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_solve_parallel_init_f64.comp -o /tmp/omni_tensor_vulkan_solve_parallel_init_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_solve_parallel_init_f64.spv`: passed.
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_solve_parallel_f64.comp -o /tmp/omni_tensor_vulkan_solve_parallel_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_solve_parallel_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned serial solve value `0.2`, staged solve value
    `1.0`, staged 9x9 multi-RHS value `18.0`, and
    `tensor/singular-matrix`.
  - Host focused `advanced-collections-module`: `pass=798 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=785 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Design and validate a tiled or multi-workgroup solver path that no longer
    depends on one workgroup's `shared` memory and `barrier()` semantics.
  - Replace the fixed `n >= 3` bring-up threshold with a measurement-backed
    threshold after the tiled path exists.

Signature: Codex GPT-5

## 2026-04-17 05:58 CEST - TENSOR-100G Vulkan Parallel Matrix Solve
- Objective attempted:
  - Start the parallel solver track by adding a real Vulkan parallel
    `matrix/solve` path behind the existing backend-neutral public surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_solve_parallel_f64.comp` and generated
    `csrc/tensor_vulkan_solve_parallel_f64_spv.c`.
  - Wired the generated SPIR-V source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_solve_parallel_f64`, a parallel-helper
    call counter, a private `uint` status reader, and a two-input/two-output
    Vulkan dispatch helper in `csrc/tensor_vulkan_helpers.c`.
  - Added C3 externs in `src/lisp/tensor_vulkan_backend.c3`.
  - Routed public Vulkan `matrix/solve` in
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
