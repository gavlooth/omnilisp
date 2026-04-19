# Session Report Index Part 09

Source: `.agents/SESSION_REPORT.md`

## 2026-04-17 22:33 CEST - TENSOR-100F Vulkan Float32 Large-Dense SVD Robustness

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and close the
    previously deferred robust large-dense Vulkan `Float32` SVD blocker.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan `Float32` singular-values/SVD shaders,
    focused advanced tests, TODO, changelog, plans, and tensor area docs.
- Code or configuration changes made:
  - Updated `csrc/tensor_vulkan_singular_values_f32.comp` and
    `csrc/tensor_vulkan_svd_f32.comp` to use scale-aware negative-eigenvalue
    tolerance before singular-value extraction.
  - Updated `csrc/tensor_vulkan_svd_f32.comp` to fill orthonormal completion
    columns for zero-singular-value vector normalization in tall/square and
    wide branches instead of failing the dispatch.
  - Regenerated the corresponding SPIR-V C embeds.
  - Added availability-gated tests for `65x65` zero, identity/diagonal, and
    all-ones/rank-deficient Vulkan `Float32` singular-values/SVD paths,
    SVD-backed norms, all-ones reconstruction, and unchanged LAPACK `dgesvd`
    counters.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`, plan docs,
    and tensor area docs so robust large-dense Vulkan `Float32` SVD is no
    longer listed as an open correctness blocker.
- Commands run and key results:
  - `glslangValidator` and `spirv-val` passed for
    `csrc/tensor_vulkan_singular_values_f32.comp` and
    `csrc/tensor_vulkan_svd_f32.comp`.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `Float32` smokes returned `(65 64.9999694824219 0.0)`,
    `(vulkan vulkan vulkan 64.9999694824219 0.0)`,
    `(64.9999694824219 65.1197662353516)`, and `(65 1.0 1.0)`.
  - Host focused `advanced-collections-module`: `pass=1177 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=1164 fail=0`.
  - Primitive docs parity and Stage 3 source parity: passed.
  - Targeted `git diff --check`: passed after mechanical whitespace cleanup
    of regenerated SPIR-V C embeds.
- Invalidated assumptions or failed approaches worth preserving:
  - Supersedes the earlier dense all-ones `65x65` Vulkan `Float32`
    single-dispatch SVD failure note. That case is now fixed by scale-aware
    eigenvalue tolerance plus orthonormal completion; do not resume staged or
    tiled `Float32` SVD as a correctness blocker unless new validation
    regresses it.
- Current best recommendation / checkpoint:
  - Continue from CPU `Float32` factor/SVD contracts, scalar/CUDA `Float32`,
    fixed-width complex, or stride/view-backed Vulkan layout work.
- Unresolved issues:
  - Future tiled/multi-dispatch SVD remains a performance-oriented follow-up,
    not the current correctness boundary.
- Signature: Codex GPT-5.4

## 2026-04-17 22:16 CEST - TENSOR-100F Vulkan Float32 Staged Parallel Solve Slice

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and land native
    staged parallel Vulkan `Float32` `matrix/solve` routing without hidden
    `Float64` widening, LAPACK fallback, or Vulkan operand copy-to-CPU
    fallback.
- Workspace/target:
  - `/home/christos/Omni`, staged Vulkan solve shaders/helpers, public
    matrix dispatch, focused advanced tests, TODO, and design/status artifacts.
- Code or configuration changes made:
  - Added dedicated `Float32` ports of the staged Vulkan solve shader family
    (`solve_parallel_init`, legacy `solve_parallel`, pivot scan/reduce/commit,
    row swap, factor, eliminate, and backsolve), generated SPIR-V C embeds,
    and helper build/project wiring.
  - Reworked the staged C helper into an explicitly dtype-aware dispatch ABI
    and added `omni_tensor_backend_vulkan_solve_parallel_f32`.
  - Added dtype-specific solve counters for `Float32` and `Float64` serial,
    staged dispatch, pivot-reduce, and factor-stage paths.
  - Updated public `matrix/solve` routing so Vulkan dense row-major `Float32`
    systems below `65` stay serial and systems with `n >= 65` use the staged
    helper. `Float32` staged shaders use a `1e-6` singularity tolerance and do
    not route through the `Float64` staged helper.
  - Added focused availability-gated tests for `2x2`/`9x9` serial routing,
    `65x65` identity and dense staged routing, matrix RHS shape/rank, staged
    singular status, mixed dtype rejection, unchanged `Float64` counters, and
    unchanged LAPACK `dgesv` counters.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md` so staged
    `Float32` solve is no longer listed as deferred.
- Commands run and key results:
  - `glslangValidator` and `spirv-val` passed for the nine `Float32` staged
    solve shaders.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `Float32` staged smokes returned `(vulkan "Float32" 1.0)`,
    dense `65x65` value `1.00000011920929`, and `tensor/singular-matrix`.
  - Local threshold probes recorded under
    `build/vulkan_solve_f32_threshold_20260417_*`: staged and forced-serial
    `65x65` identity/dense timings were tied within measurement noise
    (`0.92s/0.89s` staged vs `0.94s/0.89s` forced serial for the
    higher-iteration probes), so `65` is a parity threshold, not a decisive
    speedup claim.
  - Host focused `advanced-collections-module`: passed, `pass=1166 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1153 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating staged Vulkan `Float32` solve as deferred. It now has
    dedicated `_f32` staged shaders and dtype-specific route/counter coverage.
  - Do not claim a decisive `Float32` staged solve speedup from the local
    timing evidence; the measured result is parity at `65` on this stack.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100F` from robust large-dense Vulkan `Float32` SVD
    execution or from separate CPU `Float32` factor/SVD, CUDA `Float32`,
    scalar `Float32`, or fixed-width complex contracts.
- Unresolved issues:
  - Robust large-dense Vulkan `Float32` SVD remains deferred because dense
    all-ones `65x65` single-dispatch SVD previously failed on this stack while
    `65x65` zero matrices succeeded.
  - CPU `Float32` factor/SVD routines, CUDA `Float32` placement, scalar
    `Float32` values, and fixed-width complex Tensor storage remain
    fail-closed/deferred.
- Signature: Codex GPT-5.4

## 2026-04-17 21:46 CEST - TENSOR-100F Vulkan Float32 Serial Factor/Solve Slice

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and land native
    Vulkan `Float32` serial factor/solve support for `matrix/determinant`,
    `matrix/lu`, `matrix/solve`, `matrix/inverse`, `matrix/cholesky`, and
    `matrix/qr` without hidden `Float64` widening, LAPACK fallback, or Vulkan
    operand copy-to-CPU fallback.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Tensor factor/solve shaders/helpers, public
    matrix dispatch, focused advanced tests, TODO, and design/status artifacts.
- Code or configuration changes made:
  - Added dedicated Vulkan `Float32` factor/solve shader/SPIR-V sources for
    determinant, LU, solve, inverse, Cholesky, and QR, plus helper build and
    `project.json` wiring.
  - Added C helper exports and C3 externs for the new `_f32` paths, including
    Float32 tail-status handling and three-buffer dispatch plumbing.
  - Updated `src/lisp/prim_tensor_matrix.c3` so eligible Vulkan-placed dense
    row-major `Float32` operands route through native serial `_f32` helpers
    for the six factor/solve surfaces. Tensor outputs preserve Vulkan
    placement and `Float32` dtype; determinant and LU metadata keep existing
    public scalar/host metadata contracts.
  - Kept CPU `Float32` factor/solve routines fail-closed and kept staged
    parallel solve routing `Float64`-only. Staged parallel `Float32`
    solve/performance parity is now an explicit TODO follow-up, not an
    implicit fallback through the Float64 staged helper.
  - Added availability-gated tests for values, dtype/device preservation, lazy
    operands, singular/rank-deficient/non-SPD diagnostics, CPU `Float32`
    fail-closed behavior, and no-LAPACK counter preservation across the six
    surfaces.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md` so serial
    factor/solve is no longer listed as wholly fail-closed.
- Commands run and key results:
  - Shader worker validation: `glslangValidator` and `spirv-val` passed for
    all six new `Float32` factor/solve shaders.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `Float32` smokes returned `-2.0`,
    `0.199999988079071`, `"Float32"`, `0.5`, `2.0`, and `1.0`.
  - Host focused `advanced-collections-module`: passed, `pass=1156 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1143 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed before this report entry.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating Vulkan `Float32` factor/solve as entirely
    fail-closed. Eligible dense row-major Vulkan `Float32` operands now have
    serial native helper paths for determinant, LU, solve, inverse, Cholesky,
    and QR.
  - Do not assume the existing staged parallel `Float64` solve threshold or
    helper applies to `Float32`. `Float32` solve correctness is serial in this
    checkpoint; staged parallel `Float32` solve is a performance-parity
    follow-up requiring dtype-specific helper/status/threshold validation.
- Current best recommendation / checkpoint:
  - Continue from staged parallel Vulkan `Float32` solve/performance parity,
    robust large-dense `Float32` SVD execution, CPU `Float32` factor/SVD
    contract work, CUDA `Float32` placement, scalar `Float32` values, or the
    fixed-width complex dtype lane.
- Unresolved issues:
  - Staged parallel Vulkan `Float32` solve/performance parity remains deferred.
  - Robust large-dense Vulkan `Float32` SVD remains deferred because dense
    all-ones `65x65` single-dispatch SVD previously failed on this stack while
    `65x65` zero matrices succeeded.
  - CPU `Float32` factor/SVD routines, CUDA `Float32` placement, and scalar
    `Float32` values remain fail-closed.
- Signature: Codex GPT-5.4

## 2026-04-17 21:17 CEST - TENSOR-100F Vulkan Float32 SVD-backed Slice

- Objective attempted:
  - Continue `TENSOR-100F` by landing native Vulkan `Float32` SVD-backed
    support for `matrix/norm` `'spectral`/`'nuclear`,
    `matrix/singular-values`, and direct `matrix/svd` without hidden
    CPU/LAPACK fallback or hidden `Float64` widening.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Float32 singular-value/SVD shaders, C helper
    dispatch, public matrix routing, advanced collection tests, TODO, docs,
    and planning artifacts.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_singular_values_f32.comp`,
    `csrc/tensor_vulkan_svd_f32.comp`, generated `_spv.c` sources, build
    manifest entries, C helper exports, and C3 externs.
  - Routed Vulkan-placed dense row-major `Float32` operands through dedicated
    native `_f32` singular-value/SVD helpers. Tensor outputs preserve Vulkan
    placement and `Float32` dtype; norm scalars keep the public `Float64`
    return contract.
  - Kept CPU `Float32` `matrix/singular-values`, CPU `Float32` `matrix/svd`,
    and CPU `Float32` spectral/nuclear norm fail-closed before CPU
    double/LAPACK workspaces are reached.
  - Renamed shared multi-output Vulkan dispatch helpers from misleading `_f64`
    names to dtype-neutral byte-sized names and added a Vulkan `Float32` SVD
    over-previous-max-k/no-LAPACK regression.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md` so SVD-backed
    Vulkan `Float32` support is no longer listed as deferred.
- Commands run and key results:
  - `glslangValidator` / `spirv-val` for
    `csrc/tensor_vulkan_singular_values_f32.comp` and
    `csrc/tensor_vulkan_svd_f32.comp`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `Float32` smokes returned `3.0` for spectral norm, `2.0`
    for a copied-back singular value, and `"Float32"` for `matrix/svd` output
    dtype.
  - Host focused `advanced-collections-module`: passed, `pass=1121 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1108 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not continue from the direct-reducer checkpoint assumption that Vulkan
    `Float32` SVD-backed selectors and outputs are fail-closed. Eligible dense
    row-major Vulkan `Float32` inputs now have real shader/helper paths.
  - A dense all-ones 65x65 Vulkan `Float32` single-dispatch SVD smoke returned
    `tensor/backend-execution-failed` on this Vulkan stack while 65x65 zero
    matrices succeed. Do not treat the current shader as the robust
    large-dense performance/correctness baseline; prefer staged/tiled or
    measured large-dense SVD work for that regime.
- Current best recommendation / checkpoint:
  - Continue from Vulkan `Float32` factor/solve kernels, robust large-dense
    SVD execution, CPU `Float32` SVD contract work, CUDA `Float32` placement,
    or scalar `Float32` values.
- Unresolved issues:
  - Vulkan `Float32` factor/solve kernels remain fail-closed.
  - CPU `Float32` `matrix/singular-values`, `matrix/svd`, and SVD-backed
    norm selectors remain fail-closed by design.
  - CUDA `Float32` placement, scalar `Float32` values, fixed-width complex,
    and stride/view-backed Vulkan layouts remain deferred.
- Signature: Codex GPT-5.4

## 2026-04-17 18:57 CEST - TENSOR-100F Vulkan Symmetric Eigen Large-n

- Objective attempted:
  - Continue `TENSOR-100F` by replacing the old `n <= 64` Vulkan
    `matrix/eigenvalues` / `matrix/eigenvectors` private-array cap with a
    storage-backed larger-size path while preserving backend-neutral public
    surfaces and no hidden CPU/LAPACK fallback.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan symmetric eigen shader, helper
    validation/allocation, advanced collection tests, docs, TODO, and memory
    artifacts.
- Code or configuration changes made:
  - Reworked `csrc/tensor_vulkan_symmetric_eigen_f64.comp` to store Jacobi
    matrix scratch behind the public eigenvector output payload instead of a
    fixed private `double[4096]` array.
  - Regenerated `csrc/tensor_vulkan_symmetric_eigen_f64_spv.c`.
  - Updated `csrc/tensor_vulkan_helpers.c` validation/allocation so values
    retain the visible `[n] + status` payload, vectors retain visible `[n n]`
    metadata, and hidden vector backing storage carries `n*n` matrix scratch.
    The helper now rejects resource sizes that exceed 32-bit shader/storage
    guards or would wrap the shader's `64 * n * n` Jacobi iteration guard.
  - Replaced the old `65x65` fail-closed regression with availability-gated
    success tests for direct and lazy large Vulkan eigenvalues/eigenvectors,
    Vulkan output placement, large nonsymmetric `tensor/not-symmetric`
    diagnostics, and no `LAPACKE_dsyev` counter movement.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`, and relevant
    Vulkan/Tensor docs. Deferred large-`n` performance work is now tracked as
    measurement-driven tiling or staged execution, not as the semantic cap.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_symmetric_eigen_f64.comp -o /tmp/omni_symmetric_eigen.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_symmetric_eigen.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct smokes: `65x65` identity Vulkan eigenvalues copied back as
    `(65 1.0 1.0)`; `65x65` eigenvectors returned
    `(vulkan vulkan 65 1.0)`; `65x65` sparse nonsymmetric inputs returned
    `tensor/not-symmetric`.
  - Host focused `advanced-collections-module`: `pass=1006 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=993 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep assuming Vulkan symmetric eigen is capped at `n <= 64`.
    Dense row-major square symmetric `Float64` inputs now support `n > 64`
    within helper resource limits, 32-bit shader index guards, and the Jacobi
    iteration guard.
  - Do not build large sparse test fixtures with recursive `(range 4225)` list
    data; that stack-overflowed in a `handle` body. Use an all-zero Tensor
    converted to an Array plus `set!` for sparse large fixtures.
- Current best recommendation / checkpoint:
  - Treat semantic large-`n` Vulkan symmetric eigen support as landed for
    dense row-major `Float64` symmetric inputs. Continue `TENSOR-100F` from
    general `matrix/eigenpairs`, Vulkan `Float32`, fixed-width complex,
    stride/view metadata, or measurement-driven performance work.
- Unresolved issues:
  - The large-`n` path remains correctness-first single-dispatch Jacobi using
    storage-buffer scratch; tiled/staged performance work is deferred and
    tracked in `TODO.md`.
  - Full heavy/container-only gates were not run; focused bounded-container
    validation passed.
  - Live GPU execution coverage remains availability-gated by the local Vulkan
    stack.
- Signature: Codex GPT-5.4
