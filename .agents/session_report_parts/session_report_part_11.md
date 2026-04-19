# Session Report Index Part 11

Source: `.agents/SESSION_REPORT.md`

## 2026-04-17 14:27 CEST - Direct Vulkan Symmetric Eigenvalues And Eigenvectors
- Objective attempted:
  - Continue `TENSOR-100F` from the SVD checkpoint by landing the first direct
    Vulkan symmetric real eigen slice behind existing `matrix/eigenvalues` and
    `matrix/eigenvectors` surfaces.
- Workspace/target:
  - `/home/christos/Omni`, Tensor/Vulkan symmetric eigen route.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_symmetric_eigen_f64.comp` and generated
    `csrc/tensor_vulkan_symmetric_eigen_f64_spv.c`.
  - Added `omni_tensor_backend_vulkan_symmetric_eigen_f64`, build/project
    wiring, and a Vulkan status mapping for nonsymmetric input to
    `tensor/not-symmetric`.
  - Added the C3 extern and routed public `matrix/eigenvalues` and
    `matrix/eigenvectors` so dense row-major Vulkan `Float64` square inputs,
    including lazy Vulkan inputs, execute on Vulkan before CPU fallback.
  - Returned eigenvalues and aligned eigenvector columns as Vulkan-placed
    Tensor outputs; CPU inspection remains explicit through `to-device 'cpu`.
  - Kept `matrix/eigenpairs` fail-closed on Vulkan because its public result
    contract is still pointer-backed `BigComplex`.
  - Added availability-gated regressions for direct/lazy Vulkan eigenvalue and
    eigenvector results, output device placement, nonsymmetric
    `tensor/not-symmetric`, `n > 64` fail-closed behavior, and unchanged
    `LAPACKE_dsyev` counters.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`,
    `docs/plans/vulkan-eigensolver-plan-2026-04-17.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_symmetric_eigen_f64.comp -o /tmp/omni_tensor_vulkan_symmetric_eigen_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_symmetric_eigen_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct runtime smokes: Vulkan eigenvalues returned `("vulkan" 3.0 2.0)`;
    Vulkan eigenvectors returned `("vulkan" "vulkan" 3.0 1.0)`;
    nonsymmetric Vulkan input returned `tensor/not-symmetric`.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=948 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed, `pass=935 fail=0`.
  - `python3 -m json.tool project.json`: passed.
  - `bash -n scripts/build_omni_chelpers.sh`: passed.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Direct `matrix/eigenvalues` and `matrix/eigenvectors` are no longer
    fail-closed on supported Vulkan operands. Keep only `matrix/eigenpairs`
    fail-closed while its output contract remains `BigComplex`.
  - Do not hide `n > 64` symmetric eigen by copying Vulkan input to CPU; the
    current helper is intentionally bounded and returns
    `tensor/backend-unsupported` beyond that boundary.
- Current best recommendation / checkpoint:
  - Direct symmetric Vulkan eigen is implemented for dense row-major
    `Float64` square inputs with `n <= 64`. Continue `TENSOR-100F` from
    residual lanes such as larger eigen/SVD algorithms, helper factoring,
    native `Float32` storage/kernels, or fixed-width complex dtype work.
- Unresolved issues:
  - Full heavy/container-only gates were not rerun for this source-level
    Tensor/Vulkan slice; focused host and bounded-container coverage passed.
- Signature: Codex GPT-5.4

## 2026-04-17 14:01 CEST - Direct Vulkan Matrix SVD Factor Output
- Objective attempted:
  - Continue `TENSOR-100F` feature implementation using GPT-5.4 agents only,
    after fast/Spark agents were unavailable, and land the missing direct
    Vulkan `matrix/svd` factor-output path.
- Workspace/target:
  - `/home/christos/Omni`, Tensor/Vulkan matrix SVD route.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_svd_f64.comp` and generated
    `csrc/tensor_vulkan_svd_f64_spv.c`.
  - Added `omni_tensor_backend_vulkan_svd_f64` with one Vulkan input and
    separate Vulkan `u`, `s`, and `v` output buffers, plus build wiring in
    `scripts/build_omni_chelpers.sh` and `project.json`.
  - Added the C3 extern and routed public `matrix/svd` so dense row-major
    Vulkan `Float64` rank-2 inputs, including lazy Vulkan inputs, execute on
    Vulkan before CPU fallback.
  - Added availability-gated regressions for Vulkan SVD values, output device
    placement, lazy input, empty axes, wrong rank, `k > 64` fail-closed
    behavior, exact `k == 64` success, and unchanged `LAPACKE_dgesvd` counters.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/areas/tensor-scientific.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `docs/plans/vulkan-svd-factor-output-plan-2026-04-17.md` so direct
    Vulkan `matrix/svd` is recorded as shipped for dense row-major `Float64`
    inputs with `k <= 64`.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_svd_f64.comp -o /tmp/omni_tensor_vulkan_svd_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_svd_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `python3 -m json.tool project.json`: passed.
  - `bash -n scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=945 fail=0`.
  - `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not restore a second private `double[4096]` eigenvector array in the
    SVD shader. It compiled and validated as SPIR-V but failed at runtime on
    this Vulkan stack with `tensor/backend-execution-failed`.
  - The shipped shader stores the driving eigenvectors in the eventual output
    buffer (`V` for tall/square inputs, `U` for wide inputs) and keeps only the
    Gram matrix plus eigenvalues in private storage.
- Current best recommendation / checkpoint:
  - Direct Vulkan `matrix/svd` is implemented for dense row-major `Float64`
    rank-2 inputs with `k = min(rows, columns) <= 64`. Continue `TENSOR-100F`
    from the remaining explicit residual lanes: Vulkan eigensolvers,
    native `Float32` Tensor storage/kernels, larger-`k` SVD algorithms, or
    shared helper cleanup.
- Unresolved issues:
  - Vulkan `matrix/eigenvalues`, `matrix/eigenvectors`, and
    `matrix/eigenpairs` remain CPU-only/fail-closed on Vulkan operands.
  - `Float32` Vulkan execution remains blocked on native `Float32` Tensor
    storage and CPU semantics.
  - This SVD path is correctness-first and single-dispatch; larger `k`,
    strided/view-backed tensors, fixed-width complex, and performance SVD
    algorithms remain deferred.
- Signature: Codex GPT-5.4

## 2026-04-17 11:13 CEST - Vulkan Destination Realize Audit Fix
- Objective attempted:
  - Continue the Tensor/Vulkan audit and fix pass with parallel GPT-5.4
    auditors and implementation-scoped fallback workers after GPT-5.3 Spark
    workers were quota-blocked.
- Workspace/target:
  - `/home/christos/Omni`.
- Code or configuration changes made:
  - Added `tensor_require_cpu_destination` and routed explicit destination
    `realize` checks through it so valid non-CPU destination tensors fail with
    `tensor/backend-unsupported` instead of the missing CPU backing-storage
    runtime error.
  - Added regressions for CPU source into Vulkan destination, Vulkan source
    into Vulkan destination, and Vulkan `matrix/singular-values` into a CPU
    destination.
  - Updated `TODO.md`, `docs/areas/tensor-scientific.md`,
    `docs/reference/03-collections.md`, `memory/CHANGELOG.md`, and this plan
    state to remove stale validation/doc wording.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
    passed, `pass=907 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The source-audit concern that Vulkan singular-values still over-dispatched
    from `k + 2` payload size is stale for the current working tree; the helper
    already allocates `k + 2` storage while dispatching exactly one work item.
- Current best recommendation / checkpoint:
  - Destination `realize` remains CPU-destination-only. Keep explicit
    `to-device 'cpu` as the non-CPU source copy boundary and do not introduce
    Vulkan destination-copy semantics without a separate contract.
- Unresolved issues:
  - Full heavy/container-only gates were not rerun for this narrow audit fix.
- Signature: Codex GPT-5.4

## 2026-04-17 09:22 CEST - TENSOR-100E Vulkan Zero-Axis Contract
- Objective attempted:
  - Enable public zero-axis Tensor `contract` on Vulkan without adding a
    backend-specific surface or CPU fallback.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Relaxed `tensor_contract_try_vulkan_value` so `axis_count == 0` with null
    axis arrays is accepted, while one-or-more-axis contractions still require
    explicit axis metadata.
  - Limited the rank-1 Vulkan dot fast path to the one-contracted-axis case so
    rank-1/rank-1 `[] []` contractions use the generic outer-product shader.
  - Relaxed `omni_tensor_backend_vulkan_contract_f64` to accept zero-axis
    metadata and rank-0 scalar tensors.
  - Added availability-gated tests for rank-1/rank-1 outer products,
    rank-2/rank-1 outer products, rank-0 scalar products, Vulkan result
    residency, and zero-free-dimension zero-length output.
  - Updated `TODO.md`, `.agents/PLAN.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`,
    `docs/areas/tensor-scientific.md`, and `memory/CHANGELOG.md`.
- Key results:
  - Public `(contract left right [] [])` now supports eligible Vulkan-placed
    dense row-major `Float64` tensors, including rank-0 scalar products and
    higher-rank outer products.
  - Results remain Vulkan-placed; callers still copy back explicitly with
    `to-device 'cpu` for CPU inspection.
  - Unsupported dtypes, mixed CPU/Vulkan operands, and unsupported layouts
    remain fail-closed with Tensor backend diagnostics.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_contract_f64.comp -o
    /tmp/omni_tensor_vulkan_contract_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0
    /tmp/omni_tensor_vulkan_contract_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `60.0`, `12.0`, `vulkan`, and `0`.
  - Host focused `advanced-collections-module`: `851 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`:
    `838 passed, 0 failed`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - None new. This slice confirms the existing generic Vulkan contract shader
    and metadata writer were already compatible with zero-axis metadata; the
    blocker was the fail-closed guards.
- Current best recommendation / checkpoint:
  - Continue Vulkan Tensor expansion with another backend-neutral `Float64`
    kernel family or with explicit layout/aliasing metadata before
    stride-aware views.
- Signature: Codex GPT-5

## 2026-04-17 08:31 CEST - TENSOR-100E/F Vulkan Min Max
- Objective attempted:
  - Add real Vulkan paths for public Tensor `min` / `max` and `map min` /
    `map max` on dense row-major `Float64` tensors.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/tensor_vulkan_map_f64.comp` so opcode `4` is `min` and
    opcode `5` is `max`, then regenerated
    `csrc/tensor_vulkan_map_f64_spv.c`.
  - Widened `omni_tensor_backend_vulkan_map_f64` opcode validation to accept
    `0..5`.
  - Routed `map min` / `map max` through the existing Vulkan binary map
    selector.
  - Changed Tensor `min` / `max` to resolve concrete Tensor operands on any
    device before CPU-storage checks, then route eligible Vulkan `Float64`
    tensor/scalar, scalar/Tensor, and Tensor/Tensor broadcast cases through
    the binary Vulkan map helper.
  - Added availability-gated Vulkan map/direct/lazy/broadcast/mixed-device
    regressions plus CPU `map min` / `map max` parity tests.
  - Updated `TODO.md`, `.agents/PLAN.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`,
    `docs/areas/tensor-scientific.md`, and `memory/CHANGELOG.md`.
- Key results:
  - Public Tensor `min` / `max` and `map min` / `map max` now return
    Vulkan-placed `Float64` Tensor results for eligible dense row-major Vulkan
    operands.
  - Mixed CPU/Vulkan operands still fail closed with
    `tensor/backend-unsupported`; no CPU fallback or Big* lowering was added.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_f64.comp -o /tmp/omni_tensor_vulkan_map_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_map_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `-2.0`, `0.0`, `vulkan`, and
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `846 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`:
    `833 passed, 0 failed`.
- Invalidated assumptions or failed approaches worth preserving:
  - None new. Existing constraints still apply: do not add hidden CPU fallback
    for Vulkan operands and do not broaden Vulkan dtypes beyond `Float64`.
- Current best recommendation / checkpoint:
  - Treat Vulkan `Float64` Tensor `min` / `max` as shipped on the binary map
    helper. Remaining Vulkan math-library work should continue through
    shader-validated fixed-width `Float64` kernels or explicit dtype/layout
    design work.
- Unresolved issues:
  - NaN tie behavior is inherited from the current shader compare/select
    expression and is not separately specified in this slice.
- Signature: Codex GPT-5

## 2026-04-17 08:18 CEST - TENSOR-100E/F Vulkan Component Ops
- Objective attempted:
  - Add real Vulkan paths for public real-valued Tensor component operations
    through the existing dense row-major `Float64` unary helper.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/tensor_vulkan_map_unary_f64.comp` so opcode `3` is
    identity and opcode `4` is zero-fill, then regenerated
    `csrc/tensor_vulkan_map_unary_f64_spv.c`.
  - Widened `omni_tensor_backend_vulkan_map_unary_f64` opcode validation to
    accept `0..4`.
  - Routed Vulkan `map real-part`, `map imag-part`, `map conjugate`, and
    direct Tensor `real-part` / `imag-part` / `conjugate` through the separate
    unary helper for dense row-major Vulkan `Float64` tensors.
  - Changed the direct component Tensor paths to resolve concrete tensors on
    any device before entering CPU-storage logic, avoiding hidden CPU fallback
    and avoiding host-storage copy helpers for Vulkan buffers.
  - Added availability-gated Vulkan direct/map/lazy regressions plus CPU
    `map` parity tests for `real-part`, `imag-part`, and `conjugate`.
  - Updated `TODO.md`, `.agents/PLAN.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`,
    `docs/areas/tensor-scientific.md`, and `memory/CHANGELOG.md`.
- Key results:
  - Public `(map real-part <vulkan-tensor>)`,
    `(map imag-part <vulkan-tensor>)`, `(map conjugate <vulkan-tensor>)`, and
    direct Tensor component calls now return Vulkan-placed `Float64` Tensor
    results for eligible real-valued Vulkan inputs.
  - `real-part` and `conjugate` are identity over real tensors; `imag-part`
    returns a zero tensor.
  - Unsupported Vulkan unary callables such as `sin` still raise
    `tensor/backend-unsupported`; the invalidated generic Vulkan unary
    `map` mode-3 branch remains out of use.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_unary_f64.comp -o
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `-2.5`, `0.0`, `vulkan`, and `0.0` for
    direct `real-part`, direct `imag-part`, direct `conjugate` placement, and
    `map imag-part`.
  - Host focused `advanced-collections-module`: `836 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`:
    `823 passed, 0 failed`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not attempt `exp(double)`, `log(double)`, or `log10` as simple new
    GLSL builtins on this helper; fresh probes failed glslang compilation.
    Keep that family behind a separate approximation/library decision.
- Current best recommendation / checkpoint:
  - Treat real-valued Vulkan component ops as shipped on the unary helper.
    Continue Vulkan unary expansion only with shader-validated non-
    transcendental ops or a separately designed approximation/library path.
- Unresolved issues:
  - This slice does not add fixed-width complex Vulkan Tensor storage; real
    `Float64` component semantics are intentionally real-valued.
- Signature: Codex GPT-5
