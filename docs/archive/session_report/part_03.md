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
- Current best recommendation/checkpoint:
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

## 2026-04-17 20:42 CEST - TENSOR-100F Vulkan Float32 Direct Reducer Slice

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and land Vulkan
    `Float32` direct reducers without treating `Float32` as a fallback for
    `Float64`, silently widening through LAPACK/SVD, or copying unsupported
    Vulkan paths to CPU.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Tensor reducer shaders/helpers, public
    matrix rank/norm dispatch, focused tests, TODO, design/status docs, and
    session handoff artifacts.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_norm_f32.comp`,
    `csrc/tensor_vulkan_rank_f32.comp`, and generated matching `_spv.c`
    sources.
  - Added `omni_tensor_backend_vulkan_norm_f32` and
    `omni_tensor_backend_vulkan_rank_f32` in `csrc/tensor_vulkan_helpers.c`
    using explicit `Float32` storage, a `Float32` rank push-constant layout,
    and matching C3 extern/build-manifest wiring.
  - Updated `src/lisp/prim_tensor_matrix.c3` so public `matrix/rank` accepts
    rank-2 `Float64` or `Float32` tensors. CPU `Float32` rank uses the pure
    elimination path with values widened only inside the local workspace;
    Vulkan `Float32` rank routes through the dedicated `_f32` helper.
  - Updated `matrix/norm` so `Float32` supports direct selectors
    default/`'frobenius`, `'one`, `'infinity`, and `'max` on CPU and Vulkan,
    returning the existing public `Float64` scalar shape. `Float32`
    `'spectral` and `'nuclear` fail closed with `tensor/backend-unsupported`
    until native `Float32` singular-value kernels land.
  - Added CPU and availability-gated Vulkan tests in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` for eager/lazy
    `Float32` rank and norm, zero-size shapes, tolerance behavior,
    vector-shape rejection, no-LAPACK routing, and fail-closed Float32
    spectral/nuclear selectors.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md` so direct
    reducers are no longer listed as deferred.
- Commands run and key results:
  - `glslangValidator` and `spirv-val` for
    `csrc/tensor_vulkan_norm_f32.comp` and
    `csrc/tensor_vulkan_rank_f32.comp`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct CPU/Vulkan `Float32` smokes returned `2` for `matrix/rank`, `5.0`
    for `matrix/norm`, and `tensor/backend-unsupported` for Float32
    spectral/nuclear `matrix/norm`.
  - Host focused `advanced-collections-module`: passed, `pass=1098 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1085 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating all Vulkan `Float32` reducers as fail-closed. Dense
    row-major matching-`Float32` `matrix/rank` and direct `matrix/norm`
    selectors now have real Vulkan shader/helper paths.
  - Do not route `Float32` rank or direct norm through existing `Float64`
    LAPACK/SVD helpers. SVD-backed `Float32` selectors remain separate and
    fail-closed until native singular-value kernels exist.
- Current best recommendation/checkpoint:
  - Continue Vulkan `Float32` from SVD-backed reducers
    (`matrix/norm` `'spectral`/`'nuclear`, `matrix/singular-values`,
    `matrix/svd`) or factor/solve kernels with dedicated Float32
    shader/helper ABI names and tolerance/oracle tests.
- Unresolved issues:
  - Vulkan `Float32` SVD-backed reducers, singular-value/SVD outputs, and
    factor/solve kernels remain fail-closed.
  - CUDA `Float32` placement and scalar `Float32` values remain fail-closed.
- Signature: Codex GPT-5.4

## 2026-04-18 11:53 CEST - CPU Fixed-Width Complex Scalar And Tensor Support

- Objective attempted:
  - Land native CPU `Complex128`/`Complex64` scalar and Tensor semantics, then
    update the backlog/docs so the remaining fixed-width complex work is
    correctly framed as CUDA/Vulkan backend execution.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - scalar value/runtime/type integration
  - CPU Tensor storage, map/contract/component helpers, and matrix structural
    operations
  - `TODO.md`, `memory/CHANGELOG.md`, language/reference docs, and Tensor/Vulkan
    planning docs
- Code or configuration changes made:
  - Added `Complex128` and `Complex64` scalar value families with constructors,
    stdlib predicates, dispatch/type integration, printing/String conversion,
    equality/hash, AOT/literal serialization, and boundary copy/promotion
    handling.
  - Added CPU Tensor `Complex128`/`Complex64` dtype/storage support for
    explicit and inferred construction, scalar fill, `ref`, `Array`/`List`
    conversion, `realize`, mixed fixed-complex `map` promotion, `contract`,
    `real-part`, `imag-part`, `abs`, `conjugate`, unary minus, and structural
    matrix operations (`matrix/transpose`,
    `matrix/diagonal`, `matrix/diagonal-matrix`, `matrix/identity`, and
    `matrix/trace`).
  - Kept CUDA/Vulkan fixed-width complex placement and backend execution
    fail-closed. Future GPU complex support needs explicit backend capability
    bits, device layout/copy semantics, status contracts, and kernels.
  - Updated fixed-width complex docs/backlog/planning wording so CPU support is
    no longer described as deferred.
- Commands run and key results:
  - `c3c build`: passed.
  - Direct `--eval` smokes for scalar `String`, Tensor `ref`, component dtype,
    and Complex128 `contract`: passed.
  - Host focused scalar advanced tests:
    `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-unicode-iterator ./build/main --test-suite lisp`
    passed (`180 passed, 0 failed`).
  - Host focused Tensor advanced tests:
    `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
    passed (`1383 passed, 0 failed`).
  - Bounded-container focused Tensor advanced tests:
    `OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
    passed (`1366 passed, 0 failed`).
  - Targeted `git diff --check`: passed.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating fixed-width complex as missing at the CPU
    scalar/Tensor layer. The remaining fixed-width complex work is backend
    placement/execution and direct Vulkan general `matrix/eigenpairs` contract
    work.
  - Do not infer complex GPU support from real `float64` or `float32`
    `tensor-backends` capability bits.
- Current best recommendation / checkpoint:
  - Use the landed CPU `Complex128`/`Complex64` Tensor behavior as the oracle
    for the next CUDA/Vulkan complex ABI. Start with explicit capability bits,
    device layout/copy semantics, and a first operation family such as
    `map`/`contract` before attempting general complex eigenpairs.
- Unresolved issues:
  - Fixed-width complex scientific/transcendental operations beyond arithmetic,
    component helpers, `abs`, and `conjugate` remain fail-closed pending an
    explicit approximation/precision contract.
  - CUDA/Vulkan `Complex128`/`Complex64` placement, backend `map`, backend
    `contract`, backend matrix kernels, and direct Vulkan general
    `matrix/eigenpairs` remain unimplemented.
- Signature: Codex GPT-5.4

## 2026-04-18 11:12 CEST - TENSOR-100F Vulkan Map Handler Propagation

- Objective attempted:
  - Close the unsupported Vulkan `map` handler follow-up and preserve
    structured Tensor backend errors through direct `map`, lazy-source `map`,
    and `realize` paths.
- Code or configuration changes made:
  - Split Vulkan map unsupported-callable preflight into small wrapper paths
    before the heavier direct/value map execution frames.
  - Built no-`data` recoverable raise payloads directly in root scope before
    setting `raise_pending`, avoiding boundary-promotion stack pressure inside
    constrained handle bodies.
  - Propagated evaluated `ERROR` arguments before compiled one-argument and
    multi-argument primitive dispatch.
  - Increased normal StackCtx usable stack from 128KB to 256KB so handled CPU
    lazy Tensor map realization succeeds on small tensors.
  - Added regressions in the advanced collections Tensor block for handled
    unsupported Vulkan `map`, handled `realize`, CPU lazy-map `realize` inside
    `handle`, and `realize` source-error propagation.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed, with existing deprecation warnings.
  - Direct smokes covered handled Vulkan direct/lazy-source `map`, direct and
    handled `realize`, handled CPU lazy-map `realize`, and destination-form
    `realize` source-error propagation.
  - Host focused `advanced-collections-module`: `pass=1352 fail=0`.
  - Host `advanced-effect-continuation`: `pass=56 fail=0`.
  - Host `advanced-effect-union-limit`: `pass=68 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=1335 fail=0`.
  - Stack suite: `pass=23 fail=0`.
  - Targeted `git diff --check`: passed.
- Preserved constraints:
  - Vulkan `floor`/rounding support was not widened through generic `map`.
  - Unsupported Vulkan callables remain outside the kernel whitelist.
  - No hidden CPU/GPU fallback was added.
- Unresolved issues:
  - The 256KB StackCtx budget is a pragmatic runtime fix. A future stack-light
    rewrite of lazy Tensor map materialization may reduce this pressure, but
    the validated small-tensor handler path is no longer blocked.
- Signature: Codex GPT-5.4

## 2026-04-17 15:39 CEST - Constructor-Driven Iterator Tensor Materialization
- Objective attempted:
  - Prioritize the owner design correction that iterators are already lazy
    computations and constructors, not `delay`/`force` or general `realize`,
    should be the public materialization boundary.
- Workspace/target:
  - `/home/christos/Omni`, Tensor constructor dispatch, iterator consumption,
    and scientific Tensor design docs.
- Code or configuration changes made:
  - Updated `src/lisp/prim_tensor.c3` so `(Tensor iterator)`,
    `(Tensor iterator dtype)`, and `(Tensor dtype shape iterator)` consume
    finite numeric iterators through the Tensor constructor path.
  - Added native `Iterator(^Tensor)` dispatch through a C iterator thunk/state:
    Tensor iteration yields flat row-major elements, realizes lazy CPU Tensor
    expressions once into Tensor storage, and fails closed on non-CPU device
    tensors until explicitly copied with `to-device 'cpu`.
  - Added focused advanced collection regressions for inferred iterator Tensor
    construction, lazy iterator `map` into `Tensor`, explicit-shape iterator
    data, BigInteger dtype preservation, non-numeric iterator failure,
    Tensor-to-Iterator row-major iteration, lazy CPU Tensor iteration,
    BigInteger Tensor iteration, empty Tensor iteration, and device fail-closed
    behavior.
  - Updated `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/reference/11-appendix-primitives.md`,
    `docs/reference/12-appendix-stdlib.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
    `docs/plans/README.md`, `.agents/PLAN.md`, `TODO.md`, and
    `memory/CHANGELOG.md` to make constructor dispatch the canonical public
    materialization model and demote `realize` to low-level Tensor
    destination-storage use.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct runtime smokes returned `3.0`, `3.0`, `6.0`,
    `"9223372036854775808"`, `3.0`, `3.0`, and
    `"9223372036854775808"` for iterator-to-Tensor and Tensor-to-Iterator
    constructor paths.
  - Host focused `advanced-collections-module`: passed, `pass=969 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=956 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check` passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not describe `realize` as the canonical public lazy/materialization
    boundary. Iterators already carry suspended computation, and constructors
    are the public terminal consumers.
  - Do not add a public Clojure-style `delay`/`force` surface for iterator or
    Tensor laziness.
- Current best recommendation / checkpoint:
  - Continue from the single remaining live parent, `TENSOR-100F`. The
    constructor-driven iterator/Tensor materialization lane is now closed as
    `TENSOR-076B` plus `TENSOR-076C`.
- Unresolved issues:
  - Full heavy/container-only gates were not rerun for this source-level slice;
    focused host and bounded-container advanced collection coverage passed.
- Signature: Codex GPT-5.4

## 2026-04-17 15:09 CEST - CUDA Destination Realize Support
- Objective attempted:
  - Continue `TENSOR-100F` feature implementation with multiple GPT-5.4
    agents by extending destination-form `realize` parity to CUDA.
- Workspace/target:
  - `/home/christos/Omni`, Tensor/CUDA destination realization.
- Code or configuration changes made:
  - Added existing-buffer CUDA host-to-device, device-to-device, and scalar
    `Float64` fill helpers in `csrc/tensor_cuda_helpers.c`.
  - Exposed the helpers through `src/lisp/tensor_cuda_backend.c3`.
  - Split destination-form `realize` in `src/lisp/prim_tensor.c3` so existing
    dense row-major CUDA `Float64` destinations accept matching CPU sources,
    CUDA sources, lazy CPU expressions, supported lazy CUDA contract results,
    and scalar fills.
  - Preserved CPU destination fail-closed behavior for CUDA sources without
    explicit `(to-device source 'cpu)`.
  - Hardened unsupported device `map` construction so CUDA map operands fail
    immediately with `tensor/backend-unsupported` instead of producing a lazy
    expression that later fails at boundary copy time.
  - Added availability-gated regressions for CUDA destination copies, lazy
    cuBLAS contract into CUDA destination, CPU-destination fail-closed
    behavior, unsupported CUDA map fail-closed behavior, and CUDA/Vulkan
    cross-backend destination rejection.
  - Updated Tensor docs, CUDA/Vulkan plans, TODO, `.agents/PLAN.md`, and
    `memory/CHANGELOG.md`.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct runtime smokes returned `2.0`, `3.0`, `4.0`, `1.0`, and `154.0`
    for CPU source, CUDA source, lazy CPU source, scalar fill, and lazy cuBLAS
    contract into CUDA destinations; CUDA source into CPU destination and
    unsupported CUDA map both returned `tensor/backend-unsupported`.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=959 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed, `pass=946 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Unsupported CUDA `map` operands should fail at map construction with
    `tensor/backend-unsupported`; do not allow them to become lazy Tensor
    expressions that reach boundary-copy or CPU fallback paths.
  - CUDA destination realization does not authorize CUDA/Vulkan cross-backend
    destination copies. Use explicit `to-device 'cpu` and then the desired
    target when a transfer is intentional.
- Current best recommendation / checkpoint:
  - Destination-form `realize` is now implemented for dense row-major
    `Float64` CPU, CUDA, and Vulkan destination classes under one
    backend-neutral surface. Remaining `TENSOR-100F` lanes are stride/view
    metadata, native `Float32`, fixed-width complex, additional unary helper
    opcodes with correct semantics, rank-1 CUDA dot if desired, and broad
    bounded-container validation before parent closure.
- Unresolved issues:
  - Full heavy/container-only gates were not rerun for this source-level slice;
    focused host and bounded-container advanced collection coverage passed.
- Signature: Codex GPT-5.4

## 2026-04-17 14:55 CEST - Vulkan Destination Realize Support
- Objective attempted:
  - Continue `TENSOR-100F` feature implementation with multiple GPT-5.4
    agents by landing explicit Vulkan destination-form `realize` support.
- Workspace/target:
  - `/home/christos/Omni`, Tensor/Vulkan destination realization.
- Code or configuration changes made:
  - Added existing-buffer Vulkan host-to-device, device-to-device, and scalar
    `Float64` fill helpers in `csrc/tensor_vulkan_helpers.c`.
  - Exposed the helpers through `src/lisp/tensor_vulkan_backend.c3`.
  - Split destination-form `realize` in `src/lisp/prim_tensor.c3` so existing
    dense row-major Vulkan `Float64` destinations accept matching CPU sources,
    Vulkan sources, lazy Vulkan results, and scalar fills.
  - Preserved the existing CPU destination rule: device sources still require
    explicit `(to-device source 'cpu)` before destination realization.
  - Updated availability-gated advanced collection tests, Tensor docs,
    Vulkan roadmap/status docs, TODO, `.agents/PLAN.md`, and
    `memory/CHANGELOG.md`.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct runtime smokes returned `2.0`, `3.0`, `4.0`, and `1.0` for CPU,
    Vulkan, lazy Vulkan, and scalar sources into Vulkan destinations; Vulkan
    source into CPU destination still returned `tensor/backend-unsupported`.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=949 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed, `pass=936 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The old audit checkpoint that treated every non-CPU destination as
    intentionally fail-closed is superseded for Vulkan `Float64` destinations.
    Keep CPU destinations fail-closed for device sources unless the caller
    performs explicit `to-device 'cpu`.
- Current best recommendation / checkpoint:
  - Vulkan destination realization is implemented for dense row-major
    `Float64` storage. Remaining `TENSOR-100F` work should continue from
    explicit residual lanes: CUDA destination contracts, stride/view metadata,
    native `Float32`, fixed-width complex, additional unary helper opcodes, or
    broad bounded-container validation before parent closure.
- Unresolved issues:
  - Full heavy/container-only gates were not rerun for this source-level slice;
    focused host and bounded-container advanced collection coverage passed.
- Signature: Codex GPT-5.4

## 2026-04-17 14:27 CEST - Direct Vulkan Symmetric Eigenvalues And Eigenvectors
- Objective attempted:
  - Continue `TENSOR-100F` by landing the first direct Vulkan symmetric real
    eigen slice behind existing `matrix/eigenvalues` and
    `matrix/eigenvectors` surfaces.
- Workspace/target:
  - `/home/christos/Omni`, Tensor/Vulkan symmetric eigen route.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_symmetric_eigen_f64.comp` and generated
    `csrc/tensor_vulkan_symmetric_eigen_f64_spv.c`.
  - Added `omni_tensor_backend_vulkan_symmetric_eigen_f64`, build/project
    wiring, and Vulkan status mapping for `tensor/not-symmetric`.
  - Routed public `matrix/eigenvalues` and `matrix/eigenvectors` so concrete
    and lazy Vulkan dense row-major square `Float64` inputs execute on Vulkan
    before CPU fallback and return Vulkan-placed Tensor outputs.
  - Kept `matrix/eigenpairs` fail-closed on Vulkan because its public result
    contract is still pointer-backed `BigComplex`.
  - Added availability-gated regressions for values/vectors, device placement,
    lazy Vulkan inputs, nonsymmetric diagnostics, `n > 64` fail-closed
    behavior, and unchanged `LAPACKE_dsyev` counters.
  - Updated Tensor/Vulkan docs, TODO, `.agents/PLAN.md`, and
    `memory/CHANGELOG.md`.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_symmetric_eigen_f64.comp -o /tmp/omni_tensor_vulkan_symmetric_eigen_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_symmetric_eigen_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct runtime smokes returned `("vulkan" 3.0 2.0)`,
    `("vulkan" "vulkan" 3.0 1.0)`, and `tensor/not-symmetric`.
  - Host focused `advanced-collections-module`: passed, `pass=948 fail=0`.
  - Bounded container focused `advanced-collections-module`: passed,
    `pass=935 fail=0`.
  - `python3 -m json.tool project.json`: passed.
  - `bash -n scripts/build_omni_chelpers.sh`: passed.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
