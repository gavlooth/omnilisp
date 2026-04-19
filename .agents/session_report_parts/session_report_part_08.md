# Session Report Index Part 08

Source: `.agents/SESSION_REPORT.md`

## 2026-04-18 00:40 CEST - Scalar Float32 Runtime Value Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by replacing the
    scalar `Float32` fail-closed boundary with a native runtime value connected
    to dispatch, Tensor extraction, copy/promotion, numeric, compiler, and
    persistence machinery.
- Workspace/target:
  - `/home/christos/Omni`, scalar value representation, conversion
    constructors, numeric helpers, Tensor extraction, matrix scalar readback,
    AOT/expression serialization, tests, TODO, changelog, plans, and Tensor
    docs.
- Code or configuration changes made:
  - Added scalar `Float32` value tag/payload/constructor, `make_float32`,
    type-id cache, `Number` parent wiring, `type-of` / `is?`, and stdlib
    `float32?`.
  - Wired `Float32`, `(Float x 32)`, and `(Float x "32")` through finite/range
    checked construction, string/format/print conversion, numeric
    comparison/equality/hash/order, arithmetic, schema validation, tuple
    encoding, JSON/CSV emission, FFI `Double` widening, sleep/random numeric
    consumers, expression serialization, and AOT literal lowering.
  - Preserved region-centric lifetime semantics by treating scalar `Float32` as
    a copy/promotion/env-copy leaf and adding boundary graph audit handling.
  - Updated Tensor `Float32` `ref`, `Array`, `List`, `Iterator`, contract scalar
    readback, CPU/copyback extraction, and structural matrix scalar readback so
    element access returns scalar `Float32` instead of widening to `Float64`.
    Matrix determinant and norm retain their documented `Float64` scalar return
    contract.
  - Updated current docs/plans/TODO/changelog so scalar `Float32` is no longer
    listed as deferred. Deferred work remains in `TODO.md` for fixed-width
    complex, stride/view-backed Vulkan coverage, CUDA `map`, and broad
    Docker-bound parent validation; CUDA zero-size contract identity/fill is
    covered by the later checkpoint above.
  - Updated stale tests that still used `test_eq_double` for `Float32`
    structural matrix element extraction and the advanced-core zero-arity `+`
    message now that `+` is unary-or-binary.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct runtime smokes for constructor/type/predicate, arithmetic widening
    policy, closure capture, Tensor extraction, structural matrix extraction,
    contract extraction, and `matrix/trace`: all returned `true`.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-core-semantics ./build/main --test-suite lisp`: passed `pass=71 fail=0`.
  - `OMNI_LISP_TEST_SLICE=arithmetic-comparison ./build/main --test-suite lisp`: passed `pass=47 fail=0`.
  - `./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed `pass=1205 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `c3c build --sanitize=address`: attempted; local `c3c` rejected sanitizer
    mode with `Address sanitizer is only supported on Linux, FreeBSD, NetBSD,
    Darwin and Windows.`
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep assuming Tensor `Float32` extraction should widen to scalar
    `Float64`. Scalar `Float32` exists now; element extraction should preserve
    scalar `Float32` for `Float32` Tensor storage.
  - Do not keep treating scalar `Float32` / `(Float x 32)` as fail-closed.
    Constructor failures now apply to nonnumeric, nonfinite, or out-of-range
    narrowing inputs.
- Current best recommendation or checkpoint:
  - Continue `TENSOR-100F` from CUDA elementwise `map`, fixed-width complex
    Tensor semantics, stride/view-backed Vulkan coverage, or broad Docker-bound
    parent validation.
- Unresolved issues:
  - Local ASAN compile remains unavailable through the current `c3c` invocation.
  - No full heavy/global/container-only parent gate was run in this slice.
- Dependencies, blockers, or restart requirements:
  - No long-running process was left active. Any already-running external
    process needs a rebuild/restart to pick up the new scalar `Float32` runtime.
- Signature: GPT-5 Codex

## 2026-04-17 23:46 CEST - TENSOR-100F CUDA Float32 Placement And Contract

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by landing CUDA
    `Float32` placement/copyback, destination `realize`, and eligible cuBLAS
    contract routing without adding backend-specific public APIs or implicit
    CPU/GPU transfer.
- Workspace/target:
  - `/home/christos/Omni`, CUDA helper ABI, Tensor CUDA backend externs,
    public Tensor placement/realize/contract routing, focused advanced tests,
    TODO, changelog, plans, and Tensor docs.
- Code or configuration changes made:
  - Added runtime-loaded CUDA `Float32` fill support and cuBLAS `sgemm` /
    `sgemv` wrappers while preserving existing `Float64` cuBLAS availability
    as independent from optional `Sgemm` / `Sgemv` symbol resolution.
  - Broadened CUDA `to-device`, CPU copyback, destination-form `realize`,
    scalar destination fills, and supported lazy CUDA contract destination
    writes to dense row-major `Float32` Tensor storage.
  - Routed matching CUDA `Float32` rank-2/rank-2, rank-2/rank-1, and
    rank-1/rank-2 single-axis contractions through the new cuBLAS `Float32`
    helpers, preserving CUDA placement and `Float32` dtype.
  - Added `tensor-backends` CUDA/cuBLAS `float64` / `float32` capability
    fields, CUDA `Float32` placement/copyback/destination/contract tests, and
    fail-closed coverage for mixed CUDA dtypes and lazy CUDA `Float32` map.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and the CUDA/cuBLAS decision note so
    CUDA `Float32` placement/contract is no longer listed as current deferred
    work.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check -- csrc/tensor_cuda_helpers.c src/lisp/tensor_cuda_backend.c3 src/lisp/prim_tensor.c3 src/lisp/tests_advanced_stdlib_module_groups.c3 docs/reference/03-collections.md docs/plans/cuda-cublas-backend-decision-2026-04-16.md docs/LANGUAGE_SPEC.md docs/areas/tensor-scientific.md TODO.md memory/CHANGELOG.md .agents/PLAN.md docs/SESSION_REPORT.md .agents/SESSION_REPORT.md`
  - `./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- Key results:
  - Helper rebuild passed.
  - `c3c build --obj-out obj` passed with existing deprecation warnings.
  - Host focused `advanced-collections-module` passed `1218/0` and exercised
    the CUDA/cuBLAS `Float32` paths on the local CUDA stack.
  - Primitive docs parity, Stage 3 source parity, and targeted diff check
    passed.
  - Bounded-container focused `advanced-collections-module` passed `1205/0`.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not make `omni_tensor_backend_cublas_available()` depend on
    `cublasSgemm_v2` / `cublasSgemv_v2`; that would regress the older
    `Float64` cuBLAS contract on installations where only double symbols are
    available. Keep `Float32` cuBLAS availability as a separate probe.
- Current best recommendation or checkpoint:
  - CUDA `Float32` placement/copyback/destination/eligible cuBLAS contract is
    landed behind the existing explicit-device Tensor surface.
- Unresolved issues:
  - CUDA elementwise `map`, fixed-width complex Tensor layout, stride/view
    Vulkan dispatch, and broad/heavy parent-level container validation remain
    explicit TODO items. Scalar `Float32` and CUDA zero-size contract
    identity/fill are covered by later checkpoints.
- Dependencies, blockers, or restart requirements:
  - No long-running process was left active. A rebuild/restart is required for
    any external process that was started before this code change.
- Signature: GPT-5.4 Codex

## 2026-04-17 23:25 CEST - TENSOR-100F Vulkan Float32 Scientific Unary

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and expand dense
    row-major Vulkan `Float32` unary math beyond `sqrt` without hidden
    downcasts or the invalidated generic unary map branch.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan unary shaders/helpers, public Tensor unary
    dispatch, focused advanced tests, TODO, changelog, plans, and tensor docs.
- Code or configuration changes made:
  - Added dedicated Vulkan `Float32` unary opcodes for `sin`, `cos`, `tan`,
    `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, and
    `log10`, and regenerated the checked-in `Float32` SPIR-V C embed.
  - Routed public `(map <scientific-unary> <vulkan-float32-tensor>)` and
    direct Tensor unary math through the Vulkan `Float32` unary helper while
    preserving Vulkan placement and `Float32` dtype.
  - Kept Vulkan `Float64` scientific unary fail-closed after validation showed
    the current Vulkan 1.0 GLSL path rejects double transcendental builtins.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and the Vulkan plan docs so the landed
    `Float32` unary family and remaining deferred boundaries are explicit.
- Commands run and key results:
  - `glslangValidator` and `spirv-val` passed for the `Float32` and reverted
    `Float64` unary shaders.
  - `./scripts/build_omni_chelpers.sh`: passed after regenerating the
    checked-in `Float32` SPIR-V C embed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes passed for `map` `sin`/`cos`/`exp`/`log`, direct
    Tensor `log10`, and fail-closed Vulkan `Float64` `map sin`.
  - Host focused `advanced-collections-module`: `pass=1198 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=1185 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - `./scripts/build_omni_chelpers.sh` does not regenerate SPIR-V C embeds;
    shader changes must update the checked-in `_spv.c` file or runtime keeps
    the old shader behavior.
  - Do not implement Vulkan `Float64` scientific unary by downcasting to
    `Float32`. The current GLSL double-transcendental path failed validation,
    and silent precision loss would break the dtype contract.
- Current best recommendation / checkpoint:
  - Continue from scalar `Float32` runtime values or CUDA `Float32`
    placement/contract support before broader fixed-width complex or
    stride/view-backed Vulkan work.
- Unresolved issues:
  - Scalar `Float32` values, CUDA `Float32`, fixed-width complex,
    strided/view-backed Vulkan layouts, and measured Vulkan SVD performance
    remain open.
- Signature: Codex GPT-5.4

## 2026-04-18 09:03 CEST - TENSOR-100F Vulkan Float64 Normal Quantile Runtime Slice

- Objective attempted:
  - Implement only the Vulkan dense row-major `Float64` runtime/helper path for
    `stats/normal-quantile`, preserving the existing local `Float64`
    `stats/normal-cdf` `op == 19` path.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_normal_quantile_f64.comp` and generated
    `csrc/tensor_vulkan_normal_quantile_f64_spv.c`.
  - Added `omni_tensor_backend_vulkan_map_normal_quantile_f64` in
    `csrc/tensor_vulkan_helpers.c`, using the same status-buffer convention as
    the existing Float32 quantile helper: status `2` maps to non-finite input
    and outranks status `1` domain errors.
  - Registered the new SPIR-V source in `scripts/build_omni_chelpers.sh` and
    `project.json`, added the C3 extern in
    `src/lisp/tensor_vulkan_backend.c3`, and routed Vulkan `Float64`
    `op == 20` through the new helper in `src/lisp/prim_tensor.c3`.
- Key results:
  - Shader uses arithmetic-only bisection over the existing piecewise polynomial
    Float64 normal CDF approximation. It intentionally avoids GLSL
    `log(double)`, which the local Vulkan 1.0 shader toolchain rejects.
  - Direct and `map` `stats/normal-quantile` on Vulkan `Float64` tensors now
    preserve Vulkan placement and `Float64` dtype for valid probabilities.
  - Zero/one probabilities fail closed as
    `stats/normal-quantile: probability must be between 0 and 1`.
  - Non-finite inputs fail closed as
    `stats/normal-quantile: expected finite numeric input`, including when a
    domain error is also present in the same tensor.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_normal_quantile_f64.comp -o /tmp/tensor_vulkan_normal_quantile_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/tensor_vulkan_normal_quantile_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `git diff --check -- csrc/tensor_vulkan_normal_quantile_f64.comp csrc/tensor_vulkan_normal_quantile_f64_spv.c csrc/tensor_vulkan_helpers.c scripts/build_omni_chelpers.sh project.json src/lisp/tensor_vulkan_backend.c3 src/lisp/prim_tensor.c3`: passed.
  - `c3c build --obj-out obj`: passed with pre-existing deprecation warnings.
  - Guarded direct Vulkan `Float64` normal-quantile smoke returned `true`.
  - Guarded `map` Vulkan `Float64` normal-quantile smoke returned `true`.
  - Guarded zero-probability Vulkan smoke failed with the expected probability
    domain error.
  - Guarded mixed zero-plus-NaN Vulkan smoke failed with the expected
    non-finite priority error.
- Unresolved issues:
  - No tests or project docs were edited in this slice by request.
  - The arithmetic-only inverse is bounded by the polynomial CDF inversion range
    `[-8, 8]`; probabilities in the far double tails return the bounded
    approximation rather than a log-tail Acklam/Wichura-style value.
- Signature: Codex GPT-5.4

## 2026-04-18 08:56 CEST - Vulkan Float64 Quantile Test/Docs Staging

- Objective attempted:
  - Prepare focused test coverage and documentation/backlog state for Vulkan
    `Float64` `stats/normal-quantile` while leaving runtime/helper files to
    the concurrent implementation worker.
- Relevant workspace / target:
  - `/home/christos/Omni`, scoped to advanced stdlib module tests and
    documentation/backlog files for Vulkan Float64 distribution math.
- Code or configuration changes made:
  - Replaced the two unconditional Vulkan `Float64` quantile fail-closed
    checks in `src/lisp/tests_advanced_stdlib_module_groups.c3` with coverage
    for the landed Float64 helper contract.
  - The block checks direct and `map stats/normal-quantile` Vulkan placement,
    `Float64` dtype, and values for `0.025`, `0.5`, and `0.975`; it also
    checks invalid `0`/`1` probabilities and non-finite inputs return the
    scalar-compatible diagnostics without exposing output.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md` to mark Vulkan
    `Float64` quantile as landed and make CUDA-first dtype-changing rounding
    the next active Vulkan math-library item.
- Commands run and key results:
  - Memory search for prior Vulkan/CUDA quantile decisions: found CUDA
    quantile and Vulkan Float32 quantile landed, and a negative constraint
    against inferring Vulkan Float64 quantile support.
  - `jj status`: showed a heavily dirty concurrent tensor/backend worktree;
    changes were kept within the assigned files.
  - `rg` sweeps for `normal-quantile`/Vulkan Float64 support: the tree shifted
    while validating. A transient snapshot had helper references without the
    generated SPIR-V C source; the current snapshot includes
    `csrc/tensor_vulkan_normal_quantile_f64_spv.c`, project/build-script
    wiring, C helper symbols, and C3 externs.
  - `c3c build --obj-out obj`: first attempt failed at link during the
    transient missing-SPIR-V snapshot; rerun after concurrent helper artifacts
    landed passed and linked `build/main`.
  - `OMNI_LISP_TEST_SLICE=advanced
    OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module
    OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main
    --test-suite lisp`: passed with `pass=1317 fail=0`.
  - Targeted `git diff --check` on the assigned test/docs/backlog files:
    passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not claim Vulkan `Float64` quantile from Float32/CUDA/CDF adjacency
    alone; it requires its own Float64 helper/build artifact and tests. The
    current tree now has that artifact and focused tests pass.
- Current best recommendation / checkpoint:
  - Vulkan `Float64` `stats/normal-quantile` is now treated as landed in the
    docs/backlog, assuming the concurrent helper/runtime implementation is the
    intended final helper path. Continue with CUDA-first dtype-changing Tensor
    rounding unless a broader validation gate regresses the quantile helper.
- Unresolved issues:
  - This worker did not edit helper/runtime files and did not separately run
    `glslangValidator`/`spirv-val` for the Float64 quantile shader; validation
    here is build plus focused runtime tests.
- Signature: Codex GPT-5.4

## 2026-04-17 23:08 CEST - TENSOR-100F CPU Float32 Matrix Factor/SVD Contract

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and close the CPU
    `Float32` matrix factor/SVD public contract without hidden `Float64`
    widening or LAPACK fallback.
- Workspace/target:
  - `/home/christos/Omni`, CPU matrix primitives, focused advanced tests,
    TODO, changelog, plans, and tensor area docs.
- Code or configuration changes made:
  - Added native CPU `Float32` matrix helpers/workspaces for LU, solve,
    inverse, QR, Cholesky, and SVD/Jacobi singular-value paths.
  - Routed public CPU `Tensor Float32` surfaces for `matrix/determinant`,
    `matrix/lu`, `matrix/solve`, `matrix/inverse`, `matrix/cholesky`,
    `matrix/qr`, `matrix/singular-values`, `matrix/svd`, and SVD-backed
    `matrix/norm` selectors through native `Float32` implementations.
  - Replaced CPU `Float32` fail-closed tests with positive value/dtype tests
    and no-hidden-`d*` LAPACK counter guards.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`, plan docs,
    and tensor area docs so CPU `Float32` matrix factor/SVD is no longer
    listed as an open deferred boundary.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct CPU `Float32` smokes passed for determinant, solve, inverse, LU,
    QR, Cholesky, singular-values, SVD, and spectral/nuclear norm.
  - Host focused `advanced-collections-module`: `pass=1195 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=1182 fail=0`.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating CPU `Float32` matrix factor/SVD as deferred or
    fail-closed. Do not route these paths through `matrix_copy_float64_workspace`
    or the existing `dgesv`/`dgetrf`/`dgeqrf`/`dpotrf`/`dgesvd` helpers.
- Current best recommendation / checkpoint:
  - Continue from scalar `Float32` values, CUDA `Float32` placement/contract,
    fixed-width complex, stride/view-backed Vulkan layouts, measured Vulkan
    SVD performance, or additional Vulkan unary opcodes.
- Unresolved issues:
  - Scalar `Float32` values, CUDA `Float32`, fixed-width complex,
    strided/view-backed Vulkan layouts, and measured Vulkan SVD performance
    remain open.
- Signature: Codex GPT-5.4
