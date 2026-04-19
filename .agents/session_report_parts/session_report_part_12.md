# Session Report Index Part 12

Source: `.agents/SESSION_REPORT.md`

## 2026-04-17 08:04 CEST - TENSOR-100E/F Vulkan Unary Sqrt
- Objective attempted:
  - Add a real Vulkan path for public unary square root through the separate
    dense row-major `Float64` unary helper.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/tensor_vulkan_map_unary_f64.comp` so opcode `2` is
    `sqrt(value)` and regenerated `csrc/tensor_vulkan_map_unary_f64_spv.c`.
  - Widened `omni_tensor_backend_vulkan_map_unary_f64` opcode validation to
    accept `0..2`.
  - Routed `map sqrt` and direct Tensor `sqrt` to opcode `2` for Vulkan dense
    row-major `Float64` tensors.
  - Changed direct Tensor unary math on Vulkan to fail closed for unsupported
    operations instead of materializing hidden CPU results.
  - Added Vulkan `map sqrt`, result placement, direct `sqrt`, lazy direct
    `sqrt`, unsupported `map sin`, and CPU `map sqrt` parity regressions.
- Key results:
  - `(map sqrt <vulkan-tensor>)` and `(sqrt <vulkan-tensor>)` now return
    Vulkan-placed Tensor results for positive finite `Float64` inputs.
  - CPU inspection still requires explicit `(to-device ... 'cpu)`.
  - Unsupported Vulkan unary callables such as `sin` still raise
    `tensor/backend-unsupported`.
  - The generic Vulkan unary `map` mode-3 branch remains invalidated and out
    of use.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_unary_f64.comp -o
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `3.0`, `vulkan`, `2.0`, `vulkan`, and
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `pass=824 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=811 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Negative-domain Float64 `sqrt` parity is not expanded in this slice; if
    Omni wants stronger cross-driver NaN guarantees, add a focused decision and
    test rather than inferring it from positive-input coverage.
  - Continue broader Vulkan unary support only through explicitly validated
    helper opcodes. The earlier `sin(double)` shader failure remains a
    constraint for transcendental operations.

Signature: Codex GPT-5

## 2026-04-17 10:37 CEST - TENSOR-100F1 Vulkan Audit Hardening

Objective attempted:
- Continue auditing and fixing the Tensor/Vulkan SVD-backed slice using
  multiple agents, then close the audit findings with focused validation.

Workspace:
- `/home/christos/Omni`

Code/configuration changes made:
- Continued GPT-5.4 high audit coverage for source/device semantics and
  tests/docs. The requested GPT-5.3-Codex-Spark implementation workers were
  quota-blocked by the environment, so implementation proceeded locally with
  fallback fast workers limited to no-conflict report/roadmap review.
- Hardened `realize` semantics:
  - one-argument `realize` preserves concrete Vulkan placement;
  - explicit destination `realize` now rejects concrete or lazy Vulkan sources
    with `tensor/backend-unsupported` instead of falling into missing CPU
    storage diagnostics.
- Hardened Vulkan singular-values execution:
  - `omni_tensor_backend_vulkan_singular_values_f64` now dispatches one work
    item for the single-invocation shader while preserving the `k + 2`
    output/status/nuclear payload;
  - shader non-convergence now maps through
    `OMNI_TENSOR_VULKAN_NO_CONVERGENCE` to public `tensor/no-convergence`
    instead of generic `tensor/backend-execution-failed`;
  - the helper now rejects logical matrices whose element count exceeds the
    shader's 32-bit index space before dispatch.
- Broadened tests for Vulkan `matrix/singular-values`, spectral/nuclear
  `matrix/norm`, CPU-only direct `matrix/svd`/eigen fail-closed behavior on
  lazy Vulkan inputs, explicit-destination `realize`, and CPU-only numeric
  Tensor helpers on Vulkan operands.
- Added durable regressions for the `k == 64` singular-values boundary,
  rectangular `2x65`, empty `0x65`, status-payload non-convergence mapping,
  and the oversized-index validation guard.
- Updated TODO/docs/plan/changelog wording for the `k <= 64` cap, direct
  SVD/eigen CPU-only behavior, stale `sqrt` roadmap wording, zero-axis
  `contract` support, destination `realize`, and latest validation details.

Commands run and key results:
- `glslangValidator -V --target-env vulkan1.0 -o /tmp/omni_tensor_vulkan_singular_values_f64.spv csrc/tensor_vulkan_singular_values_f64.comp`: passed.
- `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_singular_values_f64.spv`: passed.
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=904 fail=0`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=411 fail=0`.
- Durable advanced regressions now cover:
  - `matrix/singular-values` at `k == 64`;
  - `matrix/singular-values` at `k == 65` returning
    `tensor/backend-unsupported`;
  - wide `2x65` singular-values shape;
  - empty `0x65` singular-values length;
  - status-payload mapping from shader non-convergence to
    `tensor/no-convergence`;
  - oversized logical index validation before Vulkan dispatch.
- `./scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- Targeted `git diff --check` over touched source/docs/generated files:
  passed after regenerating the SPIR-V C array without trailing whitespace.

Invalidated assumptions / failed approaches:
- Do not rely on shallow root `source.device` checks for lazy Tensor
  expressions. CPU-only helpers must inspect Tensor expression leaves before
  generic realization or they can accidentally execute hidden device paths.
- Do not treat the Vulkan singular-values shader status tail as a generic
  singular-matrix status. Its non-convergence status is now a separate
  backend code mapped to `tensor/no-convergence`.
- Do not rely only on per-axis `UINT32_MAX` checks in Vulkan helpers whose
  shaders index with 32-bit arithmetic; guard total logical element count
  before dispatch.
- Do not treat destination `realize` as an implicit GPU-to-CPU copy boundary;
  explicit `to-device 'cpu` is the copy boundary for non-CPU sources.

Current best recommendation / checkpoint:
- The `TENSOR-100F1` Vulkan singular-values helper is verified for the current
  dense row-major `Float64`, `k <= 64` contract. Next useful work should be a
  separate Vulkan factor-output SVD/eigensolver design, or a shared Vulkan
  helper cleanup that preserves the public contracts.

Unresolved issues:
- Full heavy/container-only gates were not run in this pass.
- Actual shader iteration-exhaustion is still not forced by a public numeric
  fixture; the status-payload mapping and C3 status-code mapping are now
  covered by deterministic probes.
- Direct `matrix/svd`, `matrix/eigenvalues`, `matrix/eigenvectors`, and
  `matrix/eigenpairs` still need dedicated Vulkan design/implementation before
  they can support Vulkan operands.

Signature: Codex GPT-5

## 2026-04-17 09:59 CEST - TENSOR-100F1 Vulkan SVD-Backed Singular Values

Objective attempted:
- Continue from the latest Vulkan math-library handoff using multiple agents,
  verify the in-progress Vulkan SVD-backed slice, and align the runtime
  contract with docs/plans.

Workspace:
- `/home/christos/Omni`

Code/configuration changes made:
- Preserved and verified the existing dense row-major Vulkan `Float64`
  `matrix/singular-values` implementation and spectral/nuclear `matrix/norm`
  routing through `omni_tensor_backend_vulkan_singular_values_f64`.
- Fixed `matrix/svd`, `matrix/eigenvalues`, `matrix/eigenvectors`, and
  `matrix/eigenpairs` so non-CPU Tensor placement is rejected before generic
  realization can silently copy Vulkan input through CPU paths.
- Updated current Tensor/Vulkan docs, TODO, plan, and changelog artifacts so
  `matrix/singular-values` plus `matrix/norm` `'spectral` / `'nuclear` are
  recorded as supported on Vulkan, while direct SVD/eigen surfaces remain
  CPU-only.

Commands run and key results:
- `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_singular_values_f64.comp -o /tmp/omni_tensor_vulkan_singular_values_f64.spv`: passed.
- `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_singular_values_f64.spv`: passed.
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing deprecation warnings.
- Direct Vulkan smokes returned `3.0`, `5.0`, `vulkan`, and empty length `0`.
- Initial focused `advanced-collections-module` run exposed stale/real
  unsupported-surface failures for direct `matrix/svd` and eigen surfaces.
- After the fix,
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  passed with `pass=866 fail=0`.

Invalidated assumptions / failed approaches:
- Do not treat direct `matrix/svd` or eigen surfaces as implicitly safe on
  Vulkan just because `matrix/singular-values` has a Vulkan helper. Those
  factor/eigensolver surfaces remain CPU-only until explicit Vulkan plans land.
- Do not allow CPU-only matrix primitives to call generic realization before
  checking source device placement; that can hide unsupported Vulkan input
  behind CPU copyback.

Current best recommendation / checkpoint:
- Continue `TENSOR-100F` from the verified Vulkan singular-value helper. Next
  useful implementation should either refactor shared Vulkan helper plumbing
  without changing exported contracts, or add another explicit backend-neutral
  kernel with dedicated Vulkan semantics.

Unresolved issues:
- Bounded-container and full heavy gates were not run in this pass.
- Direct `matrix/svd`, `matrix/eigenvalues`, `matrix/eigenvectors`, and
  `matrix/eigenpairs` still need dedicated Vulkan design/implementation before
  they can support Vulkan operands.

Signature: Codex GPT-5

## 2026-04-17 07:54 CEST - TENSOR-100E/F Vulkan Unary Negation
- Objective attempted:
  - Make unary `-` coherent across the documented scalar surface, CPU Tensor
    direct/map execution, and the Vulkan dense row-major `Float64` unary helper.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Changed the dispatched primitive registration for `-` so one-argument
    calls reach `prim_sub`, and tightened `prim_sub` to reject anything outside
    the `1..2` argument contract.
  - Added Tensor handling to unary numeric negation and implemented
    `tensor_neg_value` for CPU `Float64`, `BigInteger`, `BigFloat`, and
    `BigComplex` tensors.
  - Extended `csrc/tensor_vulkan_map_unary_f64.comp` so opcode `1` is
    negation, regenerated `csrc/tensor_vulkan_map_unary_f64_spv.c`, and
    widened the C helper opcode guard to accept `0..1`.
  - Extended the C3 Vulkan unary selector so `map -` and direct Tensor
    `(- tensor)` route to the separate unary helper for Vulkan dense row-major
    `Float64` inputs.
  - Added scalar, JIT, CPU Tensor, Big* Tensor, and Vulkan Tensor regressions.
- Key results:
  - `(- 1)` now works through the public primitive path, while `(- 1 2 3)`
    still raises an arity error.
  - `(- (Tensor ...))` and `(map - (Tensor ...))` now work on CPU tensors.
  - `(map - <vulkan-tensor>)` and `(- <vulkan-tensor>)` now return
    Vulkan-placed Tensor results and require explicit `(to-device ... 'cpu)`
    for CPU inspection.
  - Unsupported Vulkan unary callables such as `sqrt` still raise
    `tensor/backend-unsupported`.
  - The invalidated generic Vulkan unary `map` mode-3 branch remains out of
    use.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_unary_f64.comp -o
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct smokes returned `-1`, CPU Tensor `2.5`, Big* Tensor strings
    `-9223372036854775808`, `-1e+309`, and `-3+4i`, Vulkan map value `2.0`,
    Vulkan result device `vulkan`, direct Vulkan value `-1.5`, and
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `pass=819 fail=0`.
  - Host `basic` slice: `pass=144 fail=0`.
  - Host `compiler` slice: `pass=277 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=806 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Continue additional Vulkan unary arithmetic operations only through the
    separate unary helper pattern or another explicitly debugged helper ABI.
  - Do not resume the generic Vulkan unary `map` mode-3 branch.

Signature: Codex GPT-5

## 2026-04-17 07:41 CEST - TENSOR-100E/F Vulkan Unary Abs Helper
- Objective attempted:
  - Add a real Vulkan path for public unary absolute value without reviving the
    invalidated generic Vulkan `map` mode-3 branch.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_map_unary_f64.comp` and generated
    `csrc/tensor_vulkan_map_unary_f64_spv.c`.
  - Wired the generated shader into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_map_unary_f64`, a separate two-buffer
    Vulkan helper with its own push constants.
  - Added the C3 Vulkan extern and shared C3 construction path for dense
    row-major Vulkan `Float64` unary helper results.
  - Routed public `(map abs <vulkan-tensor>)` and direct Tensor
    `(abs <vulkan-tensor>)` through the helper.
  - Added availability-gated tests for `map abs` roundtrip, Vulkan result
    placement, direct Tensor `abs`, lazy Vulkan `abs`, and unsupported
    callable fail-closed behavior.
- Key results:
  - `map abs` and direct Tensor `abs` now return Vulkan-placed Tensor results
    for Vulkan dense row-major `Float64` inputs.
  - CPU inspection still requires explicit `(to-device ... 'cpu)`.
  - Unsupported unary Vulkan callables such as `sqrt` still raise
    `tensor/backend-unsupported`.
  - The generic Vulkan unary `map` mode-3 branch remains invalidated and out
    of use.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_unary_f64.comp -o
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `1.5`, `vulkan`, `1.5`, `vulkan`, and
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `pass=810 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=797 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Additional unary Vulkan operations should use separate helper ABIs and
    should not restart the generic map mode-3 branch.
  - Full blocked trailing-update LU remains a future performance lane only if
    later solve measurements justify it.

Signature: Codex GPT-5

## 2026-04-17 07:39 CEST - TENSOR-100G Measured Solve Threshold
- Objective attempted:
  - Replace the fixed Vulkan `matrix/solve` bring-up threshold with measured
    routing for the current serial and staged parallel `Float64` solve paths.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added private `OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_MIN_N = 65` and routed
    Vulkan dense row-major `Float64` solve systems with `n < 65` to the serial
    helper and `n >= 65` to the staged parallel helper.
  - Added `omni_tensor_backend_vulkan_solve_serial_call_count` plus the C3
    extern so tests can prove below-threshold serial execution directly.
  - Updated availability-gated solve tests for below-threshold serial routing,
    65x65 parallel threshold entry, pivot/factor counter movement, no-LAPACK
    behavior, and 65x65 parallel singular status.
- Key results:
  - Local in-process threshold logs under `build/vulkan_solve_threshold_20260417_*`
    showed `3`, `9`, `16`, and `32` tied or serial-favorable, while `65` was
    the first tested size where the staged parallel route won across identity
    and dense fixtures.
  - The serial helper still owns its private trailing `double` status sentinel;
    the parallel helper still owns its private typed `uint` status buffer.
  - Full blocked trailing-update LU remains a future performance lane only if
    later measurements justify it; it is not required for the current
    thresholded parallel solve contract.
- Commands run and key results:
  - Temporary high-threshold rebuilds were used only for measurement, then
    replaced by the final `n >= 65` route.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `9.0`, `1.0`, `1.0`, and
    `tensor/singular-matrix`.
  - Host focused `advanced-collections-module`: `pass=806 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=793 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Continue broader Vulkan math-library work from the thresholded solve
    baseline. Keep public Tensor surfaces backend-neutral and fail closed for
    unsupported Vulkan dtype/layout cases.

Signature: Codex GPT-5
