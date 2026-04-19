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
    `docs/reference/03-collections.md`, `memory/CHANGELOG.md`, and
    `.agents/PLAN.md` to remove stale validation/doc wording.
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
