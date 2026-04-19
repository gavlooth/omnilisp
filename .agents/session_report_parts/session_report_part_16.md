# Session Report Index Part 16

Source: `.agents/SESSION_REPORT.md`

## 2026-04-17 01:14 CEST - TENSOR-100E Vulkan Map Broadcasting
- Objective attempted:
  - Extend the Vulkan `Float64` dense row-major `map` kernel from exact-shape
    Tensor/Tensor arithmetic to right-aligned singleton-axis Tensor/Tensor
    broadcasting through the same public `map` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Updated `csrc/tensor_vulkan_map_f64.comp` to compute broadcast operand
    offsets from output shape/strides and operand shape/strides.
  - Regenerated `csrc/tensor_vulkan_map_f64_spv.c`.
  - Extended `omni_tensor_backend_vulkan_map_f64` in
    `csrc/tensor_vulkan_helpers.c` with a fourth metadata storage buffer.
  - Updated the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Relaxed `src/lisp/prim_tensor.c3` Vulkan `map` gates to use the CPU Tensor
    right-aligned broadcast compatibility checks.
  - Added focused availability-gated broadcast regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated TODO, Tensor docs/plans, `.agents/PLAN.md`, and
    `memory/CHANGELOG.md`.
- Key results:
  - Vulkan `map` now supports `+`, `-`, `*`, and `/` for Tensor/scalar,
    scalar/Tensor, exact-shape Tensor/Tensor, and right-aligned singleton-axis
    Tensor/Tensor broadcasting.
  - Results remain Vulkan-placed tensors; callers still copy back explicitly
    with `to-device 'cpu`.
  - Incompatible broadcast shapes, unsupported callables, unsupported dtypes,
    and mixed CPU/Vulkan operands still fail closed.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0`: passed.
  - `spirv-val --target-env vulkan1.0`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan broadcast smokes returned `36.0`, `32.0`, `11.0`, and
    `vulkan`.
  - Host focused `advanced-collections-module`: `683 passed, 0 failed`.
  - Bounded focused `advanced-collections-module`: `670 passed, 0 failed`.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100E` by extending the metadata-buffer pattern to the next
    eligible `Float64` kernel family or to stride-aware views only after layout
    and aliasing metadata are explicit.
- Signature: Codex GPT-5

## 2026-04-17 01:01 CEST - TENSOR-100E Vulkan Map Arithmetic
- Objective attempted:
  - Broaden the policy-backed Vulkan `Float64` dense row-major `map` kernel
    family beyond add-scalar while preserving the backend-neutral public
    `map` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_map_f64.comp` as the checked-in GLSL source for
    the Vulkan `Float64` elementwise map shader family.
  - Added generated `csrc/tensor_vulkan_map_f64_spv.c` and wired it through
    `project.json` and `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_map_f64` in the runtime-loaded Vulkan
    helper and exposed it through `src/lisp/tensor_vulkan_backend.c3`.
  - Routed public `map` in `src/lisp/prim_tensor.c3` for Vulkan-placed
    dense row-major `Float64` Tensor/scalar, scalar/Tensor, and exact-shape
    Tensor/Tensor arithmetic.
  - Added focused availability-gated regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated TODO, Tensor docs/plans, `.agents/PLAN.md`, and
    `memory/CHANGELOG.md`.
- Key results:
  - Vulkan `map` now supports `+`, `-`, `*`, and `/` for Tensor/scalar,
    scalar/Tensor, and exact-shape Tensor/Tensor operands.
  - Results remain Vulkan-placed tensors; callers still copy back explicitly
    with `to-device 'cpu` for CPU inspection.
  - Unary callables, unsupported callables, unsupported dtypes, mixed
    CPU/Vulkan operands, and broadcasting shapes fail closed with Tensor
    backend diagnostics.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0`: passed.
  - `spirv-val --target-env vulkan1.0`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan map smokes returned `8.0`, `12.0`, `26.0`, `80.0`,
    `90.0`, `4.0`, `vulkan`, and `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `679 passed, 0 failed`.
  - Bounded focused `advanced-collections-module`: `666 passed, 0 failed`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`:
    passed.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100E` with Vulkan `Float64` map broadcasting or a rank-N
    dense descriptor only after the required shape/stride metadata is explicit.
- Signature: Codex GPT-5

## 2026-04-17 00:49 CEST - TENSOR-100E Vulkan Dtype Layout Policy
- Objective attempted:
  - Close the Vulkan dtype/layout policy decision before adding more public
    backend behavior.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`.
  - Linked the policy from TODO, the plans index, Vulkan decision note, Tensor
    area status, integrated Tensor plan, and `.agents/PLAN.md`.
  - Corrected `docs/reference/11-appendix-primitives.md` so the Tensor row
    lists the current four native Tensor dtypes.
- Key results:
  - Policy is now explicit: keep extending fixed-width `Float64` dense
    row-major Vulkan kernels first; do not downcast to `Float32`; do not lower
    pointer-backed `BigInteger`, `BigFloat`, or `BigComplex` tensors to Vulkan;
    defer fixed-width complex and stride-aware layouts until those contracts
    exist.
  - The next Vulkan implementation boundary is a policy-backed `Float64`
    dense row-major `map` kernel family for Tensor/scalar and Tensor/Tensor
    elementwise arithmetic through public `map`.
- Commands run and key results:
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`:
    passed.
- Current best recommendation / checkpoint:
  - Implement the Float64 Vulkan `map` kernel family next, preserving
    backend-neutral public surface and fail-closed diagnostics.
- Signature: Codex GPT-5

## 2026-04-17 00:44 CEST - TENSOR-100E Vulkan Contract Dispatch Hardening
- Objective attempted:
  - Harden the Vulkan generic `Float64` single-axis `contract` execution layout
    without changing the public Tensor surface or supported dtype/layout gate.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_contract_f64.comp` as the checked-in GLSL source
    for the generic Vulkan contract shader.
  - Regenerated `csrc/tensor_vulkan_contract_f64_spv.c` from that GLSL source.
  - Updated `omni_tensor_backend_vulkan_contract_f64` to use shape-aware 16x4
    rank-2 output dispatch instead of flattening all non-scalar output through
    one-dimensional workgroups.
  - Updated TODO, Tensor area/planning docs, `.agents/PLAN.md`, session
    reports, and `memory/CHANGELOG.md`.
- Key results:
  - Public behavior remains unchanged: Vulkan `contract` still supports dense
    row-major `Float64` rank-1/rank-2 single-axis layouts and still fails
    closed for unsupported dtype/layout/device cases.
  - The next open Vulkan Tensor item is now the broader dtype/layout policy
    decision before any new public behavior is added.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0`: passed.
  - `spirv-val --target-env vulkan1.0`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan contract smokes returned `154.0`, `60.0`, `167.0`, `69.0`,
    `122.0`, `69.0`, `46.0`, `43.0`, `vulkan`,
    `tensor/backend-unsupported`, and zero-free length `0`.
  - Host focused `advanced-collections-module`: `674 passed, 0 failed`.
  - Bounded focused `advanced-collections-module`: `661 passed, 0 failed`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`:
    passed.
- Current best recommendation / checkpoint:
  - Decide the Vulkan dtype/layout policy as a separate slice. Preserve the
    backend-neutral `Tensor`/`map`/`contract`/`to-device` surface, explicit
    placement/copyback, and fail-closed diagnostics.
- Signature: Codex GPT-5

## 2026-04-17 00:20 CEST - TENSOR-100E Vulkan Zero-Size Contract
- Objective attempted:
  - Align Vulkan single-axis `contract` zero-size behavior with the CPU Tensor
    oracle for the supported rank-1/rank-2 `Float64` layouts.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Updated `omni_tensor_backend_vulkan_contract_f64` so zero-element results
    succeed without a buffer handle and zero-contracted non-empty results
    allocate and zero-fill Vulkan output storage.
  - Relaxed Vulkan C3 dispatch validation so zero contracted axes and zero
    free dimensions are accepted for supported rank/layout combinations.
  - Added availability-gated Vulkan regressions for zero-size rank-1 dot,
    rank-2/rank-2 identity output, zero-length rank-2 output, zero-length
    Vulkan residency, rank-2/rank-1 identity output, and rank-1/rank-2
    identity output.
  - Updated TODO, Tensor docs/plans, area status, session reports, `.agents`
    plan, and `memory/CHANGELOG.md`.
- Key results:
  - Zero-size contracted axes now produce additive-identity output in Vulkan
    storage, matching CPU Tensor semantics.
  - Zero free dimensions preserve zero-length result shapes and remain
    Vulkan-placed.
- Commands run and key results:
  - Direct Vulkan zero-size smokes returned `0.0`, `0.0`, `0`, `vulkan`,
    `0.0`, `0`, `0.0`, and `0`.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Host focused `advanced-collections-module`: `674 passed, 0 failed`.
  - Bounded focused `advanced-collections-module`: `661 passed, 0 failed`.
  - Bounded full `advanced`: `1978 passed, 0 failed`.
  - Bounded `memory-lifetime-smoke`: `227 passed, 0 failed`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`:
    passed.
  - `c3c build --sanitize=address --obj-out obj`: still blocked by the local
    C3 sanitizer platform guard.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100E` with Vulkan contract performance/tiling hardening
    and broader dtype/layout policy.
- Signature: Codex GPT-5

## 2026-04-16 23:58 CEST - TENSOR-100E Vulkan Generic Contract
- Objective attempted:
  - Continue `TENSOR-100E` from rank-1 dot into generic dense row-major
    Vulkan `Float64` single-axis `contract` support for ranks 1 and 2.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_contract_f64_spv.c`, an embedded SPIR-V generic
    contract shader for dense row-major `Float64` Tensor layouts.
  - Extended `csrc/tensor_vulkan_helpers.c` with
    `omni_tensor_backend_vulkan_contract_f64`.
  - Routed public `contract` on Vulkan-placed dense row-major `Float64`
    rank-1/rank-2 operands through the generic Vulkan helper when exactly one
    axis is contracted.
  - Added availability-gated advanced tests for all supported rank-2/rank-2,
    rank-2/rank-1, and rank-1/rank-2 axis orientations, Vulkan result
    residency, and multi-axis fail-closed behavior.
  - Updated TODO, Vulkan backend decision, Tensor area status, integrated
    Tensor plan, language spec, collections reference, plans index,
    session reports, `.agents/PLAN.md`, and `memory/CHANGELOG.md`.
- Key results:
  - Public `contract` now supports Vulkan-placed dense row-major `Float64`
    tensors of ranks 1 and 2 with one contracted axis.
  - Supported layouts are rank-1/rank-1 dot (`[0 0]` or explicit `[0] [0]`),
    rank-2/rank-2 `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`,
    rank-2/rank-1 `[1 0]` and `[0 0]`, and rank-1/rank-2 `[0 0]` and
    `[0 1]`.
  - Results remain Vulkan-placed tensors; callers use `to-device 'cpu` before
    CPU inspection.
  - Unsupported Vulkan contract cases, including multi-axis contractions,
    unsupported ranks/layouts/dtypes, and mixed CPU/Vulkan operands, fail
    closed with Tensor backend diagnostics. Zero-size contracted axes were
    aligned with CPU semantics in the follow-up zero-size slice.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct smokes returned `154.0`, `60.0`, `167.0`, `69.0`, `122.0`,
    `69.0`, `46.0`, `43.0`, `vulkan`, and `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `668 passed, 0 failed`.
  - Bounded focused `advanced-collections-module`: `655 passed, 0 failed`.
  - Bounded full `advanced`: `1972 passed, 0 failed`.
  - Bounded `memory-lifetime-smoke`: `227 passed, 0 failed`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`:
    passed.
  - `c3c build --sanitize=address --obj-out obj`: still blocked by the local
    C3 sanitizer platform guard.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100E` with performance/tiling hardening for the Vulkan
    contract kernel and a broader dtype/layout policy decision before adding
    more public behavior.
- Signature: Codex GPT-5

## 2026-04-16 23:44 CEST - TENSOR-100E Vulkan Rank-1 Dot
- Objective attempted:
  - Continue `TENSOR-100E` from Vulkan placement and add-scalar `map` into the
    first Vulkan reduction kernel behind the public backend-neutral `contract`
    surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_contract_dot_f64_spv.c`, an embedded SPIR-V
    compute reduction shader for rank-1/rank-1 `Float64` dot.
  - Extended `csrc/tensor_vulkan_helpers.c` with a shared Vulkan backend
    context and `omni_tensor_backend_vulkan_contract_dot_f64`.
  - Wired the new shader blob through `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Routed public `contract` on two Vulkan `Float64` rank-1 tensors through
    the Vulkan dot helper for `[0 0]` and explicit `[0] [0]`.
  - Added availability-gated advanced tests for roundtrip, explicit axes,
    device residency, mixed-device rejection, and rank-2 fail-closed behavior.
  - Updated TODO, Vulkan backend decision, Tensor area status, integrated
    Tensor plan, language spec, collections reference, plans index,
    session reports, `.agents/PLAN.md`, and `memory/CHANGELOG.md`.
- Key results:
  - Public `contract` now supports two Vulkan `Float64` rank-1 tensors with
    one contracted axis (`[0 0]` or explicit `[0] [0]`), returning a Vulkan
    scalar Tensor.
  - The explicit transfer contract is preserved: callers use `to-device 'cpu`
    before CPU inspection.
  - Unsupported Vulkan contract shapes, rank-2 contractions, and mixed
    CPU/Vulkan operands remain fail-closed with `tensor/backend-unsupported`.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed.
  - Direct smokes returned `140.0`, `140.0`, `vulkan`, and
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `659 passed, 0 failed`.
  - Bounded focused `advanced-collections-module`: `646 passed, 0 failed`.
  - Bounded full `advanced`: `1963 passed, 0 failed`.
  - Bounded `memory-lifetime-smoke`: `227 passed, 0 failed`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
  - `c3c build --sanitize=address --obj-out obj`: attempted, but the local C3
    toolchain reported address sanitizer unsupported on this platform.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100E` with rank-2 Vulkan `contract` kernels while
    preserving backend-neutral surface names, explicit device movement, and
    fail-closed backend diagnostics.
- Signature: Codex GPT-5

## 2026-04-16 21:20 CEST - TENSOR-100E Vulkan SPIR-V Map Kernel

Objective attempted:
- Continue `TENSOR-100E` from placement/copy into the first real Vulkan compute
  kernel while preserving the backend-neutral `Tensor`/`map`/`to-device`
  surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `csrc/tensor_vulkan_map_add_scalar_f64_spv.c`, an embedded SPIR-V
  add-scalar compute shader generated locally with `glslangValidator`.
- Extended `csrc/tensor_vulkan_helpers.c` with dynamic Vulkan compute symbols,
  descriptor/pipeline/command-buffer setup, synchronous queue dispatch, and
  `omni_tensor_backend_vulkan_map_add_scalar_f64`.
- Refactored Vulkan buffers to use a shared context plus refcounted handles so
  map output can safely share the input device and boundary copies can retain
  real Vulkan handles without double-freeing buffers.
- Added C3 extern/status wiring for Vulkan unsupported/execution failures and
  retained Vulkan handles.
- Routed public `map +` with one Vulkan `Float64` Tensor operand and one
  Float64 scalar through the SPIR-V kernel, returning a Vulkan-placed Tensor.
- Fixed `to-device 'cpu` for lazy expressions that realize to a non-CPU Tensor
  by realizing first and then copying the realized device value back to CPU.
- Added focused availability-gated tests for Vulkan map scalar-right,
  scalar-left, and unsupported-callable diagnostics.
- Updated TODO, Tensor docs, Vulkan plan, area status, integrated Tensor plan,
  and memory changelog.

Key results:
- Direct host smokes returned `8.0` for `(map + vulkan-tensor 5.0)` copied
  back to CPU and `12.0` for `(map + 10.0 vulkan-tensor)` copied back to CPU.
- Public `map`, not only private `__tensor-map`, now works for the first Vulkan
  kernel path.
- Unsupported Vulkan map callables fail closed with `tensor/backend-unsupported`.
- `contract` remains intentionally fail-closed until the Vulkan contraction
  kernel slice lands.

Commands run and key results:
- `sudo apt-get update && sudo apt-get install -y glslang-tools`: installed
  local shader tooling used to generate the checked-in SPIR-V C blob.
- `glslangValidator -V --target-env vulkan1.0`: generated the add-scalar
  SPIR-V module.
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct host Vulkan map smokes: returned `8.0`, `vulkan`, and `12.0`.
- Direct unsupported-callable smoke: returned `tensor/backend-unsupported`.
- Host focused `advanced-collections-module`: `655 passed, 0 failed`.
- Bounded-container focused `advanced-collections-module`: `642 passed, 0 failed`.
- Bounded-container full `advanced`: `1959 passed, 0 failed`.
- Bounded-container `memory-lifetime-smoke`: `227 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Unresolved issues / next actions:
- Next Vulkan compute work should add reductions/rank-1 dot or rank-2
  `contract` kernels, preserving explicit device movement and fail-closed
  diagnostics.

Signature: Codex GPT-5
