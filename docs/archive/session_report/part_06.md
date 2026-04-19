  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    non-`Float64` Vulkan dtypes, and Vulkan unary scientific `map` remain
    unsupported.

Signature: Codex GPT-5

## 2026-04-17 02:35 CEST - TENSOR-100E Vulkan Multi-Axis Contract
- Objective attempted:
  - Continue `TENSOR-100E` by extending the Vulkan dense row-major `Float64`
    rank-N `contract` path from exactly one contracted axis pair to one or more
    explicit contracted axis pairs through the existing backend-neutral public
    `contract` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Updated `csrc/tensor_vulkan_contract_f64.comp` so contracted axes are read
    from metadata lists instead of single push-constant axes, and each output
    invocation flattens the full contracted coordinate space.
  - Regenerated `csrc/tensor_vulkan_contract_f64_spv.c`.
  - Updated `omni_tensor_backend_vulkan_contract_f64` in
    `csrc/tensor_vulkan_helpers.c` to validate/pass axis-list metadata and keep
    zero-contracted output zero-fill behavior.
  - Updated `src/lisp/tensor_vulkan_backend.c3` and
    `src/lisp/prim_tensor.c3` so public Vulkan `contract` accepts one or more
    explicit axis pairs for dense row-major `Float64` operands.
  - Added availability-gated scalar and rank-3 multi-axis Vulkan regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `contract` now supports Vulkan-placed dense row-major rank-N
    `Float64` tensors with one or more contracted axis pairs.
  - Output axes remain ordered as free left axes followed by free right axes.
  - Results remain Vulkan-placed tensors; CPU inspection still requires
    explicit `to-device 'cpu`.
  - Zero-axis contractions, unsupported layouts/dtypes, and mixed CPU/Vulkan
    operands still fail closed with Tensor backend diagnostics.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_contract_f64.comp -o /tmp/omni_tensor_vulkan_contract_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_contract_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `70.0` for scalar multi-axis contract,
    `210.0` for rank-3 multi-axis contract, `460.0` for existing rank-N
    single-axis contract, `8.0` for binary Vulkan `map +`, and
    `tensor/backend-unsupported` for unsupported Vulkan `map sqrt`.
  - Host focused `advanced-collections-module`: `pass=689 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=676 fail=0`.
- Invalidated assumptions / failed approaches:
  - The earlier generic mode-3 Vulkan unary `map` branch remains rolled back and
    should not be resumed as the default baseline.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    and non-`Float64` Vulkan dtypes remain unsupported.
  - The next `TENSOR-100E` slice should move to another backend-neutral
    `Float64` kernel family, such as structural matrix transpose, or explicitly
    design layout/aliasing metadata before enabling stride-aware dispatch.

Signature: Codex GPT-5

## 2026-04-17 01:40 CEST - TENSOR-100E Vulkan Rank-N Contract
- Objective attempted:
  - Continue `TENSOR-100E` by extending the Vulkan `Float64` dense row-major
    single-axis `contract` path from rank-1/rank-2 coverage to rank-N coverage
    through the existing backend-neutral public `contract` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Replaced `csrc/tensor_vulkan_contract_f64.comp` with a
    rank/shape/stride metadata-buffer shader for dense row-major single-axis
    contractions.
  - Regenerated `csrc/tensor_vulkan_contract_f64_spv.c`.
  - Extended `omni_tensor_backend_vulkan_contract_f64` in
    `csrc/tensor_vulkan_helpers.c` to accept full operand/output rank, shape,
    and stride metadata, added the metadata storage buffer, switched generic
    dispatch to flat rank-N output indexing, and fixed the descriptor pool
    size for the four-buffer layout.
  - Updated `src/lisp/tensor_vulkan_backend.c3` and
    `src/lisp/prim_tensor.c3` so public Vulkan `contract` accepts dense
    row-major `Float64` tensors of arbitrary rank when exactly one axis pair
    is contracted.
  - Added availability-gated rank-3/rank-2, rank-2/rank-3, rank-3/rank-3,
    residency, and zero-contracted rank-3 regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated TODO, plan, reference/spec, Vulkan decision/policy, Tensor area
    status, and changelog documentation.
- Key results:
  - Public `contract` now supports Vulkan-placed dense row-major rank-N
    `Float64` tensors with exactly one contracted axis.
  - Output axes are ordered as free left axes followed by free right axes.
  - Results remain Vulkan-placed tensors; CPU inspection still requires
    explicit `to-device 'cpu`.
  - Multi-axis contractions, unsupported layouts/dtypes, and mixed CPU/Vulkan
    operands still fail closed with Tensor backend diagnostics.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_contract_f64.comp -o /tmp/omni_tensor_vulkan_contract_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_contract_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan rank-N contract smokes returned `460.0`, `139.0`,
    `3340.0`, `vulkan`, and `0.0`.
  - Host focused `advanced-collections-module`: `pass=688 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=675 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Invalidated assumptions / failed approaches:
  - A follow-up Vulkan unary `map` experiment was rolled back before completion.
    GLSL `double` transcendental calls such as `sin(double)` failed shader
    compilation, and an arithmetic-only generic mode-3 unary branch for
    `abs`/negation compiled but failed at runtime with
    `map: Vulkan Float64 kernel failed` while binary Vulkan `map` and rank-N
    `contract` remained healthy.
  - Do not resume that generic mode-3 branch as the baseline. If unary Vulkan
    map is pursued, use a separately debugged unary shader/helper entrypoint or
    first diagnose the current descriptor/dispatch failure. Do not use
    `Float32` casts or a hidden CPU fallback.
  - After rollback, direct sanity checks returned `8.0` for binary Vulkan
    `map +`, `460.0` for rank-N Vulkan `contract`, and
    `tensor/backend-unsupported` for unsupported Vulkan `map sqrt`.
    `c3c build --obj-out obj`, host focused `advanced-collections-module`
    (`pass=688 fail=0`), and bounded-container focused
    `advanced-collections-module` (`pass=675 fail=0`) still passed.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, and multi-axis Vulkan
    contractions remain unsupported.
  - The next `TENSOR-100E` slice should move to the next eligible dense
    row-major `Float64` kernel family, or explicitly design layout/aliasing
    metadata before enabling stride-aware dispatch. Unary Vulkan `map` remains
    open, but should not continue through the rolled-back generic mode-3 branch.
  - `jj status` shows the expected broad dirty worktree from the ongoing
    Tensor/CUDA/LAPACK/Vulkan lane, including unrelated prior changes and the
    untracked `out` file.

Signature: Codex GPT-5

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
- Objective attempted:
  - Continue `TENSOR-100E` from placement/copy into the first real Vulkan
    compute kernel while preserving the backend-neutral `Tensor`/`map`/
    `to-device` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_map_add_scalar_f64_spv.c`, an embedded SPIR-V
    add-scalar compute shader generated locally with `glslangValidator`.
  - Extended `csrc/tensor_vulkan_helpers.c` with dynamic Vulkan compute
    symbols, descriptor/pipeline/command-buffer setup, synchronous queue
    dispatch, and `omni_tensor_backend_vulkan_map_add_scalar_f64`.
  - Refactored Vulkan buffers to use a shared context plus refcounted handles
    so map output can safely share the input device and boundary copies can
    retain real Vulkan handles without double-freeing buffers.
  - Added C3 extern/status wiring for Vulkan unsupported/execution failures and
    retained Vulkan handles.
  - Routed public `map +` with one Vulkan `Float64` Tensor operand and one
    Float64 scalar through the SPIR-V kernel, returning a Vulkan-placed Tensor.
  - Fixed `to-device 'cpu` for lazy expressions that realize to a non-CPU
    Tensor by realizing first and then copying the realized device value back to
    CPU.
  - Added focused availability-gated tests for Vulkan map scalar-right,
    scalar-left, and unsupported-callable diagnostics.
  - Updated TODO, Tensor docs, Vulkan plan, area status, integrated Tensor plan,
    `.agents/PLAN.md`, `.agents/SESSION_REPORT.md`, and `memory/CHANGELOG.md`.
- Key results:
  - Direct host smokes returned `8.0` for `(map + vulkan-tensor 5.0)` copied
    back to CPU and `12.0` for `(map + 10.0 vulkan-tensor)` copied back to CPU.
  - Public `map`, not only private `__tensor-map`, works for the first Vulkan
    kernel path.
  - Unsupported Vulkan map callables fail closed with
    `tensor/backend-unsupported`.
  - `contract` remains intentionally fail-closed until the Vulkan contraction
    kernel slice lands.
- Commands run and key results:
  - `sudo apt-get update && sudo apt-get install -y glslang-tools`: installed
    local shader tooling used to generate the checked-in SPIR-V C blob.
  - `glslangValidator -V --target-env vulkan1.0`: generated the add-scalar
    SPIR-V module.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
  - Direct host Vulkan map smokes: returned `8.0`, `vulkan`, and `12.0`.
  - Direct unsupported-callable smoke: returned `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `655 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `642 passed,
    0 failed`.
  - Bounded-container full `advanced`: `1959 passed, 0 failed`.
  - Bounded-container `memory-lifetime-smoke`: `227 passed, 0 failed`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Invalidated assumptions / failed approaches:
  - Do not implement Vulkan `map` by returning a lazy map expression that
    captures a Vulkan Tensor operand; public `map` uses dispatched overloads
    and the boundary can attempt an escape copy of the opaque device handle.
    Supported Vulkan map cases should route eagerly, and real Vulkan buffer
    handles need explicit retain ownership before boundary clones can keep
    them.
- Current best recommendation / checkpoint:
  - Next Vulkan compute work should add reductions/rank-1 dot or rank-2
    `contract` kernels, preserving explicit device movement and fail-closed
    diagnostics.
- Signature: Codex GPT-5

## 2026-04-15 19:07 CEST - Tensor GCD/LCM Semantics
- Objective attempted:
  - Retry native Tensor `gcd` and `lcm` support after the prior raw-handle
    attempt produced corrupted tensor-scalar results.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Routed `gcd` and `lcm` through Tensor handling when either argument is a
    Tensor.
  - Added a shared exact-integer Tensor binary helper for tensor-scalar,
    scalar-tensor, and broadcast tensor-tensor operation.
  - Tensor operands must be native `BigInteger` tensors; inexact and complex
    Tensor dtypes fail closed.
  - Results are native `BigInteger` tensors.
  - Lazy BigInteger Tensor operands are realized before evaluation.
  - The final helper uses a raw BigInteger Tensor kernel: Tensor element
    handles are borrowed from Tensor storage, `Integer` scalars route through
    the existing `i64` BigInteger C ABI helpers, and `BigInteger` scalars use
    their existing scoped handles.
  - Added advanced collections/module regressions for exact tensor-scalar
    results, dtype, broadcast tensor-tensor, lazy realization, and Double
    Tensor rejection.
  - Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (ref (gcd (Tensor BigInteger [1] [(BigInteger \"6\")]) 6) [0]))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (ref (lcm (Tensor BigInteger [1] [(BigInteger \"6\")]) 6) [0]))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (ref (lcm (Tensor BigInteger [2 1] [(BigInteger \"3\") (BigInteger \"4\")]) (Tensor BigInteger [1 2] [(BigInteger \"5\") (BigInteger \"6\")])) [1 1]))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(gcd (Tensor Double [1] [6.0]) 3)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Direct smokes returned `"6"` for tensor-scalar `gcd`, `"6"` for
    tensor-scalar `lcm`, and `"12"` for broadcast Tensor/Tensor `lcm`.
  - Double Tensor input fails closed with
    `gcd: tensor inputs must be exact integers`.
  - Host targeted `advanced-collections-module` group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=392 fail=0`.
  - Bounded container `advanced-collections-module` group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=392 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - ASAN build attempt failed immediately with the local C3 compiler's
    platform support message.
- Invalidated assumptions or failed approaches worth preserving:
  - The manufactured scalar-handle Tensor helper path is not trustworthy here:
    tensor-tensor raw handles worked, but tensor-scalar `gcd` returned `"1"`
    and tensor-scalar `lcm` returned corrupted large values even after moving
    scalar cleanup out to function scope. The working fast path does not
    manufacture scalar handles; it uses existing scalar values through the
    `i64` or borrowed BigInteger C ABI variants.
- Current best recommendation:
  - Treat Tensor `gcd` and `lcm` as closed for native BigInteger Tensor
    inputs, with broadcast as an internal shape rule only.
- Unresolved issues:
  - ASAN coverage remains unavailable through the local C3 compiler invocation
    until proven otherwise.
- Next actions:
  - Commit and push the Tensor `gcd`/`lcm` slice.

Signature: GPT-5 Codex

## 2026-04-15 16:34 CEST - Tensor Min/Max Semantics
- Objective attempted:
  - Continue precision Tensor work by adding ordered real comparison for
    native Tensor inputs via `min` and `max`.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Routed `min` and `max` through Tensor handling when either argument is a
    Tensor.
  - Added native Tensor min/max handling for tensor-scalar, scalar-tensor, and
    broadcast tensor-tensor inputs.
  - Result dtype policy is `BigFloat` if either input is BigFloat, `Double` if
    either input is Double, otherwise `BigInteger`.
  - Integer scalar comparisons now normalize into BigInteger Tensor storage so
    exact integer comparisons stay exact.
  - Lazy Tensor operands are realized before ordered comparison.
  - Complex Tensor inputs fail closed.
  - Added advanced collections/module regressions for Double, BigInteger,
    BigFloat, broadcast tensor-tensor, lazy BigFloat, and complex rejection
    coverage.
  - Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (ref (min (Tensor Double [1] [3.7]) 4.2) [0]))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (dtype (min (Tensor BigInteger [1] [(BigInteger \"5\")]) 4)))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (ref (max (Tensor BigFloat [1] [(BigFloat \"2.5\")]) 4) [0]))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (dtype (max (Tensor BigFloat [1] [(BigFloat \"2.5\")]) 4)))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(min (Tensor BigComplex [1] [(BigComplex 1 0)]) 1)'`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Direct smokes confirmed Double Tensor min/max preserves Double dtype,
    BigInteger Tensor comparisons stay exact, BigFloat Tensor comparisons
    preserve BigFloat dtype, and complex operands fail closed.
  - Host targeted `advanced-collections-module` group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=387 fail=0`.
  - Bounded container `advanced-collections-module` group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=387 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed immediately with the local C3 compiler's
    platform support message.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not classify scalar `Int` operands as Double in Tensor min/max when the
    other side is exact integer storage; preserve BigInteger result dtype.
- Current best recommendation:
  - Treat Tensor min/max as closed for real ordered native Tensor dtypes.
- Unresolved issues:
  - Public LAPACK solver/decomposition naming remains unresolved.
  - ASAN coverage remains unavailable through the local C3 compiler invocation
    until proven otherwise.
- Next actions:
  - Commit and push the Tensor min/max slice.

Signature: GPT-5 Codex

## 2026-04-15 14:38 CEST - Tensor Rounding Semantics
- Objective attempted:
  - Continue precision Tensor work by adding exact-integer rounding results for
    real Tensor inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
