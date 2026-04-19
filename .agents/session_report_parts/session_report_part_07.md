# Session Report Index Part 07

Source: `.agents/SESSION_REPORT.md`

## 2026-04-18 02:23 CEST - CUDA Arithmetic Unary Map Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by landing CUDA
    arithmetic/component unary map support without adding hidden CPU fallback
    or pretending scientific CUDA math is implemented.
- Workspace/target:
  - `/home/christos/Omni`, embedded CUDA map PTX, CUDA unary helper ABI,
    Tensor CUDA map routing, direct Tensor unary primitives, focused advanced
    tests, TODO, changelog, plans, and Tensor docs.
- Code or configuration changes made:
  - Added embedded PTX entries `omni_cuda_map_unary_f64` and
    `omni_cuda_map_unary_f32` plus C ABI helpers
    `omni_tensor_backend_cuda_map_unary_f64` and
    `omni_tensor_backend_cuda_map_unary_f32`.
  - Wired C3 externs and added `tensor_cuda_map_unary_value` for dense
    row-major `Float64`/`Float32` CUDA tensors.
  - Routed public direct `map`, lazy map realization, CUDA destination
    realization, and direct Tensor `abs`, unary `-`, `sqrt`, `real-part`,
    `imag-part`, and `conjugate` through the CUDA unary helper for op ids
    `0..4`.
  - Added availability-gated regressions for CUDA unary plus/abs/negation/sqrt,
    Float32 unary map, direct Tensor unary/component primitives, destination
    realization from lazy unary CUDA map, and scientific CUDA fail-closed
    behavior.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and CUDA/Vulkan plan notes so arithmetic
    CUDA unary map is no longer listed as deferred.
- Commands run and key results:
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - CUDA backend probe: `[true true true nil]`.
  - Direct CUDA smokes for `map abs`, direct `sqrt`, Float32 `map sqrt`,
    `imag-part`, CUDA destination realization from lazy `map sqrt`, and
    Float64/Float32 scientific fail-closed behavior: all returned `true`.
  - Host focused `advanced-collections-module`: passed `pass=1256 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1243 fail=0`.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The earlier “CUDA map residual is unary/scientific” wording is now too
    broad. Arithmetic/component unary CUDA map is implemented for dense
    row-major `Float64`/`Float32`; the residual is scientific CUDA unary math
    plus unsupported layouts/devices/dtypes/callables.
  - Do not implement CUDA scientific op ids `5..16` by stitching together
    partial raw PTX approximations. Prefer a deliberate libdevice/NVRTC/fatbin
    strategy, or explicitly documented approximation kernels with tolerances.
- Current best recommendation or checkpoint:
  - Continue CUDA map work from scientific unary math only after choosing the
    CUDA math implementation strategy, or move to a separate residual lane such
    as arbitrary foreign CUDA payload clone semantics, fixed-width complex, or
    stride/view-backed Vulkan layouts.
- Unresolved issues:
  - Scientific CUDA unary map/direct math, unsupported CUDA layouts, mixed
    unsupported CUDA operands, arbitrary foreign CUDA payload clone semantics,
    fixed-width complex Tensor layout, stride/view-backed Vulkan dispatch, and
    broad parent-level validation remain explicit TODO items.
- Dependencies, blockers, or restart requirements:
  - `ptxas` is not installed in this environment, so standalone PTX assembly
    was not run; runtime CUDA execution did load and execute the embedded PTX.
  - No long-running process was left active. A rebuild/restart is required for
    any external process that was started before this code change.
- Signature: GPT-5 Codex

## 2026-04-18 02:05 CEST - CUDA Map Broadcast Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by broadening the
    landed CUDA binary map path from exact Tensor/Tensor operands to
    right-aligned singleton-axis Tensor/Tensor broadcasting without hidden CPU
    tensor-data materialization.
- Workspace/target:
  - `/home/christos/Omni`, embedded CUDA map PTX, CUDA helper ABI, Tensor CUDA
    map routing, focused advanced tests, TODO, changelog, plans, and Tensor
    docs.
- Code or configuration changes made:
  - Widened `omni_tensor_backend_cuda_map_f64` and
    `omni_tensor_backend_cuda_map_f32` to accept output and operand
    rank/shape/stride metadata from C3.
  - Updated the embedded PTX kernels to accept optional device offset maps for
    Tensor operands, so exact-shape operands keep direct indexing and
    broadcast operands index through shape-derived offset buffers.
  - Added host-side offset-map construction in the CUDA helper. Only
    shape-derived indices are staged; Tensor payload data remains on CUDA.
  - Changed `tensor_cuda_map_binary_resolved` from exact-shape validation to
    the same right-aligned `tensor_map_operand_matches_shape` rule used by
    CPU/Vulkan while preserving dense row-major, matching dtype/device, and no
    hidden CPU fallback requirements.
  - Added CUDA Float64/Float32 broadcast map regressions plus CUDA destination
    realization coverage and incompatible-broadcast fail-closed coverage.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and CUDA/Vulkan plan notes so
    right-aligned CUDA Tensor/Tensor map broadcasting is no longer listed as
    deferred.
- Commands run and key results:
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct CUDA-gated smokes for Float64 broadcast map, Float32 broadcast map,
    CUDA destination realization from a broadcast map, and incompatible
    broadcast rejection: all returned expected results.
  - Host focused `advanced-collections-module`: passed `pass=1240 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1227 fail=0`.
- Invalidated assumptions or failed approaches worth preserving:
  - The earlier “CUDA map is exact-shape only” boundary is no longer current.
    Dense row-major binary CUDA map supports scalar, exact-shape, and
    right-aligned singleton-axis Tensor/Tensor broadcast operands.
  - Do not treat the offset-map helper as arbitrary-layout or scientific
    CUDA map support. Unsupported layouts, mixed devices/dtypes, unsupported
    callables, scientific CUDA unary map, and arbitrary foreign CUDA payload
    clone semantics still fail closed.
- Current best recommendation or checkpoint:
  - Superseded by the 02:23 checkpoint above: continue the CUDA map lane from
    scientific CUDA unary map, or choose a separate remaining `TENSOR-100F`
    lane such as fixed-width complex or stride/view-backed Vulkan.
- Unresolved issues:
  - Scientific CUDA unary map, unsupported CUDA layouts, arbitrary foreign CUDA
    payload clone semantics, fixed-width complex Tensor layout,
    stride/view-backed Vulkan dispatch, and broad parent-level validation
    remain explicit TODO items.
- Dependencies, blockers, or restart requirements:
  - No long-running process was left active. A rebuild/restart is required for
    any external process that was started before this code change.
- Signature: GPT-5 Codex

## 2026-04-18 01:53 CEST - CUDA Elementwise Binary Map Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by replacing the
    stale CUDA `map` blocker with a real CUDA execution path while preserving
    explicit fail-closed boundaries and no hidden CPU fallback.
- Workspace/target:
  - `/home/christos/Omni`, CUDA helper ABI, Tensor CUDA routing, lazy map
    realization, CUDA destination realization, tests, TODO, changelog, plans,
    and Tensor docs.
- Code or configuration changes made:
  - Added runtime-loaded CUDA Driver API symbol resolution and embedded PTX map
    kernels in `csrc/tensor_cuda_helpers.c` for dense row-major `Float64` and
    `Float32` binary elementwise operations.
  - Exposed CUDA map availability and dtype-specific map helpers through
    `src/lisp/tensor_cuda_backend.c3`.
  - Routed public `map`, lazy map realization, and CUDA destination-form
    `realize` through the CUDA helper for Tensor/scalar, scalar/Tensor, and
    exact-shape Tensor/Tensor operands over `+`, `-`, `*`, `/`, `min`, and
    `max`.
  - Added `tensor-backends` `elementwise-map-float64` and
    `elementwise-map-float32` capability keys for CPU, Vulkan, and CUDA.
  - Fixed the CUDA concrete clone boundary for Omni-owned CUDA payloads by
    copying device storage through the existing CUDA device-to-device helper;
    arbitrary foreign CUDA payload clone remains rejected.
  - Added availability-gated regressions for direct CUDA maps, lazy CUDA map
    destination realization, capability keys, and unsupported callable,
    mixed-device, and broadcast fail-closed cases.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and CUDA/Vulkan plan notes so the
    landed CUDA map subset and residual work are explicit.
- Commands run and key results:
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct CUDA-gated smokes for capability reporting, direct `Float64` and
    `Float32` scalar maps, binary `-`/`*`/`/`/`min`/`max`/`+` Tensor maps,
    lazy map CUDA destination realization, unsupported callable rejection,
    mixed CPU operand rejection, and broadcast rejection: all returned the
    expected results.
  - Host focused `advanced-collections-module`: passed `pass=1237 fail=0`.
  - Bounded-container `memory-lifetime-smoke`: passed `pass=228 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1224 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `c3c build --sanitize=address --obj-out obj-asan`: not run to completion;
    this `c3c` build reports address sanitizer as unavailable on the current
    platform/toolchain.
- Invalidated assumptions or failed approaches worth preserving:
  - The previous “CUDA map blocker” is no longer authoritative for dense
    row-major binary `Float64`/`Float32` scalar and exact-shape operands; CUDA
    map now has a real embedded-PTX helper path.
  - Do not treat this as full CUDA map parity: right-aligned Tensor/Tensor
    broadcasting, scientific CUDA unary map, unsupported layouts, mixed
    CPU/CUDA operands, mixed CUDA dtypes/devices, unsupported callables, and
    arbitrary foreign CUDA payload clone semantics still fail closed.
- Current best recommendation or checkpoint:
  - Superseded by later checkpoints above: continue the CUDA map lane from one
    named residual family, now scientific CUDA unary map, using the
    embedded-PTX Driver API helper as the baseline.
- Unresolved issues:
  - CUDA map broadcasting/unary extension, arbitrary foreign CUDA payload clone
    semantics, fixed-width complex Tensor layout, stride/view-backed Vulkan
    dispatch, and broad parent-level validation remain explicit TODO items.
- Dependencies, blockers, or restart requirements:
  - No long-running process was left active. A rebuild/restart is required for
    any external process that was started before this code change.
- Signature: GPT-5 Codex

## 2026-04-18 01:13 CEST - CUDA Rank-1 Dot Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by landing the next
    coherent CUDA contract execution slice and sharpening the remaining CUDA
    `map` blocker.
- Workspace/target:
  - `/home/christos/Omni`, CUDA Tensor contract routing, destination
    realization tests, TODO, changelog, plans, and Tensor docs.
- Code or configuration changes made:
  - Extended `tensor_contract_try_cuda_value` so CUDA-placed contiguous matching
    `Float64` or matching `Float32` rank-1/rank-1 single-axis contractions
    return CUDA-placed scalar Tensor outputs.
  - Reused the existing cuBLAS GEMV helper by treating the left vector as a
    one-row row-major matrix, avoiding a new C helper ABI while still executing
    the nonzero dot through cuBLAS.
  - Preserved zero-size rank-1 dot through the CUDA additive-identity fill path.
  - Added availability-gated direct and destination-realize regressions for
    `Float64` and `Float32` rank-1 dot, including scalar `Float32` extraction
    and zero-size identity.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and CUDA/Vulkan plan notes so CUDA
    rank-1/rank-1 dot is no longer listed as future work.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct CUDA-gated smokes for `Float64` rank-1 dot, explicit-axis rank-1
    dot, `Float32` rank-1 dot with scalar `Float32` extraction, zero-size
    rank-1 dot identity, rank-0 CUDA destination realization for `Float64`
    rank-1 dot, and rank-0 CUDA destination realization for `Float32` rank-1
    dot: all returned `true`.
  - `./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed `pass=1217 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - CUDA rank-1/rank-1 dot does not require a new public primitive or helper
    ABI in this codebase; existing row-major GEMV helper semantics cover the
    nonzero dot while preserving CUDA placement.
  - Superseded by the later CUDA elementwise binary map checkpoint above: the
    helper-layer blocker no longer applies to dense row-major `Float64` and
    `Float32` binary scalar/exact-shape CUDA map.
- Current best recommendation or checkpoint:
  - For CUDA `map`, continue from the residual families named in the later
    CUDA elementwise binary map checkpoint above, or continue the fixed-width
    complex / stride-view Vulkan TODO lanes.
- Unresolved issues:
  - CUDA `map` broadcasting/unary residuals, fixed-width complex Tensor
    layout, stride/view-backed Vulkan dispatch, and broad parent-level
    validation remain explicit TODO items.
- Dependencies, blockers, or restart requirements:
  - No long-running process was left active. A rebuild/restart is required for
    any external process that was started before this code change.
- Signature: GPT-5 Codex

## 2026-04-18 01:00 CEST - CUDA Zero-Size Contract Identity

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by closing the
    explicit CUDA zero-size contract identity/fill TODO without changing the
    backend-neutral Tensor public surface or adding hidden CPU fallback.
- Workspace/target:
  - `/home/christos/Omni`, CUDA Tensor contract routing, focused advanced
    tests, TODO, changelog, plans, and Tensor docs.
- Code or configuration changes made:
  - Updated `tensor_contract_try_cuda_value` so supported dense row-major CUDA
    `Float64` and `Float32` rank-2/rank-2, rank-2/rank-1, and rank-1/rank-2
    single-axis contractions no longer reject zero free or zero contracted
    dimensions before allocation.
  - Zero free dimensions now return CUDA-placed zero-length Tensor outputs with
    the computed result shape. Zero contracted dimensions with non-empty output
    allocate CUDA result storage and fill additive identity through
    `omni_tensor_backend_cuda_fill_f64` / `omni_tensor_backend_cuda_fill_f32`,
    skipping cuBLAS.
  - Added availability-gated regressions for `Float64` and `Float32`
    zero-contracted rank-2 identity fill, zero-free rank-2 output preservation,
    rank-2/rank-1 identity fill, rank-1/rank-2 identity fill, and the
    cuBLAS-disabled zero-contracted path.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and relevant CUDA/Vulkan plan notes so
    CUDA zero-size contract identity/fill is no longer listed as deferred.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct CUDA-gated smokes for `Float64` zero-contracted rank-2, `Float32`
    zero-contracted rank-2 with scalar `Float32` extraction, zero-free rank-2
    output preservation, rank-2/rank-1 identity fill, and rank-1/rank-2
    identity fill: all returned `true`.
  - Host focused `advanced-collections-module` was attempted with
    `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module`;
    it stayed mostly idle past ten minutes and was terminated, so it is not a
    validation signal.
  - `./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed `pass=1210 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating zero-size CUDA contract dimensions as an unsupported
    cuBLAS problem. The zero-output and additive-identity cases are owned by
    CUDA allocation/fill semantics and must not call cuBLAS for correctness.
- Current best recommendation or checkpoint:
  - Continue `TENSOR-100F` from CUDA elementwise `map`, fixed-width complex
    Tensor semantics, stride/view-backed Vulkan coverage, or broad Docker-bound
    parent validation. CUDA rank-1/rank-1 dot is covered by the later
    checkpoint above.
- Unresolved issues:
  - CUDA `map`, fixed-width complex Tensor layout, and stride/view-backed
    Vulkan dispatch remain explicit TODO items. CUDA rank-1/rank-1 dot is
    covered by the later checkpoint above.
- Dependencies, blockers, or restart requirements:
  - No long-running process was left active. A rebuild/restart is required for
    any external process that was started before this code change.
- Signature: GPT-5 Codex
