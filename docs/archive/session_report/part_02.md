## 2026-04-18 02:56 CEST - CUDA Scientific Unary Map Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by landing CUDA
    scientific unary `map` and direct Tensor unary math without hidden CPU
    materialization or handwritten approximation PTX.
- Workspace/target:
  - `/home/christos/Omni`, CUDA helper PTX/module loading, C3 Tensor CUDA
    dispatch, `tensor-backends` capability reporting, focused advanced tests,
    TODO, changelog, plans, and Tensor docs.
- Code or configuration changes made:
  - Added `csrc/tensor_cuda_scientific_unary.cu` as the canonical CUDA C source
    for scientific unary kernels and embedded generated CUDA 13 PTX in
    `csrc/tensor_cuda_helpers.c` as a separate optional CUDA Driver API module.
  - Added C ABI helpers and C3 externs for `scientific-map-float64` /
    `scientific-map-float32` support and routed CUDA unary op ids `5..16`
    (`sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`,
    `exp`, `log`, `log10`) through the generated-PTX helper.
  - Updated public `map`, lazy CUDA map realization, CUDA destination
    realization, and direct Tensor scientific primitives to preserve CUDA
    placement for eligible dense row-major real CUDA tensors.
  - Added availability-gated tests for CUDA scientific Float64/Float32 family
    mapping, direct `sin`, direct Float32 `log10`, and destination realization.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/cuda-cublas-backend-decision-2026-04-16.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
- Commands run and key results:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_scientific_unary.cu -o /tmp/omni_tensor_cuda_scientific_unary.ptx`: passed.
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/omni_tensor_cuda_scientific_unary.ptx -o /tmp/omni_tensor_cuda_scientific_unary.cubin`: passed.
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - CUDA runtime smokes for capability reporting, Float64 `map sin`, Float32
    direct `log10`, direct `sin`, Float64 `acos`/`log10`, and lazy destination
    `realize`: returned true.
  - Host focused `advanced-collections-module`: passed `pass=1258 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1245 fail=0` using `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The prior scientific CUDA residual is now closed for dense row-major
    `Float64`/`Float32` op ids `5..16`; do not leave docs/TODO phrased as if
    scientific CUDA unary remains unimplemented.
  - Do not use the validation Docker image's baked-in `/opt/c3/c3c` on this
    arm64 host yet: rebuilding `omni-validation:2026-03-10` still downloads an
    x86-64 `c3-linux.tar.gz` compiler into an arm64 image. Use
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local` until the Dockerfile
    selects/builds native C3.
- Current best recommendation or checkpoint:
  - Superseded by the 03:07 foreign CUDA payload clone checkpoint above. The
    remaining CUDA map residuals should be split by explicit layout/view
    metadata, mixed-device policy, mixed-dtype policy, or callable extension.
- Unresolved issues:
  - Unsupported CUDA layouts/views, mixed CPU/CUDA operands, mixed CUDA
    dtype/device operands, unsupported CUDA callables, fixed-width complex Tensor
    layout, stride/view-backed Vulkan dispatch, broad parent-level validation,
    and the validation-image C3 architecture bug remain TODO items.
- Dependencies, blockers, or restart requirements:
  - Existing long-running Omni processes must be rebuilt/restarted before they
    see the new helper symbols and embedded PTX.
  - CUDA scientific support depends on driver support for the generated PTX
    target used here (`compute_75`) and remains capability-gated at runtime.
- Signature: GPT-5 Codex

## 2026-04-18 02:23 CEST - CUDA Arithmetic Unary Map Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by landing CUDA
    arithmetic/component unary map support without hidden CPU fallback.
- Workspace/target:
  - `/home/christos/Omni`, embedded CUDA map PTX, CUDA helper ABI, Tensor CUDA
    routing, direct Tensor unary primitives, focused tests, TODO, changelog,
    plans, and Tensor docs.
- Code or configuration changes made:
  - Added separate embedded PTX unary kernels and C ABI helpers for CUDA
    `Float64` and `Float32` unary map op ids `0..4`: `abs`, unary `-`,
    `sqrt`, identity, and zero-fill.
  - Routed public direct `map`, lazy map realization, CUDA destination
    realization, and direct Tensor `abs`, unary `-`, `sqrt`, `real-part`,
    `imag-part`, and `conjugate` through the new CUDA unary helper for eligible
    dense row-major real CUDA tensors.
  - Preserved scientific CUDA unary op ids `5..16` as explicit
    `tensor/backend-unsupported` cases until a deliberate libdevice/NVRTC/
    fatbin or documented approximation strategy exists.
  - Added CUDA-gated tests for unary arithmetic/component success, destination
    realization, and scientific fail-closed behavior.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and CUDA/Vulkan plan notes.
- Commands run and key results:
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - CUDA backend probe returned `[true true true nil]`.
  - Direct CUDA smokes for unary `abs`, direct `sqrt`, Float32 `sqrt`,
    `imag-part`, destination realization from lazy `map sqrt`, and
    Float64/Float32 scientific fail-closed behavior all returned `true`.
  - Host focused `advanced-collections-module`: passed `pass=1256 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1243 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The remaining CUDA map residual is no longer “unary/scientific” as a
    single bucket. Arithmetic/component unary map is landed; scientific unary
    CUDA math remains unresolved.
  - Do not wire scientific CUDA ops through partial raw PTX approximations
    without an explicit accuracy and implementation decision.
- Current best recommendation or checkpoint:
  - Continue CUDA map work from scientific unary math only after choosing the
    CUDA math implementation strategy, or switch to another residual lane such
    as foreign CUDA payload clone semantics, fixed-width complex, or
    stride/view-backed Vulkan layouts.
- Unresolved issues:
  - Scientific CUDA unary map/direct math, unsupported CUDA layouts, arbitrary
    foreign CUDA payload clone semantics, fixed-width complex Tensor layout,
    stride/view-backed Vulkan dispatch, and broad parent-level validation
    remain explicit TODO items.
- Dependencies, blockers, or restart requirements:
  - `ptxas` was unavailable, so no standalone PTX assembly check was run beyond
    runtime CUDA execution. No long-running process was left active. A rebuild
    or process restart is required for pre-existing external processes to see
    the new helper code.
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
