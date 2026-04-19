# Session Report Index Part 06

Source: `.agents/SESSION_REPORT.md`

## 2026-04-18 04:25 CEST - Tensor stats/normal-cdf CUDA Scientific Opcode

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by extending
    `stats/normal-cdf` from a scalar-only standard-normal helper to CPU Tensor
    elementwise support and a CUDA fixed scientific unary opcode.
- Workspace/target:
  - `/home/christos/Omni`, Tensor unary math dispatch, CUDA generated PTX helper,
    CUDA Driver API resolver, advanced collection tests, Tensor docs, TODO,
    changelog, and active plan artifacts.
- Code or configuration changes made:
  - `prim_math_core.c3` now routes Tensor operands for `stats/normal-cdf`
    through the shared Tensor unary math path.
  - `prim_tensor.c3` now evaluates CPU Tensor `stats/normal-cdf` elementwise:
    `Float64` and `Float32` preserve float dtype, `BigInteger` returns
    `Float64`, `BigFloat` preserves dtype, lazy CPU Tensor sources realize, and
    `BigComplex` fails closed because no complex distribution Tensor contract is
    shipped.
  - CUDA callable lowering recognizes `stats/normal-cdf` as CUDA-only fixed op
    id `19`, without adding it to the shared Vulkan unary callable table.
  - `csrc/tensor_cuda_scientific_unary.cu` and the embedded generated PTX in
    `csrc/tensor_cuda_helpers.c` now cover scientific op ids `5..19`; the CUDA
    scientific helper wrapper max-op range was widened to `19`.
  - Restored `omni_tensor_cuda_driver_resolve` in `csrc/tensor_cuda_helpers.c`
    so direct helper-object linking resolves CUDA Driver API calls used by
    embedded PTX modules.
  - Direct CUDA `map` now rejects unsupported CUDA callables and mixed CPU/CUDA
    operands before returning lazy Tensor expressions, preserving the existing
    payload diagnostics and no-hidden-materialization contract.
  - Added tests for CPU Tensor `stats/normal-cdf`, CUDA `map stats/normal-cdf`,
    direct Tensor CUDA `stats/normal-cdf`, Float32 dtype preservation, lazy CPU
    source realization, CUDA destination realization, Vulkan fail-closed
    behavior, and direct-map CUDA diagnostic eagerness.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/reference/11-appendix-primitives.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/README.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `docs/plans/cuda-cublas-backend-decision-2026-04-16.md`.
- Commands run and key results:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_scientific_unary.cu -o /tmp/omni_tensor_cuda_scientific_unary.ptx`: passed.
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/omni_tensor_cuda_scientific_unary.ptx -o /tmp/omni_tensor_cuda_scientific_unary.cubin`: passed.
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct smokes: CPU Tensor `stats/normal-cdf` returned `0.5`; CPU Float32
    Tensor preserved dtype; CUDA `map stats/normal-cdf` returned `0.5`; direct
    CUDA Float32 `stats/normal-cdf` preserved `"Float32"`; CUDA destination
    `realize` produced `(cuda 0.5)`; Vulkan `map stats/normal-cdf` returned
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: passed `pass=1282 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1269 fail=0` using `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat `stats/normal-cdf` support as arbitrary CUDA callable support;
    it is a fixed opcode extension.
  - Do not add `stats/normal-quantile` as a fixed CUDA opcode until the Tensor
    probability-domain and GPU error/status contract exists.
  - Do not infer Vulkan `stats/normal-cdf` support from CUDA opcode `19`; Vulkan
    remains fail-closed for this callable.
- Current best recommendation or checkpoint:
  - Continue CUDA/Vulkan broadening from explicit residuals: unsupported
    layouts/views, a defined mixed-device execution policy, Vulkan
    distribution-function policy, dtype-changing GPU rounding, or
    `stats/normal-quantile` after its domain/error contract is designed.
- Unresolved issues:
  - Unsupported CUDA layouts/views, arbitrary unsupported CUDA callables,
    dtype-changing GPU rounding, Tensor `stats/normal-quantile`, Vulkan
    `stats/normal-cdf`, fixed-width complex Tensor layout, broad parent-level
    validation, and the validation-image C3 architecture bug remain TODO items.
- Dependencies, blockers, or restart requirements:
  - Existing long-running Omni processes must be rebuilt/restarted before they
    see the new Tensor math and embedded PTX.
- Signature: GPT-5 Codex

## 2026-04-18 03:35 CEST - Tensor math/erf and math/erfc CUDA Scientific Opcodes

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by extending Tensor
    `math/erf` and `math/erfc` from scalar-only primitives to CPU Tensor
    elementwise support and CUDA fixed scientific unary opcodes.
- Workspace/target:
  - `/home/christos/Omni`, Tensor unary math dispatch, CUDA generated PTX helper,
    CUDA helper runtime resolver, advanced collection tests, Tensor docs, TODO,
    changelog, and active plan artifacts.
- Code or configuration changes made:
  - `prim_math_core.c3` now routes Tensor operands for `math/erf` and
    `math/erfc` through the shared Tensor unary math path.
  - `prim_tensor.c3` now evaluates CPU Tensor `math/erf` / `math/erfc`
    elementwise; `Float64` and `Float32` preserve float dtype, `BigInteger`
    returns `Float64`, `BigFloat` preserves dtype, and `BigComplex` fails closed
    because no complex error-function Tensor contract is shipped.
  - CUDA callable lowering recognizes `math/erf` and `math/erfc` as CUDA-only
    fixed op ids `17` and `18`, without adding them to the shared Vulkan unary
    callable table.
  - `csrc/tensor_cuda_scientific_unary.cu` and the embedded generated PTX in
    `csrc/tensor_cuda_helpers.c` now cover op ids `5..18`; the CUDA scientific
    helper wrapper max-op range was widened to `18`.
  - Restored `omni_tensor_cuda_resolve` in `csrc/tensor_cuda_helpers.c` so direct
    helper-object linking resolves CUDA runtime API calls instead of relying on a
    stale archive.
  - Added tests for CPU Tensor `math/erf` / `math/erfc`, CUDA `map math/erf`,
    CUDA `map math/erfc`, direct Tensor CUDA `math/erf` / `math/erfc`, Float32
    dtype preservation, CUDA destination realization, and Vulkan fail-closed
    behavior for the new callables.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/reference/11-appendix-primitives.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/README.md`, `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
    and `docs/plans/cuda-cublas-backend-decision-2026-04-16.md`.
- Commands run and key results:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_scientific_unary.cu -o /tmp/omni_tensor_cuda_scientific_unary.ptx`: passed.
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/omni_tensor_cuda_scientific_unary.ptx -o /tmp/omni_tensor_cuda_scientific_unary.cubin`: passed.
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct smokes: CPU Tensor `math/erf` returned `0.842700792949715`; CUDA
    `map math/erf` returned `0.842700792949715`; direct CUDA Float32
    `math/erfc` preserved `"Float32"`; Vulkan `map math/erf` returned
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: passed `pass=1271 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1258 fail=0` using `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not add `floor`, `ceiling`, `round`, or `truncate` to CUDA unary map as
    same-dtype opcodes. Tensor rounding returns `Tensor BigInteger`, so a GPU
    implementation needs an explicit dtype-changing result path.
  - Do not treat `math/erf` / `math/erfc` support as arbitrary CUDA callable
    support; it is a fixed opcode extension.
- Current best recommendation or checkpoint:
  - Continue CUDA map broadening from real residuals: unsupported layouts/views,
    explicit mixed-device execution if desired, fixed-width complex Tensor dtype,
    or another fixed built-in same-dtype callable family with CPU Tensor semantics
    first.
- Unresolved issues:
  - Unsupported CUDA layouts/views, arbitrary unsupported CUDA callables,
    dtype-changing GPU rounding, fixed-width complex Tensor layout,
    stride/view-backed Vulkan dispatch, broad parent-level validation, and the
    validation-image C3 architecture bug remain TODO items.
- Dependencies, blockers, or restart requirements:
  - Existing long-running Omni processes must be rebuilt/restarted before they
    see the new Tensor math and embedded PTX.
- Signature: GPT-5 Codex

## 2026-04-18 03:19 CEST - CUDA Map Mixed Operand Diagnostics

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by hardening CUDA `map`
    mixed-operand diagnostics and no-hidden-materialization behavior.
- Workspace/target:
  - `/home/christos/Omni`, CUDA/Vulkan Tensor map preflight, advanced collection
    tests, TODO, changelog, and active operational plan.
- Code or configuration changes made:
  - Added recursive non-materializing Tensor expression device probes for CUDA map
    preflight.
  - CUDA `map` now raises the explicit `map: CUDA operands must remain
    CUDA-placed` diagnostic for mixed CPU/CUDA operands, including a nested CPU
    lazy operand that would raise if materialized.
  - CUDA `map` now raises `tensor/dtype-mismatch` for mixed CUDA Tensor dtypes
    from the CUDA helper path instead of a generic backend unsupported message.
  - Vulkan and CUDA map probes now check whether an expression tree touches the
    probed device before resolving concrete storage, preventing the Vulkan probe
    from materializing CPU lazy operands in a CUDA-only failure path.
  - Added focused regressions for mixed CPU/CUDA placement diagnostics,
    fail-before-CPU-lazy-materialization, and mixed CUDA dtype diagnostics.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Host focused `advanced-collections-module`: passed `pass=1261 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1248 fail=0` using `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat symbol-only mixed-device rejection as sufficient for lazy Tensor
    expressions. Backend probes must reject unsupported mixed-device expression
    trees before they call concrete-storage realization helpers.
- Current best recommendation or checkpoint:
  - Remaining CUDA map work is now actual semantic broadening: unsupported
    layouts/views, explicit mixed-device execution support if desired, or callable
    extension beyond the fixed op-id table.
- Unresolved issues:
  - Unsupported CUDA layouts/views, unsupported CUDA callables, fixed-width complex
    Tensor layout, stride/view-backed Vulkan dispatch, broad parent-level
    validation, and the validation-image C3 architecture bug remain TODO items.
- Dependencies, blockers, or restart requirements:
  - Existing long-running Omni processes must be rebuilt/restarted before they
    see the new map preflight diagnostics.
- Signature: GPT-5 Codex

## 2026-04-18 03:07 CEST - Foreign CUDA Payload Clone Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by closing the foreign
    CUDA concrete Tensor clone/copy ownership contract.
- Workspace/target:
  - `/home/christos/Omni`, Tensor payload cloning, memory-lifetime regressions,
    CUDA ownership docs, TODO, changelog, and operational plan artifacts.
- Code or configuration changes made:
  - `tensor_clone_payload` now accepts valid CUDA concrete payload metadata from
    foreign finalizer sources, allocates a fresh Omni-owned CUDA buffer, copies
    source bytes device-to-device, and installs `tensor_cuda_device_finalizer` on
    the clone.
  - Fake, stale, malformed, or otherwise invalid CUDA handles remain fail-closed;
    the fake CUDA handle regression still rejects clone and verifies source
    finalizer destruction.
  - Added a real CUDA memory-lifetime regression for a foreign-finalizer source
    handle: copied values round-trip, clone handle is distinct, clone finalizer is
    Omni CUDA, and the source foreign finalizer fires once.
  - During review, corrected the Vulkan clone retain branch to keep
    `tensor_vulkan_device_finalizer` rather than accidentally installing a CUDA
    finalizer.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`,
    `docs/plans/cuda-cublas-backend-decision-2026-04-16.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh bash -lc 'c3c build --obj-out obj && env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`: passed `pass=229 fail=0`.
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`: passed `pass=1258 fail=0`.
  - A host-side `memory-lifetime-smoke` attempt was rejected by the repo policy
    guard before execution; the valid run is the bounded-container run above.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating valid foreign CUDA concrete payload clone as
    unimplemented or rejected. The supported ownership rule is now deep-copy into
    fresh Omni-owned CUDA storage, with the normal CUDA finalizer on the clone.
  - Do not infer that arbitrary pointer-shaped data is valid CUDA storage; invalid
    handles still fail closed and must not be copied.
- Current best recommendation or checkpoint:
  - Continue CUDA map broadening from a remaining explicit residual: unsupported
    layouts/views, mixed CPU/CUDA policy, mixed CUDA dtype/device policy, or
    callable extension beyond the fixed op-id table.
- Unresolved issues:
  - Unsupported CUDA layouts/views, mixed CPU/CUDA operands, mixed CUDA
    dtype/device operands, unsupported CUDA callables, fixed-width complex Tensor
    layout, stride/view-backed Vulkan dispatch, broad parent-level validation,
    and the validation-image C3 architecture bug remain TODO items.
- Dependencies, blockers, or restart requirements:
  - Existing long-running Omni processes must be rebuilt/restarted before they
    see the corrected clone/finalizer code.
- Signature: GPT-5 Codex

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
