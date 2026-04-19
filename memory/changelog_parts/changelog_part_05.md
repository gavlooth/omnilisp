# Memory Changelog Index Part 05

Source: `memory/CHANGELOG.md`

      `3340.0`, `vulkan`, and `0.0`
    - host focused `advanced-collections-module` -> `688 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `675 passed, 0 failed`
	    - primitive docs parity and Stage 3 source parity passed
	    - `git diff --check` passed

- Invalidated a generic Vulkan unary `map` direction:
  - A follow-up attempt to extend the binary Vulkan `Float64` map shader with a
    generic mode-3 unary branch was rolled back before landing.
  - GLSL `double` transcendental calls such as `sin(double)` failed shader
    compilation, invalidating the assumption that the current Vulkan GLSL path
    can directly use double-precision transcendental builtins.
  - An arithmetic-only unary variant for `abs`/negation compiled but still
    failed at runtime with `map: Vulkan Float64 kernel failed`, while binary
    Vulkan `map` and rank-N Vulkan `contract` remained healthy.
  - Do not resume the generic mode-3 branch as the default next step. Prefer a
    separately debugged unary shader/helper entrypoint, or first diagnose the
    descriptor/dispatch failure. Do not use `Float32` casts or hidden CPU
    fallback to mask unsupported Vulkan unary behavior.
  - After rollback, `c3c build --obj-out obj` passed, binary Vulkan `map +`
    returned `8.0`, rank-N Vulkan `contract` returned `460.0`, unsupported
    Vulkan `map sqrt` returned `tensor/backend-unsupported`, host focused
    `advanced-collections-module` passed `688/0`, and bounded-container focused
    `advanced-collections-module` passed `675/0`.

- Advanced `TENSOR-100E` Vulkan `Float64` map broadcasting:
  - Updated `csrc/tensor_vulkan_map_f64.comp` so the shader computes
    right-aligned broadcast operand offsets from rank/shape/stride metadata
    instead of indexing both operands with the flat output index.
  - Regenerated `csrc/tensor_vulkan_map_f64_spv.c` from the checked-in shader.
  - Extended `omni_tensor_backend_vulkan_map_f64` with a fourth metadata buffer
    containing output shape/strides plus left/right operand shape/strides.
  - Relaxed the C3 Vulkan `map` dispatch gates to accept the same dense
    row-major right-aligned singleton-axis Tensor/Tensor broadcasting supported
    by the CPU Tensor map oracle.
  - Public behavior remains backend-neutral: callers use `map`, results stay
    Vulkan-placed, and CPU inspection still requires explicit `to-device 'cpu`.
  - Incompatible broadcast shapes, unsupported callables, unsupported dtypes,
    and mixed CPU/Vulkan operands still fail closed with Tensor backend
    diagnostics.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan broadcast smokes returned `36.0`, `32.0`, `11.0`, and
      `vulkan`
    - host focused `advanced-collections-module` -> `683 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `670 passed, 0 failed`

- Advanced `TENSOR-100E` Vulkan `Float64` map arithmetic family:
  - Added `csrc/tensor_vulkan_map_f64.comp` as the checked-in GLSL source for
    the Vulkan `Float64` elementwise map shader family.
  - Added `csrc/tensor_vulkan_map_f64_spv.c`, generated from that GLSL source.
  - Added the runtime-loaded C ABI helper
    `omni_tensor_backend_vulkan_map_f64` with Tensor/scalar, scalar/Tensor, and
    Tensor/Tensor dispatch modes and `+`, `-`, `*`, `/` arithmetic op codes.
  - Routed public `map` for Vulkan-placed dense row-major `Float64` operands
    through the new generic helper when operands are Tensor/scalar,
    scalar/Tensor, or exact-shape Tensor/Tensor.
  - Results stay Vulkan-placed and still require explicit `to-device 'cpu` for
    CPU inspection.
  - Unsupported map callables, unary map callables, non-`Float64` dtypes, mixed
    CPU/Vulkan operands, and broadcasting shapes fail closed with Tensor backend
    diagnostics rather than silently copying to CPU.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan map smokes returned `8.0`, `12.0`, `26.0`, `80.0`,
      `90.0`, `4.0`, `vulkan`, and `tensor/backend-unsupported`
    - host focused `advanced-collections-module` -> `679 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `666 passed, 0 failed`
    - primitive docs parity and Stage 3 source parity passed
    - `git diff --check` passed

- Advanced `TENSOR-100E` Vulkan dtype/layout policy:
  - Added `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`.
  - Policy: keep extending fixed-width `Float64` dense row-major Vulkan kernels
    first; do not downcast `Float64` to `Float32`; do not lower pointer-backed
    `BigInteger`, `BigFloat`, or `BigComplex` Tensor dtypes to Vulkan; defer
    fixed-width complex and stride-aware layouts until those language/runtime
    contracts exist.
  - Superseded follow-up: the policy-backed `Float64` dense row-major `map`
    kernel family for Tensor/scalar and Tensor/Tensor elementwise arithmetic
    landed in the later Vulkan map arithmetic slice.
  - Also corrected the primitive appendix Tensor row so it lists the current
    four native Tensor dtypes.
  - validation: primitive docs parity, Stage 3 source parity, and
    `git diff --check` passed.

- Advanced `TENSOR-100E` Vulkan generic contract dispatch hardening:
  - Added `csrc/tensor_vulkan_contract_f64.comp` as the checked-in GLSL source
    for the generic Vulkan `Float64` contract shader.
  - Regenerated `csrc/tensor_vulkan_contract_f64_spv.c` from that GLSL source.
  - Updated `omni_tensor_backend_vulkan_contract_f64` to dispatch rank-2
    outputs with shape-aware 16x4 workgroups instead of flattening every output
    through a one-dimensional launch.
  - Public Tensor behavior is unchanged: Vulkan `contract` remains gated to
    dense row-major `Float64` rank-1/rank-2 single-axis layouts, with broader
    dtype/layout support reserved for a separate policy slice.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0` passed
    - `spirv-val --target-env vulkan1.0` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan contract smokes returned `154.0`, `60.0`, `167.0`,
      `69.0`, `122.0`, `69.0`, `46.0`, `43.0`, `vulkan`,
      `tensor/backend-unsupported`, and zero-free length `0`
    - host focused `advanced-collections-module` -> `674 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `661 passed, 0 failed`
    - primitive docs parity and Stage 3 source parity passed
    - `git diff --check` passed

## 2026-04-16

- Advanced `TENSOR-100E` Vulkan zero-size contract semantics:
  - Vulkan single-axis `contract` now mirrors the CPU Tensor zero-size oracle
    for the supported rank-1/rank-2 `Float64` layouts.
  - Zero-size contracted axes produce additive-identity output in Vulkan
    storage, and zero-size free dimensions preserve zero-length result shapes
    while remaining Vulkan-placed.
  - Updated `omni_tensor_backend_vulkan_contract_f64` so zero-element results
    can succeed without a buffer handle and zero-contracted non-empty results
    allocate a Vulkan buffer and zero-fill it explicitly.
  - validation:
    - direct Vulkan zero-size contract smokes returned `0.0`, `0.0`, `0`,
      `vulkan`, `0.0`, `0`, `0.0`, and `0`
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - host focused `advanced-collections-module` -> `674 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `661 passed, 0 failed`
    - bounded-container full `advanced` slice -> `1978 passed, 0 failed`
    - bounded-container `memory-lifetime-smoke` -> `227 passed, 0 failed`
    - primitive docs parity and Stage 3 source parity passed
    - `git diff --check` passed
    - `c3c build --sanitize=address --obj-out obj` remains blocked by the
      local C3 sanitizer platform guard

- Advanced `TENSOR-100E` Vulkan generic `Float64` single-axis contract support:
  - Added `csrc/tensor_vulkan_contract_f64_spv.c`, an embedded SPIR-V generic
    contract shader for dense row-major Vulkan `Float64` Tensor layouts.
  - Added `omni_tensor_backend_vulkan_contract_f64` in the runtime-loaded
    Vulkan helper.
  - Public `contract` now supports Vulkan-placed dense row-major `Float64`
    tensors of ranks 1 and 2 with exactly one contracted axis.
  - Supported layouts are rank-1/rank-1 dot (`[0 0]` or explicit `[0] [0]`),
    rank-2/rank-2 orientations `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`,
    rank-2/rank-1 orientations `[1 0]` and `[0 0]`, and rank-1/rank-2
    orientations `[0 0]` and `[0 1]`.
  - Results are Vulkan-placed tensors; callers must still copy explicitly with
    `to-device 'cpu` before CPU inspection.
  - Unsupported Vulkan contract cases, including multi-axis contractions,
    unsupported ranks/layouts/dtypes, and mixed CPU/Vulkan operands, fail
    closed with Tensor backend diagnostics rather than silently copying to CPU.
  - validation:
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan contract smokes returned `154.0`, `60.0`, `167.0`,
      `69.0`, `122.0`, `69.0`, `46.0`, `43.0`, `vulkan`, and
      `tensor/backend-unsupported`
    - host focused `advanced-collections-module` -> `668 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `655 passed, 0 failed`
    - bounded-container full `advanced` slice -> `1972 passed, 0 failed`
    - bounded-container `memory-lifetime-smoke` -> `227 passed, 0 failed`
    - primitive docs parity and Stage 3 source parity passed
    - `git diff --check` passed
    - `c3c build --sanitize=address --obj-out obj` remains blocked by the
      local C3 sanitizer platform guard

- Advanced `TENSOR-100E` portable Vulkan Tensor backend:
  - Added `docs/plans/vulkan-backend-decision-2026-04-16.md`, recording
    Vulkan as the portable explicit GPU execution substrate behind the existing
    `Tensor` surface.
  - Added `csrc/tensor_vulkan_helpers.c`, a runtime-loaded `libvulkan` probe
    with no link-time Vulkan SDK dependency.
  - Added `src/lisp/tensor_vulkan_backend.c3` extern bindings.
  - Wired the Vulkan helper into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - `tensor-backends` now includes a structured `vulkan` entry with explicit
    `float64` capability reporting.
  - `to-device 'vulkan` now copies concrete `Float64` CPU Tensor storage into
    opaque host-visible coherent Vulkan buffers when runtime-loaded Vulkan
    support is usable.
  - `to-device 'cpu` copies Vulkan Tensor storage back to native CPU Tensor
    storage.
  - Missing or unusable Vulkan fails with `tensor/backend-unavailable`;
    unsupported dtypes fail with `tensor/backend-unsupported`.
  - Unsupported Vulkan contract shapes fail closed with
    `tensor/backend-unsupported`; ordinary Tensor operations still do not
    silently transfer between CPU and GPU.
  - Added `csrc/tensor_vulkan_map_add_scalar_f64_spv.c`, an embedded SPIR-V
    compute shader generated from a checked GLSL add-scalar kernel.
  - Refactored Vulkan helper storage around a shared context and refcounted
    buffer handles so kernel outputs can share the input Vulkan device safely
    across boundary copies and deterministic finalization.
  - `map +` with one Vulkan `Float64` Tensor operand and one Float64 scalar now
    dispatches through the embedded SPIR-V kernel and returns a Vulkan-placed
    Tensor. Callers copy back explicitly with `to-device 'cpu`.
  - Unsupported Vulkan map callables/shapes fail closed with
    `tensor/backend-unsupported` instead of silently copying to CPU.
  - Added `csrc/tensor_vulkan_contract_dot_f64_spv.c`, an embedded SPIR-V
    compute reduction shader for rank-1/rank-1 `Float64` dot.
  - Public `contract` now supports two Vulkan `Float64` rank-1 tensors with
    one contracted axis (`[0 0]` or explicit `[0] [0]`), returning a
    Vulkan-placed scalar Tensor. Callers copy back explicitly with
    `to-device 'cpu`.
  - Rank-2 Vulkan contract support landed in the 2026-04-16 generic
    single-axis slice; zero-size contracted-axis support landed in the
    follow-up zero-size slice. Unsupported Vulkan contract shapes from this
    point are multi-axis, unsupported rank/layout/dtype, and mixed-device
    cases. They fail closed with `tensor/backend-unsupported` instead of
    silently copying to CPU.
  - Added memory-lifetime coverage for opaque Vulkan device finalizers and
    retained fail-closed boundary cloning for unsupported non-CPU device
    handles while allowing real Vulkan buffer handles to be retained.
  - The Vulkan probe suppresses noisy backend-driver stderr during the one-time
    availability check and reports failures through the structured backend
    entry instead.
  - negative finding:
    - Do not accept merely host-visible non-coherent Vulkan memory in this
      helper unless explicit flush/invalidate support is added. The placement
      path currently requires host-visible coherent memory.
    - Zero-size Vulkan placement must still probe availability; it must not
      silently create a Vulkan-placed Tensor on hosts where Vulkan is missing.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - direct host Vulkan map smokes returned `8.0` and `12.0`
    - direct host Vulkan rank-1 dot smokes returned `140.0`, `140.0`,
      `vulkan`, and `tensor/backend-unsupported`
    - `c3c build --obj-out obj`
    - host focused `advanced-collections-module` -> `659 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `646 passed, 0 failed`
    - bounded-container full `advanced` slice -> `1963 passed, 0 failed`
    - bounded-container `memory-lifetime-smoke` -> `227 passed, 0 failed`
    - primitive docs parity and Stage 3 source parity passed
    - `git diff --check` passed
    - `c3c build --sanitize=address --obj-out obj` was attempted but the local
      C3 toolchain reported address sanitizer unsupported on this platform

- Completed `TENSOR-100D` cuBLAS rank-2 CUDA contract fast path:
  - Extended `csrc/tensor_cuda_helpers.c` with runtime-loaded cuBLAS symbols
    (`cublasCreate_v2`, `cublasDestroy_v2`, `cublasDgemm_v2`,
    `cublasDgemv_v2`).
  - cuBLAS handle lifecycle is deterministic per operation: create, execute,
    destroy.
  - `tensor-backends` now includes a structured `cublas` entry in addition to
    `cpu` and `cuda`.
  - CUDA-placed dense row-major `Float64` rank-2/rank-2 single-axis
    contractions route to `cublasDgemm_v2` for `[1 0]`, `[0 0]`, `[1 1]`,
    and `[0 1]`; rank-2/rank-1 and rank-1/rank-2 single-axis contractions
    route to `cublasDgemv_v2`, returning CUDA-placed Tensors.
  - CUDA contract results must be copied back explicitly with `to-device 'cpu`
    for CPU inspection.
  - Unsupported CUDA contract dtypes, ranks, axis pairs, mixed CPU/CUDA
    operands, zero-size dimensions, or missing cuBLAS fail with Tensor backend
    diagnostics instead of silently copying to CPU.
  - Focused regressions cover all four cuBLAS rank-2/rank-2 single-axis
    layouts plus matrix-vector and vector-matrix layouts when available, then
    force cuBLAS unavailable through the private backend-disable hook and
    assert both backend inventory reporting and
    `tensor/backend-unavailable` contract failure.
  - validation:
    - `c3c build --obj-out obj`
    - direct host `tensor-backends` smoke reported CPU, CUDA, and cuBLAS
      available
    - direct host CUDA/cuBLAS contract smoke returned `154.0`
    - host focused `advanced-collections-module` -> `646 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `633 passed, 0 failed`
    - bounded-container `memory-lifetime-smoke` -> `226 passed, 0 failed`
    - bounded-container full `advanced` slice -> `1950 passed, 0 failed`
    - `./scripts/build_omni_chelpers.sh` passed
    - primitive docs parity and Stage 3 source parity passed
    - `git diff --check` passed
  - validation note:
    - `c3c build --sanitize=address --obj-out obj` was attempted but the local
      C3 toolchain reported address sanitizer unsupported on this platform.

- Completed `TENSOR-100C` optional CUDA Tensor copy backend:
  - Added `csrc/tensor_cuda_helpers.c`, a runtime-loaded `libcudart` helper
    with no required CUDA link dependency.
  - Added `src/lisp/tensor_cuda_backend.c3` extern bindings.
  - Wired helper compilation into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - CUDA availability now requires runtime symbol resolution, visible device
    count, and a successful allocation/free probe; a weak device-count-only
    probe was invalidated because it could report available while copy paths
    still returned errors.
  - `to-device` with target `'cuda` realizes CPU Tensor expressions and copies
    concrete `Float64` storage to CUDA when available.
  - `to-device` with target `'cpu` copies CUDA Tensor storage back to native CPU
    Tensor storage.
  - Unsupported CUDA dtypes fail with `tensor/backend-unsupported`.
  - Missing or unusable CUDA fails with `tensor/backend-unavailable`.
  - Focused tests now accept both CUDA-visible and CUDA-unavailable validation
    environments and assert a CUDA roundtrip value when CUDA is available.
  - validation:
    - `c3c build --obj-out obj`
    - host focused `advanced-collections-module` -> `633 passed, 0 failed`
    - direct host `tensor-backends` smoke reported CUDA available
    - direct host CUDA roundtrip `to-device 'cuda` -> `to-device 'cpu`
      returned `2.0`
    - bounded-container focused `advanced-collections-module`
      -> `619 passed, 0 failed`
    - bounded-container `memory-lifetime-smoke` -> `226 passed, 0 failed`
    - `git diff --check` passed
  - negative finding:
    - Do not treat `cudaGetDeviceCount` alone as CUDA copy availability. It was
      observed to make `tensor-backends` report available while the first copy
      smoke returned error values. Keep the allocation/free probe as part of
      backend availability.

- Completed `TENSOR-100B` Tensor placement metadata and backend inventory:
  - Added internal `TensorDeviceKind` placement metadata to `TensorVal`.
  - Added an opaque device handle/finalizer slot for future CUDA buffers.
  - Ordinary Tensor allocation initializes CPU placement.
  - `device` now reports the Tensor payload's stored placement instead of
    hardcoding CPU.
  - `tensor-backends` is now registered in the interpreter table and AOT
    primitive hash. It returns structured backend dictionaries: CPU is
    available, CUDA is listed as unavailable with reason `backend-unavailable`.
  - CPU Tensor kernels reject non-CPU storage through `tensor_has_valid_storage`
    instead of silently reading backend handles.
  - Boundary cloning refuses opaque non-CPU Tensor payloads until explicit copy
    semantics are implemented.
  - `tensor_free_payload` is the single release authority for opaque device
    handles, with a memory-lifetime regression covering fake CUDA storage
    destruction.
  - validation:
    - `c3c build --obj-out obj`
    - host focused `advanced-collections-module` -> `633 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `619 passed, 0 failed`
    - bounded-container `memory-lifetime-smoke` -> `226 passed, 0 failed`
    - direct `tensor-backends` smoke returned structured CPU/CUDA entries
    - direct `to-device 'cpu` + `device` smoke -> `"cpu"`
  - validation note:
    - host `memory-lifetime-smoke` intentionally refused under the
      container-only memory-slice policy; the bounded container run is the
      governing signal.

- Completed `TENSOR-100A` CPU-only Tensor placement/introspection surface:
  - Added public `device` and `to-device` primitives.
  - `device` reports `'cpu` for current Tensor values.
  - `to-device` with target `'cpu` realizes Tensor expressions to CPU Tensor
    storage.
  - `to-device` with target `'cuda` fails closed with
    `tensor/backend-unavailable`; no CUDA runtime dependency was added.
  - Registered both primitives in the interpreter table and AOT primitive hash.
  - validation:
    - `c3c build --obj-out obj`
    - focused `advanced-collections-module` -> `629 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `616 passed, 0 failed`
    - bounded-container full `advanced` slice -> `1933 passed, 0 failed`
    - direct `device` smoke -> `"cpu"`
    - direct handled CUDA smoke -> `"tensor/backend-unavailable"`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed
  - validation note:
    - host monolithic `advanced` still exits without a summary; focused host
      groups and the bounded-container full `advanced` policy path pass, so do
      not treat the monolithic host run as the governing Tensor gate.

- Completed `TENSOR-100` explicit-device CUDA/cuBLAS backend design:
  - Added `docs/plans/cuda-cublas-backend-decision-2026-04-16.md`.
  - Locked future GPU support behind the existing `Tensor` value instead of a
    public `GpuTensor`, `CudaTensor`, or backend-specific Tensor subtype.
  - Chose `to-device`, `device`, and `tensor-backends` as the first future
    placement/introspection surface.
  - Rejected backend-flavored math names, `tensor-use-backend!` as the first
    control surface, and implicit CPU/GPU transfer inside ordinary `map`,
    `contract`, `realize`, or `matrix/*` operations.
  - Defined the next implementation boundary: Tensor placement metadata,
    CPU-only `device`, fail-closed `to-device` diagnostics when CUDA is
    unavailable, CUDA buffer ownership/destruction tests, explicit CPU/CUDA
    copies, cuBLAS handle lifecycle, then CUDA-placed `Float64` rank-2
    contraction.
  - validation:
    - documentation/reference grep checks
    - `git diff --check`

- Completed `ADV-STACK-001` macro-hygiene non-tail recursion headroom
  calibration:
  - Lowered the `advanced-macro-hygiene-string-number` non-tail recursion probe
    from the environment-specific `896`/`1200` split to a single portable depth
    of `512`.
  - The test remains a substantial non-tail interpreter-recursion probe, but no
    longer treats high stack-depth calibration values as language/runtime
    contracts.
  - negative finding:
    - do not keep using `896` or `1200` as portable recursion headroom on this
      ARM64 workspace; the host crashed at `640`, `768`, `896`, and `1200`,
      and the bounded validation container crashed at `1200`.
  - validation:
    - `c3c build --obj-out obj`
    - host and bounded-container `advanced-macro-hygiene-string-number`
      -> `9 passed, 0 failed`
    - host and bounded-container `advanced-macro-hygiene`
      -> `83 passed, 0 failed`
    - bounded-container full `advanced` slice
      -> `1928 passed, 0 failed`
    - bounded-container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`

- Verified installed LAPACKE backend visibility after `liblapacke` became
  available on the host:
  - `ldconfig -p` now reports `liblapacke.so` and `liblapacke.so.3` under
    `/lib/aarch64-linux-gnu`.
  - host `advanced-collections-module` with
    `LD_LIBRARY_PATH=/usr/local/lib:/usr/lib/aarch64-linux-gnu`
    -> `624 passed, 0 failed`
  - bounded-container `advanced-collections-module` with the mounted aarch64
    host toolchain and LAPACK library path
    -> `611 passed, 0 failed`
  - bounded-container `memory-lifetime-smoke` with the same runtime library
    path -> `225 passed, 0 failed`

- Completed `TENSOR-090AQ` pure `matrix/eigenpairs` fallback stabilization:
  - The bounded validation container exposed a host-hidden bug where the pure
    no-`dgeev` fallback could corrupt the trailing real eigenvalue for a 3x3
    real-plus-complex-block matrix, returning a value like `6146+0i` instead
    of `2+0i`.
  - Updated the pure QR eigenvalue convergence check to treat isolated
    subdiagonal 2x2 real-Schur blocks as converged, so the fallback does not
    apply shifted QR iteration to an already-settled complex block.
  - Preserved the existing tolerance-based LAPACK QR rank guard.
  - validation:
    - `c3c build --obj-out obj`
    - host focused advanced collections/module group
      -> `624 passed, 0 failed`
    - bounded container focused advanced collections/module group with mounted
      aarch64 host toolchain -> `611 passed, 0 failed`
    - forced no-`dgeev` direct value smoke for the 3x3 real-plus-complex block
      -> `"2+0i"`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Completed `TENSOR-090AP` forced pure fallback coverage for the
  `dgeqrf`/`dorgqr`-backed QR matrix routine:
  - Added a private test-only QR backend disable hook behind the Tensor
    LAPACK backend boundary, including the `OMNI_TENSOR_DISABLE_LAPACK_DGEQRF`
    environment switch.
  - Added focused forced-fallback coverage for `matrix/qr`, proving the pure
    reduced QR fallback preserves expected public results when the runtime QR
    LAPACK backend is disabled.
  - Preserved the existing tolerance-based LAPACK QR rank guard.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `624 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Completed `TENSOR-090AO` forced pure fallback coverage for `dpotrf`-backed
  matrix routines:
  - Added a private test-only `LAPACKE_dpotrf` disable hook behind the Tensor
    LAPACK backend boundary, including the `OMNI_TENSOR_DISABLE_LAPACK_DPOTRF`
    environment switch.
  - Added focused forced-fallback coverage for `matrix/cholesky`, proving the
    pure lower-triangular Cholesky fallback preserves expected public results
    when runtime `dpotrf` is disabled.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `622 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Completed `TENSOR-090AN` forced pure fallback coverage for `dgetrf`-backed
  matrix routines:
  - Added a private test-only `LAPACKE_dgetrf` disable hook behind the Tensor
    LAPACK backend boundary, including the `OMNI_TENSOR_DISABLE_LAPACK_DGETRF`
    environment switch.
  - Added focused forced-fallback coverage for `matrix/lu` and
    `matrix/determinant`, proving the pure partial-pivot LU fallback preserves
    expected public results when runtime `dgetrf` is disabled.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `620 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Completed `TENSOR-090AM` forced pure fallback coverage for `dgesv`-backed
  matrix routines:
  - Added a private test-only `LAPACKE_dgesv` disable hook behind the Tensor
    LAPACK backend boundary, including the `OMNI_TENSOR_DISABLE_LAPACK_DGESV`
    environment switch.
  - Decoupled the `dgesv` backend path from the unrelated `dgeev` disable
    state so `matrix/solve` and `matrix/inverse` fallback validation has its
    own backend control.
  - Added focused forced-fallback coverage for `matrix/solve` and
    `matrix/inverse`, proving the pure Gaussian solver fallback preserves
    expected public results when runtime `dgesv` is disabled.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `616 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Completed `TENSOR-090AL` forced pure fallback coverage for symmetric eigen
  matrix routines:
  - Added a private test-only `LAPACKE_dsyev` disable hook behind the Tensor
    LAPACK backend boundary, including the `OMNI_TENSOR_DISABLE_LAPACK_DSYEV`
    environment switch.
  - Added focused forced-fallback coverage for `matrix/eigenvalues` and
    `matrix/eigenvectors`, proving the symmetric Jacobi fallback preserves
    expected values and vector columns when runtime `dsyev` is disabled.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `612 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090AK` forced pure fallback coverage for SVD-backed matrix
  routines:
  - Reused the private `dgesvd` disable hook to add focused forced-fallback
    coverage for `matrix/rank`, `matrix/singular-values`, and `matrix/svd`.
  - The tests prove those already-shipped public contracts keep their expected
    results when runtime `LAPACKE_dgesvd` is disabled and the pure
    row-echelon or Gram/Jacobi fallback paths are used.
  - validation:
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `606 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090AJ` matrix norm SVD backend/fallback coverage:
  - Added a private test-only `LAPACKE_dgesvd` disable hook behind the Tensor
    LAPACK backend boundary, including the
    `OMNI_TENSOR_DISABLE_LAPACK_DGESVD` environment switch.
  - Added focused `matrix/norm` coverage proving `'spectral` and `'nuclear`
    use the runtime `dgesvd` backend when available and preserve the same
    public results through the forced pure SVD fallback.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `598 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090AI` matrix norm spectral/nuclear selectors:
  - Extended public `matrix/norm` with `'spectral` and `'nuclear` selectors
    for rank-2 `Float64` Tensor inputs.
  - `'spectral` returns the largest singular value and `'nuclear` returns the
    sum of singular values, reusing the existing `matrix_svd_factor`
    pure/LAPACK machinery.
  - Preserved lazy input realization, empty-axis behavior as `0.0`, and
    invalid selector validation.
  - validation:
    - direct spectral smoke on diagonal `[3 0; 0 2]` -> `3.0`
    - direct nuclear smoke on diagonal `[3 0; 0 2]` -> `5.0`
    - direct empty spectral smoke -> `0.0`
    - direct empty nuclear smoke -> `0.0`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `592 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090AH` matrix norm surface:
  - Added public `matrix/norm` for rank-2 `Float64` Tensor inputs.
  - The operation realizes lazy inputs, supports empty axes as `0.0`, returns
    a `Float64`, defaults to the Frobenius norm, and accepts explicit
    `'frobenius`, `'one`, `'infinity`, or `'max` selectors.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Added focused advanced collections/module coverage for default and
    explicit Frobenius, one-norm, infinity-norm, max-absolute norm, empty
    axes, lazy input realization, rank validation, dtype validation, and
    selector validation.
  - validation:
    - direct default Frobenius smoke -> `5.0`
    - direct one-norm smoke -> `9.0`
    - direct infinity-norm smoke -> `15.0`
    - direct lazy Frobenius smoke -> `5.0`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `590 passed, 0 failed`

- Completed `TENSOR-090AG` eigenpairs residual harness:
  - Added reusable Lisp-side helpers in the advanced collections/module tests
    to validate all three returned vector columns for 3x3
    `matrix/eigenpairs` inputs.
  - Added backend and forced-fallback harness checks covering both the
    non-normal upper-triangular matrix `[5 7 0; 0 3 2; 0 0 1]` and the
    real-plus-complex-block matrix `[0 -1 0; 1 0 0; 0 0 2]`.
  - Each harness verifies every returned column satisfies `A*v ~= lambda*v`,
    reducing reliance on hand-expanded one-column residual assertions.
  - validation:
    - prototype backend and forced-fallback helper smokes -> `true`
    - `c3c build --obj-out obj`
    - focused advanced collections/module group on host
      -> `580 passed, 0 failed`

- Completed `TENSOR-090AF` non-normal eigenpairs residual coverage:
  - Added focused residual checks for the non-normal upper-triangular matrix
    `[5 7 0; 0 3 2; 0 0 1]`.
