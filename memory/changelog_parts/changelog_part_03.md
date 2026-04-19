# Memory Changelog Index Part 03

Source: `memory/CHANGELOG.md`

    parity, Stage 3 source parity, and targeted `git diff --check`.

- TENSOR-100F Vulkan `Float32` rank-N contract checkpoint:
  - Added `csrc/tensor_vulkan_contract_f32.comp` and generated
    `csrc/tensor_vulkan_contract_f32_spv.c` as a distinct dense row-major
    generic `Float32` contract shader path.
  - Added `omni_tensor_backend_vulkan_contract_f32` and C3 extern wiring while
    preserving the existing `Float64` helper and keeping the rank-1 dot fast
    path `Float64`-only.
  - Updated public Vulkan `contract` dispatch to accept matching dense
    row-major `Float32` operands, allocate `Float32` result metadata, and call
    the `Float32` helper without hidden widening, hidden downcast, or CPU
    fallback.
  - Added availability-gated regressions for eager and lazy Vulkan `Float32`
    contract operands across rank-1 dot, rank-2, rank-N single-axis,
    multi-axis, zero-axis, zero-size, zero-free output, dtype preservation,
    result placement, mixed-device rejection, and mixed-dtype rejection.
  - Superseded by later checkpoints above: deferred `Float32` boundaries now
    exclude Vulkan structural matrix, reducer, SVD-backed, serial
    factor/solve kernels, staged parallel solve, and large-dense SVD
    robustness. CUDA `Float32` placement, scalar `Float32` values, CPU
    `Float32` factor/SVD contracts, fixed-width complex, and
    stride/view-backed Vulkan layouts remain deferred/fail-closed.
  - Validation passed: `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_contract_f32.comp -o
    /tmp/omni_tensor_vulkan_contract_f32.spv`, `spirv-val --target-env
    vulkan1.0 /tmp/omni_tensor_vulkan_contract_f32.spv`,
    `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`, direct
    Vulkan `Float32` contract smokes (`154.0`, `"Float32"`, `vulkan`, `0.0`,
    `460.0`, `210.0`, zero-free length `0`, and
    `tensor/dtype-mismatch` for mixed dtype), host focused
    `advanced-collections-module` `pass=1039 fail=0`, bounded-container
    focused `advanced-collections-module` `pass=1026 fail=0`, primitive docs
    parity, Stage 3 source parity, and targeted `git diff --check`.

- TENSOR-100F Vulkan `Float32` placement/map/unary/minmax checkpoint:
  - Added explicit Vulkan `Float32` capability reporting through
    `tensor-backends` and `omni_tensor_backend_vulkan_float32_available`.
  - `to-device 'vulkan` and `to-device 'cpu` now support concrete
    `Tensor Float32` placement and copyback when Vulkan is available.
  - Vulkan destination-form `realize` now accepts matching dense row-major
    `Float32` CPU/Vulkan/lazy sources and scalar fills without adding a
    backend-specific public API.
  - Added distinct `Float32` Vulkan shader/helper ABI paths for dense
    row-major binary `map` arithmetic/min/max and unary map operations:
    `csrc/tensor_vulkan_map_f32.comp`,
    `csrc/tensor_vulkan_map_unary_f32.comp`,
    `omni_tensor_backend_vulkan_map_f32`,
    `omni_tensor_backend_vulkan_map_unary_f32`, and
    `omni_tensor_backend_vulkan_fill_f32`.
  - Public `map`, direct unary Tensor operations, and direct `min`/`max` now
    dispatch eligible matching `Float32` Vulkan operands to real Vulkan
    helpers. `Float64` remains `Float64`; no hidden `Float64` downcast to
    `Float32`, hidden widening, or CPU fallback was added.
  - Added focused availability-gated regressions for `Float32` backend
    capability, placement/copyback dtype preservation, Vulkan destination
    `realize`, elementwise `map`, unary helpers, direct `min`/`max`, and
    `Float64` no-downcast preservation.
  - Superseded by later checkpoints above: deferred `Float32` boundaries now
    exclude Vulkan rank-N `contract`, structural matrix, reducer, SVD-backed,
    serial factor/solve kernels, staged parallel solve, and large-dense SVD
    robustness. CUDA `Float32` placement, scalar `Float32` values, CPU
    `Float32` factor/SVD contracts, fixed-width complex, and
    stride/view-backed Vulkan layouts remain deferred/fail-closed.
  - Validation passed: `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_f32.comp -o /tmp/omni_map_f32.spv`, `spirv-val
    --target-env vulkan1.0 /tmp/omni_map_f32.spv`, `glslangValidator -V
    --target-env vulkan1.0 csrc/tensor_vulkan_map_unary_f32.comp -o
    /tmp/omni_map_unary_f32.spv`, `spirv-val --target-env vulkan1.0
    /tmp/omni_map_unary_f32.spv`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct Vulkan `Float32` smokes, host focused
    `advanced-collections-module` `pass=1025 fail=0`, and bounded-container
    focused `advanced-collections-module` `pass=1012 fail=0`, primitive docs
    parity, Stage 3 source parity, and targeted `git diff --check`.

- TENSOR-100F Vulkan symmetric eigen large-`n` checkpoint:
  - Removed the old `n <= 64` Vulkan `matrix/eigenvalues` /
    `matrix/eigenvectors` private-array cap for dense row-major square
    symmetric `Float64` inputs.
  - `csrc/tensor_vulkan_symmetric_eigen_f64.comp` now stores Jacobi matrix
    scratch in hidden storage behind the public eigenvector output payload
    instead of a fixed private `double[4096]` array.
  - `omni_tensor_backend_vulkan_symmetric_eigen_f64` now validates visible
    value/vector counts separately from vector backing storage, allocates
    `2 * n * n` vector storage, preserves the public `[n]` and `[n n]`
    Tensor shapes, and rejects resource sizes that would exceed 32-bit shader
    indices or wrap the shader's `64 * n * n` Jacobi iteration guard.
  - Public behavior remains backend-neutral: concrete and lazy Vulkan
    symmetric inputs return Vulkan-placed values/vectors; nonsymmetric inputs
    map to `tensor/not-symmetric`; no-convergence maps to
    `tensor/no-convergence`; unsupported dtype/layout/resource/capability
    cases fail closed; and there is no hidden CPU/LAPACK fallback.
  - Added availability-gated `65x65` regressions for direct and lazy
    eigenvalues/eigenvectors, Vulkan output placement, large nonsymmetric
    diagnostics, and unchanged `LAPACKE_dsyev` counter behavior.
  - Large sparse nonsymmetric tests should not use a recursive
    `(range 4225)` list fixture; that stack-overflowed in a handle body.
    Build sparse large fixtures from `(Array (Tensor Float64 [65 65] 0.0))`
    plus `set!` instead.
  - Deferred performance work is tracked in `TODO.md` as measurement-driven
    tiling or staged execution for the storage-backed large-`n` path, not as a
    semantic support blocker.
  - Validation passed: `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_symmetric_eigen_f64.comp -o
    /tmp/omni_symmetric_eigen.spv`, `spirv-val --target-env vulkan1.0
    /tmp/omni_symmetric_eigen.spv`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct `65x65` Vulkan smokes, host focused
    `advanced-collections-module` `pass=1006 fail=0`, and bounded-container
    focused `advanced-collections-module` `pass=993 fail=0`, primitive docs
    parity, Stage 3 source parity, and targeted `git diff --check`.

- TENSOR-100F Vulkan status-readback helper factoring checkpoint:
  - Factored shared `Float64` Vulkan status payload readback boilerplate into a
    common copy-to-host path plus mapper-based status decoding.
  - Migrated compatible trailing-status, SVD/singular-values, and
    symmetric-eigen status readers through the shared readback path while
    preserving distinct domain mappings: generic trailing nonzero still maps to
    singular, SVD/singular-values status still distinguishes no-convergence, and
    symmetric eigen status still distinguishes not-symmetric.
  - LU metadata validation now reuses the generic trailing-status mapper while
    keeping pivot and swap-count validation LU-specific. Parallel solve remains
    on its separate `uint32_t` status-buffer ABI by design.
  - Public Tensor behavior is unchanged: Vulkan result placement, diagnostics,
    output ownership, no hidden CPU/LAPACK fallback, and explicit CPU
    inspection requirements are preserved.
  - Validation passed: `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, host focused `advanced-collections-module`
    `pass=995 fail=0`, bounded-container focused
    `advanced-collections-module` `pass=982 fail=0`, primitive docs parity,
    Stage 3 source parity, and targeted `git diff --check`.

- TENSOR-100F Vulkan serial dispatch helper factoring checkpoint:
  - Factored shared sequential storage-buffer descriptor layout creation,
    descriptor pool/allocation/update, and standard one-time command
    recording/submission for the mature serial Vulkan dispatch helper family:
    two-buffer, three-buffer, one-input/two-output, and one-input/three-output.
  - Preserved public Tensor behavior: descriptor binding order, synchronous
    submit/wait behavior, Vulkan result placement, output-buffer ownership,
    diagnostics, and no hidden CPU/LAPACK fallback are unchanged.
  - Later checkpoint above factored compatible `Float64` status-readback
    boilerplate while keeping parallel solve's typed `uint32_t` status-buffer
    ABI separate by design.
  - Validation passed: `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, host focused `advanced-collections-module`
    `pass=995 fail=0`, bounded-container focused
    `advanced-collections-module` `pass=982 fail=0`, primitive docs parity,
    Stage 3 source parity, and targeted `git diff --check`.

- TENSOR-100F native CPU `Tensor Float32` storage checkpoint:
  - Added `TENSOR_DTYPE_FLOAT32` as native Tensor storage with 32-bit element
    allocation, zeroing, copy/clone, dtype name, and dtype symbol support.
  - `Tensor Float32` constructors now narrow finite numeric data into
    homogeneous 32-bit storage and reject non-finite or out-of-range elements.
    Historical note, superseded by the scalar checkpoint above: scalar
    `Float32` / `(Float x 32)` constructors were fail-closed until a scalar
    value representation existed.
  - CPU `map`, `contract`, `realize`, `to-device 'cpu`, `ref`, `Array`,
    `List`, and `Iterator` now support `Float32` Tensor storage. Historical
    note, superseded by the scalar checkpoint above: element extraction
    originally widened to scalar `Float64` values before the scalar `Float32`
    value tag landed.
  - Structural CPU matrix helpers now support `Float32` where they preserve
    dtype or return scalar readback: `matrix/transpose`, `matrix/diagonal`,
    `matrix/diagonal-matrix`, `matrix/identity`, and `matrix/trace`. Heavier
    LAPACK-style routines remain explicit non-`Float32` lanes and must not
    silently widen and narrow.
  - Historical note, superseded by later Vulkan and CUDA checkpoints:
    CUDA/Vulkan placement remained fail-closed for `Float32`; the then-remaining
    `Float32` TODO was explicit device copy paths, Vulkan kernels, backend
    capability reporting, and no-downcast tests.
  - Validation passed: `c3c build --obj-out obj`, host focused
    `advanced-collections-module` `pass=995 fail=0`, host focused
    `advanced-unicode-iterator` `pass=159 fail=0`, bounded-container focused
    `advanced-collections-module` `pass=982 fail=0`, bounded-container
    `memory-lifetime-smoke` `pass=228 fail=0`, primitive docs parity, Stage 3
    source parity, and targeted `git diff --check`. ASAN build was attempted
    but the local `c3c` rejected sanitizer mode with
    "Address sanitizer is only supported on Linux, FreeBSD, NetBSD, Darwin and
    Windows."

- TENSOR-100F Vulkan unary identity and helper factoring checkpoint:
  - Routed unary Tensor `map +` for dense row-major Vulkan `Float64` operands
    through the existing separate unary helper identity opcode. This preserves
    Vulkan placement and keeps the invalidated generic unary map branch out of
    use.
  - Made public unary `+` a scalar/Tensor identity surface by registering `+`
    as one-or-two-argument dispatched primitive and keeping `prim_add` explicit
    about over-arity rejection. `(+ 1 2 3)` still raises arity instead of
    silently becoming variadic addition.
  - Updated tests for the changed unary `+` contract, including no-auto-partial
    coverage on `*`, over-arity rejection for `+`, typed unary `+` method
    dispatch, Vulkan unary `map +` roundtrip/placement, and direct Tensor
    unary `+` placement preservation.
  - Reused `omni_tensor_vulkan_create_compute_pipeline_for_shader` across the
    mature serial Vulkan dispatch helper family: two-buffer, three-buffer,
    one-input/two-output, and one-input/three-output helpers. Descriptor,
    command-buffer, and status-copy helper factoring remain open.
  - Validation passed: `glslangValidator` and `spirv-val` for
    `csrc/tensor_vulkan_map_unary_f64.comp`,
    `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`, host
    focused `advanced-unicode-iterator` `pass=159 fail=0`, host focused
    `advanced-collections-module` `pass=972 fail=0`, bounded-container focused
    `advanced-collections-module` `pass=959 fail=0`, primitive docs parity,
    Stage 3 source parity, and targeted `git diff --check`.

- TENSOR-076B constructor-driven iterator materialization checkpoint:
  - Design priority changed: constructor dispatch is the canonical public
    materialization boundary. Iterators are already lazy pull computations, and
    Tensor lazy expression payloads stay internal to `Tensor`; do not introduce
    public `delay` / `force` or treat `realize` as the general lazy protocol.
  - `realize` is now documented as a low-level Tensor storage primitive for
    exact-shape/dtype destination writes and compatibility with existing
    storage tests, including the primitive appendix row that previously used
    stale force-style wording.
  - Implemented `(Tensor iterator)`, `(Tensor iterator dtype)`, and
    `(Tensor dtype shape iterator)` by consuming finite numeric iterators
    through constructor dispatch. This covers lazy iterator pipelines such as
    `(Tensor (map f (Iterator xs)))`.
  - Added regressions for inferred iterator Tensor construction, lazy iterator
    `map` into `Tensor`, explicit-shape iterator data, BigInteger dtype
    preservation, and fail-closed non-numeric iterator elements.
  - `TENSOR-076C` follow-up completed in the same session:
    `Iterator(^Tensor)` now yields flat row-major Tensor elements, matching
    `(Array tensor)` and `(List tensor)`. Lazy CPU Tensor expressions are
    realized once into Tensor storage for iteration, and non-CPU device tensors
    fail closed until explicitly copied with `to-device 'cpu`.
  - Validation passed: `c3c build --obj-out obj`, direct iterator-to-Tensor
    smokes (`3.0`, `3.0`, `6.0`, and `"9223372036854775808"`), and host
    focused `advanced-collections-module` `pass=969 fail=0`,
    bounded-container focused `advanced-collections-module` `pass=956 fail=0`,
    primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check`.

- TENSOR-100F CUDA destination-form `realize` checkpoint:
  - Added existing-buffer CUDA helpers:
    `omni_tensor_backend_cuda_copy_to_existing_device`,
    `omni_tensor_backend_cuda_copy_device_to_existing_device`, and
    `omni_tensor_backend_cuda_fill_f64`, with C3 externs in
    `src/lisp/tensor_cuda_backend.c3`.
  - Destination-form `(realize source destination)` now supports existing
    dense row-major CUDA `Float64` destinations for matching CPU sources,
    CUDA sources, supported lazy CUDA contract results, lazy CPU expressions,
    and scalar fills. The public surface remains backend-neutral; CPU
    destinations still reject CUDA sources until the caller explicitly copies
    with `to-device 'cpu`.
  - Hardened unsupported device `map` construction so CUDA map operands fail
    immediately with `tensor/backend-unsupported` instead of producing a lazy
    expression that later fails at boundary copy time.
  - Added availability-gated regressions for CPU source -> CUDA destination,
    CUDA source -> CUDA destination, lazy CPU source -> CUDA destination,
    scalar fill -> CUDA destination, lazy cuBLAS contract -> CUDA destination,
    CUDA source -> CPU destination fail-closed behavior, unsupported CUDA map
    fail-closed behavior, and CUDA/Vulkan cross-backend destination rejection.
  - Validation passed: `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct runtime smokes for CUDA destination cases
    and unsupported CUDA map diagnostics, host focused
    `advanced-collections-module` `pass=959 fail=0`, bounded-container focused
    `advanced-collections-module` `pass=946 fail=0`, primitive docs parity,
    and Stage 3 source parity.

- TENSOR-100F Vulkan destination-form `realize` checkpoint:
  - Added existing-buffer Vulkan copy/fill helpers:
    `omni_tensor_backend_vulkan_copy_to_existing_device`,
    `omni_tensor_backend_vulkan_copy_device_to_existing_device`, and
    `omni_tensor_backend_vulkan_fill_f64`, with C3 externs in
    `src/lisp/tensor_vulkan_backend.c3`.
  - Destination-form `(realize source destination)` now supports existing
    dense row-major Vulkan `Float64` destinations for matching CPU sources,
    Vulkan sources, lazy Vulkan results, and scalar fills. The public surface
    remains backend-neutral; CPU destinations still reject device sources until
    the caller explicitly copies with `to-device 'cpu`.
  - Added availability-gated regressions for CPU source -> Vulkan destination,
    Vulkan source -> Vulkan destination, lazy Vulkan source -> Vulkan
    destination, and scalar fill -> Vulkan destination, while preserving
    Vulkan source -> CPU destination fail-closed behavior.
  - Validation passed: `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct runtime smokes for the four supported
    Vulkan destination cases plus preserved CPU-destination failure, host
    focused `advanced-collections-module` `pass=949 fail=0`, bounded-container
    focused `advanced-collections-module` `pass=936 fail=0`, primitive docs
    parity, and Stage 3 source parity.

- TENSOR-100F direct Vulkan `matrix/svd` factor-output checkpoint:
  - Added `csrc/tensor_vulkan_svd_f64.comp`, generated
    `csrc/tensor_vulkan_svd_f64_spv.c`, and wired
    `omni_tensor_backend_vulkan_svd_f64` through the helper build and
    `project.json`.
  - Public `matrix/svd` now routes dense row-major Vulkan `Float64` rank-2
    tensors through Vulkan before CPU fallback, including lazy Vulkan inputs,
    and returns Vulkan-placed `u`, `s`, and `v` tensors with shapes
    `[rows k]`, `[k]`, and `[cols k]` for `k = min(rows, columns) <= 64`.
  - The Vulkan SVD path keeps CPU copyback explicit through `to-device 'cpu`,
    rejects `k > 64` with `tensor/backend-unsupported`, maps shader
    non-convergence through the Vulkan status path, and asserts no hidden
    `LAPACKE_dgesvd` counter movement for direct, lazy, and exact-`k == 64`
    Vulkan inputs.
  - Negative memory: do not restore a second private `double[4096]`
    eigenvector array in the shader. That approach compiled but failed at
    runtime on this Vulkan stack with `tensor/backend-execution-failed`; the
    current shader stores the driving eigenvectors in the eventual output
    buffer and keeps only the Gram matrix plus eigenvalues in private storage.
  - Validation passed: SVD shader compile/`spirv-val`,
    `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`, focused
    `advanced-collections-module` `pass=945 fail=0`, JSON/shell syntax checks,
    and `git diff --check`.

- TENSOR-100F audit-hardening implementation checkpoint:
  - Updated generic Vulkan contract failure context from
    `contract: Vulkan single-axis kernel failed` to
    `contract: Vulkan Float64 contract kernel failed`, matching the current
    zero-or-more-axis dense row-major helper.
  - Added public zero-size Vulkan `matrix/norm` `'spectral` / `'nuclear`
    regressions for empty rows and empty columns, plus unchanged `dgesvd`
    counter coverage.
  - Added unchanged LAPACK counter guards for direct and lazy Vulkan
    fail-closed `matrix/eigenvalues`, `matrix/eigenvectors`, and
    `matrix/eigenpairs`; the earlier `matrix/svd` fail-closed guard was later
    superseded by direct Vulkan `Float64` factor-output support.
  - Added diagnostic-order regressions proving invalid-but-Vulkan SVD/eigen
    operands return `tensor/backend-unsupported` before CPU shape or symmetry
    diagnostics.
  - Added internal lazy-contract helper coverage for zero-axis and multi-axis
    Vulkan contractions.
  - Validation passed: `c3c build --obj-out obj`, focused
    `advanced-collections-module` `pass=935 fail=0`, targeted
    `git diff --check`, primitive docs parity, and Stage 3 source parity.

- Vulkan direct SVD/eigen/Float32 planning checkpoint:
  - Added `docs/plans/vulkan-svd-factor-output-plan-2026-04-17.md` for direct
    Vulkan `matrix/svd` under the existing backend-neutral surface.
  - Added `docs/plans/vulkan-eigensolver-plan-2026-04-17.md` for direct Vulkan
    `matrix/eigenvalues`, `matrix/eigenvectors`, and `matrix/eigenpairs`.
    Symmetric real `Float64` eigenvalues/eigenvectors are the first eligible
    phase; general `matrix/eigenpairs` remains blocked for Vulkan while the
    public output contract is pointer-backed `BigComplex`.
  - Added `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`.
    `Float32` remains a future native Tensor dtype and Vulkan kernel tier, not
    a downcast fallback for current `Float64` execution.
  - Updated the Vulkan roadmap, plan index, TODO, and area status so direct
    `matrix/svd` is now tracked as implemented for dense row-major Vulkan
    `Float64` rank-2 inputs with `k <= 64`, while eigen and `Float32` remain
    planned/fail-closed until their gates land.
  - Normalized stale planning text that still claimed Vulkan spectral/nuclear
    norms or zero-axis contractions were unsupported.

- Advanced `TENSOR-100F1` Vulkan SVD-backed scalar/vector slice:
  - Added dense row-major Vulkan `Float64` `matrix/singular-values` through a
    checked-in Gram/Jacobi singular-value shader, returning a Vulkan-placed
    rank-1 Tensor and preserving explicit `to-device 'cpu` inspection.
  - Extended Vulkan `matrix/norm` `'spectral` and `'nuclear` selectors to reuse
    the same singular-value helper and read back only the scalar norm result.
  - Direct `matrix/svd` has since landed as a dedicated Vulkan factor-output
    path for dense row-major `Float64` rank-2 inputs with `k <= 64`; direct
    `matrix/eigenvalues`, `matrix/eigenvectors`, and `matrix/eigenpairs`
    remain CPU-only/fail-closed pending Vulkan eigensolver gates.
  - Fixed the unsupported direct SVD/eigen entrypoints so non-CPU Tensor
    placement is rejected before generic realization can silently copy a Vulkan
    input through the CPU path.
  - Hardened the public realization boundary so one-argument `realize` preserves
    concrete Vulkan placement, while explicit destination `realize` rejects
    concrete or lazy Vulkan sources with `tensor/backend-unsupported` instead
    of a misleading missing CPU backing-storage error.
  - Hardened destination `realize` diagnostics so valid non-CPU destination
    tensors, including Vulkan destinations, fail closed with
    `tensor/backend-unsupported` instead of the missing CPU backing-storage
    runtime error.
  - Hardened the Vulkan singular-values helper:
    - dispatches only one work item because the shader is single-invocation;
    - keeps `output_count = k + 2` for the output/status/nuclear payload;
    - maps shader non-convergence status to `tensor/no-convergence` through a
      dedicated Vulkan status code instead of collapsing it into generic
      backend execution failure;
    - rejects logical matrices whose element count exceeds the shader's
      32-bit index space before dispatch.
  - Added availability-gated regressions for `k > 64` fail-closed behavior,
    `k == 64` boundary support, wide/rank-deficient/empty Vulkan singular
    values, spectral/nuclear norms, lazy Vulkan eigen fail-closed inputs,
    explicit-destination `realize` fail-closed behavior, CPU-only numeric Tensor
    helpers on Vulkan operands, status-payload non-convergence mapping, and the
    oversized-index validation guard; lazy Vulkan `matrix/svd` is now covered
    by the direct Vulkan factor-output slice.
  - Normalized Vulkan zero-axis `contract` docs so current public references
    say zero or more explicit contracted axis pairs instead of the stale
    one-or-more wording.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0
      csrc/tensor_vulkan_singular_values_f64.comp` passed
    - `spirv-val --target-env vulkan1.0
      /tmp/omni_tensor_vulkan_singular_values_f64.spv` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - durable advanced regressions now cover `k == 64`, `k == 65`
      fail-closed, wide `2x65`, empty `0x65`, status mapping, and oversized
      index validation
    - host focused `advanced-collections-module` -> `907 passed, 0 failed`
    - host focused `advanced-stdlib-numeric` -> `411 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and targeted
      `git diff --check` passed

- Advanced `TENSOR-100E` Vulkan zero-axis contract:
  - Enabled public `(contract left right [] [])` for Vulkan-placed dense
    row-major `Float64` tensors through the existing generic Vulkan contract
    shader and metadata layout.
  - Relaxed the public Vulkan `contract` gate and
    `omni_tensor_backend_vulkan_contract_f64` so `axis_count == 0` with null
    axis arrays is accepted. Rank-0 scalar tensors are also accepted by the
    helper for scalar-scalar no-axis products.
  - Kept the rank-1 dot fast path limited to the one-contracted-axis case so
    rank-1/rank-1 zero-axis contractions fall through to the generic outer
    product shader path.
  - Added availability-gated regressions for rank-1/rank-1 outer products,
    rank-2/rank-1 outer products, rank-0 scalar products, Vulkan result
    residency, and zero-free-dimension zero-length output.
  - Stride-aware/non-contiguous layouts, unsupported dtypes, and mixed
    CPU/Vulkan operands still fail closed with Tensor backend diagnostics.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0
      csrc/tensor_vulkan_contract_f64.comp` passed
    - `spirv-val --target-env vulkan1.0
      /tmp/omni_tensor_vulkan_contract_f64.spv` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan smokes returned `60.0`, `12.0`, `vulkan`, and `0`
    - host focused `advanced-collections-module` -> `851 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `838 passed, 0 failed`
    - primitive docs parity and Stage 3 source parity passed
    - `git diff --check` passed

- Advanced `TENSOR-100E/F` Vulkan `Float64` `min` / `max`:
  - Extended the existing `csrc/tensor_vulkan_map_f64.comp` binary map helper
    so op `4` is `min(left, right)` and op `5` is `max(left, right)`, then
    regenerated `csrc/tensor_vulkan_map_f64_spv.c`.
  - Widened `omni_tensor_backend_vulkan_map_f64` validation to accept opcodes
    `0..5` while preserving the existing binary descriptor and metadata ABI.
  - Routed public direct Tensor `min` / `max` through the Vulkan binary helper
    for dense row-major Vulkan `Float64` tensor/scalar, scalar/Tensor, and
    Tensor/Tensor broadcast cases. Results remain Vulkan-placed and require
    explicit `(to-device ... 'cpu)` for CPU inspection.
  - Added `map min` / `map max` Vulkan opcode selection on the same binary
    map path.
  - Changed Tensor `min` / `max` operand resolution to use the any-device
    concrete resolver before CPU-storage checks, so eligible Vulkan operands
    execute on Vulkan while mixed CPU/Vulkan operands fail closed instead of
    triggering hidden CPU materialization.
  - Added Vulkan direct, Vulkan map, lazy Vulkan input, mixed-device
    fail-closed, broadcast, and CPU map parity regressions.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0
      csrc/tensor_vulkan_map_f64.comp` passed
    - `spirv-val --target-env vulkan1.0
      /tmp/omni_tensor_vulkan_map_f64.spv` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct smokes returned Vulkan `map min` value `-2.0`, direct Vulkan
      `min` value `0.0`, lazy direct Vulkan `max` device `vulkan`, and mixed
      CPU/Vulkan `tensor/backend-unsupported`
    - host focused `advanced-collections-module` -> `846 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `833 passed, 0 failed`

- Advanced `TENSOR-100E/F` Vulkan `Float64` real-valued component operations:
  - Extended the separate `csrc/tensor_vulkan_map_unary_f64.comp` helper so
    op `3` is identity and op `4` is zero-fill, then regenerated
    `csrc/tensor_vulkan_map_unary_f64_spv.c`.
  - Widened `omni_tensor_backend_vulkan_map_unary_f64` validation to accept
    opcodes `0..4` while keeping the same two-buffer descriptor ABI and
    without reviving the invalidated generic Vulkan `map` mode-3 branch.
  - Routed public `(map real-part <vulkan-tensor>)`,
    `(map imag-part <vulkan-tensor>)`, `(map conjugate <vulkan-tensor>)`, and
    direct Tensor `real-part` / `imag-part` / `conjugate` through the unary
    Vulkan helper for dense row-major Vulkan `Float64` tensors. Results remain
    Vulkan-placed and require explicit `(to-device ... 'cpu)` for CPU
    inspection.
  - Changed the direct Tensor component paths to resolve concrete tensors on
    any device before the CPU-storage branch, avoiding hidden CPU fallback and
    avoiding host-storage copy helpers for Vulkan buffers.
  - Added Vulkan direct, Vulkan map, lazy Vulkan input, and CPU map parity
    regressions for `real-part`, `imag-part`, and `conjugate`.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0
      csrc/tensor_vulkan_map_unary_f64.comp` passed
    - `spirv-val --target-env vulkan1.0
      /tmp/omni_tensor_vulkan_map_unary_f64.spv` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct smokes returned direct Vulkan `real-part` value `-2.5`,
      direct Vulkan `imag-part` value `0.0`, direct Vulkan `conjugate` device
      `vulkan`, and Vulkan `map imag-part` value `0.0`
    - host focused `advanced-collections-module` -> `836 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `823 passed, 0 failed`

- Advanced `TENSOR-100E/F` Vulkan `Float64` unary square root:
  - Extended the separate `csrc/tensor_vulkan_map_unary_f64.comp` helper so
    op `2` is `sqrt(value)`, then regenerated
    `csrc/tensor_vulkan_map_unary_f64_spv.c`.
  - Widened `omni_tensor_backend_vulkan_map_unary_f64` validation to accept
    opcodes `0..2` while keeping the same two-buffer descriptor ABI and
    without reviving the invalidated generic Vulkan `map` mode-3 branch.
  - Routed public `(map sqrt <vulkan-tensor>)` and direct Tensor
    `(sqrt <vulkan-tensor>)` through the unary Vulkan helper for dense
    row-major Vulkan `Float64` tensors. Results remain Vulkan-placed and
    require explicit `(to-device ... 'cpu)` for CPU inspection.
  - Changed direct Tensor unary math on Vulkan to fail closed for unsupported
    operations instead of materializing hidden CPU results. `map sin` remains
    unsupported on Vulkan and returns `tensor/backend-unsupported`.
  - Added positive finite-input Vulkan tests for `map sqrt`, result placement,
    direct Tensor `sqrt`, and lazy direct Tensor `sqrt`, plus CPU
    `(map sqrt <Float64 tensor>)` parity coverage.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0
      csrc/tensor_vulkan_map_unary_f64.comp` passed
    - `spirv-val --target-env vulkan1.0
      /tmp/omni_tensor_vulkan_map_unary_f64.spv` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct smokes returned Vulkan `map sqrt` value `3.0`, Vulkan result
      device `vulkan`, direct Vulkan `sqrt` value `2.0`, lazy direct Vulkan
      `sqrt` device `vulkan`, and unsupported `map sin`
      `tensor/backend-unsupported`
    - host focused `advanced-collections-module` -> `824 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `811 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100E/F` Vulkan `Float64` unary negation:
  - Extended the separate `csrc/tensor_vulkan_map_unary_f64.comp` helper so
    op `0` remains absolute value and op `1` is unary negation, then
    regenerated `csrc/tensor_vulkan_map_unary_f64_spv.c`.
  - Widened `omni_tensor_backend_vulkan_map_unary_f64` validation to accept
    the negation opcode while keeping the same two-buffer descriptor ABI and
    without reviving the invalidated generic Vulkan `map` mode-3 branch.
  - Fixed the documented scalar unary `-` surface by registering `-` so the
    existing unary branch in `prim_sub` is reachable; `prim_sub` now rejects
    calls outside the `1..2` argument contract itself.
  - Added direct Tensor unary `-` for CPU tensors, preserving native Tensor
    dtype for `Float64`, `BigInteger`, `BigFloat`, and `BigComplex`.
  - Routed public `(map - <vulkan-tensor>)` and direct Tensor
    `(- <vulkan-tensor>)` through the unary Vulkan helper for dense row-major
    Vulkan `Float64` tensors. Results remain Vulkan-placed and require
    explicit `(to-device ... 'cpu)` for CPU inspection.
  - Preserved fail-closed behavior for unsupported Vulkan unary callables:
    `(map sqrt <vulkan-tensor>)` still returns `tensor/backend-unsupported`.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0
      csrc/tensor_vulkan_map_unary_f64.comp` passed
    - `spirv-val --target-env vulkan1.0
      /tmp/omni_tensor_vulkan_map_unary_f64.spv` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct smokes returned scalar `-1`, CPU Tensor `2.5`, Big* Tensor
      strings `-9223372036854775808`, `-1e+309`, `-3+4i`, Vulkan map value
      `2.0`, Vulkan result device `vulkan`, direct Vulkan value `-1.5`, and
      `tensor/backend-unsupported`
    - host focused `advanced-collections-module` -> `819 passed, 0 failed`
    - host `basic` slice -> `144 passed, 0 failed`
    - host `compiler` slice -> `277 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `806 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100E/F` Vulkan `Float64` unary absolute value:
  - Added `csrc/tensor_vulkan_map_unary_f64.comp` plus generated
    `csrc/tensor_vulkan_map_unary_f64_spv.c`, wired through `project.json`
    and `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_map_unary_f64`, a separate two-buffer
    helper with its own push-constant ABI. This does not reuse or resurrect
    the invalidated generic Vulkan `map` mode-3 unary branch.
  - Added the C3 Vulkan extern and a shared C3 result constructor for dense
    row-major Vulkan `Float64` unary helper outputs.
  - Routed public `(map abs <vulkan-tensor>)` and direct Tensor
    `(abs <vulkan-tensor>)` through the helper for Vulkan-placed dense
    row-major `Float64` tensors. Results remain Vulkan-placed and require
    explicit `(to-device ... 'cpu)` for CPU inspection.
  - Preserved fail-closed behavior for unsupported Vulkan unary callables:
    `(map sqrt <vulkan-tensor>)` still returns `tensor/backend-unsupported`.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0
      csrc/tensor_vulkan_map_unary_f64.comp` passed
    - `spirv-val --target-env vulkan1.0
      /tmp/omni_tensor_vulkan_map_unary_f64.spv` passed
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan smokes returned `1.5`, `vulkan`, `1.5`, `vulkan`, and
      `tensor/backend-unsupported`
    - host focused `advanced-collections-module` -> `810 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `797 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
      passed

- Advanced `TENSOR-100G` measured threshold routing for Vulkan `Float64`
  solve:
  - Replaced the fixed `n >= 3` bring-up condition in public `matrix/solve`
    routing with private `OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_MIN_N = 65`.
    Vulkan dense row-major `Float64` solve systems below `65` use the serial
    helper; systems at or above `65` use the staged parallel helper.
  - Added `omni_tensor_backend_vulkan_solve_serial_call_count` as a private
    test/diagnostic counter so below-threshold tests can prove direct serial
    execution, not just unchanged parallel counters.
  - Local in-process measurements are recorded under
    `build/vulkan_solve_threshold_20260417_*`: `3`, `9`, `16`, and `32` were
    tied or serial-favorable, while `65` was the first tested size where the
    staged parallel route won across identity and dense fixtures.
  - Added availability-gated regressions proving `2x2` and `9x9` stay serial,
    `65x65` enters the staged parallel helper without LAPACK, pivot-reduce and
    factor-stage counters still move, and a `65x65` singular system exercises
    the parallel status path.
  - validation:
    - `./scripts/build_omni_chelpers.sh` passed
    - `c3c build --obj-out obj` passed with existing deprecation warnings
    - direct Vulkan `matrix/solve` smokes returned `9.0`, `1.0`, `1.0`, and
      `tensor/singular-matrix`
    - host focused `advanced-collections-module` -> `806 passed, 0 failed`
    - bounded-container focused `advanced-collections-module`
      -> `793 passed, 0 failed`
    - primitive docs parity, Stage 3 source parity, and `git diff --check`
