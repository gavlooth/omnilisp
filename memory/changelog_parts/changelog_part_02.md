# Memory Changelog Index Part 02

Source: `memory/CHANGELOG.md`

    unsupported unary callable preflight, and mixed CPU/Vulkan lazy `min`;
    bounded-container focused `advanced-collections-module` `pass=1287 fail=0`
    with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`; targeted
    `git diff --check`.

- TENSOR-100F Tensor `stats/normal-quantile` CPU and Vulkan `Float32`
  `stats/normal-cdf` checkpoint:
  - CPU Tensor `stats/normal-quantile` now applies elementwise through the
    checked Boost.Math wrapper. `Float64` and `Float32` tensors preserve float
    dtype, `BigFloat` preserves `BigFloat`, `BigInteger` inputs convert through
    the Float64 probability path and therefore fail for non-empty integer
    probabilities outside `0 < p < 1`, and `BigComplex` remains unsupported.
    CPU Tensor quantile fails the whole operation on the first invalid
    probability instead of producing sentinel values.
  - Superseded for CUDA by the later CUDA probability-status checkpoint above
    and for Vulkan Float32 by the later Vulkan quantile checkpoint above:
    CUDA `stats/normal-quantile` is now shipped for eligible dense row-major
    `Float64`/`Float32` tensors, and Vulkan `Float32`
    `stats/normal-quantile` is shipped through a separate status-bearing
    helper. Vulkan Float64 quantile remains fail-closed pending a double
    inverse-CDF/status policy.
  - Vulkan dense row-major `Float32` `stats/normal-cdf` now routes through the
    existing Vulkan unary helper as fixed op id `19`, matching the CUDA
    distribution opcode. The shader uses a same-dtype Float32 erf-based
    approximation and preserves Vulkan placement for public `map` and direct
    Tensor unary math. Vulkan `Float64` `stats/normal-cdf` remains fail-closed
    because the current Vulkan 1.0 GLSL path still lacks double `erf`/`erfc`
    and double transcendental support.
  - CUDA construction-time fail-closed map diagnostics are now asserted from
    the C test harness with `test_error_contains` for the affected cases,
    avoiding the known Lisp `handle` stack-overflow path around immediate
    CUDA map construction errors.
  - Validation passed: `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_unary_f32.comp -o
    /tmp/omni_tensor_vulkan_map_unary_f32.spv`; `spirv-val --target-env
    vulkan1.0 /tmp/omni_tensor_vulkan_map_unary_f32.spv`;
    `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
    CPU/CUDA/Vulkan smokes; host focused `advanced-collections-module`
    `pass=1296 fail=0`; bounded-container focused
    `advanced-collections-module` `pass=1283 fail=0` with
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`; primitive docs
    parity; Stage 3 source parity; targeted `git diff --check`.

- TENSOR-100F Tensor `stats/normal-cdf` CUDA scientific opcode checkpoint:
  - CPU Tensor `stats/normal-cdf` now applies elementwise. `Float64` and
    `Float32` tensors preserve float dtype, `BigInteger` tensors return
    `Float64`, `BigFloat` tensors preserve `BigFloat`, lazy CPU Tensor sources
    realize through the shared Tensor unary math path, and `BigComplex` tensors
    fail closed because no complex distribution Tensor contract is shipped.
  - CUDA scientific unary map/direct math now recognizes fixed op id `19` for
    `stats/normal-cdf`, extending the generated CUDA C/libdevice PTX family
    from `5..18` to `5..19`.
  - Public `map stats/normal-cdf`, direct Tensor `stats/normal-cdf`, lazy CUDA
    map realization, and destination-form `realize` now preserve CUDA placement
    for eligible dense row-major `Float64`/`Float32` CUDA tensors when
    `scientific-map-float64` / `scientific-map-float32` are available.
  - Direct CUDA `map` now rejects unsupported CUDA callables and mixed
    CPU/CUDA operands before returning a lazy Tensor expression, preserving the
    existing payload diagnostics and no-hidden-materialization contract.
  - Preserved boundary: `stats/normal-cdf` is a fixed recognized opcode, not
    arbitrary GPU callable support. Vulkan remains unchanged and fail-closed
    for `stats/normal-cdf`. Superseded for CUDA by the later probability-status
    checkpoint above: eligible CUDA Tensor `stats/normal-quantile` is no longer
    scalar-only, while Vulkan quantile still needs a Tensor probability-domain
    and GPU error/status contract.
  - Helper hardening: restored the runtime CUDA Driver API resolver in
    `csrc/tensor_cuda_helpers.c`, so direct `c3c build` links the helper object
    and CUDA map modules load through `libcuda` symbols instead of depending on
    stale helper archive state.
  - Validation passed: `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75
    csrc/tensor_cuda_scientific_unary.cu -o
    /tmp/omni_tensor_cuda_scientific_unary.ptx`;
    `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75
    /tmp/omni_tensor_cuda_scientific_unary.ptx -o
    /tmp/omni_tensor_cuda_scientific_unary.cubin`; `cc -O2 -fPIC
    -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o
    /tmp/tensor_cuda_helpers.o`; `./scripts/build_omni_chelpers.sh`;
    `c3c build --obj-out obj`; direct CPU/CUDA/Vulkan-boundary smokes; host
    focused `advanced-collections-module` `pass=1282 fail=0`;
    bounded-container focused `advanced-collections-module` `pass=1269 fail=0`
    with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`; primitive
    docs parity; Stage 3 source parity; targeted `git diff --check`.

- TENSOR-100F Tensor `math/erf` / `math/erfc` CUDA scientific opcode checkpoint:
  - CPU Tensor `math/erf` and `math/erfc` now apply elementwise. `Float64` and
    `Float32` tensors preserve float dtype, `BigInteger` tensors return
    `Float64`, `BigFloat` tensors preserve `BigFloat`, and `BigComplex` tensors
    fail closed because no complex error-function Tensor contract is shipped.
  - CUDA scientific unary map/direct math now recognizes fixed op ids `17` and
    `18` for `math/erf` and `math/erfc`, extending the generated CUDA
    C/libdevice PTX family from `5..16` to `5..18`.
  - Public `map math/erf`, `map math/erfc`, direct Tensor `math/erf` /
    `math/erfc`, lazy CUDA map realization, and destination-form `realize` now
    preserve CUDA placement for eligible dense row-major `Float64`/`Float32`
    CUDA tensors when `scientific-map-float64` / `scientific-map-float32` are
    available.
  - Preserved boundary: `math/erf` and `math/erfc` are fixed recognized opcodes,
    not arbitrary GPU callable support. Vulkan remains unchanged and fail-closed
    for these callables. Tensor rounding remains a dtype-changing
    `Tensor BigInteger` contract and must not be added to CUDA same-dtype map
    opcodes without a matching result-dtype path.
  - Helper hardening: restored the runtime CUDA library resolver in
    `csrc/tensor_cuda_helpers.c`, so direct `c3c build` links the C helper object
    instead of relying on stale helper archive state.
  - Validation passed: `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75
    csrc/tensor_cuda_scientific_unary.cu -o
    /tmp/omni_tensor_cuda_scientific_unary.ptx`;
    `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75
    /tmp/omni_tensor_cuda_scientific_unary.ptx -o
    /tmp/omni_tensor_cuda_scientific_unary.cubin`; `cc -O2 -fPIC
    -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o
    /tmp/tensor_cuda_helpers.o`; `./scripts/build_omni_chelpers.sh`;
    `c3c build --obj-out obj`; direct CPU/CUDA/Vulkan-boundary smokes; host
    focused `advanced-collections-module` `pass=1271 fail=0`;
    bounded-container focused `advanced-collections-module` `pass=1258 fail=0`
    with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`; primitive docs
    parity; Stage 3 source parity; targeted `git diff --check`.

- TENSOR-100F CUDA map mixed-operand diagnostic checkpoint:
  - Mixed CPU/CUDA Tensor operands in CUDA `map` now fail with the explicit
    `map: CUDA operands must remain CUDA-placed` diagnostic instead of falling
    through to a generic dense-kernel unsupported message.
  - Mixed CUDA Tensor dtypes now report `tensor/dtype-mismatch` from the CUDA map
    helper path, preserving the existing direct-map dtype contract for both direct
    and lazy-realized CUDA map expressions.
  - CUDA and Vulkan map probes now check whether an expression tree actually
    touches the probed device before calling `tensor_expr_resolve_concrete_any_device`.
    This prevents unsupported mixed CPU/CUDA lazy operands from being CPU-realized
    before the CUDA placement diagnostic is raised.
  - This is a fail-closed policy hardening slice only: it preserves the
    no-hidden-transfer rule and does not add mixed-device execution support.
  - Validation passed: `c3c build --obj-out obj`; host focused
    `advanced-collections-module` `pass=1261 fail=0`; bounded-container focused
    `advanced-collections-module` `pass=1248 fail=0` using
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.

- TENSOR-100F foreign CUDA payload clone checkpoint:
  - Valid foreign CUDA concrete Tensor payloads now clone by allocating fresh
    Omni-owned CUDA storage and copying the source device bytes through the CUDA
    device-to-device helper.
  - The cloned payload always installs `tensor_cuda_device_finalizer`; the source
    payload's foreign finalizer remains responsible only for the source handle.
  - Fake, stale, malformed, or otherwise invalid CUDA handles remain
    fail-closed: the fake CUDA storage regression still rejects cloning and only
    exercises destruction through the source finalizer.
  - During implementation review, the Vulkan retain/clone branch was corrected
    to keep `tensor_vulkan_device_finalizer`; CUDA clone finalizer installation is
    now explicit to the CUDA branch only.
  - Validation passed: `c3c build --obj-out obj`; bounded-container
    `memory-lifetime-smoke` `pass=229 fail=0` using
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`; host focused
    `advanced-collections-module` `pass=1258 fail=0`.

- TENSOR-100F CUDA scientific unary map checkpoint:
  - CUDA now has generated CUDA C/libdevice PTX kernels for dense row-major
    `Float64` and `Float32` scientific unary op ids `5..16`: `sin`, `cos`,
    `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, and
    `log10`.
  - Public direct `map`, lazy CUDA map realization, CUDA destination-form
    `realize` from lazy scientific maps, and direct Tensor scientific
    primitives preserve CUDA placement for eligible real CUDA tensors without
    hidden CPU materialization.
  - The generated PTX is loaded as a separate optional CUDA Driver API module,
    so existing CUDA binary/arithmetic map availability does not depend on
    scientific module loading. `tensor-backends` reports separate
    `scientific-map-float64` and `scientific-map-float32` capability bits.
  - Preserved fail-closed boundaries: unsupported layouts, mixed CPU/CUDA
    operands, mixed CUDA dtypes/devices, and unsupported callables remain outside
    this slice.
  - Negative constraint update: the prior "do not fake scientific CUDA math
    with partial raw PTX approximations" constraint remains active for any
    future scientific broadening. This checkpoint satisfies it by generating PTX
    from CUDA C and libdevice, not by hand-maintaining transcendental PTX.
  - Validation passed: `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75
    csrc/tensor_cuda_scientific_unary.cu -o
    /tmp/omni_tensor_cuda_scientific_unary.ptx`;
    `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75
    /tmp/omni_tensor_cuda_scientific_unary.ptx -o
    /tmp/omni_tensor_cuda_scientific_unary.cubin`; `cc -O2 -fPIC
    -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o
    /tmp/tensor_cuda_helpers.o`; `./scripts/build_omni_chelpers.sh`;
    `c3c build --obj-out obj`; CUDA-gated runtime smokes for capability
    reporting, Float64 `map sin`, Float32 direct `log10`, direct `sin`,
    Float64 `acos`/`log10`, and lazy destination `realize` returned true; host
    focused `advanced-collections-module` `pass=1258 fail=0`;
    bounded-container focused `advanced-collections-module` `pass=1245 fail=0`
    using `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`; primitive docs
    parity; Stage 3 source parity; targeted `git diff --check`.
  - Validation-image note: rebuilding `omni-validation:2026-03-10` still leaves
    `/opt/c3/c3c` as an x86-64 binary inside an arm64 image because
    `docker/validation.Dockerfile` downloads `c3-linux.tar.gz`. Use the host
    toolchain mount on this arm64 host until the Dockerfile selects a native C3
    compiler artifact or builds C3 from source.

## 2026-04-17

- TENSOR-100F CUDA arithmetic unary map checkpoint:
  - CUDA now has separate embedded-PTX CUDA Driver API unary map kernels and C
    ABI helpers for dense row-major `Float64` and `Float32` tensors.
  - Supported unary op ids are `0..4`: `abs`, unary `-`, `sqrt`, identity
    (`map +`, `real-part`, and `conjugate` on real tensors), and zero-fill
    (`imag-part` on real tensors).
  - Public direct `map`, lazy CUDA map realization, CUDA destination-form
    `realize` from lazy unary maps, and direct Tensor primitives `abs`, unary
    `-`, `sqrt`, `real-part`, `imag-part`, and `conjugate` now preserve CUDA
    placement for eligible dense row-major `Float64`/`Float32` tensors without
    hidden CPU materialization.
  - Superseded boundary note: scientific CUDA unary op ids `5..16` and valid
    foreign CUDA payload clone semantics are covered by later checkpoints above.
    Unsupported callables, unsupported layouts, mixed CPU/CUDA operands, and
    mixed CUDA dtypes/devices remain explicit fail-closed residuals.
  - Negative constraint: do not implement CUDA scientific unary by composing
    partial raw PTX approximations such as approximate trig/log fragments.
    Prefer a deliberate libdevice/NVRTC/fatbin strategy, or explicitly
    documented approximation kernels with tolerances, before wiring op ids
    `5..16`.
  - Validation passed: `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c
    csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`;
    `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; CUDA
    backend probe returned `[true true true nil]`; direct CUDA-gated smokes for
    `map abs`, direct `sqrt`, Float32 `map sqrt`, `imag-part`, CUDA destination
    realization from lazy unary `map sqrt`, and Float64/Float32 scientific
    fail-closed behavior all returned `true`; host focused
    `advanced-collections-module` `pass=1256 fail=0`; bounded-container
    focused `advanced-collections-module` `pass=1243 fail=0`; primitive docs
    parity; Stage 3 source parity; targeted `git diff --check`.
    `ptxas` was not installed in the current environment, so no standalone PTX
    assembler check was run beyond runtime CUDA execution.

- TENSOR-100F CUDA elementwise binary map checkpoint:
  - CUDA now has embedded-PTX CUDA Driver API elementwise binary map kernels
    for dense row-major `Float64` and `Float32` tensors.
  - Public `map`, lazy map realization, and CUDA destination-form `realize`
    now route eligible CUDA operands through the CUDA map helper without
    hidden CPU materialization. Supported operations are `+`, `-`, `*`, `/`,
    `min`, and `max`; supported operand shapes are Tensor/scalar,
    scalar/Tensor, exact-shape Tensor/Tensor, and right-aligned singleton-axis
    Tensor/Tensor broadcasting.
  - `tensor-backends` reports `elementwise-map-float64` and
    `elementwise-map-float32` capability keys for CPU, Vulkan, and CUDA.
  - CUDA-owned concrete Tensor clone/copy now copies Omni-owned CUDA payloads
    through the existing device-to-device helper so direct CUDA map results can
    cross the existing return/copy boundary. Valid foreign CUDA clone semantics
    are covered by the later checkpoint above.
  - Preserved fail-closed boundaries now narrowed by later checkpoints above:
    unsupported layouts, mixed CPU/CUDA operands, mixed CUDA dtypes/devices, and
    unsupported callables remain outside this slice.
  - Validation passed: `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c
    csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`;
    `c3c build --obj-out obj`; direct CUDA-gated smokes for capability
    reporting, direct `Float64` and `Float32` scalar maps, binary
    `-`/`*`/`/`/`min`/`max`/`+` Tensor maps, right-aligned broadcast maps,
    broadcast CUDA destination realization, lazy map CUDA destination
    realization, unsupported callable rejection, mixed CPU operand rejection,
    and incompatible-broadcast rejection; host focused
    `advanced-collections-module` `pass=1240 fail=0`; bounded-container
    `memory-lifetime-smoke` `pass=228 fail=0`; bounded-container focused
    `advanced-collections-module` `pass=1224 fail=0`; primitive docs parity;
    Stage 3 source parity.
    `c3c build --sanitize=address --obj-out obj-asan` could not run because
    this `c3c` build reports address sanitizer as unavailable on the current
    platform/toolchain.

- TENSOR-100F CUDA rank-1 dot checkpoint:
  - CUDA-placed contiguous matching `Float64` or matching `Float32`
    rank-1/rank-1 single-axis contractions now return CUDA-placed scalar
    Tensor outputs through the existing explicit-device `contract` surface.
  - The implementation reuses the existing cuBLAS GEMV helper by treating the
    left vector as a one-row row-major matrix; no new CUDA helper ABI was
    required.
  - Zero-size rank-1 dots use the existing CUDA additive-identity fill path.
    Nonzero rank-1 dots still require the matching cuBLAS GEMV helper and fail
    unavailable when cuBLAS is forced unavailable for tests.
  - Preserved fail-closed boundaries: CUDA `map`, CUDA rank-1/rank-1
    zero-axis outer product, multi-axis CUDA contract, unsupported
    layouts/ranks, mixed devices, and mixed CUDA dtypes remain outside this
    slice.
  - Validation passed: `c3c build --obj-out obj`; direct CUDA-gated smokes for
    `Float64` rank-1 dot, explicit-axis rank-1 dot, `Float32` rank-1 dot with
    scalar `Float32` extraction, zero-size rank-1 dot identity, `Float64`
    rank-1 dot destination realization into a rank-0 CUDA tensor, and
    `Float32` rank-1 dot destination realization into a rank-0 CUDA tensor;
    bounded-container focused `advanced-collections-module` `pass=1217
    fail=0`; primitive docs parity; Stage 3 source parity; targeted
    `git diff --check`.

- Superseded CUDA `map` blocker update:
  - The earlier helper-layer blocker is superseded by the CUDA elementwise
    binary map checkpoint above. Do not keep treating dense row-major
    `Float64`/`Float32` binary CUDA map for scalar, exact-shape, and
    right-aligned broadcast operands as blocked.
  - Superseded by the later arithmetic/scientific unary and foreign CUDA clone
    checkpoints above: remaining CUDA `map` work is now unsupported layouts,
    mixed devices/dtypes, mixed CPU/CUDA operands, and unsupported callables.

- TENSOR-100F CUDA zero-size contract identity/fill checkpoint:
  - CUDA-placed dense row-major matching `Float64` or matching `Float32`
    rank-2/rank-2, rank-2/rank-1, and rank-1/rank-2 single-axis contractions
    now mirror CPU/Vulkan zero-size contract semantics for the CUDA-supported
    layout family.
  - Zero free dimensions return CUDA-placed zero-length Tensor outputs with
    the exact computed result shape and explicit CPU inspection through
    `to-device 'cpu`.
  - Zero contracted dimensions with non-empty outputs allocate CUDA result
    storage and fill the output with the dtype-preserving additive identity
    through the existing CUDA fill helpers instead of calling cuBLAS. This
    keeps zero-size identity independent of cuBLAS availability while nonzero
    CUDA contractions still require the matching cuBLAS GEMM/GEMV helper.
  - Preserved fail-closed boundaries: multi-axis CUDA contract, unsupported
    layouts/ranks, CUDA `map`, mixed devices, and mixed CUDA dtypes remain
    outside this slice. CUDA rank-1/rank-1 dot is covered by the later
    checkpoint above.
  - Validation passed: `c3c build --obj-out obj`; direct CUDA-gated smokes for
    `Float64` zero-contracted rank-2 identity fill, `Float32`
    zero-contracted rank-2 identity fill with scalar `Float32` extraction,
    zero-free rank-2 output preservation, rank-2/rank-1 identity fill, and
    rank-1/rank-2 identity fill; bounded-container focused
    `advanced-collections-module` `pass=1210 fail=0`; primitive docs parity;
    Stage 3 source parity; targeted `git diff --check`. Host focused
    `advanced-collections-module` was attempted but stayed idle past ten
    minutes and was terminated as inconclusive.

- Scalar `Float32` runtime value checkpoint:
  - Added a dedicated scalar `Float32` runtime value tag/payload/constructor,
    type-id cache, `Number` parent wiring, `type-of`/`is?` support, and
    stdlib `float32?` predicate.
  - `Float32`, `(Float x 32)`, and `(Float x "32")` now construct native
    32-bit float scalar values from representable numeric and numeric-string
    inputs. Out-of-range, non-finite, or non-numeric inputs fail closed with
    `type/arg-mismatch`.
  - Wired scalar `Float32` through region-owned copy/promotion/env-copy paths,
    boundary graph leaf auditing, printing/string/format conversion, JSON/CSV
    emission, schema validation, tuple persistence, AOT/native literal
    lowering, numeric comparison/equality/hash/order helpers, basic arithmetic,
    min/max/abs, FFI `Double` argument widening, and sleep/random-int numeric
    consumers.
  - Tensor `Float32` `ref`, `Array`, `List`, `Iterator`, contract scalar
    extraction, CPU/copyback extraction, and `matrix/trace` now return scalar
    `Float32` instead of widening to `Float64`. Matrix determinant and norm
    keep their documented `Float64` scalar return contract.
  - Validation passed: `c3c build --obj-out obj`; direct runtime smokes for
    constructor/type/predicate, arithmetic widening policy, closure capture,
    schema, format/sort-by numeric comparator, Tensor `Float32` ref/iterator,
    structural matrix extraction, contract extraction, and `matrix/trace`;
    focused advanced core semantics `pass=71 fail=0`;
    arithmetic-comparison `pass=47 fail=0`; bounded-container focused
    `advanced-collections-module` `pass=1205 fail=0`; primitive docs parity;
    Stage 3 source parity. Local ASAN compile was attempted but the local `c3c`
    rejected sanitizer mode with "Address sanitizer is only supported on Linux,
    FreeBSD, NetBSD, Darwin and Windows."

- TENSOR-100F CUDA `Float32` placement and contract checkpoint:
  - Extended the explicit CUDA storage boundary to concrete dense row-major
    `Float32` Tensor storage: `to-device 'cuda`, `to-device 'cpu`,
    destination-form `realize`, scalar destination fills, and supported lazy
    CUDA contract destination writes now work for `Float32` alongside
    `Float64`.
  - Added runtime-loaded CUDA `Float32` fill support and cuBLAS `sgemm` /
    `sgemv` contract helpers for matching CUDA-placed `Float32` rank-2/rank-2,
    rank-2/rank-1, and rank-1/rank-2 single-axis contractions across the same
    row-major layout family already supported by the `Float64` cuBLAS path.
  - `tensor-backends` now reports CUDA and cuBLAS `float64` / `float32`
    capability bits. cuBLAS `Float64` availability remains tied to the
    existing `dgemm` / `dgemv` symbols; `Float32` availability is reported
    separately from `sgemm` / `sgemv` and does not weaken the older contract.
  - Preserved explicit-device semantics: ordinary CPU `map`, `contract`,
    one-argument `realize`, CPU-destination `realize`, and `matrix/*` do not
    silently transfer between CPU and CUDA. CUDA `map` and mixed CUDA dtypes
    remain fail-closed/deferred; zero-size CUDA contract identity/fill is
    superseded by the checkpoint above. The
    scalar `Float32` runtime value boundary is superseded by the scalar
    checkpoint above.
  - Validation passed: `./scripts/build_omni_chelpers.sh`, `c3c build
    --obj-out obj`, host focused `advanced-collections-module` `pass=1218
    fail=0`, bounded-container focused `advanced-collections-module`
    `pass=1205 fail=0`, primitive docs parity, Stage 3 source parity, and
    targeted `git diff --check`.

- TENSOR-100F Vulkan `Float32` scientific unary checkpoint:
  - Extended the dedicated Vulkan `Float32` unary shader/helper ABI with
    opcodes for `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`,
    `tanh`, `exp`, `log`, and `log10`.
  - Public `(map <unary> <vulkan-float32-tensor>)` and direct Tensor unary
    math route through the `Float32` helper, preserve `Float32` dtype and
    Vulkan placement, and require explicit `(to-device ... 'cpu)` for CPU
    inspection.
  - Vulkan `Float64` scientific unary remains fail-closed; `glslangValidator`
    rejected double transcendental builtins under the current Vulkan 1.0
    validation path, and no hidden `Float32` downcast was introduced.
  - Validation passed: `glslangValidator`/`spirv-val` for the unary
    shaders, regenerated the `Float32` SPIR-V C embed, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct Vulkan smokes for `sin`/`cos`/`exp`/`log`
    and `log10`, direct Vulkan `Float64` `map sin` fail-closed smoke, and host
    focused `advanced-collections-module` `pass=1198 fail=0`, plus
    bounded-container focused `advanced-collections-module` `pass=1185 fail=0`.

- TENSOR-100F CPU `Float32` matrix factor/SVD checkpoint:
  - Added native CPU `Float32` matrix helpers/workspaces for LU, solve,
    inverse, QR, Cholesky, and SVD/Jacobi singular-value paths in
    `src/lisp/prim_tensor_matrix.c3`.
  - Public CPU `Tensor Float32` matrix surfaces now support
    `matrix/determinant`, `matrix/lu`, `matrix/solve`, `matrix/inverse`,
    `matrix/cholesky`, `matrix/qr`, `matrix/singular-values`, `matrix/svd`,
    and `matrix/norm` `'spectral` / `'nuclear`.
  - Tensor outputs preserve `Float32`; determinant and norm scalars preserve
    the existing public `Float64` scalar return behavior. The CPU `Float32`
    paths do not call the existing Float64 `dgesv`/`dgetrf`/`dgeqrf`/`dpotrf`
    / `dgesvd` LAPACK helpers and do not silently widen Tensor outputs through
    Float64 workspaces.
  - Replaced CPU `Float32` fail-closed tests with positive value/dtype tests
    and no-hidden-Float64-LAPACK counter guards for factor/solve/SVD paths.
  - Supersedes earlier notes that listed CPU `Float32` factor/SVD public
    contracts as deferred. The later scalar and CUDA checkpoints also supersede
    the earlier scalar/CUDA deferred boundaries. Remaining deferred adjacent
    boundaries are fixed-width complex, stride/view-backed Vulkan layouts, and
    measured Vulkan SVD performance work beyond the validated correctness path.
  - Validation passed: `c3c build --obj-out obj`, direct CPU `Float32` smokes
    for determinant/solve/inverse/LU/QR/Cholesky/singular-values/SVD/norm,
    host focused `advanced-collections-module` `pass=1195 fail=0`, and
    bounded-container focused `advanced-collections-module` `pass=1182
    fail=0`.

- TENSOR-100F Vulkan `Float32` large-dense SVD robustness checkpoint:
  - Fixed the previously failing dense all-ones / rank-deficient `65x65`
    Vulkan `Float32` SVD path with scale-aware eigenvalue tolerance and
    orthonormal completion in the `Float32` singular-values/SVD shaders.
  - The repair preserves Vulkan placement and `Float32` Tensor output dtype
    for Tensor results, keeps the public norm scalar return behavior, and does
    not add hidden CPU/LAPACK fallback or hidden `Float64` widening.
  - Added availability-gated regressions for `65x65` zero, identity/diagonal,
    and all-ones/rank-deficient `Float32` singular-values/SVD paths, plus
    all-ones reconstruction spot-checks, spectral/nuclear norm coverage, and
    no-`LAPACKE_dgesvd` counter checks.
  - Supersedes the earlier negative validation note that dense all-ones
    `65x65` Vulkan `Float32` SVD returned `tensor/backend-execution-failed`.
    Do not resume staged/tiled `Float32` SVD as a correctness blocker unless
    new validation regresses this result.
  - Superseded by the CPU `Float32` matrix checkpoint above: remaining
    deferred `Float32` boundaries are scalar `Float32`, CUDA `Float32`
    placement/contract, fixed-width complex, and stride/view-backed Vulkan
    layouts.
  - Validation passed: `glslangValidator` and `spirv-val` for
    `csrc/tensor_vulkan_singular_values_f32.comp` and
    `csrc/tensor_vulkan_svd_f32.comp`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct Vulkan `Float32` smokes
    (`(65 64.9999694824219 0.0)`,
    `(vulkan vulkan vulkan 64.9999694824219 0.0)`,
    `(64.9999694824219 65.1197662353516)`, and `(65 1.0 1.0)`), host focused
    `advanced-collections-module` `pass=1177 fail=0`, and bounded-container
    focused `advanced-collections-module` `pass=1164 fail=0`.

- TENSOR-100F Vulkan `Float32` staged parallel solve checkpoint:
  - Added dedicated `Float32` ports of the staged Vulkan solve shader family
    (`solve_parallel_init`, pivot scan/reduce/commit, row swap, factor,
    eliminate, and backsolve), generated SPIR-V C embeds, and helper/build
    wiring.
  - Reworked the staged solve C helper into an explicit dtype-aware dispatch
    ABI and added `omni_tensor_backend_vulkan_solve_parallel_f32` without
    hidden `Float64` widening, hidden CPU transfer, or LAPACK fallback.
  - Public `matrix/solve` now routes dense row-major Vulkan `Float32` systems
    with `n >= 65` through the staged parallel helper and keeps `2x2` / `9x9`
    systems on the serial `Float32` helper.
  - Added dtype-specific diagnostic counters for staged/serial `Float32` and
    `Float64` solve paths. Focused tests prove `Float32` staged solves move
    only the `Float32` staged counters, keep `Float64` and LAPACK counters
    unchanged, preserve Vulkan placement and `Float32` dtype, support matrix
    RHS shape/rank, reject mixed dtype, and return `tensor/singular-matrix`
    for a staged `65x65` singular system.
  - Local timing logs are recorded under
    `build/vulkan_solve_f32_threshold_20260417_*`. On this stack, `65x65`
    staged and forced-serial `Float32` identity/dense runs were tied within
    measurement noise (`0.92s/0.89s` staged vs `0.94s/0.89s` forced serial for
    the higher-iteration identity/dense probes), so `65` is documented as a
    parity threshold, not a decisive speedup claim.
  - Superseded by the later large-dense SVD robustness and CPU `Float32`
    matrix checkpoints: remaining deferred `Float32` boundaries are scalar
    `Float32`, CUDA `Float32` placement, fixed-width complex, and
    stride/view-backed Vulkan layouts.
  - Validation passed: `glslangValidator` and `spirv-val` for the nine
    `Float32` solve shaders, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct staged Vulkan `Float32` solve smokes
    (`(vulkan "Float32" 1.0)`, dense value `1.00000011920929`, and
    `tensor/singular-matrix`), local Float32 threshold timing probes, and host
    focused `advanced-collections-module` `pass=1166 fail=0`.

- TENSOR-100F Vulkan `Float32` serial factor/solve checkpoint:
  - Added dedicated Vulkan `Float32` serial matrix factor/solve shader/helper
    paths for `matrix/determinant`, `matrix/lu`, `matrix/solve`,
    `matrix/inverse`, `matrix/cholesky`, and `matrix/qr`.
  - Updated public Vulkan routing so eligible dense row-major `Float32`
    operands use native `_f32` kernels. Tensor results preserve Vulkan
    placement and `Float32` dtype; determinant and LU metadata preserve the
    existing public scalar/host metadata contracts.
  - Kept CPU `Float32` factor/solve routines fail-closed and kept Vulkan
    `Float32` solve on the serial helper. This slice does not silently widen
    through `Float64`, call LAPACK, or copy Vulkan operands to CPU.
  - Added availability-gated Vulkan regressions for values, dtype/device
    preservation, lazy operands, singular/rank-deficient/non-SPD diagnostics,
    CPU `Float32` fail-closed behavior, and no-LAPACK counter preservation
    across `matrix/determinant`, `matrix/lu`, `matrix/solve`,
    `matrix/inverse`, `matrix/cholesky`, and `matrix/qr`.
  - Superseded by the later staged parallel solve, large-dense SVD robustness,
    and CPU `Float32` matrix checkpoints: remaining deferred `Float32`
    boundaries are CUDA `Float32` placement, scalar `Float32` values,
    fixed-width complex, and stride/view-backed Vulkan layouts.
  - Validation passed: `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct Vulkan `Float32` smokes (`-2.0`,
    `0.199999988079071`, `"Float32"`, `0.5`, `2.0`, and `1.0`), host focused
    `advanced-collections-module` `pass=1156 fail=0`, and bounded-container
    focused `advanced-collections-module` `pass=1143 fail=0`.

- TENSOR-100F Vulkan `Float32` SVD-backed checkpoint:
  - Added dedicated Vulkan `Float32` singular-value shader/helper paths for
    `matrix/singular-values`, direct `matrix/svd`, and `matrix/norm`
    `'spectral` / `'nuclear`: `csrc/tensor_vulkan_singular_values_f32.comp`,
    `csrc/tensor_vulkan_svd_f32.comp`, generated `_spv.c` sources, C helper
    ABI exports, C3 externs, and build manifest wiring.
  - Updated public Vulkan routing so dense row-major `Float32` tensors use the
    native `_f32` singular-value and SVD helpers. `matrix/singular-values` and
    `matrix/svd` preserve `Float32` dtype and Vulkan placement for Tensor
    outputs; `matrix/norm` keeps the public `Float64` scalar return while
    computing the singular values in `Float32` storage.
  - Kept CPU `Float32` `matrix/singular-values`, CPU `Float32` `matrix/svd`,
    and CPU `Float32` spectral/nuclear `matrix/norm` fail-closed until that
    public CPU contract is explicitly chosen. This slice does not silently
    widen CPU `Float32` SVD workspaces or narrow results.
  - Added availability-gated Vulkan regressions for `Float32`
    spectral/nuclear norms, lazy operands, singular-values value/dtype/device
    preservation, empty and large-`k` zero matrices, direct SVD output
    value/dtype/device preservation, no-LAPACK routing, and an
    over-previous-max-k direct SVD zero-matrix path. Added explicit CPU
    `Float32` fail-closed tests for `matrix/singular-values` and `matrix/svd`.
  - Renamed the shared Vulkan two-output and three-output dispatch helpers
    from misleading `_f64` names to dtype-neutral byte-sized names; the
    `Float32` SVD paths do not route through hidden Float64 widening.
  - Superseded by the later large-dense SVD robustness checkpoint: the dense
    all-ones / rank-deficient `65x65` failure is fixed by scale-aware
    eigenvalue tolerance plus orthonormal completion.
  - Superseded by the later serial factor/solve, staged parallel solve,
    large-dense SVD robustness, and CPU `Float32` matrix checkpoints:
    remaining deferred `Float32` boundaries are CUDA `Float32` placement,
    scalar `Float32` values, fixed-width complex, and stride/view-backed
    Vulkan layouts.
  - Validation passed: `glslangValidator` and `spirv-val` for
    `csrc/tensor_vulkan_singular_values_f32.comp` and
    `csrc/tensor_vulkan_svd_f32.comp`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct Vulkan `Float32` smokes (`3.0`, `2.0`,
    and `"Float32"`), host focused `advanced-collections-module`
    `pass=1121 fail=0`, bounded-container focused
    `advanced-collections-module` `pass=1108 fail=0`, primitive docs parity,
    Stage 3 source parity, stale-current docs scan, and targeted
    `git diff --check`.

- TENSOR-100F Vulkan `Float32` direct reducer checkpoint:
  - Added dedicated Vulkan `Float32` direct reducer shader/helper paths for
    `matrix/norm` default/`'frobenius`, `'one`, `'infinity`, and `'max`, plus
    `matrix/rank`: `csrc/tensor_vulkan_norm_f32.comp`,
    `csrc/tensor_vulkan_rank_f32.comp`, generated `_spv.c` sources, C helper
    ABI exports, C3 externs, and build manifest wiring.
  - Updated CPU/runtime matrix reducers so native `Tensor Float32` has public
    oracles for `matrix/rank` and direct `matrix/norm` selectors without
    routing through `Float64` LAPACK/SVD helpers. Rank uses the pure elimination
    path with `Float32` values widened only into the local numeric workspace;
    direct norms read `float*` storage and return the existing public
    `Float64` scalar result shape.
  - Updated Vulkan public routing to accept matching dense row-major `Float32`
    operands for direct reducers, preserve `Float64` behavior, read back only
    scalar payloads, and avoid hidden `Float64` downcast, hidden widening, or
    CPU fallback.
  - Later checkpoints supersede this boundary: `Float32` SVD-backed norm
    selectors, `matrix/singular-values`, `matrix/svd`, and serial factor/solve
    kernels now have Vulkan paths. Staged parallel `Float32` solve/performance
    parity remains deferred.
  - Added CPU and availability-gated Vulkan regressions for eager and lazy
    `Float32` rank/norm operands, zero-size shapes, tolerance behavior,
    vector-shape rejection, no-LAPACK routing, and fail-closed `Float32`
    spectral/nuclear selectors.
  - Superseded by later deferred-boundary wording: SVD-backed reducers,
    singular-value/SVD outputs, serial factor/solve kernels, staged parallel
    solve, and large-dense SVD robustness now have Vulkan `Float32` paths.
    CUDA `Float32` placement, scalar `Float32` values, CPU `Float32`
    factor/SVD contracts, fixed-width complex, and stride/view-backed Vulkan
    layouts remain deferred.
  - Validation passed: `glslangValidator` and `spirv-val` for
    `csrc/tensor_vulkan_norm_f32.comp` and
    `csrc/tensor_vulkan_rank_f32.comp`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct CPU/Vulkan `Float32` smokes (`2`,
    `5.0`, `tensor/backend-unsupported` for SVD-backed norm selectors), host
    focused `advanced-collections-module` `pass=1098 fail=0`,
    bounded-container focused `advanced-collections-module` `pass=1085
    fail=0`, primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check`.

- TENSOR-100F Vulkan `Float32` structural matrix checkpoint:
  - Added dedicated Vulkan `Float32` structural matrix shader/helper paths for
    `matrix/transpose`, `matrix/diagonal`, `matrix/diagonal-matrix`, and
    `matrix/trace`: `csrc/tensor_vulkan_transpose_f32.comp`,
    `csrc/tensor_vulkan_diagonal_f32.comp`,
    `csrc/tensor_vulkan_diagonal_matrix_f32.comp`,
    `csrc/tensor_vulkan_trace_f32.comp`, generated `_spv.c` sources, C helper
    ABI exports, and C3 externs.
  - Updated public Vulkan matrix routing to accept matching dense row-major
    `Float32` operands, preserve `Float32` dtype for Tensor outputs, keep scalar
    trace readback as the existing public `Float64` scalar shape, and avoid
    hidden `Float64` downcast, hidden widening, or CPU fallback.
  - Added availability-gated regressions for eager and lazy Vulkan `Float32`
    structural operands across copyback, placement, dtype preservation,
    zero-size behavior, scalar trace readback, and `Float64` no-downcast
    preservation.
  - Superseded by later checkpoints above: Vulkan `Float32` structural matrix,
    reducer, SVD-backed, serial factor/solve, and staged parallel solve paths
    are active. Remaining deferred Vulkan `Float32` boundary is robust
    large-dense SVD execution, plus CUDA `Float32` placement and scalar
    `Float32` values.
  - Validation passed: `glslangValidator` and `spirv-val` for all four
    structural `Float32` shaders, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct Vulkan `Float32` structural smokes
    (`6.0`, `"Float32"`, `0.0`, `5.0`), host focused
    `advanced-collections-module` `pass=1059 fail=0`, bounded-container
    focused `advanced-collections-module` `pass=1046 fail=0`, primitive docs
