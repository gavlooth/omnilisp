# Agent Plan Index Part 02

Source: `.agents/PLAN.md`

## Current Validation Checkpoint

- 2026-04-18 Vulkan fixed-width complex singular-values/norm checkpoint:
  - CPU `Complex128` and `Complex64` tensors now provide
    `matrix/singular-values` through realification and duplicate-pair collapse,
    returning component-width real tensors. CPU spectral/nuclear
    `matrix/norm` selectors route through the same singular-value oracle.
  - Vulkan dense row-major zero-offset `Complex128`/`Complex64` tensors route
    `matrix/singular-values` and spectral/nuclear `matrix/norm` through native
    SPIR-V helpers behind `matrix-numerical-complex128` and
    `matrix-numerical-complex64`. Tensor singular-value results stay
    Vulkan-placed and component-width real; norm reads back only the public
    `Float64` scalar.
  - Validation passed so far: complex singular-value shaders compiled with
    `glslangValidator` and `spirv-val`; `./scripts/build_omni_chelpers.sh`;
    `c3c build --obj-out obj`; direct Vulkan Complex128/Complex64
    singular-value and norm smokes; host focused
    `advanced-collections-module` passed `1598/0`; bounded-container focused
    `advanced-collections-module` passed `1581/0`; primitive docs parity,
    Stage 3 source parity, and targeted diff hygiene passed.
  - Review follow-up: native helper validation now rejects fixed-complex
    shapes whose realified Jacobi iteration count would overflow shader
    `uint`; tests cover non-diagonal lazy, wide, zero-size, and guard paths.
  - Remaining operation families: full complex `matrix/svd` factor output,
    CUDA fixed-width complex singular-values/norm/SVD, complex eigen routines,
    and direct Vulkan general `matrix/eigenpairs` result-contract work.
  - Planning follow-up: those residuals are split into
    `TENSOR-100H-SVD-FACTORS`, `TENSOR-100H-CUDA-SVD-NORMS`, and
    `TENSOR-100H-COMPLEX-EIGEN` with the active closure plan at
    `docs/plans/fixed-width-complex-closure-plan-2026-04-18.md`.

- 2026-04-18 Vulkan fixed-width complex QR/Cholesky checkpoint:
  - CPU `Complex128` and `Complex64` tensors now provide public
    `matrix/qr` and `matrix/cholesky` oracle behavior. Complex QR uses the
    Hermitian inner product and preserves fixed-complex dtype for `q`/`r`.
    Complex Cholesky accepts Hermitian positive-definite inputs and returns
    lower factors with zero upper triangle.
  - Vulkan dense row-major zero-offset `Complex128`/`Complex64` tensors route
    `matrix/qr` and `matrix/cholesky` through native SPIR-V helpers behind
    `matrix-numerical-complex128` and `matrix-numerical-complex64`.
  - CPU QR/Cholesky tolerances now match the Vulkan fixed-complex thresholds
    for near-rank-deficient and near-Hermitian cases. Vulkan Cholesky
    diagnostics now use the symmetric/Hermitian positive-definite wording.
  - Validation passed so far: four complex QR/Cholesky shaders compiled with
    `glslangValidator` and `spirv-val`; `./scripts/build_omni_chelpers.sh`;
    `c3c build --obj-out obj`; direct CPU/Vulkan QR/Cholesky smokes; host
    focused `advanced-collections-module` passed `1570/0`.
  - Remaining operation families: complex singular-values/SVD,
    spectral/nuclear complex norm selectors, complex eigen routines, and CUDA
    fixed-width complex numerical matrix variants.

- 2026-04-18 Vulkan fixed-width complex rank/direct-norm checkpoint:
  - CPU `Complex128` and `Complex64` tensors now provide public `matrix/rank`
    and direct `matrix/norm` oracle behavior. Complex rank uses
    magnitude-based pivoting and tolerance. Direct norms use complex magnitude
    for default/`'frobenius`, `'one`, `'infinity`, and `'max`; complex
    spectral/nuclear selectors fail closed until complex singular-value/SVD
    support lands.
  - Vulkan dense row-major zero-offset `Complex128`/`Complex64` tensors route
    `matrix/rank` and direct `matrix/norm` through native SPIR-V reducers
    behind `matrix-numerical-complex128` and
    `matrix-numerical-complex64`. The public scalar results are read back
    without hidden CPU fallback or lowering through real tensors.
  - Validation passed: complex rank/norm shaders compiled with
    `glslangValidator` and `spirv-val`; `./scripts/build_omni_chelpers.sh`;
    `c3c build --obj-out obj`; direct CPU/Vulkan `--eval` smokes for
    Complex128 and Complex64 rank/norm plus spectral fail-closed behavior;
    host focused `advanced-collections-module` passed `1540/0`; bounded
    container focused `advanced-collections-module` passed `1523/0`.

- 2026-04-18 Vulkan fixed-width complex inverse checkpoint:
  - CPU `Complex128` and `Complex64` tensors now provide the public
    `matrix/inverse` oracle. Vulkan dense row-major zero-offset
    `Complex128`/`Complex64` tensors route `matrix/inverse` through native
    SPIR-V Gauss-Jordan kernels behind `matrix-numerical-complex128` and
    `matrix-numerical-complex64`.
  - Tensor results preserve fixed-width complex dtype and Vulkan placement for
    Vulkan inputs. Singular matrices raise `tensor/singular-matrix`;
    unsupported layouts, devices, and dtypes fail closed without hidden CPU
    fallback or lowering through real tensors.
  - Validation passed: complex inverse shaders compiled with
    `glslangValidator` and passed `spirv-val`;
    `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
    CPU/Vulkan `--eval` smokes for Complex128 and Complex64 inverse plus
    singular diagnostics; host focused `advanced-collections-module`
    `pass=1496 fail=0`; bounded-container focused
    `advanced-collections-module` `pass=1479 fail=0`; docs parity,
    Stage 3 source parity, and targeted diff hygiene.
- 2026-04-18 CUDA fixed-width complex structural matrix checkpoint:
  - CUDA dense row-major zero-offset `Complex128` and `Complex64` tensors now
    support `matrix/transpose`, `matrix/diagonal`,
    `matrix/diagonal-matrix`, and `matrix/trace` behind
    `matrix-structural-complex128` and `matrix-structural-complex64`.
  - Tensor-returning operations preserve CUDA placement and fixed-width
    complex dtype. `matrix/trace` computes on CUDA and reads back only the
    public fixed-width complex scalar. Unsupported layouts, unavailable CUDA
    helper resolution, non-complex CUDA dtypes, and remaining numerical matrix
    families fail closed without hidden CPU fallback.
  - Validation passed: CUDA PTX generation and `ptxas`; CUDA helper compile
    and symbol checks; `./scripts/build_omni_chelpers.sh`;
    `c3c build --obj-out obj`; direct CUDA `--eval` smokes for transpose,
    diagonal-matrix, and trace; host focused `advanced-collections-module`
    `pass=1482 fail=0`; bounded-container focused
    `advanced-collections-module` `pass=1465 fail=0`.
- 2026-04-18 Vulkan fixed-width complex elementwise map checkpoint:
  - Dense row-major Vulkan `Complex128` and `Complex64` tensors now support
    elementwise `map` for `+`, `-`, `*`, `/`, unary `+`, `abs`, unary `-`,
    `real-part`, `imag-part`, and `conjugate` when the matching
    `elementwise-map-complex128` or `elementwise-map-complex64` capability is
    reported.
  - Direct Vulkan complex `abs`, `real-part`, and `imag-part` return
    component-width real Vulkan tensors; direct unary minus and `conjugate`
    preserve complex dtype and Vulkan placement.
  - Division uses a shader status buffer: zero denominator reports structured
    `tensor/domain-error`, and non-representable results fail before exposing
    output.
  - Historical boundary, superseded by the later CUDA map checkpoint below:
    CUDA complex `map` remained fail-closed at the Vulkan-map checkpoint.
    CUDA/Vulkan complex `contract` and matrix kernels remain fail-closed.
  - Validation: complex map shaders compiled with `glslangValidator` and
    passed `spirv-val`; `./scripts/build_omni_chelpers.sh`; `c3c build`; host
    focused `advanced-collections-module` `pass=1415 fail=0`; bounded
    container focused `advanced-collections-module` `pass=1398 fail=0`;
    primitive docs parity; targeted diff hygiene.
- 2026-04-18 CUDA fixed-width complex elementwise map checkpoint:
  - Dense row-major CUDA `Complex128` and `Complex64` tensors now support
    elementwise `map` for binary `+`, `-`, `*`, `/`; unary `abs`, unary `-`,
    identity/`+`, `real-part`, `imag-part`, and `conjugate` when the matching
    `elementwise-map-complex128` or `elementwise-map-complex64` capability is
    reported.
  - Generic CUDA complex `map` preserves complex dtype, including
    zero-imaginary component/magnitude results where applicable. Direct CUDA
    complex `real-part`, `imag-part`, and `abs` return component-width real
    CUDA tensors; direct unary `-` and `conjugate` preserve complex dtype.
  - Division by zero maps through native status to `tensor/domain-error`;
    nonrepresentable results map to invalid argument before result exposure.
  - Preserved boundary: CUDA/Vulkan complex `contract` and matrix kernels
    remain fail-closed.
  - Validation passed: `./scripts/build_omni_chelpers.sh`; `c3c build`; host
    focused `advanced-collections-module` `pass=1433 fail=0`; bounded
    container focused `advanced-collections-module` `pass=1416 fail=0`; CUDA
    PTX generation and `ptxas` for `csrc/tensor_cuda_complex_map.cu`; C helper
    syntax check; primitive docs parity; Stage 3 source parity; targeted diff
    hygiene.
- 2026-04-18 Vulkan transpose-view and fixed-width complex numerical checkpoint:
  - Vulkan transpose-view materialization is validated for direct rank-2
    transpose views over dense zero-offset Vulkan storage. Documentation now
    distinguishes this narrower dense-materialization boundary from future
    arbitrary view-aware Vulkan kernel execution.
  - Vulkan fixed-width complex `matrix/lu`, `matrix/determinant`, and
    `matrix/solve` are validated behind `matrix-numerical-complex128` and
    `matrix-numerical-complex64` after capability reporting, shader/helper
    status mapping, public routing, oracle tests, no-hidden-CPU-fallback
    checks, and host/container validation.
  - The earlier docs-only pass did not edit source/runtime/tests/C helpers and did not
    verify runtime behavior.
- 2026-04-18 Vulkan map handler propagation and lazy-realize stack checkpoint:
  - Unsupported Vulkan `map` callables still fail closed, but `handle` now
    receives the structured `tensor/backend-unsupported` payload for direct
    `map`, nested lazy Vulkan map sources, and one-argument `realize` of
    unsupported lazy Vulkan map expressions.
  - Recoverable raises with no `data` build their canonical payload directly in
    root scope before `raise_pending` is installed, avoiding boundary-promotion
    stack pressure inside handle bodies. Compiled one-argument and
    multi-argument primitive apply paths now propagate evaluated `ERROR`
    arguments before callee dispatch.
  - Normal StackCtx usable size is 256KB so handled CPU lazy Tensor map
    realization succeeds on small tensors. This is a runtime stack-budget
    fix, not a Vulkan callable expansion.
  - Validation: `c3c build --obj-out obj`; direct smokes for handled Vulkan
    direct/lazy-source `map`, direct and handled `realize`, handled CPU lazy
    map `realize`, and destination-form `realize` source-error propagation;
    host focused `advanced-collections-module` `pass=1352 fail=0`; host
    `advanced-effect-continuation` `pass=56 fail=0`; host
    `advanced-effect-union-limit` `pass=68 fail=0`; bounded-container focused
    `advanced-collections-module` `pass=1335 fail=0`; stack suite
    `pass=23 fail=0`; targeted diff hygiene passed.
- 2026-04-18 CUDA/Vulkan fixed-width complex storage checkpoint:
  - `Complex128`/`Complex64` CUDA and Vulkan placement now use separate
    storage-copy dtype predicates so real compute gates remain real-only.
    Concrete zero-offset dense row-major complex CPU tensors can be copied to
    CUDA/Vulkan when the matching storage capability is available, copied back
    explicitly with `to-device 'cpu`, and copied into matching existing complex
    device destinations through `realize`.
  - Historical checkpoint: `tensor-backends` reported `complex128` and
    `complex64` storage bits on CUDA/Vulkan entries, and complex
    map/contract bits were false at this storage-only point. Superseded by the
    later Vulkan and CUDA complex map checkpoints above; complex
    `contract`/matrix bits still fail closed.
  - Validation: `c3c build`; host focused `advanced-collections-module`
    `pass=1399 fail=0`; bounded-container focused `advanced-collections-module`
    `pass=1382 fail=0`; primitive docs parity; targeted diff hygiene.
- 2026-04-18 Vulkan Tensor BigInteger rounding checkpoint:
  - Dense row-major Vulkan `Float64`/`Float32` Tensor `floor`, `ceiling`,
    `round`, and `truncate` now use dedicated status-bearing integer-result
    shaders/helpers gated by Vulkan `shaderInt64` and backend
    `rounding-big-integer`.
  - The public result is CPU `Tensor BigInteger` after checked int64 copyback.
    Vulkan BigInteger storage is not claimed, and generic Vulkan
    `available`/`float64`/`float32` remains insufficient proof of this path.
  - Preserved boundary: no same-dtype Vulkan float rounding map/unary opcode,
    no downcast, and no hidden CPU fallback for unsupported Vulkan rounding.
  - Validation: Vulkan rounding shaders compiled with `glslangValidator` and
    passed `spirv-val`; helper rebuild passed; `c3c build --obj-out obj`
    passed; direct Vulkan smokes returned `("BigInteger" "cpu" "3" "-4")`
    for `Float64` floor, `("BigInteger" "cpu" "4" "-3")` for `Float64`
    ceiling, `("BigInteger" "cpu" "4" "-4")` for `Float32` round, and
    `("BigInteger" "cpu" "3" "-3")` for `Float32` truncate; overflow
    reported the Tensor integer range diagnostic; direct `map floor` remained
    Vulkan backend-unsupported; host focused `advanced-collections-module`
    `pass=1326 fail=0`; bounded-container focused
    `advanced-collections-module` `pass=1309 fail=0`; primitive docs parity,
    Stage 3 source parity, and targeted diff hygiene passed.
- 2026-04-18 Tensor layout metadata checkpoint:
  - `tensor-layout` is the public metadata query for Tensor representation
    state. It returns a `Dictionary` with dtype, device, payload, layout,
    dense-row-major, shape, strides, rank, element-count, byte-length,
    storage-offset, storage-elements, storage-bytes, is-view, owns-storage,
    owner, and write-policy.
  - Current symbols are payload `concrete`/`map`/`contract`/`view`, layout
    `dense-row-major`/`strided`, owner `self`/`view-source`/`expression`, and
    write-policy `mutable`/`immutable`/`mutable-view`/`read-only-view`.
  - Preserved boundary after the transpose-view follow-up: no GPU view-backed
    kernels, and dense GPU kernels still require zero-offset dense row-major
    storage.
  - Validation: `c3c build --obj-out obj`; direct `tensor-layout` smokes for
    dense/rank-0/lazy/fail-closed behavior; host focused
    `advanced-collections-module` `pass=1332 fail=0`; bounded-container
    focused `advanced-collections-module` `pass=1315 fail=0`;
    bounded-container `memory-lifetime-smoke` `pass=229 fail=0`; primitive
    docs parity; Stage 3 source parity; targeted diff hygiene.
- 2026-04-18 read-only Tensor transpose-view implementation checkpoint:
  - Public `matrix/transpose-view` is documented as the first explicit
    read-only rank-2 Tensor view constructor. It swaps logical shape/strides,
    keeps the source as owner, and reports `tensor-layout` payload `view`,
    owner `view-source`, `owns-storage` false, and write-policy
    `read-only-view`.
  - CPU support contract: `ref`, `(Array view)`, `(List view)`, and CPU
    `realize` materialization observe the transposed logical indexing. Writes
    through the view fail closed.
  - Existing `matrix/transpose` remains materializing for concrete tensors but
    composes structurally when its input is already a transpose view.
  - Preserved backend boundary: CUDA/Vulkan placement, destination `realize`,
    raw copy helpers, and device kernels fail closed for view payloads instead
    of hidden materialization or unsafe stride interpretation.
  - Runtime hardening: backend copy paths reject view payloads before hidden
    realization, including dense double-transpose views; post-realization
    backend copies require concrete zero-offset dense row-major storage.
  - Validation: `c3c build --obj-out obj`; direct smokes for `tensor-layout`
    view metadata, CPU `ref`, `(Array view)`, `(List view)`, CPU `realize`,
    double-transpose structural composition, return/closure capture, and
    read-only destination rejection; host focused `advanced-collections-module`
    `pass=1343 fail=0`; bounded-container focused
    `advanced-collections-module` `pass=1326 fail=0`; bounded-container
    `memory-lifetime-smoke` `pass=229 fail=0`.
- 2026-04-18 Vulkan `Float64` `stats/normal-quantile` checkpoint:
  - Dense row-major Vulkan `Float64` Tensor `stats/normal-quantile` now uses a
    dedicated status-bearing inverse-CDF shader/helper. Direct Tensor unary
    math and `map stats/normal-quantile` preserve Vulkan placement and
    `Float64` dtype for representative valid probabilities.
  - The shader avoids unavailable Vulkan 1.0 double `log` by inverting the
    landed Float64 normal-CDF polynomial approximation with bounded bisection.
    Status semantics match CUDA/Vulkan Float32: status `1` maps to
    `stats/normal-quantile: probability must be between 0 and 1`, status `2`
    maps to `stats/normal-quantile: expected finite numeric input`, and
    nonzero status is copied back before result exposure.
  - Preserved boundary: no hidden CPU fallback and no Float64-to-Float32
    downcast. The existing statusless Vulkan unary helper still must not be
    given op `20`.
  - Validation: `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_normal_quantile_f64.comp -o
    /tmp/omni_tensor_vulkan_normal_quantile_f64.spv`; `spirv-val --target-env
    vulkan1.0 /tmp/omni_tensor_vulkan_normal_quantile_f64.spv`;
    `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
    Vulkan direct/map Float64 quantile smokes plus domain/non-finite smokes;
    host focused `advanced-collections-module` `pass=1317 fail=0`;
    bounded-container focused `advanced-collections-module` `pass=1300 fail=0`
    with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- 2026-04-18 Vulkan `Float32` `stats/normal-quantile` checkpoint:
  - Dense row-major Vulkan `Float32` Tensor `stats/normal-quantile` now uses a
    dedicated three-binding shader/helper: input buffer, output buffer, and a
    `uint32` status buffer. Direct Tensor unary math and
    `map stats/normal-quantile` preserve Vulkan placement and `Float32` dtype
    for valid probabilities.
  - Status semantics match the CUDA probability-domain ABI: `0` success, `1`
    finite probability outside `0 < p < 1`, and `2` non-finite probability.
    The shader uses `atomicMax`, so non-finite status wins over a domain
    endpoint in mixed invalid tensors. Nonzero status destroys the output and
    raises the scalar-compatible diagnostic before result exposure.
  - Preserved boundary: Vulkan `Float64` `stats/normal-quantile` remains
    fail-closed. The existing statusless Vulkan unary helper still must not be
    given op `20`, and there is no hidden CPU fallback or Float64 downcast.
  - Validation: `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_normal_quantile_f32.comp -o
    /tmp/omni_tensor_vulkan_normal_quantile_f32.spv`; `spirv-val
    --target-env vulkan1.0 /tmp/omni_tensor_vulkan_normal_quantile_f32.spv`;
    `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
    Vulkan valid/domain/non-finite/mixed-invalid and Float64 fail-closed
    smokes; host focused `advanced-collections-module` `pass=1311 fail=0`;
    bounded-container focused `advanced-collections-module` `pass=1298 fail=0`
    with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- 2026-04-18 Vulkan `Float64` `stats/normal-cdf` checkpoint:
  - Dense row-major Vulkan `Float64` Tensor `stats/normal-cdf` now routes
    through fixed op id `19` in the existing two-buffer Float64 unary helper.
    Direct Tensor unary math and `map stats/normal-cdf` preserve Vulkan
    placement and `Float64` dtype for representative finite inputs.
  - The shader uses a piecewise degree-7 polynomial approximation over
    `|x| < 8`, applies symmetry for negative inputs, and saturates outside
    that range. Sampled error against the CPU double oracle stayed below
    `6e-8`; regression tests use `1e-6` tolerance at `-1.0`, `0.0`, and
    `1.0`.
  - Preserved boundary: Vulkan `Float64` `stats/normal-quantile` remains
    fail-closed with the existing diagnostic. This change does not downcast
    Float64 to Float32 and does not use hidden CPU fallback.
  - Validation: `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_unary_f64.comp -o
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`; `spirv-val --target-env
    vulkan1.0 /tmp/omni_tensor_vulkan_map_unary_f64.spv`;
    `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
    Vulkan direct/map Float64 CDF smokes; Float64 quantile fail-closed smoke;
    host focused `advanced-collections-module` `pass=1313 fail=0`;
    bounded-container focused `advanced-collections-module` `pass=1300 fail=0`
    with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- 2026-04-18 CUDA `stats/normal-quantile` probability-status checkpoint:
  - CUDA scientific unary op `20` now uses generated CUDA C/libdevice PTX with
    libdevice `normcdfinv`, preserving CUDA placement and `Float64`/`Float32`
    dtype for direct Tensor unary math and `map stats/normal-quantile`.
  - The CUDA helper now has a probability-status path: status `0` remains in
    device memory on success, status `1` maps to
    `stats/normal-quantile: probability must be between 0 and 1`, status `2`
    maps to `stats/normal-quantile: expected finite numeric input`, and higher
    status wins so non-finite input takes priority over a domain endpoint in
    mixed invalid tensors.
  - Superseded for Vulkan by the later Vulkan quantile checkpoints above.
    Vulkan Float32 and Float64 quantile now have separate status-bearing
    helper paths.
  - Validation: CUDA PTX generation, `ptxas`, helper C compile,
    `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`, direct CUDA
    valid/invalid smokes, host focused `advanced-collections-module`
    `pass=1307 fail=0`, and bounded-container focused
    `advanced-collections-module` `pass=1294 fail=0` with
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- 2026-04-18 Vulkan map preflight hardening checkpoint:
  - Vulkan public `map` lowering and lazy map realization now recursively
    detect non-Vulkan Tensor leaves before concrete realization, so mixed
    CPU/Vulkan lazy operands fail with
    `map: Vulkan operands must remain Vulkan-placed` before CPU lazy
    materialization can run.
  - Unsupported Vulkan callables are rejected before source realization, and
    direct Tensor `min` / `max` now applies the same recursive Vulkan placement
    preflight before resolving map-backed operands.
  - Preserved boundaries: no hidden CPU/GPU transfers, no mixed-device
    execution support, no generic Vulkan unary mode-3 revival, and no new
    Docker-image work.
  - Validation: `c3c build --obj-out obj`; direct Vulkan smokes for mixed
    CPU/Vulkan lazy `map`, unsupported binary callable preflight, unsupported
    unary callable preflight, and mixed CPU/Vulkan lazy `min`; bounded-container
    focused `advanced-collections-module` `pass=1287 fail=0` with
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`; targeted
    `git diff --check`.
- 2026-04-17 scalar `Float32` runtime value checkpoint:
  - Scalar `Float32` now has a runtime value tag/payload/constructor, type-id
    cache, `Number` parent wiring, `type-of` / `is?`, stdlib `float32?`,
    copy/promotion/env-copy leaf handling, printing/string/format conversion,
    numeric comparison/equality/order/hash integration, basic arithmetic,
    schema/tuple/JSON/CSV/AOT surfaces, and Tensor `Float32` extraction through
    `ref`, `Array`, `List`, `Iterator`, contract scalar readback, and structural
    matrix helpers.
  - Tensor `Float32` element extraction returns scalar `Float32` rather than
    widening to `Float64`; determinant and norm scalars keep their documented
    `Float64` return contract.
  - Validation: `c3c build --obj-out obj`, direct runtime smokes, focused
    `advanced-core-semantics` `pass=71 fail=0`, arithmetic-comparison
    `pass=47 fail=0`, bounded-container focused `advanced-collections-module`
    `pass=1205 fail=0`, primitive docs parity, and Stage 3 source parity. Local
    ASAN compile was attempted but the local `c3c` rejected sanitizer mode.
- 2026-04-17 CUDA `Float32` placement and contract checkpoint:
  - CUDA `to-device`, CPU copyback, destination-form `realize`, scalar
    destination fills, and supported lazy CUDA contract destination writes now
    accept dense row-major `Float32` Tensor storage alongside `Float64`.
  - CUDA-placed matching `Float32` rank-2/rank-2, rank-2/rank-1, and
    rank-1/rank-2 single-axis contractions route through separately resolved
    `cublasSgemm_v2` / `cublasSgemv_v2` helpers when cuBLAS reports `float32`
    capability. Existing `Float64` cuBLAS availability remains independent of
    those symbols.
  - CUDA `map` and implicit CPU/GPU transfer remain fail-closed/deferred
    surfaces. Scalar `Float32` runtime values are covered by the scalar
    checkpoint above, and zero-size CUDA contract identity/fill is covered by
    the later CUDA zero-size checkpoint.
  - Validation: `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
    host focused `advanced-collections-module` `pass=1218 fail=0`, and
    bounded-container focused `advanced-collections-module` `pass=1205
    fail=0`.
- 2026-04-18 CUDA zero-size contract checkpoint:
  - Supported dense row-major CUDA `Float64` and `Float32` rank-2/rank-2,
    rank-2/rank-1, and rank-1/rank-2 single-axis contractions now preserve
    zero-free outputs as CUDA-placed zero-length tensors and fill non-empty
    zero-contracted outputs with dtype-preserving additive identity.
  - Zero-size identity/fill skips cuBLAS and uses the existing CUDA fill path,
    so the identity case succeeds even when cuBLAS is forced unavailable for
    tests. Nonzero CUDA contractions still require the matching cuBLAS helper.
  - Remaining adjacent CUDA work is CUDA `map`; mixed devices/dtypes and
    unsupported layouts stay fail-closed.
  - Validation: `c3c build --obj-out obj`; direct CUDA-gated smokes for
    Float64/Float32 zero-contracted rank-2, zero-free rank-2, rank-2/rank-1,
    and rank-1/rank-2 outputs. Host focused `advanced-collections-module`
    was attempted and terminated after idling past ten minutes;
    bounded-container focused `advanced-collections-module` passed
    `pass=1210 fail=0`.
- 2026-04-18 CUDA rank-1 dot checkpoint:
  - CUDA-placed contiguous matching `Float64` or matching `Float32`
    rank-1/rank-1 single-axis contractions now return CUDA-placed scalar
    Tensor outputs. The implementation reuses the existing cuBLAS GEMV helper
    by treating the left vector as a one-row row-major matrix.
  - Zero-size rank-1 dots use the CUDA additive-identity fill path. Nonzero
    rank-1 dots still require cuBLAS availability. CUDA binary `map` exact
    shape/scalar execution is covered by the later CUDA map checkpoint below.
  - Validation: `c3c build --obj-out obj`; direct CUDA-gated smokes for
    Float64 rank-1 dot, explicit-axis rank-1 dot, Float32 rank-1 dot with
    scalar dtype preservation, zero-size rank-1 dot identity, rank-0
    destination realization for Float64/Float32 rank-1 dot, and
    bounded-container focused `advanced-collections-module` `pass=1217
    fail=0`.
- 2026-04-18 CUDA elementwise binary map checkpoint:
  - CUDA now has embedded-PTX CUDA Driver API elementwise binary map kernels
    for dense row-major `Float64` and `Float32` tensors. Supported operations
    are `+`, `-`, `*`, `/`, `min`, and `max`.
  - Supported operand shapes are tensor/scalar, scalar/tensor, exact-shape
    tensor/tensor, and right-aligned singleton-axis tensor/tensor
    broadcasting.
  - Supported paths are direct public `map` on CUDA tensors, lazy map
    realization to CUDA, and CUDA destination realization from lazy CUDA maps.
    There is no hidden CPU fallback.
  - `tensor-backends` reports `elementwise-map-float64` and
    `elementwise-map-float32` capability keys for CPU, Vulkan, and CUDA.
  - Superseded by later checkpoints below: scientific CUDA map and valid foreign
    CUDA clone semantics are landed. Unsupported callables, mixed CPU/CUDA
    operands, mixed dtype/device operands, and unsupported layout remain explicit
    backend-unsupported/fail-closed residuals.
  - Superseded by the later mixed-operand diagnostic checkpoint below: mixed
    CPU/CUDA map operands and mixed CUDA dtypes now have explicit fail-closed
    diagnostics, and unsupported mixed CPU/CUDA lazy operands are rejected before
    CPU lazy materialization can run.
  - Superseded by the later foreign CUDA clone checkpoint below: CUDA concrete
    tensor clone/copy now works for Omni-owned CUDA payloads and valid foreign
    CUDA concrete payloads, while fake or invalid CUDA handles stay
    fail-closed.
  - Validation: helper compile, `c3c build --obj-out obj`, direct CUDA-gated
    smokes for scalar/exact-shape/broadcast binary map and fail-closed
    residuals, host focused `advanced-collections-module` `pass=1240 fail=0`.
    Earlier exact/scalar validation also passed bounded-container
    `memory-lifetime-smoke` `pass=228 fail=0`, bounded-container focused
    `advanced-collections-module` `pass=1224 fail=0`, primitive docs parity,
    and Stage 3 source parity.
- 2026-04-18 CUDA arithmetic unary map checkpoint:
  - CUDA now has separate embedded-PTX CUDA Driver API unary map kernels for
    dense row-major `Float64` and `Float32` tensors. Supported unary op ids are
    `0..4`: `abs`, unary `-`, `sqrt`, identity, and zero-fill.
  - Public `(map + tensor)`, `(map abs tensor)`, `(map - tensor)`,
    `(map sqrt tensor)`, `(map real-part tensor)`, `(map imag-part tensor)`,
    and `(map conjugate tensor)` route through the CUDA unary helper for
    eligible CUDA tensors. Direct Tensor `abs`, unary `-`, `sqrt`,
    `real-part`, `imag-part`, and `conjugate` preserve CUDA placement through
    the same helper.
  - Superseded by the later scientific CUDA checkpoint below: scientific CUDA
    unary op ids `5..19` are landed for dense row-major `Float64`/`Float32`.
    Unsupported layouts, mixed devices/dtypes, and unsupported callables remain
    residuals.
  - Validation: helper compile, `./scripts/build_omni_chelpers.sh`, `c3c
    build --obj-out obj`, CUDA-available smokes for Float64/Float32 unary
    arithmetic/component ops and scientific fail-closed behavior, host focused
    `advanced-collections-module` `pass=1256 fail=0`, bounded-container
    focused `advanced-collections-module` `pass=1243 fail=0`, primitive docs
    parity, Stage 3 source parity, and targeted `git diff --check`.
- 2026-04-18 CUDA scientific unary map checkpoint:
  - CUDA now has generated CUDA C/libdevice PTX kernels for dense row-major
    `Float64` and `Float32` scientific unary op ids `5..19`: `sin`, `cos`,
    `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, and
    `log10`, plus `math/erf`, `math/erfc`, and `stats/normal-cdf`.
  - CPU Tensor `math/erf`, `math/erfc`, and `stats/normal-cdf` now apply
    elementwise before the CUDA path: `Float64`/`Float32` preserve float dtype,
    `BigInteger` returns `Float64`, `BigFloat` preserves dtype, and
    `BigComplex` fails closed because no Tensor complex error-function or
    distribution contract exists.
  - Public `map`, lazy CUDA map realization, CUDA destination realization, and
    direct Tensor scientific unary primitives, including `math/erf`,
    `math/erfc`, and `stats/normal-cdf`, preserve CUDA placement when the
    separate scientific module loads.
  - `tensor-backends` reports `scientific-map-float64` and
    `scientific-map-float32`; existing `elementwise-map-*` arithmetic support
    stays independent.
  - Remaining CUDA map residuals are unsupported layouts/views, mixed
    CPU/CUDA operands, mixed CUDA dtypes/devices, and unsupported arbitrary
    callables. `math/erf`, `math/erfc`, and `stats/normal-cdf` are fixed
    built-in opcodes, not a general callable ABI.
  - Validation so far: CUDA C source generated PTX with CUDA 13 `nvcc
    -arch=compute_75`; generated PTX assembled with `ptxas -arch=sm_75`; helper
    compile, helper rebuild, `c3c build --obj-out obj`, CUDA runtime smokes,
    host focused `advanced-collections-module` `pass=1258 fail=0`, bounded
    container focused `advanced-collections-module` `pass=1245 fail=0` with
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`, primitive docs
    parity, Stage 3 source parity, and targeted `git diff --check`.
  - Follow-on `math/erf`/`math/erfc` validation: CUDA PTX regeneration with
    `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75`, `ptxas -arch=sm_75`,
    helper compile, helper rebuild, `c3c build --obj-out obj`, direct CPU/CUDA
    smokes, host focused `advanced-collections-module` `pass=1271 fail=0`, and
    bounded-container focused `advanced-collections-module` `pass=1258 fail=0`.
  - Follow-on `stats/normal-cdf` validation: opcode-19 CUDA PTX regeneration
    and ptxas, helper compile/rebuild, `c3c build --obj-out obj`, direct
    CPU/CUDA/Vulkan-boundary smokes, host focused `advanced-collections-module`
    `pass=1282 fail=0`, bounded-container focused
    `advanced-collections-module` `pass=1269 fail=0` with
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`, primitive docs
    parity, Stage 3 source parity, and targeted `git diff --check`.
  - Validation-image operational note: `omni-validation:2026-03-10` is arm64,
    but its baked-in `/opt/c3/c3c` remains x86-64 after rebuild because
    `docker/validation.Dockerfile` downloads `c3-linux.tar.gz`. Use the host
    toolchain mount until the Dockerfile selects/builds a native arm64 C3.
- 2026-04-18 Tensor quantile / Vulkan Float32 distribution checkpoint:
  - CPU Tensor `stats/normal-quantile` is landed for `Float64`, `Float32`,
    and `BigFloat`, with whole-operation failure on the first probability
    outside `0 < p < 1`. `BigComplex` rejects, and non-empty `BigInteger`
    probability tensors fail closed because integer probabilities cannot
    satisfy the open probability interval.
  - Superseded by later checkpoints: CUDA `Float64`/`Float32` and Vulkan
    `Float64`/`Float32` `stats/normal-quantile` now have per-element
    probability-domain status contracts.
  - Vulkan dense row-major `Float32` `stats/normal-cdf` now uses the dedicated
    unary helper with fixed op id `19`, preserving Vulkan placement and
    `Float32` dtype for public `map` and direct Tensor unary math. Vulkan
    `Float64` `stats/normal-cdf` remains fail-closed pending a double
    approximation policy.
  - CUDA construction-time map fail-closed tests now use C harness
    `test_error_contains` where Lisp `handle` can stack-overflow around
    immediate CUDA map construction errors.
  - Validation so far: Vulkan Float32 shader compile and `spirv-val`, helper
    rebuild, `c3c build --obj-out obj`, direct CPU/CUDA/Vulkan smokes, and
    host focused `advanced-collections-module` `pass=1296 fail=0`,
    bounded-container focused `advanced-collections-module` `pass=1283 fail=0`
    with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`, primitive
    docs parity, Stage 3 source parity, and targeted `git diff --check`.
- 2026-04-18 foreign CUDA payload clone checkpoint:
  - Valid foreign CUDA concrete Tensor payloads now clone by allocating fresh
    Omni-owned CUDA storage, copying source bytes device-to-device, and
    installing `tensor_cuda_device_finalizer` on the clone.
  - Source foreign finalizers remain responsible only for their source handles;
    fake, stale, malformed, or otherwise invalid CUDA handles remain
    fail-closed.
  - Implementation review also corrected the Vulkan retain clone branch to keep
    `tensor_vulkan_device_finalizer`; CUDA finalizer installation is confined to
    the CUDA clone branch.
  - Validation: `c3c build --obj-out obj`; bounded-container
    `memory-lifetime-smoke` `pass=229 fail=0` with
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`; host focused
    `advanced-collections-module` `pass=1258 fail=0`.
- 2026-04-18 CUDA map mixed-operand diagnostic checkpoint:
  - Mixed CPU/CUDA Tensor operands in CUDA `map` now fail with the explicit
    `map: CUDA operands must remain CUDA-placed` diagnostic.
  - Mixed CUDA Tensor dtypes fail with `tensor/dtype-mismatch` from the CUDA map
    helper path instead of a generic backend unsupported message.
  - CUDA and Vulkan map probes now check whether expression trees touch the
    probed device before resolving concrete storage, so unsupported mixed
    CPU/CUDA lazy operands fail before CPU lazy materialization can run.
  - Validation: `c3c build --obj-out obj`; host focused
    `advanced-collections-module` `pass=1261 fail=0`; bounded-container focused
    `advanced-collections-module` `pass=1248 fail=0` with
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- 2026-04-17 Vulkan `Float32` scientific unary checkpoint:
  - Dense row-major Vulkan `Float32` tensors now support `sin`, `cos`, `tan`,
    `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, and `log10`
    through the dedicated unary helper ABI for public `map` and direct Tensor
    unary math.
  - Results preserve Vulkan placement and `Float32` dtype; Vulkan `Float64`
    scientific unary remains fail-closed because the current Vulkan 1.0 GLSL
    validation path rejected double transcendental builtins and no hidden
    downcast is allowed.
  - Validation: shader compile/`spirv-val`, regenerated `Float32` SPIR-V C
    embed, `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
    direct Vulkan smokes, host focused `advanced-collections-module`
    `pass=1198 fail=0`, and bounded-container focused
    `advanced-collections-module` `pass=1185 fail=0`.
- 2026-04-17 CPU `Float32` matrix factor/SVD checkpoint:
  - Native CPU `Tensor Float32` now supports the public matrix factor/SVD
    contract for `matrix/determinant`, `matrix/lu`, `matrix/solve`,
    `matrix/inverse`, `matrix/cholesky`, `matrix/qr`,
    `matrix/singular-values`, `matrix/svd`, and SVD-backed `matrix/norm`
    selectors `'spectral` / `'nuclear`.
  - Tensor outputs preserve `Float32`; scalar outputs keep the existing public
    `Float64` scalar contract. The CPU Float32 paths use explicit Float32
    workspaces/helpers and no hidden `d*` LAPACK routing.
  - Validation: `c3c build --obj-out obj`, direct CPU `Float32` smokes, host
    focused `advanced-collections-module` `pass=1195 fail=0`, and
    bounded-container focused `advanced-collections-module` `pass=1182
    fail=0`.
- 2026-04-17 Vulkan `Float32` SVD-backed checkpoint:
  - Vulkan-placed dense row-major `Float32` tensors now have dedicated
    shader/helper paths for `matrix/norm` `'spectral` and `'nuclear`,
    `matrix/singular-values`, and direct `matrix/svd`.
  - Tensor outputs preserve `Float32` dtype and Vulkan placement; scalar norm
    outputs keep the public `Float64` scalar return contract. Superseded by
    the CPU checkpoint above: CPU `Float32` singular-value/SVD surfaces are no
    longer fail-closed.
  - Validation: shader compile/`spirv-val`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct Vulkan `Float32` smokes, host focused
    `advanced-collections-module` `pass=1121 fail=0`, bounded-container
    focused `advanced-collections-module` `pass=1108 fail=0`, primitive docs
    parity, Stage 3 source parity, stale-current docs scan, and targeted
    `git diff --check`.
  - Superseded: the dense all-ones / rank-deficient `65x65` Vulkan `Float32`
    SVD failure is fixed by scale-aware eigenvalue tolerance plus orthonormal
    completion. Do not resume staged/tiled SVD work as a correctness blocker
    unless new validation regresses this result.
- 2026-04-17 Vulkan `Float32` direct reducer checkpoint:
  - Native CPU/runtime `Tensor Float32` now has reducer oracles for
    `matrix/rank` and `matrix/norm` default/`'frobenius`, `'one`,
    `'infinity`, and `'max`.
  - Vulkan `Float32` direct reducers now have dedicated shader/helper paths for
    `matrix/rank` and `matrix/norm` default/`'frobenius`, `'one`,
    `'infinity`, and `'max`. Public scalar outputs stay `Integer` for rank and
    `Float64` for norms; there is no hidden `Float64` downcast/widening helper
