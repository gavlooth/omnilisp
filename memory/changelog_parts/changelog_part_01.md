# Memory Changelog Index Part 01

Source: `memory/CHANGELOG.md`

## 2026-04-18

- Completed Vulkan fixed-width complex singular-values and spectral/nuclear
  norms:
  - Current implementation status: CPU `Complex128` and `Complex64`
    `matrix/singular-values` support is landed via realification and
    duplicate-pair collapse. Complex singular values return component-width
    real tensors (`Float64` for `Complex128`, `Float32` for `Complex64`).
  - CPU complex `matrix/norm` selectors `'spectral` and `'nuclear` now route
    through the fixed-complex singular-value oracle and return the existing
    public `Float64` scalar result.
  - Vulkan dense row-major zero-offset `Complex128`/`Complex64` tensors route
    `matrix/singular-values` and spectral/nuclear `matrix/norm` through native
    SPIR-V helpers behind `matrix-numerical-complex128` and
    `matrix-numerical-complex64`. Tensor singular-value results stay
    Vulkan-placed with component-width real dtype; norm reads back only the
    public scalar result.
  - Preserved boundary: full complex `matrix/svd` factor output remains
    fail-closed because its contract requires complex `u`/`v` and
    component-width real `s`. CUDA complex singular-values/norm/SVD and
    complex eigen result contracts remain separate work.
  - Validation:
    - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for
      `csrc/tensor_vulkan_singular_values_complex128.comp` and
      `csrc/tensor_vulkan_singular_values_complex64.comp`
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - direct Vulkan smokes: Complex128 singular-values
      `("Float64" 5.0 2.0)`, Complex64 singular-values
      `("Float32" 5.0 2.0)`, complex spectral/nuclear norm `(5.0 7.0)`
    - host focused `advanced-collections-module` passed `1598/0`
    - bounded-container focused `advanced-collections-module` passed `1581/0`
    - primitive docs parity, Stage 3 source parity, and targeted diff hygiene
      passed
    - Review follow-up added a native guard for fixed-complex realified Jacobi
      iteration-count overflow and tests for non-diagonal lazy, wide,
      zero-size, spectral/nuclear, and guard paths.
  - Current best next step: either full complex `matrix/svd` factor output,
    CUDA complex singular-values/norm selectors, or complex eigen/eigenpair
    result contracts, with no-hidden-fallback validation.

- Completed Vulkan fixed-width complex matrix QR and Cholesky:
  - Current implementation status: CPU `Complex128` and `Complex64`
    `matrix/qr` and `matrix/cholesky` oracle support is landed. Complex QR
    uses the Hermitian inner product and returns dtype-preserving `q`/`r`.
    Complex Cholesky accepts Hermitian positive-definite inputs and returns
    dtype-preserving lower factors with zero upper triangles.
  - Vulkan dense row-major zero-offset `Complex128`/`Complex64` tensors route
    `matrix/qr` and `matrix/cholesky` through native SPIR-V helpers behind
    `matrix-numerical-complex128` and `matrix-numerical-complex64`.
  - Added generated Vulkan complex QR/Cholesky shaders and SPIR-V C
    embeddings, native helper exports, C3 externs/routing, and guarded
    regression tests for CPU/Vulkan dtype/device/shape preservation,
    Hermitian QR projection, Cholesky values, singular/non-Hermitian/non-HPD
    diagnostics, no-LAPACK behavior, lazy Vulkan inputs, and fail-closed
    storage-only paths.
  - CPU fixed-complex QR and Cholesky tolerances now match the Vulkan shader
    thresholds for near-rank-deficient QR and near-Hermitian Cholesky inputs.
    Vulkan Cholesky diagnostics use symmetric/Hermitian positive-definite
    wording while preserving `tensor/not-positive-definite`.
  - Preserved boundary: the Vulkan complex numerical subset now covers
    LU/determinant/solve/inverse/rank/direct-norm/QR/Cholesky. Complex
    singular-value/SVD, spectral/nuclear complex norm selectors, complex eigen
    routines, and CUDA fixed-width complex numerical matrix variants remain
    separate operation families.
  - Validation: complex QR/Cholesky shaders compiled with
    `glslangValidator` and passed `spirv-val`;
    `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
    CPU/Vulkan smokes for Complex128 and Complex64 QR/Cholesky; host focused
    `advanced-collections-module` `1570 passed, 0 failed`;
    bounded-container focused `advanced-collections-module`
    `1553 passed, 0 failed`; primitive docs parity; Stage 3 source parity;
    targeted diff hygiene.

- Completed Vulkan fixed-width complex matrix rank and direct norms:
  - Current implementation status: CPU `Complex128` and `Complex64`
    `matrix/rank` and direct `matrix/norm` oracle support is landed.
    Complex rank uses magnitude-based pivoting and tolerance. Direct complex
    norms use magnitudes for default/`'frobenius`, `'one`, `'infinity`, and
    `'max`; complex spectral/nuclear selectors fail closed until complex
    singular-value/SVD support lands.
  - Vulkan dense row-major zero-offset `Complex128`/`Complex64` tensors route
    `matrix/rank` and direct `matrix/norm` through native SPIR-V reducers
    behind `matrix-numerical-complex128` and
    `matrix-numerical-complex64`.
  - Added generated Vulkan complex rank/norm shaders and SPIR-V C embeddings,
    native helper exports, C3 externs/routing, and guarded regression tests
    for CPU/Vulkan rank full-rank, deficient, tolerance, zero-tolerance, norm
    selector, no-LAPACK, fail-closed storage-only, and complex spectral/nuclear
    unsupported cases.
  - Preserved boundary: the Vulkan complex numerical subset now covers
    LU/determinant/solve/inverse/rank/direct-norm, but QR, Cholesky, complex
    singular-value/SVD, spectral/nuclear complex norm selectors, and complex
    eigen routines remain separate operation families. Do not infer them from
    complex storage, complex map, complex contract, structural matrix
    capability, or the broad numerical capability bits.
  - Validation: complex rank/norm shaders compiled with `glslangValidator`
    and passed `spirv-val`; `./scripts/build_omni_chelpers.sh`;
    `c3c build --obj-out obj`; direct CPU/Vulkan smokes for Complex128 and
    Complex64 rank/norm plus spectral fail-closed behavior; host focused
    `advanced-collections-module` `1540 passed, 0 failed`;
    bounded-container focused `advanced-collections-module`
    `1523 passed, 0 failed`.

- Completed Vulkan fixed-width complex matrix inverse:
  - Current implementation status: CPU `Complex128` and `Complex64`
    `matrix/inverse` oracle support is landed, and Vulkan dense row-major
    zero-offset `Complex128`/`Complex64` tensors route through native
    Gauss-Jordan inverse SPIR-V kernels behind
    `matrix-numerical-complex128` and `matrix-numerical-complex64`.
  - Added generated Vulkan complex inverse shaders and SPIR-V C embeddings,
    native helper exports, C3 externs and routing, and guarded regression tests
    for values, dtype, Vulkan placement, product identity, singular
    diagnostics, and fail-closed unsupported numerical capability cases.
  - Tensor-returning inverse operations preserve fixed-width complex dtype and
    Vulkan placement for Vulkan inputs; CPU inspection still requires explicit
    `to-device 'cpu`. Singular matrices raise `tensor/singular-matrix`;
    unsupported layouts/devices/dtypes fail closed without hidden CPU fallback
    or lowering through real tensors.
  - Preserved boundary at this checkpoint: the Vulkan complex numerical subset
    covered LU/determinant/solve/inverse. A later rank/direct-norm checkpoint
    extends that subset; QR, Cholesky, complex singular-value/SVD,
    spectral/nuclear complex norm selectors, and complex eigen routines remain
    separate operation families. Do not infer them from complex storage,
    complex map, complex contract, structural matrix capability, or the broad
    numerical capability bits.
  - Validation: complex inverse shaders compiled with `glslangValidator` and
    passed `spirv-val`; `./scripts/build_omni_chelpers.sh`;
    `c3c build --obj-out obj`; direct CPU/Vulkan smokes for Complex128 and
    Complex64 inverse plus singular diagnostics; host focused
    `advanced-collections-module` `1496 passed, 0 failed`;
    bounded-container focused `advanced-collections-module`
    `1479 passed, 0 failed`; primitive docs parity, Stage 3 source parity,
    and targeted diff hygiene passed.

- Completed CUDA fixed-width complex structural matrix kernels:
  - Current implementation status: CUDA dense row-major zero-offset
    `Complex128` and `Complex64` Tensor `matrix/transpose`,
    `matrix/diagonal`, `matrix/diagonal-matrix`, and `matrix/trace` support is
    landed behind explicit `matrix-structural-complex128` and
    `matrix-structural-complex64` operation capability bits.
  - Added generated CUDA C/PTX kernels, native helper exports, C3 externs and
    routing, `tensor-backends` capability reporting, and guarded regression
    tests that exercise positive CUDA results when the helper resolves and
    preserve fail-closed behavior otherwise.
  - Tensor-returning structural operations compute on CUDA, preserve
    fixed-width complex dtype and CUDA placement, and require explicit
    `to-device 'cpu` for CPU inspection. `matrix/trace` computes on CUDA and
    reads back only the fixed-width complex scalar required by the public
    scalar return contract.
  - Preserved boundary at this checkpoint: remaining fixed-width complex
    numerical matrix families beyond the landed Vulkan
    LU/determinant/solve/inverse lane remained separate operation families.
    The later rank/direct-norm checkpoint extends that subset. Do not infer
    QR/Cholesky, complex singular-value/SVD, spectral/nuclear complex norm, or
    complex eigenvector support from complex storage, complex map, complex
    contract, or CUDA/Vulkan structural matrix capability bits.
  - Validation: CUDA PTX generation with `nvcc` and `ptxas`; CUDA helper
    compile and exported-symbol checks; `./scripts/build_omni_chelpers.sh`;
    `c3c build --obj-out obj`; direct CUDA smokes for Complex128 transpose,
    Complex64 diagonal-matrix, and Complex128 trace; host focused
    `advanced-collections-module` `1482 passed, 0 failed`;
    bounded-container focused `advanced-collections-module`
    `1465 passed, 0 failed`.

- Completed Vulkan transpose-view materialization and fixed-width complex
  LU/determinant/solve numerical matrix checkpoint:
  - Direct rank-2 `matrix/transpose-view` values over dense zero-offset Vulkan
    storage now explicitly materialize to dense Vulkan tensors through
    `realize`, `to-device 'vulkan`, and `to-device 'cpu` copyback. This is not
    general strided/view-backed Vulkan execution; arbitrary views still require
    a separate offset/stride/backing-extent ABI and validation.
  - Historical checkpoint: CPU `matrix/lu`, `matrix/determinant`, and
    `matrix/solve` support `Complex128` and `Complex64` as the oracle family.
    Vulkan dense row-major zero-offset `Complex128`/`Complex64` tensors support
    the same public operations behind `matrix-numerical-complex128` and
    `matrix-numerical-complex64`. The later inverse entry above extends this
    numerical subset to `matrix/inverse`.
  - `matrix/lu` keeps fixed-width complex `lu` factors on Vulkan and returns
    host `pivots`/`swap-count`; `matrix/determinant` computes on Vulkan and
    reads back only the public scalar; `matrix/solve` returns a Vulkan-placed
    Tensor preserving RHS rank and dtype. Singular systems raise
    `tensor/singular-matrix`; unsupported layouts, devices, and dtypes fail
    closed without hidden CPU fallback.
  - Validation: complex LU/determinant/solve shaders compiled with
    `glslangValidator` and passed `spirv-val`; `./scripts/build_omni_chelpers.sh`;
    `c3c build --obj-out obj`; direct CPU/Vulkan smokes; host focused
    `advanced-collections-module` `1475 passed, 0 failed`;
    bounded-container focused `advanced-collections-module`
    `1458 passed, 0 failed`; targeted diff hygiene passed.
  - Historical next-step note, superseded by the inverse entry above: keep
    broader view-aware Vulkan kernels and remaining CUDA/Vulkan complex
    numerical families as explicit future operation slices. Do not infer them
    from this materialization/numerical matrix checkpoint.

- Superseded documentation/backlog checkpoint for active Vulkan lanes:
  - Updated the TODO, active plan, session report, Vulkan roadmap,
    dtype/layout policy, fixed-width complex contract note, plan index, and
    tensor area status for two then-implementation-in-progress lanes:
    Vulkan transpose-view materialization and Vulkan fixed-width complex
    `matrix/lu` / `matrix/determinant` / `matrix/solve`.
  - Vulkan transpose-view materialization is recorded as a narrower boundary
    than arbitrary view-aware GPU execution: it may explicitly materialize a
    rank-2 transpose view into dense Vulkan storage at placement/realization
    boundaries. The runtime lane above has since validated that explicit
    materialization boundary, but it does not make all strided/view-backed
    Vulkan kernels eligible.
  - Vulkan fixed-width complex LU/determinant/solve later landed with
    capability reporting, shader/helper status mapping, public routing, CPU
    oracle parity, singular diagnostics, placement/copyback checks,
    no-hidden-CPU-fallback checks, and host/container validation recorded in
    the current entry above.
  - This was a docs-only pass. No source/runtime/tests/C helpers were edited,
    and runtime behavior was not validated in this pass.

- Completed Vulkan fixed-width complex structural matrix kernels:
  - Current implementation status: Vulkan dense row-major `Complex128` and
    `Complex64` Tensor `matrix/transpose`, `matrix/diagonal`,
    `matrix/diagonal-matrix`, and `matrix/trace` support is landed behind
    explicit `matrix-structural-complex128` and
    `matrix-structural-complex64` operation capability bits.
  - Tensor-returning structural operations compute on Vulkan, preserve
    fixed-width complex dtype and placement, and require explicit
    `to-device 'cpu` for CPU inspection. `matrix/trace` computes on Vulkan and
    reads back only the fixed-width complex scalar required by the public scalar
    return contract.
  - Preserved boundary at this checkpoint: CUDA structural complex matrix
    kernels were still separate and later landed in the current CUDA
    structural entry above. CUDA/Vulkan complex numerical matrix kernels remain
    separate operation families and still fail closed unless a specific matrix
    operation has its own helper/shader/PTX ABI, backend contract/capability,
    status/error contract, and validation coverage.
  - Negative constraint: do not infer structural or numerical matrix support
    from complex storage, complex elementwise `map`, or complex `contract`, and
    do not lower complex backend matrix kernels through `Float64`, `Float32`,
    pointer-backed `BigComplex`, or hidden CPU fallback.
  - Validation: eight complex Vulkan structural shaders compiled with
    `glslangValidator` and passed `spirv-val`; Vulkan helper syntax check;
    `./scripts/build_omni_chelpers.sh`; `c3c build`; host focused
    `advanced-collections-module` `1449 passed, 0 failed`; bounded-container
    focused `advanced-collections-module` `1432 passed, 0 failed`; primitive
    docs parity, Stage 3 source parity, and targeted diff hygiene passed.
  - Current best next step: either add a CUDA structural matrix helper family
    for `matrix/transpose`, `matrix/diagonal`, `matrix/diagonal-matrix`, and
    `matrix/trace`, or choose a named complex numerical matrix family with
    explicit operation ABIs/status contracts.

- Completed CUDA/Vulkan fixed-width complex contract:
  - Current implementation status: CUDA and Vulkan dense row-major
    `Complex128` and `Complex64` Tensor `contract` support is landed behind
    explicit `contract-complex128` and `contract-complex64` operation
    capability bits.
  - CUDA routes supported single-axis rank-1/rank-2 fixed-width complex
    contractions through cuBLAS `Zgemm`/`Cgemm` and `Zgemv`/`Cgemv` without
    conjugation. Vulkan routes generic dense row-major rank-N complex
    contractions through native SPIR-V shaders.
  - The shipped contract computes supported contractions on the selected
    device, preserves fixed-width complex dtype and placement, follows public
    Tensor paired-axis / explicit left-right axis-list semantics, and requires
    explicit `to-device 'cpu` for CPU inspection.
  - Superseded stale wording that CUDA/Vulkan fixed-width complex `contract`
    remains fail-closed. Preserved boundary: fixed-width complex matrix
    kernels remain separate operation families and still fail closed unless a
    specific matrix operation has its own backend contract/capability. Complex
    matrix support is not implied by complex storage, complex elementwise
    `map`, or complex `contract`.
  - Negative constraint: do not lower complex backend contractions or future
    complex matrix kernels through `Float64`, `Float32`, pointer-backed
    `BigComplex`, or hidden CPU fallback.
  - Validation: complex Vulkan shaders compiled with `glslangValidator` and
    passed `spirv-val`; C helper syntax checks for CUDA and Vulkan;
    `./scripts/build_omni_chelpers.sh`; `c3c build`; host focused
    `advanced-collections-module` `1438 passed, 0 failed`.
  - Current best next step: split remaining complex GPU work into explicit
    matrix-operation slices, starting with a structural matrix family or
    another named operation family, rather than keeping a combined
    contract/matrix backlog item.

- Completed CUDA fixed-width complex elementwise map:
  - Added generated CUDA C/PTX-backed `Complex128` and `Complex64` binary,
    unary, and component-to-real kernels plus status-bearing native launchers.
  - Routed CUDA fixed-width complex `map` through explicit capability bits
    (`elementwise-map-complex128` and `elementwise-map-complex64`) so storage
    capability no longer implies operation capability. Runtime C3 routing now
    fails closed as `tensor/backend-unsupported` when the optional complex map
    helper is unavailable.
  - Supported CUDA complex map operations are binary `+`, `-`, `*`, `/`;
    unary `abs`, unary `-`, identity/`+`, `real-part`, `imag-part`, and
    `conjugate`. Generic map preserves complex dtype, including
    zero-imaginary component/magnitude results where applicable.
  - Direct CUDA complex `real-part`, `imag-part`, and `abs` return
    component-width real CUDA tensors (`Float64` for `Complex128`, `Float32`
    for `Complex64`); direct unary `-` and `conjugate` preserve complex dtype.
  - CUDA complex division by zero maps through native status to
    `tensor/domain-error`; nonrepresentable complex results map to invalid
    argument before result exposure. Empty CUDA complex unary/component maps
    accept null zero-byte device handles.
  - Preserved boundary: CUDA/Vulkan fixed-width complex `contract` and matrix
    kernels remain fail-closed and must get separate helper ABIs/capability
    bits before being claimed.
  - Validation: CUDA PTX generation and `ptxas` for
    `csrc/tensor_cuda_complex_map.cu`; C helper syntax check;
    `./scripts/build_omni_chelpers.sh`; `c3c build`; host focused
    `advanced-collections-module` `1433 passed, 0 failed`; bounded container
    focused `advanced-collections-module` `1416 passed, 0 failed`; primitive
    docs parity; Stage 3 source parity; targeted diff check.

- Completed Vulkan fixed-width complex elementwise map:
  - Added dedicated Vulkan `Complex128` and `Complex64` binary, unary, and
    component-to-real shaders plus checked-in SPIR-V C sources for dense
    row-major tensors.
  - Routed Vulkan complex `map` for `+`, `-`, `*`, `/`, unary `+`, `abs`,
    unary `-`, `real-part`, `imag-part`, and `conjugate` through explicit
    helper entrypoints. Tensor/scalar, scalar/tensor, tensor/tensor, and
    right-aligned singleton-axis broadcast forms preserve Vulkan placement.
  - Direct Vulkan complex `abs`, `real-part`, and `imag-part` now return
    component-width real Vulkan tensors; direct unary minus and `conjugate`
    preserve the complex dtype and placement.
  - `tensor-backends` now reports Vulkan `elementwise-map-complex128` and
    `elementwise-map-complex64` as operation capability bits independently
    from raw `complex128`/`complex64` storage bits. Historical note:
    CUDA complex map was still false at this checkpoint, but this is
    superseded by the later CUDA complex map checkpoint below.
  - Division uses a shader status buffer: zero denominators map to structured
    `tensor/domain-error`, and non-representable complex results fail before
    exposing output.
  - Historical negative memory, now partially superseded: do not infer CUDA
    complex map from storage bits or from the Vulkan complex helper. The later
    CUDA checkpoint landed generated PTX plus status ABI; complex contract and
    matrix kernels remain separate operation families.
  - Validation: six Vulkan complex shaders compiled with `glslangValidator`
    and passed `spirv-val`; helper rebuild passed; `c3c build` passed; host
    focused `advanced-collections-module` passed `1415 passed, 0 failed`;
    bounded-container focused `advanced-collections-module` passed
    `1398 passed, 0 failed`; primitive docs parity passed; targeted diff
    hygiene passed.

- Completed TENSOR-100F read-only Tensor transpose views:
  - Added `TENSOR_PAYLOAD_VIEW` and `TensorVal.view_source`.
  - Added public `matrix/transpose-view` for CPU rank-2 Tensor sources. It
    swaps logical shape/strides, borrows source storage, keeps the source as an
    ownership edge, is immutable, and reports `tensor-layout` payload `view`,
    owner `view-source`, `owns-storage` false, and write-policy
    `read-only-view`.
  - CPU `ref`, flat `(Array view)`, flat `(List view)`, and CPU `realize`
    materialization observe transposed logical indexing, including views over
    materialized lazy sources that cross return/closure boundaries.
  - Boundary copy, ESCAPE promotion, graph audit, provenance, and JIT temp-lane
    walkers now traverse `view_source`.
  - Existing `matrix/transpose` remains materializing for concrete tensors but
    composes structurally when the input is already a transpose view.
  - Writes through the view fail closed. CUDA/Vulkan placement and
    device-destination `realize` reject view payloads before hidden
    materialization, including dense double-transpose views; post-realization
    backend copies require concrete zero-offset dense row-major storage.
  - This first public view contract is CPU-only. Vulkan/device views remain
    deferred until a helper ABI explicitly accepts offset, stride, backing
    extent, alias, and write-policy metadata.
  - Negative memory: do not treat `matrix/transpose-view` as arbitrary
    `as-strided`/slice/diagonal view support or as GPU view execution support.
    The shipped operation is a CPU-readable read-only transpose view with
    fail-closed backend copy/kernel behavior.
  - Validation: `c3c build --obj-out obj` passed; direct smokes passed for
    `tensor-layout` view metadata, CPU `ref`, `(Array view)`, `(List view)`,
    CPU `realize`, double-transpose structural composition, return/closure
    capture, and read-only destination rejection; host focused
    `advanced-collections-module` passed `pass=1343 fail=0`;
    bounded-container focused `advanced-collections-module` passed
    `pass=1326 fail=0`; bounded-container `memory-lifetime-smoke` passed
    `pass=229 fail=0`.

- Completed TENSOR-100F Tensor layout metadata:
  - Added storage-offset and backing-extent metadata to `TensorVal`
    (`storage_offset`, `storage_element_count`, and `storage_byte_len`) and
    initialized the fields for newly allocated concrete Tensor payloads.
  - Lazy expression payloads now expose zero storage extent until realization,
    while retaining logical `element_count` and `byte_len` metadata.
  - Concrete Tensor clones reset to compact storage metadata, and device
    payload clone eligibility now fails closed through zero-offset device
    storage validation.
  - Tightened Tensor metadata/storage validation so concrete backing byte
    extent must cover `storage_element_count * dtype_size`, and CUDA/Vulkan
    device helpers reject nonzero storage offsets until offset-aware kernels
    exist.
  - CPU/device copy paths now require zero-offset dense row-major storage
    before raw contiguous copies.
  - Added public `tensor-layout`, registered it for interpreter and AOT lookup,
    and documented its metadata dictionary fields: `dtype`, `device`,
    `payload`, `layout`, `dense-row-major`, `shape`, `strides`, `rank`,
    `element-count`, `byte-length`, `storage-offset`, `storage-elements`,
    `storage-bytes`, `is-view`, `owns-storage`, `owner`, and `write-policy`.
  - At this metadata-only checkpoint, value domains were payload
    `concrete`/`map`/`contract`, layout `dense-row-major`/`strided`, owner
    `self`/`view-source`/`expression`, and write-policy
    `mutable`/`immutable`/`mutable-view`/`read-only-view`. The later
    transpose-view documentation checkpoint above adds payload `view`.
  - Added focused advanced stdlib assertions for CPU dense tensors, rank-0
    tensors, zero-size tensors, lazy map payload metadata, and CUDA/Vulkan
    copied dense metadata when those backends are available.
  - Preserved boundary at that checkpoint: this slice was metadata-only and
    did not ship a public view constructor, recursive/view-backed GPU kernels,
    or relaxed CUDA/Vulkan dense row-major preconditions. The later
    `matrix/transpose-view` slice adds a CPU-readable view constructor, but
    dense GPU kernels still require zero-offset dense row-major storage.
  - Negative memory: do not treat `tensor-layout` or `matrix/transpose-view`
    as GPU view execution support, and do not pass offset/stride metadata into
    GPU helpers until a view-aware helper ABI plus CPU oracle and alias/bounds
    tests exist.
  - Validation: `c3c build --obj-out obj` passed; direct `tensor-layout`
    smokes passed for dense, rank-0, lazy map, and non-Tensor fail-closed
    behavior; host focused `advanced-collections-module` passed
    `pass=1332 fail=0`; bounded-container focused `advanced-collections-module`
    passed `pass=1315 fail=0`; bounded-container `memory-lifetime-smoke`
    passed `pass=229 fail=0`; primitive docs parity, Stage 3 source parity,
    and targeted diff hygiene passed.

- Completed TENSOR-100F Vulkan dtype-changing Tensor rounding:
  - Added dedicated Vulkan `Float64` and `Float32` rounding shaders plus
    checked-in SPIR-V C sources for dense row-major `floor`, `ceiling`,
    `round`, and `truncate` into temporary signed 64-bit integer storage.
  - Vulkan backend probing now records `shaderInt64`, enables it during device
    creation when available, and reports a dedicated `rounding-big-integer`
    capability. Generic Vulkan `available`/`float64`/`float32` remains
    insufficient proof of dtype-changing rounding support.
  - Direct Tensor rounding that touches Vulkan now uses a status-bearing helper
    and materializes CPU `Tensor BigInteger` output after checked host copyback.
    Vulkan BigInteger storage is not claimed.
  - Advanced stdlib module coverage now validates Vulkan `Float64`/`Float32`
    `floor`, `ceiling`, `round`, and `truncate` when
    `rounding-big-integer` is true, while keeping fail-closed assertions for
    backends without that key.
  - Negative memory: do not add `floor`, `ceiling`, `round`, or `truncate` to
    same-dtype Vulkan float unary/map opcode tables. Tensor rounding is
    dtype-changing and must go through the `rounding-big-integer` result path.
  - Validation: Vulkan shader `glslangValidator` and `spirv-val` passed for
    both rounding shaders; helper rebuild passed; `c3c build --obj-out obj`
    passed; direct Vulkan smokes returned CPU `Tensor BigInteger` results for
    `Float64` floor/ceiling and `Float32` round/truncate; overflow reports
    `floor: tensor integer result out of supported range`; `map floor` remains
    Vulkan backend-unsupported; host focused advanced collections
    `pass=1326 fail=0`; bounded-container focused advanced collections
    `pass=1309 fail=0`; primitive docs parity, Stage 3 source parity, and
    targeted diff hygiene all passed.

- Completed TENSOR-100F CUDA dtype-changing Tensor rounding:
  - Added a dedicated CUDA rounding kernel source/PTX include plus helper
    loader/launcher entrypoints that compute dense row-major `Float64` and
    `Float32` `floor`, `ceiling`, `round`, and `truncate` on CUDA into
    temporary fixed-width integer device storage, propagate status, and copy
    checked `int64` results back to host.
  - Routed direct Tensor rounding primitives that touch CUDA through the new
    helper and materialize native CPU `Tensor BigInteger` results. Current
    `BigInteger` Tensor storage remains CPU pointer-owned, so the CUDA result
    contract is compute-on-CUDA plus explicit integer copyback, not
    CUDA-resident BigInteger storage.
  - Added the CUDA backend capability key `rounding-big-integer`; focused
    tests gate on that capability, require `BigInteger` dtype, and validate
    `Float64`/`Float32` values after explicit `to-device 'cpu` copyback.
  - Negative memory: do not add `floor`, `ceiling`, `round`, or `truncate` to
    the same-dtype CUDA unary map opcode table. Tensor rounding is
    dtype-changing, and generic CUDA availability is not enough to prove this
    result path.
  - Validation: CUDA direct smokes passed for `floor`, `ceiling`, `round`, and
    `truncate`; CUDA non-finite status returns the Tensor integer range
    diagnostic; `map floor` remains CUDA backend-unsupported; helper rebuild,
    `c3c build --obj-out obj`, host focused advanced collections
    `pass=1322 fail=0`, bounded-container focused advanced collections
    `pass=1305 fail=0`, docs parity, Stage 3 source parity, and targeted diff
    hygiene all passed.

- Superseded TENSOR-100F CUDA dtype-changing Tensor rounding test/docs
  preparation:
  - Added focused advanced stdlib module assertions for direct `floor`,
    `ceiling`, `round`, and `truncate` on dense row-major CUDA-placed
    `Float64` and `Float32` Tensor inputs, gated by the CUDA backend
    `rounding-big-integer` capability bit.
  - The staged assertions require `Tensor BigInteger` dtype and verify values
    after explicit `to-device 'cpu` copyback with `ref`, `Array`, and `List`.
  - This preparation record is superseded by the runtime landing above. Do not
    implement rounding as same-dtype CUDA float map opcodes.
  - Negative-memory note: do not gate these tests on ordinary CUDA
    `available`/`float64`/`float32`; a local focused run with those gates
    enabled failed because CUDA visibility does not prove the dtype-changing
    `Tensor BigInteger` result path exists. Gate on a dedicated
    `rounding-big-integer` capability bit or equivalent runtime proof.
  - Validation for this preparation checkpoint: `c3c build --obj-out obj`
    passed with existing deprecation warnings; host focused
    `advanced-collections-module` passed `pass=1322 fail=0`; targeted
    `git diff --check` passed.

- TENSOR-100F Vulkan `Float64` `stats/normal-quantile` checkpoint:
  - Dense row-major Vulkan `Float64` Tensor `stats/normal-quantile` now routes
    through a dedicated status-bearing inverse-CDF shader/helper. Public direct
    Tensor unary math and `map stats/normal-quantile` preserve Vulkan placement
    and `Float64` dtype for valid probabilities.
  - The shader avoids unavailable Vulkan 1.0 double `log` by inverting the
    landed Float64 normal-CDF polynomial approximation with bounded bisection.
    Status semantics match CUDA/Vulkan Float32: status `1` reports finite
    probability outside `0 < p < 1`, status `2` reports non-finite input, and
    nonzero status destroys the output before surfacing a result.
  - Added advanced stdlib module coverage for direct and mapped values at
    `0.025`, `0.5`, and `0.975`, probability `0`/`1` domain diagnostics, and
    non-finite status priority. The implementation does not downcast Float64 to
    Float32 and does not route through hidden CPU fallback.
  - Validation passed: `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_normal_quantile_f64.comp -o
    /tmp/omni_tensor_vulkan_normal_quantile_f64.spv`; `spirv-val --target-env
    vulkan1.0 /tmp/omni_tensor_vulkan_normal_quantile_f64.spv`;
    `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
    Vulkan direct/map Float64 quantile smokes plus domain/non-finite smokes;
    host focused `advanced-collections-module` `pass=1317 fail=0`;
    bounded-container focused `advanced-collections-module` `pass=1300 fail=0`
    with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.

- TENSOR-100F Vulkan `Float64` `stats/normal-cdf` checkpoint:
  - Dense row-major Vulkan `Float64` Tensor `stats/normal-cdf` now routes
    through fixed op id `19` in the existing two-buffer Float64 unary
    shader/helper. Public direct Tensor unary math and
    `map stats/normal-cdf` preserve Vulkan placement and `Float64` dtype.
  - The shader uses a bounded piecewise degree-7 polynomial approximation over
    `|x| < 8`, applies CDF symmetry for negative inputs, and saturates outside
    that range. The approximation was sampled against the CPU double oracle
    with maximum absolute error below `6e-8`; checked-in tests use `1e-6`
    tolerance around `-1.0`, `0.0`, and `1.0`.
  - Preserved boundary: Vulkan `Float64` `stats/normal-quantile` remains
    fail-closed pending a separate double inverse-CDF/status policy. Do not
    infer Float64 quantile support from CDF support, do not downcast Float64
    tensors to Float32, and do not route Vulkan distribution operands through
    hidden CPU fallback.
  - Validation passed: `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_unary_f64.comp -o
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`; `spirv-val --target-env
    vulkan1.0 /tmp/omni_tensor_vulkan_map_unary_f64.spv`;
    `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
    Vulkan direct/map Float64 CDF smokes; Float64 quantile fail-closed smoke;
    host focused `advanced-collections-module` `pass=1313 fail=0`;
    bounded-container focused `advanced-collections-module` `pass=1300 fail=0`
    with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.

- TENSOR-100F Vulkan `Float32` `stats/normal-quantile` checkpoint:
  - Dense row-major Vulkan `Float32` Tensor `stats/normal-quantile` now routes
    through a dedicated status-bearing inverse-CDF shader/helper. The shader
    uses a three-binding ABI: input buffer, output buffer, and a `uint32`
    status buffer. Public direct Tensor unary math and
    `map stats/normal-quantile` preserve Vulkan placement and `Float32` dtype
    for valid probabilities.
  - Status semantics match the CUDA probability-domain ABI: raw status `0`
    means success, `1` means finite probability outside `0 < p < 1`, and `2`
    means non-finite probability. The shader uses `atomicMax`, so non-finite
    status takes priority over a domain endpoint in mixed invalid tensors.
    Nonzero status destroys the output buffer and maps to scalar-compatible
    diagnostics before a result Tensor is exposed.
  - Preserved boundary: Vulkan `Float64` `stats/normal-quantile` remains
    fail-closed pending a separate double inverse-CDF/status policy. Do not
    assign op `20` to the existing statusless Vulkan unary helper, downcast
    Float64 to Float32, or route invalid/unsupported inputs through CPU.
  - Test maintenance: the stale `map vulkan unsupported callable fails closed`
    assertion now uses `test_error_contains`, matching the lazy-map fail-closed
    contract. Deferred follow-up is recorded in `TODO.md` for the separate
    `handle` interaction where wrapping unsupported Vulkan `map floor` can
    report `stack overflow in handle body`.
  - Validation passed: `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_normal_quantile_f32.comp -o
    /tmp/omni_tensor_vulkan_normal_quantile_f32.spv`; `spirv-val
    --target-env vulkan1.0 /tmp/omni_tensor_vulkan_normal_quantile_f32.spv`;
    `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
    Vulkan valid/domain/non-finite/mixed-invalid and Float64 fail-closed
    smokes; host focused `advanced-collections-module` `pass=1311 fail=0`;
    bounded-container focused `advanced-collections-module` `pass=1298 fail=0`
    with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.

- TENSOR-100F CUDA `stats/normal-quantile` probability-status checkpoint:
  - CUDA dense row-major `Float64`/`Float32` Tensor
    `stats/normal-quantile` now routes through generated CUDA C/libdevice PTX
    scientific unary op id `20`, using libdevice `normcdfinv` after
    probability validation.
  - The CUDA scientific unary helper now has a typed device `uint32` status
    word for data-dependent distribution math. Status `0` means success,
    status `1` means probability outside `0 < p < 1`, and status `2` means
    non-finite input. The raw device word remains `0` on success, and higher
    status wins for mixed invalid tensors so non-finite input takes priority
    over a domain endpoint. Nonzero status maps to the scalar-compatible
    diagnostic before an output Tensor is exposed.
  - Public direct Tensor unary math and `map stats/normal-quantile` preserve
    CUDA placement and `Float64`/`Float32` dtype for eligible dense row-major
    CUDA tensors. Unsupported callables, mixed CPU/CUDA operands, unsupported
    layouts, and invalid probability inputs fail closed without CPU fallback.
  - Supersedes the earlier CUDA/CDF checkpoint statement that
    `stats/normal-quantile` was scalar-only or CUDA fail-closed. Superseded for
    Vulkan Float32 by the later Vulkan quantile checkpoint above. Vulkan
    Float64 quantile remains fail-closed until a separate double inverse-CDF
    status policy lands.
  - Validation passed: `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75
    csrc/tensor_cuda_scientific_unary.cu -o
    /tmp/omni_tensor_cuda_scientific_unary.ptx`;
    `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75
    /tmp/omni_tensor_cuda_scientific_unary.ptx -o
    /tmp/omni_tensor_cuda_scientific_unary.cubin`; helper C compile;
    `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
    CUDA valid/invalid probability smokes including non-finite and mixed
    invalid priority; direct Vulkan fail-closed smoke; host focused
    `advanced-collections-module` `pass=1307 fail=0`; bounded container
    focused `advanced-collections-module` `pass=1294 fail=0` with
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.

- TENSOR-100F Vulkan `map` / `minmax` preflight hardening checkpoint:
  - Vulkan public `map` lowering and lazy map realization now perform
    callable, recursive device-placement, dtype-family, and exact dtype
    preflight before resolving concrete Tensor storage. Mixed CPU/Vulkan lazy
    operands fail with `map: Vulkan operands must remain Vulkan-placed` before
    CPU lazy materialization can run.
  - Direct Tensor `min` / `max` now applies the same recursive Vulkan
    placement preflight before resolving operands, so map-backed min/max
    kernels do not materialize CPU lazy operands before rejecting mixed
    CPU/Vulkan execution.
  - Preserved boundary: no hidden CPU/GPU transfers, no mixed-device execution
    support, no generic Vulkan unary mode-3 revival, and no new Vulkan callable
    coverage. Unsupported Vulkan callables remain fail-closed.
  - Validation passed: `c3c build --obj-out obj`; direct Vulkan smokes for
    mixed CPU/Vulkan lazy `map`, unsupported binary Vulkan callable preflight,
