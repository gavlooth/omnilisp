# Scientific Numerics And Tensor Plan

Date: 2026-04-14
Workspace: `/home/christos/Omni`

## Active Direction

This operational note now defers Tensor surface authority to the integrated
repo plan:

- `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`
- `docs/areas/tensor-scientific.md`
- `docs/LANGUAGE_SPEC.md`
- `docs/reference/03-collections.md`

The older local `.agents/PLAN.md` statement that treated `linalg/matmul` as a
chosen public name is invalidated. The recovered and integrated decision is:

- `Tensor` is the rank-polymorphic scientific numeric aggregate.
- `map` is the canonical elementwise tensor operation, including scalar
  broadcast and right-aligned singleton-axis tensor-tensor broadcasting.
- Elementwise multiplication is `(map * a b)`, not `(* a b)`.
- `contract` is the canonical summed-axis tensor contraction operation.
- Rank-2 matrix multiplication maps to `(contract a b [1 0])`; the explicit
  `(contract a b [1] [0])` form remains accepted.
- Constructor dispatch is the canonical public materialization boundary:
  `Array`, `List`, `Dictionary`, and `Tensor` consume iterators or normalize
  compatible values through their constructors.
- `realize` is demoted to a low-level Tensor storage primitive for exact
  destination writes and compatibility with existing tests; it is not Omni's
  general lazy sequence abstraction.
- `matmul` is rejected as canonical because it is rank-2 biased and programming
  jargon; do not implement `linalg/matmul` as the normal public surface.
- Do not add public `TensorExpr`, `map-into`, backend-flavored operation names,
  `delay`/`force` promise surfaces, or implicit CPU/GPU transfers as
  first-surface semantics.

## Active Non-Docker Work Plan

The validation-image architecture fix is excluded from this plan. The current
non-Docker implementation order is:

1. Landed: Vulkan `map` and map-backed `min`/`max` preflight ordering now
   checks callable/device/dtype eligibility before concrete Tensor resolution
   can realize unsupported CPU operands. Mixed CPU/Vulkan lazy operands and
   unsupported Vulkan callables fail closed before hidden CPU materialization.
2. Landed: CUDA now proves the shared probability-domain status ABI for
   data-dependent unary math through `stats/normal-quantile`: device status
   `0` means success, `1` means probability outside `0 < p < 1`, and `2` means
   non-finite input, with whole-operation failure before result exposure.
3. Landed: CUDA `stats/normal-quantile` now uses generated CUDA C/libdevice PTX
   op `20` for dense row-major `Float64`/`Float32` Tensor storage, preserving
   CUDA placement and dtype for direct and `map` paths.
4. Landed: Vulkan `Float32` `stats/normal-quantile` now uses a separate
   status-bearing same-dtype unary helper. Vulkan `Float64` stays separate; do
   not downcast or assign op `20` to the existing statusless two-buffer Vulkan
   unary helper.
5. Landed: Vulkan `Float64` `stats/normal-cdf` now uses a documented
   piecewise polynomial double approximation in the existing Float64 unary
   shader/helper, preserving Vulkan placement and dtype without hidden CPU
   fallback or Float32 downcast.
6. Landed: Vulkan `Float64` `stats/normal-quantile` now uses a Float64
   status-bearing inverse-CDF helper. Direct and `map` paths preserve Vulkan
   placement and `Float64` dtype for valid probabilities, and invalid
   probability/non-finite status maps to the scalar-compatible diagnostics
   before output exposure.
7. Landed: CUDA-first dtype-changing Tensor rounding uses a dedicated
   status-bearing CUDA helper and a native CPU `Tensor BigInteger` result path.
   Direct `floor`, `ceiling`, `round`, and `truncate` over dense row-major
   CUDA `Float64`/`Float32` inputs are gated by `tensor-backends`
   `rounding-big-integer`; do not add these operations as same-dtype GPU unary
   map opcodes.
8. Landed: Vulkan Tensor rounding now uses dedicated `shaderInt64`-gated
   status/copyback helpers for dense row-major `Float64`/`Float32` inputs and
   materializes CPU `Tensor BigInteger` output behind the Vulkan
   `rounding-big-integer` capability.
9. Landed: add Tensor view/layout representation metadata through the public
   `tensor-layout` primitive. The returned dictionary covers storage offset,
   physical strides, concrete backing extent/capacity, owner/alias metadata,
   and write-safety policy. Lazy expression payloads report zero storage
   extent until realization.
10. Landed: define and implement the narrow public read-only transpose-view
   contract through `matrix/transpose-view`. CPU `ref`, `Array`, `List`, and
   `realize` materialization observe the view; GPU/copy-kernel paths fail
   closed for view payloads.
11. Landed: unsupported Vulkan `map` callable errors propagate through
   `handle` as structured `tensor/backend-unsupported` raises for direct
   `map`, lazy Vulkan map sources, and one-argument `realize` of unsupported
   lazy Vulkan map expressions. The fix did not widen Vulkan `floor` or
   rounding coverage through generic `map`. The same runtime pass fixed
   compiled primitive `ERROR` argument propagation and increased normal
   StackCtx budget so handled CPU lazy Tensor map realization succeeds on small
   tensors instead of reporting `stack overflow in handle body`.
12. Landed: define native CPU fixed-width complex scalar and Tensor semantics
   for `Complex128` and `Complex64`, then add CUDA/Vulkan raw storage
   placement and explicit CPU copyback behind `complex128`/`complex64`
   capability bits. Vulkan dense row-major fixed-width complex elementwise
   `map` now has explicit `elementwise-map-complex128` /
   `elementwise-map-complex64` operation bits and preserves Vulkan placement
   for supported arithmetic/component operations. CUDA dense row-major
   fixed-width complex elementwise `map` now uses the same operation bits with
   generated PTX/status helpers. CUDA/Vulkan dense row-major fixed-width
   complex `contract` now has explicit `contract-complex128` /
   `contract-complex64` operation bits. Vulkan dense row-major fixed-width
   complex structural matrix kernels now have explicit
   `matrix-structural-complex128` / `matrix-structural-complex64` operation
   bits for `matrix/transpose`, `matrix/diagonal`, `matrix/diagonal-matrix`,
   and `matrix/trace`. CUDA dense row-major fixed-width complex structural
   matrix kernels now support the same public family behind those operation
   bits. Vulkan fixed-width complex `matrix/inverse`, `matrix/rank`, direct
   `matrix/norm` reducers, `matrix/qr`, and `matrix/cholesky` also landed
   behind the same numerical capability bits. Fixed-width complex numerical
  matrix kernels beyond landed Vulkan
  LU/determinant/solve/inverse/rank/norm/singular-values/QR/Cholesky and
  direct Vulkan general `matrix/eigenpairs` remain behind explicit
  backend/result-contract gates. Full fixed-width complex `matrix/svd` factor
  output remains separate because it needs complex `u`/`v` tensors and
  component-width real `s`.
  Remaining fixed-width complex closure is now split into
  `TENSOR-100H-SVD-FACTORS`, `TENSOR-100H-CUDA-SVD-NORMS`, and
  `TENSOR-100H-COMPLEX-EIGEN`; see
  `docs/plans/fixed-width-complex-closure-plan-2026-04-18.md`.
13. Landed: Vulkan materialization for direct rank-2 `matrix/transpose-view`
   operands where explicit Vulkan placement/realization/copyback needs dense
   storage. Keep this narrower than arbitrary strided GPU execution: broader
   view-aware Vulkan kernels still require an ABI for offset, strides, backing
   extent, owner/aliasing, and write policy.
14. Landed: Vulkan fixed-width complex `matrix/lu`, `matrix/determinant`, and
   `matrix/solve` for dense row-major zero-offset `Complex128`/`Complex64`
   tensors behind `matrix-numerical-complex128` and
   `matrix-numerical-complex64`; host and bounded-container focused validation
   passed.
15. Treat large-`k` SVD and large-`n` symmetric eigen work as measurement-led
   performance lanes. Do not replace the current correctness-first Vulkan
   paths without checked-in measurements showing the bottleneck.

Negative constraints:

- Do not restart the rolled-back generic Vulkan unary `map` mode-3 branch.
- Do not implement GPU quantile by assigning an opcode without per-element
  probability-domain status propagation.
- Do not implement GPU rounding as same-dtype float map output.
- Do not regress CUDA rounding into staged-only coverage; `rounding-big-integer`
  is shipped only as a direct dtype-changing CUDA helper plus CPU
  `Tensor BigInteger` materialization.
- Do not treat generic Vulkan `available`, `float64`, or `float32` capability
  as proof of dtype-changing rounding. Vulkan direct rounding must report
  `rounding-big-integer` and return CPU `Tensor BigInteger`; same-dtype Vulkan
  float rounding remains invalid.
- Do not add stride parameters to GPU helpers before Tensor values can
  represent storage offset, backing extent, and alias ownership.
- Do not claim broad view-backed GPU support from `tensor-layout` or
  `matrix/transpose-view`; the shipped Vulkan view materialization slice is
  limited to direct rank-2 transpose views and dense GPU kernels still require
  zero-offset dense row-major storage unless a
  later helper explicitly accepts view metadata. The active Vulkan
  transpose-view materialization lane creates dense Vulkan storage for the
  explicit materialization boundary, but it must not be described as general
  stride-aware GPU kernel execution.
- Do not lower pointer-backed `BigComplex` into Vulkan storage for
  `matrix/eigenpairs`.
- Do not infer CUDA/Vulkan complex kernel support from `complex128` or
  `complex64` storage capability bits. Those bits mean raw placement/copyback
  only; operation-family bits must be reported separately. Vulkan complex map
  is now reported through `elementwise-map-complex128` /
  `elementwise-map-complex64`; CUDA complex map is now reported through the
  same bits when the generated PTX/status helper is usable. Complex
  contract bits are landed; CUDA/Vulkan structural complex matrix bits are
  landed; Vulkan complex LU/determinant/solve/inverse/rank/norm/
  singular-values/QR/Cholesky bits are landed. Remaining complex numerical
  matrix bits must stay false until real kernels land.

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
    reuse or CPU fallback.
  - Superseded by later SVD-backed and CPU factor/SVD checkpoints:
    CPU and Vulkan-placed dense row-major `Float32` now support SVD-backed
    `matrix/norm` selectors through dtype-specific paths.
  - Validation: shader compile/`spirv-val`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct CPU/Vulkan smokes, host focused
    `advanced-collections-module` passed `1098/0`, bounded-container focused
    `advanced-collections-module` passed `1085/0`, primitive docs parity,
    Stage 3 source parity, and targeted `git diff --check`.
- 2026-04-17 Vulkan large-`k` SVD/singular-values checkpoint:
  - `matrix/singular-values` and direct `matrix/svd` no longer rely on fixed
    `k <= 64` private Gram/eigenvalue arrays. Both shaders now keep Gram
    scratch in Vulkan storage-buffer payload space behind the public output
    buffers, preserving public shapes and Vulkan placement.
  - Public Vulkan `matrix/norm` `'spectral` and `'nuclear` inherit the
    expanded singular-value helper and continue to read back only scalar norm
    results.
  - The shipped boundary is semantic large-`k` support with 32-bit shader index
    guards and no hidden CPU/LAPACK fallback; performance-oriented tiled or
    multi-dispatch large-`k` algorithms remain a separate measured TODO.
  - Validation so far: shader compile/`spirv-val`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, host focused `advanced-collections-module`
    passed `999/0`.
- 2026-04-17 Vulkan status-readback helper factoring checkpoint:
  - Shared `Float64` status-payload copy/readback boilerplate now covers the
    compatible mapped-status paths while preserving distinct status-domain
    mappers.
  - Generic trailing status, SVD/singular-values status, and symmetric-eigen
    status remain semantically separate. Parallel solve remains separate by
    design because it uses a typed `uint32_t` status buffer, not the trailing
    `double` payload convention.
  - LU metadata validation now reuses the generic trailing-status mapper while
    keeping pivot and swap-count validation LU-specific.
  - Validation: `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
    host focused `advanced-collections-module` passed `995/0`,
    bounded-container focused `advanced-collections-module` passed `982/0`,
    primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check`.
- 2026-04-17 Vulkan serial dispatch helper factoring checkpoint:
  - The mature serial Vulkan dispatch helper family now shares sequential
    storage-buffer descriptor layout/allocation/update helpers and a standard
    one-time command recording/submission helper, while preserving descriptor
    binding order, synchronous submit/wait behavior, output ownership, result
    placement, and no hidden CPU/LAPACK fallback.
  - Later checkpoint above factored compatible `Float64` status-readback
    boilerplate. Parallel solve remains separate by design because it uses a
    typed `uint32_t` status-buffer ABI.
  - Validation: `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
    host focused `advanced-collections-module` passed `995/0`, bounded-container
    focused `advanced-collections-module` passed `982/0`, primitive docs
    parity, Stage 3 source parity, and targeted `git diff --check`.
- 2026-04-17 Vulkan unary identity and helper factoring checkpoint:
  - Public unary `+` is now a one-or-two-argument dispatched primitive:
    `(+ scalar)` and `(+ tensor)` are identity, while `(+ a b c)` remains an
    arity error. The no-auto-partial regression moved to `*`, which remains
    binary-only.
  - Dense row-major Vulkan `Float64` `(map + tensor)` routes through the
    separate unary helper identity opcode and returns a Vulkan-placed Tensor.
    Direct `(+ <vulkan-tensor>)` preserves placement by identity.
  - The mature serial Vulkan dispatch helper family now shares
    `omni_tensor_vulkan_create_compute_pipeline_for_shader`; later checkpoint
    above also factored descriptor and command-buffer plumbing. Status-copy
    factoring remains open under `TENSOR-100F`.
  - Validation: shader compile/`spirv-val`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, focused `advanced-unicode-iterator` passed
    `159/0`, focused `advanced-collections-module` passed `972/0`, bounded
    container focused `advanced-collections-module` passed `959/0`, primitive
    docs parity, Stage 3 source parity, and targeted `git diff --check`.
- 2026-04-17 direct Vulkan `matrix/svd` factor-output checkpoint:
  - Added `csrc/tensor_vulkan_svd_f64.comp`, generated
    `csrc/tensor_vulkan_svd_f64_spv.c`, and wired
    `omni_tensor_backend_vulkan_svd_f64` through the helper build and project
    sources.
  - Public `matrix/svd` now routes dense row-major Vulkan `Float64` rank-2
    tensors through Vulkan before CPU fallback, including lazy Vulkan inputs,
    and returns Vulkan-placed `u`, `s`, and `v` tensors with shapes
    `[rows k]`, `[k]`, and `[cols k]`. A later checkpoint removed the
    original `k <= 64` private-array cap with storage-buffer Gram scratch.
  - Direct and lazy Vulkan SVD assert unchanged `LAPACKE_dgesvd` counters;
    wrong rank and unsupported Vulkan cases fail closed with Tensor backend
    diagnostics.
  - Negative constraint: do not restore a second private `double[4096]`
    eigenvector array in the shader. That approach compiled but failed at
    runtime on this Vulkan stack with `tensor/backend-execution-failed`; the
    shipped shader stores the driving eigenvectors in the relevant output
    buffer.
  - Validation: shader compile/`spirv-val`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, focused `advanced-collections-module` passed
    `945/0`, JSON/shell syntax checks, and `git diff --check` passed.
- 2026-04-17 direct Vulkan symmetric eigen checkpoint:
  - Added `csrc/tensor_vulkan_symmetric_eigen_f64.comp`, generated
    `csrc/tensor_vulkan_symmetric_eigen_f64_spv.c`, and wired
    `omni_tensor_backend_vulkan_symmetric_eigen_f64` through the helper build
    and project sources.
  - Public `matrix/eigenvalues` and `matrix/eigenvectors` now route dense
    row-major Vulkan `Float64` square tensors through Vulkan before CPU
    fallback, including lazy Vulkan inputs, and return Vulkan-placed values and
    aligned vector columns. The helper uses storage-buffer scratch, so
    symmetric dense row-major Vulkan `Float64` square inputs support `n > 64`
    within helper resource limits, 32-bit shader index guards, and the Jacobi
    iteration guard.
  - Nonsymmetric Vulkan input raises `tensor/not-symmetric`; wrong shape,
    unsupported layout, unsupported dtype, missing Vulkan/Float64 capability,
    and unsupported resource bounds fail closed with Tensor backend
    diagnostics. General `matrix/eigenpairs` remains fail-closed on Vulkan
    while its public result contract is pointer-backed `BigComplex`.
  - Direct and lazy Vulkan symmetric eigen paths assert unchanged
    `LAPACKE_dsyev` counters.
  - Validation so far: shader compile/`spirv-val`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct runtime smokes, and focused
    `advanced-collections-module` passed `948/0`.
- 2026-04-17 audit-hardening implementation checkpoint:
  - Generic Vulkan contract failures now report
    `contract: Vulkan Float64 contract kernel failed`, not the stale
    single-axis message.
  - Public tests now cover zero-size Vulkan spectral/nuclear matrix norms for
    empty rows and empty columns with unchanged `dgesvd` counter checks.
  - Direct and lazy Vulkan fail-closed paths for `matrix/eigenpairs` have
    unchanged LAPACK counter guards plus diagnostic-order tests for
    invalid-but-Vulkan operands; direct and lazy Vulkan `matrix/svd`,
    `matrix/eigenvalues`, and `matrix/eigenvectors` now route through their
    Vulkan factor/eigen paths.
  - Internal lazy-contract helper tests now cover zero-axis and multi-axis
    Vulkan contractions in addition to the existing rank-1 dot and mixed-device
    rejection coverage.
  - Validation: `c3c build --obj-out obj`, focused
    `advanced-collections-module` passed `935/0`, targeted `git diff --check`,
    primitive docs parity, and Stage 3 source parity passed.
- 2026-04-17 planning checkpoint:
  - Added dedicated planning notes for direct Vulkan `matrix/svd`,
    `matrix/eigenvalues` / `matrix/eigenvectors` / `matrix/eigenpairs`, and
    future `Float32` Tensor/Vulkan execution:
    `docs/plans/vulkan-svd-factor-output-plan-2026-04-17.md`,
    `docs/plans/vulkan-eigensolver-plan-2026-04-17.md`, and
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`.
  - Implementation state has since changed for direct `matrix/svd` and direct
    symmetric eigen: `matrix/svd` supports `k > 64` through storage-backed Gram
    scratch, and `matrix/eigenvalues` / `matrix/eigenvectors` support `n > 64`
    through storage-buffer scratch within helper resource limits, 32-bit shader
    index guards, and the Jacobi iteration guard. Vulkan `Float32`
    placement/copyback, destination `realize`, dense row-major `map`, unary
    helpers, direct `min`/`max`, rank-N `contract`, structural matrix kernels
    (`transpose`, `diagonal`, `diagonal-matrix`, `trace`), direct
    `matrix/rank`, and direct `matrix/norm` selectors default/`'frobenius`,
    `'one`, `'infinity`, `'max`, SVD-backed outputs, serial factor/solve
    surfaces, staged parallel solve, large-dense SVD robustness, and CPU
    factor/SVD contracts have since landed. CUDA `Float32`
    placement/copyback, destination `realize`, and eligible cuBLAS contract
    routing have also landed; scalar runtime values are covered by the later
    scalar `Float32` checkpoint, supported CUDA zero-size contract
    identity/fill is covered by the later CUDA zero-size checkpoint, and CUDA
    rank-1/rank-1 dot is covered by the later dot checkpoint. Remaining
    adjacent work is fixed-width complex, stride/view-backed Vulkan layouts,
    and CUDA `map`.
  - Stale planning text that still described spectral/nuclear Vulkan norms or
    zero-axis Vulkan contract as unsupported was normalized.
- 2026-04-17 audit continuation:
  - Lazy `TENSOR_PAYLOAD_CONTRACT` realization now resolves operands on their
    existing device before CPU fallback. Contracts with lazy Vulkan operands
    can route through Vulkan/CUDA backend contract helpers, while unresolved
    non-CPU operands fail closed with `tensor/backend-unsupported` instead of
    reaching CPU-only expression evaluation.
  - Focused regressions cover the public eager Vulkan map/contract route plus
    internal harness construction of lazy map/contract payloads that exercise
    `tensor_contract_try_device_value`, including the mixed CPU/Vulkan
    rejection path.
  - `omni_tensor_backend_vulkan_singular_norm_f64` now checks Vulkan
    availability, Float64 support, and the shared singular-values shape
    validator before the zero-size/zero-output success return.
  - Backlog state now has one live TODO: `TENSOR-100F`. `TENSOR-100E` is
    closed as the correctness-first Vulkan baseline, and `TENSOR-100G` is
    closed as the measured parallel-solve baseline.
  - Validation: `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
    host focused `advanced-collections-module` passed `913/0`, primitive docs
    parity, Stage 3 source parity, and targeted `git diff --check` passed.
- `TENSOR-100F1` destination-realize contract is now split by destination
  placement. CPU destinations still reject device sources unless the caller
  explicitly uses `to-device 'cpu`; existing dense row-major CUDA and Vulkan
  `Float64` destinations accept matching CPU sources, same-backend sources,
  supported lazy same-backend results, and scalar fills through the same
  backend-neutral `(realize source destination)` API. Regressions cover
  CPU/CUDA/Vulkan destination copies, supported lazy CUDA/Vulkan sources,
  unsupported CUDA `map` fail-closed behavior, cross-backend rejection, and the
  preserved device-result-into-CPU fail-closed behavior. Validation passed:
  helper rebuild, `c3c build --obj-out obj`, direct smokes, host focused
  `advanced-collections-module` `959/0`, bounded-container focused
  `advanced-collections-module` `946/0`, primitive docs parity, Stage 3 source
  parity, and targeted `git diff --check`.
- `TENSOR-100E/F` now includes a separate dense row-major Vulkan `Float64`
  unary helper for absolute value, negation, square root, and real-valued
  component operations. Public
  `(map abs <vulkan-tensor>)`, direct Tensor `(abs <vulkan-tensor>)`,
  public `(map - <vulkan-tensor>)`, and direct Tensor
  `(- <vulkan-tensor>)`, public `(map sqrt <vulkan-tensor>)`, and direct
  Tensor `(sqrt <vulkan-tensor>)`, plus public `(map real-part ...)`,
  `(map imag-part ...)`, `(map conjugate ...)`, and direct Tensor
  `real-part` / `imag-part` / `conjugate` route through the two-buffer SPIR-V
  helper for real-valued Vulkan `Float64` tensors and return Vulkan-placed
  Tensor results. Scalar unary `-` is now reachable through the
  documented public surface by registering `-` as one-or-two argument
  primitive behavior, with `prim_sub` still rejecting extra arguments. CPU
  direct Tensor unary `-` preserves native Tensor dtype for `Float64`,
  `BigInteger`, `BigFloat`, and `BigComplex`. Direct Tensor unary math on
  Vulkan now fails closed for unsupported operations instead of materializing
  hidden CPU results. The old generic Vulkan unary `map` mode-3 branch remains
  invalidated and unsupported; unsupported unary callables such as `sin` still fail closed with
  `tensor/backend-unsupported`. Validation: shader compile/validate passed,
  helper rebuild passed, `c3c build --obj-out obj` passed, direct smokes
  returned `-1`, `2.5`, `2.0`, `vulkan`, `-1.5`, `3.0`, `vulkan`, `2.0`,
  `vulkan`, `-2.5`, `0.0`, `vulkan`, and `tensor/backend-unsupported`;
  host focused `advanced-collections-module` passed `836/0`,
  bounded-container focused `advanced-collections-module` passed `823/0`, and
  static parity/diff checks passed.
- `TENSOR-100E/F` now also routes public Tensor `min` / `max` and
  `map min` / `map max` for dense row-major Vulkan `Float64` tensors through
  the existing binary map helper. Tensor/scalar, scalar/Tensor, and
  Tensor/Tensor broadcast forms remain Vulkan-placed; mixed CPU/Vulkan
  operands and non-`Float64` Vulkan dtypes fail closed. Validation:
  `csrc/tensor_vulkan_map_f64.comp` compiled and validated as Vulkan 1.0
  SPIR-V, helper rebuild passed, `c3c build --obj-out obj` passed, direct
  smokes returned `-2.0`, `0.0`, `vulkan`, and
  `tensor/backend-unsupported`, host focused `advanced-collections-module`
  passed `846/0`, and bounded-container focused `advanced-collections-module`
  passed `833/0`.
- `TENSOR-100F1` now includes dense row-major Vulkan `Float64`
  `matrix/cholesky` through the public backend-neutral `matrix/cholesky`
  surface. The shipped boundary is: Vulkan-placed square `Float64` input,
  exact symmetry/SPD failure mapped to `tensor/not-positive-definite`,
  lower-triangular result remains Vulkan-placed, lazy Vulkan input is realized
  without CPU fallback, empty square tensors preserve shape, and the Vulkan
  path does not call LAPACK. The slice also factored shared trailing-status
  copyback for serial Vulkan matrix helpers and fixed the CPU pure Cholesky
  fallback for empty square tensors. Validation: SPIR-V compile/validate
  passed, helper rebuild passed, `c3c build --obj-out obj` passed, direct
  Vulkan smokes returned `1.0`, `2.0`, `0.0`, `vulkan`, `vulkan`, `0`,
  `tensor/not-positive-definite`, `tensor/not-positive-definite`, and
  `tensor/shape-mismatch`, direct CPU empty Cholesky shape returned `0`, host
  focused `advanced-collections-module` passed `775/0`, and bounded-container
  focused `advanced-collections-module` passed `762/0`.
- `TENSOR-100F1` now also includes dense row-major Vulkan `Float64`
  `matrix/qr` through the public backend-neutral `matrix/qr` surface. The
  shipped boundary is: Vulkan-placed rank-2 `Float64` input with rows greater
  than or equal to columns, reduced `q` and `r` factor Tensors remain
  Vulkan-placed, rank-deficient input maps to `tensor/singular-matrix`, lazy
  Vulkan input is realized without CPU fallback, empty-column tensors preserve
  shape, and the Vulkan path does not call LAPACK. Validation:
  SPIR-V compile/validate passed, helper rebuild passed, `c3c build --obj-out
  obj` passed, direct Vulkan smokes returned `1.0`, `1.0`, `vulkan`,
  `vulkan`, `0`, and `tensor/singular-matrix`, and host focused
  `advanced-collections-module` passed `787/0`, bounded-container focused
  `advanced-collections-module` passed `774/0`, and static parity/diff checks
  passed.
- First `TENSOR-100G` checkpoint now routes Vulkan dense row-major `Float64`
  `matrix/solve` systems with `n >= 3` through a separate
  `omni_tensor_backend_vulkan_solve_parallel_f64` helper. The helper uses a
  64-invocation single-workgroup partial-pivot Gaussian elimination shader,
  parallelizes pivot reduction, row swaps, elimination, and RHS-column
  back-substitution, keeps `n < 3` on the existing serial Vulkan helper, uses
  a private typed `uint` status buffer, and preserves the public
  `matrix/solve` result contract. Validation so far: SPIR-V compile/validate
  passed, helper rebuild passed, `c3c build --obj-out obj` passed, direct
  Vulkan smokes returned `0.2`, `1.0`, and `tensor/singular-matrix`, and host
  focused `advanced-collections-module` passed `793/0`.
- Staged `TENSOR-100G` checkpoint now records two compute dispatches in one
  Vulkan command buffer for parallel `matrix/solve`: init/copy first, then
  factor/back-solve after an explicit buffer-memory barrier on output and
  private status buffers. The init stage dispatches over
  `max(n*n, n*rhs_cols)` work items, so coefficient/RHS copies are not capped
  at one 64-lane workgroup. Validation passed: both staged shaders compiled and
  validated as Vulkan 1.0 SPIR-V, helper rebuild passed, `c3c build --obj-out
  obj` passed, direct smokes returned `0.2`, `1.0`, `18.0`, and
  `tensor/singular-matrix`, host focused `advanced-collections-module` passed
  `798/0`, bounded-container focused `advanced-collections-module` passed
  `785/0`, and static parity/diff checks passed.
- Advanced `TENSOR-100G` checkpoint now records a multi-dispatch Vulkan
  `matrix/solve` command buffer: init/copy, per-pivot pivot/swap,
  per-pivot elimination, and descending per-row backsolve. Elimination and RHS
  backsolve can span multiple workgroups, while pivot selection/swap remains
  one workgroup until the cross-workgroup pivot-reduction scratch-buffer design
  lands. The public `matrix/solve` surface is unchanged, the serial `n < 3`
  Vulkan helper still owns its trailing `double` status sentinel, and the
  parallel helper still uses private typed `uint` status storage. Validation
  passed: pivot/eliminate/backsolve shaders compiled and validated as Vulkan
  1.0 SPIR-V, helper rebuild passed, `c3c build --obj-out obj` passed, direct
  smokes returned `0.2`, `3.0`, `18.0`, dense 9x9 value `1.0`, and
  `tensor/singular-matrix`, host focused `advanced-collections-module` passed
  `799/0`, bounded-container focused `advanced-collections-module` passed
  `786/0`, and static parity/diff checks passed.
- Advanced `TENSOR-100G` checkpoint now includes cross-workgroup pivot
  reduction. Pivot selection is split into scan, ping-pong reduce, commit, and
  row-swap shader stages. The helper binds a private pivot-magnitude scratch
  buffer plus expanded typed `uint` status scratch, recursively reduces pivot
  candidates with barriers, then swaps rows before elimination. Validation
  passed: all affected solve-multi shaders compiled and validated as Vulkan
  1.0 SPIR-V, helper rebuild passed, `c3c build --obj-out obj` passed, direct
  smokes returned `0.2`, `3.0`, `1.0` for the 65x65 cross-workgroup case, and
  `tensor/singular-matrix`, host focused `advanced-collections-module` passed
  `800/0`, bounded-container focused `advanced-collections-module` passed
  `787/0`, and static parity/diff checks passed.
- Advanced `TENSOR-100G` checkpoint now includes tiled LU staging as a
  per-pivot panel-factor stage. The helper binds a private factor scratch
  buffer, dispatches a factor kernel after row swap and before elimination,
  stores `L[row,pivot]` in scratch and the lower-triangular workspace, and has
  elimination consume staged factors. Validation passed: all affected
  solve-multi shaders compiled and validated as Vulkan 1.0 SPIR-V, helper
  rebuild passed, `c3c build --obj-out obj` passed, direct smokes returned
  dense 9x9 value `1.0`, 65x65 identity value `1.0`, dense 65x65 `I + J`
  value `1.0`, and `tensor/singular-matrix`, host focused
  `advanced-collections-module` passed `801/0`, bounded-container focused
  `advanced-collections-module` passed `788/0`, and static parity/diff checks
  passed.
- Advanced `TENSOR-100G` checkpoint now includes measured threshold routing.
  Local in-process measurements in `build/vulkan_solve_threshold_20260417_*`
  found `3`, `9`, `16`, and `32` tied or serial-favorable while `65` was the
  first tested size where the staged parallel path won across identity and
  dense fixtures. Public `matrix/solve` now keeps Vulkan dense row-major
  `Float64` systems with `n < 65` on the serial helper and routes `n >= 65`
  through the staged parallel helper. A private serial solve counter was added
  so tests prove direct serial execution below the threshold. Validation
  passed: helper rebuild, `c3c build --obj-out obj`, direct smokes returned
  `9.0`, `1.0`, `1.0`, and `tensor/singular-matrix`, host focused
  `advanced-collections-module` passed `806/0`, bounded-container focused
  `advanced-collections-module` passed `793/0`, and static parity/diff checks
  passed.
- `TENSOR-100E` now includes Vulkan dense row-major rank-N `Float64`
  zero-axis and multi-axis `contract` through the public backend-neutral
  `contract` surface. The shipped Vulkan contract boundary is: zero or more
  contracted axis pairs, output axes ordered as free left axes followed by free
  right axes, results remain Vulkan-placed, and callers copy back explicitly
  with `to-device 'cpu`. Stride-aware/non-contiguous layouts remain fail-closed
  until layout/aliasing metadata is explicit.
  Follow-up zero-axis validation passed with direct smokes `60.0`, `12.0`,
  `vulkan`, and `0`, host focused `advanced-collections-module` `851/0`, and
  bounded-container focused `advanced-collections-module` `838/0`.
  Validation:
  SPIR-V compile/validate passed, helper rebuild passed, `c3c build --obj-out
  obj` passed, direct Vulkan smokes returned `70.0`, `210.0`, `460.0`, `8.0`,
  and `tensor/backend-unsupported`, host focused `advanced-collections-module`
  passed `689/0`, and bounded-container focused `advanced-collections-module`
  passed `676/0`.
- `ADV-STACK-001` calibrated the macro-hygiene non-tail recursion headroom
  probe to depth `512`.
- Treat the old `896`/`1200` depths as invalidated portability assumptions on
  this ARM64 workspace: host validation crashed at `640`/`896`, while the
  bounded validation container crashed at `1200`.
- The fixed checkpoint is validated by host and bounded-container
  `advanced-macro-hygiene-string-number` pass=9 fail=0, host and
  bounded-container `advanced-macro-hygiene` pass=83 fail=0, and bounded
  container full `advanced` pass=1928 fail=0.

## Scalar Scientific Backend Direction

Default scalar numerics direction from the owner remains:

- Avoid GSL as the baseline scientific stack.
- Prefer Boost.Multiprecision for exact/high-precision scalar values.
- Prefer Boost.Math for scalar scientific functions behind an owned C++ shim
  with a stable C ABI.
- Keep GNU precision libraries only as possible later performance backends if
  the owner explicitly accepts that dependency path.
- Keep user-facing scalar names backend-neutral: `Integer`, `BigInteger`,
  `Float64`, `BigFloat`, optional `Complex`, and `BigComplex`.

Minimal scalar-first targets:

- `BigInteger` through Boost.Multiprecision `cpp_int` is now the first landed
  scalar slice: constructor, printing/String conversion, `Number` identity,
  `+`/`-`/`*` overflow promotion, `/`, `%`, ordering comparisons,
  `abs`, `min`, `max`, `gcd`, `lcm`, bitwise operations, equality/hash
  support, `parse-number` decimal overflow promotion, and scope-boundary
  cloning are implemented.
- `BigFloat` through Boost.Multiprecision `cpp_dec_float_50` is now the first
  high-precision decimal slice: constructor, printing/String conversion,
  `Number` identity, `Float64`/`Integer` narrowing, `+`, `-`, `*`, `/`,
  comparisons, `abs`, `min`, `max`, equality/hash support, scope-boundary
  cloning, `parse-number` floating overflow promotion, BigFloat-preserving
  scalar math for trig, inverse trig, hyperbolic, exponential/logarithmic,
  power/root, gamma/error-function, standard-normal helpers, and exact
  BigFloat `floor`/`ceiling`/`round`/`truncate` to `Integer` or `BigInteger`
  are implemented. Precision-control APIs remain a separate follow-up.
- `BigComplex` is now the first high-precision complex scalar slice:
  constructor, printing/String conversion, `Number` identity, `+`, `-`, `*`,
  `/`, unary `-`, equality/hash support, `zero?`, `abs` to `BigFloat`,
  scope-boundary cloning/promotion, fail-closed ordered operations, and
  BigComplex-preserving `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`,
  `cosh`, `tanh`, `exp`, `log`, `log10`, `sqrt`, and `pow` are implemented.
  `real-part`, `imag-part`, and `conjugate` are implemented as numeric
  primitives. Broader special functions and distributions remain separate
  follow-ups.
- `math/lgamma`, `math/erf`, `math/erfc`, `stats/normal-cdf`, and
  `stats/normal-quantile` are now validated Boost.Math wrapper slices: C++ shim
  status codes, finite-input/range/domain error mapping, primitive
  registration, AOT lookup, focused numeric tests, and direct runtime smokes
  are implemented.
- `math/erf`, `math/erfc`, and `stats/normal-cdf` are no longer scalar-only
  for Tensor inputs: CPU Tensor elementwise support and CUDA dense row-major
  `Float64`/`Float32` opcode support are implemented. Rounding primitives
  remain dtype-changing `BigInteger` Tensor operations, so they are not CUDA
  same-dtype map opcodes.
- `stats/normal-cdf` and `stats/normal-quantile` are intentionally
  one-argument standard-normal helpers. Mean/stddev parameters or other
  distribution families need separate surface decisions.

## Tensor Backend Direction

OpenBLAS/LAPACK/LAPACKE should integrate as optional backend acceleration, not
as a new public Tensor surface:

- The pure C3 Tensor fallback remains the semantic oracle.
- Ordinary Tensor storage remains C3-native and scoped; do not replace normal
  Tensor storage with `ForeignHandle`.
- BLAS rank-2 `Float64` GEMM now optimizes all contiguous row-major single-axis
  rank-2 contract layouts behind `realize` when dtype, rank, layout, device,
  and aliasing match the backend contract: `[1 0]`, `[0 0]`, `[1 1]`, and
  `[0 1]`.
- BLAS `Float64` GEMV now optimizes contiguous row-major rank-2/rank-1 and
  rank-1/rank-2 single-axis contract layouts behind the same backend boundary.
- BLAS `Float64` DDOT now optimizes contiguous rank-1/rank-1 vector dot
  contractions behind the same backend boundary.
- BLAS `Float64` DGER now optimizes contiguous rank-1/rank-1 outer-product
  contractions behind the same backend boundary.
- `matrix/solve` is the first public rank-2 Tensor solver surface. It uses a
  pure Float64 runtime solver for square coefficient tensors and rank-1/rank-2
  RHS tensors; singular systems raise `tensor/singular-matrix`.
- `matrix/solve` now has optional runtime-loaded `LAPACKE_dgesv` acceleration
  when `liblapacke` is available; missing backend support retains the pure
  solver fallback.
- `matrix/solve` and `matrix/inverse` now have forced-fallback coverage
  through a private test-only `dgesv` disable hook. The `dgesv` backend path
  has its own disable state and no longer depends on the unrelated `dgeev`
  test hook.
- `matrix/lu` is the first public rank-2 Tensor decomposition surface. It uses
  pure partial-pivot LU factorization for square `Float64` tensors, returns
  combined factor, final row-order pivots, and swap-count metadata, and raises
  `tensor/singular-matrix` for singular inputs.
- `matrix/determinant` is the first decomposition consumer. It reuses the pure
  LU kernel, returns a `Float64` scalar, returns `0.0` for singular matrices,
  and preserves the empty square determinant convention `1.0`.
- `matrix/transpose` is the first dtype-preserving structural matrix transform.
  It transposes rank-2 Tensor values, returns shape `[columns rows]`, and
  preserves native dtype across `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex`.
- `matrix/diagonal` is the second dtype-preserving structural matrix transform.
  It extracts the main diagonal from rank-2 Tensor values into a rank-1 Tensor
  of length `min(rows, columns)`, preserving native dtype across `Float64`,
  `BigInteger`, `BigFloat`, and `BigComplex`.
- `matrix/diagonal-matrix` is the first dtype-preserving structural matrix
  constructor. It builds a square rank-2 Tensor from rank-1 Tensor values,
  clones high-precision diagonal handles, and fills off-diagonal cells with
  the dtype's zero value.
- `matrix/identity` is the first size-driven structural matrix constructor. It
  builds square identity Tensors from non-negative integer sizes, defaults to
  `Float64`, and supports optional `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex` dtype selection.
- `matrix/trace` is the first dtype-preserving structural matrix reduction. It
  sums the diagonal of square rank-2 Tensor values and returns a scalar in the
  Tensor's native numeric family: `Double` for `Float64`, or `BigInteger`,
  `BigFloat`, and `BigComplex` for matching high-precision Tensor dtypes.
- `matrix/rank` is the first numerical structural matrix reduction. It
  computes rectangular rank-2 `Float64` Tensor numerical rank as an `Integer`,
  realizes lazy inputs, supports empty axes, and accepts an optional
  non-negative finite tolerance.
- `matrix/rank` now uses optional runtime-loaded `LAPACKE_dgesvd`
  acceleration when available, counting singular values above tolerance while
  retaining the pure row-echelon fallback.
- `matrix/norm` is the first general matrix norm reducer. It returns
  `Float64` norms for rank-2 `Float64` Tensor values, realizes lazy inputs,
  supports empty axes, defaults to Frobenius, and accepts explicit
  `'frobenius`, `'one`, `'infinity`, `'max`, `'spectral`, and `'nuclear`
  selectors. The spectral and nuclear selectors reuse the existing SVD
  pure/LAPACK machinery.
- `matrix/norm` spectral and nuclear selectors now have explicit backend and
  forced-fallback coverage through a private test-only `dgesvd` disable hook,
  preserving the same public results whether runtime LAPACK is available or
  the pure SVD fallback is used.
- `matrix/rank`, `matrix/singular-values`, and `matrix/svd` now also have
  forced-fallback coverage through the private `dgesvd` disable hook, so the
  SVD-backed matrix lane validates both accelerated and pure fallback
  semantics for shipped public contracts.
- `matrix/inverse` is the first solve-derived matrix transform surface. It
  returns a same-shape `Float64` Tensor inverse for nonsingular square rank-2
  `Float64` tensors, raises `tensor/singular-matrix` for singular inputs, and
  uses optional runtime-loaded `LAPACKE_dgesv` acceleration when available.
- `matrix/lu` and `matrix/determinant` now have optional runtime-loaded
  `LAPACKE_dgetrf` acceleration when `liblapacke` is available; missing backend
  support retains the pure LU fallback and existing public contracts.
- `matrix/lu` and `matrix/determinant` now also have forced-fallback coverage
  through a private test-only `dgetrf` disable hook, preserving public results
  through the pure partial-pivot LU fallback when runtime `dgetrf` is disabled.
- `matrix/qr` is the first non-LU decomposition surface. It computes reduced
  QR for full-column-rank rank-2 `Float64` tensors with rows greater than or
  equal to columns and returns `q`/`r` factors.
- `matrix/qr` now has optional runtime-loaded `LAPACKE_dgeqrf` plus
  `LAPACKE_dorgqr` acceleration when `liblapacke` is available; missing
  backend support retains the pure QR fallback and existing reduced-factor
  orientation.
- `matrix/qr` now also has forced-fallback coverage through a private test-only
  QR backend disable hook, preserving public results through the pure reduced
  QR fallback when runtime QR LAPACK is disabled. The existing tolerance-based
  LAPACK QR rank guard remains in place.
- `matrix/cholesky` is the first symmetric positive-definite decomposition
  surface. It computes a lower-triangular `Float64` factor for square
  symmetric positive-definite rank-2 `Float64` tensors and raises
  `tensor/not-positive-definite` for nonsymmetric or non-positive-definite
  inputs.
- `matrix/cholesky` now has optional runtime-loaded `LAPACKE_dpotrf`
  acceleration when `liblapacke` is available; missing backend support retains
  the pure Cholesky fallback and the exact symmetry precheck.
- `matrix/cholesky` now also has forced-fallback coverage through a private
  test-only `dpotrf` disable hook, preserving public results through the pure
  lower-triangular Cholesky fallback when runtime `dpotrf` is disabled.
- `matrix/svd` is the first rectangular decomposition surface. It computes a
  reduced SVD for rank-2 `Float64` tensors, supports rank-deficient inputs, and
  returns `u` shape `[rows k]`, `s` shape `[k]`, and `v` shape `[columns k]`,
  where `k = min(rows, columns)`.
- `matrix/svd` now has optional runtime-loaded `LAPACKE_dgesvd` acceleration
  when `liblapacke` is available; missing backend support retains the pure
  Gram/Jacobi SVD fallback and existing reduced-factor contract.
- `matrix/singular-values` exposes the `s` part of the reduced SVD contract
  directly as a rank-1 `Float64` Tensor and uses the same optional
  `LAPACKE_dgesvd` acceleration and pure fallback as `matrix/svd`.
- `matrix/eigenvalues` and `matrix/eigenvectors` are the first eigen surfaces.
  They intentionally use a symmetric-real contract for square rank-2
  `Float64` tensors. Nonsymmetric matrices raise `tensor/not-symmetric`.
- `matrix/eigenvalues` and `matrix/eigenvectors` now have optional
  runtime-loaded `LAPACKE_dsyev` acceleration when `liblapacke` is available;
  missing backend support retains the pure symmetric Jacobi fallback and the
  exact symmetry precheck.
- `matrix/eigenvalues` and `matrix/eigenvectors` now have forced-fallback
  coverage through a private test-only `dsyev` disable hook, preserving the
  same symmetric-real public contract when runtime LAPACK is disabled.
- `matrix/eigenpairs` is the chosen general nonsymmetric eigen surface. It
  returns aligned `BigComplex` `values` and `vectors` tensors for square
  rank-2 `Float64` input, realizes lazy input, supports empty square matrices,
  and uses runtime `LAPACKE_dgeev` when available with a pure fallback
  otherwise. The pure no-`dgeev` fallback is covered on 2x2 real/complex
  cases and targeted 3x3 diagonal, upper-triangular, and
  real-plus-complex-block cases, including representative residual checks for
  `A*v ~= lambda*v`. The accelerated `LAPACKE_dgeev` path is also covered by
  representative real and complex residual checks when the backend is
  available. Both backend and forced-fallback paths include a non-normal
  upper-triangular residual check for a non-basis vector column. The focused
  advanced collections/module suite also has reusable 3x3 residual helpers
  that validate all returned columns for representative non-normal and
  real-plus-complex-block matrices under backend and forced-fallback execution.
  The pure fallback now treats isolated 2x2 real-Schur blocks as converged,
  fixing a bounded-container-only trailing-real-eigenvalue corruption in the
  3x3 real-plus-complex-block case.
- `(Array tensor)` and `(List tensor)` are implemented as explicit Tensor
  collection conversions that materialize lazy Tensor expressions and return flat
  row-major values.
- `(Tensor iterator)`, `(Tensor iterator dtype)`, and
  `(Tensor dtype shape iterator)` are now the priority constructor-dispatch
  materialization path for finite numeric iterators; this replaces treating
  `realize` or a Clojure-style `force` as the public lazy boundary.
- `(Iterator tensor)` now exposes flat row-major Tensor elements through the
  canonical iterator constructor surface, matching `(Array tensor)` and
  `(List tensor)`. Non-CPU device tensors still require explicit
  `to-device 'cpu` before iteration.
- `(Tensor data)`, `(Tensor data Float64)`, and `(Tensor Float64 data)` now infer
  native `Float64` tensor shape from real numeric scalars or rectangular nested
  arrays/proper lists whose leaves can narrow to finite `Float64`, while
  preserving the explicit `(Tensor Float64 shape data-or-scalar)` constructor.
- Native `BigInteger` Tensor storage now supports constructor, `dtype`, `ref`,
  flat `(Array tensor)` / `(List tensor)` conversion, concrete `realize`,
  tensor-dispatched `map`, and pure C3 `contract` kernels. BigInteger Tensor
  data must be exact integers.
- Native `BigComplex` Tensor storage now supports constructor, `dtype`, `ref`,
  flat `(Array tensor)` / `(List tensor)` conversion, concrete `realize`,
  tensor-dispatched `map`, and pure C3 `contract` kernels. Real numeric leaves
  promote to zero-imaginary BigComplex elements.
- BigComplex Tensor component kernels now support `real-part`, `imag-part`,
  and `conjugate`. Component extraction returns BigFloat tensors; conjugate
  returns BigComplex tensors.
- Real Tensor component semantics are dtype-preserving: `real-part` and
  `conjugate` copy Float64, BigInteger, and BigFloat tensors; `imag-part`
  returns same-shape zero tensors in the same dtype.
- Tensor `abs` now supports elementwise magnitude for all native Tensor dtypes:
  real Tensor dtypes preserve dtype and shape, while BigComplex tensors return
  same-shape BigFloat magnitude tensors.
- Tensor `sqrt` now supports elementwise square root for all native Tensor
  dtypes: Float64 and BigInteger tensors return same-shape Float64 tensors,
  while BigFloat and BigComplex tensors preserve dtype.
- Tensor unary scientific math now supports elementwise `sin`, `cos`, `tan`,
  `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, and `log10`
  for all native Tensor dtypes. Float64 and BigInteger tensors return
  same-shape Float64 tensors, while BigFloat and BigComplex tensors preserve
  dtype.
- Tensor `pow` now supports tensor-scalar, scalar-tensor, and broadcast
  tensor-tensor powers. BigComplex wins result dtype, then BigFloat, otherwise
  the result is a Float64 tensor.
- Tensor `atan2` now supports tensor-scalar, scalar-tensor, and broadcast
  tensor-tensor real-plane arctangent. BigFloat preserves dtype, other
  real/exact inputs return Float64 tensors, and complex operands fail closed.
- Tensor `floor`, `ceiling`, `round`, and `truncate` now return BigInteger
  Tensor results for real Tensor inputs. BigFloat tensors use exact rounding;
  BigComplex tensors fail closed.
- CUDA exposes direct dense row-major `Float64`/`Float32` Tensor rounding
  through `rounding-big-integer` and CPU `Tensor BigInteger` materialization.
  Vulkan rounding is not implied by Vulkan Float64/Float32 support; tests are
  staged to require a Vulkan `rounding-big-integer` capability before accepting
  BigInteger results and otherwise keep direct Vulkan rounding fail-closed.
- Native `BigFloat` concrete Tensor storage now supports constructor, `dtype`,
  `ref`, flat `(Array tensor)` / `(List tensor)` conversion, and concrete
  `realize`.
- Tensor-dispatched `map` now supports native `BigFloat` tensors for unary,
  tensor-scalar, scalar-tensor, exact-shape tensor-tensor, and right-aligned
  singleton-axis broadcast cases.
- Tensor-dispatched `contract` now supports native `BigFloat` tensors through
  the pure C3 contraction fallback. BLAS fast paths remain `Float64`-only.
- Tensor `min` and `max` now support native Tensor inputs with tensor-scalar,
  scalar-tensor, and broadcast tensor-tensor real comparison. BigFloat wins
  if either input is BigFloat, Float64 wins if either input is Float64, and the
  remaining exact inputs return native `BigInteger` Tensor results.
- Tensor `gcd` and `lcm` now support native Tensor inputs with tensor-scalar,
  scalar-tensor, and broadcast tensor-tensor exact integer semantics. Tensor
  operands must be native `BigInteger` Tensor storage; results are native
  `BigInteger` tensors. The working fast path borrows Tensor element handles
  and uses existing `i64`/borrowed BigInteger C ABI helpers for scalars; the
  manufactured scalar-handle variant was invalidated by corrupted
  tensor-scalar results.
- Unsupported strides, dtypes, aliasing, device placement, or missing libraries
  must fall back or fail deterministically without changing Tensor semantics.
- Direct native backend calls are preferred for hot Tensor kernels. User-facing
  FFI handles are not part of the normal Tensor object model.
- Backend modules should sit behind names such as `tensor/blas` and
  `tensor/lapack`; only genuinely opaque backend resources need explicit
  ownership/finalizer policy.
- `TENSOR-100` is closed as the explicit-device CUDA/cuBLAS design slice:
  future GPU support stays behind the existing `Tensor` value, uses
  `to-device`, `device`, and `tensor-backends`, rejects `GpuTensor`,
  `CudaTensor`, backend-flavored math names, and implicit CPU/GPU transfers,
  and requires deterministic ownership for CUDA buffers and cuBLAS handles.
- `TENSOR-100A` is implemented as the CPU-only placement/introspection slice:
  `device` reports `'cpu`; `to-device` with target `'cpu` realizes Tensor
  expressions to CPU Tensor storage; `to-device` with target `'cuda` fails
  closed with `tensor/backend-unavailable`.
- `TENSOR-100B` is implemented as the placement metadata/backend inventory
  slice: `TensorVal` stores placement and an opaque device handle/finalizer
  slot, `tensor-backends` reports structured CPU/CUDA availability, CPU kernels
  reject non-CPU storage, boundary clones refuse opaque non-CPU payloads until
  explicit copy semantics land, and `tensor_free_payload` owns opaque device
  handle release.
- `TENSOR-100C` is implemented as the optional CUDA copy slice:
  runtime-loaded `libcudart` support copies concrete `Float64` or `Float32`
  CPU Tensor storage to CUDA through `to-device 'cuda` and copies CUDA Tensor
  storage back through `to-device 'cpu`. CUDA availability requires a successful
  allocation/free probe, not just device-count success.
- `TENSOR-100D` is implemented as the cuBLAS rank-2 CUDA contract slice:
  runtime-loaded cuBLAS handle lifecycle is per operation, `tensor-backends`
  reports `cublas` with dtype capability bits, CUDA-placed dense row-major
  matching `Float64` or matching `Float32` rank-2/rank-2 single-axis
  contractions return CUDA-placed Tensors through dtype-specific GEMM for
  `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`, and rank-2/rank-1 plus rank-1/rank-2
  contractions route through dtype-specific GEMV. `Float32` cuBLAS capability
  is probed separately from `Float64` availability. Focused tests cover
  available paths, mixed-device and unsupported multi-axis diagnostics, forced
  cuBLAS-unavailable reporting, and fail-closed zero-size CUDA dimensions.
- `TENSOR-100E` is complete as the portable Vulkan backend baseline:
  `docs/plans/vulkan-backend-decision-2026-04-16.md` records the policy,
  `tensor-backends` reports a structured `vulkan` entry with explicit
  `float64` kernel capability, `to-device 'vulkan` copies concrete `Float64`
  CPU Tensor storage into opaque host-visible coherent Vulkan buffers when available,
  and `to-device 'cpu` copies Vulkan storage back to native CPU Tensor storage.
  Vulkan `map` now supports dense row-major `Float64` elementwise arithmetic
  `+`, `-`, `*`, and `/` for Tensor/scalar, scalar/Tensor, exact-shape
  Tensor/Tensor inputs, and right-aligned singleton-axis Tensor/Tensor
  broadcasting through public `map`, returning Vulkan-placed tensors.
  The same binary map helper supports `min` and `max` for dense row-major
  Vulkan `Float64` tensor/scalar, scalar/Tensor, and Tensor/Tensor broadcast
  cases, and direct public Tensor `min` / `max` route through it for eligible
  Vulkan operands.
  Vulkan `map` also supports real-valued component operations
  `real-part`, `imag-part`, and `conjugate` for dense row-major `Float64`
  inputs through the separate unary helper; direct Tensor component operations
  on Vulkan return Vulkan-placed `Float64` tensors.
  Public `contract` now supports Vulkan-placed dense row-major rank-N
  `Float64` tensors with zero or more explicit contracted axis pairs. Output
  axes are ordered as free left axes followed by free right axes. Results
  remain Vulkan-placed tensors. The generic contract kernel is generated from
  checked-in GLSL and uses rank/shape/stride/axis-list metadata-buffer dispatch.
  Public `matrix/transpose` supports Vulkan-placed dense row-major `Float64`
  rank-2 tensors and returns a Vulkan-placed transposed Tensor.
  Public `matrix/diagonal` and `matrix/diagonal-matrix` support Vulkan-placed
  dense row-major `Float64` inputs and return Vulkan-placed Tensor results.
  Public `matrix/trace` supports Vulkan-placed dense row-major square
  `Float64` inputs, computes on Vulkan, and reads back only the scalar result
  required by the scalar return contract.
  Public `matrix/rank` supports Vulkan-placed dense row-major `Float64` or
  `Float32` rank-2 inputs, computes on Vulkan, and reads back only the scalar
  `Integer` result required by the scalar return contract.
  Public `matrix/lu` supports Vulkan-placed dense row-major square `Float64`
  inputs, computes partial-pivot LU on Vulkan, returns the combined `lu`
  factor as a Vulkan-placed Tensor, and reads back only `pivots` plus
  `swap-count` dictionary metadata required by the public contract.
  Public `matrix/determinant` supports Vulkan-placed dense row-major square
  `Float64` inputs, computes on Vulkan, and reads back only the scalar
  `Float64` result required by the scalar return contract.
  Public `matrix/solve` supports Vulkan-placed dense row-major square
  `Float64` coefficient tensors with Vulkan-placed rank-1 or rank-2 `Float64`
  right-hand tensors, computes Gaussian elimination on Vulkan, returns
  Vulkan-placed solution tensors, and raises `tensor/singular-matrix` for
  singular systems.
  Public `matrix/inverse` supports CPU and Vulkan-placed dense row-major
  square `Float64`, `Float32`, `Complex128`, or `Complex64` inputs, computes
  Gauss-Jordan elimination, returns an inverse Tensor preserving input dtype
  and Vulkan placement for Vulkan inputs, and raises `tensor/singular-matrix`
  for singular inputs.
  Public `matrix/norm` supports Vulkan-placed dense row-major `Float64` and
  `Float32` inputs for default/`'frobenius`, `'one`, `'infinity`, `'max`,
  `'spectral`, and `'nuclear`. Public `matrix/singular-values`
  supports Vulkan-placed dense row-major rank-2 `Float64` and `Float32`
  inputs, computes on Vulkan with storage-buffer Gram scratch, returns a
  Vulkan-placed rank-1 Tensor preserving the input float dtype for
  `k = min(rows, columns)`, and keeps explicit CPU copyback as the inspection
  boundary. The singular-value shader maps backend non-convergence
  to `tensor/no-convergence`; the helper rejects logical matrices whose element
  count or scratch payload exceeds the shader's 32-bit index space before
  dispatch.
  Public `matrix/svd` supports Vulkan-placed dense row-major rank-2 `Float64`
  and `Float32` inputs, including lazy Vulkan inputs, computes reduced factor
  outputs on Vulkan with storage-buffer Gram scratch, returns Vulkan-placed
  `u`, `s`, and `v` tensors with the input float dtype and shapes
  `[rows k]`, `[k]`, and `[cols k]`, and keeps explicit CPU copyback as the
  inspection boundary.
  Direct destination `realize` is not an implicit GPU-to-CPU copy boundary:
  concrete or lazy Vulkan sources must be copied with explicit `to-device 'cpu`
  first and otherwise fail closed with `tensor/backend-unsupported`.
  Unsupported Vulkan dtypes, unsupported Vulkan map callables,
  incompatible map broadcast shapes, mixed CPU/Vulkan operands, and unsupported
  Vulkan layouts fail closed with Tensor backend diagnostics.
  Zero-size contracted axes in supported Vulkan layouts produce
  additive-identity output. The dtype/layout policy is now explicit: keep
  fixed-width dtype paths explicit, do not downcast between `Float64` and
  `Float32`, do not lower pointer-backed Big* Tensor dtypes to Vulkan, keep
  landed fixed-width complex backend support behind explicit capability bits,
  and defer full complex SVD factor output, CUDA complex singular-values/norm/SVD,
  complex eigen result contracts, and stride-aware layouts until their
  contracts exist.
- `dot` / `outer` helpers may be considered later as library conveniences over
  `contract`, but they should not replace the locked first Tensor surface.

## Current Next Checkpoint

Use the integrated Tensor plan, `TODO.md`, and
`docs/plans/vulkan-math-library-roadmap-2026-04-17.md` as source of truth
before changing code.

1. Continue broader Vulkan math-library work. `TENSOR-100G` now has measured
   routing for the current staged `Float64` parallel solve path; full blocked
   trailing-update LU is a future performance lane only if later measurements
   justify it.
2. Keep the current serial Vulkan solve/LU/inverse shaders as
   correctness-preserving small-system/backend paths. The panel-factor staged
   solve path is the current large-system candidate at `n >= 65`.
3. Do not continue from the rolled-back generic `map` mode-3 unary branch:
   GLSL double transcendental builtins failed to compile for that path, and an
   arithmetic-only unary variant compiled but still failed at runtime. If unary
   Vulkan map is resumed, prefer a separately debugged unary shader/helper
   entrypoint or a deeper descriptor/dispatch diagnosis.
4. Treat mature serial helper plumbing as closed for now: compute-pipeline,
   descriptor, one-time command, and compatible `Float64` status-readback
   boilerplate are shared. Do not reopen those as the neutral baseline unless
   new duplication appears in a different helper shape.
5. Treat Vulkan `Float32` as partially active, not as a `Float64` fallback.
   Native CPU/runtime `Tensor Float32` exists, and Vulkan `Float32`
   placement/copyback, destination `realize`, dense row-major `map`, unary
   helpers, direct `min`/`max`, rank-N dense row-major `contract`, structural
   matrix kernels (`transpose`, `diagonal`, `diagonal-matrix`, `trace`), direct
   `matrix/rank`, direct `matrix/norm` selectors, `matrix/singular-values`,
   `matrix/svd`, serial factor/solve surfaces (`matrix/determinant`,
   `matrix/lu`, `matrix/solve`, `matrix/inverse`, `matrix/cholesky`, and
   `matrix/qr`), staged parallel `matrix/solve`, capability reporting, and
   no-downcast tests, large-dense SVD robustness, and CPU `Float32`
   factor/SVD surfaces have landed. CUDA `Float32` placement/copyback,
   destination `realize`, eligible cuBLAS contract routing, supported
   zero-size CUDA contract identity/fill, and CUDA rank-1/rank-1 dot have also
   landed. CPU fixed-width complex scalar and Tensor storage for
   `Complex128`/`Complex64` has landed. Remaining stride/view-backed Vulkan
   coverage, CUDA `map`, CUDA/Vulkan fixed-width complex kernels, and direct
   Vulkan general `matrix/eigenpairs` still require dedicated contracts and
   validation before they can leave fail-closed behavior. Do not lower
   `BigInteger`, `BigFloat`, or `BigComplex` to Vulkan, and do not treat real
   `float32`/`float64` backend capability bits as complex capability.
6. Preserve `Tensor`, `map`, `contract`, `matrix/*`, `to-device`, `device`,
   and `tensor-backends` as the public surface. Do not add `VulkanTensor`,
   `GpuTensor`, `CudaTensor`, backend-flavored math names, hidden CPU/GPU
   transfers, or boundary cloning of opaque non-CPU handles without explicit
   copy ownership.
6. Extend Boost.Math or scalar precision lanes only when there is a concrete
   next scientific function, distribution family, or precision contract.

Do not start by binding GSL. Do not implement `linalg/matmul` as canonical.
