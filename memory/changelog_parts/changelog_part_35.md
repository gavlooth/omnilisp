# Memory Changelog Index Part 35

Source: `memory/CHANGELOG.md`

    and lazy Vulkan inputs through that helper before CPU fallback. Returned
    values and vector columns remain Vulkan-placed; CPU inspection still
    requires explicit `to-device 'cpu`.
  - `matrix/eigenpairs` remains fail-closed on Vulkan because its public output
    contract is pointer-backed `BigComplex`.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_symmetric_eigen_f64.comp -o /tmp/omni_tensor_vulkan_symmetric_eigen_f64.spv`
    - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_symmetric_eigen_f64.spv`
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - direct Vulkan smokes for values, vectors, device placement, and
      nonsymmetric `tensor/not-symmetric`
    - host focused `advanced-collections-module`: `pass=948 fail=0`
    - bounded container focused `advanced-collections-module`: `pass=935 fail=0`
    - `python3 -m json.tool project.json`
    - `bash -n scripts/build_omni_chelpers.sh`
    - `./scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - targeted `git diff --check`

- 2026-04-17 18:42 CEST: direct Vulkan large-`k` SVD/singular-values
  checkpoint:
  - `csrc/tensor_vulkan_singular_values_f64.comp` now uses hidden
    storage-buffer Gram scratch in the output payload instead of fixed private
    `double[4096]` / `double[64]` arrays.
  - `csrc/tensor_vulkan_svd_f64.comp` now uses hidden storage-buffer Gram
    scratch behind the public `u` payload and stores eigenvalues in the
    existing `s` payload, removing the original `k <= 64` private-array cap
    while preserving reduced `u`, `s`, and `v` output shapes.
  - `csrc/tensor_vulkan_helpers.c` removed the shared SVD/singular-values
    `k <= 64` validation, allocates hidden Gram scratch, and preserves 32-bit
    shader index/storage-count guards and no hidden CPU/LAPACK fallback.
  - Public `matrix/norm` `'spectral` and `'nuclear` inherit the expanded
    singular-values helper and continue to read back only scalar norm results.
  - Tests now cover `65x65` Vulkan singular-values, spectral/nuclear norms,
    and direct `matrix/svd` output placement/no-LAPACK behavior.
  - Deferred performance work is now explicit in `TODO.md`: measure and, if
    justified, replace the correctness-first storage-backed single-dispatch
    path with tiled Gram or staged Jacobi execution.
  - validation:
    - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_singular_values_f64.comp -o /tmp/omni_singular_values.spv`
    - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_svd_f64.comp -o /tmp/omni_svd.spv`
    - `spirv-val --target-env vulkan1.0 /tmp/omni_singular_values.spv`
    - `spirv-val --target-env vulkan1.0 /tmp/omni_svd.spv`
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - host focused `advanced-collections-module`: `pass=999 fail=0`
    - direct smokes: `65x65` all-ones Vulkan SVD singular value `65.0`; `65x65`
      zero SVD outputs `(vulkan vulkan vulkan 65)`
    - bounded container focused `advanced-collections-module`: `pass=986 fail=0`
    - `./scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - targeted `git diff --check`

- 2026-04-18 11:12 CEST: Vulkan unsupported map handler and lazy-realize stack
  checkpoint:
  - Unsupported Vulkan `map` callables still fail closed with
    `tensor/backend-unsupported`, but `handle` now receives the structured
    payload for direct `map`, nested lazy Vulkan map sources, and one-argument
    `realize` of unsupported lazy Vulkan map expressions.
  - Recoverable raises with no `data` construct their canonical payload
    directly in root scope before setting `raise_pending`, avoiding
    stack-heavy root promotion inside constrained handle body stacks.
  - Compiled one-argument and multi-argument primitive apply paths now
    propagate evaluated `ERROR` arguments before invoking the callee, so
    `realize` no longer masks source errors with Tensor type diagnostics.
  - Normal StackCtx usable size is now 256KB. This closes the independent
    handled CPU lazy Tensor map realization overflow on small tensors without
    changing Vulkan callable support.
  - Preserved boundary: Vulkan `floor`/rounding support was not widened
    through generic `map`, unsupported callables remain outside the Vulkan
    whitelist, and no hidden CPU/GPU fallback was added.
  - validation:
    - `c3c build --obj-out obj`
    - direct smokes for handled Vulkan direct/lazy-source `map`, direct and
      handled `realize`, handled CPU lazy-map `realize`, and destination-form
      `realize` source-error propagation
    - host focused `advanced-collections-module`: `pass=1352 fail=0`
    - host `advanced-effect-continuation`: `pass=56 fail=0`
    - host `advanced-effect-union-limit`: `pass=68 fail=0`
  - bounded-container focused `advanced-collections-module`: `pass=1335 fail=0`
  - stack suite: `pass=23 fail=0`
  - targeted `git diff --check`

- 2026-04-18 11:53 CEST: CPU fixed-width complex scalar and Tensor checkpoint:
  - Added native scalar `Complex128` and `Complex64` value families with
    constructors, stdlib predicates, dispatch/type integration, printing/String
    conversion, equality/hash support, AOT/literal serialization, and
    scope-boundary copy/promotion handling.
  - Added CPU Tensor `Complex128`/`Complex64` storage and oracle behavior for
    explicit and inferred construction, scalar fill, `ref`, `Array`/`List`
    conversion, `realize`, mixed fixed-complex `map` promotion, `contract`,
    `real-part`, `imag-part`, `abs`, `conjugate`, unary minus, and structural
    matrix operations (`matrix/transpose`,
    `matrix/diagonal`, `matrix/diagonal-matrix`, `matrix/identity`, and
    `matrix/trace`).
  - Preserved fail-closed policy for CUDA/Vulkan fixed-width complex placement
    and backend execution. Real `float64`/`float32` backend capability bits do
    not imply complex support; future GPU complex work needs explicit backend
    capability bits, device layout/copy semantics, status contracts, and
    kernels.
  - Updated the fixed-width complex contract note, language/reference docs,
    Tensor/Vulkan planning docs, TODO, and active plan wording so fixed-width
    complex CPU support is no longer described as deferred.
  - Validation:
    - `c3c build`
    - direct `--eval` smokes for scalar `String`, Tensor `ref`, component dtype,
      and Complex128 `contract`
    - host focused advanced scalar tests:
      `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-unicode-iterator ./build/main --test-suite lisp`
      (`180 passed, 0 failed`)
    - host focused advanced Tensor tests:
      `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
      (`1383 passed, 0 failed`)
    - bounded-container focused advanced Tensor tests:
      `OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
      (`1366 passed, 0 failed`)
    - targeted `git diff --check`
  - Current best next step: design and implement CUDA/Vulkan
    `Complex128`/`Complex64` backend capability and device-layout ABI for a
    first operation family, with the CPU Tensor oracle as the semantic baseline.

- 2026-04-18 12:13 CEST: CUDA/Vulkan fixed-width complex storage and copyback
  checkpoint:
  - Added separate CUDA/Vulkan storage dtype predicates so raw copy paths can
    accept `Complex128` and `Complex64` while existing real-only compute gates
    remain unchanged.
  - CUDA/Vulkan `to-device` placement now accepts concrete zero-offset dense
    row-major `Complex128` and `Complex64` CPU Tensor storage when the backend
    is available. Explicit `to-device 'cpu` copyback preserves dtype and
    values.
  - CUDA/Vulkan destination `realize` now accepts matching CPU or same-device
    complex Tensor storage and scalar fills for existing complex device
    destinations by staging fixed-complex values through raw host-to-device
    copy helpers.
  - `tensor-backends` reports CUDA/Vulkan `complex128` and `complex64` storage
    capability bits separately from real dtype and operation capability bits.
    Complex `elementwise-map-*` and `contract-*` bits remain false.
  - Preserved boundary: CUDA/Vulkan complex `map`, `contract`, and matrix
    kernels still fail closed. Do not infer kernel support from storage
    capability bits.
  - Validation:
    - `c3c build`
    - host focused `advanced-collections-module`: `pass=1399 fail=0`
    - bounded-container focused `advanced-collections-module`: `pass=1382 fail=0`
    - `./scripts/check_primitive_docs_parity.sh`
    - targeted `git diff --check`
  - Current best next step: choose and implement the first complex CUDA/Vulkan
    operation family, likely elementwise `map`, with explicit helper ABI,
    status/capability bits, and no hidden lowering through real or
    pointer-backed complex storage.

- 2026-04-18 13:10 CEST: CUDA fixed-width complex elementwise map
  documentation checkpoint:
  - Current implementation status: CUDA dense row-major `Complex128` and
    `Complex64` elementwise `map` exists behind
    `elementwise-map-complex128` and `elementwise-map-complex64` capability
    bits.
  - Supported CUDA complex map operations are binary `+`, `-`, `*`, `/`;
    unary `abs`, unary `-`, identity/`+`, `real-part`, `imag-part`, and
    `conjugate`. Generic map `real-part`, `imag-part`, and `abs` preserve the
    map dtype as complex with zero imaginary components where applicable.
  - Direct CUDA complex `real-part`, `imag-part`, and `abs` return
    component-width real CUDA tensors (`Float64` for `Complex128`, `Float32`
    for `Complex64`). Direct unary `-` and `conjugate` preserve complex dtype.
  - CUDA complex division by zero maps through native status to
    `tensor/domain-error`; nonrepresentable complex results map to invalid
    argument.
  - Superseded stale wording that CUDA complex `map` remains fail-closed.
    Preserved boundary: CUDA/Vulkan fixed-width complex `contract` and matrix
    kernels remain fail-closed.
  - Validation already passed: `./scripts/build_omni_chelpers.sh`; `c3c
    build`; host focused `env LD_LIBRARY_PATH=/usr/local/lib
    OMNI_LISP_TEST_SLICE=advanced
    OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main
    --test-suite lisp` (`1433 passed, 0 failed`); bounded container focused
    `advanced-collections-module` (`1416 passed, 0 failed`); CUDA PTX
    generation and `ptxas` for `csrc/tensor_cuda_complex_map.cu`; C helper
    syntax check; docs parity; Stage 3 source parity; targeted diff-check.

- 2026-04-19 23:52 CEST: Vulkan ML contract first-operation checkpoint:
  - Froze `ml/linear` as the first backend-neutral public ML operation name.
    The CPU implementation computes `input[..., in_features]` by
    `weights[out_features, in_features]` plus optional `bias[out_features]`,
    returning `input[..., out_features]`.
  - `ml/linear` currently supports dense row-major CPU `Float64` and
    `Float32` tensors. Non-CPU Tensor expressions fail closed before
    realization, preserving the no-hidden-CPU-fallback Vulkan contract.
  - `tensor-backends` now reports `ml-linear` true for CPU and false for
    Vulkan, CUDA, and cuBLAS until backend kernels land.
  - AOT primitive lookup now includes `ml/linear`.
  - Preserved boundary: this is not a Vulkan ML kernel. Vulkan `ml-linear`
    remains unsupported and capability-gated false.

- 2026-04-20 00:04 CEST: ML linear audit hardening checkpoint:
  - Added regression coverage for `ml/linear` CPU lazy input realization,
    rank-3 batch projection, dtype mismatch, Vulkan weights fail-closed, and
    Vulkan bias fail-closed.
  - Split `ML-VK-010` into concrete first Vulkan linear-algebra subitems:
    `ML-VK-010-001` first Vulkan `Float32` `ml/linear` lowering,
    `ML-VK-010-002` Vulkan `Float32` batched matmul coverage, and
    `ML-VK-010-003` Vulkan `Float32` batched bias-add/reduction coverage.
  - Validation passed: `c3c build`, file-size gate, `git diff --check`, and
    focused advanced collections suite `pass=1611 fail=0`.

- 2026-04-20 00:16 CEST: Vulkan Float32 `ml/linear` no-bias checkpoint:
  - Implemented the first Vulkan `Float32` `ml/linear` lowering path by routing
    direct concrete no-bias Vulkan input/weights through the existing Tensor
    `contract` helper with axes `[input.rank - 1]` and `[1]`.
  - Preserved `tensor-backends` truthfulness: Vulkan `ml-linear` remains false
    because bias, reductions, and full operation-family support are not
    complete.
  - Added primitive-level coverage for Vulkan result placement, dtype, rank-3
    batch projection, mapped-source copyback, mixed CPU/Vulkan fail-closed
    behavior, Vulkan bias fail-closed behavior, view fail-closed behavior, and
    shape diagnostics.
  - A failed first focused run exposed one malformed test expression and showed
    that direct Vulkan `map` inputs can already materialize on Vulkan; the
    shipped tests now preserve that no-CPU-fallback success instead of calling
    it lazy-fail-closed.
  - Validation passed: `c3c build`, focused advanced collections suite
    `pass=1618 fail=0`, file-size gate, `git diff --check`, and primitive
    docs parity.

- 2026-04-20 00:32 CEST: Vulkan Float32 `ml/linear` bias checkpoint:
  - Added optional concrete Vulkan `Float32` bias support for `ml/linear` by
    running the existing no-bias Tensor `contract` route first, then applying
    the existing Vulkan broadcast `map +` path to add `bias[out_features]`.
  - The intermediate contract result is cleaned after the bias map result is
    produced. Mixed CPU/Vulkan bias and view-backed operands remain
    fail-closed.
  - Split `ML-VK-010-003` into completed bias-add coverage and open
    `ML-VK-010-004` batched-reduction coverage. The broader expression/view
    lowering item moved to `ML-VK-010-005`.
  - Validation passed: `c3c build`, focused advanced collections suite
    `pass=1620 fail=0`, file-size gate, `git diff --check`, and primitive
    docs parity.

- 2026-04-20 01:05 CEST: Vulkan ML direct capability checkpoint:
  - Added `ml-linear-direct-float32` to `tensor-backends` so CPU reports true
    and Vulkan reports the narrow direct concrete `Float32` `ml/linear`
    capability only when Vulkan `Float32` placement is available.
  - Kept broad Vulkan `ml-linear` false; the narrow bit does not imply
    full-family ML linear support, expression/view-backed lowering, mixed
    devices, or broader dtype coverage.
  - Froze `ml/linear/batched-reduce` as the public surface for the next
    Vulkan `Float32` batched-reduction lane under `ML-VK-010-004-001`.
  - Added regressions for Vulkan bias shape mismatch and mapped bias
    expression preservation.
  - Validation passed: `c3c build`, focused advanced collections suite
    `pass=1622 fail=0`, file-size gate, `git diff --check`, and primitive
    docs parity.

- 2026-04-20 01:35 CEST: Vulkan `ml/linear/batched-reduce` checkpoint:
  - Registered `ml/linear/batched-reduce` in runtime and AOT primitive lookup.
  - The new public surface shares `ml/linear` CPU dense `Float64`/`Float32`
    evaluation and the narrow direct concrete Vulkan `Float32` route through
    Tensor `contract` plus optional bias `map`.
  - `ml/linear/batched-reduce` requires input rank >= 2. Rank-1 vectors raise
    `tensor/shape-mismatch`; mixed devices, unsupported layouts, and
    view-backed Vulkan operands remain fail-closed with backend diagnostics
    before any CPU fallback.
  - `ML-VK-010-004-001` is closed; `ML-VK-010-005` remains open for the
    expression/view-backed lowering decision.
  - Validation passed: `c3c build` and focused advanced collections suite
    `pass=1632 fail=0`.

- 2026-04-20 02:18 CEST: Vulkan `ml/linear` narrow expression checkpoint:
  - Implemented the `ML-VK-010-005` implementation branch for Vulkan-only
    expressions that existing Tensor realization lowers to concrete dense
    Vulkan `Float32` storage.
  - `ml/linear` and `ml/linear/batched-reduce` now accept direct concrete
    tensors, supported Vulkan map/contract materialization, and Vulkan
    transpose views before dispatching the existing `contract` plus optional
    bias `map` route.
  - Mixed CPU/Vulkan operands and CPU views are rejected before CPU fallback;
    arbitrary recursive expression/view lowering remains unsupported.
  - Added `ML-VK-010-006` for the remaining Vulkan `Float64`
    linear/batched-reduce lane.
  - Validation passed: `c3c build` and focused advanced collections suite
    `pass=1637 fail=0`.

- 2026-04-20 02:52 CEST: Vulkan `Float64` `ml/linear` checkpoint:
  - Widened the narrow Vulkan `ml/linear` and `ml/linear/batched-reduce`
    path from Float32-only to same-dtype `Float64` or `Float32`.
  - Optional Vulkan bias add now passes the actual input/result dtype into the
    existing broadcast `map +` route instead of hard-coding Float32.
  - Added `ml-linear-direct-float64` to `tensor-backends`; broad Vulkan
    `ml-linear` remains false until the full operation family ships.
  - Added positive Vulkan Float64 coverage for direct, rank-3, bias, mapped
    bias, mapped source, transpose-view source/weights, and batched-reduce
    paths while preserving mixed CPU/Vulkan fail-closed diagnostics.
  - `ML-VK-010` and `ML-VK-010-006` are closed.
  - Validation passed: `c3c build`, focused advanced collections suite
    `pass=1652 fail=0`, file-size gate, `git diff --check`, and primitive
    docs parity.

- 2026-04-20 01:42 CEST: Vulkan ML ReLU checkpoint:
  - Split broad `ML-VK-020` audit work into shipped ReLU items plus remaining
    activation, reduction, softmax, and loss follow-ups.
  - Added `ml/relu` as the first public neural Tensor activation primitive.
    It supports `Float64` and `Float32` tensors by routing through existing
    `map max input 0` semantics, so CPU/CUDA/Vulkan placement behavior and
    no-hidden-CPU-fallback rules stay owned by Tensor map.
  - Added `ml-neural-relu-float64` and `ml-neural-relu-float32` to
    `tensor-backends`; broad `ml-neural-map` remains false until the larger
    activation family ships.
  - Validation passed: `c3c build`, focused advanced collections suite
    `pass=1658 fail=0`, file-size gate, `git diff --check`, and primitive
    docs parity.

- 2026-04-20 01:53 CEST: Vulkan ML Float32 activation checkpoint:
  - Added canonical `ml/sigmoid`, `ml/tanh`, and tanh-approximation `ml/gelu`
    activation primitives plus AOT lookup entries.
  - These are ML activation surfaces, not aliases for bare `tanh` or other
    scientific math primitives. The shipped slice supports `Float32` Tensor
    inputs and preserves CPU/CUDA/Vulkan placement by composing existing
    Tensor `map` kernels.
  - Added narrow sigmoid/tanh/GELU backend capability bits. Float64
    transcendental ML activation bits remain false until `ML-VK-020-005`
    validates the numerical policy or freezes fail-closed behavior.
  - Validation passed: `c3c build`, focused advanced collections suite
    `pass=1665 fail=0`, file-size gate, `git diff --check`, and primitive
    docs parity.

- 2026-04-20 02:00 CEST: Vulkan ML Float64 activation policy checkpoint:
  - Closed `ML-VK-020-005` as explicit fail-closed behavior for Float64
    `ml/sigmoid`, `ml/tanh`, and `ml/gelu`.
  - Added a grouped Vulkan Float64 regression so all three activations return
    `tensor/backend-unsupported` on Vulkan-placed Float64 tensors rather than
    using generic math or hidden CPU fallback.
  - Float64 sigmoid/tanh/GELU ML backend capability bits remain false until
    validated approximations or backend transcendental kernels land. Exact
    GELU still needs a real `erf` route or a separate fail-closed contract.
  - Validation passed: `c3c build` and focused advanced collections suite
    `pass=1666 fail=0`.

- 2026-04-20 02:12 CEST: ML axis reduction checkpoint:
  - Added canonical `ml/sum`, `ml/mean`, and population `ml/variance`
    primitives for CPU `Float64` and `Float32` tensors.
  - The reducers accept an integer axis or array/proper-list axes, drop
    reduced axes from the result shape, preserve dtype, and reject invalid
    axes through Tensor diagnostics.
  - Added `ml-reduction-float64` and `ml-reduction-float32` capability bits;
    CPU reports true, CUDA/cuBLAS/Vulkan report false until real backend
    axis-reduction kernels land.
  - Preserved the negative constraint: do not treat matrix `contract` as a
    softmax/loss reduction substrate. Vulkan axis reductions need a one-input
    kernel path that preserves free axes.
  - Validation passed: `c3c build` and focused advanced collections suite
    `pass=1672 fail=0`.

- 2026-04-20 02:45 CEST: ML stable reduction and softmax checkpoint:
  - Added canonical CPU `ml/max` axis reduction for Float64 and Float32 tensors
    as the max path required by stable softmax/logsumexp numerics.
  - Changed Float32 reduction output narrowing failures from
    `tensor/backend-unsupported` to `tensor/numeric-overflow`.
  - Added CPU `ml/logsumexp(input axes)` with max-shifted multi-axis reduction
    semantics and CPU `ml/softmax(input axis)` with max-shifted single-axis
    normalization.
  - Split stable reduction code into `src/lisp/prim_ml_stable_reduction.c3` so
    `src/lisp/prim_ml_reduction.c3` stays under the repository 700-line file
    cap.
  - Updated TODO and roadmap state: cross-entropy and mean-squared-error remain
    explicit open items, and cross-entropy must choose a target contract before
    exposing a public primitive.
  - Validation passed: `c3c build`, focused advanced collections suite
    `pass=1681 fail=0`, `git diff --check`, primitive docs parity, and the
    file-size gate.

- 2026-04-20 02:55 CEST: ML mean-squared-error checkpoint:
  - Added canonical CPU `ml/mean-squared-error(predictions targets)` as a
    scalar Tensor loss for same-shape, same-dtype Float64/Float32 tensors.
  - The primitive computes population MSE over all elements, returns the input
    dtype, rejects shape/dtype mismatches with Tensor diagnostics, and fails
    closed for CUDA/Vulkan instead of silently copying to CPU.
  - Added focused regressions for Float64 oracle values, Float32 dtype
    preservation, lazy CPU input materialization, mismatch diagnostics, and
    Vulkan fail-closed behavior.
  - Closed `ML-VK-020-007-C`; cross-entropy remains open pending a target
    contract decision.
  - Validation passed: `c3c build`, focused advanced collections suite
    `pass=1686 fail=0`, `git diff --check`, primitive docs parity, and the
    file-size gate.

- 2026-04-20 03:02 CEST: ML cross-entropy checkpoint:
  - Added canonical CPU `ml/cross-entropy(logits targets axis)`.
  - Froze targets as same-shape probability/one-hot tensors, not class-index
    tensors. Target slices must sum to one along the explicit class axis.
  - The primitive uses max-shifted log-softmax over the class axis and averages
    loss over non-class positions, so the denominator is batch/slice count, not
    class count.
  - Added focused regressions for large-logit stability, Float32 dtype
    preservation, non-last class axes, soft targets, invalid probability sums,
    mismatch diagnostics, and Vulkan fail-closed behavior.
  - Closed `ML-VK-020-007-B`; the CPU `ML-VK-020-007` surface is now complete.
  - Validation passed: `c3c build`, focused advanced collections suite
    `pass=1692 fail=0`, `git diff --check`, primitive docs parity, and the
    file-size gate.

- 2026-04-20 03:40 CEST: Vulkan ML axis-reduction checkpoint:
  - Implemented `ML-VK-020-006-VK` for Vulkan Float64/Float32 `ml/sum`,
    `ml/mean`, and population `ml/variance`.
  - Added a one-input Vulkan reduction helper and dtype-specific GLSL compute
    shaders/SPIR-V blobs; the helper preserves free axes and does not reuse
    matrix `contract`.
  - Routed Vulkan tensors through the new helper from `prim_ml_reduction.c3`
    without hidden CPU fallback, and updated `tensor-backends` so
    `ml-reduction-float64`/`ml-reduction-float32` track Vulkan Float64/Float32
    availability.
  - Kept `ml/max`, `ml/logsumexp`, `ml/softmax`, and loss primitives
    backend-unsupported on Vulkan; they require separate maximum and exp/log
    kernels before claiming support.
  - Added rank-3 middle-axis regression coverage for shared reduction indexing
    plus guarded Vulkan parity checks for Float64/Float32 sum/mean/variance.
  - Validation passed: shader compile/`spirv-val`, `scripts/build_omni_chelpers.sh`,
    `c3c build`, direct Vulkan reduction smokes, basic Lisp slice
    `pass=160 fail=0`, `git diff --check`, primitive docs parity, and the
    file-size gate. Full host `advanced` slice still segfaults after TCO tests
    before reaching ML part8 and remains unresolved.

- 2026-04-20 03:48 CEST: Vulkan ML max checkpoint:
  - Added and closed `ML-VK-020-006-VK-MAX` after strict audit found the
    Vulkan `ml/max` follow-up was only implicit in TODO/docs.
  - Extended the existing one-input Vulkan ML reduction helper and Float64/
    Float32 shaders with `op == 3` axis maximum support.
  - Routed Vulkan `ml/max` through `prim_ml_reduction.c3`, preserving
    Float64/Float32 dtype and Vulkan placement with no hidden CPU fallback.
  - Added guarded Vulkan parity tests for `ml/max` and kept Vulkan
    `ml/logsumexp`/`ml/softmax` backend-unsupported until exp/log-backed
    stable kernels land.
  - Validation passed: shader compile/`spirv-val`, `scripts/build_omni_chelpers.sh`,
    `c3c build`, direct Vulkan max smokes, focused advanced collections
    `pass=1695 fail=0`, basic Lisp slice `pass=160 fail=0`,
    primitive docs parity, `git diff --check`, and file-size gate.

- 2026-04-20 12:34 CEST: Vulkan ML logsumexp Float32 checkpoint:
  - Added and closed `ML-VK-020-007-VK-LSE-F32` after audit found the stable
    Vulkan follow-up needed an explicit TODO boundary.
  - Extended the Vulkan Float32 ML reduction helper/shader with `op == 4` for
    stable `ml/logsumexp(input axes)` using `max + log(sum(exp(input - max)))`.
  - Routed Vulkan Float32 `ml/logsumexp` through
    `src/lisp/prim_ml_stable_reduction.c3`, preserving Vulkan placement and
    Float32 dtype with no hidden CPU fallback.
  - Kept Vulkan Float64 `ml/logsumexp` fail-closed pending a validated Float64
    exp/log policy, and kept Vulkan `ml/softmax` fail-closed pending a
    same-shape axis-normalization shader.
  - Validation passed: shader compile/`spirv-val`,
    `scripts/build_omni_chelpers.sh`, `c3c build`, direct Vulkan
    logsumexp/fail-closed smokes, and focused advanced collections
    `pass=1696 fail=0`.

- 2026-04-20 12:52 CEST: Vulkan ML softmax Float32 checkpoint:
  - Added and closed `ML-VK-020-007-VK-SM-F32`.
  - Added a dedicated Vulkan Float32 same-shape softmax shader/helper instead
    of overloading the rank-reducing ML reduction helper.
  - Routed Vulkan Float32 `ml/softmax(input axis)` through
    `src/lisp/prim_ml_stable_reduction.c3`, preserving Vulkan placement and
    Float32 dtype with no hidden CPU fallback.
  - Kept Vulkan Float64 `ml/softmax` fail-closed pending a validated Float64
    exp/log policy.
  - Validation passed: shader compile/`spirv-val`,
    `scripts/build_omni_chelpers.sh`, `c3c build`, direct Vulkan
    softmax/fail-closed smokes, focused advanced collections
    `pass=1699 fail=0`, basic Lisp slice `pass=160 fail=0`, primitive docs
    parity, `git diff --check`, and file-size gate.

- 2026-04-20 13:09 CEST: Vulkan ML mean-squared-error checkpoint:
  - Added and closed `ML-VK-020-007-VK-MSE`.
  - Added dedicated Vulkan Float64/Float32 two-input scalar MSE shaders and a
    loss helper instead of composing public Tensor ops or falling back to CPU.
  - Routed Vulkan `ml/mean-squared-error(predictions targets)` through
    `src/lisp/prim_ml_stable_reduction.c3`, preserving dtype and Vulkan
    placement for the scalar result.
  - Kept mixed CPU/Vulkan operands fail-closed before fallback, and left
    Vulkan `ml/cross-entropy` as the remaining loss follow-up.
  - Validation passed: shader compile/`spirv-val` for Float64/Float32 MSE,
    `scripts/build_omni_chelpers.sh`, `c3c build`, direct Vulkan MSE and
    mixed-device smokes, focused advanced collections `pass=1700 fail=0`,
    basic Lisp slice `pass=160 fail=0`, primitive docs parity,
    Stage 3 source parity, baseline policy, `git diff --check`, and
    file-size gate.
  - Follow-up audit found the manifest-based AOT compile path was checked by
    stale `append_matching_sources(...)` sentinels and that the explicit Lisp
    runtime manifests omitted newer tensor/ML runtime files. Regenerated the
    non-test Lisp AOT manifests into four under-700 parts, added full
    manifest-vs-filesystem parity checks, and verified an AOT binary using
    `ml/mean-squared-error` compiles, links, and runs.
  - Production deduce row materialization no longer depends on test-only
    `test_c_getenv`; it uses the production boundary getenv wrapper so AOT
    runtime builds can exclude `src/lisp/tests_*.c3`.
  - Follow-up Vulkan MSE audit found Float32 overflow could return an `inf`
    device scalar instead of the CPU contract's `tensor/numeric-overflow`.
    The C helper now validates the Float32 scalar result after dispatch,
    destroys the device buffer on non-finite output, and the Lisp wrapper maps
    that status to `tensor/numeric-overflow`.
  - Validation rerun with `LD_LIBRARY_PATH=build:/usr/local/lib` passed:
    focused advanced collections `pass=1701 fail=0`, basic Lisp slice
    `pass=160 fail=0`, direct CPU/Vulkan Float32 overflow smokes, and the AOT
    MSE smoke.
