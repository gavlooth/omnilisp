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
