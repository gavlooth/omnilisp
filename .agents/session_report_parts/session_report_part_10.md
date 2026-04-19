# Session Report Index Part 10

Source: `.agents/SESSION_REPORT.md`

## 2026-04-17 20:42 CEST - TENSOR-100F Vulkan Float32 Direct Reducer Slice

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and land Vulkan
    `Float32` direct reducers without treating `Float32` as a fallback for
    `Float64`, silently widening through LAPACK/SVD, or copying unsupported
    Vulkan paths to CPU.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Tensor reducer shaders/helpers, public
    matrix rank/norm dispatch, focused tests, TODO, design/status docs, and
    session handoff artifacts.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_norm_f32.comp`,
    `csrc/tensor_vulkan_rank_f32.comp`, and generated matching `_spv.c`
    sources.
  - Added `omni_tensor_backend_vulkan_norm_f32` and
    `omni_tensor_backend_vulkan_rank_f32` in `csrc/tensor_vulkan_helpers.c`
    using explicit `Float32` storage, a `Float32` rank push-constant layout,
    and matching C3 extern/build-manifest wiring.
  - Updated `src/lisp/prim_tensor_matrix.c3` so public `matrix/rank` accepts
    rank-2 `Float64` or `Float32` tensors. CPU `Float32` rank uses the pure
    elimination path with values widened only inside the local workspace;
    Vulkan `Float32` rank routes through the dedicated `_f32` helper.
  - Updated `matrix/norm` so `Float32` supports direct selectors
    default/`'frobenius`, `'one`, `'infinity`, and `'max` on CPU and Vulkan,
    returning the existing public `Float64` scalar shape. `Float32`
    `'spectral` and `'nuclear` fail closed with `tensor/backend-unsupported`
    until native `Float32` singular-value kernels land.
  - Added CPU and availability-gated Vulkan tests in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` for eager/lazy
    `Float32` rank and norm, zero-size shapes, tolerance behavior,
    vector-shape rejection, no-LAPACK routing, and fail-closed Float32
    spectral/nuclear selectors.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md` so direct
    reducers are no longer listed as deferred.
- Commands run and key results:
  - `glslangValidator` and `spirv-val` for
    `csrc/tensor_vulkan_norm_f32.comp` and
    `csrc/tensor_vulkan_rank_f32.comp`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct CPU/Vulkan `Float32` smokes returned `2` for `matrix/rank`, `5.0`
    for `matrix/norm`, and `tensor/backend-unsupported` for Float32
    spectral/nuclear `matrix/norm`.
  - Host focused `advanced-collections-module`: passed, `pass=1098 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1085 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating all Vulkan `Float32` reducers as fail-closed. Dense
    row-major matching-`Float32` `matrix/rank` and direct `matrix/norm`
    selectors now have real Vulkan shader/helper paths.
  - Do not route `Float32` rank or direct norm through existing `Float64`
    LAPACK/SVD helpers. SVD-backed `Float32` selectors remain separate and
    fail-closed until native singular-value kernels exist.
- Current best recommendation / checkpoint:
  - Continue Vulkan `Float32` from SVD-backed reducers
    (`matrix/norm` `'spectral`/`'nuclear`, `matrix/singular-values`,
    `matrix/svd`) or factor/solve kernels with dedicated Float32
    shader/helper ABI names and tolerance/oracle tests.
- Unresolved issues:
  - Vulkan `Float32` SVD-backed reducers, singular-value/SVD outputs, and
    factor/solve kernels remain fail-closed.
  - CUDA `Float32` placement and scalar `Float32` values remain fail-closed.
- Signature: Codex GPT-5.4

## 2026-04-17 15:39 CEST - Constructor-Driven Iterator Tensor Materialization
- Objective attempted:
  - Prioritize the owner design correction that iterators are already lazy
    computations and constructors, not `delay`/`force` or general `realize`,
    should be the public materialization boundary.
- Workspace/target:
  - `/home/christos/Omni`, Tensor constructor dispatch, iterator consumption,
    and scientific Tensor design docs.
- Code or configuration changes made:
  - Updated `src/lisp/prim_tensor.c3` so `(Tensor iterator)`,
    `(Tensor iterator dtype)`, and `(Tensor dtype shape iterator)` consume
    finite numeric iterators through the Tensor constructor path.
  - Added native `Iterator(^Tensor)` dispatch through a C iterator thunk/state:
    Tensor iteration yields flat row-major elements, realizes lazy CPU Tensor
    expressions once into Tensor storage, and fails closed on non-CPU device
    tensors until explicitly copied with `to-device 'cpu`.
  - Added focused advanced collection regressions for inferred iterator Tensor
    construction, lazy iterator `map` into `Tensor`, explicit-shape iterator
    data, BigInteger dtype preservation, non-numeric iterator failure,
    Tensor-to-Iterator row-major iteration, lazy CPU Tensor iteration,
    BigInteger Tensor iteration, empty Tensor iteration, and device fail-closed
    behavior.
  - Updated design docs, including the primitive and stdlib appendices, plus
    TODO, `.agents/PLAN.md`, and `memory/CHANGELOG.md` so constructor dispatch
    is the canonical public materialization model and `realize` is low-level
    Tensor destination storage.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct runtime smokes returned `3.0`, `3.0`, `6.0`,
    `"9223372036854775808"`, `3.0`, `3.0`, and
    `"9223372036854775808"` for iterator-to-Tensor and Tensor-to-Iterator
    constructor paths.
  - Host focused `advanced-collections-module`: passed, `pass=969 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=956 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check` passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not describe `realize` as the canonical public lazy/materialization
    boundary.
  - Do not add a public Clojure-style `delay`/`force` surface for iterator or
    Tensor laziness.
- Current best recommendation / checkpoint:
  - Continue from the single remaining live parent, `TENSOR-100F`. The
    constructor-driven iterator/Tensor materialization lane is now closed as
    `TENSOR-076B` plus `TENSOR-076C`.
- Unresolved issues:
  - Full heavy/container-only gates were not rerun for this source-level slice;
    focused host and bounded-container advanced collection coverage passed.
- Signature: Codex GPT-5.4

## 2026-04-17 18:42 CEST - TENSOR-100F Vulkan Large-k SVD/Singular-Values

- Objective attempted:
  - Continue `TENSOR-100F` by replacing the old `k <= 64` Vulkan
    `matrix/singular-values` / direct `matrix/svd` private-array cap with a
    larger-size implementation while preserving backend-neutral public
    surfaces and no hidden CPU/LAPACK fallback.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Tensor SVD/singular-values shaders, helper
    validation/allocation, public tests, and planning artifacts.
- Code or configuration changes made:
  - Reworked `csrc/tensor_vulkan_singular_values_f64.comp` to store Gram
    scratch behind the public singular-values/status/nuclear output payload
    instead of private fixed arrays.
  - Reworked `csrc/tensor_vulkan_svd_f64.comp` to store Gram scratch behind
    the public `u` output payload and use the existing `s` payload for
    eigenvalues before singular-value extraction/sorting.
  - Regenerated `csrc/tensor_vulkan_singular_values_f64_spv.c` and
    `csrc/tensor_vulkan_svd_f64_spv.c`.
  - Updated `csrc/tensor_vulkan_helpers.c` validation/allocation to remove the
    shared `k <= 64` cap, allocate hidden scratch, and preserve 32-bit shader
    index/storage-count guards.
  - Updated advanced tests for `65x65` Vulkan singular-values, spectral/nuclear
    norms, and SVD no-LAPACK/output-placement behavior.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`, and relevant
    Vulkan/Tensor docs. Deferred performance work is now tracked as a separate
    measurement/tiled-or-staged optimization TODO, not as the old semantic cap.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_singular_values_f64.comp -o /tmp/omni_singular_values.spv`: passed.
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_svd_f64.comp -o /tmp/omni_svd.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_singular_values.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_svd.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Host focused `advanced-collections-module`: `pass=999 fail=0`.
  - Direct smokes: `65x65` all-ones Vulkan SVD singular value `65.0`; `65x65`
    zero SVD outputs `(vulkan vulkan vulkan 65)`.
  - Bounded container focused `advanced-collections-module`: `pass=986 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Current best recommendation / checkpoint:
  - Treat semantic large-`k` Vulkan SVD/singular-values support as landed for
    dense row-major `Float64` inputs within existing 32-bit shader index
    guards. Continue with the next explicit `TENSOR-100F` lane; if performance
    becomes the focus, start with measurements before designing tiled Gram or
    staged Jacobi execution.
- Unresolved issues:
  - The large-`k` path remains correctness-first single-dispatch Jacobi using
    storage-buffer scratch; tiled/multi-dispatch performance work is deferred
    and tracked in `TODO.md`.
  - Full heavy/container-only gates were not run; focused bounded-container
    validation passed.
  - Live GPU execution coverage remains availability-gated by the local Vulkan
    stack.
- Signature: Codex GPT-5.4

## 2026-04-17 17:49 CEST - Native CPU Tensor Float32 Storage

- Objective attempted:
  - Continue `TENSOR-100F` feature implementation with multiple GPT-5.4 agents,
    prioritizing the owner direction that Tensor lazy machinery should be
    constructor/iterator driven and not depend on public `delay`/`force` or a
    broad `realize` protocol.
- Workspace/target:
  - `/home/christos/Omni`, Tensor dtype/runtime storage, CPU Tensor evaluation,
    structural matrix helpers, docs, TODO, and memory artifacts.
- Code or configuration changes made:
  - Added native `TENSOR_DTYPE_FLOAT32` metadata/storage handling in
    `src/lisp/value_runtime_types.c3` and `src/lisp/value_tensor.c3`.
  - Updated `src/lisp/prim_tensor.c3` so `Tensor Float32` construction narrows
    finite numeric data into 32-bit storage, rejects out-of-range data, and
    supports CPU `map`, `contract`, `realize`, `to-device 'cpu`, `ref`,
    `Array`, `List`, and `Iterator`. Element extraction widens to scalar
    `Float64` because a scalar `Float32` value tag does not exist yet.
  - Updated `src/lisp/prim_tensor_matrix.c3` so structural CPU helpers support
    `Float32`: `matrix/transpose`, `matrix/diagonal`,
    `matrix/diagonal-matrix`, `matrix/identity`, and `matrix/trace`.
    Heavier LAPACK-style routines remain non-`Float32` lanes and must not
    silently widen and narrow.
  - Kept scalar `Float32` / `(Float x 32)` constructors fail-closed in
    `src/lisp/prim_string_convert.c3`; updated describe-mode wording to state
    scalar storage is still missing while Tensor storage exists.
  - Added focused regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` for construction, dtype,
    ref, Array/List/Iterator conversion, CPU `map`, CPU `contract`,
    destination-form `realize`, `to-device 'cpu`, mixed-dtype rejection,
    out-of-range rejection, device-placement fail-closed behavior, and
    structural matrix helpers.
  - Added low-level lifetime/allocation coverage in
    `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` for Float32 Tensor
    payload byte length, zero initialization, and clone independence.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/reference/11-appendix-primitives.md`,
    `docs/areas/tensor-scientific.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`,
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `docs/plans/README.md`.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=995 fail=0`.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-unicode-iterator OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=159 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed, `pass=982 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed, `pass=228 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check` for touched source/docs/artifacts: passed.
  - `c3c build --sanitize=address --obj-out obj-asan`: not run to completion;
    local `c3c` rejected sanitizer mode with "Address sanitizer is only
    supported on Linux, FreeBSD, NetBSD, Darwin and Windows."
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep saying native Tensor `Float32` storage is absent. The remaining
    missing pieces are scalar `Float32` values and GPU `Float32` copy/kernel
    support.
  - Do not implement Vulkan `Float32` by downcasting `Float64`; the remaining
    device work needs explicit `Float32` copy paths, helper/shader ABI names,
    capability reporting, and no-downcast tests.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100F` with Vulkan descriptor/command/status helper
    factoring or the first real Vulkan `Float32` placement/kernel slice. The
    CPU `Tensor Float32` oracle is now present for the latter.
- Unresolved issues:
  - Scalar `Float32` constructors still fail closed.
  - CUDA/Vulkan `Float32` placement still fails closed.
  - Full heavy/container gates were not run; focused bounded container
    validation passed for advanced collections and memory lifetime smoke.
- Signature: Codex GPT-5.4

## 2026-04-17 15:09 CEST - CUDA Destination Realize Support
- Objective attempted:
  - Continue `TENSOR-100F` feature implementation with multiple GPT-5.4
    agents by extending destination-form `realize` parity to CUDA.
- Workspace/target:
  - `/home/christos/Omni`, Tensor/CUDA destination realization.
- Code or configuration changes made:
  - Added existing-buffer CUDA host-to-device, device-to-device, and scalar
    `Float64` fill helpers in `csrc/tensor_cuda_helpers.c`.
  - Exposed the helpers through `src/lisp/tensor_cuda_backend.c3`.
  - Split destination-form `realize` in `src/lisp/prim_tensor.c3` so existing
    dense row-major CUDA `Float64` destinations accept matching CPU sources,
    CUDA sources, lazy CPU expressions, supported lazy CUDA contract results,
    and scalar fills.
  - Preserved CPU destination fail-closed behavior for CUDA sources without
    explicit `(to-device source 'cpu)`.
  - Hardened unsupported device `map` construction so CUDA map operands fail
    immediately with `tensor/backend-unsupported`.
  - Updated tests, Tensor docs, CUDA/Vulkan plans, TODO, `.agents/PLAN.md`,
    and `memory/CHANGELOG.md`.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct runtime smokes returned `2.0`, `3.0`, `4.0`, `1.0`, and `154.0`
    for CPU source, CUDA source, lazy CPU source, scalar fill, and lazy cuBLAS
    contract into CUDA destinations; CUDA source into CPU destination and
    unsupported CUDA map both returned `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: passed, `pass=959 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=946 fail=0`.
  - Primitive docs parity and Stage 3 source parity passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Unsupported CUDA `map` operands should fail at map construction with
    `tensor/backend-unsupported`; do not allow lazy CUDA map expressions to
    reach boundary-copy or CPU fallback paths.
  - CUDA destination realization does not authorize CUDA/Vulkan cross-backend
    destination copies.
- Current best recommendation / checkpoint:
  - Destination-form `realize` is now implemented for dense row-major
    `Float64` CPU, CUDA, and Vulkan destination classes. Continue residual
    `TENSOR-100F` lanes from TODO.
- Unresolved issues:
  - Full heavy/container-only gates were not rerun for this source-level slice.
- Signature: Codex GPT-5.4

## 2026-04-17 14:55 CEST - Vulkan Destination Realize Support
- Objective attempted:
  - Continue `TENSOR-100F` feature implementation with multiple GPT-5.4
    agents by landing explicit Vulkan destination-form `realize` support.
- Workspace/target:
  - `/home/christos/Omni`, Tensor/Vulkan destination realization.
- Code or configuration changes made:
  - Added existing-buffer Vulkan host-to-device, device-to-device, and scalar
    `Float64` fill helpers in `csrc/tensor_vulkan_helpers.c`.
  - Exposed the helpers through `src/lisp/tensor_vulkan_backend.c3`.
  - Split destination-form `realize` in `src/lisp/prim_tensor.c3` so existing
    dense row-major Vulkan `Float64` destinations accept matching CPU sources,
    Vulkan sources, lazy Vulkan results, and scalar fills.
  - Preserved CPU destination fail-closed behavior for device sources without
    explicit `(to-device source 'cpu)`.
  - Updated tests, Tensor docs, Vulkan roadmap/status docs, TODO,
    `.agents/PLAN.md`, and `memory/CHANGELOG.md`.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct runtime smokes returned `2.0`, `3.0`, `4.0`, and `1.0` for CPU,
    Vulkan, lazy Vulkan, and scalar sources into Vulkan destinations; Vulkan
    source into CPU destination still returned `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: passed, `pass=949 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=936 fail=0`.
  - Primitive docs parity and Stage 3 source parity passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The previous audit-state assumption that all non-CPU destination tensors
    must fail closed is superseded for Vulkan `Float64` destinations only.
    CPU destinations still reject device sources without explicit CPU copy.
- Current best recommendation / checkpoint:
  - Continue residual `TENSOR-100F` lanes from TODO: CUDA destination contract
    only if needed, stride/view metadata, native `Float32`, fixed-width complex,
    unary helper opcode expansion, and broad bounded-container validation
    before closing the parent.
- Unresolved issues:
  - Full heavy/container-only gates were not rerun for this source-level slice.
- Signature: Codex GPT-5.4
