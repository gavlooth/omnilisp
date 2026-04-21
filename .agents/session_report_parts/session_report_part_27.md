# Session Report Index Part 27

Source: `.agents/SESSION_REPORT.md`

## 2026-04-17 19:46 CEST - TENSOR-100F Vulkan Float32 Contract Slice

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and land Vulkan
    `Float32` rank-N `contract` without using `Float32` as a fallback for
    `Float64` or silently copying unsupported Vulkan paths to CPU.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Tensor contract shader/helper ABI, public
    contract dispatch, focused tests, TODO, and design/status artifacts.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_contract_f32.comp` and generated
    `csrc/tensor_vulkan_contract_f32_spv.c`.
  - Added `omni_tensor_backend_vulkan_contract_f32` in
    `csrc/tensor_vulkan_helpers.c` and the matching C3 extern in
    `src/lisp/tensor_vulkan_backend.c3`; build manifests now include the
    generated Float32 SPIR-V C source.
  - Updated `src/lisp/prim_tensor.c3` so Vulkan `contract` accepts matching
    dense row-major `Float32` operands, preserves result dtype, keeps the
    existing rank-1 dot fast path `Float64`-only, and dispatches `Float32`
    contractions through the dedicated generic Float32 helper.
  - Added focused availability-gated regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` for eager and lazy
    Vulkan `Float32` contract operands: rank-1 dot, rank-2, rank-N
    single-axis, multi-axis, zero-axis, zero-size, zero-free output, dtype
    preservation, result placement, mixed-device rejection, and mixed-dtype
    rejection.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md` so `Float32`
    `contract` is no longer listed as deferred.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_contract_f32.comp -o /tmp/omni_tensor_vulkan_contract_f32.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_contract_f32.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `Float32` contract smokes returned `154.0` for rank-2,
    `"Float32"` for result dtype, `vulkan` for result placement, `0.0` for
    zero-size dot, `460.0` for rank-N single-axis, `210.0` for multi-axis,
    `0` for zero-free output length, and `tensor/dtype-mismatch` for mixed
    `Float32`/`Float64` Vulkan operands.
  - Host focused `advanced-collections-module`: passed, `pass=1039 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1026 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating Vulkan `Float32` `contract` as fail-closed. Dense
    row-major rank-N matching-`Float32` contract now has a real Vulkan shader
    and helper path.
  - Do not route `Float32` rank-1 dot through the existing Float64 dot helper;
    it intentionally falls through to the generic Float32 contract helper.
- Current best recommendation / checkpoint:
  - Continue Vulkan `Float32` from structural matrix kernels (`transpose`,
    `diagonal`, `diagonal-matrix`, `trace`) with explicit Float32 shader/helper
    ABI names. Reducers and factor/solve kernels remain later work requiring
    dedicated Float32 tolerances and oracles.
- Unresolved issues:
  - Vulkan `Float32` structural matrix kernels, reducers, and factor/solve
    kernels remain fail-closed.
  - CUDA `Float32` placement and scalar `Float32` values remain fail-closed.
- Signature: Codex GPT-5.4

## 2026-04-17 20:11 CEST - TENSOR-100F Vulkan Float32 Structural Matrix Slice

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and land Vulkan
    `Float32` structural matrix kernels without using `Float32` as a fallback
    for `Float64` or silently copying unsupported Vulkan paths to CPU.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Tensor structural matrix shaders/helpers,
    public matrix dispatch, focused tests, TODO, and design/status artifacts.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_transpose_f32.comp`,
    `csrc/tensor_vulkan_diagonal_f32.comp`,
    `csrc/tensor_vulkan_diagonal_matrix_f32.comp`,
    `csrc/tensor_vulkan_trace_f32.comp`, and generated matching `_spv.c`
    sources.
  - Added `omni_tensor_backend_vulkan_transpose_f32`,
    `omni_tensor_backend_vulkan_diagonal_f32`,
    `omni_tensor_backend_vulkan_diagonal_matrix_f32`, and
    `omni_tensor_backend_vulkan_trace_f32` in `csrc/tensor_vulkan_helpers.c`
    using the existing Float32 two-buffer dispatcher, plus matching C3 externs
    and build-manifest entries.
  - Updated `src/lisp/prim_tensor_matrix.c3` so Vulkan `matrix/transpose`,
    `matrix/diagonal`, `matrix/diagonal-matrix`, and `matrix/trace` accept
    matching dense row-major `Float32` operands, preserve Tensor result dtype,
    keep trace as a public scalar `Float64`, and do not widen/downcast through
    the Float64 helpers.
  - Added availability-gated tests in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` for Vulkan `Float32`
    structural matrix copyback, placement, dtype preservation, lazy inputs,
    zero-size behavior, scalar trace readback, and `Float64` no-downcast
    preservation.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md` so structural
    matrix kernels are no longer listed as deferred.
- Commands run and key results:
  - `glslangValidator` and `spirv-val` for
    `csrc/tensor_vulkan_transpose_f32.comp`,
    `csrc/tensor_vulkan_diagonal_f32.comp`,
    `csrc/tensor_vulkan_diagonal_matrix_f32.comp`, and
    `csrc/tensor_vulkan_trace_f32.comp`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `Float32` structural smokes returned `6.0` for transpose
    copyback, `"Float32"` for diagonal result dtype, `0.0` for
    diagonal-matrix off-diagonal copyback, and `5.0` for scalar trace readback.
  - Host focused `advanced-collections-module`: passed, `pass=1059 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1046 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating Vulkan `Float32` structural matrix operations as
    fail-closed. Dense row-major matching-`Float32` transpose, diagonal,
    diagonal-matrix, and trace now have real Vulkan shader/helper paths.
  - Do not route these Float32 matrix operations through the existing Float64
    helpers; the landed path uses explicit `_f32` shader/helper ABI names.
- Current best recommendation / checkpoint:
  - Continue Vulkan `Float32` from reducer and factor/solve kernels. Structural
    matrix kernels (`transpose`, `diagonal`, `diagonal-matrix`, `trace`) have
    landed with explicit Float32 shader/helper ABI names.
- Unresolved issues:
  - Vulkan `Float32` SVD-backed reducers, singular-value/SVD outputs, and
    factor/solve kernels remain fail-closed.
  - CUDA `Float32` placement and scalar `Float32` values remain fail-closed.
- Signature: Codex GPT-5.4

## 2026-04-17 20:42 CEST - TENSOR-100F Vulkan Float32 Direct Reducer Supersession

- Objective attempted:
  - Record that the later direct reducer slice supersedes the structural
    matrix handoff line that listed all Vulkan `Float32` reducers as
    fail-closed.
- Current best recommendation / checkpoint:
  - Vulkan `Float32` `matrix/rank` and direct `matrix/norm` selectors
    default/`'frobenius`, `'one`, `'infinity`, and `'max` are landed and
    validated with dedicated `_f32` shader/helper paths.
  - Continue from SVD-backed `Float32` reducers (`matrix/norm`
    `'spectral`/`'nuclear`, `matrix/singular-values`, `matrix/svd`) or
    factor/solve kernels, not from the broad "all reducers fail-closed"
    assumption.
- Commands run and key results:
  - Full details are recorded in the `20:42 CEST - TENSOR-100F Vulkan Float32
    Direct Reducer Slice` entry above: shader validation, helper rebuild,
    `c3c build --obj-out obj`, direct smokes, host focused
    `advanced-collections-module` `pass=1098 fail=0`, bounded-container
    focused `advanced-collections-module` `pass=1085 fail=0`, primitive docs
    parity, Stage 3 source parity, and targeted `git diff --check` passed.
- Unresolved issues:
  - Vulkan `Float32` SVD-backed reducers, singular-value/SVD outputs, and
    factor/solve kernels remain fail-closed.
  - CUDA `Float32` placement and scalar `Float32` values remain fail-closed.
- Signature: Codex GPT-5.4

## 2026-04-18 10:22 CEST - Ownership Discussion Comparison

- Objective attempted:
  - Read the root `discussion` file and compare its proposed ownership ideas
    against the current Omni runtime model.
- Relevant workspace or target:
  - `/home/christos/Omni/discussion`
  - `memory/DESTINATION_ARENA_PLAN.md`
  - `src/scope_region*.c3`
  - `src/lisp/eval_boundary_*.c3`
- Code or configuration changes made:
  - None. This was an inspection-only comparison.
- Commands run:
  - `jj status`
  - `sed -n` over `discussion`, `memory/DESTINATION_ARENA_PLAN.md`,
    `docs/ARCHITECTURE.md`, `src/scope_region*.c3`, and
    `src/lisp/eval_boundary_*.c3`
  - `rg` searches for `ScopeRegion`, `TEMP`, `ESCAPE`, promotion context, and
    boundary classification symbols
- Key results:
  - The discussion recommends a tiered model: scratch arenas for temporary
    cyclic graphs, explicit promotion/freeze into stable storage, handles or
    IDs across lifetime boundaries, debug epoch/domain checks, and durable data
    trending toward DAG/tree plus interned atoms.
  - The implemented Omni model is already a specialized hybrid of those ideas:
    `ScopeRegion` remains the RC-owned lifetime unit, but each region has
    split TEMP/ESCAPE lanes, stamped provenance, destination ESCAPE builders,
    boundary return classification, promotion contexts with memo/budget state,
    graph-audit checks, and O(1) ESCAPE-lane splicing when transfer invariants
    hold.
  - The current model is not plain group reference counting and not a pure
    scratch-arena reset model. It keeps runtime boundary promotion because
    Omni's dynamic surfaces, continuations, mutation paths, and root/global
    boundaries require safety mechanisms beyond the discussion's generic
    scratch-only sketch.
- Current best recommendation:
  - Treat `memory/DESTINATION_ARENA_PLAN.md` Revision XXI/XXII plus
    `memory/CHANGELOG.md` as authoritative for current memory architecture,
    not the root `discussion` brainstorm.
  - Reuse the discussion mainly as a design checklist for future cleanup:
    explicit promotion boundaries, handles/IDs at API boundaries, debug
    lifetime verification, no raw scratch escapes, and compact stable forms.
- Unresolved issues:
  - No runtime validation was run because no code was changed.
  - The discussion's standalone `ScratchArena`/`StableArena`/`InternTable`/
    `HandleStore` candidate is not implemented as a separate architecture; the
    current equivalent is the integrated ScopeRegion TEMP/ESCAPE model plus
    AST arena and optional fiber TEMP backing.
- Signature: Codex GPT-5.4

## 2026-04-18 10:40 CEST - Ownership Hardening TODOs Added

- Objective attempted:
  - Convert the ownership-model improvement recommendations into concrete live
    backlog items.
- Relevant workspace or target:
  - `/home/christos/Omni/TODO.md`
- Code or configuration changes made:
  - Added live parent `OWNERSHIP-HARDENING-001` and updated the live parent
    count from 1 to 2.
  - Added deferred subtasks for the memory-model ADR, ownership-boundary
    checklist, debug memory boundary stats, durable-graph rule, handles/IDs
    policy, required graph-carrying `ValueTag` boundary tests, static
    allocation routing as optimization-only, boundary vocabulary glossary,
    opaque-payload policy, and ownership-model examples.
- Commands run and key results:
  - `git diff --check -- TODO.md`: passed.
  - `rg` scan confirmed the new parent and all requested suggestion items are
    present in `TODO.md`.
- Current best recommendation / checkpoint:
  - Work `OWNERSHIP-HARDENING-001` as a hardening/documentation parent, not as
    permission to reopen the completed ownership model or add public ownership
    syntax.
- Unresolved issues:
  - No runtime validation was run because only backlog text changed.
- Signature: Codex GPT-5.4

## 2026-04-18 11:12 CEST - Vulkan Map Handler Propagation And Lazy Realize Stack

- Objective attempted:
  - Close the open TENSOR-100F follow-up where unsupported Vulkan `map`
    callable errors reported `stack overflow in handle body` under `handle`,
    then address the related `realize` and CPU lazy-map stack edges exposed by
    agent audit.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_tensor.c3`
  - `src/lisp/value_constructors.c3`
  - `src/lisp/jit_apply_runtime.c3`
  - `src/lisp/jit_apply_multi_prims.c3`
  - `src/stack_engine_region.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups.c3`
- Code or configuration changes made:
  - Split Vulkan map callable preflight into a small wrapper path before the
    heavier direct/value map execution frames.
  - Added root-scoped construction for canonical recoverable raise payloads
    with no `data`, avoiding boundary-promotion stack pressure before
    `raise_pending` is installed.
  - Added compiled one-argument and multi-argument primitive apply propagation
    for evaluated `ERROR` arguments.
  - Increased normal StackCtx usable stack from 128KB to 256KB for handled lazy
    Tensor materialization.
  - Added guarded regressions for handled unsupported Vulkan `map` and
    `realize`, plus non-Vulkan regressions for handled CPU lazy-map `realize`
    and source-error propagation through `realize`.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed, with existing deprecation warnings.
  - Direct `--eval` smokes confirmed handled direct/lazy-source Vulkan `map`,
    direct/handled `realize`, handled CPU lazy-map `realize`, and
    destination-form `realize` source-error propagation.
  - Host focused `advanced-collections-module`: `pass=1352 fail=0`.
  - Host `advanced-effect-continuation`: `pass=56 fail=0`.
  - Host `advanced-effect-union-limit`: `pass=68 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=1335 fail=0`.
  - Stack suite: `pass=23 fail=0`.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating this as missing Vulkan `floor` support. The correct
    contract remains fail-closed unsupported-callable routing through
    `tensor/backend-unsupported`.
  - Do not assume direct `map` handler success covers `realize`; handled lazy
    Tensor materialization also needs enough StackCtx headroom.
- Current best recommendation / checkpoint:
  - The handler propagation follow-up is closed. Continue from fixed-width
    complex Tensor storage/scalar semantics before Vulkan complex kernels, or
    from explicitly measured performance lanes if those measurements are the
    next priority.
- Unresolved issues:
  - Normal StackCtx is now 256KB, which is a pragmatic runtime budget increase.
    A later stack-light rewrite of lazy Tensor map materialization could reduce
    that pressure, but it is no longer an active blocker for the validated
    small-tensor handler path.
- Signature: Codex GPT-5.4

## 2026-04-18 12:45 CEST - Vulkan Fixed-Width Complex Elementwise Map

- Objective attempted:
  - Continue fixed-width complex backend implementation by landing the first
    real GPU compute operation family for Vulkan `Complex128` and `Complex64`
    while keeping CUDA and contract/matrix residuals explicit.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `csrc/tensor_vulkan_map_complex128.comp`
  - `csrc/tensor_vulkan_map_complex64.comp`
  - `csrc/tensor_vulkan_map_complex128_unary.comp`
  - `csrc/tensor_vulkan_map_complex64_unary.comp`
  - `csrc/tensor_vulkan_map_complex128_to_real.comp`
  - `csrc/tensor_vulkan_map_complex64_to_real.comp`
  - `csrc/tensor_vulkan_helpers.c`
  - `src/lisp/prim_tensor.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups.c3`
  - docs, TODO, memory, and operational plan artifacts
- Code or configuration changes made:
  - Added Vulkan complex binary map, unary map, and component-to-real shader
    families plus generated SPIR-V C sources and build wiring.
  - Added C helper entrypoints for Vulkan `Complex128` and `Complex64` map,
    unary map, and to-real operations using the existing device-buffer
    ownership model.
  - Routed Lisp `map` dispatch for Vulkan fixed-width complex tensors through
    explicit complex op tables. Binary support covers `+`, `-`, `*`, and `/`;
    unary support covers unary `+`, `abs`, unary `-`, `real-part`,
    `imag-part`, and `conjugate`.
  - Routed direct Vulkan complex `abs`, `real-part`, `imag-part`, unary `-`,
    and `conjugate` through device helpers. Direct component/magnitude helpers
    return component-width real Vulkan tensors.
  - Changed Vulkan `tensor-backends` `elementwise-map-complex128` and
    `elementwise-map-complex64` from hard false storage placeholders to real
    operation capability bits. Historical note: CUDA complex map remained false
    at this checkpoint, superseded by the later CUDA complex map checkpoint.
  - Added capability-gated tests for tensor/scalar, scalar/tensor,
    tensor/tensor, broadcast, unary, direct component/magnitude helpers,
    division status, unsupported `min`, and lazy realization behavior.
- Commands run and key results:
  - `glslangValidator` and `spirv-val` for the six Vulkan complex shaders:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build`: passed.
  - Host focused `advanced-collections-module`: `1415 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`:
    `1398 passed, 0 failed`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - Targeted `git diff --check` for touched files: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The to-real shader op table must use op `4` for `imag-part`; an initial
    op `2` mismatch caused Vulkan `Complex64` direct `imag-part` to return the
    wrong component and was fixed before validation.
  - Complex division status must raise a structured Tensor payload. Plain
    `raise_error` lost the expected `tensor/domain-error` code on the
    interpreter path and was replaced with `raise_error_with_payload_names`.
  - Historical, superseded for CUDA map: do not copy the Vulkan complex map
    implementation to CUDA by assumption. CUDA complex map later landed through
    generated PTX and the same status ABI.
- Current best recommendation / checkpoint:
  - Historical checkpoint for Vulkan map. Superseded by the later CUDA map
    checkpoint for CUDA elementwise map; remaining backend complex work is now
    complex `contract` and matrix kernels.
- Unresolved issues:
  - Superseded: CUDA complex `map` no longer remains fail-closed after the
    later CUDA map checkpoint.
  - CUDA/Vulkan complex `contract` and complex matrix kernels remain
    fail-closed.
- Signature: Codex GPT-5.4
