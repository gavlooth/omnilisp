    expansion can keep pipe ergonomics without preserving bare primitive
    partial calls.
- Current best recommendation/checkpoint:
  - Next session should finish the `false`/`nil` split before calling this
    cleanup complete:
    - sweep remaining docs that still describe `false` as `nil`,
    - sweep remaining tests that encode alias expectations,
    - run actual build/tests once `c3c` is available.
- Unresolved issues:
  - This session could not run compile/test verification because `c3c` is not
    installed in the current environment.
  - The worktree was already dirty before these edits; current cleanup edits
    were kept narrow, but a real validation pass is still required.
- Signature: Codex (GPT-5)

2026-04-08 11:52 CEST - Arm64 build lane repaired and binary startup restored

- Objectives attempted:
  - Fix the current tree so `c3c build` succeeds on this Ubuntu 24.04 arm64
    host and verify that the produced binary actually starts.
- Code/config changes made:
  - Repaired compile regressions surfaced during the first real build pass:
    - `src/lisp/deduce_schema_query_define_relation.c3`
    - `src/lisp/deduce_db_handles_core.c3`
    - `src/lisp/deduce_relation_ops_query.c3`
    - `src/lisp/eval_boundary_commit_escape.c3`
    - `src/lisp/eval_repl_server_worker.c3`
    - `src/lisp/scheduler_thread_tasks.c3`
    - `src/lisp/eval_boundary_commit_flow.c3`
  - Reworked the arm64 stack-engine compile path so x86_64-only context switch
    assembly no longer blocks whole-program builds:
    - `src/stack_engine_abi_switch.c3`
    - `csrc/stack_helpers.c`
  - Repaired C-helper include/build plumbing around vendored dependencies:
    - `csrc/json_helpers.c`
    - `csrc/tls_helpers.c`
    - `csrc/uv_helpers.c`
    - `csrc/uv_helpers_work.c`
    - `csrc/uv_helpers_thread.c`
    - `csrc/uv_helpers_pipe.c`
    - `csrc/uv_helpers_tcp.c`
    - `csrc/uv_helpers_process.c`
    - `csrc/uv_helpers_signal.c`
    - `csrc/toml_helpers.c`
    - `scripts/build_omni_chelpers.sh`
    - `project.json`
  - Adjusted local link truth:
    - `project.json` now compiles `clib/mathutils.c` directly,
    - `project.json` restores the `omni_ftxui` archive link edge,
    - `project.json` now embeds `RUNPATH=$ORIGIN`,
    - local `liblightning` and `libreplxx` shared libraries were staged into
      `build/`.
- Commands run:
  - `~/.local/bin/c3c build` repeatedly while fixing blockers
  - `git show 65b2841:src/lisp/deduce_db_handles.c3`
  - `bash deps/build_static.sh`
  - `make -C deps/src/lmdb/libraries/liblmdb clean all`
  - `bash scripts/build_omni_chelpers.sh`
  - `readelf -d build/main`
  - `ldd build/main`
  - `./build/main --help`
- Key results and conclusions:
  - `~/.local/bin/c3c build` now exits 0 and links `build/main`.
  - `build/main` now carries `RUNPATH=$ORIGIN`, and `ldd build/main` resolves
    `liblightning.so.2` and `libreplxx.so.0.0.4` from `build/`.
  - `./build/main --help` executes successfully on this host.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep assuming the old `deps/lib/*.a` archives are usable on arm64;
    they were stale/incompatible and had to be rebuilt locally.
  - Do not assume `c3c` will compile the FTXUI C++ shim directly from the
    `.cpp` entries in `project.json`; the repo still relies on the separate
    `omni_ftxui` archive built by `scripts/build_omni_chelpers.sh`.
  - Do not assume arm64 stack-engine support exists just because the build now
    passes; the non-x86 path is fail-closed for context switching.
- Current best recommendation/checkpoint:
  - The build lane is healthy enough again to continue cleanup work or run
    targeted validation.
  - The next cleanup step should return to the unfinished `false`/`nil` sweep
    rather than more build plumbing.
- Unresolved issues:
  - Warnings remain in entry/reporting and REPL server code, but they are not
    build blockers.
  - Continuation/coroutine stack switching is still effectively x86_64-only.
- Signature: Codex (GPT-5)

2026-04-09 06:05 CEST - Documentation contract and compatibility hardening

- Objectives attempted:
  - Aggressively improve top-level documentation quality/coverage.
  - Remove remaining top-level documentation drift around legacy syntax and
    duplicate authority.
  - Establish one explicit compatibility source for removed/renamed surface
    syntax.
- Code/config/doc changes made:
  - Added documentation authority matrix and update discipline:
    - `docs/DOCS_CONTRACT.md`
  - Added canonical compatibility map for removed/changed syntax:
    - `docs/SURFACE_COMPATIBILITY.md`
  - Reworked top-level docs entrypoint and authority policy:
    - `docs/README.md`
  - Replaced `docs/FEATURES.md` with an index/coverage map (no longer a second
    full spec).
  - Updated reference navigation contract:
    - `docs/OMNI_REFERENCE.md`
  - Updated spec docs to reference authority/compatibility sources and remove
    duplicate legacy accessor examples:
    - `docs/LANGUAGE_SPEC.md`
    - `docs/SYNTAX_SPEC.md`
    - `docs/reference/03-collections.md`
- Commands run:
  - `jj status`
  - `jj diff --stat`
  - `rg` audits across updated docs for:
    - callable quoted-symbol accessor remnants,
    - removed alias spellings (`fn`, `begin`, `letrec`, `reset`, `shift`,
      `with-handlers`, `handle-chain`)
  - `sed` inspection passes on edited docs
- Key results and conclusions:
  - Documentation now has an explicit authority contract (`DOCS_CONTRACT`) and
    explicit compatibility contract (`SURFACE_COMPATIBILITY`).
  - `FEATURES.md` now acts as a high-signal index instead of duplicating large
    parts of `LANGUAGE_SPEC.md`.
  - Legacy callable quoted-symbol accessor syntax is no longer presented as
    active syntax in top-level/user-reference docs; it is now scoped to
    compatibility/removal policy.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating `FEATURES.md` as a second spec source; duplicate specs
    drift quickly and blur authority.
  - Do not scatter removed-syntax notes across many top-level docs; use
    `SURFACE_COMPATIBILITY.md` as the single compatibility source.
- Current best recommendation/checkpoint:
  - Keep future syntax-surface updates in one patch touching:
    - `SYNTAX_SPEC`,
    - `LANGUAGE_SPEC`,
    - `SURFACE_COMPATIBILITY`,
    - and `DOCS_CONTRACT` when authority/coverage scope changes.
- Unresolved issues:
  - This pass did not run compiler/runtime tests because changes were docs-only.
  - Historical plan/session artifacts still include legacy names by design;
    those files are historical records, not canonical language contracts.
- Signature: Codex (GPT-5)

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
  - Updated `TODO.md`, `.agents/PLAN.md`, `.agents/SESSION_REPORT.md`,
    `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/README.md`,
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
- Current best recommendation/checkpoint:
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
- Current best recommendation/checkpoint:
  - Continue Vulkan `Float32` from reducer and factor/solve kernels. Structural
    matrix kernels (`transpose`, `diagonal`, `diagonal-matrix`, `trace`) have
    landed with explicit Float32 shader/helper ABI names.
- Unresolved issues:
  - Vulkan `Float32` SVD-backed reducers, singular-value/SVD outputs, and
    factor/solve kernels remain fail-closed.
  - CUDA `Float32` placement and scalar `Float32` values remain fail-closed.
- Signature: Codex GPT-5.4

## 2026-04-18 18:37 CEST - Vulkan Fixed-Complex Singular Values And Norms

- Objective attempted:
  - Continue TENSOR-100F by landing the next fixed-width complex numerical
    matrix slice after QR/Cholesky: `matrix/singular-values` plus dependent
    spectral/nuclear `matrix/norm` selectors.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_tensor_matrix.c3`
  - `csrc/tensor_vulkan_singular_values_complex128.comp`
  - `csrc/tensor_vulkan_singular_values_complex64.comp`
  - Tensor Vulkan helper ABI and advanced stdlib matrix tests
- Code or configuration changes made:
  - Added CPU `Complex128`/`Complex64` singular-value helpers using
    realification and duplicate-pair collapse. Complex singular values return
    component-width real tensors: `Float64` for `Complex128`, `Float32` for
    `Complex64`.
  - Routed CPU complex `matrix/norm` `'spectral` and `'nuclear` through the
    same singular-value oracle while preserving the public `Float64` norm
    result.
  - Added Vulkan `Complex128`/`Complex64` singular-value shaders and helper
    exports. Vulkan complex singular-values return Vulkan-placed
    component-width real tensors, and Vulkan spectral/nuclear complex norms
    read back only the scalar `Float64` public result.
  - Kept full complex `matrix/svd` factor output fail-closed because its
    public contract needs complex `u`/`v` tensors and component-width real `s`.
  - Added a native validation guard so Vulkan fixed-complex singular-value
    shapes whose realified Jacobi iteration bound would overflow `uint` fail
    closed before shader dispatch, plus tests for non-diagonal lazy, wide,
    zero-size, spectral/nuclear, and overflow-guard behavior.
  - Updated TODO, active plan, language/reference docs, tensor area docs, and
    fixed-width complex contract notes so complex singular-values and
    spectral/nuclear norms are no longer described as deferred for CPU/Vulkan.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for
    `tensor_vulkan_singular_values_complex128.comp` and
    `tensor_vulkan_singular_values_complex64.comp`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes passed:
    - Complex128 singular-values copyback returned `("Float64" 5.0 2.0)`.
    - Complex64 singular-values copyback returned `("Float32" 5.0 2.0)`.
    - Vulkan complex spectral/nuclear norm smoke returned `(5.0 7.0)`.
  - Host focused `advanced-collections-module`: passed, `pass=1598 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1581 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and targeted diff hygiene
    passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating fixed-width complex `matrix/singular-values` or
    spectral/nuclear `matrix/norm` selectors as CPU/Vulkan deferred. They are
    now implemented and validated for `Complex128` and `Complex64`.
  - Do not infer full complex `matrix/svd` factor support from singular-value
    support; complex SVD factor output remains a separate ABI/result-contract
    item.
  - Do not rely only on storage-size validation for Vulkan fixed-complex
    singular-values. The realified Jacobi iteration bound has its own `uint`
    overflow limit and is now guarded explicitly.
- Current best recommendation/checkpoint:
  - Next fixed-width complex numerical work should target either full complex
    `matrix/svd` factor output, CUDA complex singular-values/norm selectors,
    or complex eigen/eigenpair result contracts. Preserve no-hidden-fallback
    tests for every backend path.
- Unresolved issues:
  - Full complex `matrix/svd` factor output remains fail-closed.
  - CUDA fixed-width complex singular-values, spectral/nuclear norms, and SVD
    remain unimplemented.
- Signature: Codex GPT-5.4

## 2026-04-17 20:42 CEST - TENSOR-100F Vulkan Float32 Direct Reducer Supersession

- Objective attempted:
  - Record that the later direct reducer slice supersedes the structural
    matrix handoff line that listed all Vulkan `Float32` reducers as
    fail-closed.
- Current best recommendation/checkpoint:
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
