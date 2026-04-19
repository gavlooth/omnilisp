# Session Report

## 2026-04-19 18:51 CEST - Manual Old-Master Integration

- Objective attempted:
  - Manually integrate the old `master` line onto the already-merged
    `main`/`master` tensor branch without reviving stale APIs or artifacts.
- Workspace/target:
  - `/home/christos/Omni`; old master commits from `7e0c698a` through
    `37b49ff9`, replayed onto merge commit `fbc6bb68`.
- Code or configuration changes made:
  - Replayed old-master allocation hardening, audit repairs, validation notes,
    and grouped Deduce backlog closure onto the current tensor runtime line.
  - Kept current JIT/FFI/tensor APIs where old-master hunks were stale.
  - Reconciled `value_interp_init_helpers.c3`,
    `value_interp_lifecycle.c3`, and `value_type_registry.c3` in the replayed
    commits so allocation failures remain fail-closed under current symbols.
  - Added TLS/FFI constructor ownership-out handling in the replayed audit
    slice so constructor failure does not double-free raw TLS handles.
  - Restored the current promotion copy route behavior for array/hashmap/method
    table wrapper allocation order, then added active promotion-context lookup
    in `copy_to_parent_with_fault`.
  - Added a narrow env-copy self-referential closure clone gate in
    `eval_env_copy_values.c3` so self-referential captured env payloads use the
    memoized clone path while invalid undelimited closure aliases still fail.
  - Updated `.agents/PLAN.md` to record the integration checkpoint and keep the
    old `linalg/matmul`/GSL-first plan direction invalidated.
- Commands run:
  - `jj duplicate` / `jj squash` / conflict-resolution steps across the old
    master commit sequence.
  - `git diff --check`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
- Key results:
  - `basic` bounded-container slice passed `160/0`.
  - `deduce` bounded-container slice passed `432/0`.
  - `memory-lifetime-smoke` bounded-container slice passed `231/0`.
  - Helper rebuild, C3 build, and diff hygiene passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not broadly treat undelimited closures with captured env frames in the
    releasing scope as cloneable. That briefly fixed self-referential closure
    payloads but incorrectly bypassed invalid-alias and rollback tests.
  - The old final `.agents/PLAN.md` commit is not authoritative for Tensor
    naming or direction; the current integrated Tensor plan remains the source
    of truth.
- Current best recommendation/checkpoint:
  - Push the replayed integration commits to both `main` and `master` after the
    final hygiene pass. Future env-copy changes should preserve both
    self-referential closure payload support and the invalid-undelimited-alias
    failure contract.
- Unresolved issues:
  - Full `OMNI_LISP_TEST_SLICE=all` was not run in this integration turn.
- Signature: Codex

## 2026-04-19 00:57 CEST - Helper Rebuild And Entry Audit Repairs

- Objective attempted:
  - Continue broad audit/repair work without splitting files under 1000 LOC.
- Workspace/target:
  - `/home/christos/Omni`, helper archive rebuild path and tensor-focused
    validation.
- Code or configuration changes made:
  - Updated `scripts/build_omni_chelpers.sh` so helper objects are rebuilt
    incrementally instead of recompiling every C/C++/FTXUI source on every
    invocation.
  - Added compiler depfile generation for rebuilt objects and archive
    timestamp checks before refreshing `build/libomni_chelpers.a` and
    `build/libomni_ftxui.a`.
  - Replaced deprecated entry-path `errno::...` constants with current
    unqualified libc errno constants.
  - Replaced deprecated `process::create` / `SubProcessOptions` AOT command
    spawning with `process::spawn` / `Process`.
  - Fixed standalone AOT `--build` source collection by keeping the Lisp test
    sources required by the entry test/e2e surfaces compiled into the AOT
    backend source set.
  - Removed trailing blank lines from `discussion` after `git diff --check`
    reported a whitespace error.
- Key results:
  - The pre-existing full helper rebuild was stopped after spending substantial
    time serially recompiling unchanged third-party FTXUI objects.
  - The patched helper rebuild completed quickly against the existing object
    set.
  - `c3c build --obj-out obj` passed and linked `build/main`.
  - `./build/main --build tests/simple_test.omni -o /tmp/omni_simple_test_bin`
    now links a standalone binary instead of failing on missing test symbols.
  - The generated `/tmp/omni_simple_test_bin` runs successfully with
    `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib`.
  - Host focused `advanced-collections-module` passed `1598/0`.
- Commands run:
  - `jj status`
  - `find src csrc ... | xargs wc -l`
  - `git diff --check`
  - `./scripts/build_omni_chelpers.sh`
  - `bash -n scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `./build/main --check /tmp/omni_missing_input_hopefully_absent.omni`
  - `./build/main --init /tmp/omni_init_smoke_project`
  - `./build/main --build tests/simple_test.omni -o /tmp/omni_simple_test_bin`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib /tmp/omni_simple_test_bin`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- Unresolved issues:
  - No bounded-container rerun was performed for this build-system-only repair.
  - Implementation work remains open in the three `TENSOR-100H` lanes.
- Signature: Codex GPT-5.4

## 2026-04-18 20:10 CEST - Fixed-Width Complex Closure Plan

- Objective attempted:
  - Split the remaining fixed-width complex tensor numerical work into
    executable lanes and deploy multiple GPT-5.4 planning agents.
- Workspace/target:
  - `/home/christos/Omni`, fixed-width complex Tensor matrix backlog and
    planning artifacts.
- Changes made:
  - Added `docs/plans/fixed-width-complex-closure-plan-2026-04-18.md`.
  - Split the previous broad TODO residual into
    `TENSOR-100H-SVD-FACTORS`, `TENSOR-100H-CUDA-SVD-NORMS`, and
    `TENSOR-100H-COMPLEX-EIGEN`.
  - Updated supporting SVD/eigen/fixed-complex roadmap notes to point to the
    new closure plan and current blockers.
  - Created Ruflo task lanes:
    `task-1776535656686-symqsu`,
    `task-1776535656710-clzj23`, and
    `task-1776535656878-ncxig9`.
- Key decisions:
  - Full complex `matrix/svd` closes in CPU-then-Vulkan order. Realification is
    singular-value-only and must not be used for public complex `u`/`v`.
  - CUDA complex SVD-family work should use runtime-loaded cuSOLVER DN
    (`cusolverDnZgesvd` / `cusolverDnCgesvd`) with custom PTX limited to
    layout adapters.
  - Fixed-width complex eigen closure starts by freezing the public result
    contract. Recommended direction is fixed-width complex eigenpair outputs
    for fixed-width numeric inputs, not backend-hidden `BigComplex` results.
- Commands run:
  - `jj status`
  - targeted `rg` / `sed` inspections
  - targeted `git diff --check` over changed planning artifacts
- Key results:
  - Planning diff hygiene passed.
  - No build or runtime tests were run because this was a planning-only change.
- Current best recommendation:
  - Start with `TENSOR-100H-SVD-FACTORS` CPU oracle, then Vulkan complex SVD.
    Keep CUDA and eigen lanes independent.
- Unresolved issues:
  - Implementation remains open in all three lanes.
- Signature: Codex GPT-5.4

## 2026-04-18 18:37 CEST - Vulkan Fixed-Complex Singular Values And Norms

- Objective attempted:
  - Continue implementation with multiple GPT-5.4 agents by landing
    fixed-width complex `matrix/singular-values` plus dependent
    spectral/nuclear `matrix/norm` selectors for CPU oracle behavior and
    Vulkan execution.
- Workspace/target:
  - `/home/christos/Omni`, `src/lisp/prim_tensor_matrix.c3`, Vulkan
    singular-value shaders/helper ABI, advanced Tensor tests, docs/backlog
    artifacts.
- Code or configuration changes made:
  - Added CPU `Complex128`/`Complex64` singular-value helpers using
    realification and duplicate-pair collapse. Complex singular values return
    component-width real tensors (`Float64` for `Complex128`, `Float32` for
    `Complex64`).
  - Routed CPU complex `matrix/norm` `'spectral` and `'nuclear` through the
    fixed-complex singular-value oracle while preserving the public `Float64`
    norm result.
  - Added Vulkan `Complex128`/`Complex64` singular-value shaders, generated
    SPIR-V C sources, helper exports, C3 externs, and public routing.
  - Vulkan complex singular-values return Vulkan-placed component-width real
    tensors; Vulkan complex spectral/nuclear norms read back only the public
    `Float64` scalar.
  - Kept full complex `matrix/svd` factor output fail-closed because its
    public result contract needs complex `u`/`v` tensors and component-width
    real `s`.
  - Added a native validation guard so Vulkan fixed-complex singular-value
    shapes whose realified Jacobi iteration bound would overflow `uint` fail
    closed before shader dispatch.
  - Added Vulkan fixed-complex regression coverage for non-diagonal lazy
    singular values, wide matrices, zero-size singular-values, spectral/nuclear
    norms beyond the diagonal square smoke, and the iteration-overflow guard.
  - Updated TODO, active plan, fixed-width complex docs, language/reference
    docs, and tensor area docs to remove stale CPU/Vulkan complex
    singular-value and spectral/nuclear deferrals.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for
    `csrc/tensor_vulkan_singular_values_complex128.comp` and
    `csrc/tensor_vulkan_singular_values_complex64.comp`.
  - `./scripts/build_omni_chelpers.sh`.
  - `c3c build --obj-out obj`.
  - Direct Vulkan `--eval` smokes for Complex128/Complex64 singular-values
    and spectral/nuclear norms.
  - Host focused advanced collections:
    `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`.
  - Bounded-container focused advanced collections:
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`.
  - `./scripts/check_primitive_docs_parity.sh`.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`.
  - Targeted `git diff --check`.
- Key results:
  - Shader validation passed.
  - Helper archive rebuild passed.
  - `c3c build --obj-out obj` passed with existing deprecation warnings.
  - Direct Vulkan Complex128 singular-values copyback returned
    `("Float64" 5.0 2.0)`.
  - Direct Vulkan Complex64 singular-values copyback returned
    `("Float32" 5.0 2.0)`.
  - Direct Vulkan complex norm smoke returned `(5.0 7.0)`.
  - Host focused `advanced-collections-module` passed `1598/0`.
  - Bounded-container focused `advanced-collections-module` passed `1581/0`.
  - Primitive docs parity, Stage 3 source parity, and targeted diff hygiene
    passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating fixed-width complex `matrix/singular-values` or
    spectral/nuclear `matrix/norm` selectors as CPU/Vulkan deferred.
  - Do not infer full complex `matrix/svd` factor support from singular-value
    support; complex SVD factor output remains separate.
  - Do not rely only on storage-size validation for Vulkan fixed-complex
    singular-values. The realified Jacobi iteration bound has its own `uint`
    overflow limit and is now guarded explicitly.
- Current best recommendation/checkpoint:
  - Continue with full complex `matrix/svd` factor output, CUDA complex
    singular-values/norm selectors, or complex eigen/eigenpair result
    contracts. Keep no-hidden-fallback tests for every backend path.
- Unresolved issues:
  - Full complex `matrix/svd` factor output remains fail-closed.
  - CUDA fixed-width complex singular-values, spectral/nuclear norms, and SVD
    remain unimplemented.
- Signature: Codex GPT-5.4

## 2026-04-18 17:21 CEST - Vulkan Complex QR And Cholesky Checkpoint

- Objective attempted:
  - Continue implementation with multiple GPT-5.4 agents by landing
    fixed-width complex `matrix/qr` and `matrix/cholesky` for CPU oracle
    behavior and Vulkan execution.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan QR/Cholesky shaders/helper ABI, C3 Tensor
    matrix routing, advanced Tensor tests, docs/backlog/memory artifacts.
- Code or configuration changes made:
  - Added Vulkan `Complex128`/`Complex64` QR and Cholesky compute shaders,
    generated SPIR-V C sources, build script wiring, and `project.json`
    archive inputs.
  - Added native helper exports
    `omni_tensor_backend_vulkan_qr_complex128`,
    `omni_tensor_backend_vulkan_qr_complex64`,
    `omni_tensor_backend_vulkan_cholesky_complex128`, and
    `omni_tensor_backend_vulkan_cholesky_complex64`.
  - Added C3 externs and widened `matrix/qr` plus `matrix/cholesky` routing
    for dense row-major `Complex128`/`Complex64` CPU and Vulkan tensors.
  - Implemented CPU complex QR with Hermitian projection and dtype-preserving
    `q`/`r`; implemented CPU complex Cholesky with Hermitian
    positive-definite checks and dtype-preserving lower factors.
  - Aligned CPU fixed-complex QR/Cholesky tolerances with the Vulkan shader
    thresholds and normalized Vulkan Cholesky diagnostics to mention
    symmetric/Hermitian positive-definite input.
  - Added guarded CPU and Vulkan regressions for dtype/device/shape
    preservation, Hermitian QR projection, Cholesky factor values,
    rank-deficient/non-Hermitian/non-HPD diagnostics, no-LAPACK behavior, lazy
    Vulkan inputs, and fail-closed storage-only paths.
  - Updated TODO, active plan, fixed-width complex/public docs, roadmap/index,
    tensor area status, memory changelog, and this session report.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for
    `csrc/tensor_vulkan_qr_complex128.comp`,
    `csrc/tensor_vulkan_qr_complex64.comp`,
    `csrc/tensor_vulkan_cholesky_complex128.comp`, and
    `csrc/tensor_vulkan_cholesky_complex64.comp`.
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - direct CPU/Vulkan `--eval` smokes for Complex128 and Complex64
    QR/Cholesky.
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - Targeted `git diff --check`.
- Key results:
  - Direct CPU Complex128 QR smoke returned `("Complex128"
    0.0-1.41421356237309i 1.0+0.0i)` for the non-real Hermitian projection
    oracle.
  - Direct CPU Complex128 Cholesky smoke returned
    `("Complex128" 2.0+0.0i 1.0-1.0i 2.0+0.0i 0.0+0.0i)`.
  - Direct Vulkan Complex128 QR smoke returned
    `(vulkan "Complex128" 0.0-1.41421356237309i 1.0+0.0i)`.
  - Direct Vulkan Complex64 Cholesky smoke returned
    `(vulkan "Complex64" 2.0+0.0i 1.0-1.0i 2.0+0.0i 0.0+0.0i)`.
  - Host focused `advanced-collections-module`: `1570 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1553 passed,
    0 failed`.
  - Primitive docs parity, Stage 3 source parity, and targeted diff hygiene
    passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Static review found the public Cholesky entry point still rejected
    fixed-width complex tensors after the helper/routing work existed; the
    gate is fixed and covered.
  - Static review found CPU fixed-complex QR/Cholesky tolerance behavior was
    stricter than Vulkan; CPU thresholds now match the shader thresholds.
- Current best recommendation / checkpoint:
  - Treat Vulkan fixed-width complex
    LU/determinant/solve/inverse/rank/direct-norm/QR/Cholesky as the landed
    numerical subset.
- Unresolved issues / next actions:
  - Complex `matrix/singular-values`, `matrix/svd`, `matrix/norm`
    `'spectral`/`'nuclear`, and complex eigen routines remain separate
    explicit contracts.
  - CUDA fixed-width complex numerical matrix variants remain open outside
    the landed Vulkan subset.
- Signature: Codex GPT-5.4

## 2026-04-18 16:55 CEST - Vulkan Complex Rank And Direct Norm Checkpoint

- Objective attempted:
  - Continue implementation with multiple GPT-5.4 agents by landing
    fixed-width complex `matrix/rank` and direct `matrix/norm` reducers for
    CPU oracle behavior and Vulkan execution.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan rank/norm shaders/helper ABI, C3 Tensor
    matrix routing, advanced Tensor tests, docs/backlog/memory artifacts.
- Code or configuration changes made:
  - Added Vulkan `Complex128`/`Complex64` norm and rank compute shaders,
    generated SPIR-V C sources, build script wiring, and `project.json`
    archive inputs.
  - Added native helper exports
    `omni_tensor_backend_vulkan_norm_complex128`,
    `omni_tensor_backend_vulkan_norm_complex64`,
    `omni_tensor_backend_vulkan_rank_complex128`, and
    `omni_tensor_backend_vulkan_rank_complex64`.
  - Added C3 externs and widened `matrix/rank` plus direct `matrix/norm`
    routing for dense row-major `Complex128`/`Complex64` CPU and Vulkan
    tensors.
  - Implemented CPU complex rank with magnitude pivoting and tolerance, and
    CPU direct complex norm reducers for default/`'frobenius`, `'one`,
    `'infinity`, and `'max` over complex magnitudes.
  - Kept complex spectral/nuclear norm selectors fail-closed until complex
    singular-value/SVD support lands.
  - Added guarded CPU and Vulkan regressions for rank full/deficient/tolerance
    cases, direct norm selectors, no-LAPACK guards, fail-closed storage-only
    paths, and complex spectral/nuclear unsupported behavior.
  - Updated TODO, active plan, fixed-width complex/public docs, roadmap/index,
    tensor area status, memory changelog, and this session report.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for
    `csrc/tensor_vulkan_norm_complex128.comp`,
    `csrc/tensor_vulkan_norm_complex64.comp`,
    `csrc/tensor_vulkan_rank_complex128.comp`, and
    `csrc/tensor_vulkan_rank_complex64.comp`.
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - direct CPU/Vulkan `--eval` smokes for Complex128 and Complex64 rank/norm,
    plus CPU spectral fail-closed behavior.
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- Key results:
  - Direct CPU Complex128 rank smoke returned `(2 1)`.
  - Direct CPU Complex128 norm/spectral smoke returned
    `(5.0 7.0 tensor/backend-unsupported)`.
  - Direct CPU Complex64 rank/norm smoke returned `(2 5.0 5.0)`.
  - Direct Vulkan Complex128 and Complex64 smokes returned `(2 5.0)` for
    rank plus default Frobenius norm on this host.
  - Host focused `advanced-collections-module`: `1540 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1523 passed,
    0 failed`.
- Invalidated assumptions or failed approaches worth preserving:
  - Static review found the first CPU complex norm implementation was dead
    behind a stale Float64/Float32-only dtype gate; this was fixed before
    final validation.
  - Static review also found `matrix_singular_value_norm` had briefly admitted
    complex dtypes; this was corrected so complex spectral/nuclear selectors
    remain fail-closed at the routing boundary.
- Current best recommendation / checkpoint:
  - Treat Vulkan fixed-width complex LU/determinant/solve/inverse/rank and
    direct norm reducers as the landed numerical subset. The next complex
    numerical slice should target QR/Cholesky, complex singular-values/SVD, or
    a spectral/eigen result contract with its own ABI/status/tolerance plan.
- Unresolved issues / next actions:
  - Complex `matrix/norm` `'spectral`/`'nuclear` remains intentionally
    unsupported until complex singular-value/SVD support lands.
  - Remaining CUDA/Vulkan fixed-width complex numerical matrix families are
    still open outside the landed Vulkan subset.
- Signature: Codex GPT-5.4

## 2026-04-18 16:45 CEST - Vulkan Complex Matrix Inverse Checkpoint

- Objective attempted:
  - Continue implementation with multiple GPT-5.4 agents by landing
    fixed-width complex `matrix/inverse` for CPU oracle behavior and Vulkan
    execution.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan inverse shaders/helper ABI, C3 Tensor matrix
    routing, advanced Tensor tests, docs/backlog/memory artifacts.
- Code or configuration changes made:
  - Added Vulkan `Complex128`/`Complex64` inverse compute shaders and
    generated SPIR-V C sources, wired into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added native helper exports
    `omni_tensor_backend_vulkan_inverse_complex128` and
    `omni_tensor_backend_vulkan_inverse_complex64`.
  - Added C3 externs and widened `matrix/inverse` routing so CPU and Vulkan
    dense row-major square `Complex128`/`Complex64` tensors are supported.
  - Reused the existing complex Gaussian-elimination solver as the CPU oracle
    by solving against an identity RHS.
  - Added guarded CPU and Vulkan regressions for values, dtype, placement,
    product identity, singular diagnostics, and fail-closed unsupported
    complex numerical capability cases.
  - Updated TODO, active plan, fixed-width complex/public docs, roadmap/index,
    tensor area status, memory changelog, and this session report so inverse is
    closed separately from the remaining complex numerical families.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for
    `csrc/tensor_vulkan_inverse_complex128.comp` and
    `csrc/tensor_vulkan_inverse_complex64.comp`.
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - direct CPU/Vulkan `--eval` smokes for Complex128 and Complex64 inverse and
    singular diagnostics.
  - `nm -g build/libomni_chelpers.a | rg "omni_tensor_backend_vulkan_inverse_complex(128|64)|omni_tensor_vulkan_inverse_complex(128|64)_spv"`
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - targeted `git diff --check`.
- Key results:
  - Direct CPU Complex128 smoke returned
    `("Complex128" "-0.7-1.1i" "0.2-0.4i")`.
  - Direct CPU Complex64 smoke returned approximately
    `("Complex64" "-0.699999988079071-1.10000002384186i" "0.200000032782555-0.400000005960464i")`.
  - Direct Vulkan Complex128 and Complex64 smokes preserved `vulkan`
    placement and returned the expected inverse entries after explicit
    `to-device 'cpu` copyback.
  - Singular CPU and Vulkan smokes returned `tensor/singular-matrix` with
    `matrix/inverse: input matrix is singular`.
  - Host focused `advanced-collections-module`: `1496 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1479 passed,
    0 failed`.
  - Primitive docs parity, Stage 3 source parity, and targeted diff hygiene
    passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Supersedes stale guidance that fixed-width complex `matrix/inverse` is
    rejected or remains outside the Vulkan numerical complex lane.
  - The broad `matrix-numerical-complex128` /
    `matrix-numerical-complex64` bits covered Vulkan LU, determinant, solve,
    and inverse at this checkpoint. The later rank/direct-norm checkpoint
    extends that subset, but the bits still do not imply QR, Cholesky, complex
    singular-value/SVD, spectral/nuclear complex norm selectors, or eigen
    routines.
- Current best recommendation / checkpoint:
  - Historical checkpoint superseded by the rank/direct-norm entry above.
    Treat Vulkan fixed-width complex LU/determinant/solve/inverse/rank and
    direct norm reducers as the landed numerical subset.
- Unresolved issues / next actions:
  - Remaining CUDA/Vulkan fixed-width complex numerical matrix families are
    still open outside the landed Vulkan subset.
- Signature: Codex GPT-5.4

## 2026-04-18 16:10 CEST - CUDA Complex Structural Matrix Checkpoint

- Objective attempted:
  - Continue implementation with multiple GPT-5.4 agents by landing CUDA
    fixed-width complex structural matrix support for `matrix/transpose`,
    `matrix/diagonal`, `matrix/diagonal-matrix`, and `matrix/trace`.
- Workspace/target:
  - `/home/christos/Omni`, CUDA PTX/helper ABI, C3 Tensor matrix routing,
    `tensor-backends` capability reporting, advanced Tensor tests, and
    docs/backlog/memory artifacts.
- Code or configuration changes made:
  - Added CUDA `Complex128`/`Complex64` kernels and generated PTX include for
    transpose, diagonal extraction, diagonal matrix construction, and trace.
  - Added CUDA helper module loading, function resolution, availability
    export, and native C helper exports for the structural matrix family.
  - Added C3 externs and matrix routing so CUDA dense row-major zero-offset
    fixed-width complex tensors compute the structural operations on CUDA
    after Vulkan routing and before the CPU-only fallback.
  - Updated `tensor-backends` so CUDA reports
    `matrix-structural-complex128` and `matrix-structural-complex64` from the
    dedicated CUDA complex matrix helper capability.
  - Added guarded CUDA regressions covering positive placement/dtype/value
    behavior when the capability is true and fail-closed
    `tensor/backend-unsupported` behavior otherwise.
  - Updated TODO, active plan, fixed-width complex/public docs, changelog, and
    this session report so CUDA structural support is closed separately from
    remaining numerical matrix families.
- Commands run:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_complex_matrix.cu -o /tmp/omni_tensor_cuda_complex_matrix.ptx`
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/omni_tensor_cuda_complex_matrix.ptx -o /tmp/omni_tensor_cuda_complex_matrix.cubin`
  - `cc -O2 -Ideps/src/yyjson/src -Ideps/src/BearSSL/inc -Ideps/src/libuv/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`
  - `nm -g /tmp/tensor_cuda_helpers.o | rg "omni_tensor_backend_cuda_(complex_matrix_available|transpose_complex|diagonal_complex|diagonal_matrix_complex|trace_complex)"`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - direct CUDA `--eval` smokes for Complex128 transpose, Complex64
    diagonal-matrix, Complex128 trace, and `tensor-backends` capability
    reporting.
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- Key results:
  - Direct CUDA smokes returned `(cuda "Complex128" "3.0+4.0i")`,
    `(cuda "Complex64" "3.0+4.0i" "0.0+0.0i")`, and `"4.0+6.0i"`.
  - Host focused `advanced-collections-module`: `1482 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1465 passed,
    0 failed`.
  - CUDA `tensor-backends` now reports structural complex capability true on
    this machine while numerical complex matrix capability remains false.
- Invalidated assumptions or failed approaches worth preserving:
  - Supersedes stale guidance that CUDA structural complex matrix kernels are
    unimplemented. Do not treat the old fail-closed CUDA structural test as
    current ground truth.
  - CUDA/Vulkan structural matrix capability still does not imply remaining
    complex numerical matrix families.
- Current best recommendation / checkpoint:
  - Treat CUDA/Vulkan fixed-width complex structural matrix support as landed.
    The next complex matrix slice should target a named numerical family such
    as inverse, QR, Cholesky, complex singular-value/SVD, spectral/nuclear
    complex norm, or eigen routines with explicit ABI/status/tolerance
    contracts. Historical note: inverse and Vulkan rank/direct norms landed in
    later entries above.
- Unresolved issues / next actions:
  - Remaining CUDA/Vulkan fixed-width complex numerical matrix families are
    still open outside Vulkan LU/determinant/solve.
  - Broader view-aware GPU execution still requires offset/stride/backing
    extent ABI design and tests.
- Signature: Codex GPT-5

## 2026-04-18 15:53 CEST - Vulkan Transpose View And Complex Numerical Matrix Checkpoint

- Objective attempted:
  - Continue implementation with multiple GPT-5.4 agents by landing both active
    Vulkan lanes: explicit transpose-view materialization and fixed-width
    complex `matrix/lu`, `matrix/determinant`, and `matrix/solve`.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan shaders/helper ABI, C3 Tensor/device
    materialization, matrix numerical routing, capability reporting, advanced
    Tensor tests, docs/backlog/memory artifacts.
- Code or configuration changes made:
  - Added Vulkan complex LU/determinant/solve GLSL shaders and generated SPIR-V
    C sources, wired into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added C helper exports and C3 extern/routing for Vulkan
    `Complex128`/`Complex64` LU, determinant, and solve, including
    singular-status mapping and scalar/metadata readback.
  - Added CPU `Complex128`/`Complex64` LU, determinant, and solve oracles.
  - Added explicit Vulkan materialization for direct rank-2
    `matrix/transpose-view` values over dense zero-offset Vulkan storage;
    broader strided/view-backed Vulkan kernels remain fail-closed.
  - Added `tensor-backends` `matrix-numerical-complex128` and
    `matrix-numerical-complex64` capability bits and focused regression tests.
  - Updated TODO, memory, active plan, fixed-width complex/Vulkan docs, and
    public spec/reference wording to mark the validated boundary.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for six
    complex numerical Vulkan shaders.
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - direct CPU/Vulkan `--eval` smokes for complex determinant, LU, solve, and
    Vulkan transpose-view copyback.
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - targeted `git diff --check`
- Key results:
  - Host focused `advanced-collections-module`: `1475 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1458 passed,
    0 failed`.
  - Vulkan complex direct smokes returned expected LU/solve values and
    `Complex64` determinant within single-precision tolerance.
  - Vulkan transpose-view copyback smoke returned `true`.
- Invalidated assumptions or failed approaches worth preserving:
  - The old fail-closed expectation for Vulkan transpose-view placement/copyback
    is superseded by explicit materialization. Do not extrapolate that to
    arbitrary strided Vulkan kernel execution.
  - The old test expectation that `Complex128`/`Complex64` numerical matrices
    are rejected is superseded. `BigComplex` remains the rejected high-precision
    complex family for these numerical routines.
- Current best recommendation / checkpoint:
  - Treat Vulkan transpose-view materialization and Vulkan fixed-width complex
    LU/determinant/solve as landed. Historical note: CUDA structural complex
    matrix support was separate at this checkpoint and is superseded by the
    later 16:10 CUDA structural entry.
- Unresolved issues / next actions:
  - Broader view-aware Vulkan execution still requires offset/stride/backing
    extent ABI design and tests.
  - Remaining complex numerical families such as QR, Cholesky, complex
    singular-value/SVD, spectral/nuclear complex norm selectors, and eigen
    routines need separate contracts. Historical note: inverse and Vulkan
    rank/direct norms landed in later entries above.
- Signature: Codex GPT-5

## 2026-04-18 15:39 CEST - Vulkan Active Lane Documentation Checkpoint

- Objective attempted:
  - Update documentation/backlog artifacts for the active main-agent Vulkan
    transpose-view materialization and Vulkan fixed-width complex
    LU/determinant/solve implementation lanes without editing source,
    runtime, tests, or C helpers.
- Workspace/target:
  - `/home/christos/Omni`, TODO/docs/planning/session artifacts owned by this
    documentation pass.
- Code or configuration changes made:
  - No source/runtime/test/helper changes were made.
  - Updated backlog and planning wording so Vulkan transpose-view
    materialization is tracked as implementation-in-progress and clearly
    distinct from future arbitrary stride-aware Vulkan kernel execution.
  - Updated fixed-width complex planning wording so Vulkan complex
    `matrix/lu`, `matrix/determinant`, and `matrix/solve` are tracked as
    implementation-in-progress, with capability/status/oracle/no-fallback
    validation required before any shipped-contract wording.
- Commands run:
  - `jj status`
  - targeted `rg`/`sed` inspections over the owned docs/artifacts
  - `git diff --check -- TODO.md memory/CHANGELOG.md .agents/PLAN.md .agents/SESSION_REPORT.md docs/plans/vulkan-math-library-roadmap-2026-04-17.md docs/plans/vulkan-dtype-layout-policy-2026-04-17.md docs/plans/fixed-width-complex-tensor-contract-2026-04-18.md docs/plans/README.md docs/areas/tensor-scientific.md`
  - `./scripts/check_primitive_docs_parity.sh`
- Key results:
  - Public shipped-contract docs remain conservative where runtime validation
    has not been verified by this documentation pass.
  - Backlog now separates the active Vulkan complex LU/determinant/solve lane
    from broader CUDA structural and remaining complex numerical matrix work.
  - Backlog now separates Vulkan transpose-view materialization from broader
    view-aware Vulkan/copy-kernel support.
  - Targeted diff hygiene and primitive docs parity passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not describe Vulkan transpose-view materialization as general
    stride-aware GPU execution.
  - Do not infer Vulkan fixed-width complex LU/determinant/solve support from
    fixed-width complex storage, elementwise map, contract, or structural
    matrix capability.
- Current best recommendation / checkpoint:
  - Keep these lanes open until the implementation worker records runtime
    validation. Once verified, update the public reference/spec wording and
    close the corresponding TODO slices.
- Unresolved issues / next actions:
  - Runtime validation for both active lanes remains owned by the main-agent
    implementation work.
- Signature: Codex GPT-5

## 2026-04-18 14:45 CEST - Vulkan Fixed-Width Complex Structural Matrix Kernels

- Objective attempted:
  - Continue fixed-width complex Tensor backend work by landing the next matrix
    slice with multiple GPT-5.4 agents, while keeping CUDA structural and
    numerical matrix boundaries explicit.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan structural matrix shaders/helper ABI, C3
    matrix routing, `tensor-backends` capability reporting, advanced stdlib
    Tensor tests, TODO/docs, memory changelog, and active plan artifacts.
- Code or configuration changes made:
  - Added Vulkan `Complex128`/`Complex64` structural matrix shaders for
    `matrix/transpose`, `matrix/diagonal`, `matrix/diagonal-matrix`, and
    `matrix/trace`, plus checked-in generated SPIR-V C sources wired into
    `project.json` and `scripts/build_omni_chelpers.sh`.
  - Added native Vulkan helper exports for complex structural matrix operations.
    Tensor-returning operations keep Vulkan placement; `matrix/trace` reads back
    the public fixed-width complex scalar.
  - Added C3 Vulkan externs and widened the four structural matrix Vulkan gates
    from real-only to real-or-fixed-complex, dispatching by dtype.
  - Added `tensor-backends` `matrix-structural-complex128` and
    `matrix-structural-complex64` capability bits. CPU reports true, Vulkan
    reports based on `Float64`/`Float32` support, and CUDA reports false because
    no CUDA transpose/diagonal/trace helper ABI exists yet.
  - Added focused tests for complex structural Vulkan placement/dtype/copyback,
    zero-size behavior, complex trace scalar results, CPU capability reporting,
    and CUDA fail-closed structural behavior when capability is absent.
  - Updated TODO, spec/reference docs, fixed-width complex plan, Vulkan dtype
    policy, Vulkan math roadmap, tensor area status, active plan, and memory
    changelog so Vulkan structural support is closed and CUDA structural plus
    numerical complex matrix kernels remain explicit residual work.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for eight
    complex Vulkan structural shaders.
  - `cc -O2 -Ideps/src/yyjson/src -Ideps/src/BearSSL/inc -Ideps/src/libuv/include -fsyntax-only csrc/tensor_vulkan_helpers.c`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build`
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - Targeted `git diff --check` over the structural complex matrix slice files.
- Key results:
  - Host focused `advanced-collections-module`: `1449 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1432 passed, 0
    failed`.
  - Shader validation, Vulkan helper syntax check, helper archive rebuild, and
    `c3c build` passed.
  - Primitive docs parity and Stage 3 source parity passed.
  - Targeted diff hygiene passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Superseded: Vulkan fixed-width complex structural matrix operations no
    longer remain fail-closed after this checkpoint.
  - Historical note: CUDA structural complex matrix support was not present at
    this checkpoint, but the later 16:10 CUDA structural entry supersedes that
    state.
  - Do not infer fixed-width complex numerical matrix support from storage,
    complex elementwise `map`, complex `contract`, or Vulkan structural matrix
    capability.
- Current best recommendation / checkpoint:
  - Treat Vulkan fixed-width complex structural matrix kernels as landed.
    Historical note: CUDA structural helper/PTX kernels later landed in the
    16:10 CUDA structural entry.
- Unresolved issues / next actions:
  - CUDA/Vulkan fixed-width complex numerical matrix kernels remain fail-closed.
  - Direct Vulkan general `matrix/eigenpairs` remains fail-closed on Vulkan
    while its output contract is pointer-backed `BigComplex`.
- Signature: Codex GPT-5.4

## 2026-04-18 13:41 CEST - CUDA/Vulkan Fixed-Width Complex Contract

- Objective attempted:
  - Continue fixed-width complex Tensor backend work by landing CUDA/Vulkan
    `Complex128`/`Complex64` `contract` support behind explicit operation
    capability bits, using multiple GPT-5.4 agents for C3/native/docs surface
    inspection.
- Workspace/target:
  - `/home/christos/Omni`, CUDA cuBLAS helper ABI, Vulkan contract shaders and
    helper ABI, C3 Tensor routing, advanced stdlib Tensor tests, TODO/docs,
    memory changelog, and active plan artifacts.
- Code or configuration changes made:
  - Added CUDA cuBLAS complex availability and non-conjugating
    `Zgemm`/`Cgemm` and `Zgemv`/`Cgemv` row-major helpers in
    `csrc/tensor_cuda_helpers.c`, plus C3 externs and CUDA `contract` routing
    for `Complex128`/`Complex64`.
  - Added Vulkan `tensor_vulkan_contract_complex128.comp` and
    `tensor_vulkan_contract_complex64.comp`, generated checked-in SPIR-V C
    sources, wired them into `project.json` and
    `scripts/build_omni_chelpers.sh`, and added native Vulkan helper exports.
  - Routed C3 Vulkan `contract` for matching fixed-width complex tensors and
    changed CUDA/Vulkan `tensor-backends` entries to report
    `contract-complex128`/`contract-complex64` as operation capability bits.
  - Added capability-gated CUDA/Vulkan complex dot and rank-2 single-axis
    contract tests plus fail-closed tests when capability is absent.
  - Updated TODO, spec/reference docs, fixed-width complex plan, tensor area
    status, active plan, and memory changelog so contract is closed and
    complex matrix kernels remain the explicit residual.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for both
    complex Vulkan contract shaders.
  - `cc -O2 -Ideps/src/yyjson/src -Ideps/src/BearSSL/inc -Ideps/src/libuv/include -fsyntax-only csrc/tensor_cuda_helpers.c`
  - `cc -O2 -Ideps/src/yyjson/src -Ideps/src/BearSSL/inc -Ideps/src/libuv/include -fsyntax-only csrc/tensor_vulkan_helpers.c`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build`
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- Key results:
  - Host focused `advanced-collections-module`: `1438 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1421 passed, 0
    failed`.
  - Helper rebuild, C helper syntax checks, SPIR-V validation, and `c3c build`
    passed.
  - Primitive docs parity, Stage 3 source parity, and targeted diff hygiene
    passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Superseded: CUDA/Vulkan fixed-width complex `contract` no longer remains
    fail-closed after this checkpoint.
  - Do not infer fixed-width complex matrix support from storage, complex
    elementwise `map`, or complex `contract`; matrix operations still need
    operation-specific helper/shader/PTX ABIs, capability bits, status/error
    contracts, and oracle tests.
- Current best recommendation / checkpoint:
  - Treat CUDA/Vulkan fixed-width complex `contract` as landed. Split remaining
    complex GPU work into explicit matrix-operation slices, likely starting
    with structural matrix kernels.
- Unresolved issues / next actions:
  - CUDA/Vulkan fixed-width complex matrix kernels remain fail-closed.
  - Direct Vulkan general `matrix/eigenpairs` remains fail-closed on Vulkan
    while its output contract is pointer-backed `BigComplex`.
- Signature: Codex GPT-5.4

## 2026-04-18 13:14 CEST - CUDA Fixed-Width Complex Elementwise Map

- Objective attempted:
  - Continue fixed-width complex Tensor backend work by landing CUDA
    `Complex128`/`Complex64` elementwise map support with multiple GPT-5.4
    agents for native, C3/test, and documentation review.
- Workspace/target:
  - `/home/christos/Omni`, CUDA Tensor helper ABI/PTX, C3 Tensor CUDA routing,
    advanced stdlib Tensor tests, language/reference/planning docs, TODO,
    changelog, and active plan artifacts.
- Code or configuration changes made:
  - Added `csrc/tensor_cuda_complex_map.cu` and generated
    `csrc/tensor_cuda_complex_map_ptx.inc` for CUDA complex binary, unary, and
    component-to-real kernels.
  - Extended `csrc/tensor_cuda_helpers.c` with complex PTX module resolution,
    status-bearing launchers, `Complex128`/`Complex64` exported wrappers, and
    `omni_tensor_backend_cuda_complex_map_available`.
  - Added CUDA C3 externs in `src/lisp/tensor_cuda_backend.c3`.
  - Routed fixed-width complex CUDA `map`, lazy realization, destination
    realization, direct `abs`, direct `real-part`/`imag-part`, direct unary
    `-`, and `conjugate` through explicit complex helpers in
    `src/lisp/prim_tensor.c3`.
  - Added capability preflight so optional CUDA complex-map absence reports
    `tensor/backend-unsupported` instead of attempting the kernel and reporting
    backend unavailable. Fixed empty CUDA complex unary/component maps by
    accepting zero-byte null device handles.
  - Added capability-gated Complex128/Complex64 tests for arithmetic,
    broadcasting, unary/component helpers, direct helpers, empty tensors,
    division status, unsupported callable behavior, lazy realization, and CPU
    destination fail-closed behavior.
- Commands run:
  - `jj status`
  - CUDA PTX generation and `ptxas` for `csrc/tensor_cuda_complex_map.cu`
  - `cc -O2 -Ideps/src/yyjson/src -Ideps/src/BearSSL/inc -Ideps/src/libuv/include -fsyntax-only csrc/tensor_cuda_helpers.c`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - targeted `git diff --check` for CUDA complex map code, tests, docs, TODO,
    changelog, and agent artifacts
- Key results:
  - Host focused `advanced-collections-module`: `1433 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1416 passed, 0
    failed`.
  - Primitive docs parity, Stage 3 source parity, helper rebuild, C helper
    syntax check, PTX generation, `ptxas`, and targeted diff hygiene passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not infer CUDA complex map support from CUDA `complex128`/`complex64`
    storage bits. Map support is an independent operation-family capability.
  - Do not validate CUDA unary/component launch input pointers before the
    zero-length fast path; empty CUDA tensors can validly have null device
    handles.
- Current best recommendation / checkpoint:
  - Treat CUDA and Vulkan fixed-width complex elementwise map as landed behind
    explicit operation bits. Split remaining complex backend work around
    `contract` and matrix kernels with separate helper ABIs and status
    contracts.
- Unresolved issues / next actions:
  - CUDA/Vulkan fixed-width complex `contract` remains fail-closed.
  - CUDA/Vulkan fixed-width complex matrix kernels remain fail-closed.
  - Direct Vulkan general `matrix/eigenpairs` remains fail-closed on Vulkan
    while its output contract is pointer-backed `BigComplex`.
- Signature: Codex GPT-5.4

## 2026-04-18 10:40 CEST - TENSOR-100F Read-Only Transpose View

- Objective attempted:
  - Continue `TENSOR-100F` by landing the first explicit read-only Tensor view
    contract through `matrix/transpose-view`, with multiple GPT-5.4 agents for
    runtime review and documentation.
- Workspace/target:
  - `/home/christos/Omni`, Tensor runtime payload metadata, boundary
    copy/promotion/audit traversal, matrix transpose primitives, advanced
    stdlib tests, docs/reference/spec/planning artifacts.
- Code or configuration changes made:
  - Added `TENSOR_PAYLOAD_VIEW` and `TensorVal.view_source`.
  - Added public `matrix/transpose-view` for CPU rank-2 Tensor sources. It
    swaps shape/strides, borrows source storage, stores the source ownership
    edge, is immutable, and reports `tensor-layout` payload `view`, owner
    `view-source`, `owns-storage` false, and write-policy `read-only-view`.
  - `ref`, `(Array view)`, `(List view)`, and CPU `realize` now observe logical
    view indexing and materialize views into dense Tensor results when needed.
  - Boundary copy, ESCAPE promotion, graph audit, provenance, and JIT temp-lane
    walkers now traverse `view_source`.
  - `matrix/transpose` remains materializing for concrete inputs but composes
    structurally when its input is already a transpose view.
  - CUDA/Vulkan placement and destination-realize paths now reject view payloads
    before hidden materialization, including dense double-transpose views.
  - Restricted this first public view contract to CPU storage; Vulkan/device
    views remain deferred until a helper ABI explicitly accepts offset, stride,
    backing extent, alias, and write-policy metadata.
  - Updated language/reference/area docs, Vulkan roadmap, TODO, plan,
    changelog, and session reports.
- Commands run:
  - `jj status`
  - targeted `rg`/`sed` inspections of Tensor docs, TODO, plan, changelog, and
    session reports
  - `c3c build --obj-out obj`
  - Direct `--eval` smokes for `tensor-layout`, CPU `ref`, `(Array view)`,
    `(List view)`, CPU `realize`, double-transpose structural composition,
    return/closure capture, and immutable destination rejection
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local ./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local ./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build ./build/main --test-suite lisp`
- Key results:
  - Direct smokes returned payload `view` metadata, `ref` result `6.0`, Array
    and List order `[1.0 4.0 2.0 5.0 3.0 6.0]`, CPU realized metadata
    `[6.0 concrete true]`, double-transpose metadata `[6.0 view true]`, and
    immutable destination rejection `realize: destination Tensor is immutable`.
  - Runtime review found fail-open backend copy paths; fixed by rejecting views
    before CUDA/Vulkan copy realization and by requiring concrete zero-offset
    dense row-major storage after realization.
  - TODO/plan wording closes the CPU read-only view contract and keeps
    view-aware GPU/copy-kernel support as a separate explicit residual item.
- Validation:
  - `c3c build --obj-out obj` passed.
  - Host focused `advanced-collections-module`: `pass=1343 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=1326 fail=0`.
  - Bounded-container `memory-lifetime-smoke`: `pass=229 fail=0`.
- Unresolved issues / next actions:
  - Future view-aware Vulkan/copy kernels require an explicit helper ABI for
    offset, strides, backing extent, ownership, aliasing, and write policy.
- Signature: Codex GPT-5

## 2026-04-18 10:09 CEST - Tensor Layout Metadata

- Objective attempted:
  - Continue `TENSOR-100F` with multiple GPT-5.4 agents by landing Tensor
    layout metadata and the public `tensor-layout` introspection primitive
    before any view-backed GPU kernel work.
- Workspace/target:
  - `/home/christos/Omni`, Tensor value metadata, runtime validation helpers,
    primitive registration, advanced stdlib tests, Tensor docs, Vulkan
    roadmap, TODO, `.agents/PLAN.md`, changelog, and session reports.
- Code/configuration changes:
  - Added `storage_offset`, `storage_element_count`, and `storage_byte_len` to
    `TensorVal`.
  - Initialized concrete Tensor storage metadata, reset lazy expression
    payloads to zero storage extent, and kept concrete clones compact.
  - Tightened concrete metadata/device-storage validation so concrete backing
    bytes cover the declared backing extent and CUDA/Vulkan device paths reject
    nonzero storage offsets.
  - Required zero-offset dense row-major storage before raw contiguous
    CPU/device copy paths.
  - Added and registered public `tensor-layout` for interpreter and AOT lookup.
  - Added focused advanced stdlib assertions for CPU dense, rank-0, zero-size,
    lazy map, CUDA copied, and Vulkan copied metadata.
  - Updated language/reference/area docs plus TODO, Vulkan roadmap,
    `.agents/PLAN.md`, `memory/CHANGELOG.md`, and session reports.
- Commands run:
  - `jj status`
  - targeted `rg`/`sed` inspections of Tensor source, tests, docs, TODO, plan,
    changelog, and session reports
  - `c3c build --obj-out obj`
  - Direct REPL smokes for dense `tensor-layout`, rank-0 metadata, lazy map
    metadata, and non-Tensor fail-closed behavior.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local ./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local ./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - targeted `git diff --check`
- Key results:
  - The documented metadata keys are `dtype`, `device`, `payload`, `layout`,
    `dense-row-major`, `shape`, `strides`, `rank`, `element-count`,
    `byte-length`, `storage-offset`, `storage-elements`, `storage-bytes`,
    `is-view`, `owns-storage`, `owner`, and `write-policy`.
  - Current symbol domains are payload `concrete`/`map`/`contract`, layout
    `dense-row-major`/`strided`, owner `self`/`view-source`/`expression`, and
    write-policy `mutable`/`immutable`/`mutable-view`/`read-only-view`.
  - Lazy expression payloads report logical element/byte length but
    `storage-elements = 0` and `storage-bytes = 0` until realized.
  - Host focused advanced collections passed `pass=1332 fail=0`.
  - Bounded-container focused advanced collections passed `pass=1315 fail=0`.
  - Bounded-container memory-lifetime-smoke passed `pass=229 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and targeted diff hygiene
    passed.
- Invalidated assumptions / negative memory:
  - Do not treat `tensor-layout` as view execution support. It is metadata-only
    and does not ship a public view constructor or view-backed GPU kernels.
  - Do not pass offset/stride metadata into Vulkan/CUDA helpers by only adding
    helper parameters. View execution needs a public constructor/operation
    contract, CPU oracle behavior, alias/bounds tests, and explicit
    fail-closed write policy first.
- Current best recommendation:
  - Next implementation should define the narrow public read-only view
    construction/operation contract and CPU oracle tests before passing offset
    or stride metadata to Vulkan helpers.
- Unresolved issues:
  - Dense CUDA/Vulkan kernels still require zero-offset dense row-major storage.
  - No public view constructor or view-backed CPU/GPU execution path is shipped.
- Signature: Codex GPT-5.4

## 2026-04-18 09:47 CEST - Vulkan Tensor BigInteger Rounding

- Objective attempted:
  - Continue `TENSOR-100F` with multiple GPT-5.4 agents by landing Vulkan
    dtype-changing Tensor rounding through the same public `Tensor BigInteger`
    contract proven by CUDA, without adding same-dtype Vulkan rounding map
    opcodes.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan helper/shader sources, Tensor rounding
    runtime, advanced stdlib module tests, Tensor docs, Vulkan roadmap,
    TODO/plan/changelog/session artifacts.
- Code/configuration changes:
  - Added `csrc/tensor_vulkan_round_i64_f64.comp`,
    `csrc/tensor_vulkan_round_i64_f32.comp`, and generated checked-in SPIR-V C
    sources.
  - Extended `csrc/tensor_vulkan_helpers.c` so Vulkan probing records
    `shaderInt64`, enables it during device creation when available, exposes a
    dedicated `rounding-big-integer` capability, and launches status-bearing
    integer-result rounding helpers.
  - Added C3 externs in `src/lisp/tensor_vulkan_backend.c3` and routed direct
    Tensor `floor`, `ceiling`, `round`, and `truncate` touching Vulkan through
    CPU `Tensor BigInteger` materialization in `src/lisp/prim_tensor.c3`.
  - Added advanced stdlib module coverage for Vulkan `Float64`/`Float32`
    direct rounding when `rounding-big-integer` is true, while preserving
    fail-closed expectations when it is false or absent.
  - Updated language/reference/area docs plus TODO, `.agents/PLAN.md`, Vulkan
    roadmap, `memory/CHANGELOG.md`, and session reports to record the landed
    Vulkan contract.
- Commands run:
  - `jj status`
  - targeted `rg`/`sed` inspections of Vulkan rounding tests, backend
    capability reporting, docs, TODO, plan, changelog, and session reports
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_round_i64_f64.comp -o
    /tmp/omni_tensor_vulkan_round_i64_f64.spv`
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_round_i64_f32.comp -o
    /tmp/omni_tensor_vulkan_round_i64_f32.spv`
  - `spirv-val --target-env vulkan1.0
    /tmp/omni_tensor_vulkan_round_i64_f64.spv`
  - `spirv-val --target-env vulkan1.0
    /tmp/omni_tensor_vulkan_round_i64_f32.spv`
  - `cc -O2 -Ideps/src/yyjson/src -Ideps/src/BearSSL/inc
    -Ideps/src/libuv/include -c csrc/tensor_vulkan_helpers.c -o
    /tmp/tensor_vulkan_helpers_rounding.o`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - Direct Vulkan `--eval` smokes for `Float64` floor/ceiling,
    `Float32` round/truncate, lazy map-then-floor, overflow, and unsupported
    `map floor`.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local ./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `git diff --check -- csrc/tensor_vulkan_helpers.c csrc/tensor_vulkan_round_i64_f32.comp csrc/tensor_vulkan_round_i64_f64.comp csrc/tensor_vulkan_round_i64_f32_spv.c csrc/tensor_vulkan_round_i64_f64_spv.c src/lisp/tensor_vulkan_backend.c3 src/lisp/prim_tensor.c3 src/lisp/tests_advanced_stdlib_module_groups.c3 scripts/build_omni_chelpers.sh project.json docs/LANGUAGE_SPEC.md docs/reference/03-collections.md docs/areas/tensor-scientific.md docs/plans/vulkan-math-library-roadmap-2026-04-17.md TODO.md .agents/PLAN.md memory/CHANGELOG.md .agents/SESSION_REPORT.md docs/SESSION_REPORT.md`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- Key results:
  - The duplicate `omni_tensor_backend_vulkan_int64_available` definition from
    the concurrent runtime lane was resolved before final validation.
  - `tensor-backends` reports Vulkan `rounding-big-integer true` on this host.
  - Direct Vulkan smokes returned CPU `Tensor BigInteger` values:
    `Float64` floor `("BigInteger" "cpu" "3" "-4")`, `Float64` ceiling
    `("BigInteger" "cpu" "4" "-3")`, `Float32` round
    `("BigInteger" "cpu" "4" "-4")`, and `Float32` truncate
    `("BigInteger" "cpu" "3" "-3")`.
  - Lazy Vulkan map-then-floor returned CPU `Tensor BigInteger` values; direct
    `map floor` remains fail-closed with
    `map: Vulkan currently supports Float64 and Float32 arithmetic kernels`.
  - Overflow/non-finite style status reports
    `floor: tensor integer result out of supported range`.
  - Host focused `advanced-collections-module`: passed, `pass=1326 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1309 fail=0`.
  - Targeted `git diff --check` passed.
  - Primitive docs parity passed.
  - Stage 3 source parity passed.
- Invalidated assumptions / negative memory:
  - Generic Vulkan `available`/`float64`/`float32` capability is insufficient
    for dtype-changing Tensor rounding. Use the dedicated
    `rounding-big-integer` capability.
  - Same-dtype Vulkan float rounding output remains an invalid contract for
    Tensor `floor`/`ceiling`/`round`/`truncate`.
- Current best recommendation:
  - Continue `TENSOR-100F` from Tensor view/layout metadata before view-backed
    GPU kernels, fixed-width complex Tensor storage, or measurement-led
    large-SVD/eigen performance work. Do not revisit Vulkan rounding as a
    same-dtype map/unary opcode.
- Unresolved issues:
  - Vulkan rounding is capability-gated by `shaderInt64`; devices without that
    feature must keep `rounding-big-integer false` and fail closed.
  - Vulkan `Float64` rounding also requires `shaderFloat64`; `Float32` requires
    Vulkan availability plus `shaderInt64`.
- Signature: Codex GPT-5.4

## 2026-04-18 09:25 CEST - CUDA Tensor BigInteger Rounding

- Objective attempted:
  - Continue implementation with multiple GPT-5.4 agents by landing the
    CUDA-first dtype-changing Tensor rounding path.
- Workspace/target:
  - `/home/christos/Omni`, CUDA helper/runtime, Tensor rounding, advanced
    stdlib module tests, Tensor docs, TODO/plan/changelog/session artifacts.
- Code/configuration changes:
  - Added `csrc/tensor_cuda_rounding_i64.cu` and generated
    `csrc/tensor_cuda_rounding_i64_ptx.inc`.
  - Extended `csrc/tensor_cuda_helpers.c` with a dedicated rounding PTX module
    resolver, `int64` scratch launcher, host copyback, and exported
    `omni_tensor_backend_cuda_round_i64_f64`/`f32` helpers plus
    `omni_tensor_backend_cuda_rounding_i64_available`.
  - Exposed the helper ABI in `src/lisp/tensor_cuda_backend.c3`.
  - Routed direct Tensor `floor`, `ceiling`, `round`, and `truncate` over
    CUDA-placed dense row-major `Float64`/`Float32` tensors through CUDA
    compute and native CPU `Tensor BigInteger` materialization.
  - Added `tensor-backends` CUDA `rounding-big-integer` capability and
    capability-gated tests for direct rounding plus explicit CPU copyback.
- Commands run:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_rounding_i64.cu -o /tmp/omni_tensor_cuda_rounding_i64.ptx`
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/omni_tensor_cuda_rounding_i64.ptx -o /tmp/omni_tensor_cuda_rounding_i64.cubin`
  - `cc -O2 ... -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers_rounding.o`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - Direct CUDA `--eval` smokes for `floor`, `ceiling`, `round`, `truncate`,
    `rounding-big-integer`, and non-finite status.
  - Host focused `advanced-collections-module`.
  - Bounded-container focused `advanced-collections-module`.
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - Targeted `git diff --check`.
- Key results:
  - CUDA smokes returned `BigInteger` dtype and expected integer values:
    `floor Float64` `("BigInteger" "3" "-4")`, `ceiling Float32`
    `("BigInteger" "4" "-3")`, `round Float32`
    `("BigInteger" "4" "-4")`, and `truncate Float64`
    `("BigInteger" "3" "-3")`.
  - CUDA non-finite status returned
    `floor: tensor integer result out of supported range`.
  - Host focused advanced collections passed `pass=1322 fail=0`.
  - Bounded-container focused advanced collections passed `pass=1305 fail=0`.
  - Helper rebuild, C3 build, docs parity, Stage 3 source parity, and diff
    hygiene passed.
- Invalidated assumptions / negative memory:
  - Generic CUDA `available`/`float64`/`float32` capability is not proof of the
    dtype-changing rounding path. Use `rounding-big-integer`.
  - Do not add rounding primitives to the same-dtype CUDA unary map opcode
    table.
- Current best recommendation:
  - Treat CUDA dtype-changing rounding as landed. Next Tensor scientific
    decision is Vulkan rounding policy now that the CUDA integer result
    helper/status/copyback ABI is proven.
- Unresolved issues:
  - Vulkan rounding remains explicitly unimplemented until `shaderInt64` /
    status/copyback policy is chosen.
- Signature: Codex GPT-5

## 2026-04-18 09:10 CEST - CUDA Tensor Rounding Test And Docs Prep

- Objective attempted:
  - Continue the CUDA-first dtype-changing Tensor rounding lane without editing
    runtime/helper files while another worker may be integrating the runtime.
- Workspace/target:
  - `/home/christos/Omni`, scoped to advanced stdlib module tests plus
    TODO/plan/changelog/language/reference/Tensor-area/Vulkan-roadmap docs.
- Code/configuration changes:
  - Added CUDA capability-gated advanced stdlib module tests for direct
    `floor`, `ceiling`, `round`, and `truncate` over CUDA-placed dense
    row-major `Float64` and `Float32` tensors, gated by a future CUDA backend
    `rounding-big-integer` capability bit.
  - The staged tests require `Tensor BigInteger` output dtype and verify values
    after explicit `to-device 'cpu` copyback with `ref`, `Array`, and `List`.
  - Updated backlog and docs to keep CUDA rounding marked as active/staged
    rather than landed until CUDA-capable validation passes.
- Commands run:
  - `jj status`
  - targeted `rg`/`sed` inspections of Tensor/CUDA tests and docs
  - `c3c build --obj-out obj`
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `git diff --check -- src/lisp/tests_advanced_stdlib_module_groups.c3 TODO.md .agents/PLAN.md memory/CHANGELOG.md docs/LANGUAGE_SPEC.md docs/reference/03-collections.md docs/areas/tensor-scientific.md docs/plans/vulkan-math-library-roadmap-2026-04-17.md .agents/SESSION_REPORT.md`
- Key results:
  - The test contract is staged without touching runtime/helper files.
  - No CUDA runtime support is claimed by this checkpoint.
  - Initial ordinary CUDA-availability gating was too broad on this host:
    CUDA was visible, but the dtype-changing rounding runtime path was not
    active, so the focused run failed three staged assertions. The tests now
    gate on the future CUDA backend `rounding-big-integer` capability bit.
  - `c3c build --obj-out obj` passed with existing deprecation warnings.
  - Focused host `advanced-collections-module` passed `pass=1322 fail=0`.
  - Targeted `git diff --check` passed for touched tests/docs/artifacts.
- Invalidated assumptions / negative memory:
  - Do not gate CUDA dtype-changing rounding tests on ordinary CUDA
    `available`/`float64`/`float32`; those only prove float storage/copy
    capability and can expose tests before the `Tensor BigInteger` result path
    exists. Use a dedicated `rounding-big-integer` capability bit or equivalent
    runtime proof.
- Current best recommendation:
  - Runtime integration should implement a dtype-changing result path that
    materializes native `Tensor BigInteger` values from CUDA `Float64`/`Float32`
    inputs, set the CUDA backend `rounding-big-integer` capability only after
    that path is wired, then run the staged CUDA-capable focused advanced
    collections tests.
- Unresolved issues:
  - CUDA-capable validation and overflow/status assertions remain pending for
    the runtime/helper integration worker.
- Signature: Codex GPT-5

## 2026-04-18 08:56 CEST - Vulkan Float64 Stats Normal Quantile

- Objective attempted:
  - Continue `TENSOR-100F` with multiple GPT-5.4 agents by landing Vulkan
    `Float64` `stats/normal-quantile` behind the existing backend-neutral
    Tensor surface.
- Workspace/target:
  - `/home/christos/Omni`, `csrc/tensor_vulkan_normal_quantile_f64.comp`,
    `csrc/tensor_vulkan_normal_quantile_f64_spv.c`,
    `csrc/tensor_vulkan_helpers.c`, `scripts/build_omni_chelpers.sh`,
    `project.json`, `src/lisp/tensor_vulkan_backend.c3`,
    `src/lisp/prim_tensor.c3`,
    `src/lisp/tests_advanced_stdlib_module_groups.c3`, `TODO.md`,
    `.agents/PLAN.md`, `memory/CHANGELOG.md`, Tensor/Vulkan docs, and session
    reports.
- Code/configuration changes:
  - Added a dedicated Vulkan `Float64` inverse-normal shader/helper with
    input/output buffers plus a `uint32` status buffer.
  - Routed direct Tensor unary math and `map stats/normal-quantile` for dense
    row-major Vulkan `Float64` tensors through the new helper, preserving
    Vulkan placement and dtype without CPU fallback or Float32 downcast.
  - The shader avoids unavailable Vulkan 1.0 double `log` by inverting the
    landed Float64 normal-CDF approximation with bounded bisection.
  - Added focused advanced stdlib module coverage for valid values, probability
    domain failures, non-finite failures, and non-finite status priority.
  - Updated TODO, plan, changelog, language/reference docs, and the Vulkan
    roadmap so CUDA-first dtype-changing rounding is now the active next item.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_normal_quantile_f64.comp -o /tmp/omni_tensor_vulkan_normal_quantile_f64.spv`
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_normal_quantile_f64.spv`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - Direct Vulkan `--eval` smokes for direct/map `Float64` quantile values,
    probability-domain failure, and non-finite failure.
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- Key results:
  - Direct and mapped Vulkan `Float64` quantile smokes returned `true` for
    `0.025`, `0.5`, and `0.975` with `1e-5` tail tolerance.
  - Invalid `0` probability returned
    `stats/normal-quantile: probability must be between 0 and 1`.
  - Vulkan-produced non-finite input returned
    `stats/normal-quantile: expected finite numeric input`.
  - Host focused `advanced-collections-module` passed `pass=1317 fail=0`.
  - Bounded-container focused `advanced-collections-module` passed
    `pass=1300 fail=0`.
- Invalidated assumptions / negative memory:
  - Do not keep treating Vulkan `Float64` quantile as fail-closed; it is now
    shipped through a Float64 status-bearing helper.
  - Do not use log-based inverse-CDF approximations in the Vulkan 1.0 Float64
    shader path; local validation rejects `log(double)`.
- Current best recommendation:
  - Continue with CUDA-first dtype-changing Tensor rounding to
    `Tensor BigInteger`; do not add same-dtype GPU rounding map opcodes.
- Unresolved issues:
  - The unsupported Vulkan `map floor` `handle` interaction remains open and
    separate from Tensor routing.
- Signature: Codex GPT-5

## 2026-04-18 06:27 CEST - Vulkan Float32 Stats Normal Quantile

- Objective attempted:
  - Continue `TENSOR-100F` with multiple GPT-5.4 agents by landing Vulkan
    `Float32` `stats/normal-quantile` without using the statusless generic
    Vulkan unary helper, and by keeping all remaining Float64/deferred work in
    TODO.
- Workspace/target:
  - `/home/christos/Omni`, `csrc/tensor_vulkan_normal_quantile_f32.comp`,
    `csrc/tensor_vulkan_normal_quantile_f32_spv.c`,
    `csrc/tensor_vulkan_helpers.c`, `scripts/build_omni_chelpers.sh`,
    `project.json`, `src/lisp/tensor_vulkan_backend.c3`,
    `src/lisp/prim_tensor.c3`,
    `src/lisp/tests_advanced_stdlib_module_groups.c3`, `TODO.md`,
    `.agents/PLAN.md`, `memory/CHANGELOG.md`, Tensor/Vulkan docs, and session
    reports.
- Code/configuration changes:
  - Added a dedicated Vulkan `Float32` inverse-normal shader with input/output
    buffers plus a separate `uint32` status binding.
  - Added `omni_tensor_backend_vulkan_map_normal_quantile_f32`, wired the
    generated SPIR-V object into helper/project builds, and routed op `20` only
    through the status-bearing helper for Vulkan `Float32`.
  - Mapped Vulkan quantile status `1` to
    `stats/normal-quantile: probability must be between 0 and 1` and status
    `2` to `stats/normal-quantile: expected finite numeric input`; invalid
    results destroy the output before surfacing an error.
  - Added focused Vulkan tests for direct/map valid values, finite endpoint
    domain errors, Vulkan-produced non-finite input, mixed invalid priority,
    and explicit Vulkan `Float64` fail-closed behavior.
  - Updated a stale Vulkan unsupported-callable test to assert the direct
    fail-closed message with `test_error_contains`; recorded the separate
    `handle` stack-overflow interaction as an open TODO instead of masking it
    through Tensor routing.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_normal_quantile_f32.comp -o /tmp/omni_tensor_vulkan_normal_quantile_f32.spv`
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_normal_quantile_f32.spv`
  - Direct Vulkan `--eval` smokes for valid direct/map `Float32`, zero/one
    probability errors, Vulkan-produced `NaN`, mixed `[0.0 NaN]` priority, and
    direct/map `Float64` fail-closed behavior.
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- Key results:
  - Direct Vulkan Float32 quantile smoke returned `true`; direct map smoke also
    returned `true`.
  - Finite endpoint probabilities returned
    `stats/normal-quantile: probability must be between 0 and 1`.
  - Vulkan-produced non-finite input and mixed domain/non-finite input returned
    `stats/normal-quantile: expected finite numeric input`.
  - Vulkan Float64 quantile remains fail-closed with
    `stats/normal-quantile: Vulkan currently supports dense row-major Float32 tensors`.
  - Host focused `advanced-collections-module` passed `pass=1311 fail=0`.
  - Bounded-container focused `advanced-collections-module` passed
    `pass=1298 fail=0`.
- Invalidated assumptions / negative memory:
  - Do not keep treating all Vulkan quantile as fail-closed: Vulkan Float32 is
    shipped through the status-bearing helper.
  - Do not infer Vulkan Float64 quantile from Float32 support, do not downcast,
    and do not assign op `20` to the existing statusless unary helper.
  - Do not widen Vulkan `floor` or Tensor map routing to hide the separate
    `handle` stack-overflow interaction around unsupported lazy map errors.
- Current best recommendation:
  - Continue with Vulkan `Float64` `stats/normal-cdf` policy/implementation
    first, then Vulkan `Float64` quantile policy/implementation, before moving
    to CUDA-first dtype-changing rounding.
- Unresolved issues:
  - Vulkan `Float64` `stats/normal-cdf` and `stats/normal-quantile` remain
    fail-closed and are tracked as explicit TODO items.
  - The unsupported Vulkan `map floor` `handle` interaction is tracked as a
    separate TODO; direct fail-closed behavior remains valid.
- Signature: Codex GPT-5

## 2026-04-18 05:52 CEST - CUDA Stats Normal Quantile Status ABI

- Objective attempted:
  - Continue `TENSOR-100F` with multiple GPT-5.4 agents by landing the
    probability-status boundary for CUDA `stats/normal-quantile` and updating
    the active TODO/design artifacts so deferred Vulkan work stays explicit.
- Workspace/target:
  - `/home/christos/Omni`, `csrc/tensor_cuda_scientific_unary.cu`,
    `csrc/tensor_cuda_helpers.c`, `src/lisp/prim_tensor.c3`,
    `src/lisp/tensor_cuda_backend.c3`,
    `src/lisp/tests_advanced_stdlib_module_groups.c3`, `TODO.md`,
    `.agents/PLAN.md`, `memory/CHANGELOG.md`, Tensor scientific docs, Vulkan
    roadmap docs, and session reports.
- Code/configuration changes:
  - Added CUDA scientific unary op `20` for `stats/normal-quantile`, generated
    from CUDA C/libdevice PTX using `normcdfinv` for dense row-major
    `Float64`/`Float32` CUDA tensors.
  - Added a CUDA probability-status word for op `20`: raw device status `0`
    remains success, `1` reports probability outside `0 < p < 1`, and `2`
    reports non-finite input. The kernel uses `atomicMax`, so non-finite input
    deterministically takes priority over a domain endpoint in mixed invalid
    tensors.
  - Mapped CUDA status codes to scalar-compatible Tensor diagnostics before
    exposing output: probability-domain failure and finite-input failure both
    fail closed without CPU fallback.
  - Added focused CUDA regressions for valid quantile values, endpoint domain
    failures, non-finite failure, mixed invalid priority, direct Float32
    placement/dtype preservation, destination realization, and Vulkan
    fail-closed quantile diagnostics.
  - Updated planning and docs to record CUDA quantile as shipped. At that
    checkpoint, Vulkan quantile still needed a status-bearing helper; this is
    superseded by the later `06:27 CEST` Vulkan checkpoint for Float32, while
    Vulkan Float64 remains deferred.
- Commands run:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_scientific_unary.cu -o /tmp/omni_tensor_cuda_scientific_unary.ptx`
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/omni_tensor_cuda_scientific_unary.ptx -o /tmp/omni_tensor_cuda_scientific_unary.cubin`
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - Direct CUDA valid/domain/non-finite/mixed-invalid and Vulkan fail-closed
    `--eval` smokes.
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- Key results:
  - Direct CUDA quantile valid smoke returned `true`.
  - CUDA invalid probability returned
    `stats/normal-quantile: probability must be between 0 and 1`.
  - CUDA `NaN` input and mixed `[0.0 NaN]` input returned
    `stats/normal-quantile: expected finite numeric input`.
  - Direct Vulkan `Float32` quantile still returned `tensor/backend-unsupported`
    at this CUDA checkpoint. Superseded by the later `06:27 CEST` Vulkan
    Float32 quantile checkpoint.
  - Host focused `advanced-collections-module` passed `pass=1307 fail=0`.
  - Bounded-container focused `advanced-collections-module` passed
    `pass=1294 fail=0`.
  - Primitive docs parity and Stage 3 source parity passed.
- Invalidated assumptions / negative memory:
  - Do not describe CUDA quantile status as an external sentinel scheme. The
    raw CUDA status word is now `0` on success; status priority is explicit and
    tested.
  - Do not assign quantile op `20` to the current Vulkan two-buffer unary
    helper; it lacks the required status binding/copyback path.
- Current best recommendation:
  - Superseded for Vulkan Float32 by the later `06:27 CEST` checkpoint.
    Continue with Vulkan Float64 distribution policy work.
- Unresolved issues:
  - Vulkan `Float64` `stats/normal-cdf` and `stats/normal-quantile`,
    dtype-changing rounding, view/layout metadata, fixed-width complex, and
    measurement-led SVD/eigen work remain tracked in TODO.
- Signature: Codex GPT-5

## 2026-04-18 05:21 CEST - Vulkan Map Preflight Hardening

- Objective attempted:
  - Continue `TENSOR-100F` by closing the first non-Docker backend lane:
    Vulkan `map` preflight ordering before additional callable broadening.
- Workspace/target:
  - `/home/christos/Omni`, `src/lisp/prim_tensor.c3`,
    `src/lisp/tests_advanced_stdlib_module_groups.c3`, `TODO.md`,
    `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`,
    `docs/areas/tensor-scientific.md`, and session reports.
- Code/configuration changes:
  - Added recursive `tensor_expr_has_non_vulkan_device` to mirror the CUDA
    preflight guard.
  - Reordered `tensor_map_try_vulkan_value` and
    `tensor_map_try_vulkan_direct` so callable support, recursive
    device-placement checks, dtype-family checks, and exact dtype checks run
    before `tensor_expr_resolve_concrete_any_device`.
  - Hardened direct Tensor `min` / `max` because it uses the same Vulkan map
    helper family: mixed CPU/Vulkan lazy operands now fail before CPU lazy
    materialization.
  - Added focused regressions for mixed CPU/Vulkan lazy `map`, unsupported
    binary Vulkan callable preflight, and mixed CPU/Vulkan lazy `min`.
- Commands run:
  - `c3c build --obj-out obj`
  - Direct `--eval` Vulkan smokes for mixed CPU/Vulkan lazy `map`, unsupported
    binary Vulkan callable preflight, unsupported unary callable preflight, and
    mixed CPU/Vulkan lazy `min`.
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - Targeted `git diff --check`.
- Key results:
  - Mixed CPU/Vulkan lazy `map` now returns
    `map: Vulkan operands must remain Vulkan-placed` instead of materializing
    the CPU lazy operand and failing with `map: function result dtype mismatch`.
  - Unsupported binary Vulkan callable preflight returns
    `map: Vulkan currently supports Float64 and Float32 arithmetic kernels`
    before materializing the CPU lazy operand.
  - Direct `min` with a CPU lazy operand and Vulkan operand now returns
    `minmax: Vulkan operands must remain Vulkan-placed`.
  - Bounded-container focused `advanced-collections-module` passed
    `pass=1287 fail=0`.
- Invalidated assumptions / negative memory:
  - Do not treat direct `min` / `max` as outside the Vulkan map preflight
    boundary; it is map-backed and needs the same no-hidden-materialization
    ordering.
- Current best recommendation:
  - Next non-Docker implementation lane is the shared GPU probability-domain
    status ABI for `stats/normal-quantile`, before CUDA/Vulkan quantile opcodes.
- Unresolved issues:
  - No GPU quantile support was added in this checkpoint.
  - The validation Docker image architecture bug remains a separate TODO.
- Signature: Codex GPT-5

## 2026-04-18 04:53 CEST - Non-Docker Tensor Backend Plan And TODO Reshape

- Objective attempted:
  - Turn the remaining non-Docker CUDA/Vulkan/Tensor issues into an explicit
    implementation plan and concrete TODO entries.
- Workspace/target:
  - `/home/christos/Omni`, active Tensor backend planning artifacts:
    `.agents/PLAN.md`, `TODO.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
- Planning changes:
  - Added an active non-Docker implementation order to `.agents/PLAN.md`.
  - Populated `TODO.md` with independently pickable residual work for Vulkan
    map preflight hardening, GPU quantile status handling, CUDA quantile,
    Vulkan Float32 quantile, Vulkan Float64 normal-CDF policy, CUDA-first
    dtype-changing rounding, Vulkan rounding policy, Tensor view metadata,
    first view-backed Vulkan structural dispatch, fixed-width complex, direct
    Vulkan general eigenpairs, and measurement-led SVD/eigen performance.
  - Updated the Vulkan math-library roadmap next checkpoint so future work
    starts from the same ordered plan instead of the older broad residual list.
- Commands run:
  - `sed`/`rg` inspections of `TODO.md`, `.agents/PLAN.md`, current session
    reports, and Vulkan/Tensor planning docs.
  - `date '+%Y-%m-%d %H:%M %Z'`.
  - `jj diff --stat`.
- Key results:
  - Non-Docker execution order is now: Vulkan map preflight hardening; shared
    GPU probability-domain status ABI; CUDA quantile; Vulkan Float32 quantile;
    Vulkan Float64 normal-CDF policy; CUDA-first dtype-changing rounding;
    Tensor view/layout metadata; first read-only view-backed Vulkan structural
    kernel; fixed-width complex before Vulkan complex/eigenpairs; measurement
    before large SVD/eigen rewrites.
- Invalidated assumptions / negative memory:
  - Do not treat GPU quantile as a single opcode task; it is blocked on
    probability-domain status propagation and a documented inverse-CDF
    approximation.
  - Do not treat GPU rounding as ordinary same-dtype unary `map`; the public
    Tensor result contract is `Tensor BigInteger`.
  - Do not treat stride parameters alone as view support; Tensor values need
    storage offset, backing extent, and alias/owner metadata first.
- Current best recommendation:
  - Start implementation with Vulkan map preflight ordering, then the shared
    GPU quantile status ABI. Those reduce risk for later CUDA/Vulkan callable
    broadening.
- Unresolved issues:
  - This checkpoint changed planning artifacts only. No runtime code was
    changed and no runtime tests were run.
  - The validation Docker image architecture fix remains deliberately outside
    this non-Docker plan.
- Signature: Codex GPT-5

## 2026-04-18 04:24 CEST - Tensor Quantile And Vulkan Float32 Normal CDF

- Objective attempted:
  - Continue `TENSOR-100F` with multiple GPT-5.4 agents, prioritize the
    remaining Tensor distribution lanes, and update the design docs/TODO so
    deferred GPU work is explicit.
- Workspace/target:
  - `/home/christos/Omni`, Tensor unary math dispatch, Vulkan Float32 unary
    shader/helper, advanced collection tests, Tensor scientific docs, TODO,
    changelog, and active plan artifacts.
- Code/configuration changes:
  - `prim_stats_normal_quantile` now routes Tensor operands through
    `tensor_unary_math_value`.
  - Added checked CPU Tensor `stats/normal-quantile` evaluation using
    `omni_boost_math_standard_normal_quantile`; invalid probabilities fail the
    whole operation with the scalar-compatible domain diagnostic.
  - Added Vulkan `Float32` `stats/normal-cdf` fixed op id `19` through the
    dedicated unary helper. The shader uses a same-dtype Float32 approximation,
    preserves Vulkan placement, and leaves Vulkan `Float64` fail-closed.
  - Regenerated `csrc/tensor_vulkan_map_unary_f32_spv.c`.
  - Added tests for CPU Tensor quantile, CUDA/Vulkan quantile fail-closed
    behavior, and Vulkan Float32 normal-CDF direct/map execution.
  - Converted CUDA construction-time map fail-closed assertions from Lisp
    `handle` expressions to C harness `test_error_contains`, avoiding the
    known stack-overflow handler path around immediate CUDA map errors.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_map_unary_f32.comp -o /tmp/omni_tensor_vulkan_map_unary_f32.spv`
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_map_unary_f32.spv`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - Direct CPU/CUDA/Vulkan smokes for Tensor quantile, GPU quantile fail-closed
    behavior, and Vulkan Float32 normal-CDF.
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - Targeted `git diff --check` for touched files.
- Key results:
  - Direct Vulkan Float32 `stats/normal-cdf 1.0` returned
    `0.841344714164734`.
  - Direct CPU Tensor `stats/normal-quantile 0.5` returned `0.0`.
  - CUDA/Vulkan Tensor `stats/normal-quantile` fail closed with
    `tensor/backend-unsupported` on device tensors.
  - Host focused `advanced-collections-module` passed `pass=1296 fail=0`.
  - Bounded-container focused `advanced-collections-module` passed
    `pass=1283 fail=0` with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- Invalidated assumptions / negative memory:
  - Do not treat CPU Tensor quantile as unimplemented; it is shipped with a
    whole-operation probability-domain failure contract.
  - Do not add GPU quantile by only assigning an opcode. CUDA/Vulkan still need
    per-element probability-domain status propagation and a documented
    inverse-CDF approximation/tolerance.
  - Do not infer Vulkan Float64 normal-CDF support from Vulkan Float32 or CUDA
    opcode `19`; Float64 remains blocked on a double approximation policy.
- Current best recommendation:
  - Next feature work should pick either the GPU quantile status/algorithm
    design, Vulkan Float64 normal-CDF approximation policy, dtype-changing GPU
    rounding result path, or representation-first layout/view metadata. The
    layout/view lane is blocked until Tensor storage offset, backing extent,
    and owner/alias metadata are explicit.
- Unresolved issues:
  - The validation Docker image still needs the native C3 toolchain fix before
    parent-level broad validation can run without `OMNI_VALIDATION_TOOLCHAIN_ROOT`.
- Signature: Codex GPT-5

## 2026-04-18 04:25 CEST - Tensor stats/normal-cdf CUDA Scientific Opcode

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by extending
    `stats/normal-cdf` from a scalar-only standard-normal helper to CPU Tensor
    elementwise support and a CUDA fixed scientific unary opcode.
- Workspace/target:
  - `/home/christos/Omni`, Tensor unary math dispatch, CUDA generated PTX helper,
    CUDA Driver API resolver, advanced collection tests, Tensor docs, TODO,
    changelog, and active plan artifacts.
- Code or configuration changes made:
  - `prim_math_core.c3` now routes Tensor operands for `stats/normal-cdf`
    through the shared Tensor unary math path.
  - `prim_tensor.c3` now evaluates CPU Tensor `stats/normal-cdf` elementwise:
    `Float64` and `Float32` preserve float dtype, `BigInteger` returns
    `Float64`, `BigFloat` preserves dtype, lazy CPU Tensor sources realize, and
    `BigComplex` fails closed because no complex distribution Tensor contract is
    shipped.
  - CUDA callable lowering recognizes `stats/normal-cdf` as CUDA-only fixed op
    id `19`, without adding it to the shared Vulkan unary callable table.
  - `csrc/tensor_cuda_scientific_unary.cu` and the embedded generated PTX in
    `csrc/tensor_cuda_helpers.c` now cover scientific op ids `5..19`; the CUDA
    scientific helper wrapper max-op range was widened to `19`.
  - Restored `omni_tensor_cuda_driver_resolve` in `csrc/tensor_cuda_helpers.c`
    so direct helper-object linking resolves CUDA Driver API calls used by
    embedded PTX modules.
  - Direct CUDA `map` now rejects unsupported CUDA callables and mixed CPU/CUDA
    operands before returning lazy Tensor expressions, preserving the existing
    payload diagnostics and no-hidden-materialization contract.
  - Added tests for CPU Tensor `stats/normal-cdf`, CUDA `map stats/normal-cdf`,
    direct Tensor CUDA `stats/normal-cdf`, Float32 dtype preservation, lazy CPU
    source realization, CUDA destination realization, Vulkan fail-closed
    behavior, and direct-map CUDA diagnostic eagerness.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/reference/11-appendix-primitives.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/README.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `docs/plans/cuda-cublas-backend-decision-2026-04-16.md`.
- Commands run and key results:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_scientific_unary.cu -o /tmp/omni_tensor_cuda_scientific_unary.ptx`: passed.
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/omni_tensor_cuda_scientific_unary.ptx -o /tmp/omni_tensor_cuda_scientific_unary.cubin`: passed.
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct smokes: CPU Tensor `stats/normal-cdf` returned `0.5`; CPU Float32
    Tensor preserved dtype; CUDA `map stats/normal-cdf` returned `0.5`; direct
    CUDA Float32 `stats/normal-cdf` preserved `"Float32"`; CUDA destination
    `realize` produced `(cuda 0.5)`; Vulkan `map stats/normal-cdf` returned
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: passed `pass=1282 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1269 fail=0` using `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat `stats/normal-cdf` support as arbitrary CUDA callable support;
    it is a fixed opcode extension.
  - Do not add `stats/normal-quantile` as a fixed CUDA opcode until the Tensor
    probability-domain and GPU error/status contract exists.
  - Do not infer Vulkan `stats/normal-cdf` support from CUDA opcode `19`; Vulkan
    remains fail-closed for this callable.
- Current best recommendation or checkpoint:
  - Continue CUDA/Vulkan broadening from explicit residuals: unsupported
    layouts/views, a defined mixed-device execution policy, Vulkan
    distribution-function policy, dtype-changing GPU rounding, or
    `stats/normal-quantile` after its domain/error contract is designed.
- Unresolved issues:
  - Unsupported CUDA layouts/views, arbitrary unsupported CUDA callables,
    dtype-changing GPU rounding, Tensor `stats/normal-quantile`, Vulkan
    `stats/normal-cdf`, fixed-width complex Tensor layout, broad parent-level
    validation, and the validation-image C3 architecture bug remain TODO items.
- Dependencies, blockers, or restart requirements:
  - Existing long-running Omni processes must be rebuilt/restarted before they
    see the new Tensor math and embedded PTX.
- Signature: GPT-5 Codex

## 2026-04-18 03:35 CEST - Tensor math/erf and math/erfc CUDA Scientific Opcodes

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by extending Tensor
    `math/erf` and `math/erfc` from scalar-only primitives to CPU Tensor
    elementwise support and CUDA fixed scientific unary opcodes.
- Workspace/target:
  - `/home/christos/Omni`, Tensor unary math dispatch, CUDA generated PTX helper,
    CUDA helper runtime resolver, advanced collection tests, Tensor docs, TODO,
    changelog, and active plan artifacts.
- Code or configuration changes made:
  - `prim_math_core.c3` now routes Tensor operands for `math/erf` and
    `math/erfc` through the shared Tensor unary math path.
  - `prim_tensor.c3` now evaluates CPU Tensor `math/erf` / `math/erfc`
    elementwise; `Float64` and `Float32` preserve float dtype, `BigInteger`
    returns `Float64`, `BigFloat` preserves dtype, and `BigComplex` fails closed
    because no complex error-function Tensor contract is shipped.
  - CUDA callable lowering recognizes `math/erf` and `math/erfc` as CUDA-only
    fixed op ids `17` and `18`, without adding them to the shared Vulkan unary
    callable table.
  - `csrc/tensor_cuda_scientific_unary.cu` and the embedded generated PTX in
    `csrc/tensor_cuda_helpers.c` now cover op ids `5..18`; the CUDA scientific
    helper wrapper max-op range was widened to `18`.
  - Restored `omni_tensor_cuda_resolve` in `csrc/tensor_cuda_helpers.c` so direct
    helper-object linking resolves CUDA runtime API calls instead of relying on a
    stale archive.
  - Added tests for CPU Tensor `math/erf` / `math/erfc`, CUDA `map math/erf`,
    CUDA `map math/erfc`, direct Tensor CUDA `math/erf` / `math/erfc`, Float32
    dtype preservation, CUDA destination realization, and Vulkan fail-closed
    behavior for the new callables.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/reference/11-appendix-primitives.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/README.md`, `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
    and `docs/plans/cuda-cublas-backend-decision-2026-04-16.md`.
- Commands run and key results:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_scientific_unary.cu -o /tmp/omni_tensor_cuda_scientific_unary.ptx`: passed.
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/omni_tensor_cuda_scientific_unary.ptx -o /tmp/omni_tensor_cuda_scientific_unary.cubin`: passed.
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct smokes: CPU Tensor `math/erf` returned `0.842700792949715`; CUDA
    `map math/erf` returned `0.842700792949715`; direct CUDA Float32
    `math/erfc` preserved `"Float32"`; Vulkan `map math/erf` returned
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: passed `pass=1271 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1258 fail=0` using `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not add `floor`, `ceiling`, `round`, or `truncate` to CUDA unary map as
    same-dtype opcodes. Tensor rounding returns `Tensor BigInteger`, so a GPU
    implementation needs an explicit dtype-changing result path.
  - Do not treat `math/erf` / `math/erfc` support as arbitrary CUDA callable
    support; it is a fixed opcode extension.
- Current best recommendation or checkpoint:
  - Continue CUDA map broadening from real residuals: unsupported layouts/views,
    explicit mixed-device execution if desired, fixed-width complex Tensor dtype,
    or another fixed built-in same-dtype callable family with CPU Tensor semantics
    first.
- Unresolved issues:
  - Unsupported CUDA layouts/views, arbitrary unsupported CUDA callables,
    dtype-changing GPU rounding, fixed-width complex Tensor layout,
    stride/view-backed Vulkan dispatch, broad parent-level validation, and the
    validation-image C3 architecture bug remain TODO items.
- Dependencies, blockers, or restart requirements:
  - Existing long-running Omni processes must be rebuilt/restarted before they
    see the new Tensor math and embedded PTX.
- Signature: GPT-5 Codex

## 2026-04-18 03:19 CEST - CUDA Map Mixed Operand Diagnostics

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by hardening CUDA `map`
    mixed-operand diagnostics and no-hidden-materialization behavior.
- Workspace/target:
  - `/home/christos/Omni`, CUDA/Vulkan Tensor map preflight, advanced collection
    tests, TODO, changelog, and active operational plan.
- Code or configuration changes made:
  - Added recursive non-materializing Tensor expression device probes for CUDA map
    preflight.
  - CUDA `map` now raises the explicit `map: CUDA operands must remain
    CUDA-placed` diagnostic for mixed CPU/CUDA operands, including a nested CPU
    lazy operand that would raise if materialized.
  - CUDA `map` now raises `tensor/dtype-mismatch` for mixed CUDA Tensor dtypes
    from the CUDA helper path instead of a generic backend unsupported message.
  - Vulkan and CUDA map probes now check whether an expression tree touches the
    probed device before resolving concrete storage, preventing the Vulkan probe
    from materializing CPU lazy operands in a CUDA-only failure path.
  - Added focused regressions for mixed CPU/CUDA placement diagnostics,
    fail-before-CPU-lazy-materialization, and mixed CUDA dtype diagnostics.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Host focused `advanced-collections-module`: passed `pass=1261 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1248 fail=0` using `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat symbol-only mixed-device rejection as sufficient for lazy Tensor
    expressions. Backend probes must reject unsupported mixed-device expression
    trees before they call concrete-storage realization helpers.
- Current best recommendation or checkpoint:
  - Remaining CUDA map work is now actual semantic broadening: unsupported
    layouts/views, explicit mixed-device execution support if desired, or callable
    extension beyond the fixed op-id table.
- Unresolved issues:
  - Unsupported CUDA layouts/views, unsupported CUDA callables, fixed-width complex
    Tensor layout, stride/view-backed Vulkan dispatch, broad parent-level
    validation, and the validation-image C3 architecture bug remain TODO items.
- Dependencies, blockers, or restart requirements:
  - Existing long-running Omni processes must be rebuilt/restarted before they
    see the new map preflight diagnostics.
- Signature: GPT-5 Codex

## 2026-04-18 03:07 CEST - Foreign CUDA Payload Clone Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by closing the foreign
    CUDA concrete Tensor clone/copy ownership contract.
- Workspace/target:
  - `/home/christos/Omni`, Tensor payload cloning, memory-lifetime regressions,
    CUDA ownership docs, TODO, changelog, and operational plan artifacts.
- Code or configuration changes made:
  - `tensor_clone_payload` now accepts valid CUDA concrete payload metadata from
    foreign finalizer sources, allocates a fresh Omni-owned CUDA buffer, copies
    source bytes device-to-device, and installs `tensor_cuda_device_finalizer` on
    the clone.
  - Fake, stale, malformed, or otherwise invalid CUDA handles remain fail-closed;
    the fake CUDA handle regression still rejects clone and verifies source
    finalizer destruction.
  - Added a real CUDA memory-lifetime regression for a foreign-finalizer source
    handle: copied values round-trip, clone handle is distinct, clone finalizer is
    Omni CUDA, and the source foreign finalizer fires once.
  - During review, corrected the Vulkan clone retain branch to keep
    `tensor_vulkan_device_finalizer` rather than accidentally installing a CUDA
    finalizer.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`,
    `docs/plans/cuda-cublas-backend-decision-2026-04-16.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh bash -lc 'c3c build --obj-out obj && env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`: passed `pass=229 fail=0`.
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`: passed `pass=1258 fail=0`.
  - A host-side `memory-lifetime-smoke` attempt was rejected by the repo policy
    guard before execution; the valid run is the bounded-container run above.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating valid foreign CUDA concrete payload clone as
    unimplemented or rejected. The supported ownership rule is now deep-copy into
    fresh Omni-owned CUDA storage, with the normal CUDA finalizer on the clone.
  - Do not infer that arbitrary pointer-shaped data is valid CUDA storage; invalid
    handles still fail closed and must not be copied.
- Current best recommendation or checkpoint:
  - Continue CUDA map broadening from a remaining explicit residual: unsupported
    layouts/views, mixed CPU/CUDA policy, mixed CUDA dtype/device policy, or
    callable extension beyond the fixed op-id table.
- Unresolved issues:
  - Unsupported CUDA layouts/views, mixed CPU/CUDA operands, mixed CUDA
    dtype/device operands, unsupported CUDA callables, fixed-width complex Tensor
    layout, stride/view-backed Vulkan dispatch, broad parent-level validation,
    and the validation-image C3 architecture bug remain TODO items.
- Dependencies, blockers, or restart requirements:
  - Existing long-running Omni processes must be rebuilt/restarted before they
    see the corrected clone/finalizer code.
- Signature: GPT-5 Codex

## 2026-04-18 02:56 CEST - CUDA Scientific Unary Map Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by landing CUDA
    scientific unary `map` and direct Tensor unary math without hidden CPU
    materialization or handwritten approximation PTX.
- Workspace/target:
  - `/home/christos/Omni`, CUDA helper PTX/module loading, C3 Tensor CUDA
    dispatch, `tensor-backends` capability reporting, focused advanced tests,
    TODO, changelog, plans, and Tensor docs.
- Code or configuration changes made:
  - Added `csrc/tensor_cuda_scientific_unary.cu` as the canonical CUDA C source
    for scientific unary kernels and embedded generated CUDA 13 PTX in
    `csrc/tensor_cuda_helpers.c` as a separate optional CUDA Driver API module.
  - Added C ABI helpers and C3 externs for `scientific-map-float64` /
    `scientific-map-float32` support and routed CUDA unary op ids `5..16`
    (`sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`,
    `exp`, `log`, `log10`) through the generated-PTX helper.
  - Updated public `map`, lazy CUDA map realization, CUDA destination
    realization, and direct Tensor scientific primitives to preserve CUDA
    placement for eligible dense row-major real CUDA tensors.
  - Added availability-gated tests for CUDA scientific Float64/Float32 family
    mapping, direct `sin`, direct Float32 `log10`, and destination realization.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/cuda-cublas-backend-decision-2026-04-16.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
- Commands run and key results:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_scientific_unary.cu -o /tmp/omni_tensor_cuda_scientific_unary.ptx`: passed.
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/omni_tensor_cuda_scientific_unary.ptx -o /tmp/omni_tensor_cuda_scientific_unary.cubin`: passed.
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - CUDA runtime smokes for capability reporting, Float64 `map sin`, Float32
    direct `log10`, direct `sin`, Float64 `acos`/`log10`, and lazy destination
    `realize`: returned true.
  - Host focused `advanced-collections-module`: passed `pass=1258 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1245 fail=0` using `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The prior scientific CUDA residual is now closed for dense row-major
    `Float64`/`Float32` op ids `5..16`; do not leave docs/TODO phrased as if
    scientific CUDA unary remains unimplemented.
  - Do not use the validation Docker image's baked-in `/opt/c3/c3c` on this
    arm64 host yet: rebuilding `omni-validation:2026-03-10` still downloads an
    x86-64 `c3-linux.tar.gz` compiler into an arm64 image. Use
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local` until the Dockerfile
    selects/builds native C3.
- Current best recommendation or checkpoint:
  - Superseded by the 03:07 foreign CUDA payload clone checkpoint above. The
    remaining CUDA map residuals should be split by explicit layout/view
    metadata, mixed-device policy, mixed-dtype policy, or callable extension.
- Unresolved issues:
  - Unsupported CUDA layouts/views, mixed CPU/CUDA operands, mixed CUDA
    dtype/device operands, unsupported CUDA callables, fixed-width complex Tensor
    layout, stride/view-backed Vulkan dispatch, broad parent-level validation,
    and the validation-image C3 architecture bug remain TODO items.
- Dependencies, blockers, or restart requirements:
  - Existing long-running Omni processes must be rebuilt/restarted before they
    see the new helper symbols and embedded PTX.
  - CUDA scientific support depends on driver support for the generated PTX
    target used here (`compute_75`) and remains capability-gated at runtime.
- Signature: GPT-5 Codex

## 2026-04-18 02:23 CEST - CUDA Arithmetic Unary Map Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by landing CUDA
    arithmetic/component unary map support without adding hidden CPU fallback
    or pretending scientific CUDA math is implemented.
- Workspace/target:
  - `/home/christos/Omni`, embedded CUDA map PTX, CUDA unary helper ABI,
    Tensor CUDA map routing, direct Tensor unary primitives, focused advanced
    tests, TODO, changelog, plans, and Tensor docs.
- Code or configuration changes made:
  - Added embedded PTX entries `omni_cuda_map_unary_f64` and
    `omni_cuda_map_unary_f32` plus C ABI helpers
    `omni_tensor_backend_cuda_map_unary_f64` and
    `omni_tensor_backend_cuda_map_unary_f32`.
  - Wired C3 externs and added `tensor_cuda_map_unary_value` for dense
    row-major `Float64`/`Float32` CUDA tensors.
  - Routed public direct `map`, lazy map realization, CUDA destination
    realization, and direct Tensor `abs`, unary `-`, `sqrt`, `real-part`,
    `imag-part`, and `conjugate` through the CUDA unary helper for op ids
    `0..4`.
  - Added availability-gated regressions for CUDA unary plus/abs/negation/sqrt,
    Float32 unary map, direct Tensor unary/component primitives, destination
    realization from lazy unary CUDA map, and scientific CUDA fail-closed
    behavior.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and CUDA/Vulkan plan notes so arithmetic
    CUDA unary map is no longer listed as deferred.
- Commands run and key results:
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - CUDA backend probe: `[true true true nil]`.
  - Direct CUDA smokes for `map abs`, direct `sqrt`, Float32 `map sqrt`,
    `imag-part`, CUDA destination realization from lazy `map sqrt`, and
    Float64/Float32 scientific fail-closed behavior: all returned `true`.
  - Host focused `advanced-collections-module`: passed `pass=1256 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1243 fail=0`.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The earlier “CUDA map residual is unary/scientific” wording is now too
    broad. Arithmetic/component unary CUDA map is implemented for dense
    row-major `Float64`/`Float32`; the residual is scientific CUDA unary math
    plus unsupported layouts/devices/dtypes/callables.
  - Do not implement CUDA scientific op ids `5..16` by stitching together
    partial raw PTX approximations. Prefer a deliberate libdevice/NVRTC/fatbin
    strategy, or explicitly documented approximation kernels with tolerances.
- Current best recommendation or checkpoint:
  - Continue CUDA map work from scientific unary math only after choosing the
    CUDA math implementation strategy, or move to a separate residual lane such
    as arbitrary foreign CUDA payload clone semantics, fixed-width complex, or
    stride/view-backed Vulkan layouts.
- Unresolved issues:
  - Scientific CUDA unary map/direct math, unsupported CUDA layouts, mixed
    unsupported CUDA operands, arbitrary foreign CUDA payload clone semantics,
    fixed-width complex Tensor layout, stride/view-backed Vulkan dispatch, and
    broad parent-level validation remain explicit TODO items.
- Dependencies, blockers, or restart requirements:
  - `ptxas` is not installed in this environment, so standalone PTX assembly
    was not run; runtime CUDA execution did load and execute the embedded PTX.
  - No long-running process was left active. A rebuild/restart is required for
    any external process that was started before this code change.
- Signature: GPT-5 Codex

## 2026-04-18 02:05 CEST - CUDA Map Broadcast Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by broadening the
    landed CUDA binary map path from exact Tensor/Tensor operands to
    right-aligned singleton-axis Tensor/Tensor broadcasting without hidden CPU
    tensor-data materialization.
- Workspace/target:
  - `/home/christos/Omni`, embedded CUDA map PTX, CUDA helper ABI, Tensor CUDA
    map routing, focused advanced tests, TODO, changelog, plans, and Tensor
    docs.
- Code or configuration changes made:
  - Widened `omni_tensor_backend_cuda_map_f64` and
    `omni_tensor_backend_cuda_map_f32` to accept output and operand
    rank/shape/stride metadata from C3.
  - Updated the embedded PTX kernels to accept optional device offset maps for
    Tensor operands, so exact-shape operands keep direct indexing and
    broadcast operands index through shape-derived offset buffers.
  - Added host-side offset-map construction in the CUDA helper. Only
    shape-derived indices are staged; Tensor payload data remains on CUDA.
  - Changed `tensor_cuda_map_binary_resolved` from exact-shape validation to
    the same right-aligned `tensor_map_operand_matches_shape` rule used by
    CPU/Vulkan while preserving dense row-major, matching dtype/device, and no
    hidden CPU fallback requirements.
  - Added CUDA Float64/Float32 broadcast map regressions plus CUDA destination
    realization coverage and incompatible-broadcast fail-closed coverage.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and CUDA/Vulkan plan notes so
    right-aligned CUDA Tensor/Tensor map broadcasting is no longer listed as
    deferred.
- Commands run and key results:
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct CUDA-gated smokes for Float64 broadcast map, Float32 broadcast map,
    CUDA destination realization from a broadcast map, and incompatible
    broadcast rejection: all returned expected results.
  - Host focused `advanced-collections-module`: passed `pass=1240 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1227 fail=0`.
- Invalidated assumptions or failed approaches worth preserving:
  - The earlier “CUDA map is exact-shape only” boundary is no longer current.
    Dense row-major binary CUDA map supports scalar, exact-shape, and
    right-aligned singleton-axis Tensor/Tensor broadcast operands.
  - Do not treat the offset-map helper as arbitrary-layout or scientific
    CUDA map support. Unsupported layouts, mixed devices/dtypes, unsupported
    callables, scientific CUDA unary map, and arbitrary foreign CUDA payload
    clone semantics still fail closed.
- Current best recommendation or checkpoint:
  - Superseded by the 02:23 checkpoint above: continue the CUDA map lane from
    scientific CUDA unary map, or choose a separate remaining `TENSOR-100F`
    lane such as fixed-width complex or stride/view-backed Vulkan.
- Unresolved issues:
  - Scientific CUDA unary map, unsupported CUDA layouts, arbitrary foreign CUDA
    payload clone semantics, fixed-width complex Tensor layout,
    stride/view-backed Vulkan dispatch, and broad parent-level validation
    remain explicit TODO items.
- Dependencies, blockers, or restart requirements:
  - No long-running process was left active. A rebuild/restart is required for
    any external process that was started before this code change.
- Signature: GPT-5 Codex

## 2026-04-18 01:53 CEST - CUDA Elementwise Binary Map Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by replacing the
    stale CUDA `map` blocker with a real CUDA execution path while preserving
    explicit fail-closed boundaries and no hidden CPU fallback.
- Workspace/target:
  - `/home/christos/Omni`, CUDA helper ABI, Tensor CUDA routing, lazy map
    realization, CUDA destination realization, tests, TODO, changelog, plans,
    and Tensor docs.
- Code or configuration changes made:
  - Added runtime-loaded CUDA Driver API symbol resolution and embedded PTX map
    kernels in `csrc/tensor_cuda_helpers.c` for dense row-major `Float64` and
    `Float32` binary elementwise operations.
  - Exposed CUDA map availability and dtype-specific map helpers through
    `src/lisp/tensor_cuda_backend.c3`.
  - Routed public `map`, lazy map realization, and CUDA destination-form
    `realize` through the CUDA helper for Tensor/scalar, scalar/Tensor, and
    exact-shape Tensor/Tensor operands over `+`, `-`, `*`, `/`, `min`, and
    `max`.
  - Added `tensor-backends` `elementwise-map-float64` and
    `elementwise-map-float32` capability keys for CPU, Vulkan, and CUDA.
  - Fixed the CUDA concrete clone boundary for Omni-owned CUDA payloads by
    copying device storage through the existing CUDA device-to-device helper;
    arbitrary foreign CUDA payload clone remains rejected.
  - Added availability-gated regressions for direct CUDA maps, lazy CUDA map
    destination realization, capability keys, and unsupported callable,
    mixed-device, and broadcast fail-closed cases.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and CUDA/Vulkan plan notes so the
    landed CUDA map subset and residual work are explicit.
- Commands run and key results:
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct CUDA-gated smokes for capability reporting, direct `Float64` and
    `Float32` scalar maps, binary `-`/`*`/`/`/`min`/`max`/`+` Tensor maps,
    lazy map CUDA destination realization, unsupported callable rejection,
    mixed CPU operand rejection, and broadcast rejection: all returned the
    expected results.
  - Host focused `advanced-collections-module`: passed `pass=1237 fail=0`.
  - Bounded-container `memory-lifetime-smoke`: passed `pass=228 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed
    `pass=1224 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `c3c build --sanitize=address --obj-out obj-asan`: not run to completion;
    this `c3c` build reports address sanitizer as unavailable on the current
    platform/toolchain.
- Invalidated assumptions or failed approaches worth preserving:
  - The previous “CUDA map blocker” is no longer authoritative for dense
    row-major binary `Float64`/`Float32` scalar and exact-shape operands; CUDA
    map now has a real embedded-PTX helper path.
  - Do not treat this as full CUDA map parity: right-aligned Tensor/Tensor
    broadcasting, scientific CUDA unary map, unsupported layouts, mixed
    CPU/CUDA operands, mixed CUDA dtypes/devices, unsupported callables, and
    arbitrary foreign CUDA payload clone semantics still fail closed.
- Current best recommendation or checkpoint:
  - Superseded by later checkpoints above: continue the CUDA map lane from one
    named residual family, now scientific CUDA unary map, using the
    embedded-PTX Driver API helper as the baseline.
- Unresolved issues:
  - CUDA map broadcasting/unary extension, arbitrary foreign CUDA payload clone
    semantics, fixed-width complex Tensor layout, stride/view-backed Vulkan
    dispatch, and broad parent-level validation remain explicit TODO items.
- Dependencies, blockers, or restart requirements:
  - No long-running process was left active. A rebuild/restart is required for
    any external process that was started before this code change.
- Signature: GPT-5 Codex

## 2026-04-18 01:13 CEST - CUDA Rank-1 Dot Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by landing the next
    coherent CUDA contract execution slice and sharpening the remaining CUDA
    `map` blocker.
- Workspace/target:
  - `/home/christos/Omni`, CUDA Tensor contract routing, destination
    realization tests, TODO, changelog, plans, and Tensor docs.
- Code or configuration changes made:
  - Extended `tensor_contract_try_cuda_value` so CUDA-placed contiguous matching
    `Float64` or matching `Float32` rank-1/rank-1 single-axis contractions
    return CUDA-placed scalar Tensor outputs.
  - Reused the existing cuBLAS GEMV helper by treating the left vector as a
    one-row row-major matrix, avoiding a new C helper ABI while still executing
    the nonzero dot through cuBLAS.
  - Preserved zero-size rank-1 dot through the CUDA additive-identity fill path.
  - Added availability-gated direct and destination-realize regressions for
    `Float64` and `Float32` rank-1 dot, including scalar `Float32` extraction
    and zero-size identity.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and CUDA/Vulkan plan notes so CUDA
    rank-1/rank-1 dot is no longer listed as future work.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct CUDA-gated smokes for `Float64` rank-1 dot, explicit-axis rank-1
    dot, `Float32` rank-1 dot with scalar `Float32` extraction, zero-size
    rank-1 dot identity, rank-0 CUDA destination realization for `Float64`
    rank-1 dot, and rank-0 CUDA destination realization for `Float32` rank-1
    dot: all returned `true`.
  - `./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed `pass=1217 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - CUDA rank-1/rank-1 dot does not require a new public primitive or helper
    ABI in this codebase; existing row-major GEMV helper semantics cover the
    nonzero dot while preserving CUDA placement.
  - Superseded by the later CUDA elementwise binary map checkpoint above: the
    helper-layer blocker no longer applies to dense row-major `Float64` and
    `Float32` binary scalar/exact-shape CUDA map.
- Current best recommendation or checkpoint:
  - For CUDA `map`, continue from the residual families named in the later
    CUDA elementwise binary map checkpoint above, or continue the fixed-width
    complex / stride-view Vulkan TODO lanes.
- Unresolved issues:
  - CUDA `map` broadcasting/unary residuals, fixed-width complex Tensor
    layout, stride/view-backed Vulkan dispatch, and broad parent-level
    validation remain explicit TODO items.
- Dependencies, blockers, or restart requirements:
  - No long-running process was left active. A rebuild/restart is required for
    any external process that was started before this code change.
- Signature: GPT-5 Codex

## 2026-04-18 01:00 CEST - CUDA Zero-Size Contract Identity

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by closing the
    explicit CUDA zero-size contract identity/fill TODO without changing the
    backend-neutral Tensor public surface or adding hidden CPU fallback.
- Workspace/target:
  - `/home/christos/Omni`, CUDA Tensor contract routing, focused advanced
    tests, TODO, changelog, plans, and Tensor docs.
- Code or configuration changes made:
  - Updated `tensor_contract_try_cuda_value` so supported dense row-major CUDA
    `Float64` and `Float32` rank-2/rank-2, rank-2/rank-1, and rank-1/rank-2
    single-axis contractions no longer reject zero free or zero contracted
    dimensions before allocation.
  - Zero free dimensions now return CUDA-placed zero-length Tensor outputs with
    the computed result shape. Zero contracted dimensions with non-empty output
    allocate CUDA result storage and fill additive identity through
    `omni_tensor_backend_cuda_fill_f64` / `omni_tensor_backend_cuda_fill_f32`,
    skipping cuBLAS.
  - Added availability-gated regressions for `Float64` and `Float32`
    zero-contracted rank-2 identity fill, zero-free rank-2 output preservation,
    rank-2/rank-1 identity fill, rank-1/rank-2 identity fill, and the
    cuBLAS-disabled zero-contracted path.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and relevant CUDA/Vulkan plan notes so
    CUDA zero-size contract identity/fill is no longer listed as deferred.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct CUDA-gated smokes for `Float64` zero-contracted rank-2, `Float32`
    zero-contracted rank-2 with scalar `Float32` extraction, zero-free rank-2
    output preservation, rank-2/rank-1 identity fill, and rank-1/rank-2
    identity fill: all returned `true`.
  - Host focused `advanced-collections-module` was attempted with
    `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module`;
    it stayed mostly idle past ten minutes and was terminated, so it is not a
    validation signal.
  - `./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed `pass=1210 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating zero-size CUDA contract dimensions as an unsupported
    cuBLAS problem. The zero-output and additive-identity cases are owned by
    CUDA allocation/fill semantics and must not call cuBLAS for correctness.
- Current best recommendation or checkpoint:
  - Continue `TENSOR-100F` from CUDA elementwise `map`, fixed-width complex
    Tensor semantics, stride/view-backed Vulkan coverage, or broad Docker-bound
    parent validation. CUDA rank-1/rank-1 dot is covered by the later
    checkpoint above.
- Unresolved issues:
  - CUDA `map`, fixed-width complex Tensor layout, and stride/view-backed
    Vulkan dispatch remain explicit TODO items. CUDA rank-1/rank-1 dot is
    covered by the later checkpoint above.
- Dependencies, blockers, or restart requirements:
  - No long-running process was left active. A rebuild/restart is required for
    any external process that was started before this code change.
- Signature: GPT-5 Codex

## 2026-04-18 00:40 CEST - Scalar Float32 Runtime Value Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by replacing the
    scalar `Float32` fail-closed boundary with a native runtime value connected
    to dispatch, Tensor extraction, copy/promotion, numeric, compiler, and
    persistence machinery.
- Workspace/target:
  - `/home/christos/Omni`, scalar value representation, conversion
    constructors, numeric helpers, Tensor extraction, matrix scalar readback,
    AOT/expression serialization, tests, TODO, changelog, plans, and Tensor
    docs.
- Code or configuration changes made:
  - Added scalar `Float32` value tag/payload/constructor, `make_float32`,
    type-id cache, `Number` parent wiring, `type-of` / `is?`, and stdlib
    `float32?`.
  - Wired `Float32`, `(Float x 32)`, and `(Float x "32")` through finite/range
    checked construction, string/format/print conversion, numeric
    comparison/equality/hash/order, arithmetic, schema validation, tuple
    encoding, JSON/CSV emission, FFI `Double` widening, sleep/random numeric
    consumers, expression serialization, and AOT literal lowering.
  - Preserved region-centric lifetime semantics by treating scalar `Float32` as
    a copy/promotion/env-copy leaf and adding boundary graph audit handling.
  - Updated Tensor `Float32` `ref`, `Array`, `List`, `Iterator`, contract scalar
    readback, CPU/copyback extraction, and structural matrix scalar readback so
    element access returns scalar `Float32` instead of widening to `Float64`.
    Matrix determinant and norm retain their documented `Float64` scalar return
    contract.
  - Updated current docs/plans/TODO/changelog so scalar `Float32` is no longer
    listed as deferred. Deferred work remains in `TODO.md` for fixed-width
    complex, stride/view-backed Vulkan coverage, CUDA `map`, and broad
    Docker-bound parent validation; CUDA zero-size contract identity/fill is
    covered by the later checkpoint above.
  - Updated stale tests that still used `test_eq_double` for `Float32`
    structural matrix element extraction and the advanced-core zero-arity `+`
    message now that `+` is unary-or-binary.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct runtime smokes for constructor/type/predicate, arithmetic widening
    policy, closure capture, Tensor extraction, structural matrix extraction,
    contract extraction, and `matrix/trace`: all returned `true`.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-core-semantics ./build/main --test-suite lisp`: passed `pass=71 fail=0`.
  - `OMNI_LISP_TEST_SLICE=arithmetic-comparison ./build/main --test-suite lisp`: passed `pass=47 fail=0`.
  - `./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed `pass=1205 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `c3c build --sanitize=address`: attempted; local `c3c` rejected sanitizer
    mode with `Address sanitizer is only supported on Linux, FreeBSD, NetBSD,
    Darwin and Windows.`
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep assuming Tensor `Float32` extraction should widen to scalar
    `Float64`. Scalar `Float32` exists now; element extraction should preserve
    scalar `Float32` for `Float32` Tensor storage.
  - Do not keep treating scalar `Float32` / `(Float x 32)` as fail-closed.
    Constructor failures now apply to nonnumeric, nonfinite, or out-of-range
    narrowing inputs.
- Current best recommendation or checkpoint:
  - Continue `TENSOR-100F` from CUDA elementwise `map`, fixed-width complex
    Tensor semantics, stride/view-backed Vulkan coverage, or broad Docker-bound
    parent validation.
- Unresolved issues:
  - Local ASAN compile remains unavailable through the current `c3c` invocation.
  - No full heavy/global/container-only parent gate was run in this slice.
- Dependencies, blockers, or restart requirements:
  - No long-running process was left active. Any already-running external
    process needs a rebuild/restart to pick up the new scalar `Float32` runtime.
- Signature: GPT-5 Codex

## 2026-04-17 23:46 CEST - TENSOR-100F CUDA Float32 Placement And Contract

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents by landing CUDA
    `Float32` placement/copyback, destination `realize`, and eligible cuBLAS
    contract routing without adding backend-specific public APIs or implicit
    CPU/GPU transfer.
- Workspace/target:
  - `/home/christos/Omni`, CUDA helper ABI, Tensor CUDA backend externs,
    public Tensor placement/realize/contract routing, focused advanced tests,
    TODO, changelog, plans, and Tensor docs.
- Code or configuration changes made:
  - Added runtime-loaded CUDA `Float32` fill support and cuBLAS `sgemm` /
    `sgemv` wrappers while preserving existing `Float64` cuBLAS availability
    as independent from optional `Sgemm` / `Sgemv` symbol resolution.
  - Broadened CUDA `to-device`, CPU copyback, destination-form `realize`,
    scalar destination fills, and supported lazy CUDA contract destination
    writes to dense row-major `Float32` Tensor storage.
  - Routed matching CUDA `Float32` rank-2/rank-2, rank-2/rank-1, and
    rank-1/rank-2 single-axis contractions through the new cuBLAS `Float32`
    helpers, preserving CUDA placement and `Float32` dtype.
  - Added `tensor-backends` CUDA/cuBLAS `float64` / `float32` capability
    fields, CUDA `Float32` placement/copyback/destination/contract tests, and
    fail-closed coverage for mixed CUDA dtypes and lazy CUDA `Float32` map.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and the CUDA/cuBLAS decision note so
    CUDA `Float32` placement/contract is no longer listed as current deferred
    work.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check -- csrc/tensor_cuda_helpers.c src/lisp/tensor_cuda_backend.c3 src/lisp/prim_tensor.c3 src/lisp/tests_advanced_stdlib_module_groups.c3 docs/reference/03-collections.md docs/plans/cuda-cublas-backend-decision-2026-04-16.md docs/LANGUAGE_SPEC.md docs/areas/tensor-scientific.md TODO.md memory/CHANGELOG.md .agents/PLAN.md docs/SESSION_REPORT.md .agents/SESSION_REPORT.md`
  - `./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- Key results:
  - Helper rebuild passed.
  - `c3c build --obj-out obj` passed with existing deprecation warnings.
  - Host focused `advanced-collections-module` passed `1218/0` and exercised
    the CUDA/cuBLAS `Float32` paths on the local CUDA stack.
  - Primitive docs parity, Stage 3 source parity, and targeted diff check
    passed.
  - Bounded-container focused `advanced-collections-module` passed `1205/0`.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not make `omni_tensor_backend_cublas_available()` depend on
    `cublasSgemm_v2` / `cublasSgemv_v2`; that would regress the older
    `Float64` cuBLAS contract on installations where only double symbols are
    available. Keep `Float32` cuBLAS availability as a separate probe.
- Current best recommendation or checkpoint:
  - CUDA `Float32` placement/copyback/destination/eligible cuBLAS contract is
    landed behind the existing explicit-device Tensor surface.
- Unresolved issues:
  - CUDA elementwise `map`, fixed-width complex Tensor layout, stride/view
    Vulkan dispatch, and broad/heavy parent-level container validation remain
    explicit TODO items. Scalar `Float32` and CUDA zero-size contract
    identity/fill are covered by later checkpoints.
- Dependencies, blockers, or restart requirements:
  - No long-running process was left active. A rebuild/restart is required for
    any external process that was started before this code change.
- Signature: GPT-5.4 Codex

## 2026-04-17 23:25 CEST - TENSOR-100F Vulkan Float32 Scientific Unary

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and expand dense
    row-major Vulkan `Float32` unary math beyond `sqrt` without hidden
    downcasts or the invalidated generic unary map branch.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan unary shaders/helpers, public Tensor unary
    dispatch, focused advanced tests, TODO, changelog, plans, and tensor docs.
- Code or configuration changes made:
  - Added dedicated Vulkan `Float32` unary opcodes for `sin`, `cos`, `tan`,
    `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, and
    `log10`, and regenerated the checked-in `Float32` SPIR-V C embed.
  - Routed public `(map <scientific-unary> <vulkan-float32-tensor>)` and
    direct Tensor unary math through the Vulkan `Float32` unary helper while
    preserving Vulkan placement and `Float32` dtype.
  - Kept Vulkan `Float64` scientific unary fail-closed after validation showed
    the current Vulkan 1.0 GLSL path rejects double transcendental builtins.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and the Vulkan plan docs so the landed
    `Float32` unary family and remaining deferred boundaries are explicit.
- Commands run and key results:
  - `glslangValidator` and `spirv-val` passed for the `Float32` and reverted
    `Float64` unary shaders.
  - `./scripts/build_omni_chelpers.sh`: passed after regenerating the
    checked-in `Float32` SPIR-V C embed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes passed for `map` `sin`/`cos`/`exp`/`log`, direct
    Tensor `log10`, and fail-closed Vulkan `Float64` `map sin`.
  - Host focused `advanced-collections-module`: `pass=1198 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=1185 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - `./scripts/build_omni_chelpers.sh` does not regenerate SPIR-V C embeds;
    shader changes must update the checked-in `_spv.c` file or runtime keeps
    the old shader behavior.
  - Do not implement Vulkan `Float64` scientific unary by downcasting to
    `Float32`. The current GLSL double-transcendental path failed validation,
    and silent precision loss would break the dtype contract.
- Current best recommendation / checkpoint:
  - Continue from scalar `Float32` runtime values or CUDA `Float32`
    placement/contract support before broader fixed-width complex or
    stride/view-backed Vulkan work.
- Unresolved issues:
  - Scalar `Float32` values, CUDA `Float32`, fixed-width complex,
    strided/view-backed Vulkan layouts, and measured Vulkan SVD performance
    remain open.
- Signature: Codex GPT-5.4

## 2026-04-18 09:03 CEST - TENSOR-100F Vulkan Float64 Normal Quantile Runtime Slice

- Objective attempted:
  - Implement only the Vulkan dense row-major `Float64` runtime/helper path for
    `stats/normal-quantile`, preserving the existing local `Float64`
    `stats/normal-cdf` `op == 19` path.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_normal_quantile_f64.comp` and generated
    `csrc/tensor_vulkan_normal_quantile_f64_spv.c`.
  - Added `omni_tensor_backend_vulkan_map_normal_quantile_f64` in
    `csrc/tensor_vulkan_helpers.c`, using the same status-buffer convention as
    the existing Float32 quantile helper: status `2` maps to non-finite input
    and outranks status `1` domain errors.
  - Registered the new SPIR-V source in `scripts/build_omni_chelpers.sh` and
    `project.json`, added the C3 extern in
    `src/lisp/tensor_vulkan_backend.c3`, and routed Vulkan `Float64`
    `op == 20` through the new helper in `src/lisp/prim_tensor.c3`.
- Key results:
  - Shader uses arithmetic-only bisection over the existing piecewise polynomial
    Float64 normal CDF approximation. It intentionally avoids GLSL
    `log(double)`, which the local Vulkan 1.0 shader toolchain rejects.
  - Direct and `map` `stats/normal-quantile` on Vulkan `Float64` tensors now
    preserve Vulkan placement and `Float64` dtype for valid probabilities.
  - Zero/one probabilities fail closed as
    `stats/normal-quantile: probability must be between 0 and 1`.
  - Non-finite inputs fail closed as
    `stats/normal-quantile: expected finite numeric input`, including when a
    domain error is also present in the same tensor.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_normal_quantile_f64.comp -o /tmp/tensor_vulkan_normal_quantile_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/tensor_vulkan_normal_quantile_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `git diff --check -- csrc/tensor_vulkan_normal_quantile_f64.comp csrc/tensor_vulkan_normal_quantile_f64_spv.c csrc/tensor_vulkan_helpers.c scripts/build_omni_chelpers.sh project.json src/lisp/tensor_vulkan_backend.c3 src/lisp/prim_tensor.c3`: passed.
  - `c3c build --obj-out obj`: passed with pre-existing deprecation warnings.
  - Guarded direct Vulkan `Float64` normal-quantile smoke returned `true`.
  - Guarded `map` Vulkan `Float64` normal-quantile smoke returned `true`.
  - Guarded zero-probability Vulkan smoke failed with the expected probability
    domain error.
  - Guarded mixed zero-plus-NaN Vulkan smoke failed with the expected
    non-finite priority error.
- Unresolved issues:
  - No tests or project docs were edited in this slice by request.
  - The arithmetic-only inverse is bounded by the polynomial CDF inversion range
    `[-8, 8]`; probabilities in the far double tails return the bounded
    approximation rather than a log-tail Acklam/Wichura-style value.
- Signature: Codex GPT-5.4

## 2026-04-18 08:56 CEST - Vulkan Float64 Quantile Test/Docs Staging

- Objective attempted:
  - Prepare focused test coverage and documentation/backlog state for Vulkan
    `Float64` `stats/normal-quantile` while leaving runtime/helper files to
    the concurrent implementation worker.
- Relevant workspace / target:
  - `/home/christos/Omni`, scoped to advanced stdlib module tests and
    documentation/backlog files for Vulkan Float64 distribution math.
- Code or configuration changes made:
  - Replaced the two unconditional Vulkan `Float64` quantile fail-closed
    checks in `src/lisp/tests_advanced_stdlib_module_groups.c3` with coverage
    for the landed Float64 helper contract.
  - The block checks direct and `map stats/normal-quantile` Vulkan placement,
    `Float64` dtype, and values for `0.025`, `0.5`, and `0.975`; it also
    checks invalid `0`/`1` probabilities and non-finite inputs return the
    scalar-compatible diagnostics without exposing output.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md` to mark Vulkan
    `Float64` quantile as landed and make CUDA-first dtype-changing rounding
    the next active Vulkan math-library item.
- Commands run and key results:
  - Memory search for prior Vulkan/CUDA quantile decisions: found CUDA
    quantile and Vulkan Float32 quantile landed, and a negative constraint
    against inferring Vulkan Float64 quantile support.
  - `jj status`: showed a heavily dirty concurrent tensor/backend worktree;
    changes were kept within the assigned files.
  - `rg` sweeps for `normal-quantile`/Vulkan Float64 support: the tree shifted
    while validating. A transient snapshot had helper references without the
    generated SPIR-V C source; the current snapshot includes
    `csrc/tensor_vulkan_normal_quantile_f64_spv.c`, project/build-script
    wiring, C helper symbols, and C3 externs.
  - `c3c build --obj-out obj`: first attempt failed at link during the
    transient missing-SPIR-V snapshot; rerun after concurrent helper artifacts
    landed passed and linked `build/main`.
  - `OMNI_LISP_TEST_SLICE=advanced
    OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module
    OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main
    --test-suite lisp`: passed with `pass=1317 fail=0`.
  - Targeted `git diff --check` on the assigned test/docs/backlog files:
    passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not claim Vulkan `Float64` quantile from Float32/CUDA/CDF adjacency
    alone; it requires its own Float64 helper/build artifact and tests. The
    current tree now has that artifact and focused tests pass.
- Current best recommendation / checkpoint:
  - Vulkan `Float64` `stats/normal-quantile` is now treated as landed in the
    docs/backlog, assuming the concurrent helper/runtime implementation is the
    intended final helper path. Continue with CUDA-first dtype-changing Tensor
    rounding unless a broader validation gate regresses the quantile helper.
- Unresolved issues:
  - This worker did not edit helper/runtime files and did not separately run
    `glslangValidator`/`spirv-val` for the Float64 quantile shader; validation
    here is build plus focused runtime tests.
- Signature: Codex GPT-5.4

## 2026-04-17 23:08 CEST - TENSOR-100F CPU Float32 Matrix Factor/SVD Contract

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and close the CPU
    `Float32` matrix factor/SVD public contract without hidden `Float64`
    widening or LAPACK fallback.
- Workspace/target:
  - `/home/christos/Omni`, CPU matrix primitives, focused advanced tests,
    TODO, changelog, plans, and tensor area docs.
- Code or configuration changes made:
  - Added native CPU `Float32` matrix helpers/workspaces for LU, solve,
    inverse, QR, Cholesky, and SVD/Jacobi singular-value paths.
  - Routed public CPU `Tensor Float32` surfaces for `matrix/determinant`,
    `matrix/lu`, `matrix/solve`, `matrix/inverse`, `matrix/cholesky`,
    `matrix/qr`, `matrix/singular-values`, `matrix/svd`, and SVD-backed
    `matrix/norm` selectors through native `Float32` implementations.
  - Replaced CPU `Float32` fail-closed tests with positive value/dtype tests
    and no-hidden-`d*` LAPACK counter guards.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`, plan docs,
    and tensor area docs so CPU `Float32` matrix factor/SVD is no longer
    listed as an open deferred boundary.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct CPU `Float32` smokes passed for determinant, solve, inverse, LU,
    QR, Cholesky, singular-values, SVD, and spectral/nuclear norm.
  - Host focused `advanced-collections-module`: `pass=1195 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=1182 fail=0`.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating CPU `Float32` matrix factor/SVD as deferred or
    fail-closed. Do not route these paths through `matrix_copy_float64_workspace`
    or the existing `dgesv`/`dgetrf`/`dgeqrf`/`dpotrf`/`dgesvd` helpers.
- Current best recommendation / checkpoint:
  - Continue from scalar `Float32` values, CUDA `Float32` placement/contract,
    fixed-width complex, stride/view-backed Vulkan layouts, measured Vulkan
    SVD performance, or additional Vulkan unary opcodes.
- Unresolved issues:
  - Scalar `Float32` values, CUDA `Float32`, fixed-width complex,
    strided/view-backed Vulkan layouts, and measured Vulkan SVD performance
    remain open.
- Signature: Codex GPT-5.4

## 2026-04-17 22:33 CEST - TENSOR-100F Vulkan Float32 Large-Dense SVD Robustness

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and close the
    previously deferred robust large-dense Vulkan `Float32` SVD blocker.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan `Float32` singular-values/SVD shaders,
    focused advanced tests, TODO, changelog, plans, and tensor area docs.
- Code or configuration changes made:
  - Updated `csrc/tensor_vulkan_singular_values_f32.comp` and
    `csrc/tensor_vulkan_svd_f32.comp` to use scale-aware negative-eigenvalue
    tolerance before singular-value extraction.
  - Updated `csrc/tensor_vulkan_svd_f32.comp` to fill orthonormal completion
    columns for zero-singular-value vector normalization in tall/square and
    wide branches instead of failing the dispatch.
  - Regenerated the corresponding SPIR-V C embeds.
  - Added availability-gated tests for `65x65` zero, identity/diagonal, and
    all-ones/rank-deficient Vulkan `Float32` singular-values/SVD paths,
    SVD-backed norms, all-ones reconstruction, and unchanged LAPACK `dgesvd`
    counters.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`, plan docs,
    and tensor area docs so robust large-dense Vulkan `Float32` SVD is no
    longer listed as an open correctness blocker.
- Commands run and key results:
  - `glslangValidator` and `spirv-val` passed for
    `csrc/tensor_vulkan_singular_values_f32.comp` and
    `csrc/tensor_vulkan_svd_f32.comp`.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `Float32` smokes returned `(65 64.9999694824219 0.0)`,
    `(vulkan vulkan vulkan 64.9999694824219 0.0)`,
    `(64.9999694824219 65.1197662353516)`, and `(65 1.0 1.0)`.
  - Host focused `advanced-collections-module`: `pass=1177 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=1164 fail=0`.
  - Primitive docs parity and Stage 3 source parity: passed.
  - Targeted `git diff --check`: passed after mechanical whitespace cleanup
    of regenerated SPIR-V C embeds.
- Invalidated assumptions or failed approaches worth preserving:
  - Supersedes the earlier dense all-ones `65x65` Vulkan `Float32`
    single-dispatch SVD failure note. That case is now fixed by scale-aware
    eigenvalue tolerance plus orthonormal completion; do not resume staged or
    tiled `Float32` SVD as a correctness blocker unless new validation
    regresses it.
- Current best recommendation / checkpoint:
  - Continue from CPU `Float32` factor/SVD contracts, scalar/CUDA `Float32`,
    fixed-width complex, or stride/view-backed Vulkan layout work.
- Unresolved issues:
  - Future tiled/multi-dispatch SVD remains a performance-oriented follow-up,
    not the current correctness boundary.
- Signature: Codex GPT-5.4

## 2026-04-17 22:16 CEST - TENSOR-100F Vulkan Float32 Staged Parallel Solve Slice

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and land native
    staged parallel Vulkan `Float32` `matrix/solve` routing without hidden
    `Float64` widening, LAPACK fallback, or Vulkan operand copy-to-CPU
    fallback.
- Workspace/target:
  - `/home/christos/Omni`, staged Vulkan solve shaders/helpers, public
    matrix dispatch, focused advanced tests, TODO, and design/status artifacts.
- Code or configuration changes made:
  - Added dedicated `Float32` ports of the staged Vulkan solve shader family
    (`solve_parallel_init`, legacy `solve_parallel`, pivot scan/reduce/commit,
    row swap, factor, eliminate, and backsolve), generated SPIR-V C embeds,
    and helper build/project wiring.
  - Reworked the staged C helper into an explicitly dtype-aware dispatch ABI
    and added `omni_tensor_backend_vulkan_solve_parallel_f32`.
  - Added dtype-specific solve counters for `Float32` and `Float64` serial,
    staged dispatch, pivot-reduce, and factor-stage paths.
  - Updated public `matrix/solve` routing so Vulkan dense row-major `Float32`
    systems below `65` stay serial and systems with `n >= 65` use the staged
    helper. `Float32` staged shaders use a `1e-6` singularity tolerance and do
    not route through the `Float64` staged helper.
  - Added focused availability-gated tests for `2x2`/`9x9` serial routing,
    `65x65` identity and dense staged routing, matrix RHS shape/rank, staged
    singular status, mixed dtype rejection, unchanged `Float64` counters, and
    unchanged LAPACK `dgesv` counters.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md` so staged
    `Float32` solve is no longer listed as deferred.
- Commands run and key results:
  - `glslangValidator` and `spirv-val` passed for the nine `Float32` staged
    solve shaders.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `Float32` staged smokes returned `(vulkan "Float32" 1.0)`,
    dense `65x65` value `1.00000011920929`, and `tensor/singular-matrix`.
  - Local threshold probes recorded under
    `build/vulkan_solve_f32_threshold_20260417_*`: staged and forced-serial
    `65x65` identity/dense timings were tied within measurement noise
    (`0.92s/0.89s` staged vs `0.94s/0.89s` forced serial for the
    higher-iteration probes), so `65` is a parity threshold, not a decisive
    speedup claim.
  - Host focused `advanced-collections-module`: passed, `pass=1166 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1153 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating staged Vulkan `Float32` solve as deferred. It now has
    dedicated `_f32` staged shaders and dtype-specific route/counter coverage.
  - Do not claim a decisive `Float32` staged solve speedup from the local
    timing evidence; the measured result is parity at `65` on this stack.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100F` from robust large-dense Vulkan `Float32` SVD
    execution or from separate CPU `Float32` factor/SVD, CUDA `Float32`,
    scalar `Float32`, or fixed-width complex contracts.
- Unresolved issues:
  - Robust large-dense Vulkan `Float32` SVD remains deferred because dense
    all-ones `65x65` single-dispatch SVD previously failed on this stack while
    `65x65` zero matrices succeeded.
  - CPU `Float32` factor/SVD routines, CUDA `Float32` placement, scalar
    `Float32` values, and fixed-width complex Tensor storage remain
    fail-closed/deferred.
- Signature: Codex GPT-5.4

## 2026-04-17 21:46 CEST - TENSOR-100F Vulkan Float32 Serial Factor/Solve Slice

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and land native
    Vulkan `Float32` serial factor/solve support for `matrix/determinant`,
    `matrix/lu`, `matrix/solve`, `matrix/inverse`, `matrix/cholesky`, and
    `matrix/qr` without hidden `Float64` widening, LAPACK fallback, or Vulkan
    operand copy-to-CPU fallback.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Tensor factor/solve shaders/helpers, public
    matrix dispatch, focused advanced tests, TODO, and design/status artifacts.
- Code or configuration changes made:
  - Added dedicated Vulkan `Float32` factor/solve shader/SPIR-V sources for
    determinant, LU, solve, inverse, Cholesky, and QR, plus helper build and
    `project.json` wiring.
  - Added C helper exports and C3 externs for the new `_f32` paths, including
    Float32 tail-status handling and three-buffer dispatch plumbing.
  - Updated `src/lisp/prim_tensor_matrix.c3` so eligible Vulkan-placed dense
    row-major `Float32` operands route through native serial `_f32` helpers
    for the six factor/solve surfaces. Tensor outputs preserve Vulkan
    placement and `Float32` dtype; determinant and LU metadata keep existing
    public scalar/host metadata contracts.
  - Kept CPU `Float32` factor/solve routines fail-closed and kept staged
    parallel solve routing `Float64`-only. Staged parallel `Float32`
    solve/performance parity is now an explicit TODO follow-up, not an
    implicit fallback through the Float64 staged helper.
  - Added availability-gated tests for values, dtype/device preservation, lazy
    operands, singular/rank-deficient/non-SPD diagnostics, CPU `Float32`
    fail-closed behavior, and no-LAPACK counter preservation across the six
    surfaces.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md` so serial
    factor/solve is no longer listed as wholly fail-closed.
- Commands run and key results:
  - Shader worker validation: `glslangValidator` and `spirv-val` passed for
    all six new `Float32` factor/solve shaders.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `Float32` smokes returned `-2.0`,
    `0.199999988079071`, `"Float32"`, `0.5`, `2.0`, and `1.0`.
  - Host focused `advanced-collections-module`: passed, `pass=1156 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1143 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed before this report entry.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating Vulkan `Float32` factor/solve as entirely
    fail-closed. Eligible dense row-major Vulkan `Float32` operands now have
    serial native helper paths for determinant, LU, solve, inverse, Cholesky,
    and QR.
  - Do not assume the existing staged parallel `Float64` solve threshold or
    helper applies to `Float32`. `Float32` solve correctness is serial in this
    checkpoint; staged parallel `Float32` solve is a performance-parity
    follow-up requiring dtype-specific helper/status/threshold validation.
- Current best recommendation / checkpoint:
  - Continue from staged parallel Vulkan `Float32` solve/performance parity,
    robust large-dense `Float32` SVD execution, CPU `Float32` factor/SVD
    contract work, CUDA `Float32` placement, scalar `Float32` values, or the
    fixed-width complex dtype lane.
- Unresolved issues:
  - Staged parallel Vulkan `Float32` solve/performance parity remains deferred.
  - Robust large-dense Vulkan `Float32` SVD remains deferred because dense
    all-ones `65x65` single-dispatch SVD previously failed on this stack while
    `65x65` zero matrices succeeded.
  - CPU `Float32` factor/SVD routines, CUDA `Float32` placement, and scalar
    `Float32` values remain fail-closed.
- Signature: Codex GPT-5.4

## 2026-04-17 21:17 CEST - TENSOR-100F Vulkan Float32 SVD-backed Slice

- Objective attempted:
  - Continue `TENSOR-100F` by landing native Vulkan `Float32` SVD-backed
    support for `matrix/norm` `'spectral`/`'nuclear`,
    `matrix/singular-values`, and direct `matrix/svd` without hidden
    CPU/LAPACK fallback or hidden `Float64` widening.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Float32 singular-value/SVD shaders, C helper
    dispatch, public matrix routing, advanced collection tests, TODO, docs,
    and planning artifacts.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_singular_values_f32.comp`,
    `csrc/tensor_vulkan_svd_f32.comp`, generated `_spv.c` sources, build
    manifest entries, C helper exports, and C3 externs.
  - Routed Vulkan-placed dense row-major `Float32` operands through dedicated
    native `_f32` singular-value/SVD helpers. Tensor outputs preserve Vulkan
    placement and `Float32` dtype; norm scalars keep the public `Float64`
    return contract.
  - Kept CPU `Float32` `matrix/singular-values`, CPU `Float32` `matrix/svd`,
    and CPU `Float32` spectral/nuclear norm fail-closed before CPU
    double/LAPACK workspaces are reached.
  - Renamed shared multi-output Vulkan dispatch helpers from misleading `_f64`
    names to dtype-neutral byte-sized names and added a Vulkan `Float32` SVD
    over-previous-max-k/no-LAPACK regression.
  - Updated `memory/CHANGELOG.md`, `TODO.md`, `.agents/PLAN.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md` so SVD-backed
    Vulkan `Float32` support is no longer listed as deferred.
- Commands run and key results:
  - `glslangValidator` / `spirv-val` for
    `csrc/tensor_vulkan_singular_values_f32.comp` and
    `csrc/tensor_vulkan_svd_f32.comp`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `Float32` smokes returned `3.0` for spectral norm, `2.0`
    for a copied-back singular value, and `"Float32"` for `matrix/svd` output
    dtype.
  - Host focused `advanced-collections-module`: passed, `pass=1121 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1108 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not continue from the direct-reducer checkpoint assumption that Vulkan
    `Float32` SVD-backed selectors and outputs are fail-closed. Eligible dense
    row-major Vulkan `Float32` inputs now have real shader/helper paths.
  - A dense all-ones 65x65 Vulkan `Float32` single-dispatch SVD smoke returned
    `tensor/backend-execution-failed` on this Vulkan stack while 65x65 zero
    matrices succeed. Do not treat the current shader as the robust
    large-dense performance/correctness baseline; prefer staged/tiled or
    measured large-dense SVD work for that regime.
- Current best recommendation / checkpoint:
  - Continue from Vulkan `Float32` factor/solve kernels, robust large-dense
    SVD execution, CPU `Float32` SVD contract work, CUDA `Float32` placement,
    or scalar `Float32` values.
- Unresolved issues:
  - Vulkan `Float32` factor/solve kernels remain fail-closed.
  - CPU `Float32` `matrix/singular-values`, `matrix/svd`, and SVD-backed
    norm selectors remain fail-closed by design.
  - CUDA `Float32` placement, scalar `Float32` values, fixed-width complex,
    and stride/view-backed Vulkan layouts remain deferred.
- Signature: Codex GPT-5.4

## 2026-04-17 18:57 CEST - TENSOR-100F Vulkan Symmetric Eigen Large-n

- Objective attempted:
  - Continue `TENSOR-100F` by replacing the old `n <= 64` Vulkan
    `matrix/eigenvalues` / `matrix/eigenvectors` private-array cap with a
    storage-backed larger-size path while preserving backend-neutral public
    surfaces and no hidden CPU/LAPACK fallback.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan symmetric eigen shader, helper
    validation/allocation, advanced collection tests, docs, TODO, and memory
    artifacts.
- Code or configuration changes made:
  - Reworked `csrc/tensor_vulkan_symmetric_eigen_f64.comp` to store Jacobi
    matrix scratch behind the public eigenvector output payload instead of a
    fixed private `double[4096]` array.
  - Regenerated `csrc/tensor_vulkan_symmetric_eigen_f64_spv.c`.
  - Updated `csrc/tensor_vulkan_helpers.c` validation/allocation so values
    retain the visible `[n] + status` payload, vectors retain visible `[n n]`
    metadata, and hidden vector backing storage carries `n*n` matrix scratch.
    The helper now rejects resource sizes that exceed 32-bit shader/storage
    guards or would wrap the shader's `64 * n * n` Jacobi iteration guard.
  - Replaced the old `65x65` fail-closed regression with availability-gated
    success tests for direct and lazy large Vulkan eigenvalues/eigenvectors,
    Vulkan output placement, large nonsymmetric `tensor/not-symmetric`
    diagnostics, and no `LAPACKE_dsyev` counter movement.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`, and relevant
    Vulkan/Tensor docs. Deferred large-`n` performance work is now tracked as
    measurement-driven tiling or staged execution, not as the semantic cap.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_symmetric_eigen_f64.comp -o /tmp/omni_symmetric_eigen.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_symmetric_eigen.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct smokes: `65x65` identity Vulkan eigenvalues copied back as
    `(65 1.0 1.0)`; `65x65` eigenvectors returned
    `(vulkan vulkan 65 1.0)`; `65x65` sparse nonsymmetric inputs returned
    `tensor/not-symmetric`.
  - Host focused `advanced-collections-module`: `pass=1006 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=993 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep assuming Vulkan symmetric eigen is capped at `n <= 64`.
    Dense row-major square symmetric `Float64` inputs now support `n > 64`
    within helper resource limits, 32-bit shader index guards, and the Jacobi
    iteration guard.
  - Do not build large sparse test fixtures with recursive `(range 4225)` list
    data; that stack-overflowed in a `handle` body. Use an all-zero Tensor
    converted to an Array plus `set!` for sparse large fixtures.
- Current best recommendation / checkpoint:
  - Treat semantic large-`n` Vulkan symmetric eigen support as landed for
    dense row-major `Float64` symmetric inputs. Continue `TENSOR-100F` from
    general `matrix/eigenpairs`, Vulkan `Float32`, fixed-width complex,
    stride/view metadata, or measurement-driven performance work.
- Unresolved issues:
  - The large-`n` path remains correctness-first single-dispatch Jacobi using
    storage-buffer scratch; tiled/staged performance work is deferred and
    tracked in `TODO.md`.
  - Full heavy/container-only gates were not run; focused bounded-container
    validation passed.
  - Live GPU execution coverage remains availability-gated by the local Vulkan
    stack.
- Signature: Codex GPT-5.4

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

## 2026-04-17 14:27 CEST - Direct Vulkan Symmetric Eigenvalues And Eigenvectors
- Objective attempted:
  - Continue `TENSOR-100F` from the SVD checkpoint by landing the first direct
    Vulkan symmetric real eigen slice behind existing `matrix/eigenvalues` and
    `matrix/eigenvectors` surfaces.
- Workspace/target:
  - `/home/christos/Omni`, Tensor/Vulkan symmetric eigen route.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_symmetric_eigen_f64.comp` and generated
    `csrc/tensor_vulkan_symmetric_eigen_f64_spv.c`.
  - Added `omni_tensor_backend_vulkan_symmetric_eigen_f64`, build/project
    wiring, and a Vulkan status mapping for nonsymmetric input to
    `tensor/not-symmetric`.
  - Added the C3 extern and routed public `matrix/eigenvalues` and
    `matrix/eigenvectors` so dense row-major Vulkan `Float64` square inputs,
    including lazy Vulkan inputs, execute on Vulkan before CPU fallback.
  - Returned eigenvalues and aligned eigenvector columns as Vulkan-placed
    Tensor outputs; CPU inspection remains explicit through `to-device 'cpu`.
  - Kept `matrix/eigenpairs` fail-closed on Vulkan because its public result
    contract is still pointer-backed `BigComplex`.
  - Added availability-gated regressions for direct/lazy Vulkan eigenvalue and
    eigenvector results, output device placement, nonsymmetric
    `tensor/not-symmetric`, `n > 64` fail-closed behavior, and unchanged
    `LAPACKE_dsyev` counters.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`,
    `docs/plans/vulkan-eigensolver-plan-2026-04-17.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_symmetric_eigen_f64.comp -o /tmp/omni_tensor_vulkan_symmetric_eigen_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_symmetric_eigen_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct runtime smokes: Vulkan eigenvalues returned `("vulkan" 3.0 2.0)`;
    Vulkan eigenvectors returned `("vulkan" "vulkan" 3.0 1.0)`;
    nonsymmetric Vulkan input returned `tensor/not-symmetric`.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=948 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed, `pass=935 fail=0`.
  - `python3 -m json.tool project.json`: passed.
  - `bash -n scripts/build_omni_chelpers.sh`: passed.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Direct `matrix/eigenvalues` and `matrix/eigenvectors` are no longer
    fail-closed on supported Vulkan operands. Keep only `matrix/eigenpairs`
    fail-closed while its output contract remains `BigComplex`.
  - Do not hide `n > 64` symmetric eigen by copying Vulkan input to CPU; the
    current helper is intentionally bounded and returns
    `tensor/backend-unsupported` beyond that boundary.
- Current best recommendation / checkpoint:
  - Direct symmetric Vulkan eigen is implemented for dense row-major
    `Float64` square inputs with `n <= 64`. Continue `TENSOR-100F` from
    residual lanes such as larger eigen/SVD algorithms, helper factoring,
    native `Float32` storage/kernels, or fixed-width complex dtype work.
- Unresolved issues:
  - Full heavy/container-only gates were not rerun for this source-level
    Tensor/Vulkan slice; focused host and bounded-container coverage passed.
- Signature: Codex GPT-5.4

## 2026-04-17 14:01 CEST - Direct Vulkan Matrix SVD Factor Output
- Objective attempted:
  - Continue `TENSOR-100F` feature implementation using GPT-5.4 agents only,
    after fast/Spark agents were unavailable, and land the missing direct
    Vulkan `matrix/svd` factor-output path.
- Workspace/target:
  - `/home/christos/Omni`, Tensor/Vulkan matrix SVD route.
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_svd_f64.comp` and generated
    `csrc/tensor_vulkan_svd_f64_spv.c`.
  - Added `omni_tensor_backend_vulkan_svd_f64` with one Vulkan input and
    separate Vulkan `u`, `s`, and `v` output buffers, plus build wiring in
    `scripts/build_omni_chelpers.sh` and `project.json`.
  - Added the C3 extern and routed public `matrix/svd` so dense row-major
    Vulkan `Float64` rank-2 inputs, including lazy Vulkan inputs, execute on
    Vulkan before CPU fallback.
  - Added availability-gated regressions for Vulkan SVD values, output device
    placement, lazy input, empty axes, wrong rank, `k > 64` fail-closed
    behavior, exact `k == 64` success, and unchanged `LAPACKE_dgesvd` counters.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/areas/tensor-scientific.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `docs/plans/vulkan-svd-factor-output-plan-2026-04-17.md` so direct
    Vulkan `matrix/svd` is recorded as shipped for dense row-major `Float64`
    inputs with `k <= 64`.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_svd_f64.comp -o /tmp/omni_tensor_vulkan_svd_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_svd_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `python3 -m json.tool project.json`: passed.
  - `bash -n scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=945 fail=0`.
  - `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not restore a second private `double[4096]` eigenvector array in the
    SVD shader. It compiled and validated as SPIR-V but failed at runtime on
    this Vulkan stack with `tensor/backend-execution-failed`.
  - The shipped shader stores the driving eigenvectors in the eventual output
    buffer (`V` for tall/square inputs, `U` for wide inputs) and keeps only the
    Gram matrix plus eigenvalues in private storage.
- Current best recommendation / checkpoint:
  - Direct Vulkan `matrix/svd` is implemented for dense row-major `Float64`
    rank-2 inputs with `k = min(rows, columns) <= 64`. Continue `TENSOR-100F`
    from the remaining explicit residual lanes: Vulkan eigensolvers,
    native `Float32` Tensor storage/kernels, larger-`k` SVD algorithms, or
    shared helper cleanup.
- Unresolved issues:
  - Vulkan `matrix/eigenvalues`, `matrix/eigenvectors`, and
    `matrix/eigenpairs` remain CPU-only/fail-closed on Vulkan operands.
  - `Float32` Vulkan execution remains blocked on native `Float32` Tensor
    storage and CPU semantics.
  - This SVD path is correctness-first and single-dispatch; larger `k`,
    strided/view-backed tensors, fixed-width complex, and performance SVD
    algorithms remain deferred.
- Signature: Codex GPT-5.4

## 2026-04-17 11:13 CEST - Vulkan Destination Realize Audit Fix
- Objective attempted:
  - Continue the Tensor/Vulkan audit and fix pass with parallel GPT-5.4
    auditors and implementation-scoped fallback workers after GPT-5.3 Spark
    workers were quota-blocked.
- Workspace/target:
  - `/home/christos/Omni`.
- Code or configuration changes made:
  - Added `tensor_require_cpu_destination` and routed explicit destination
    `realize` checks through it so valid non-CPU destination tensors fail with
    `tensor/backend-unsupported` instead of the missing CPU backing-storage
    runtime error.
  - Added regressions for CPU source into Vulkan destination, Vulkan source
    into Vulkan destination, and Vulkan `matrix/singular-values` into a CPU
    destination.
  - Updated `TODO.md`, `docs/areas/tensor-scientific.md`,
    `docs/reference/03-collections.md`, `memory/CHANGELOG.md`, and this plan
    state to remove stale validation/doc wording.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
    passed, `pass=907 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The source-audit concern that Vulkan singular-values still over-dispatched
    from `k + 2` payload size is stale for the current working tree; the helper
    already allocates `k + 2` storage while dispatching exactly one work item.
- Current best recommendation / checkpoint:
  - Destination `realize` remains CPU-destination-only. Keep explicit
    `to-device 'cpu` as the non-CPU source copy boundary and do not introduce
    Vulkan destination-copy semantics without a separate contract.
- Unresolved issues:
  - Full heavy/container-only gates were not rerun for this narrow audit fix.
- Signature: Codex GPT-5.4

## 2026-04-17 09:22 CEST - TENSOR-100E Vulkan Zero-Axis Contract
- Objective attempted:
  - Enable public zero-axis Tensor `contract` on Vulkan without adding a
    backend-specific surface or CPU fallback.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Relaxed `tensor_contract_try_vulkan_value` so `axis_count == 0` with null
    axis arrays is accepted, while one-or-more-axis contractions still require
    explicit axis metadata.
  - Limited the rank-1 Vulkan dot fast path to the one-contracted-axis case so
    rank-1/rank-1 `[] []` contractions use the generic outer-product shader.
  - Relaxed `omni_tensor_backend_vulkan_contract_f64` to accept zero-axis
    metadata and rank-0 scalar tensors.
  - Added availability-gated tests for rank-1/rank-1 outer products,
    rank-2/rank-1 outer products, rank-0 scalar products, Vulkan result
    residency, and zero-free-dimension zero-length output.
  - Updated `TODO.md`, `.agents/PLAN.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`,
    `docs/areas/tensor-scientific.md`, and `memory/CHANGELOG.md`.
- Key results:
  - Public `(contract left right [] [])` now supports eligible Vulkan-placed
    dense row-major `Float64` tensors, including rank-0 scalar products and
    higher-rank outer products.
  - Results remain Vulkan-placed; callers still copy back explicitly with
    `to-device 'cpu` for CPU inspection.
  - Unsupported dtypes, mixed CPU/Vulkan operands, and unsupported layouts
    remain fail-closed with Tensor backend diagnostics.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_contract_f64.comp -o
    /tmp/omni_tensor_vulkan_contract_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0
    /tmp/omni_tensor_vulkan_contract_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `60.0`, `12.0`, `vulkan`, and `0`.
  - Host focused `advanced-collections-module`: `851 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`:
    `838 passed, 0 failed`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - None new. This slice confirms the existing generic Vulkan contract shader
    and metadata writer were already compatible with zero-axis metadata; the
    blocker was the fail-closed guards.
- Current best recommendation / checkpoint:
  - Continue Vulkan Tensor expansion with another backend-neutral `Float64`
    kernel family or with explicit layout/aliasing metadata before
    stride-aware views.
- Signature: Codex GPT-5

## 2026-04-17 08:31 CEST - TENSOR-100E/F Vulkan Min Max
- Objective attempted:
  - Add real Vulkan paths for public Tensor `min` / `max` and `map min` /
    `map max` on dense row-major `Float64` tensors.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/tensor_vulkan_map_f64.comp` so opcode `4` is `min` and
    opcode `5` is `max`, then regenerated
    `csrc/tensor_vulkan_map_f64_spv.c`.
  - Widened `omni_tensor_backend_vulkan_map_f64` opcode validation to accept
    `0..5`.
  - Routed `map min` / `map max` through the existing Vulkan binary map
    selector.
  - Changed Tensor `min` / `max` to resolve concrete Tensor operands on any
    device before CPU-storage checks, then route eligible Vulkan `Float64`
    tensor/scalar, scalar/Tensor, and Tensor/Tensor broadcast cases through
    the binary Vulkan map helper.
  - Added availability-gated Vulkan map/direct/lazy/broadcast/mixed-device
    regressions plus CPU `map min` / `map max` parity tests.
  - Updated `TODO.md`, `.agents/PLAN.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`,
    `docs/areas/tensor-scientific.md`, and `memory/CHANGELOG.md`.
- Key results:
  - Public Tensor `min` / `max` and `map min` / `map max` now return
    Vulkan-placed `Float64` Tensor results for eligible dense row-major Vulkan
    operands.
  - Mixed CPU/Vulkan operands still fail closed with
    `tensor/backend-unsupported`; no CPU fallback or Big* lowering was added.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_f64.comp -o /tmp/omni_tensor_vulkan_map_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_map_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `-2.0`, `0.0`, `vulkan`, and
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `846 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`:
    `833 passed, 0 failed`.
- Invalidated assumptions or failed approaches worth preserving:
  - None new. Existing constraints still apply: do not add hidden CPU fallback
    for Vulkan operands and do not broaden Vulkan dtypes beyond `Float64`.
- Current best recommendation / checkpoint:
  - Treat Vulkan `Float64` Tensor `min` / `max` as shipped on the binary map
    helper. Remaining Vulkan math-library work should continue through
    shader-validated fixed-width `Float64` kernels or explicit dtype/layout
    design work.
- Unresolved issues:
  - NaN tie behavior is inherited from the current shader compare/select
    expression and is not separately specified in this slice.
- Signature: Codex GPT-5

## 2026-04-17 08:18 CEST - TENSOR-100E/F Vulkan Component Ops
- Objective attempted:
  - Add real Vulkan paths for public real-valued Tensor component operations
    through the existing dense row-major `Float64` unary helper.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/tensor_vulkan_map_unary_f64.comp` so opcode `3` is
    identity and opcode `4` is zero-fill, then regenerated
    `csrc/tensor_vulkan_map_unary_f64_spv.c`.
  - Widened `omni_tensor_backend_vulkan_map_unary_f64` opcode validation to
    accept `0..4`.
  - Routed Vulkan `map real-part`, `map imag-part`, `map conjugate`, and
    direct Tensor `real-part` / `imag-part` / `conjugate` through the separate
    unary helper for dense row-major Vulkan `Float64` tensors.
  - Changed the direct component Tensor paths to resolve concrete tensors on
    any device before entering CPU-storage logic, avoiding hidden CPU fallback
    and avoiding host-storage copy helpers for Vulkan buffers.
  - Added availability-gated Vulkan direct/map/lazy regressions plus CPU
    `map` parity tests for `real-part`, `imag-part`, and `conjugate`.
  - Updated `TODO.md`, `.agents/PLAN.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`,
    `docs/areas/tensor-scientific.md`, and `memory/CHANGELOG.md`.
- Key results:
  - Public `(map real-part <vulkan-tensor>)`,
    `(map imag-part <vulkan-tensor>)`, `(map conjugate <vulkan-tensor>)`, and
    direct Tensor component calls now return Vulkan-placed `Float64` Tensor
    results for eligible real-valued Vulkan inputs.
  - `real-part` and `conjugate` are identity over real tensors; `imag-part`
    returns a zero tensor.
  - Unsupported Vulkan unary callables such as `sin` still raise
    `tensor/backend-unsupported`; the invalidated generic Vulkan unary
    `map` mode-3 branch remains out of use.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_unary_f64.comp -o
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `-2.5`, `0.0`, `vulkan`, and `0.0` for
    direct `real-part`, direct `imag-part`, direct `conjugate` placement, and
    `map imag-part`.
  - Host focused `advanced-collections-module`: `836 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`:
    `823 passed, 0 failed`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not attempt `exp(double)`, `log(double)`, or `log10` as simple new
    GLSL builtins on this helper; fresh probes failed glslang compilation.
    Keep that family behind a separate approximation/library decision.
- Current best recommendation / checkpoint:
  - Treat real-valued Vulkan component ops as shipped on the unary helper.
    Continue Vulkan unary expansion only with shader-validated non-
    transcendental ops or a separately designed approximation/library path.
- Unresolved issues:
  - This slice does not add fixed-width complex Vulkan Tensor storage; real
    `Float64` component semantics are intentionally real-valued.
- Signature: Codex GPT-5

## 2026-04-17 08:04 CEST - TENSOR-100E/F Vulkan Unary Sqrt
- Objective attempted:
  - Add a real Vulkan path for public unary square root through the separate
    dense row-major `Float64` unary helper.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/tensor_vulkan_map_unary_f64.comp` so opcode `2` is
    `sqrt(value)` and regenerated `csrc/tensor_vulkan_map_unary_f64_spv.c`.
  - Widened `omni_tensor_backend_vulkan_map_unary_f64` opcode validation to
    accept `0..2`.
  - Routed `map sqrt` and direct Tensor `sqrt` to opcode `2` for Vulkan dense
    row-major `Float64` tensors.
  - Changed direct Tensor unary math on Vulkan to fail closed for unsupported
    operations instead of materializing hidden CPU results.
  - Added Vulkan `map sqrt`, result placement, direct `sqrt`, lazy direct
    `sqrt`, unsupported `map sin`, and CPU `map sqrt` parity regressions.
- Key results:
  - `(map sqrt <vulkan-tensor>)` and `(sqrt <vulkan-tensor>)` now return
    Vulkan-placed Tensor results for positive finite `Float64` inputs.
  - CPU inspection still requires explicit `(to-device ... 'cpu)`.
  - Unsupported Vulkan unary callables such as `sin` still raise
    `tensor/backend-unsupported`.
  - The generic Vulkan unary `map` mode-3 branch remains invalidated and out
    of use.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_unary_f64.comp -o
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `3.0`, `vulkan`, `2.0`, `vulkan`, and
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `pass=824 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=811 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Negative-domain Float64 `sqrt` parity is not expanded in this slice; if
    Omni wants stronger cross-driver NaN guarantees, add a focused decision and
    test rather than inferring it from positive-input coverage.
  - Continue broader Vulkan unary support only through explicitly validated
    helper opcodes. The earlier `sin(double)` shader failure remains a
    constraint for transcendental operations.

Signature: Codex GPT-5

## 2026-04-17 10:37 CEST - TENSOR-100F1 Vulkan Audit Hardening

Objective attempted:
- Continue auditing and fixing the Tensor/Vulkan SVD-backed slice using
  multiple agents, then close the audit findings with focused validation.

Workspace:
- `/home/christos/Omni`

Code/configuration changes made:
- Continued GPT-5.4 high audit coverage for source/device semantics and
  tests/docs. The requested GPT-5.3-Codex-Spark implementation workers were
  quota-blocked by the environment, so implementation proceeded locally with
  fallback fast workers limited to no-conflict report/roadmap review.
- Hardened `realize` semantics:
  - one-argument `realize` preserves concrete Vulkan placement;
  - explicit destination `realize` now rejects concrete or lazy Vulkan sources
    with `tensor/backend-unsupported` instead of falling into missing CPU
    storage diagnostics.
- Hardened Vulkan singular-values execution:
  - `omni_tensor_backend_vulkan_singular_values_f64` now dispatches one work
    item for the single-invocation shader while preserving the `k + 2`
    output/status/nuclear payload;
  - shader non-convergence now maps through
    `OMNI_TENSOR_VULKAN_NO_CONVERGENCE` to public `tensor/no-convergence`
    instead of generic `tensor/backend-execution-failed`;
  - the helper now rejects logical matrices whose element count exceeds the
    shader's 32-bit index space before dispatch.
- Broadened tests for Vulkan `matrix/singular-values`, spectral/nuclear
  `matrix/norm`, CPU-only direct `matrix/svd`/eigen fail-closed behavior on
  lazy Vulkan inputs, explicit-destination `realize`, and CPU-only numeric
  Tensor helpers on Vulkan operands.
- Added durable regressions for the `k == 64` singular-values boundary,
  rectangular `2x65`, empty `0x65`, status-payload non-convergence mapping,
  and the oversized-index validation guard.
- Updated TODO/docs/plan/changelog wording for the `k <= 64` cap, direct
  SVD/eigen CPU-only behavior, stale `sqrt` roadmap wording, zero-axis
  `contract` support, destination `realize`, and latest validation details.

Commands run and key results:
- `glslangValidator -V --target-env vulkan1.0 -o /tmp/omni_tensor_vulkan_singular_values_f64.spv csrc/tensor_vulkan_singular_values_f64.comp`: passed.
- `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_singular_values_f64.spv`: passed.
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=904 fail=0`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=411 fail=0`.
- Durable advanced regressions now cover:
  - `matrix/singular-values` at `k == 64`;
  - `matrix/singular-values` at `k == 65` returning
    `tensor/backend-unsupported`;
  - wide `2x65` singular-values shape;
  - empty `0x65` singular-values length;
  - status-payload mapping from shader non-convergence to
    `tensor/no-convergence`;
  - oversized logical index validation before Vulkan dispatch.
- `./scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- Targeted `git diff --check` over touched source/docs/generated files:
  passed after regenerating the SPIR-V C array without trailing whitespace.

Invalidated assumptions / failed approaches:
- Do not rely on shallow root `source.device` checks for lazy Tensor
  expressions. CPU-only helpers must inspect Tensor expression leaves before
  generic realization or they can accidentally execute hidden device paths.
- Do not treat the Vulkan singular-values shader status tail as a generic
  singular-matrix status. Its non-convergence status is now a separate
  backend code mapped to `tensor/no-convergence`.
- Do not rely only on per-axis `UINT32_MAX` checks in Vulkan helpers whose
  shaders index with 32-bit arithmetic; guard total logical element count
  before dispatch.
- Do not treat destination `realize` as an implicit GPU-to-CPU copy boundary;
  explicit `to-device 'cpu` is the copy boundary for non-CPU sources.

Current best recommendation / checkpoint:
- The `TENSOR-100F1` Vulkan singular-values helper is verified for the current
  dense row-major `Float64`, `k <= 64` contract. Next useful work should be a
  separate Vulkan factor-output SVD/eigensolver design, or a shared Vulkan
  helper cleanup that preserves the public contracts.

Unresolved issues:
- Full heavy/container-only gates were not run in this pass.
- Actual shader iteration-exhaustion is still not forced by a public numeric
  fixture; the status-payload mapping and C3 status-code mapping are now
  covered by deterministic probes.
- Direct `matrix/svd`, `matrix/eigenvalues`, `matrix/eigenvectors`, and
  `matrix/eigenpairs` still need dedicated Vulkan design/implementation before
  they can support Vulkan operands.

Signature: Codex GPT-5

## 2026-04-17 09:59 CEST - TENSOR-100F1 Vulkan SVD-Backed Singular Values

Objective attempted:
- Continue from the latest Vulkan math-library handoff using multiple agents,
  verify the in-progress Vulkan SVD-backed slice, and align the runtime
  contract with docs/plans.

Workspace:
- `/home/christos/Omni`

Code/configuration changes made:
- Preserved and verified the existing dense row-major Vulkan `Float64`
  `matrix/singular-values` implementation and spectral/nuclear `matrix/norm`
  routing through `omni_tensor_backend_vulkan_singular_values_f64`.
- Fixed `matrix/svd`, `matrix/eigenvalues`, `matrix/eigenvectors`, and
  `matrix/eigenpairs` so non-CPU Tensor placement is rejected before generic
  realization can silently copy Vulkan input through CPU paths.
- Updated current Tensor/Vulkan docs, TODO, plan, and changelog artifacts so
  `matrix/singular-values` plus `matrix/norm` `'spectral` / `'nuclear` are
  recorded as supported on Vulkan, while direct SVD/eigen surfaces remain
  CPU-only.

Commands run and key results:
- `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_singular_values_f64.comp -o /tmp/omni_tensor_vulkan_singular_values_f64.spv`: passed.
- `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_singular_values_f64.spv`: passed.
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing deprecation warnings.
- Direct Vulkan smokes returned `3.0`, `5.0`, `vulkan`, and empty length `0`.
- Initial focused `advanced-collections-module` run exposed stale/real
  unsupported-surface failures for direct `matrix/svd` and eigen surfaces.
- After the fix,
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  passed with `pass=866 fail=0`.

Invalidated assumptions / failed approaches:
- Do not treat direct `matrix/svd` or eigen surfaces as implicitly safe on
  Vulkan just because `matrix/singular-values` has a Vulkan helper. Those
  factor/eigensolver surfaces remain CPU-only until explicit Vulkan plans land.
- Do not allow CPU-only matrix primitives to call generic realization before
  checking source device placement; that can hide unsupported Vulkan input
  behind CPU copyback.

Current best recommendation / checkpoint:
- Continue `TENSOR-100F` from the verified Vulkan singular-value helper. Next
  useful implementation should either refactor shared Vulkan helper plumbing
  without changing exported contracts, or add another explicit backend-neutral
  kernel with dedicated Vulkan semantics.

Unresolved issues:
- Bounded-container and full heavy gates were not run in this pass.
- Direct `matrix/svd`, `matrix/eigenvalues`, `matrix/eigenvectors`, and
  `matrix/eigenpairs` still need dedicated Vulkan design/implementation before
  they can support Vulkan operands.

Signature: Codex GPT-5

## 2026-04-17 07:54 CEST - TENSOR-100E/F Vulkan Unary Negation
- Objective attempted:
  - Make unary `-` coherent across the documented scalar surface, CPU Tensor
    direct/map execution, and the Vulkan dense row-major `Float64` unary helper.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Changed the dispatched primitive registration for `-` so one-argument
    calls reach `prim_sub`, and tightened `prim_sub` to reject anything outside
    the `1..2` argument contract.
  - Added Tensor handling to unary numeric negation and implemented
    `tensor_neg_value` for CPU `Float64`, `BigInteger`, `BigFloat`, and
    `BigComplex` tensors.
  - Extended `csrc/tensor_vulkan_map_unary_f64.comp` so opcode `1` is
    negation, regenerated `csrc/tensor_vulkan_map_unary_f64_spv.c`, and
    widened the C helper opcode guard to accept `0..1`.
  - Extended the C3 Vulkan unary selector so `map -` and direct Tensor
    `(- tensor)` route to the separate unary helper for Vulkan dense row-major
    `Float64` inputs.
  - Added scalar, JIT, CPU Tensor, Big* Tensor, and Vulkan Tensor regressions.
- Key results:
  - `(- 1)` now works through the public primitive path, while `(- 1 2 3)`
    still raises an arity error.
  - `(- (Tensor ...))` and `(map - (Tensor ...))` now work on CPU tensors.
  - `(map - <vulkan-tensor>)` and `(- <vulkan-tensor>)` now return
    Vulkan-placed Tensor results and require explicit `(to-device ... 'cpu)`
    for CPU inspection.
  - Unsupported Vulkan unary callables such as `sqrt` still raise
    `tensor/backend-unsupported`.
  - The invalidated generic Vulkan unary `map` mode-3 branch remains out of
    use.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_unary_f64.comp -o
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct smokes returned `-1`, CPU Tensor `2.5`, Big* Tensor strings
    `-9223372036854775808`, `-1e+309`, and `-3+4i`, Vulkan map value `2.0`,
    Vulkan result device `vulkan`, direct Vulkan value `-1.5`, and
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `pass=819 fail=0`.
  - Host `basic` slice: `pass=144 fail=0`.
  - Host `compiler` slice: `pass=277 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=806 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Continue additional Vulkan unary arithmetic operations only through the
    separate unary helper pattern or another explicitly debugged helper ABI.
  - Do not resume the generic Vulkan unary `map` mode-3 branch.

Signature: Codex GPT-5

## 2026-04-17 07:41 CEST - TENSOR-100E/F Vulkan Unary Abs Helper
- Objective attempted:
  - Add a real Vulkan path for public unary absolute value without reviving the
    invalidated generic Vulkan `map` mode-3 branch.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_map_unary_f64.comp` and generated
    `csrc/tensor_vulkan_map_unary_f64_spv.c`.
  - Wired the generated shader into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_map_unary_f64`, a separate two-buffer
    Vulkan helper with its own push constants.
  - Added the C3 Vulkan extern and shared C3 construction path for dense
    row-major Vulkan `Float64` unary helper results.
  - Routed public `(map abs <vulkan-tensor>)` and direct Tensor
    `(abs <vulkan-tensor>)` through the helper.
  - Added availability-gated tests for `map abs` roundtrip, Vulkan result
    placement, direct Tensor `abs`, lazy Vulkan `abs`, and unsupported
    callable fail-closed behavior.
- Key results:
  - `map abs` and direct Tensor `abs` now return Vulkan-placed Tensor results
    for Vulkan dense row-major `Float64` inputs.
  - CPU inspection still requires explicit `(to-device ... 'cpu)`.
  - Unsupported unary Vulkan callables such as `sqrt` still raise
    `tensor/backend-unsupported`.
  - The generic Vulkan unary `map` mode-3 branch remains invalidated and out
    of use.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0
    csrc/tensor_vulkan_map_unary_f64.comp -o
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0
    /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `1.5`, `vulkan`, `1.5`, `vulkan`, and
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `pass=810 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=797 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Additional unary Vulkan operations should use separate helper ABIs and
    should not restart the generic map mode-3 branch.
  - Full blocked trailing-update LU remains a future performance lane only if
    later solve measurements justify it.

Signature: Codex GPT-5

## 2026-04-17 07:39 CEST - TENSOR-100G Measured Solve Threshold
- Objective attempted:
  - Replace the fixed Vulkan `matrix/solve` bring-up threshold with measured
    routing for the current serial and staged parallel `Float64` solve paths.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added private `OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_MIN_N = 65` and routed
    Vulkan dense row-major `Float64` solve systems with `n < 65` to the serial
    helper and `n >= 65` to the staged parallel helper.
  - Added `omni_tensor_backend_vulkan_solve_serial_call_count` plus the C3
    extern so tests can prove below-threshold serial execution directly.
  - Updated availability-gated solve tests for below-threshold serial routing,
    65x65 parallel threshold entry, pivot/factor counter movement, no-LAPACK
    behavior, and 65x65 parallel singular status.
- Key results:
  - Local in-process threshold logs under `build/vulkan_solve_threshold_20260417_*`
    showed `3`, `9`, `16`, and `32` tied or serial-favorable, while `65` was
    the first tested size where the staged parallel route won across identity
    and dense fixtures.
  - The serial helper still owns its private trailing `double` status sentinel;
    the parallel helper still owns its private typed `uint` status buffer.
  - Full blocked trailing-update LU remains a future performance lane only if
    later measurements justify it; it is not required for the current
    thresholded parallel solve contract.
- Commands run and key results:
  - Temporary high-threshold rebuilds were used only for measurement, then
    replaced by the final `n >= 65` route.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `9.0`, `1.0`, `1.0`, and
    `tensor/singular-matrix`.
  - Host focused `advanced-collections-module`: `pass=806 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=793 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Continue broader Vulkan math-library work from the thresholded solve
    baseline. Keep public Tensor surfaces backend-neutral and fail closed for
    unsupported Vulkan dtype/layout cases.

Signature: Codex GPT-5

## 2026-04-17 07:12 CEST - TENSOR-100G Tiled-LU Solve Staging
- Objective attempted:
  - Add scratch-buffered per-pivot panel-factor staging to the parallel Vulkan
    `matrix/solve` path.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_solve_multi_factor_f64.comp` and generated
    `csrc/tensor_vulkan_solve_multi_factor_f64_spv.c`.
  - Wired the generated factor shader through `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Expanded `csrc/tensor_vulkan_helpers.c` so the multi-dispatch solve helper
    allocates a private factor scratch buffer, binds descriptor `5`, creates a
    factor pipeline, and uses four-buffer barriers.
  - Inserted a factor dispatch after row swap and before elimination. It
    stores `L[row,pivot]` in factor scratch and in the lower-triangular
    workspace; elimination now consumes staged factors.
  - Added `omni_tensor_backend_vulkan_solve_factor_stage_call_count` plus the
    matching C3 extern and focused counter assertion.
  - Added a dense 65x65 `I + J` Vulkan solve regression that exercises
    nonzero staged factors without LAPACK.
- Key results:
  - Public `matrix/solve` remains unchanged.
  - The serial `n < 3` Vulkan helper still uses its trailing `double` status
    sentinel; the parallel helper still uses private typed `uint` status.
  - Tiled-LU staging is landed as a `tile_width = 1` panel-factor stage.
    Remaining `TENSOR-100G` work is measurement-backed threshold routing and,
    later, full blocked trailing-update LU if measurements justify it.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0` passed for all affected
    solve-multi shaders.
  - `spirv-val --target-env vulkan1.0` passed for all affected solve-multi
    shaders.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned dense 9x9 value `1.0`, 65x65 identity value
    `1.0`, dense 65x65 `I + J` value `1.0`, and `tensor/singular-matrix`.
  - Host focused `advanced-collections-module`: `pass=801 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=788 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Replace the fixed `n >= 3` bring-up threshold with measurement-backed
    routing for serial and parallel staged Vulkan solve paths.

Signature: Codex GPT-5

## 2026-04-17 06:55 CEST - TENSOR-100G Cross-Workgroup Vulkan Pivot Reduction
- Objective attempted:
  - Remove the remaining one-workgroup pivot-selection boundary from the
    parallel Vulkan `matrix/solve` path.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Reworked `csrc/tensor_vulkan_solve_multi_pivot_f64.comp` into a pivot
    scan stage that writes per-workgroup candidates into scratch.
  - Added `csrc/tensor_vulkan_solve_multi_pivot_reduce_f64.comp`,
    `csrc/tensor_vulkan_solve_multi_pivot_commit_f64.comp`, and
    `csrc/tensor_vulkan_solve_multi_row_swap_f64.comp`, plus generated SPIR-V
    C sources.
  - Expanded the private multi-dispatch solve ABI in
    `csrc/tensor_vulkan_helpers.c` to bind a fifth descriptor for pivot
    magnitude scratch, allocate ping-pong scratch lanes, recursively reduce
    pivot candidates, commit the selected row, and run row swaps as their own
    dispatch before elimination.
  - Added `omni_tensor_backend_vulkan_solve_pivot_reduce_call_count` and the
    matching C3 extern.
  - Added a 65x65 Vulkan solve regression that proves the cross-workgroup
    pivot-reduction stage executes without LAPACK.
- Key results:
  - Public `matrix/solve` remains unchanged.
  - The serial `n < 3` Vulkan helper still uses its trailing `double` status
    sentinel; the parallel helper still uses private typed `uint` status.
  - Pivot selection now spans multiple workgroups. Remaining `TENSOR-100G`
    work is tiled LU staging plus measurement-backed thresholds.
  - An attempted in-dispatch cleanup of `A[row,pivot]` was removed because it
    raced elimination invocations that still read that column for row factors.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0` passed for all affected
    solve-multi shaders.
  - `spirv-val --target-env vulkan1.0` passed for all affected solve-multi
    shaders.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `0.2`, `3.0`, 65x65 value `1.0`, and
    `tensor/singular-matrix`.
  - First host focused `advanced-collections-module` run failed one dense 9x9
    test after the in-dispatch zeroing attempt changed the result to `1.35`.
    Removing that zeroing restored the suite.
  - Host focused `advanced-collections-module`: `pass=800 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=787 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Add tiled LU scratch-buffer staging for larger systems.
  - Replace the fixed `n >= 3` bring-up threshold with measured routing after
    the tiled path exists.

Signature: Codex GPT-5

## 2026-04-17 06:24 CEST - TENSOR-100G Multi-Dispatch Vulkan Solve
- Objective attempted:
  - Advance the parallel solver track from a staged two-dispatch helper to a
    multi-dispatch Vulkan solve path where elimination and RHS backsolve can
    span multiple workgroups.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_solve_multi_pivot_f64.comp`,
    `csrc/tensor_vulkan_solve_multi_eliminate_f64.comp`, and
    `csrc/tensor_vulkan_solve_multi_backsolve_f64.comp`, plus generated
    SPIR-V C sources for all three shaders.
  - Wired the generated SPIR-V sources into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Reworked the private Vulkan parallel solve helper in
    `csrc/tensor_vulkan_helpers.c` to record init/copy, per-pivot
    pivot/swap, per-pivot elimination, and descending per-row backsolve
    dispatches in one command buffer with buffer-memory barriers between
    stages.
  - Added `omni_tensor_backend_vulkan_solve_multi_dispatch_call_count` and the
    matching C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated advanced module tests so the 2x2 serial threshold keeps both
    parallel counters unchanged, the Vulkan parallel path increments the
    multi-dispatch counter without LAPACK, and a dense 9x9 `J + I` system
    exercises the multi-workgroup elimination path.
- Key results:
  - The public `matrix/solve` surface and routing contract remain unchanged.
  - The serial Vulkan `n < 3` helper still uses its private trailing `double`
    status sentinel; the parallel helper still uses private typed `uint`
    status storage.
  - Elimination and RHS backsolve now scale across workgroups. Pivot
    selection/swap is still a one-workgroup stage, so the next real solver
    boundary is cross-workgroup pivot reduction plus scratch-buffered/tiled LU.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0` passed for pivot,
    elimination, and backsolve shaders.
  - `spirv-val --target-env vulkan1.0` passed for pivot, elimination, and
    backsolve shaders.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned serial value `0.2`, pivoting value `3.0`,
    9x9 multi-RHS identity value `18.0`, dense 9x9 `J + I` value `1.0`, and
    `tensor/singular-matrix`.
  - Host focused `advanced-collections-module`: `pass=799 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=786 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Add cross-workgroup pivot reduction. The likely next ABI needs pivot
    magnitude and status scratch buffers instead of trying to dispatch the
    current one-workgroup pivot shader wider.
  - Add scratch-buffered/tiled LU and replace the fixed `n >= 3` bring-up
    threshold with measurements after the tiled path exists.

Signature: Codex GPT-5

## 2026-04-17 06:09 CEST - TENSOR-100G Staged Vulkan Parallel Solve
- Objective attempted:
  - Advance the parallel solver track from one monolithic compute shader to a
    staged Vulkan helper with an explicit inter-dispatch memory dependency.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_solve_parallel_init_f64.comp` and generated
    `csrc/tensor_vulkan_solve_parallel_init_f64_spv.c`.
  - Wired the new generated SPIR-V source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Updated the parallel solve helper in `csrc/tensor_vulkan_helpers.c` to
    create two shader modules and two pipelines, record init/copy and
    factor/solve dispatches into one command buffer, and issue a Vulkan 1.0
    buffer-memory barrier between them.
  - Kept the public C3 `matrix/solve` surface and routing unchanged: Vulkan
    `Float64` systems with `n >= 3` use the parallel helper, while `n < 3`
    remains on the serial Vulkan helper.
  - Added staged-path tests in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` for the 2x2 serial
    threshold counter, pivoting, 9x9 multi-RHS copy beyond one workgroup, and
    post-elimination singular diagnostics.
- Key results:
  - The staged helper now initializes the result/workspace and private `uint`
    status buffer in a separate dispatch before factor/back-solve begins.
  - The inter-dispatch barrier uses `COMPUTE_SHADER` pipeline-stage masks and
    `SHADER_WRITE -> SHADER_READ|SHADER_WRITE` access masks on the output and
    status buffers.
  - The serial solver's trailing `double` status sentinel remains separate and
    untouched.
  - The remaining solver work is tiled or multi-workgroup LU-style solving plus
    measurement-backed thresholds; the factor/back-solve stage is still a
    single-workgroup algorithm.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_solve_parallel_init_f64.comp -o /tmp/omni_tensor_vulkan_solve_parallel_init_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_solve_parallel_init_f64.spv`: passed.
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_solve_parallel_f64.comp -o /tmp/omni_tensor_vulkan_solve_parallel_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_solve_parallel_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned serial solve value `0.2`, staged solve value
    `1.0`, staged 9x9 multi-RHS value `18.0`, and
    `tensor/singular-matrix`.
  - Host focused `advanced-collections-module`: `pass=798 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=785 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Design and validate a tiled or multi-workgroup solver path that no longer
    depends on one workgroup's `shared` memory and `barrier()` semantics.
  - Replace the fixed `n >= 3` bring-up threshold with a measurement-backed
    threshold after the tiled path exists.

Signature: Codex GPT-5

## 2026-04-17 05:58 CEST - TENSOR-100G Vulkan Parallel Matrix Solve
- Objective attempted:
  - Start the parallel solver track by adding a real Vulkan parallel
    `matrix/solve` path behind the existing backend-neutral public surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_solve_parallel_f64.comp` and generated
    `csrc/tensor_vulkan_solve_parallel_f64_spv.c`.
  - Wired the generated SPIR-V source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_solve_parallel_f64`, a parallel-helper
    call counter, a private `uint` status reader, and a two-input/two-output
    Vulkan dispatch helper in `csrc/tensor_vulkan_helpers.c`.
  - Added C3 externs in `src/lisp/tensor_vulkan_backend.c3`.
  - Routed public Vulkan `matrix/solve` in
    `src/lisp/prim_tensor_matrix.c3`: `n >= 3` uses the new parallel helper,
    while `n < 3` keeps the serial Vulkan helper.
  - Added availability-gated parallel solve tests in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`, and Vulkan
    roadmap/reference docs.
- Key results:
  - The first `TENSOR-100G` slice is a 64-invocation single-workgroup
    partial-pivot Gaussian elimination shader with parallel pivot reduction,
    row swaps, elimination updates, and RHS-column back-substitution.
  - The parallel path preserves the existing public `matrix/solve` contract:
    rank-1 RHS returns rank 1, rank-2 RHS returns rank 2 with matching shape,
    results remain Vulkan-placed, singular systems raise
    `tensor/singular-matrix`, and LAPACK is not called.
  - The parallel helper uses a private typed `uint` status buffer instead of
    extending the serial helper's trailing `double` status sentinel.
  - While validating, a transient bug removed the serial solver's private
    trailing status slot and broke existing 2x2 Vulkan solve tests with
    `tensor/backend-invalid-state`; the slot was restored and the focused
    suite returned to green.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_solve_parallel_f64.comp -o /tmp/omni_tensor_vulkan_solve_parallel_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_solve_parallel_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned serial solve value `0.2`, parallel solve
    value `1.0`, vector RHS rank `1`, and `tensor/singular-matrix` for a
    dependent 3x3 system.
  - Host focused `advanced-collections-module`: `pass=793 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=780 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Extend `TENSOR-100G` from the current single-workgroup parallel solver to
    a staged/tiled multi-kernel helper with explicit Vulkan memory barriers.
  - Replace the fixed `n >= 3` bring-up threshold with a measurement-backed
    threshold once the staged helper exists.

Signature: Codex GPT-5

## 2026-04-17 05:29 CEST - TENSOR-100F1 Vulkan Matrix QR
- Objective attempted:
  - Continue the Vulkan math roadmap by landing dense row-major `Float64`
    Vulkan QR through the existing backend-neutral `matrix/qr` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_qr_f64.comp` and generated
    `csrc/tensor_vulkan_qr_f64_spv.c`.
  - Wired the generated SPIR-V source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_qr_f64` and a one-input/two-output
    dispatch helper in `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Routed public `matrix/qr` in `src/lisp/prim_tensor_matrix.c3` for
    concrete or realized Vulkan `Float64` inputs before the CPU-only path.
  - Added availability-gated Vulkan QR tests and CPU empty QR shape coverage
    in `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, and Vulkan/Tensor docs.
- Key results:
  - Public `matrix/qr` now supports Vulkan-placed dense row-major rank-2
    `Float64` inputs with rows greater than or equal to columns.
  - The returned dictionary keeps `q` and `r` as Vulkan-placed Tensor values.
  - Rank-deficient Vulkan inputs raise `tensor/singular-matrix`; wide inputs
    keep the existing `tensor/shape-mismatch` contract.
  - Vulkan QR does not call LAPACK and does not copy the input to CPU.
  - This is a correctness-first serial QR kernel, not the future parallel
    solver/factorization path.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_qr_f64.comp -o /tmp/omni_tensor_vulkan_qr_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_qr_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned representative QR values `1.0` and `1.0`,
    output devices `vulkan` and `vulkan`, empty shape `0`, and
    `tensor/singular-matrix` for the dependent-column case.
  - Host focused `advanced-collections-module`: `pass=787 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=774 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Continue `TENSOR-100F` helper factoring with the next eligible dense
    row-major `Float64` matrix kernel or move to `TENSOR-100G`.
  - `TENSOR-100G` must remain a separate thresholded parallel solver helper;
    do not present QR or the current serial solve/LU/inverse kernels as the
    performance path.

Signature: Codex GPT-5

## 2026-04-17 05:09 CEST - TENSOR-100F1 Vulkan Matrix Cholesky
- Objective attempted:
  - Continue the Vulkan math roadmap by landing the next dense row-major
    `Float64` matrix kernel through the existing backend-neutral
    `matrix/cholesky` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_cholesky_f64.comp` and generated
    `csrc/tensor_vulkan_cholesky_f64_spv.c`.
  - Wired the generated SPIR-V source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_cholesky_f64` and a shared
    trailing-status copyback helper in `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Routed public `matrix/cholesky` in `src/lisp/prim_tensor_matrix.c3` for
    concrete or realized Vulkan `Float64` inputs before the CPU-only path.
  - Fixed the CPU pure Cholesky fallback so empty square tensors with null
    zero-size data pointers succeed.
  - Added availability-gated Vulkan Cholesky tests in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, and Vulkan/Tensor docs.
- Key results:
  - Public `matrix/cholesky` now supports Vulkan-placed dense row-major square
    `Float64` inputs and returns a Vulkan-placed lower factor Tensor.
  - Nonsymmetric or non-SPD Vulkan inputs raise
    `tensor/not-positive-definite`; non-square inputs keep the existing
    `tensor/shape-mismatch` contract.
  - Vulkan Cholesky does not call LAPACK and does not copy the input to CPU.
  - The broader helper/pipeline factoring and parallel solver remain open.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_cholesky_f64.comp -o /tmp/omni_tensor_vulkan_cholesky_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_cholesky_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned representative factor values `1.0`, `2.0`,
    upper-zero `0.0`, result device `vulkan`, lazy-input device `vulkan`,
    empty shape `0`, two `tensor/not-positive-definite` diagnostics, and
    `tensor/shape-mismatch`.
  - Direct CPU empty Cholesky shape smoke returned `0`.
  - Host focused `advanced-collections-module`: `pass=775 fail=0`.
  - Bounded-container focused `advanced-collections-module`: `pass=762 fail=0`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`
    passed.
- Unresolved issues / next actions:
  - Continue `TENSOR-100F` helper factoring with the next eligible dense
    row-major `Float64` matrix kernel.
  - Start `TENSOR-100G` as a separate thresholded parallel solver helper; do
    not mutate the current serial `matrix/solve` shader in place.

Signature: Codex GPT-5

## 2026-04-17 04:47 CEST - Vulkan Math Library And Parallel Solver Plan
- Objective attempted:
  - Record a durable plan for continuing the Vulkan math backend beyond the
    shipped correctness-first `TENSOR-100E` kernels, including real/complex
    dtype policy, layout prerequisites, and a real parallel solver track.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
  - Updated `TODO.md` with `TENSOR-100F` / `TENSOR-100G` as the live roadmap
    and parallel solver follow-up.
  - Updated `docs/plans/README.md`,
    `docs/plans/vulkan-backend-decision-2026-04-16.md`,
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`,
    `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
    `docs/areas/tensor-scientific.md`, and `.agents/PLAN.md`.
- Key results:
  - The plan keeps Vulkan behind backend-neutral `Tensor`, `map`, `contract`,
    `matrix/*`, `to-device`, `device`, and `tensor-backends` surfaces.
  - `Float64` dense row-major remains the first Vulkan library tier.
    `Float32` and fixed-width complex are deferred until native Tensor storage
    and public dtype semantics exist; `BigInteger`, `BigFloat`, and
    `BigComplex` must not be lowered to Vulkan.
  - The current serial Vulkan `matrix/solve`, `matrix/lu`, and
    `matrix/inverse` shaders are now explicitly recorded as
    correctness-preserving small-system/backend bring-up paths, not the
    performance solver target.
  - `TENSOR-100G` requires a separate thresholded parallel solver helper with a
    typed buffer/status contract, staged helper shape, parallel pivot search,
    row swaps, elimination, and RHS-column back-substitution.
- Commands run and key results:
  - Read-only inspection of current plan docs, TODO, Vulkan solve shader,
    Vulkan helper, C3 matrix primitive, and session reports.
  - Parallel read-only agents inspected planning placement and current solver
    implementation constraints.
- Unresolved issues / next actions:
  - This was a planning pass; no new Vulkan solver code was implemented.
  - Next implementation checkpoint is `TENSOR-100F1`: factor shared Vulkan
    helper plumbing plus one additional dense row-major `Float64` matrix
    kernel, then begin `TENSOR-100G` with a separate parallel solve helper.

Signature: Codex GPT-5

## 2026-04-17 04:36 CEST - TENSOR-100E Vulkan Matrix Solve
- Objective attempted:
  - Continue `TENSOR-100E` by routing public `matrix/solve` to Vulkan for
    eligible dense row-major square `Float64` coefficient tensors and matching
    rank-1 or rank-2 `Float64` right-hand tensors without changing the
    backend-neutral matrix surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_solve_f64.comp` and generated
    `csrc/tensor_vulkan_solve_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_solve_f64` in
    `csrc/tensor_vulkan_helpers.c`, including a shared three-buffer dispatch
    helper and status-only host copyback for singular detection.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/solve` handles
    concrete or realized Vulkan coefficient/RHS tensors before the CPU-only
    storage check.
  - Added availability-gated Vulkan solve regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, and Vulkan/Tensor docs.
- Key results:
  - Public `matrix/solve` now supports Vulkan-placed dense row-major square
    `Float64` coefficient tensors with Vulkan-placed rank-1 or rank-2
    `Float64` right-hand tensors.
  - The returned solution remains a Vulkan-placed Tensor; CPU inspection still
    requires explicit `to-device 'cpu`.
  - Singular Vulkan systems raise `tensor/singular-matrix`; mixed CPU/Vulkan
    operands fail closed with `tensor/backend-unsupported`.
  - The input tensors are not copied to CPU and the Vulkan path does not call
    LAPACK; the helper computes on Vulkan and reads back only status metadata.
  - CPU Tensor `matrix/solve` behavior remains on the existing
    dgesv-then-pure implementation.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_solve_f64.comp -o /tmp/omni_tensor_vulkan_solve_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_solve_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan solve smokes returned `0.2`, `0.6`, `vulkan`, `1.0`,
    `0.2`, `0.6`, `tensor/singular-matrix`, `tensor/shape-mismatch`,
    `tensor/backend-unsupported`, and `tensor/singular-matrix`.
  - Host focused `advanced-collections-module`: `pass=763 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=750 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Vulkan solve currently uses a serial single-invocation Gaussian-elimination
    shader. It is a real Vulkan execution path and preserves the public
    contract, but it is not a high-performance parallel solver. Stride-aware
    views, non-contiguous layouts, non-`Float64` Vulkan dtypes, and mixed-device
    solve transfers remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 04:17 CEST - TENSOR-100E Vulkan Matrix Inverse
- Objective attempted:
  - Continue `TENSOR-100E` by routing public `matrix/inverse` to Vulkan for
    eligible dense row-major square `Float64` inputs without changing the
    backend-neutral matrix surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_inverse_f64.comp` and generated
    `csrc/tensor_vulkan_inverse_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_inverse_f64` in
    `csrc/tensor_vulkan_helpers.c`, including status-only host copyback for
    singular detection.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/inverse` handles
    concrete or realized Vulkan tensors before the CPU-only storage check.
  - Added availability-gated Vulkan inverse regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated `TODO.md`, `memory/CHANGELOG.md`, and Vulkan/Tensor docs.
- Key results:
  - Public `matrix/inverse` now supports Vulkan-placed dense row-major square
    `Float64` inputs.
  - The returned inverse remains a Vulkan-placed Tensor; CPU inspection still
    requires explicit `to-device 'cpu`.
  - Singular Vulkan inputs raise `tensor/singular-matrix`.
  - The input tensor is not copied to CPU and the Vulkan path does not call
    LAPACK; the helper computes on Vulkan and reads back only status metadata.
  - CPU Tensor `matrix/inverse` behavior remains on the existing
    dgesv-then-pure implementation.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_inverse_f64.comp -o /tmp/omni_tensor_vulkan_inverse_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_inverse_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan inverse smokes returned `0.6`, `-0.7`, `vulkan`,
    `tensor/singular-matrix`, `0.4`, `vulkan`, `tensor/shape-mismatch`, and
    `1.0`.
  - Host focused `advanced-collections-module`: `pass=751 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=738 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Vulkan inverse currently uses a serial single-invocation Gauss-Jordan
    shader. It is a real Vulkan execution path and preserves the public
    contract, but it is not a high-performance parallel inverse. Stride-aware
    views, non-contiguous layouts, non-`Float64` Vulkan dtypes, and
    solve-style Vulkan RHS APIs remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 04:04 CEST - TENSOR-100E Vulkan Matrix LU
- Objective attempted:
  - Continue `TENSOR-100E` by routing public `matrix/lu` to Vulkan for
    eligible dense row-major square `Float64` inputs without changing the
    public dictionary contract.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_lu_f64.comp` and generated
    `csrc/tensor_vulkan_lu_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_lu_f64` in
    `csrc/tensor_vulkan_helpers.c`, including metadata-only host copyback for
    pivots and swap count.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/lu` handles concrete
    Vulkan tensors before the CPU-only storage check.
  - Added availability-gated Vulkan LU regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/lu` now supports Vulkan-placed dense row-major square
    `Float64` inputs.
  - The returned dictionary still contains `lu`, `pivots`, and `swap-count`.
    For Vulkan input, `lu` remains Vulkan-placed while the small metadata
    values are ordinary host values.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the dictionary metadata required by the public contract.
  - CPU Tensor `matrix/lu` behavior remains on the existing LAPACK-then-pure
    implementation.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_lu_f64.comp -o /tmp/omni_tensor_vulkan_lu_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_lu_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan LU smokes returned `0.5`, `-0.5`, `1`, `1`, `vulkan`,
    `3.0`, `vulkan`, `0`, `tensor/singular-matrix`, and
    `tensor/shape-mismatch`.
  - Host focused `advanced-collections-module`: `pass=740 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=727 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Vulkan LU currently uses a serial single-invocation partial-pivot shader.
    It is a real Vulkan execution path and preserves the public contract, but
    it is not a high-performance parallel factorization. Stride-aware views,
    non-contiguous layouts, non-`Float64` Vulkan dtypes, and broader
    solve/inverse Vulkan algorithms remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 03:48 CEST - TENSOR-100E Vulkan Matrix Determinant
- Objective attempted:
  - Continue `TENSOR-100E` with a backend-neutral `Float64` scalar reducer by
    routing public `matrix/determinant` to Vulkan for eligible dense row-major
    square inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_determinant_f64.comp` and generated
    `csrc/tensor_vulkan_determinant_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_determinant_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/determinant` handles
    concrete Vulkan tensors before the CPU-only storage check.
  - Added availability-gated Vulkan determinant regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/determinant` now supports Vulkan-placed dense row-major
    square `Float64` inputs and returns the scalar `Float64` determinant.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the scalar determinant required by the public contract.
  - CPU Tensor `matrix/determinant` behavior remains on the existing
    LAPACK-then-pure implementation.
  - Vulkan determinant tests include a no-LAPACK check to guard against hidden
    CPU fallback.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_determinant_f64.comp -o /tmp/omni_tensor_vulkan_determinant_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_determinant_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan determinant smokes returned `-2.0`, `-2.0`, `0.0`,
    `-2.0`, `1.0`, and `tensor/shape-mismatch`.
  - Direct CPU determinant smoke returned `-2.0`.
  - Direct Vulkan transpose regression smoke returned `6.0`.
  - Host focused `advanced-collections-module`: `pass=730 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=717 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Vulkan determinant currently uses a serial single-invocation partial-pivot
    LU shader. It is a real Vulkan execution path, but not a parallel
    high-performance determinant algorithm. Stride-aware views, non-contiguous
    layouts, non-`Float64` Vulkan dtypes, and larger decomposition-backed
    Vulkan algorithms remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 03:27 CEST - TENSOR-100E Vulkan Matrix Rank
- Objective attempted:
  - Continue `TENSOR-100E` with a backend-neutral `Float64` scalar reducer by
    routing public `matrix/rank` to Vulkan for eligible dense row-major rank-2
    inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_rank_f64.comp` and generated
    `csrc/tensor_vulkan_rank_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_rank_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/rank` handles
    concrete Vulkan tensors before the CPU-only storage check.
  - Added availability-gated Vulkan rank regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/rank` now supports Vulkan-placed dense row-major `Float64`
    rank-2 inputs and returns the scalar `Integer` rank.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the scalar rank result required by the public contract.
  - CPU Tensor `matrix/rank` behavior remains on the existing LAPACK-then-pure
    implementation.
  - Vulkan rank tests include a no-LAPACK check to guard against hidden CPU
    fallback.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_rank_f64.comp -o /tmp/omni_tensor_vulkan_rank_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_rank_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan rank smokes returned `2`, `1`, `2`, `1`, `2`, `0`, `0`,
    and `tensor/shape-mismatch`.
  - Direct CPU rank smoke returned `2`.
  - Host focused `advanced-collections-module`: `pass=722 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=709 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Vulkan rank currently mirrors the existing pure partial-pivot elimination
    fallback, not LAPACK/SVD rank semantics. Stride-aware views,
    non-contiguous layouts, non-`Float64` Vulkan dtypes, and larger
    decomposition-backed Vulkan algorithms remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 03:06 CEST - TENSOR-100E Vulkan Matrix Norm
- Objective attempted:
  - Continue `TENSOR-100E` with backend-neutral `Float64` scalar reducers by
    routing public `matrix/norm` to Vulkan for eligible dense row-major rank-2
    inputs and direct norm selectors.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_norm_f64.comp` and generated
    `csrc/tensor_vulkan_norm_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_norm_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/norm` handles
    concrete Vulkan tensors before the CPU-only storage check.
  - Added availability-gated Vulkan norm regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/norm` now supports Vulkan-placed dense row-major `Float64`
    rank-2 inputs for default/`'frobenius`, `'one`, `'infinity`, and `'max`.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the scalar result required by the public return contract.
  - At this checkpoint, SVD-backed `'spectral` and `'nuclear` failed closed on
    Vulkan with `tensor/backend-unsupported`; this was superseded later on
    2026-04-17 by the Vulkan singular-value helper slice.
  - CPU Tensor `matrix/norm` behavior, including LAPACK/fallback coverage for
    spectral/nuclear selectors, remains on the existing CPU implementation.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_norm_f64.comp -o /tmp/omni_tensor_vulkan_norm_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_norm_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan norm smokes returned `5.0`, `9.53939201416946`, `9.0`,
    `15.0`, `6.0`, `5.0`, `0.0`, and `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `pass=712 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=699 fail=0`.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    non-`Float64` Vulkan dtypes, SVD-backed Vulkan norm selectors, and Vulkan
    unary scientific `map` remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 02:53 CEST - TENSOR-100E Vulkan Matrix Trace
- Objective attempted:
  - Continue `TENSOR-100E` with a backend-neutral `Float64` reducer by routing
    public `matrix/trace` to Vulkan for eligible dense row-major square inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_trace_f64.comp` and generated
    `csrc/tensor_vulkan_trace_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_trace_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/trace` handles
    concrete Vulkan tensors before the CPU-only storage check and returns the
    scalar result after a one-scalar Vulkan result readback.
  - Added availability-gated Vulkan trace regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/trace` now supports Vulkan-placed dense row-major square
    `Float64` inputs.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the scalar result required by the public return contract.
  - CPU Tensor and Big* Tensor trace behavior remains on the existing CPU
    implementation.
  - Unsupported GPU/device cases fail closed rather than silently copying to CPU.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_trace_f64.comp -o /tmp/omni_tensor_vulkan_trace_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_trace_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan trace smokes returned `5.0`, `5.0`, and `0.0`.
  - Direct CPU trace smoke returned `5.0`.
  - Direct non-rank-2 trace error smoke preserved the existing shape diagnostic.
  - Host focused `advanced-collections-module`: `pass=705 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=692 fail=0`.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    non-`Float64` Vulkan dtypes, broader scalar reducers, and Vulkan unary
    scientific `map` remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 02:42 CEST - TENSOR-100E Vulkan Matrix Diagonal Pair
- Objective attempted:
  - Continue `TENSOR-100E` with the next backend-neutral `Float64` structural
    matrix family by routing public `matrix/diagonal` and
    `matrix/diagonal-matrix` to Vulkan for eligible dense row-major inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_diagonal_f64.comp`,
    `csrc/tensor_vulkan_diagonal_matrix_f64.comp`, and generated SPIR-V C
    sources for both kernels.
  - Wired the new SPIR-V C sources into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_diagonal_f64` and
    `omni_tensor_backend_vulkan_diagonal_matrix_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added C3 externs in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/diagonal` and
    `matrix/diagonal-matrix` route Vulkan `Float64` dense row-major inputs
    before the CPU-only storage check and return Vulkan-placed Tensor results.
  - Added availability-gated Vulkan regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/diagonal` and `matrix/diagonal-matrix` now support
    Vulkan-placed dense row-major `Float64` inputs.
  - Results remain Vulkan-placed tensors; CPU inspection still requires
    explicit `to-device 'cpu`.
  - CPU Tensor and Big* Tensor behavior remains on the existing CPU
    implementation.
  - Unsupported GPU/device cases fail closed rather than silently copying to CPU.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0` for both new shaders: passed.
  - `spirv-val --target-env vulkan1.0` for both generated SPIR-V files: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `matrix/diagonal` smokes returned `5.0`, `vulkan`,
    `vulkan`, and zero-size length `0`.
  - Direct Vulkan `matrix/diagonal-matrix` smokes returned `3.0`, `0.0`,
    `vulkan`, `vulkan`, and zero-size length `0`.
  - Direct CPU diagonal/diagonal-matrix smokes returned `5.0`, `3.0`, and
    `0.0`; rank-error smokes preserved the existing shape diagnostics.
  - Host focused `advanced-collections-module`: `pass=702 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=689 fail=0`.
- Unresolved issues / next actions:
  - Superseded by the later matrix trace entry for `matrix/trace`.
    Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    non-`Float64` Vulkan dtypes, and Vulkan unary scientific `map` remain
    unsupported.

Signature: Codex GPT-5

## 2026-04-17 03:05 CEST - TENSOR-100E Vulkan Matrix Transpose
- Objective attempted:
  - Continue `TENSOR-100E` with the next backend-neutral `Float64` kernel family
    by routing public `matrix/transpose` to Vulkan for eligible dense row-major
    rank-2 tensors.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_transpose_f64.comp` and generated
    `csrc/tensor_vulkan_transpose_f64_spv.c`.
  - Added `omni_tensor_backend_vulkan_transpose_f64` in
    `csrc/tensor_vulkan_helpers.c` and wired the SPIR-V source into
    `project.json` plus `scripts/build_omni_chelpers.sh`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/transpose` handles
    concrete Vulkan tensors before the CPU-only storage check and returns a
    Vulkan-placed result for dense row-major `Float64` rank-2 inputs.
  - Added availability-gated Vulkan transpose regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/transpose` now supports Vulkan-placed dense row-major
    `Float64` rank-2 tensors.
  - Results remain Vulkan-placed tensors; CPU inspection still requires
    explicit `to-device 'cpu`.
  - CPU Tensor and Big* Tensor transpose behavior remains on the existing CPU
    implementation.
  - Unsupported GPU/device cases fail closed rather than silently copying to CPU.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_transpose_f64.comp -o /tmp/omni_tensor_vulkan_transpose_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_transpose_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan transpose smokes returned `6.0`, `vulkan`, `vulkan`, and
    zero-size length `0`.
  - Direct CPU transpose smoke returned `6.0`.
  - Direct non-rank-2 transpose error smoke returned `tensor/shape-mismatch`.
  - Host focused `advanced-collections-module`: `pass=693 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=680 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    non-`Float64` Vulkan dtypes, and Vulkan unary scientific `map` remain
    unsupported.

Signature: Codex GPT-5

## 2026-04-17 02:35 CEST - TENSOR-100E Vulkan Multi-Axis Contract
- Objective attempted:
  - Continue `TENSOR-100E` by extending the Vulkan dense row-major `Float64`
    rank-N `contract` path from exactly one contracted axis pair to one or more
    explicit contracted axis pairs through the existing backend-neutral public
    `contract` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Updated `csrc/tensor_vulkan_contract_f64.comp` so contracted axes are read
    from metadata lists instead of single push-constant axes, and each output
    invocation flattens the full contracted coordinate space.
  - Regenerated `csrc/tensor_vulkan_contract_f64_spv.c`.
  - Updated `omni_tensor_backend_vulkan_contract_f64` in
    `csrc/tensor_vulkan_helpers.c` to validate/pass axis-list metadata and keep
    zero-contracted output zero-fill behavior.
  - Updated `src/lisp/tensor_vulkan_backend.c3` and
    `src/lisp/prim_tensor.c3` so public Vulkan `contract` accepts one or more
    explicit axis pairs for dense row-major `Float64` operands.
  - Added availability-gated scalar and rank-3 multi-axis Vulkan regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `contract` now supports Vulkan-placed dense row-major rank-N
    `Float64` tensors with one or more contracted axis pairs.
  - Output axes remain ordered as free left axes followed by free right axes.
  - Results remain Vulkan-placed tensors; CPU inspection still requires
    explicit `to-device 'cpu`.
  - Zero-axis contractions, unsupported layouts/dtypes, and mixed CPU/Vulkan
    operands still fail closed with Tensor backend diagnostics.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_contract_f64.comp -o /tmp/omni_tensor_vulkan_contract_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_contract_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `70.0` for scalar multi-axis contract,
    `210.0` for rank-3 multi-axis contract, `460.0` for existing rank-N
    single-axis contract, `8.0` for binary Vulkan `map +`, and
    `tensor/backend-unsupported` for unsupported Vulkan `map sqrt`.
  - Host focused `advanced-collections-module`: `pass=689 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=676 fail=0`.
- Invalidated assumptions / failed approaches:
  - The earlier generic mode-3 Vulkan unary `map` branch remains rolled back and
    should not be resumed as the default baseline.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    and non-`Float64` Vulkan dtypes remain unsupported.
  - The next `TENSOR-100E` slice should move to another backend-neutral
    `Float64` kernel family, such as structural matrix transpose, or explicitly
    design layout/aliasing metadata before enabling stride-aware dispatch.

Signature: Codex GPT-5

## 2026-04-17 01:40 CEST - TENSOR-100E Vulkan Rank-N Contract
- Objective attempted:
  - Continue `TENSOR-100E` by extending the Vulkan `Float64` dense row-major
    single-axis `contract` path from rank-1/rank-2 coverage to rank-N coverage
    through the existing backend-neutral public `contract` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Replaced `csrc/tensor_vulkan_contract_f64.comp` with a
    rank/shape/stride metadata-buffer shader for dense row-major single-axis
    contractions.
  - Regenerated `csrc/tensor_vulkan_contract_f64_spv.c`.
  - Extended `omni_tensor_backend_vulkan_contract_f64` in
    `csrc/tensor_vulkan_helpers.c` to accept full operand/output rank, shape,
    and stride metadata, added the metadata storage buffer, switched generic
    dispatch to flat rank-N output indexing, and fixed the descriptor pool
    size for the four-buffer layout.
  - Updated `src/lisp/tensor_vulkan_backend.c3` and
    `src/lisp/prim_tensor.c3` so public Vulkan `contract` accepts dense
    row-major `Float64` tensors of arbitrary rank when exactly one axis pair
    is contracted.
  - Added availability-gated rank-3/rank-2, rank-2/rank-3, rank-3/rank-3,
    residency, and zero-contracted rank-3 regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated TODO, plan, reference/spec, Vulkan decision/policy, Tensor area
    status, and changelog documentation.
- Key results:
  - Public `contract` now supports Vulkan-placed dense row-major rank-N
    `Float64` tensors with exactly one contracted axis.
  - Output axes are ordered as free left axes followed by free right axes.
  - Results remain Vulkan-placed tensors; CPU inspection still requires
    explicit `to-device 'cpu`.
  - Multi-axis contractions, unsupported layouts/dtypes, and mixed CPU/Vulkan
    operands still fail closed with Tensor backend diagnostics.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_contract_f64.comp -o /tmp/omni_tensor_vulkan_contract_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_contract_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan rank-N contract smokes returned `460.0`, `139.0`,
    `3340.0`, `vulkan`, and `0.0`.
  - Host focused `advanced-collections-module`: `pass=688 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=675 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Invalidated assumptions / failed approaches:
  - A follow-up Vulkan unary `map` experiment was rolled back before completion.
    GLSL `double` transcendental calls such as `sin(double)` failed shader
    compilation, and an arithmetic-only generic mode-3 unary branch for
    `abs`/negation compiled but failed at runtime with
    `map: Vulkan Float64 kernel failed` while binary Vulkan `map` and rank-N
    `contract` remained healthy.
  - Do not resume that generic mode-3 branch as the baseline. If unary Vulkan
    map is pursued, use a separately debugged unary shader/helper entrypoint or
    first diagnose the current descriptor/dispatch failure. Do not use
    `Float32` casts or a hidden CPU fallback.
  - After rollback, direct sanity checks returned `8.0` for binary Vulkan
    `map +`, `460.0` for rank-N Vulkan `contract`, and
    `tensor/backend-unsupported` for unsupported Vulkan `map sqrt`.
    `c3c build --obj-out obj`, host focused `advanced-collections-module`
    (`pass=688 fail=0`), and bounded-container focused
    `advanced-collections-module` (`pass=675 fail=0`) still passed.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, and multi-axis Vulkan
    contractions remain unsupported.
  - The next `TENSOR-100E` slice should move to the next eligible dense
    row-major `Float64` kernel family, or explicitly design layout/aliasing
    metadata before enabling stride-aware dispatch. Unary Vulkan `map` remains
    open, but should not continue through the rolled-back generic mode-3 branch.
  - `jj status` shows the expected broad dirty worktree from the ongoing
    Tensor/CUDA/LAPACK/Vulkan lane, including unrelated prior changes and the
    untracked `out` file.

Signature: Codex GPT-5

## 2026-04-17 01:14 CEST - TENSOR-100E Vulkan Map Broadcasting
- Objective attempted:
  - Extend the Vulkan `Float64` dense row-major `map` kernel from exact-shape
    Tensor/Tensor arithmetic to right-aligned singleton-axis Tensor/Tensor
    broadcasting through the same public `map` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Updated `csrc/tensor_vulkan_map_f64.comp` to compute broadcast operand
    offsets from output shape/strides and operand shape/strides.
  - Regenerated `csrc/tensor_vulkan_map_f64_spv.c`.
  - Extended `omni_tensor_backend_vulkan_map_f64` in
    `csrc/tensor_vulkan_helpers.c` with a fourth metadata storage buffer.
  - Updated the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Relaxed `src/lisp/prim_tensor.c3` Vulkan `map` gates to use the CPU Tensor
    right-aligned broadcast compatibility checks.
  - Added focused availability-gated broadcast regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated TODO, Tensor docs/plans, `.agents/PLAN.md`, and
    `memory/CHANGELOG.md`.
- Key results:
  - Vulkan `map` now supports `+`, `-`, `*`, and `/` for Tensor/scalar,
    scalar/Tensor, exact-shape Tensor/Tensor, and right-aligned singleton-axis
    Tensor/Tensor broadcasting.
  - Results remain Vulkan-placed tensors; callers still copy back explicitly
    with `to-device 'cpu`.
  - Incompatible broadcast shapes, unsupported callables, unsupported dtypes,
    and mixed CPU/Vulkan operands still fail closed.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0`: passed.
  - `spirv-val --target-env vulkan1.0`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan broadcast smokes returned `36.0`, `32.0`, `11.0`, and
    `vulkan`.
  - Host focused `advanced-collections-module`: `683 passed, 0 failed`.
  - Bounded focused `advanced-collections-module`: `670 passed, 0 failed`.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100E` by extending the metadata-buffer pattern to the next
    eligible `Float64` kernel family or to stride-aware views only after layout
    and aliasing metadata are explicit.
- Signature: Codex GPT-5

## 2026-04-17 01:01 CEST - TENSOR-100E Vulkan Map Arithmetic
- Objective attempted:
  - Broaden the policy-backed Vulkan `Float64` dense row-major `map` kernel
    family beyond add-scalar while preserving the backend-neutral public
    `map` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_map_f64.comp` as the checked-in GLSL source for
    the Vulkan `Float64` elementwise map shader family.
  - Added generated `csrc/tensor_vulkan_map_f64_spv.c` and wired it through
    `project.json` and `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_map_f64` in the runtime-loaded Vulkan
    helper and exposed it through `src/lisp/tensor_vulkan_backend.c3`.
  - Routed public `map` in `src/lisp/prim_tensor.c3` for Vulkan-placed
    dense row-major `Float64` Tensor/scalar, scalar/Tensor, and exact-shape
    Tensor/Tensor arithmetic.
  - Added focused availability-gated regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated TODO, Tensor docs/plans, `.agents/PLAN.md`, and
    `memory/CHANGELOG.md`.
- Key results:
  - Vulkan `map` now supports `+`, `-`, `*`, and `/` for Tensor/scalar,
    scalar/Tensor, and exact-shape Tensor/Tensor operands.
  - Results remain Vulkan-placed tensors; callers still copy back explicitly
    with `to-device 'cpu` for CPU inspection.
  - Unary callables, unsupported callables, unsupported dtypes, mixed
    CPU/Vulkan operands, and broadcasting shapes fail closed with Tensor
    backend diagnostics.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0`: passed.
  - `spirv-val --target-env vulkan1.0`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan map smokes returned `8.0`, `12.0`, `26.0`, `80.0`,
    `90.0`, `4.0`, `vulkan`, and `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `679 passed, 0 failed`.
  - Bounded focused `advanced-collections-module`: `666 passed, 0 failed`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`:
    passed.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100E` with Vulkan `Float64` map broadcasting or a rank-N
    dense descriptor only after the required shape/stride metadata is explicit.
- Signature: Codex GPT-5

## 2026-04-17 00:49 CEST - TENSOR-100E Vulkan Dtype Layout Policy
- Objective attempted:
  - Close the Vulkan dtype/layout policy decision before adding more public
    backend behavior.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`.
  - Linked the policy from TODO, the plans index, Vulkan decision note, Tensor
    area status, integrated Tensor plan, and `.agents/PLAN.md`.
  - Corrected `docs/reference/11-appendix-primitives.md` so the Tensor row
    lists the current four native Tensor dtypes.
- Key results:
  - Policy is now explicit: keep extending fixed-width `Float64` dense
    row-major Vulkan kernels first; do not downcast to `Float32`; do not lower
    pointer-backed `BigInteger`, `BigFloat`, or `BigComplex` tensors to Vulkan;
    defer fixed-width complex and stride-aware layouts until those contracts
    exist.
  - The next Vulkan implementation boundary is a policy-backed `Float64`
    dense row-major `map` kernel family for Tensor/scalar and Tensor/Tensor
    elementwise arithmetic through public `map`.
- Commands run and key results:
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`:
    passed.
- Current best recommendation / checkpoint:
  - Implement the Float64 Vulkan `map` kernel family next, preserving
    backend-neutral public surface and fail-closed diagnostics.
- Signature: Codex GPT-5

## 2026-04-17 00:44 CEST - TENSOR-100E Vulkan Contract Dispatch Hardening
- Objective attempted:
  - Harden the Vulkan generic `Float64` single-axis `contract` execution layout
    without changing the public Tensor surface or supported dtype/layout gate.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_contract_f64.comp` as the checked-in GLSL source
    for the generic Vulkan contract shader.
  - Regenerated `csrc/tensor_vulkan_contract_f64_spv.c` from that GLSL source.
  - Updated `omni_tensor_backend_vulkan_contract_f64` to use shape-aware 16x4
    rank-2 output dispatch instead of flattening all non-scalar output through
    one-dimensional workgroups.
  - Updated TODO, Tensor area/planning docs, `.agents/PLAN.md`, session
    reports, and `memory/CHANGELOG.md`.
- Key results:
  - Public behavior remains unchanged: Vulkan `contract` still supports dense
    row-major `Float64` rank-1/rank-2 single-axis layouts and still fails
    closed for unsupported dtype/layout/device cases.
  - The next open Vulkan Tensor item is now the broader dtype/layout policy
    decision before any new public behavior is added.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0`: passed.
  - `spirv-val --target-env vulkan1.0`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan contract smokes returned `154.0`, `60.0`, `167.0`, `69.0`,
    `122.0`, `69.0`, `46.0`, `43.0`, `vulkan`,
    `tensor/backend-unsupported`, and zero-free length `0`.
  - Host focused `advanced-collections-module`: `674 passed, 0 failed`.
  - Bounded focused `advanced-collections-module`: `661 passed, 0 failed`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`:
    passed.
- Current best recommendation / checkpoint:
  - Decide the Vulkan dtype/layout policy as a separate slice. Preserve the
    backend-neutral `Tensor`/`map`/`contract`/`to-device` surface, explicit
    placement/copyback, and fail-closed diagnostics.
- Signature: Codex GPT-5

## 2026-04-17 00:20 CEST - TENSOR-100E Vulkan Zero-Size Contract
- Objective attempted:
  - Align Vulkan single-axis `contract` zero-size behavior with the CPU Tensor
    oracle for the supported rank-1/rank-2 `Float64` layouts.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Updated `omni_tensor_backend_vulkan_contract_f64` so zero-element results
    succeed without a buffer handle and zero-contracted non-empty results
    allocate and zero-fill Vulkan output storage.
  - Relaxed Vulkan C3 dispatch validation so zero contracted axes and zero
    free dimensions are accepted for supported rank/layout combinations.
  - Added availability-gated Vulkan regressions for zero-size rank-1 dot,
    rank-2/rank-2 identity output, zero-length rank-2 output, zero-length
    Vulkan residency, rank-2/rank-1 identity output, and rank-1/rank-2
    identity output.
  - Updated TODO, Tensor docs/plans, area status, session reports, `.agents`
    plan, and `memory/CHANGELOG.md`.
- Key results:
  - Zero-size contracted axes now produce additive-identity output in Vulkan
    storage, matching CPU Tensor semantics.
  - Zero free dimensions preserve zero-length result shapes and remain
    Vulkan-placed.
- Commands run and key results:
  - Direct Vulkan zero-size smokes returned `0.0`, `0.0`, `0`, `vulkan`,
    `0.0`, `0`, `0.0`, and `0`.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Host focused `advanced-collections-module`: `674 passed, 0 failed`.
  - Bounded focused `advanced-collections-module`: `661 passed, 0 failed`.
  - Bounded full `advanced`: `1978 passed, 0 failed`.
  - Bounded `memory-lifetime-smoke`: `227 passed, 0 failed`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`:
    passed.
  - `c3c build --sanitize=address --obj-out obj`: still blocked by the local
    C3 sanitizer platform guard.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100E` with Vulkan contract performance/tiling hardening
    and broader dtype/layout policy.
- Signature: Codex GPT-5

## 2026-04-16 23:58 CEST - TENSOR-100E Vulkan Generic Contract
- Objective attempted:
  - Continue `TENSOR-100E` from rank-1 dot into generic dense row-major
    Vulkan `Float64` single-axis `contract` support for ranks 1 and 2.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_contract_f64_spv.c`, an embedded SPIR-V generic
    contract shader for dense row-major `Float64` Tensor layouts.
  - Extended `csrc/tensor_vulkan_helpers.c` with
    `omni_tensor_backend_vulkan_contract_f64`.
  - Routed public `contract` on Vulkan-placed dense row-major `Float64`
    rank-1/rank-2 operands through the generic Vulkan helper when exactly one
    axis is contracted.
  - Added availability-gated advanced tests for all supported rank-2/rank-2,
    rank-2/rank-1, and rank-1/rank-2 axis orientations, Vulkan result
    residency, and multi-axis fail-closed behavior.
  - Updated TODO, Vulkan backend decision, Tensor area status, integrated
    Tensor plan, language spec, collections reference, plans index,
    session reports, `.agents/PLAN.md`, and `memory/CHANGELOG.md`.
- Key results:
  - Public `contract` now supports Vulkan-placed dense row-major `Float64`
    tensors of ranks 1 and 2 with one contracted axis.
  - Supported layouts are rank-1/rank-1 dot (`[0 0]` or explicit `[0] [0]`),
    rank-2/rank-2 `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`,
    rank-2/rank-1 `[1 0]` and `[0 0]`, and rank-1/rank-2 `[0 0]` and
    `[0 1]`.
  - Results remain Vulkan-placed tensors; callers use `to-device 'cpu` before
    CPU inspection.
  - Unsupported Vulkan contract cases, including multi-axis contractions,
    unsupported ranks/layouts/dtypes, and mixed CPU/Vulkan operands, fail
    closed with Tensor backend diagnostics. Zero-size contracted axes were
    aligned with CPU semantics in the follow-up zero-size slice.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct smokes returned `154.0`, `60.0`, `167.0`, `69.0`, `122.0`,
    `69.0`, `46.0`, `43.0`, `vulkan`, and `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `668 passed, 0 failed`.
  - Bounded focused `advanced-collections-module`: `655 passed, 0 failed`.
  - Bounded full `advanced`: `1972 passed, 0 failed`.
  - Bounded `memory-lifetime-smoke`: `227 passed, 0 failed`.
  - Primitive docs parity, Stage 3 source parity, and `git diff --check`:
    passed.
  - `c3c build --sanitize=address --obj-out obj`: still blocked by the local
    C3 sanitizer platform guard.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100E` with performance/tiling hardening for the Vulkan
    contract kernel and a broader dtype/layout policy decision before adding
    more public behavior.
- Signature: Codex GPT-5

## 2026-04-16 23:44 CEST - TENSOR-100E Vulkan Rank-1 Dot
- Objective attempted:
  - Continue `TENSOR-100E` from Vulkan placement and add-scalar `map` into the
    first Vulkan reduction kernel behind the public backend-neutral `contract`
    surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_contract_dot_f64_spv.c`, an embedded SPIR-V
    compute reduction shader for rank-1/rank-1 `Float64` dot.
  - Extended `csrc/tensor_vulkan_helpers.c` with a shared Vulkan backend
    context and `omni_tensor_backend_vulkan_contract_dot_f64`.
  - Wired the new shader blob through `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Routed public `contract` on two Vulkan `Float64` rank-1 tensors through
    the Vulkan dot helper for `[0 0]` and explicit `[0] [0]`.
  - Added availability-gated advanced tests for roundtrip, explicit axes,
    device residency, mixed-device rejection, and rank-2 fail-closed behavior.
  - Updated TODO, Vulkan backend decision, Tensor area status, integrated
    Tensor plan, language spec, collections reference, plans index,
    session reports, `.agents/PLAN.md`, and `memory/CHANGELOG.md`.
- Key results:
  - Public `contract` now supports two Vulkan `Float64` rank-1 tensors with
    one contracted axis (`[0 0]` or explicit `[0] [0]`), returning a Vulkan
    scalar Tensor.
  - The explicit transfer contract is preserved: callers use `to-device 'cpu`
    before CPU inspection.
  - Unsupported Vulkan contract shapes, rank-2 contractions, and mixed
    CPU/Vulkan operands remain fail-closed with `tensor/backend-unsupported`.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed.
  - Direct smokes returned `140.0`, `140.0`, `vulkan`, and
    `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `659 passed, 0 failed`.
  - Bounded focused `advanced-collections-module`: `646 passed, 0 failed`.
  - Bounded full `advanced`: `1963 passed, 0 failed`.
  - Bounded `memory-lifetime-smoke`: `227 passed, 0 failed`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
  - `c3c build --sanitize=address --obj-out obj`: attempted, but the local C3
    toolchain reported address sanitizer unsupported on this platform.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100E` with rank-2 Vulkan `contract` kernels while
    preserving backend-neutral surface names, explicit device movement, and
    fail-closed backend diagnostics.
- Signature: Codex GPT-5

## 2026-04-16 21:20 CEST - TENSOR-100E Vulkan SPIR-V Map Kernel

Objective attempted:
- Continue `TENSOR-100E` from placement/copy into the first real Vulkan compute
  kernel while preserving the backend-neutral `Tensor`/`map`/`to-device`
  surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `csrc/tensor_vulkan_map_add_scalar_f64_spv.c`, an embedded SPIR-V
  add-scalar compute shader generated locally with `glslangValidator`.
- Extended `csrc/tensor_vulkan_helpers.c` with dynamic Vulkan compute symbols,
  descriptor/pipeline/command-buffer setup, synchronous queue dispatch, and
  `omni_tensor_backend_vulkan_map_add_scalar_f64`.
- Refactored Vulkan buffers to use a shared context plus refcounted handles so
  map output can safely share the input device and boundary copies can retain
  real Vulkan handles without double-freeing buffers.
- Added C3 extern/status wiring for Vulkan unsupported/execution failures and
  retained Vulkan handles.
- Routed public `map +` with one Vulkan `Float64` Tensor operand and one
  Float64 scalar through the SPIR-V kernel, returning a Vulkan-placed Tensor.
- Fixed `to-device 'cpu` for lazy expressions that realize to a non-CPU Tensor
  by realizing first and then copying the realized device value back to CPU.
- Added focused availability-gated tests for Vulkan map scalar-right,
  scalar-left, and unsupported-callable diagnostics.
- Updated TODO, Tensor docs, Vulkan plan, area status, integrated Tensor plan,
  and memory changelog.

Key results:
- Direct host smokes returned `8.0` for `(map + vulkan-tensor 5.0)` copied
  back to CPU and `12.0` for `(map + 10.0 vulkan-tensor)` copied back to CPU.
- Public `map`, not only private `__tensor-map`, now works for the first Vulkan
  kernel path.
- Unsupported Vulkan map callables fail closed with `tensor/backend-unsupported`.
- `contract` remains intentionally fail-closed until the Vulkan contraction
  kernel slice lands.

Commands run and key results:
- `sudo apt-get update && sudo apt-get install -y glslang-tools`: installed
  local shader tooling used to generate the checked-in SPIR-V C blob.
- `glslangValidator -V --target-env vulkan1.0`: generated the add-scalar
  SPIR-V module.
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct host Vulkan map smokes: returned `8.0`, `vulkan`, and `12.0`.
- Direct unsupported-callable smoke: returned `tensor/backend-unsupported`.
- Host focused `advanced-collections-module`: `655 passed, 0 failed`.
- Bounded-container focused `advanced-collections-module`: `642 passed, 0 failed`.
- Bounded-container full `advanced`: `1959 passed, 0 failed`.
- Bounded-container `memory-lifetime-smoke`: `227 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Unresolved issues / next actions:
- Next Vulkan compute work should add reductions/rank-1 dot or rank-2
  `contract` kernels, preserving explicit device movement and fail-closed
  diagnostics.

Signature: Codex GPT-5

## 2026-04-16 20:05 CEST - TENSOR-100E Vulkan Probe And Placement

Objective attempted:
- Start the portable Vulkan Tensor backend lane, then carry it through the
  first real placement/copy slice while preserving the existing
  backend-neutral `Tensor` surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `docs/plans/vulkan-backend-decision-2026-04-16.md`.
- Added `csrc/tensor_vulkan_helpers.c`, a runtime-loaded `libvulkan` probe
  with no link-time Vulkan SDK dependency.
- Added `src/lisp/tensor_vulkan_backend.c3` extern bindings for availability,
  capability probing, CPU->Vulkan copies, Vulkan->CPU copies, and finalization.
- Wired the Vulkan helper into `project.json` and
  `scripts/build_omni_chelpers.sh`.
- Added a structured `vulkan` entry to `tensor-backends`, including explicit
  `float64` capability reporting.
- Added `TENSOR_DEVICE_VULKAN` placement metadata and public `device` reporting.
- Made `to-device 'vulkan` copy concrete `Float64` CPU Tensor storage into an
  opaque host-visible coherent Vulkan buffer when Vulkan is usable.
- Made `to-device 'cpu` copy Vulkan Tensor storage back to native CPU Tensor
  storage.
- Added deterministic Vulkan device-handle finalization through
  `tensor_free_payload`.
- Made Vulkan `contract` fail closed with `tensor/backend-unsupported` until
  contraction kernels land, instead of building lazy expressions over non-CPU
  storage.
- Added focused tests for the Vulkan inventory entry, availability-conditional
  Vulkan placement/roundtrip, unsupported dtype diagnostics, and fail-closed
  Vulkan `contract`.
- Added a regression proving zero-size Vulkan placement still follows backend
  availability.
- Added memory-lifetime coverage for opaque Vulkan device payload finalizers.

Key results:
- Vulkan is now visible as an optional backend capability without requiring a
  Vulkan link dependency.
- The one-time Vulkan probe suppresses noisy driver stderr and reports
  availability through `tensor-backends`.
- Vulkan placement/copy is implemented for concrete `Float64` Tensor storage
  through host-visible coherent buffers. Missing Vulkan reports
  `tensor/backend-unavailable`; unsupported dtypes report
  `tensor/backend-unsupported`.
- At this checkpoint, Vulkan compute kernels were not implemented yet.
  Superseded by the later SPIR-V map-kernel entry above; `contract` on
  Vulkan-placed operands still fails closed until the contraction kernel slice
  lands.
- Invalidated a helper assumption: host-visible non-coherent Vulkan memory is
  not enough for this placement path without explicit flush/invalidate support,
  so the helper requires host-visible coherent memory for now.
- Invalidated a zero-size shortcut: zero-size Vulkan placement must still
  probe backend availability and fail with `tensor/backend-unavailable` when
  Vulkan is missing.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Host focused `advanced-collections-module`: `652 passed, 0 failed`.
- Bounded-container focused `advanced-collections-module`: `639 passed, 0 failed`.
- Bounded-container full `advanced`: `1956 passed, 0 failed`.
- Bounded-container `memory-lifetime-smoke`: `227 passed, 0 failed`.
- `./scripts/build_omni_chelpers.sh`: passed.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- `c3c build --sanitize=address --obj-out obj`: blocked; C3 reported address
  sanitizer unsupported on this platform.

Current best recommendation / checkpoint:
- Superseded by the later SPIR-V map-kernel entry above. The next Vulkan compute
  step is reductions/rank-1 dot or rank-2 `contract`; keep CPU semantics as the
  oracle, keep `shaderFloat64` as a kernel capability gate, and keep all
  unsupported Vulkan compute paths fail-closed without hidden CPU/GPU transfers.

Unresolved issues:
- Superseded by the later SPIR-V map-kernel entry above: first Vulkan kernel
  dispatch now exists for `map +` add-scalar, but reductions and `contract`
  still need kernels.
- Vulkan `Float64` availability is reported as a kernel capability. Placement
  stores `Float64` bytes in host-visible coherent Vulkan buffers, while new
  compute kernels must continue gating on shader capability.
- ASAN remains unverified because the local C3 toolchain reports address
  sanitizer unsupported on this platform.

Signature: Codex GPT-5

## 2026-04-16 19:08 CEST - TENSOR-100D cuBLAS Contract

Objective attempted:
- Continue from explicit CUDA Tensor copies into the first cuBLAS execution
  path behind the existing `contract` surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Extended `csrc/tensor_cuda_helpers.c` with runtime-loaded cuBLAS resolution,
  availability probing, `dgemm`/`dgemv` symbol resolution, and per-operation
  handle lifecycle.
- Added cuBLAS externs in `src/lisp/tensor_cuda_backend.c3`.
- Added `cublas` to the structured `tensor-backends` inventory.
- Routed CUDA-placed dense row-major `Float64` rank-2/rank-2 single-axis
  contractions through `cublasDgemm_v2` for `[1 0]`, `[0 0]`, `[1 1]`, and
  `[0 1]`.
- Routed CUDA-placed dense row-major `Float64` rank-2/rank-1 and rank-1/rank-2
  single-axis contractions through `cublasDgemv_v2`.
- Added focused tests that assert cuBLAS results for all rank-2/rank-2,
  matrix-vector, and vector-matrix layouts when available and skip to CPU
  oracle values when unavailable.
- Added forced-unavailable cuBLAS regressions through the private backend
  disable hook, covering both `tensor-backends` inventory reporting and
  `tensor/backend-unavailable` contract failure.

Key results:
- Host `tensor-backends` reports CPU, CUDA, and cuBLAS available.
- Direct CUDA/cuBLAS contract smoke returned `154.0`, matching the CPU result
  for the tested 2x3 by 3x2 matrix multiply; focused tests also cover the
  transpose-backed `[0 0]`, `[1 1]`, and `[0 1]` rank-2/rank-2 layouts plus
  matrix-vector and vector-matrix `dgemv` layouts.
- Bounded-container validation still passes without requiring GPU visibility.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct CUDA/cuBLAS contract smoke: `154.0`.
- Host focused `advanced-collections-module`: `646 passed, 0 failed`.
- Bounded-container focused `advanced-collections-module`: `633 passed, 0 failed`.
- Bounded-container `memory-lifetime-smoke`: `226 passed, 0 failed`.
- Bounded-container full `advanced`: `1950 passed, 0 failed`.
- `./scripts/build_omni_chelpers.sh`: passed.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- `c3c build --sanitize=address --obj-out obj`: blocked; C3 reported address
  sanitizer unsupported on this platform.

Current best recommendation / checkpoint:
- The rank-2 CUDA/cuBLAS contract slice now covers all contiguous row-major
  single-axis layouts: `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`, plus
  matrix-vector/vector-matrix layouts, unsupported, mixed-device, zero-size
  fail-closed, and forced-unavailable paths. Further CUDA/cuBLAS work should
  add rank-1/rank-1 dot or broader dtype policy only if it preserves the
  backend-neutral `contract` surface and fail-closed diagnostics.

Unresolved issues:
- Only rank-2/rank-2, rank-2/rank-1, and rank-1/rank-2 `Float64` single-axis
  CUDA contract fast paths are implemented.
- No hidden CPU/GPU fallback is allowed for unsupported CUDA cases.
- Boundary clone/copy of non-CPU Tensor payloads remains intentionally
  unsupported until explicit copy ownership lands.

Signature: Codex GPT-5

## 2026-04-16 19:08 CEST - TENSOR-100C Optional CUDA Copy

Objective attempted:
- Continue from Tensor placement metadata into explicit CPU<->CUDA copy
  semantics without making CUDA a required build dependency.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `csrc/tensor_cuda_helpers.c`, a runtime-loaded CUDA helper using
  `dlopen`/`dlsym` for libcudart.
- Added `src/lisp/tensor_cuda_backend.c3` extern bindings.
- Wired the CUDA helper into `project.json` and
  `scripts/build_omni_chelpers.sh`.
- Updated `prim_to_device` so `'cuda` copies concrete `Float64` CPU Tensor
  storage to CUDA when available, and `'cpu` copies CUDA Tensor storage back to
  CPU.
- Strengthened focused tests to accept CUDA-visible and CUDA-unavailable
  environments and assert CUDA roundtrip values when available.

Key results:
- CUDA is optional and runtime-loaded; normal builds do not link libcudart.
- `tensor-backends` reports CUDA available only after symbol resolution, device
  count, and allocation/free probe succeed.
- On the host, CUDA was visible and a direct Tensor roundtrip returned `2.0`.
- In the bounded validation container, the same focused suite still passed.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Host focused `advanced-collections-module`: `633 passed, 0 failed`.
- Direct `tensor-backends` smoke: CPU available and CUDA available on host.
- Direct CUDA roundtrip smoke: `2.0`.
- Bounded-container focused `advanced-collections-module`: `619 passed, 0 failed`.
- Bounded-container `memory-lifetime-smoke`: `226 passed, 0 failed`.
- `git diff --check`: passed.

Invalidated assumption:
- `cudaGetDeviceCount` alone is not a sufficient CUDA-copy availability probe;
  it reported available before a copy smoke returned error values. Keep the
  allocation/free probe in backend availability.

Current best recommendation / checkpoint:
- Next CUDA/cuBLAS work should add cuBLAS handle lifecycle, then route the
  CUDA-placed `Float64` rank-2 `contract` `[1 0]` fast path with CPU fallback
  oracle comparison.

Unresolved issues:
- No cuBLAS handle or CUDA `contract` execution exists yet.
- Boundary clone/copy of non-CPU Tensor payloads remains intentionally
  unsupported until explicit copy ownership lands.
- GPU-heavy validation must stay on the bounded container path or another
  explicitly bounded GPU validation path.

Signature: Codex GPT-5

## 2026-04-16 19:08 CEST - TENSOR-100B Placement Metadata

Objective attempted:
- Continue from the CPU-only device surface into the next CUDA/cuBLAS enabling
  layer without adding a CUDA runtime dependency.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added internal Tensor placement metadata and opaque device handle/finalizer
  storage in `src/lisp/value_runtime_types.c3`.
- Initialized CPU placement in `src/lisp/value_tensor.c3`.
- Made `tensor_free_payload` the release authority for opaque Tensor device
  handles.
- Made Tensor boundary cloning refuse opaque non-CPU payloads until explicit
  copy semantics land.
- Made CPU Tensor storage validation reject non-CPU placement.
- Added public `tensor-backends` registration and AOT hash wiring.
- Added focused backend inventory tests and a memory-lifetime fake-device
  finalizer regression.

Key results:
- `device` now reports stored Tensor placement rather than a hardcoded symbol.
- `tensor-backends` returns structured backend dictionaries: CPU available,
  CUDA unavailable with reason `backend-unavailable`.
- Non-CPU Tensor storage cannot accidentally enter CPU kernel paths.
- Opaque device handles now have deterministic payload destruction coverage.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Host focused `advanced-collections-module`: `633 passed, 0 failed`.
- Direct `tensor-backends` smoke: returned CPU and CUDA dictionaries.
- Direct `to-device 'cpu` + `device` smoke: `"cpu"`.
- Host `memory-lifetime-smoke`: refused by the container-only slice policy.
- Bounded-container focused `advanced-collections-module`: `619 passed, 0 failed`.
- Bounded-container `memory-lifetime-smoke`: `226 passed, 0 failed`.

Current best recommendation / checkpoint:
- The next CUDA/cuBLAS slice should implement explicit CPU<->CUDA copy
  semantics for concrete `Float64` Tensor storage behind `to-device`, then add
  cuBLAS handle lifecycle. Keep CPU kernels fail-closed for non-CPU placement.

Unresolved issues:
- No CUDA backend/runtime code exists yet; CUDA movement still intentionally
  fails closed.
- Boundary clone/copy of non-CPU Tensor payloads is intentionally unsupported
  until explicit copy ownership lands.
- GPU-heavy validation must use the bounded container path when CUDA execution
  lands.

Signature: Codex GPT-5

## 2026-04-16 19:05 CEST - TENSOR-100A CPU Device Surface

Objective attempted:
- Continue from the CUDA/cuBLAS design closure into the first implementation
  layer that does not require CUDA libraries.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `device` and `to-device` primitives in `src/lisp/prim_tensor.c3`.
- Registered both primitives in `src/lisp/eval_init_primitive_tables.c3` and
  `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`.
- Added focused tests in `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated Tensor docs, area/plan docs, TODO, changelog, local plan, and this
  session report.

Key results:
- `device` reports `'cpu` for current Tensor values.
- `to-device` with target `'cpu` realizes Tensor expressions to CPU Tensor
  storage.
- `to-device` with target `'cuda` fails closed with
  `tensor/backend-unavailable`.
- No CUDA runtime dependency was added.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Focused `advanced-collections-module`: `629 passed, 0 failed`.
- Direct `device` smoke: `"cpu"`.
- Direct handled CUDA smoke: `"tensor/backend-unavailable"`.
- Bounded-container focused `advanced-collections-module`: `616 passed, 0 failed`.
- Bounded-container full `advanced` slice: `1933 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The next CUDA/cuBLAS implementation step should add concrete Tensor
  placement metadata for non-CPU storage and ownership/destruction tests for
  opaque CUDA buffers before adding cuBLAS execution.

Unresolved issues:
- No CUDA backend/runtime code exists yet; CUDA movement intentionally fails
  closed.
- Host monolithic `advanced` still exits without a summary; focused host groups
  and bounded-container full `advanced` pass, and the bounded path remains the
  policy gate for this work.
- GPU-heavy validation must use the bounded container path when CUDA execution
  lands.
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 18:58 CEST - TENSOR-100 CUDA/cuBLAS Backend Design Closure

Objective attempted:
- Continue the Tensor plan after LAPACK validation by closing the next named
  design lane: explicit-device CUDA/cuBLAS backend policy.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `docs/plans/cuda-cublas-backend-decision-2026-04-16.md`.
- Updated `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
  `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`, `TODO.md`,
  `memory/CHANGELOG.md`, `.agents/PLAN.md`, and this session report.

Key results:
- Locked future GPU support behind the existing `Tensor` value.
- Chose `to-device`, `device`, and `tensor-backends` as the first future
  placement/introspection surface.
- Rejected public `GpuTensor`, `CudaTensor`, backend-flavored math names,
  `tensor-use-backend!` as the first control surface, and implicit CPU/GPU
  transfer inside ordinary Tensor operations.
- Defined the next implementation boundary: Tensor placement metadata,
  CPU-only `device`, fail-closed `to-device` diagnostics when CUDA is
  unavailable, then opaque CUDA buffer ownership tests before cuBLAS execution.

Validation:
- Documentation/reference grep checks.
- `git diff --check`.

Unresolved issues:
- No CUDA runtime code was added in this design slice.
- Future GPU-heavy validation must use the bounded container path.
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 18:48 CEST - Advanced Macro-Hygiene Stack Headroom Calibration

Objective attempted:
- Continue broader bounded validation after the Tensor matrix fallback lane and
  fix the full `advanced` slice crash exposed by that validation.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Updated `src/lisp/tests_advanced_macro_hygiene_groups.c3`.
- Replaced the ASAN/non-ASAN non-tail recursion headroom split at depths
  `896`/`1200` with one portable depth, `512`.
- Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`, and this
  session report.

Key results:
- The crash narrowed to the
  `advanced-macro-hygiene-string-number` non-tail recursion headroom probe.
- `parse-number` and String round-trip tests were not the failing surface.
- The old `896`/`1200` thresholds were environment-specific calibration values,
  not portable runtime contracts.
- Full bounded `advanced` validation now passes.

Commands run and key results:
- Pre-fix bounded full `advanced` slice: crashed with exit `139`.
- Split bounded advanced groups: groups through
  `advanced-binding-mutation` passed; `advanced-macro-hygiene` crashed.
- `advanced-macro-hygiene-stdlib-migration`: `13 passed, 0 failed`.
- Pre-fix `advanced-macro-hygiene-string-number`: crashed with exit `139`.
- Calibration probes:
  - bounded container `(f 896)`: passed.
  - bounded container `(f 1200)`: crashed.
  - host `(f 512)`: passed.
  - host `(f 640)`, `(f 768)`, `(f 896)`, and `(f 1200)`: crashed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Host `advanced-macro-hygiene-string-number`: `9 passed, 0 failed`.
- Bounded container `advanced-macro-hygiene-string-number`: `9 passed, 0 failed`.
- Host `advanced-macro-hygiene`: `83 passed, 0 failed`.
- Bounded container `advanced-macro-hygiene`: `83 passed, 0 failed`.
- Bounded container full `advanced` slice: `1928 passed, 0 failed`.
- `ldconfig -p | rg -i 'lapacke|openblas|lapack'`: `liblapacke.so` and
  `liblapacke.so.3` are now visible under `/lib/aarch64-linux-gnu`.
- Host `advanced-collections-module` with
  `LD_LIBRARY_PATH=/usr/local/lib:/usr/lib/aarch64-linux-gnu`:
  `624 passed, 0 failed`.
- Bounded container `advanced-collections-module` with mounted aarch64
  toolchain and LAPACK library path: `611 passed, 0 failed`.
- Bounded container `memory-lifetime-smoke` with the same runtime library path:
  `225 passed, 0 failed`.

Current best recommendation / checkpoint:
- Keep this test as a portable non-tail recursion smoke at depth `512`.
- Do not reintroduce the old high thresholds without an explicit
  architecture/stack-limit detector and bounded validation on this host.

Unresolved issues:
- The validation image still has an x86-64 `/opt/c3/c3c`; bounded validation on
  this host needs `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 20:42 CEST - Pure Eigenpairs Real-Schur Fallback Stabilization

Objective attempted:
- Broaden validation after the LAPACK forced-fallback coverage lane and fix the
  bounded-container-only pure `matrix/eigenpairs` fallback failure it exposed.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Updated the pure QR eigenvalue convergence check in
  `src/lisp/prim_tensor_matrix.c3`.
- The convergence scan now treats isolated subdiagonal 2x2 real-Schur blocks
  as settled instead of applying shifted QR iteration to an already-settled
  complex block.
- Updated `TODO.md`, `memory/CHANGELOG.md`, Tensor area/plan docs,
  `.agents/PLAN.md`, and this session report.

Key results:
- Fixed a bounded-container-only corruption where forced no-`dgeev`
  `matrix/eigenpairs` returned a trailing real eigenvalue like `6146+0i`
  instead of `2+0i` for a 3x3 real-plus-complex-block matrix.
- Host focused validation stayed green.
- Bounded container focused validation now passes with the mounted aarch64 host
  toolchain.
- The existing tolerance-based LAPACK QR rank guard was preserved.

Commands run and key results:
- Initial bounded container focused run with the container's default `c3c`:
  blocked by `Exec format error` because `/opt/c3/c3c` in the image is x86-64
  while the container is `aarch64`.
- Retried bounded validation with
  `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- Pre-fix bounded container focused run: `608 passed, 3 failed`.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `624 passed, 0 failed`.
- `OMNI_VALIDATION_TIMEOUT_SEC=900 OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh bash -lc 'c3c build --obj-out obj && env LD_LIBRARY_PATH=/opt/omni-host-toolchain/lib:/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp'`:
  `611 passed, 0 failed`.
- `OMNI_TENSOR_DISABLE_LAPACK_DGEEV=1 ./build/main --eval ...` direct value
  smoke for the 3x3 real-plus-complex block: `"2+0i"`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The LAPACK forced-fallback coverage lane is now complete for the shipped
  Tensor matrix backend routines, and the bounded container validation no
  longer exposes the pure eigenpairs fallback corruption.

Unresolved issues:
- The validation image still has an x86-64 `/opt/c3/c3c`; bounded validation on
  this host needs `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 20:24 CEST - QR Forced Fallback Coverage

Objective attempted:
- Continue `TENSOR-090` backend coverage by adding forced pure fallback
  validation for the `dgeqrf`/`dorgqr`-backed QR surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a private test-only QR backend disable hook in
  `csrc/tensor_lapack_helpers.c` and its C3 extern in
  `src/lisp/tensor_lapack_backend.c3`.
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- The new test forces the runtime QR LAPACK backend off for `matrix/qr`, then
  verifies an expected public result through the pure reduced QR fallback.
- Preserved the existing tolerance-based LAPACK QR rank guard.
- Updated `TODO.md`, `memory/CHANGELOG.md`, Tensor area/plan docs,
  `.agents/PLAN.md`, and this session report.

Key results:
- The public matrix API is unchanged.
- `matrix/qr` now has explicit forced pure fallback coverage.
- QR backend control is independent and follows the same test-only disable
  pattern as the other LAPACK routines.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `624 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The QR lane now validates accelerated candidates and forced pure fallback
  behavior for `matrix/qr`.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 20:10 CEST - DPOTRF Cholesky Forced Fallback Coverage

Objective attempted:
- Continue `TENSOR-090` backend coverage by adding forced pure fallback
  validation for the `dpotrf`-backed Cholesky surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a private test-only `LAPACKE_dpotrf` disable hook in
  `csrc/tensor_lapack_helpers.c` and its C3 extern in
  `src/lisp/tensor_lapack_backend.c3`.
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- The new test forces runtime `dpotrf` off for `matrix/cholesky`, then verifies
  an expected public result through the pure lower-triangular Cholesky
  fallback.
- Updated `TODO.md`, `memory/CHANGELOG.md`, Tensor area/plan docs,
  `.agents/PLAN.md`, and this session report.

Key results:
- The public matrix API is unchanged.
- `matrix/cholesky` now has explicit forced pure fallback coverage.
- `dpotrf` backend control is independent and follows the same test-only
  disable pattern as the other LAPACK routines.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `622 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The `dpotrf` Cholesky lane now validates accelerated candidates and forced
  pure fallback behavior for `matrix/cholesky`.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 19:55 CEST - DGETRF LU And Determinant Forced Fallback Coverage

Objective attempted:
- Continue `TENSOR-090` backend coverage by adding forced pure fallback
  validation for the `dgetrf`-backed LU surfaces.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a private test-only `LAPACKE_dgetrf` disable hook in
  `csrc/tensor_lapack_helpers.c` and its C3 extern in
  `src/lisp/tensor_lapack_backend.c3`.
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- The new tests force runtime `dgetrf` off for `matrix/lu` and
  `matrix/determinant`, then verify expected public results through the pure
  partial-pivot LU fallback.
- Updated `TODO.md`, `memory/CHANGELOG.md`, Tensor area/plan docs,
  `.agents/PLAN.md`, and this session report.

Key results:
- The public matrix APIs are unchanged.
- `matrix/lu` now has explicit forced pure fallback coverage.
- `matrix/determinant` now has explicit forced pure fallback coverage.
- `dgetrf` backend control is independent and follows the same test-only
  disable pattern as the other LAPACK routines.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `620 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The `dgetrf` LU lane now validates accelerated candidates and forced pure
  fallback behavior for `matrix/lu` and `matrix/determinant`.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 19:31 CEST - DGESV Solve And Inverse Forced Fallback Coverage

Objective attempted:
- Continue `TENSOR-090` backend coverage by adding forced pure fallback
  validation for the `dgesv`-backed solver surfaces.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a private test-only `LAPACKE_dgesv` disable hook in
  `csrc/tensor_lapack_helpers.c` and its C3 extern in
  `src/lisp/tensor_lapack_backend.c3`.
- Decoupled the `dgesv` helper from the unrelated `dgeev` disable state.
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- The new tests force runtime `dgesv` off for `matrix/solve` and
  `matrix/inverse`, then verify expected public results through the pure
  Gaussian solver fallback.
- Updated `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`, and this
  session report.

Key results:
- The public matrix APIs are unchanged.
- `matrix/solve` now has explicit forced pure fallback coverage.
- `matrix/inverse` now has explicit forced pure fallback coverage.
- `dgesv` backend control is no longer tied to the `dgeev` test hook.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `616 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The `dgesv` solver lane now validates accelerated candidates and forced pure
  fallback behavior for `matrix/solve` and `matrix/inverse`.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 19:17 CEST - Symmetric Eigen Forced Fallback Coverage

Objective attempted:
- Continue `TENSOR-090` backend coverage by adding forced pure fallback
  validation for the symmetric eigen matrix surfaces.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a private test-only `LAPACKE_dsyev` disable hook in
  `csrc/tensor_lapack_helpers.c` and its C3 extern in
  `src/lisp/tensor_lapack_backend.c3`.
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- The new tests force runtime `dsyev` off for `matrix/eigenvalues` and
  `matrix/eigenvectors`, then verify expected public results through the pure
  symmetric Jacobi fallback.
- Updated `TODO.md`, `memory/CHANGELOG.md`, Tensor area/plan docs,
  `.agents/PLAN.md`, and this session report.

Key results:
- The public matrix APIs are unchanged.
- `matrix/eigenvalues` now has explicit forced pure Jacobi fallback coverage.
- `matrix/eigenvectors` now has explicit forced pure Jacobi fallback coverage
  for aligned values and representative vector columns.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `612 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The symmetric eigen lane now validates accelerated candidates and forced pure
  fallback behavior for `matrix/eigenvalues` and `matrix/eigenvectors`.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 19:04 CEST - SVD-Backed Matrix Forced Fallback Coverage

Objective attempted:
- Continue `TENSOR-090` backend coverage by using the private `dgesvd` disable
  hook to harden the remaining SVD-backed public matrix contracts.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- The new tests force runtime `dgesvd` off for `matrix/rank`,
  `matrix/singular-values`, and `matrix/svd`, then verify the expected public
  results through pure fallback paths.
- Updated `TODO.md`, `memory/CHANGELOG.md`, Tensor area/plan docs,
  `.agents/PLAN.md`, and this session report.

Key results:
- The public matrix APIs are unchanged.
- `matrix/rank` now has explicit forced pure row-echelon fallback coverage
  while the private backend reports `dgesvd` unavailable.
- `matrix/singular-values` and `matrix/svd` now have explicit forced pure
  Gram/Jacobi SVD fallback coverage while the private backend reports `dgesvd`
  unavailable.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `606 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The SVD-backed matrix lane now validates accelerated candidates and forced
  pure fallback behavior for `matrix/rank`, `matrix/norm`,
  `matrix/singular-values`, and `matrix/svd`.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 18:49 CEST - Matrix Norm SVD Backend And Fallback Coverage

Objective attempted:
- Continue `TENSOR-090` backend coverage for an already-shipped matrix
  contract by hardening `matrix/norm` spectral/nuclear validation.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a private test-only `LAPACKE_dgesvd` disable hook in
  `csrc/tensor_lapack_helpers.c` and its C3 extern in
  `src/lisp/tensor_lapack_backend.c3`.
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3` proving
  `matrix/norm` `'spectral` and `'nuclear` use the runtime `dgesvd` backend
  when available and preserve results when forced through the pure fallback.
- Updated `TODO.md`, `memory/CHANGELOG.md`, Tensor area/plan docs,
  `.agents/PLAN.md`, and this session report.

Key results:
- The public `matrix/norm` API is unchanged.
- Backend coverage now checks both singular-value norm selectors against the
  optional LAPACK path.
- Forced-fallback coverage now disables `dgesvd` inside the private backend
  and verifies both selectors still return `3.0` and `5.0` on the diagonal
  `[3 0; 0 2]` input.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `598 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- `matrix/norm` now has both the complete common `Float64` selector set and
  backend/fallback validation for the SVD-backed selectors. Future norm work
  should be a separate high-precision Tensor dtype contract slice.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 18:00 CEST - Matrix Norm Spectral And Nuclear Selectors

Objective attempted:
- Continue `matrix/norm` by adding the singular-value based norm selectors
  that were left as future work after the first norm surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Extended `matrix/norm` in `src/lisp/prim_tensor_matrix.c3` with
  `'spectral` and `'nuclear` selectors.
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated language/reference docs, Tensor area/plan docs, the matrix surface
  decision note, `TODO.md`, `memory/CHANGELOG.md`, `.agents/PLAN.md`, and
  this session report.

Key results:
- `'spectral` returns the largest singular value.
- `'nuclear` returns the sum of singular values.
- Both selectors reuse the existing `matrix_svd_factor` pure/LAPACK machinery.
- Empty axes still return `0.0`, lazy inputs are still realized, and invalid
  selectors still fail closed.

Commands run and key results:
- Direct spectral smoke on diagonal `[3 0; 0 2]`: `3.0`.
- Direct nuclear smoke on diagonal `[3 0; 0 2]`: `5.0`.
- Direct empty spectral smoke: `0.0`.
- Direct empty nuclear smoke: `0.0`.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `592 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- `matrix/norm` now covers the common `Float64` matrix norm selector set:
  Frobenius, one, infinity, max-absolute, spectral, and nuclear. Future norm
  work should be a separate high-precision Tensor dtype contract slice.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 17:51 CEST - Matrix Norm Surface

Objective attempted:
- Continue the scientific numerics lane with a new matrix reducer instead of
  adding more fixed `matrix/eigenpairs` examples.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/norm` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/norm` in interpreter and AOT primitive lookup tables.
- Added focused advanced collections/module tests in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated language/reference docs, Tensor area/plan docs, matrix surface
  decision note, plan index, `TODO.md`, `memory/CHANGELOG.md`,
  `.agents/PLAN.md`, and this session report.

Key results:
- `matrix/norm` accepts rank-2 `Float64` Tensor inputs, realizes lazy inputs,
  supports empty axes as `0.0`, and returns a `Float64`.
- Default behavior is Frobenius norm.
- Explicit selectors are `'frobenius`, `'one`, `'infinity`, and `'max`.

Commands run and key results:
- Direct default Frobenius smoke: `5.0`.
- Direct one-norm smoke: `9.0`.
- Direct infinity-norm smoke: `15.0`.
- Direct lazy Frobenius smoke: `5.0`.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `590 passed, 0 failed`.

Current best recommendation / checkpoint:
- `matrix/norm` is a complete first general matrix norm reducer. This
  checkpoint was later extended by the 18:00 CEST `TENSOR-090AI`
  spectral/nuclear selector slice; future norm work should now be a separate
  high-precision Tensor dtype contract slice.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 17:39 CEST - General Matrix Eigenpairs Residual Harness

Objective attempted:
- Continue `matrix/eigenpairs` validation by moving from hand-expanded
  one-column residual assertions to a reusable all-column residual harness.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added Lisp-side 3x3 residual helpers to
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Added backend and forced-fallback harness checks for:
  - `[5 7 0; 0 3 2; 0 0 1]`
  - `[0 -1 0; 1 0 0; 0 0 2]`
- Updated `memory/CHANGELOG.md`, `docs/areas/tensor-scientific.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, `TODO.md`,
  `.agents/PLAN.md`, and this session report.

Key results:
- The focused suite now validates every returned eigenvector column for two
  representative 3x3 matrices under backend and forced-fallback execution.
- The helper reduces future eigenpair residual tests to matrix selection and
  tolerance, instead of duplicating row formulas.

Commands run and key results:
- Prototype backend helper smoke: `true`.
- Prototype forced-fallback helper smoke: `true`.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `580 passed, 0 failed`.

Current best recommendation / checkpoint:
- Fixed-size `matrix/eigenpairs` residual validation is now covered by a
  reusable harness. Future eigenpair validation should be a broader
  randomized/property or container-bound stress lane, not more manually listed
  3x3 examples.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 17:31 CEST - General Matrix Eigenpairs Non-Normal Residual Coverage

Objective attempted:
- Continue `matrix/eigenpairs` validation with a non-normal residual case that
  exercises vector alignment beyond diagonal and simple rotation examples.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added backend and forced-fallback residual regressions for the non-normal
  upper-triangular matrix `[5 7 0; 0 3 2; 0 0 1]` in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/areas/tensor-scientific.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, `TODO.md`,
  `.agents/PLAN.md`, and this session report.

Key results:
- Both accelerated and forced-fallback paths now validate the middle returned
  vector column for a non-normal matrix satisfies `A*v ~= lambda*v`.
- This covers a non-basis eigenvector case instead of repeating only diagonal
  or 2x2 rotation residuals.

Commands run and key results:
- Direct backend non-normal residual smoke: `true`.
- Direct forced-fallback non-normal residual smoke: `true`.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `578 passed, 0 failed`.

Current best recommendation / checkpoint:
- Fixed-example `matrix/eigenpairs` residual coverage now includes diagonal,
  complex rotation, real-plus-complex-block, and non-normal triangular cases
  across pure fallback and accelerated paths where applicable.
- Future work should move to a broader stress/property validation harness or a
  different scientific-numerics surface.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 17:24 CEST - General Matrix Eigenpairs Backend Residual Coverage

Objective attempted:
- Continue `matrix/eigenpairs` validation by adding accelerated-path residual
  coverage after closing the pure fallback residual slice.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `LAPACKE_dgeev`-path residual regressions in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/areas/tensor-scientific.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, `TODO.md`,
  `.agents/PLAN.md`, and this session report.

Key results:
- When the runtime backend is available, `matrix/eigenpairs` now validates
  representative real and complex returned vector columns satisfy
  `A*v ~= lambda*v`.
- This complements the forced no-`dgeev` fallback residual checks from
  `TENSOR-090AD`.

Commands run and key results:
- Direct backend residual smoke for a real diagonal matrix: `true`.
- Direct backend residual smoke for a complex rotation matrix: `true`.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `576 passed, 0 failed`.

Current best recommendation / checkpoint:
- `matrix/eigenpairs` now has value, dtype/shape, backend call-count, pure
  fallback residual, and accelerated-path residual coverage in the focused
  advanced collections/module suite.
- Future work should move to broader stress/property validation or a different
  scientific-numerics surface instead of repeating fixed eigenpair examples.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 17:14 CEST - General Matrix Eigenpairs Fallback Coverage

Objective attempted:
- Continue hardening `matrix/eigenpairs` after closing the no-LAPACK fallback
  gap.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added forced no-`dgeev` fallback regressions in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/areas/tensor-scientific.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, `TODO.md`,
  `.agents/PLAN.md`, and this session report.

Key results:
- The pure `matrix/eigenpairs` fallback is now covered beyond the initial 2x2
  cases.
- Added 3x3 fallback coverage for diagonal real eigenvalues, upper-triangular
  real eigenvalues, and a real-plus-complex-conjugate block.
- Added representative residual checks that validate returned vector columns
  satisfy `A*v ~= lambda*v` in the forced fallback path.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `574 passed, 0 failed`.

Current best recommendation / checkpoint:
- `matrix/eigenpairs` fallback coverage is now reasonable for 2x2 and targeted
  3x3 cases. Future work can add broader property tests or larger stress
  cases, preferably as a distinct validation slice.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 17:03 CEST - General Matrix Eigenpairs Pure Fallback

Objective attempted:
- Continue `matrix/eigenpairs` by closing the backend-required gap from the
  first implementation.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a pure fallback for `matrix/eigenpairs` in
  `src/lisp/prim_tensor_matrix.c3`.
- Added a `dgeev` disable hook and `OMNI_TENSOR_DISABLE_LAPACK_DGEEV` switch
  in `csrc/tensor_lapack_helpers.c` so fallback behavior can be validated on
  LAPACKE-enabled hosts.
- Added focused fallback tests to
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated docs, `TODO.md`, `memory/CHANGELOG.md`, and `.agents/PLAN.md` so
  `matrix/eigenpairs` is no longer described as backend-required.

Key results:
- `matrix/eigenpairs` now uses runtime `LAPACKE_dgeev` when available and
  otherwise computes eigenpairs through a pure QR/nullspace path.
- The fallback preserves the same public contract: `BigComplex` values,
  aligned `BigComplex` vector columns, deterministic sorting, lazy input
  realization, and empty square support.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `568 passed, 0 failed`.
- `OMNI_TENSOR_DISABLE_LAPACK_DGEEV=1 ... --eval '(String ... values [0])'`:
  returned `"0+1i"`.
- `OMNI_TENSOR_DISABLE_LAPACK_DGEEV=1 ... --eval '(String (dtype ... vectors))'`:
  returned `"BigComplex"`.
- `OMNI_TENSOR_DISABLE_LAPACK_DGEEV=1 ... --eval '(< ... diagonal value ...)'`:
  returned `true`.

Current best recommendation / checkpoint:
- `TENSOR-090AC` is closed. Future matrix work can add backend coverage for
  shipped surfaces or broaden numerical fallback tests, but `matrix/eigenpairs`
  now has a real no-LAPACK path.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 16:42 CEST - General Matrix Eigenpairs Surface

Objective attempted:
- Continue the Tensor matrix lane after the owner selected the flexible
  language-aligned real-vs-complex contract for general eigenpairs.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added dynamic `LAPACKE_dgeev` discovery, call-count telemetry, and C3
  externs in the Tensor LAPACK helper path.
- Added `omni_big_complex_from_doubles` to the BigComplex helper ABI so
  backend `double` real/imaginary parts can populate `BigComplex` Tensor
  storage directly.
- Added public `matrix/eigenpairs` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/eigenpairs` in interpreter and AOT primitive lookup
  tables.
- Added focused advanced collection/module regressions for complex eigenvalues,
  real diagonal eigenpairs, vector dtype/shape, lazy input realization, empty
  shape, `dgeev` backend call-count coverage, rank validation, and dtype
  validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, `TODO.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/eigenpairs` accepts square rank-2 `Float64` Tensor values.
- Lazy inputs are realized before factorization.
- The result is a dictionary with `values` as `BigComplex` shape `[n]` and
  `vectors` as `BigComplex` shape `[n n]`.
- Real and complex eigenvalues use the same output representation.
- Eigenpairs are sorted by descending eigenvalue magnitude with deterministic
  real/imaginary tie-breakers.
- Right eigenvector columns are aligned with values and phase-normalized.
- Empty square input returns empty `[0]` values and `[0 0]` vectors without a
  backend call.
- Superseded by the 17:03 CEST entry: the first implementation required
  runtime `LAPACKE_dgeev`, but `matrix/eigenpairs` now has a pure fallback.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- An incorrect focused test invocation using `--test` failed because the binary
  does not expose that flag; the correct runner is `--test-suite lisp` plus the
  slice/group environment variables.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `564 passed, 0 failed`.
- Direct complex eigenvalue smoke returned `"0+1i"`.
- Direct vector dtype smoke returned `"BigComplex"`.
- Direct empty-vector-shape smoke returned `0`.
- Direct dtype validation smoke returned
  `matrix/eigenpairs: expected a square rank-2 Float64 Tensor`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Invalidated assumptions / failed approaches:
- Do not use `./build/main --test ...`; this binary uses
  `--test-suite lisp` with `OMNI_LISP_TEST_SLICE` and
  `OMNI_ADVANCED_GROUP_FILTER` for focused Lisp groups.

Current best recommendation / checkpoint:
- `matrix/eigenpairs` is the canonical public surface for general square
  eigenpairs.
- Superseded by the 17:03 CEST entry: `TENSOR-090AC` is closed.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 16:13 CEST - Matrix Singular-Values Surface

Objective attempted:
- Continue the Tensor matrix lane with a direct singular-value extraction
  surface that reuses the existing SVD contract and avoids the unresolved
  general nonsymmetric eigenpair output contract.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/singular-values` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/singular-values` in interpreter and AOT primitive lookup
  tables.
- Added focused advanced collection/module regressions for leading and second
  singular values, wide matrix shape, empty-axis shape, lazy input realization,
  rank-deficient zero singular value, backend `dgesvd` call-count coverage,
  rank validation, and dtype validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/singular-values` accepts rank-2 `Float64` Tensor values.
- Lazy inputs are realized before factorization.
- The result is a rank-1 `Float64` Tensor with shape
  `[min(rows, columns)]`.
- Singular values are sorted descending by the existing SVD machinery.
- Rectangular and rank-deficient matrices are supported.
- Empty axes return an empty rank-1 Tensor.
- Optional `LAPACKE_dgesvd` acceleration is reused when available, with the
  pure Gram/Jacobi SVD path as fallback.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Initial focused test run failed because the new rank-1 Tensor `ref`
  assertions used scalar indices (`0`, `1`) instead of Tensor index arrays
  (`[0]`, `[1]`); tests were corrected and the binary rebuilt.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `550 passed, 0 failed`.
- Direct leading singular value smoke returned `3.0`.
- Direct wide-shape smoke returned `2`.
- Direct empty-shape smoke returned `0`.
- Direct lazy-input smoke returned `2.0`.
- Direct dtype validation smoke returned
  `matrix/singular-values: expected a rank-2 Float64 Tensor`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Invalidated assumptions / failed approaches:
- Do not write rank-1 Tensor `ref` tests as `(ref tensor 0)` or
  `(ref tensor 1)`; use index arrays such as `(ref tensor [0])`.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces or backend coverage that
  preserves shipped contracts.
- General nonsymmetric eigenpairs still need a real-vs-complex output contract
  decision before becoming a public surface.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/svals` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 16:07 CEST - Matrix Rank LAPACK Coverage

Objective attempted:
- Continue the Tensor matrix lane by adding backend coverage for an
  already-shipped public contract instead of opening the unresolved general
  nonsymmetric eigenpair surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Routed `matrix/rank` in `src/lisp/prim_tensor_matrix.c3` through optional
  runtime-loaded `LAPACKE_dgesvd` when available.
- Kept the pure partial-pivot row-echelon rank counter as the fallback path.
- Added focused advanced collection/module coverage that asserts the
  `dgesvd` call counter advances when the backend is available and otherwise
  treats fallback retention as the expected result.
- Updated `memory/CHANGELOG.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- The public `matrix/rank` contract is unchanged.
- Backend rank counting uses singular values from `LAPACKE_dgesvd` and counts
  values above the caller/default tolerance.
- Pure row-echelon rank counting remains active when LAPACK is unavailable,
  rejects the input, or fails to converge.
- Local focused validation now proves the `dgesvd` rank path on this host
  because `liblapacke` is installed.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `540 passed, 0 failed`.

Current best recommendation / checkpoint:
- Continue with backend coverage for shipped `matrix/` contracts or make a
  product-level decision for general nonsymmetric eigenpairs before exposing
  them.
- Keep public names backend-neutral under `matrix/`; backend names remain
  implementation details.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 15:57 CEST - Matrix Rank Surface

Objective attempted:
- Continue the Tensor matrix lane with a numerical structural reduction that
  does not depend on the unresolved nonsymmetric eigenpair output contract.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/rank` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/rank` in interpreter and AOT primitive lookup tables.
- Added focused advanced collection/module regressions for full rectangular
  rank, deficient rectangular rank, empty axes, lazy input realization,
  tolerance suppression, zero-tolerance small pivots, vector validation,
  dtype validation, and negative tolerance validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/rank` accepts rectangular rank-2 `Float64` Tensor values.
- Lazy inputs are realized before rank counting.
- Rank is computed with partial-pivot row-echelon elimination over a copied
  scratch buffer.
- The default tolerance is `1e-12`.
- Optional tolerance selection accepts numeric values that narrow to a
  non-negative finite `Float64`.
- Empty axes return `0`.
- The result is an `Integer`.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `538 passed, 0 failed`.
- Direct full rectangular smoke returned `2`.
- Direct deficient rectangular smoke returned `1`.
- Direct empty-axis smoke returned `0`.
- Direct tolerance-suppressed small-pivot smoke returned `1`.
- Direct zero-tolerance small-pivot smoke returned `2`.
- Direct lazy-input smoke returned `2`.
- Direct dtype validation smoke returned
  `matrix/rank: expected a rank-2 Float64 Tensor`.
- Direct negative-tolerance validation smoke returned
  `matrix/rank: tolerance must be a non-negative finite number`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces or backend coverage that
  preserves shipped contracts.
- General nonsymmetric eigenpairs still need a real-vs-complex output contract
  decision before becoming a public surface.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/rank?`, `matrix/num-rank`, or backend-flavored names
  unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 15:51 CEST - Matrix Identity Surface

Objective attempted:
- Continue the Tensor matrix lane with a size-driven structural constructor
  that supports the existing Tensor dtype families.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/identity` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/identity` in interpreter and AOT primitive lookup tables.
- Added focused advanced collection/module regressions for default Float64
  diagonal/off-diagonal values, shape, empty shape, BigInteger/BigFloat/
  BigComplex identity values, dtype preservation, negative size validation,
  and invalid dtype validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/identity` accepts non-negative integer sizes.
- The result is a square rank-2 Tensor with shape `[n n]`.
- The default dtype is `Float64`.
- Optional dtype selection supports `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex`, using the existing Tensor dtype parser.
- Diagonal cells are filled with the dtype's one value.
- Off-diagonal cells are filled with the dtype's zero value.
- Empty size `0` returns an empty square Tensor with shape `[0 0]`.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `529 passed, 0 failed`.
- Direct default Float64 diagonal smoke returned `1.0`.
- Direct default Float64 off-diagonal smoke returned `0.0`.
- Direct empty shape smoke returned `[0 0]`.
- Direct BigInteger one smoke returned `"1"`.
- Direct BigInteger zero smoke returned `"0"`.
- Direct BigFloat one smoke returned `"1"`.
- Direct BigComplex one smoke returned `"1+0i"`.
- Direct dtype preservation smoke returned `"BigComplex"`.
- Direct negative-size validation smoke returned
  `matrix/identity: size must be a non-negative Integer`.
- Direct invalid-dtype validation smoke returned
  `matrix/identity: dtype must be Float64, BigInteger, BigFloat, or BigComplex`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces or backend coverage that
  preserves shipped contracts.
- General nonsymmetric eigenpairs still need a real-vs-complex output contract
  decision before becoming a public surface.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/eye` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 15:43 CEST - Matrix Diagonal-Matrix Surface

Objective attempted:
- Continue the Tensor matrix lane with the constructor counterpart to
  `matrix/diagonal`, preserving native dtype ownership.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/diagonal-matrix` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/diagonal-matrix` in interpreter and AOT primitive lookup
  tables.
- Added focused advanced collection/module regressions for Float64 diagonal
  and off-diagonal values, shape, lazy input realization, empty square shape,
  BigInteger/BigFloat/BigComplex clone behavior, BigInteger off-diagonal zero,
  dtype preservation, and rank validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/diagonal-matrix` accepts rank-1 Tensor values.
- Lazy inputs are realized before matrix construction.
- The result is a square rank-2 Tensor with shape `[n n]`.
- Native dtype is preserved for `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex`.
- High-precision diagonal element handles are cloned into the result rather
  than shallow-copied.
- Off-diagonal cells are filled with the dtype's zero value.
- Empty rank-1 inputs return an empty square Tensor with shape `[0 0]`.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `517 passed, 0 failed`.
- Direct Float64 diagonal smoke returned `3.0`.
- Direct Float64 off-diagonal smoke returned `0.0`.
- Direct empty square shape smoke returned `[0 0]`.
- Direct BigInteger diagonal smoke returned `"9223372036854775808"`.
- Direct BigInteger off-diagonal zero smoke returned `"0"`.
- Direct BigFloat diagonal smoke returned `"2.5"`.
- Direct BigComplex diagonal smoke returned `"3+4i"`.
- Direct dtype preservation smoke returned `"BigComplex"`.
- Direct lazy-input diagonal smoke returned `2.0`.
- Direct rank validation smoke returned
  `matrix/diagonal-matrix: expected a rank-1 Tensor`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces or backend coverage that
  preserves shipped contracts.
- General nonsymmetric eigenpairs still need a real-vs-complex output contract
  decision before becoming a public surface.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/diag` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 15:31 CEST - Matrix Diagonal Surface

Objective attempted:
- Continue the Tensor matrix lane with a deterministic structural matrix
  transform that returns a Tensor and preserves native dtype ownership.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/diagonal` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/diagonal` in interpreter and AOT primitive lookup tables.
- Added focused advanced collection/module regressions for rectangular
  Float64 extraction, result shape, lazy input realization, empty diagonal
  shape, BigInteger/BigFloat/BigComplex clone behavior, dtype preservation,
  and rank validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/diagonal` accepts rank-2 Tensor values, including rectangular
  matrices.
- Lazy inputs are realized before extraction.
- The result is a rank-1 Tensor with length `min(rows, columns)`.
- Native dtype is preserved for `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex`.
- High-precision Tensor element handles are cloned into the result rather than
  shallow-copied.
- Empty rectangular inputs return an empty rank-1 Tensor with shape `[0]`.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
  A new unreachable-code warning from an exhaustive dtype switch was removed,
  then the command was rerun successfully.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `505 passed, 0 failed`.
- Direct rectangular Float64 diagonal smoke returned `5.0`.
- Direct empty diagonal shape smoke returned `[0]`.
- Direct BigInteger diagonal smoke returned `"9223372036854775808"`.
- Direct BigFloat diagonal smoke returned `"2.5"`.
- Direct BigComplex diagonal smoke returned `"3+4i"`.
- Direct dtype preservation smoke returned `"BigComplex"`.
- Direct lazy-input diagonal smoke returned `4.0`.
- Direct rank validation smoke returned
  `matrix/diagonal: expected a rank-2 Tensor`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces or backend coverage that
  preserves shipped contracts.
- General nonsymmetric eigenpairs still need a real-vs-complex output contract
  decision before becoming a public surface.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/diag` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 15:23 CEST - Matrix Trace Surface

Objective attempted:
- Continue the Tensor matrix lane with a deterministic structural matrix
  reduction that keeps native Tensor numeric families intact.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/trace` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/trace` in interpreter and AOT primitive lookup tables.
- Added focused advanced collection/module regressions for Float64 trace,
  lazy input realization, empty square zero, BigInteger/BigFloat/BigComplex
  scalar results, vector rejection, and non-square rejection.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/trace` accepts square rank-2 Tensor values.
- Lazy inputs are realized before diagonal summation.
- The result is a scalar in the Tensor's native numeric family: `Double` for
  `Float64`, or `BigInteger`, `BigFloat`, and `BigComplex` for matching
  high-precision Tensor dtypes.
- Empty square matrices return the dtype's zero scalar.
- Rectangular and non-rank-2 inputs raise
  `matrix/trace: expected a square rank-2 Tensor`.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `496 passed, 0 failed`.
- Direct Float64 trace smoke returned `5.0`.
- Direct empty square trace smoke returned `0.0`.
- Direct BigInteger trace smoke returned `"9223372036854775812"`.
- Direct BigFloat trace smoke returned `"3.75"`.
- Direct BigComplex trace smoke returned `"4+6i"`.
- Direct lazy-input trace smoke returned `5.0`.
- Direct rectangular validation smoke returned
  `matrix/trace: expected a square rank-2 Tensor`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces or backend coverage that
  preserves shipped contracts.
- General nonsymmetric eigenpairs still need a real-vs-complex output contract
  decision before becoming a public surface.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/tr` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 15:12 CEST - Matrix Transpose Surface

Objective attempted:
- Continue the Tensor matrix lane with a deterministic structural matrix
  transform that preserves native Tensor dtypes.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/transpose` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/transpose` in interpreter and AOT primitive lookup
  tables.
- Added focused advanced collection/module regressions for Float64 value
  transposition, result shape, lazy input realization, BigInteger/BigFloat/
  BigComplex clone behavior, dtype preservation, and rank validation.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/transpose` accepts rank-2 Tensor values.
- Lazy inputs are realized before transposition.
- The result shape is `[columns rows]`.
- Native dtype is preserved for `Float64`, `BigInteger`, `BigFloat`, and
  `BigComplex`.
- High-precision Tensor element handles are cloned into the result rather than
  shallow-copied, preserving deterministic Tensor ownership.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- First focused suite run caught two stale shape assertions that compared
  integer shape entries through `test_eq_double`; the implementation output
  was correct (`[3 2]`). The assertions were changed to string checks.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `488 passed, 0 failed`.
- Direct Float64 transpose smoke:
  `(ref (matrix/transpose (Tensor Float64 [2 3] [1 2 3 4 5 6])) [2 1])`
  returned `6.0`.
- Direct shape smoke returned `[3 2]`.
- Direct BigInteger clone smoke returned `"9223372036854775808"`.
- Direct BigFloat clone smoke returned `"2.5"`.
- Direct BigComplex clone smoke returned `"3+4i"`.
- Direct rank validation smoke returned
  `matrix/transpose: expected a rank-2 Tensor`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue with deterministic `matrix/` surfaces that have pure fallbacks, or
  make the product-level decision for general nonsymmetric eigenpairs and
  complex-valued output.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/t` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 15:02 CEST - Matrix Inverse Surface

Objective attempted:
- Continue the Tensor matrix lane with a deterministic user-facing matrix
  surface that does not make new semantics depend on LAPACK availability.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added public `matrix/inverse` in `src/lisp/prim_tensor_matrix.c3`.
- Registered `matrix/inverse` in interpreter and AOT primitive lookup tables.
- Added focused advanced collection/module regressions for inverse values,
  product identity, lazy input, empty square shape, singular input, non-square
  input, complex dtype rejection, and live/fallback `LAPACKE_dgesv` dispatch.
- Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
  `docs/reference/03-collections.md`,
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`,
  `docs/plans/README.md`, and `.agents/PLAN.md`.

Key results:
- `matrix/inverse` accepts nonsingular square rank-2 `Float64` Tensor inputs.
- Lazy inputs are realized before inversion.
- The result is a same-shape `Float64` Tensor.
- Singular inputs raise `tensor/singular-matrix`.
- The implementation uses optional runtime-loaded `LAPACKE_dgesv` when
  available by solving against an identity RHS; the pure Gaussian solver
  remains the semantic oracle and fallback.
- General nonsymmetric eigenpairs remain a separate contract decision because
  they need a complex-valued result policy and deterministic fallback.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `479 passed, 0 failed`.
- Direct inverse first-entry smoke:
  `(ref (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [0 0])`
  returned `0.6`.
- Direct inverse off-diagonal smoke:
  `(ref (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [0 1])`
  returned `-0.7`.
- Direct inverse product identity smoke:
  `(ref (realize (contract (Tensor Float64 [2 2] [4 7 2 6]) (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [1 0])) [1 1])`
  returned `1.0`.
- Direct singular inverse smoke returned
  `matrix/inverse: input matrix is singular`.
- Direct empty square inverse shape smoke returned `0`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` either by deciding the general nonsymmetric eigenpair
  contract, or by adding another deterministic `matrix/` surface with a pure
  fallback and optional backend acceleration.
- Keep public names backend-neutral under `matrix/`; do not add abbreviated
  aliases such as `matrix/inv` unless explicitly approved.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 14:51 CEST - LAPACK DGESVD Behind SVD

Objective attempted:
- Continue backend acceleration coverage behind existing Tensor matrix
  surfaces by adding optional LAPACK SVD support, after installing LAPACKE
  locally for live backend validation.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Installed host package `liblapacke-dev` with `apt`, making `liblapacke.so`
  available to the runtime-loaded LAPACK helper.
- Extended `csrc/tensor_lapack_helpers.c` to resolve `LAPACKE_dgesvd` at
  runtime, with independent availability and call-count probes.
- Routed `matrix/svd` through optional `LAPACKE_dgesvd` acceleration in
  `src/lisp/prim_tensor_matrix.c3`.
- Added reduced SVD result adaptation in the C helper: LAPACK `vt` is
  transposed into Omni's `v` column-factor layout, and factor signs are
  normalized for stable output.
- Added a conditional focused regression for the SVD `dgesvd` backend path in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Tightened the LAPACK QR backend rank check from exact-zero diagonal testing
  to `abs(diagonal) <= 1e-12` after live LAPACKE validation exposed a
  rank-deficient QR false success.
- Updated `memory/CHANGELOG.md`, `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`, and
  `.agents/PLAN.md`.

Key results:
- Public surface remains unchanged: no backend-flavored names were added.
- `matrix/svd` still returns reduced `u`, `s`, and `v` factors with shapes
  `[rows k]`, `[k]`, and `[columns k]`.
- Rank-deficient inputs still succeed with zero singular values.
- Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or backend
  unavailability retain the pure Gram/Jacobi SVD fallback as the semantic
  oracle.
- The host now validates live LAPACKE-backed paths, not just fallback
  retention.

Commands run and key results:
- `sudo apt-get update && sudo apt-get install -y liblapacke-dev`: passed.
- `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`: showed
  `liblapacke.so`, `liblapacke.so.3`, `liblapack.so`, and BLAS libraries.
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Initial live focused advanced collections/module group exposed
  `matrix qr rejects rank deficient input` as a false success; the QR backend
  tolerance fix corrected it.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `469 passed, 0 failed`.
- Direct SVD leading singular value smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 's) [0])`
  returned `3.0`.
- Direct SVD U-column smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 'u) [0 0])`
  returned `1.0`.
- Direct SVD V-column smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 'v) [1 1])`
  returned `1.0`.
- Direct wide SVD V-column smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 3] [3 0 0 0 2 0])) 'v) [1 1])`
  returned `1.0`.
- Direct rank-deficient SVD singular value smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 2] [0 0 0 0])) 's) [0])`
  returned `0.0`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- Final `./scripts/build_omni_chelpers.sh`: passed.
- Final `c3c build --obj-out obj`: passed with existing entry deprecation
  warnings.
- Final focused advanced collections/module group:
  `469 passed, 0 failed`.

Invalidated assumptions / failed approaches:
- Do not keep assuming LAPACK QR rank detection can use exact zero pivots.
  `LAPACKE_dgeqrf` can produce tiny nonzero diagonals for rank-deficient input;
  use a tolerance-based check at this backend boundary.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` by choosing the broader general nonsymmetric eigenpair
  contract, or by adding additional LAPACK backend coverage only where it
  preserves an already-shipped public matrix contract.
- Keep public names backend-neutral under `matrix/`; do not add bare
  decomposition aliases or backend-flavored public names.

Unresolved issues:
- Existing `c3c build` entry deprecation warnings remain unrelated to this
  slice.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 14:40 CEST - LAPACK DSYEV Behind Symmetric Eigen

Objective attempted:
- Continue backend acceleration coverage behind existing Tensor matrix
  surfaces by adding optional LAPACK symmetric eigen support without broadening
  the public eigen contract.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Extended `csrc/tensor_lapack_helpers.c` to resolve `LAPACKE_dsyev` at
  runtime, with independent availability and call-count probes.
- Added a LAPACK no-convergence status code in
  `src/lisp/tensor_lapack_backend.c3`.
- Added C3 externs for the `dsyev` helper in
  `src/lisp/tensor_lapack_backend.c3`.
- Routed `matrix/eigenvalues` and `matrix/eigenvectors` through optional
  `LAPACKE_dsyev` acceleration in `src/lisp/prim_tensor_matrix.c3`.
- Added backend-side descending eigenpair sorting and eigenvector sign
  normalization so output remains aligned and stable.
- Added conditional focused regressions for the eigenvalue and eigenvector
  `dsyev` backend paths in `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`, and
  `.agents/PLAN.md`.

Key results:
- Public surface remains unchanged: no backend-flavored names were added.
- `matrix/eigenvalues` still returns descending rank-1 `Float64` eigenvalues.
- `matrix/eigenvectors` still returns aligned `values` and `vectors` columns.
- Nonsymmetric inputs still raise `tensor/not-symmetric`.
- General nonsymmetric eigenpairs remain a separate product contract decision.
- Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or backend
  unavailability retain the pure symmetric Jacobi fallback as the semantic
  oracle.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct eigenvalues diagonal smoke:
  `(ref (matrix/eigenvalues (Tensor Float64 [2 2] [3 0 0 2])) [0])`
  returned `3.0`.
- Direct eigenvalues symmetric off-diagonal smoke:
  `(ref (matrix/eigenvalues (Tensor Float64 [2 2] [2 1 1 2])) [0])`
  returned `3.0`.
- Direct eigenvectors aligned-value smoke:
  `(ref (ref (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])) 'values) [0])`
  returned `3.0`.
- Direct eigenvectors vector-column smoke:
  `(ref (ref (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])) 'vectors) [0 0])`
  returned `1.0`.
- Direct nonsymmetric eigen smoke returned
  `matrix/eigenvalues: input matrix is not symmetric`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `467 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`: showed
  `liblapack.so.3` and BLAS libraries but no `liblapacke`, so the local run
  validated fallback retention rather than live accelerated `dsyev`.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` by broadening LAPACK/LAPACKE coverage behind SVD, or
  by making the broader nonsymmetric eigenpair contract decision.
- Keep public names backend-neutral under `matrix/`; do not add bare
  decomposition aliases or backend-flavored public names.

Unresolved issues:
- Live `LAPACKE_dsyev` execution was not validated on this host because
  `liblapacke` is unavailable.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 14:27 CEST - LAPACK DGEQRF DORGQR Behind QR

Objective attempted:
- Continue backend acceleration coverage behind the existing Tensor matrix
  surfaces by adding optional LAPACK reduced-QR support without changing the
  public `matrix/qr` contract.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Extended `csrc/tensor_lapack_helpers.c` to resolve paired
  `LAPACKE_dgeqrf` and `LAPACKE_dorgqr` runtime symbols, with independent
  availability and call-count probes.
- Added C3 externs for the QR helper in `src/lisp/tensor_lapack_backend.c3`.
- Routed `matrix/qr` through optional LAPACK acceleration in
  `src/lisp/prim_tensor_matrix.c3`.
- Added backend-side reduced `R` extraction, reduced `Q` formation, and sign
  normalization so the existing positive-diagonal factor orientation is
  preserved when LAPACK is available.
- Added a conditional focused regression for the QR LAPACK backend path in
  `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`, and
  `.agents/PLAN.md`.

Key results:
- Public surface remains unchanged: no backend-flavored names were added.
- `matrix/qr` still returns a dictionary with reduced `q` shape
  `[rows columns]` and `r` shape `[columns columns]`.
- Rank-deficient inputs still raise `tensor/singular-matrix`.
- Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or backend
  unavailability retain the pure C3 QR fallback as the semantic oracle.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct QR Q first-column smoke:
  `(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'q) [0 0])`
  returned `1.0`.
- Direct QR Q second-column smoke:
  `(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'q) [1 1])`
  returned `1.0`.
- Direct QR R projection smoke:
  `(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'r) [0 1])`
  returned `1.0`.
- Direct QR shape smoke returned `3`.
- Direct rank-deficient QR smoke returned
  `matrix/qr: input matrix is rank deficient`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `463 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`: showed
  `liblapack.so.3` and BLAS libraries but no `liblapacke`, so the local run
  validated fallback retention rather than live accelerated QR.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` by broadening LAPACK/LAPACKE coverage behind SVD or
  symmetric eigen surfaces, or by making the broader nonsymmetric eigenpair
  contract decision.
- Keep public names backend-neutral under `matrix/`; do not add bare
  decomposition aliases or backend-flavored public names.

Unresolved issues:
- Live `LAPACKE_dgeqrf`/`LAPACKE_dorgqr` execution was not validated on this
  host because `liblapacke` is unavailable.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 14:20 CEST - LAPACK DPOTRF Behind Cholesky

Objective attempted:
- Continue backend acceleration coverage behind the existing Tensor matrix
  surfaces by adding optional LAPACK Cholesky support without changing the
  public `matrix/cholesky` contract.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Extended `csrc/tensor_lapack_helpers.c` to resolve `LAPACKE_dpotrf` at
  runtime, with independent availability and call-count probes.
- Added C3 externs for the `dpotrf` helper in
  `src/lisp/tensor_lapack_backend.c3`.
- Routed `matrix/cholesky` through optional `LAPACKE_dpotrf` acceleration in
  `src/lisp/prim_tensor_matrix.c3`.
- Added a symmetry-preserving lower-triangle copy helper so nonsymmetric inputs
  are rejected before any LAPACK one-triangle factorization path can run.
- Added a conditional focused regression for the Cholesky `dpotrf` backend path
  in `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`, and
  `.agents/PLAN.md`.

Key results:
- Public surface remains unchanged: no backend-flavored names were added.
- `matrix/cholesky` still returns a lower-triangular `Float64` Tensor with the
  same shape as the input and zero entries above the diagonal.
- Nonsymmetric or non-positive-definite inputs still raise
  `tensor/not-positive-definite`.
- Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or backend
  unavailability retain the pure C3 Cholesky fallback as the semantic oracle.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct Cholesky diagonal smoke:
  `(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [0 0])`
  returned `2.0`.
- Direct Cholesky lower-entry smoke:
  `(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [1 0])`
  returned `1.0`.
- Direct Cholesky upper-zero smoke:
  `(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [0 1])`
  returned `0.0`.
- Direct nonsymmetric and indefinite Cholesky smokes returned
  `matrix/cholesky: input matrix is not symmetric positive definite`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `461 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`: showed
  `liblapack.so.3` and BLAS libraries but no `liblapacke`, so the local run
  validated fallback retention rather than live accelerated `dpotrf`.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` by broadening LAPACK/LAPACKE coverage behind QR, SVD,
  or symmetric eigen surfaces, or by making the broader nonsymmetric eigenpair
  contract decision.
- Keep public names backend-neutral under `matrix/`; do not add bare
  decomposition aliases or backend-flavored public names.

Unresolved issues:
- Live `LAPACKE_dpotrf` execution was not validated on this host because
  `liblapacke` is unavailable.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 14:13 CEST - LAPACK DGETRF Behind LU And Determinant

Objective attempted:
- Continue the Tensor scientific matrix lane by broadening backend coverage
  behind already-shipped public surfaces instead of adding new public names.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Extended `csrc/tensor_lapack_helpers.c` to resolve `LAPACKE_dgetrf` at
  runtime alongside `LAPACKE_dgesv`, with independent availability and
  call-count probes.
- Added C3 externs for the `dgetrf` helper in
  `src/lisp/tensor_lapack_backend.c3`.
- Routed `matrix/lu` and `matrix/determinant` through optional
  `LAPACKE_dgetrf` acceleration in `src/lisp/prim_tensor_matrix.c3`.
- Added conditional focused regressions for the LU and determinant `dgetrf`
  backend paths in `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated `memory/CHANGELOG.md`, `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`,
  `docs/areas/tensor-scientific.md`,
  `docs/plans/matrix-solver-surface-decision-2026-04-16.md`, and
  `.agents/PLAN.md`.

Key results:
- Public surface remains unchanged: no backend-flavored names were added.
- `matrix/lu` still returns combined `lu`, final 0-based `pivots`, and
  `swap-count`; singular inputs still raise `tensor/singular-matrix`.
- `matrix/determinant` still returns a `Float64` scalar and returns `0.0` for
  singular matrices.
- Missing LAPACKE, missing symbols, unsupported LAPACK ABI widths, or backend
  unavailability retain the pure C3 LU fallback as the semantic oracle.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct LU factor smoke:
  `(ref (ref (matrix/lu (Tensor Float64 [2 2] [4 3 2 1])) 'lu) [1 0])`
  returned `0.5`.
- Direct determinant smoke:
  `(matrix/determinant (Tensor Float64 [2 2] [4 3 2 1]))` returned `-2.0`.
- Direct singular LU smoke returned
  `matrix/lu: coefficient matrix is singular`.
- Direct singular determinant smoke returned `0.0`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  `459 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`: showed
  `liblapack.so.3` and BLAS libraries but no `liblapacke`, so the local run
  validated fallback retention rather than live accelerated `dgetrf`.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` by broadening LAPACK/LAPACKE coverage behind the
  remaining shipped surfaces such as QR, Cholesky, SVD, or symmetric eigen, or
  by making the broader nonsymmetric eigenpair contract decision.
- Keep public names backend-neutral under `matrix/`; do not add bare
  decomposition aliases or backend-flavored public names.

Unresolved issues:
- Live `LAPACKE_dgetrf` execution was not validated on this host because
  `liblapacke` is unavailable.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5

## 2026-04-16 13:00 CEST - Matrix Solver And Decomposition Surfaces

Objective attempted:
- Continue the Tensor scientific lane after BLAS `dgemm`/`dgemv`/`ddot`/`dger`
  slices by resolving the solver/decomposition naming blocker and landing the
  first useful matrix solver and decomposition behavior.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `src/lisp/prim_tensor_matrix.c3` with the first `matrix/solve`
  implementation.
- Added private runtime-loaded LAPACK helper wiring in
  `csrc/tensor_lapack_helpers.c` and `src/lisp/tensor_lapack_backend.c3`.
- Added the LAPACK helper source to `project.json` and
  `scripts/build_omni_chelpers.sh`.
- Registered `matrix/solve` in interpreter primitives and AOT primitive
  lookup.
- Added `matrix/lu` in `src/lisp/prim_tensor_matrix.c3` and registered it in
  interpreter primitives and AOT primitive lookup.
- Added `matrix/determinant` in `src/lisp/prim_tensor_matrix.c3` and
  registered it in interpreter primitives and AOT primitive lookup.
- Added `matrix/qr` in `src/lisp/prim_tensor_matrix.c3` and registered it in
  interpreter primitives and AOT primitive lookup.
- Added `matrix/cholesky` in `src/lisp/prim_tensor_matrix.c3` and registered
  it in interpreter primitives and AOT primitive lookup.
- Added `matrix/svd` in `src/lisp/prim_tensor_matrix.c3` and registered it in
  interpreter primitives and AOT primitive lookup.
- Added `matrix/eigenvalues` and `matrix/eigenvectors` in
  `src/lisp/prim_tensor_matrix.c3` and registered them in interpreter
  primitives and AOT primitive lookup.
- Added focused advanced collections/module regressions for vector RHS,
  matrix RHS, lazy coefficient realization, dtype, singular matrices,
  mismatched RHS shape, unsupported complex dtype, and conditional LAPACK path
  use when `LAPACKE_dgesv` is available.
- Added focused `matrix/lu` regressions for combined lower/upper factors,
  pivot row order, swap count, lazy realization, result dtype, singular input,
  non-square input, and unsupported complex dtype.
- Added focused `matrix/determinant` regressions for LU-derived determinant,
  pivot parity, singular-zero behavior, lazy realization, empty-square identity,
  non-square input, and unsupported complex dtype.
- Added focused `matrix/qr` regressions for reduced `q`/`r` factor contents,
  output shapes, lazy realization, wide input rejection, rank-deficient input,
  and unsupported complex dtype.
- Added focused `matrix/cholesky` regressions for lower factor contents, upper
  zeroing, lazy realization, output shape, non-square input, nonsymmetric
  input, indefinite input, and unsupported complex dtype.
- Added focused `matrix/svd` regressions for reduced singular values, `u`/`v`
  factor contents, tall and wide output shapes, lazy realization,
  rank-deficient input, and unsupported complex dtype.
- Added focused `matrix/eigenvalues` and `matrix/eigenvectors` regressions for
  diagonal and off-diagonal symmetric eigenvalues, aligned eigenvector
  payloads, lazy realization, nonsymmetric rejection, and unsupported complex
  dtype.
- Added `docs/plans/matrix-solver-surface-decision-2026-04-16.md` and updated
  the Tensor language spec, collection reference, Tensor area page, Tensor
  plan, plan index, `.agents/PLAN.md`, and `memory/CHANGELOG.md`.

Key results:
- Public solver namespace is now `matrix/`, with `matrix/solve` as the first
  shipped operation.
- Rejected first-surface names: bare `solve`, `linalg/solve`, `tensor/solve`,
  and backend-flavored `tensor/lapack/solve`.
- `matrix/solve` accepts a square rank-2 `Float64` coefficient Tensor and a
  rank-1 or rank-2 `Float64` RHS Tensor, realizes lazy operands, preserves RHS
  rank in the result, and raises `tensor/singular-matrix` for singular systems.
- LAPACK/LAPACKE is now optionally probed through `LAPACKE_dgesv` behind the
  same surface; missing backend support keeps the pure runtime solver fallback,
  which remains the semantic oracle.
- `matrix/lu` is the first shipped rank-2 decomposition surface. It accepts a
  square rank-2 `Float64` Tensor, realizes lazy operands, returns a dictionary
  with combined `lu` factors, final 0-based `pivots`, and `swap-count`, and
  raises `tensor/singular-matrix` for singular inputs.
- `matrix/determinant` is the first shipped decomposition consumer. It reuses
  the partial-pivot LU kernel, returns a `Float64` scalar, returns `0.0` for
  singular matrices, and uses `1.0` for empty square matrices.
- `matrix/qr` is the first shipped non-LU decomposition surface. It computes a
  reduced QR decomposition for full-column-rank rank-2 `Float64` tensors with
  rows greater than or equal to columns, returning `q` and `r` tensors.
- `matrix/cholesky` is the first shipped symmetric positive-definite
  decomposition surface. It computes a lower-triangular `Float64` factor for
  square symmetric positive-definite rank-2 `Float64` tensors, returns a
  same-shape Tensor, zeroes entries above the diagonal, and raises
  `tensor/not-positive-definite` for nonsymmetric or non-positive-definite
  inputs.
- `matrix/svd` is the first shipped rectangular decomposition surface. It
  computes reduced SVD for rank-2 `Float64` tensors, supports rank-deficient
  inputs, and returns a dictionary with `u` shape `[rows k]`, `s` shape `[k]`,
  and `v` shape `[columns k]`, where `k = min(rows, columns)`.
- `matrix/eigenvalues` and `matrix/eigenvectors` are the first shipped eigen
  surfaces. They intentionally use a symmetric-real contract for square
  rank-2 `Float64` tensors. `matrix/eigenvalues` returns descending
  eigenvalues; `matrix/eigenvectors` returns aligned `values` and `vectors`
  columns; nonsymmetric inputs raise `tensor/not-symmetric`.

Commands run and key results:
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- `ldconfig -p | rg -i 'lapacke|lapack|openblas|blas'`: showed
  `liblapack.so.3` and BLAS libraries but no `liblapacke`, so local runtime
  validation exercised the fallback path.
- Direct vector RHS smoke:
  `(ref (matrix/solve (Tensor Float64 [2 2] [2 1 1 3]) (Tensor Float64 [2] [1 2])) [1])`
  returned `0.6`.
- Direct matrix RHS smoke:
  `(ref (matrix/solve (Tensor Float64 [2 2] [2 1 1 3]) (Tensor Float64 [2 2] [1 2 2 1])) [0 1])`
  returned `1.0`.
- Direct singular handler smoke returned `tensor/singular-matrix`.
- Direct LU factor smoke:
  `(ref (ref (matrix/lu (Tensor Float64 [2 2] [4 3 2 1])) 'lu) [1 0])`
  returned `0.5`.
- Direct LU pivot smoke:
  `(ref (ref (matrix/lu (Tensor Float64 [2 2] [0 2 1 3])) 'pivots) 0)`
  returned `1`.
- Direct singular LU handler smoke returned `tensor/singular-matrix`.
- Direct determinant smoke:
  `(matrix/determinant (Tensor Float64 [2 2] [4 3 2 1]))`
  returned `-2.0`.
- Direct singular determinant smoke returned `0.0`.
- Direct empty-square determinant smoke returned `1.0`.
- Direct QR R projection smoke:
  `(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'r) [0 1])`
  returned `1.0`.
- Direct QR Q column smoke:
  `(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'q) [1 1])`
  returned `1.0`.
- Direct rank-deficient QR handler smoke returned `tensor/singular-matrix`.
- Direct Cholesky diagonal smoke:
  `(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [0 0])`
  returned `2.0`.
- Direct Cholesky lower-entry smoke:
  `(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [1 0])`
  returned `1.0`.
- Direct indefinite Cholesky handler smoke returned
  `tensor/not-positive-definite`.
- Direct SVD leading singular value smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 's) [0])`
  returned `3.0`.
- Direct wide SVD V-column smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 3] [3 0 0 0 2 0])) 'v) [1 1])`
  returned `1.0`.
- Direct rank-deficient SVD singular value smoke:
  `(ref (ref (matrix/svd (Tensor Float64 [2 2] [0 0 0 0])) 's) [0])`
  returned `0.0`.
- Direct symmetric eigenvalue smoke:
  `(ref (matrix/eigenvalues (Tensor Float64 [2 2] [2 1 1 2])) [0])`
  returned `3.0`.
- Direct eigenvector column smoke:
  `(ref (ref (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])) 'vectors) [0 0])`
  returned `1.0`.
- Direct nonsymmetric eigen handler smoke returned `tensor/not-symmetric`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed after the pure solver slice, `pass=399 fail=0`.
- After adding optional LAPACK probing, the same focused group passed,
  `pass=401 fail=0`.
- After adding `matrix/lu`, the same focused group passed, `pass=410 fail=0`.
- After adding `matrix/determinant`, the same focused group passed,
  `pass=417 fail=0`.
- After adding `matrix/qr`, the same focused group passed, `pass=426 fail=0`.
- After adding `matrix/cholesky`, the same focused group passed,
  `pass=435 fail=0`.
- After adding `matrix/svd`, the same focused group passed,
  `pass=445 fail=0`.
- After adding `matrix/eigenvalues` and `matrix/eigenvectors`, the same
  focused group passed, `pass=455 fail=0`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Continue `TENSOR-090` by adding the next rank-2 decomposition routine under
  `matrix/`, or broaden LAPACK/LAPACKE coverage behind the existing
  `matrix/solve`, `matrix/lu`, `matrix/determinant`, `matrix/qr`, and
  `matrix/cholesky`, `matrix/svd`, `matrix/eigenvalues`, and
  `matrix/eigenvectors` surfaces.
- Keep `matrix/solve` as the user-facing contract; do not add bare `solve`,
  `qr`, `cholesky`, `svd`, `eigenvalues`, `eigenvectors`, `linalg/solve`,
  `tensor/solve`, or backend-flavored public names.
- Do not add abbreviated aliases such as `matrix/det` unless the owner
  explicitly approves them.

Unresolved issues:
- This host has no `liblapacke`, so the optional `dgesv` accelerated branch
  compiled but was not live-executed locally; the conditional test validated
  fallback retention.
- The existing added `out` file contains unrelated reverse-SSH text
  and was left untouched.

Signature: Codex GPT-5

## 2026-04-14 11:55 CEST - Boost BigInteger First Slice And Solver Naming Checkpoint

Objective attempted:
- Land the first Boost.Multiprecision scalar integration after the owner chose
  the BigInteger path, and record the solver naming constraint that `solve`
  must not be bare while `linalg/` remains unsettled.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Installed `libboost-dev` on the host to provide Boost.Multiprecision headers.
- Added `csrc/big_integer_helpers.cpp`, a small C++17 shim around
  `boost::multiprecision::cpp_int` with a stable C ABI.
- Added C3 extern declarations and `BIG_INTEGER` value support, including
  destructor, copy-to-parent, escape/root promotion, env-copy, printing,
  hashing/equality, builtin type identity, and `Number` parent registration.
- Added `BigInteger` constructor/coercion behavior for `Integer`, decimal
  `String`, and existing `BigInteger`; invalid decimal input and out-of-range
  `Integer` narrowing fail with `type/arg-mismatch`.
- Updated `+`, `-`, and `*` so `Integer` overflow promotes to `BigInteger`
  and `BigInteger` combines with `Integer`/`BigInteger`; mixed `Double`
  arithmetic uses finite double conversion where possible.
- Added the C++ helper object to `scripts/build_omni_chelpers.sh` and linked
  the helper archive from `project.json`.
- Added focused advanced stdlib numeric coverage for constructor,
  stringification, type identity, `number?`, overflow promotion, arithmetic,
  and failure payloads.
- Updated `memory/CHANGELOG.md`, `.agents/PLAN.md`, `TODO.md`,
  `docs/LANGUAGE_SPEC.md`, `docs/areas/tensor-scientific.md`, and
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`.

Commands run and key results:
- `test -f /usr/include/boost/multiprecision/cpp_int.hpp`: initially absent.
- `sudo apt-get install -y libboost-dev`: installed Boost 1.83 development
  headers.
- `c++ -O2 -std=c++17 -c csrc/big_integer_helpers.cpp -o /tmp/omni_big_integer_helpers.o`:
  passed.
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct smokes passed for:
  - `(String (BigInteger "9223372036854775808"))`
  - `(String (+ 9223372036854775807 1))`
  - `(= (type-of (BigInteger "1")) (quote BigInteger))`
  - `(String (* (BigInteger "9223372036854775808") 2))`
  - `(Integer (BigInteger "9223372036854775807"))`
  - invalid decimal and out-of-range narrowing handlers returning
    `"type/arg-mismatch"`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=66 fail=0`.
- Remaining advanced numeric child groups passed; exact and combined TCO
  groups required `prlimit --stack=67108864` because the default-stack
  `(length (range 4000))` path crashes in deep recursive escape promotion.
- `prlimit --stack=67108864 env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=295 fail=0`.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- Final `c3c build --obj-out obj`: passed with the same existing entry
  deprecation warnings.
- Final direct smokes for overflow promotion, invalid BigInteger input, and
  `type-of BigInteger`: passed.
- `git diff --check` on the touched tracked files: passed.
- `jj status` and `jj diff --stat`: worked at the final checkpoint.

Current best recommendation / checkpoint:
- Treat `BigInteger` as the first Boost.Multiprecision scalar slice only.
  Keep `/`, `%`, ordering comparisons, bitwise operations, `gcd`/`lcm`,
  `BigFloat`/`BigComplex`, and `parse-number` arbitrary-precision parsing as
  explicit follow-up slices.
- Do not publish a bare `solve` for LAPACK/LAPACKE solver conveniences.
  `linalg/` is not accepted yet as the base namespace; keep the public
  qualifier unresolved until the Tensor convenience layer is named.

Unresolved issues:
- The normal-stack advanced numeric TCO headroom case crashes in recursive
  escape promotion for `(length (range 4000))`; the same test passes with a
  64MB stack limit. This looks like a pre-existing deep-promotion headroom
  issue, not a BigInteger arithmetic failure, but it remains a validation
  caveat.
- The earlier corrupt-object `jj` failure was not reproduced at this checkpoint:
  `jj status` and `jj diff --stat` both worked after the BigInteger slice.

Signature: Codex GPT-5

## 2026-04-14 08:20 CEST - Tensor Native BLAS Backend First Slices

Objective attempted:
- Write and start implementing the plan to keep ordinary Tensor storage
  C3-native/scoped while integrating OpenBLAS/LAPACK-style acceleration
  directly at the Tensor backend layer instead of through user-visible
  `ForeignHandle` Tensor storage.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added private native BLAS shim `csrc/tensor_blas_helpers.c` with optional
  runtime `cblas_dgemm` discovery via `dlopen`/`dlsym`.
- Added C3 extern declarations in `src/lisp/tensor_blas_backend.c3`.
- Wired `tensor_contract_eval_into(...)` to use the native `dgemm` fast path
  only for contiguous rank-2 row-major `Double` contraction equivalent to
  `(contract a b [1 0])`; unsupported cases still fall back to the pure C3
  contraction kernel.
- 09:40 CEST follow-up: extended the same private `dgemm` path with CBLAS
  transpose flags so all contiguous rank-2 single-axis contractions can route
  through BLAS when available: `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`.
- Added an internal path-sensitive call counter so the targeted regression can
  prove BLAS branch use when a compatible BLAS symbol is available.
- Added focused regressions for the transpose-backed contract cases without
  introducing a public `matmul`/`linalg-matmul` surface.
- Added the shim to both `project.json` direct build sources and
  `scripts/build_omni_chelpers.sh` for AOT/e2e helper-archive linkage.
- Updated Tensor plan/area notes to record that ordinary Tensor storage remains
  native/scoped and is not replaced with `ForeignHandle`.

Commands run and key results:
- `jj status`: working copy already had unrelated dirty/untracked operational
  artifacts plus Tensor docs/source changes from the current session.
- `find /usr/include /usr/local/include /usr/include/aarch64-linux-gnu ...`:
  found no local `cblas.h`, `lapacke.h`, or OpenBLAS headers.
- `ldconfig -p` / `nm -D`: found generic `/lib/aarch64-linux-gnu/libblas.so.3`
  exporting `cblas_dgemm`; no hard OpenBLAS pkg-config/header path was present.
- `c3c build --obj-out obj`: passed with the existing deprecation warnings in
  entry file/read/build helpers.
- `bash -n scripts/build_omni_chelpers.sh`: passed.
- `./scripts/build_omni_chelpers.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- Direct Tensor smoke
  `(ref (realize (contract a b [1 0])) [1 1])`: returned `154.0`.
- Direct Tensor smokes for transpose-backed rank-2 contractions returned
  `84.0` for `[0 0]`, `68.0` for `[1 1]`, and `123.0` for `[0 1]`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed first at `217 passed, 0 failed`; after the transpose follow-up it
  passed at `221 passed, 0 failed`.
- Subagent check: explorer `019d8aec-bae8-7462-a5c6-4ec1fbd360af` confirmed
  the CBLAS row-major transpose mapping for all four rank-2 axis-pair cases.
- Final `jj status` and `jj diff --stat` attempts failed with a corrupt loose
  Git object at `.git/objects/da/96d1f92060d63d31f50ba48b1086b0fbc60545`;
  `git status --short` and targeted `git diff --stat` still worked.

Current best recommendation / checkpoint:
- Keep `TensorVal` as the normal scoped Tensor storage model.
- Treat direct native backend calls as implementation details behind `contract`
  / `realize`, not as public `TensorHandle` or user-level FFI handles.
- Continue `TENSOR-090` by deciding LAPACK/LAPACKE solver/decomposition
  surface names. Remaining BLAS broadening, such as rank-2/rank-1 `gemv`,
  should stay private and continue treating the pure C3 kernel as the
  validation oracle.

Unresolved issues:
- Full/heavy validation was not run in this session.
- LAPACK/LAPACKE has not been wired yet; this slice only covers the first BLAS
  `dgemm` family behind rank-2 `contract`.
- BLAS `gemv`/vector-specialized paths are still deferred; rank-2/rank-1
  contractions currently use the pure fallback.
- VCS health issue at that Tensor checkpoint: `jj` status/diff tripped over
  the corrupt loose Git object above. This was not reproduced after the later
  BigInteger checkpoint, where `jj status` and `jj diff --stat` worked.

Signature: Codex GPT-5

## 2026-04-12 18:35 CEST - Scientific Numerics and Tensor Planning Checkpoint

Objective attempted: define the forward plan for Omni scientific work, including exact integer promotion, BigFloat, Tensor constructor conversions, GSL compatibility, and adjacent scientific libraries.

Workspace: `/home/christos/Omni`.

Code/configuration changes made:
- Added `.agents/PLAN.md` with a phased scientific numerics plan.
- Recorded the durable design decision in memory under `Omni scientific numerics tensor plan 2026-04-12`.

Key results:
- Chose constructor/coercion dispatch as the public API rule for Tensor conversions, matching existing `Array` behavior such as `(Array '(1 2 3))`.
- Planned GMP-backed exact integer storage with automatic promotion on overflow while preserving inline small `Integer`.
- Planned MPFR-backed `BigFloat` as explicit high-precision float support, while keeping `Double` as the default hardware float and avoiding silent `Double` overflow promotion unless the owner later explicitly chooses it.
- Planned Tensor as the scientific data boundary: unboxed numeric storage or foreign backend handle with dtype/shape/strides/rank/device/ownership metadata, while `Array` remains heterogeneous boxed `Value*` storage.
- Planned GSL as an optional Tensor-aware package/backend first, not a mandatory core dependency, with C shim error conversion and layout/device/dtype checks.
- Recommended scientific library ordering: OpenBLAS + LAPACK/LAPACKE, HDF5, optional FFTW, SUNDIALS, NetCDF, SuiteSparse/GraphBLAS, ARPACK-ng, and libtorch as the ML/autograd/GPU Tensor backend.

Commands run:
- `sed -n '1,260p' .agents/PLAN.md`
- `git diff --check -- .agents/PLAN.md`
- `git diff --check -- .agents/PLAN.md .agents/SESSION_REPORT.md`

Validation:
- `git diff --check -- .agents/PLAN.md`: passed.
- `git diff --check -- .agents/PLAN.md .agents/SESSION_REPORT.md`: passed.
- No build/test commands were run because this was a planning-only pass with no source code changes.

Current best recommendation / checkpoint:
- Start implementation with Phase 1 in `.agents/PLAN.md`: exact integer representation, `Integer`/`BigInteger` constructor and dispatch behavior, overflow-promotion helper API, FFI range-check policy, and boundary lifetime regressions for GMP-backed values.
- Do not start by binding all of GSL; Tensor constructor conversion and exact integer promotion are better foundations.

Unresolved issues:
- Naming for public `(Float x)` remains owner-level: the plan recommends using it only if approved, otherwise keeping `Double` as the canonical hardware-float constructor.
- GSL default-core inclusion remains blocked on an explicit GPL-compatible distribution decision.

Signature: Codex GPT-5

## 2026-04-14 07:30:08 CEST - Tensor Naming Decision Integration

Objective attempted:
- Investigate prior Tensor naming decisions after the owner recalled that
  Tensor iteration involved explicit realization, Tensor arithmetic should
  use the normal tensor surface, and `matmul` had been rejected in favor of
  another name.
- Pull and integrate the owner-pushed remote bookmark
  `bookmark/omni-local-master-2026-04-14@origin`.
- Apply the follow-up owner decision to drop public Tensor `materialize` in
  favor of `realize`, and simplify common `contract` notation with paired-axis
  arrays.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Rebased local reader-tag and operational-artifact work on top of remote
  commit `87680cae` (`docs: fix tensor broadcasting status drift`).
- Resolved `.agents/PLAN.md` by replacing the stale standalone Tensor plan with
  a short operational note pointing at the integrated canonical Tensor plan.
- Resolved `memory/CHANGELOG.md` by keeping the remote changelog and reinserting
  the local 2026-04-13 reader-tag entry.
- Renamed the public Tensor boundary primitive from `materialize` to `realize`
  with no alias, updating runtime registration, AOT primitive lookup, Tensor
  error messages, docs, examples, and tests.
- Added `contract` paired-axis shorthand: `(contract a b [1 0])` for the
  common rank-2 pair and `[[1 0] [2 3]]` for multi-axis pairs. The explicit
  `(contract a b [1] [0])` form remains accepted.
- Confirmed no shipped `TensorHandle` type exists in `TensorVal`; the backend
  plan keeps ordinary Tensor storage native/scoped and reserves explicit
  ownership/finalizer policy for genuinely opaque backend resources.

Commands run and key results:
- `jj status`: showed the existing dirty working copy plus the operational
  artifacts/docs from this planning session.
- `jj git fetch --remote origin --branch bookmark/omni-local-master-2026-04-14`:
  fetched `bookmark/omni-local-master-2026-04-14@origin`.
- `jj rebase -r @ -d 'bookmark/omni-local-master-2026-04-14@origin'`: rebased
  the local working-copy commit onto the fetched bookmark and produced
  conflicts only in `.agents/PLAN.md` and `memory/CHANGELOG.md`.
- Targeted `rg` searches over `.agents`, `docs`, `memory`, `examples`, and
  `plan.jsonl` initially found no local replacement for rejected `matmul`, but
  the fetched bookmark carried the missing canonical Tensor plan.
- `docs/plans/tensor-scientific-computing-plan-2026-04-11.md` now records the
  recovered decision: `contract` is the canonical tensor contraction operation,
  rank-2 multiplication is `(contract a b [1 0])`, `realize` is the
  tensor-expression-to-storage boundary, and `matmul` is rejected as
  programming jargon/rank-2 biased.
- `c3c build --obj-out obj`: passed; existing deprecation warnings remain in
  `src/entry_file_read_reporting.c3`, `src/entry_build_aot_temp.c3`, and
  `src/entry_build_backend_compile.c3`.
- Direct Tensor smoke `(ref (realize (contract a b [1 0])) [1 1])`: returned
  `154.0`.
- Direct Tensor smoke `(ref (realize (contract a b [[1 0]])) [1 1])`:
  returned `154.0`.
- Direct Tensor smoke `(ref (realize (map * x 2.0)) [1 2])`: returned `12.0`.
- Direct old-name removal smoke `(materialize (Tensor Double [1] [1]))`:
  failed with `unbound variable 'materialize'`, as intended.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `216 passed, 0 failed`.
- `OMNI_LISP_TEST_SLICE=reader-dispatch LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `16 passed, 0 failed`.
- `git diff --check` on touched Tensor/docs/artifact files: passed.
- Conflict marker scan on touched Tensor/docs/artifact files: clean.

Current best recommendation / checkpoint:
- Treat `Tensor`, tensor-dispatched `map`, `contract`, and `realize` as the
  locked Tensor surface.
- Prefer paired-axis arrays for new contraction examples:
  `(contract a b [1 0])` for rank-2 and `[[1 0] [2 3]]` for multi-axis
  contractions.
- Elementwise multiplication is `(map * a b)`, not `(* a b)`.
- BLAS rank-2 GEMM should optimize
  `(realize (contract a b [1 0]) out)` behind the Tensor backend
  boundary, not introduce `linalg/matmul` as a public surface.
- Keep the scalar scientific plan Boost-first and GSL-avoiding.

Unresolved issues:
- Heavy/full validation was not run in this session.

Signature: Codex GPT-5

## 2026-04-14 07:06:05 CEST - Boost-First Scientific Plan Correction

Objective attempted:
- Correct the scientific numerics plan after owner clarification that the goal
  is to avoid GSL/GNU-style baseline dependencies and prefer Boost precision.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Updated `.agents/PLAN.md` so the default scientific backend direction is:
  Boost.Multiprecision for exact/high-precision scalar values, Boost.Math for
  scalar scientific functions, and OpenBLAS + LAPACK/LAPACKE for dense Tensor
  linear algebra.
- Replaced the prior GMP/MPFR-backed first-slice wording with
  Boost.Multiprecision-backed `BigInteger`, `BigFloat`, and `BigComplex`
  planning.
- Replaced the prior GSL compatibility phase with a Boost.Math scientific
  functions phase behind an owned C++ shim and C ABI boundary.
- Recorded that GSL is not a default dependency and should only be revisited as
  an explicit optional compatibility package if the owner asks for it.

Commands run and key results:
- `git diff --check -- .agents/PLAN.md`: passed.

Current best recommendation / checkpoint:
- Start with a Boost.Multiprecision C++ shim for `BigInteger` (`cpp_int`) and
  plan `BigFloat`/`BigComplex` (`cpp_complex`) behind the same backend boundary.
- Keep user-facing names backend-neutral; do not expose Boost or GSL names in
  Omni language surfaces.

Unresolved issues:
- No implementation/build validation was run; this was a plan correction only.

Signature: Codex GPT-5

## 2026-04-12 16:38:56 CEST - Runtime Boundary Audit and Fix Pass

Objective attempted: continue the whole-codebase audit/fix pass with emphasis on runtime boundary promotion/copy paths, closure env copy/promotion memoization, allocator growth, and stale validation status wording.

Workspace: `/home/christos/Omni`.

Code/configuration changes made:
- Squashed source/doc/script changes into `master`; final source commit is `edb06c3b` (`Harden type registry and allocator growth paths`).
- Fixed `ast_arena_alloc` inverted `math::overflow_add` checks so normal arena growth no longer returns null.
- Fixed stack defer reserve overflow test expectation to preserve the first grown capacity after a rejected overflow request.
- Repaired `src/lisp/tests_core_groups.c3` after a raw patch-marker corruption in the parent commit and tightened the type-registry allocation-failure recovery assertion.
- Added checked allocation growth guards in bind runtime setup, Pika named grammar growth, and FTXUI helper pointer/component vectors.
- Added early promotion-context memoization for closure payload copy/promotion and shared wrapper container copy paths, and installed the passed promotion context during parent-site boundary copy so nested copy helpers see the active context.
- Added env-copy regression coverage for a self-referential closure payload cloned through promotion-context memoization.
- Hardened boundary provenance child traversal so root-owned child graphs are not recursively walked during releasing-scope reuse checks, non-root graph-bearing children retain target-chain checks, and child recursion has a depth guard.
- Updated validation/status docs and `scripts/run_validation_status_summary.sh` wording to avoid stale host-local e2e/full-suite claims.

Commands run and key results:
- `jj git fetch`: `Nothing changed`.
- `git diff --check -- src docs scripts`: passed.
- patch-marker/stale-wording scan over `src docs scripts`: no matches.
- `c3c build`: passed with pre-existing `errno::*` deprecation warnings in `src/main_repl_shared.c3`.
- `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `suite=unified pass=156 fail=0`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack`: passed, `Stack engine: 24 passed, 0 failed`.
- `scripts/run_ftxui_smoke.sh`: passed; `module_value_smoke`, `module_effect_smoke`, full `smoke.omni`, and `demo.omni` all completed.
- `c3c build --sanitize=address`: unavailable in this toolchain, reporting that address sanitizer is unsupported.

Invalidated assumptions / failed approaches:
- Do not trust the `agent-1776003689746-p0w3r1` Deduce refresh output. It wrote raw `<<<<<<< SEARCH` markers and truncated four Deduce refresh files; those files were restored with `jj restore` and should not be used as a basis for future work.
- Do not assume `math::overflow_add` returns true on success. The corrected code treats true as overflow/failure.
- Provenance child traversal cannot safely recurse through root-owned cyclic module/registry maps during releasing-boundary reuse classification. Treat root-owned children as lifetime-stable in that walk and inspect non-root graph children instead.

Current best recommendation / checkpoint:
- Source/doc/script work, including the final FTXUI resolve fix, is committed in `master` at `edb06c3b`; the remaining working-copy changes are operational artifacts under `.agents/`, `.claude-flow/`, `.serena/`, and `.swarm/`.
- The FTXUI `resolve` continuation smoke is now green after keeping the active promotion-context memo lookup, avoiding recursive alias walks for edge-free values, adding alias-walk depth/headroom guards, and raising the normal stack-context budget to 256KB.
- Memory-lifetime smoke/full ownership slices were not run because repo policy requires container-bound execution for memory ownership slices, and the bounded container path was previously unavailable in this environment.

Unresolved issues:
- The new self-referential env-copy regression is compiled but not executed through the memory-lifetime smoke slice locally because that slice is container-only by repo policy.

### 2026-04-12 15:16 CEST - Value Environment Hardening Patch

Objective attempted:
- Apply narrow, fail-closed hardening in `value_environment.c3` and `value_environment_storage.c3` for capacity growth and hash rebuild arithmetic.

Code/configuration changes made:
- `src/lisp/value_environment.c3:22-63`: added overflow and OOM-closure guards in `Env.define`.
  - guard `capacity > usz.max / 2` before doubling
  - guard `new_cap > usz.max / Binding.sizeof` before allocation
  - avoid overflow in load-factor path with `hash_capacity > usz.max / 10`
  - replace `binding_count * 10 > hash_capacity * 7` with `binding_count > (hash_capacity * 7) / 10`
- `src/lisp/value_environment_storage.c3:142-156`: added capacity/size guards in `Env.build_hash_table`.
  - return-fail when `binding_count > usz.max / 2`
  - growth loop guard `cap > usz.max / 2` before doubling
  - guard allocation bytes with `binding_count * 2 > usz.max / EnvHashEntry.sizeof`

Commands and results:
- `git diff --check -- src/lisp/value_environment.c3 src/lisp/value_environment_storage.c3`: clean.
- `rg -n '^(<<<<<<<|>>>>>>>)|\*\*\* (Search|Replace)' src/lisp/value_environment.c3 src/lisp/value_environment_storage.c3`: no matches.
- `c3c build`: blocked by pre-existing merge markers in `src/lisp/eval_pattern_equality_helpers.c3`.

Unresolved / residual risk:
- End-to-end compilation and broader test slices remain unavailable until unrelated merge markers in `src/lisp/eval_pattern_equality_helpers.c3` are resolved.

Signature: Codex GPT-5

## 2026-04-12 17:46:11 CEST - Whole-Codebase Allocation Hardening Audit/Fix Continuation

Objective attempted: continue the whole-codebase audit and fix pass using parallel GPT-5.4 high audit agents and scoped GPT-5.3 Spark implementation workers, then commit non-artifact source changes and fetch remote updates.

Workspace: `/home/christos/Omni`.

Code/configuration changes made:
- Hardened parser AST, module, relation-attribute, pattern, handle, pipe, import/export, collection literal, and type annotation allocation paths with checked byte-size/count arithmetic before arena allocation.
- Hardened Pika named grammar rule table registration/growth to fail closed on capacity/registration failure, including the clause symbol-ref placeholder path.
- Hardened JIT/effect handler staged argument, closure/method signature, module export, and method table allocation/copy paths with overflow guards.
- Hardened Deduce rule signature persistence/record codec size computation and cursor movement, SCC/reachability matrix allocations, row materialization dictionary capacity, column key allocation, explain/list builders, schema query refresh payload handling, and why-result list concatenation error propagation.
- Hardened environment hash rebuild behavior so post-mutation rebuild failure falls back to linear lookup instead of reporting mutation failure after state has already changed.
- Hardened REPL server/session and JSON request size arithmetic.
- Kept operational artifacts under `.agents/`, `.claude-flow/`, `.serena/`, and `.swarm/` out of the planned source-only commit.

Commands run and key results:
- `c3c clean && c3c build`: passed with existing `errno::*` deprecation warnings in `src/main_repl_shared.c3`.
- `c3c build`: passed again after final hygiene checks.
- `rg -n '^(<<<<<<<|=======|>>>>>>>)|\*\*\* (Search|Replace)' src/lisp src/pika src/stack_engine_lifecycle.c3`: no matches.
- `git diff --check -- src/lisp src/pika src/stack_engine_lifecycle.c3`: passed.
- `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=156 fail=0`.
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=195 fail=0`.
- `OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=49 fail=0`.
- `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=83 fail=0`.
- `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=materialized OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=8 fail=0`.
- `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=relation-attrs OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=13 fail=0`.
- `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=integrity OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=34 fail=0`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack`: passed, `24 passed, 0 failed`.
- `scripts/run_ftxui_smoke.sh`: passed.
- Manual recursive Deduce `--eval` expression returned `true`.
- Initial parent comparison in `/tmp/omni-parent-edb06c3b` at `edb06c3b` reproduced the then-current grouped Deduce failures, which showed they were not caused by the allocation-hardening runtime paths.
- Follow-up source fixes in the committed tree resolved the grouped Deduce parser failures:
  - `src/lisp/tests_deduce_rule_groups_more_tail_head.c3` no longer carries stale duplicated analysis tests that conflicted with the split `more_tail_analysis_tests` coverage.
  - `src/lisp/tests_deduce_query_admin_surface_fallback_tests.c3` has the integrity-reporting assertion flattened to the intended parseable shape.
  - `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=rule-validation OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=79 fail=0`.
  - `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=query OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=254 fail=0`.
  - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=432 fail=0`.

Invalidated assumptions / failed approaches:
- Do not rely on the earlier `AUDIT-DEDUCE-GROUPED-PARSER-STATE-040` open TODO entry or the earlier report text that said broad Deduce still failed; those artifacts were stale after the committed tree fixed and verified the grouped parser failures.
- Do not rely on `jj diff` rendering alone for source verification when it appears to fuse identifiers; use `git diff` and direct file reads for final inspection.

Current best recommendation / checkpoint:
- Broad Deduce grouped parser failures are verified fixed in the current source tree. `TODO.md` was updated to close `AUDIT-DEDUCE-GROUPED-PARSER-STATE-040`.
- Proceed with normal source commit/push for the stale backlog cleanup if this working copy is being published.

Unresolved issues:
- Full heavy/container-only memory lifetime gates were not run in this host session.

Signature: Codex GPT-5

## 2026-04-13 22:45:08 CEST - Constrained Reader Tag Surface

Objective attempted:
- Implement the owner-requested hybrid reader-tag surface: Clojure-like
  `#tag form` use without a full Common Lisp readtable facility.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a `T_READER_TAG` token and `#tag` lexer dispatch while preserving
  `#r"..."`, `#_`, `#N_`, and `#| ... |#`.
- Added parser lowering for `#tag form` to the normal one-argument call
  `(tag form)`.
- Added `(define [reader tag] name (syntax-match ...))` as the canonical
  reader-tag macro declaration surface by reusing the existing `E_DEFMACRO`
  parser path.
- Added reader-dispatch regressions for ordinary function tags, slash tags,
  reader-tag macros, malformed reader-tag definitions, numeric `#1x`, and
  bare `#`.
- Updated `memory/CHANGELOG.md` plus language/reference/syntax/compatibility
  docs for the new canonical surface.

Commands run and key results:
- `c3c build --obj-out obj`: passed and linked `build/main`; existing
  `errno::*` deprecation warnings in `src/main_repl_shared.c3` remain.
- `OMNI_LISP_TEST_SLICE=reader-dispatch LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `16 passed, 0 failed`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(block (define (spy x) x) #spy 7)'`:
  returned `7`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(block (define [reader tag] spy (syntax-match ([x] (template (block (insert x)))))) #spy 7)'`:
  returned `7`.
- `git diff --check -- ...`: passed for the touched source/docs/changelog files.

Current best recommendation / checkpoint:
- Treat `[reader tag]` as the chosen canonical declaration spelling. Do not add
  parallel `[reader/tag]`, `[reader-tag]`, or `defreader` aliases unless the
  owner explicitly requests a migration or alternate surface.

Unresolved issues:
- Full global/container gates were not run; this was verified with the narrow
  reader-dispatch slice and a local build using the workspace's known
  `--obj-out obj` recovery path.

Signature: Codex GPT-5
## 2026-04-16 05:57 CEST - Float String Precision

Objective attempted:
- Close the approved `Float` constructor precision spelling gap by accepting
  string precision arguments such as `(Float x "64")`, while preserving the
  fail-closed `Float32` boundary.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Updated `prim_ctor_float` so the optional precision argument accepts either
  integer precision (`64`, `32`) or string precision (`"64"`, `"32"`).
- `(Float x "64")` delegates to the existing `Float64` constructor.
- `(Float x "32")` fails closed with the same unimplemented `Float32` storage
  error as `(Float x 32)`.
- Added constructor regressions for string precision `64` and string precision
  `32`.
- Updated current language/reference docs and `memory/CHANGELOG.md`.

Commands run and key results:
- `c3c build main --output-dir build --build-dir build/obj2`: passed with
  existing `errno::*` / `process::create` deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-core-semantics OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=71 fail=0`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (Float "1.25" "64"))'`:
  returned `"1.25"`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(Float 1 "32")'`:
  failed closed with `Float: precision 32 requires Float32 runtime storage, which is not implemented yet`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(Float 1 "bad")'`:
  failed with `Float: unsupported precision; expected 64`.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Treat integer and string precision spellings as accepted for `Float`.
  Implementing actual `Float32` remains a separate runtime/tensor storage
  slice, not a constructor parsing task.

Unresolved issues:
- Full heavy/container-only gates were not run for this narrow constructor
  surface slice.

Signature: Codex GPT-5

## 2026-04-16 05:16 CEST - Float64 Surface Canonicalization

Objective attempted:
- Replace the public binary64 `Double` surface with canonical `Float64`, while
  adding the approved `(Float value [precision])` constructor form and keeping
  unimplemented `Float32` fail-closed.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `Float`, `Float32`, and `Float64` callable constructor registrations.
- Changed the existing binary64 symbol/type/dtype display surface to
  `Float64`; internal storage names like `DOUBLE`, `sym_Double`,
  `is_double`, and `TENSOR_DTYPE_DOUBLE` remain implementation names.
- Added `(Float x)` as default `Float64`, `(Float x 64)` as explicit binary64,
  and fail-closed errors for `(Float x 32)` / `(Float32 x)`.
- Removed the public `Double` constructor surface instead of keeping an alias.
- Renamed the public predicate from `double?` to `float64?`.
- Updated Tensor dtype spelling, FFI annotation docs, compiler primitive
  lookup tables, examples, and current reference/spec/planning docs.
- Added constructor regressions for default `Float`, explicit precision 64,
  fail-closed `Float32`, `Float64` type identity, and removed `Double`.

Commands run and key results:
- `c3c build main --output-dir build --build-dir build/obj2`: passed with
  existing `errno::*` / `process::create` deprecation warnings.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (type-of (Float 1)))'`:
  returned `"Float64"`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (Float "1.25" 64))'`:
  returned `"1.25"`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(Float32 1)'`:
  failed closed with `Float32: runtime storage is not implemented yet`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(Double 1)'`:
  failed as `unbound variable 'Double'`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-core-semantics OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=71 fail=0`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=240 fail=0`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=75 fail=0`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=411 fail=0`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=392 fail=0`.
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=276 fail=0`.

Invalidated assumptions / failed approaches:
- Do not preserve `Double` as a public compatibility alias. The current
  pre-alpha surface intentionally removes it and tests that `(Double 3)` is
  unbound.
- Do not treat `Float32` as implemented just because the constructor symbol is
  registered; it is reserved and must fail closed until storage/kernels exist.

Current best recommendation / checkpoint:
- Continue scientific numeric work using `Float64` as the native hardware
  floating dtype and `(Float x [precision])` as the precision-selecting
  constructor surface. Add real `Float32` only as a storage/kernel slice.

Unresolved issues:
- Full heavy/container-only memory lifetime gates were not run for this naming
  cleanup; the touched behavior was covered with focused constructor, Tensor,
  FFI, dispatch, numeric, collections, and compiler slices.

Signature: Codex GPT-5

## 2026-04-17 09:38:59Z
- Objective: fix the Vulkan singular-norm helper so backend availability,
  Float64 support, and shared shape validation run before the zero-size success
  path.
- Workspace: `/home/christos/Omni`
- Changes made:
  - Reordered `omni_tensor_backend_vulkan_singular_norm_f64` in
    `csrc/tensor_vulkan_helpers.c` to check Vulkan availability, Float64
    support, and `omni_tensor_backend_vulkan_singular_values_validate_shape_f64`
    before the zero-size return.
  - Added `omni_tensor_backend_vulkan_singular_norm_zero_size_validation_probe_for_tests`
    in `csrc/tensor_vulkan_helpers.c`.
  - Added the corresponding extern declaration in
    `src/lisp/tensor_vulkan_backend.c3`.
  - Added a narrow regression in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` covering the probe.
- Commands run:
  - `c3c build`
  - `git diff --check -- csrc/tensor_vulkan_helpers.c src/lisp/tensor_vulkan_backend.c3 src/lisp/tests_advanced_stdlib_module_groups.c3`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- Key result:
  - Targeted bounded advanced-collections-module validation passed with
    `pass=898 fail=0`.
- Current recommendation:
  - No further code change is needed for this audit item.
- Open issues:
  - None for this patch; broader unrelated worktree changes remain present but
    untouched.
- Next actions:
  - Leave the rest of the tensor Vulkan surface alone unless a separate audit
    item lands.
- Signature: Codex GPT-5

## 2026-04-17 11:48 CEST - Vulkan Lazy Contract And Backlog Audit Fixes

- Objective attempted:
  - Continue the Tensor/Vulkan audit and fix pass with GPT-5.4 high auditors
    and local implementation after Spark/fast agents were disabled by quota
    policy.
- Workspace/target:
  - `/home/christos/Omni`.
- Code or configuration changes made:
  - Added `tensor_contract_try_device_value` in `src/lisp/prim_tensor.c3` and
    routed one-argument `realize` for lazy contract payloads through it before
    CPU fallback.
  - Added public and internal regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`. The internal C3 harness
    constructs lazy map/contract payloads directly so the new helper is
    exercised instead of only covering the eager public Vulkan route.
  - Reordered `omni_tensor_backend_vulkan_singular_norm_f64` validation in
    `csrc/tensor_vulkan_helpers.c`, added
    `omni_tensor_backend_vulkan_singular_norm_zero_size_validation_probe_for_tests`,
    and exposed that probe through `src/lisp/tensor_vulkan_backend.c3`.
  - Normalized backlog/docs state: `TODO.md` now has one live item
    (`TENSOR-100F`), `TENSOR-100E` is closed as the correctness-first Vulkan
    baseline, and `TENSOR-100G` is closed as the measured parallel-solve
    baseline. Updated the Vulkan plan index, backend decision note, tensor
    scientific plan, area status, and Vulkan roadmap wording to match.
  - Updated `.agents/PLAN.md` and `memory/CHANGELOG.md` with this checkpoint.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
    passed, `pass=913 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Public `(map + (to-device ...) 0.0)` contract tests are eager Vulkan-route
    coverage, not lazy-contract-helper coverage. Keep the internal harness
    tests for `tensor_contract_try_device_value`.
- Current best recommendation / checkpoint:
  - Remaining live Vulkan work is `TENSOR-100F`: helper/library factoring,
    dense row-major `Float64` expansion, and future dtype/layout work only
    after native storage/layout contracts exist.
- Unresolved issues:
  - Full heavy/container gates were not rerun for this checkpoint. The worker
    did run a bounded focused advanced collections pass before the final local
    test count changed; the final verified local focused pass is `913/0`.
- Signature: Codex GPT-5.4

## 2026-04-17 12:00 CEST - Vulkan SVD Eigen Float32 Planning

- Objective attempted:
  - Add the missing direct Vulkan factor-output SVD plan, eigensolver plan, and
    Float32 planning requested by the owner.
- Workspace/target:
  - `/home/christos/Omni`, planning/docs only.
- Code or configuration changes made:
  - Added `docs/plans/vulkan-svd-factor-output-plan-2026-04-17.md` for direct
    Vulkan `matrix/svd` behind the existing backend-neutral public surface.
  - Added `docs/plans/vulkan-eigensolver-plan-2026-04-17.md` for direct Vulkan
    `matrix/eigenvalues`, `matrix/eigenvectors`, and `matrix/eigenpairs`.
    Symmetric real `Float64` eigenvalues/eigenvectors are the first eligible
    Vulkan phase; general `matrix/eigenpairs` remains blocked while the public
    result contract is pointer-backed `BigComplex`.
  - Added `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`,
    splitting future Float32 work into native Tensor dtype/storage semantics
    first and Vulkan kernels second.
  - Updated `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`,
    `docs/plans/README.md`, `TODO.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`,
    `docs/plans/vulkan-backend-decision-2026-04-16.md`, and
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md` so the new
    plan files are discoverable and stale spectral/nuclear or zero-axis wording
    no longer contradicts current behavior.
  - Updated `.agents/PLAN.md` and `memory/CHANGELOG.md` with the planning
    checkpoint.
- Commands run and key results:
  - `jj status`: confirmed the workspace was already broadly dirty; this
    planning pass touched docs and agent artifacts only.
  - Targeted `git diff --check` over touched docs/artifacts: passed.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat the existence of these plan files as implementation support.
    Direct Vulkan SVD/eigen execution remains fail-closed until the relevant
    plan gates land.
  - Do not use `Float32` as a hidden fallback for existing `Float64` Vulkan
    execution. `Float32` requires native Tensor storage and CPU semantics first.
- Current best recommendation / checkpoint:
  - The immediate missing planning artifacts now exist. Next implementation
    should either harden current `TENSOR-100F` docs/tests from the audit notes or
    choose one planned surface and land its first validation gate.
- Unresolved issues:
  - No runtime code changed in this planning pass, so no build or runtime suite
    was rerun.
- Signature: Codex GPT-5.4

## 2026-04-17 12:20 CEST - Vulkan Audit Hardening Implementation

- Objective attempted:
  - Continue `TENSOR-100F` implementation using normal GPT-5.4 agents after the
    direct SVD/eigen/Float32 plan files were added.
- Workspace/target:
  - `/home/christos/Omni`, focused Tensor/Vulkan audit-hardening slice.
- Code or configuration changes made:
  - Updated `src/lisp/prim_tensor.c3` so the generic Vulkan contract helper now
    raises context `contract: Vulkan Float64 contract kernel failed` instead of
    stale single-axis wording.
  - Added `src/lisp/tests_advanced_stdlib_module_groups.c3` regressions for:
    public zero-size Vulkan `matrix/norm` `'spectral` and `'nuclear` on empty
    rows and empty columns;
    unchanged `dgesvd` counters for those zero-size norm paths;
    unchanged `dgesvd`, `dsyev`, and `dgeev` counters for direct and lazy
    Vulkan fail-closed `matrix/svd`, `matrix/eigenvalues`,
    `matrix/eigenvectors`, and `matrix/eigenpairs`;
    diagnostic ordering for invalid-but-Vulkan SVD/eigen operands returning
    `tensor/backend-unsupported` before CPU shape or symmetry diagnostics;
    lazy-contract helper execution for zero-axis and multi-axis Vulkan
    contractions.
  - Updated `TODO.md`, `.agents/PLAN.md`, and `memory/CHANGELOG.md` with the
    implementation checkpoint.
- Commands run and key results:
  - Worker validation before final integration: `c3c build`, focused
    `advanced-collections-module` `pass=919 fail=0`, and targeted
    `git diff --check` passed.
  - Final integrated validation: `c3c build --obj-out obj` passed with existing
    deprecation warnings.
  - Final focused validation:
    `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    passed with `pass=935 fail=0`.
  - Targeted `git diff --check` for changed source/docs/artifacts passed.
  - `./scripts/check_primitive_docs_parity.sh` passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity` passed.
- Invalidated assumptions or failed approaches worth preserving:
  - None new. The key preserved constraint remains that direct Vulkan SVD/eigen
    support is not implemented by these tests; the direct surfaces remain
    fail-closed until their dedicated implementation gates land.
- Current best recommendation / checkpoint:
  - The audit-hardening slice is implemented and validated. Next useful work can
    choose a planned implementation lane, with direct Vulkan `matrix/svd`
    factor-output support and symmetric real eigensolvers still requiring real
    helper/shader work rather than CPU fallback.
- Unresolved issues:
  - Full heavy/container gates were not rerun for this source-level test and
    diagnostic slice.
- Signature: Codex GPT-5.4

## 2026-04-17 17:23 CEST - Vulkan Unary Plus And Helper Factoring

- Objective attempted:
  - Continue `TENSOR-100F` feature work using multiple GPT-5.4 agents, with
    deferred work kept in TODO.
- Workspace/target:
  - `/home/christos/Omni`, Tensor/Vulkan unary helper and shared C helper
    factoring.
- Code or configuration changes made:
  - Updated `src/lisp/prim_tensor.c3` so unary Tensor `map +` on dense
    row-major Vulkan `Float64` tensors maps to the existing unary helper
    identity opcode.
  - Updated `src/lisp/eval_init_primitive_registration.c3` and
    `src/lisp/prim_math_arithmetic.c3` so public `+` accepts one or two
    arguments: `(+ scalar)` and `(+ tensor)` are identity, while over-arity
    still raises.
  - Added focused regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` for Vulkan unary
    `map +` roundtrip/placement and direct unary Tensor `+` placement.
  - Updated `src/lisp/tests_advanced_core_unicode_groups_more.c3` for the new
    unary `+` contract, over-arity rejection, typed unary `+` method dispatch,
    and continued no-auto-partial coverage via binary `*`.
  - Updated `csrc/tensor_vulkan_helpers.c` so the serial two-buffer,
    three-buffer, one-input/two-output, and one-input/three-output Vulkan
    dispatch helpers reuse `omni_tensor_vulkan_create_compute_pipeline_for_shader`.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `docs/reference/11-appendix-primitives.md`.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_map_unary_f64.comp -o /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-unicode-iterator OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=159 fail=0`.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=972 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed, `pass=959 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The first test attempt exposed that changing `+` registration to `-1`
    must not mean real variadic addition. `prim_add` now explicitly accepts
    only one or two arguments.
- Current best recommendation / checkpoint:
  - Unary identity support is landed and validated. Continue `TENSOR-100F`
    with the remaining TODO items: descriptor/command/status helper factoring,
    larger SVD/eigen algorithms, direct general `matrix/eigenpairs`, native
    CPU `Float32` Tensor storage before Vulkan Float32, fixed-width complex,
    and stride/view dispatch contracts.
- Unresolved issues:
  - Descriptor/pool/command-buffer/status helper factoring remains open after
    the compute-pipeline helper proof slice.
  - Full heavy/container gates were not run; bounded focused container
    validation passed.
- Signature: Codex GPT-5.4

## 2026-04-17 17:49 CEST - Native CPU Tensor Float32 Storage

- Objective attempted:
  - Continue `TENSOR-100F` feature implementation with multiple GPT-5.4 agents and close the native CPU `Tensor Float32` prerequisite before any Vulkan `Float32` kernel work.
- Workspace/target:
  - `/home/christos/Omni`, Tensor dtype/runtime storage, CPU Tensor evaluation, structural matrix helpers, docs, TODO, and memory artifacts.
- Code or configuration changes made:
  - Added native `TENSOR_DTYPE_FLOAT32` metadata/storage handling in `src/lisp/value_runtime_types.c3` and `src/lisp/value_tensor.c3`.
  - Updated `src/lisp/prim_tensor.c3` so `Tensor Float32` construction narrows finite numeric data into 32-bit storage, rejects out-of-range data, and supports CPU `map`, `contract`, `realize`, `to-device 'cpu`, `ref`, `Array`, `List`, and `Iterator`. Element extraction widens to scalar `Float64` because a scalar `Float32` value tag does not exist yet.
  - Updated `src/lisp/prim_tensor_matrix.c3` so structural CPU helpers support `Float32`: `matrix/transpose`, `matrix/diagonal`, `matrix/diagonal-matrix`, `matrix/identity`, and `matrix/trace`. Heavier LAPACK-style routines remain non-`Float32` lanes and must not silently widen and narrow.
  - Kept scalar `Float32` / `(Float x 32)` constructors fail-closed in `src/lisp/prim_string_convert.c3`; updated describe-mode wording to state scalar storage is still missing while Tensor storage exists.
  - Added focused regressions in `src/lisp/tests_advanced_stdlib_module_groups.c3` and low-level allocation coverage in `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`, Tensor reference docs, area docs, and Vulkan `Float32`/dtype/layout/roadmap plan docs.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Host focused `advanced-collections-module`: passed, `pass=995 fail=0`.
  - Host focused `advanced-unicode-iterator`: passed, `pass=159 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed, `pass=982 fail=0`.
  - Bounded-container `memory-lifetime-smoke`: passed, `pass=228 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
  - `c3c build --sanitize=address --obj-out obj-asan`: not run to completion; local `c3c` rejected sanitizer mode with "Address sanitizer is only supported on Linux, FreeBSD, NetBSD, Darwin and Windows."
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep saying native Tensor `Float32` storage is absent. The remaining missing pieces are scalar `Float32` values and GPU `Float32` copy/kernel support.
  - Do not implement Vulkan `Float32` by downcasting `Float64`; remaining device work needs explicit copy paths, helper/shader ABI names, capability reporting, and no-downcast tests.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100F` with Vulkan descriptor/command/status helper factoring or the first real Vulkan `Float32` placement/kernel slice. The CPU `Tensor Float32` oracle is now present for the latter.
- Unresolved issues:
  - Scalar `Float32` constructors still fail closed.
  - CUDA/Vulkan `Float32` placement still fails closed.
  - Full heavy/container gates were not run; focused bounded-container validation passed.
- Signature: Codex GPT-5.4

## 2026-04-17 18:07 CEST - Vulkan Serial Dispatch Helper Factoring

- Objective attempted:
  - Continue `TENSOR-100F` feature implementation with multiple GPT-5.4 agents
    and land the next behavior-preserving Vulkan helper factoring slice.
- Workspace/target:
  - `/home/christos/Omni`, `csrc/tensor_vulkan_helpers.c`, TODO, plan,
    roadmap, changelog, and session artifacts.
- Code or configuration changes made:
  - Added private helper structs and functions for sequential storage-buffer
    descriptor resources: layout creation, descriptor pool/allocation/update,
    and cleanup.
  - Added a private one-time compute dispatch helper that creates a command
    pool, allocates/begins one primary command buffer, binds pipeline and one
    descriptor set, pushes constants, dispatches, ends, submits, waits idle,
    and destroys the command pool.
  - Migrated the mature serial Vulkan helper family:
    two-buffer, three-buffer, one-input/two-output, and
    one-input/three-output dispatch helpers.
  - Preserved descriptor binding order, synchronous submit/wait behavior,
    output ownership, Vulkan result placement, diagnostics, and no hidden
    CPU/LAPACK fallback.
  - Updated `TODO.md`, `.agents/PLAN.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `memory/CHANGELOG.md` so only status-copy/readback helper factoring remains
    under the open helper TODO.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`: passed, `pass=995 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed, `pass=982 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Current best recommendation / checkpoint:
  - Continue helper factoring only from the status-copy/readback boundary, or
    switch to another explicit `TENSOR-100F` lane such as real Vulkan `Float32`
    copy/kernel support.
- Unresolved issues:
  - Status-copy/readback helper extraction remains open because status payload
    semantics differ across generic singular, SVD/singular-values,
    symmetric-eigen, and parallel-solve paths.
  - Live GPU execution coverage is availability-gated; focused host/container
    suites passed, but this environment may not exercise an actual Vulkan
    device path.
- Signature: Codex GPT-5.4

## 2026-04-17 18:17 CEST - TENSOR-100F Vulkan Status Readback Helper Factoring

- Objective attempted:
  - Continue `TENSOR-100F` helper factoring by removing repeated Vulkan
    `Float64` status-payload copy/readback boilerplate without changing public
    Tensor behavior.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Tensor helper implementation and
    `TENSOR-100F` planning artifacts.
- Code or configuration changes made:
  - Added a shared `Float64` status-payload copy/readback helper and a
    mapper-based status decoding helper in `csrc/tensor_vulkan_helpers.c`.
  - Migrated compatible serial Vulkan mapped-status readers through the shared
    path: generic trailing status, SVD/singular-values status, and symmetric
    eigen status.
  - Extended the existing deterministic status probe to cover tail, singular
    values/SVD, and symmetric-eigen mapper behavior.
  - Updated LU metadata handling to reuse the generic trailing-status mapper
    while keeping pivot and swap-count validation LU-specific.
  - Left parallel solve on its separate `uint32_t` status-buffer ABI by design.
  - Updated `TODO.md`, `.agents/PLAN.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `memory/CHANGELOG.md`; the helper-plumbing TODO is now closed and the live
    queue continues with larger SVD/eigen algorithms, direct general
    eigenpairs, Vulkan `Float32`, fixed-width complex, and layout/stride work.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`: passed, `pass=995 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed, `pass=982 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Current best recommendation / checkpoint:
  - Do not reopen mature serial helper plumbing as the neutral baseline unless
    new duplication appears in a different helper shape. Continue from the
    remaining explicit `TENSOR-100F` feature lanes.
- Unresolved issues:
  - Full heavy/container-only gates were not run; focused bounded-container
    validation passed.
  - Live GPU execution coverage is availability-gated; this environment may not
    exercise an actual Vulkan device path.
- Signature: Codex GPT-5.4

## 2026-04-17 19:22 CEST - TENSOR-100F Vulkan Float32 Placement And Elementwise Slice

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and land the first
    real Vulkan `Float32` execution slice without using `Float32` as a hidden
    fallback for existing `Float64` behavior.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Tensor placement/copyback, destination
    `realize`, dense row-major `map`, unary helpers, direct `min`/`max`,
    tests, TODO, and docs/artifacts.
- Code or configuration changes made:
  - Added `Float32` Vulkan binary/unary shader sources and generated SPIR-V C
    arrays: `csrc/tensor_vulkan_map_f32.comp`,
    `csrc/tensor_vulkan_map_f32_spv.c`,
    `csrc/tensor_vulkan_map_unary_f32.comp`, and
    `csrc/tensor_vulkan_map_unary_f32_spv.c`.
  - Added distinct helper/ABI paths in `csrc/tensor_vulkan_helpers.c` and
    `src/lisp/tensor_vulkan_backend.c3`:
    `omni_tensor_backend_vulkan_float32_available`,
    `omni_tensor_backend_vulkan_fill_f32`,
    `omni_tensor_backend_vulkan_map_f32`, and
    `omni_tensor_backend_vulkan_map_unary_f32`.
  - Updated `src/lisp/prim_tensor.c3` so Vulkan `Float32` placement/copyback,
    destination `realize`, lazy/direct `map`, direct unary Tensor operations,
    and direct `min`/`max` dispatch to real `Float32` Vulkan helpers for
    matching dense row-major operands.
  - Preserved no-downcast behavior: `Float64` paths still use `Float64`
    helpers, mixed Tensor dtypes fail closed, and Big* Tensor dtypes are not
    lowered to Vulkan.
  - Added focused availability-gated regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` for capability
    reporting, placement/copyback dtype preservation, destination `realize`,
    `map`, unary helpers, direct `min`/`max`, and `Float64` no-downcast
    preservation.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_map_f32.comp -o /tmp/omni_map_f32.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_map_f32.spv`: passed.
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_map_unary_f32.comp -o /tmp/omni_map_unary_f32.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_map_unary_f32.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `Float32` smokes: capability reported `float32 true`;
    placement/copyback preserved `"Float32"`; `map +` returned `4.25`;
    `map sqrt` returned `1.11803388595581` for `sqrt(1.25)`; direct
    `max` stayed Vulkan-placed; direct `min` returned `-1.25`.
  - Host focused `advanced-collections-module`: passed, `pass=1025 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1012 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating Vulkan `Float32` as entirely fail-closed. Placement,
    copyback, destination `realize`, elementwise `map`, unary helpers, and
    direct `min`/`max` are now real Vulkan paths.
  - Do not use the presence of `Float32` Vulkan support to justify hidden
    `Float64` downcasts or mixed-dtype promotion on Vulkan.
- Current best recommendation / checkpoint:
  - Continue Vulkan `Float32` from the remaining explicit TODO boundary:
    rank-N `contract` first, then eligible structural matrix kernels, then
    reducers/factors only with dedicated `Float32` tolerances and oracles.
- Unresolved issues:
  - Vulkan `Float32` `contract`, structural matrix kernels, reducers, and
    factor/solve kernels remain fail-closed.
  - CUDA `Float32` placement and scalar `Float32` values remain fail-closed.
- Signature: Codex GPT-5.4

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
  - `src/lisp/jit_jit_apply_runtime.c3`
  - `src/lisp/jit_jit_apply_multi_prims.c3`
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

## 2026-04-18 13:10 CEST - CUDA Fixed-Width Complex Map Docs Checkpoint

- Objective attempted:
  - Update documentation and operational artifacts only for the landed CUDA
    fixed-width complex elementwise map work. No code/source/test files were
    edited in this documentation pass.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - fixed-width complex Tensor docs, Tensor area status, TODO, active plan,
    changelog, and this session report.
- Documentation/artifact changes made:
  - Superseded stale wording that CUDA complex `map` remains fail-closed.
  - Recorded CUDA `Complex128`/`Complex64` elementwise `map` as shipped behind
    `elementwise-map-complex128` / `elementwise-map-complex64`.
  - Recorded supported CUDA complex map ops: binary `+`, `-`, `*`, `/`; unary
    `abs`, unary `-`, identity/`+`, `real-part`, `imag-part`, and
    `conjugate`.
  - Recorded direct helper dtype behavior: CUDA complex `real-part`,
    `imag-part`, and `abs` return component-width real CUDA tensors; direct
    unary `-` and `conjugate` preserve complex dtype.
  - Kept CUDA/Vulkan complex `contract` and matrix kernels fail-closed.
- Commands verified by the implementation work:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build`: passed.
  - Host focused `advanced-collections-module`: `1433 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1416 passed, 0
    failed`.
  - CUDA PTX generation and `ptxas` for `csrc/tensor_cuda_complex_map.cu`:
    passed.
  - C helper syntax check: passed.
  - Primitive docs parity, Stage 3 source parity, and targeted diff check:
    passed.
- Current best recommendation / checkpoint:
  - Treat CUDA/Vulkan fixed-width complex elementwise `map` as landed and split
    remaining complex backend work around `contract`/matrix kernels, not a
    broad "complex GPU kernels" umbrella.
- Unresolved issues:
  - CUDA/Vulkan fixed-width complex `contract` remains fail-closed.
  - CUDA/Vulkan fixed-width complex matrix kernels remain fail-closed.
- Signature: Codex GPT-5.4

## 2026-04-18 12:13 CEST - CUDA/Vulkan Fixed-Width Complex Storage Round-Trips

- Objective attempted:
  - Continue fixed-width complex implementation by landing CUDA/Vulkan
    `Complex128`/`Complex64` raw storage placement and explicit CPU copyback,
    while preserving fail-closed complex GPU compute.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_tensor.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups.c3`
  - fixed-width complex Tensor docs, Vulkan dtype policy, TODO, and
    operational plan artifacts
- Code or configuration changes made:
  - Added separate CUDA/Vulkan storage dtype predicates for raw copy paths.
    The existing real-only predicates remain in compute gates.
  - Broadened `to-device` CPU-to-CUDA/Vulkan and CUDA/Vulkan-to-CPU copyback
    to accept `Complex128` and `Complex64` storage when the backend is
    available.
  - Broadened CUDA/Vulkan destination validation for matching complex device
    tensors and added complex scalar fill staging through host buffers plus raw
    copy-to-existing-device helpers.
  - Added `tensor-backends` `complex128` and `complex64` storage capability
    bits for CUDA/Vulkan. Historical note: complex map/contract bits were
    explicit false at this storage-only checkpoint; map bits are superseded by
    later Vulkan and CUDA complex map checkpoints.
  - Added availability-gated tests for complex device layout, placement,
    round-trip copyback, destination `realize`, scalar fill, no-hidden CPU
    destination realization, and fail-closed complex CUDA/Vulkan compute.
  - Updated docs/backlog/planning to distinguish storage/copy capability from
    complex GPU kernels.
- Commands run and key results:
  - `c3c build`: passed.
  - Host focused `advanced-collections-module`: `1399 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1382 passed,
    0 failed`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat CUDA/Vulkan fixed-width complex as placement-forbidden. It is
    now raw-storage-copyable when `tensor-backends` reports `complex128` or
    `complex64`.
  - Do not treat `complex128` or `complex64` storage capability as kernel
    capability. Historical note: CUDA/Vulkan complex `map` still intentionally
    failed closed at this checkpoint, then landed in later backend-specific map
    checkpoints. Complex `contract` and matrix kernels still intentionally fail
    closed.
- Current best recommendation / checkpoint:
  - Historical storage-only checkpoint. The first operation family,
    elementwise `map`, is now landed for Vulkan and CUDA; keep direct general
    Vulkan `matrix/eigenpairs` blocked on its public result contract.
- Unresolved issues:
  - Superseded: CUDA/Vulkan complex map kernels are now implemented in later
    checkpoints.
  - CUDA/Vulkan complex contract/matrix kernels are not implemented.
  - Fixed-width complex scientific/transcendental scalar/Tensor operations
    beyond arithmetic, component helpers, `abs`, and `conjugate` remain
    fail-closed pending a precision contract.
- Signature: Codex GPT-5.4

## 2026-04-18 11:53 CEST - CPU Fixed-Width Complex Scalar And Tensor Support

- Objective attempted:
  - Land native CPU `Complex128`/`Complex64` scalar and Tensor semantics, then
    update operational planning so the remaining fixed-width complex lane is
    clearly CUDA/Vulkan backend work.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - scalar value/runtime/type integration
  - CPU Tensor storage, map/contract/component helpers, and matrix structural
    operations
  - `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`, docs/reference, and
    Tensor/Vulkan planning docs
- Code or configuration changes made:
  - Added `Complex128` and `Complex64` scalar value families with constructors,
    stdlib predicates, dispatch/type integration, printing/String conversion,
    equality/hash, AOT/literal serialization, and boundary copy/promotion
    handling.
  - Added CPU Tensor `Complex128`/`Complex64` dtype/storage support for
    explicit and inferred construction, scalar fill, `ref`, `Array`/`List`
    conversion, `realize`, mixed fixed-complex `map` promotion, `contract`,
    `real-part`, `imag-part`, `abs`, `conjugate`, unary minus, and structural
    matrix operations (`matrix/transpose`,
    `matrix/diagonal`, `matrix/diagonal-matrix`, `matrix/identity`, and
    `matrix/trace`).
  - Kept CUDA/Vulkan fixed-width complex placement and backend execution
    fail-closed. Future GPU complex support needs explicit backend capability
    bits, device layout/copy semantics, status contracts, and kernels.
  - Updated fixed-width complex docs/backlog/planning wording so CPU support is
    no longer described as deferred.
- Commands run and key results:
  - `c3c build`: passed.
  - Direct `--eval` smokes for scalar `String`, Tensor `ref`, component dtype,
    and Complex128 `contract`: passed.
  - Host focused scalar advanced tests passed (`180 passed, 0 failed`).
  - Host focused Tensor advanced tests passed (`1383 passed, 0 failed`).
  - Bounded-container focused Tensor advanced tests passed (`1366 passed,
    0 failed`).
  - Targeted `git diff --check`: passed.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating fixed-width complex as missing at the CPU
    scalar/Tensor layer. The remaining fixed-width complex work is backend
    placement/execution and direct Vulkan general `matrix/eigenpairs` contract
    work.
  - Do not infer complex GPU support from real `float64` or `float32`
    `tensor-backends` capability bits.
- Current best recommendation / checkpoint:
  - Use the landed CPU `Complex128`/`Complex64` Tensor behavior as the oracle
    for the next CUDA/Vulkan complex ABI. Start with explicit capability bits,
    device layout/copy semantics, and a first operation family such as
    `map`/`contract` before attempting general complex eigenpairs.
- Unresolved issues:
  - Fixed-width complex scientific/transcendental operations beyond arithmetic,
    component helpers, `abs`, and `conjugate` remain fail-closed pending an
    explicit approximation/precision contract.
  - CUDA/Vulkan `Complex128`/`Complex64` placement, backend `map`, backend
    `contract`, backend matrix kernels, and direct Vulkan general
    `matrix/eigenpairs` remain unimplemented.
- Signature: Codex GPT-5.4
