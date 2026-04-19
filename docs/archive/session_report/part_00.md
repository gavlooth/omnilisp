## 2026-04-19 10:52 CEST - Guard Container Toolchain Repair

- Objective attempted:
  - Continue broad audit/repair work without splitting files under 1000 LOC,
    following up on failed bounded guard scripts.
- Workspace/target:
  - `/home/christos/Omni`, bounded validation wrappers
    `scripts/check_jit_env_scope_guards.sh` and
    `scripts/check_scheduler_state_guards.sh`.
- Code or configuration changes made:
  - Changed both guard scripts so, when `OMNI_VALIDATION_TOOLCHAIN_ROOT` is
    unset, they derive the host toolchain root from `command -v c3c`, then fall
    back to `$HOME/.local`, then to `/usr/local` only if
    `/usr/local/bin/c3c` exists.
- Key results:
  - The scripts no longer select `/usr/local` merely because that directory
    exists, which avoids falling through to the container's incompatible
    `/usr/local/bin/c3c`.
  - `env -u OMNI_VALIDATION_TOOLCHAIN_ROOT scripts/check_jit_env_scope_guards.sh`
    now builds in the bounded container and completes the JIT env/scope guard
    run.
  - `env -u OMNI_VALIDATION_TOOLCHAIN_ROOT scripts/check_scheduler_state_guards.sh`
    now builds in the bounded container and completes the scheduler state guard
    run.
- Commands run:
  - `bash -n scripts/check_jit_env_scope_guards.sh scripts/check_scheduler_state_guards.sh`
  - `env -u OMNI_VALIDATION_TOOLCHAIN_ROOT scripts/check_jit_env_scope_guards.sh`
  - `env -u OMNI_VALIDATION_TOOLCHAIN_ROOT scripts/check_scheduler_state_guards.sh`
  - `git diff --check`
- Key validation results:
  - Shell syntax check passed.
  - Both bounded container builds linked `build/main`.
  - JIT env/scope guard script completed with
    `OK: JIT env/scope guards passed.`
  - Scheduler state guard script completed with
    `OK: scheduler state guards passed.`
  - `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat `/usr/local` as a valid validation toolchain root just because
    the directory exists; require a usable `bin/c3c` or derive the root from
    the active host `c3c`.
- Unresolved issues:
  - Full all-slice/high-memory validation was not run in this checkpoint.
- Signature: Codex GPT-5.4

## 2026-04-19 09:17 CEST - Advanced Slice Arity Diagnostic Closure

- Objective attempted:
  - Continue broad audit/repair work without splitting files under 1000 LOC,
    starting from the previously observed bounded `advanced` slice failures.
- Workspace/target:
  - `/home/christos/Omni`, primitive diagnostics and advanced/scheduler
    validation.
- Code or configuration changes made:
  - Updated the shared primitive arity diagnostic helper so `type/arity`
    errors include the canonical user-facing phrase `arity mismatch` while
    preserving the `type/arity` payload code.
  - Updated the exact `try catch arity error` assertion to the new canonical
    primitive arity message.
- Key results:
  - The isolated scheduler join-timeout payload expression now returns
    `"scheduler/thread-join-timeout"` on the current built binary; no scheduler
    code change was needed because the generic no-data raise payload rooting
    path already owns that contract in this working copy.
  - The previously failing `advanced-stdlib-numeric` arity assertions now pass.
  - The full bounded `advanced` slice now passes.
- Commands run:
  - `jj status`
  - `c3c build --obj-out obj`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(+ 1 2 3)"`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-core-semantics OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-unicode-iterator OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=scheduler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `git diff --check`
- Key validation results:
  - Direct `(+ 1 2 3)` now fails with
    `+: arity mismatch: expected 1 or 2 arguments, got 3`.
  - Focused bounded `advanced-stdlib-numeric`: `pass=411 fail=0`.
  - Focused bounded `advanced-core-semantics`: `pass=71 fail=0`.
  - Focused bounded `advanced-unicode-iterator`: `pass=180 fail=0`.
  - Full bounded `advanced`: `pass=2922 fail=0`.
  - Focused bounded `scheduler`: `pass=113 fail=0`.
  - Basic Lisp default slice: `pass=144 fail=0`.
  - `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not patch `thread-join-timeout` specifically for the prior payload-code
    symptom unless it reproduces after rebuild; the current generic raise
    payload path and focused scheduler validation already satisfy that contract.
- Unresolved issues:
  - Full all-slice/high-memory validation was not run in this checkpoint.
  - The worktree still contains broad pre-existing Tensor/CUDA/Vulkan changes
    outside this repair.
- Signature: Codex GPT-5.4

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

## 2026-04-19 08:26 CEST - Strict SymbolId Cleanup and Deduce Large-JIT Crash Repair

- Objective attempted:
  - Continue repository audit/repair without splitting files under the 1000 LOC
    threshold, preserving the existing tensor/Vulkan worktree while clearing
    strict-deprecation fallout and repairing the bounded Deduce slice crash.
- Code or configuration changes made:
  - Normalized remaining repo-owned raw `SymbolId` zero sentinels to explicit
    `(SymbolId)0` casts across parser, eval/type, schema-query, JIT closure,
    memory-lifetime, and Deduce helper/test paths.
  - Removed the JIT helper-emission dependency on a thread-local
    `g_jit_interp_stack_offset`; helper emitters now receive the active
    `JitLocals*` and restore `JIT_V0` from that function's own
    `interp_stack_offset`.
  - Added staged JIT lowering for oversized block expressions and long
    right-nested logical `and`/`or` chains. This avoids generating one huge
    native branch/helper-call body for large assertion/proof expressions while
    preserving short-circuit behavior.
  - Added the targeted Deduce query filter
    `OMNI_DEDUCE_QUERY_FILTER=why-result-admin-multi-rule` for the formerly
    crashing admin-surface proof block.
  - Removed a non-language debug `(printn ...)` probe from that proof test.
  - Hardened `runtime_eval_expr` so the TCO recycle error path restores the
    saved `jit_env` before returning an error.
- Commands run and key results:
  - `git diff --check`: passed.
  - `c3c build --obj-out obj`: passed.
  - `c3c build --obj-out build/strict-deprecation-obj --warn-deprecation=error`
    linked successfully; the only remaining strict-deprecation diagnostic is
    external C3 stdlib
    `/home/christos/.local/lib/c3/std/net/os/linux.c3:11`.
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`:
    basic slice passed, `pass=144 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`:
    passed, `pass=51 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=why-result-admin-multi-rule OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`:
    passed, `pass=1 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=admin-surface OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`:
    passed, `pass=129 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`:
    passed, `pass=330 fail=0`.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not resume the Deduce crash from raw `jit_env` save/restore around call
    operand lowering, high-arity call staging through `jit_eval_call_cont_safe`,
    lookup-time pointer guards, or a runtime active-interpreter global. Those
    approaches either did not improve the Deduce repro or regressed bootstrap.
  - The key crash trigger was large native JIT lowering of one big Deduce
    proof/assertion expression; staging long blocks/logical chains changed the
    regime and cleared the bounded Deduce slice.
- Current best recommendation/checkpoint:
  - Treat the bounded Deduce slice as green again at `330/0`. Continue audit
    from remaining tensor/Vulkan/CUDA/LAPACK work or broader validation gates,
    not from the old admin-surface crash.
  - If future JIT crashes appear in very large expressions, first check whether
    another expression family needs staged lowering before adding downstream
    pointer guards.
- Unresolved issues:
  - Strict-deprecation still reports the external C3 stdlib `AIFamily`
    diagnostic; no repo-owned strict diagnostics were observed.
  - Full all-slice/high-memory validation was not run in this checkpoint.
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

## 2026-04-18 17:21 CEST - Vulkan Complex QR And Cholesky Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` by landing fixed-width complex `matrix/qr` and
    `matrix/cholesky` for CPU oracle behavior and Vulkan execution, using
    multiple GPT-5.4 agents for Vulkan implementation, test coverage, and
    review.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan complex QR/Cholesky shaders/helper ABI, C3
    Tensor matrix routing, advanced stdlib tests, docs/backlog/memory
    artifacts.
- Code or configuration changes made:
  - Added Vulkan `Complex128`/`Complex64` QR and Cholesky compute shaders,
    generated SPIR-V C sources, build script wiring, and `project.json`
    archive inputs.
  - Added helper exports and C3 extern routing for
    `omni_tensor_backend_vulkan_qr_complex128`,
    `omni_tensor_backend_vulkan_qr_complex64`,
    `omni_tensor_backend_vulkan_cholesky_complex128`, and
    `omni_tensor_backend_vulkan_cholesky_complex64`.
  - Implemented CPU `Complex128`/`Complex64` QR with Hermitian projection and
    Cholesky with Hermitian positive-definite checks.
  - Aligned CPU tolerance thresholds with Vulkan for near-rank-deficient QR
    and near-Hermitian Cholesky cases.
  - Added guarded CPU/Vulkan tests for values, dtype/device/shape
    preservation, Hermitian projection, no-LAPACK behavior, lazy inputs, and
    rank-deficient/non-Hermitian/non-HPD diagnostics.
  - Updated TODO, active plan, fixed-width complex/public docs, roadmap/index,
    tensor area status, memory changelog, and session reports.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for the four
    complex QR/Cholesky shaders.
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - Direct CPU/Vulkan `--eval` smokes for Complex128 and Complex64
    QR/Cholesky.
  - Host focused `advanced-collections-module`:
    `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - Bounded-container focused `advanced-collections-module`:
    `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - Targeted `git diff --check`.
- Key results:
  - Direct CPU Complex128 QR smoke returned the non-real Hermitian projection
    `0.0-1.41421356237309i`.
  - Direct CPU Complex128 Cholesky smoke returned `1.0-1.0i` for the lower
    off-diagonal and `2.0+0.0i` for both diagonals.
  - Direct Vulkan Complex128 QR smoke preserved `vulkan` placement and
    returned `0.0-1.41421356237309i`.
  - Direct Vulkan Complex64 Cholesky smoke preserved `vulkan` placement and
    returned `1.0-1.0i`.
  - Host focused `advanced-collections-module`: `1570 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1553 passed,
    0 failed`.
  - Primitive docs parity, Stage 3 source parity, and targeted diff hygiene
    passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The first review found public `matrix/cholesky` still rejected
    fixed-width complex tensors even though helpers existed; the public dtype
    gate now admits Complex128/Complex64 and routes to CPU/Vulkan helpers.
  - CPU fixed-complex QR/Cholesky exact checks were not the correct baseline
    after Vulkan introduced numerical thresholds; CPU now matches the shader
    thresholds for parity.
- Current best recommendation / checkpoint:
  - Treat Vulkan fixed-width complex
    LU/determinant/solve/inverse/rank/direct-norm/QR/Cholesky as the landed
    numerical subset.
- Unresolved issues / next actions:
  - Complex `matrix/singular-values`, `matrix/svd`, spectral/nuclear complex
    norm selectors, and complex eigen routines remain separate explicit
    contracts.
  - CUDA fixed-width complex numerical matrix variants remain open.
- Signature: Codex GPT-5.4

## 2026-04-18 16:55 CEST - Vulkan Complex Rank And Direct Norm Checkpoint

- Objective attempted:
  - Continue `TENSOR-100F` by landing fixed-width complex `matrix/rank` and
    direct `matrix/norm` reducers for CPU oracle behavior and Vulkan
    execution, using multiple GPT-5.4 agents for implementation/review.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan complex rank/norm shaders/helper ABI, C3
    Tensor matrix routing, advanced stdlib tests, docs/backlog/memory
    artifacts.
- Code or configuration changes made:
  - Added Vulkan `Complex128`/`Complex64` norm and rank compute shaders,
    generated SPIR-V C sources, build script wiring, and `project.json`
    archive inputs.
  - Added helper exports and C3 extern routing for
    `omni_tensor_backend_vulkan_norm_complex128`,
    `omni_tensor_backend_vulkan_norm_complex64`,
    `omni_tensor_backend_vulkan_rank_complex128`, and
    `omni_tensor_backend_vulkan_rank_complex64`.
  - Implemented CPU `Complex128`/`Complex64` rank with magnitude pivoting and
    tolerance, plus direct norm reducers for default/`'frobenius`, `'one`,
    `'infinity`, and `'max` over complex magnitudes.
  - Kept complex spectral/nuclear norm selectors fail-closed until complex
    singular-value/SVD support lands.
  - Added guarded CPU/Vulkan tests for rank full/deficient/tolerance cases,
    direct norm selectors, no-LAPACK behavior, and fail-closed residual
    complex spectral/nuclear behavior.
  - Updated TODO, active plan, fixed-width complex/public docs, roadmap/index,
    tensor area status, memory changelog, and session reports.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for
    `csrc/tensor_vulkan_norm_complex128.comp`,
    `csrc/tensor_vulkan_norm_complex64.comp`,
    `csrc/tensor_vulkan_rank_complex128.comp`, and
    `csrc/tensor_vulkan_rank_complex64.comp`.
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - Direct CPU/Vulkan `--eval` smokes for Complex128 and Complex64 rank/norm,
    plus CPU spectral fail-closed behavior.
  - Host focused `advanced-collections-module`:
    `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - Bounded-container focused `advanced-collections-module`:
    `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - Targeted `git diff --check`.
- Key results:
  - Direct CPU Complex128 rank smoke returned `(2 1)`.
  - Direct CPU Complex128 norm/spectral smoke returned
    `(5.0 7.0 tensor/backend-unsupported)`.
  - Direct CPU Complex64 rank/norm smoke returned `(2 5.0 5.0)`.
  - Direct Vulkan Complex128 and Complex64 smokes returned `(2 5.0)` for rank
    plus default Frobenius norm on this host.
  - Host focused `advanced-collections-module`: `1540 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1523 passed,
    0 failed`.
  - Primitive docs parity, Stage 3 source parity, and targeted diff hygiene
    passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The initial CPU complex norm path was dead behind a stale Float64/Float32
    dtype gate; the gate is fixed and covered by runtime tests.
  - `matrix_singular_value_norm` must remain real-only until complex SVD
    lands; complex spectral/nuclear norms fail closed at routing boundaries.
- Current best recommendation / checkpoint:
  - Treat Vulkan fixed-width complex LU/determinant/solve/inverse/rank and
    direct norm reducers as the landed numerical subset.
- Unresolved issues / next actions:
  - Complex `matrix/norm` `'spectral`/`'nuclear`, QR/Cholesky, complex
    singular-value/SVD, and complex eigen routines remain separate explicit
    contracts.
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
  - Continue `TENSOR-100F` with multiple GPT-5.4 agents by landing CUDA-first
    dtype-changing Tensor rounding without exposing same-dtype GPU rounding
    map opcodes.
- Workspace/target:
  - `/home/christos/Omni`, `csrc/tensor_cuda_helpers.c`,
    `csrc/tensor_cuda_rounding_i64.cu`,
    `csrc/tensor_cuda_rounding_i64_ptx.inc`,
    `src/lisp/tensor_cuda_backend.c3`, `src/lisp/prim_tensor.c3`,
    `src/lisp/tests_advanced_stdlib_module_groups.c3`, Tensor docs,
    TODO/plan/changelog/session artifacts.
- Code/configuration changes:
  - Added a CUDA rounding kernel source and checked-in PTX include for dense
    row-major `Float64`/`Float32` inputs.
  - Added a status-bearing CUDA helper path that writes temporary device
    `int64` results, copies them to host, and exposes Float64/Float32 C ABI
    entrypoints.
  - Routed direct Tensor `floor`, `ceiling`, `round`, and `truncate` touching
    CUDA through the helper, then materialized native CPU `Tensor BigInteger`
    output. Current BigInteger Tensor storage remains CPU pointer-owned.
  - Added `tensor-backends` CUDA `rounding-big-integer` capability and focused
    capability-gated tests for BigInteger dtype and explicit CPU copyback.
- Commands run:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_rounding_i64.cu -o /tmp/omni_tensor_cuda_rounding_i64.ptx`
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/omni_tensor_cuda_rounding_i64.ptx -o /tmp/omni_tensor_cuda_rounding_i64.cubin`
  - `cc -O2 ... -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers_rounding.o`
  - `./scripts/build_omni_chelpers.sh`
