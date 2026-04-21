# Session Report Part 39

Source: `.agents/SESSION_REPORT.md`

## 2026-04-20 23:28 CEST - Vulkan Clip-Gradients Float32 Runtime

Objective attempted:
- Implement ML-VK-060-010: Vulkan dense row-major `Float32`
  `ml/clip-gradients` and optimizer `clip-norm` support.

Relevant workspace or target:
- `/home/christos/Omni`
- `csrc/tensor_vulkan_helpers_ml_clip.c`
- `src/lisp/prim_ml_gradient_clip_vulkan.c3`
- `src/lisp/prim_ml_gradient_clip.c3`
- `src/lisp/prim_tensor_backend_ops.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- Vulkan ML roadmap, TODO, language reference, changelog, and plan artifacts.

Code or configuration changes made:
- Added Vulkan `Float32` clip sum-squares and scale compute shaders plus
  generated checked-in SPIR-V C payloads.
- Added C helper functions
  `omni_tensor_backend_vulkan_ml_clip_sum_squares_f32` and
  `omni_tensor_backend_vulkan_ml_clip_scale_f32`.
- Added C3 extern wiring and a separate Vulkan gradient-clip dispatch file.
- Routed `ml/clip-gradients` through the Vulkan path before CPU materialization
  when any gradient tree leaf touches Vulkan.
- Preserved fail-closed semantics for mixed CPU/Vulkan gradient trees,
  unsupported Vulkan dtypes, non-dense layouts, empty leaves, and missing device
  storage.
- Added `tensor-backends` capability key `ml-clip-gradients-float32`.
- Updated tests and docs for Vulkan clipping and optimizer `clip-norm` before
  Vulkan updates.

Commands run:
- `glslangValidator -V --target-env vulkan1.0` for both clip shaders.
- `spirv-val --target-env vulkan1.0` for both generated SPIR-V binaries.
- `scripts/build_omni_chelpers.sh`
- `c3c build`
- Direct `build/main --eval` Vulkan clipping and clip-norm smoke checks.
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Helper archive rebuild passed.
- `c3c build` linked `build/main`.
- Direct clipping smoke preserved Vulkan placement and clipped `[3, 4]` to
  `[1.5, 2.0]` with max norm `2.5`.
- Direct optimizer clip-norm smoke preserved Vulkan placement and updated
  `[10, 10]` with clipped `[3, 4]` gradients to about `[9.4, 9.2]`.
- Focused advanced collections passed with `pass=1811 fail=0`.
- Basic Lisp passed with `pass=160 fail=0`.
- Compiler slice passed with `pass=281 fail=0`.
- Primitive docs parity, Stage 3 source parity, code file-size gate, and
  `git diff --check` passed.

Invalidated assumptions or failed approaches worth preserving:
- A docs/tests-only pass briefly left the build link-broken because
  `ml-clip-gradients-float32` tests referenced helper symbols before the helper
  C/shader implementation existed. Treat ML surface docs/tests as incomplete
  until runtime and build-link surfaces are landed together.

Unresolved issues:
- At that checkpoint, CUDA optimizer kernels remained unimplemented; see the
  later ML-VK-060-011 entry below for the map-backed CUDA SGD route that has
  since landed.
- `nn/train-step` remains deferred until the autograd surface is sufficient.
- Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this slice.

Next actions:
- Continue `ML-VK-060` with CUDA optimizer work or move to the next `ML-VK-070`
  data-oriented model/layer library item.
- Commit and push all non-artifact changes from this checkpoint.

Signature: GPT-5 Codex

## 2026-04-20 23:46 CEST - CUDA Dense Float32 SGD Optimizer Runtime

Objective attempted:
- Implement ML-VK-060-011: CUDA dense row-major `Float32` SGD
  `ml/optimizer-step` support, keeping the broad `ml-optimizer` capability
  false until the optimizer family is complete.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_ml_optimizer.c3`
- `src/lisp/prim_ml_optimizer_cuda.c3`
- `src/lisp/prim_tensor_backend_ops.c3`
- `src/entry_build_runtime_manifest_lisp_part3.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `docs/LANGUAGE_SPEC.part-01b.md`
- `docs/reference/03-collections.part-01.md`
- `docs/reference/11-appendix-primitives.md`
- `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- `docs/todo_parts/todo_part_14.md`
- `.agents/PLAN.md`

Code or configuration changes made:
- Added `src/lisp/prim_ml_optimizer_cuda.c3` to route all-CUDA dense row-major
  `Float32` SGD tensor leaves before Vulkan/CPU fallback.
- The CUDA path composes existing CUDA elementwise map kernels for weight decay,
  optional momentum/velocity update, learning-rate scaling, and parameter
  subtraction; it is intentionally map-backed, not a fused optimizer PTX kernel.
- Added runtime fail-closed checks for mixed placement, non-CUDA realized
  storage, non-`Float32` CUDA tensors, shape mismatch, empty leaves, and missing
  CUDA backing storage.
- Registered the CUDA optimizer file in the AOT runtime manifest.
- Updated `tensor-backends` so CUDA reports narrow
  `ml-optimizer-sgd-float32` when CUDA `elementwise-map-float32` is available,
  while broad `ml-optimizer` remains false.
- Added CUDA-dense row-major Float32 `ml/optimizer-step` coverage to the
  advanced collections test slice for stateless SGD, momentum velocity
  initialization, momentum continuation, and mixed-device fail-closed diagnostics.
- Updated language/spec and reference docs with the new slice contract, including
  optional momentum/velocity state and fail-closed mixed-device behavior.
- Updated roadmap and TODO entries to add `ML-VK-060-011` and split out CUDA
  fused-kernel work from the now-shipped map-backed CUDA SGD route.

Commands run:
- `scripts/build_omni_chelpers.sh`
- `c3c build`
- Direct `build/main --eval` CUDA capability smoke.
- Direct `build/main --eval` CUDA optimizer smoke for stateless SGD with weight
  decay, momentum continuation, and mixed-device fail-closed diagnostics.
- `git diff --check`
- `scripts/check_file_size_gate.sh`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`

Key results:
- Helper archive rebuild passed.
- `c3c build` linked `build/main`.
- CUDA capability smoke reported `{cuda-sgd-float32 true cuda-float32 true
  cuda-map-float32 true cuda-ml-optimizer false}` on this machine.
- Direct CUDA optimizer smoke returned `true`.
- Focused advanced collections passed with `pass=1815 fail=0`.
- Basic Lisp passed with `pass=160 fail=0`.
- Compiler slice passed with `pass=281 fail=0`.
- Primitive docs parity, Stage 3 source parity, code file-size gate, and
  `git diff --check` passed.

Invalidated assumptions or failed approaches worth preserving:
- Do not describe ML-VK-060-011 as a fused CUDA optimizer kernel. The shipped
  contract is CUDA execution for dense row-major `Float32` SGD, implemented by
  composing existing CUDA elementwise map kernels.

Unresolved issues:
- Fused CUDA kernels for optimizer families beyond map-backed SGD remain open.
- At this ML-VK-060-011 checkpoint, CUDA Adam/AdamW/RMSProp remained
  fail-closed; see the later ML-VK-060-012/013 entry below for the map-backed
  runtime implementation.
- `nn/train-step` integration remains out of scope until optimizer kernels and
  autograd coverage are sufficient.
- Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this slice.

Next actions:
- Continue `ML-VK-060` with fused CUDA Adam/AdamW/RMSProp or the next
  data-oriented `nn/train-step` prerequisite after autograd readiness is
  confirmed.

Signature: GPT-5 Codex

## 2026-04-20 - ML-VK-060-012/013 CUDA Map-Backed Adam AdamW RMSProp Runtime

- Objective attempted: implement CUDA map-backed `ml/optimizer-step` for dense
  row-major `Float32` Adam, AdamW, and RMSProp.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_ml_optimizer.c3`
  - `src/lisp/prim_ml_optimizer_cuda.c3`
  - `src/lisp/prim_tensor_backend_ops.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
  - `docs/LANGUAGE_SPEC.part-01b.md`
  - `docs/reference/03-collections.part-01.md`
  - `docs/reference/11-appendix-primitives.md`
  - `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - `docs/todo_parts/todo_part_14.md`
  - `.agents/PLAN.md`
  - `memory/changelog_parts/changelog_part_36.md`
  - `memory/CHANGELOG.md`
- Code or configuration changes made:
  - Added CUDA runtime dispatch for Adam/AdamW and RMSProp tensor leaves before
    Vulkan/CPU fallback.
  - Implemented map-backed CUDA Adam/AdamW/RMSProp formulas by composing existing
    CUDA elementwise map kernels, including `sqrt`, without adding fused PTX.
  - Preserved explicit Adam/AdamW first/second moment state, RMSProp
    square-average/velocity state, and integer step state.
  - Added CUDA map-backed `ml/optimizer-step` tests for Adam, AdamW, and RMSProp
    with stateful continuation and mixed-device fail-closed checks.
  - Updated CUDA tensor-backend capability assertions so narrow optimizer keys for
    Adam/AdamW/RMSProp align with `elementwise-map-float32`.
  - Updated language/reference docs, roadmap, TODO, plan, and changelog entries to
    track `ML-VK-060-012/013` as shipped map-backed CUDA optimizer scope, while
    keeping fused CUDA kernels and `nn/train-step` open.
- Commands run:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - Direct `build/main --eval` CUDA capability and Adam/AdamW/RMSProp smoke.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
- Key results:
  - Direct CUDA smoke returned true for capabilities, Adam, AdamW, and RMSProp.
  - Focused advanced collections passed with `pass=1823 fail=0`.
  - Basic Lisp passed with `pass=160 fail=0`.
  - Compiler slice passed with `pass=281 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
  - Broad `ml-optimizer` remains false in both changed docs and tests.
- Unresolved issues:
  - Fused CUDA optimizer kernels remain open, including PTX-only implementations.
  - `nn/train-step` remains out of scope for this checkpoint.
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this slice.
- Signature: GPT-5 Codex

## 2026-04-21 - ML-VK-070-005 Dense NN Training Facade

- Objective attempted: add the first data-oriented NN training facade over the
  shipped DataSpec, `ml/grad`, and `ml/optimizer-step` surfaces.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_nn_training.c3`
  - `src/lisp/eval_init_primitive_tables.c3`
  - `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`
  - `src/entry_build_runtime_manifest_lisp_part3.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
  - `src/lisp/tests_compiler_codegen_groups.c3`
  - docs, TODO, plan, and changelog surfaces for `ML-VK-070-005`.
- Code or configuration changes made:
  - Added `nn/forward`, `nn/grad`, and `nn/train-step`.
  - `nn/grad` lowers train-mode dense or sequential dense-plus-activation model
    data to existing CPU `ml/grad` linear MSE / softmax CE specs.
  - `nn/train-step` composes `nn/grad` with `ml/optimizer-step` and returns
    updated model data and optimizer state without hidden mutation.
  - Added runtime/AOT primitive registration and AOT manifest coverage.
- Key results:
  - `scripts/build_omni_chelpers.sh` passed.
  - `c3c build` linked `build/main`.
  - Focused advanced collections passed with `pass=1828 fail=0`.
  - Compiler slice passed with `pass=284 fail=0`.
  - Basic Lisp passed with `pass=160 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this slice.
  - `ML-VK-070-006` remains open for ergonomic optimizer spec constructors.
  - Broader tape-backed autograd and CUDA/Vulkan backward kernels remain open.
- Signature: GPT-5 Codex

## 2026-04-21 - ML-VK-070-006 Optimizer Spec Constructors

- Objective attempted: add data-first NN optimizer spec constructors for the
  training facade.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_nn_training.c3`
  - `src/lisp/eval_init_primitive_tables.c3`
  - `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
  - `src/lisp/tests_compiler_codegen_groups.c3`
  - docs, TODO, plan, and changelog surfaces for `ML-VK-070-006`.
- Code or configuration changes made:
  - Added `nn/sgd`, `nn/adam`, `nn/adamw`, and `nn/rmsprop`.
  - Constructors validate learning-rate, supported option keys, and
    optimizer-specific hyperparameter ranges.
  - Constructors return ordinary optimizer spec dictionaries consumable by
    `nn/train-step` and `ml/optimizer-step`; no hidden state ownership or
    mutation was introduced.
  - Added runtime/AOT primitive registration and AOT compiler lookup coverage.
- Commands run:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - Direct `build/main --eval` constructor and invalid-range smokes.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Direct constructor and invalid-range smokes returned `true`.
  - Focused advanced collections passed with `pass=1831 fail=0`.
  - Compiler slice passed with `pass=285 fail=0`.
  - Basic Lisp passed with `pass=160 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this slice.
  - Broader tape-backed autograd and fused CUDA optimizer kernels remain open.
  - CUDA/Vulkan backward kernels remain fail-closed through `ml/grad`.
- Signature: GPT-5 Codex

## 2026-04-21 01:33 CEST - ML-VK-080-001 Kernel Value Surface

- Objective attempted: add the first official custom `Kernel` value surface for
  data-oriented user-defined backend kernels.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_kernel.c3`
  - parser postfix path/index chaining
  - runtime type introspection and primitive registration
  - AOT manifest/compiler lookup tables
  - tests, docs, TODO, plan, and changelog surfaces for `ML-VK-080-001`.
- Code or configuration changes made:
  - Added `Kernel(spec)` as a validated dictionary-backed type overlay.
  - Added `kernel/run(kernel inputs push)` as an explicit execution boundary
    that currently fails closed with `tensor/backend-unsupported`.
  - Registered `Kernel` in type symbols, TypeIds, `type-of`, and `is?`.
  - Wired runtime primitive registration, compiler primitive lookup, and AOT
    runtime source manifest coverage.
  - Fixed postfix parsing so `rows.[0].name` and `k.inputs.[0].name` work
    without reintroducing removed leading-dot accessor forms.
- Commands run:
  - `c3c build`
  - Direct `build/main --eval` smokes for Kernel type/path access, raw
    dictionary structural guard, postfix chained access, and fail-closed
    `kernel/run`.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/build_omni_chelpers.sh`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Direct Kernel/path/run smokes returned expected values.
  - Focused advanced collections exited 0.
  - Compiler slice passed with `pass=287 fail=0`.
  - Basic Lisp passed with `pass=161 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this slice.
  - `kernel/run` has no backend compiler/runner yet and intentionally fails
    closed.
  - Graph capture, command-buffer batching, fusion, and device-buffer reuse
    remain open under `ML-VK-080`.
  - Owner revision after this slice: do not implement `(define [kernel] ...)`.
    Named kernels should use ordinary `(define name (Kernel spec))` bindings.
- Signature: GPT-5 Codex

## 2026-04-21 04:38 CEST - ML-VK-080-002 Checked Vulkan Kernel Runner

- Objective attempted: continue `ML-VK-080` by turning `kernel/run` from a pure
  stub into the first real checked Vulkan execution route, without adding
  `(define [kernel] ...)` sugar.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_kernel.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
  - language/reference docs, Vulkan ML roadmap, TODO, plan, changelog, and
    session-report artifacts.
- Code or configuration changes made:
  - Added `kernel/run` support for `backend 'vulkan`, `operation 'scale-f32`.
  - The runner validates one input descriptor, one output descriptor, `Float32`
    descriptor dtypes, descriptor shape agreement, and a `scale` push constant
    representable as `Float32`.
  - Runtime input must resolve to dense row-major Vulkan `Float32` storage.
  - Execution uses the existing checked Vulkan Float32 scale helper and returns
    an ordinary dictionary keyed by the output descriptor name.
  - CPU placement, mixed placement, unsupported dtype, empty tensor, shape
    mismatch, missing push data, and unsupported operations fail closed.
  - Kept arbitrary backend source compilation, graph capture, command-buffer
    batching, fusion, and buffer reuse planning open.
- Commands run:
  - `c3c build`
  - Direct `build/main --eval` smokes for CPU-placement fail-closed and
    Vulkan-gated `scale-f32` execution.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Build linked `build/main`.
  - Direct smokes returned `true`.
  - Focused advanced collections passed with `pass=1837 fail=0`.
  - Basic Lisp passed with `pass=161 fail=0`.
  - Compiler slice passed with `pass=287 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The old `kernel/run` handoff text saying execution always fails closed is
    now stale. The correct current boundary is narrow checked Vulkan
    `scale-f32` execution plus fail-closed unsupported custom compilation.
  - Do not revive `(define [kernel] ...)`; owner explicitly rejected that sugar.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this slice.
  - Arbitrary user kernel source compilation, graph capture, batching, fusion,
    buffer reuse/lifetime planning, and deterministic invalidation remain open
    under `ML-VK-080`.
- Next actions:
  - Continue `ML-VK-080` with operation DAG capture or a second checked
    helper-backed kernel operation before attempting general shader compilation.
- Signature: GPT-5 Codex

## 2026-04-21 05:26 CEST - ML-VK-080-003 Checked Vulkan Add Kernel Runner

- Objective attempted: continue `ML-VK-080` by adding a second real checked
  Vulkan `kernel/run` execution route while preserving the data-oriented
  `Kernel(spec)` surface and not adding `(define [kernel] ...)` sugar.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_kernel.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
  - language/reference docs, Vulkan ML roadmap, TODO, plan, changelog, and
    session-report artifacts.
- Code or configuration changes made:
  - Added `kernel/run` support for `backend 'vulkan`, `operation 'add-f32`.
  - The runner validates two input descriptors, one output descriptor, `Float32`
    descriptor dtypes, empty spec/runtime push dictionaries, descriptor shape
    agreement, same-shape runtime tensors, and dense row-major Vulkan `Float32`
    storage for both inputs.
  - Execution uses the existing checked Vulkan Float32 map helper in add mode
    and returns an ordinary dictionary keyed by the output descriptor name.
  - Refactored shared Kernel runner helpers for output dictionaries and
    descriptor-based Vulkan input resolution, keeping `scale-f32` behavior
    unchanged.
  - CPU placement, non-empty runtime push dictionaries, unsupported dtype,
    empty tensor, shape mismatch, unsupported operations, arbitrary backend
    source compilation, graph capture, batching, fusion, and buffer reuse
    planning remain fail-closed.
- Commands run:
  - `c3c build`
  - Direct `build/main --eval` smokes for Vulkan-gated `add-f32` execution and
    invalid runtime push rejection.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Build linked `build/main`.
  - Direct smokes returned `true`.
  - Focused advanced collections passed with `pass=1839 fail=0`.
  - Basic Lisp passed with `pass=161 fail=0`.
  - Compiler slice passed with `pass=287 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this
    slice.
  - Arbitrary user kernel source compilation, graph capture, batching, fusion,
    buffer reuse/lifetime planning, and deterministic invalidation remain open.
- Next actions:
  - Continue `ML-VK-080` with graph/DAG capture or another checked
    helper-backed runner.
- Signature: GPT-5 Codex

## 2026-04-21 05:52 CEST - ML-VK-080-004 Checked Vulkan Binary Float32 Kernel Family

- Objective attempted: continue `ML-VK-080` by expanding checked helper-backed
  Kernel execution from single `add-f32` coverage to the full Vulkan Float32
  binary map opcode family without adding `(define [kernel] ...)` sugar.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_kernel.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
  - language/reference docs, Vulkan ML roadmap, TODO, plan, changelog, and
    session-report artifacts.
- Code or configuration changes made:
  - Generalized the checked binary Float32 Kernel runner over existing Vulkan
    map opcodes.
  - Added `kernel/run` support for `operation 'sub-f32`, `mul-f32`, `div-f32`,
    `min-f32`, and `max-f32`; `add-f32` now uses the same shared path.
  - Preserved the strict contract: two Float32 input descriptors, one Float32
    output descriptor, matching shapes, empty spec/runtime push dictionaries,
    and dense row-major Vulkan Float32 runtime inputs.
  - Unsupported backend source compilation, graph capture, batching, fusion,
    and buffer reuse planning remain fail-closed.
- Commands run:
  - `c3c build`
  - Direct `build/main --eval` smoke for Vulkan-gated `mul-f32` execution.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Build linked `build/main`.
  - Direct smoke returned `true`.
  - Focused advanced collections passed with `pass=1840 fail=0`.
  - Basic Lisp passed with `pass=161 fail=0`.
  - Compiler slice passed with `pass=287 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this
    slice.
  - Arbitrary user kernel source compilation, graph capture, batching, fusion,
    buffer reuse/lifetime planning, and deterministic invalidation remain open.
- Next actions:
  - Commit and push this slice, then continue `ML-VK-080` with graph/DAG
    capture or another real backend execution boundary.
- Signature: GPT-5 Codex

## 2026-04-21 05:39 CEST - ML-VK-080-006 Checked Vulkan Unary Float32 Kernel Family

- Objective attempted: continue `ML-VK-080` by adding real checked unary
  Kernel execution on top of the existing Vulkan Float32 unary map helper,
  without adding `(define [kernel] ...)` sugar or custom source compilation.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_kernel.c3`
  - `src/lisp/prim_kernel_unary.c3`
  - `src/entry_build_runtime_manifest_lisp_part3.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
  - language/reference docs, Vulkan ML roadmap, TODO, plan, changelog, and
    session-report artifacts.
- Code or configuration changes made:
  - Added unary `kernel/run` operations `abs-f32`, `neg-f32`, `sqrt-f32`,
    `identity-f32`, `zero-f32`, `sin-f32`, `cos-f32`, `tan-f32`, `asin-f32`,
    `acos-f32`, `atan-f32`, `sinh-f32`, `cosh-f32`, `tanh-f32`, `exp-f32`,
    `log-f32`, `log10-f32`, and `normal-cdf-f32`.
  - Split the unary dispatch/validation/execution logic into
    `src/lisp/prim_kernel_unary.c3` to keep `prim_kernel.c3` below the code
    file-size gate.
  - Added the new source file to the AOT runtime source manifest after Stage 3
    source parity caught the missing entry.
  - The unary runner validates one input descriptor, one output descriptor,
    `Float32` descriptor dtypes, matching descriptor/runtime tensor shapes,
    empty spec/runtime push dictionaries, and dense row-major Vulkan Float32
    input storage.
  - Unsupported backend source compilation, graph capture, batching, fusion,
    and buffer reuse planning remain fail-closed.
- Commands run:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Build linked `build/main`.
  - Focused advanced collections passed with `pass=1844 fail=0`.
  - Basic Lisp passed with `pass=161 fail=0`.
  - Compiler slice passed with `pass=287 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Invalidated assumptions or failed approaches:
  - The new split source file was not automatically covered by the AOT runtime
    manifest; Stage 3 source parity failed until
    `src/entry_build_runtime_manifest_lisp_part3.c3` was updated.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this
    slice.
  - Arbitrary user kernel source compilation, graph capture, batching, fusion,
    buffer reuse/lifetime planning, and deterministic invalidation remain open.
- Next actions:
  - Commit and push this slice, then continue `ML-VK-080` with graph/DAG
    capture, source compilation, command-buffer batching, fusion, or
    device-buffer lifetime planning.
- Signature: GPT-5 Codex

## 2026-04-21 05:52 CEST - ML-VK-080-007 Checked Vulkan Single-Node Kernel Capture

- Objective attempted: continue `ML-VK-080` by adding the first real
  graph-capture surface for the checked Vulkan direct-helper Kernel families,
  without changing scalar/Tensor semantics or launching kernels during capture.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_kernel_capture.c3`
  - `src/lisp/eval_init_primitive_tables.c3`
  - `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`
  - `src/entry_build_runtime_manifest_lisp_part3.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
  - `src/lisp/tests_compiler_codegen_groups.c3`
  - language/reference docs, Vulkan ML roadmap, TODO, plan, changelog, and
    session-report artifacts.
- Code or configuration changes made:
  - Added `kernel/capture(kernel inputs push)`.
  - Capture validates the same checked direct-helper Vulkan `Float32` Kernel
    families supported by `kernel/run`: scale, binary, scalar, and unary.
  - Capture returns ordinary `kernel-graph` data with a single `kernel-node`,
    backend, operation, family, dtype, device, direct-helper execution,
    input/output names, concrete runtime shape, push data, and invalidation key.
  - Added primitive registration, AOT compiler primitive lookup, and AOT runtime
    manifest wiring.
  - Unsupported multi-node Tensor expression DAG capture, command-buffer
    batching, fusion, source compilation, and buffer lifetime planning remain
    fail-closed/deferred under explicit TODO scope.
- Commands run:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Build linked `build/main`.
  - Focused advanced collections passed with `pass=1846 fail=0`.
  - Compiler slice passed with `pass=288 fail=0`.
  - Basic Lisp passed with `pass=161 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this
    slice.
  - Multi-node Tensor expression DAG capture, command-buffer batching, fusion,
    arbitrary source compilation, buffer reuse/lifetime planning, and
    deterministic invalidation beyond the single-node key remain open.
- Next actions:
  - Commit and push this slice, then continue `ML-VK-080` toward multi-node
    Tensor expression DAG capture or command-buffer batching/fusion.
- Signature: GPT-5 Codex

## 2026-04-21 06:17 CEST - ML-VK-080-005 Checked Vulkan Scalar Float32 Kernel Family

- Objective attempted: continue `ML-VK-080` by adding real checked scalar
  Kernel execution on top of the existing Vulkan Float32 map helper scalar
  modes, without adding `(define [kernel] ...)` sugar.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_kernel.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
  - language/reference docs, Vulkan ML roadmap, TODO, plan, changelog, and
    session-report artifacts.
- Code or configuration changes made:
  - Added tensor-scalar `kernel/run` operations `add-scalar-f32`,
    `sub-scalar-f32`, `mul-scalar-f32`, `div-scalar-f32`, `min-scalar-f32`,
    and `max-scalar-f32`.
  - Added scalar-left noncommutative operations `scalar-sub-f32` and
    `scalar-div-f32`.
  - The scalar runner validates one input descriptor, one output descriptor,
    `Float32` descriptor dtypes, matching descriptor/runtime tensor shapes,
    `push` spec containing only `scalar 'Float32`, runtime push containing
    exactly one `scalar` value representable as `Float32`, and dense row-major
    Vulkan Float32 input storage.
  - Unsupported backend source compilation, graph capture, batching, fusion,
    and buffer reuse planning remain fail-closed.
- Commands run:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Build linked `build/main`.
  - Focused advanced collections passed with `pass=1842 fail=0`.
  - Basic Lisp passed with `pass=161 fail=0`.
  - Compiler slice passed with `pass=287 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this
    slice.
  - Arbitrary user kernel source compilation, graph capture, batching, fusion,
    buffer reuse/lifetime planning, and deterministic invalidation remain open.
- Next actions:
  - Commit and push this slice, then continue `ML-VK-080` with graph/DAG
    capture or another real backend execution boundary.
- Signature: GPT-5 Codex

## 2026-04-21 06:13 CEST - ML-VK-080-008 Vulkan Tensor Map Graph Capture

- Objective attempted: continue `ML-VK-080` by adding the first multi-node
  Tensor expression graph capture surface for Vulkan ML planning, without
  launching the graph or introducing `(define [kernel] ...)` sugar.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_tensor_capture.c3`
  - `src/lisp/prim_tensor_device_copy.c3`
  - `src/lisp/eval_init_primitive_tables.c3`
  - `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`
  - `src/entry_build_runtime_manifest_lisp_part3.c3`
  - advanced/compiler tests, language/reference docs, Vulkan ML roadmap, TODO,
    plan, and changelog artifacts.
- Code or configuration changes made:
  - Added `tensor/capture(source)` for supported all-Vulkan `Float32`
    concrete/map Tensor expression graphs.
  - Capture returns ordinary `tensor-graph` data with source/map nodes, node ids,
    input edges, scalar operands, output node id, shape, dtype/device/backend,
    family `map-expression`, and invalidation key.
  - Added explicit graph preservation for supported CPU lazy `Float32` map
    expressions under `to-device 'vulkan`, so capture can inspect a real Vulkan
    map DAG instead of only a materialized Tensor.
  - Registered the primitive, added AOT compiler lookup, and wired
    `src/lisp/prim_tensor_capture.c3` into the AOT runtime manifest.
  - Documented the new Tensor graph capture surface and closed
    `ML-VK-080-008` in the live TODO/roadmap.
- Commands run:
  - `c3c build`
  - direct eval smoke for `to-device` graph preservation plus `tensor/capture`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Build linked `build/main`.
  - Direct capture smoke returned a three-node `tensor-graph`.
  - Focused advanced collections passed with `pass=1848 fail=0`.
  - Compiler slice passed with `pass=289 fail=0`.
  - Basic Lisp passed with `pass=161 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Invalidated assumptions or failed approaches:
  - Ordinary Vulkan `map` materializes immediately, so it cannot by itself leave
    a multi-node graph for `tensor/capture`; the useful explicit path is now
    CPU lazy map expression plus `to-device 'vulkan` graph preservation.
  - Returning the captured Tensor source inside the plan caused boundary
    promotion faults for graph-preserved expressions, so `tensor-graph` records
    graph metadata and nodes rather than embedding the source Tensor value.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this
    slice.
  - Contract/view graph capture, command-buffer batching, fusion, arbitrary
    source compilation, buffer reuse/lifetime planning, and broader
    deterministic invalidation remain open.
- Next actions:
  - Continue `ML-VK-080` with contract/view capture or command-buffer
    batching/fusion now that map DAG data exists.
- Signature: GPT-5 Codex

## 2026-04-21 06:23 CEST - ML-VK-080-009 Vulkan Tensor Contract Graph Capture

- Objective attempted: continue `ML-VK-080` by extending Tensor expression graph
  capture from map DAGs to contract DAGs without launching the graph or adding
  kernel declaration sugar.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_tensor_device_copy.c3`
  - `src/lisp/prim_tensor_capture.c3`
  - advanced tests, language/reference docs, Vulkan ML roadmap, TODO, plan, and
    changelog artifacts.
- Code or configuration changes made:
  - Generalized Vulkan expression-preserving `to-device` copying so supported
    CPU lazy `Float32` contract expressions become Vulkan Tensor expressions.
  - Extended `tensor/capture(source)` to count and append contract expression
    nodes.
  - Contract graph nodes record input ids, left/right axes, axis count, output
    shape, dtype/device/backend, and family `contract-f32`.
  - Kept capture metadata-only; the returned plan still does not embed the
    source Tensor or launch/realize the graph.
- Commands run:
  - `c3c build`
  - direct eval smoke for `to-device` contract graph preservation plus
    `tensor/capture`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Build linked `build/main`.
  - Direct contract capture smoke returned
    `[tensor-graph contract-expression 3 tensor-contract 1 0]`.
  - Focused advanced collections passed with `pass=1849 fail=0`.
  - Compiler slice passed with `pass=289 fail=0`.
  - Basic Lisp passed with `pass=161 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Invalidated assumptions or failed approaches:
  - None new. The prior constraint still holds: build capturable DAGs through
    CPU lazy expressions plus explicit `to-device 'vulkan` graph preservation,
    not by assuming direct Vulkan execution leaves an expression graph.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this
    slice.
  - View graph capture, command-buffer batching, fusion, arbitrary source
    compilation, buffer reuse/lifetime planning, and broader deterministic
    invalidation remain open.
- Next actions:
  - Commit and push the slice, then continue `ML-VK-080` with view capture or
    command-buffer batching/fusion.
- Signature: GPT-5 Codex

## 2026-04-21 06:35 CEST - ML-VK-080-010 Vulkan Tensor Direct View Graph Capture

- Objective attempted: continue `ML-VK-080` by extending Tensor expression graph
  capture to the existing direct Vulkan transpose-view boundary without
  claiming arbitrary strided view execution.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_tensor_capture.c3`
  - advanced tests, language/reference docs, Vulkan ML roadmap, TODO, plan, and
    changelog artifacts.
- Code or configuration changes made:
  - Extended `tensor/capture(source)` to count and append direct rank-2
    `matrix/transpose-view` nodes over concrete dense Vulkan `Float32` backing
    tensors.
  - View graph nodes record input id, output shape, strides, storage offset,
    dtype/device/backend, operation `transpose-view`, and family
    `transpose-view-f32`.
  - Kept arbitrary strided views fail-closed and kept capture metadata-only.
- Commands run:
  - `c3c build`
  - direct eval smoke for Vulkan `matrix/transpose-view` plus `tensor/capture`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Build linked `build/main`.
  - Direct view capture smoke returned
    `[tensor-graph view-expression 2 tensor-view transpose-view 1 3]`.
  - Focused advanced collections passed with `pass=1850 fail=0`.
  - Compiler slice passed with `pass=289 fail=0`.
  - Basic Lisp passed with `pass=161 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Invalidated assumptions or failed approaches:
  - None new. Arbitrary strided GPU view execution remains unsupported; this
    slice captures only the existing direct transpose-view representation.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this
    slice.
  - Arbitrary view graph capture, command-buffer batching, fusion, arbitrary
    source compilation, buffer reuse/lifetime planning, and broader
    deterministic invalidation remain open.
- Next actions:
  - Commit and push the slice, then continue `ML-VK-080` with command-buffer
    batching/fusion or broader view planning.
- Signature: GPT-5 Codex

## 2026-04-21 06:46 CEST - ML-VK-080-011 Tensor Graph Schedule Metadata

- Objective attempted: continue `ML-VK-080` by adding non-executing schedule
  metadata to captured Tensor graphs as a bridge toward command planning.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_tensor_capture.c3`
  - advanced tests, language/reference docs, Vulkan ML roadmap, TODO, plan, and
    changelog artifacts.
- Code or configuration changes made:
  - Added per-node execution classes to captured Tensor nodes:
    `external-buffer`, `direct-helper`, and `metadata-only`.
  - Added a topological graph `schedule` array with step, node id, kind,
    execution class, dependencies, and launch flag.
  - Added graph-level `launch-count`, `execution 'not-launched`,
    `command-batching 'none`, and `fusion 'none`.
  - Kept capture non-executing and metadata-only.
- Commands run:
  - `c3c build`
  - direct eval smoke for schedule metadata on a captured Vulkan map graph
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - Build linked `build/main`.
  - Direct schedule smoke returned
    `[2 not-launched external-buffer true true]`.
  - Focused advanced collections passed with `pass=1850 fail=0`.
  - Compiler slice passed with `pass=289 fail=0`.
  - Basic Lisp passed with `pass=161 fail=0`.
  - Primitive docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check` passed.
- Invalidated assumptions or failed approaches:
  - None new. `direct-helper` is descriptive planner metadata and does not
    mean capture launches any helper.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run for this
    slice.
  - Command-buffer batching, fusion, execution, arbitrary source compilation,
    buffer reuse/lifetime planning, and broader deterministic invalidation
    remain open.
- Next actions:
  - Commit and push the slice, then continue `ML-VK-080` with command-buffer
    batching/fusion or buffer planning.
- Signature: GPT-5 Codex
