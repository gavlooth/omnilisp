# Changelog Part 37

## 2026-04-22 - ML-VK-040 Train Batch Normalization DataSpec

- Closed `ML-VK-040-TRAIN-BN-001` by adding explicit stateful
  `nn/batch-normalization(channels channel-axis [options])`.
- The layer spec validates positive `channels`, integer `channel-axis`,
  positive finite `epsilon`, finite `[0, 1]` `momentum`, and optional
  `scale-init`/`bias-init` initializers.
- `nn/init` now creates transparent BN model data: `scale` and `bias`
  parameters plus `running-mean` and `running-variance` state tensors in the
  requested dtype/device.
- `nn/apply` lowers BN inference/eval through the existing explicit-stat
  `ml/batch-normalization` primitive using running state.
- Train-mode `nn/forward` computes current-batch stats for CPU dense row-major
  `Float64`/`Float32` tensors, returns ordinary `nn-forward` data with
  `output`, updated `state`, and updated `model`, and does not mutate the
  original model/state.
- Explicit `nn/forward` arity now accepts `(Dictionary 'mode 'train)` to thread
  state through standalone or sequential specs. CUDA/Vulkan/current-batch BN
  training remains fail-closed without hidden CPU fallback.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1923 fail=0`
  - compiler slice -> `pass=290 fail=0`
  - primitive docs parity
  - e2e baseline policy
  - code file-size gate
  - `git diff --check`

## 2026-04-21 - JIT Tail Error Arguments and Tensor Contract Alias Lineage

- Fixed tail multi-argument closure calls that carry error-valued arguments as
  ordinary data until the callee uses them.
  - `jit_cons_escape(...)` now follows the same promoted-error contract as
    `make_cons(...)`: an `ERROR` value in the cons car is valid argument data
    when promotion produced an acceptable escaped error value.
  - This restores named-let cases such as binding an error-valued `x` while
    returning `y`, and preserves the original error when the callee returns
    `x`.
- Fixed lazy tensor contract destination alias checks after boundary copies.
  - Added `TensorVal.alias_origin` as a compact logical storage-lineage stamp.
  - New tensor payloads receive a fresh origin, boundary clones preserve the
    origin, and transpose views inherit their source origin.
  - `tensor_contract_sources_alias_destination(...)` now catches contract
    source/destination aliases by alias-origin before falling back to raw
    backing-storage pointer equality.
- Validation:
  - `c3c build --obj-out obj`
  - direct named-let tail probes for ignored and preserved error-valued
    arguments
  - direct `realize` source-error propagation probe
  - direct contract alias probes returned
    `"contract: destination aliases source tensor"`
  - direct map-in-place probe returned `5.0`
  - basic Lisp slice: `pass=166 fail=0`
  - focused advanced collections module: `pass=1894 fail=0`
  - code file-size gate
  - targeted `git diff --check`

## 2026-04-21 - FFI Item 7: Generic Callback Registration (`ffi-callback`)

- Added libffi closure support for creating C-callable callbacks from Omni lambdas.
- Added `omni_ffi_closure_alloc`, `omni_ffi_closure_free`, and
  `omni_ffi_test_callback_int_int` to `csrc/ffi_helpers.c`.
- Created `src/lisp/prim_ffi_callback.c3` with:
  - `FfiCallbackContext` struct holding lambda, param types, return type, interp,
    owner scope, and libffi closure handles.
  - `ffi_callback_arg_to_value` and `ffi_callback_value_to_c` conversion functions.
  - `ffi_callback_dispatch` — exported via `@extern("omni_ffi_callback_dispatch")`,
    called from C when libffi invokes the closure. Converts args to Omni values,
    calls `jit_apply_multi_args`, writes the result back.
  - `prim_ffi_callback` primitive supporting two syntaxes:
    - `(ffi-callback lambda ['Type1 'Type2 ...] 'ReturnType)` (list/array form)
    - `(ffi-callback lambda 'Type1 'Type2 ... 'ReturnType)` (variadic form)
  - Type symbols mapped to `FfiTypeTag` via `ffi_symbol_value_to_tag`.
- Registered `{ "ffi-callback", &prim_ffi_callback, -1 }` in
  `eval_init_primitive_tables.c3`.
- Added 6 C3-level tests in `tests_advanced_io_effect_ffi_ffi_surface_groups.c3`:
  list form, variadic form, unsupported parameter type, unsupported return type,
  non-callable first arg, and end-to-end invocation via
  `omni_ffi_test_callback_int_int`.
- Removed unused `src/ffi_callback_trampoline.c3`.
- Validation:
  - `c3c build` passes
  - Advanced FFI system tests: 133 passed, 0 failed
  - Basic tests: 166 passed, 0 failed

## 2026-04-21 - FFI Item 10: Shared dlopen Registry per Library Path

- Created `src/lisp/ffi_dlopen_registry.c3` with thread-safe (`std::thread::Mutex`)
  global registry mapping library path → `{ void* handle; uint refcount; }`.
- `ffi_dlopen_registry_acquire(path)` — returns existing handle + increments refcount,
  or dlopen's new library and adds with refcount=1.
- `ffi_dlopen_registry_lookup(path)` — returns handle without changing refcount.
- `ffi_dlopen_registry_release(path)` — decrements refcount, dlcloses + removes when 0.
- `ffi_dlopen_registry_release_all()` — dlcloses all remaining entries (for teardown).
- Updated `eval_ffi_eval.c3:eval_ffi_lib` to call `registry_acquire` instead of direct
  `dlopen`. Error paths no longer call `dlclose` directly; registry cleanup is handled
  by `ffi_handle_release` when the FfiHandle box is freed.
- Updated `eval_ffi_bound_call.c3:foreign_runtime_load_c_abi_bound` to call
  `registry_acquire` instead of direct `dlopen` for lazy library loading.
- Updated `aot_runtime_bridge_ffi_lib.c3:ffi_declare_lib` to use `registry_acquire`
  so AOT-compiled FFI libraries share handles with interpreter-loaded libraries.
- Updated `value_constructors.c3:ffi_handle_release` to release library handles
  through the shared registry when the FfiHandle box is freed (refcount hits 0).
- `foreign-release` on library handles still returns "handle is not releasable";
  registry cleanup only happens via scope destructor (`scope_dtor_value`), not
  user-facing `foreign-release`.
- Added C3-level test in `tests_advanced_io_effect_ffi_ffi_surface_groups.c3`
  verifying that `(define [ffi lib] a "libc.so.6")` and
  `(define [ffi lib] b "libc.so.6")` share the same `lib_handle` pointer.
- Validation:
  - `c3c build` passes
  - Advanced FFI system tests: 133 passed, 0 failed
  - Basic tests: 166 passed, 0 failed

## 2026-04-21 - ML-VK-080-036 Shared Source Layout and Selected-Region Planner

- Closed the consolidated high-churn `ML-VK-080-036` boundary by replacing the
  previous case-specific residuals with one shared source/layout contract and
  one selected-region planner output consumed by runtime command lowering.
- Kernel source layout contract:
  - Checked direct scale/unary/binary SPIR-V Kernel sources may now declare a
    `metadata` dictionary with kind `kernel-source-layout`, version `1`,
    matching ABI, descriptor layout, dtype `Float32`, input/output counts, and
    optional push layout.
  - Constructor validation and `kernel/run` runtime dispatch both check this
    dictionary before accepting direct SPIR-V words.
  - The legacy binary `metadata 'storage2-output1-f32-v1` spelling remains
    accepted for the checked binary ABI only.
- Tensor selected-region planner:
  - `tensor/capture(source)` now records top-level `selected-region-plan`
    metadata with kind `tensor-selected-region-plan`, version `1`, backend
    `vulkan`, dtype `Float32`, and `selected-region-command-plan` policy.
  - Candidate regions cover scalar-map chains, tensor/scalar map chains,
    direct-view scalar chains, view/tensor scalar chains, and contract scalar
    chains.
  - Each candidate names the consumed command batch, node ids, dependencies,
    dispatch ids, launch/dispatch/barrier counts, and
    `native-selected-region-executor` runtime support.
- Runtime consumption:
  - `tensor/run(graph)` now requires a matching selected-region candidate plus
    validated command-batch metadata before entering any native selected-region
    executor.
  - Missing or corrupted selected-region plan metadata skips the native route
    and falls back to serial Vulkan graph replay, preserving existing numeric
    behavior without inferring native lowering from node shape alone.
- Contract:
  - Source-language parsing/compilation, reflection, arbitrary direct-SPIR-V
    descriptor schemas beyond checked scale/unary/binary ABIs,
    memory-plan-backed runtime reuse, arbitrary mixed schedules, and fused
    dispatch execution remain future capability boundaries rather than open
    `ML-VK-080` case-specific children.
- Validation:
  - `c3c build`
  - focused advanced collections module with `pass=1892 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate
  - `git diff --check`
  - open `ML-VK-080-0xx` TODO scan returned no unchecked child items

## 2026-04-21 - ML-VK-080-013 Tensor Memory-Plan Metadata

- Added metadata-only memory planning to `tensor/capture(source)` for supported
  all-Vulkan `Float32` Tensor graph capture.
  - The top-level capture result remains ordinary `kind 'tensor-graph`; this
    preserves existing schedule, command-batch planning, execution, fusion,
    shape, and invalidation metadata.
  - Graph plans now include a nested `memory-plan` dictionary with kind
    `tensor-memory-plan`, `version 1`, backend `vulkan`, dtype `Float32`,
    policy `metadata-only`, false `allocates`, false `retains-handles`,
    `external-bytes`, `transient-bytes`, and `node-memory`.
  - Captured nodes now expose layout/allocation metadata:
    `element-count`, `byte-length`, `storage-offset`, `storage-elements`,
    `storage-bytes`, `allocation`, `owner`, and `write-policy`.
  - Concrete source nodes are marked as external/self/read-only storage.
  - Map and contract nodes are marked as planned transient/write-once storage.
  - Direct transpose-view nodes are marked as aliases owned by their view
    source and read-only views.
  - Byte totals aggregate external storage bytes separately from planned
    transient output bytes; alias/view nodes do not add transient or external
    allocations.
- This remains descriptive metadata only.
  - Capture does not allocate runtime buffers.
  - Capture does not retain Tensor handles.
  - Capture does not reuse runtime buffers.
  - Capture does not record, submit, or execute Vulkan command buffers.
- Invalidated approach:
  - Do not make `tensor/capture(source)` return top-level
    `kind 'tensor-memory-plan`; that drops the graph contract and previously
    shipped schedule/command-batch metadata.
- Validation:
  - `c3c build`
  - focused advanced collections slice with `pass=1852 fail=0`
  - compiler slice with `pass=289 fail=0`
  - basic Lisp slice with `pass=161 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate
  - `git diff --check`

## 2026-04-21 - ML-VK-080-014 Tensor Fusion Eligibility Metadata

- Added metadata-only fusion eligibility planning to `tensor/capture(source)`
  for supported all-Vulkan `Float32` Tensor graph capture.
  - The top-level capture result remains `kind 'tensor-graph`.
  - The top-level `fusion` field remains `none`.
  - Graph plans now include a nested `fusion-plan` dictionary with kind
    `tensor-fusion-plan`, `version 1`, backend `vulkan`, dtype `Float32`,
    policy `eligibility-only`, candidate records, barrier records, and false
    `fused`.
  - Direct Vulkan Float32 map chains with two or more direct-helper map nodes
    are reported as `map-chain` fusion candidates.
  - Contract and direct transpose-view nodes are reported as hard fusion
    barriers with `contract-boundary` and `view-boundary` reasons.
- This remains descriptive metadata only.
  - Capture does not compile fused shaders.
  - Capture does not record, submit, or execute Vulkan command buffers.
  - Capture does not allocate or reuse runtime buffers.
- At the time of this slice, runtime work remained explicitly open for
  executable Tensor command-buffer batching, source-backed custom Kernel
  compilation/dispatch, and contracted buffer reuse/lifetime planning.
- Validation:
  - `c3c build`
  - focused advanced collections slice with `pass=1852 fail=0`
  - compiler slice with `pass=289 fail=0`
  - basic Lisp slice with `pass=161 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate
  - `git diff --check`

## 2026-04-21 - ML-VK-080-015 Vulkan Map Chain Command-Buffer Batch

- Added the first executable Tensor command-buffer batching slice for Vulkan
  `Float32` map expressions.
  - Supported path: an explicit `to-device 'vulkan` graph expression followed
    by `realize` for a two-node scalar map chain such as
    `(map * (map + x (Float32 3.0)) (Float32 2.0))`.
  - The native helper records both scalar map dispatches into one command
    buffer, inserts a shader write/read barrier for the intermediate buffer,
    and submits once.
  - Runtime dispatch uses the existing Vulkan Float32 map shader and existing
    explicit placement route; no hidden CPU fallback is introduced.
  - A focused test asserts both numeric correctness and that the new native
    chain dispatch counter increments when Vulkan Float32 is available.
- Scope boundaries:
  - `tensor/capture` remains metadata-only and does not execute graph
    dictionaries.
  - Arbitrary captured graph execution remains open as `ML-VK-080-018`.
  - Source-backed custom `Kernel` compilation/dispatch remains open as
    `ML-VK-080-016`.
  - Contracted buffer reuse/lifetime planning remains open as `ML-VK-080-017`.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - focused advanced collections slice with `pass=1854 fail=0`
  - compiler slice with `pass=289 fail=0`
  - basic Lisp slice with `pass=161 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate
  - `git diff --check`

## 2026-04-21 - ML-VK-080-016/017/018 Source Kernels, Reuse Metadata, Tensor Graph Run

- Added registered-source custom Kernel dispatch for Vulkan `Float32`.
  - `kernel/run` now detects `Kernel.source` and routes `source-scale-f32`
    through a new source-backed path.
  - Supported source contract: `source 'ml-clip-scale-f32`, `entry 'main`,
    one Vulkan `Float32` input, one Vulkan `Float32` output, and push
    `scale Float32`.
  - Native execution goes through a new exported helper,
    `omni_tensor_backend_vulkan_kernel_source_scale_f32`, which dispatches the
    registered SPIR-V payload through the generic Vulkan shader-module helper.
  - Unsupported source names, non-`main` entrypoints, arbitrary source text,
    and user-provided SPIR-V arrays fail closed.
- Extended Tensor capture memory planning to version 2.
  - Moved memory-plan helpers out of `prim_tensor_capture.c3` into
    `prim_tensor_capture_memory_plan.c3` to keep code files below the 700 LOC
    split gate.
  - Nested `memory-plan` now records `reuse-policy`, `runtime-ownership`,
    `executes-reuse`, `reuse-group-count`, and `reuse-groups`.
  - Transient reuse groups are metadata-only candidates; the runtime still does
    not allocate, retain, alias, transfer ownership, or reuse Vulkan buffers.
- Added `tensor/run(graph)` for captured all-Vulkan `Float32` source/map graph
  dictionaries.
  - `tensor/capture` now stores executable concrete source tensors on
    `tensor-source` nodes as `source-value`.
  - `tensor/run` validates the captured graph dictionary and replays
    `tensor-source` and `tensor-map` nodes through existing Vulkan map helpers.
  - Contract/view nodes, fused dispatch, command-buffer lowering from capture
    metadata, mixed placement, unsupported dtypes, and hidden CPU fallback fail
    closed.
- Split remaining work:
  - `ML-VK-080-019`: arbitrary source compilation or direct user-provided
    SPIR-V arrays for custom `Kernel`.
  - `ML-VK-080-020`: runtime buffer reuse/ownership transfer.
  - `ML-VK-080-021`: broader captured graph executor semantics for
    contract/view nodes and command-buffer-backed execution.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - focused advanced collections slice with `pass=1857 fail=0`
  - compiler slice with `pass=290 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate

## 2026-04-21 - ML-VK-080-019/021 Source Validation and Tensor Graph Replay

- Added checked direct SPIR-V source data validation for custom `Kernel`.
  - `Kernel.source` now accepts the existing registered-source symbol form,
    registered-source dictionaries, and direct SPIR-V dictionaries with
    unsigned 32-bit word arrays.
  - Direct SPIR-V word-array validation checks the SPIR-V header magic,
    positive version, positive id bound, and schema 0.
  - Direct SPIR-V runtime execution still fails closed with
    `tensor/backend-unsupported`; executable direct SPIR-V or source-language
    compilation is split into `ML-VK-080-022`.
- Extended `tensor/run(graph)` beyond source/map replay.
  - Captured all-Vulkan `Float32` contract nodes replay through the existing
    Vulkan contract helper with captured axis and shape validation.
  - Captured direct `matrix/transpose-view` nodes replay as view metadata via
    the existing transpose-view construction path.
  - Arbitrary strided views, view-consuming map/contract materialization,
    fused dispatch, command-buffer lowering from capture metadata, mixed
    placement, unsupported dtypes, and hidden CPU fallback remain closed.
- Recorded runtime reuse direction from parallel inspection.
  - Real reuse should not mutate or alias `TensorVal.device_handle` in C3.
  - The next safe boundary is an opaque native Vulkan executor that owns
    scratch buffers for a synchronous graph run and transfers exactly one final
    output handle to a fresh `TensorVal`.
- Split remaining work:
  - `ML-VK-080-020`: runtime buffer reuse/ownership transfer through an opaque
    native executor boundary.
  - `ML-VK-080-022`: executable direct SPIR-V or source-language custom
    `Kernel` compilation.
  - `ML-VK-080-023`: command-buffer-backed captured graph execution and any
    required view-consuming map/contract materialization semantics.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - focused advanced collections slice with `pass=1861 fail=0`
  - compiler slice with `pass=290 fail=0`
  - basic Lisp slice with `pass=161 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate
  - `git diff --check`

## 2026-04-21 - ML-VK-080-020 Tensor Run Native Reuse Boundary

- Added the first runtime reuse/ownership-transfer path for captured Tensor
  graph execution.
  - `tensor/run(graph)` now detects the captured all-Vulkan `Float32`
    source -> scalar map -> scalar map graph shape.
  - Eligible graphs route through the existing native Vulkan two-map
    command-buffer batch helper instead of serially replaying both map nodes.
  - The native helper borrows the concrete source device handle, owns the
    intermediate scratch buffer internally, and transfers one final output
    handle to a fresh Tensor value.
- Ownership boundaries:
  - This does not mutate or alias `TensorVal.device_handle` in C3.
  - This does not make `memory-plan` execute general reuse; the capture plan
    remains metadata-only with `executes-reuse false`.
  - This does not add a general graph buffer pool or region-wide scratch
    allocator.
- Added a focused test that checks numeric correctness, source immutability,
  Vulkan placement, and native reuse-path execution through the existing
  map-chain dispatch counter.
- Split remaining reuse work:
  - `ML-VK-080-024`: generalized runtime reuse beyond the initial
    two-scalar-map graph boundary through a native opaque executor.
- Validation:
  - `c3c build`
  - focused advanced collections slice with `pass=1863 fail=0`
  - compiler slice with `pass=290 fail=0`
  - basic Lisp slice with `pass=161 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate
  - `git diff --check`

## 2026-04-21 - ML-VK-080-024 Linear Scalar-Map Runtime Reuse

- Generalized the captured graph runtime reuse path from the initial two-map
  shape to linear scalar-map chains.
  - Added a native Vulkan scalar-chain executor that accepts arrays of scalar
    map operations, scalar modes, and scalar values.
  - The executor records the chain into one command buffer, inserts barriers
    between dispatches, owns all intermediate scratch buffers internally, and
    transfers one final output handle to C3.
  - `tensor/run(graph)` now detects captured all-Vulkan `Float32` source ->
    scalar-map* graphs with two or more map nodes and routes them through the
    native scalar-chain executor.
- Ownership boundaries:
  - No C3 `TensorVal.device_handle` mutation or aliasing was introduced.
  - Capture `memory-plan` remains metadata-only; this is a runtime executor
    path, not general memory-plan execution.
  - Tensor/tensor map chains, contract/view-consuming graph regions, fused
    dispatch, and arbitrary command-buffer lowering remain open.
- Added focused tests for:
  - two-map captured graph reuse through the generic scalar-chain counter;
  - longer captured scalar-map graph reuse, numeric correctness, Vulkan
    placement, and captured source immutability.
- Split remaining reuse work:
  - `ML-VK-080-025`: generalized runtime reuse beyond linear scalar-map
    chains, including tensor/tensor map support or selected graph regions.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - focused advanced collections slice with `pass=1865 fail=0`
  - compiler slice with `pass=290 fail=0`
  - basic Lisp slice with `pass=161 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate
  - `git diff --check`

## 2026-04-21 - ML-VK-080-025 Tensor-Map Selected-Region Runtime Reuse

- Extended captured graph runtime reuse beyond linear scalar-map chains.
  - Added a native Vulkan selected-region executor for dense same-shape
    `Float32` tensor/tensor map -> scalar-map* regions with two concrete
    all-Vulkan source tensors.
  - The executor records the tensor/tensor map and following scalar maps into
    one command buffer, inserts barriers between dispatches, owns all
    intermediate scratch buffers internally, and transfers one final output
    handle to C3.
  - `tensor/run(graph)` now validates graph shape and captured
    `command-buffer-candidate` metadata before using the native selected-region
    executor. Invalid metadata still falls back to existing serial replay when
    the graph nodes themselves are supported.
- Ownership boundaries:
  - No C3 `TensorVal.device_handle` mutation or aliasing was introduced.
  - Capture `memory-plan` remains metadata-only; this is an opaque native
    executor path, not general runtime memory-plan execution.
  - Contract/view-consuming regions, mixed multi-source DAGs, fused dispatch,
    and arbitrary command-buffer lowering remain open.
- Added a focused test for tensor/tensor map plus scalar-map reuse that verifies
  native counter movement, numeric correctness, Vulkan placement, and captured
  source immutability.
- Split remaining selected-region reuse work:
  - `ML-VK-080-027`: selected-region runtime reuse beyond dense tensor/tensor
    map plus scalar-map tails, including contract/view-consuming regions,
    mixed DAGs, or memory-plan-backed reuse.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - focused advanced collections slice with `pass=1869 fail=0`
  - compiler slice with `pass=290 fail=0`
  - basic Lisp slice with `pass=161 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate
  - `git diff --check`

## 2026-04-21 - ML-VK-080-026 Tensor-Map Command-Buffer Metadata Lowering

- Closed the concrete command-buffer lowering slice that became executable with
  the selected-region runtime executor.
  - `tensor/run(graph)` now validates captured `command-buffer-candidate`
    metadata for dense same-shape `Float32` tensor/tensor map -> scalar-map*
    regions, not only single-source scalar-map chains.
  - The lowering checks external source dependencies, dispatch node ids,
    dispatch operations, launch/dispatch counts, and batch node ordering before
    entering the native selected-region command-buffer executor.
  - Invalid command-batch metadata still skips native command-buffer execution
    and uses serial replay when the graph nodes remain supported.
- Split remaining command-buffer lowering work:
  - `ML-VK-080-028`: command-buffer lowering beyond dense tensor/tensor map
    selected regions, including contract nodes, direct-view consumers, selected
    mixed graph regions, or required view materialization semantics.
- Validation:
  - covered by the ML-VK-080-025 validation sweep: native helper rebuild,
    `c3c build`, focused advanced collections `pass=1869 fail=0`, compiler
    `pass=290 fail=0`, basic Lisp `pass=161 fail=0`, primitive docs parity,
    Stage 3 source parity, code file-size gate, and `git diff --check`.

## 2026-04-21 - Tensor Graph-Run Chain Helper Split

- Split native chain-detection helpers out of `src/lisp/prim_tensor_graph_run.c3`
  into `src/lisp/prim_tensor_graph_run_chains.c3`.
  - The main graph runner now keeps source/map/contract/view replay and the
    `tensor/run` primitive entrypoint.
  - The new chain helper module owns scalar-chain and dense tensor/tensor map
    -> scalar-map* native reuse detection.
  - `src/entry_build_runtime_manifest_lisp_part3.c3` now includes the new
    C3 source file.
- Rationale:
  - `prim_tensor_graph_run.c3` had reached 676 LOC after the selected-region
    work and would block the next runtime-reuse slice under the hard 700 LOC
    code-file gate.
  - After the split, `prim_tensor_graph_run.c3` is 533 LOC and
    `prim_tensor_graph_run_chains.c3` is 146 LOC.
- Validation:
  - `c3c build`
  - focused advanced collections slice with `pass=1869 fail=0`
  - code file-size gate
  - `git diff --check`

## 2026-04-21 - ML-VK-080-022 Direct SPIR-V Kernel Execution

- Added executable direct SPIR-V word-array support for checked Vulkan
  `source-scale-f32` kernels.
  - `kernel/run` no longer rejects direct SPIR-V sources before execution for
    the `source-scale-f32` operation.
  - C3 copies the validated `Kernel.source.words` array into temporary native
    `uint32_t` scratch before dispatch.
  - The native Vulkan backend validates the SPIR-V header, creates a compute
    pipeline from the provided words, and dispatches through the same two-buffer
    scale ABI used by the registered source-backed path.
- Contract:
  - This is executable direct SPIR-V for the checked `source-scale-f32` ABI:
    one Vulkan `Float32` input tensor, one output tensor, entry `main`, and
    push constants `{count, scale}`.
  - Arbitrary source languages, arbitrary descriptor layouts, arbitrary custom
    operations, and unvalidated shader interfaces remain fail closed.
- Added a focused regression that passes the built-in scale shader as a direct
  SPIR-V word array and verifies Vulkan output placement plus numeric results.
- Split remaining source work:
  - `ML-VK-080-029`: source-language custom `Kernel` compilation or broader
    direct-SPIR-V ABI contracts.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - focused advanced collections slice with `pass=1870 fail=0`
  - compiler slice with `pass=290 fail=0`
  - basic Lisp slice with `pass=161 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate
  - `git diff --check`

## 2026-04-21 - ML-VK-080-027 Contract-Scalar Selected-Region Runtime Reuse

- Extended captured graph selected-region runtime reuse beyond dense
  tensor/tensor map tails.
  - `tensor/run(graph)` now detects captured all-Vulkan `Float32` contract ->
    scalar-map* regions with two concrete source tensors.
  - The C3 graph runner validates the contract node, scalar-map chain,
    source dependencies, output shape, captured command-batch nodes, and
    dispatch operations before entering the native selected-region executor.
  - The native executor owns the contract intermediate and scalar scratch
    buffers internally and transfers one final Vulkan output handle to C3.
- Ownership boundaries:
  - No C3 `TensorVal.device_handle` mutation or aliasing was introduced.
  - Capture `memory-plan` remains metadata-only; direct-view consumers, mixed
    DAGs, and general memory-plan-backed reuse remain split to
    `ML-VK-080-031`.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - focused advanced collections slice with `pass=1873 fail=0`

## 2026-04-21 - ML-VK-080-028 Contract-Scalar Command-Buffer Lowering

- Closed the concrete command-buffer lowering slice beyond tensor/tensor map
  selected regions.
  - `omni_tensor_backend_vulkan_contract_scalar_chain_f32` now records the
    contract dispatch and following scalar-map dispatches into one Vulkan
    command buffer.
  - The command buffer inserts shader-write/read barriers from contract output
    to the first scalar map and between scalar tail dispatches.
  - The C3 route only uses this helper after validating captured
    `command-buffer-candidate` metadata for the selected contract-scalar
    region.
- Split remaining command-buffer work:
  - `ML-VK-080-032`: direct-view consumers, selected mixed graph regions, and
    fused dispatch plans still need explicit materialization/stride semantics
    before they can lower into one-command native executors.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - focused advanced collections slice with `pass=1873 fail=0`

## 2026-04-21 - ML-VK-080-029 Direct SPIR-V ABI Contract

- Made the direct SPIR-V source ABI explicit.
  - Direct source dictionaries may now carry `abi 'source-scale-f32-v1` for
    the checked Vulkan scale ABI.
  - Omitted `abi` remains compatible with the same checked scale ABI.
  - Any other direct-SPIR-V `abi` fails closed during `Kernel` validation
    instead of being treated as an implicit future layout.
- Contract:
  - This closes the broader direct-SPIR-V ABI contract for the currently
    executable source path.
  - Source-language compilation and arbitrary direct-SPIR-V descriptor layouts
    beyond the checked scale/unary ABIs remain split to `ML-VK-080-033`.
- Validation:
  - `c3c build`
  - focused advanced collections slice with `pass=1873 fail=0`

## 2026-04-21 - ML-VK-080-030 Source Unary SPIR-V ABI

- Added a second executable checked direct-SPIR-V ABI for Vulkan custom
  `Kernel` sources.
  - `kernel/run` now routes source-backed unary `Float32` operations through a
    checked `source-unary-f32-v1` ABI when `Kernel.source` is either
    `format 'builtin-spirv` with `name 'map-unary-f32` or `format 'spirv`
    with direct word-array data and `abi 'source-unary-f32-v1`.
  - The native helper validates Vulkan availability, Float32 support, SPIR-V
    headers for direct word arrays, the unary operation id, tensor lengths, and
    the one-input/one-output `Float32` byte contract before dispatch.
  - Registered and direct unary source paths share the existing unary shader
    ABI: push constants `{count, op, padding0, padding1}` and two storage
    buffers.
- Contract:
  - `source-scale-f32-v1` remains the checked scale ABI; omitted direct-source
    ABI remains compatible only with that scale ABI.
  - `source-unary-f32-v1` must be explicit for direct unary SPIR-V sources.
  - Arbitrary source languages, arbitrary descriptor layouts, recursive source
    compilation, and fused/custom graph lowering remain fail closed and are
    split to `ML-VK-080-033`.
- Tests:
  - Added constructor validation for `source-unary-f32-v1`.
  - Added runtime coverage for registered `map-unary-f32` and direct
    word-array `source-unary-f32-v1` sqrt kernels.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - focused advanced collections module with `pass=1875 fail=0`

## 2026-04-21 - ML-VK-080-031/032 Direct-View Scalar-Chain Runtime Lowering

- Extended selected-region runtime reuse to the first direct-view consumer
  family.
  - `tensor/run(graph)` now detects captured all-Vulkan `Float32` source ->
    direct transpose-view -> scalar-map* regions and routes validated regions
    through a native Vulkan view/scalar-chain executor.
  - The native executor records the scalar-tail dispatches into one command
    buffer, uses view-stride metadata for the first tensor operand, uses dense
    scratch/output metadata for materialized intermediates, owns C-side scratch
    buffers, and transfers one final dense Vulkan output handle.
  - `to-device 'vulkan` graph copy now preserves transpose-view nodes instead
    of eagerly realizing them, and Vulkan-touching lazy map construction keeps
    non-concrete view operands lazy until the explicit graph copy path.
- Extended command-buffer lowering to the same direct-view scalar-tail family.
  - Captured command-batch metadata is validated for the view dependency,
    batch node ids, dispatch node ids, scalar operations, launch shape, and
    barrier shape before entering the native executor.
  - Invalid command-batch metadata skips the native view/scalar-chain path and
    falls back to serial graph replay.
  - Serial replay can now read Vulkan transpose-view payloads without forcing
    hidden CPU fallback while still producing dense concrete outputs.
- Residuals:
  - `ML-VK-080-034`: mixed DAG or memory-plan-backed selected-region runtime
    reuse remains fail closed.
  - `ML-VK-080-035`: selected mixed-region or fused command-buffer lowering
    remains fail closed.
  - `ML-VK-080-033`: arbitrary source-language compilation or arbitrary
    direct-SPIRV descriptor-layout schemas remain fail closed.
- Negative memory:
  - Do not reuse the dense scalar-chain helper for view consumers; it writes
    one dense metadata shape for output and operands, but transpose-view
    consumers require view strides for the first dispatch input and dense
    strides for scratch/output.
  - Do not mark intermediate lazy map expressions over Vulkan views as
    Vulkan-device expressions during construction. That hit the boundary
    promotion guard; keep them structurally lazy until explicit `to-device`
    graph copying preserves the view.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - focused advanced collections module with `pass=1879 fail=0`
  - compiler slice with `pass=290 fail=0`
  - basic Lisp slice with `pass=161 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate
  - `git diff --check`

## 2026-04-21 - ML-VK-080-033/034/035 Source-Binary ABI and Mixed DAG Lowering

- Added the checked `source-binary-f32-v1` direct-SPIR-V Kernel ABI.
  - Direct `Kernel.source` dictionaries may now declare `format 'spirv`,
    `abi 'source-binary-f32-v1`, SPIR-V `words`, and optional
    `metadata 'storage2-output1-f32-v1`.
  - `kernel/run` validates the existing binary `Float32` Kernel descriptor
    contract, requires two Vulkan `Float32` inputs with matching concrete
    shapes, rejects push dictionary entries, copies SPIR-V words into native
    scratch, and dispatches the supplied shader through the three-storage
    buffer Vulkan path.
  - Arbitrary source languages, reflection, push layouts, descriptor schemas,
    and non-checked direct-SPIR-V ABIs remain fail closed and are split to
    `ML-VK-080-036`.
- Added native selected-region reuse for a concrete mixed DAG family.
  - `tensor/run(graph)` now detects captured all-Vulkan `Float32` source ->
    direct transpose-view plus dense second source -> tensor/tensor map ->
    scalar-map* graphs and routes validated metadata through a native mixed
    Vulkan executor.
  - The native executor records the tensor/tensor dispatch and scalar-tail
    dispatches into one command buffer, uses view strides for the first map
    operand, uses dense strides for the second source and scalar tails, owns
    C-side scratch buffers, and transfers one final dense output handle.
- Added command-buffer lowering validation for the same mixed DAG family.
  - The lowering checks source/view dependencies, batch node ids, dispatch
    operations, launch shape, and barrier shape before entering the native
    path.
  - Invalid command-batch metadata skips the native mixed path and falls back
    to serial replay.
- Churn correction:
  - The remaining work is no longer tracked as three independent residuals.
    The source/custom Kernel lane, selected-region runtime reuse lane, and
    command-buffer lowering lane each accumulated repeated case-specific slices
    while preserving the same broader capability as a residual.
  - `ML-VK-080-036` is now the consolidated semantic closure boundary: define
    one shared source/layout contract plus one selected-region planner output
    that command lowering can consume.
  - Former `ML-VK-080-037` and `ML-VK-080-038` are absorbed as sub-boundaries of
    `ML-VK-080-036`, not independent case-specific work items.
- Negative memory:
  - Do not inline large source-binary direct-SPIR-V execution fixtures in the
    long advanced collections harness. Standalone `--eval` execution of the
    optimized 389-word binary-add shader returned `[vulkan 9.0 16.0]`, but
    placing that execution fixture after the direct scale/unary SPIR-V tests
    corrupted the JIT/apply path. Keep constructor/fail-closed coverage in the
    suite and use a standalone probe or a future non-variadic fixture builder
    for direct source-binary runtime validation.
- Tests:
  - Added constructor validation for `source-binary-f32-v1` and
    `storage2-output1-f32-v1` metadata.
  - Added fail-closed `kernel/run` coverage for wrong direct ABI, CPU
    placement, non-empty push dictionaries, and source-binary shape mismatch.
  - Added mixed DAG capture/runtime regressions, including native-dispatch
    counter coverage and invalid command-batch fallback.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - focused advanced collections module with `pass=1886 fail=0`
  - standalone source-binary direct-SPIR-V `--eval` probe returned
    `[vulkan 9.0 16.0]`

## 2026-04-21 - ML-VK-080-023 Scalar-Map Command-Batch Metadata Lowering

- Lowered captured scalar-map command-batch metadata into the runtime executor.
  - `tensor/run(graph)` now validates the captured `command-buffer-candidate`
    batch before routing a linear scalar-map graph to the native scalar-chain
    command-buffer executor.
  - The lowering validates batch execution kind, barrier policy, launch and
    dispatch counts, batch node ids, dispatch node ids, dispatch operations,
    and external source dependency metadata.
  - If command-batch metadata is missing or invalid, the scalar-chain native
    path is skipped and `tensor/run` falls back to existing serial helper
    replay rather than inferring command-buffer execution from node shape alone.
- Added a focused regression that mutates captured command-batch metadata and
  verifies numeric fallback still works while the native scalar-chain dispatch
  counter does not move.
- Split remaining command-buffer work:
  - `ML-VK-080-026`: command-buffer lowering beyond scalar-map metadata,
    including tensor/tensor maps, contract nodes, selected mixed regions, or
    required view materialization semantics.
- Validation:
  - `c3c build`
  - focused advanced collections slice with `pass=1867 fail=0`
  - compiler slice with `pass=290 fail=0`
  - basic Lisp slice with `pass=161 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate
  - `git diff --check`

## 2026-04-21 - Audit and Memory Remediation Wave 1

- Closed five actionable items from the audit/proposal backfill:
  - `AUDIT-2026-C1-ADDRINFO-ABI`
  - `AUDIT-2026-C2-RELATION-VALUE-LIFETIME`
  - `AUDIT-2026-H1-FORMAT-STRINGS`
  - `AUDIT-2026-L1-INVALID-SYMBOL-ID`
  - `MEMORY-P0-PROMOTION-LEAKS`
- Replaced C3-side hardcoded `struct addrinfo` and `sockaddr` offset reads
  with typed C helper functions for connect and IP rendering paths, and wired
  the helper archive build to include `csrc/addrinfo_helpers.c`.
- Changed materialized deduce relation column-key caching from long-lived
  `Value*` pointers to stable `SymbolId` values. Row materialization now
  constructs root-backed dictionary symbol keys at output time.
- Fixed the audit-listed C3 format-string mismatches using C3-supported
  `%d`/`%x` forms. Do not apply C printf width spellings mechanically here:
  the C3 compiler rejects `%ld`, `%zu`, and `%u`.
- Replaced raw `0xFFFFFFFF` symbol-id sentinel checks in the audit-listed
  compiler and JIT closure helper files with `INVALID_SYMBOL_ID`.
- Hardened promotion/escape failure paths:
  - partial closure wrapper clone failure now releases materialized payload
    state;
  - cloned closure payloads start detached until env-scope ownership is
    established;
  - promoted cons abort paths clean partially built ESCAPE spines;
  - destination cons escape aborts clean materialized cons wrappers;
  - partial closure promotion failure releases retained/detached env scope.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `scripts/check_async_fallback_policy.sh`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    passed with `pass=290 fail=0`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=query ./build/main --test-suite lisp`
    passed with `pass=214 fail=0`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=async ./build/main --test-suite lisp`
    passed with `pass=65 fail=0`
  - bounded container `memory-lifetime-smoke` passed with `pass=231 fail=0`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Residuals:
  - ASAN was not run for this wave.
  - Raw `0xFFFFFFFF` values outside the audit-listed symbol-id sentinel scope
    were left untouched.
  - The remaining audit/proposal backlog items stay open in
    `docs/todo_parts/todo_part_15.md`.

## 2026-04-21 - Audit and Memory Remediation Wave 2

- Closed four additional audit/proposal items from
  `docs/todo_parts/todo_part_15.md`:
  - `AUDIT-2026-H3-IGNORED-RETURNS`
  - `AUDIT-2026-H6-BOUNDED-CSTR`
  - `AUDIT-2026-H7-OPTIONAL-UNWRAP`
  - `AUDIT-MEM-P1-STRUCT-SIZE-ASSERTS`
- Ignored-return handling:
  - interp macro/module init now explicitly leaves recoverable empty storage
    when initial allocation fails, preserving existing storage-hardening tests;
  - env hash-table allocation/rebuild failures are explicit and documented as
    linear-lookup fallbacks;
  - FFI handle and REPL session cleanup failures now log warnings;
  - tensor default dtype inference fails closed for invalid inferred data;
  - TCP listen setup closes/fails when `SO_REUSEADDR` setup fails.
- Bounded C strings:
  - project-path resolution now scans `getcwd` and explicit project-dir inputs
    only within the fixed project path buffer capacity;
  - REPL server host/auth token C strings now scan within `REPL_SERVER_LINE_MAX`
    and fail closed on unterminated input.
- Global scope mutex initialization:
  - replaced force unwrap with an explicit catch path that aborts with the
    named invariant `scope global mutex init failed`;
  - no unlocked fallback is allowed because the mutex guards shared scope
    freelist/generation state.
- Struct-size assertions:
  - added compile-time `$assert`s for measured ABI/interop sizes:
    `Value` 56, `ScopeRegion` 144, `Closure` 88, `TensorVal` 296,
    `HashMap` 24, `Array` 24, `Env` 136, `Interp` 2288, `Primitive` 88,
    `SymbolTable` 40, and `MethodTable` 40.
- Validation:
  - `c3c build`
  - basic Lisp slice `pass=161 fail=0`
  - async Lisp slice `pass=65 fail=0`
  - compiler slice `pass=290 fail=0`
  - focused advanced collections slice `pass=1892 fail=0`
  - project-path smoke
  - REPL-server stdio smoke
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Residuals:
  - Synthetic unterminated-pointer fixtures were not added for H6.
  - ASAN was not run for this wave.

## 2026-04-21 - Audit and Memory Remediation Wave 3

- Closed four additional audit/proposal items from
  `docs/todo_parts/todo_part_15.md`:
  - `AUDIT-2026-H2-VALUETAG-EXHAUSTIVE`
  - `AUDIT-2026-H4-REQUIRE-CONTRACTS`
  - `AUDIT-2026-M7-FAULT-INJECTION-GLOBALS`
  - `MEMORY-P0-SPLICE-SCOPE-GEN`
- ValueTag exhaustiveness:
  - replaced hidden `default:` fallbacks in the audit-listed `ValueTag`
    switches with explicit handled cases across evaluator dispatch,
    env-copy, boundary provenance/reachability, checkpoint encoding, and JIT
    literal comparison;
  - left non-`ValueTag` defaults in tensor-capture op/payload switches intact.
- Contract hardening:
  - added focused `@require` contracts for high-risk dereference assumptions in
    evaluator falsiness, boundary alias traversal, kernel source validation and
    source-backed runners, and neural checkpoint helpers;
  - preserved null-tolerant boundary transfer probes where tests intentionally
    verify fail-closed null behavior.
- Fault-injection globals:
  - gated symbol-table, type-registry, and interp lifecycle deterministic
    allocation-failure globals behind `OMNI_TEST_FAULT_INJECTION`;
  - normal builds now compile those probes inactive and do not export the
    `g_symbol_table_force_*`, `g_type_registry_force_*`, or `g_interp_force_*`
    symbols.
- Escape splice stamps:
  - added a root-aware splice transfer helper that walks the committed root
    graph before `scope_splice_escapes`, rewrites child ESCAPE value/env
    generation stamps to the parent ESCAPE generation, updates closure/env
    owner-scope metadata, and rejects adoption on traversal failure;
  - commit paths now use the root-aware splice helper while the legacy rootless
    transfer helper remains available for direct transfer probes;
  - added a memory-lifetime smoke regression covering adopted root/leaf/tail
    generation stamps.
- Validation:
  - `c3c build`
  - compiler slice `pass=290 fail=0`
  - normal basic slice `pass=150 fail=0`
  - feature-gated build:
    `c3c -D OMNI_TEST_FAULT_INJECTION --obj-out obj-fi build`
  - feature-gated basic slice `pass=161 fail=0`
  - bounded container `memory-lifetime-smoke` `pass=232 fail=0`
  - scoped ValueTag default scan
  - normal-binary `nm` check for absence of gated fault-injection globals
  - `git diff --check`
- Residuals:
  - ASAN was not run for this wave.
  - Debug reachability validation at the splice boundary remained open at this
    checkpoint and was closed in the next wave.

## 2026-04-21 - Audit and Memory Remediation Wave 4

- Closed `AUDIT-MEM-P1-SPLICE-REACHABILITY`.
- Added `boundary_debug_graph_audit_pre_splice_escape_root`, a policy-gated
  graph-audit check that runs at the root-aware boundary splice layer before
  raw `scope_splice_escapes` adoption.
- The raw `main` scope layer still owns chunk transfer and TEMP teardown, but
  committed-root reachability validation lives at the Lisp boundary call site
  because that is where the root graph and global-env traversal context exist.
- Root-aware splice now rejects pre-splice graph-audit violations with
  `BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID` before generation-stamp
  rewrite or chunk adoption.
- Added a memory-lifetime smoke regression for the debug-audit path:
  graph audit is enabled, a releasing ESCAPE cons points at a releasing TEMP
  leaf, and the splice is rejected while both objects remain in the releasing
  scope.
- Validation:
  - `c3c build`
  - bounded container `memory-lifetime-smoke` `pass=233 fail=0`
- Residuals:
  - ASAN was not run for this wave.

## 2026-04-21 - Audit and Memory Remediation Wave 5

- Closed `AUDIT-2026-M1-DUPLICATION-CONSTANTS`.
- Added shared ML constants for:
  - GELU tanh-approximation cube scale and sqrt-two-over-pi factors;
  - optimizer default epsilon, Adam beta defaults, and RMSProp alpha default.
- Added shared async I/O constants for TCP read limits and HTTP request/response
  buffer limits.
- Replaced duplicated TCP/HTTP fiber-required and loop-unavailable raiser bodies
  with shared async I/O helper functions while preserving existing operation
  names, codes, and fallback messages.
- Merged `eval_error` and `eval_error_expr` through one location-aware
  constructor.
- Moved tuple encoded-size ownership to `deduce_tuple_codec.c3`; while doing so,
  corrected symbol tuple sizing to 5 bytes, matching the encoder's tag plus
  four-byte symbol id.
- Validation:
  - `c3c build`
  - basic slice `pass=150 fail=0`
  - async slice `pass=65 fail=0`
  - deduce query slice `pass=214 fail=0`
  - focused advanced collections `pass=1892 fail=0`
  - `git diff --check`
- Residuals:
  - Broader cosmetic helper dedupe outside the audit-listed families was left
    untouched.

## 2026-04-21 - Audit and Memory Remediation Wave 6

- Closed `AUDIT-MEM-P0-DEFER-MULTI-RESOURCE`.
- Converted `tensor_clone_payload` from repeated manual `tensor_free_payload`
  returns to one scoped cleanup guard with an explicit commit flag. Successful
  view, Vulkan, CUDA, and concrete/map/contract clone returns now transfer
  ownership by setting the commit flag immediately before returning.
- Converted shared wrapper parent-copy paths for arrays, hashmap-backed
  wrappers, and method tables to scoped cleanup guards.
  - Array guards track copied item count.
  - Hashmap guards track initialized/processed slots.
  - Method-table guards track copied signatures/entries and fallback ownership.
- Converted matching escape-promotion wrappers to the same guard shape so child
  promotion failures and result-wrapper allocation failures share cleanup
  behavior.
- Tightened method-table abort cleanup: if implementation copy/promotion fails
  after a signature copy succeeds, the current entry now counts as copied for
  cleanup, so the copied signature is destroyed instead of relying on every
  error branch to remember that local state.
- Validation:
  - `c3c build`
  - bounded container `memory-lifetime-smoke` `pass=233 fail=0`
  - `git diff --check`
- Residuals:
  - `c3c build --sanitize=address` was attempted, but the local `c3c`
    toolchain rejected sanitizer mode before building with:
    `Address sanitizer is only supported on Linux, FreeBSD, NetBSD, Darwin and Windows.`

## 2026-04-21 - Audit and Memory Remediation Wave 7

- Closed `AUDIT-2026-M3-FOREACH-LOOPS`.
- Converted practical manual index loops to `foreach` where the index was not
  part of the behavior:
  - `PrintBuf.append_str` now iterates source chars directly while preserving
    the destination capacity guard.
  - Hashmap and set print buffers iterate backing entries with count-style
    slices while preserving slot order.
  - Kernel source SPIR-V validation/source-dict validation and word copying use
    foreach iteration.
  - Tensor capture metadata array creation and membership scans use foreach.
  - NN checkpoint array encode/decode, dictionary entry encode, and JSON-source
    whitespace scan use foreach.
  - Closure env self-reference scanning uses foreach over bindings.
- Kept indexed loops where the index carries semantics: paired method-signature
  arrays, reverse list reconstruction, Tensor shape presentation, shape/stride
  math, sparse slot identity where needed, and offset/count-sensitive builder
  logic.
- Negative memory:
  - Do not use C3 `[..len]` pointer slices as count-style foreach input in
    these runtime loops. Validation showed that form included one extra element
    for the checkpoint array path. Use `[0:len]` for count-style pointer slices.
- Validation:
  - `c3c build`
  - basic slice `pass=150 fail=0`
  - focused advanced collections module `pass=1892 fail=0`
  - model checkpoint round-trip `--eval` probe returned `true`
  - `git diff --check`

## 2026-04-21 - Audit and Memory Remediation Wave 8

- Closed `AUDIT-2026-M4-GLOBAL-STATE-CONTRACTS`.
- Documented scheduler global state as single-runtime-thread fiber/uv-loop
  state; cross-thread offload completion remains limited to the existing task
  mutex, condition variable, reliable wakeup, and offload queues.
- Documented JIT runtime globals and the JIT tracked-state pool/spill list as
  owner-thread-only state protected by `jit_require_owner_thread`.
- Replaced the open-coded `g_jit_compile_nonce_counter` increment in
  `jit_track_compiled_state` with `runtime_backend_next_compile_nonce`, which
  re-enters the owner-thread guard and preserves nonzero nonce wrapping.
- Documented symbol-table, type-registry, and interpreter allocation
  fault-injection globals as feature-gated, test-only, single-threaded controls
  compiled out of normal builds.
- Validation:
  - `c3c build`
  - compiler slice `pass=290 fail=0`
  - scheduler slice `pass=113 fail=0`
  - `scripts/check_jit_env_scope_guards.sh`
  - `scripts/check_scheduler_state_guards.sh`
  - `git diff --check`
- Residuals:
  - No atomics or mutexes were added for JIT globals because the current runtime
    contract is thread-affine. Multi-thread JIT compilation would be a broader
    contract change around owner-token and state-pool ownership.

## 2026-04-21 - Audit and Memory Remediation Wave 9

- Closed `AUDIT-2026-M5-MODULE-CYCLE`.
- Added `docs/plans/main-lisp-module-cycle-isolation-2026-04-21.md` as the
  explicit contract for the current `main` <-> `lisp` module cycle.
- Mapped the current boundary:
  - 22 `module main` files import `lisp`; all are entrypoint, CLI, REPL,
    build, bindgen, source-check, or test adapter surfaces.
  - 413 files under `src/lisp` import `main`, primarily for scope/region,
    stack/coroutine, lifecycle, runtime entry, bindgen manifest, diagnostics,
    and test services still hosted in `main`.
- Documented that a real structural break should extract a neutral runtime
  module around `scope_region*`, `stack_engine*`, and related lifecycle/owner
  helpers rather than reshuffling random leaf imports.
- Restored guarded AOT Lisp runtime manifest parity discovered by the required
  Stage 3 source parity check:
  - `src/entry_build_runtime_manifest_lisp_part0.c3` now includes
    `src/lisp/async_io_shared.c3`.
  - `src/entry_build_runtime_manifest_lisp_part3.c3` now includes
    `src/lisp/prim_ml_constants.c3`.
- Updated `docs/plans/README.md` and `docs/todo_parts/todo_part_15.md` to
  reflect the closed isolation boundary.
- Validation:
  - exact `module main` importer scan
  - exact `src/lisp -> main` importer count
  - `c3c build`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Residuals:
  - Neutral runtime module extraction remains a future high-blast-radius
    modularization task if the project later wants the cycle physically broken.

## 2026-04-21 - Audit and Memory Remediation Wave 10

- Closed `AUDIT-2026-M6-BUILD-CONFIG`.
- Updated `project.json` so the project manifest includes:
  - FTXUI include roots: `third_party/ftxui/include` and
    `third_party/ftxui/src`.
  - Helper C/Vulkan ML sources that were present in
    `scripts/build_omni_chelpers.sh` but missing from the project manifest,
    including `addrinfo_helpers`, view/mixed dispatch helpers, ML Vulkan
    helper families, and ML shader SPV source translation units.
- Added `scripts/check_build_config_parity.sh` as a focused guard for:
  - JSON parseability;
  - FTXUI include-dir coverage;
  - helper-source coverage between the helper archive builder and
    `project.json`;
  - e2e and AOT library-path override hooks.
- Replaced hardcoded host library search paths where practical:
  - `scripts/run_e2e.sh` now uses `OMNI_RUNTIME_TOOLCHAIN_LIB_PATH` with the
    existing Docker toolchain override still taking precedence.
  - `src/entry_build_backend_compile.c3` now uses
    `OMNI_AOT_LINK_LIBRARY_PATH` for the AOT link library search path, falling
    back to `/usr/local/lib`.
- Kept a separate C3 test target out of scope: the repo currently has one
  executable target, and test separation is selected by runtime/test scripts
  rather than a second project target.
- Updated `docs/PROJECT_TOOLING.part-02.md` with the new runtime/AOT library
  path overrides.
- Validation:
  - `python3 -m json.tool project.json`
  - `scripts/check_build_config_parity.sh`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/run_ftxui_smoke.sh`
  - `LD_LIBRARY_PATH="${OMNI_RUNTIME_TOOLCHAIN_LIB_PATH:-/usr/local/lib}" ./build/main --help`
  - `git diff --check`
- Residuals:
  - `OMNI_E2E_COMPILE_ONLY=1 scripts/run_validation_container.sh scripts/run_e2e.sh`
    was attempted, but the validation image failed before repo build with
    `/usr/local/bin/c3c: cannot execute binary file: Exec format error`.

## 2026-04-21 - Audit and Memory Remediation Wave 11

- Closed `AUDIT-2026-L2-JIT-FILE-NAMES`.
- Mechanically renamed every active double-prefix JIT C3 file under
  `src/lisp/` to the single-prefix `jit_*.c3` form.
- Updated active references in source manifests, scripts, current docs/plans,
  TODOs, and audit/proposal files so active source/script/planning surfaces no
  longer contain the old redundant JIT file prefix.
- Stage 3 AOT source parity now lists the renamed JIT files under
  `src/entry_build_runtime_manifest_lisp_part2.c3`.
- The rename exposed two legacy `make_error` calls in migrated JIT effect
  surfaces; converted them to canonical payloaded runtime errors:
  - `src/lisp/jit_handle_signal.c3`
  - `src/lisp/jit_reset_shift.c3`
- Validation:
  - `c3c build`
  - compiler slice `pass=290 fail=0`
  - jit-policy slice `pass=51 fail=0`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_effects_contract_policy.sh`
  - active redundant JIT-prefix reference scan
  - `git diff --check`
- Residuals:
  - `scripts/check_boundary_hotpath_formatting.sh` still fails on debug
    `io::printfn` calls in `src/lisp/jit_eval_scopes.c3`; that belongs to the
    open `AUDIT-2026-L5-DEBUG-BUILD-SIDE-EFFECTS` item.

## 2026-04-21 - Audit and Memory Remediation Wave 12

- Closed `AUDIT-2026-L5-DEBUG-BUILD-SIDE-EFFECTS`.
- Verified the audit-listed `DEBUG_BUILD` blocks in
  `src/lisp/jit_compiler_compile.c3` and
  `src/lisp/runtime_backend_hooks_cache.c3` only gate diagnostics:
  - JIT pool-pressure GC scheduling remains outside the debug block.
  - Runtime cache clear/retry/commit behavior remains outside debug blocks.
  - Pool warning-state mutations remain outside debug blocks.
- Added `scripts/check_debug_build_side_effects.sh` to keep those debug blocks
  diagnostic-only.
- Moved opt-in TCO scope trace formatting out of hot
  `src/lisp/jit_eval_scopes.c3` into `src/lisp/jit_eval_scope_trace.c3`, then
  added the new helper to the guarded AOT Lisp runtime manifest.
- Validation:
  - `c3c build`
  - jit-policy slice `pass=51 fail=0`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `scripts/check_debug_build_side_effects.sh`
  - `scripts/check_boundary_hotpath_formatting.sh`
  - `git diff --check`
- Residuals:
  - None for this item.

## 2026-04-21 - Audit and Memory Remediation Wave 13

- Closed `AUDIT-2026-L4-MAGIC-NUMBERS`.
- Named the remaining audit-listed raw constants:
  - `src/lisp/prim_math.c3`: random mantissa mask and `2^52` scale.
  - `src/lisp/prim_nn_init.c3`: LCG multiplier/increment and `2^32` scale.
  - `src/stack_engine_backend_contract.c3`: x86 MXCSR and x87 control-word
    defaults.
  - `src/lisp/prim_collection_hashmap_key_helpers.c3`: time-zone offset hash
    bias.
- Confirmed the async TCP and HTTP buffer-size values were already represented
  by named constants in `src/lisp/async_io_shared.c3`.
- Validation:
  - `c3c build`
  - basic slice `pass=150 fail=0`
  - async slice `pass=65 fail=0`
  - focused advanced collections `pass=1892 fail=0`
  - stack suite `pass=24 fail=0`
  - `git diff --check`
- Residuals:
  - None for this item.

## 2026-04-21 - Audit and Memory Remediation Wave 14

- Closed `MEMORY-P1-TELEMETRY`.
- Added cumulative scope-memory telemetry counters for:
  - TEMP and ESCAPE aligned allocation bytes.
  - TEMP and ESCAPE chunk bytes acquired/freed.
  - Scope create/release/destroy/recycle counts.
- Extended copy-to-parent stats with outermost promotion allocation bytes so
  `copy-to-parent-bytes` can quantify copy/promotion cost without double
  counting recursive copy calls.
- Added `runtime-memory-stats`, returning a structured dictionary with
  `scope`, `scope-transfer`, `fiber-temp`, and `copy-to-parent` sections.
- Added `OMNI_MEM_TELEMETRY=1` teardown output for runtime entry paths and the
  Lisp test runner. The output is a compact JSON object carrying the same
  headline counters for process-level inspection.
- Added basic/native telemetry regressions covering the new primitive and the
  direct scope counter path.
- Validation:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(runtime-memory-stats)'`
  - `OMNI_MEM_TELEMETRY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '1'`
  - basic Lisp slice `pass=152 fail=0`
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- Residuals:
  - None for this item.

## 2026-04-21 - Audit and Memory Remediation Wave 15

- Closed `MEMORY-P2-JIT-ESCAPE-OPCODES`.
- Added JIT ESCAPE-lane allocation helpers for tail-return construction:
  - `jit_make_nil_escape`
  - `jit_cons_escape`
  - `jit_make_array_escape`
  - `jit_make_string_escape` plus a string-literal wrapper helper
- Lowered proven tail constructors to the ESCAPE variants:
  - tail `cons` calls allocate the pair directly in ESCAPE after promoting
    children;
  - tail `List`/`list` calls build the returned spine with ESCAPE nil/cons;
  - tail `Array` calls allocate an ESCAPE array wrapper and store
    ESCAPE-promoted elements;
  - tail string literals are copied into ESCAPE-owned string storage.
- Kept shadowed constructor names and non-tail constructor calls on the normal
  apply/allocation path, preserving local binding semantics.
- Added JIT policy regression coverage for tail constructor escape allocation
  counters and returned values.
- Validation:
  - `c3c build`
  - focused JIT policy slice with
    `OMNI_JIT_POLICY_FILTER=tail-constructor-escape-opcode`: `pass=1 fail=0`
  - full JIT policy slice: `pass=52 fail=0`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(runtime-memory-stats)'`
- Residuals:
  - None for the proposal-scoped cons/array/string tail-constructor helpers.

## 2026-04-21 - Audit and Memory Remediation Wave 16

- Closed `MEMORY-P2-TCO-LANE-RESET`.
- Verified the existing TCO recycle implementation already includes:
  - `scope_reset_temp_lane(ScopeRegion*)`, which runs TEMP destructors, frees
    overflow TEMP chunks, resets the TEMP bump cursor, and preserves ESCAPE
    chunks/destructors;
  - `runtime_prepare_tco_recycle_env`, which routes eligible TCO bounces
    through TEMP-lane reset when env frames, bindings, root-persistent boxes,
    and nested binding graphs make reset safe;
  - guarded fallback to fresh-scope env copying when surviving TEMP-reachable
    data would make a fast reset unsafe.
- Validation:
  - scope suite: `63 passed, 0 failed`
  - full JIT policy slice: `pass=52 fail=0`
  - bounded container `tco-recycling` slice with
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`: `pass=11 fail=0`
  - bounded container memory-lifetime smoke with
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`: `pass=233 fail=0`
- Operational note:
  - The default validation image's `/usr/local/bin/c3c` is x86-64 in this
    environment and fails with `Exec format error` on the aarch64 host. Mount
    the local aarch64 toolchain via `OMNI_VALIDATION_TOOLCHAIN_ROOT` for
    bounded validation here.
- Residuals:
  - None for TEMP-lane reset routing. The next open P2 item is
    `MEMORY-P2-TCO-ENV-REUSE`, which targets reducing remaining env-copy churn.

## 2026-04-21 - Audit and Memory Remediation Wave 17

- Closed `MEMORY-P2-TCO-ENV-REUSE`.
- Added shared env-frame reuse guards for generic boundary env-copy and JIT TCO
  env-copy:
  - target/root-owned env frames can be reused directly when the parent can be
    kept as-is and all binding values pass the existing boundary provenance
    reuse rules;
  - root-persistent mutable boxes keep the existing identity-preserving
    parent-rewrite path;
  - frames or bindings that still depend on the releasing scope continue to
    materialize replacements and route copied binding values through the
    existing copy sites.
- Added memory-lifetime regression coverage for target-owned env-frame reuse
  across both the generic boundary copy wrapper and the JIT TCO env-chain copy.
- Validation:
  - `c3c build`
  - bounded container memory-lifetime smoke with
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`: `pass=234 fail=0`
  - bounded container `tco-recycling` slice with
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`: `pass=11 fail=0`
- Residuals:
  - None for Tier 2 env-frame reuse. Remaining open memory proposal work is
    Tier 3 evaluation/design work: inline small collections, scope paging, and
    per-thread scope pools.

## 2026-04-21 - Audit and Memory Remediation Wave 18

- Closed the remaining Tier 3 memory proposal evaluation items:
  - `MEMORY-P3-INLINE-SMALL-COLLECTIONS`
  - `MEMORY-P3-SCOPE-PAGING`
  - `MEMORY-P3-PER-THREAD-SCOPE-POOLS`
- Added `docs/plans/memory-tier3-evaluation-2026-04-21.md` as the decision
  note for the long-term memory architecture items.
- Key decisions:
  - Do not implement literal inline collection storage in `Value` without
    benchmark evidence; it would break the current 56-byte `Value` ABI and the
    pointer-backed collection payload contract. Future work should start, if
    justified, with an `Array` payload-level small-buffer pilot that leaves
    `Value` unchanged.
  - Do not ship TEMP lane 4KB/16KB/64KB paging without class-level telemetry
    for chunk capacities, wasted bytes at release/reset, and fiber-temp
    behavior by capacity.
  - Do not implement cross-thread scope pools without a transfer contract.
    Same-owner-thread local recycle caches may be feasible later, but the
    current scope and JIT runtimes intentionally fail fast on non-owner-thread
    use.
- Validation:
  - Design/inspection closure only after the P2 build and bounded memory slices
    had passed; no representation or allocator policy code was changed in this
    wave.
- Residuals:
  - No open `MEMORY-*` proposal TODO items remain in `docs/todo_parts/todo_part_15.md`.
  - Future implementation should begin with telemetry/benchmark items, not
    direct representation or threading rewrites.

## 2026-04-21 - Ownership Hardening Documentation Wave

- Closed `OWNERSHIP-HARDENING-001`.
- Added `ADR-2026-04-21-A: Scope/Region Ownership Contract` to
  `docs/ARCHITECTURE.md`.
- Added ownership review guidance to `docs/C3_STYLE.md`.
- The accepted contract now records:
  - `ScopeRegion` as the ownership unit for ordinary Omni language values;
  - TEMP and ESCAPE lane responsibilities;
  - the committed ESCAPE root invariant forbidding reachable Omni-owned TEMP
    edges;
  - shared boundary promotion/copy context requirements;
  - the retired status of `scope_adopt` in normal return flow;
  - the no-per-type-RC policy for ordinary language values;
  - the narrow opaque foreign-resource exception policy;
  - durable graph, handle/ID, static allocation-routing, and boundary
    vocabulary rules;
  - examples for lists, iterators, closure capture, root/global define, Tensor
    lazy expressions, FFI/backend handles, mutable collection update, and
    failure during ESCAPE publication.
- The `docs/C3_STYLE.md` checklist now requires new graph-carrying tags and
  wrappers to name owned edges, boundary-copy routes, ESCAPE promotion routes,
  root-store behavior, graph-audit traversal or opaque exclusion, destructor
  authority, rollback behavior, and regression coverage.
- Validation:
  - docs-only change; no runtime semantics changed.
- Residuals:
  - None for `OWNERSHIP-HARDENING-001`. Runtime stats visibility is already
    covered by the landed `runtime-memory-stats` primitive and
    `OMNI_MEM_TELEMETRY=1` teardown JSON.

## 2026-04-21 - Plan TODO Backfill Wave

- Closed `META-PLAN-TODO-BACKFILL-001`.
- Audited 113 Markdown plan files under `docs/plans/` with local scans and
  three parallel subagent review slices.
- Added live TODO tracking for the plan residuals that were not already
  represented in `docs/todo_parts/`:
  - `RUNTIME-FILE-SPLIT-QUEUE-001`
  - `UI-LIB-FACADE-001`
  - `FTXUI-C-ABI-CONTRACT-001`
  - `FTXUI-C-ABI-WRAPPERS-001`
  - `REPL-SERVER-MULTICLIENT-001`
  - `FFI-CALLBACK-UV-001`
  - `FALLBACK-RUNTIME-OWNERSHIP-001`
  - `ACCESS-UNIFY-INDEX-REF-002`
- Added closed historical tracking for `STACK-AARCH64-001`, because
  `docs/plans/stack-aarch64-implementation-plan-2026-04-08.md` already records
  arm64 runtime parity evidence and the related `STACK-AARCH64-CONT-001` item
  is closed.
- Skipped closed decision-only plans and already TODO-backed plan families,
  including the fixed-width complex tensor `TENSOR-100H-*` lanes.
- Validation:
  - docs-only change; `git diff --check` is the governing validation for this
    backfill wave.
- Residuals:
  - The backfilled open items are now the source of truth for those plan
    residuals. Continue from the individual TODO entries rather than from an
    implicit `docs/plans/` scan.

## 2026-04-21 - Index/Ref Error-Path Parity Wave

- Closed `ACCESS-UNIFY-INDEX-REF-002`.
- Confirmed implementation state:
  - `prim_ref(...)` and `jit_do_index(...)` both route array/list/dict/string
    lookup through `ref_try_lookup_collection(...)`;
  - unsupported collection errors route through shared `ref_type_error(...)`;
  - `jit_compile_index(...)` still spills the collection before compiling the
    index expression, then reloads before calling `jit_do_index(...)`.
- Added regression coverage in `src/lisp/tests_core_groups.c3` for:
  - array/list/string out-of-bounds parity;
  - array/list/string wrong-index-type parity;
  - dictionary missing-key `nil` parity;
  - unsupported collection error parity;
  - canonical postfix error messages for array/list/string out-of-bounds and
    wrong-index-type errors.
- Validation:
  - `c3c build --obj-out obj` passed.
  - Direct rebuilt-binary probes for representative canonical postfix errors
    returned `true`.
  - `OMNI_LISP_TEST_SLICE=basic` ran the new index tests and increased the
    pass count, but the slice remains red on two unrelated tail multi-arg
    failures: `pass=164 fail=2`.
  - Plain `c3c build` remains blocked in this workspace by the known missing
    `build/obj/linux-aarch64` object/link state; use `--obj-out obj` for this
    recovery path.
- Residuals:
  - None for index/ref error-path parity. Dictionary has no out-of-bounds or
    wrong-index-type error case because dictionary lookup accepts arbitrary
    keys and missing keys return `nil`.

## 2026-04-21 - Runtime Split Queue Superseded

- Closed `RUNTIME-FILE-SPLIT-QUEUE-001` without splitting code.
- The owner rule is now: do not split files unless they exceed 1000 LOC.
- Superseded `docs/plans/largest-runtime-files-pass-2026-03-19.md` because its
  `Next Queue` was stale:
  - `src/lisp/aot_runtime_bridge.c3`: current `126` LOC, not `433`;
  - `src/lisp/async_tcp_transport_core.c3`: current `126` LOC, not `431`;
  - `src/lisp/eval_promotion_copy.c3`: current `284` LOC;
  - `src/lisp/eval_promotion_escape.c3`: current `196` LOC;
  - `src/lisp/primitives_meta_types.c3`: current `169` LOC;
  - `src/lisp/prim_string_format.c3`: current `59` LOC.
- Current `src/` + `csrc/` source inventory has no code file above 1000 LOC;
  the largest observed source file is
  `src/lisp/tests_runtime_feature_jit_groups_failures.c3` at `762` LOC.
- Negative memory:
  - Do not reopen the 2026-03-19 largest-runtime-file queue as active work
    from the plan title or stale status alone.
  - Under the current owner policy, file splitting is not a valid churn target
    unless a file exceeds 1000 LOC or a real semantic ownership boundary
    independently requires the split.
- Validation:
  - Current line-count inventory confirmed no source file above 1000 LOC.
  - Targeted `git diff --check` passed for the touched artifacts.

## 2026-04-21 - FFI Async Offload Lifetime and Float32 Returns

- Closed `AUDIT-2026-FFI-ASYNC-OFFLOAD-LIFETIME-FLOAT32`.
- Added an owned custom offload context release hook:
  - `OffloadWork` now carries an optional `custom_free` callback;
  - queued worker records release owned custom context after callback execution;
  - admission, cancellation, backend-unavailable, pending-fiber, and OS-thread
    error paths release owned custom context when work never reaches execution;
  - `ffi-async-call` now transfers its heap call context to the scheduler
    instead of freeing it immediately after enqueue.
- Fixed C ABI Float32 return storage:
  - sync FFI calls now pass a `float` return slot to libffi for
    `FFI_TYPE_FLOAT32`;
  - async FFI calls do the same and widen the returned `float` value to the
    existing async double-result lane after the call completes.
- Follow-up async contract hardening:
  - sync variadic C ABI argument inference now lives in
    `ffi_pack_variadic_call_arg(...)`;
  - `ffi-async-call` reuses that helper for variadic extras instead of
    treating every extra argument as a raw pointer;
  - async string returns now copy through the scheduler shared-byte result
    lane, including null string returns mapping to `nil`;
  - pointer-like async returns (`^ForeignHandle`, buffer, and struct lanes)
    now fail closed instead of returning a raw address as an `Integer`.
- Added FFI surface tests using `sinf(Float32)` so non-zero Float32 return
  payloads catch bit-pattern-as-integer regressions, plus regressions for
  async variadic `snprintf`, async `strchr` string/null returns, and fail-closed
  async pointer-like returns.
- Validation:
  - `c3c build --obj-out obj`
  - focused FFI advanced slice:
    `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface`
    -> `pass=109 fail=0`
  - targeted `git diff --check`

## 2026-04-21 - ML-VK-080 Graph Replay Fail-Closed Hardening

- Closed `ML-VK-080-HARDEN-001`.
- `Kernel` construction now rejects source-backed specs whose `entry` is not
  `main`, matching the runtime source-dispatch contract at construction time.
- Bare symbolic `source 'map-unary-f32` now validates and executes consistently
  with the already-supported dictionary `builtin-spirv` unary source form.
- `tensor/run(graph)` now rejects malformed binary map nodes that carry both
  `left-scalar` and `right-scalar` instead of silently preferring one side.
- `tensor/capture` now validates map operand shape before appending graph
  nodes, preventing malformed unary/binary TensorVal payloads from being
  partially captured.
- Validation:
  - `c3c build --obj-out obj`
  - direct rebuilt-binary probes for non-`main` Kernel rejection and both-scalar
    graph replay rejection returned `true`
  - broader `advanced-collections-module` passed with `pass=1895 fail=0`

## 2026-04-21 - UV Timer Callback Handle Closure

- Closed `FFI-CALLBACK-UV-001`.
- Confirmed the shipped surface is the explicit `uv`-scoped callback-handle
  lane only:
  - `__raw-uv-timer-callback-handle`
  - `__raw-uv-timer-callback-invoke`
  - `__raw-uv-timer-callback-unhandle`
  - `__raw-uv-timer-callback-dispatch-count`
- Preserved the product boundary that generic closure-to-native-function-pointer
  coercion remains unshipped and fail-closed.
- Added focused lifetime coverage for the two remaining teardown edges:
  - FFI wrapper box/value allocation failure after C shim registration releases
    the retained callback owner scope through the shared `make_ffi_handle_ex`
    finalizer path.
  - Wrapper-scope destruction without explicit unhandle runs the FFI handle
    finalizer and releases the retained callback owner scope.
- Validation:
  - `c3c build --obj-out obj`
  - focused FFI advanced slice:
    `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface`
    -> `pass=72 fail=0`
  - targeted `git diff --check`

## 2026-04-21 - Runtime Ownership Fallback Typed Fault Checkpoint

- Advanced `FALLBACK-RUNTIME-OWNERSHIP-001` without closing it.
- `copy_to_parent_with_fault(...)` now reports missing target/releasing scope
  provenance as `BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE` in checked mode
  by returning `null` and setting the typed fault, rather than returning an
  `ERROR` value through the checked helper surface.
- Extended the boundary copy fault regression to cover both the public checked
  facade and the direct checked helper.
- Split the scope-chain raw scan loop from its policy gate so a hinted lookup
  that falls back to a raw chain scan consumes `scope_chain_scan_budget` once
  for the logical lookup, not once before fallback and again inside the raw
  scan.
- Split `boundary_can_reuse_value_with_target_chain(...)` from the generic
  reuse predicate so `boundary_classify_return_value(...)` reuses its already
  computed target-chain membership instead of performing a second target-chain
  scan in the same classification decision.
- Added a focused scan-budget regression in `memory-lifetime-smoke`.
- Invalidated a broad deletion path: bounded `memory-lifetime-smoke` with
  `OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BYPASS=1` failed reuse/classification
  invariants (`pass=215 fail=9`). Do not globally bypass or delete scope-chain
  scans until route-specific replacements preserve those semantics.
- Validation:
  - `c3c build --obj-out obj`
  - `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`
  - bounded `memory-lifetime-smoke` normal run -> `pass=235 fail=0`
  - bounded counters-enabled `memory-lifetime-smoke` run -> `pass=235 fail=0`
  - `scripts/check_file_size_gate.sh`
  - targeted `git diff --check`

## 2026-04-21 - Runtime Ownership Fallback Destination Checked Copy

- Advanced `FALLBACK-RUNTIME-OWNERSHIP-001` without closing it.
- Replaced destination cons, partial, and iterator child-copy routes'
  unchecked
  `boundary_copy_to_parent_site_ctx(...)` calls with
  `boundary_copy_to_parent_site_ctx_checked(...)` in:
  - `src/lisp/eval_boundary_commit_escape_cons.c3`
  - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3`
  - `src/lisp/eval_boundary_commit_escape_wrappers.c3`
- Preserved destination error bubbling semantics for child-copy faults: checked
  copy failures now produce an error child that can be promoted by the
  destination builder instead of aborting the active promotion context before
  `boundary_destination_bubble_child_fault(...)` can run.
- Validation:
  - `c3c build --obj-out obj`
  - bounded `memory-lifetime-smoke` -> `pass=235 fail=0`

## 2026-04-21 - Runtime Ownership Fallback Root Checked Copy

- Advanced `FALLBACK-RUNTIME-OWNERSHIP-001` without closing it.
- Replaced the remaining root-store and root-clone unchecked
  `boundary_copy_to_parent_site_ctx(...)` call sites with
  `boundary_copy_to_parent_site_ctx_checked(...)` in:
  - `src/lisp/eval_promotion_root_store.c3`
  - `src/lisp/eval_promotion_root_clone_basic.c3`
  - `src/lisp/eval_promotion_root_clones.c3`
- Preserved the existing fail-closed root clone contract by converting typed
  copy faults to explicit error values at the clone boundary, then running the
  existing partial-copy cleanup paths before returning those errors.
- Current unchecked-wrapper inventory:
  - `rg -n "boundary_copy_to_parent_site_ctx\\(" src/lisp/eval_boundary_*
    src/lisp/eval_promotion_* src/lisp/value_* src/lisp/jit_*` reports only
    the wrapper definition in `src/lisp/eval_boundary_api.c3`.
- Validation:
  - `c3c build --obj-out obj`
  - bounded `memory-lifetime-smoke` -> `pass=235 fail=0`

## 2026-04-21 - Runtime Ownership Fallback Reuse Precheck

- Advanced `FALLBACK-RUNTIME-OWNERSHIP-001` without closing it.
- Reordered `boundary_can_reuse_value(...)` so the target-chain membership
  check runs before alias-safety traversal and is passed into
  `boundary_can_reuse_value_with_target_chain(...)`.
- This preserves the existing reuse rules while avoiding alias graph scans for
  values that are already known to be outside the target scope chain.
- Validation:
  - `c3c build --obj-out obj`
  - bounded `memory-lifetime-smoke` -> `pass=235 fail=0`

## 2026-04-21 - FTXUI C ABI Contract Decisions

- Closed `FTXUI-C-ABI-CONTRACT-001`.
- Updated `docs/plans/ftxui-c-abi-shim.md` from open interface questions to
  locked ABI contract decisions:
  - inbound strings are borrowed only for the ABI call and must be eagerly
    copied by the shim whenever FTXUI state can retain them;
  - `status_name` returns static storage and `context_last_error_message`
    returns context-owned storage valid only until the next context mutation,
    clear, or destroy;
  - the C ABI target remains generic-builder-first, with dedicated entry points
    reserved for lifetime/callback/state contracts that generic builders cannot
    express cleanly;
  - `OMNI_FTXUI_EVENT_CUSTOM` remains payload-free, with richer payloads routed
    through callback `user_data` or the Omni-facing effect/runtime layer.
- Left `FTXUI-C-ABI-WRAPPERS-001` open at this checkpoint for
  implementation/freezing of the remaining wrapper families. This was later
  closed by the `2026-04-21 - FTXUI ABI Wrapper Contract Freeze` entry.
- Validation:
  - targeted `git diff --check`

## 2026-04-21 - REPL Server Concurrent Client Handoff

- Closed `REPL-SERVER-MULTICLIENT-001` for the transport-concurrency boundary
  and split the residual queueing work into `REPL-SERVER-REQUEST-QUEUE-001`.
- Unix socket and TCP REPL listeners now hand each accepted client fd to a
  detached client-handler thread and immediately return to `accept`.
- The accepted-client handler owns stream cleanup after successful handoff.
- Handler-start failure now sends a structured
  `runtime/client-handler-start-failed` protocol error to that accepted client
  and closes the fd instead of serially falling back to listener-thread
  handling.
- Added socketpair-based regression coverage that starts two independent REPL
  client-handler threads and verifies both can answer `describe` with the
  expected transport names.
- `docs/plans/repl-server-protocol-2026-03-30.md` now records transport
  concurrency as shipped and request queueing as the remaining unshipped
  protocol phase.
- Validation:
  - `c3c build --obj-out obj`
  - bounded `async` slice -> `pass=66 fail=0`
  - targeted `git diff --check`
  - `scripts/check_file_size_gate.sh`

## 2026-04-21 - REPL Server Bounded Request Queue

- Closed `REPL-SERVER-REQUEST-QUEUE-001`.
- Replaced the REPL worker's single `pending_command` slot with an 8-entry FIFO
  queue.
- Preserved exactly one active runtime command per connection while allowing
  `clone`, `eval`, `load-file`, `complete`, and `close` to queue behind the
  active request.
- Changed `protocol/server-busy` to mean queue-full backpressure for runtime
  work instead of "any active request exists."
- Preserved interrupt semantics as active-request-only.
- Extended stdin routing so queued `eval` / `load-file` requests can still be
  targeted by request id before execution begins.
- Queued `clone` work now allocates the new session on the worker execution
  path, not the request-reader path, so the session table cannot be reallocated
  while the worker may hold active session pointers.
- Queued commands are now released during connection teardown.
- Added regressions for multiple queued commands, FIFO order, queued stdin
  targeting, active-only interrupt behavior, queue-full backpressure, and
  deferred clone session allocation.
- Validation:
  - `c3c build --obj-out obj`
  - bounded `async` slice -> `pass=69 fail=0`

## 2026-04-21 - FTXUI C ABI Focused Wrapper Coverage

- Advanced `FTXUI-C-ABI-WRAPPERS-001` as a focused coverage checkpoint. The
  remaining advanced wrapper-family backlog was later closed by the
  `2026-04-21 - FTXUI ABI Wrapper Contract Freeze` entry.
- Added `omni_ftxui_component_handle_event(...)` to the C ABI and C3 extern
  mirror so runtime code and tests can dispatch an event to a component without
  starting an interactive screen loop.
- Added the previously documented-but-missing
  `omni_ftxui_component_wrap_action(...)` action callback wrapper.
- Tightened `component_handle_event` so callback-side context failures are
  returned instead of being cleared after `OnEvent`.
- Corrected the C3 ABI mirror for FTXUI callback fields: C3 function aliases
  already model function-pointer slots, so callback struct fields now use the
  alias directly instead of pointer-to-function-pointer fields.
- Added focused advanced FFI surface regressions for:
  - context creation, last-error capture/clear, screen create/control/post
    event/exit;
  - table selection and render-to-element conversion;
  - canvas draw/render conversion;
  - event-handler callback delivery through `component_handle_event`;
  - action callback delivery through `component_wrap_action` and Return-key
    dispatch.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - bounded `advanced-ffi-system-surface` -> `pass=102 fail=0`
  - `scripts/run_ftxui_smoke.sh`

## 2026-04-21 - Runtime Ownership Zero-Hint Scope Cache

- Advanced `FALLBACK-RUNTIME-OWNERSHIP-001` without reopening the invalidated
  global scope-chain scan bypass.
- Removed the early cache-bypass branch for zero-generation hinted
  `boundary_ptr_in_target_scope_chain_with_hint(...)` lookups.
- Zero-generation hinted lookups now use the same active promotion-context
  route cache as generation-pinned lookups, keyed by pointer, target scope,
  target generations, and pinned generation `0`.
- Added a smoke regression proving repeated zero-generation target-chain
  lookups scan once within one active promotion context.
- Removed dead non-hinted scope-chain helper surfaces
  `boundary_ptr_in_target_scope_chain(...)`,
  `boundary_ptr_in_scope_chain_scan(...)`, and
  `boundary_ptr_in_scope_chain(...)`; all live callers now use the
  hint/cache-aware route.
- Validation:
  - `c3c build --obj-out obj`
  - bounded `memory-lifetime-smoke` -> `pass=236 fail=0`
  - counters build `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`
  - counters-enabled bounded `memory-lifetime-smoke` -> `pass=236 fail=0`
  - normal rebuild `c3c build --obj-out obj`

## 2026-04-21 - UI Facade Dotted Module Closure

- Closed `UI-LIB-FACADE-001` for the facade/module split and shipped
  public-surface coverage.
- Rewired `examples/libraries/ftxui/ui.omni` so the canonical public facade now
  loads `examples/libraries/ftxui/lib/ui/{nodes,effects,layout,style,runtime,ftxui}.omni`
  as the implementation source instead of relying on flat compatibility helper
  modules.
- Fixed sibling imports for `lib/ui/layout.omni`, `lib/ui/style.omni`,
  `lib/ui/runtime.omni`, and `lib/ui/evaluate.omni` so direct default-path
  dotted imports such as `(import ui.nodes)` and `(import ui.runtime)` load
  cleanly from the FTXUI example subtree.
- Added `examples/libraries/ftxui/module_direct_smoke.omni` and wired it into
  `scripts/run_ftxui_smoke.sh`.
- Removed the stale static evaluator copy from `ui.runtime`; static tree
  evaluation remains isolated in `lib/ui/evaluate.omni`.
- Split the remaining runtime execution semantics into
  `UI-LIB-RUNTIME-BACKEND-001`: at this checkpoint, `ui.runtime.dispatch`
  still lowered declarative effect trees to Omni `signal` effects for
  handler/test capture, and a backend-owned lifecycle interpreter still needed
  to consume those events through `ui.ftxui`. This was later closed by the
  `2026-04-21 - UI Runtime FTXUI Backend Dispatch` entry below; the remaining
  interactive read/update/render loop is tracked separately.
- Validation:
  - `c3c build --obj-out obj`
  - direct loads of `examples/libraries/ftxui/lib/ui/runtime.omni` and
    `examples/libraries/ftxui/lib/ui/evaluate.omni`
  - `scripts/run_ftxui_smoke.sh`

## 2026-04-21 - FTXUI ABI Wrapper Contract Freeze

- Closed `FTXUI-C-ABI-WRAPPERS-001` for the chosen generic-builder-first ABI
  contract instead of leaving upstream parity gaps as ambiguous TODOs.
- Explicitly froze the remaining advanced upstream wrapper families as
  non-goals until a concrete Omni-facing surface needs them: arbitrary
  selection pixel callbacks, gradient decorators, animated/richer widget
  options, active-screen access, captured-mouse exposure, `WithRestoredIO`,
  selection API callbacks, and one-function-per-upstream-overload parity.
- Added focused fail-closed ABI regression coverage for the unsupported
  piped-input runtime-helper path: `screen_create(... handle_piped_input=true
  ...)` and `screen_set_handle_piped_input(... true ...)` must return
  `OMNI_FTXUI_STATUS_NOT_SUPPORTED` instead of silently ignoring the request.
- Remaining UI runtime work now lives under
  `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001`; raw upstream ABI parity is no longer
  the default next step.
- Validation:
  - `c3c build --obj-out obj`
  - bounded `advanced-ffi-system-surface` -> `pass=105 fail=0`
  - `scripts/run_ftxui_smoke.sh`
  - targeted `git diff --check`
  - `scripts/check_file_size_gate.sh`

## 2026-04-21 - Runtime Ownership Unchecked Copy Wrapper Deletion

- Advanced `FALLBACK-RUNTIME-OWNERSHIP-001` with another route-specific
  cleanup, without reopening the invalidated global scope-chain scan deletion.
- Removed the dead unchecked `boundary_copy_to_parent_site_ctx(...)` wrapper
  from `src/lisp/eval_boundary_api.c3`; test-only callers now use
  `boundary_copy_to_parent_site_ctx_checked(...)` and handle
  `BoundaryCopyResult` explicitly.
- Removed the dead unchecked `copy_to_parent_site(...)` helper from
  `src/lisp/eval_promotion_copy.c3`; the live site-accounting path is
  `copy_to_parent_site_checked(...)`.
- Removed the plain unchecked `copy_to_parent(...)` convenience after its
  remaining test-only callers moved to `test_copy_to_parent(...)` or direct
  `copy_to_parent_with_fault(...)` assertions.
- Live inventory now reports no `copy_to_parent(...)`,
  `boundary_copy_to_parent_site_ctx`, or `copy_to_parent_site` symbols under
  `src/lisp`.
- Validation:
  - `c3c build --obj-out obj`
  - bounded `memory-lifetime-smoke` -> `pass=236 fail=0`
  - targeted `git diff --check`
  - `scripts/check_file_size_gate.sh`

## 2026-04-21 - UI Runtime FTXUI Backend Dispatch

- Closed `UI-LIB-RUNTIME-BACKEND-001` for the non-interactive backend lifecycle
  contract.
- Added hidden primitive `__ui-ftxui-dispatch`, which requires a root
  `open_tree` with backend `ftxui`, creates a real FTXUI context/screen, lowers
  `render_tree` node payloads to FTXUI components, maps `invalidate_tree` to
  `screen_request_animation_frame`, maps `post_event_tree` to C ABI
  event-create/post, and maps `close_tree` to screen exit.
- Added `ui.runtime.dispatch_to` and `ui.ftxui.dispatch` so backend execution
  is reached through the runtime handoff rather than a facade-only shortcut.
- Added `examples/libraries/ftxui/module_backend_smoke.omni` and wired it into
  `scripts/run_ftxui_smoke.sh`.
- The backend smoke covers the happy path and fail-closed behavior for
  unsupported `read_event_tree` and render-less open trees.
- Split remaining interactive/read-loop behavior into
  `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001`.
- Validation:
  - `c3c build --obj-out obj`
  - direct `module_backend_smoke.omni`
  - `scripts/run_ftxui_smoke.sh`
  - bounded `advanced-ffi-system-surface` -> `pass=102 fail=0`
  - targeted `git diff --check`
  - `scripts/check_file_size_gate.sh`

## 2026-04-22 - ML SGD Step Device Kernel Reuse

- Advanced `ML-VK-060` by closing `ML-VK-060-014`: `ml/sgd-step(parameters
  gradients learning-rate)` now reuses the existing stateless CUDA/Vulkan dense
  row-major `Float32` SGD optimizer kernels for all-device parameter and
  gradient leaves.
- Mixed CPU/device `ml/sgd-step` leaves remain fail-closed with
  `tensor/backend-unsupported`; the primitive does not introduce hidden
  transfers.
- Added focused regressions proving CUDA/Vulkan `ml/sgd-step` preserves device
  placement when `ml-optimizer-sgd-float32` is available, and renamed the mixed
  Vulkan negative-path test to match the new contract.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1897 fail=0`
  - compiler slice -> `pass=290 fail=0`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`

## 2026-04-22 - ML Gradient Finite-Difference Validation

- Advanced `ML-VK-090` by closing `ML-VK-090-001`: the shipped `ml/grad`
  contracts now have CPU finite-difference regressions for representative
  differentiable losses.
- Added central finite-difference checks for `linear-mean-squared-error` input
  gradients and `linear-softmax-cross-entropy` weight gradients, using the
  public `ml/linear`, `ml/mean-squared-error`, and `ml/cross-entropy`
  primitives as the numeric oracle.
- At this checkpoint, remaining `ML-VK-090` work was Vulkan no-hidden-fallback
  visibility, bounded-container ML slices, and benchmark fixtures. Later
  `ML-VK-090-002` and `ML-VK-090-003` closed those fallback and bounded
  benchmark slices, leaving broader availability-path coverage.
- Tightened stale fallback diagnostics in `ml/clip-gradients` and
  `ml/optimizer-step`: the remaining Vulkan fallback errors now describe mixed
  or unsupported Vulkan contracts instead of claiming Vulkan kernels are
  globally unimplemented.
- Closed `ML-VK-090-002` with no-hidden-CPU-fallback diagnostic probes for
  mixed Vulkan ML trees. The focused tests require mixed Vulkan/CPU
  `ml/clip-gradients` and `ml/optimizer-step` failures to expose the all-Vulkan
  placement contract in their error messages.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1900 fail=0`

## 2026-04-22 - ML Validation Bench Fixture

- Closed `ML-VK-090-003` with the first bounded ML validation entrypoint and
  benchmark fixture.
- Added opt-in `OMNI_ML_BENCH=1` summaries in the focused advanced collections
  ML/Tensor tests:
  - `ml_inference_oracle` measures repeated CPU `ml/linear` inference;
  - `ml_training_step_oracle` measures repeated CPU `ml/optimizer-step`
    training-step updates and reports scope recycle plus copy-to-parent reuse
    deltas.
- Added `scripts/run_ml_validation_slice.sh`, which routes the focused
  advanced collections group through `scripts/run_validation_container.sh` and
  asserts the benchmark summary iteration counts.
- The local validation image cannot execute its bundled `/usr/local/bin/c3c`
  on this host (`Exec format error`), so `scripts/run_ml_validation_slice.sh`
  now builds before entering the container and runs only the focused test/bench
  workload inside bounded validation.
- Validation:
  - `c3c build --obj-out obj`
  - local focused `advanced-collections-module` with `OMNI_ML_BENCH=1` ->
    `pass=1900 fail=0`, `inference_ok=128`, `train_ok=64`
  - bounded `scripts/run_ml_validation_slice.sh` -> `pass=1883 fail=0`,
    `inference_ok=128`, `train_ok=64`

## 2026-04-22 - ML Vulkan Availability Gate Regression

- Closed `ML-VK-090` for the current Vulkan ML validation gate by expanding
  `ML-VK-090-004` into an explicit ML Vulkan operation-family
  availability-gate regression.
- Added an advanced collections test that exercises both host-path contracts:
  Vulkan-visible hosts must keep Float32 ML capability bits tied to actual
  Float32 placement across linear, activation, reduction, normalization,
  attention, convolution/pooling, optimizer, and gradient clipping families,
  and run a device-resident `ml/linear` smoke; Vulkan-unavailable hosts must
  report false ML Vulkan capability bits and fail closed with
  `tensor/backend-unavailable` for `to-device 'vulkan`.
- Future broader validation expansion should be tracked as a new item for the
  specific operation family or benchmark dimension it adds, not as a residual
  under `ML-VK-090`.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1901 fail=0`
  - bounded `scripts/run_ml_validation_slice.sh` -> `pass=1884 fail=0`,
    `inference_ok=128`, `train_ok=64`
  - `scripts/check_file_size_gate.sh`
  - targeted `git diff --check`

## 2026-04-22 - Runtime Ownership Env Barrier Checked Copy

- Advanced `FALLBACK-RUNTIME-OWNERSHIP-001` with another route-specific copy
  fallback cleanup.
- `promote_for_env_target(...)` no longer calls
  `copy_to_parent_by_route(...)` directly for detached environment writes.
  It now uses `copy_to_parent_with_fault(...)` and converts typed copy faults
  to explicit error values at the env write boundary.
- Live inventory now reports `copy_to_parent_by_route(...)` only inside
  `src/lisp/eval_promotion_copy.c3`: the implementation plus its two internal
  checked-copy call sites.
- Validation:
  - `c3c build --obj-out obj`
  - bounded `memory-lifetime-smoke` -> `pass=236 fail=0`

## 2026-04-22 - Runtime Ownership Explicit Target Scope Cache

- Advanced `FALLBACK-RUNTIME-OWNERSHIP-001` with a route-specific
  scope-chain-cache cleanup.
- Added `boundary_ptr_in_scope_chain_with_hint_cached(...)` for callers that
  need an explicit target scope instead of `interp.current_scope`.
- Routed `boundary_classify_return_value(...)` and destination partial child
  reuse checks through the explicit-target cache, preserving existing
  scope-chain semantics while avoiding repeated raw scans in one active
  promotion context.
- Added a memory-lifetime smoke regression proving repeated explicit-target
  zero-generation lookups scan once under an active promotion context.
- Validation:
  - `c3c build --obj-out obj`
  - bounded `memory-lifetime-smoke` -> `pass=237 fail=0`
  - `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`
  - counters-enabled bounded `memory-lifetime-smoke` -> `pass=237 fail=0`
  - normal rebuild `c3c build --obj-out obj`
  - targeted `git diff --check`
  - `scripts/check_file_size_gate.sh`

## 2026-04-22 - Runtime Ownership Fallback Closure Guard

- Closed `FALLBACK-RUNTIME-OWNERSHIP-001` for the staged runtime ownership
  fallback cleanup boundary.
- Extended `scripts/check_boundary_facade_usage.sh` so production
  `copy_to_parent_by_route(...)`, `boundary_copy_to_parent_site_ctx(...)`, and
  uncached `boundary_ptr_in_scope_chain_with_hint(...)` call sites are blocked
  outside sanctioned boundary implementation files.
- Routed tensor expression-edge escape promotion through
  `boundary_promote_to_escape(...)` instead of direct `promote_to_escape(...)`;
  this was the remaining production facade bypass exposed by the strengthened
  guard.
- Updated `docs/plans/fallback-inventory.md` to mark the runtime ownership
  fallback family done. The invalidated global scope-chain scan deletion path
  remains explicitly rejected and must not be treated as the closure criterion.
- Validation:
  - `bash scripts/check_boundary_facade_usage.sh`
  - `c3c build --obj-out obj`
  - bounded `memory-lifetime-smoke` -> `pass=237 fail=0`
  - targeted `git diff --check`
  - `scripts/check_file_size_gate.sh`

## 2026-04-22 - ML-VK-020 Leaky ReLU Closure

- Closed `ML-VK-020` for the current neural elementwise, reduction, softmax,
  and loss kernel contract.
- Added canonical `ml/leaky-relu(input [negative-slope])` with default slope
  `0.01`, non-negative finite slope validation, Float64/Float32 Tensor support,
  and CPU/CUDA/Vulkan placement preservation through composed Tensor map
  kernels.
- Added explicit `tensor-backends` narrow capability bits
  `ml-neural-leaky-relu-float64` and `ml-neural-leaky-relu-float32`, plus
  interpreter/AOT registration, spec/reference docs, and CPU/Vulkan regression
  coverage.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1908 fail=0`
  - bounded `scripts/run_ml_validation_slice.sh` -> `pass=1891 fail=0`,
    `inference_ok=128`, `train_ok=64`
  - compiler slice -> `pass=290 fail=0`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - targeted `git diff --check`

## 2026-04-22 - ML Backlog Reshape And Leaky ReLU Backward

- Closed `ML-VK-050-004` by adding CPU `linear-activation-mean-squared-error`
  `leaky-relu` backward support to `ml/grad` for `Float64` and `Float32`.
- `ml/grad` direct specs now accept optional `negative-slope` for
  `leaky-relu`, default to `0.01`, reject negative/non-finite slopes, and
  require Float32-representable slopes for Float32 gradients.
- Added default-slope `nn/leaky-relu` DataSpec support: constructor,
  validation, `nn/apply`/`nn/predict` lowering through `ml/leaky-relu`, and
  AOT/interpreter primitive registration.
- Reshaped the Vulkan ML backlog so shipped umbrella parents are closed:
  `ML-VK-001`, `ML-VK-040`, `ML-VK-060`, `ML-VK-070`, and `ML-VK-080`.
  Remaining real work is explicit under `ML-VK-040-TRAIN-BN-001`,
  `ML-VK-040-FUSED-ATTENTION-001`, `ML-VK-050-006`,
  `ML-VK-050-007`, and `ML-VK-060-FUSED-CUDA-001`.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1911 fail=0`

## 2026-04-22 - ML-VK-050 Gradient Tape Metadata Contract

- Closed `ML-VK-050-005` by adding a reusable metadata-only gradient tape
  representation to shipped `ml/grad` result dictionaries.
- Each current `ml/grad` result now includes `tape`, an ordinary
  scope-region-owned dictionary with `kind 'gradient-tape`, `version 1`,
  `policy 'metadata-only`, `owner 'scope-region`, false `retains-handles`,
  CPU dtype/source metadata, optional activation metadata, `node-count`, and
  an ordinary `nodes` array.
- The tape contract intentionally carries no native handles, executor state, or
  hidden lifetime authority. Generic runtime accumulation remains the next
  semantic boundary under `ML-VK-050-006`.
- Updated the Vulkan ML roadmap, TODO part 14, language/reference docs, active
  plan, and session report to close `ML-VK-050-005` and keep remaining autograd
  work explicit.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1912 fail=0`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`

## 2026-04-22 - ML-VK-050 CPU Tensor Expression Gradients

- Closed `ML-VK-050-006` by adding CPU dense row-major Tensor-expression
  reverse accumulation to `ml/grad`.
- Added `tensor-mean-squared-error` and `tensor-softmax-cross-entropy` gradient
  specs. They differentiate supported `Float64`/`Float32` expression graphs
  through `map` and `contract` nodes, compute MSE and softmax-cross-entropy
  upstream gradients, return the existing metadata-only `gradient-tape`
  contract, and keep CUDA/Vulkan backward fail-closed.
- Added single-leaf `wrt` disambiguation so copied dictionary values can still
  match the intended concrete Tensor leaf while duplicate equal leaves fail
  closed instead of accumulating against an ambiguous target.
- Updated AOT runtime source manifest, language/reference docs, roadmap, TODO,
  active plan, and session report.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1916 fail=0`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`

## 2026-04-22 - ML-VK-050 Explicit Device Backward Semantics

- Closed `ML-VK-050-007` by making Tensor-expression `ml/grad` reject
  non-CPU operands before `wrt` leaf matching.
- This fixes the device fail-closed ordering for copied dictionary specs:
  Vulkan forward tensors without matching backward kernels now report
  `tensor/backend-unsupported` instead of falling through to
  ambiguous/unreachable `wrt` diagnostics.
- Added focused coverage for `tensor-mean-squared-error` and
  `tensor-softmax-cross-entropy` over Vulkan `Float32` forward tensors, and
  asserted broad `ml-autograd` remains false across CPU/CUDA/cuBLAS/Vulkan
  until a complete backend autograd family exists.
- Closed the broad `ML-VK-050` parent for the current CPU reverse-mode plus
  explicit device fail-closed contract. Split future native Vulkan backward
  work into `ML-VK-050-VK-BWD-001`.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1919 fail=0`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_e2e_baseline_policy.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`

## 2026-04-22 - UI FTXUI Blocking Effect Loop

- Closed `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001` for the one-shot blocking
  effect-tree loop contract.
- Added hidden primitive `__ui-ftxui-loop`, public `ui.ftxui.loop`, public
  `ui.loop`, and `ui.runtime.loop_to`; kept the flat compatibility
  `ui_ftxui.omni` / `ui_runtime.omni` modules aligned with the canonical
  `lib/ui/*` modules.
- The loop path consumes a root `open_tree` with backend `ftxui`, requires
  exactly one `render_tree`, lowers it through the existing FTXUI app wrapper,
  applies `invalidate_tree`, `post_event_tree`, and explicit pre-loop
  `close_tree`, then enters the blocking app loop. `read_event_tree` remains
  fail-closed for this one-shot helper.
- Added `module_interactive_loop_smoke.omni` and wired it into
  `scripts/run_ftxui_smoke.sh`.
- Split the remaining session-owned external read/update/render work to
  `UI-LIB-RUNTIME-SESSION-001`.
- Validation:
  - `c3c build --obj-out obj`
  - direct piped `module_interactive_loop_smoke.omni`
  - `OMNI_FTXUI_RUNTIME_LD_LIBRARY_PATH=build:/usr/local/lib timeout 60s scripts/run_ftxui_smoke.sh`
  - focused `advanced-ffi-system-surface` -> `pass=109 fail=0`

## 2026-04-22 - UI FTXUI Session Runtime

- Closed `UI-LIB-RUNTIME-SESSION-001` for the session-owned lifecycle surface.
- Added a native `omni_ftxui_session_t` wrapper around FTXUI `Loop` with
  explicit create/destroy, non-blocking `run_once`, blocking `run_once_blocking`,
  and `has_quitted` ABI functions. The wrapper balances FTXUI `PreMain` /
  `PostMain` through session lifetime instead of reusing the one-shot blocking
  app helper.
- Added owned Omni `ui-ftxui-session` `ForeignHandle` support with a single
  finalizer authority. The finalizer destroys the native session loop before
  the retained component list, heap state, screen, and context.
- Exposed public session helpers through the canonical dotted modules, flat
  compatibility modules, and `ui` facade:
  `open_session`, `update_session`, `render_session`, `read_event_session`,
  `invalidate_session`, `post_event_session`, and `close_session`.
- Added `module_session_smoke.omni` and expanded the module/value/direct FTXUI
  smokes to assert the new public surface. At the lifecycle-slice boundary,
  `read_event_session` was kept fail-closed and the payload contract was split
  to the follow-up item closed below.
- Split raw event payload reads into
  `UI-LIB-RUNTIME-SESSION-EVENT-PAYLOAD-001` instead of leaving the shipped
  session lifecycle item open.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - direct `module_session_smoke.omni`
  - `OMNI_FTXUI_RUNTIME_LD_LIBRARY_PATH=build:/usr/local/lib scripts/run_ftxui_smoke.sh`
  - `scripts/check_build_config_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`

## 2026-04-22 - TENSOR-100H CUDA cuSOLVER Complex SVD Loader

- Closed `TENSOR-100H-CUDA-SVD-NORMS-LOADER` as the loader/probe boundary for
  CUDA fixed-width complex SVD/norm work.
- Added runtime `libcusolver` discovery in `csrc/tensor_cuda_helpers.c` for
  `cusolverDnZgesvd`, `cusolverDnCgesvd`, their buffer-size probes, and
  `cusolverDnCreate`/`cusolverDnDestroy`. cuSOLVER remains dynamically loaded;
  it is not a link-time dependency.
- Added public helper probes and test controls:
  `omni_tensor_backend_cusolver_available`,
  `omni_tensor_backend_cusolver_complex_svd_available`,
  `omni_tensor_backend_cusolver_disable_for_tests`,
  `omni_tensor_backend_cusolver_zgesvd_call_count`, and
  `omni_tensor_backend_cusolver_cgesvd_call_count`, with C3 externs in
  `src/lisp/tensor_cuda_backend.c3`.
- `tensor-backends` now exposes CUDA `cusolver` and
  `cusolver-complex-svd` loader capabilities. Operation-specific
  `matrix-singular-values-complex128`, `matrix-singular-values-complex64`,
  `matrix-svd-complex128`, and `matrix-svd-complex64` remain false until the
  CUDA layout adapters and public execution routes land.
- Split the remaining CUDA lane into adapter and execution sub-boundaries in
  `docs/todo_parts/todo_part_01.md`; next work is
  `TENSOR-100H-CUDA-SVD-NORMS-ADAPTERS`.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1943 fail=0`

## 2026-04-22 - TENSOR-100H CPU Float64 Fixed-Width Eigenpairs

- Closed `TENSOR-100H-COMPLEX-EIGEN-F64-EIGENPAIRS` for the existing CPU
  `Float64` `matrix/eigenpairs` route.
- `matrix/eigenpairs` for CPU `Float64` tensors now returns fixed-width
  `Complex128` `values` and `vectors` tensors, replacing the previous
  `BigComplex` result dtype for this fixed-width contract slice. The public
  function name and existing Float64 input acceptance are unchanged.
- Added shared Complex128 eigenpair result builders for LAPACK raw-vector
  output and pure fallback vector output. Both paths keep deterministic
  eigenvalue ordering and phase normalization while storing fixed-width
  `Complex128Val` slots instead of heap-allocated BigComplex handles.
- Updated residual tests to exercise fixed-width Complex128 arithmetic directly
  instead of preserving the old BigComplex residual harness.
- Local blocker recorded for the adjacent CUDA SVD adapter lane: `nvcc` and
  `ptxas` were not available on `PATH`, so checked-in CUDA adapter PTX could
  not be generated or validated in this host session.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1943 fail=0`

## 2026-04-22 - UI FTXUI Session Event Payload Reads

- Closed `UI-LIB-RUNTIME-SESSION-EVENT-PAYLOAD-001` for session-owned raw event
  payload reads.
- Added `omni_ftxui_event_read_result` and
  `omni_ftxui_session_take_event` to the FTXUI shim ABI. Session roots are now
  wrapped with `ftxui::CatchEvent`, which records the last event observed
  during a loop step without consuming downstream handling.
- `ui.read_event_session` now runs one blocking loop step, drains the captured
  payload from the native session, and returns `nil` or an Omni dictionary with
  `kind` and optional `text`. Character events, known special keys,
  payload-free custom events, and fallback special-key input text are covered.
- Updated `module_session_smoke.omni` to post a character event through the
  session handle and assert that the read path returns
  `{'kind 'character 'text "x"}`.
- Preserved the one-shot dispatch/loop contract: `read_event_tree` remains
  fail-closed outside explicit session-owned state.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - direct `module_session_smoke.omni`
  - `OMNI_FTXUI_RUNTIME_LD_LIBRARY_PATH=build:/usr/local/lib scripts/run_ftxui_smoke.sh`
  - `scripts/check_build_config_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`

## 2026-04-22 - ML-VK-050 First Native Vulkan MSE Backward Kernel

- Closed `ML-VK-050-VK-BWD-001` for the first native Vulkan backward slice.
- `ml/grad` now supports `tensor-mean-squared-error` for dense row-major
  `Float32` Vulkan tensors when `wrt` is the concrete prediction tensor.
  The forward loss stays on Vulkan and the input-gradient is computed on
  Vulkan as `(prediction - targets) * (2 / element-count)` through the existing
  direct Vulkan map kernels.
- The returned metadata-only tape now records `device 'vulkan` for this path.
  Broad backend `ml-autograd` remains false because only this narrow backward
  kernel ships.
- Preserved fail-closed semantics for unsupported Vulkan graph/map-expression
  backward. Normal Vulkan `map` eagerly materializes concrete device tensors and
  drops expression edges, so recursive Vulkan map-expression backward is split
  to `ML-VK-050-VK-MAP-BWD-001` rather than being hidden behind a global lazy
  `map` behavior change.
- Kept the improved FFI callback bridge ABI intact by aligning
  `csrc/ffi_helpers.c` with `omni_ffi_callback_dispatch` instead of restoring
  the older `omni_ffi_closure_trampoline` path.
- Validation:
  - `c3c build --obj-out obj`
  - direct Vulkan `ml/grad` `--eval` probe returning loss `5.0` and gradient
    `[1.0, 3.0]` on Vulkan
  - focused `advanced-collections-module` -> `pass=1924 fail=0`
  - focused `advanced-ffi-system-surface` -> `pass=115 fail=0`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`

## 2026-04-22 - ML-VK-050 Vulkan Map Backward Provenance

- Closed `ML-VK-050-VK-MAP-BWD-001` by preserving provenance for eager Vulkan
  `map` results and consuming it in the native Vulkan MSE backward path.
- Eager Vulkan `map` semantics are unchanged: direct Vulkan `map` still returns
  concrete device tensors. The concrete result now records map function,
  tensor-child, and scalar provenance when the language-facing `map` primitive
  produced it. Internal Vulkan map uses such as `ml/linear` bias maps and
  backward helper maps do not record provenance.
- Concrete tensors carrying map provenance now participate in boundary
  copy/promote/cleanup through the existing scope/region expression-edge
  helpers. This preserves the region-centric memory model and avoids adding
  per-Tensor ownership.
- `ml/grad` `tensor-mean-squared-error` on dense row-major `Float32` Vulkan
  tensors now backpropagates through same-shape map provenance for `map +`,
  `map -`, and `map *`. The shipped tests cover direct prediction gradients,
  `map +`, `map *` scalar, `map *` tensor, and fail-closed unsupported `max`.
- At that milestone, broadcast-gradient reductions were split to
  `ML-VK-050-VK-MAP-BCAST-BWD-001`; that residual is closed by the later
  broadcast-reduction entry below.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1927 fail=0`

## 2026-04-22 - ML-VK-050 Native Vulkan Softmax-CE Backward

- Closed `ML-VK-050-VK-SOFTMAX-CE-BWD-001` for native Vulkan
  `tensor-softmax-cross-entropy` backward.
- `ml/grad` now supports dense row-major `Float32` Vulkan logits/targets when
  `wrt` is the concrete logits tensor or a same-shape map-provenance leaf.
  Loss, softmax output, and input-gradient stay on Vulkan.
- The upstream gradient is computed as
  `(softmax(logits) - targets) / slice-count` through existing native Vulkan
  softmax, cross-entropy, and direct map kernels. This reuses the same
  provenance traversal introduced for Vulkan MSE map backward.
- Broad backend `ml-autograd` remains false. Unsupported dtypes, mixed
  placement, unsupported expression graphs, and broadcast map reductions still
  fail closed.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1928 fail=0`

## 2026-04-22 - ML-VK-050 Vulkan Broadcast Map Backward Reductions

- Closed `ML-VK-050-VK-MAP-BCAST-BWD-001` for broadcast-compatible Vulkan map
  provenance backward.
- `ml/grad` now reduces dense row-major `Float32` Vulkan upstream gradients
  back to broadcasted wrt shapes through native Vulkan `ml/sum` reduction when
  traversing `map +`, `map -`, and `map *` provenance. This keeps gradients on
  device and does not introduce a CPU fallback.
- The new helper preserves scope-owned tensor metadata by retaining the reduced
  Vulkan device handle when a rank/shape alias is needed, rather than adding a
  separate tensor ownership model.
- Focused tests cover leading-axis, inner singleton-axis, and rank-0 broadcast
  wrt reductions for Vulkan MSE map backward. Unsupported operators, duplicate
  wrt leaves, non-broadcast-compatible shapes, unsupported dtypes, and mixed
  placement still fail closed. Broad backend `ml-autograd` remains false.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1931 fail=0`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`

## 2026-04-22 - ML-VK-040 Fused Vulkan Attention Closure

- Closed `ML-VK-040-FUSED-ATTENTION-001` as a stale residual against the
  current implementation.
- `ml/scaled-dot-product-attention` already uses a single direct dense Vulkan
  `Float32` attention shader for the supported inference/eval contract, rather
  than an unfused composition of public softmax and matmul primitives.
- Added focused Vulkan tests for additive `[Q K]` masks and batched rank-3
  masks to harden the direct fused shader path against the same mask families
  accepted by the primitive contract.
- Preserved fail-closed behavior for mixed CPU/Vulkan operands and Vulkan
  `Float64`. Dropout/training attention remains outside the shipped
  `ml/scaled-dot-product-attention` surface rather than an open `ML-VK-040`
  residual.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1933 fail=0`

## 2026-04-22 - ML-VK-060 Fused CUDA SGD Optimizer Kernel

- Closed `ML-VK-060-FUSED-CUDA-001` for the first native fused CUDA optimizer
  kernel beyond map-backed execution.
- Dense row-major `Float32` CUDA `ml/optimizer-step` SGD now attempts a native
  fused helper before the previous map-backed CUDA route. The helper computes
  weight decay, optional momentum velocity initialization/continuation, updated
  parameters, and updated velocity state in one CUDA kernel.
- Added `csrc/tensor_cuda_helpers_ml_optimizer.inc` and
  `csrc/tensor_cuda_ml_optimizer_ptx.inc`, wired them into
  `csrc/tensor_cuda_helpers.c`, exposed the helper through
  `src/lisp/tensor_cuda_backend.c3`, and routed
  `src/lisp/prim_ml_optimizer_cuda.c3` SGD leaves through it when the native
  optimizer module is available.
- Preserved fail-closed semantics: if the fused CUDA SGD module resolves but a
  kernel/allocation path fails, `ml/optimizer-step` reports the CUDA backend
  error instead of silently hiding the failure behind the map-backed route. The
  map-backed route remains the fallback only when the native optimizer module is
  unavailable.
- Split the still-open stateful optimizer work to
  `ML-VK-060-FUSED-CUDA-STATEFUL-001`: native fused CUDA Adam, AdamW, and
  RMSProp kernels remain map-backed until multi-output state kernels and
  CUDA-host oracle coverage are added.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1933 fail=0`

## 2026-04-22 - TENSOR-100F Stale Matrix Split Residual Closure

- Closed the stale `TENSOR-100F` subitem that still requested largest-first
  source splitting from `src/lisp/prim_tensor_matrix.c3`.
- Current measured code-file counts show the matrix primitive family has
  already been split below the owner gate: `src/lisp/prim_tensor_matrix.c3` is
  173 LOC, `src/lisp/prim_tensor_matrix_lu_tensor_ops.c3` is 665 LOC, and
  `csrc/tensor_vulkan_helpers.c` is 265 LOC.
- No runtime semantics changed. The remaining open `TENSOR-100F` items are
  Vulkan runtime/performance work: large-`k` SVD/Singular-values measurement,
  large-`n` symmetric eigen measurement, and a future direct Vulkan general
  `matrix/eigenpairs` contract/implementation.
- Validation:
  - repository code-file count scan
  - `scripts/check_file_size_gate.sh`

## 2026-04-22 - Validation Docker Native Arm64 C3 Toolchain

- Closed the `TENSOR-100F` validation Docker image arm64 compiler architecture
  item.
- `docker/validation.Dockerfile` now selects the C3 install path by build
  architecture: `amd64`/`x86_64` still uses the checked upstream Linux release
  tarball, while `arm64`/`aarch64` builds C3 from the checked source tarball
  because upstream C3 does not publish a Linux arm64 binary tarball.
- Updated the validation image C3 pin to `v0.7.11`, matching the repo's active
  host compiler contract, and installed LLVM/LLD/Polly 19 for the arm64 source
  build. A prior `v0.7.10` source-build image fixed the architecture error but
  failed to build the repo because the codebase now uses C3 0.7.11-facing
  syntax/APIs.
- Negative constraint: do not use Android aarch64 C3 release artifacts in the
  Ubuntu validation image; they are not a native Linux toolchain.
- Validation:
  - `scripts/build_validation_image.sh`
  - `docker run --rm --entrypoint /bin/sh omni-validation:2026-03-10 -lc 'uname -m; file /opt/c3/c3c; c3c -V'`
  - `scripts/run_validation_container.sh c3c build --obj-out obj`

## 2026-04-22 - TENSOR-100H CPU Fixed-Width Complex SVD Factors

- Closed `TENSOR-100H-SVD-FACTORS-CPU` for CPU fixed-width complex
  `matrix/svd` factor output.
- `matrix/svd` now accepts CPU `Complex128` and `Complex64` tensors. The result
  dictionary keeps `u`, `s`, and `v`; `u`/`v` preserve the fixed-width complex
  dtype, while `s` uses the component real dtype (`Float64` for Complex128,
  `Float32` for Complex64).
- Added a native complex CPU factor path in
  `src/lisp/prim_tensor_matrix_lu_svd_core_b.c3`: Hermitian Gram construction,
  deterministic power/eigenvector extraction, Hermitian orthonormal completion,
  phase normalization, and Complex64 widening/narrowing around the Complex128
  oracle. Public complex `u`/`v` factors do not use realified SVD factor
  extraction.
- Updated `src/lisp/prim_tensor_matrix_svd_primitives.c3` routing and result
  allocation, and replaced the old pending-contract rejection tests with CPU
  Complex128/Complex64 dtype, reconstruction, wide-shape, zero/rank-deficient,
  empty-axis, and no-Float64-LAPACK coverage in
  `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part7.c3`.
- Split the remaining GPU work into `TENSOR-100H-SVD-FACTORS-VULKAN`; Vulkan
  fixed-width complex `matrix/svd` factor output still needs native shaders and
  helper ABI wiring. Copying Vulkan inputs to CPU/LAPACK remains prohibited.
- Validation:
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1937 fail=0`

## 2026-04-22 - TENSOR-100H Vulkan Fixed-Width Complex SVD Factors

- Closed `TENSOR-100H-SVD-FACTORS-VULKAN` for Vulkan fixed-width complex
  `matrix/svd` factor output.
- Added native Vulkan Complex128 and Complex64 SVD factor shaders:
  `csrc/tensor_vulkan_svd_complex128.comp` and
  `csrc/tensor_vulkan_svd_complex64.comp`, plus generated SPIR-V C/INC blobs.
  The shaders build a Hermitian Gram matrix on device, compute deterministic
  complex eigenvectors with deflation, complete zero/rank-deficient columns,
  phase-normalize paired factors, and return component-real singular values.
- Added C helper ABI entries
  `omni_tensor_backend_vulkan_svd_complex128` and
  `omni_tensor_backend_vulkan_svd_complex64`, including checked complex shape
  validation and native status-slot handling. Wired those helpers through the
  C3 extern layer and `matrix_svd_try_vulkan_value`.
- `matrix/svd` on Vulkan `Complex128` now returns Vulkan `Complex128` `u`/`v`
  tensors and Vulkan `Float64` `s`; Vulkan `Complex64` returns Vulkan
  `Complex64` `u`/`v` and Vulkan `Float32` `s`. The path fails closed through
  the Vulkan helper status instead of copying to CPU/LAPACK.
- Added focused tests for Complex128/Complex64 Vulkan factor dtype, shape,
  device placement, singular values, and no-LAPACK fallback.
- Validation:
  - `glslangValidator -V csrc/tensor_vulkan_svd_complex128.comp`
  - `glslangValidator -V csrc/tensor_vulkan_svd_complex64.comp`
  - `spirv-val /tmp/tensor_vulkan_svd_complex128.spv`
  - `spirv-val /tmp/tensor_vulkan_svd_complex64.spv`
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1941 fail=0`
  - `scripts/check_build_config_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`

## 2026-04-22 - TENSOR-100H CPU Hermitian Fixed-Width Complex Eigen

- Closed `TENSOR-100H-COMPLEX-EIGEN-HERMITIAN-CPU` for CPU Hermitian
  fixed-width complex `matrix/eigenvalues` and `matrix/eigenvectors`.
- `matrix/eigenvalues` now accepts exact-Hermitian CPU `Complex128` and
  `Complex64` rank-2 square tensors. Returned values are component-real:
  `Float64` for `Complex128`, `Float32` for `Complex64`.
- `matrix/eigenvectors` now returns `{ values, vectors }` for the same fixed
  complex Hermitian inputs. Values are component-real and vectors preserve the
  input complex dtype.
- Added an exact Hermitian validation gate and a pure Complex128 Jacobi
  factorization path with deterministic phase normalization. `Complex64`
  widens to the Complex128 oracle and narrows values/vectors back to
  `Float32`/`Complex64`.
- Negative constraint: do not reuse the existing SVD Hermitian power helper for
  general Hermitian eigenvalues; that helper intentionally clamps negative
  eigenvalues for positive-semidefinite Gram matrices.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1948 fail=0`

## 2026-04-22 - TENSOR-100H Hermitian Complex Eigen Capability Bits

- Closed `TENSOR-100H-COMPLEX-EIGEN-HERMITIAN-CAPS` for narrow
  operation-specific eigen capability reporting.
- `tensor-backends` now reports `matrix-hermitian-eigen-complex128` and
  `matrix-hermitian-eigen-complex64` true only on CPU, matching the shipped
  CPU Hermitian fixed-width complex `matrix/eigenvalues` /
  `matrix/eigenvectors` implementation.
- `matrix-eigenpairs-complex128` and `matrix-eigenpairs-complex64` remain false
  on CPU, CUDA, cuBLAS, and Vulkan because the general fixed-width
  `matrix/eigenpairs` contract for `Float32`, `Complex128`, and `Complex64` is
  still open.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1948 fail=0`

## 2026-04-22 - LAPACK dgeev Disable Gate Audit Fix

- Fixed `omni_tensor_backend_lapack_dgeev` so the execution wrapper honors the
  `dgeev` disable hook and `OMNI_TENSOR_DISABLE_LAPACK_DGEEV`, returning
  unavailable before attempting LAPACK dispatch.
- Strengthened the forced pure `matrix/eigenpairs` fallback test to assert that
  the `dgeev` call counter is unchanged while the routine is disabled. This
  prevents a false pass where `available()` reports disabled but the wrapper
  still calls LAPACK.
- Negative constraint: fallback tests that disable a backend routine must check
  execution-side evidence, such as per-routine call counters, not just
  capability probe results.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1948 fail=0`

## 2026-04-22 - TENSOR-100H CPU General Fixed-Width Eigenpairs

- Closed `TENSOR-100H-COMPLEX-EIGEN-GENERAL-CPU` for CPU general fixed-width
  `matrix/eigenpairs`.
- `matrix/eigenpairs` now accepts square CPU `Float32`, `Complex128`, and
  `Complex64` tensors in addition to the existing `Float64` route.
  `Float32`/`Complex64` return `Complex64` `values` and `vectors`; `Float64` /
  `Complex128` return `Complex128` result tensors.
- Added runtime-loaded LAPACK `sgeev`, `zgeev`, and `cgeev` helper wiring with
  availability probes, call counters, test disable hooks, and environment
  disables (`OMNI_TENSOR_DISABLE_LAPACK_SGEEV`,
  `OMNI_TENSOR_DISABLE_LAPACK_ZGEEV`, `OMNI_TENSOR_DISABLE_LAPACK_CGEEV`).
- Float32 uses `sgeev` when available and falls back to the widened pure real
  eigenpair path when `sgeev` is unavailable/disabled. Complex128 and Complex64
  use `zgeev`/`cgeev` and fail closed with `tensor/backend-unavailable` when the
  required complex LAPACK entrypoint is unavailable.
- CPU `tensor-backends` now reports `matrix-eigenpairs-complex128` and
  `matrix-eigenpairs-complex64` according to `zgeev`/`cgeev` availability;
  CUDA, cuBLAS, and Vulkan remain false.
- Negative constraint: do not use realification as a pure general complex
  eigensolver because it cannot distinguish an arbitrary complex eigenvalue
  from its conjugate.
- Validation:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1969 fail=0`

## 2026-04-22 - TENSOR-100H Vulkan Hermitian Fixed-Width Complex Eigen

- Closed `TENSOR-100H-COMPLEX-EIGEN-VULKAN-HERMITIAN` for backend-native
  Vulkan fixed-width complex `matrix/eigenvalues` and `matrix/eigenvectors`.
- Added native Vulkan `Complex128` and `Complex64` Hermitian Jacobi shaders,
  checked-in SPIR-V blobs, helper ABI exports, C3 externs, and routing from the
  public matrix eigenvalue/eigenvector primitives.
- `matrix/eigenvalues` now accepts Vulkan-placed dense row-major Hermitian
  `Complex128`/`Complex64` tensors and returns Vulkan-placed component-real
  `Float64`/`Float32` value tensors.
- `matrix/eigenvectors` now returns Vulkan-placed `{ values, vectors }` for the
  same inputs; vectors preserve `Complex128`/`Complex64` dtype.
- `tensor-backends` now reports Vulkan `matrix-hermitian-eigen-complex128`
  according to Float64 support and `matrix-hermitian-eigen-complex64`
  according to Float32 support. Vulkan general `matrix-eigenpairs-complex128`
  and `matrix-eigenpairs-complex64` remain false.
- Split the former Vulkan Hermitian/general TODO into the closed Hermitian item
  and open `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL`.
- Negative constraint: do not treat the shipped Hermitian Jacobi shader as a
  general non-Hermitian complex eigensolver; arbitrary complex Vulkan
  `matrix/eigenpairs` needs a separate backend-native solver design and must
  not fall back to CPU/LAPACK or realification.
- Validation:
  - `glslangValidator -V csrc/tensor_vulkan_hermitian_eigen_complex128.comp`
  - `glslangValidator -V csrc/tensor_vulkan_hermitian_eigen_complex64.comp`
  - `spirv-val --target-env vulkan1.0` for both generated SPIR-V modules
  - `./scripts/build_omni_chelpers.sh`
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1975 fail=0`

## 2026-04-22 - Vulkan General Complex Eigenpairs Fail-Closed Audit

- Closed the stale broad TODO wording that still described direct Vulkan
  general `matrix/eigenpairs` as blocked by the old BigComplex result contract.
  CPU fixed-width general eigenpairs now define the public result contract, and
  Vulkan's remaining open work is specifically backend-native non-Hermitian
  `Complex128`/`Complex64` solver execution.
- Strengthened Vulkan fixed-width complex `matrix/eigenpairs` regression
  coverage so `Complex128` and `Complex64` Vulkan inputs fail closed while the
  backend capability bits remain false.
- Added execution-side no-fallback evidence: the focused fail-closed path now
  asserts LAPACK `zgeev`/`cgeev` call counters do not change when Vulkan
  general complex eigenpairs are requested.
- Updated the Vulkan eigensolver plan to name the shipped phases
  (symmetric real Float64 and Hermitian Complex128/Complex64), the remaining
  general non-Hermitian solver boundary, and the recommended staged
  Hessenberg/shifted-QR helper ABI direction.
- Negative constraint: do not route Vulkan general complex `matrix/eigenpairs`
  through CPU/LAPACK fallback, realification, or the Hermitian Jacobi shader.
- Validation:
  - `c3c build --obj-out obj`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
  - focused `advanced-collections-module` -> `pass=1979 fail=0`

## 2026-04-22 - CUDA PTX Resolver Retry and Module Cleanup Fix

- Fixed CUDA PTX helper resolvers for dense map, scientific map, rounding, and
  native ML optimizer kernels so temporary CUDA unavailability does not
  permanently poison the module-attempted cache.
- The resolvers now retry after `omni_tensor_backend_cuda_available()`,
  CUDA-driver resolution, or `cuInit` fail. They only cache failure after a
  PTX module load or required-kernel symbol lookup failure, matching the
  existing complex-map and complex-matrix resolver posture.
- Fixed loaded-module cleanup on required-symbol lookup failure for dense map,
  scientific map, and rounding resolvers; those paths now call
  `cuModuleUnload` before returning unavailable, as the complex resolver paths
  already did.
- Applied the same retry/cache policy to the native CUDA ML optimizer resolver,
  preserving the existing contract that resolved native fused-kernel failures
  fail closed instead of silently falling back.
- Validation:
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` -> `pass=1979 fail=0`

## 2026-04-22 - FFI dlopen Registry Path Hardening

- Hardened the shared FFI `dlopen` registry so library path slices are copied
  into an explicit NUL-terminated buffer before calling `dlopen`.
- Empty library paths and paths at or above the registry storage cap now fail
  closed before `dlopen`, avoiding invalid `&path[0]` use for empty slices and
  avoiding silent registry-key truncation for overlong paths.
- `eval_ffi_lib` now reports a direct user-facing diagnostic for empty or
  overlong paths. The AOT FFI bridge rejects overlong sonames before creating a
  truncated `FFI_HANDLE` wrapper.
- Added focused FFI surface coverage for empty-path rejection and direct
  registry rejection of a 256-byte non-NUL path slice.
- Validation:
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface ./build/main --test-suite lisp` -> `pass=117 fail=0`
  - `git diff --check`

## 2026-04-22 - CUDA Explicit Copy Materializes CPU Transpose Views

- Removed the front-door CUDA copy veto for CPU-backed `matrix/transpose-view`
  sources at explicit copy boundaries. `to-device 'cuda` now lets the existing
  CPU view realization path produce dense CPU storage before copying to CUDA.
- Extended destination-form `realize` into CUDA tensors the same way:
  CPU-backed transpose views may materialize to dense CPU storage, then copy
  into an existing CUDA destination. Non-CPU views and arbitrary backend-view
  execution still fail closed.
- Added focused advanced collection tests for CUDA `to-device` and CUDA
  destination `realize` materializing CPU transpose views when CUDA is
  available, plus fail-closed coverage for Vulkan-backed transpose views sent
  to CUDA.
- Negative constraint: this is not stride-aware CUDA kernel execution. Do not
  route CUDA/Vulkan kernels or raw backend helpers through implicit copies to
  hide missing offset/stride/backing-extent helper ABIs.
- Validation:
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` -> `pass=1980 fail=0`

## 2026-04-22 - Adam Optimizer Nil Moment State Normalization

- Normalized `ml/optimizer-step` Adam/AdamW state handling so explicit
  `nil` `first-moment` and `second-moment` entries are treated the same as
  absent optional state, matching the existing SGD `velocity` and RMSProp
  `square-average` / `velocity` optional-state convention.
- One-sided Adam state still fails closed: if exactly one moment is present
  after `nil` normalization, the call raises `tensor/invalid-argument`.
- Threaded the same presence check through CPU, CUDA, and Vulkan Adam leaf
  dispatch so lower-level device paths do not reinterpret `nil` state as
  malformed tensor state.
- Added CPU, CUDA, and Vulkan advanced collection regressions. CUDA/Vulkan
  cases are backend-capability guarded and assert initialized moment tensors
  preserve the active device.
- Validation:
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` -> `pass=1983 fail=0`

## 2026-04-22 - Vulkan SGD Accepts Zero Learning Rate

- Fixed the Vulkan `ml/optimizer-step` SGD helper so the C helper accepts
  `learning_rate == 0.0f`. This matches the public optimizer spec validation
  and the CPU/CUDA optimizer paths, which allow non-negative learning rates.
- Added a guarded Vulkan `Float32` SGD regression proving zero learning rate is
  a no-op that preserves Vulkan placement and returns stateless optimizer state.
- Validation:
  - `scripts/check_file_size_gate.sh`
  - `git diff --check -- csrc/tensor_vulkan_helpers_ml_optimizer.c src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` -> `pass=1984 fail=0`

## 2026-04-22 - Vulkan Transpose View Constructor Fail-Closes Nested Views

- Tightened `matrix/transpose-view` for Vulkan tensors so the view constructor
  now enforces its documented contract: the source must be concrete,
  zero-offset, dense row-major Vulkan storage. A valid device handle alone is
  not enough.
- This prevents nested Vulkan transpose views from being represented as if they
  were direct views over concrete storage. Direct concrete Vulkan transpose
  views are still supported, and `realize` remains the explicit path for
  materializing a direct Vulkan view into dense storage.
- Added a guarded advanced regression asserting that
  `matrix/transpose-view` over an existing Vulkan transpose view fails closed
  with `tensor/backend-unsupported`.
- Negative constraint: do not treat view-backed Vulkan tensors as concrete
  backend helper inputs unless the helper ABI explicitly receives and validates
  the required view metadata, including offset/backing extent.
- Validation:
  - `scripts/check_file_size_gate.sh`
  - `git diff --check -- src/lisp/prim_tensor_matrix_lu_svd_core_b.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part4.c3`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` -> `pass=1985 fail=0`

## 2026-04-22 - CUDA SGD Optimizer Dense Layout and Capability Guard

- Hardened the CUDA `ml/optimizer-step` SGD leaf path so it now explicitly
  rejects non-dense-row-major parameter, gradient, and velocity tensors before
  entering either the fused CUDA SGD helper or the map-backed fallback.
- This aligns CUDA SGD with CUDA Adam/RMSProp and Vulkan SGD, which already
  enforced dense layout before raw backend helper dispatch. It also prevents
  future CUDA view or strided concrete tensors from being interpreted as linear
  `byte_len` storage by the fused SGD kernel ABI.
- Corrected CUDA backend capability reporting so
  `ml-optimizer-sgd-float32` is true when either the fused CUDA SGD helper or
  the generic CUDA Float32 elementwise map path is available. Adam/AdamW and
  RMSProp remain map-backed and still follow `elementwise-map-float32`.
- The native fused CUDA Adam/AdamW/RMSProp TODO remains open as
  `ML-VK-060-FUSED-CUDA-STATEFUL-001`; this checkpoint does not add new fused
  stateful CUDA kernels.
- Validation:
  - `git diff --check -- src/lisp/prim_ml_optimizer_cuda.c3 src/lisp/prim_tensor_backend_ops.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` -> `pass=1985 fail=0`

## 2026-04-22 - CUDA Backend Disable Clears Cached Module Capabilities

- Hardened CUDA module resolver entrypoints so cached PTX function pointers no
  longer bypass the live CUDA availability/test-disable gate. Elementwise map,
  scientific map, complex map, complex matrix, rounding, and ML optimizer
  resolver paths now return unavailable when `omni_tensor_backend_cuda_available`
  is false, even if the module was resolved earlier in the process.
- This keeps `tensor-backends` capability reporting aligned with live backend
  state: disabling CUDA for tests now clears cached operation capability bits
  instead of leaving stale map/scientific/complex/rounding/optimizer fields
  truthy.
- Added a focused advanced regression asserting that
  `omni_tensor_backend_cuda_disable_for_tests(1)` clears cached CUDA operation
  capabilities after the CUDA backend row has been queried.
- Negative constraint: cached CUDA helper function pointers are module state,
  not live availability authority. Public capability probes must still pass
  through the backend availability gate.
- Validation:
  - `git diff --check -- csrc/tensor_cuda_helpers.c csrc/tensor_cuda_helpers_ml_optimizer.inc src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3`
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` -> `pass=1986 fail=0`

## 2026-04-22 - Vulkan MSE View Materialization and Helper Layout Guard

- Hardened the internal Vulkan `ml/mean-squared-error` helper boundary so it
  now requires dense row-major prediction and target tensors before dispatching
  the raw linear Vulkan MSE helper.
- Added a focused public regression proving `ml/mean-squared-error` preserves
  logical semantics for Vulkan transpose views by materializing them before the
  raw helper path. A transposed Vulkan `Float32` prediction compared with its
  dense logical equivalent returns zero loss on Vulkan.
- Invalidated assumption: public `ml/mean-squared-error` does not pass Vulkan
  view payloads directly to the raw helper; the concrete resolver materializes
  views first. The dense helper guard is still retained as an internal
  precondition so future direct helper use cannot reinterpret strided storage
  as linear storage.
- Validation:
  - `git diff --check -- src/lisp/prim_ml_vulkan_losses.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` -> `pass=1987 fail=0`

## 2026-04-22 - Vulkan Autograd Broadcast Alias Layout Guard

- Hardened the Vulkan `ml/grad` broadcast-reduction helper so shape aliasing
  over retained Vulkan storage now requires valid Vulkan device storage and
  dense row-major metadata for both the reduced gradient source and the target
  shape tensor.
- The broadcast-reduction helper now also enforces dense row-major upstream,
  child, and output tensors before invoking the Vulkan reduction path. This
  keeps the internal helper contract aligned with the public autograd preflight
  and prevents future direct helper calls from treating strided or view-backed
  tensors as linear `byte_len` storage.
- Added a focused advanced regression proving the Vulkan broadcast-gradient
  alias path returns a Vulkan dense concrete gradient with the logical
  singleton shape and correct values after CPU copyback.
- Removed a duplicated `module lisp`/import prologue from
  `src/lisp/prim_tensor_validation.c3`; this is a non-behavioral cleanup of
  validation helper source noise found during the adjacent device-storage guard
  audit.
- Removed the same duplicated prologue pattern from
  `src/lisp/prim_tensor_map_callable_ops.c3`, and verified that a repo scan for
  duplicate `module lisp;` declarations in `src/lisp` now returns no matches.
- Corrected stale fixed-width complex plan state in
  `docs/plans/fixed-width-complex-closure-plan-2026-04-18.md`: CPU general
  fixed-width `matrix/eigenpairs` and operation-specific capability bits are
  now marked closed, and the execution order points to the concrete open CUDA
  adapter/execution TODOs instead of the superseded broad
  `TENSOR-100H-CUDA-SVD-NORMS` umbrella.
- Repaired `scripts/check_status_consistency.sh` after TODO, changelog, and
  memory-runtime docs were split into index plus part files. The checker now
  counts unchecked TODO checkboxes from `docs/todo_parts/`, extracts the latest
  dated changelog heading from `memory/changelog_parts/`, and reads status
  metadata from split area-doc part directories. Area status `As of` metadata
  for memory runtime, types/dispatch, FFI/foreign runtime, and validation was
  advanced to 2026-04-22 after the gate parsed and verified the expected
  green/green/yellow/green status contract.
- Hardened the split TODO count path so a future zero-open-item part set
  returns `0` under `set -o pipefail` instead of failing the checker when
  `rg` exits non-zero for no matches.
- Negative constraint: do not treat a retained Vulkan buffer or non-null
  device handle as sufficient authority to re-label storage under new dense
  tensor metadata. Shape aliasing is only valid when the source storage and
  target metadata are dense row-major with matching element and byte counts.
- Validation:
  - `git diff --check -- src/lisp/prim_tensor_validation.c3 src/lisp/prim_ml_autograd_vulkan_broadcast.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  - `rg -n "^module lisp;" src/lisp | awk -F: '{count[$1]++} END {for (f in count) if (count[f] > 1) print count[f], f}' | sort -nr` -> no output
  - `rg -n '\[~\] Add CPU general|\[~\] Add operation-specific|Next: TENSOR-100H-CUDA-SVD-NORMS' docs/plans/fixed-width-complex-closure-plan-2026-04-18.md` -> no output
  - `bash -n scripts/check_status_consistency.sh`
  - `scripts/check_status_consistency.sh` -> latest changelog date `2026-04-22`, TODO actionable count `11`, memory runtime `green`, types dispatch `green`, FFI foreign runtime `yellow`, validation status `green`
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` -> `pass=1988 fail=0`

## 2026-04-22 - Autograd Device Tensor Identity Storage Guard

- Hardened `ml_grad_expr_same_tensor` so non-CPU concrete Tensor identity via
  shared device handle now also requires valid dense zero-offset device
  storage on both operands before treating the handles as the same autograd
  leaf.
- This prevents future offset/strided device-backed concrete tensors from
  being treated as identical solely because they share an opaque device
  allocation and metadata shape. CPU identity comparison remains value-based
  after concrete storage validation.
- File-size note: `src/lisp/prim_ml_autograd_tensor_expr.c3` is now exactly
  1000 LOC, which is within the active code-file split threshold and must not
  receive further line additions without splitting or removing equivalent
  lines.
- Negative constraint: do not use opaque CUDA/Vulkan handle equality as the
  only identity authority for autograd `wrt` leaf matching. Device identity
  needs the same valid-storage/dense-layout preconditions as raw helper
  dispatch paths.
- Validation:
  - `git diff --check -- src/lisp/prim_ml_autograd_tensor_expr.c3`
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` -> `pass=1988 fail=0`

## 2026-04-22 - Split TODO Open-Item ID Guard

- Assigned concrete task IDs to the remaining anonymous unchecked nested TODO
  rows under the live `TENSOR-100F` backlog:
  `TENSOR-100F-VK-SVD-LARGE-K-PERF-001`,
  `TENSOR-100F-VK-EIGEN-LARGE-N-PERF-001`,
  `TENSOR-100F-STRIDE-AWARE-HELPERS-001`,
  `TENSOR-100F-CUDA-MAP-BROADEN-001`,
  `TENSOR-100F-LU-BLOCKED-PERF-001`, and
  `TENSOR-100F-BROAD-VALIDATION-001`.
- Extended `scripts/check_status_consistency.sh` so every unchecked TODO row in
  `docs/todo_parts/` must start with a backtick task ID. The guard uses a
  portable `rg | awk` filter instead of unsupported regex lookahead.
- Validation:
  - `bash -n scripts/check_status_consistency.sh`
  - `scripts/check_status_consistency.sh` -> latest changelog date `2026-04-22`, TODO actionable count `11`, memory runtime `green`, types dispatch `green`, FFI foreign runtime `yellow`, validation status `green`
  - `(rg -n '^[[:space:]]*-[[:space:]]+\[[[:space:]]\]' docs/todo_parts || true) | awk '$0 !~ /\][[:space:]]+`/'` -> no output
  - `git diff --check -- docs/todo_parts/todo_part_01.md docs/todo_parts/todo_part_02.md scripts/check_status_consistency.sh`
  - `scripts/check_file_size_gate.sh`

## 2026-04-22 - Vulkan Attention No-Mask ABI Cleanup

- Cleaned up the Vulkan `ml/scaled-dot-product-attention` no-mask ABI so the
  C3 caller now passes `null` and `0` for the mask buffer when `mask_kind == 0`
  instead of passing the query tensor as a placeholder mask.
- Updated `omni_tensor_backend_vulkan_ml_attention_f32` to allow a null mask
  pointer only for `mask_kind == 0`. The helper still binds the query buffer as
  an internal descriptor placeholder because the shader layout has a mask
  binding, but the public/native ABI no longer pretends a mask exists.
- Negative constraint: optional backend operands should be represented as
  `null`/`0` at the ABI boundary when absent. If a fixed shader descriptor
  layout needs a dummy binding, choose that internally after validating
  `mask_kind`, not in the C3 caller.
- Validation:
  - `git diff --check -- src/lisp/prim_ml_attention.c3 csrc/tensor_vulkan_helpers_ml_attention.c`
  - `cc -fsyntax-only -I csrc csrc/tensor_vulkan_helpers_ml_attention.c`
  - `scripts/check_file_size_gate.sh`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` -> `pass=1988 fail=0`

## 2026-04-22 - Vulkan Map Dtype-Mismatch Diagnostic Hardening

- Aligned Vulkan map dtype diagnostics with the CUDA map contract: same-device
  Tensor operands with mismatched storage dtypes now raise
  `tensor/dtype-mismatch` with `map: Vulkan tensor dtype mismatch` instead of
  reporting the mismatch as backend-unsupported.
- The change covers the public map preflight, direct Vulkan map execution, and
  lazy Vulkan map-expression realization. Unsupported dtype families and
  unsupported layouts still fail closed as backend-unsupported.
- Added a guarded advanced regression for mixed Float64/Float32 Vulkan map
  operands when both Vulkan placement dtypes are available.
- Validation:
  - `git diff --check -- src/lisp/prim_tensor_map.c3 src/lisp/prim_tensor_vulkan_map_direct.c3 src/lisp/prim_tensor_cuda_map_expr.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3`
  - `scripts/check_file_size_gate.sh`
  - `scripts/check_status_consistency.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1989 fail=0`

## 2026-04-22 - Matrix Workspace Storage Guard Audit

- Hardened matrix workspace/copy helper boundaries so non-empty malformed CPU
  tensors with null backing storage fail closed before allocation or
  `mem::copy`.
- Covered helper paths in `src/lisp/prim_tensor_matrix_eigen_primitives.c3`,
  `src/lisp/prim_tensor_matrix_lu_svd_core_b.c3`, and
  `src/lisp/prim_tensor_matrix_lu_cpu_solve.c3`, including Hermitian complex
  eigen workspaces, general fixed-width eigen fallback copies, SVD workspace
  copies, and complex solve/inverse/LU/determinant work buffers.
- Negative constraint: do not treat a non-zero `byte_len` or shape-derived
  element count as proof that `TensorVal.data` is non-null. Shared helpers must
  enforce the storage precondition locally because future internal callers may
  not be protected by public entry-point validation.
- Validation:
  - `git diff --check -- src/lisp/prim_tensor_matrix_eigen_primitives.c3 src/lisp/prim_tensor_matrix_lu_svd_core_b.c3 src/lisp/prim_tensor_matrix_lu_cpu_solve.c3`
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1989 fail=0`

## 2026-04-22 - CUDA Launch Grid Guard Audit

- Added a shared `omni_tensor_cuda_grid_dim_1d` helper that computes CUDA
  one-dimensional launch grids without addition overflow and rejects launch
  counts whose grid dimension would exceed the CUDA driver `unsigned int`
  parameter limit.
- Replaced unchecked cast-based grid calculations in CUDA Float32/Float64 map,
  rounding, complex matrix structural helpers, and the native fused CUDA SGD
  optimizer helper.
- Added `omni_tensor_backend_cuda_grid_dim_oversized_guard_for_tests` and
  focused advanced coverage proving oversized launch requests return
  `OMNI_TENSOR_CUDA_INVALID_ARGUMENT` instead of silently truncating the grid.
- Negative constraint: do not use
  `(unsigned int)((count + block - 1) / block)` or similar unchecked casts for
  CUDA launches. Route new 1-D helpers through the shared checked grid helper
  so oversized work fails closed before output buffers are exposed.
- Validation:
  - `cc -fsyntax-only -I csrc csrc/tensor_cuda_helpers.c`
  - `git diff --check -- csrc/tensor_cuda_helpers.c csrc/tensor_cuda_helpers_ml_optimizer.inc csrc/tensor_cuda_helpers_map_binary.inc csrc/tensor_cuda_helpers_map_unary_round.inc csrc/tensor_cuda_helpers_complex_matrix.inc src/lisp/tensor_cuda_backend.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3`
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1990 fail=0`

## 2026-04-22 - CUDA Copy Null-Alias Guard Audit

- Hardened `omni_tensor_backend_cuda_copy_device_to_existing_device` so
  non-empty null source/destination pointers are rejected before same-pointer
  alias no-op handling.
- Zero-byte CUDA device-to-device copies still succeed as no-ops, and valid
  non-null same-device pointer copies still succeed without calling
  `cudaMemcpy`.
- Added `omni_tensor_backend_cuda_copy_null_alias_guard_for_tests`, C3 extern
  wiring, and focused advanced coverage proving `(NULL, 1, NULL)` returns
  `OMNI_TENSOR_CUDA_INVALID`.
- Negative constraint: do not check source/destination pointer equality before
  validating non-empty CUDA copy pointers. Null aliases are invalid for
  non-empty copies.
- Validation:
  - `cc -fsyntax-only -I csrc csrc/tensor_cuda_helpers.c`
  - `git diff --check -- csrc/tensor_cuda_helpers_public_memory.inc src/lisp/tensor_cuda_backend.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3`
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1991 fail=0`

## 2026-04-22 - CUDA Map Grid-Failure Cleanup Audit

- Closed cleanup gaps created by explicit checked-grid failure paths in CUDA
  map helpers.
- `csrc/tensor_cuda_helpers_map_binary.inc` now frees broadcast-offset buffers
  before returning scalar/binary grid-size errors, and frees broadcast offsets,
  status device storage, and output storage before returning complex-map
  grid-size errors.
- `csrc/tensor_cuda_helpers_map_unary_round.inc` now frees the status device
  buffer before returning scientific unary, complex unary, and round-to-int
  grid-size errors.
- Negative constraint: do not add fail-closed validation after helper
  allocations without auditing all temporaries already owned by that branch.
  CUDA map grid-size failures are cleanup paths, not pure argument-validation
  paths.
- Validation:
  - `rg -n -C 3 "grid_status != OMNI_TENSOR_CUDA_SUCCESS" csrc/tensor_cuda_helpers_map_binary.inc csrc/tensor_cuda_helpers_map_unary_round.inc csrc/tensor_cuda_helpers_complex_matrix.inc`
  - `cc -fsyntax-only -I csrc csrc/tensor_cuda_helpers.c`
  - `git diff --check -- csrc/tensor_cuda_helpers_map_binary.inc csrc/tensor_cuda_helpers_map_unary_round.inc`
  - `scripts/check_file_size_gate.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1991 fail=0`

## 2026-04-22 - CUDA ML SGD Byte-Length Overflow Guard Audit

- Hardened `omni_tensor_backend_cuda_ml_sgd_f32` so it rejects
  `element_count > SIZE_MAX / sizeof(float)` before evaluating
  `element_count * sizeof(float)` in the byte-length precondition.
- Added `omni_tensor_backend_cuda_ml_sgd_byte_len_overflow_guard_for_tests`,
  C3 extern wiring, and focused advanced coverage proving the oversized count
  returns `OMNI_TENSOR_CUDA_INVALID` before backend availability or allocation
  can mask the contract.
- Negative constraint: do not compare a provided byte length against
  `count * sizeof(T)` until `count` has been bounded against `SIZE_MAX /
  sizeof(T)`. Wrapped products must fail closed as invalid arguments.
- Validation:
  - `cc -fsyntax-only -I csrc csrc/tensor_cuda_helpers.c`
  - `git diff --check -- csrc/tensor_cuda_helpers_ml_optimizer.inc src/lisp/tensor_cuda_backend.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1992 fail=0`
  - `scripts/check_file_size_gate.sh`
  - `scripts/check_status_consistency.sh`

## 2026-04-22 - Vulkan Map Chain Byte-Length Overflow Guard Audit

- Hardened Vulkan Float32 map-chain dispatch byte-length validation so
  oversized `element_count` values are rejected before evaluating
  `element_count * sizeof(float)`.
- Added `omni_tensor_vulkan_map_chain_f32_validate_byte_len` and reused it in
  both `omni_tensor_backend_vulkan_map_scalar_chain_f32` and
  `omni_tensor_backend_vulkan_map_tensor_scalar_chain_f32`.
- Added
  `omni_tensor_backend_vulkan_map_chain_f32_byte_len_overflow_guard_for_tests`,
  C3 extern wiring, and focused advanced coverage proving the oversized count
  returns `OMNI_TENSOR_VULKAN_UNSUPPORTED` without requiring Vulkan
  availability.
- Negative constraint: do not place `byte_len != count * sizeof(T)` before
  count/size bounds in compound native preconditions. Route duplicated
  byte-length contracts through a shared helper when the same guard applies to
  multiple Vulkan/CUDA entry points.
- Validation:
  - `cc -fsyntax-only -I csrc csrc/tensor_vulkan_helpers_dispatch_batch.c`
  - `git diff --check -- csrc/tensor_vulkan_helpers_dispatch_batch.c src/lisp/tensor_vulkan_backend_batch.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1993 fail=0`
  - `scripts/check_file_size_gate.sh`
  - `scripts/check_status_consistency.sh`

## 2026-04-22 - TENSOR-100H CUDA Complex SVD Adapter PTX

- Closed `TENSOR-100H-CUDA-SVD-NORMS-ADAPTERS`.
- Added CUDA Complex128/Complex64 SVD layout adapter kernels to
  `csrc/tensor_cuda_complex_matrix.cu`:
  - row-major Omni matrix to cuSOLVER column-major input,
  - wide-matrix adjoint-input preparation,
  - cuSOLVER column-major `U` to public row-major `u`,
  - cuSOLVER `VT = V^H` to public row-major `v`.
- Regenerated the complex matrix PTX and split it into
  `csrc/tensor_cuda_complex_matrix_ptx_part_00.inc` and
  `csrc/tensor_cuda_complex_matrix_ptx_part_01.inc` because the generated
  single include exceeded the 1000 LOC gate.
- Wired the new symbols into `omni_tensor_cuda_complex_matrix_resolve` and
  exposed native helper entry points plus
  `omni_tensor_backend_cuda_svd_adapter_shape_guard_for_tests`.
- Fixed an existing cleanup leak in
  `csrc/tensor_cuda_helpers_complex_matrix.inc`: if launch-grid calculation
  fails after device output allocation, the allocated output buffer is freed
  before returning.
- Added C3 externs in `src/lisp/tensor_cuda_backend.c3` and a focused
  advanced collections guard test in
  `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3`.
- Negative constraint: do not realify fixed-width complex CUDA SVD through
  doubled real matrices, do not expose cuSOLVER `VT` directly as public `v`,
  and do not silently copy CUDA operands to CPU/LAPACK. Adapter shape/byte
  validation must happen before CUDA availability, allocation, or launch.
- Invalidated blocker: local CUDA PTX generation is available in this
  workspace via `/usr/local/cuda-13.0/bin/nvcc` and
  `/usr/local/cuda-13.0/bin/ptxas`.
- Remaining CUDA SVD work: `TENSOR-100H-CUDA-SVD-NORMS-EXEC` must add the
  cuSOLVER execution helper and route `matrix/singular-values`, spectral/nuclear
  `matrix/norm`, and `matrix/svd`.
- Validation:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_complex_matrix.cu -o /tmp/tensor_cuda_complex_matrix.ptx`
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/tensor_cuda_complex_matrix.ptx -o /tmp/tensor_cuda_complex_matrix.o`
  - `cc -fsyntax-only -I csrc csrc/tensor_cuda_helpers.c`
  - `git diff --check -- csrc/tensor_cuda_complex_matrix.cu csrc/tensor_cuda_helpers.c csrc/tensor_cuda_helpers_complex_matrix.inc csrc/tensor_cuda_complex_matrix_ptx_part_00.inc csrc/tensor_cuda_complex_matrix_ptx_part_01.inc src/lisp/tensor_cuda_backend.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1994 fail=0`
  - `scripts/check_file_size_gate.sh`

## 2026-04-22 - TENSOR-100H CUDA Complex SVD Execution Route

- Closed `TENSOR-100H-CUDA-SVD-NORMS-EXEC`.
- Added `csrc/tensor_cuda_helpers_cusolver_svd.inc`, included from
  `csrc/tensor_cuda_helpers.c`, with cuSOLVER-backed Complex128/Complex64
  reduced SVD helpers.
- The native helpers:
  - validate shape products, byte lengths, and cuSOLVER `int` dimensions before
    CUDA allocation,
  - use the shipped CUDA SVD adapter ABI for row-major input, wide-matrix
    adjoint input, column-major `U`, and `VT = V^H`,
  - return CUDA-owned `u/s/v` device buffers,
  - update `zgesvd` / `cgesvd` counters,
  - clean up all temporary CUDA allocations on failure.
- Routed CUDA fixed-width complex public operations:
  - `matrix/singular-values` returns CUDA Float64/Float32 singular-value
    tensors for CUDA Complex128/Complex64 inputs,
  - spectral/nuclear `matrix/norm` computes through cuSOLVER singular values,
  - `matrix/svd` returns CUDA `u`, `s`, and `v` tensors with reduced factor
    shapes `[rows k]`, `[k]`, and `[cols k]`.
- Updated `tensor-backends` so CUDA operation-specific
  `matrix-singular-values-complex128`, `matrix-singular-values-complex64`,
  `matrix-svd-complex128`, and `matrix-svd-complex64` follow
  `cusolver-complex-svd && matrix-structural-complex*`; broad CUDA
  `matrix-numerical-complex128` and `matrix-numerical-complex64` remain false.
- Added focused advanced tests for CUDA Complex128 singular values/norms,
  CUDA Complex64 wide SVD factors, and cuSOLVER counter movement.
- Negative constraint: do not silently copy CUDA operands to CPU/LAPACK, do not
  realify fixed-width complex CUDA SVD through doubled real matrices, and do
  not broaden CUDA complex numerical matrix capability from this operation
  family alone.
- Validation:
  - `cc -fsyntax-only -I csrc csrc/tensor_cuda_helpers.c`
  - `git diff --check -- csrc/tensor_cuda_helpers.c csrc/tensor_cuda_helpers_cusolver_svd.inc src/lisp/tensor_cuda_backend.c3 src/lisp/prim_tensor_matrix_lu_cuda_backend.c3 src/lisp/prim_tensor_matrix_svd_primitives.c3 src/lisp/prim_tensor_matrix_lu_svd_core_c.c3 src/lisp/prim_tensor_backend_ops.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part5.c3`
  - `scripts/check_file_size_gate.sh`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - direct CUDA probes for Complex128 singular values/norm and Complex64 wide
    SVD
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1999 fail=0`

## 2026-04-22 - FFI Native Argument Vector Guard Audit

- Hardened `omni_ffi_call` and `omni_ffi_call_var` so positive-arity calls
  reject null `arg_types` or `arg_values` before allocating libffi type
  storage or dereferencing caller metadata.
- Hardened `omni_ffi_closure_alloc` so caller output slots are cleared on
  entry, invalid positive-arity closure type vectors fail closed, and `out_code`
  is cleared again if `ffi_prep_closure_loc` fails after `ffi_closure_alloc`
  produced an executable pointer.
- Added native no-crash probes for fixed-call, variadic-call, and closure
  failure-output contracts; exposed them through `src/lisp/prim_ffi_callback.c3`
  and covered them in the focused advanced FFI surface group.
- Hardened callback construction in `src/lisp/prim_ffi_callback.c3` so callback
  parameter counts are bounded before `FfiTypeTag`/C `int` type-table
  allocation and before narrowing the count into the native libffi `int` ABI.
- Hardened declarative FFI binding setup in `src/lisp/eval_ffi_eval.c3` so
  interpreter-side parameter metadata allocation rejects counts above the
  supported primitive arity and rejects `FfiTypeTag`/`FfiHandlePolicy` table
  size overflow before root-scope allocation.
- Hardened `src/lisp/aot_runtime_bridge_ffi.c3` so AOT FFI binding setup
  applies the same size guard to both parameter ABI-tag and handle-policy
  tables.
- Negative constraint: do not rely solely on C3-level FFI packers to protect
  `csrc/ffi_helpers.c`. The C shim is a native ABI boundary and must validate
  pointer vectors and output-parameter state for direct helper, async, callback,
  test, and future AOT/native callers.
- Validation:
  - `cc -fsyntax-only -I csrc csrc/ffi_helpers.c`
  - `git diff --check -- csrc/ffi_helpers.c src/lisp/prim_ffi_callback.c3 src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface ./build/main --test-suite lisp` -> `pass=120 fail=0`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` -> `pass=290 fail=0`
  - `scripts/build_omni_chelpers.sh`
  - `scripts/check_file_size_gate.sh`
  - `scripts/check_status_consistency.sh`

## 2026-04-22 - Vulkan ML Optimizer Output Handle Guard Audit

- Hardened `ml_optimizer_vulkan_tensor_value` so Vulkan ML optimizer outputs
  reject null source metadata or null native device handles before creating a
  concrete Vulkan Tensor payload.
- Aligned the Vulkan optimizer wrapper with the existing CUDA wrapper contract:
  required optimizer outputs must have real backend storage, and optional
  velocity/state outputs are skipped by callers before wrapping.
- Simplified accepted output payload setup so `tensor_vulkan_device_finalizer`
  is assigned only after the native output handle has been proven non-null.
- Negative constraint: do not represent a successful non-empty Vulkan optimizer
  output as `TENSOR_PAYLOAD_CONCRETE` with `device_handle == null`. That masks a
  backend contract failure behind invalid Tensor metadata.
- Validation:
  - `git diff --check -- src/lisp/prim_ml_optimizer_vulkan.c3`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1993 fail=0`

## 2026-04-22 - Vulkan ML Loss Output Handle Guard Audit

- Hardened Vulkan `ml/mean-squared-error` and `ml/cross-entropy` wrappers so a
  successful native helper status with `out_device == null` fails closed before
  concrete Tensor metadata is returned.
- Required scalar loss outputs now only install `tensor_vulkan_device_finalizer`
  after Vulkan result storage has been proven non-null.
- Negative constraint: do not represent a successful Vulkan scalar loss result
  as `TENSOR_PAYLOAD_CONCRETE` with `device_handle == null`. These helpers
  reject empty element counts, so null required output storage after success is
  a backend contract failure.
- Validation:
  - `git diff --check -- src/lisp/prim_ml_vulkan_losses.c3`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1993 fail=0`
  - `scripts/check_file_size_gate.sh`
  - `scripts/check_status_consistency.sh`

## 2026-04-22 - Vulkan ML Attention Output Handle Guard Audit

- Hardened Vulkan `ml/scaled-dot-product-attention` so a successful native
  helper status with `out_device == null` fails closed before returning
  concrete Tensor metadata.
- The wrapper now frees the partial Tensor payload on that impossible-success
  path and assigns `tensor_vulkan_device_finalizer` only after result storage
  has been proven non-null.
- Negative constraint: do not blanket-rewrite conditional device finalizers in
  generic Tensor helpers without proving zero-sized output semantics. For ML
  attention, validation rejects zero query/key/head/value dimensions, so null
  required output storage after success is a backend contract failure.
- Validation:
  - `git diff --check -- src/lisp/prim_ml_attention.c3`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1993 fail=0`
  - `scripts/check_file_size_gate.sh`
  - `scripts/check_status_consistency.sh`

## 2026-04-22 - Vulkan ML Conv/Pool Output Handle Guard Audit

- Hardened Vulkan `ml/conv1d`, `ml/conv2d`, and `ml/max-pool2d` /
  `ml/avg-pool2d` wrappers so successful native helper statuses with
  `out_device == null` fail closed before concrete Tensor metadata is returned.
- The wrappers now free partial Tensor payloads on impossible-success null
  output paths and assign `tensor_vulkan_device_finalizer` only after result
  storage has been proven non-null.
- Negative constraint: do not apply the same rewrite to `prim_ml_reduction.c3`,
  `prim_ml_stable_reduction.c3`, or `prim_ml_normalization.c3` without a
  separate zero-element semantics review. Those surfaces can represent
  zero-sized output shapes differently from conv/pool required outputs.
- Validation:
  - `git diff --check -- src/lisp/prim_ml_conv.c3 src/lisp/prim_ml_conv2d.c3 src/lisp/prim_ml_pool2d.c3`
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1993 fail=0`
  - `scripts/check_file_size_gate.sh`
  - `scripts/check_status_consistency.sh`

## 2026-04-22 - CUDA Stateful Optimizer Fused Kernels

- Closed `ML-VK-060-FUSED-CUDA-STATEFUL-001` by replacing the remaining
  map-backed CUDA Adam, AdamW, and RMSProp dense row-major `Float32` optimizer
  leaf updates with native fused CUDA kernels when the optimizer PTX module is
  available.
- Added `csrc/tensor_cuda_ml_optimizer.cu` as the source of truth for optimizer
  PTX generation and regenerated `csrc/tensor_cuda_ml_optimizer_ptx.inc`.
  The module now resolves `omni_cuda_ml_sgd_f32`,
  `omni_cuda_ml_adam_f32`, and `omni_cuda_ml_rmsprop_f32` together.
- Added native C helper entry points for fused Adam/AdamW and RMSProp:
  `omni_tensor_backend_cuda_ml_adam_f32` and
  `omni_tensor_backend_cuda_ml_rmsprop_f32`. Both validate byte lengths,
  element-count overflow, finite hyperparameters, and required state handles
  before allocation or launch.
- Updated CUDA optimizer routing so Adam/AdamW produce updated parameters plus
  first- and second-moment state in one native launch, while RMSProp produces
  updated parameters, square-average state, and optional velocity state in one
  native launch.
- Updated `tensor-backends` so CUDA `ml-optimizer-adam-float32`,
  `ml-optimizer-adamw-float32`, and `ml-optimizer-rmsprop-float32` report true
  when either the native fused optimizer module or map fallback route is
  available.
- Preserved the negative fallback contract: if the native optimizer module is
  unavailable, CUDA can still use the older map-backed route; once the native
  module resolves, fused kernel allocation or launch failures fail closed as
  CUDA backend errors and are not masked by map fallback.
- Validation:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_ml_optimizer.cu -o /tmp/tensor_cuda_ml_optimizer.ptx`
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/tensor_cuda_ml_optimizer.ptx -o /tmp/tensor_cuda_ml_optimizer.o`
  - `cc -fsyntax-only -I csrc csrc/tensor_cuda_helpers.c`
  - `git diff --check -- csrc/tensor_cuda_ml_optimizer.cu csrc/tensor_cuda_ml_optimizer_ptx.inc csrc/tensor_cuda_helpers.c csrc/tensor_cuda_helpers_ml_optimizer.inc src/lisp/tensor_cuda_backend.c3 src/lisp/prim_ml_optimizer_cuda.c3 src/lisp/prim_tensor_backend_ops.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
  - `scripts/check_file_size_gate.sh`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - direct CUDA probes for optimizer capability, Adam, AdamW, and RMSProp
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=2000 fail=0`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=build:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1969 fail=0`

## 2026-04-22 - Vulkan Math Performance Probe Baseline

- Added `scripts/run_vulkan_math_perf_probe.sh` as a focused reproducible
  probe for the open `TENSOR-100F` Vulkan SVD/eigen performance residuals. The
  script emits `OMNI_BENCH_SUMMARY` lines and measures each operation with
  Omni `time-ms` inside the runtime process, avoiding process-startup skew.
- Added `docs/plans/vulkan-math-performance-measurements-2026-04-22.md` as the
  checked-in measurement artifact.
- Current host results do not justify a tiled/staged SVD/eigen rewrite or a
  blocked trailing-update LU rewrite for the measured boundary:
  - SVD: `Float64` identity 64 = 332 ms, `Float64` identity 65 = 320 ms,
    `Float64` all-ones 65 = 358 ms, `Float64` zero 65 = 325 ms,
    `Float32` identity 65 = 323 ms, `Float64` identity 96 = 333 ms,
    `Float64` identity 128 = 389 ms, `Float64` identity 192 = 616 ms, and
    `Float64` all-ones 128 = 559 ms.
  - Eigen: `Float64` identity 64 = 315 ms, `Float64` identity 65 = 336 ms,
    `Float64` all-ones 65 = 350 ms, `Complex64` zero 65 = 347 ms,
    `Float64` identity 128 = 323 ms, and `Float64` all-ones 128 = 482 ms.
  - Solve: `Float64` identity 65 = 323 ms, `Float64` `I + ones` 65 = 304 ms,
    `Float64` identity 128 = 303 ms, `Float64` `I + ones` 128 = 326 ms, and
    `Float64` identity 192 = 314 ms.
- Closed `TENSOR-100F-VK-SVD-LARGE-K-PERF-001` and
  `TENSOR-100F-VK-EIGEN-LARGE-N-PERF-001` as measured with no rewrite
  justified. Closed `TENSOR-100F-LU-BLOCKED-PERF-001` as measured with no
  blocked trailing-update LU rewrite justified.
- Validation:
  - `bash -n scripts/run_vulkan_math_perf_probe.sh`
  - `git diff --check -- scripts/run_vulkan_math_perf_probe.sh`
  - `scripts/run_vulkan_math_perf_probe.sh`
  - `OMNI_VULKAN_MATH_PERF_SCALE=1 scripts/run_vulkan_math_perf_probe.sh`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=2000 fail=0`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=build:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1969 fail=0`

## 2026-04-22 - CUDA Zero-Offset Transpose-View Binary Map

- Closed `TENSOR-100F-STRIDE-AWARE-HELPERS-001` for the first CUDA
  stride-aware helper boundary and closed `TENSOR-100F-CUDA-MAP-BROADEN-001`
  for the unsupported-layout/view residual now covered by that boundary.
- `matrix/transpose-view` now accepts CUDA-backed zero-offset dense row-major
  Tensor storage for `Float64`, `Float32`, `Complex128`, and `Complex64`,
  producing CUDA read-only strided view metadata that preserves the source
  device handle instead of materializing to CPU.
- CUDA binary map dispatch now preserves zero-offset strided CUDA Tensor
  operands directly through lazy realization and direct map execution. The
  native helper validates zero-offset storage spans and builds per-output
  operand offset tables when operand strides differ from dense output strides,
  so transpose-view operands read the correct device elements.
- Preserved fail-closed boundaries: raw CUDA view materialization to CPU,
  nonzero-offset/arbitrary views, mixed CPU/CUDA execution, and CUDA
  unary/scientific map over strided view operands are not shipped by this
  slice.
- Negative-memory update: do not skip CUDA binary map operand offset-table
  construction merely because operand and output shapes match. Matching shapes
  still require offset tables when operand strides differ from output strides;
  the prior same-shape shortcut read transpose views as dense row-major.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `git diff --check -- csrc/tensor_cuda_helpers_map_binary.inc src/lisp/prim_tensor_storage.c3 src/lisp/prim_tensor_matrix_lu_svd_core_b.c3 src/lisp/prim_tensor_map_backend_dispatch.c3 src/lisp/prim_tensor_backend_expr.c3 src/lisp/prim_tensor_copy_cpu.c3 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3`
  - `c3c build --obj-out obj`
  - direct CUDA probes for transpose-view metadata, scalar binary map,
    tensor/tensor binary map, chained lazy binary map, and unary fail-closed
    behavior
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=2004 fail=0`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=build:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` -> `pass=1973 fail=0`

## 2026-04-22 - TENSOR-100F Broad Validation Closure

- Closed `TENSOR-100F-BROAD-VALIDATION-001` and the `TENSOR-100F` parent after
  bounded-container global validation. Remaining backend-native non-Hermitian
  complex Vulkan eigenpairs work stays open as the promoted
  `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL` residual.
- The broad gate surfaced and drove two root-cause runtime repairs before
  closure:
  - `src/lisp/ast_arena.c3` now makes `ast_arena_alloc` fail closed when the
    current chunk is already corrupt (`used > capacity`) instead of allocating a
    new chunk and mutating allocator state after corruption.
  - `src/lisp/jit_compile_expr_core.c3` no longer applies the tail-position
    `List` / `Array` constructor shortcut to one-argument calls. One-argument
    constructor/conversion calls now route through the primitive path, preserving
    improper-list and malformed-iterator-tail validation, while zero-argument
    and variadic construction keep the fast path.
- `scripts/run_global_gates.sh` now records unsupported ASAN builds as an
  explicit skip when the C3 toolchain reports address sanitizer unsupported for
  the target. The script also avoids the earlier status-capture mistake where a
  failed ASAN build could fall through into normal-binary tests labeled as ASAN.
- Negative-memory update: do not treat the JIT tail `List` / `Array`
  constructor shortcut as semantics-preserving for one-argument calls; those are
  conversion calls and must preserve primitive validation.
- Negative-memory update: do not continue AST arena allocation after detecting
  a corrupt current chunk; adding a new chunk after `used > capacity` masks the
  allocator invariant violation.
- Classified audit diagnostic: the broad run prints boundary graph-audit
  diagnostic lines during `memory-lifetime-smoke` while reporting `fail=0`.
  `MEM-GRAPH-AUDIT-DIAGNOSTIC-20260422-001` is closed as expected output from
  the passing negative regression `lifetime: root splice debug audit rejects
  releasing temp edge`. The diagnostic is a deliberate child-scope ESCAPE
  `CONS` root reaching a child-scope TEMP `INT`, not the older lazy Tensor
  return repro and not a `TENSOR-100F` blocker.
- Validation:
  - `bash -n scripts/run_global_gates.sh`
  - `git diff --check -- scripts/run_global_gates.sh src/lisp/ast_arena.c3 src/lisp/jit_compile_expr_core.c3`
  - `c3c build --obj-out obj`
  - bounded-container `allocator-validation` -> `pass=1 fail=0`
  - bounded-container `advanced` -> `pass=3377 fail=0`
  - `scripts/run_validation_container.sh scripts/run_global_gates.sh` passed
    file-size gate, normal build, all configured normal lisp slices, compiler
    slice, and FTXUI smokes; ASAN build/tests were skipped explicitly because
    the current C3 toolchain reports address sanitizer unsupported for this
    target.
  - narrowed graph-audit classification probe showed the two diagnostic lines
    immediately followed by `[PASS] lifetime: root splice debug audit rejects
    releasing temp edge` and final `pass=237 fail=0`.

## 2026-04-22 - TENSOR-100H Vulkan General Eigen Design/ABI Checkpoint

- Advanced `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL` without flipping public
  capability bits or routing `matrix/eigenpairs` to Vulkan yet.
- Recorded the native solver boundary in
  `docs/plans/vulkan-eigensolver-plan-2026-04-17.md`: staged complex Hessenberg
  reduction plus implicit shifted QR, right-eigenvector back-accumulation,
  private helper status/scratch, public Vulkan `Complex128`/`Complex64`
  `values` and `vectors` outputs, and no hidden CPU/LAPACK fallback.
- Added fail-closed helper ABI declarations and implementation stubs for
  `omni_tensor_backend_vulkan_general_eigen_complex128` and
  `omni_tensor_backend_vulkan_general_eigen_complex64`. The stubs validate
  output pointers and shape/byte-length bounds, return success for empty
  matrices, and return `OMNI_TENSOR_VULKAN_UNSUPPORTED` for non-empty matrices
  until the shader solver lands.
- Added matching C3 extern declarations in
  `src/lisp/tensor_vulkan_backend_factorization.c3` and corrected reference
  docs so the remaining Vulkan blocker is the non-Hermitian solver, not stale
  high-precision output storage wording.
- Strengthened the existing Vulkan Hermitian fixed-width complex eigen tests
  with a no-LAPACK guard over the `zgeev` and `cgeev` counters, so the shipped
  native Jacobi path is pinned as backend-native while general `matrix/eigenpairs`
  remains fail-closed.
- Negative-memory update: do not implement this item with a 2x2 shortcut,
  realification, Hermitian Jacobi reuse, or hidden CPU/LAPACK fallback. The
  next implementation must target the shared Hessenberg/QR boundary or record
  a concrete blocker against that boundary.
- Validation:
  - `git diff --check -- csrc/tensor_vulkan_helpers_api_decls.h csrc/tensor_vulkan_helpers_matrix_ops_eigen_complex.c src/lisp/tensor_vulkan_backend_factorization.c3 docs/plans/vulkan-eigensolver-plan-2026-04-17.md docs/todo_parts/todo_part_01.md .agents/PLAN.md`
  - `cc -fsyntax-only -I csrc csrc/tensor_vulkan_helpers_matrix_ops_eigen_complex.c`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - bounded focused `advanced-collections-module` -> `pass=1975 fail=0`

## 2026-04-22 - TENSOR-100H Vulkan General Complex Eigenpairs

- Closed `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL` for Vulkan fixed-width
  complex `matrix/eigenpairs`.
- Added native serial shifted-QR shaders for `Complex128` and `Complex64`:
  `csrc/tensor_vulkan_general_eigen_complex128.comp` and
  `csrc/tensor_vulkan_general_eigen_complex64.comp`, with checked-in SPV
  wrappers/parts and helper build wiring.
- Replaced the fail-closed general eigen helper stubs with dispatch through
  `omni_tensor_backend_vulkan_general_eigen_complex128` and
  `omni_tensor_backend_vulkan_general_eigen_complex64`. The helper allocates
  public values, public vectors plus private scratch, and a private status
  buffer; failed convergence maps to `tensor/no-convergence` instead of falling
  back to CPU/LAPACK.
- Routed public Vulkan `Complex128`/`Complex64` `matrix/eigenpairs` before CPU
  resolution, returning Vulkan-placed fixed-width complex `values` and
  `vectors`. Real-valued Vulkan `Float64`/`Float32` `matrix/eigenpairs` remains
  fail-closed.
- `tensor-backends` now reports Vulkan `matrix-eigenpairs-complex128` according
  to Float64 support and `matrix-eigenpairs-complex64` according to Float32
  support. CUDA/cuBLAS general complex eigenpairs remain false.
- Strengthened tests from fail-closed assertions to native success,
  residual-check, and no-LAPACK-counter assertions for Vulkan fixed-width
  complex general eigenpairs.
- Decision update: the earlier staged Hessenberg/QR design remains the preferred
  future hardening/performance direction, but it is no longer the capability
  blocker. The shipped pre-alpha backend-native solver is serial shifted QR
  with explicit no-convergence status, not a 2x2 shortcut, realification,
  Hermitian reuse, or hidden CPU fallback.
- Validation:
  - `glslangValidator -V` and `spirv-val --target-env vulkan1.0` for both new
    shaders
  - `cc -fsyntax-only -I csrc csrc/tensor_vulkan_helpers_matrix_ops_eigen_complex.c`
  - `cc -fsyntax-only -I csrc csrc/tensor_vulkan_helpers_matrix_ops_status.c`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - direct Vulkan `Complex128` and `Complex64` triangular general eigenpair
    probes returned Vulkan values/vectors with expected sorted eigenvalues
  - bounded focused `advanced-collections-module` -> `pass=1977 fail=0`

## 2026-04-22 - Vulkan Eigen Follow-Up Risk TODOs

- Reopened the live TODO queue with two explicit follow-up items from the
  Vulkan general eigen handoff:
  - `TENSOR-100H-VK-REAL-GENERAL-EIGEN-001` tracks native Vulkan real-valued
    general `matrix/eigenpairs` for dense row-major `Float64` and `Float32`,
    replacing the current intentional fail-closed route only after residual and
    no-LAPACK-counter tests pass.
  - `TENSOR-100H-VK-COMPLEX-GENERAL-EIGEN-HARDENING-001` tracks
    measurement-gated hardening of the shipped Vulkan fixed-width complex
    serial shifted-QR eigensolver for larger or difficult spectra.
- Planning note: the fixed-width complex general eigen parent remains closed.
  Future complex work should use the hardening item and measurement gate rather
  than reopening the capability parent or adding another case-specific tweak.

## 2026-04-22 - Audit Remediation: FFI Callback Release + Vulkan Backward Status

- Closed `AUDIT-2026-FFI-CALLBACK-RELEASE-SEGFAULT` (critical).
  - `prim_ffi_callback.c3` stored `ctx.code_ptr` as the `FfiHandle` payload,
    but `ffi_callback_context_finalizer` expected `FfiCallbackContext*`.
    `foreign-release` passed the executable pointer to the finalizer,
    causing a segfault.
  - Added `void* user_data` field to `FfiHandle` (`value_runtime_types.c3`)
    so the finalizer receives the context pointer while `lib_handle` remains
    the public callable code pointer.
  - Updated `make_ffi_box`, `make_ffi_handle_ex`,
    `make_ffi_handle_ex_with_descriptor`, and `ffi_handle_release_payload`
    in `value_constructors.c3` to route `user_data` to the finalizer.
  - Removed the redundant `handle.ffi_val.lib_handle = ctx.code_ptr` line in
    `prim_ffi_callback.c3` and passed `ctx` as `user_data`.
  - Added regression test `ffi-callback foreign-release` in
    `tests_advanced_io_effect_ffi_ffi_surface_groups.c3`.
  - Validation: `c3c build`; repro returns `#<void>`; targeted
    `advanced-ffi-system` group passes.

- Closed `AUDIT-2026-VULKAN-BWD-STATUS-CONTRADICTION` (high).
  - Spec `LANGUAGE_SPEC.part-01b.md:394` claimed all CUDA/Vulkan backward
    fail-closed, but TODO records shipped Vulkan backward for MSE, map
    expressions, broadcast map, and softmax-CE.
  - Updated spec to enumerate shipped Vulkan backward operations and keep
    only unsupported configurations fail-closed.
  - Replaced stale blanket rejection messages
    (`"Vulkan backward kernels are not implemented"`) with qualified messages
    (`"Vulkan backward is not supported for this expression configuration"` and
    `"Vulkan backward is not supported for this operation"`) in
    `prim_ml_autograd_tensor_expr.c3` and `prim_ml_autograd.c3`.
  - Validation: `c3c build`.

- Added TODO items for remaining 2026-04-22 audit findings:
  - `AUDIT-2026-ERROR-MODEL-MIGRATION` (1013 raw `raise_error` sites)
  - `AUDIT-2026-ML-VISUALIZATION-GAP`
  - `AUDIT-2026-VALIDATION-MASKING`
  - `AUDIT-2026-CSTRING-SCANNING`
  - `AUDIT-2026-TAGGED-SWITCH`

## 2026-04-22 - Error Model Migration Wave 1

- Advanced `AUDIT-2026-ERROR-MODEL-MIGRATION` (high severity).
- Converted ~149 raw `raise_error` calls to `raise_error_with_payload_names` across
  8 files, using domain-specific helpers (`ffi_raise`, `http_raise`, `json_raise`,
  `collection_raise`).
- Files converted:
  - `src/lisp/prim_ffi_callback.c3`: 19 calls → `ffi_raise` helper + regression test
  - `src/lisp/foreign_runtime_core.c3`: 29 calls → `ffi_raise` helper
  - `src/lisp/http.c3`: 10 calls → `http_raise` helper
  - `src/lisp/json.c3`: 9 calls → `json_raise` helper
  - `src/lisp/prim_collection_generic_set.c3`: 20+ calls → `collection_raise` helper
  - `src/lisp/eval_ffi_eval.c3`: 24 calls → reuses `ffi_raise` helper
  - `src/lisp/eval_ffi_bound_call.c3`: 38 calls → reuses `ffi_raise` helper
  - `src/lisp/json_emit.c3`: 13 calls → reuses `json_raise` helper
  - `src/lisp/prim_ui_ftxui_helpers.c3`: 13 calls → `ftxui_raise` helper
  - `src/lisp/prim_ui_ftxui.c3`: 66 calls → `ftxui_raise` helper
- Fixed 13 test regressions in `src/lisp/tests_core_groups.c3` caused by the
  structured payload contract change: tests using `(String e)` now extract the
  message via `(ref e 'message)` before string conversion.
- Fixed pre-existing build breaks: added missing Vulkan ML f64 SPIR-V files to
  `project.json` (`sgd_f64`, `sgd_init_momentum_f64`, `sgd_momentum_f64`,
  `adam_f64`, `adam_init_f64`, `rmsprop_f64`, `layer_norm_f64`, `batch_norm_f64`).
- Validation:
  - `c3c build` passes
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite all`: 166 passed, 0 failed
  - `scripts/check_file_size_gate.sh`
- Residuals:
  - ~878 raw `raise_error` calls remain across `src/lisp`; next suggested targets:
    `prim_math_core.c3` (40 calls), `eval_promotion_copy_route_helpers.c3` (37 calls),
    `eval_promotion_escape_structured.c3` (32 calls).

## 2026-04-22 - Error Model Migration Completion

- Closed `AUDIT-2026-ERROR-MODEL-MIGRATION`.
- Converted all remaining ~878 raw `raise_error` calls to
  `raise_error_with_payload_names` across ~40 files in `src/lisp`.
- Domain-specific `@inline` helpers established and reused:
  - `boundary_raise` in `eval_promotion_copy_route_helpers.c3`
  - `jit_raise` in `jit_apply_helpers.c3`
  - `string_raise` in `unicode.c3`
  - `data_format_raise` in `primitives_data_formats.c3`
  - `runtime_raise` in `threads.c3`
  - `tensor_raise` in `prim_tensor_rounding_math.c3`
  - `big_float_raise` in `value_big_float.c3`
  - `big_complex_raise` in `value_big_complex.c3`
  - `big_integer_raise` in `value_big_integer.c3`
  - `math_raise` in `prim_math_core.c3`
  - Existing helpers reused: `io_raise`, `collection_raise`, `ftxui_raise`,
    `ffi_raise`, `json_raise`, `http_raise`, `tensor_runtime_raise`.
- Fixed cross-module `aot_runtime_bridge*.c3` calls to use
  `lisp::raise_error_with_payload_names`.
- Fixed multi-line `data_format_raise` calls in `json_pointer_options.c3`.
- Fixed char[]-to-String cast issues in boundary helpers.
- Fixed pre-existing `left_path`/`right_path` use-before-declare bug in
  `prim_ml_autograd_tensor_expr.c3:227`.
- Original `raise_error` function definition preserved as compatibility wrapper
  in `value_constructors.c3:560`.
- Total migration: ~1013 raw `raise_error` call sites converted.
- Updated `docs/ERROR_MODEL.md` status line and migration matrix.
- Validation:
  - `c3c build` passes
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite all`: 166 passed, 0 failed
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`

## 2026-04-22 - C-String Scanning Boundedness Audit Closure

- Closed `AUDIT-2026-CSTRING-SCANNING`.
- Added inline caps to previously unbounded C string scans around host/runtime
  boundaries:
  - `src/entry_cli_helpers.c3:15` — `cstr_len` capped at 65536.
  - `src/entry_bind_paths.c3:52` — `bind_dep_name_is_safe_output_name` capped at 256.
  - `src/entry_bind_paths.c3:71` — `build_bind_module_path` dep_name scan capped at 256.
  - `src/entry_bind_dep_generation.c3:371` — dep_name/dep_lib length scans capped at
    256 and 512.
  - `src/main_repl_shared.c3:21` — `repl_shared_cstr_len` capped at 4096.
- Reference pattern preserved: `repl_cstr_len_bounded` in
  `src/entry_runtime_project_paths.c3:9`.
- Validation: `c3c build`; basic tests pass.

## 2026-04-22 - Tagged Switch Exhaustiveness Audit Closure

- Closed `AUDIT-2026-TAGGED-SWITCH`.
- Replaced `default:` fallbacks with explicit enum cases where the switch was
  already exhaustive:
  - `src/lisp/value_print.c3` — added `case NIL:` return, removed default.
  - `src/lisp/eval_promotion_copy.c3` — added explicit non-leaf
    `CopyParentRoute` cases to `copy_parent_route_is_leaf`, removed default.
  - `src/lisp/prim_tensor_storage.c3` — added `case TENSOR_PAYLOAD_CONCRETE:`,
    removed default.
  - `src/lisp/foreign_runtime_core.c3` — removed defaults from
    `foreign_handle_policy_ownership_name`,
    `foreign_handle_policy_nullability_name`, and `foreign_ffi_abi_type_name`.
- Validation: `c3c build`; basic tests pass.

## 2026-04-22 - Validation Harness Diagnostic Masking Audit Closure

- Closed `AUDIT-2026-VALIDATION-MASKING`.
- Gated `io::eprintfn` diagnostic output in
  `boundary_debug_graph_audit_pre_splice_escape_root` and
  `boundary_debug_graph_audit_committed_escape_root` behind
  `boundary_verbose_telemetry_enabled()`.
- The `[boundary][graph-audit] pre-splice escape root reachability violation`
  diagnostic emitted during `memory-lifetime-smoke` was already expected output
  from the passing negative regression
  `lifetime: root splice debug audit rejects releasing temp edge`.
- Normal test runs no longer print boundary graph-audit diagnostics to stderr.
- Validation: `c3c build`; basic tests pass.

## 2026-04-22 - ML Visualization Surface Gap Audit Closure

- Closed `AUDIT-2026-ML-VISUALIZATION-GAP` as a design decision.
- Added `docs/plans/ml-visualization-surface-decision-2026-04-22.md` with:
  - Canonical primitive names: `ml/plot`, `ml/loss-curve`,
    `ml/confusion-matrix`, `ml/tensor-summary`, `ml/export-image`.
  - Backend contracts: declarative layer, FTXUI terminal default, no hidden
    CPU fallback, options dictionary shape.
  - Scope boundaries: image rasterization, pixel buffer ABI, file-format
    encoders deferred.
  - Rejected primitives: `ml/feature-map`, `ml/animate-training`,
    `ml/interactive-plot`.
- Deferred implementation backlog: `ML-VIZ-001` through `ML-VIZ-004`.

## 2026-04-22 - Vulkan Min/Max Map Backward Closure

- Closed `ML-VK-050-VK-BWD-MINMAX-001`.
- CPU Tensor-expression `ml/grad` now supports `map min` and `map max` backward
  rules for `tensor-mean-squared-error` by accumulating comparison-mask
  derivatives into the selected operand.
- Vulkan dense row-major `Float32` and `Float64` MSE map backward now supports
  `min` and `max` by reusing the existing binary map comparison mask ops
  (`step(right, left)` and `step(left, right)`) and multiplying the upstream
  gradient on device.
- Fixed scalar-side Vulkan min/max provenance by materializing the recorded
  scalar value before launching the comparison mask helper.
- Validation:
  - `c3c build --obj-out obj` passes.
  - Direct CPU `map max` MSE gradient eval returns `true`.
  - Direct Vulkan `Float32` `map min` MSE gradient eval returns `true`.
  - Direct Vulkan `Float64` `map max` MSE gradient eval returns `true`.
  - Focused `advanced-collections-module` rerun improved from
    `pass=1994 fail=22` to `pass=1996 fail=20`; remaining failures are
    pre-existing Vulkan contract expectation drift outside this slice.

## 2026-04-22 - Vulkan General Eigenpair Real Route and Exact-Shift Hardening

- Closed `TENSOR-100H-VK-REAL-GENERAL-EIGEN-001`.
- Closed `TENSOR-100H-VK-COMPLEX-GENERAL-EIGEN-HARDENING-001` and promoted the
  separately discovered active-submatrix residual to
  `TENSOR-100H-VK-GENERAL-EIGEN-DEFLATION-001`.
- Real-valued Vulkan general `matrix/eigenpairs` now routes `Float64` and
  `Float32` tensors through native checked-in real-input shaders and returns
  Vulkan-placed `Complex128`/`Complex64` `values` and `vectors` tensors.
- Added build/API integration for the real and complex general eigen SPIR-V
  objects in `project.json` and `csrc/tensor_vulkan_helpers_api_decls.h`.
- Hardened all four native general eigen shaders
  (`Float64`, `Float32`, `Complex128`, `Complex64`) for exact 2x2 complex-shift
  spectra by adding direct analytic 2x2 eigenpair handling and QR shift
  regularization.
- Replaced stale fail-closed Vulkan real eigenpair tests with positive
  diagonal, lazy-input, exact complex-shift residual, device-placement, dtype,
  and no-LAPACK-counter coverage.
- Extended `scripts/run_vulkan_math_perf_probe.sh` with named real/complex
  general eigenpair fixtures:
  - `f64_exact_complex_shift_2`,
  - `f32_exact_complex_shift_2`,
  - `c128_triangular_3`,
  - `c64_exact_complex_shift_2`,
  - `f64_mixed_block_deflation_3`,
  - `f32_mixed_block_deflation_3`,
  - `c128_mixed_block_deflation_3`,
  - `c64_mixed_block_deflation_3`.
- Closed `TENSOR-100H-VK-GENERAL-EIGEN-DEFLATION-001`.
- Added active-submatrix deflation before the full-matrix QR loop: negligible
  trailing subdiagonal couplings are deflated as scalar blocks, the active
  leading 2x2 block is solved analytically through the accumulated basis, and
  reconstructed leading output columns are staged in scratch before copying
  back so basis columns are not overwritten mid-reconstruction.
- `[INVALIDATED]` Do not treat the old real-valued Vulkan fail-closed tests as
  authoritative; the native real route now exists and is validated.
- `[INVALIDATED]` Widening QR shift perturbation was not the right fix for
  mixed-block 3x3 spectra; active-submatrix deflation is the shipped closure.
- `[INVALIDATED]` Reconstructing leading 2x2 vectors directly into the public
  output/accumulated-basis buffer corrupts later columns; stage reconstructed
  vectors in scratch and copy them back only after both leading columns are
  complete.
- Reconciled the remaining focused advanced ML/Vulkan contract drift after the
  eigen work:
  - Vulkan `ml-layer-normalization-float64` capability now reflects the
    implemented native Float64 layer-normalization helper.
  - Mixed CPU/Vulkan `ml/mean-squared-error`, `ml/cross-entropy`,
    `ml/conv1d`, `ml/conv2d`, `ml/sgd-step`, `ml/clip-gradients`, and
    Vulkan optimizer tests now assert CPU-to-Vulkan migration with
    Vulkan-placed outputs instead of stale fail-closed expectations.
- Validation:
  - `glslangValidator -V` and `spirv-val` passed while regenerating the four
    general eigen SPIR-V artifacts.
  - `scripts/build_omni_chelpers.sh` passes.
  - `c3c build --obj-out obj` passes.
  - Direct Vulkan `Float64`, `Float32`, `Complex128`, and `Complex64` exact
    2x2 complex-shift residual probes return `true`.
  - `scripts/run_vulkan_math_perf_probe.sh` passes and reports the new general
    eigenpair fixtures as available with Vulkan output placement.
  - Direct mixed-block deflation residual probes for Vulkan `Float64`,
    `Float32`, `Complex128`, and `Complex64` return `true`.
  - Focused `advanced-collections-module` reports `pass=2025 fail=0`; the
    eigenpair failures and stale ML/Vulkan mixed-migration contract failures
    are cleared.
  - Bounded container focused `advanced-collections-module` reports
    `pass=1994 fail=0`.

## 2026-04-23 - Raise Payload Fault Propagation and Global Gate Closure

- Fixed the bounded global-gate `memory-lifetime-smoke` regression where
  handled raise payload string materialization under forced string-allocation
  failure returned the generic `raise: failed to construct payload` error.
  Canonical raise payload builders now optionally surface the underlying
  ERROR-valued message/data allocation failure to handled raise dispatch.
- Kept the direct `make_raise_payload_names` helper contract unchanged: direct
  helper failures still return `null` for payload-map and mid-build allocation
  failures.
- Added an explicit payloadless fallback raise helper and routed only no-data
  deduce raises through it. This preserves generic/JIT strict fail-closed
  behavior for canonical payload-map construction failure while keeping deduce's
  documented payload-less degradation for rich-payload OOM cases.
- Updated FTXUI smoke examples to extract handled raise messages from the
  structured payload dictionary when present, with raw string fallback retained.
- Validation:
  - `c3c build --obj-out obj` passes.
  - Bounded `memory-lifetime-smoke` reports `pass=237 fail=0`.
  - Bounded `deduce` reports `pass=392 fail=0`.
  - Bounded `jit-policy` reports `pass=52 fail=0`.
  - Bounded `scripts/run_ftxui_smoke.sh` passes all configured smoke programs.
  - Bounded `scripts/run_global_gates.sh` passes file-size gate, normal build,
    all configured normal lisp slices, compiler slice, and FTXUI smokes; ASAN
    is explicitly skipped because the current C3 toolchain reports address
    sanitizer unsupported for this target.
- `[INVALIDATED]` Do not treat every canonical raise payload construction miss
  as the same failure mode. ERROR-valued message/data materialization must
  propagate the original ERROR; selected subsystem payload-map OOM may degrade
  to payload-less dispatch only through an explicit fallback helper.

## 2026-04-23 - ML FTXUI Plotting Surface Closure

- Closed `ML-VIZ-001`.
- Added `ml/plot(data [options])` and `ml/loss-curve(losses [options])` as
  declarative ML visualization primitives over the existing FTXUI `graph` node
  contract.
- Runtime behavior:
  - both primitives return ordinary dictionaries with `kind 'graph`,
    `props.series`, `props.visualization`, `props.backend 'ftxui`, `props.title`,
    and `children nil`;
  - `ml/plot` accepts finite scalar Array/List series, x/y pair Array/List
    series, rank-1 CPU Float64/Float32 tensors, and rank-2 `[N 2]` CPU
    Float64/Float32 tensors, preserving x-values in `props.x`;
  - `ml/loss-curve` accepts finite scalar Array/List series and rank-1 CPU
    Float64/Float32 tensors;
  - unsupported options backends fail closed with `ml/backend-unsupported`;
    malformed data fails with `ml/invalid-visualization`; unsupported tensor
    dtype/device/layout paths fail closed instead of falling back silently.
- Wiring:
  - runtime primitive table,
  - AOT primitive lookup map,
  - runtime source manifest,
  - compiler codegen tests,
  - focused advanced runtime tests,
  - primitive/reference docs,
  - ML visualization plan/TODO state.
- Remaining explicit backlog:
  - `ML-VIZ-002` `ml/tensor-summary`,
  - `ML-VIZ-003` `ml/confusion-matrix`,
  - `ML-VIZ-004` export-only image backend decision/implementation.
- Validation:
  - `c3c build --obj-out obj` passes.
  - Compiler slice passes with `pass=292 fail=0`.
  - Focused `advanced-collections-module` passes with `pass=2030 fail=0`.
  - `git diff --check`, `scripts/check_primitive_docs_parity.sh`,
    `scripts/check_status_consistency.sh`, and `scripts/check_file_size_gate.sh`
    pass.
- `[INVALIDATED]` The ML visualization plan's old statement that no ML-facing
  visualization primitives exist is historical only; `ml/plot` and
  `ml/loss-curve` now exist.
- `[INVALIDATED]` Do not add gnuplot as the primary ML plotting backend; the
  shipped path is FTXUI graph-node data. Image tooling remains an export-only
  decision, not the terminal plotting plan.

## 2026-04-23 - ML Tensor Summary Surface Closure

- Closed `ML-VIZ-002`.
- Added `ml/tensor-summary(tensor [options])` as ordinary ML visualization
  summary data for any Tensor.
- Runtime behavior:
  - returns `kind 'tensor-summary`;
  - reports dtype, device, payload kind, layout, dense-row-major flag, shape,
    strides, rank, element-count, byte-length, backend, and title;
  - reports `stats.status 'available` with finite min/max/mean/count for direct
    concrete CPU Float64/Float32 tensors;
  - reports explicit unavailable stats reasons for unsupported dtype,
    non-CPU-device, non-concrete payload, missing storage, or numeric overflow;
  - does not perform hidden device-to-CPU fallback for stats.
- Wiring:
  - runtime primitive table,
  - AOT primitive lookup map,
  - compiler codegen tests,
  - focused advanced runtime tests,
  - primitive/reference docs,
  - ML visualization plan/TODO state.
- Remaining explicit backlog:
  - `ML-VIZ-003` `ml/confusion-matrix`,
  - `ML-VIZ-004` export-only image backend decision/implementation.
- Validation:
  - `c3c build --obj-out obj` passes.
  - Compiler slice passes with `pass=293 fail=0`.
  - Focused `advanced-collections-module` passes with `pass=2032 fail=0`.

## 2026-04-23 - ML Confusion Matrix Surface Closure

- Closed `ML-VIZ-003`.
- Added `ml/confusion-matrix(predictions targets [options])` as ordinary ML
  visualization confusion-matrix data.
- Runtime behavior:
  - accepts equal-length integer class-id Array/List labels;
  - accepts equal-length rank-1 concrete CPU Float64/Float32 Tensor labels;
  - rejects empty inputs, unequal lengths, non-integer labels, negative labels,
    unsupported tensor dtypes, non-CPU tensor labels, and unsupported backends;
  - returns `kind 'confusion-matrix`, `labels`, row-major `matrix`, `total`,
    `correct`, `accuracy`, `orientation
    'target-rows-predicted-columns`, `backend`, and `title`;
  - does not perform hidden device-to-CPU fallback for tensor labels.
- Wiring:
  - runtime primitive table,
  - AOT primitive lookup map,
  - compiler codegen tests,
  - focused advanced runtime tests,
  - primitive/reference docs,
  - ML visualization plan/TODO state.
- Remaining explicit backlog:
  - `ML-VIZ-004` export-only image backend decision/implementation.
- Validation:
  - `c3c build --obj-out obj` passes.
  - Compiler slice passes with `pass=294 fail=0`.
  - Focused `advanced-collections-module` passes with `pass=2035 fail=0`.
- `[INVALIDATED]` `ML-VIZ-003` is no longer deferred; the shipped label domain
  is non-negative integer class IDs rather than arbitrary hashable labels.

## 2026-04-23 - ML Image Export Surface Closure

- Closed `ML-VIZ-004`.
- Added `ml/export-image(tensor path [options])` as dependency-free PPM image
  export for ML visualization.
- Runtime behavior:
  - accepts rank-2 grayscale CPU Float64/Float32 tensor images;
  - accepts rank-3 HWC CPU Float64/Float32 tensor images with 1 or 3 channels;
  - default `'range 'auto` scales `0..1` data as unit range and writes values
    above `1` up to `255` as byte range;
  - explicit `'range` may be `'auto`, `'unit`, or `'byte`;
  - explicit `'format` may only be `'ppm`;
  - explicit `'backend` may be `'image` or `'ppm`;
  - rejects empty dimensions, unsupported channel counts, non-finite values,
    negative values, out-of-range values, unsupported dtypes, non-CPU tensors,
    and unsupported formats/backends;
  - writes a plain PPM `P3` file and returns ordinary `kind 'image-export`
    metadata with path, format, backend, range, width, height, and channels;
  - does not auto-display and does not perform hidden device-to-CPU fallback.
- Dependency decision:
  - chose an in-repo PPM writer to close export-only support without adding
    ImageMagick, gnuplot, shell-out tooling, PNG/JPEG encoders, or a new image
    dependency.
- Wiring:
  - runtime primitive table,
  - AOT primitive lookup map,
  - compiler codegen tests,
  - focused advanced runtime tests,
  - primitive/reference docs,
  - ML visualization plan/TODO state.
- Remaining explicit backlog:
  - none for the ML visualization surface decision.
- Validation:
  - `c3c build --obj-out obj` passes.
  - Compiler slice passes with `pass=295 fail=0`.
  - Focused `advanced-collections-module` passes with `pass=2038 fail=0`.
  - Direct PPM smoke writes `P3`, dimensions, max value, and RGB pixel rows.
- `[INVALIDATED]` `ML-VIZ-004` is no longer deferred; do not treat image export
  as requiring ImageMagick or another external dependency for the shipped
  baseline.

## 2026-04-23 - ML Typed Plot Family Expansion

- Closed `ML-VIZ-005`.
- Extended the ML visualization surface beyond the initial line/loss and
  export slices without adding PNG/JPEG, image display, ImageMagick, gnuplot,
  or shell-out dependencies.
- Runtime behavior:
  - `ml/plot` now records `props.plot-kind 'line`, `'xy`, or `'multi-series`
    and accepts Array/List multi-series overlays with `props.series-list`;
  - `ml/scatter(points [options])` accepts x/y pair Array/List data or rank-2
    `[N 2]` CPU Float64/Float32 tensors and records `props.points` plus
    marker metadata;
  - `ml/bar-chart(values [options])` accepts finite scalar series and records
    `props.plot-kind 'bar-chart` plus optional labels;
  - `ml/histogram(values [options])` accepts finite scalar series and returns
    histogram counts in `props.series` plus `props.bin-edges` and
    `props.sample-count`;
  - `ml/heatmap(matrix [options])` accepts finite rectangular matrix data or
    rank-2 CPU Float64/Float32 tensors and returns ordinary matrix metadata,
    not an image/display primitive;
  - `ml/roc-curve(points [options])` and `ml/pr-curve(points [options])`
    provide named graph-compatible curve primitives over supplied x/y points;
  - graph-family primitives retain existing FTXUI `props.series`
    compatibility and support non-interactive `zoom` data-window metadata,
    applying x-window filtering where the input has a direct index/x domain.
- Wiring:
  - new split source owner `src/lisp/prim_ml_plot_extensions.c3`;
  - runtime primitive table,
  - AOT primitive lookup map,
  - compiler codegen tests,
  - focused advanced runtime tests,
  - primitive/reference docs,
  - ML visualization plan/TODO state.
- Validation:
  - `c3c build --obj-out obj` passes.
  - Compiler slice passes with `pass=301 fail=0`.
  - Focused `advanced-collections-module` passes with `pass=2045 fail=0`.
- `[INVALIDATED]` The earlier `ML-VIZ-005` limitation that typed plot metadata
  was preserved only for future richer renderers is superseded by `ML-VIZ-006`;
  typed ML plot nodes now lower through a native FTXUI canvas plot shim.

## 2026-04-23 - Native FTXUI Typed Plot Rendering

- Closed `ML-VIZ-006`.
- Added native FTXUI canvas rendering for typed ML plot metadata without adding
  PNG/JPEG, image display, ImageMagick, gnuplot, shell-out tooling, or a new
  dependency.
- Runtime behavior:
  - untyped `ui.graph` keeps the existing FTXUI graph callback path;
  - graph nodes with `props.plot-kind` lower through `omni_ftxui_element_plot`;
  - `ml/plot` multi-series overlays render as multiple colored canvas lines;
  - `ml/scatter` renders x/y points with point/cross/plus marker shapes;
  - `ml/bar-chart` and `ml/histogram` render vertical bars;
  - `ml/roc-curve` and `ml/pr-curve` render supplied x/y curve data;
  - `ml/heatmap` lowers ordinary finite matrix data to a colored FTXUI canvas
    heatmap and still does not use image display.
- Wiring:
  - `csrc/ftxui_shim.h`,
  - `csrc/ftxui_shim.cpp`,
  - `csrc/ftxui_shim_element.inc`,
  - `src/lisp/ftxui_ffi_constants.c3`,
  - `src/lisp/ftxui_ffi_types.c3`,
  - `src/lisp/ftxui_ffi.c3`,
  - new `src/lisp/prim_ui_ftxui_plot_lowering.c3`,
  - `src/lisp/prim_ui_ftxui_lowering.c3`,
  - runtime manifest,
  - FTXUI FFI surface tests,
  - primitive/reference docs,
  - ML visualization plan/TODO state.
- Validation:
  - `scripts/build_omni_chelpers.sh` passes.
  - `c3c build --obj-out obj` passes.
  - `scripts/run_ftxui_smoke.sh` passes.
  - Direct typed plot `__ui-ftxui-run` smokes for multi-series, scatter,
    bar chart, and heatmap return `true`.
  - Focused `advanced-ffi-system-surface` passes with `pass=123 fail=0`.
  - Focused `advanced-collections-module` passes with `pass=2045 fail=0`.

## 2026-04-23 - Slash Primitive Naming Policy

- Documented the owner decision that slash-qualified names are canonical
  single symbols for always-present core primitive families, not implicit
  module dereferences.
- Updated the language token section, module section, primitive reference, and
  agent naming policy to state that names such as `io/println`,
  `matrix/eigenpairs`, `tensor/run`, `ml/plot`, `nn/apply`, and `ui/...`
  remain ordinary primitive symbols.
- Clarified that module/value access remains the dotted/path surface
  (`mod.sym`) plus explicit `module` / `import` / `export-from` forms.
- Added the ergonomics constraint that slash prefixes should help human
  developers scan dense primitive families, not become mandatory hierarchy
  syntax. Generic cross-cutting operations should remain unprefixed, distinct
  one-off operations should stay short when a prefix adds no meaning, deep or
  mechanically long slash pseudo-paths are discouraged, and optional or
  independently versioned surfaces should use real modules/imports.
- No runtime behavior changed.
- Validation:
  - `git diff --check` passes.
  - `scripts/check_primitive_docs_parity.sh` passes.
  - `scripts/check_status_consistency.sh` passes with TODO actionable count 0.
  - `scripts/check_file_size_gate.sh` passes.

## 2026-04-23 - Slash Surface Naming Audit Plan

- Added `docs/plans/slash-surface-naming-audit-plan-2026-04-23.md`.
- Recorded the owner decision that Pika is language-core and Omni should be
  built around Pika rather than treating `pika/...` as an optional helper
  module.
- Recorded the preferred Deduce direction: the broad `deduce/...` surface
  should move toward a real module/facade boundary rather than continuing as a
  growing primitive-family namespace by default.
- Recorded the ML question as an explicit decision item because `ml/...` is
  broad and likely to grow; current shipped names should be classified before
  adding more names under that prefix.
- Opened TODO-backed work items:
  - `SURFACE-NAMING-001` document Pika as language-core;
  - `SURFACE-NAMING-002` produce the Deduce module-boundary decision;
  - `SURFACE-NAMING-003` decide the ML module split;
  - `SURFACE-NAMING-004` flatten or internalize `ml/linear-batched-reduce`;
  - `SURFACE-NAMING-005` resolve special-function math naming;
  - `SURFACE-NAMING-006` tighten ML/NN activation docs.
- No runtime behavior changed in this planning pass.
- Validation:
  - `git diff --check` passes.
  - `scripts/check_status_consistency.sh` passes with TODO actionable count 6.
  - TODO/plan ID sweep confirms all six `SURFACE-NAMING-*` IDs are present in
    both the plan and TODO part.

## 2026-04-23 - Slash Surface Naming Decisions and Linear Rename

- Integrated the parallel naming audit follow-ups:
  - `SURFACE-NAMING-001` closed with Pika documented as Omni language-core and
    parser/grammar substrate.
  - `SURFACE-NAMING-002` closed with
    `docs/plans/deduce-module-boundary-decision-2026-04-23.md`; the accepted
    direction moves broad `deduce/...` callable aliases toward an imported
    `deduce` facade and keeps the unified dispatcher only as planned
    `deduce.dispatch`.
  - `SURFACE-NAMING-003` closed with
    `docs/plans/ml-module-surface-split-decision-2026-04-23.md`; compact
    tensor/learning kernels stay under `ml/...`, while visualization and
    optimizer/checkpoint helpers move toward `ml.visualization.*` and
    `ml.optimizers.*` facades in a later implementation slice.
  - `SURFACE-NAMING-006` closed by tightening ML/NN activation docs so
    `ml/<activation>` means eager Tensor activation and `nn/<activation>`
    means layer-spec constructor.
- Renamed the public batched linear primitive from the old deep pseudo-path
  spelling to canonical `ml/linear-batched-reduce`:
  - runtime registration now exposes `ml/linear-batched-reduce`;
  - AOT primitive lookup now resolves `ml/linear-batched-reduce`;
  - advanced runtime tests, diagnostics, language docs, primitive reference
    docs, TODO/plans, and handoff artifacts use the new spelling;
  - the old `ml/linear/batched-reduce` spelling has no compatibility alias.
- Remaining open naming item:
  - `SURFACE-NAMING-005` still needs the special-function math naming decision
    for `math/lgamma`, `math/erf`, and `math/erfc`.
- Validation:
  - `c3c build --obj-out obj` passes.
  - Focused advanced collections-module slice passes with `pass=2045 fail=0`.
  - Focused compiler slice passes with `pass=301 fail=0`; expected bindgen
    negative-test diagnostics are printed by that slice.

## 2026-04-23 - Vulkan Float32 Math Erf/Erfc Extension

- Implemented `MATHSTATS-VK-001` for the dense row-major Vulkan `Float32`
  semantic boundary.
- `csrc/tensor_vulkan_map_unary_f32.comp` now exposes `math.erf` and
  `math.erfc` through unary op ids `17` and `18`, sharing the same
  Abramowitz/Stegun-style approximation family used by `stats.normal-cdf`.
- Regenerated `csrc/tensor_vulkan_map_unary_f32_spv.c` from a
  `glslangValidator`-compiled and `spirv-val`-validated shader.
- `csrc/tensor_vulkan_helpers.c` and
  `csrc/tensor_vulkan_helpers_ml_clip.c` now allow Float32 unary op ids through
  `19`, so direct Tensor `map`, direct Tensor unary math, checked kernel
  sources, and ML clip helper paths agree on the supported op range.
- `src/lisp/prim_tensor_map_callable_ops.c3` maps `math.erf` and
  `math.erfc` module primitives to Vulkan op ids `17` and `18`; direct Tensor
  unary math uses the same op ids. Vulkan `Float64` `math.erf` / `math.erfc`
  still fail closed pending a double approximation policy.
- `src/lisp/prim_kernel_unary.c3` exposes checked `kernel/run` operation names
  `erf-f32` and `erfc-f32` for the same Vulkan Float32 unary helper.
- Advanced tests now cover public `map math.erf`, `map math.erfc`, direct
  `math.erf`, direct `math.erfc`, Float64 fail-closed behavior, and
  `kernel/run` `erf-f32` / `erfc-f32`.
- While rebuilding helpers, two pre-existing dirty compile breaks were exposed
  and fixed:
  - `csrc/tensor_cuda_helpers.c` had mutex/goto resolution changes with
    `omni_tensor_cublas_resolve` still missing its `done:` label.
  - `csrc/tensor_vulkan_helpers_runtime_decls.h` had redundant forward
    typedefs that conflicted with concrete Vulkan struct typedefs from
    `tensor_vulkan_helpers_internal.h`.
- Validation:
  - `glslangValidator -V csrc/tensor_vulkan_map_unary_f32.comp` passes.
  - `spirv-val /tmp/tensor_vulkan_map_unary_f32.spv` passes.
  - `scripts/build_omni_chelpers.sh` passes.
  - `c3c build --obj-out obj` passes.
  - Direct Vulkan smokes for `map math.erf`, direct `math.erfc`, and
    fail-closed Vulkan `Float64` `math.erf` pass.
  - Focused advanced collections-module slice passes with `pass=2060 fail=0`.
  - Focused compiler slice passes with `pass=301 fail=0`; expected bindgen
    negative-test diagnostics are printed by that slice.
  - `scripts/check_primitive_docs_parity.sh` passes.
  - `scripts/check_file_size_gate.sh` passes.
- `scripts/check_status_consistency.sh` passes.
- `scripts/check_e2e_baseline_policy.sh` passes.
- `git diff --check` passes.

## 2026-04-23 - Vulkan Float64 Unary Bound Fix

- Fixed the mismatch between the public `stats.normal-quantile` mapping and
  the Vulkan `Float64` helper whitelist.
- `omni_tensor_backend_vulkan_map_unary_f64` now accepts op `20` as well as
  op `19`, while still failing closed for Vulkan `Float64` `math.erf` and
  `math.erfc`.
- Direct `stats.normal-quantile` Vulkan `Float64` smoke returns a numeric
  result again.
- Focused `advanced-collections-module` passed with `pass=2062 fail=0` after
  the fix.
  - Direct eval of `ml/linear-batched-reduce` returns `167.0`.
  - Direct eval of the removed old spelling returns
    `runtime/evaluation-error`.
  - `scripts/check_primitive_docs_parity.sh` passes.
  - `scripts/check_status_consistency.sh` passes.
  - `scripts/check_file_size_gate.sh` passes.
  - `git diff --check` passes.

## 2026-04-23 - Math/Stats Scientific Module Plan

- Added `docs/plans/math-stats-scientific-module-plan-2026-04-23.md`.
- Closed `SURFACE-NAMING-005` as a design decision:
  - `math` and `stats` should become real core scientific modules/facades;
  - current bare scientific functions and current `math/...` / `stats/...`
    slash functions are migration inputs, not the final canonical surface;
  - final target names use `math.*` and `stats.*`, including `math.lgamma`,
    `math.erf`, `math.erfc`, `stats.normal-cdf`, and
    `stats.normal-quantile`;
  - do not add global special-function names such as `erf`, `erfc`,
    `lgamma`, `gamma`, `digamma`, or `bessel-j`;
  - do not add deeper scientific slash pseudo-paths.
- Planned the current scientific surface migration:
  - elementary, rounding, complex, integer, and special-function exports under
    `math.*`;
  - distribution/statistical exports under `stats.*`;
  - old slash callables removed after facade parity unless the owner approves a
    short migration window;
  - bare elementary names require an explicit prelude-export decision if they
    remain.
- Planned Vulkan extension work for both modules:
  - expose existing Vulkan-supported elementary math through `math.*`;
  - expose existing Vulkan normal distribution support through `stats.*`;
  - add dense row-major Vulkan `Float32` `math.erf` and `math.erfc` first;
  - keep Vulkan `Float64` special functions and `math.lgamma` behind separate
    approximation/domain-policy validation;
  - split granular math/stats capability keys from broad `scientific-map-*`
    reporting while preserving no-hidden-CPU-fallback behavior.
- Opened TODO-backed implementation items:
  - `MATHSTATS-MODULE-001` core `math` and `stats` module facades;
  - `MATHSTATS-MODULE-002` docs/tests/examples migration;
  - `MATHSTATS-MODULE-003` old-callable removal or explicit prelude decision;
  - `MATHSTATS-VK-001` Vulkan math special-function extension;
  - `MATHSTATS-VK-002` Vulkan stats distribution extension alignment.
- No runtime behavior changed in this planning pass.

## 2026-04-23 - Core Math/Stats Module Facades

- Implemented `MATHSTATS-MODULE-001`.
- Added root-owned core module facades during primitive initialization:
  - `math` exports elementary math, rounding, complex helpers, integer helpers,
    `math.lgamma`, `math.erf`, and `math.erfc`;
  - `stats` exports `stats.normal-cdf` and `stats.normal-quantile`.
- The module exports share existing primitive values, so scalar, Tensor,
  callable/map, CUDA, and Vulkan behavior remains on the current implementation
  paths.
- Fixed `define` method-table fallback replacement to check only the current
  frame before replacing an existing method table fallback. This prevents module
  bodies from observing a parent global method table and mutating its fallback
  while defining an export with the same name.
- Removed the file-backed `lib/math.omni` and `lib/stats.omni` facade attempt.
  Validation showed that `(define abs abs)` in a module could poison global
  `abs`, and core scientific modules should not depend on source-relative
  `lib/*.omni` lookup.
- Updated module/scientific docs to state that `math` and `stats` are prebound
  core module values and that old slash scientific spellings are transitional
  single-symbol primitives.
- Validation:
  - `c3c build --obj-out obj` passes.
  - Direct REPL smoke confirms `(import math)` no longer breaks `(abs 0.0)`.
  - Direct eval smoke confirms prebound `math.erf` and `stats.normal-cdf`.
  - `/tmp` script smoke confirms `(import math)` / `(import stats)` do not
    require repo-relative `lib/*.omni` files.
  - Focused advanced collections-module slice passes with `pass=2053 fail=0`.
  - Focused compiler slice passes with `pass=301 fail=0`; expected bindgen
    negative-test diagnostics are printed by that slice.
  - `scripts/check_e2e_baseline_policy.sh` passes.
  - `scripts/check_status_consistency.sh` passes.
  - `scripts/check_file_size_gate.sh` passes.
  - `scripts/check_primitive_docs_parity.sh` passes.
  - `git diff --check` passes.

## 2026-04-23 - Math/Stats Module Surface Docs and Tests Migration

- Implemented `MATHSTATS-MODULE-002`.
- Migrated active scientific runtime tests from the slash special/stat call
  surface to the core module surface:
  - scalar/special numeric tests now call `math.lgamma`, `math.erf`,
    `math.erfc`, `stats.normal-cdf`, and `stats.normal-quantile`;
  - CUDA/Vulkan map/direct Tensor tests now call module members;
  - module facade tests no longer compare against slash callables.
- Updated active scientific Tensor docs, CUDA/Vulkan scientific map docs,
  primitive docs, and language docs to prefer `math.*` / `stats.*`.
- Updated scientific special/stat diagnostics to report dotted module names.
- Remaining slash references in active docs are explicit compatibility notes;
  remaining slash names in runtime code are compatibility registrations and
  backend callable-recognition keys for the module-exported primitive values.
- Validation:
  - `c3c build --obj-out obj` passes.
  - Focused advanced collections-module slice passes with `pass=2053 fail=0`.
  - Focused advanced stdlib numeric slice passes with `pass=411 fail=0`.
  - Focused compiler slice passes with `pass=301 fail=0`; expected bindgen
    negative-test diagnostics are printed by that slice.
  - `scripts/check_e2e_baseline_policy.sh` passes.
  - `scripts/check_file_size_gate.sh` passes.
  - `scripts/check_primitive_docs_parity.sh` passes.

## 2026-04-23 - Math/Stats Slash Scientific Callable Removal

- Implemented `MATHSTATS-MODULE-003`.
- Removed public runtime registrations and AOT primitive hash entries for
  `math/lgamma`, `math/erf`, `math/erfc`, `stats/normal-cdf`, and
  `stats/normal-quantile`.
- `math` / `stats` module exports now create their own dotted-name primitive
  values for the special/stat functions.
- Backend callable recognition now keys on dotted primitive names such as
  `math.erf` and `stats.normal-quantile`, preserving `map math.erf` and
  direct Tensor module-call behavior without public slash bindings.
- Added negative runtime tests proving old slash spellings fail.
- Bare elementary names remain as prelude/core primitive exports; removing
  them is a separate language-prelude decision outside this special/stat slash
  cleanup.
- Validation:
  - `c3c build --obj-out obj` passes.
  - Direct eval smokes for `math.erf`, `stats.normal-cdf`, `map math.erf`, and
    old slash removal pass.
  - Focused advanced collections-module slice passes with `pass=2055 fail=0`.
  - Focused advanced stdlib numeric slice passes with `pass=411 fail=0`.
  - Focused compiler slice passes with `pass=301 fail=0`; expected bindgen
    negative-test diagnostics are printed by that slice.

## 2026-04-23 - Math/Stats Backend Capability Granularity

- Implemented `MATHSTATS-VK-002`.
- `tensor-backends` now reports granular scientific capability fields on every
  backend entry:
  - `math-elementary-float64`
  - `math-elementary-float32`
  - `math-error-function-float64`
  - `math-error-function-float32`
  - `stats-distribution-float64`
  - `stats-distribution-float32`
- The `math-error-function-*` keys intentionally cover `math.erf` and
  `math.erfc`; they do not claim `math.lgamma`.
- Broad `scientific-map-float64` / `scientific-map-float32` and legacy
  `stats-normal-float64` / `stats-normal-float32` fields remain compatibility
  fields.
- Vulkan reports partial support truthfully:
  - `math-elementary-float64` is false;
  - `math-elementary-float32` follows Vulkan Float32 availability;
  - `math-error-function-float64` is false;
  - `math-error-function-float32` follows Vulkan Float32 availability;
  - `stats-distribution-float64` follows Vulkan Float64 availability;
  - `stats-distribution-float32` follows Vulkan Float32 availability.
- Policy documented: Vulkan `Float64` `math.erf` / `math.erfc` stay
  fail-closed until a validated approximation contract is introduced, and
  Vulkan `math.lgamma` remains a separate hardening item with its own
  approximation/domain/status policy.
- Validation:
  - `c3c build --obj-out obj` passes.
  - Focused advanced collections-module slice passes with `pass=2062 fail=0`.
  - `scripts/check_primitive_docs_parity.sh` passes.
  - `scripts/check_file_size_gate.sh` passes.
  - `scripts/check_status_consistency.sh` passes.
  - `scripts/check_e2e_baseline_policy.sh` passes.
  - `git diff --check` passes.

## 2026-04-23 - Process Spawn Fs-Handle Double-Free Fix

- Fixed a double-free in `src/lisp/prim_io_fs_stream.c3` on the
  `make_ffi_handle_ex(...) == null` path for `fs-handle` construction.
- Root cause: `make_ffi_handle_ex` already releases the `FsHandle` payload via
  the FFI finalizer on constructor failure, so the constructor's explicit
  `mem::free(fh)` duplicated the free and crashed the memory-lifetime smoke
  slice.
- The constructor now lets the FFI failure path own the cleanup and only
  reports the trace flag when the handle allocation path fails.
- Validation:
  - `c3c build --obj-out obj` passes.
  - `scripts/run_validation_container.sh env OMNI_TEST_VERBOSE=0
    OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke
    LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` passes
    with `238 passed, 0 failed`.
  - `git diff --check` passes.

## 2026-04-23 - Dispatch Ambiguous Tie-List OOM Hardening

- Hardened `src/lisp/eval_dispatch_match.c3` so ambiguous method tie-list
  construction uses `make_cons_or_error(...)` instead of raw `make_cons(...)`
  for the candidate-index list. This keeps the failure mode explicit when the
  allocation path is forced or exhausted and prevents a malformed tie list from
  leaking into the ambiguous-dispatch payload path.
- Added a focused regression in
  `src/lisp/tests_advanced_type_dispatch_groups.c3` that forces the checked
  cons-allocation failure hook and verifies the dispatch path returns the
  expected out-of-memory error message instead of continuing into ambiguity
  handling.
- Validation:
  - `c3c build --obj-out obj` passes.
  - `scripts/run_validation_container.sh env OMNI_TEST_VERBOSE=0
    OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced
    OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain
    LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` passes
    with `241 passed, 0 failed`.
  - `git diff --check` passes.
## 2026-04-23

- Hardened the Vulkan tensor unary host wrappers in `csrc/tensor_vulkan_helpers.c` so non-empty calls now reject a null `input_device_ptr` consistently across `f32`, `f64`, `complex64`, `complex128`, and the complex-to-real variants before dispatch.
- Tightened `tensor_graph_run_vulkan_readable_float32()` in `src/lisp/prim_tensor_graph_run.c3` so Vulkan view tensors now require a device handle even when the byte length is zero; the zero-byte/null-handle hole is closed.
- Made `find_best_method()` in `src/lisp/eval_dispatch_match.c3` fail closed on `make_int` / `make_cons` allocation failures while building ambiguity diagnostics instead of threading malformed candidate lists through dispatch errors.
- Added regressions in `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3` for the Vulkan readable-helper zero-byte view guard and the null-input Vulkan unary host check.
- Verification for the updated slice passed: `c3c build --obj-out obj`, `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric ./build/main --test-suite lisp`, `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain ./build/main --test-suite lisp`, `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface ./build/main --test-suite lisp`, `scripts/check_status_consistency.sh`, and `git diff --check`.
## 2026-04-23 TLS lifecycle follow-up

- Added a direct TLS lifecycle regression in `src/lisp/tests_runtime_async_io_tls_groups.c3` that proves the shared in-flight gate rejects concurrent `tls-read` / `tls-write` / `tls-close` entry.
- Updated the foreign-runtime plan and area note to mark the scheduler-bound TLS lifecycle regression as covered by regression tests rather than an open follow-up.
- Verification passed on the async TLS slice: `OMNI_LISP_TEST_SLICE=async ./build/main --test-suite lisp` -> `70 passed, 0 failed`.

## 2026-04-23 audit-repair loop continuation

- Restored recursive traversal in `src/lisp/compiler_mutable_capture_detection_walk.c3` for effect-form subtrees so nested lambdas under `E_PERFORM`, `E_HANDLE`, and `E_RESOLVE` are visible to mutable-capture detection.
- Hardened `csrc/tensor_vulkan_helpers_core.c` so Vulkan physical-device selection only considers compute-capable queue families.
- Restored the zero-byte Vulkan readable-tensor contract in `src/lisp/prim_tensor_graph_run.c3` so null device handles remain accepted when `byte_len == 0`.
- Added regressions in `src/lisp/tests_compiler_codegen_groups_tail.c3`, `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3`, and `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`, plus the Vulkan test extern in `src/lisp/tensor_vulkan_backend.c3`.
- Verification passed: `c3c build --obj-out obj`, `OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` (`306 passed, 0 failed`), `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` (`2069 passed, 0 failed`), `scripts/check_status_consistency.sh`, and `git diff --check`.

## 2026-04-23 re-audit follow-up

- Re-read the remaining audit entries against the current tree and confirmed that the previously reported runtime/memory issues were already resolved or were non-live contract boundaries.
- No additional code changes were required in this pass.
- Verification: `jj status`, `jj diff --name-only`, `git diff --check`, and `scripts/check_status_consistency.sh`.

## 2026-04-23 Vulkan selector hardening

- Added a physical-device-properties probe to the Vulkan helper layer and loaded `vkGetPhysicalDeviceProperties` alongside the existing Vulkan entry points.
- Scored discrete GPUs ahead of integrated, virtual, and CPU devices so selector preference now reflects device class, not just “first compute-capable queue.”
- Added a host-side regression in `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3` and exposed the helper through `src/lisp/tensor_vulkan_backend.c3`.
- Verification passed: `c3c build --obj-out obj`, `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp` (`2070 passed, 0 failed`), `scripts/check_status_consistency.sh`, and `git diff --check`.

## 2026-04-24 Stable Escape Graph Shape Audit Kickoff

- Started the stable-escape-graph rollout with shape-audit telemetry instead of a
  speculative stable-store rewrite.
- Added `BoundaryEscapeShapeStats` tracking in `src/lisp/eval.c3` and wired
  `boundary_graph_alias_unsafe_for_reuse(...)` to record candidate-root shape
  counts, graph-root counts, unsafe-root counts, and cons-spine lengths.
- Exposed the new snapshot through runtime memory stats and boundary traversal
  summary output, and reset the counters alongside the existing promotion
  telemetry.
- Added a smoke regression in
  `src/lisp/tests_memory_lifetime_boundary_groups.c3` that exercises cons,
  array, and scalar promotions and asserts the new shape counters move.
- Opened the live TODO queue with `STABLE-ESCAPE-SHAPE-001`,
  `STABLE-ESCAPE-STORE-001`, and `STABLE-ESCAPE-PREP-001` in
  `docs/todo_parts/todo_part_16.md` and linked them from `TODO.md` and the
  proposal note.
- Validation passed: `c3c build --obj-out obj`,
  `OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
  (`239 passed, 0 failed`), `scripts/check_status_consistency.sh`, and
  `git diff --check`.

## 2026-04-24 Stable Escape Store Skeleton

- Added `src/lisp/stable_escape_store.c3` as an interpreter-owned stable-handle
  registry with slot/generation handles, publish / resolve / retire / reset
  operations, and scope-generation validation.
- Added `StableEscapeStore*` to `Interp`, initialized it in
  `interp_init_runtime_state`, and reset/free it during interpreter teardown so
  stable handles remain interpreter-local instead of global.
- Added a smoke regression in
  `src/lisp/tests_memory_lifetime_boundary_groups.c3` that publishes an escape
  value, resolves it, explicitly retires it, and confirms the stale handle
  fails closed after retirement.
- Closed `STABLE-ESCAPE-STORE-001` in the live TODO queue and updated the
  proposal note / index so the remaining implementation focus is prepared
  publication.
- Validation passed: `c3c build --obj-out obj`,
  `OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
  (`240 passed, 0 failed`), `scripts/check_status_consistency.sh`, and
  `git diff --check`.

## 2026-04-24 Stable Escape Prepared-Publication Hook

- Updated `boundary_commit_escape_commit_reused_in_target_chain(...)` so the
  already-safe reuse path publishes and resolves the result through the
  interpreter-owned stable store before returning it.
- Added publication-hook telemetry in `src/lisp/stable_escape_store.c3` and
  reset it during unified test bootstrap.
- Extended the already-safe reuse regression in
  `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3`
  so it asserts the publication hook increments exactly once while the return
  value remains unchanged and the copy-site count stays flat.
- The stable-escape TODO queue now has only `STABLE-ESCAPE-PREP-001` open.
- Validation passed: `c3c build --obj-out obj`,
  `OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
  (`240 passed, 0 failed`), `scripts/check_status_consistency.sh`, and
  `git diff --check`.

## 2026-04-24 Stable Escape Rollout Closure

- Added a prepare-gated publication hook for the already-safe reuse path in
  `src/lisp/eval_boundary_commit_escape_helpers.c3` / `src/lisp/stable_escape_store.c3`,
  so the boundary path now publishes through the interpreter-owned stable
  store after passing the alias/shape gate.
- Extended the already-safe reuse regression in
  `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3`
  to assert both the publication and prepared-publication hook counters
  increment exactly once while the returned value and copy-site count stay
  unchanged.
- Closed the stable-escape rollout queue completely: `STABLE-ESCAPE-SHAPE-001`,
  `STABLE-ESCAPE-STORE-001`, and `STABLE-ESCAPE-PREP-001` are all done, and
  `TODO.md` now reports no live items.
- Validation passed: `c3c build --obj-out obj`,
  `OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
  (`240 passed, 0 failed`), `scripts/check_status_consistency.sh`, and
  `git diff --check`.

## 2026-04-24 Stable Escape Owner-Scope Retention Hardening

- Hardened `src/lisp/stable_escape_store.c3` so published handles retain the
  owning `ScopeRegion` until retire/reset, eliminating the dangling owner-scope
  pointer hazard in the store entry.
- Updated `stable_escape_store_reset(...)` and `stable_escape_store_retire(...)`
  to release retained scopes after clearing the entry, preserving deterministic
  cleanup without leaking refcounts.
- Extended the boundary smoke regression in
  `src/lisp/tests_memory_lifetime_boundary_groups.c3` so a published handle
  remains valid after the original scope reference is released, then fails
  closed after explicit retire.
- Validation passed: `c3c build --obj-out obj`,
  `OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
  (`240 passed, 0 failed`), `scripts/check_status_consistency.sh`, and
  `git diff --check`.

## 2026-04-24 Stable Escape Mutex-Boundary Audit Fix

- Fixed `src/lisp/stable_escape_store.c3` so store reset/retire no longer call
  `scope_release(...)` while holding the store mutex.
- The retained owner scopes are still released deterministically, but now only
  after the store entry table is unlocked and stable, which keeps the store’s
  internal lock boundary separate from scope teardown work.
- Validation passed: `c3c build --obj-out obj`,
  `OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
  (`240 passed, 0 failed`), `scripts/check_status_consistency.sh`, and
  `git diff --check`.

## 2026-04-24 Stable Escape Prepared Graph Data Structure Slice

- Replaced the registry-only placeholder in `src/lisp/stable_escape_store.c3`
  with the first real prepared graph representation: reusable `CONS` roots
  with scalar leaves now publish a prepared node table with stable child
  indices in addition to the root handle metadata.
- Added narrow inspection helpers for prepared node count, root index, node
  tags, and child indices so the runtime contract is testable without
  inventing a second public value representation.
- Tightened prepared-node liveness validation after the implementation audit so
  stored node sources must remain valid in the full owner scope chain, not only
  through the root object’s owner scope membership check.
- Kept the current caller contract stable: the boundary path still resolves the
  original reusable root after publication, but the store now owns real graph
  metadata instead of only round-tripping a raw pointer.
- Added a regression in
  `src/lisp/tests_memory_lifetime_boundary_groups.c3` that publishes a
  promoted `CONS` list through the prepared path, asserts the exact prepared
  node shape (`CONS -> INT / CONS -> INT / NIL`), and then retires the handle
  cleanly.
- Validation passed: `c3c build --obj-out obj`,
  `scripts/run_validation_container.sh env OMNI_TEST_VERBOSE=0 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  (`241 passed, 0 failed`), `scripts/check_status_consistency.sh`, and
  `git diff --check`.

## 2026-04-24 Stable Escape Publication Outcome Observability

- Made prepared-publication route selection explicit in
  `src/lisp/stable_escape_store.c3`:
  - prepared publication success keeps its existing counter,
  - raw compatibility publication now has a separate counter,
  - prepare-failure fallback is counted before the raw route,
  - alias-unsafe skips and resolve failures are counted separately.
- Exposed the stable publication outcome counters through the existing
  `runtime-memory-stats` escape-shape dictionary so diagnostics can distinguish
  prepared success from compatibility fallback.
- Reset the new counters during unified test bootstrap in `src/lisp/tests_tests.c3`.
- Added a boundary regression in
  `src/lisp/tests_memory_lifetime_boundary_groups.c3` proving unsupported
  prepared graphs increment fallback/raw counters without incrementing prepared
  success.
- Reconciled stable-escape plan/TODO wording with current runtime truth:
  prepared `ARRAY` roots with shared child reuse are landed, while dictionaries,
  closures, cycles, and mutation policy remain future semantic boundaries.
- Validation: C3 diagnostics passed for touched runtime/test C3 files. Host shell
  build/test commands could not run in this environment because every shell
  command currently fails before execution with
  `bwrap: loopback: Failed RTM_NEWADDR`.

## 2026-04-24 Stable Escape Dictionary Prepared Graph Slice

- Extended `src/lisp/stable_escape_store.c3` so prepared graph publication now
  supports `HASHMAP` roots.
  - Dictionary entries publish as key/value child-index pairs in hash-table slot
    order.
  - The existing prepared-graph memo table preserves shared key/value identity.
  - Prepared-node liveness validation now checks dictionary backing storage and
    child count against `HashMap.count * 2`.
- Added a regression in `src/lisp/tests_memory_lifetime_boundary_groups.c3` that
  publishes a dictionary with two symbol keys sharing the same cons value and
  verifies the prepared graph reuses the value node.
- Updated the unsupported fallback regression at this checkpoint to use `SET`,
  leaving set graphs as the compatibility-fallback probe before the set slice
  landed.
- Reconciled stable-escape plan/TODO wording at this checkpoint: prepared
  `CONS`, `ARRAY`, and dictionary roots were landed; closures, cycles, sets,
  and mutation policy remained future semantic boundaries.
- Validation: C3 diagnostics passed for touched runtime/test C3 files. Host shell
  build/test commands remain blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

## 2026-04-24 Stable Escape Set Prepared Graph Slice

- Extended `src/lisp/stable_escape_store.c3` so prepared graph publication now
  supports `SET` roots.
  - Set entries publish member child indices only, matching the boundary graph
    audit's set semantics and avoiding backing true-values as graph edges.
  - Prepared-node liveness validation now checks set backing storage and child
    count against `HashMap.count`.
- Added a regression in `src/lisp/tests_memory_lifetime_boundary_groups.c3` that
  publishes a set with a cons member and symbol member, verifies member-only
  child edges, checks the cons child shape, and retires the handle.
- Retargeted unsupported prepared-graph fallback observability to `CLOSURE`, so
  the fallback counter test remains meaningful after dictionaries and sets are
  both prepared.
- Reconciled stable-escape plan/TODO wording: prepared `CONS`, `ARRAY`,
  dictionary, and set roots are landed; closures, cycles, and mutation policy
  remain future semantic boundaries.
- Validation: C3 diagnostics passed for touched runtime/test C3 files. Host shell
  build/test commands remain blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

## 2026-04-24 Stable Escape Cycle Prepared Graph Slice

- Added a regression in `src/lisp/tests_memory_lifetime_boundary_groups.c3` for
  a self-referential `CONS` graph.
  - The prepared graph has exactly one node.
  - Both child slots point back to the root node index.
  - The invalid child ordinal guard and handle retirement path are checked.
- This pins the existing placeholder-node behavior in
  `src/lisp/stable_escape_store.c3`: container nodes are registered before
  walking children, so cycles resolve through memoized back-edges instead of
  recursive expansion.
- Reconciled stable-escape plan/TODO wording: prepared `CONS`, `ARRAY`,
  dictionary, set, and cyclic container roots are landed; closures and mutation
  policy remain future semantic boundaries.
- Validation: C3 diagnostics passed for touched runtime/test C3 files. Host shell
  build/test commands remain blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

## 2026-04-24 Stable Escape Closure Prepared Graph Slice

- Extended `src/lisp/stable_escape_store.c3` so prepared graph publication now
  supports `CLOSURE` roots.
  - The prepared graph scratch records `global_env` so closure preparation walks
    captured environment frames only up to, but not including, the global frame.
  - Closure prepared nodes publish child edges for captured binding values in
    local-to-parent frame order.
  - Prepared-node liveness validation recomputes captured binding-value counts
    against the stored `global_env` boundary.
- Added a regression in `src/lisp/tests_memory_lifetime_boundary_groups.c3` that
  publishes a closure capturing a cons and symbol value, verifies the closure
  root's captured-value child edges, checks the captured cons child shape, and
  retires the handle.
- Retargeted unsupported prepared-graph fallback observability to `ITERATOR`, so
  the fallback counter test remains meaningful after closures are prepared.
- Reconciled stable-escape plan/TODO wording: prepared `CONS`, `ARRAY`,
  dictionary, set, cyclic container, and closure roots are landed; mutation
  policy remains the remaining semantic boundary.
- Validation: C3 diagnostics passed for touched runtime/test C3 files. Host shell
  build/test commands remain blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

## 2026-04-24 Stable Escape Nested Edge Ordering Audit Fix

- Audited prepared graph child-edge layout after the closure slice and found that
  `ARRAY`, `HASHMAP`, `SET`, and closure env preparation captured the parent
  `child_offset` before recursively preparing child graphs.
- Fixed `src/lisp/stable_escape_store.c3` so those cases first collect child
  node indices, then append the parent's edges after recursive child preparation
  completes. Parent edge ranges are now contiguous and cannot point at nested
  child edges emitted by a child graph.
- This preserves the existing `CONS` behavior, which already prepared both
  children before appending the parent edges.
- Validation: C3 diagnostics passed for touched runtime/test C3 files. Host shell
  build/test commands remain blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

## 2026-04-24 Stable Escape Mutation Policy Slice

- Implemented fail-closed structural mutation policy in
  `src/lisp/stable_escape_store.c3`.
  - Prepared-node liveness now checks that each node's current tag still matches
    the published tag.
  - `CONS`, `ARRAY`, `HASHMAP`, `SET`, and `CLOSURE` nodes now verify current
    child pointers/captured binding values against the prepared child-index
    snapshot.
  - If the graph has structurally drifted, handle liveness fails and resolve
    returns `null`; explicit retire remains available for cleanup.
- Added regressions in `src/lisp/tests_memory_lifetime_boundary_groups.c3` for
  prepared cons child mutation and prepared closure captured-binding mutation.
- Reconciled stable-escape plan/TODO wording: the prepared data model now covers
  `CONS`, `ARRAY`, dictionary, set, cyclic containers, closures, and structural
  mutation drift. The next plan-level question is reducing older transplant
  machinery to compatibility-only status.
- Validation: C3 diagnostics passed for touched runtime/test C3 files. Host shell
  build/test commands remain blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

## 2026-04-24 Stable Escape Compatibility Route Visibility Slice

- Added route flags to `BoundaryCommitEscapeResult` in
  `src/lisp/eval_boundary_api_types.c3`.
  - `stable_publication_attempted` marks the already-safe boundary path that
    attempted stable prepared publication.
  - `compatibility_path_used` marks older destination promotion, releasing-scope
    splice, and mixed-destination promotion paths.
- Updated `src/lisp/eval_boundary_commit_escape.c3`,
  `src/lisp/eval_boundary_commit_escape_helpers.c3`, and
  `src/lisp/eval_boundary_graph_audit_logging.c3` so commit results and graph
  audit context no longer require callers to infer stable-vs-compatibility
  routing from outcome names.
- Extended `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3`
  to prove TEMP cons commits use compatibility routing while already-safe
  reusable commits use stable prepared publication.
- Updated stable-escape plan/TODO wording. The next semantic boundary is
  materializing prepared graph data outside the source scope so TEMP return
  values can stop depending on compatibility promotion/copy.
- Validation: C3 diagnostics and parse-document passed for touched runtime/test
  C3 files. Host shell build/test commands remain blocked by
  `bwrap: loopback: Failed RTM_NEWADDR`.

## 2026-04-24 Stable Escape Prepared Materialization Slice

- Added prepared-materialization routing in
  `src/lisp/eval_boundary_commit_destination.c3`: TEMP `CONS`, `ARRAY`,
  dictionary, set, closure, string/error payload, time-point, and big-number
  roots or nested nodes now prepare graph metadata, allocate copies in a target
  build scope, wire prepared child indices, and commit the build scope before
  the releasing scope can die.
- Kept `src/lisp/stable_escape_store.c3` as the prepared-graph authority and
  moved destination materialization out of that near-1000-line file into the
  smaller destination-commit module because this sandbox could not create the
  intended new `src/lisp/stable_escape_materialize.c3` file.
- Preserved fallback for unsupported prepared graph shapes and preserved
  promotion-budget/memo accounting through the active `PromotionContext`; failed
  materialization attempts restore destination-builder memo state before older
  compatibility promotion can run.
- Extended boundary commit result visibility with `stable_materialization_used`
  and updated `src/lisp/eval_boundary_commit_escape_helpers.c3`,
  `src/lisp/eval_boundary_graph_audit_logging.c3`, and
  `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3` so
  regressions distinguish stable publication, stable materialization, and
  compatibility routing. The regression suite now includes a TEMP array root
  with copied string, BigFloat, and BigComplex payloads; a root BigInteger clone;
  and a typed closure whose signature is copied into the destination ESCAPE lane.
- Updated the stable-escape plan/TODO queue. `STABLE-ESCAPE-ARENA-001`,
  `STABLE-ESCAPE-ARENA-002`, and `STABLE-ESCAPE-ARENA-003` are closed; no open
  stable-escape prepared-materialization rollout item remains.
- Validation: C3 diagnostics and parse-document passed for the touched runtime
  and regression C3 files. Host shell build/status/check commands remain blocked
  by `bwrap: loopback: Failed RTM_NEWADDR`.

## 2026-04-24 Memory Boundary Ownership Policy Slice

- Added `docs/plans/memory-boundary-architecture-spec-2026-04-24.md` as the
  current implementation spec for region-RC, TEMP/ESCAPE, stable
  materialization, and FFI/foreign-handle boundary policy.
- Added `src/lisp/value_boundary_ownership_policy.c3` with a runtime-private
  `ValueTag` policy helper that classifies immediate values, heap-cloned
  payloads, graph containers, closure graphs, shared wrappers, opaque foreign
  handles, interpreter-stable values, and unsupported boundary values.
- Routed stable materialization eligibility,
  `boundary_alias_value_has_child_payload`, and graph-audit edge-class mapping
  through the shared policy helper. `FFI_HANDLE` is explicitly opaque foreign
  state: it is not traversed as an Omni graph and is not a stable
  materialization candidate without a future bridge-specific policy.
- Added a boundary commit regression proving the policy keeps FFI opaque while
  preserving array materialization, big payload clone policy, and closure env
  audit classification.
- Validation passed for the policy slice: `c3c build --obj-out obj`,
  C3 diagnostics for touched runtime/test files, bounded container
  `memory-lifetime-policy` slice (`2 passed, 0 failed`),
  `scripts/check_status_consistency.sh`, and `git diff --check`.
- Follow-up verification closed `MEM-BOUNDARY-VERIFY-001`: stable-store
  publishability now accepts roots in the retained owner scope chain rather than
  requiring the root itself to live in the current scope's ESCAPE lane, and
  copied/materialized destination closure commits now roll back staged source
  closure-env normalization instead of finalizing source mutations when the
  committed value is a distinct destination copy.
- Validation for the follow-up passed: `c3c build --obj-out obj`; bounded
  container `memory-lifetime-smoke` (`253 passed, 0 failed`); rebuilt default
  validation image with `valgrind`; bounded container Valgrind
  `memory-lifetime-smoke` (`253 passed, 0 failed`); host Valgrind default
  Lisp/basic suite (`167 passed, 0 failed`); `scripts/check_status_consistency.sh`;
  and `git diff --check`.
- The memory boundary architecture spec now records stable indexed publication
  and region transplanting as constrained fast paths. They are valid only when
  owner-scope-chain or whole-region lifetime proofs preserve the invariant that
  committed ESCAPE roots do not retain reachable Omni-owned edges into
  non-surviving TEMP storage.
- Added `docs/plans/memory-boundary-proof-planner-roadmap-2026-04-24.md` as the
  next architecture roadmap. It combines stable graph passports, planner route
  reasons, mutation epochs, explicit transplant proofs, FFI bridge declarations,
  copy-debt telemetry, and planner-owned commit-path migration.
- Open roadmap work is tracked in `docs/todo_parts/todo_part_18.md` as
  `MEM-BOUNDARY-PLANNER-001`, `MEM-BOUNDARY-PASSPORT-001`,
  `MEM-BOUNDARY-EPOCH-001`, `MEM-BOUNDARY-TRANSPLANT-001`,
  `MEM-BOUNDARY-FFI-BRIDGE-001`, `MEM-BOUNDARY-COPY-DEBT-001`, and
  `MEM-BOUNDARY-PLAN-MIGRATE-001`.
- Implemented and closed `MEM-BOUNDARY-PLANNER-001`.
  - Added planner route/reason enums, `BoundaryPlanDecision`, and
    `boundary_plan_commit_escape(...)`.
  - `BoundaryCommitEscapeResult` now records planned and selected routes.
  - Boundary commit helpers mark selected stable publish, stable materialization,
    compatibility destination, region transplant, mixed destination, and
    fail-closed outcomes explicitly.
  - Graph-audit commit-context logs include planned/selected route and selected
    reason names.
  - Regression coverage now proves the route ladder baseline and selected routes
    for stable materialization, stable publish/reuse, and compatibility
    destination commits.
  - Validation passed: `c3c build --obj-out obj`; bounded container
    `memory-lifetime-smoke` (`254 passed, 0 failed`); bounded container
    Valgrind `memory-lifetime-smoke` (`254 passed, 0 failed`).
- Implemented and closed `MEM-BOUNDARY-PASSPORT-001`.
  - Stable prepared publications now store `StableGraphPassport` summaries with
    owner-scope generation stamps, owner escape generation, prepared node/edge
    counts, root index, policy flags, risk flags, and invalidation reason.
  - Added `stable_escape_store_passport_snapshot(...)` for planner-side proof
    inspection without transferring ownership of ordinary Omni values.
  - Regression coverage asserts valid passport summaries and stale prepared
    graph invalidation after cons child mutation.
  - Validation passed: `c3c build --obj-out obj`; bounded container
    `memory-lifetime-smoke` (`254 passed, 0 failed`); bounded container
    Valgrind `memory-lifetime-smoke` (`254 passed, 0 failed`).
- Implemented and closed `MEM-BOUNDARY-EPOCH-001`.
  - Added a pointer-keyed stable graph mutation epoch table and prepared-node
    epoch snapshots.
  - Passport validation now rejects stale prepared graphs through epoch mismatch
    before relying on the deeper shape walk.
  - Env binding writes, dictionary/set writes, and array write/push helpers now
    stamp the specific mutated frame/container instead of globally invalidating
    unrelated prepared graphs.
  - Validation passed: `c3c build --obj-out obj`; bounded container
    `memory-lifetime-smoke` (`254 passed, 0 failed`); bounded container
    Valgrind `memory-lifetime-smoke` (`254 passed, 0 failed`).

## 2026-04-24 Vulkan/CUDA/ML audit Critical/High remediation

- Closed the Critical and High findings from
  `AUDIT_REPORT_VULKAN_CUDA_ML_2026-04-23.md`.
- Vulkan shared context publication and refcount updates are now guarded by a
  process-local mutex instead of unsynchronized double-checked publication.
- Added `omni_tensor_vulkan_group_count_1d(...)` and routed all
  `csrc/tensor_vulkan_helpers*.c` 1D dispatch group-count calculations through
  checked size_t arithmetic; added
  `omni_tensor_backend_vulkan_group_count_overflow_guard_for_tests()`.
- Adam bias corrections now reject subnormal denominators in the CPU, CUDA, and
  Vulkan optimizer paths; native CUDA/Vulkan Adam helpers also fail closed below
  `FLT_MIN`/`DBL_MIN`.
- CUDA complex trace kernels now guard `out[0]` writes to one thread, and CUDA
  optimizer/complex-matrix PTX includes were regenerated from the `.cu` sources
  with CUDA 13 nvcc.
- Medium and Low audit residuals were recorded in
  `docs/todo_parts/todo_part_17.md`.
- Validation passed:
  - C3 diagnostics for touched optimizer/backend/test C3 files.
  - `git diff --check` for the working tree.
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_ml_optimizer.cu -o /tmp/tensor_cuda_ml_optimizer.ptx`
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_complex_matrix.cu -o /tmp/tensor_cuda_complex_matrix.ptx`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite all`
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`2071 passed, 0 failed`)

## 2026-04-24 memory boundary transplant proof implementation

- Implemented and closed `MEM-BOUNDARY-TRANSPLANT-001`.
  - Added `BoundaryTransplantProof` as the explicit proof object for
    whole-region splice/transplant eligibility.
  - Scope-transfer legality now builds proof fields for parent/child
    non-nullness, same-scope rejection, immediate-child relationship,
    child-refcount uniqueness, owner-thread match, and TEMP/ESCAPE lane shapes.
  - Root-splice legality now records root presence, source ESCAPE-root
    ownership, source escape graph safety, target root-audit safety,
    closure-env safety, FFI/native opacity, mutation applicability, and stamp
    rewrite safety before mutating scope state.
  - Boundary commit splice candidates now consume
    `BoundaryTransplantProof` instead of the former standalone
    `boundary_commit_can_splice_escape_root(...)` boolean precheck.
  - Regression coverage asserts explicit proof fields for legal/rejected scope
    transfers and the root-audit proof rejection for a releasing TEMP edge.
- Validation passed: `c3c build --obj-out obj`; bounded container
  `memory-lifetime-smoke` (`254 passed, 0 failed`); bounded container Valgrind
  `memory-lifetime-smoke` (`254 passed, 0 failed`).

## 2026-04-24 memory boundary FFI bridge declarations

- Implemented and closed `MEM-BOUNDARY-FFI-BRIDGE-001`.
  - Added `FfiBridgeBoundaryMode` and `FfiBridgeBoundaryDeclaration` helpers.
  - `FfiHandle` now records a per-handle boundary mode; all existing FFI box
    construction defaults to `FFI_BRIDGE_BOUNDARY_OPAQUE`, preserving current
    behavior.
  - Boundary transplant proof now consumes the bridge declaration and fails
    closed for trace/copy-hook declarations until explicit traversal/copy hooks
    exist.
  - Regression coverage asserts the opaque default and the fail-closed behavior
    for a trace-hook foreign handle in root transplant proof.
- Validation passed: `c3c build --obj-out obj`; bounded container
  `memory-lifetime-smoke` (`255 passed, 0 failed`); bounded container Valgrind
  `memory-lifetime-smoke` (`255 passed, 0 failed`).

## 2026-04-24 Implicit Body Block Syntax

- Confirmed and covered the parser contract that `lambda`, shorthand function
  `define`, ordinary `let`, `let ^rec`, and named `let` bodies accept multiple
  expressions and lower them through the shared implicit `E_BLOCK` body helper.
- Added runtime regressions for each requested body form in
  `src/lisp/tests_advanced_core_unicode_groups_more.c3`.
- Added compiler coverage that an implicit body block preserves tail-call
  position for a final recursive call in `src/lisp/tests_compiler_codegen_groups.c3`.
- Updated `docs/LANGUAGE_SPEC.part-02.md` to document the concise body syntax
  and keep explicit `block` as the expression-level sequencing form.

## 2026-04-24 Concise Syntax Surface Follow-through

- Promoted `λ` as the canonical lambda spelling in parser/compiler/macro output
  while retaining `lambda` as the long input alias.
- Added built-in `str` macro expansion: `(str "hello {name}")` rewrites before
  evaluation to `string-append` plus `String` coercion of parsed hole
  expressions, preserving lexical call-site bindings without lexer changes.
- Added parser sugar for bare symbol keys in `{}` dictionary literals; key
  position symbols auto-quote while non-symbol keys remain explicit.
- Added reader radix integer literals: `#x`, `#b`, and `#o`, including negative
  forms such as `#x-10`.
- Added reader-tag data helpers:
  - `#hex "ff 0a"` via `hex` returns an Array of integer bytes.
  - `#time "2024-01-15T10:30:00Z"` via `time` returns a UTC `TimePoint`.
  - `#uuid "550e8400-e29b-41d4-a716-446655440000"` via `uuid` validates and
    returns the canonical UUID string.
- Updated language spec docs and focused runtime/compiler tests for the new
  syntax surfaces.

Validation:
- C3 diagnostics passed for touched parser, macro, primitive, and test files.
- `c3c build` passed.
- Focused runtime smokes passed for `λ`/`lambda`, `str` macro expansion,
  dict-key sugar, radix literals, `#hex`, `#time`, and `#uuid`.
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` passed.
- `OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-lambda-syntax LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` passed.
- `OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-macro-hygiene-quasi-pattern LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` passed.
- `OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` passed.
- `OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=unicode LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` passed.
- `OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` passed.
- `git diff --check` passed.

## 2026-04-24 memory boundary copy-debt telemetry

- Implemented and closed `MEM-BOUNDARY-COPY-DEBT-001`.
  - Boundary decision stats now record planned routes, selected routes,
    fail-closed route reasons, materialization success totals, materialized graph
    node totals, and estimated materialized payload bytes.
  - Test summaries, graph-audit telemetry, JSON runtime memory telemetry, and
    `(runtime-memory-stats)` now expose the route/copy-debt counters.
  - Stable prepared graph traversal now keeps large temporary child-index
    buffers off recursive stack frames while preserving original edge ordering.
- Invalidated approach:
  - A reserved-edge placeholder rewrite reduced stack pressure but changed
    prepared-edge ordering/metadata semantics and failed stable array,
    dictionary, set, closure metadata tests plus the nested effect payload graph
    check. Preserve edge ordering when reducing stack use.
- Validation passed:
  - `c3c build --obj-out obj`
  - direct nested effect payload eval returned `true`
  - bounded container `memory-lifetime-smoke` (`255 passed, 0 failed`)
  - bounded container `basic` (`169 passed, 0 failed`)
  - multi-feature instrumentation build with `OMNI_BOUNDARY_INSTR_COUNTERS`,
    `OMNI_BOUNDARY_INSTR_TRACE`, and `OMNI_BOUNDARY_INSTR_BENCHMARK`
  - bounded container multi-feature `memory-lifetime-smoke`
    (`255 passed, 0 failed`) with route/copy-debt counters reporting
    `materialization_node_count=87` and `materialization_copy_bytes=10096`
  - normal rebuild after instrumentation validation plus bounded container
    Valgrind `memory-lifetime-smoke` (`255 passed, 0 failed`)
