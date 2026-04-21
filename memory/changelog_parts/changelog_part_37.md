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
