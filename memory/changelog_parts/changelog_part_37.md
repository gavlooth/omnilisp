# Changelog Part 37

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
