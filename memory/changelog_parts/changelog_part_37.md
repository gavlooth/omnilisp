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
