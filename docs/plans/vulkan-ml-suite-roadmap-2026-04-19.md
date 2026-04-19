# Vulkan ML Suite Roadmap

Date: 2026-04-19
Status: active roadmap for TODO lanes `ML-VK-*`

## Decision

Build Omni's ML stack on the existing backend-neutral `Tensor` surface with
Vulkan as an explicit execution backend, not as a separate public tensor type.
The public API should name ML concepts, not the device:

- placement remains `(to-device tensor 'vulkan)`;
- capability discovery remains `tensor-backends`;
- unsupported Vulkan execution must fail closed with Tensor backend diagnostics;
- no ML operation may silently copy Vulkan inputs to CPU to satisfy a missing
  kernel;
- no public `VulkanTensor`, `vulkan/conv`, `vulkan/train`, or backend-flavored
  user-facing API should be introduced.

The Vulkan ML suite should be complete enough to cover practical inference and
training for dense neural networks before it is called complete.

## Scope

The suite covers:

- dense batched linear algebra needed by ML workloads;
- convolution, pooling, normalization, activation, softmax, and loss kernels;
- reverse-mode autodiff over Tensor expressions;
- optimizer kernels and parameter update semantics;
- model/layer surfaces and serialization;
- graph capture, fusion, command batching, and buffer reuse;
- benchmarking and validation against CPU oracles.

Out of scope for the first complete Vulkan ML suite:

- pointer-backed `BigInteger`, `BigFloat`, or `BigComplex` device execution;
- hidden mixed precision;
- sparse tensors unless a separate sparse Tensor contract lands;
- distributed training;
- backend-specific public operation names.

## Live Work Items

### `ML-VK-001` Capability Matrix And Contract Freeze

Define the ML capability table exposed through `tensor-backends` and the
backend-neutral public names for ML operations. The first deliverable is a
contract document plus tests proving unsupported ML operations fail closed on
Vulkan rather than falling back to CPU.

Required decisions:

- canonical namespace for layer and loss functions;
- which operations are inference-only versus training-capable;
- dtype support per operation for `Float32`, `Float64`, `Complex64`, and
  `Complex128`;
- exact behavior for mixed CPU/Vulkan operands and lazy Tensor operands;
- capability keys for operation families such as `ml-linear`, `ml-convolution`,
  `ml-autograd`, and `ml-optimizer`.

### `ML-VK-010` Batched Linear Algebra Foundation

Add the ML-scale dense linear algebra layer: batched matmul, batched bias add,
batched reductions, and a stable matrix multiply route suitable for fully
connected layers and attention blocks.

Acceptance:

- preserves placement and dtype;
- supports `Float32` first, then `Float64`;
- rejects unsupported layouts and mixed devices before dispatch;
- has no-LAPACK/no-CPU-fallback counters for Vulkan operands;
- includes shape/broadcast diagnostics matching CPU oracle behavior.

### `ML-VK-020` Neural Elementwise And Reduction Kernels

Add numerically stable Vulkan kernels for common neural-network scalar and
reduction operations:

- `relu`, `leaky-relu`, `sigmoid`, `tanh`, `gelu`;
- `exp`, `log`, `logsumexp`, `softmax`;
- `sum`, `mean`, `variance`, and axis reductions needed by normalization;
- cross-entropy and mean-squared-error loss primitives.

This lane must not reuse invalidated GLSL double-transcendental assumptions.
Float64 transcendental support needs a validated approximation or a separate
shader capability path. Float32 may land first when its shader path validates.

### `ML-VK-030` Convolution Pooling And Image Tensor Kernels

Add 1D and 2D convolution plus pooling for dense tensors:

- direct and/or im2col-backed convolution;
- stride, padding, dilation, groups, and batch/channel layout policy;
- max/average pooling;
- convolution backward kernels only after `ML-VK-050` defines gradient storage.

The first accepted layout must be explicit. Do not infer support for arbitrary
views or transposed layouts from dense row-major Tensor support.

### `ML-VK-040` Normalization And Attention Primitives

Add Vulkan kernels for normalization and attention building blocks:

- batch normalization and layer normalization;
- stable scaled dot-product attention;
- mask handling with fail-closed shape and dtype diagnostics;
- optional fused softmax/dropout/matmul only after the unfused oracle kernels
  pass.

### `ML-VK-050` Autograd Core

Define and implement reverse-mode autodiff over Tensor operations with explicit
device semantics:

- gradient tape representation and lifetime rules;
- Tensor gradient accumulation on CPU/Vulkan without hidden transfers;
- backward definitions for map, contract, matrix, convolution, normalization,
  softmax, and loss operations;
- clear fail-closed behavior when a forward Vulkan op lacks a Vulkan backward
  kernel.

Autograd must preserve the runtime memory model and must not introduce a
separate per-Tensor ownership system.

### `ML-VK-060` Optimizer Suite

Add backend-neutral optimizer surfaces with Vulkan parameter-update kernels:

- SGD with momentum;
- Adam and AdamW;
- RMSProp;
- gradient clipping;
- weight decay;
- parameter state initialization, checkpoint, and restore.

Optimizer state must keep dtype/device placement explicit and must reject
mixed-device parameter groups unless an explicit transfer step is requested.

### `ML-VK-070` Model Layer Library And Serialization

Add the first complete user-facing ML layer library:

- linear/dense layer;
- convolution layer;
- embedding layer;
- normalization layers;
- sequential/composition helpers;
- model parameter traversal;
- checkpoint serialization and loading.

The public layer surface should remain backend-neutral. Vulkan execution follows
from Tensor placement and reported backend capabilities.

### `ML-VK-080` Graph Capture Fusion And Memory Planning

Add optional execution graph capture for inference and training steps:

- operation DAG capture for Tensor expressions;
- command-buffer batching;
- kernel fusion for safe elementwise/reduction chains;
- device buffer reuse and lifetime planning;
- deterministic invalidation when shapes, dtypes, devices, or capability bits
  change.

This lane is performance work and must not change scalar or Tensor semantics.

### `ML-VK-090` Validation And Benchmark Suite

Add a Vulkan ML validation gate:

- CPU oracle comparisons for every supported ML op;
- gradient checks against finite differences for differentiable ops;
- no-hidden-CPU-fallback probes for Vulkan operands;
- availability-gated tests for Vulkan-visible and Vulkan-unavailable hosts;
- bounded container slices for ML workloads;
- benchmark fixtures for inference throughput, training step time, and memory
  reuse.

## Ordering

Implement in this order unless a blocker forces a narrower proof:

1. `ML-VK-001`
2. `ML-VK-010`
3. `ML-VK-020`
4. `ML-VK-030`
5. `ML-VK-050`
6. `ML-VK-060`
7. `ML-VK-070`
8. `ML-VK-040`
9. `ML-VK-080`
10. `ML-VK-090`

The ordering deliberately puts contracts, linear algebra, scalar kernels, and
autograd before higher-level model ergonomics. A usable ML suite depends on
correct gradient and optimizer semantics, not just inference kernels.

## Validation Gate

Each landed item must run:

- shader compile and `spirv-val` for new Vulkan shaders;
- `./scripts/build_omni_chelpers.sh`;
- `c3c build --obj-out obj`;
- direct Vulkan smokes for placement, dtype, shape, and diagnostics;
- focused advanced ML/Tensor test group;
- bounded-container focused ML/Tensor group;
- no-hidden-CPU-fallback counter checks where applicable;
- `scripts/check_file_size_gate.sh`;
- `git diff --check`.
