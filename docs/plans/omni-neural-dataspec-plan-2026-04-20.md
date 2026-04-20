# Omni Neural DataSpec Plan

Date: 2026-04-20
Status: accepted design direction for `ML-VK-050`, `ML-VK-060`, and `ML-VK-070`

## Decision

The official high-level neural-network design is **Omni Neural DataSpec**.

An Omni neural network is a validated data specification plus explicit
parameter and state trees. High-level helpers may bundle these into a transparent
model value for ergonomics, but execution is always a function of data, not
hidden object identity.

The core model is:

- `spec`: architecture data;
- `params`: trainable tensor tree;
- `state`: non-trainable runtime state tree;
- `options`: dtype, device, mode, and seed for explicit initialization and
  placement.

`ml/*` remains the primitive Tensor math namespace. `nn/*` is the ergonomic
model/layer namespace built from data specs and functions over those specs.

## Why

The library should be easier to read than Lux-style explicit parameter/state
threading in ordinary user code, but it must not become a TensorFlow-style
mutable object graph.

Omni should use the Lisp/Clojure-friendly path:

- networks are inspectable data;
- parameters are inspectable data;
- state is inspectable data;
- execution is a function over those values;
- Vulkan execution follows explicit Tensor placement and capability reporting.

## Non-Negotiable Rules

- No hidden mutable layer objects.
- No class hierarchy or inheritance-based layer dispatch.
- No backend-specific public layer names such as `vulkan/dense`.
- No implicit CPU fallback for Vulkan operands.
- No implicit CPU/Vulkan transfer during training or inference.
- No build-on-first-call parameter creation as the primary contract.
- No hidden parameter registry.
- No hidden optimizer state.
- No per-Tensor ownership model outside the existing region/scope runtime model.

## Public Namespace Split

Use this split consistently:

- `ml/*`: primitive Tensor operations such as `ml/linear`, `ml/conv2d`,
  `ml/softmax`, and losses.
- `nn/*`: data-spec constructors, model lifecycle helpers, training helpers,
  checkpoint helpers, and summaries.

Layer constructors return normalized data, not opaque objects.

## Core Data Shapes

### Layer Spec

A layer spec is a normalized dictionary with a required `kind` field.

Example dense layer:

```lisp
(Dictionary
  'kind 'dense
  'input-features 784
  'output-features 128
  'use-bias true
  'activation 'relu
  'kernel-init (Dictionary 'kind 'kaiming-uniform)
  'bias-init (Dictionary 'kind 'zeros))
```

Example convolution layer:

```lisp
(Dictionary
  'kind 'conv2d
  'in-channels 3
  'out-channels 32
  'kernel [3 3]
  'stride [1 1]
  'padding [1 1]
  'dilation [1 1]
  'groups 1
  'use-bias true
  'activation 'relu
  'kernel-init (Dictionary 'kind 'kaiming-uniform)
  'bias-init (Dictionary 'kind 'zeros))
```

### Model Value

An initialized model is also data:

```lisp
(Dictionary
  'kind 'model
  'spec spec
  'params params
  'state state
  'mode 'eval
  'dtype Float32
  'device 'vulkan
  'metadata metadata)
```

The common path may accept this bundled model value:

```lisp
(nn/predict model input)
```

The explicit path must remain available:

```lisp
(nn/apply (nn/spec model)
          (nn/parameters model)
          (nn/state model)
          input
          (Dictionary 'mode 'eval 'device 'vulkan))
```

## Initial Public Surface

### Spec Constructors

- `nn/sequential`
- `nn/dense`
- `nn/conv1d`
- `nn/conv2d`
- `nn/max-pool2d`
- `nn/avg-pool2d`
- `nn/flatten`
- `nn/activation`
- `nn/relu`
- `nn/sigmoid`
- `nn/tanh`
- `nn/gelu`
- `nn/softmax`

### Lifecycle

- `nn/validate`
- `nn/init`
- `nn/spec`
- `nn/parameters`
- `nn/state`
- `nn/mode`
- `nn/with-parameters`
- `nn/with-state`
- `nn/train-mode`
- `nn/eval-mode`
- `nn/to-device`
- `nn/predict`
- `nn/apply`
- `nn/summary`

### Checkpoints

- `nn/save-spec`
- `nn/load-spec`
- `nn/save`
- `nn/load`

### Training

Training lands after `ML-VK-050` autograd and `ML-VK-060` optimizers:

- `nn/forward`
- `nn/grad`
- `nn/train-step`
- `nn/sgd`
- `nn/adam`
- `nn/adamw`
- `nn/rmsprop`

Training helpers return updated data values. They must not mutate hidden object
state.

## Vulkan Contract

Vulkan is first-class for both inference and training, but only through the
existing backend-neutral Tensor surface.

Required behavior:

- model initialization can place parameter tensors on Vulkan with
  `:device 'vulkan`;
- `nn/to-device` moves the model's parameter and state trees through explicit
  Tensor `to-device` semantics;
- `nn/predict` preserves Vulkan placement for supported Vulkan operations;
- `nn/forward` in training mode preserves Vulkan placement for supported
  forward operations and returns updated model data when state changes;
- `nn/grad` must either compute Vulkan gradients for the full supported path or
  fail closed before hidden CPU fallback;
- optimizer updates must keep parameter and optimizer-state placement explicit;
- mixed-device parameter groups fail closed unless the user requested an
  explicit transfer.

Broad backend capability bits must stay truthful:

- `ml-convolution` remains false until pooling lands;
- `ml-autograd` remains false until reverse-mode Tensor gradients exist for the
  supported training path;
- `ml-optimizer` remains false until optimizer state and parameter updates work
  for supported CPU/Vulkan parameter trees;
- `ml-graph-execution` remains false until graph capture and execution planning
  are real, validated behavior.

## Inference MVP

The first ergonomic inference MVP is complete when all of these are true:

1. `nn/sequential` validates and normalizes nested specs.
2. `nn/dense`, activation layers, `nn/flatten`, `nn/conv1d`, `nn/conv2d`,
   `nn/max-pool2d`, and `nn/avg-pool2d` lower to existing `ml/*` operations.
3. `nn/init` creates explicit parameter and state trees with deterministic
   initialization.
4. `nn/predict` runs CPU and Vulkan inference without hidden fallback.
5. `nn/parameters`, `nn/state`, and `nn/summary` expose inspectable data.
6. `nn/save` and `nn/load` round-trip spec, params, state, dtype, and placement
   metadata.
7. End-to-end examples cover an MLP and a small ConvNet on CPU and Vulkan.

## Training MVP

The first ergonomic training MVP is complete when all of these are true:

1. `ML-VK-050` supplies reverse-mode Tensor autograd for dense, activation,
   reduction, softmax, loss, convolution, and pooling paths included in the
   inference MVP.
2. `nn/grad` computes gradients over explicit model data.
3. `ML-VK-060` supplies SGD and AdamW with explicit optimizer state.
4. `nn/train-step` returns updated model and optimizer data.
5. Vulkan training either remains on Vulkan for the supported path or fails
   closed before CPU fallback.
6. Gradient finite-difference tests exist for CPU and Vulkan-supported paths.
7. A small MLP trains end-to-end on CPU and Vulkan-visible hosts.

## Implementation Slices

### `ML-VK-070-001` DataSpec Decision And Validators

Freeze the normalized dictionary schemas for:

- sequential;
- dense;
- activation;
- flatten;
- conv1d;
- conv2d;
- max-pool2d;
- avg-pool2d;
- model bundle;
- parameter tree;
- state tree.

Add `nn/validate` with strict diagnostics before any parameter allocation.

Shipped baseline:

- `nn/validate(spec)` validates normalized DataSpec dictionaries and returns
  the input spec on success.
- Invalid specs raise `nn/invalid-spec` with a diagnostic payload containing
  at least `path`, `expected`, and `actual-kind`.
- Data-only constructors are available for the frozen layer schemas:
  `nn/sequential`, `nn/dense`, `nn/conv1d`, `nn/conv2d`, `nn/max-pool2d`,
  `nn/avg-pool2d`, `nn/flatten`, `nn/activation`, `nn/relu`, `nn/sigmoid`,
  `nn/tanh`, `nn/gelu`, and `nn/softmax`.
- This baseline intentionally does not allocate parameter tensors, lower specs
  to `ml/*`, serialize checkpoints, or expose training helpers; those remain
  owned by `ML-VK-070-002` through `ML-VK-070-005`.

### `ML-VK-070-002` Parameter Initialization

`ML-VK-070-002` shipped.

`nn/init(spec [options])` is now the public lifecycle entrypoint for model
initialization. It accepts:

- mandatory `spec` (validated DataSpec)
- optional `options` Dictionary with explicit initialization controls:
  - `dtype`: `Float32` or `Float64`
  - `device`: `'cpu`, `'cuda`, or `'vulkan`
  - `seed`: non-negative integer
  - `mode`: `'eval` or `'train`

Runtime behavior:


- deterministic init families are supported for parameterized layers
  (`nn/dense`, `nn/conv1d`, `nn/conv2d`):
  - `zeros`
  - `ones`
  - `uniform`
  - `normal`
  - `xavier-uniform` / `glorot-uniform`
  - `kaiming-uniform` / `kaiming-normal`
- return value is a transparent model bundle with `spec`, `params`, `state`,
  `mode`, `dtype`, `device`, and `metadata`.
- placement is explicit and deterministic: parameter/state tensors are allocated in
  the requested dtype and device.

### `ML-VK-070-003` Inference Apply

Add `nn/apply`, `nn/predict`, and `nn/summary` for the inference MVP.

Lowering rules:

- dense lowers to `ml/linear`;
- conv1d lowers to `ml/conv1d`;
- conv2d lowers to `ml/conv2d`;
- pooling lowers to `ml/max-pool2d` and `ml/avg-pool2d`;
- activations lower to `ml/*` activation primitives;
- softmax lowers to `ml/softmax`.

### `ML-VK-070-004` Checkpoint Round Trip

Add spec/model save/load with dtype, shape, parameter paths, state paths, and
placement metadata.

### `ML-VK-070-005` Training Facade

After `ML-VK-050` and `ML-VK-060`, add:

- `nn/forward`;
- `nn/grad`;
- `nn/train-step`;
- optimizer constructors under `nn/*` that delegate to the optimizer suite.

## Validation

Each implementation slice must run:

- `c3c build`;
- focused advanced ML/Tensor tests;
- Vulkan-visible direct smokes where the host exposes Vulkan;
- Vulkan-unavailable fail-closed tests where applicable;
- `scripts/check_primitive_docs_parity.sh`;
- `scripts/check_file_size_gate.sh`;
- `git diff --check`.

Training slices additionally require:

- gradient finite-difference checks;
- no-hidden-CPU-fallback probes;
- bounded-container ML validation slices.

## Relationship To Existing Roadmap

This plan refines `ML-VK-070` and constrains `ML-VK-050`/`ML-VK-060`.

It does not replace the Vulkan ML suite roadmap. It makes the layer/model design
official so implementation cannot drift into either:

- TensorFlow-style hidden mutable object graphs; or
- Lux-style user-facing parameter/state threading as the only normal path.
