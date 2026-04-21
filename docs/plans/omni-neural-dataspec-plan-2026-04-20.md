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

## Custom Kernel Surface

Custom kernels should follow the same data-oriented rule as models and
optimizers: the user creates inspectable data, and execution is explicit.

`Kernel` is the public type/value for user-defined backend kernels. The first
shipped surface validates and normalizes the data spec, reports `type-of` /
`is?` as `Kernel`, and still stores the value as ordinary Omni dictionary data.
Backend compilation, cache state, ABI checks, and actual launch semantics are
still future work:

```lisp
(define sgd-update
  (Kernel
    {'backend 'vulkan
     'operation 'elementwise
     'inputs [(Dictionary 'name 'params 'dtype Float32 'shape ['n])
              (Dictionary 'name 'grads 'dtype Float32 'shape ['n])]
     'outputs [(Dictionary 'name 'updated 'dtype Float32 'shape ['n])]
     'push {'learning-rate Float32 'weight-decay Float32}
     'workgroup [64 1 1]}))
```

Kernel fields should use Omni path and postfix-index conventions:

```lisp
sgd-update.inputs
sgd-update.inputs.[0]
sgd-update.inputs.[0].name
```

Use `ref` for dynamic keys or higher-order lookup, not as a hidden execution
surface. Kernel execution should remain explicit, for example:

```lisp
(kernel/run sgd-update
  {'params params-tensor 'grads grads-tensor}
  {'learning-rate (Float32 0.1) 'weight-decay (Float32 0.0)})
```

`kernel/run` is present as the explicit execution boundary. It now supports
checked helper-backed Vulkan runners: `operation 'scale-f32` for one dense
row-major `Float32` tensor input and a `scale` `Float32` push constant, plus
binary `add-f32`, `sub-f32`, `mul-f32`, `div-f32`, `min-f32`, and `max-f32`
for two same-shape dense row-major `Float32` tensor inputs, plus scalar
`add-scalar-f32`, `sub-scalar-f32`, `mul-scalar-f32`, `div-scalar-f32`,
`min-scalar-f32`, `max-scalar-f32`, `scalar-sub-f32`, and `scalar-div-f32`
for one dense row-major `Float32` tensor input and one `scalar` `Float32` push
constant, plus unary `abs-f32`, `neg-f32`, `sqrt-f32`, `identity-f32`,
`zero-f32`, `sin-f32`, `cos-f32`, `tan-f32`, `asin-f32`, `acos-f32`,
`atan-f32`, `sinh-f32`, `cosh-f32`, `tanh-f32`, `exp-f32`, `log-f32`,
`log10-f32`, and `normal-cdf-f32` for one dense row-major `Float32` tensor
input and no push constants.
`kernel/capture(kernel inputs push)` validates the same checked Vulkan
direct-helper Kernel families against runtime inputs and push data and returns a
single-node `kernel-graph` launch plan with backend, operation, family, dtype,
device, direct-helper execution, input/output names, concrete runtime shape,
push data, node data, and an invalidation key without executing the kernel.
That capture surface is not arbitrary graph execution: the current
ML-VK-080-015 work item is a narrow executable Vulkan `Float32` two-scalar-map
command-buffer batching slice, and it still must fail closed without hidden CPU
fallback or `(define [kernel] ...)` sugar.

Do not add `(define [kernel] ...)` declaration sugar. Named kernels should use
ordinary Omni binding over the value constructor:

```lisp
(define sgd-update (Kernel spec))
```

Do not make `Kernel` pretend to be a `Lambda`, do not overload ordinary
function invocation for kernel execution, and do not overload path or `ref`
access into execution.

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
- `nn/leaky-relu`
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
- `ml/save-optimizer`
- `ml/load-optimizer`

`nn/save-spec(spec [path])` serializes a non-model DataSpec to a JSON checkpoint
string, or writes that checkpoint to `path` and returns `Void`.
`nn/load-spec(source)` accepts either a checkpoint JSON string or a path, restores
the DataSpec, and re-runs `nn/validate`.

`nn/save(model [path])` does the same for transparent model bundles, including
`spec`, `params`, `state`, `mode`, `dtype`, `device`, and `metadata`.
Parameter tensors are serialized with explicit dtype, shape, original placement,
and flat data. Non-CPU tensors are copied through explicit `to-device` routes for
serialization and are restored to their recorded placement on load.
Malformed checkpoint envelopes, wrong payload families, unsupported tensor dtypes,
and invalid restored DataSpecs fail closed with `nn/invalid-spec`.

`ml/save-optimizer(spec state [path])` and `ml/load-optimizer(source)` use the
same checkpoint envelope for explicit optimizer data. The payload kind is
`optimizer`; load returns an ordinary `Dictionary` containing `kind`, `spec`, and
`state` after validating the envelope family, supported optimizer spec, and state
container.

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
3. `ML-VK-060` supplies `ml/optimizer-step` SGD momentum state plus Adam,
   AdamW, RMSProp, max-norm clipping, and optimizer checkpoint helpers.
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
  `nn/avg-pool2d`, `nn/flatten`, `nn/activation`, `nn/relu`,
  `nn/leaky-relu`, `nn/sigmoid`, `nn/tanh`, `nn/gelu`, and `nn/softmax`.
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

`ML-VK-070-003` shipped.

`nn/apply`, `nn/predict`, and `nn/summary` are implemented for inference in the
first public inference family:

- supports explicit inference over model data via `(nn/apply model input)` and
  `(nn/apply (nn/spec model) (nn/parameters model) (nn/state model) input)`
- accepts only an empty reserved options dictionary on the explicit `nn/apply`
  path, so future options cannot be silently ignored
- exposes `nn/predict(model input)` as the explicit evaluation-mode inference path
- lowers `dense`, `batch-normalization`, `conv1d`, `conv2d`, `max-pool2d`,
  `avg-pool2d`, `activation`, and `softmax` to the corresponding `ml/*`
  operations
- lowers `nn/flatten` using explicit CPU flattening
- returns inspection summary metadata through `nn/summary`
- no hidden CPU fallback in supported inference paths; unsupported flatten and
  convolution bias add paths are fail-closed through explicit backend errors

Lowering rules:

- dense lowers to `ml/linear`;
- conv1d lowers to `ml/conv1d`;
- conv2d lowers to `ml/conv2d`;
- pooling lowers to `ml/max-pool2d` and `ml/avg-pool2d`;
- activations lower to `ml/*` activation primitives;
- softmax lowers to `ml/softmax`.

### `ML-VK-070-004` Checkpoint Round Trip

Shipped: `nn/save-spec`, `nn/load-spec`, `nn/save`, and `nn/load` round-trip
specs and model bundles through checkpoint strings or paths. Model checkpoints
preserve `params`, `state`, dtype, shape, mode, metadata, and placement metadata
with no hidden device fallback. Optimizer checkpoints are covered by
`ML-VK-060-006` through `ml/save-optimizer` and `ml/load-optimizer`.

### `ML-VK-070-005` Training Facade

Shipped first dense training facade:

- `nn/forward` runs the same model or explicit `(spec params state input
  [options])` data shapes as `nn/apply` without requiring eval mode.
- train-mode `nn/batch-normalization` computes current-batch CPU dense
  row-major statistics, returns ordinary `nn-forward` data with updated running
  state/model, and leaves the original model immutable; explicit forward
  options accept `(Dictionary 'mode 'train)` for state threading without hidden
  mutation.
- `nn/grad(model input targets [options])` requires a train-mode model and
  builds gradients for direct dense specs or sequential dense-plus-activation
  specs by lowering to the existing CPU `ml/grad` linear MSE and linear
  softmax-cross-entropy contracts.
- `nn/train-step(model input targets optimizer-spec optimizer-state [options])`
  composes `nn/grad` with `ml/optimizer-step` and returns ordinary updated data:
  `model`, `parameters`, `optimizer-state`, `gradients`, `loss`, `loss-kind`,
  `output`, `input-gradient`, and nested gradient/optimizer result dictionaries.
- CUDA/Vulkan backward remains fail-closed through the underlying `ml/grad`
  contract; no hidden CPU fallback is introduced.

### `ML-VK-070-006` Optimizer Spec Constructors

Shipped data constructors for optimizer specs:

- `nn/sgd(learning-rate [options])` returns a validated ordinary SGD optimizer
  spec dictionary with optional `momentum`, `weight-decay`, and `clip-norm`.
- `nn/adam(learning-rate [options])` and
  `nn/adamw(learning-rate [options])` return validated ordinary Adam-family
  optimizer spec dictionaries with optional `beta1`, `beta2`, `epsilon`,
  `weight-decay`, and `clip-norm`.
- `nn/rmsprop(learning-rate [options])` returns a validated ordinary RMSProp
  optimizer spec dictionary with optional `alpha`, `epsilon`, `momentum`,
  `weight-decay`, and `clip-norm`.
- Constructors do not execute an optimizer step or own hidden state; returned
  dictionaries are consumed by `nn/train-step` or `ml/optimizer-step`.

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
