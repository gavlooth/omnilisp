# Vulkan ML Suite Roadmap

Date: 2026-04-19
Status: roadmap record for `ML-VK-*`; no `ML-VK` TODO-backed live lane is
currently open in `TODO.md`.

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

Initial public operation names and capability inventory:

- `ml/linear` is the first frozen public ML operation name. It is
  backend-neutral and computes an affine dense projection:
  `input[..., in_features]` by `weights[out_features, in_features]`, with an
  optional `bias[out_features]`, producing `input[..., out_features]`.
- `tensor-backends` exposes `ml-linear`, `ml-convolution`, `ml-neural-map`,
  `ml-normalization`, `ml-attention`, `ml-autograd`, `ml-optimizer`, and
  `ml-graph-execution` for every backend entry.
- `ml-linear` is true for CPU dense `Float64` and `Float32` Tensor execution.
  It remains false for Vulkan, CUDA, and cuBLAS until backend kernels land.
  Other ML capability keys are explicit `false` until the named operation
  family has real backend kernels and fail-closed tests.
- `ml-linear-direct-float64` and `ml-linear-direct-float32` are narrow partial
  capabilities for same-dtype direct `ml/linear` execution. They are true for
  CPU and for Vulkan when matching dtype placement is available. The Vulkan
  route covers already-materialized concrete input and weight tensors, plus
  optional concrete bias, through Tensor `contract` plus broadcast `map`;
  Vulkan-only expressions may participate only when existing Tensor realization
  lowers them to concrete dense Vulkan storage without CPU fallback. These keys
  do not change the broad Vulkan `ml-linear` backend bit, and they do not imply
  arbitrary view support or mixed-device coverage.
- Non-CPU `ml/linear` operands fail closed with Tensor backend diagnostics
  before any implicit CPU materialization, except for the narrow
  `ml-linear-direct-float64`/`ml-linear-direct-float32` direct path above.

`ML-VK-001` is closed for the backend-neutral capability matrix and contract
freeze. New operation families should add truthful narrow capability keys and
fail-closed diagnostics under their own semantic items instead of reopening this
contract umbrella.

### `ML-VK-010` Batched Linear Algebra Foundation

Add the ML-scale dense linear algebra layer: batched matmul, batched bias add,
batched reductions, and a stable matrix multiply route suitable for fully
connected layers and attention blocks.

First executable split:

- `ML-VK-010-001`: implement the first direct concrete Vulkan `Float32`
  no-bias lowering path for `ml/linear` through the existing contract route.
  It must preserve Vulkan Tensor placement, keep `tensor-backends` truthful,
  and fail closed on mixed-device or unsupported-layout operands.
- `ML-VK-010-002`: add Vulkan `Float32` no-bias batched matmul CPU-oracle
  coverage, dtype/device preservation checks, shape diagnostics, and
  no-hidden-CPU-fallback regressions.
- `ML-VK-010-003`: add Vulkan `Float32` batched bias-add coverage with
  no-CPU-fallback regressions.
- `ML-VK-010-004`: freeze `ml/linear-batched-reduce` as the public
  batched-reduction surface for Vulkan `Float32` ml/linear before reducer
  coverage lands.
- `ML-VK-010-004-001`: implement Vulkan `Float32` batched-reduction coverage
  for `ml/linear-batched-reduce` with no-CPU-fallback regressions.
  - audit checks:
    - public docs updated for `ml/linear-batched-reduce` contract surface;
    - capability truth for `ml-linear-direct-float32`/`ml-linear` preserved during partial
      Vulkan rollout;
    - backend-unsupported diagnostics cover mixed-device, unsupported layout,
      and view-backed cases before any fallback path.
- `ML-VK-010-005`: decide and implement expression-backed Vulkan `ml/linear`
  lowering beyond already-materialized direct map results, or keep the
  concrete/view boundary explicit with permanent fail-closed tests for
  view-backed operands.
- `ML-VK-010-006`: shipped Vulkan `Float64` `ml/linear` and
  `ml/linear-batched-reduce` coverage through existing `contract` plus bias
  `map` paths with fail-closed mixed-device tests.

Acceptance:

- preserves placement and dtype;
- supports `Float32` first, then `Float64`;
- rejects unsupported layouts and mixed devices before dispatch;
- has no-LAPACK/no-CPU-fallback counters for Vulkan operands;
- includes shape/broadcast diagnostics matching CPU oracle behavior.

### `ML-VK-020` Neural Elementwise And Reduction Kernels

Add numerically stable Vulkan kernels for common neural-network scalar and
reduction operations:

- `ML-VK-020-001`/`002`/`003`: shipped `ml/relu` as the first public neural
  elementwise surface. It uses existing Tensor `map max x 0`, reports narrow
  `ml-neural-relu-float64`/`ml-neural-relu-float32` capability bits, preserves
  dtype/device placement, and keeps broad `ml-neural-map` false.
- `ML-VK-020-003-LR`: shipped `ml/leaky-relu(input [negative-slope])` through
  composed `Tensor` map kernels using `max(input, 0) + slope * min(input, 0)`.
  It supports `Float64` and `Float32`, preserves CPU/CUDA/Vulkan placement, and
  reports narrow `ml-neural-leaky-relu-float64`/`ml-neural-leaky-relu-float32`
  capability bits.
- `ML-VK-020-004`: shipped the remaining Float32 activation surfaces:
  canonical `ml/sigmoid`, `ml/tanh`, and tanh-approximation `ml/gelu`.
  These are `ml/*` activation surfaces, not aliases for bare scientific math
  primitives; they compose existing Tensor `map` kernels and preserve
  CPU/CUDA/Vulkan placement with no hidden CPU fallback.
- `ML-VK-020-005`: shipped the current Float64 transcendental activation
  policy as explicit fail-closed behavior. Float64 `ml/sigmoid`, `ml/tanh`,
  and `ml/gelu` capability bits stay false until validated approximations or
  backend transcendental kernels land.
- `ML-VK-020-006`: shipped canonical CPU `ml/sum`, `ml/mean`, and population
  `ml/variance` axis reductions as the real backend-neutral reduction surface.
  They accept an integer axis or array/proper-list multi-axis reductions, drop
  reduced axes, and preserve Float64/Float32 dtype.
- `ML-VK-020-006-MAX`: shipped canonical CPU `ml/max` as the axis maximum
  reducer needed by stable `logsumexp`/`softmax` max-shift paths. It shares
  the same axis parsing, result-shape, dtype preservation, and CUDA
  fail-closed contract as the other CPU reductions.
- `ML-VK-020-006-VK`: shipped real Vulkan Float64/Float32 axis-reduction
  kernels for `ml/sum`/`ml/mean`/`ml/variance` using a one-input reducer that
  preserves free axes.
- `ML-VK-020-006-VK-MAX`: shipped Vulkan Float64/Float32 axis-maximum
  reductions for `ml/max` through the same reduction helper, preserving
  Vulkan placement.
- `ML-VK-020-007-A`: shipped CPU `ml/logsumexp(input axes)` and
  `ml/softmax(input axis)` with max-shifted numerics, Float64/Float32 dtype
  preservation, no hidden CUDA/Vulkan CPU fallback, and focused large-logit
  regressions.
- `ML-VK-020-007-VK-LSE-F32`: shipped Vulkan Float32
  `ml/logsumexp(input axes)` through the reduction helper with a stable
  max-shifted shader. Vulkan Float64 `ml/logsumexp` remains fail-closed until
  a validated Float64 exp/log policy lands.
- `ML-VK-020-007-VK-SM-F32`: shipped Vulkan Float32
  `ml/softmax(input axis)` through a dedicated same-shape axis-normalization
  shader/helper. Vulkan Float64 `ml/softmax` and losses remain fail-closed
  until their dedicated kernels/policies land.
- `ML-VK-020-007-VK-MSE`: shipped Vulkan Float64/Float32
  `ml/mean-squared-error(predictions targets)` through a dedicated two-input
  scalar loss shader/helper. Mixed CPU/Vulkan operands fail closed before CPU
  fallback.
- `ML-VK-020-007-B`: shipped CPU `ml/cross-entropy(logits targets axis)` for
  same-shape probability/one-hot targets. It uses max-shifted log-softmax over
  one explicit class axis, averages over non-class positions, rejects
  non-normalized target slices, and does not accept class-index tensors.
- `ML-VK-020-007-VK-CE-F32`: shipped Vulkan Float32
  `ml/cross-entropy(logits targets axis)` through a dedicated fused loss
  shader that preserves Vulkan placement, target probability diagnostics, and
  the no-hidden-CPU-fallback contract. Vulkan Float64 remains fail-closed until
  a validated Float64 exp/log policy lands.
- `ML-VK-020-007-C`: shipped CPU `ml/mean-squared-error(predictions targets)`
  as a scalar Tensor loss for same-shape, same-dtype Float64/Float32 tensors,
  with explicit Tensor shape/dtype diagnostics and no hidden CUDA/Vulkan
  fallback.

Scope:

- `relu`, `leaky-relu`, `sigmoid`, `tanh`, `gelu`;
- `exp`, `log`, `logsumexp`, `softmax`;
- `sum`, `mean`, `variance`, `max`, and axis reductions needed by
  normalization;
- cross-entropy loss primitive.

This lane must not reuse invalidated GLSL double-transcendental assumptions.
Float64 transcendental support needs a validated approximation or a separate
shader capability path. Float32 may land first when its shader path validates.
`ML-VK-020` is closed for the current scoped neural elementwise, reduction,
softmax, and loss kernel contract. Future activation or loss expansion should
be opened as a new operation-specific item instead of keeping this parent open.

### `ML-VK-030` Convolution Pooling And Image Tensor Kernels

Add 1D and 2D convolution plus pooling for dense tensors:

- direct and/or im2col-backed convolution;
- stride, padding, dilation, groups, and batch/channel layout policy;
- max/average pooling;
- convolution backward kernels only after `ML-VK-050` defines gradient storage.

The first accepted layout must be explicit. Do not infer support for arbitrary
views or transposed layouts from dense row-major Tensor support.

`ML-VK-030-001` shipped `ml/conv1d(input kernel stride padding dilation groups)`
for dense row-major NCW input and OCK kernels. CPU supports `Float64`/`Float32`;
Vulkan supports direct dense `Float32` with no hidden CPU fallback. Broad
`ml-convolution` remains false until 2D convolution and pooling land.

`ML-VK-030-002` shipped `ml/conv2d(input kernel stride-height stride-width
padding-height padding-width dilation-height dilation-width groups)` for dense
row-major NCHW input and OIHW kernels. CPU supports `Float64`/`Float32`;
Vulkan supports direct dense `Float32` with no hidden CPU fallback. Broad
`ml-convolution` remains false until max/average pooling lands.

`ML-VK-030-003` shipped `ml/max-pool2d(input window-height window-width
stride-height stride-width padding-height padding-width)` and `ml/avg-pool2d`
for dense row-major NCHW input. CPU supports `Float64`/`Float32`; Vulkan
supports direct dense `Float32` with no hidden CPU fallback. Average pooling
excludes padding from the divisor and all-padding windows are rejected by
validation. Broad `ml-convolution` is now true for CPU and for Vulkan when the
direct Float32 dense convolution/pooling family is available; it still does not
claim arbitrary views, transposed layouts, or backward kernels.

### `ML-VK-040` Normalization And Attention Primitives

Add Vulkan kernels for normalization and attention building blocks:

- `ML-VK-040-001`: shipped `ml/layer-normalization(input axis [epsilon])`
  for CPU `Float64`/`Float32` and direct Vulkan `Float32`. It preserves input
  shape, normalizes each explicit axis slice, reports narrow
  `ml-layer-normalization-*` capability bits, and turns broad
  `ml-normalization` true when at least one dtype route exists.
- `ML-VK-040-002`: shipped
  `ml/batch-normalization(input scale bias mean variance channel-axis [epsilon])`
  for CPU `Float64`/`Float32` and direct Vulkan `Float32`/`Float64`. It preserves
  input shape, requires rank-1 scale/bias/mean/variance tensors matching the
  channel axis, reports narrow `ml-batch-normalization-*` capability bits, and
  keeps mixed CPU/Vulkan operands fail-closed before CPU fallback.
- `ML-VK-040-003`: shipped
  `ml/scaled-dot-product-attention(query key value [mask] [scale])` for CPU
  `Float64`/`Float32` and direct dense Vulkan `Float32`. It computes
  max-shifted attention over `[... Q D]`, `[... K D]`, and `[... K V]`, accepts
  optional additive `[Q K]` or batched masks, reports narrow
  `ml-scaled-dot-product-attention-*` capability bits, and keeps mixed
  CPU/Vulkan and Vulkan `Float64` paths fail-closed before CPU fallback.
- `ML-VK-040-TRAIN-BN-001`: shipped `nn/batch-normalization(channels
  channel-axis [options])` as the explicit stateful training layer. `nn/init`
  creates `scale`/`bias` params plus `running-mean`/`running-variance` state,
  `nn/apply` uses running stats for inference/eval lowering through
  `ml/batch-normalization`, and train-mode `nn/forward` computes current-batch
  CPU dense row-major stats while returning updated ordinary state/model data
  without hidden mutation. CUDA/Vulkan/current-batch training remains
  fail-closed until native backward/state kernels are implemented.
- `ML-VK-040-FUSED-ATTENTION-001`: closed after confirming the supported
  inference/eval attention contract already routes through a single direct
  dense Vulkan `Float32` attention shader and adding oracle coverage for
  additive and batched masks. Dropout/training attention remains outside the
  shipped `ml/scaled-dot-product-attention` surface rather than an open
  residual of `ML-VK-040`.

### `ML-VK-050` Autograd Core

Define and implement reverse-mode autodiff over Tensor operations with explicit
device semantics:

- `ML-VK-050-001`: shipped `ml/grad` for the first data-oriented gradient
  spec, `linear-mean-squared-error`. It supports CPU `Float64`/`Float32`,
  returns loss, forward output, input gradient, and weights/bias gradients as
  ordinary data, and keeps Vulkan backward fail-closed until real Vulkan
  gradient kernels exist.
- `ML-VK-050-002`: shipped `linear-activation-mean-squared-error` for `ml/grad`.
  It supports CPU dense row-major linear-plus-activation MSE gradients, `relu`
  and `leaky-relu` backward for `Float64`/`Float32`, and
  `sigmoid`/`tanh`/tanh-approximation `gelu` backward for `Float32`.
  `leaky-relu` accepts optional `negative-slope` in the gradient spec and
  defaults to `0.01`. CUDA/Vulkan backward remains fail-closed.
- `ML-VK-050-003`: shipped `linear-softmax-cross-entropy` for `ml/grad`. It
  supports CPU dense row-major `Float64`/`Float32` linear classifier gradients
  with stable last-axis softmax cross-entropy and validated probability targets.
  CUDA/Vulkan backward remains fail-closed until real device kernels exist.
- `ML-VK-050-005`: shipped the reusable gradient tape representation and
  lifetime contract for current `ml/grad` results. The returned `tape` is a
  versioned ordinary dictionary with `kind 'gradient-tape`,
  `policy 'metadata-only`, `owner 'scope-region`, false `retains-handles`,
  dtype/source metadata, optional activation metadata, and an ordinary `nodes`
  array. It does not retain native handles or execute generic accumulation.
- `ML-VK-050-006`: shipped CPU generic Tensor gradient accumulation for dense
  row-major `Float64`/`Float32` expression graphs under `ml/grad`
  `tensor-mean-squared-error` and `tensor-softmax-cross-entropy` specs.
  Reverse accumulation walks supported `map` and `contract` nodes, computes
  MSE and softmax-cross-entropy upstream gradients, rejects unsupported map
  backward rules, and requires a single unambiguous `wrt` leaf match.
- `ML-VK-050-007`: shipped explicit device-backward availability semantics.
  Tensor-expression `ml/grad` preflights non-CPU operands before `wrt` leaf
  matching so shipped Vulkan forward tensors without matching backward kernels
  fail closed under `tensor/backend-unsupported`. Broad backend `ml-autograd`
  remains false until a complete backend autograd family exists.
- `ML-VK-050-VK-BWD-001`: shipped the first native Vulkan backward kernel.
  `tensor-mean-squared-error` now supports dense row-major `Float32` Vulkan
  tensors when `wrt` is the concrete prediction tensor. Loss and
  input-gradient stay on Vulkan, the gradient is computed as
  `(prediction - targets) * (2 / element-count)` through native Vulkan map
  kernels, and the metadata-only tape records `device 'vulkan`.

Future native device backward work is split into operation-specific items.
`ML-VK-050-VK-MAP-BWD-001` shipped graph-preserving Vulkan map-expression
backward for same-shape dense row-major `Float32` `map +`, `map -`, and
`map *` under MSE. `ML-VK-050-VK-SOFTMAX-CE-BWD-001` shipped native Vulkan
`tensor-softmax-cross-entropy` backward for direct logits and same-shape map
provenance. The implementation keeps normal Vulkan `map` eager, records
scope-safe provenance on concrete map results, and consumes that provenance in
native Vulkan loss backward paths. `ML-VK-050-VK-MAP-BCAST-BWD-001` then added
native Vulkan reduction of broadcasted map upstream gradients back to wrt
shapes for dense row-major `Float32` map provenance. Every Vulkan op without a
matching backward kernel must continue to fail closed.
- `ML-VK-050-VK-BWD-DIV-001`: shipped Vulkan `map /` backward rule for
  `tensor-mean-squared-error`. Left-path gradient is `upstream / right_operand`;
  right-path gradient computes `-output / right_operand` then multiplies by
  upstream. It uses existing generic Vulkan map primitives (`/`, `*`, unary `-`)
  without a dedicated division-backward shader, and supports dense row-major
  `Float32` and `Float64`.
- `ML-VK-050-VK-BWD-F64-001`: shipped native Vulkan `Float64` backward for
  `tensor-mean-squared-error` and `tensor-softmax-cross-entropy`. Scalar creation
  in backward helpers now uses `make_double` for `Float64` instead of hardcoded
  `make_float32`, and all dtype gates in `ml_grad_tensor_mse_try_vulkan` and
  `ml_grad_tensor_softmax_ce_try_vulkan` accept `TENSOR_DTYPE_DOUBLE`.
- `ML-VK-050-VK-BWD-MIXED-001`: shipped mixed-device auto-migration for Vulkan
  backward loss targets. When `ml/grad` `tensor-mean-squared-error` or
  `tensor-softmax-cross-entropy` has Vulkan predictions/logits and wrt but CPU
  targets, the backward path auto-migrates targets to Vulkan via
  `tensor_expr_resolve_vulkan_or_migrate` before computing gradients. Predictions
  and wrt remain strict Vulkan requirements to preserve provenance. Mixed-device
  auto-migration keeps gradients on-device and avoids silent CPU fallback.
- `ML-VK-050-VK-BWD-MINMAX-001`: shipped `map min` and `map max` backward rules
  for `tensor-mean-squared-error`. CPU Tensor-expression gradients now apply
  comparison-mask derivatives, and Vulkan dense row-major `Float32`/`Float64`
  gradients reuse existing binary map comparison mask ops so gradients stay on
  device without adding a dedicated backward shader.

Autograd must preserve the runtime memory model and must not introduce a
separate per-Tensor ownership system.

### `ML-VK-060` Optimizer Suite

Add backend-neutral optimizer surfaces with Vulkan parameter-update kernels:

- `ML-VK-060-001`: shipped CPU `ml/sgd-step(parameters gradients learning-rate)`
  for immutable data-oriented parameter trees. It recurses through arrays and
  dictionaries, updates dense row-major `Float64`/`Float32` tensor leaves, keeps
  CUDA/Vulkan optimizer kernels fail-closed, and leaves broad `ml-optimizer`
  false until optimizer state and device kernels exist.
- `ML-VK-060-002`: shipped CPU `ml/optimizer-step(spec parameters gradients
  state)` for data-oriented SGD specs with optional momentum and weight decay.
  It returns explicit updated `parameters` plus optimizer `state`, initializes
  and consumes momentum velocity trees, and keeps CUDA/Vulkan optimizer kernels
  fail-closed.
- `ML-VK-060-003`: shipped CPU `ml/optimizer-step(spec parameters gradients
  state)` for `adam` and `adamw` specs with explicit first-moment,
  second-moment, and step state. Adam uses coupled weight decay; AdamW uses
  decoupled weight decay. CUDA/Vulkan optimizer kernels stay fail-closed.
- `ML-VK-060-004`: shipped CPU `ml/optimizer-step(spec parameters gradients
  state)` for `rmsprop` specs with explicit square-average, optional momentum
  velocity, and step state. CUDA/Vulkan optimizer kernels stay fail-closed.
- `ML-VK-060-005`: shipped CPU `ml/clip-gradients(gradients max-norm)` and
  optional `clip-norm` on `ml/optimizer-step` specs. Gradients are max-norm
  clipped before optimizer state updates. CPU and all-Vulkan dense row-major
  `Float32` gradient trees now share clipping support through the narrow
  `ml-clip-gradients-float32` capability key.
- `ML-VK-060-006`: shipped CPU `ml/save-optimizer(spec state [path])` and
  `ml/load-optimizer(source)` for explicit optimizer spec/state checkpoint
  round trips through the existing checkpoint envelope. Checkpoint load
  revalidates the payload family, supported optimizer spec, and state container;
  optimizer tree/device compatibility remains enforced at `ml/optimizer-step`.
- `ML-VK-060-007`: shipped Vulkan dense row-major `Float32` SGD optimizer
kernels for all-Vulkan parameter/gradient/velocity leaves. Stateless SGD,
initial momentum velocity creation, and momentum velocity consumption preserve
Vulkan placement and explicit state. `tensor-backends` exposes the narrow
`ml-optimizer-sgd-float32` capability while broad `ml-optimizer` remains
false.
- `ML-VK-060-011`: shipped CUDA dense row-major `Float32` SGD optimizer
  execution backed by existing CUDA elementwise map kernels, with all-CUDA
  parameter/gradient/velocity leaves, optional momentum/velocity state,
  mixed-device fail-closed diagnostics, and narrow `ml-optimizer-sgd-float32`
  reporting when CUDA `Float32` map kernels are available.
- `ML-VK-060-012`: shipped CUDA dense row-major `Float32` map-backed Adam and
  AdamW execution in `ml/optimizer-step`, with explicit state/step preservation,
  all-CUDA placement, and mixed-device fail-closed semantics.
- `ML-VK-060-013`: shipped CUDA dense row-major `Float32` map-backed RMSProp
  execution in `ml/optimizer-step`, with explicit square-average/velocity state
  initialization and continuation, all-CUDA placement, and mixed-device
  fail-closed semantics.
- `ML-VK-060-008`: shipped Vulkan dense row-major `Float32` Adam and AdamW
  optimizer kernels for all-Vulkan parameter/gradient/first-moment/second-moment
  leaves. Initial moment creation and moment-state continuation preserve Vulkan
  placement, explicit step state, and Adam versus AdamW weight-decay semantics.
  `tensor-backends` exposes `ml-optimizer-adam-float32` and
  `ml-optimizer-adamw-float32` while broad `ml-optimizer` remains false.
- `ML-VK-060-009`: shipped Vulkan dense row-major `Float32` RMSProp optimizer
  kernels for all-Vulkan parameter/gradient/square-average/velocity leaves.
  Missing square-average or velocity state initializes from zero, matching the
  CPU explicit-state contract; output square-average and optional velocity state
  preserve Vulkan placement. `tensor-backends` exposes
  `ml-optimizer-rmsprop-float32` while broad `ml-optimizer` remains false.
- `ML-VK-060-010`: shipped Vulkan dense row-major `Float32` gradient clipping
  for all-vulkan `ml/clip-gradients` trees through `ml-clip-gradients-float32`,
  and optimizer-step `clip-norm` now clips before Vulkan updates through the same
  path.
- `ML-VK-060-014`: `ml/sgd-step(parameters gradients learning-rate)` now
  reuses the existing stateless CUDA/Vulkan dense row-major `Float32` SGD
  kernels for all-device parameter and gradient leaves. The primitive preserves
  CUDA/Vulkan placement when `ml-optimizer-sgd-float32` is available and keeps
  mixed CPU/device leaves fail-closed instead of silently transferring tensors.
- `ML-VK-060-F64-CAP-001`: fixed stale `ml-optimizer-{sgd,adam,adamw,rmsprop}-float64`
  capability bits for Vulkan from hardcoded `false` to `vulkan_float64_available`,
  aligning narrow capability truth with actual f64 optimizer shader availability.
- `ML-VK-060` is closed for the explicit-state optimizer suite: CPU, Vulkan
  dense row-major `Float32`, map-backed CUDA dense row-major `Float32`,
  checkpoint helpers, gradient clipping, and `nn/train-step` integration are
  shipped.
- Adam/AdamW optional moment state follows the same convention as SGD/RMSProp
  optional state: explicit `nil` `first-moment` and `second-moment` entries
  initialize fresh moments, while one-sided present moment state remains an
  invalid optimizer state. CPU, CUDA, and Vulkan dispatch share this contract.
- Vulkan SGD accepts `learning-rate 0.0` as a valid no-op update, matching the
  public non-negative optimizer spec contract and CPU/CUDA behavior.
- `ML-VK-060-FUSED-CUDA-001`: shipped native fused CUDA dense row-major
  `Float32` SGD execution for `ml/optimizer-step`. The helper computes weight
  decay, optional momentum velocity initialization/continuation, updated
  parameters, and updated velocity in one CUDA kernel, then falls back to the
  existing map-backed route only when the native optimizer module is
  unavailable.
- `ML-VK-060-FUSED-CUDA-STATEFUL-001`: open stateful CUDA fusion boundary for
  Adam, AdamW, and RMSProp. These remain map-backed until native multi-output
  state kernels and CUDA-host oracle coverage are added. Resolved native fused
  kernel failures must fail closed rather than falling back to map-backed
  execution.

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

Accepted design direction: `docs/plans/omni-neural-dataspec-plan-2026-04-20.md`
defines this layer as **Omni Neural DataSpec**. The `nn/*` surface uses
validated data specs, explicit parameter/state trees, and transparent model
bundles instead of hidden mutable layer objects. Vulkan inference and training
remain first-class through explicit Tensor placement, fail-closed unsupported
paths, and truthful `tensor-backends` capability reporting.

Shipped training facade slice:

- `ML-VK-070-005`: `nn/forward`, `nn/grad`, and `nn/train-step` now connect
  train-mode dense model data to the existing CPU `ml/grad` linear MSE/softmax
  CE contracts and `ml/optimizer-step`, returning updated model and optimizer
  data without hidden mutation.
- `ML-VK-070-006`: `nn/sgd`, `nn/adam`, `nn/adamw`, and `nn/rmsprop` now
  construct validated ordinary optimizer spec dictionaries for use with
  `nn/train-step` and `ml/optimizer-step`.
- `ML-VK-070-007`: `nn/leaky-relu` now constructs a default-slope
  `leaky-relu` activation DataSpec, `nn/validate` accepts it, and
  `nn/apply`/`nn/predict` lower it through `ml/leaky-relu`.

`ML-VK-070` is closed for the current backend-neutral model/layer library and
serialization contract. Future layer families should be added as their own
operation-specific items instead of reopening this umbrella.

### `ML-VK-080` Graph Capture Fusion And Memory Planning

Add optional execution graph capture for inference and training steps:

- operation DAG capture for Tensor expressions;
- command-buffer batching;
- kernel fusion for safe elementwise/reduction chains;
- custom `Kernel` values for user-defined backend kernels, constructed as
  explicit data specs and executed through an explicit kernel runner;
- no `(define [kernel] ...)` declaration sugar; named kernels use ordinary
  `(define name (Kernel spec))` bindings;
- device buffer reuse and lifetime planning;
- deterministic invalidation when shapes, dtypes, devices, or capability bits
  change.

This lane is performance and extension work. It must not change scalar or
Tensor semantics, must not turn `Kernel` values into ordinary `Lambda`
closures, and must not overload path access, postfix indexing, or `ref` into
kernel execution. Kernel specs should use ordinary Omni data, quoted symbol
keys, path access such as `k.inputs.[0].name`, and explicit execution.

`ML-VK-080` is closed for the checked graph capture, direct-SPIR-V ABI,
selected-region planning, and native selected-region execution boundary shipped
through `ML-VK-080-036`. Source-language parsing/compilation, arbitrary
descriptor schemas, memory-plan-backed runtime reuse, arbitrary mixed schedules,
and fused dispatch execution are future capability boundaries and should be
tracked as new semantic items, not residual children of this umbrella.

Shipped first slice:

- `ML-VK-080-001`: `Kernel(spec)` now validates a data-oriented kernel
  dictionary, normalizes `kind` to `'kernel`, exposes `type-of` / `is?`
  as `Kernel`, and preserves ordinary dictionary/path access. `kernel/run`
  is public and explicit.
- `ML-VK-080-002`: `kernel/run` now executes checked helper-backed Vulkan
  `scale-f32` kernels over dense row-major `Float32` tensors. Specs must use
  one input descriptor, one output descriptor, matching shapes, and a `scale`
  `Float32` push constant; CPU, mixed-device, unsupported dtype, missing push,
  and unsupported operation paths fail closed.
- `ML-VK-080-003`: `kernel/run` now also executes checked helper-backed Vulkan
  `add-f32` kernels over two same-shape dense row-major `Float32` tensor inputs.
  Specs must use two input descriptors, one output descriptor, matching shapes,
  and no push constants; CPU, mixed-device, unsupported dtype, unexpected push,
  and shape-mismatch paths fail closed.
- `ML-VK-080-004`: the checked helper-backed binary `Float32` Kernel family now
  includes `sub-f32`, `mul-f32`, `div-f32`, `min-f32`, and `max-f32` in addition
  to `add-f32`, all with two same-shape dense row-major Vulkan tensor inputs,
  one matching output descriptor, and no push constants.
- `ML-VK-080-005`: `kernel/run` now executes checked helper-backed scalar
  `Float32` kernels with one dense row-major Vulkan tensor input and one
  `scalar` `Float32` push constant. Tensor-scalar operations are
  `add-scalar-f32`, `sub-scalar-f32`, `mul-scalar-f32`, `div-scalar-f32`,
  `min-scalar-f32`, and `max-scalar-f32`; scalar-left noncommutative operations
  are `scalar-sub-f32` and `scalar-div-f32`.
- `ML-VK-080-006`: `kernel/run` now executes checked helper-backed unary
  `Float32` kernels with one dense row-major Vulkan tensor input, one matching
  output descriptor, and no push constants. Operations are `abs-f32`,
  `neg-f32`, `sqrt-f32`, `identity-f32`, `zero-f32`, `sin-f32`, `cos-f32`,
  `tan-f32`, `asin-f32`, `acos-f32`, `atan-f32`, `sinh-f32`, `cosh-f32`,
  `tanh-f32`, `exp-f32`, `log-f32`, `log10-f32`, and `normal-cdf-f32`.
- `ML-VK-080-007`: `kernel/capture` now validates the checked helper-backed
  Vulkan `Float32` Kernel families against runtime inputs and push data and
  returns a single-node `kernel-graph` launch plan. The plan records backend,
  operation, family, dtype, device, direct-helper execution, input/output names,
  concrete runtime shape, push data, node data, and an invalidation key without
  launching the kernel.
- `ML-VK-080-008`: `tensor/capture(source)` now returns a non-executing
  `tensor-graph` plan for all-Vulkan `Float32` concrete/map Tensor expression
  graphs. Explicit `to-device 'vulkan` preserves supported CPU lazy `Float32`
  map graphs as Vulkan Tensor expressions so capture can record multi-node
  source/map DAG data without launching the graph.
- `ML-VK-080-009`: `tensor/capture(source)` now also records all-Vulkan
  `Float32` contract Tensor expression graphs. Explicit `to-device 'vulkan`
  preserves supported CPU lazy `Float32` contract graphs as Vulkan Tensor
  expressions, and contract nodes record input ids, left/right axes, output
  shape, dtype/device/backend, and invalidation data without launching the
  graph.
- `ML-VK-080-010`: `tensor/capture(source)` now records direct all-Vulkan
  `Float32` transpose-view graph nodes. This is limited to the existing
  rank-2 `matrix/transpose-view` boundary over concrete dense Vulkan `Float32`
  storage; arbitrary strided views, batching, and fusion remain fail-closed.
- `ML-VK-080-011`: `tensor/capture(source)` now includes descriptive
  topological schedule metadata, per-node execution classes, graph
  `launch-count`, and `fusion 'none`. This is planning metadata only; it does
  not launch graph nodes.
- `ML-VK-080-012`: `tensor/capture(source)` now includes non-executing
  command-batch planning metadata for launchable `direct-helper` schedule
  steps. Launchable graphs record `command-batching 'metadata` with one serial
  `command-buffer-candidate` batch descriptor; non-launching graphs record
  `command-batching 'none`. This does not allocate, record, submit, or execute
  Vulkan command buffers, and it does not fuse graph nodes.
- `ML-VK-080-013`: `tensor/capture(source)` now records a nested
  metadata-only `memory-plan` dictionary with kind `tensor-memory-plan`,
  `version`, `backend`, `dtype`, `policy`, `allocates`, `retains-handles`,
  `external-bytes`, `transient-bytes`, and `node-memory`. Capture nodes record
  `element-count`, `byte-length`, `storage-offset`, `storage-elements`,
  `storage-bytes`, `allocation`, `owner`, and `write-policy`. This slice is
  descriptive only; it does not allocate, retain handles, or reuse runtime
  buffers, and the top-level capture result remains a `tensor-graph`.
- `ML-VK-080-015`: shipped executable Vulkan Float32 two-scalar-map
  command-buffer batching for the narrow expression materialization path. The
  executor records two scalar map dispatches into one Vulkan command buffer,
  inserts one shader write/read barrier, and submits once during `realize` of
  an explicit `to-device 'vulkan` graph expression.
- Next runtime boundaries are source-backed custom `Kernel`
  compilation/dispatch, contracted buffer reuse/lifetime planning, and general
  captured Tensor graph execution (`ML-VK-080-018`). Each requires additional
  native/runtime execution plumbing, so they remain separate from the narrow
  batching slice.

### `ML-VK-090` Validation And Benchmark Suite

Add a Vulkan ML validation gate:

- CPU oracle comparisons for every supported ML op;
- gradient checks against finite differences for differentiable ops;
- no-hidden-CPU-fallback probes for Vulkan operands;
- availability-gated tests for Vulkan-visible and Vulkan-unavailable hosts;
- bounded container slices for ML workloads;
- benchmark fixtures for inference throughput, training step time, and memory
  reuse.

Progress:

- `ML-VK-090-001` adds CPU finite-difference regressions for the shipped
  `ml/grad` contracts. The focused tests perturb public losses through
  `ml/linear` plus `ml/mean-squared-error` or `ml/cross-entropy`, then compare
  the central numeric gradient with analytic `ml/grad` input/weight gradients.
- `ML-VK-090-002` adds no-hidden-CPU-fallback diagnostic probes for mixed
  Vulkan ML trees. The focused tests assert that mixed Vulkan/CPU
  `ml/clip-gradients` and `ml/optimizer-step` failures expose the relevant
  all-Vulkan placement contract in the error message instead of collapsing to a
  generic backend failure.
- `ML-VK-090-003` adds the bounded ML validation entrypoint and the first ML
  benchmark fixture. `scripts/run_ml_validation_slice.sh` runs the focused
  advanced collections ML/Tensor group through the Docker validation path, and
  `OMNI_ML_BENCH=1` emits `ml_inference_oracle` and
  `ml_training_step_oracle` summaries with inference, optimizer-step, and
  runtime memory reuse counters.
- `ML-VK-090-004` adds the explicit ML Vulkan operation-family
  availability-gate regression. Vulkan-visible hosts must keep Float32 ML
  capability bits tied to actual Float32 placement across linear, activation,
  reduction, normalization, attention, convolution/pooling, optimizer, and
  gradient clipping families, and run a device-resident `ml/linear` smoke.
  Vulkan-unavailable hosts must report false ML Vulkan capability bits and
  fail closed with `tensor/backend-unavailable` for `to-device 'vulkan`.

`ML-VK-090` is closed for the current Vulkan ML validation gate: CPU oracle
coverage, shipped-gradient finite differences, no-hidden-CPU-fallback probes,
Vulkan visible/unavailable host gates, bounded focused validation, and the
first benchmark fixture are all in place. Future broader validation expansion
should be tracked as a new item for the specific operation family or benchmark
dimension it adds, not as a residual under `ML-VK-090`.

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
