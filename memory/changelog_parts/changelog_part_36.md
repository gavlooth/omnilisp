- 2026-04-20 17:06 CEST: Omni Neural DataSpec inference checkpoint:
  - Closed `ML-VK-070-003`.
  - Added public `nn/apply`, `nn/predict`, `nn/summary`, `nn/spec`,
    `nn/parameters`, `nn/state`, and `nn/mode`.
  - `nn/apply` supports both transparent model bundles and explicit
    `(spec params state input [options])` data paths. The optional options
    dictionary is currently reserved and must be empty; non-empty options raise
    `nn/invalid-spec` rather than being ignored.
  - `nn/predict` requires `'eval` mode and delegates to the same apply path.
  - First inference lowering covers sequential, dense, conv1d, conv2d,
    max/avg pool2d, activation, softmax, and CPU flatten through the existing
    `ml/*` primitive layer.
  - `nn/summary` returns model metadata plus layer, parameter-tensor, and
    parameter-element counts, and malformed parameter trees fail as
    `nn/invalid-spec`.
  - CPU conv bias and flatten materialization are explicit first-lane limits;
    unsupported non-CPU/layout cases fail closed instead of hidden fallback.
  - Validation passed: `c3c build`, focused advanced collections
    `pass=1748 fail=0`, basic Lisp `pass=160 fail=0`, primitive docs parity,
    `git diff --check`, and file-size gate.

- 2026-04-20 17:35 CEST: Omni Neural DataSpec checkpoint round-trip:
  - Closed `ML-VK-070-004`.
  - Added `nn/save-spec` and `nn/load-spec` for non-model DataSpec checkpoint
    JSON strings or paths.
  - Added `nn/save` and `nn/load` for transparent model bundles, preserving
    `spec`, `params`, `state`, `mode`, `dtype`, `device`, `metadata`, tensor
    dtype, tensor shape, flat tensor data, and recorded tensor placement.
  - Non-CPU tensor restore uses explicit `to-device` placement; unsupported
    device routes still fail through existing backend checks.
  - Malformed checkpoint envelopes and payload-family mismatches fail closed
    with `nn/invalid-spec`.
  - Validation passed: `c3c build`, focused advanced collections
    `pass=1753 fail=0`, basic Lisp `pass=160 fail=0`, primitive docs parity,
    file-size gate, and `git diff --check`.

- 2026-04-20 18:21 CEST: Vulkan ML layer normalization:
  - Closed `ML-VK-040-001`.
  - Added `ml/layer-normalization(input axis [epsilon])` with positive finite
    epsilon, default `1e-5`, input-shape preservation, CPU `Float64`/`Float32`,
    and direct Vulkan `Float32`.
  - Added a dedicated Vulkan same-shape layer-normalization helper and shader
    pair instead of reusing rank-reducing reductions or falling back to CPU.
  - Added `ml-layer-normalization-float64`,
    `ml-layer-normalization-float32`, and broad `ml-normalization` capability
    reporting.
  - Vulkan Float64 and CUDA paths remain fail-closed until validated kernels
    exist for those placements.
  - Validation passed: `scripts/build_omni_chelpers.sh`, `c3c build`, direct
    CPU/Vulkan eval smokes, focused advanced collections `pass=1758 fail=0`,
    basic Lisp `pass=160 fail=0`, primitive docs parity, file-size gate, and
    `git diff --check`.

- 2026-04-20 18:58 CEST: Vulkan ML batch normalization:
  - Closed `ML-VK-040-002`.
  - Added public
    `ml/batch-normalization(input scale bias mean variance channel-axis [epsilon])`
    with explicit rank-1 affine/stat tensors instead of hidden mutable layer
    state.
  - CPU supports `Float64` and `Float32`; Vulkan supports direct dense
    row-major `Float32` with a dedicated seven-buffer helper/shader path.
  - Added `ml-batch-normalization-float64`,
    `ml-batch-normalization-float32`, and updated broad `ml-normalization`
    capability reporting.
  - Mixed CPU/Vulkan operands and Vulkan Float64 remain fail-closed before CPU
    fallback. Training-mode/current-batch-stat batch normalization is deferred
    to the state/autograd work.
  - Validation passed: shader compile and `spirv-val`,
    `scripts/build_omni_chelpers.sh`, `c3c build`, direct CPU/Vulkan eval
    smokes, focused advanced collections `pass=1764 fail=0`, basic Lisp
    `pass=160 fail=0`, primitive docs parity, file-size gate, and
    `git diff --check`.

## 2026-04-20 - ML-VK-040-003 Scaled Dot-Product Attention

- Implemented `ml/scaled-dot-product-attention(query key value [mask] [scale])`.
  - Shape contract: query `[... Q D]`, key `[... K D]`, value `[... K V]`,
    output `[... Q V]`, with matching batch prefixes.
  - Optional masks are additive logits masks, either shared `[Q K]` or batched
    `[..., Q K]`; custom scale must be positive and finite, otherwise the
    default is `1 / sqrt(D)`.
  - CPU supports `Float64` and `Float32` with max-shifted softmax.
  - Vulkan supports direct dense row-major `Float32` through a dedicated
    five-buffer helper/shader path and generated SPIR-V C embedding.
  - Added `ml-scaled-dot-product-attention-float64`,
    `ml-scaled-dot-product-attention-float32`, and broad `ml-attention`
    capability reporting.
  - Mixed CPU/Vulkan operands and Vulkan Float64 remain fail-closed before CPU
    fallback.
  - Validation passed: shader compile and `spirv-val`,
    `scripts/build_omni_chelpers.sh`, `c3c build`, direct CPU/Vulkan eval
    smokes, focused advanced collections `pass=1769 fail=0`, basic Lisp
    `pass=160 fail=0`, primitive docs parity, Stage 3 source parity, file-size
    gate, and `git diff --check`.

## 2026-04-20 - ML-VK-050-001 Data Gradient Primitive

- Implemented `ml/grad` as the first data-oriented gradient spec evaluator.
  - Supported spec kind: `linear-mean-squared-error`.
  - CPU supports dense row-major `Float64` and `Float32` tensors.
  - Return value is a Dictionary with `kind`, `source`, scalar `loss`, forward
    `output`, `input-gradient`, and `parameter-gradients` containing weights
    and bias gradient tensors.
  - Vulkan backward remains fail-closed before CPU fallback; broad
    `ml-autograd` stays false until tape-backed reverse-mode gradients cover
    the supported training path.
  - Validation passed: `c3c build`, direct CPU eval smokes, focused advanced
    collections `pass=1772 fail=0`, basic Lisp `pass=160 fail=0`, primitive
    docs parity, Stage 3 source parity, file-size gate, and `git diff --check`.

## 2026-04-20 - ML-VK-050-002 Activation Gradient Primitive

- Implemented `linear-activation-mean-squared-error` as the second `ml/grad`
  data spec.
  - CPU dense row-major gradients now cover linear-plus-activation MSE.
  - `relu` backward supports `Float64`/`Float32`.
  - `sigmoid`, `tanh`, and tanh-approximation `gelu` backward support `Float32`,
    matching the existing forward activation dtype policy.
  - CUDA/Vulkan backward stays fail-closed before CPU fallback; broad
    `ml-autograd` remains false.
  - Validation passed: `c3c build`, direct CPU eval smokes, focused advanced
    collections `pass=1775 fail=0`, basic Lisp `pass=160 fail=0`, primitive
    docs parity, Stage 3 source parity, file-size gate, and `git diff --check`.

## 2026-04-20 - ML-VK-050-003 Softmax Cross-Entropy Gradient Primitive

- Implemented `linear-softmax-cross-entropy` as the third `ml/grad` data spec.
  - CPU dense row-major `Float64`/`Float32` gradients now cover dense linear
    classifiers with stable last-axis softmax cross-entropy.
  - Probability targets are validated as finite, non-negative, and sum-to-one
    per class slice.
  - CUDA/Vulkan backward stays fail-closed before CPU fallback; broad
    `ml-autograd` remains false.
  - Validation passed: `c3c build`, direct CPU eval smokes, focused advanced
    collections `pass=1778 fail=0`, basic Lisp `pass=160 fail=0`, primitive
    docs parity, Stage 3 source parity, file-size gate, and `git diff --check`.

## 2026-04-20 - ML-VK-060-001 CPU SGD Step Primitive

- Implemented `ml/sgd-step(parameters gradients learning-rate)`.
  - The primitive returns immutable updated parameter trees and recurses through
    matching arrays and dictionaries.
  - Tensor leaves require dense row-major CPU `Float64` or `Float32`, matching
    dtype and shape, and a non-negative finite learning rate.
  - CUDA/Vulkan optimizer kernels stay fail-closed before CPU fallback; broad
    `ml-optimizer` remains false.
  - Owner rule update: the hard 700 LOC split/gate now applies only to code
    files; docs and operational artifacts are no longer hard-failed by the
    gate.
  - Validation passed: `c3c build`, direct CPU eval smokes, focused advanced
    collections `pass=1781 fail=0`, basic Lisp `pass=160 fail=0`, primitive
    docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check`.

## 2026-04-20 - ML-VK-060-002 Optimizer Step Primitive

- Implemented `ml/optimizer-step(spec parameters gradients state)`.
  - The first supported optimizer spec is `{'kind 'sgd 'learning-rate n}` with
    optional `momentum` and `weight-decay`.
  - The primitive returns a dictionary containing fresh `parameters` and
    explicit optimizer `state`; momentum state is stored as a `velocity`
    parameter tree.
  - CPU dense row-major `Float64`/`Float32` tensor leaves are supported through
    matching array/dictionary parameter, gradient, and velocity trees.
  - CUDA/Vulkan optimizer kernels stay fail-closed before CPU fallback; broad
    `ml-optimizer` remains false.
  - Validation passed: `c3c build`, direct CPU eval smokes, focused advanced
    collections `pass=1785 fail=0`, compiler slice `pass=278 fail=0`, basic
    Lisp `pass=160 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, and `git diff --check`.

## 2026-04-20 - ML-VK-060-003 Adam And AdamW Optimizer Step

- Implemented CPU `ml/optimizer-step(spec parameters gradients state)` support
  for `adam` and `adamw` specs.
  - Adam/AdamW state is explicit data: returned state contains
    `first-moment`, `second-moment`, and integer `step`.
  - Adam uses coupled weight decay; AdamW uses decoupled weight decay.
  - Supported spec fields are `learning-rate`, optional `beta1`, optional
    `beta2`, optional `epsilon`, and optional `weight-decay`, with range
    validation before execution.
  - CPU dense row-major `Float64`/`Float32` tensor leaves are supported through
    matching array/dictionary parameter, gradient, and moment trees.
  - CUDA/Vulkan optimizer kernels stay fail-closed before CPU fallback; broad
    `ml-optimizer` remains false.
  - Validation passed: `c3c build`, direct CPU eval smokes, focused advanced
    collections `pass=1788 fail=0`, compiler slice `pass=278 fail=0`, basic
    Lisp `pass=160 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, and `git diff --check`.

## 2026-04-20 - ML-VK-060-004 RMSProp Optimizer Step

- Implemented CPU `ml/optimizer-step(spec parameters gradients state)` support
  for `rmsprop` specs.
  - RMSProp state is explicit data: returned state contains `square-average`,
    integer `step`, and `velocity` when `momentum` is positive.
  - Supported spec fields are `learning-rate`, optional `alpha`, optional
    `epsilon`, optional `momentum`, and optional `weight-decay`, with range
    validation before execution.
  - The supported contract is uncentered RMSProp with coupled weight decay and
    optional momentum velocity.
  - CPU dense row-major `Float64`/`Float32` tensor leaves are supported through
    matching array/dictionary parameter, gradient, square-average, and velocity
    trees.
  - CUDA/Vulkan optimizer kernels stay fail-closed before CPU fallback; broad
    `ml-optimizer` remains false.
  - Validation passed: `c3c build`, direct CPU eval smokes, focused advanced
    collections `pass=1791 fail=0`, compiler slice `pass=278 fail=0`, basic
    Lisp `pass=160 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, and `git diff --check`.

## 2026-04-20 - ML-VK-060-005 Gradient Clipping

- Implemented CPU `ml/clip-gradients(gradients max-norm)`.
  - The primitive computes global L2 norm across explicit array/dictionary
    gradient trees and scales dense row-major `Float64`/`Float32` tensor leaves
    to the requested max norm.
  - `ml/optimizer-step(spec parameters gradients state)` now accepts optional
    `clip-norm`; clipping is applied before SGD, Adam, AdamW, or RMSProp state
    updates.
  - `max-norm` and `clip-norm` must be non-negative finite numbers.
  - CUDA/Vulkan clipping and optimizer kernels stay fail-closed before CPU
    fallback; broad `ml-optimizer` remains false.
  - Validation passed: `c3c build`, direct CPU eval smokes, focused advanced
    collections `pass=1795 fail=0`, compiler slice `pass=279 fail=0`, basic
    Lisp `pass=160 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, and `git diff --check`.

## 2026-04-20 - ML-VK-060-006 Optimizer Checkpoint Helpers

- Implemented CPU `ml/save-optimizer(spec state [path])` and
  `ml/load-optimizer(source)`.
  - Optimizer checkpoints wrap an ordinary `{kind spec state}` Dictionary in the
    existing checkpoint envelope with payload kind `optimizer`.
  - Save and load validate supported optimizer specs for SGD, Adam, AdamW, and
    RMSProp, including numeric range checks for learning rate, weight decay,
    clip norm, momentum, beta values, alpha, and epsilon.
  - Load rejects wrong payload families before returning ordinary data.
  - Checkpoint validation covers envelope/spec/state-container integrity;
    parameter-tree/device compatibility remains enforced by `ml/optimizer-step`.
  - Shared checkpoint path I/O errors now use generic checkpoint wording instead
    of hard-coded `nn/save` text.
  - Validation passed: `c3c build`, direct CPU eval smokes for Adam state
    string round trip, SGD path round trip, and invalid spec rejection, focused
    advanced collections `pass=1798 fail=0`, compiler slice `pass=281 fail=0`,
    basic Lisp `pass=160 fail=0`, primitive docs parity, Stage 3 source parity,
    code file-size gate, and `git diff --check`.
