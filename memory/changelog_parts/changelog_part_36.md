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

## 2026-04-20 - ML-VK-060-007 Vulkan Float32 SGD Optimizer

- Implemented all-Vulkan dense row-major `Float32` SGD support for
  `ml/optimizer-step(spec parameters gradients state)`.
  - Added Vulkan compute shaders and C helper plumbing for stateless SGD,
    initial momentum velocity creation, and momentum velocity consumption.
  - Added `src/lisp/prim_ml_optimizer_vulkan.c3` so SGD tensor leaves intercept
    all-Vulkan operands before CPU materialization.
  - Parameter and velocity outputs preserve Vulkan placement; mixed CPU/Vulkan
    optimizer leaves fail closed before hidden fallback.
  - `tensor-backends` now reports narrow `ml-optimizer-sgd-float32` support for
    CPU and Vulkan where `Float32` placement is available. Broad `ml-optimizer`
    remains false until the optimizer family is complete.
  - Adam, AdamW, RMSProp, clipping kernels, and CUDA optimizer kernels remain
    fail-closed.
  - Validation passed: GLSL compile for the three SGD shaders,
    `spirv-val --target-env vulkan1.0`, `scripts/build_omni_chelpers.sh`,
    `c3c build`, direct Vulkan SGD stateless and momentum smokes, direct Adam
    fail-closed smoke, focused advanced collections `pass=1803 fail=0`, code
    file-size gate, basic Lisp `pass=160 fail=0`, compiler slice
    `pass=281 fail=0`, primitive docs parity, Stage 3 source parity, and
    `git diff --check`.

## 2026-04-20 - ML-VK-060-008 Vulkan Float32 Adam And AdamW Optimizer

- Implemented all-Vulkan dense row-major `Float32` Adam and AdamW support for
  `ml/optimizer-step(spec parameters gradients state)`.
  - Added Vulkan compute shaders and C helper plumbing for initial first/second
    moment creation and prior-moment continuation.
  - The Vulkan path preserves the CPU optimizer state contract: explicit
    `first-moment`, `second-moment`, and integer `step` state with Adam coupled
    weight decay and AdamW decoupled weight decay.
  - Added C3 Vulkan dispatch before CPU materialization for all-Vulkan
    parameter/gradient/moment leaves. Mixed placement and non-`Float32` Vulkan
    leaves still fail closed.
  - `tensor-backends` now reports narrow `ml-optimizer-adam-float32` and
    `ml-optimizer-adamw-float32` support where Vulkan `Float32` placement is
    available. Broad `ml-optimizer` remains false.
  - RMSProp, clipping kernels, and CUDA optimizer kernels remain fail-closed.
  - Validation passed: GLSL compile and `spirv-val --target-env vulkan1.0` for
    the two Adam shaders, `scripts/build_omni_chelpers.sh`, `c3c build`, direct
    Vulkan Adam/AdamW smokes, focused advanced collections `pass=1807 fail=0`,
    basic Lisp `pass=160 fail=0`, compiler slice `pass=281 fail=0`, primitive
    docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check`.

## 2026-04-20 - ML-VK-060-009 Vulkan Float32 RMSProp Optimizer

- Implemented all-Vulkan dense row-major `Float32` RMSProp support for
  `ml/optimizer-step(spec parameters gradients state)`.
  - Added a Vulkan compute shader and C helper plumbing for square-average
    initialization, square-average continuation, optional velocity
    initialization, and velocity continuation.
  - The Vulkan path preserves the CPU explicit-state contract: missing
    `square-average` or `velocity` leaves initialize from zero, returned state
    contains `square-average`, and returned state contains `velocity` only when
    momentum is positive.
  - Added C3 Vulkan dispatch before CPU materialization for all-Vulkan
    parameter/gradient/square-average/velocity leaves. Mixed placement and
    non-`Float32` Vulkan leaves still fail closed.
  - `tensor-backends` now reports narrow `ml-optimizer-rmsprop-float32` support
    where Vulkan `Float32` placement is available. Broad `ml-optimizer` remains
    false.
  - Vulkan clipping and CUDA optimizer kernels remain fail-closed.
  - Validation passed: GLSL compile and `spirv-val --target-env vulkan1.0` for
    the RMSProp shader, `scripts/build_omni_chelpers.sh`, `c3c build`, direct
    Vulkan RMSProp smokes, focused advanced collections `pass=1809 fail=0`,
    basic Lisp `pass=160 fail=0`, compiler slice `pass=281 fail=0`, primitive
    docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check`.

## 2026-04-20 - ML-VK-060-010 Vulkan Float32 Gradient Clipping

- Implemented all-Vulkan dense row-major `Float32` gradient clipping for
  `ml/clip-gradients(gradients max-norm)` and optimizer `clip-norm`.
  - Added Vulkan compute shaders and C helper plumbing for sum-of-squares norm
    accumulation and scaled gradient output.
  - Added C3 dispatch before CPU materialization for all-Vulkan gradient trees.
    Mixed CPU/Vulkan leaves and non-`Float32` Vulkan leaves fail closed with
    `tensor/backend-unsupported`.
  - `ml/optimizer-step` now applies `clip-norm` before Vulkan SGD, Adam, AdamW,
    and RMSProp updates through the same `ml/clip-gradients` path.
  - `tensor-backends` now reports narrow `ml-clip-gradients-float32` support
    where Vulkan `Float32` placement is available. Broad `ml-optimizer` remains
    false.
  - CUDA optimizer kernels and `nn/train-step` integration remain open.
  - Validation passed: GLSL compile and `spirv-val --target-env vulkan1.0` for
    the clip shaders, `scripts/build_omni_chelpers.sh`, `c3c build`, direct
    Vulkan clipping and clip-norm smokes, focused advanced collections
    `pass=1811 fail=0`, basic Lisp `pass=160 fail=0`, compiler slice
    `pass=281 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, and `git diff --check`.

## 2026-04-20 - ML-VK-060-011 CUDA Float32 SGD Optimizer

- Implemented all-CUDA dense row-major `Float32` SGD support for
  `ml/optimizer-step(spec parameters gradients state)`.
  - Added `src/lisp/prim_ml_optimizer_cuda.c3` and routed CUDA tensor leaves
    before Vulkan/CPU fallback in `ml_optimizer_step_tensor`.
  - The CUDA route is map-backed: it composes existing CUDA elementwise map
    kernels for optional weight decay, optional momentum/velocity state,
    learning-rate scaling, and parameter subtraction.
  - Parameter and velocity outputs preserve CUDA placement; mixed CPU/CUDA
    leaves, non-`Float32` CUDA leaves, non-dense layouts, empty leaves, and
    missing CUDA backing storage fail closed before hidden CPU fallback.
  - `tensor-backends` now reports narrow `ml-optimizer-sgd-float32` support
    when CUDA `elementwise-map-float32` is available. Broad `ml-optimizer`
    remains false until the optimizer family is complete.
  - Fused CUDA optimizer kernels beyond map-backed SGD, CUDA Adam/AdamW/RMSProp,
    and `nn/train-step` integration remain open.
  - Validation passed: `scripts/build_omni_chelpers.sh`, `c3c build`, direct
    CUDA capability and SGD smokes, focused advanced collections
    `pass=1815 fail=0`, basic Lisp `pass=160 fail=0`, compiler slice
    `pass=281 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, and `git diff --check`.

## 2026-04-20 - ML-VK-060-012/013 CUDA Map-Backed Adam AdamW RMSProp

- Implemented all-CUDA dense row-major `Float32` Adam, AdamW, and RMSProp
  support for `ml/optimizer-step(spec parameters gradients state)`.
  - Added CUDA dispatch before Vulkan/CPU fallback for Adam/AdamW moment leaves
    and RMSProp square-average/velocity leaves.
  - The CUDA route is map-backed: it composes existing CUDA elementwise map
    kernels, including `sqrt`, rather than adding fused CUDA PTX kernels.
  - Adam and AdamW preserve explicit `first-moment`, `second-moment`, and
    integer `step` state, including Adam coupled weight decay versus AdamW
    decoupled weight decay.
  - RMSProp preserves explicit `square-average`, optional `velocity`, and
    integer `step` state, including missing-state initialization from zero.
  - Mixed CPU/CUDA leaves, non-`Float32` CUDA leaves, non-dense layouts, empty
    leaves, and missing CUDA backing storage fail closed before hidden CPU
    fallback.
  - `tensor-backends` now reports narrow `ml-optimizer-adam-float32`,
    `ml-optimizer-adamw-float32`, and `ml-optimizer-rmsprop-float32` support
    when CUDA `elementwise-map-float32` is available. Broad `ml-optimizer`
    remains false.
  - Validation passed: `scripts/build_omni_chelpers.sh`, `c3c build`, direct
    CUDA Adam/AdamW/RMSProp smokes, focused advanced collections
    `pass=1823 fail=0`, basic Lisp `pass=160 fail=0`, compiler slice
    `pass=281 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, and `git diff --check`.

## 2026-04-21 - ML-VK-070-005 Dense NN Training Facade

- Implemented `nn/forward`, `nn/grad`, and `nn/train-step`.
  - `nn/forward` delegates through `nn/apply` and supports the same model or
    explicit `(spec params state input [options])` data arities without
    requiring eval mode.
  - `nn/grad` requires train-mode model bundles and supports direct dense or
    sequential dense-plus-activation specs. It lowers to the existing CPU
    `ml/grad` linear mean-squared-error and linear softmax cross-entropy
    contracts, then lifts parameter gradients back into the model parameter-tree
    shape.
  - `nn/train-step` composes `nn/grad` with `ml/optimizer-step` and returns
    ordinary updated data: `model`, `parameters`, `optimizer-state`,
    `gradients`, `loss`, `loss-kind`, `output`, `input-gradient`, and nested
    gradient/optimizer results.
  - CUDA/Vulkan backward remains fail-closed through the underlying `ml/grad`
    contract; no hidden CPU fallback or hidden mutable model state was added.
  - Follow-up shipped in `ML-VK-070-006`: `nn/sgd`, `nn/adam`, `nn/adamw`, and
    `nn/rmsprop` optimizer spec constructors.

## 2026-04-21 - ML-VK-070-006 Optimizer Spec Constructors

- Implemented `nn/sgd`, `nn/adam`, `nn/adamw`, and `nn/rmsprop`.
  - Constructors return ordinary optimizer spec dictionaries consumable by
    `nn/train-step` and `ml/optimizer-step`; no hidden optimizer state or
    mutation is introduced.
  - `learning-rate` must be finite and non-negative.
  - Common optional fields are `weight-decay` and `clip-norm`, both finite and
    non-negative.
  - SGD accepts optional `momentum` in `[0, 1)`.
  - Adam and AdamW accept optional `beta1` / `beta2` in `[0, 1)` and positive
    finite `epsilon`.
  - RMSProp accepts optional `alpha` and `momentum` in `[0, 1)` plus positive
    finite `epsilon`.
  - Unknown option keys fail closed with `nn/invalid-spec`.
  - Validation passed: `scripts/build_omni_chelpers.sh`, `c3c build`, direct
    constructor and invalid-range smokes, focused advanced collections
    `pass=1831 fail=0`, compiler slice `pass=285 fail=0`, basic Lisp
    `pass=160 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, and `git diff --check`.

## 2026-04-21 - ML-VK-080-001 Kernel Value Surface

- Implemented the first data-oriented custom `Kernel` surface.
  - `Kernel(spec)` validates recognized spec keys, required backend/operation,
    input/output descriptors, optional push constants, and optional workgroup
    dimensions.
  - Constructed Kernel values normalize `kind` to `'kernel`, remain ordinary
    dictionary data for path/index/ref access, and report `type-of` / `is?`
    as `Kernel`.
  - Added `kernel/run(kernel inputs push)` as the explicit execution boundary;
    it currently fails closed with `tensor/backend-unsupported` until backend
    compilation and launch semantics exist.
  - Fixed postfix parsing so indexed data can continue through symbol-key
    access, for example `rows.[0].name` and `k.inputs.[0].name`, without
    reintroducing removed leading-dot accessors.
  - Validation passed: `c3c build`, direct eval smokes for Kernel type/path
    access and fail-closed `kernel/run`, focused advanced collections exit 0,
    compiler slice `pass=287 fail=0`, basic Lisp `pass=161 fail=0`, primitive
    docs parity, Stage 3 source parity, code file-size gate, and
    `git diff --check`.

## 2026-04-21 - ML-VK-080-002 Checked Vulkan Kernel Runner

- Implemented the first real `kernel/run` execution route.
  - Supported spec: `backend 'vulkan`, `operation 'scale-f32`.
  - Contract: one input descriptor, one output descriptor, `Float32` descriptor
    dtypes, matching descriptor shapes, dense row-major Vulkan `Float32`
    runtime storage, and push key `scale` representable as `Float32`.
  - Execution uses the existing checked Vulkan Float32 scale helper and returns
    an ordinary dictionary keyed by the output descriptor name.
  - CPU placement, mixed placement, unsupported dtype, empty tensor, shape
    mismatch, missing push data, and unsupported operations fail closed without
    hidden CPU fallback.
  - Arbitrary user source compilation, graph capture, command-buffer batching,
    fusion, device-buffer reuse/lifetime planning, deterministic invalidation,
    and `(define [kernel] ...)` sugar remain out of scope.
  - Validation passed: `c3c build`, direct eval smokes, focused advanced
    collections `pass=1837 fail=0`, basic Lisp `pass=161 fail=0`, compiler
    slice `pass=287 fail=0`, primitive docs parity, Stage 3 source parity,
    code file-size gate, and `git diff --check`.

## 2026-04-21 - ML-VK-080-003 Checked Vulkan Add Kernel Runner

- Implemented the second checked Vulkan `kernel/run` execution route.
  - Supported spec: `backend 'vulkan`, `operation 'add-f32`.
  - Contract: two input descriptors, one output descriptor, `Float32`
    descriptor dtypes, empty spec/runtime push dictionaries, matching
    descriptor shapes, same-shape runtime tensors, and dense row-major Vulkan
    `Float32` storage for both inputs.
  - Execution uses the existing checked Vulkan Float32 map helper in add mode
    and returns an ordinary dictionary keyed by the output descriptor name.
  - Shared Kernel runner helpers now cover output dictionaries and
    descriptor-based Vulkan input resolution for `scale-f32` and `add-f32`.
  - CPU placement, non-empty runtime push dictionaries, unsupported dtype,
    empty tensor, shape mismatch, unsupported operations, arbitrary user source
    compilation, graph capture, command-buffer batching, fusion, device-buffer
    reuse/lifetime planning, deterministic invalidation, and
    `(define [kernel] ...)` sugar remain out of scope.
  - Validation passed: `c3c build`, direct eval smokes, focused advanced
    collections `pass=1839 fail=0`, basic Lisp `pass=161 fail=0`, compiler
    slice `pass=287 fail=0`, primitive docs parity, Stage 3 source parity,
    code file-size gate, and `git diff --check`.

## 2026-04-21 - ML-VK-080-004 Checked Vulkan Binary Float32 Kernel Family

- Generalized checked Vulkan `kernel/run` binary Float32 execution.
  - Supported specs now include `operation 'add-f32`, `sub-f32`, `mul-f32`,
    `div-f32`, `min-f32`, and `max-f32`.
  - Contract: two input descriptors, one output descriptor, `Float32`
    descriptor dtypes, empty spec/runtime push dictionaries, matching
    descriptor shapes, same-shape runtime tensors, and dense row-major Vulkan
    `Float32` storage for both inputs.
  - Execution uses the existing checked Vulkan Float32 map helper opcodes and
    returns an ordinary dictionary keyed by the output descriptor name.
  - Arbitrary user source compilation, graph capture, command-buffer batching,
    fusion, device-buffer reuse/lifetime planning, deterministic invalidation,
    and `(define [kernel] ...)` sugar remain out of scope.
  - Validation passed: `c3c build`, direct `mul-f32` smoke, focused advanced
    collections `pass=1840 fail=0`, basic Lisp `pass=161 fail=0`, compiler
    slice `pass=287 fail=0`, primitive docs parity, Stage 3 source parity,
    code file-size gate, and `git diff --check`.

## 2026-04-21 - ML-VK-080-005 Checked Vulkan Scalar Float32 Kernel Family

- Added checked Vulkan `kernel/run` scalar Float32 execution.
  - Supported tensor-scalar specs: `operation 'add-scalar-f32`,
    `sub-scalar-f32`, `mul-scalar-f32`, `div-scalar-f32`, `min-scalar-f32`,
    and `max-scalar-f32`.
  - Supported scalar-left specs: `operation 'scalar-sub-f32` and
    `scalar-div-f32`.
  - Contract: one input descriptor, one output descriptor, `Float32` descriptor
    dtypes, spec push dictionary containing only `scalar 'Float32`, runtime
    push dictionary containing exactly one `scalar` value representable as
    `Float32`, matching descriptor shapes, and dense row-major Vulkan `Float32`
    runtime input storage.
  - Execution uses the existing checked Vulkan Float32 map helper scalar modes
    and returns an ordinary dictionary keyed by the output descriptor name.
  - Arbitrary user source compilation, graph capture, command-buffer batching,
    fusion, device-buffer reuse/lifetime planning, deterministic invalidation,
    and `(define [kernel] ...)` sugar remain out of scope.
  - Validation passed: `c3c build`, focused advanced collections
    `pass=1842 fail=0`, basic Lisp `pass=161 fail=0`, compiler slice
    `pass=287 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, and `git diff --check`.

## 2026-04-21 - ML-VK-080-006 Checked Vulkan Unary Float32 Kernel Family

- Added checked Vulkan `kernel/run` unary Float32 execution.
  - Supported specs now include `operation 'abs-f32`, `neg-f32`, `sqrt-f32`,
    `identity-f32`, `zero-f32`, `sin-f32`, `cos-f32`, `tan-f32`, `asin-f32`,
    `acos-f32`, `atan-f32`, `sinh-f32`, `cosh-f32`, `tanh-f32`, `exp-f32`,
    `log-f32`, `log10-f32`, and `normal-cdf-f32`.
  - Contract: one input descriptor, one output descriptor, `Float32`
    descriptor dtypes, empty spec/runtime push dictionaries, matching
    descriptor shape and runtime tensor shape, and dense row-major Vulkan
    `Float32` runtime input storage.
  - Execution uses the existing checked Vulkan Float32 unary map helper and
    returns an ordinary dictionary keyed by the output descriptor name.
  - Added `src/lisp/prim_kernel_unary.c3` and wired it into the AOT runtime
    source manifest after Stage 3 source parity caught the missing source entry.
  - Arbitrary user source compilation, graph capture, command-buffer batching,
    fusion, device-buffer reuse/lifetime planning, deterministic invalidation,
    and `(define [kernel] ...)` sugar remain out of scope.
  - Validation passed: `c3c build`, focused advanced collections
    `pass=1844 fail=0`, basic Lisp `pass=161 fail=0`, compiler slice
    `pass=287 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, and `git diff --check`.

## 2026-04-21 - ML-VK-080-007 Checked Vulkan Single-Node Kernel Capture

- Added `kernel/capture(kernel inputs push)` for checked Vulkan Kernel launch
  planning.
  - Supported capture families match the checked direct-helper `kernel/run`
    coverage for Vulkan `Float32`: `scale-f32`, binary operations, scalar
    operations, and unary operations.
  - Capture validates backend, operation family, descriptor arity/dtypes,
    runtime input placement, push contract, and concrete runtime shape without
    launching the kernel.
  - Capture returns ordinary `kernel-graph` data with one `kernel-node`,
    backend, operation, family, dtype, device, direct-helper execution,
    input/output names, concrete runtime shape, push data, and invalidation key.
  - Added primitive registration, AOT compiler primitive lookup, AOT runtime
    source manifest wiring, tests, docs, TODO, plan, and session report updates.
  - Multi-node Tensor expression DAG capture, command-buffer batching, fusion,
    arbitrary source compilation, buffer reuse/lifetime planning, and broader
    deterministic invalidation remain open.
  - Validation passed: `c3c build`, focused advanced collections
    `pass=1846 fail=0`, compiler slice `pass=288 fail=0`, basic Lisp
    `pass=161 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, and `git diff --check`.

## 2026-04-21 - ML-VK-080-008 Vulkan Tensor Map Graph Capture

- Added `tensor/capture(source)` for Vulkan Tensor graph planning.
  - Supported graphs are all-Vulkan `Float32` concrete/map Tensor expression
    graphs.
  - Capture returns ordinary `tensor-graph` data with source/map nodes, node
    ids, input edges, scalar operands, output node id, shape, dtype/device,
    backend, family `map-expression`, and invalidation key.
  - Explicit `to-device 'vulkan` now preserves supported CPU lazy `Float32` map
    expressions as Vulkan Tensor expressions so capture can inspect real
    multi-node map DAGs without launching them.
  - Contract/view graph capture, command-buffer batching, fusion, arbitrary
    source compilation, buffer reuse/lifetime planning, and broader
    invalidation remain open.
  - Validation passed: `c3c build`, direct capture smoke, focused advanced
    collections `pass=1848 fail=0`, compiler slice `pass=289 fail=0`, basic
    Lisp `pass=161 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, and `git diff --check`.

## 2026-04-21 - ML-VK-080-009 Vulkan Tensor Contract Graph Capture

- Extended `tensor/capture(source)` for Vulkan Tensor graph planning.
  - Supported graphs are now all-Vulkan `Float32` concrete/map/contract Tensor
    expression graphs.
  - Capture returns ordinary `tensor-graph` data with source/map/contract
    nodes, node ids, input edges, scalar operands, contract left/right axes,
    axis count, output node id, shape, dtype/device/backend, graph family, and
    invalidation key.
  - Explicit `to-device 'vulkan` now preserves supported CPU lazy `Float32`
    contract expressions as Vulkan Tensor expressions so capture can inspect
    real multi-node contract DAGs without launching them.
  - View graph capture, command-buffer batching, fusion, arbitrary source
    compilation, buffer reuse/lifetime planning, and broader invalidation
    remain open.
  - Validation passed: `c3c build`, direct contract-capture eval smoke,
    focused advanced collections `pass=1849 fail=0`, compiler slice
    `pass=289 fail=0`, basic Lisp `pass=161 fail=0`, primitive docs parity,
    Stage 3 source parity, code file-size gate, and `git diff --check`.
