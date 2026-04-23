# ML Module Surface Split Decision - 2026-04-23

Item: `SURFACE-NAMING-003`
Status: Accepted surface decision. `SURFACE-NAMING-004` has also resolved the
batched linear spelling as public `ml/linear-batched-reduce`.

## Context

Slash-qualified names in Omni are ordinary symbols. They are appropriate for
compact, dense primitive families where the prefix improves scanning, but they
are not module paths and should not become a substitute for real
module/facade boundaries.

The current `ml/...` surface has grown from tensor learning kernels into
visualization, checkpointing, optimizer state, and model-adjacent workflow
helpers. That breadth no longer fits one always-present primitive namespace.
This note classifies the current surface and defines which pieces should remain
slash primitives and which should move behind modules or facades.

## Current `ml/...` Inventory

The current shipped `ml/...` names fall into these groups:

| Classification | Current names | Decision |
|----------------|---------------|----------|
| Core tensor/learning primitives | `ml/linear`, `ml/linear-batched-reduce`, `ml/relu`, `ml/leaky-relu`, `ml/sigmoid`, `ml/tanh`, `ml/gelu`, `ml/sum`, `ml/mean`, `ml/variance`, `ml/max`, `ml/logsumexp`, `ml/softmax`, `ml/layer-normalization`, `ml/batch-normalization`, `ml/scaled-dot-product-attention`, `ml/cross-entropy`, `ml/mean-squared-error`, `ml/conv1d`, `ml/conv2d`, `ml/max-pool2d`, `ml/avg-pool2d`, `ml/grad` | Keep as the slash primitive family when the operation is a low-level tensor/learning kernel or gradient evaluator. `ml/linear-batched-reduce` is the canonical public spelling for the batched linear projection surface. |
| Visualization helpers | `ml/plot`, `ml/loss-curve`, `ml/scatter`, `ml/bar-chart`, `ml/histogram`, `ml/heatmap`, `ml/roc-curve`, `ml/pr-curve`, `ml/tensor-summary`, `ml/confusion-matrix`, `ml/export-image` | Move to an importable visualization facade. Canonical qualified calls should be `ml.visualization.plot`, `ml.visualization.loss-curve`, `ml.visualization.scatter`, `ml.visualization.bar-chart`, `ml.visualization.histogram`, `ml.visualization.heatmap`, `ml.visualization.roc-curve`, `ml.visualization.pr-curve`, `ml.visualization.tensor-summary`, `ml.visualization.confusion-matrix`, and `ml.visualization.export-image`. Do not add `ml/visualization/...` slash names. |
| Optimizer and checkpoint helpers | `ml/sgd-step`, `ml/clip-gradients`, `ml/optimizer-step`, `ml/save-optimizer`, `ml/load-optimizer` | Move to an importable optimizer facade. Canonical qualified calls should be `ml.optimizers.sgd-step`, `ml.optimizers.clip-gradients`, `ml.optimizers.step`, `ml.optimizers.save`, and `ml.optimizers.load`. Keep `ml/grad` in the core slash family; it is a low-level gradient primitive consumed by training facades. |
| Model-facing APIs | No model constructors are currently spelled as `ml/...`; model APIs are already `nn/...` (`nn/init`, `nn/sequential`, `nn/dense`, `nn/apply`, `nn/forward`, `nn/train-step`, `nn/save`, `nn/load`, etc.) | Keep model-facing APIs out of `ml/...`. The model facade remains `nn/...` for the current surface and may later become an importable `nn` module/facade, but it should not be reintroduced as `ml/model-*` or `ml/models/...`. |

## Decision

Keep `ml/...` only for compact, always-present low-level learning kernels over
Tensor data and for the low-level gradient evaluator. The prefix means "machine
learning tensor primitive", not "the whole ML standard library".

Move visualization, optimizer state/checkpointing, and broader model workflows
to real module/facade surfaces:

- `ml.visualization.*` owns plot/data-summary/export helpers.
- `ml.optimizers.*` owns optimizer update, gradient clipping, and optimizer
  checkpoint helpers.
- `nn/...` remains the current model-facing facade. Future module work may
  make `nn` importable, but `ml/...` should not absorb model constructors or
  training-loop APIs.

Do not introduce deeper slash pseudo-paths such as `ml/visualization/plot`,
`ml/optimizers/step`, or new `ml/model/...` names. Dotted access is the module
and facade syntax; slash remains ordinary compact primitive naming.

## Canonical Slash Primitive Boundary

The canonical `ml/...` primitive family should contain:

- Dense linear and reduction kernels: `ml/linear`, `ml/sum`, `ml/mean`,
  `ml/variance`, `ml/max`, `ml/logsumexp`, `ml/softmax`.
- Tensor activation kernels: `ml/relu`, `ml/leaky-relu`, `ml/sigmoid`,
  `ml/tanh`, `ml/gelu`.
- Normalization, attention, and loss kernels:
  `ml/layer-normalization`, `ml/batch-normalization`,
  `ml/scaled-dot-product-attention`, `ml/cross-entropy`,
  `ml/mean-squared-error`.
- Dense convolution and pooling kernels: `ml/conv1d`, `ml/conv2d`,
  `ml/max-pool2d`, `ml/avg-pool2d`.
- Low-level gradient evaluation: `ml/grad`.

This boundary permits compact future kernel names when they are peer operations
in the same dense tensor family. It does not permit slash expansion merely
because a feature is ML-related.

## Migration Implications

This document does not change runtime behavior. The currently shipped slash
names remain active until an implementation slice changes the surface.

When the implementation slice lands, prefer one canonical surface per concept:

- Add `ml.visualization` and `ml.optimizers` facades first, with qualified
  exports that call the existing implementations or their renamed owners.
- In the same implementation plan, either remove the moved slash names or record
  an explicit owner-approved migration window. Do not silently keep permanent
  compatibility aliases.
- Update public docs so visualization examples import/use
  `ml.visualization.*` and optimizer examples import/use `ml.optimizers.*`.
- Keep `nn/...` examples model-facing; they may lower to `ml/...` kernels and
  `ml.optimizers.*` helpers internally, but user-facing model construction
  should remain under the neural/model facade.
- Keep `ml/linear-batched-reduce` as the canonical public spelling for the
  batched linear projection surface. Do not reintroduce the old
  `ml/linear/batched-reduce` pseudo-path spelling.

## Validation Implications

For this decision-only slice, `git diff --check` on the touched decision files
is sufficient.

For a future implementation slice that moves names:

- Update eval primitive registration, AOT primitive lookup, runtime manifest
  generation, docs, examples, and compiler codegen tests together.
- Run primitive-doc parity/status consistency checks so removed slash names are
  not still advertised.
- Run focused runtime tests for the moved visualization or optimizer surface,
  plus AOT/codegen coverage for the new facade access.
- If slash names are removed rather than kept behind an owner-approved
  migration window, add negative tests proving the removed names fail closed
  with the expected unknown-symbol/unknown-primitive behavior.
- Keep tensor/backend validation unchanged for the underlying kernels; this is
  a naming/facade split, not permission for hidden CPU fallback or backend
  broadening.

## Non-Goals

- This decision does not rename tensor, matrix, kernel, stats, or NN surfaces.
- This decision does not add a new dependency or visualization backend.
- This decision does not change the current `nn/...` model API.
- This decision does not decide tensor, matrix, kernel, stats, Deduce, or Pika
  naming beyond the ML split described above.
