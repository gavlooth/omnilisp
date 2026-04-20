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
