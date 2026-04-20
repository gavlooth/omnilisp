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
