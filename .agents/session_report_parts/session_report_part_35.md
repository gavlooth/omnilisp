# 2026-04-20 17:06 CEST - Omni Neural DataSpec Inference

- Objective attempted:
  - Continue `ML-VK-070` by implementing `ML-VK-070-003`, inference
    application and inspection for Omni Neural DataSpec model bundles.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - Vulkan ML roadmap item `ML-VK-070-003`.
- Code or configuration changes made:
  - Added public `nn/apply`, `nn/predict`, `nn/summary`, `nn/spec`,
    `nn/parameters`, `nn/state`, and `nn/mode` primitives.
  - Wired runtime primitive registration, AOT primitive lookup, and runtime
    manifest inclusion for `src/lisp/prim_nn_apply.c3`.
  - Implemented model-bundle inference via `(nn/apply model input)` and
    explicit data inference via `(nn/apply spec params state input [options])`.
  - `nn/predict` now requires model mode `'eval`.
  - `nn/summary` reports inspectable model metadata and parameter counts.
  - Lowering covers dense, conv1d, conv2d, max/avg pool2d, activation,
    softmax, sequential, and CPU flatten paths through the existing `ml/*`
    primitive layer.
  - Tightened the explicit `nn/apply` options contract so only an empty
    reserved options dictionary is accepted; non-empty options fail closed.
  - Tightened `nn/summary` parameter-tree errors so malformed trees report
    `nn/invalid-spec` instead of generic count failure.
  - Added focused tests and updated language spec, primitive appendix, TODO,
    plan, and changelog artifacts.
- Commands run:
  - Fast subagent contract audit for inference/accessor surface.
  - Fast subagent docs/backlog update.
  - Fast subagent test implementation.
  - Fast subagent review-only audit for `nn/apply` and docs/tests.
  - `c3c build`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - `c3c build` passed.
  - Focused advanced collections passed with `pass=1748 fail=0`.
  - Basic Lisp passed with `pass=160 fail=0`.
  - Primitive docs parity, file-size gate, and whitespace checks passed.
  - `docs/LANGUAGE_SPEC.part-01b.md` was split top-down again by moving the
    tail backend notes into `docs/LANGUAGE_SPEC.part-01c.md`; all tracked text
    files remain under 700 lines.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not accept non-empty `nn/apply` options as a no-op. Until real options
    exist, accepting them silently is a broken contract.
  - Do not collapse malformed `nn/summary` parameter trees into generic count
    failures; invalid model data should remain `nn/invalid-spec`.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run in this
    slice.
  - `ML-VK-070-004` checkpoint round-trip support remains open.
- Next actions:
  - Continue `ML-VK-070-004`: checkpoint serialization/restore for transparent
    model bundles.
  - Keep full training facade work under `ML-VK-070-005` until autograd and
    optimizer contracts are ready.
- Signature: Codex GPT-5.4

# 2026-04-20 17:35 CEST - Omni Neural DataSpec Checkpoints

- Objective attempted:
  - Continue `ML-VK-070` by implementing `ML-VK-070-004`, checkpoint
    serialization and restore for Omni Neural DataSpec specs and model bundles.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - Vulkan ML roadmap item `ML-VK-070-004`.
- Code or configuration changes made:
  - Added public `nn/save-spec`, `nn/load-spec`, `nn/save`, and `nn/load`
    primitives.
  - Wired runtime primitive registration, AOT primitive lookup, and runtime
    manifest inclusion for `src/lisp/prim_nn_checkpoint.c3`.
  - Implemented checkpoint JSON strings and path-based save/load.
  - Encoded model bundle data explicitly: spec, params, state, mode, dtype,
    device, metadata, tensor dtype, tensor shape, flat tensor data, and recorded
    tensor placement.
  - Restored non-CPU tensors through explicit `to-device` routes.
  - Added focused round-trip and payload-family mismatch tests.
  - Updated language spec, primitive appendix, Omni Neural DataSpec plan, TODO,
    active plan, and memory changelog artifacts.
- Commands run:
  - Fast read-only subagent pass for test and docs insertion points.
  - `c3c build`
  - Direct `--eval` smoke checks for string and path checkpoint round trips.
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - `c3c build` passed.
  - Direct string and path checkpoint smokes returned `true`.
  - Focused advanced collections passed with `pass=1753 fail=0`.
  - Basic Lisp passed with `pass=160 fail=0`.
  - Primitive docs parity, file-size gate, and whitespace checks passed.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run in this
    slice.
  - `ML-VK-070-005` remains blocked on the lower-level autograd and optimizer
    roadmap lanes.
- Next actions:
  - Continue Vulkan ML prerequisites for `ML-VK-050` autograd and `ML-VK-060`
    optimizers before training facade work.
- Signature: Codex GPT-5.4
