# 2026-04-20 16:32 CEST - Omni Neural DataSpec Initialization

- Objective attempted:
  - Continue `ML-VK-070` by implementing `ML-VK-070-002`, deterministic
    parameter initialization for Omni Neural DataSpec model bundles.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - Vulkan ML roadmap item `ML-VK-070-002`.
- Code or configuration changes made:
  - Added public `nn/init(spec [options])`.
  - Wired runtime primitive registration, AOT primitive lookup, and runtime
    manifest source inclusion for `src/lisp/prim_nn_init.c3`.
  - Added deterministic `Float64`/`Float32` parameter initialization for dense,
    conv1d, and conv2d specs with explicit `dtype`, `device`, `seed`, and
    `mode` options.
  - Extended conv1d/conv2d DataSpecs to accept the same `use-bias`,
    `activation`, `kernel-init`, and `bias-init` option contract as dense specs.
  - Returned transparent model bundles with `kind`, `spec`, `params`, `state`,
    `mode`, `dtype`, `device`, and `metadata`.
  - Added focused `nn/init` tests and updated public docs, TODO, plan, and
    changelog artifacts.
- Commands run:
  - Fast subagent source audit for `nn/init` compile/contract issues.
  - Fast subagent test implementation for focused `nn/init` coverage.
  - Fast subagent docs/backlog update for `ML-VK-070-002`.
  - `c3c clean`
  - `c3c build`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=build:/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
- Key results:
  - `c3c build` passed after a clean build-artifact refresh.
  - Focused advanced collections passed with `pass=1739 fail=0`.
  - Basic Lisp passed with `pass=160 fail=0`.
  - Primitive docs parity and whitespace checks passed.
  - File-size gate initially failed because `docs/LANGUAGE_SPEC.part-01b.md`
    reached 714 lines; the truthiness/equality tail was split into
    `docs/LANGUAGE_SPEC.part-01c.md`, and the rerun passed.
  - The source audit found a real conv initializer gap; conv DataSpecs and
    `nn/init` now honor initializer/bias options instead of forcing hidden
    defaults.
  - Initial test failures were due malformed worker-added Lisp `let` bindings
    and a helper success-return bug in `nn_init_layer`; both were fixed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat non-null helper returns in `nn_init_layer` as success values;
    `prim_nn_init` uses the local convention that `null` means success and
    non-null means an error value to return.
  - Do not document conv parameter initialization without schema support;
    conv1d/conv2d specs must carry validated initializer options for that
    contract to be real.
- Unresolved issues:
  - Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run in this
    slice.
  - `ML-VK-070-003` inference apply remains open.
- Next actions:
  - Commit and push this checkpoint.
  - Start `ML-VK-070-003`: `nn/apply`, `nn/predict`, and `nn/summary`.
- Signature: Codex GPT-5.4
