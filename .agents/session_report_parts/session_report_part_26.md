# Session Report Index Part 26

Source: `.agents/SESSION_REPORT.md`

## 2026-04-17 12:00 CEST - Vulkan SVD Eigen Float32 Planning

- Objective attempted:
  - Add the missing direct Vulkan factor-output SVD plan, eigensolver plan, and
    Float32 planning requested by the owner.
- Workspace/target:
  - `/home/christos/Omni`, planning/docs only.
- Code or configuration changes made:
  - Added `docs/plans/vulkan-svd-factor-output-plan-2026-04-17.md` for direct
    Vulkan `matrix/svd` behind the existing backend-neutral public surface.
  - Added `docs/plans/vulkan-eigensolver-plan-2026-04-17.md` for direct Vulkan
    `matrix/eigenvalues`, `matrix/eigenvectors`, and `matrix/eigenpairs`.
    Symmetric real `Float64` eigenvalues/eigenvectors are the first eligible
    Vulkan phase; general `matrix/eigenpairs` remains blocked while the public
    result contract is pointer-backed `BigComplex`.
  - Added `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`,
    splitting future Float32 work into native Tensor dtype/storage semantics
    first and Vulkan kernels second.
  - Updated `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`,
    `docs/plans/README.md`, `TODO.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`,
    `docs/plans/vulkan-backend-decision-2026-04-16.md`, and
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md` so the new
    plan files are discoverable and stale spectral/nuclear or zero-axis wording
    no longer contradicts current behavior.
  - Updated `.agents/PLAN.md` and `memory/CHANGELOG.md` with the planning
    checkpoint.
- Commands run and key results:
  - `jj status`: confirmed the workspace was already broadly dirty; this
    planning pass touched docs and agent artifacts only.
  - Targeted `git diff --check` over touched docs/artifacts: passed.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat the existence of these plan files as implementation support.
    Direct Vulkan SVD/eigen execution remains fail-closed until the relevant
    plan gates land.
  - Do not use `Float32` as a hidden fallback for existing `Float64` Vulkan
    execution. `Float32` requires native Tensor storage and CPU semantics first.
- Current best recommendation / checkpoint:
  - The immediate missing planning artifacts now exist. Next implementation
    should either harden current `TENSOR-100F` docs/tests from the audit notes or
    choose one planned surface and land its first validation gate.
- Unresolved issues:
  - No runtime code changed in this planning pass, so no build or runtime suite
    was rerun.
- Signature: Codex GPT-5.4

## 2026-04-17 12:20 CEST - Vulkan Audit Hardening Implementation

- Objective attempted:
  - Continue `TENSOR-100F` implementation using normal GPT-5.4 agents after the
    direct SVD/eigen/Float32 plan files were added.
- Workspace/target:
  - `/home/christos/Omni`, focused Tensor/Vulkan audit-hardening slice.
- Code or configuration changes made:
  - Updated `src/lisp/prim_tensor.c3` so the generic Vulkan contract helper now
    raises context `contract: Vulkan Float64 contract kernel failed` instead of
    stale single-axis wording.
  - Added `src/lisp/tests_advanced_stdlib_module_groups.c3` regressions for:
    public zero-size Vulkan `matrix/norm` `'spectral` and `'nuclear` on empty
    rows and empty columns;
    unchanged `dgesvd` counters for those zero-size norm paths;
    unchanged `dgesvd`, `dsyev`, and `dgeev` counters for direct and lazy
    Vulkan fail-closed `matrix/svd`, `matrix/eigenvalues`,
    `matrix/eigenvectors`, and `matrix/eigenpairs`;
    diagnostic ordering for invalid-but-Vulkan SVD/eigen operands returning
    `tensor/backend-unsupported` before CPU shape or symmetry diagnostics;
    lazy-contract helper execution for zero-axis and multi-axis Vulkan
    contractions.
  - Updated `TODO.md`, `.agents/PLAN.md`, and `memory/CHANGELOG.md` with the
    implementation checkpoint.
- Commands run and key results:
  - Worker validation before final integration: `c3c build`, focused
    `advanced-collections-module` `pass=919 fail=0`, and targeted
    `git diff --check` passed.
  - Final integrated validation: `c3c build --obj-out obj` passed with existing
    deprecation warnings.
  - Final focused validation:
    `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    passed with `pass=935 fail=0`.
  - Targeted `git diff --check` for changed source/docs/artifacts passed.
  - `./scripts/check_primitive_docs_parity.sh` passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity` passed.
- Invalidated assumptions or failed approaches worth preserving:
  - None new. The key preserved constraint remains that direct Vulkan SVD/eigen
    support is not implemented by these tests; the direct surfaces remain
    fail-closed until their dedicated implementation gates land.
- Current best recommendation / checkpoint:
  - The audit-hardening slice is implemented and validated. Next useful work can
    choose a planned implementation lane, with direct Vulkan `matrix/svd`
    factor-output support and symmetric real eigensolvers still requiring real
    helper/shader work rather than CPU fallback.
- Unresolved issues:
  - Full heavy/container gates were not rerun for this source-level test and
    diagnostic slice.
- Signature: Codex GPT-5.4

## 2026-04-17 17:23 CEST - Vulkan Unary Plus And Helper Factoring

- Objective attempted:
  - Continue `TENSOR-100F` feature work using multiple GPT-5.4 agents, with
    deferred work kept in TODO.
- Workspace/target:
  - `/home/christos/Omni`, Tensor/Vulkan unary helper and shared C helper
    factoring.
- Code or configuration changes made:
  - Updated `src/lisp/prim_tensor.c3` so unary Tensor `map +` on dense
    row-major Vulkan `Float64` tensors maps to the existing unary helper
    identity opcode.
  - Updated `src/lisp/eval_init_primitive_registration.c3` and
    `src/lisp/prim_math_arithmetic.c3` so public `+` accepts one or two
    arguments: `(+ scalar)` and `(+ tensor)` are identity, while over-arity
    still raises.
  - Added focused regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` for Vulkan unary
    `map +` roundtrip/placement and direct unary Tensor `+` placement.
  - Updated `src/lisp/tests_advanced_core_unicode_groups_more.c3` for the new
    unary `+` contract, over-arity rejection, typed unary `+` method dispatch,
    and continued no-auto-partial coverage via binary `*`.
  - Updated `csrc/tensor_vulkan_helpers.c` so the serial two-buffer,
    three-buffer, one-input/two-output, and one-input/three-output Vulkan
    dispatch helpers reuse `omni_tensor_vulkan_create_compute_pipeline_for_shader`.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `docs/reference/11-appendix-primitives.md`.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_map_unary_f64.comp -o /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_map_unary_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-unicode-iterator OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=159 fail=0`.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=972 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed, `pass=959 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The first test attempt exposed that changing `+` registration to `-1`
    must not mean real variadic addition. `prim_add` now explicitly accepts
    only one or two arguments.
- Current best recommendation / checkpoint:
  - Unary identity support is landed and validated. Continue `TENSOR-100F`
    with the remaining TODO items: descriptor/command/status helper factoring,
    larger SVD/eigen algorithms, direct general `matrix/eigenpairs`, native
    CPU `Float32` Tensor storage before Vulkan Float32, fixed-width complex,
    and stride/view dispatch contracts.
- Unresolved issues:
  - Descriptor/pool/command-buffer/status helper factoring remains open after
    the compute-pipeline helper proof slice.
  - Full heavy/container gates were not run; bounded focused container
    validation passed.
- Signature: Codex GPT-5.4

## 2026-04-17 17:49 CEST - Native CPU Tensor Float32 Storage

- Objective attempted:
  - Continue `TENSOR-100F` feature implementation with multiple GPT-5.4 agents and close the native CPU `Tensor Float32` prerequisite before any Vulkan `Float32` kernel work.
- Workspace/target:
  - `/home/christos/Omni`, Tensor dtype/runtime storage, CPU Tensor evaluation, structural matrix helpers, docs, TODO, and memory artifacts.
- Code or configuration changes made:
  - Added native `TENSOR_DTYPE_FLOAT32` metadata/storage handling in `src/lisp/value_runtime_types.c3` and `src/lisp/value_tensor.c3`.
  - Updated `src/lisp/prim_tensor.c3` so `Tensor Float32` construction narrows finite numeric data into 32-bit storage, rejects out-of-range data, and supports CPU `map`, `contract`, `realize`, `to-device 'cpu`, `ref`, `Array`, `List`, and `Iterator`. Element extraction widens to scalar `Float64` because a scalar `Float32` value tag does not exist yet.
  - Updated `src/lisp/prim_tensor_matrix.c3` so structural CPU helpers support `Float32`: `matrix/transpose`, `matrix/diagonal`, `matrix/diagonal-matrix`, `matrix/identity`, and `matrix/trace`. Heavier LAPACK-style routines remain non-`Float32` lanes and must not silently widen and narrow.
  - Kept scalar `Float32` / `(Float x 32)` constructors fail-closed in `src/lisp/prim_string_convert.c3`; updated describe-mode wording to state scalar storage is still missing while Tensor storage exists.
  - Added focused regressions in `src/lisp/tests_advanced_stdlib_module_groups.c3` and low-level allocation coverage in `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`, Tensor reference docs, area docs, and Vulkan `Float32`/dtype/layout/roadmap plan docs.
- Commands run and key results:
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Host focused `advanced-collections-module`: passed, `pass=995 fail=0`.
  - Host focused `advanced-unicode-iterator`: passed, `pass=159 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed, `pass=982 fail=0`.
  - Bounded-container `memory-lifetime-smoke`: passed, `pass=228 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
  - `c3c build --sanitize=address --obj-out obj-asan`: not run to completion; local `c3c` rejected sanitizer mode with "Address sanitizer is only supported on Linux, FreeBSD, NetBSD, Darwin and Windows."
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep saying native Tensor `Float32` storage is absent. The remaining missing pieces are scalar `Float32` values and GPU `Float32` copy/kernel support.
  - Do not implement Vulkan `Float32` by downcasting `Float64`; remaining device work needs explicit copy paths, helper/shader ABI names, capability reporting, and no-downcast tests.
- Current best recommendation / checkpoint:
  - Continue `TENSOR-100F` with Vulkan descriptor/command/status helper factoring or the first real Vulkan `Float32` placement/kernel slice. The CPU `Tensor Float32` oracle is now present for the latter.
- Unresolved issues:
  - Scalar `Float32` constructors still fail closed.
  - CUDA/Vulkan `Float32` placement still fails closed.
  - Full heavy/container gates were not run; focused bounded-container validation passed.
- Signature: Codex GPT-5.4

## 2026-04-17 18:07 CEST - Vulkan Serial Dispatch Helper Factoring

- Objective attempted:
  - Continue `TENSOR-100F` feature implementation with multiple GPT-5.4 agents
    and land the next behavior-preserving Vulkan helper factoring slice.
- Workspace/target:
  - `/home/christos/Omni`, `csrc/tensor_vulkan_helpers.c`, TODO, plan,
    roadmap, changelog, and session artifacts.
- Code or configuration changes made:
  - Added private helper structs and functions for sequential storage-buffer
    descriptor resources: layout creation, descriptor pool/allocation/update,
    and cleanup.
  - Added a private one-time compute dispatch helper that creates a command
    pool, allocates/begins one primary command buffer, binds pipeline and one
    descriptor set, pushes constants, dispatches, ends, submits, waits idle,
    and destroys the command pool.
  - Migrated the mature serial Vulkan helper family:
    two-buffer, three-buffer, one-input/two-output, and
    one-input/three-output dispatch helpers.
  - Preserved descriptor binding order, synchronous submit/wait behavior,
    output ownership, Vulkan result placement, diagnostics, and no hidden
    CPU/LAPACK fallback.
  - Updated `TODO.md`, `.agents/PLAN.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `memory/CHANGELOG.md` so only status-copy/readback helper factoring remains
    under the open helper TODO.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`: passed, `pass=995 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed, `pass=982 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Current best recommendation / checkpoint:
  - Continue helper factoring only from the status-copy/readback boundary, or
    switch to another explicit `TENSOR-100F` lane such as real Vulkan `Float32`
    copy/kernel support.
- Unresolved issues:
  - Status-copy/readback helper extraction remains open because status payload
    semantics differ across generic singular, SVD/singular-values,
    symmetric-eigen, and parallel-solve paths.
  - Live GPU execution coverage is availability-gated; focused host/container
    suites passed, but this environment may not exercise an actual Vulkan
    device path.
- Signature: Codex GPT-5.4

## 2026-04-17 18:17 CEST - TENSOR-100F Vulkan Status Readback Helper Factoring

- Objective attempted:
  - Continue `TENSOR-100F` helper factoring by removing repeated Vulkan
    `Float64` status-payload copy/readback boilerplate without changing public
    Tensor behavior.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Tensor helper implementation and
    `TENSOR-100F` planning artifacts.
- Code or configuration changes made:
  - Added a shared `Float64` status-payload copy/readback helper and a
    mapper-based status decoding helper in `csrc/tensor_vulkan_helpers.c`.
  - Migrated compatible serial Vulkan mapped-status readers through the shared
    path: generic trailing status, SVD/singular-values status, and symmetric
    eigen status.
  - Extended the existing deterministic status probe to cover tail, singular
    values/SVD, and symmetric-eigen mapper behavior.
  - Updated LU metadata handling to reuse the generic trailing-status mapper
    while keeping pivot and swap-count validation LU-specific.
  - Left parallel solve on its separate `uint32_t` status-buffer ABI by design.
  - Updated `TODO.md`, `.agents/PLAN.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, and
    `memory/CHANGELOG.md`; the helper-plumbing TODO is now closed and the live
    queue continues with larger SVD/eigen algorithms, direct general
    eigenpairs, Vulkan `Float32`, fixed-width complex, and layout/stride work.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`: passed, `pass=995 fail=0`.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`: passed, `pass=982 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Current best recommendation / checkpoint:
  - Do not reopen mature serial helper plumbing as the neutral baseline unless
    new duplication appears in a different helper shape. Continue from the
    remaining explicit `TENSOR-100F` feature lanes.
- Unresolved issues:
  - Full heavy/container-only gates were not run; focused bounded-container
    validation passed.
  - Live GPU execution coverage is availability-gated; this environment may not
    exercise an actual Vulkan device path.
- Signature: Codex GPT-5.4

## 2026-04-17 19:22 CEST - TENSOR-100F Vulkan Float32 Placement And Elementwise Slice

- Objective attempted:
  - Continue `TENSOR-100F` using multiple GPT-5.4 agents and land the first
    real Vulkan `Float32` execution slice without using `Float32` as a hidden
    fallback for existing `Float64` behavior.
- Workspace/target:
  - `/home/christos/Omni`, Vulkan Tensor placement/copyback, destination
    `realize`, dense row-major `map`, unary helpers, direct `min`/`max`,
    tests, TODO, and docs/artifacts.
- Code or configuration changes made:
  - Added `Float32` Vulkan binary/unary shader sources and generated SPIR-V C
    arrays: `csrc/tensor_vulkan_map_f32.comp`,
    `csrc/tensor_vulkan_map_f32_spv.c`,
    `csrc/tensor_vulkan_map_unary_f32.comp`, and
    `csrc/tensor_vulkan_map_unary_f32_spv.c`.
  - Added distinct helper/ABI paths in `csrc/tensor_vulkan_helpers.c` and
    `src/lisp/tensor_vulkan_backend.c3`:
    `omni_tensor_backend_vulkan_float32_available`,
    `omni_tensor_backend_vulkan_fill_f32`,
    `omni_tensor_backend_vulkan_map_f32`, and
    `omni_tensor_backend_vulkan_map_unary_f32`.
  - Updated `src/lisp/prim_tensor.c3` so Vulkan `Float32` placement/copyback,
    destination `realize`, lazy/direct `map`, direct unary Tensor operations,
    and direct `min`/`max` dispatch to real `Float32` Vulkan helpers for
    matching dense row-major operands.
  - Preserved no-downcast behavior: `Float64` paths still use `Float64`
    helpers, mixed Tensor dtypes fail closed, and Big* Tensor dtypes are not
    lowered to Vulkan.
  - Added focused availability-gated regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` for capability
    reporting, placement/copyback dtype preservation, destination `realize`,
    `map`, unary helpers, direct `min`/`max`, and `Float64` no-downcast
    preservation.
  - Updated `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`, `docs/plans/README.md`,
    `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`,
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_map_f32.comp -o /tmp/omni_map_f32.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_map_f32.spv`: passed.
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_map_unary_f32.comp -o /tmp/omni_map_unary_f32.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_map_unary_f32.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `Float32` smokes: capability reported `float32 true`;
    placement/copyback preserved `"Float32"`; `map +` returned `4.25`;
    `map sqrt` returned `1.11803388595581` for `sqrt(1.25)`; direct
    `max` stayed Vulkan-placed; direct `min` returned `-1.25`.
  - Host focused `advanced-collections-module`: passed, `pass=1025 fail=0`.
  - Bounded-container focused `advanced-collections-module`: passed,
    `pass=1012 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating Vulkan `Float32` as entirely fail-closed. Placement,
    copyback, destination `realize`, elementwise `map`, unary helpers, and
    direct `min`/`max` are now real Vulkan paths.
  - Do not use the presence of `Float32` Vulkan support to justify hidden
    `Float64` downcasts or mixed-dtype promotion on Vulkan.
- Current best recommendation / checkpoint:
  - Continue Vulkan `Float32` from the remaining explicit TODO boundary:
    rank-N `contract` first, then eligible structural matrix kernels, then
    reducers/factors only with dedicated `Float32` tolerances and oracles.
- Unresolved issues:
  - Vulkan `Float32` `contract`, structural matrix kernels, reducers, and
    factor/solve kernels remain fail-closed.
  - CUDA `Float32` placement and scalar `Float32` values remain fail-closed.
- Signature: Codex GPT-5.4
