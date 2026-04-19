# Session Report Index Part 05

Source: `.agents/SESSION_REPORT.md`

## 2026-04-18 08:56 CEST - Vulkan Float64 Stats Normal Quantile

- Objective attempted:
  - Continue `TENSOR-100F` with multiple GPT-5.4 agents by landing Vulkan
    `Float64` `stats/normal-quantile` behind the existing backend-neutral
    Tensor surface.
- Workspace/target:
  - `/home/christos/Omni`, `csrc/tensor_vulkan_normal_quantile_f64.comp`,
    `csrc/tensor_vulkan_normal_quantile_f64_spv.c`,
    `csrc/tensor_vulkan_helpers.c`, `scripts/build_omni_chelpers.sh`,
    `project.json`, `src/lisp/tensor_vulkan_backend.c3`,
    `src/lisp/prim_tensor.c3`,
    `src/lisp/tests_advanced_stdlib_module_groups.c3`, `TODO.md`,
    `.agents/PLAN.md`, `memory/CHANGELOG.md`, Tensor/Vulkan docs, and session
    reports.
- Code/configuration changes:
  - Added a dedicated Vulkan `Float64` inverse-normal shader/helper with
    input/output buffers plus a `uint32` status buffer.
  - Routed direct Tensor unary math and `map stats/normal-quantile` for dense
    row-major Vulkan `Float64` tensors through the new helper, preserving
    Vulkan placement and dtype without CPU fallback or Float32 downcast.
  - The shader avoids unavailable Vulkan 1.0 double `log` by inverting the
    landed Float64 normal-CDF approximation with bounded bisection.
  - Added focused advanced stdlib module coverage for valid values, probability
    domain failures, non-finite failures, and non-finite status priority.
  - Updated TODO, plan, changelog, language/reference docs, and the Vulkan
    roadmap so CUDA-first dtype-changing rounding is now the active next item.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_normal_quantile_f64.comp -o /tmp/omni_tensor_vulkan_normal_quantile_f64.spv`
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_normal_quantile_f64.spv`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - Direct Vulkan `--eval` smokes for direct/map `Float64` quantile values,
    probability-domain failure, and non-finite failure.
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- Key results:
  - Direct and mapped Vulkan `Float64` quantile smokes returned `true` for
    `0.025`, `0.5`, and `0.975` with `1e-5` tail tolerance.
  - Invalid `0` probability returned
    `stats/normal-quantile: probability must be between 0 and 1`.
  - Vulkan-produced non-finite input returned
    `stats/normal-quantile: expected finite numeric input`.
  - Host focused `advanced-collections-module` passed `pass=1317 fail=0`.
  - Bounded-container focused `advanced-collections-module` passed
    `pass=1300 fail=0`.
- Invalidated assumptions / negative memory:
  - Do not keep treating Vulkan `Float64` quantile as fail-closed; it is now
    shipped through a Float64 status-bearing helper.
  - Do not use log-based inverse-CDF approximations in the Vulkan 1.0 Float64
    shader path; local validation rejects `log(double)`.
- Current best recommendation:
  - Continue with CUDA-first dtype-changing Tensor rounding to
    `Tensor BigInteger`; do not add same-dtype GPU rounding map opcodes.
- Unresolved issues:
  - The unsupported Vulkan `map floor` `handle` interaction remains open and
    separate from Tensor routing.
- Signature: Codex GPT-5

## 2026-04-18 06:27 CEST - Vulkan Float32 Stats Normal Quantile

- Objective attempted:
  - Continue `TENSOR-100F` with multiple GPT-5.4 agents by landing Vulkan
    `Float32` `stats/normal-quantile` without using the statusless generic
    Vulkan unary helper, and by keeping all remaining Float64/deferred work in
    TODO.
- Workspace/target:
  - `/home/christos/Omni`, `csrc/tensor_vulkan_normal_quantile_f32.comp`,
    `csrc/tensor_vulkan_normal_quantile_f32_spv.c`,
    `csrc/tensor_vulkan_helpers.c`, `scripts/build_omni_chelpers.sh`,
    `project.json`, `src/lisp/tensor_vulkan_backend.c3`,
    `src/lisp/prim_tensor.c3`,
    `src/lisp/tests_advanced_stdlib_module_groups.c3`, `TODO.md`,
    `.agents/PLAN.md`, `memory/CHANGELOG.md`, Tensor/Vulkan docs, and session
    reports.
- Code/configuration changes:
  - Added a dedicated Vulkan `Float32` inverse-normal shader with input/output
    buffers plus a separate `uint32` status binding.
  - Added `omni_tensor_backend_vulkan_map_normal_quantile_f32`, wired the
    generated SPIR-V object into helper/project builds, and routed op `20` only
    through the status-bearing helper for Vulkan `Float32`.
  - Mapped Vulkan quantile status `1` to
    `stats/normal-quantile: probability must be between 0 and 1` and status
    `2` to `stats/normal-quantile: expected finite numeric input`; invalid
    results destroy the output before surfacing an error.
  - Added focused Vulkan tests for direct/map valid values, finite endpoint
    domain errors, Vulkan-produced non-finite input, mixed invalid priority,
    and explicit Vulkan `Float64` fail-closed behavior.
  - Updated a stale Vulkan unsupported-callable test to assert the direct
    fail-closed message with `test_error_contains`; recorded the separate
    `handle` stack-overflow interaction as an open TODO instead of masking it
    through Tensor routing.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_normal_quantile_f32.comp -o /tmp/omni_tensor_vulkan_normal_quantile_f32.spv`
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_normal_quantile_f32.spv`
  - Direct Vulkan `--eval` smokes for valid direct/map `Float32`, zero/one
    probability errors, Vulkan-produced `NaN`, mixed `[0.0 NaN]` priority, and
    direct/map `Float64` fail-closed behavior.
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- Key results:
  - Direct Vulkan Float32 quantile smoke returned `true`; direct map smoke also
    returned `true`.
  - Finite endpoint probabilities returned
    `stats/normal-quantile: probability must be between 0 and 1`.
  - Vulkan-produced non-finite input and mixed domain/non-finite input returned
    `stats/normal-quantile: expected finite numeric input`.
  - Vulkan Float64 quantile remains fail-closed with
    `stats/normal-quantile: Vulkan currently supports dense row-major Float32 tensors`.
  - Host focused `advanced-collections-module` passed `pass=1311 fail=0`.
  - Bounded-container focused `advanced-collections-module` passed
    `pass=1298 fail=0`.
- Invalidated assumptions / negative memory:
  - Do not keep treating all Vulkan quantile as fail-closed: Vulkan Float32 is
    shipped through the status-bearing helper.
  - Do not infer Vulkan Float64 quantile from Float32 support, do not downcast,
    and do not assign op `20` to the existing statusless unary helper.
  - Do not widen Vulkan `floor` or Tensor map routing to hide the separate
    `handle` stack-overflow interaction around unsupported lazy map errors.
- Current best recommendation:
  - Continue with Vulkan `Float64` `stats/normal-cdf` policy/implementation
    first, then Vulkan `Float64` quantile policy/implementation, before moving
    to CUDA-first dtype-changing rounding.
- Unresolved issues:
  - Vulkan `Float64` `stats/normal-cdf` and `stats/normal-quantile` remain
    fail-closed and are tracked as explicit TODO items.
  - The unsupported Vulkan `map floor` `handle` interaction is tracked as a
    separate TODO; direct fail-closed behavior remains valid.
- Signature: Codex GPT-5

## 2026-04-18 05:52 CEST - CUDA Stats Normal Quantile Status ABI

- Objective attempted:
  - Continue `TENSOR-100F` with multiple GPT-5.4 agents by landing the
    probability-status boundary for CUDA `stats/normal-quantile` and updating
    the active TODO/design artifacts so deferred Vulkan work stays explicit.
- Workspace/target:
  - `/home/christos/Omni`, `csrc/tensor_cuda_scientific_unary.cu`,
    `csrc/tensor_cuda_helpers.c`, `src/lisp/prim_tensor.c3`,
    `src/lisp/tensor_cuda_backend.c3`,
    `src/lisp/tests_advanced_stdlib_module_groups.c3`, `TODO.md`,
    `.agents/PLAN.md`, `memory/CHANGELOG.md`, Tensor scientific docs, Vulkan
    roadmap docs, and session reports.
- Code/configuration changes:
  - Added CUDA scientific unary op `20` for `stats/normal-quantile`, generated
    from CUDA C/libdevice PTX using `normcdfinv` for dense row-major
    `Float64`/`Float32` CUDA tensors.
  - Added a CUDA probability-status word for op `20`: raw device status `0`
    remains success, `1` reports probability outside `0 < p < 1`, and `2`
    reports non-finite input. The kernel uses `atomicMax`, so non-finite input
    deterministically takes priority over a domain endpoint in mixed invalid
    tensors.
  - Mapped CUDA status codes to scalar-compatible Tensor diagnostics before
    exposing output: probability-domain failure and finite-input failure both
    fail closed without CPU fallback.
  - Added focused CUDA regressions for valid quantile values, endpoint domain
    failures, non-finite failure, mixed invalid priority, direct Float32
    placement/dtype preservation, destination realization, and Vulkan
    fail-closed quantile diagnostics.
  - Updated planning and docs to record CUDA quantile as shipped. At that
    checkpoint, Vulkan quantile still needed a status-bearing helper; this is
    superseded by the later `06:27 CEST` Vulkan checkpoint for Float32, while
    Vulkan Float64 remains deferred.
- Commands run:
  - `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75 csrc/tensor_cuda_scientific_unary.cu -o /tmp/omni_tensor_cuda_scientific_unary.ptx`
  - `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75 /tmp/omni_tensor_cuda_scientific_unary.ptx -o /tmp/omni_tensor_cuda_scientific_unary.cubin`
  - `cc -O2 -fPIC -I/usr/local/include -I/usr/include -c csrc/tensor_cuda_helpers.c -o /tmp/tensor_cuda_helpers.o`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - Direct CUDA valid/domain/non-finite/mixed-invalid and Vulkan fail-closed
    `--eval` smokes.
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- Key results:
  - Direct CUDA quantile valid smoke returned `true`.
  - CUDA invalid probability returned
    `stats/normal-quantile: probability must be between 0 and 1`.
  - CUDA `NaN` input and mixed `[0.0 NaN]` input returned
    `stats/normal-quantile: expected finite numeric input`.
  - Direct Vulkan `Float32` quantile still returned `tensor/backend-unsupported`
    at this CUDA checkpoint. Superseded by the later `06:27 CEST` Vulkan
    Float32 quantile checkpoint.
  - Host focused `advanced-collections-module` passed `pass=1307 fail=0`.
  - Bounded-container focused `advanced-collections-module` passed
    `pass=1294 fail=0`.
  - Primitive docs parity and Stage 3 source parity passed.
- Invalidated assumptions / negative memory:
  - Do not describe CUDA quantile status as an external sentinel scheme. The
    raw CUDA status word is now `0` on success; status priority is explicit and
    tested.
  - Do not assign quantile op `20` to the current Vulkan two-buffer unary
    helper; it lacks the required status binding/copyback path.
- Current best recommendation:
  - Superseded for Vulkan Float32 by the later `06:27 CEST` checkpoint.
    Continue with Vulkan Float64 distribution policy work.
- Unresolved issues:
  - Vulkan `Float64` `stats/normal-cdf` and `stats/normal-quantile`,
    dtype-changing rounding, view/layout metadata, fixed-width complex, and
    measurement-led SVD/eigen work remain tracked in TODO.
- Signature: Codex GPT-5

## 2026-04-18 05:21 CEST - Vulkan Map Preflight Hardening

- Objective attempted:
  - Continue `TENSOR-100F` by closing the first non-Docker backend lane:
    Vulkan `map` preflight ordering before additional callable broadening.
- Workspace/target:
  - `/home/christos/Omni`, `src/lisp/prim_tensor.c3`,
    `src/lisp/tests_advanced_stdlib_module_groups.c3`, `TODO.md`,
    `.agents/PLAN.md`, `memory/CHANGELOG.md`,
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`,
    `docs/areas/tensor-scientific.md`, and session reports.
- Code/configuration changes:
  - Added recursive `tensor_expr_has_non_vulkan_device` to mirror the CUDA
    preflight guard.
  - Reordered `tensor_map_try_vulkan_value` and
    `tensor_map_try_vulkan_direct` so callable support, recursive
    device-placement checks, dtype-family checks, and exact dtype checks run
    before `tensor_expr_resolve_concrete_any_device`.
  - Hardened direct Tensor `min` / `max` because it uses the same Vulkan map
    helper family: mixed CPU/Vulkan lazy operands now fail before CPU lazy
    materialization.
  - Added focused regressions for mixed CPU/Vulkan lazy `map`, unsupported
    binary Vulkan callable preflight, and mixed CPU/Vulkan lazy `min`.
- Commands run:
  - `c3c build --obj-out obj`
  - Direct `--eval` Vulkan smokes for mixed CPU/Vulkan lazy `map`, unsupported
    binary Vulkan callable preflight, unsupported unary callable preflight, and
    mixed CPU/Vulkan lazy `min`.
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - Targeted `git diff --check`.
- Key results:
  - Mixed CPU/Vulkan lazy `map` now returns
    `map: Vulkan operands must remain Vulkan-placed` instead of materializing
    the CPU lazy operand and failing with `map: function result dtype mismatch`.
  - Unsupported binary Vulkan callable preflight returns
    `map: Vulkan currently supports Float64 and Float32 arithmetic kernels`
    before materializing the CPU lazy operand.
  - Direct `min` with a CPU lazy operand and Vulkan operand now returns
    `minmax: Vulkan operands must remain Vulkan-placed`.
  - Bounded-container focused `advanced-collections-module` passed
    `pass=1287 fail=0`.
- Invalidated assumptions / negative memory:
  - Do not treat direct `min` / `max` as outside the Vulkan map preflight
    boundary; it is map-backed and needs the same no-hidden-materialization
    ordering.
- Current best recommendation:
  - Next non-Docker implementation lane is the shared GPU probability-domain
    status ABI for `stats/normal-quantile`, before CUDA/Vulkan quantile opcodes.
- Unresolved issues:
  - No GPU quantile support was added in this checkpoint.
  - The validation Docker image architecture bug remains a separate TODO.
- Signature: Codex GPT-5

## 2026-04-18 04:53 CEST - Non-Docker Tensor Backend Plan And TODO Reshape

- Objective attempted:
  - Turn the remaining non-Docker CUDA/Vulkan/Tensor issues into an explicit
    implementation plan and concrete TODO entries.
- Workspace/target:
  - `/home/christos/Omni`, active Tensor backend planning artifacts:
    `.agents/PLAN.md`, `TODO.md`, and
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
- Planning changes:
  - Added an active non-Docker implementation order to `.agents/PLAN.md`.
  - Populated `TODO.md` with independently pickable residual work for Vulkan
    map preflight hardening, GPU quantile status handling, CUDA quantile,
    Vulkan Float32 quantile, Vulkan Float64 normal-CDF policy, CUDA-first
    dtype-changing rounding, Vulkan rounding policy, Tensor view metadata,
    first view-backed Vulkan structural dispatch, fixed-width complex, direct
    Vulkan general eigenpairs, and measurement-led SVD/eigen performance.
  - Updated the Vulkan math-library roadmap next checkpoint so future work
    starts from the same ordered plan instead of the older broad residual list.
- Commands run:
  - `sed`/`rg` inspections of `TODO.md`, `.agents/PLAN.md`, current session
    reports, and Vulkan/Tensor planning docs.
  - `date '+%Y-%m-%d %H:%M %Z'`.
  - `jj diff --stat`.
- Key results:
  - Non-Docker execution order is now: Vulkan map preflight hardening; shared
    GPU probability-domain status ABI; CUDA quantile; Vulkan Float32 quantile;
    Vulkan Float64 normal-CDF policy; CUDA-first dtype-changing rounding;
    Tensor view/layout metadata; first read-only view-backed Vulkan structural
    kernel; fixed-width complex before Vulkan complex/eigenpairs; measurement
    before large SVD/eigen rewrites.
- Invalidated assumptions / negative memory:
  - Do not treat GPU quantile as a single opcode task; it is blocked on
    probability-domain status propagation and a documented inverse-CDF
    approximation.
  - Do not treat GPU rounding as ordinary same-dtype unary `map`; the public
    Tensor result contract is `Tensor BigInteger`.
  - Do not treat stride parameters alone as view support; Tensor values need
    storage offset, backing extent, and alias/owner metadata first.
- Current best recommendation:
  - Start implementation with Vulkan map preflight ordering, then the shared
    GPU quantile status ABI. Those reduce risk for later CUDA/Vulkan callable
    broadening.
- Unresolved issues:
  - This checkpoint changed planning artifacts only. No runtime code was
    changed and no runtime tests were run.
  - The validation Docker image architecture fix remains deliberately outside
    this non-Docker plan.
- Signature: Codex GPT-5

## 2026-04-18 04:24 CEST - Tensor Quantile And Vulkan Float32 Normal CDF

- Objective attempted:
  - Continue `TENSOR-100F` with multiple GPT-5.4 agents, prioritize the
    remaining Tensor distribution lanes, and update the design docs/TODO so
    deferred GPU work is explicit.
- Workspace/target:
  - `/home/christos/Omni`, Tensor unary math dispatch, Vulkan Float32 unary
    shader/helper, advanced collection tests, Tensor scientific docs, TODO,
    changelog, and active plan artifacts.
- Code/configuration changes:
  - `prim_stats_normal_quantile` now routes Tensor operands through
    `tensor_unary_math_value`.
  - Added checked CPU Tensor `stats/normal-quantile` evaluation using
    `omni_boost_math_standard_normal_quantile`; invalid probabilities fail the
    whole operation with the scalar-compatible domain diagnostic.
  - Added Vulkan `Float32` `stats/normal-cdf` fixed op id `19` through the
    dedicated unary helper. The shader uses a same-dtype Float32 approximation,
    preserves Vulkan placement, and leaves Vulkan `Float64` fail-closed.
  - Regenerated `csrc/tensor_vulkan_map_unary_f32_spv.c`.
  - Added tests for CPU Tensor quantile, CUDA/Vulkan quantile fail-closed
    behavior, and Vulkan Float32 normal-CDF direct/map execution.
  - Converted CUDA construction-time map fail-closed assertions from Lisp
    `handle` expressions to C harness `test_error_contains`, avoiding the
    known stack-overflow handler path around immediate CUDA map errors.
- Commands run:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_map_unary_f32.comp -o /tmp/omni_tensor_vulkan_map_unary_f32.spv`
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_map_unary_f32.spv`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - Direct CPU/CUDA/Vulkan smokes for Tensor quantile, GPU quantile fail-closed
    behavior, and Vulkan Float32 normal-CDF.
  - `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `./scripts/check_primitive_docs_parity.sh`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - Targeted `git diff --check` for touched files.
- Key results:
  - Direct Vulkan Float32 `stats/normal-cdf 1.0` returned
    `0.841344714164734`.
  - Direct CPU Tensor `stats/normal-quantile 0.5` returned `0.0`.
  - CUDA/Vulkan Tensor `stats/normal-quantile` fail closed with
    `tensor/backend-unsupported` on device tensors.
  - Host focused `advanced-collections-module` passed `pass=1296 fail=0`.
  - Bounded-container focused `advanced-collections-module` passed
    `pass=1283 fail=0` with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
- Invalidated assumptions / negative memory:
  - Do not treat CPU Tensor quantile as unimplemented; it is shipped with a
    whole-operation probability-domain failure contract.
  - Do not add GPU quantile by only assigning an opcode. CUDA/Vulkan still need
    per-element probability-domain status propagation and a documented
    inverse-CDF approximation/tolerance.
  - Do not infer Vulkan Float64 normal-CDF support from Vulkan Float32 or CUDA
    opcode `19`; Float64 remains blocked on a double approximation policy.
- Current best recommendation:
  - Next feature work should pick either the GPU quantile status/algorithm
    design, Vulkan Float64 normal-CDF approximation policy, dtype-changing GPU
    rounding result path, or representation-first layout/view metadata. The
    layout/view lane is blocked until Tensor storage offset, backing extent,
    and owner/alias metadata are explicit.
- Unresolved issues:
  - The validation Docker image still needs the native C3 toolchain fix before
    parent-level broad validation can run without `OMNI_VALIDATION_TOOLCHAIN_ROOT`.
- Signature: Codex GPT-5
