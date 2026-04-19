# Session Report Index Part 15

Source: `.agents/SESSION_REPORT.md`

## 2026-04-17 03:27 CEST - TENSOR-100E Vulkan Matrix Rank
- Objective attempted:
  - Continue `TENSOR-100E` with a backend-neutral `Float64` scalar reducer by
    routing public `matrix/rank` to Vulkan for eligible dense row-major rank-2
    inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_rank_f64.comp` and generated
    `csrc/tensor_vulkan_rank_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_rank_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/rank` handles
    concrete Vulkan tensors before the CPU-only storage check.
  - Added availability-gated Vulkan rank regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/rank` now supports Vulkan-placed dense row-major `Float64`
    rank-2 inputs and returns the scalar `Integer` rank.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the scalar rank result required by the public contract.
  - CPU Tensor `matrix/rank` behavior remains on the existing LAPACK-then-pure
    implementation.
  - Vulkan rank tests include a no-LAPACK check to guard against hidden CPU
    fallback.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_rank_f64.comp -o /tmp/omni_tensor_vulkan_rank_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_rank_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan rank smokes returned `2`, `1`, `2`, `1`, `2`, `0`, `0`,
    and `tensor/shape-mismatch`.
  - Direct CPU rank smoke returned `2`.
  - Host focused `advanced-collections-module`: `pass=722 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=709 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Vulkan rank currently mirrors the existing pure partial-pivot elimination
    fallback, not LAPACK/SVD rank semantics. Stride-aware views,
    non-contiguous layouts, non-`Float64` Vulkan dtypes, and larger
    decomposition-backed Vulkan algorithms remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 03:06 CEST - TENSOR-100E Vulkan Matrix Norm
- Objective attempted:
  - Continue `TENSOR-100E` with backend-neutral `Float64` scalar reducers by
    routing public `matrix/norm` to Vulkan for eligible dense row-major rank-2
    inputs and direct norm selectors.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_norm_f64.comp` and generated
    `csrc/tensor_vulkan_norm_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_norm_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/norm` handles
    concrete Vulkan tensors before the CPU-only storage check.
  - Added availability-gated Vulkan norm regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/norm` now supports Vulkan-placed dense row-major `Float64`
    rank-2 inputs for default/`'frobenius`, `'one`, `'infinity`, and `'max`.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the scalar result required by the public return contract.
  - At this checkpoint, SVD-backed `'spectral` and `'nuclear` failed closed on
    Vulkan with `tensor/backend-unsupported`; this was superseded later on
    2026-04-17 by the Vulkan singular-value helper slice.
  - CPU Tensor `matrix/norm` behavior, including LAPACK/fallback coverage for
    spectral/nuclear selectors, remains on the existing CPU implementation.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_norm_f64.comp -o /tmp/omni_tensor_vulkan_norm_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_norm_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan norm smokes returned `5.0`, `9.53939201416946`, `9.0`,
    `15.0`, `6.0`, `5.0`, `0.0`, and `tensor/backend-unsupported`.
  - Host focused `advanced-collections-module`: `pass=712 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=699 fail=0`.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    non-`Float64` Vulkan dtypes, SVD-backed Vulkan norm selectors, and Vulkan
    unary scientific `map` remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 02:53 CEST - TENSOR-100E Vulkan Matrix Trace
- Objective attempted:
  - Continue `TENSOR-100E` with a backend-neutral `Float64` reducer by routing
    public `matrix/trace` to Vulkan for eligible dense row-major square inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_trace_f64.comp` and generated
    `csrc/tensor_vulkan_trace_f64_spv.c`.
  - Wired the new SPIR-V C source into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_trace_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/trace` handles
    concrete Vulkan tensors before the CPU-only storage check and returns the
    scalar result after a one-scalar Vulkan result readback.
  - Added availability-gated Vulkan trace regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/trace` now supports Vulkan-placed dense row-major square
    `Float64` inputs.
  - The input tensor is not copied to CPU; the helper computes on Vulkan and
    reads back only the scalar result required by the public return contract.
  - CPU Tensor and Big* Tensor trace behavior remains on the existing CPU
    implementation.
  - Unsupported GPU/device cases fail closed rather than silently copying to CPU.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_trace_f64.comp -o /tmp/omni_tensor_vulkan_trace_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_trace_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan trace smokes returned `5.0`, `5.0`, and `0.0`.
  - Direct CPU trace smoke returned `5.0`.
  - Direct non-rank-2 trace error smoke preserved the existing shape diagnostic.
  - Host focused `advanced-collections-module`: `pass=705 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=692 fail=0`.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    non-`Float64` Vulkan dtypes, broader scalar reducers, and Vulkan unary
    scientific `map` remain unsupported.

Signature: Codex GPT-5

## 2026-04-17 02:42 CEST - TENSOR-100E Vulkan Matrix Diagonal Pair
- Objective attempted:
  - Continue `TENSOR-100E` with the next backend-neutral `Float64` structural
    matrix family by routing public `matrix/diagonal` and
    `matrix/diagonal-matrix` to Vulkan for eligible dense row-major inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_diagonal_f64.comp`,
    `csrc/tensor_vulkan_diagonal_matrix_f64.comp`, and generated SPIR-V C
    sources for both kernels.
  - Wired the new SPIR-V C sources into `project.json` and
    `scripts/build_omni_chelpers.sh`.
  - Added `omni_tensor_backend_vulkan_diagonal_f64` and
    `omni_tensor_backend_vulkan_diagonal_matrix_f64` in
    `csrc/tensor_vulkan_helpers.c`.
  - Added C3 externs in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/diagonal` and
    `matrix/diagonal-matrix` route Vulkan `Float64` dense row-major inputs
    before the CPU-only storage check and return Vulkan-placed Tensor results.
  - Added availability-gated Vulkan regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/diagonal` and `matrix/diagonal-matrix` now support
    Vulkan-placed dense row-major `Float64` inputs.
  - Results remain Vulkan-placed tensors; CPU inspection still requires
    explicit `to-device 'cpu`.
  - CPU Tensor and Big* Tensor behavior remains on the existing CPU
    implementation.
  - Unsupported GPU/device cases fail closed rather than silently copying to CPU.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0` for both new shaders: passed.
  - `spirv-val --target-env vulkan1.0` for both generated SPIR-V files: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan `matrix/diagonal` smokes returned `5.0`, `vulkan`,
    `vulkan`, and zero-size length `0`.
  - Direct Vulkan `matrix/diagonal-matrix` smokes returned `3.0`, `0.0`,
    `vulkan`, `vulkan`, and zero-size length `0`.
  - Direct CPU diagonal/diagonal-matrix smokes returned `5.0`, `3.0`, and
    `0.0`; rank-error smokes preserved the existing shape diagnostics.
  - Host focused `advanced-collections-module`: `pass=702 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=689 fail=0`.
- Unresolved issues / next actions:
  - Superseded by the later matrix trace entry for `matrix/trace`.
    Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    non-`Float64` Vulkan dtypes, and Vulkan unary scientific `map` remain
    unsupported.

Signature: Codex GPT-5

## 2026-04-17 03:05 CEST - TENSOR-100E Vulkan Matrix Transpose
- Objective attempted:
  - Continue `TENSOR-100E` with the next backend-neutral `Float64` kernel family
    by routing public `matrix/transpose` to Vulkan for eligible dense row-major
    rank-2 tensors.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/tensor_vulkan_transpose_f64.comp` and generated
    `csrc/tensor_vulkan_transpose_f64_spv.c`.
  - Added `omni_tensor_backend_vulkan_transpose_f64` in
    `csrc/tensor_vulkan_helpers.c` and wired the SPIR-V source into
    `project.json` plus `scripts/build_omni_chelpers.sh`.
  - Added the C3 extern in `src/lisp/tensor_vulkan_backend.c3`.
  - Updated `src/lisp/prim_tensor_matrix.c3` so `matrix/transpose` handles
    concrete Vulkan tensors before the CPU-only storage check and returns a
    Vulkan-placed result for dense row-major `Float64` rank-2 inputs.
  - Added availability-gated Vulkan transpose regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `matrix/transpose` now supports Vulkan-placed dense row-major
    `Float64` rank-2 tensors.
  - Results remain Vulkan-placed tensors; CPU inspection still requires
    explicit `to-device 'cpu`.
  - CPU Tensor and Big* Tensor transpose behavior remains on the existing CPU
    implementation.
  - Unsupported GPU/device cases fail closed rather than silently copying to CPU.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_transpose_f64.comp -o /tmp/omni_tensor_vulkan_transpose_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_transpose_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan transpose smokes returned `6.0`, `vulkan`, `vulkan`, and
    zero-size length `0`.
  - Direct CPU transpose smoke returned `6.0`.
  - Direct non-rank-2 transpose error smoke returned `tensor/shape-mismatch`.
  - Host focused `advanced-collections-module`: `pass=693 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=680 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    non-`Float64` Vulkan dtypes, and Vulkan unary scientific `map` remain
    unsupported.

Signature: Codex GPT-5

## 2026-04-17 02:35 CEST - TENSOR-100E Vulkan Multi-Axis Contract
- Objective attempted:
  - Continue `TENSOR-100E` by extending the Vulkan dense row-major `Float64`
    rank-N `contract` path from exactly one contracted axis pair to one or more
    explicit contracted axis pairs through the existing backend-neutral public
    `contract` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Updated `csrc/tensor_vulkan_contract_f64.comp` so contracted axes are read
    from metadata lists instead of single push-constant axes, and each output
    invocation flattens the full contracted coordinate space.
  - Regenerated `csrc/tensor_vulkan_contract_f64_spv.c`.
  - Updated `omni_tensor_backend_vulkan_contract_f64` in
    `csrc/tensor_vulkan_helpers.c` to validate/pass axis-list metadata and keep
    zero-contracted output zero-fill behavior.
  - Updated `src/lisp/tensor_vulkan_backend.c3` and
    `src/lisp/prim_tensor.c3` so public Vulkan `contract` accepts one or more
    explicit axis pairs for dense row-major `Float64` operands.
  - Added availability-gated scalar and rank-3 multi-axis Vulkan regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Key results:
  - Public `contract` now supports Vulkan-placed dense row-major rank-N
    `Float64` tensors with one or more contracted axis pairs.
  - Output axes remain ordered as free left axes followed by free right axes.
  - Results remain Vulkan-placed tensors; CPU inspection still requires
    explicit `to-device 'cpu`.
  - Zero-axis contractions, unsupported layouts/dtypes, and mixed CPU/Vulkan
    operands still fail closed with Tensor backend diagnostics.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_contract_f64.comp -o /tmp/omni_tensor_vulkan_contract_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_contract_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `70.0` for scalar multi-axis contract,
    `210.0` for rank-3 multi-axis contract, `460.0` for existing rank-N
    single-axis contract, `8.0` for binary Vulkan `map +`, and
    `tensor/backend-unsupported` for unsupported Vulkan `map sqrt`.
  - Host focused `advanced-collections-module`: `pass=689 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=676 fail=0`.
- Invalidated assumptions / failed approaches:
  - The earlier generic mode-3 Vulkan unary `map` branch remains rolled back and
    should not be resumed as the default baseline.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, zero-axis Vulkan contractions,
    and non-`Float64` Vulkan dtypes remain unsupported.
  - The next `TENSOR-100E` slice should move to another backend-neutral
    `Float64` kernel family, such as structural matrix transpose, or explicitly
    design layout/aliasing metadata before enabling stride-aware dispatch.

Signature: Codex GPT-5

## 2026-04-17 01:40 CEST - TENSOR-100E Vulkan Rank-N Contract
- Objective attempted:
  - Continue `TENSOR-100E` by extending the Vulkan `Float64` dense row-major
    single-axis `contract` path from rank-1/rank-2 coverage to rank-N coverage
    through the existing backend-neutral public `contract` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Replaced `csrc/tensor_vulkan_contract_f64.comp` with a
    rank/shape/stride metadata-buffer shader for dense row-major single-axis
    contractions.
  - Regenerated `csrc/tensor_vulkan_contract_f64_spv.c`.
  - Extended `omni_tensor_backend_vulkan_contract_f64` in
    `csrc/tensor_vulkan_helpers.c` to accept full operand/output rank, shape,
    and stride metadata, added the metadata storage buffer, switched generic
    dispatch to flat rank-N output indexing, and fixed the descriptor pool
    size for the four-buffer layout.
  - Updated `src/lisp/tensor_vulkan_backend.c3` and
    `src/lisp/prim_tensor.c3` so public Vulkan `contract` accepts dense
    row-major `Float64` tensors of arbitrary rank when exactly one axis pair
    is contracted.
  - Added availability-gated rank-3/rank-2, rank-2/rank-3, rank-3/rank-3,
    residency, and zero-contracted rank-3 regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`.
  - Updated TODO, plan, reference/spec, Vulkan decision/policy, Tensor area
    status, and changelog documentation.
- Key results:
  - Public `contract` now supports Vulkan-placed dense row-major rank-N
    `Float64` tensors with exactly one contracted axis.
  - Output axes are ordered as free left axes followed by free right axes.
  - Results remain Vulkan-placed tensors; CPU inspection still requires
    explicit `to-device 'cpu`.
  - Multi-axis contractions, unsupported layouts/dtypes, and mixed CPU/Vulkan
    operands still fail closed with Tensor backend diagnostics.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_contract_f64.comp -o /tmp/omni_tensor_vulkan_contract_f64.spv`:
    passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_contract_f64.spv`:
    passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan rank-N contract smokes returned `460.0`, `139.0`,
    `3340.0`, `vulkan`, and `0.0`.
  - Host focused `advanced-collections-module`: `pass=688 fail=0`.
  - Bounded-container focused `advanced-collections-module`:
    `pass=675 fail=0`.
  - `scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - `git diff --check`: passed.
- Invalidated assumptions / failed approaches:
  - A follow-up Vulkan unary `map` experiment was rolled back before completion.
    GLSL `double` transcendental calls such as `sin(double)` failed shader
    compilation, and an arithmetic-only generic mode-3 unary branch for
    `abs`/negation compiled but failed at runtime with
    `map: Vulkan Float64 kernel failed` while binary Vulkan `map` and rank-N
    `contract` remained healthy.
  - Do not resume that generic mode-3 branch as the baseline. If unary Vulkan
    map is pursued, use a separately debugged unary shader/helper entrypoint or
    first diagnose the current descriptor/dispatch failure. Do not use
    `Float32` casts or a hidden CPU fallback.
  - After rollback, direct sanity checks returned `8.0` for binary Vulkan
    `map +`, `460.0` for rank-N Vulkan `contract`, and
    `tensor/backend-unsupported` for unsupported Vulkan `map sqrt`.
    `c3c build --obj-out obj`, host focused `advanced-collections-module`
    (`pass=688 fail=0`), and bounded-container focused
    `advanced-collections-module` (`pass=675 fail=0`) still passed.
- Unresolved issues / next actions:
  - Stride-aware views, non-contiguous layouts, and multi-axis Vulkan
    contractions remain unsupported.
  - The next `TENSOR-100E` slice should move to the next eligible dense
    row-major `Float64` kernel family, or explicitly design layout/aliasing
    metadata before enabling stride-aware dispatch. Unary Vulkan `map` remains
    open, but should not continue through the rolled-back generic mode-3 branch.
  - `jj status` shows the expected broad dirty worktree from the ongoing
    Tensor/CUDA/LAPACK/Vulkan lane, including unrelated prior changes and the
    untracked `out` file.

Signature: Codex GPT-5
