# Active TODO Index Part 03

Source: `TODO.md`

      single-axis wording to `contract: Vulkan Float64 contract kernel failed`.
  - shipped direct Vulkan `matrix/svd` factor-output slice:
    - added `csrc/tensor_vulkan_svd_f64.comp`, generated
      `csrc/tensor_vulkan_svd_f64_spv.c`, and the
      `omni_tensor_backend_vulkan_svd_f64` helper for dense row-major
      Vulkan `Float64` rank-2 inputs;
    - public `matrix/svd` now routes concrete and lazy Vulkan inputs through
      Vulkan before the CPU SVD fallback and returns Vulkan-placed `u`, `s`,
      and `v` tensors with shapes `[rows k]`, `[k]`, and `[cols k]`;
    - the Vulkan path preserves the existing reduced SVD dictionary contract,
      keeps CPU inspection explicit through `to-device 'cpu`, and asserts no
      hidden `LAPACKE_dgesvd` counter movement for direct or lazy Vulkan
      inputs;
    - the factor-output helper now uses storage-buffer Gram scratch behind the
      public `u` output payload, so `k = min(rows, columns) > 64` is supported
      without hidden CPU/LAPACK fallback while preserving 32-bit shader index
      guards; unsupported dtype/layout and unsupported non-CPU cases fail
      closed with Tensor backend diagnostics.
  - shipped direct Vulkan symmetric eigen slice:
    - added `csrc/tensor_vulkan_symmetric_eigen_f64.comp`, generated
      `csrc/tensor_vulkan_symmetric_eigen_f64_spv.c`, and the
      `omni_tensor_backend_vulkan_symmetric_eigen_f64` helper for dense
      row-major Vulkan `Float64` square inputs;
    - public `matrix/eigenvalues` and `matrix/eigenvectors` now route concrete
      and lazy Vulkan inputs through Vulkan before the CPU resolver and return
      Vulkan-placed values and aligned vector columns;
    - the Vulkan path preserves the existing symmetric-real output contracts,
      keeps CPU inspection explicit through `to-device 'cpu`, maps
      nonsymmetric Vulkan input to `tensor/not-symmetric`, and asserts no
      hidden `LAPACKE_dsyev` counter movement;
    - the symmetric eigen helper uses storage-buffer scratch behind the public
      Vulkan outputs, so dense row-major Vulkan `Float64` square symmetric
      inputs support `n > 64` within helper resource limits, 32-bit shader
      index guards, and the Jacobi iteration guard; unsupported dtype/layout,
      unsupported shape/resource bounds, missing Vulkan/Float64 capability,
      and unsupported non-CPU cases fail closed with Tensor backend diagnostics.
  - shipped first `TENSOR-100G` slice:
    - added `omni_tensor_backend_vulkan_solve_parallel_f64`, generated
      `csrc/tensor_vulkan_solve_parallel_f64_spv.c`, and routed public
      `matrix/solve` to it for Vulkan `Float64` systems with `n >= 3`;
    - kept the serial Vulkan solve helper as the small-size path for `n < 3`;
    - implemented a 64-invocation workgroup-parallel partial-pivot Gaussian
      elimination shader with parallel pivot reduction, row swaps,
      elimination, and RHS-column back-substitution;
    - added an explicit private `uint` status buffer for the parallel helper
      instead of extending the old trailing-`double` serial status sentinel;
    - added availability-gated tests for parallel vector RHS, matrix RHS,
      rank/shape preservation, singular diagnostics, parallel-helper counter
      movement, and unchanged LAPACK `dgesv` call count.
  - shipped staged `TENSOR-100G` slice:
    - added `csrc/tensor_vulkan_solve_parallel_init_f64.comp` and generated
      `csrc/tensor_vulkan_solve_parallel_init_f64_spv.c`;
    - records init/copy and factor/solve dispatches in one command buffer with
      a `vkCmdPipelineBarrier` buffer-memory dependency between them;
    - init dispatch covers `max(n*n, n*rhs_cols)` work items, so coefficient
      and RHS copies are not capped at one 64-lane workgroup;
    - added regressions for 2x2 serial-threshold counter stability, staged
      pivoting, 9x9 staged copy beyond one workgroup, and post-elimination
      singular diagnostics.
  - shipped multi-dispatch `TENSOR-100G` slice:
    - added pivot/swap, elimination, and backsolve shaders under
      `csrc/tensor_vulkan_solve_multi_*_f64.comp`;
    - generated and wired the corresponding SPIR-V C sources;
    - reworked the parallel solve helper to record init/copy, per-pivot
      pivot/swap, multi-workgroup elimination, and descending backsolve
      dispatches into one Vulkan command buffer with buffer barriers between
      dependent stages;
    - added a private multi-dispatch helper counter and tests for serial
      threshold isolation, counter movement, no-LAPACK behavior, and dense 9x9
      elimination correctness.
  - shipped cross-workgroup pivot `TENSOR-100G` slice:
    - split pivot selection into scan, ping-pong reduce, commit, and row-swap
      shader stages;
    - added a private `double` pivot magnitude scratch buffer and expanded the
      private typed `uint` status buffer for candidate row scratch slots;
    - reworked the solve command buffer so each pivot scans candidate rows
      across workgroups, recursively reduces candidates with barriers, commits
      the selected row, then swaps rows before elimination;
    - added a private pivot-reduce counter and a 65x65 solve regression that
      proves the cross-workgroup pivot-reduction stage executes without LAPACK.
  - shipped tiled LU staging `TENSOR-100G` slice:
    - added `csrc/tensor_vulkan_solve_multi_factor_f64.comp` and generated
      SPIR-V wiring for a per-pivot panel-factor stage;
    - expanded the private solve ABI with a factor scratch buffer at binding
      `5`, a factor pipeline, and four-buffer barriers covering output,
      status, pivot scratch, and factor scratch;
    - the helper now computes `L[row,pivot]` factors in their own dispatch
      after row swap and before elimination, then elimination consumes staged
      factors instead of recomputing them in every coefficient/RHS update;
    - added a private factor-stage counter and dense 65x65 `I + J`
      regression proving the factor stage executes without LAPACK while
      preserving the public solve contract.
  - shipped measured-threshold `TENSOR-100G` slice:
    - replaced the fixed `n >= 3` bring-up route with private
      `OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_MIN_N = 65`;
    - added a private serial solve call counter so tests prove below-threshold
      systems execute the serial helper directly instead of only proving
      parallel counters stayed unchanged;
    - recorded local in-process threshold evidence under `build/`, where
      `3`, `9`, `16`, and `32` were tied or serial-favorable and `65` was the
      first tested size where the staged parallel route won across identity and
      dense fixtures;
    - added availability-gated tests proving `2x2` and `9x9` stay serial,
      `65x65` enters the parallel helper without LAPACK, pivot/factor counters
      still move, and a `65x65` singular system uses the parallel status path.
  - shipped Vulkan `Float32` staged parallel solve slice:
    - added dedicated `Float32` ports of the staged Vulkan solve shader family
      and generated SPIR-V C embeds;
    - made the staged solve helper dtype-aware and added
      `omni_tensor_backend_vulkan_solve_parallel_f32` without hidden
      widening/downcast, CPU fallback, or LAPACK routing;
    - routed public Vulkan `Float32` `matrix/solve` systems with `n >= 65`
      through the staged helper while preserving serial routing for `2x2` and
      `9x9`;
    - added dtype-specific `Float32`/`Float64` solve counters so tests prove
      the staged `Float32` path does not borrow the `Float64` helper;
    - recorded local parity timing logs under
      `build/vulkan_solve_f32_threshold_20260417_*`: on this stack the
      `65x65` staged and forced-serial `Float32` identity/dense probes were
      tied within measurement noise, so `65` is a parity threshold rather than
      a decisive speedup claim.
  - current negative constraints:
    - do not resume the rolled-back generic Vulkan unary `map` mode-3 branch;
      the shipped unary `+` / `abs` / unary `-` / unary `sqrt` support uses a
      separate unary shader/helper ABI;
    - do not treat the serial Vulkan LU/solve/inverse shaders as the
      performance solution;
    - do not lower `BigInteger`, `BigFloat`, or `BigComplex` to Vulkan;
    - do not hide unsupported Vulkan cases behind CPU fallback.
  - next checkpoint: continue broader Vulkan math-library baseline work.
    Direct Vulkan `matrix/svd` factor output has landed for dense row-major
    `Float64` and `Float32` inputs and now uses storage-backed Gram scratch for `k > 64`;
    keep the plan note as the design record and performance tracker. Direct
    Vulkan `matrix/eigenvalues` and
    `matrix/eigenvectors` have landed for dense row-major `Float64` square
    symmetric inputs, including `n > 64` through storage-buffer scratch within
    helper resource limits, 32-bit shader index guards, and the Jacobi
    iteration guard; keep the eigen plan note as the residual-boundary and
    performance tracker. Future `Float32` planning now lives in
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`.
    Direct general `matrix/eigenpairs` remains intentionally CPU-only and
    fail-closed on Vulkan operands until its gates land. Vulkan `Float32`
    execution has landed for placement/copyback, destination `realize`,
    elementwise `map`, unary helpers, direct `min`/`max`, rank-N `contract`,
    structural matrix kernels (`transpose`, `diagonal`, `diagonal-matrix`,
    `trace`), direct `matrix/rank`, direct `matrix/norm` selectors,
    `matrix/singular-values`, `matrix/svd`, serial factor/solve surfaces,
    staged parallel Vulkan `matrix/solve`, large-dense SVD robustness, and CPU
    factor/SVD oracle surfaces. CUDA `Float32` placement/copyback,
    destination `realize`, and eligible cuBLAS contract routing have also
    landed; scalar `Float32` runtime values are closed above. Remaining CUDA
    fixed-width complex structural matrix kernels, fixed-width complex numerical
    matrix kernels, and stride/view-backed Vulkan layouts stay
    fail-closed/deferred until dedicated gates land.
    `matrix/singular-values` /
    `matrix/svd` now support `k > 64` through
    storage-backed Gram scratch, while symmetric eigen now supports `n > 64`
    through storage-buffer scratch within helper resource limits, 32-bit shader
    index guards, and the Jacobi iteration guard. Unary `+`,
    `abs`, unary `-`, and unary `sqrt` have a separate unary Vulkan helper, and
    full blocked trailing-update LU remains a future performance lane only if
    measurements justify it.

## Recently Closed

- [x] `TENSOR-100F` Vulkan helper plumbing through status-readback
  - shipped slices: the mature serial Vulkan dispatch helper family now shares
    compute-pipeline creation, sequential storage-buffer descriptor
    layout/allocation/update helpers, standard one-time command
    recording/submission, and shared `Float64` status-payload readback
    boilerplate for compatible mapped-status paths.
  - shipped contract: status-domain semantics remain separate. Generic trailing
    `double` nonzero still maps to singular; SVD/singular-values status still
    distinguishes no-convergence; symmetric eigen status still distinguishes
    not-symmetric; and parallel solve intentionally remains on its separate
    `uint32_t` status-buffer ABI.
  - proof migration: serial solve, inverse, cholesky, QR, singular-values, SVD,
    and symmetric eigen status readers use the shared mapped readback path, and
    LU metadata validation reuses the generic trailing-status mapper while
    keeping pivot and swap-count validation LU-specific.
  - validation: `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
    host focused `advanced-collections-module` `pass=995 fail=0`,
    bounded-container focused `advanced-collections-module` `pass=982 fail=0`,
    primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check` passed.

- [x] `TENSOR-076C` native `Iterator(Tensor)` view semantics
  - shipped contract: `Iterator(^Tensor)` yields flat row-major Tensor elements,
    matching `(Array tensor)` and `(List tensor)` collection conversions.
    Lazy CPU Tensor expressions are realized once into Tensor storage before
    iteration; non-CPU device tensors fail closed until explicitly copied with
    `to-device 'cpu`.
  - validation: `c3c build --obj-out obj`, direct `Iterator(Tensor)` smokes,
    host focused `advanced-collections-module` `pass=969 fail=0`,
    bounded-container focused `advanced-collections-module` `pass=956 fail=0`,
    primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check` passed.

- [x] `TENSOR-076B` constructor-driven iterator materialization into `Tensor`
  - shipped slice: `(Tensor iterator)`, `(Tensor iterator dtype)`, and
    `(Tensor dtype shape iterator)` now consume finite numeric iterators through
    constructor dispatch. Iterator pipelines such as
    `(Tensor (map f (Iterator xs)))` materialize through `Tensor` without
    introducing public `delay`, `force`, or a special lazy realization surface.
  - docs: `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md` now prioritize constructor-driven materialization and
    demote `realize` to low-level destination-storage use.
  - validation: `c3c build --obj-out obj`, direct iterator-to-Tensor smokes,
    host focused `advanced-collections-module` `pass=964 fail=0`,
    bounded-container focused `advanced-collections-module` `pass=951 fail=0`,
    primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check` passed.

- [x] `TENSOR-100G` complete measured Vulkan parallel solve baseline
  - shipped solver path: public `matrix/solve` now routes Vulkan dense
    row-major `Float64` systems with `n >= 65` through the staged
    multi-dispatch parallel helper and keeps smaller systems on the serial
    Vulkan helper.
  - shipped implementation slices: separate parallel solve helper, staged
    init/copy and factor/solve dispatches, multi-dispatch pivot/swap,
    elimination and backsolve, cross-workgroup pivot reduction, per-pivot
    factor staging, private typed status buffers, no-LAPACK counter coverage,
    and measurement-backed threshold routing.
  - remaining solver direction: full blocked trailing-update LU is not current
    correctness-required work; reopen it only as a separate performance item if
    measurements justify the extra algorithmic scope.

- [x] `TENSOR-100E` complete portable Vulkan Tensor backend baseline
  - shipped baseline: runtime-optional Vulkan placement/introspection,
    explicit `to-device` roundtrips, SPIR-V `map` arithmetic and broadcasting,
    zero-or-more-axis `contract`, and baseline Vulkan matrix kernels for
    `transpose`, `diagonal`, `diagonal-matrix`, `trace`, `rank`, `lu`,
    `solve`, `determinant`, and `inverse`.
  - subsequent `TENSOR-100F1` shipped extensions added `cholesky`, `qr`,
    `matrix/singular-values`, and spectral/nuclear norm routing; keep that
    ownership under the live `TENSOR-100F` history above.
  - route forward: the remaining Vulkan math direction now lives in
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md` as active
    `TENSOR-100F` work, with `TENSOR-100G` closed as the measured parallel
    solver baseline.

- [x] `TENSOR-100D` add cuBLAS rank-2 CUDA contract fast path
  - shipped slice: runtime-loaded cuBLAS handle lifecycle now lives in the
    optional CUDA helper; `tensor-backends` reports a structured `cublas`
    backend entry.
  - execution: CUDA-placed dense row-major `Float64` rank-2/rank-2
    single-axis contractions route to `cublasDgemm_v2` for `[1 0]`, `[0 0]`,
    `[1 1]`, and `[0 1]`; rank-2/rank-1 and rank-1/rank-2 single-axis
    contractions route to `cublasDgemv_v2`. Results are CUDA-placed Tensors;
    callers copy back explicitly with `to-device 'cpu`.
  - unsupported CUDA contract shapes, dtypes, devices, or axis pairs fail with
    Tensor backend diagnostics instead of silently copying to CPU; zero-size
    CUDA contract dimensions fail closed until an explicit CUDA zero-fill path
    exists.
  - coverage: focused regressions cover all four cuBLAS rank-2/rank-2
    single-axis layouts plus matrix-vector and vector-matrix layouts when
    available, then force cuBLAS unavailable through the private
    backend-disable hook and assert both `tensor-backends` inventory reporting
    and `tensor/backend-unavailable` contract failure.
  - validation: `c3c build --obj-out obj`; direct host CUDA/cuBLAS contract
    smoke returned `154.0`; host focused `advanced-collections-module`
    pass=646 fail=0; bounded-container focused `advanced-collections-module`
    pass=633 fail=0; bounded-container `memory-lifetime-smoke` pass=226
    fail=0; bounded-container full `advanced` pass=1950 fail=0;
    `./scripts/build_omni_chelpers.sh`, primitive docs parity, Stage 3 source
    parity, and `git diff --check` passed. ASAN build was attempted but
    blocked by the local C3 sanitizer platform guard.

- [x] `TENSOR-100C` add optional CUDA Tensor copy backend
  - shipped slice: added a runtime-loaded `libcudart` helper with no required
    CUDA link dependency. CUDA availability now requires symbol resolution,
    visible device count, and a successful allocation/free probe.
  - current `to-device` behavior: target `'cuda` realizes CPU Tensor
    expressions and copies concrete `Float64` or `Float32` storage to CUDA
    when available; target `'cpu` copies CUDA Tensor storage back to native CPU
    storage.
  - unsupported CUDA dtypes outside `Float64` and `Float32` fail with
    `tensor/backend-unsupported`; unavailable or unusable CUDA fails with
    `tensor/backend-unavailable`.
  - validation: `c3c build --obj-out obj`; host focused
    `advanced-collections-module` pass=633 fail=0; direct host CUDA
    roundtrip returned `2.0`; bounded-container focused
    `advanced-collections-module` pass=619 fail=0; bounded-container
    `memory-lifetime-smoke` pass=226 fail=0; `git diff --check` passed.

- [x] `TENSOR-100B` add Tensor placement metadata and backend inventory
  - shipped slice: `TensorVal` now carries internal placement metadata and an
    opaque device handle/finalizer slot; ordinary constructors initialize CPU
    placement, `device` reports the stored placement, and CPU Tensor kernels
    reject non-CPU storage instead of silently reading backend handles.
  - public introspection: `tensor-backends` is registered and returns a list of
    dictionaries; CPU reports `available true`, while CUDA reports
    `available false` with reason `backend-unavailable`.
  - ownership guard: boundary clones refuse opaque non-CPU Tensor payloads
    until explicit copy semantics land, and `tensor_free_payload` is the single
    release authority for opaque device handles.
  - validation: `c3c build --obj-out obj`; host focused
    `advanced-collections-module` pass=633 fail=0; bounded-container focused
    `advanced-collections-module` pass=619 fail=0; bounded-container
    `memory-lifetime-smoke` pass=226 fail=0; direct `tensor-backends` and
    `to-device 'cpu` smokes passed.

- [x] `TENSOR-100A` implement CPU-only Tensor placement/introspection surface
  - shipped slice: `device` reports `'cpu` for current Tensor values, and
    `to-device` with target `'cpu` realizes Tensor expressions to CPU Tensor
    storage.
  - CUDA behavior: `to-device` with target `'cuda` fails closed with
    `tensor/backend-unavailable`; no CUDA runtime dependency was added.
  - registration: interpreter primitive table and AOT primitive hash table.
  - validation: `c3c build --obj-out obj`; focused
    `advanced-collections-module` pass=629 fail=0; direct `device` smoke
    returned `"cpu"`; direct handled CUDA smoke returned
    `"tensor/backend-unavailable"`; bounded-container focused
    `advanced-collections-module` pass=616 fail=0; bounded-container full
    `advanced` slice pass=1933 fail=0; primitive docs parity, Stage 3 source
    parity, and `git diff --check` passed.
