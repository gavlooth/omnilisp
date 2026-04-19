# Agent Plan Index Part 03

Source: `.agents/PLAN.md`

    reuse or CPU fallback.
  - Superseded by later SVD-backed and CPU factor/SVD checkpoints:
    CPU and Vulkan-placed dense row-major `Float32` now support SVD-backed
    `matrix/norm` selectors through dtype-specific paths.
  - Validation: shader compile/`spirv-val`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct CPU/Vulkan smokes, host focused
    `advanced-collections-module` passed `1098/0`, bounded-container focused
    `advanced-collections-module` passed `1085/0`, primitive docs parity,
    Stage 3 source parity, and targeted `git diff --check`.
- 2026-04-17 Vulkan large-`k` SVD/singular-values checkpoint:
  - `matrix/singular-values` and direct `matrix/svd` no longer rely on fixed
    `k <= 64` private Gram/eigenvalue arrays. Both shaders now keep Gram
    scratch in Vulkan storage-buffer payload space behind the public output
    buffers, preserving public shapes and Vulkan placement.
  - Public Vulkan `matrix/norm` `'spectral` and `'nuclear` inherit the
    expanded singular-value helper and continue to read back only scalar norm
    results.
  - The shipped boundary is semantic large-`k` support with 32-bit shader index
    guards and no hidden CPU/LAPACK fallback; performance-oriented tiled or
    multi-dispatch large-`k` algorithms remain a separate measured TODO.
  - Validation so far: shader compile/`spirv-val`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, host focused `advanced-collections-module`
    passed `999/0`.
- 2026-04-17 Vulkan status-readback helper factoring checkpoint:
  - Shared `Float64` status-payload copy/readback boilerplate now covers the
    compatible mapped-status paths while preserving distinct status-domain
    mappers.
  - Generic trailing status, SVD/singular-values status, and symmetric-eigen
    status remain semantically separate. Parallel solve remains separate by
    design because it uses a typed `uint32_t` status buffer, not the trailing
    `double` payload convention.
  - LU metadata validation now reuses the generic trailing-status mapper while
    keeping pivot and swap-count validation LU-specific.
  - Validation: `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
    host focused `advanced-collections-module` passed `995/0`,
    bounded-container focused `advanced-collections-module` passed `982/0`,
    primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check`.
- 2026-04-17 Vulkan serial dispatch helper factoring checkpoint:
  - The mature serial Vulkan dispatch helper family now shares sequential
    storage-buffer descriptor layout/allocation/update helpers and a standard
    one-time command recording/submission helper, while preserving descriptor
    binding order, synchronous submit/wait behavior, output ownership, result
    placement, and no hidden CPU/LAPACK fallback.
  - Later checkpoint above factored compatible `Float64` status-readback
    boilerplate. Parallel solve remains separate by design because it uses a
    typed `uint32_t` status-buffer ABI.
  - Validation: `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
    host focused `advanced-collections-module` passed `995/0`, bounded-container
    focused `advanced-collections-module` passed `982/0`, primitive docs
    parity, Stage 3 source parity, and targeted `git diff --check`.
- 2026-04-17 Vulkan unary identity and helper factoring checkpoint:
  - Public unary `+` is now a one-or-two-argument dispatched primitive:
    `(+ scalar)` and `(+ tensor)` are identity, while `(+ a b c)` remains an
    arity error. The no-auto-partial regression moved to `*`, which remains
    binary-only.
  - Dense row-major Vulkan `Float64` `(map + tensor)` routes through the
    separate unary helper identity opcode and returns a Vulkan-placed Tensor.
    Direct `(+ <vulkan-tensor>)` preserves placement by identity.
  - The mature serial Vulkan dispatch helper family now shares
    `omni_tensor_vulkan_create_compute_pipeline_for_shader`; later checkpoint
    above also factored descriptor and command-buffer plumbing. Status-copy
    factoring remains open under `TENSOR-100F`.
  - Validation: shader compile/`spirv-val`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, focused `advanced-unicode-iterator` passed
    `159/0`, focused `advanced-collections-module` passed `972/0`, bounded
    container focused `advanced-collections-module` passed `959/0`, primitive
    docs parity, Stage 3 source parity, and targeted `git diff --check`.
- 2026-04-17 direct Vulkan `matrix/svd` factor-output checkpoint:
  - Added `csrc/tensor_vulkan_svd_f64.comp`, generated
    `csrc/tensor_vulkan_svd_f64_spv.c`, and wired
    `omni_tensor_backend_vulkan_svd_f64` through the helper build and project
    sources.
  - Public `matrix/svd` now routes dense row-major Vulkan `Float64` rank-2
    tensors through Vulkan before CPU fallback, including lazy Vulkan inputs,
    and returns Vulkan-placed `u`, `s`, and `v` tensors with shapes
    `[rows k]`, `[k]`, and `[cols k]`. A later checkpoint removed the
    original `k <= 64` private-array cap with storage-buffer Gram scratch.
  - Direct and lazy Vulkan SVD assert unchanged `LAPACKE_dgesvd` counters;
    wrong rank and unsupported Vulkan cases fail closed with Tensor backend
    diagnostics.
  - Negative constraint: do not restore a second private `double[4096]`
    eigenvector array in the shader. That approach compiled but failed at
    runtime on this Vulkan stack with `tensor/backend-execution-failed`; the
    shipped shader stores the driving eigenvectors in the relevant output
    buffer.
  - Validation: shader compile/`spirv-val`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, focused `advanced-collections-module` passed
    `945/0`, JSON/shell syntax checks, and `git diff --check` passed.
- 2026-04-17 direct Vulkan symmetric eigen checkpoint:
  - Added `csrc/tensor_vulkan_symmetric_eigen_f64.comp`, generated
    `csrc/tensor_vulkan_symmetric_eigen_f64_spv.c`, and wired
    `omni_tensor_backend_vulkan_symmetric_eigen_f64` through the helper build
    and project sources.
  - Public `matrix/eigenvalues` and `matrix/eigenvectors` now route dense
    row-major Vulkan `Float64` square tensors through Vulkan before CPU
    fallback, including lazy Vulkan inputs, and return Vulkan-placed values and
    aligned vector columns. The helper uses storage-buffer scratch, so
    symmetric dense row-major Vulkan `Float64` square inputs support `n > 64`
    within helper resource limits, 32-bit shader index guards, and the Jacobi
    iteration guard.
  - Nonsymmetric Vulkan input raises `tensor/not-symmetric`; wrong shape,
    unsupported layout, unsupported dtype, missing Vulkan/Float64 capability,
    and unsupported resource bounds fail closed with Tensor backend
    diagnostics. General `matrix/eigenpairs` remains fail-closed on Vulkan
    while its public result contract is pointer-backed `BigComplex`.
  - Direct and lazy Vulkan symmetric eigen paths assert unchanged
    `LAPACKE_dsyev` counters.
  - Validation so far: shader compile/`spirv-val`, `./scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, direct runtime smokes, and focused
    `advanced-collections-module` passed `948/0`.
- 2026-04-17 audit-hardening implementation checkpoint:
  - Generic Vulkan contract failures now report
    `contract: Vulkan Float64 contract kernel failed`, not the stale
    single-axis message.
  - Public tests now cover zero-size Vulkan spectral/nuclear matrix norms for
    empty rows and empty columns with unchanged `dgesvd` counter checks.
  - Direct and lazy Vulkan fail-closed paths for `matrix/eigenpairs` have
    unchanged LAPACK counter guards plus diagnostic-order tests for
    invalid-but-Vulkan operands; direct and lazy Vulkan `matrix/svd`,
    `matrix/eigenvalues`, and `matrix/eigenvectors` now route through their
    Vulkan factor/eigen paths.
  - Internal lazy-contract helper tests now cover zero-axis and multi-axis
    Vulkan contractions in addition to the existing rank-1 dot and mixed-device
    rejection coverage.
  - Validation: `c3c build --obj-out obj`, focused
    `advanced-collections-module` passed `935/0`, targeted `git diff --check`,
    primitive docs parity, and Stage 3 source parity passed.
- 2026-04-17 planning checkpoint:
  - Added dedicated planning notes for direct Vulkan `matrix/svd`,
    `matrix/eigenvalues` / `matrix/eigenvectors` / `matrix/eigenpairs`, and
    future `Float32` Tensor/Vulkan execution:
    `docs/plans/vulkan-svd-factor-output-plan-2026-04-17.md`,
    `docs/plans/vulkan-eigensolver-plan-2026-04-17.md`, and
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`.
  - Implementation state has since changed for direct `matrix/svd` and direct
    symmetric eigen: `matrix/svd` supports `k > 64` through storage-backed Gram
    scratch, and `matrix/eigenvalues` / `matrix/eigenvectors` support `n > 64`
    through storage-buffer scratch within helper resource limits, 32-bit shader
    index guards, and the Jacobi iteration guard. Vulkan `Float32`
    placement/copyback, destination `realize`, dense row-major `map`, unary
    helpers, direct `min`/`max`, rank-N `contract`, structural matrix kernels
    (`transpose`, `diagonal`, `diagonal-matrix`, `trace`), direct
    `matrix/rank`, and direct `matrix/norm` selectors default/`'frobenius`,
    `'one`, `'infinity`, `'max`, SVD-backed outputs, serial factor/solve
    surfaces, staged parallel solve, large-dense SVD robustness, and CPU
    factor/SVD contracts have since landed. CUDA `Float32`
    placement/copyback, destination `realize`, and eligible cuBLAS contract
    routing have also landed; scalar runtime values are covered by the later
    scalar `Float32` checkpoint, supported CUDA zero-size contract
    identity/fill is covered by the later CUDA zero-size checkpoint, and CUDA
    rank-1/rank-1 dot is covered by the later dot checkpoint. Remaining
    adjacent work is fixed-width complex, stride/view-backed Vulkan layouts,
    and CUDA `map`.
  - Stale planning text that still described spectral/nuclear Vulkan norms or
    zero-axis Vulkan contract as unsupported was normalized.
- 2026-04-17 audit continuation:
  - Lazy `TENSOR_PAYLOAD_CONTRACT` realization now resolves operands on their
    existing device before CPU fallback. Contracts with lazy Vulkan operands
    can route through Vulkan/CUDA backend contract helpers, while unresolved
    non-CPU operands fail closed with `tensor/backend-unsupported` instead of
    reaching CPU-only expression evaluation.
  - Focused regressions cover the public eager Vulkan map/contract route plus
    internal harness construction of lazy map/contract payloads that exercise
    `tensor_contract_try_device_value`, including the mixed CPU/Vulkan
    rejection path.
  - `omni_tensor_backend_vulkan_singular_norm_f64` now checks Vulkan
    availability, Float64 support, and the shared singular-values shape
    validator before the zero-size/zero-output success return.
  - Backlog state now has one live TODO: `TENSOR-100F`. `TENSOR-100E` is
    closed as the correctness-first Vulkan baseline, and `TENSOR-100G` is
    closed as the measured parallel-solve baseline.
  - Validation: `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
    host focused `advanced-collections-module` passed `913/0`, primitive docs
    parity, Stage 3 source parity, and targeted `git diff --check` passed.
- `TENSOR-100F1` destination-realize contract is now split by destination
  placement. CPU destinations still reject device sources unless the caller
  explicitly uses `to-device 'cpu`; existing dense row-major CUDA and Vulkan
  `Float64` destinations accept matching CPU sources, same-backend sources,
  supported lazy same-backend results, and scalar fills through the same
  backend-neutral `(realize source destination)` API. Regressions cover
  CPU/CUDA/Vulkan destination copies, supported lazy CUDA/Vulkan sources,
  unsupported CUDA `map` fail-closed behavior, cross-backend rejection, and the
  preserved device-result-into-CPU fail-closed behavior. Validation passed:
  helper rebuild, `c3c build --obj-out obj`, direct smokes, host focused
  `advanced-collections-module` `959/0`, bounded-container focused
  `advanced-collections-module` `946/0`, primitive docs parity, Stage 3 source
  parity, and targeted `git diff --check`.
- `TENSOR-100E/F` now includes a separate dense row-major Vulkan `Float64`
  unary helper for absolute value, negation, square root, and real-valued
  component operations. Public
  `(map abs <vulkan-tensor>)`, direct Tensor `(abs <vulkan-tensor>)`,
  public `(map - <vulkan-tensor>)`, and direct Tensor
  `(- <vulkan-tensor>)`, public `(map sqrt <vulkan-tensor>)`, and direct
  Tensor `(sqrt <vulkan-tensor>)`, plus public `(map real-part ...)`,
  `(map imag-part ...)`, `(map conjugate ...)`, and direct Tensor
  `real-part` / `imag-part` / `conjugate` route through the two-buffer SPIR-V
  helper for real-valued Vulkan `Float64` tensors and return Vulkan-placed
  Tensor results. Scalar unary `-` is now reachable through the
  documented public surface by registering `-` as one-or-two argument
  primitive behavior, with `prim_sub` still rejecting extra arguments. CPU
  direct Tensor unary `-` preserves native Tensor dtype for `Float64`,
  `BigInteger`, `BigFloat`, and `BigComplex`. Direct Tensor unary math on
  Vulkan now fails closed for unsupported operations instead of materializing
  hidden CPU results. The old generic Vulkan unary `map` mode-3 branch remains
  invalidated and unsupported; unsupported unary callables such as `sin` still fail closed with
  `tensor/backend-unsupported`. Validation: shader compile/validate passed,
  helper rebuild passed, `c3c build --obj-out obj` passed, direct smokes
  returned `-1`, `2.5`, `2.0`, `vulkan`, `-1.5`, `3.0`, `vulkan`, `2.0`,
  `vulkan`, `-2.5`, `0.0`, `vulkan`, and `tensor/backend-unsupported`;
  host focused `advanced-collections-module` passed `836/0`,
  bounded-container focused `advanced-collections-module` passed `823/0`, and
  static parity/diff checks passed.
- `TENSOR-100E/F` now also routes public Tensor `min` / `max` and
  `map min` / `map max` for dense row-major Vulkan `Float64` tensors through
  the existing binary map helper. Tensor/scalar, scalar/Tensor, and
  Tensor/Tensor broadcast forms remain Vulkan-placed; mixed CPU/Vulkan
  operands and non-`Float64` Vulkan dtypes fail closed. Validation:
  `csrc/tensor_vulkan_map_f64.comp` compiled and validated as Vulkan 1.0
  SPIR-V, helper rebuild passed, `c3c build --obj-out obj` passed, direct
  smokes returned `-2.0`, `0.0`, `vulkan`, and
  `tensor/backend-unsupported`, host focused `advanced-collections-module`
  passed `846/0`, and bounded-container focused `advanced-collections-module`
  passed `833/0`.
- `TENSOR-100F1` now includes dense row-major Vulkan `Float64`
  `matrix/cholesky` through the public backend-neutral `matrix/cholesky`
  surface. The shipped boundary is: Vulkan-placed square `Float64` input,
  exact symmetry/SPD failure mapped to `tensor/not-positive-definite`,
  lower-triangular result remains Vulkan-placed, lazy Vulkan input is realized
  without CPU fallback, empty square tensors preserve shape, and the Vulkan
  path does not call LAPACK. The slice also factored shared trailing-status
  copyback for serial Vulkan matrix helpers and fixed the CPU pure Cholesky
  fallback for empty square tensors. Validation: SPIR-V compile/validate
  passed, helper rebuild passed, `c3c build --obj-out obj` passed, direct
  Vulkan smokes returned `1.0`, `2.0`, `0.0`, `vulkan`, `vulkan`, `0`,
  `tensor/not-positive-definite`, `tensor/not-positive-definite`, and
  `tensor/shape-mismatch`, direct CPU empty Cholesky shape returned `0`, host
  focused `advanced-collections-module` passed `775/0`, and bounded-container
  focused `advanced-collections-module` passed `762/0`.
- `TENSOR-100F1` now also includes dense row-major Vulkan `Float64`
  `matrix/qr` through the public backend-neutral `matrix/qr` surface. The
  shipped boundary is: Vulkan-placed rank-2 `Float64` input with rows greater
  than or equal to columns, reduced `q` and `r` factor Tensors remain
  Vulkan-placed, rank-deficient input maps to `tensor/singular-matrix`, lazy
  Vulkan input is realized without CPU fallback, empty-column tensors preserve
  shape, and the Vulkan path does not call LAPACK. Validation:
  SPIR-V compile/validate passed, helper rebuild passed, `c3c build --obj-out
  obj` passed, direct Vulkan smokes returned `1.0`, `1.0`, `vulkan`,
  `vulkan`, `0`, and `tensor/singular-matrix`, and host focused
  `advanced-collections-module` passed `787/0`, bounded-container focused
  `advanced-collections-module` passed `774/0`, and static parity/diff checks
  passed.
- First `TENSOR-100G` checkpoint now routes Vulkan dense row-major `Float64`
  `matrix/solve` systems with `n >= 3` through a separate
  `omni_tensor_backend_vulkan_solve_parallel_f64` helper. The helper uses a
  64-invocation single-workgroup partial-pivot Gaussian elimination shader,
  parallelizes pivot reduction, row swaps, elimination, and RHS-column
  back-substitution, keeps `n < 3` on the existing serial Vulkan helper, uses
  a private typed `uint` status buffer, and preserves the public
  `matrix/solve` result contract. Validation so far: SPIR-V compile/validate
  passed, helper rebuild passed, `c3c build --obj-out obj` passed, direct
  Vulkan smokes returned `0.2`, `1.0`, and `tensor/singular-matrix`, and host
  focused `advanced-collections-module` passed `793/0`.
- Staged `TENSOR-100G` checkpoint now records two compute dispatches in one
  Vulkan command buffer for parallel `matrix/solve`: init/copy first, then
  factor/back-solve after an explicit buffer-memory barrier on output and
  private status buffers. The init stage dispatches over
  `max(n*n, n*rhs_cols)` work items, so coefficient/RHS copies are not capped
  at one 64-lane workgroup. Validation passed: both staged shaders compiled and
  validated as Vulkan 1.0 SPIR-V, helper rebuild passed, `c3c build --obj-out
  obj` passed, direct smokes returned `0.2`, `1.0`, `18.0`, and
  `tensor/singular-matrix`, host focused `advanced-collections-module` passed
  `798/0`, bounded-container focused `advanced-collections-module` passed
  `785/0`, and static parity/diff checks passed.
- Advanced `TENSOR-100G` checkpoint now records a multi-dispatch Vulkan
  `matrix/solve` command buffer: init/copy, per-pivot pivot/swap,
  per-pivot elimination, and descending per-row backsolve. Elimination and RHS
  backsolve can span multiple workgroups, while pivot selection/swap remains
  one workgroup until the cross-workgroup pivot-reduction scratch-buffer design
  lands. The public `matrix/solve` surface is unchanged, the serial `n < 3`
  Vulkan helper still owns its trailing `double` status sentinel, and the
  parallel helper still uses private typed `uint` status storage. Validation
  passed: pivot/eliminate/backsolve shaders compiled and validated as Vulkan
  1.0 SPIR-V, helper rebuild passed, `c3c build --obj-out obj` passed, direct
  smokes returned `0.2`, `3.0`, `18.0`, dense 9x9 value `1.0`, and
  `tensor/singular-matrix`, host focused `advanced-collections-module` passed
  `799/0`, bounded-container focused `advanced-collections-module` passed
  `786/0`, and static parity/diff checks passed.
- Advanced `TENSOR-100G` checkpoint now includes cross-workgroup pivot
  reduction. Pivot selection is split into scan, ping-pong reduce, commit, and
  row-swap shader stages. The helper binds a private pivot-magnitude scratch
  buffer plus expanded typed `uint` status scratch, recursively reduces pivot
  candidates with barriers, then swaps rows before elimination. Validation
  passed: all affected solve-multi shaders compiled and validated as Vulkan
  1.0 SPIR-V, helper rebuild passed, `c3c build --obj-out obj` passed, direct
  smokes returned `0.2`, `3.0`, `1.0` for the 65x65 cross-workgroup case, and
  `tensor/singular-matrix`, host focused `advanced-collections-module` passed
  `800/0`, bounded-container focused `advanced-collections-module` passed
  `787/0`, and static parity/diff checks passed.
- Advanced `TENSOR-100G` checkpoint now includes tiled LU staging as a
  per-pivot panel-factor stage. The helper binds a private factor scratch
  buffer, dispatches a factor kernel after row swap and before elimination,
  stores `L[row,pivot]` in scratch and the lower-triangular workspace, and has
  elimination consume staged factors. Validation passed: all affected
  solve-multi shaders compiled and validated as Vulkan 1.0 SPIR-V, helper
  rebuild passed, `c3c build --obj-out obj` passed, direct smokes returned
  dense 9x9 value `1.0`, 65x65 identity value `1.0`, dense 65x65 `I + J`
  value `1.0`, and `tensor/singular-matrix`, host focused
  `advanced-collections-module` passed `801/0`, bounded-container focused
  `advanced-collections-module` passed `788/0`, and static parity/diff checks
  passed.
- Advanced `TENSOR-100G` checkpoint now includes measured threshold routing.
  Local in-process measurements in `build/vulkan_solve_threshold_20260417_*`
  found `3`, `9`, `16`, and `32` tied or serial-favorable while `65` was the
  first tested size where the staged parallel path won across identity and
  dense fixtures. Public `matrix/solve` now keeps Vulkan dense row-major
  `Float64` systems with `n < 65` on the serial helper and routes `n >= 65`
  through the staged parallel helper. A private serial solve counter was added
  so tests prove direct serial execution below the threshold. Validation
  passed: helper rebuild, `c3c build --obj-out obj`, direct smokes returned
  `9.0`, `1.0`, `1.0`, and `tensor/singular-matrix`, host focused
  `advanced-collections-module` passed `806/0`, bounded-container focused
  `advanced-collections-module` passed `793/0`, and static parity/diff checks
  passed.
- `TENSOR-100E` now includes Vulkan dense row-major rank-N `Float64`
  zero-axis and multi-axis `contract` through the public backend-neutral
  `contract` surface. The shipped Vulkan contract boundary is: zero or more
  contracted axis pairs, output axes ordered as free left axes followed by free
  right axes, results remain Vulkan-placed, and callers copy back explicitly
  with `to-device 'cpu`. Stride-aware/non-contiguous layouts remain fail-closed
  until layout/aliasing metadata is explicit.
  Follow-up zero-axis validation passed with direct smokes `60.0`, `12.0`,
  `vulkan`, and `0`, host focused `advanced-collections-module` `851/0`, and
  bounded-container focused `advanced-collections-module` `838/0`.
  Validation:
  SPIR-V compile/validate passed, helper rebuild passed, `c3c build --obj-out
  obj` passed, direct Vulkan smokes returned `70.0`, `210.0`, `460.0`, `8.0`,
  and `tensor/backend-unsupported`, host focused `advanced-collections-module`
  passed `689/0`, and bounded-container focused `advanced-collections-module`
  passed `676/0`.
- `ADV-STACK-001` calibrated the macro-hygiene non-tail recursion headroom
  probe to depth `512`.
- Treat the old `896`/`1200` depths as invalidated portability assumptions on
  this ARM64 workspace: host validation crashed at `640`/`896`, while the
  bounded validation container crashed at `1200`.
- The fixed checkpoint is validated by host and bounded-container
  `advanced-macro-hygiene-string-number` pass=9 fail=0, host and
  bounded-container `advanced-macro-hygiene` pass=83 fail=0, and bounded
  container full `advanced` pass=1928 fail=0.
