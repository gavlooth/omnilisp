# Vulkan Math Library Roadmap

Date: 2026-04-17
Status: Closed for `TENSOR-100F`; `TENSOR-100G` is a closed measured solver baseline, and `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL` is closed.

## Decision

Omni should keep building a portable Vulkan math backend as an owned runtime
library behind the existing backend-neutral `Tensor` surface. The public
language should not grow backend-flavored operation names. Vulkan remains an
explicit placement target selected with `(to-device tensor 'vulkan)`, inspected
with `device` / `tensor-backends`, and copied back explicitly with
`(to-device tensor 'cpu)`.

The current Vulkan matrix solver has both a serial GPU execution path and a
measured staged parallel route for dense row-major `Float64` systems. It also
has native `Float32` staged parallel solve parity at the same `65` threshold,
with local timing tied against forced serial on this stack. Further solver
work should reopen as a separate performance item only if measurements justify
broader algorithmic scope.

`TENSOR-100F` closed on 2026-04-22 after bounded-container global validation.
The broad gate passed normal build/tests and FTXUI smoke; ASAN was explicitly
skipped because the current C3 toolchain reports address sanitizer unsupported
for this target. During that closure pass, `ast_arena_alloc` was hardened to
fail closed on corrupt current chunks and the JIT tail `List` / `Array`
constructor shortcut was narrowed so one-argument conversion calls preserve
primitive malformed-list and malformed-iterator validation.

## Scope

This plan covers the work needed to support the broader Vulkan math direction:

- dense `Float64` Vulkan matrix and Tensor kernels as the first library tier,
- fixed-width real dtype expansion only after language/runtime storage exists,
- fixed-width complex tensors only after an explicit native complex dtype and
  component layout are chosen,
- stride-aware layouts only after shape/stride/offset/alias metadata is
  explicit and a separate view construction/execution contract is landed,
- parallel solver and factorization kernels that are real Vulkan algorithms,
  not hidden CPU/LAPACK fallbacks.

Out of scope for this roadmap:

- lowering `BigInteger`, `BigFloat`, or `BigComplex` to approximate Vulkan
  storage,
- claiming GPU Tensor rounding support unless the backend returns the public
  dtype-changing `Tensor BigInteger` contract and passes backend-capable
  validation,
- silent `Float64` to `Float32` downcasts,
- backend-specific public names such as `vulkan/solve`,
- implicit CPU/GPU transfers inside `map`, `contract`, `realize`, or
  `matrix/*`.

## Tracks

### 1. Kernel Library Baseline (`TENSOR-100F1`)

Keep the first Vulkan library tier focused on dense row-major `Float64`.

Required implementation shape:

- factor shared descriptor/pipeline dispatch helpers so new kernels do not
  duplicate allocation, descriptor, command-buffer, and status-copy plumbing;
- keep checked-in GLSL plus generated SPIR-V C artifacts as the source of
  truth;
- expose only backend-neutral operations through existing public primitives;
- retain CPU pure/LAPACK paths as semantic oracles, not fallback paths for
  Vulkan operands.

Landed kernel slice:

- `matrix/cholesky`: Tensor-returning lower factor, Vulkan result, status-only
  copyback for `tensor/not-positive-definite`;
- `matrix/qr`: reduced factors, Vulkan `q` and `r` tensors, singular/rank
  status copyback;
- `map +`, direct Tensor unary `+`, `map abs`, direct Tensor `abs`, `map -`,
  direct Tensor unary `-`, `map sqrt`, and direct Tensor `sqrt`: separate
  two-buffer unary helper for mapped identity/abs/negation/sqrt operations,
  Vulkan result, no reuse of the invalidated generic unary map mode.
- `map real-part`, `map imag-part`, `map conjugate`, and direct Tensor
  `real-part` / `imag-part` / `conjugate`: real-valued `Float64` component
  operations on the same separate two-buffer unary helper, with Vulkan-placed
  results and no hidden CPU fallback.
- `map min`, `map max`, and direct Tensor `min` / `max`: dense row-major
  `Float64` tensor/scalar, scalar/Tensor, and Tensor/Tensor broadcast
  compare/select operations on the existing binary map helper, with
  Vulkan-placed results and mixed-device fail-closed behavior.
- destination-form `realize`: existing dense row-major Vulkan `Float64`
  destinations accept matching CPU sources, Vulkan sources, lazy Vulkan
  results, and scalar fills through backend-neutral `(realize source
  destination)`. CPU destinations still reject device sources unless the caller
  explicitly copies with `to-device 'cpu`.
- `contract` zero-axis outer products and rank-0 scalar products: dense
  row-major Vulkan `Float64` tensors through the existing generic contract
  shader, with free-left-then-free-right output order, Vulkan-placed results,
  and no hidden CPU fallback.
- helper factoring proof: the mature serial dispatch helpers now share
  compute-pipeline creation through
  `omni_tensor_vulkan_create_compute_pipeline_for_shader`, sequential
  storage-buffer descriptor layout/allocation/update helpers, and standard
  one-time command recording/submission. Compatible `Float64` status-readback
  boilerplate is also shared through mapper-specific decoding for generic
  trailing status, SVD/singular-values status, and symmetric-eigen status.
  Parallel solve remains on its separate typed `uint32_t` status-buffer ABI by
  design.
- `matrix/singular-values` and `matrix/norm` `'spectral` / `'nuclear`:
  dense row-major Vulkan `Float64` rank-2 tensors through a checked-in
  Gram/Jacobi singular-value shader, with Vulkan-placed singular-value results
  for `matrix/singular-values` and scalar readback only for norm selectors.
  The shader now uses storage-buffer Gram scratch behind the public output
  payload, so `k = min(rows, columns) > 64` is supported without hidden
  CPU/LAPACK fallback; shader non-convergence raises `tensor/no-convergence`.
- `matrix/svd`: dense row-major Vulkan `Float64` rank-2 tensors through a
  checked-in factor-output Gram/Jacobi shader, with Vulkan-placed reduced
  `u`, `s`, and `v` factors. The shader now stores Gram scratch behind the
  public `u` payload, so `k = min(rows, columns) > 64` is supported without
  hidden CPU/LAPACK fallback; shader non-convergence raises
  `tensor/no-convergence`.
- `Float32` SVD-backed math: dense row-major Vulkan `Float32` tensors use
  dedicated singular-value and SVD shaders for `matrix/norm` `'spectral` and
  `'nuclear`, `matrix/singular-values`, and direct `matrix/svd`, preserving
  Vulkan placement and `Float32` Tensor output dtype where Tensor results are
  returned.
- Distribution unary math: dense row-major Vulkan `Float32` tensors use
  dedicated helpers for `stats/normal-cdf` fixed op id `19` and
  `stats/normal-quantile` fixed op id `20`, preserving Vulkan placement and
  `Float32` dtype for public `map` and direct Tensor unary math. Dense
  row-major Vulkan `Float64` tensors support `stats/normal-cdf` through fixed
  op id `19` in the existing Float64 unary helper, preserving Vulkan placement
  and `Float64` dtype. CDF uses same-dtype shader approximations; Float32
  quantile uses a separate status-bearing inverse-CDF shader so invalid
  probabilities fail before output exposure. Vulkan `Float64`
  `stats/normal-quantile` remains deferred pending explicit double
  inverse-CDF/status policy.
- `Float32` large-dense SVD robustness: the singular-values/SVD shaders use
  scale-aware eigenvalue tolerance plus orthonormal completion to handle the
  previously failing dense all-ones / rank-deficient `65x65` cases without
  hidden CPU/LAPACK fallback or hidden `Float64` widening.

Eligible next kernels:

- direct symmetric `matrix/eigenvalues` and `matrix/eigenvectors` have landed
  for dense row-major Vulkan `Float64` square inputs, including `n > 64`
  through storage-buffer scratch within helper resource limits, 32-bit shader
  index guards, and the Jacobi iteration guard; remaining eigen boundaries
  follow `docs/plans/vulkan-eigensolver-plan-2026-04-17.md`.
- direct general `matrix/eigenpairs` also follows the eigensolver plan, but
  Vulkan execution remains blocked while its public output contract is
  pointer-backed `BigComplex`.
- `Float32` Vulkan execution follows
  `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`; CPU
  `Tensor Float32` storage and oracles have landed, and Vulkan `Float32`
  placement/copyback, destination `realize`, elementwise `map`, unary helpers
  including the `Float32` scientific unary family,
  direct `min`/`max`, rank-N `contract`, structural matrix kernels
  (`transpose`, `diagonal`, `diagonal-matrix`, `trace`), direct `matrix/rank`,
  direct `matrix/norm` selectors, direct `matrix/singular-values`, direct
  `matrix/svd`, serial factor/solve surfaces (`matrix/determinant`,
  `matrix/lu`, `matrix/solve`, `matrix/inverse`, `matrix/cholesky`, and
  `matrix/qr`), staged parallel `matrix/solve`, and large-dense SVD
  robustness, CPU factor/SVD contracts, scalar runtime values, and CUDA
  placement/copyback/eligible cuBLAS contract routing have real helper/shader
  paths. CUDA zero-size contract identity/fill and rank-1/rank-1 dot also
  preserve CUDA placement for supported layouts. CUDA elementwise `map` has
  landed for dense row-major `Float64`/`Float32` binary scalar/exact-shape/
  right-aligned broadcast operands, zero-offset CUDA transpose-view binary
  operands, arithmetic/component unary ops, and generated CUDA C/libdevice PTX
  scientific unary ops; valid foreign CUDA concrete payload clone now
  deep-copies into Omni-owned CUDA storage.
  `matrix/transpose-view` has landed as the first read-only Tensor view
  constructor, and direct rank-2 Vulkan transpose views can materialize into
  dense Vulkan storage at explicit placement/realization/copyback boundaries;
  CUDA transpose views can feed supported binary map kernels through explicit
  stride-offset metadata. Raw CUDA view materialization, nonzero-offset views,
  and CUDA unary/scientific view-backed kernels remain fail-closed. Fixed-width complex raw
  storage, elementwise map, contract, CUDA/Vulkan structural matrix kernels,
  and Vulkan fixed-width complex `matrix/lu`, `matrix/determinant`,
  `matrix/solve`, `matrix/inverse`, `matrix/rank`, `matrix/norm`,
  `matrix/singular-values`, `matrix/qr`, and `matrix/cholesky` are landed
  behind explicit operation capability bits. Remaining adjacent work is full
  complex SVD factor output, complex eigen result contracts, CUDA complex
  singular-value/norm support, and explicit view-aware Vulkan layout ABIs.
- additional unary arithmetic-only `map` operations only through separate
  explicitly debugged unary helpers, with CPU surface semantics decided first.
- dtype-changing Tensor rounding for Vulkan is not part of same-dtype unary
  map. When the Vulkan backend reports `rounding-big-integer`, direct Vulkan
  `floor`, `ceiling`, `round`, and `truncate` compute through the dedicated
  integer/status/copyback helper and return CPU `Tensor BigInteger` results;
  devices without that capability remain fail-closed even when generic
  `float64` or `float32` Vulkan support is available.
- CUDA destination-form `realize` has landed under the same backend-neutral
  public surface for existing dense row-major `Float64` and `Float32` CUDA
  destinations;
  remaining CUDA work should stay in the CUDA/cuBLAS plan rather than this
  Vulkan kernel roadmap.
- Tensor layout metadata has landed as `tensor-layout`. The primitive returns a
  dictionary describing dtype, device, payload kind, dense/strided layout,
  shape, strides, rank, element count, logical byte length, storage offset,
  concrete backing storage extent, ownership/view status, owner, and write
  policy. Lazy expression payloads report `0` storage extent until realization.
- Read-only transpose views have landed through `matrix/transpose-view`.
  `tensor-layout` reports these as payload `view`, layout `strided`, owner
  `view-source`, `owns-storage` false, and write-policy `read-only-view`.
  CPU `ref`, `Array`, `List`, and `realize` materialization observe the view.
  Vulkan/CUDA kernels and raw backend helpers still fail closed for view
  payloads instead of consuming offset/stride metadata or hiding
  materialization. Follow-on explicit materialization lanes have landed for
  producing dense Vulkan storage from direct rank-2 Vulkan transpose views and
  dense CUDA storage from CPU-backed transpose views at `to-device`/destination
  copy boundaries; those are narrower than arbitrary stride-aware GPU kernel
  execution.
- Vulkan fixed-width complex LU/determinant/solve/inverse/rank/norm and
  singular-values are landed for dense row-major zero-offset
  `Complex128`/`Complex64` tensors. The
  public boundary is backend-neutral `matrix/lu`, `matrix/determinant`,
  `matrix/solve`, `matrix/inverse`, `matrix/rank`, and direct `matrix/norm`
  reducers, plus `matrix/singular-values` and spectral/nuclear `matrix/norm`,
  with Tensor outputs preserving fixed-width complex dtype where applicable,
  singular-value tensors returning component-width real dtype on Vulkan,
  scalar/metadata readback limited to the public return contract, singular
  systems/matrices and tolerance behavior mapped through Tensor diagnostics,
  and no hidden CPU fallback. Full complex `matrix/svd` factor output remains
  deferred because it needs complex `u`/`v` with component-width real `s`.

Validation gate:

- shader compile and `spirv-val`;
- helper rebuild;
- `c3c build --obj-out obj`;
- direct Vulkan smokes proving result device, representative values, error
  mapping, lazy input realization, and no LAPACK counter movement where
  applicable;
- focused advanced collections/module suite on host and bounded container;
- docs parity, Stage 3 source parity, and `git diff --check`.

Completed checkpoint:

- 2026-04-17: `matrix/cholesky` landed for dense row-major Vulkan `Float64`
  inputs. It returns a Vulkan-placed lower factor Tensor, maps nonsymmetric and
  non-SPD inputs to `tensor/not-positive-definite`, supports lazy Vulkan input
  realization, preserves empty square shape, and does not call LAPACK on the
  Vulkan path. The slice also factored shared trailing-status copyback for
  serial Vulkan matrix helpers and fixed the CPU pure Cholesky fallback for
  empty square tensors.
- 2026-04-17: `matrix/qr` landed for dense row-major Vulkan `Float64` rank-2
  inputs with rows greater than or equal to columns. It returns Vulkan-placed
  reduced `q` and `r` factor Tensors, maps rank-deficient inputs to
  `tensor/singular-matrix`, supports lazy Vulkan input realization, preserves
  empty-column shapes, and does not call LAPACK on the Vulkan path.
- 2026-04-17: unary absolute value landed for dense row-major Vulkan
  `Float64` tensors. Public `(map abs <vulkan-tensor>)` and direct Tensor
  `(abs <vulkan-tensor>)` route through a new two-buffer SPIR-V helper and
  return Vulkan-placed Tensor results. Unsupported unary callables such as
  `sin` still fail closed with `tensor/backend-unsupported`; the invalidated
  generic unary map mode remains out of use.
- 2026-04-17: unary identity landed on the same separate dense row-major
  Vulkan `Float64` helper for public `(map + <vulkan-tensor>)`. Direct Tensor
  `(+ <tensor>)` is now the Tensor identity surface and preserves the Tensor's
  current placement.
- 2026-04-17: compute-pipeline creation factoring landed for the serial
  Vulkan dispatch helper family. The two-buffer, three-buffer,
  one-input/two-output, and one-input/three-output helpers now reuse
  `omni_tensor_vulkan_create_compute_pipeline_for_shader` without changing
  descriptor or command-buffer behavior.
- 2026-04-17: unary negation landed on the same separate dense row-major
  Vulkan `Float64` helper. Public `(map - <vulkan-tensor>)` and direct Tensor
  `(- <vulkan-tensor>)` return Vulkan-placed Tensor results; CPU direct Tensor
  unary `-` preserves `Float64`, `BigInteger`, `BigFloat`, and `BigComplex`
  dtypes. The scalar `(- x)` public surface now reaches the existing unary
  arithmetic implementation, while `(- a b c)` remains rejected by `prim_sub`.
- 2026-04-17: unary square root landed on the same separate dense row-major
  Vulkan `Float64` helper. Public `(map sqrt <vulkan-tensor>)` and direct
  Tensor `(sqrt <vulkan-tensor>)` return Vulkan-placed Tensor results for
  positive finite inputs. Direct Tensor unary math on Vulkan now fails closed
  for unsupported operations instead of materializing hidden CPU results; `sin`
  remains unsupported on Vulkan after the earlier GLSL double-transcendental
  compile failure.
- 2026-04-17: real-valued component operations landed on the same separate
  dense row-major Vulkan `Float64` helper. Public
  `(map real-part <vulkan-tensor>)`, `(map imag-part <vulkan-tensor>)`,
  `(map conjugate <vulkan-tensor>)`, and direct Tensor `real-part`,
  `imag-part`, and `conjugate` return Vulkan-placed `Float64` Tensor results;
  `real-part` and `conjugate` are identity over real tensors, while
  `imag-part` produces zeros. The path resolves lazy Vulkan inputs without
  materializing hidden CPU storage.
- 2026-04-17: destination-form `realize` now supports existing Vulkan
  `Float64` destinations for matching CPU sources, Vulkan sources, lazy Vulkan
  results, and scalar fills. The public surface remains `(realize source
  destination)`, CPU destinations still reject device sources without explicit
  `to-device 'cpu`, and unsupported cross-backend sources fail closed.
- 2026-04-17: `min` / `max` landed on the existing dense row-major Vulkan
  `Float64` binary map helper. Public direct Tensor `min` / `max` and
  `map min` / `map max` support tensor/scalar, scalar/Tensor, and
  Tensor/Tensor broadcast cases for eligible Vulkan operands, return
  Vulkan-placed results, and reject mixed CPU/Vulkan operands with
  `tensor/backend-unsupported`.
- 2026-04-17: zero-axis `contract` landed on the existing dense row-major
  Vulkan `Float64` generic contract shader. Public `(contract a b [] [])`
  now supports rank-0 scalar products, rank-1/rank-1 outer products,
  higher-rank outer products, and zero-free-dimension zero-length outputs for
  eligible Vulkan operands; results remain Vulkan-placed and require explicit
  `to-device 'cpu` for CPU inspection.
- 2026-04-17: `Float32` rank-N `contract` landed on a distinct dense row-major
  Vulkan generic contract shader/helper. Public Vulkan `contract` now preserves
  matching `Float32` dtype for rank-N, zero-axis, zero-size, and lazy mapped
  operands without using `Float32` as a `Float64` fallback.
- 2026-04-17: `matrix/singular-values` landed for dense row-major Vulkan
  `Float64` rank-2 inputs. It computes singular values on Vulkan with a
  storage-backed Gram/Jacobi shader, returns a Vulkan-placed rank-1 Tensor,
  preserves empty result shapes, supports `k = min(rows, columns) > 64`, and
  does not call LAPACK. `matrix/norm` `'spectral` and `'nuclear` reuse that
  Vulkan singular-value helper and read back only the scalar norm result.
- 2026-04-17: direct `matrix/svd` landed for dense row-major Vulkan `Float64`
  rank-2 inputs. It computes reduced factor outputs on Vulkan with a
  storage-backed Gram/Jacobi shader, returns Vulkan-placed `u`, `s`, and `v`
  tensors, supports `k = min(rows, columns) > 64`, preserves zero-`k`
  empty-axis shapes, and does not call LAPACK.
- 2026-04-17: direct symmetric `matrix/eigenvalues` and
  `matrix/eigenvectors` landed for dense row-major Vulkan `Float64` square
  inputs. They compute Jacobi eigenvalues/eigenvectors on Vulkan, return
  Vulkan-placed values and aligned vector columns, preserve empty square
  shapes, support `n > 64` through storage-buffer scratch within helper
  resource limits, 32-bit shader index guards, and the Jacobi iteration guard,
  raise `tensor/not-symmetric` for nonsymmetric Vulkan inputs, map
  non-convergence to `tensor/no-convergence`, and do not call LAPACK.

### 2. Dtype Roadmap (`TENSOR-100F2`)

The language-level dtype contract comes before backend storage.

Order:

1. Continue `Float64` because it is already a native fixed-width Tensor dtype
   and can be guarded by Vulkan `shaderFloat64`.
2. Continue Vulkan `Float32` after the landed placement/map/unary/minmax,
   rank-N contract, structural matrix, reducer, singular-value, SVD, serial
   factor/solve, staged parallel solve, and large-dense SVD robustness slices.
   Future SVD work is performance-oriented tiled/multi-dispatch execution, not
   a correctness blocker for the validated dense row-major `65x65` cases.
   `Float32` must not become a backend fallback for `Float64`.
3. Continue fixed-width complex Vulkan execution from the shipped raw storage,
   elementwise map, contract, and structural matrix families. Add numerical
   complex matrix operations only with operation-specific capability bits,
   status/error contracts, and CPU oracle coverage. Real `float64`/`float32`
   capability bits do not imply additional complex support.
4. Keep `BigInteger`, `BigFloat`, and `BigComplex` CPU-only unless a future
   exact arbitrary-precision device representation is designed explicitly.

Acceptance criteria:

- `tensor-backends` reports each dtype capability separately;
- unsupported dtype/device combinations fail with `tensor/backend-unsupported`;
- mixed real/complex rules match the CPU Tensor oracle;
- no kernel accepts a dtype by approximating it silently.

### 3. Layout Roadmap (`TENSOR-100F3`)

Dense row-major contiguous storage remains the active Vulkan layout for kernel
dispatch. The first public metadata slice has landed as `tensor-layout`, which
reports:

- logical shape and physical strides,
- storage offset,
- dtype, rank, element count, and logical byte length,
- concrete backing storage element and byte extent (`0` for lazy expression
  payloads until realization),
- payload kind (`concrete`, `map`, `contract`, or `view`),
- layout kind (`dense-row-major` or `strided`),
- owner (`self`, `view-source`, or `expression`),
- view flags and write policy (`mutable`, `immutable`, `mutable-view`, or
  `read-only-view`).

`matrix/transpose-view` is the first public read-only view constructor. It
ships CPU `ref`, `Array`, `List`, and `realize` materialization support, but it
is not GPU view execution support. There is still no recursive/view GPU kernel
support, and dense CUDA/Vulkan kernels still require zero-offset dense
row-major storage unless a later helper explicitly accepts offset/stride
metadata.

First stride-aware targets:

- view-aware Vulkan execution for `matrix/transpose-view` or diagonal access;
- read-only `contract` operands with explicit strides;
- destination writes only when aliasing is proven absent or a temporary commit
  path exists.

Unsupported layouts must continue to fail closed. They must not trigger hidden
contiguous CPU or GPU copies unless the public operation explicitly asks for a
copy or realization.

### 4. Parallel Solver Track (`TENSOR-100G`)

The parallel solver should replace the serial `matrix/solve` Vulkan algorithm
for sufficiently large dense `Float64` systems while preserving the same public
contract.

Current baseline facts:

- the existing Vulkan solve shader is intentionally serial:
  `local_size_x = 1`, one global invocation, in-place copy, pivot search, row
  swaps, forward elimination, and back-substitution;
- the parallel Vulkan solve path now exists as a separate staged helper used
  for `n >= 65`, while the serial helper remains the small-size path;
- increasing dispatch width on that shader would introduce races, not
  parallelism;
- adjacent solver-like Vulkan kernels (`matrix/lu`, `matrix/inverse`,
  `matrix/rank`, and determinant internals) are also correctness-first serial
  GPU execution paths;
- serial solve output storage is larger than the logical Tensor because it
  stores `[solution][coefficient workspace][status]`; future callers must keep
  using logical Tensor byte lengths when operating on the public result;
- parallel solve output storage is also larger than the logical Tensor because
  it stores `[solution][coefficient workspace]`, but singular/success status is
  a separate private `uint` status buffer that is destroyed after host-side
  status readback;
- the parallel solve path now uses solver-specific staged and multi-dispatch
  command-buffer recording. Remaining performance work should focus on object
  reuse opportunities and measured routing rather than treating a staged
  helper as future work.

Public contract:

- same primitive: `matrix/solve`;
- coefficients and RHS must both be Vulkan-placed dense row-major `Float64`;
- RHS rank remains rank-1 or rank-2;
- result remains Vulkan-placed;
- singular systems raise `tensor/singular-matrix`;
- mixed CPU/Vulkan operands fail closed;
- no CPU Tensor copy and no LAPACK call on the Vulkan path.

Float32 solve correctness is covered by the serial Vulkan factor/solve path.
Staged parallel Float32 solve routing is a deferred performance parity item and
must not silently widen through the Float64 staged helper.

Implementation stages:

1. Done for the first slice: add
   `omni_tensor_backend_vulkan_solve_parallel_f64`, keeping the existing serial
   helper as the small-size and bring-up path.
2. Done for the first slice: use a private typed `uint` status buffer for the
   parallel helper. Future staged algorithms still need explicit pivots,
   permutation, scratch tiles, per-stage status, and layout versioning.
3. Done for the first slice: implement parallel pivot search as a workgroup
   reduction over candidate rows with deterministic tie-breaking.
4. Done for the first slice: implement row swaps as parallel row-copy loops
   over coefficient and RHS storage.
5. Done for the first slice: implement elimination with workgroup-parallel
   coefficient and RHS updates after each pivot.
6. Done for the first slice: implement RHS-column-parallel back-substitution.
7. Done for the staged slice: launch init/copy and factor/solve kernels against
   shared coefficient/RHS/output/status buffers in one command buffer with an
   explicit Vulkan buffer-memory barrier between dispatches.
8. Done for the multi-dispatch slice: split pivot/swap, elimination, and
   backsolve into separate dispatch families, with elimination and RHS
   backsolve able to span multiple workgroups.
9. Done for the cross-workgroup pivot slice: add pivot magnitude and candidate
   row scratch buffers, scan pivot candidates across workgroups, recursively
   reduce candidates through ping-pong scratch lanes, commit the selected row,
   then run row swaps as their own dispatch before elimination.
10. Done for the tiled LU staging slice: add per-pivot panel-factor scratch
   staging for larger matrices.
11. Done for the measured-threshold slice: add a size threshold chosen by
   measurement, not guesswork. Below the threshold, dispatch the existing
   serial Vulkan solver to avoid kernel launch overhead.
12. Add result/status layout versioning so serial and parallel solvers can share
   public C3 construction while keeping implementation-specific scratch space
   private.

Preferred algorithm sequence:

- first: parallel partial-pivot Gaussian elimination for correctness parity;
- second: factor the core into a reusable parallel LU workspace so
  `matrix/lu`, `matrix/determinant`, `matrix/inverse`, and `matrix/solve` can
  share the same factorization path;
- third: grow the landed panel-factor staging into blocked/tiled LU for larger
  matrices after threshold measurements identify the useful regime;
- later: reusable factorization adoption by related `matrix/*` operations and
  QR/Cholesky/SVD-specific parallel algorithms, each under its public
  `matrix/*` contract.

Required tests:

- small systems compare exactly with existing serial Vulkan smoke expectations;
- larger deterministic systems compare against CPU pure/LAPACK oracle within
  existing tolerance;
- multiple RHS columns exercise RHS-column parallelism;
- singular and near-singular cases preserve diagnostics;
- threshold tests prove both serial and parallel Vulkan helpers are exercised;
- no-LAPACK counter checks prove Vulkan solver paths do not route through CPU
  solver backends.

Completed checkpoint:

- 2026-04-17: first `TENSOR-100G` Vulkan solve slice landed. Public
  `matrix/solve` routes Vulkan dense row-major `Float64` systems with `n >= 3`
  through a separate 64-invocation workgroup-parallel partial-pivot Gaussian
  elimination shader; `n < 3` keeps using the serial Vulkan helper. The
  parallel helper uses a private typed `uint` status buffer, preserves rank-1
  and rank-2 RHS result shape, keeps results Vulkan-placed, maps singular
  systems to `tensor/singular-matrix`, and is covered by a helper call-count
  test plus unchanged LAPACK `dgesv` counter validation.
- 2026-04-17: staged `TENSOR-100G` Vulkan solve slice landed. The parallel
  helper now records init/copy and factor/backsolve dispatches in one command
  buffer with an explicit Vulkan buffer-memory barrier between them, so
  coefficient/RHS initialization is no longer capped by one 64-lane workgroup.
- 2026-04-17: multi-dispatch `TENSOR-100G` Vulkan solve slice landed. The
  helper now records init/copy, per-pivot pivot/swap, per-pivot elimination,
  and descending per-row backsolve dispatches. Elimination and RHS backsolve
  can span multiple workgroups, the public `matrix/solve` contract is
  unchanged, and tests prove serial threshold routing, no LAPACK fallback,
  multi-dispatch counter movement, dense 9x9 solving, and singular diagnostics.
- 2026-04-17: cross-workgroup pivot `TENSOR-100G` Vulkan solve slice landed.
  The helper now binds a private pivot-magnitude scratch buffer and expanded
  typed `uint` status buffer, scans pivot candidates across workgroups,
  recursively reduces candidates through ping-pong scratch lanes, commits the
  selected pivot row, and runs row swaps as a separate dispatch before
  elimination. Tests prove the 65x65 path executes the pivot-reduction stage
  without LAPACK while preserving dense 9x9 elimination and singular
  diagnostics.
- 2026-04-17: tiled LU staging `TENSOR-100G` Vulkan solve slice landed. The
  helper now binds a private factor scratch buffer and runs a per-pivot
  panel-factor dispatch after row swap and before elimination. The factor stage
  stores `L[row,pivot]` in scratch and in the lower-triangular workspace, then
  elimination consumes staged factors instead of recomputing them in every
  coefficient/RHS update. Tests prove the factor stage executes on a dense
  65x65 `I + J` solve without LAPACK.
- 2026-04-17: measured threshold routing landed. Local in-process measurement
  logs under `build/vulkan_solve_threshold_20260417_*` selected
  `OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_MIN_N = 65`: `3`, `9`, `16`, and `32`
  were tied or serial-favorable, while `65` was the first tested size where
  the staged parallel path won across identity and dense fixtures. Tests prove
  `2x2` and `9x9` stay serial, `65x65` enters the staged parallel helper
  without LAPACK, pivot/factor counters still move, and parallel singular
  status is exercised at 65x65.
- 2026-04-17: Vulkan `Float32` staged parallel solve parity landed. Dedicated
  `_f32` staged shaders plus an explicit dtype-aware shared dispatch route
  public Vulkan `Float32` solve systems with `n >= 65` through the staged
  helper. Local logs under `build/vulkan_solve_f32_threshold_20260417_*`
  showed `65x65` staged and forced-serial `Float32` identity/dense probes tied
  within measurement noise, so the threshold is a parity threshold rather than
  a decisive speedup claim. Tests prove small `Float32` systems stay serial,
  `65x65` systems enter the staged helper, dtype-specific `Float32` counters
  move, `Float64`/LAPACK counters do not, matrix RHS shape is preserved, mixed
  dtype rejects, and staged singular status maps to `tensor/singular-matrix`.

`TENSOR-100G` closure:

- no current correctness-required parallel solve work remains. Full blocked
  trailing-update LU is a future performance lane only if later measurements
  justify it.

### 5. Complex Solver Track (`TENSOR-100H`)

Complex Vulkan solving should build on the landed CPU `Complex128`/`Complex64`
Tensor oracle. Raw storage, elementwise map, contract, and CUDA/Vulkan
structural matrix kernels have landed behind explicit operation capability
bits. Numerical matrix support remains operation-specific. Vulkan `matrix/lu`,
`matrix/determinant`, and `matrix/solve` are now landed as siblings to real
`Float64`/`Float32`, not as `BigComplex` lowering.

Resolved prerequisites:

- component dtype and memory layout;
- copy/placement semantics for `Complex64` and `Complex128`;
- first operation families for raw storage, elementwise map, contract, and
  CUDA/Vulkan structural matrix kernels;
- `tensor-backends` storage and operation capability flags for landed
  fixed-width complex families.

Resolved decisions for the LU/determinant/solve/inverse/rank/direct-norm lane:

- singularity tolerance and pivot magnitude rule;
- operation capability naming/reporting for complex numerical matrix kernels;
- status mapping for singular systems and invalid/nonrepresentable results;
- validation proving no hidden CPU fallback and preserving fixed-width complex
  dtype/device contracts.

Initial numerical surface landed:

- `matrix/lu` using complex partial-pivot LU factorization;
- `matrix/determinant` using the complex LU factorization and pivot parity;
- `matrix/solve` using complex partial-pivot Gaussian elimination for rank-1
  and rank-2 right-hand sides;
- `matrix/inverse` using complex Gauss-Jordan elimination against an identity
  RHS;
- `matrix/rank` using complex magnitude pivoting and tolerance;
- direct `matrix/norm` using complex magnitude for default/`'frobenius`,
  `'one`, `'infinity`, and `'max`.
- `matrix/singular-values` and spectral/nuclear `matrix/norm` over fixed-width
  complex tensors using component-width real singular-value outputs.

`TENSOR-100H` closure is split into three lanes:

1. `TENSOR-100H-SVD-FACTORS`: full fixed-width complex `matrix/svd` factor
   output. CPU oracle support lands before backend execution. Vulkan/CUDA
   support must return fixed-width complex `u`/`v` tensors plus
   component-width real `s`; singular-value-only helpers are insufficient.
2. `TENSOR-100H-CUDA-SVD-NORMS`: CUDA fixed-width complex
   `matrix/singular-values`, spectral/nuclear `matrix/norm`, and
   `matrix/svd`. This lane needs cuSOLVER-backed helper ABIs, adapter PTX,
   capability bits, status mapping, shape guards, tolerance policy, and tests
   proving no hidden CPU fallback.
3. `TENSOR-100H-COMPLEX-EIGEN`: complex eigen/eigenpair contracts across CPU
   oracle and eligible backends. This lane is closed for CPU and Vulkan:
   fixed-width complex values/vectors are returned where applicable through
   existing `matrix/eigenvalues`, `matrix/eigenvectors`, and
   `matrix/eigenpairs`; CUDA eigen remains false until a separate cuSOLVER
   eigen lane is explicitly opened.

The operational closure plan is
`docs/plans/fixed-width-complex-closure-plan-2026-04-18.md`.

## Current Negative Constraints

- Do not resume the rolled-back generic Vulkan unary `map` mode-3 branch as the
  next default. `sin(double)` failed GLSL compilation and an arithmetic-only
  unary branch failed at runtime while binary `map` stayed healthy. The landed
  unary `+` / `abs` path is a separate two-buffer helper with its own
  push-constant ABI.
- Do not treat serial Vulkan LU/solve/inverse shaders as the performance
  solution. They are correctness-preserving backend paths and small-size
  fallbacks.
- Do not treat Big* Tensor storage as Vulkan-compatible. It is pointer-backed
  and semantically high precision.
- Do not add hidden CPU fallback for Vulkan operands. If Vulkan cannot execute
  an operation, raise the existing Tensor backend diagnostic.

## Next Checkpoint

Continue broader Vulkan math-library work from the shipped dense row-major
`Float64`/`Float32` helper families, not from the rolled-back generic unary
`map` mode-3 branch.

Non-Docker implementation order:

1. Landed: Vulkan `map` and map-backed `min` / `max` preflight ordering now
   rejects unsupported mixed CPU/Vulkan lazy operands and unsupported callables
   before concrete realization can materialize CPU inputs.
2. Landed: CUDA now proves the shared probability-domain status ABI for
   data-dependent unary distribution math through `stats/normal-quantile`.
   Status `1` reports invalid probability outside `0 < p < 1`; status `2`
   reports non-finite input; higher status wins for mixed invalid tensors, and
   nonzero status fails before result exposure.
3. Landed: CUDA `stats/normal-quantile` uses generated CUDA C/libdevice PTX op
   `20` for dense row-major `Float64`/`Float32` Tensor storage.
4. Landed: Vulkan `Float32` `stats/normal-quantile` uses a separate
   status-bearing inverse-CDF helper. Status semantics match CUDA while
   preserving Vulkan placement and `Float32` dtype for valid probabilities.
5. Landed: Vulkan `Float64` `stats/normal-cdf` uses a documented piecewise
   polynomial double approximation in the Float64 unary shader/helper without
   hidden CPU fallback or Float32 downcast.
6. Landed: Vulkan `Float64` `stats/normal-quantile` uses a separate
   status-bearing inverse-CDF helper. Direct and mapped paths preserve Vulkan
   placement and `Float64` dtype for representative valid probabilities;
   invalid probability and non-finite status map to the scalar-compatible
   diagnostics before output exposure. Do not infer this support from CUDA
   opcode `20` or Vulkan `Float32`, and do not downcast or route through CPU.
7. Landed: CUDA-first dtype-changing Tensor rounding uses a backend
   helper/status/copyback path and native CPU `Tensor BigInteger` result
   materialization; do not add same-dtype rounding map opcodes.
8. Landed: Vulkan Tensor rounding remains separate from generic Vulkan
   Float64/Float32 and same-dtype map/scientific capabilities. Direct Vulkan
   `floor`, `ceiling`, `round`, and `truncate` report
   `rounding-big-integer` only when `shaderInt64`-backed integer/status/copyback
   support is available and return CPU `Tensor BigInteger` results.
9. Landed: add Tensor view/layout representation metadata through
   `tensor-layout`.
10. Landed: define the narrow public read-only transpose-view contract through
   `matrix/transpose-view`, with CPU read/materialize support and fail-closed
   GPU/copy-kernel posture.
11. Landed: define CPU fixed-width complex Tensor storage before Vulkan complex
    kernels and direct Vulkan general `matrix/eigenpairs`; Vulkan complex GPU
    ABI, capability reporting, copy semantics, and shipped kernels are now
    closed for this lane.
12. Treat large-`k` SVD and large-`n` symmetric eigen improvements as
   measurement-led performance work. Direct Vulkan `matrix/svd` and symmetric
   eigen correctness already landed for dense row-major supported inputs; do
   not replace them without checked-in measurements that justify a tiled or
   staged rewrite.

Existing measured solve facts remain active: `Float64` staged solve keeps the
measured `OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_MIN_N_F64 = 65` threshold, and
`Float32` staged solve keeps the matched
`OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_MIN_N_F32 = 65` parity threshold. Full
blocked trailing-update LU is a future performance lane only if later
measurements justify it. Mature serial helper plumbing is shared for
compute-pipeline creation, sequential storage-buffer descriptor resources,
one-time command submission, and compatible `Float64` mapped status readback.
Parallel solve status remains a separate typed `uint32_t` ABI by design.
