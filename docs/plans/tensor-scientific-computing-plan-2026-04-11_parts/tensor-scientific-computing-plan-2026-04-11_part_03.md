# tensor-scientific-computing-plan-2026-04-11 Part 03

Source: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`

### Architecture

Runtime value model:

- Add a native `Tensor` runtime value representation rather than modeling
  tensors as `Array` of boxed values.
- Add a tensor-expression protocol. A concrete `Tensor` participates in that
  protocol; lazy `map`/`contract` expression values can be added behind the
  same surface.
- Prefer a heap-backed `TensorVal` payload owned by the current scope/region
  wrapper destructor path.
- Keep the ownership authority in scope/region retain/release machinery.
- Do not introduce a per-type refcount for tensors.
- Storage metadata should include:
  - dtype enum or interned type symbol,
  - rank,
  - shape array,
  - strides array,
  - element count,
  - byte length,
  - data pointer,
  - mutability flag,
  - view/owner flag reserved for later slicing.

Likely code ownership:

- value/tag plumbing:
  - `src/lisp/value_types*.c3`
  - `src/lisp/value_predicates_accessors_basic.c3`
  - `src/lisp/value_print_helpers.c3`
  - `src/lisp/primitives_meta_types.c3`
- primitive implementation:
  - new `src/lisp/prim_tensor*.c3` files
  - primitive registration maps
  - compiler primitive variable/hash maps where needed
- dispatch/type integration:
  - builtin type registry setup
  - `type-of`, `is?`, `type-args`, and method dispatch applicability
- collection integration:
  - `length`
  - `ref`
  - `(set! tensor [i j] value)` only after indexing semantics are stable
  - `map`
- compiler/JIT/AOT:
  - direct primitive calls first,
  - compiler parity cases after interpreter behavior is green,
  - fusion for `(realize (map ...) out)` and `(realize (contract ...) out)`
    only after allocation semantics are correct.

Storage and backend policy:

- MVP uses native contiguous CPU `Float64` storage; the first follow-up storage
  dtype is concrete native `BigFloat`.
- Strides should be present in the metadata early, even if all MVP tensors are
  contiguous. This avoids painting slicing/views into a corner.
- The core implementation owns the semantic fallback for `map`, `contract`,
  and `realize`; it must not require BLAS, LAPACK, CUDA, or cuBLAS to be
  present for correctness.
- BLAS/LAPACK/CUDA/cuBLAS integrations are optional backend optimizations, not
  separate first-surface types.
- Optional means both build-optional and run-optional:
  - a normal Omni build and test run must succeed without BLAS/LAPACK/CUDA
    headers, shared libraries, drivers, or GPUs,
  - backend modules may be compiled or loaded only when their dependencies are
    present,
  - missing backends must fail or fall back deterministically rather than
    changing tensor semantics.
- Native library and context resources must use the FFI handle policy:
  - `dlopen`/backend library handles and cuBLAS context handles are
    `ForeignHandle`-style opaque resources, not raw integer pointers,
  - ownership and finalizers are explicit at the backend boundary,
  - backend handles must not own Omni `Value` graphs.
- Sparse and GPU storage strategies are deferred behind `Tensor`; they must not
  introduce user-facing `SparseTensor` or `GpuTensor` as the first canonical
  surface.

### `TENSOR-080` Backend boundary decision

Status: design/contract slice only. No backend runtime code lands here.

This slice closes the semantic contract for later BLAS/cuBLAS work without
adding a new user-facing runtime surface.

Decision points:

- The pure `Tensor` fallback remains the semantic source of truth for
  `map`, `contract`, and `realize`.
- Optional backend modules may accelerate evaluation only when their capability
  contract matches the operation.
- Backend selection is capability-driven and deterministic.
- Missing backend dependencies must fall back to the pure implementation rather
  than altering tensor semantics.
- Ordinary Tensor storage remains native and scope/region owned.
- Backend resources that are truly opaque foreign resources use explicit
  ownership/finalizer policy and must not own Omni `Value` graphs.
- GPU/device movement is explicit; ordinary tensor operations never imply
  implicit CPU/GPU transfers.
- No public backend-flavored mathematical names are introduced as the normal
  surface.

Capability / fallback contract:

| Operation class | Pure fallback | Optional backend module | Dispatch contract |
| --- | --- | --- | --- |
| Dense rank-2 `Float64` contraction | pure `contract` kernel | `tensor/blas` | only contiguous `Float64`, exact rank-2 layout, no aliasing, no hidden transfer |
| Decomposition and solve routines | pure `matrix/solve` fallback, pure `matrix/lu` factorization, pure `matrix/determinant` via LU, pure `matrix/inverse` via identity solve, pure `matrix/qr` reduced factorization, pure `matrix/cholesky` factorization, pure `matrix/singular-values` extraction, pure `matrix/svd` reduced factorization, pure symmetric `matrix/eigenvalues` and `matrix/eigenvectors`, pure general `matrix/eigenpairs` fallback, pure dtype-preserving `matrix/diagonal` transform, pure dtype-preserving `matrix/diagonal-matrix` constructor, pure dtype-selectable `matrix/identity` constructor, pure dtype-preserving `matrix/trace` reduction, pure `matrix/rank` numerical reduction, pure `matrix/norm` reduction, optional `dgesv` for solving/inversion, optional `dgetrf` for LU/determinant factorization, optional `dgeqrf`/`dorgqr` for QR factorization, optional `dpotrf` for Cholesky factorization, optional `dgesvd` for singular values, reduced SVD, rank singular-value counting, and spectral/nuclear norm selectors, optional `dsyev` for symmetric eigenpairs, optional `dgeev` for general eigenpairs | `tensor/lapack` | only when the operation can preserve existing `Tensor` semantics |
| Explicit host/device movement | none; CPU tensors stay CPU by default | `tensor/cuda` | move only when the caller requests it |
| Dense GPU contraction | pure `contract` kernel on host | `tensor/cublas` | requires explicit CUDA placement and compatible dtype/layout |

Module layering:

- Core runtime owns:
  - `Tensor` value representation,
  - dtype, shape, rank, total length, and `ref`,
  - tensor-expression protocol,
  - `map`, `contract`, and `realize` semantics,
  - pure CPU fallback kernels,
  - deterministic diagnostics and scope/region lifetime behavior.
- A later `tensor` library may own conveniences such as `sum`, `mean`,
  `reshape`, `transpose`, `permute`, `dot`, and `outer`.
- Optional backend modules may provide acceleration:
  - `tensor/blas` for BLAS-backed dense CPU kernels,
  - `tensor/lapack` for decomposition and solve routines,
  - `tensor/cuda` for explicit CUDA device storage/movement policy,
  - `tensor/cublas` for cuBLAS-backed dense GPU kernels.
- Backend modules own discovery and capability checks:
  - detect candidate shared libraries at runtime when possible,
  - expose availability through a small capability table,
  - pick accelerated kernels only when dtype, rank, layout, aliasing, and
    device placement match the kernel contract,
  - otherwise route to the pure fallback.
- Backend modules optimize execution; they do not change `Tensor`, `map`,
  `contract`, or `realize` semantics.
- The first CPU BLAS integration is a native runtime shim over shared-library
  discovery for `cblas_dgemm`; it is not routed through user-visible FFI
  `ForeignHandle` values.
- Solver/decomposition conveniences use the `matrix/` namespace for rank-2
  Tensor operations. Do not publish bare `solve`, `linalg/solve`,
  `tensor/solve`, or backend-flavored public solver names.
- The canonical user-facing contraction stays `contract`. Do not expose
  `blas-matmul`, `cublas-contract`, or similar backend-flavored names as the
  normal mathematical surface.
- Backend dispatch should happen at the realization/evaluation boundary,
  especially for `(realize (contract ...) out)` and
  `(realize (map ...) out)`.
- GPU/CUDA support requires explicit device movement in the first design.
  Do not introduce implicit CPU-GPU transfer in ordinary `map`, `contract`, or
  `realize`.
- BLAS first target:
  - optimize only contiguous `Float64` rank-2 contraction equivalent to
    `(contract a b [1 0])`,
  - preserve the C3 fallback as the validation oracle,
  - route unsupported strides, dtypes, aliasing, or shapes back to fallback.
  - Status: implemented 2026-04-14 as `TENSOR-090A` through a private
    `dlopen`/`dlsym` C shim for `cblas_dgemm`, with path-sensitive regression
    coverage when a compatible BLAS library is available.
  - Extended 2026-04-15 as `TENSOR-090C` to rank-2/rank-1 and rank-1/rank-2
    `cblas_dgemv` contraction layouts through the same private backend seam.
- cuBLAS first target:
  - require explicit CUDA device placement before invoking cuBLAS,
  - keep host tensors on CPU fallback unless explicitly moved,
  - keep host/device synchronization and error propagation deterministic.

Locked future device/backend introspection forms from `TENSOR-100`:

```lisp
(to-device x 'cuda)
(to-device x 'cpu)
(device x)

(tensor-backends)
```

`tensor-use-backend!` and backend-specific math names are rejected as the first
GPU control surface. Backend selection remains capability-driven, but GPU
placement is explicit through `to-device`.

### Refinement

Rollout slices:

1. `TENSOR-001` Documentation and surface freeze
   - Land this plan and TODO pointer.
   - Add a short note to future language docs only when implementation starts.

2. `TENSOR-010` Runtime representation
   - Add native Tensor payload, destructor, printing, `tensor?`, `dtype`,
     `shape`, `rank`, and `length`.
   - No `map`, `contract`, or `realize` yet.
   - Status: implemented 2026-04-11. Payloads are current-scope owned,
     payload copy/promotion paths deep-clone shape/stride/data storage, and
     the boundary graph audit treats tensors as leaf payloads.

3. `TENSOR-020` Constructor and indexing
   - Implement `(Tensor Float64 shape data-or-scalar)`.
   - Implement `(ref tensor index-array)`.
   - Keep variadic tensor `ref` deferred.
   - Status: implemented 2026-04-11. The constructor accepts `Float64` only,
     shape as an array or proper list of non-negative integers, and data as
     a numeric scalar fill or exact-length array/proper list. Tensor `ref`
     accepts array/proper-list indices and supports negative indexing per
     axis. Inferred-shape constructor overloads are implemented as
     `TENSOR-076`.
     axis.

4. `TENSOR-030` Tensor-expression protocol and `realize` — complete for
   concrete tensor/scalar sources
   - Treat concrete `Tensor` as a tensor expression.
   - Implement `(realize expr)`.
   - Implement `(realize expr out)`.
   - Implement scalar-to-destination fill through `(realize scalar out)`.
   - No lazy expression fusion is required yet.

5. `TENSOR-040` Elementwise `map`
   - Implement unary tensor `map`.
   - Implement tensor-scalar `map`.
   - Implement exact-shape tensor-tensor `map`.
   - Defer singleton-axis broadcast until after exact-shape coverage is green.
   - Status: implemented 2026-04-11. The runtime exposes fixed-arity typed
     `map` overloads over an internal `__tensor-map` primitive for unary,
     tensor-scalar, scalar-tensor, and exact-shape tensor-tensor `Float64`
     inputs. The later `TENSOR-060B` slice changed the returned payload to a
     lazy Tensor expression under the same public `Tensor` value.

6. `TENSOR-050` `contract`
   - Implement pure C3 fallback contraction for `Float64`.
   - Support one or more axis pairs.
   - Add rank-2 contraction examples through `contract`.
   - Status: implemented 2026-04-11. The runtime exposes public `contract`
     with pure `Float64` fallback, paired-axis shorthand, exact axis-list arity
     validation, duplicate-axis checks, paired-dimension checks, rank-0 scalar
     result support, and lazy Tensor expression realization through
     `TENSOR-060B`.

7. `TENSOR-060A` Destination realization and expression fusion audit
   - Status: implemented 2026-04-11 as a split/decision slice.
   - Current eager `map` and `contract` already make
     `(realize (map ...) out)` and `(realize (contract ...) out)`
     semantically correct, but they allocate a source tensor before
     `realize` sees it.
   - True fusion requires either a native lazy tensor-expression value that
     `map` and `contract` can return, or making `realize` a macro/special
     form that rewrites recognized source expressions before ordinary argument
     evaluation.
   - Choose the lazy tensor-expression value path for the next implementation
     slice. It keeps `realize` as a runtime storage boundary and gives
     backend dispatch a real expression node to inspect later.
   - Do not add public `map-into`, `matmul`, or backend-flavored escape names
     to compensate for the eager temporary.

8. `TENSOR-060B` Tensor-expression protocol and destination fusion
   - Optimize `(realize (map ...) out)` and
     `(realize (contract ...) out)` when the expression representation
     exists.
   - Implementation prep landed 2026-04-11: the first pure `map` and
     `contract` paths were split through destination-ready internal kernels
     before lazy payloads were wired on top.
   - Status: implemented 2026-04-11. Tensor-dispatched `map` and `contract`
     now return lazy expression payloads under the existing `Tensor` value,
     with `realize` forcing them into allocated concrete storage or staging
     evaluation before copying into an exact-shape/dtype destination tensor
     after success.
   - `TENSOR-060C` follow-up status: implemented 2026-04-12. Shape parsing now
     checks the signed-to-`usz` boundary through an unsigned comparison, and
     zero-size contracted axes realize to the additive identity.
   - The implementation keeps expression nodes internal to native `Tensor`
     payloads; no public `TensorExpr` type, macro rewrite, `map-into`, or
     backend-specific operation name was added.
   - Keep ordinary eager-source copy semantics as the fallback.
   - Aliasing policy: elementwise `map` may safely realize into an input
     tensor; `contract` rejects destinations that alias either source tensor.

9. `TENSOR-070` Broadcasting extension
   - Concrete trailing singleton rule for tensor-tensor `map`:
     - align shapes by trailing axes, treating missing leading axes as `1`;
     - axis pair is valid when equal or one side is `1`;
     - result axis is axis-wise max;
     - incompatible axes raise `tensor/shape-mismatch`.
   - Keep shape errors deterministic and payloaded.
   - Keep map dispatch backend-neutral: no backend-specific tensor types in
     public surface signatures.
   - Status: implemented 2026-04-12. Tensor-tensor `map` now right-aligns
     shapes, treats missing leading axes as `1`, expands singleton axes to the
     result max dimension, broadcasts rank-0 tensors as tensor scalars, and
     raises deterministic `tensor/shape-mismatch` errors for incompatible
     axes.

10. `TENSOR-080` Tensor library facade and backend boundary
   - Status: design/contract slice only; no backend runtime code lands here.
   - Decide the import/module path for the high-level tensor library.
   - Keep backend-specific names out of the public semantic surface.
   - Document that `contract` and `realize` are the semantic hooks that
     backend optimizations must preserve.
   - Define the optional backend capability table and fallback routing
     contract.
   - Split the backend boundary into a pure fallback facade plus optional
     `tensor/blas`, `tensor/lapack`, `tensor/cuda`, and `tensor/cublas`
     modules, without making those backends required for core correctness.
   - Record the no-implicit-transfer rule for CUDA-style movement and the
     explicit ownership/finalizer boundary for genuinely opaque native backend
     resources.
   - Acceptance criteria:
     - the pure fallback remains valid for every covered tensor operation,
     - backend selection is capability-driven and deterministic,
     - missing backend dependencies fall back without changing semantics,
     - native handles stay outside Omni `Value` ownership,
     - no public `TensorExpr`, backend-flavored math name, or implicit
       host/device transfer surface is introduced.
   - Status: closed 2026-04-12 as a design/contract slice.

11. `TENSOR-090` BLAS/LAPACK backend design and rank-2 contraction
    - Use FFI/native library paths only after the pure fallback behavior is
      stable.
    - Keep fallback available for validation and portability.
    - Treat BLAS-backed rank-2 contraction as an optimization of
      `(realize (contract a b [1 0]) out)`, not as a new canonical
      contraction operation.
    - Keep ordinary Tensor storage native/scoped; gate genuinely opaque native
      handles through explicit ownership and finalizer policy.

12. `TENSOR-100` CUDA/cuBLAS backend design
    - Require explicit host/device movement in the design.
    - Keep GPU storage behind `Tensor` rather than introducing a first-surface
      GPU tensor type.
    - Keep GPU-heavy validation on the bounded container path.
    - Treat cuBLAS handles as owned foreign handles with deterministic
      creation/destruction and no Omni value ownership.
    - Status: closed 2026-04-16. The canonical future device surface is
      `to-device`, `device`, and `tensor-backends`; `GpuTensor`,
      `CudaTensor`, backend-flavored math names, `tensor-use-backend!` as the
      first control surface, and implicit CPU/GPU transfer are rejected.
    - `TENSOR-100A` status: implemented 2026-04-16. `device` and `to-device`
      are registered for CPU-only placement semantics, with CUDA movement
      failing closed until a CUDA backend lands.
    - `TENSOR-100B` status: implemented 2026-04-16. `TensorVal` stores
      placement metadata and an opaque device handle/finalizer slot,
      `tensor-backends` reports structured CPU/CUDA availability, CPU kernels
      reject non-CPU storage, and fake-device destruction is covered by
      memory-lifetime tests. The next CUDA/cuBLAS slice is explicit
      CPU<->CUDA copy semantics for concrete `Float64` Tensor storage.
    - `TENSOR-100C` status: implemented 2026-04-16. `to-device 'cuda` copies
      concrete `Float64` CPU Tensor storage to CUDA when runtime-loaded
      libcudart is usable, `to-device 'cpu` copies CUDA storage back to native
      CPU storage, and CUDA availability requires symbol resolution, device
      count, and allocation/free probing. The next slice is cuBLAS handle
      lifecycle and CUDA-placed rank-2 `Float64` contract execution.
    - `TENSOR-100D` status: implemented 2026-04-16. Runtime-loaded cuBLAS
      handle lifecycle is per operation, `tensor-backends` reports `cublas`,
      CUDA-placed dense row-major `Float64` rank-2/rank-2 single-axis
      contractions execute through `cublasDgemm_v2` for `[1 0]`, `[0 0]`,
      `[1 1]`, and `[0 1]`, and rank-2/rank-1 plus rank-1/rank-2
      contractions execute through `cublasDgemv_v2`, returning CUDA-placed
      Tensor storage. Focused regressions cover available execution for all
      GEMM/GEMV layouts, unsupported CUDA contract diagnostics, zero-size
      fail-closed behavior, and forced cuBLAS unavailable reporting.
    - `TENSOR-100E` status: completed correctness-first baseline 2026-04-17.
      Vulkan backend probing is
      runtime-loaded and optional, `tensor-backends` reports `vulkan` with
      explicit `Float64` kernel capability, `to-device 'vulkan` copies
      concrete `Float64` CPU Tensor storage into opaque Vulkan buffers when
      available, `to-device 'cpu` copies Vulkan storage back to native CPU
      Tensor storage, `map` supports dense row-major Vulkan `Float64`
      arithmetic `+`, `-`, `*`, and `/` for Tensor/scalar, scalar/Tensor,
      exact-shape Tensor/Tensor inputs, and right-aligned singleton-axis
      Tensor/Tensor broadcasting, `matrix/transpose` supports Vulkan-placed
      dense row-major `Float64` rank-2 tensors, `matrix/diagonal` and
      `matrix/diagonal-matrix` support Vulkan-placed dense row-major `Float64`
      inputs, `matrix/trace` supports Vulkan-placed dense row-major square
      `Float64` inputs, `matrix/rank` supports Vulkan-placed dense row-major
      `Float64` rank-2 inputs, `matrix/determinant` supports Vulkan-placed
      dense row-major square `Float64` inputs, `matrix/solve` supports
      Vulkan-placed dense row-major square `Float64` coefficient tensors with
      Vulkan-placed rank-1 or rank-2 `Float64` right-hand tensors and routes
      larger systems through the first thresholded parallel solve helper,
      `matrix/inverse` supports Vulkan-placed dense row-major square
      `Float64` inputs, `matrix/cholesky` supports Vulkan-placed dense
      row-major square `Float64` inputs, `matrix/qr` supports Vulkan-placed
      dense row-major rank-2 `Float64` inputs with rows greater than or equal
      to columns, `matrix/norm` supports
      Vulkan-placed dense row-major
      `Float64` inputs for
      default/`'frobenius`, `'one`, `'infinity`, and `'max`, and public
      `contract` supports Vulkan-placed dense row-major
      rank-N `Float64` tensors with one or more
      explicit contracted axis pairs. Contract output axes are ordered as free
      left axes followed by free right axes. Results remain Vulkan-placed. The
      generic contract shader is generated from checked-in GLSL and uses
      rank/shape/stride/axis-list metadata-buffer dispatch.
      Unsupported Vulkan contract cases fail closed with backend diagnostics
      rather than silently copying to CPU; zero-size contracted axes in
      supported Vulkan layouts produce additive-identity output.
      The Vulkan dtype/layout policy is recorded in
      `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`: keep fixed-width
      dtype paths explicit, do not downcast between `Float64` and `Float32`,
      do not lower pointer-backed Big* Tensor dtypes to Vulkan, keep landed
      fixed-width complex backend support behind explicit capability bits, and
      defer full complex SVD factor output, CUDA complex singular-values/norm/SVD,
      complex eigen result contracts, and stride-aware layouts until their
      contracts exist.

13. `TENSOR-110` Example migration - complete
    - `examples/scicomp_demo.omni` now uses canonical `Tensor`, `map`,
      `contract`, and `realize`.
    - `vec-*`, `mat-*`, and `mat-mul` are removed from the canonical example
      surface.
    - Lazy `map`/`contract` return and closure-capture regressions now cover
      the cleanup lane.

14. `TENSOR-120` Autodiff design note
    - Design AD around differentiable tensor operations, not arbitrary Omni
      execution.
    - Treat `map` and `contract` as the first differentiable primitive family.

First implementation decisions:

- `Tensor` constructor accepts value-position dtype descriptors like `Float64`
  and `BigFloat` as the canonical dtype argument. Quoted dtype symbols such as
  `'Float64` and `'BigFloat` are accepted compatibility inputs, but docs should
  prefer value-position descriptors.
- Tensor `map` result dtype is the dtype of the first tensor input for the
  `Float64` kernel slice. Explicit conversion can be expressed as another
  mapped operation once non-Float64 kernels exist.
- Destination `realize` requires exact dtype compatibility in the first
  slice. Explicit conversion must happen before realization.
- `TENSOR-070` concrete singleton-axis broadcasting is implemented:
  right-align
  shapes, treat missing leading axes as `1`, require compatibility when either
  axis is `1` or equal, and set result axes with max. Exact-shape path stays the
  primary fast path.
- Map dispatch stays backend-neutral: only canonical `Tensor` surface types
  participate; no backend-specific tensor types or names are introduced.
- `length` for `Tensor` means total element count.
