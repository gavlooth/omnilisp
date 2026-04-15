# Tensor Scientific Computing Plan (2026-04-11)

Status: `in progress`; active TODO lanes
`LANG-TENSOR-BACKEND-BOUNDARY-092` and `LANG-TENSOR-BROADCASTING-093` are
closed
Owner: language owner + Codex workflow
Mode: implementation plan and progress log
TODO lanes: `LANG-TENSOR-BACKEND-BOUNDARY-092`,
`LANG-TENSOR-BROADCASTING-093`
Historical lane: `LANG-TENSOR-SCIENTIFIC-SURFACE-091` closed

## Purpose

Define Omni's first coherent scientific/ML numeric surface without copying
Julia syntax wholesale and without weakening Omni's existing language
invariants.

The chosen direction is:

- `Tensor` is the canonical rank-polymorphic numeric aggregate.
- `Array` remains the generic boxed collection.
- `map` is extended through dispatch for elementwise tensor operations and
  scalar broadcast.
- `contract` is the canonical tensor contraction operation.
- `realize` is the canonical tensor-expression-to-storage boundary.

This plan records the naming decisions, semantic contracts, rollout slices,
test gates, deferred work, and implementation progress.

## Current Context

Omni already has the right language substrate for a scientific layer:

- multiple dispatch is a core extension mechanism,
- typed method dispatch is deterministic and test-anchored,
- parametric types and constructor type-application checking exist,
- numeric conversion in dispatch is explicit by design,
- generic collection operations already exist (`map`, `filter`, `foldl`,
  `ref`, `length`),
- deterministic scope/region memory ownership is a language invariant.

The missing piece is the numerical substrate. Current `Array` is a mutable
dynamic collection of boxed Omni values. It is useful as a general collection,
but it is not a high-performance homogeneous numeric storage type.

`examples/scicomp_demo.omni` now uses canonical `Tensor`, `map`,
`contract`, and `realize` forms. The older `vec-*`, `mat-*`, and `mat-mul`
prototype vocabulary is no longer part of the shipped surface.

Name collision note:

- Deduce already has the namespaced command `deduce/materialize!` and the
  relation attribute `[relation ... materialized]`.
- Tensor `realize` is a different concept: it turns a tensor expression
  into concrete tensor storage, optionally into an existing destination tensor.
- Do not rename or fold the Deduce materialized-view command into this tensor
  operation.

## Implementation Progress

- `TENSOR-001` is complete: the surface is frozen around `Tensor`, tensor
  dispatch through `map`, `contract`, and `realize`.
- `TENSOR-010` is complete: native `Tensor` runtime payloads, destructor,
  printing, type identity, boundary copy/promotion support, and `tensor?`,
  `dtype`, `shape`, `rank`, and `length` are implemented and tested.
- `TENSOR-020` is complete: `(Tensor Double shape data-or-scalar)` constructs
  native double tensors from scalar fills or exact-length array/proper-list
  data, and `(ref tensor index-array)` indexes tensors with per-axis negative
  index support.
- `TENSOR-076` is complete: `(Tensor data)`, `(Tensor data Double)`, and
  `(Tensor Double data)` infer native `Double` tensor shape from real numeric
  scalars or rectangular nested arrays/proper lists whose leaves can narrow to
  finite `Double` while preserving the explicit shape/data constructor.
- `TENSOR-030` is complete for concrete tensor realization: concrete
  tensors satisfy the tensor-expression boundary, `(realize tensor)`
  returns the already-concrete tensor, `(realize tensor out)` copies into a
  mutable exact-shape/dtype destination, and `(realize scalar out)` fills
  the destination.
- `TENSOR-040` is complete for elementwise `map`: unary tensor, tensor-scalar,
  scalar-tensor, and exact-shape tensor-tensor `Double` inputs route through
  the existing generic `map` dispatch.
- `TENSOR-070` is complete: Tensor `map` now supports NumPy-style trailing
  singleton-axis broadcasting for tensor-tensor inputs, with rank-0 tensors
  treated as broadcast-compatible tensor scalars and deterministic
  `tensor/shape-mismatch` failures for incompatible shapes.
- `TENSOR-075` is complete: the canonical collection constructor/conversion
  surfaces now accept Tensor values. `(Array tensor)` and `(List tensor)`
  force lazy Tensor expressions if needed and expose flat row-major element
  values; shape/rank metadata remains available through `shape` and `rank`
  rather than nested collection conversion.
- `TENSOR-050` is complete for pure `Double` `contract`: paired-axis arrays
  and explicit axis-list contraction support one or more axis pairs, rank-0
  scalar results, multi-axis contractions, per-axis negative normalization,
  and deterministic axis/dimension diagnostics.
- `TENSOR-060A` is complete as a fusion-boundary audit: true
  `(realize (map ...) out)` / `(realize (contract ...) out)` fusion
  requires a lazy tensor-expression value or a macro/special-form rewrite,
  because ordinary eager evaluation realizes the source before
  `realize` receives it.
- `TENSOR-060B-prep` is complete: eager `map` and `contract` now execute
  through internal destination-ready pure Tensor kernels, so a later lazy
  expression value can realize through staged destination storage without
  reimplementing the elementwise or contraction loops.
- `TENSOR-060B` is complete: Tensor-dispatched `map` and `contract` now return
  lazy expression payloads under the existing `Tensor` value, and
  `realize` can allocate a concrete result or stage evaluation before
  copying into a destination tensor after success.
- `TENSOR-060C` is complete: valid non-negative Tensor dimensions no longer
  trip the platform-size overflow guard on 64-bit builds, and contraction over
  zero-size axes realizes the additive identity instead of entering the
  contracted-index divide/modulo path.
- `TENSOR-060D` is complete: rank-0 concrete destination realization,
  zero-size destination/realization paths, aliased elementwise `map`
  destination realization, and duplicate-axis checks after negative-axis
  normalization are covered.
- `TENSOR-060E` is complete: tensor expression child values copied or promoted
  across boundary paths are now rolled back if a later tensor copy/promotion or
  wrapper allocation step fails. Generic realized-value cleanup also
  recurses through tensor expression edges before freeing the copied tensor
  payload.
- `TENSOR-060F` is complete: tensor values now use the non-unique destination
  retry promotion path at return boundaries, and concrete `realize` fast
  paths validate source backing storage before returning or copying malformed
  tensors.
- `TENSOR-060G` is complete: lazy Tensor `ref` cancels and destroys the
  temporary concrete realization after extracting the scalar result, with
  destructor-count regression coverage in the memory-lifetime smoke slice.
- `TENSOR-060H` is complete: nested lazy Tensor `realize` now cleans
  temporary child tensors created while resolving nested `map`/`contract`
  expression operands, leaving only the top-level realized result live
  until caller cleanup.
- `TENSOR-060I` is complete: failed lazy Tensor realization now cleans the
  fresh concrete result tensor through the generic fresh-value cleanup path,
  with nested `contract` realization and failed `map` realization
  destructor-count coverage in the memory-lifetime smoke slice.
- `TENSOR-060J` is complete: lazy Tensor `realize` into an explicit
  destination now stages expression evaluation into a temporary Tensor and
  commits to the destination only after success. The contract destination/source
  aliasing rejection walks lazy expression operands while ignoring zero-byte
  tensor storage, and failed Tensor constructor data validation cleans the
  unreturned tensor wrapper.
- `TENSOR-060K` is complete: lazy Tensor expression edge rollback now has
  deterministic copy/promotion fail counters and regression coverage for a
  mid-edge copy failure after a prior child tensor copy has committed. Root
  promotion of lazy Tensor expressions now promotes tensor operand edges into
  the destination ESCAPE lane, while graph-audit traversal treats shared
  dispatch method tables as callable descriptors rather than Tensor-owned
  expression data.
- `TENSOR-110` is complete: the Tensor cleanup surface lane is closed with a
  canonical example in `examples/scicomp_demo.omni` and newly added lazy
  `map`/`contract` return and closure-capture regression coverage.
  Metadata examples retain `Array`-based shape/axes payload forms where that
  transport format was already canonical.
- Current next slices are optional backend follow-ons: the remaining
  `TENSOR-090` BLAS/LAPACK work beyond the transpose-capable rank-2 `dgemm`
  and rank-2/rank-1 `dgemv` fast paths, and `TENSOR-100` explicit
  CUDA/cuBLAS design.
- `TENSOR-090A` is complete: dense rank-2 `Double` contraction equivalent to
  `(contract a b [1 0])` now has a direct native BLAS `dgemm` fast path at the
  Tensor evaluation boundary. The fast path is optional at runtime, uses the
  existing scoped native `TensorVal` storage, does not introduce a public
  `TensorHandle`, and falls back to the pure C3 contraction kernel when no
  compatible BLAS library/symbol is available or when dtype/rank/layout/axis
  conditions do not match.
- `TENSOR-090B` is complete: that private `dgemm` backend now accepts
  transpose flags and covers all contiguous row-major rank-2 single-axis
  `Double` contractions, including `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`,
  while still using the pure C3 contraction kernel as the semantic fallback.
- `TENSOR-090C` is complete: the same private BLAS backend now resolves
  `cblas_dgemv` and covers contiguous row-major rank-2/rank-1 and rank-1/rank-2
  single-axis `Double` contractions, including transposed matrix-vector and
  vector-matrix layouts. Unsupported rank/layout/symbol cases still fall back
  to the pure C3 contraction kernel.
- Solver naming checkpoint: future LAPACK/LAPACKE solve/decomposition
  conveniences must not expose a bare `solve`; `linalg/` is not yet locked as
  the base namespace. Backend-module labels such as `tensor/lapack` describe
  implementation ownership, not a finalized public convenience prefix.

## Locked Naming Decisions

### `Tensor`

Use `Tensor` as the canonical public name for the n-dimensional homogeneous
numeric aggregate.

Rationale:

- It matches the practical ML/scientific meaning used by NumPy, PyTorch, JAX,
  and similar ecosystems.
- It avoids exposing storage representation in the public type name.
- It covers rank 0, rank 1, rank 2, and rank n without introducing `Vector`
  terminology.
- It keeps `Matrix` free for a possible rank-2 convenience layer later.

Contract:

- `Tensor` is computational, not a promise of full coordinate-transform tensor
  algebra.
- The first surface means: dtype, shape, rank, homogeneous numeric storage,
  indexing, elementwise expressions, contraction expressions, and explicit
  realization into storage. Reductions remain planned library conveniences,
  not part of the shipped first surface.

### `Array`

Keep `Array` as the generic boxed collection.

Rules:

- Do not make `Array` the scientific numeric aggregate.
- Do not rename `Array` to `Tensor`.
- Do not introduce `Vector` as a public generic collection term.

### `map`

Extend `map` through dispatch as the elementwise tensor operation.

Canonical forms:

```lisp
(map sqrt x)
(map + x y)
(map * x 2.0)
```

Contract:

- For tensors, `map` produces a tensor-shaped expression.
- The current implementation represents that expression as a lazy `Tensor`
  payload and realizes it only at `realize` or `ref`.
- Tensor `map` currently accepts one or two tensor/scalar inputs.
- Scalar arguments broadcast over tensor arguments.
- First implementation supports scalar broadcast and exact-shape tensor-tensor
  inputs.
- `TENSOR-070` adds concrete trailing singleton expansion for tensor-tensor `map`.
  Exact-shape matching remains the exact fast path.
- Elementwise multiplication is `(map * a b)`, not `(* a b)`.

`TENSOR-070` broadcast rule for tensor-tensor `map`:

- Align shapes by trailing axes (right-align dimensions).
- Treat missing leading axes of a shorter shape as `1`.
- For each aligned axis pair, dimensions are compatible when equal or either is `1`.
- If all axes are compatible, result axis size is `max(axis_a, axis_b)`.
- If any axis pair is incompatible, raise `tensor/shape-mismatch`.
- Do not add backend-specific tensor types or map names in this dispatch path.

### `contract`

Use `contract` as the canonical summed-index tensor algebra operation.

Canonical form:

```lisp
(contract a b [1 0])
```

Meaning:

- contract axis `1` of `a` with axis `0` of `b`.
- for rank-2 shapes `[m k]` and `[k n]`, result shape is `[m n]`.

Multi-axis form:

```lisp
(contract a b [[1 0] [2 3]])
```

Rules:

- `contract` produces a tensor-shaped expression.
- paired-axis arrays are canonical; the explicit
  `(contract a b left-axes right-axes)` form remains accepted.
- explicit axis lists must have equal length.
- each axis must be in range for its tensor.
- dimensions for paired axes must match.
- result shape is all non-contracted axes from the left tensor followed by all
  non-contracted axes from the right tensor, preserving source order.
- no implicit default contraction is provided for general tensors.

### `realize`

Use `realize` as the canonical tensor-expression-to-storage boundary.

Canonical forms:

```lisp
(realize (map + a b))
(realize (map sqrt x) x)
(realize (contract a b [1 0]) c)
(realize 0.0 x)
(realize src out)
```

Contract:

- `(realize expr)` returns a concrete `Tensor`.
- `(realize expr out)` writes the expression into existing mutable tensor
  `out` and returns `out`.
- A concrete `Tensor` satisfies the tensor-expression protocol.
- If `expr` is already a concrete tensor and no destination is provided,
  `realize` may return the tensor unchanged.
- If `expr` is a scalar, a destination tensor is required; every element of the
  destination is set to that scalar, subject to dtype policy.
- If `expr` is a tensor and a destination is provided, shape and dtype
  compatibility are required.
- If `expr` is a lazy tensor expression, the first implementation may evaluate
  it into a temporary tensor and copy it into the destination.
- Later compiler/JIT optimization may fuse recognized `map` and `contract`
  expressions into destination storage, but fusion is an optimization, not the
  user-visible semantic contract.
- Unsafe aliasing must either use a scratch tensor or raise a deterministic
  diagnostic.

## Rejected Names And Non-Goals

Rejected as canonical:

- `DenseArray`: names storage representation, not the mathematical object.
- `Matrix`: too rank-2 in normal mathematical/programming use; do not use as
  the rank-polymorphic aggregate name.
- `Vector`: conflicts with the repo's collection terminology guardrail and
  should not be introduced for the first tensor surface.
- `matmul`: programming jargon and rank-2 biased; use `contract`.
- `map!`: compact but hides destination and aliasing semantics.
- `map-into`: technically clear, but awkward to read and too tied to `map`.
- `compute`: too broad.
- `assign`: too vague and too close to binding/update semantics.
- `copy` / `fill` as peers to `realize`: too close to each other; they may
  exist later only as convenience wrappers over `realize`.

Non-goals for the first implementation:

- no Julia dotted operator syntax,
- no implicit numeric widening in method dispatch,
- no automatic `(* tensor tensor)` semantics for general tensors,
- no GPU execution or storage backend in the core MVP,
- no autodiff,
- no sparse tensor backend,
- no view mutation semantics until slicing/view policy is explicit,
- no new shorthand aliases unless the owner explicitly approves them.

## Syntax Target

Use value-position type descriptors for dtype arguments, not `^` annotations in
constructor argument position.

Preferred first surface:

```lisp
(define x
  (Tensor Double [2 3]
    [1.0 2.0 3.0
     4.0 5.0 6.0]))

(dtype x)       ; => 'Double
(shape x)       ; => [2 3]
(rank x)        ; => 2
(length x)      ; => 6
(ref x [1 2])   ; => 6.0
```

Rationale:

- `Double` already exists as a value-position type descriptor/coercion
  surface.
- `^Double` should remain annotation syntax unless a separate type-literal
  expression surface is deliberately added.
- `(ref x [i j])` keeps `ref` as a two-argument operation instead of forcing a
  variadic `ref` decision in the first slice.

Optional constructors after the base constructor is stable:

```lisp
(Tensor Double [2 3] 0.0)  ; scalar fill constructor
(Tensor Integer [4] [1 2 3 4])
```

## SPARC Plan

### Specification

Requirements:

- `Tensor` values are homogeneous.
- Every tensor has:
  - dtype,
  - shape,
  - rank,
  - element count,
  - contiguous or strided numeric storage metadata.
- Initial dtype target is `Double`.
- `Integer` can follow after the `Double` path is validated.
- `Boolean`, `Complex`, `Float32`, `Float64`, and fixed-width integer dtypes
  are deferred.
- Shape vectors use zero-based axis numbering, matching Omni's existing index
  examples.
- Rank 0 must remain design-compatible, even if not implemented in the first
  slice.
- Tensor construction validates data length against shape product.
- Tensor shape product arithmetic must be overflow-checked before allocation.
- Tensor operations report deterministic payloaded `raise` failures.

Diagnostic target:

- Domain: `tensor`.
- Representative codes:
  - `tensor/shape-mismatch`
  - `tensor/rank-mismatch`
  - `tensor/dtype-mismatch`
  - `tensor/axis-out-of-range`
  - `tensor/contract-dimension-mismatch`
  - `tensor/constructor-data-length`
  - `tensor/alias-unsafe`
  - `tensor/out-of-memory`

Acceptance:

- `Tensor` constructor success and failure behavior is documented.
- `map`, `contract`, and `realize` have deterministic shape/dtype/aliasing
  rules.
- The old `vec-*` / `mat-*` exploratory example is not presented as the
  canonical surface.

### Pseudocode

Tensor construction:

```text
Tensor(dtype_descriptor, shape_array, data_or_scalar):
  validate dtype descriptor
  validate shape is an Array of non-negative Integers
  compute element_count with overflow checks
  allocate TensorVal metadata and numeric storage
  if data_or_scalar is scalar:
    coerce scalar to dtype
    fill storage
  else:
    validate data_or_scalar is finite Array or list with element_count items
    coerce every element to dtype or raise dtype mismatch
    write unboxed storage
  return Tensor wrapper
```

Tensor `map`:

```text
map(f, inputs...):
  classify inputs as tensors or scalars
  if tensor inputs are exact-shape equal:
    result_shape = that shared shape
  else:
    align trailing axes between tensor shapes (missing leading axes => 1)
    if every aligned axis satisfies equality or contains 1:
      result_shape = axis-wise max
    else:
      raise tensor/shape-mismatch
  create a tensor expression that records:
    function f
    input expressions/scalars
    result shape
    result dtype policy
  return tensor expression
```

Tensor `contract`:

```text
contract(a, b, left_axes, right_axes):
  validate a and b are tensors
  validate axes are unique, in range, and same count
  validate paired contracted dimensions match
  compute result shape from non-contracted axes
  create a tensor expression that records:
    left expression
    right expression
    contraction axes
    result shape
    result dtype policy
  return tensor expression
```

`realize`:

```text
realize(source, maybe_dest):
  if maybe_dest is omitted:
    if source is concrete Tensor:
      return source
    if source is tensor expression:
      allocate a concrete Tensor for source shape/dtype
      evaluate expression into the new Tensor
      return Tensor
    otherwise:
      raise tensor/dtype-mismatch or type/arg-mismatch

  validate maybe_dest is mutable Tensor
  if source is scalar:
    coerce scalar to maybe_dest dtype
    fill maybe_dest storage
    return maybe_dest
  if source is Tensor or tensor expression:
    validate shape compatibility
    validate dtype compatibility or explicit conversion policy
    handle aliasing
    evaluate/copy source into maybe_dest
    return maybe_dest
  raise tensor/dtype-mismatch or type/arg-mismatch
```

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

- MVP uses native contiguous CPU `Double` storage.
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
| Dense rank-2 `Double` contraction | pure `contract` kernel | `tensor/blas` | only contiguous `Double`, exact rank-2 layout, no aliasing, no hidden transfer |
| Decomposition and solve routines | pure library routine or no-op fallback if not surfaced | `tensor/lapack` | only when the operation can preserve existing `Tensor` semantics |
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
- Solver/decomposition conveniences remain naming-deferred: do not publish
  bare `solve`, and do not assume `linalg/` is the qualifier until the public
  Tensor convenience layer is named.
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
  - optimize only contiguous `Double` rank-2 contraction equivalent to
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

Candidate later device/backend introspection forms:

```lisp
(to-device x 'cuda)
(to-device x 'cpu)
(device x)

(tensor-backends)
(tensor-use-backend! 'blas)
(tensor-backend x)
```

These names are candidates, not locked first-surface decisions.

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
   - Implement `(Tensor Double shape data-or-scalar)`.
   - Implement `(ref tensor index-array)`.
   - Keep variadic tensor `ref` deferred.
   - Status: implemented 2026-04-11. The constructor accepts `Double` only,
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
     tensor-scalar, scalar-tensor, and exact-shape tensor-tensor `Double`
     inputs. The later `TENSOR-060B` slice changed the returned payload to a
     lazy Tensor expression under the same public `Tensor` value.

6. `TENSOR-050` `contract`
   - Implement pure C3 fallback contraction for `Double`.
   - Support one or more axis pairs.
   - Add rank-2 contraction examples through `contract`.
   - Status: implemented 2026-04-11. The runtime exposes public `contract`
     with pure `Double` fallback, paired-axis shorthand, exact axis-list arity
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

- `Tensor` constructor accepts value-position dtype descriptors like `Double`
  as the canonical dtype argument.
- Quoted dtype symbols like `'Double` are not part of the first canonical
  constructor surface.
- Tensor `map` result dtype is the dtype of the first tensor input in the first
  slice. Explicit conversion can be expressed as another mapped operation.
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

### Completion

Documentation gates:

- `docs/LANGUAGE_SPEC.md` documents:
  - `Tensor`,
  - `dtype`, `shape`, `rank`,
  - `length` as tensor element count,
  - tensor `ref`,
  - concrete tensor/scalar/expression `realize`,
  - lazy tensor `map` and `contract` expression payloads.
  - remaining gates for later slices: optional backend routing and diagnostic
    code refinements.
- `docs/type-system-syntax.md` documents how `Tensor` participates in type
  annotations and dispatch, and that `(Tensor data)` plus
  `(Tensor Double shape data-or-scalar)` are the constructor surfaces,
  including Tensor-specific `map`, `contract`, and `realize` behavior.
- `docs/SURFACE_COMPATIBILITY.md` records any scicomp prototype surface
  removal if old names are removed.
- `examples/scicomp_demo.omni` is updated to the canonical `Tensor`, `map`,
  `contract`, and `realize` surface.
- `memory/CHANGELOG.md` records behavior only after code lands and validation
  has concrete evidence.

Test gates:

- Constructor:
  - valid `Double` tensor construction,
  - scalar-fill construction,
  - data length mismatch,
  - invalid shape,
  - shape product overflow,
  - dtype mismatch.
- Introspection:
  - `type-of`,
  - `tensor?`,
  - `dtype`,
  - `shape`,
  - `rank`,
  - `length`.
- Indexing:
  - `(ref tensor [i ...])`,
  - wrong rank index,
  - out-of-range index,
  - negative index policy, either accepted by explicit rule or rejected.
- `map`:
  - unary tensor,
  - tensor-scalar,
  - exact-shape tensor-tensor,
  - singleton-axis tensor-tensor expansion using trailing-axis alignment,
  - shape mismatch,
  - function result dtype mismatch.
- `contract`:
  - rank-2 contraction via `(contract a b [1 0])`,
  - rank-0 scalar result for vector dot product,
  - zero-axis outer product,
  - multi-axis contraction,
  - paired-axis array shorthand plus array and proper-list axis inputs,
  - negative axis normalization,
  - axis length mismatch,
  - axis out of range,
  - duplicate contracted axis,
  - contracted dimension mismatch,
  - non-tensor operands,
  - malformed axis containers,
  - non-integer axes.
- `realize`:
  - concrete tensor source without destination returns the already-concrete
    tensor,
  - allocate concrete tensor from `map`,
  - allocate concrete tensor from `contract`,
  - realize scalar into destination,
  - realize tensor into destination,
  - realize `map` into destination,
  - realize `contract` into destination,
  - destination shape mismatch,
  - dtype mismatch,
  - safe self-update,
  - unsafe alias handling.
- Memory/lifetime:
  - tensor returned across a function boundary,
  - tensor captured in closure env,
  - tensor realized across scope boundaries,
  - tensor destruction path under normal and ASAN builds,
  - no hidden per-type refcount ownership path.
- Execution parity:
  - interpreter behavior first,
  - JIT behavior after interpreter is green,
  - AOT/compiler e2e rows after primitive/codegen hooks exist.
- Optional backend parity, when those backends land:
  - BLAS rank-2 contraction matches pure fallback,
  - LAPACK library routines document their pure fallback or dependency policy,
  - CUDA/cuBLAS paths require explicit device movement,
  - disabled or missing backend dependencies fail or fall back deterministically.

Validation guidance:

- Run targeted tensor tests after each slice.
- Run `c3c build` for integration safety after code slices.
- Run `c3c build --sanitize=address` for tensor storage/lifetime slices.
- Use bounded container validation for broad or high-memory tensor test slices.
- Keep BLAS/GPU/heavy scientific benchmarks container-bound.

Latest targeted validation for the tensor boundary cleanup audit:

- `c3c build --warn-deprecation=no`
- Host targeted `advanced-collections-module` group: `pass=212 fail=0`
- Bounded container `memory-lifetime-smoke` slice: `pass=222 fail=0`

## Deferred Future Work

- `Matrix` rank-2 convenience constructor or alias, only if owner explicitly
  wants it after `Tensor` is stable.
- `dot` and `outer` as library conveniences over `contract`.
- `trace`, `transpose`, `permute`, `reshape`, `sum`, `mean`, and axis
  reductions.
- Slicing and views with explicit copy/view mutation policy.
- `Complex` dtype.
- fixed-width numeric dtypes.
- BLAS/LAPACK backend modules and kernel selection.
- CUDA/cuBLAS storage/backend modules with explicit host/device movement.
- autodiff over differentiable tensor primitives.

## First User-Facing Example Target

```lisp
(define a
  (Tensor Double [2 3]
    [1.0 2.0 3.0
     4.0 5.0 6.0]))

(define b
  (Tensor Double [3 2]
    [7.0  8.0
     9.0  10.0
     11.0 12.0]))

(define c (Tensor Double [2 2] 0.0))

(realize (contract a b [1 0]) c)

(define shifted (realize (map + c 1.0)))
(define squared (realize (map * shifted shifted)))
```

This is the intended mental model:

- `Tensor` is the object.
- `map` is elementwise/broadcast computation.
- `contract` is tensor algebra.
- `realize` is the expression-to-storage boundary, with optional
  destination reuse.
