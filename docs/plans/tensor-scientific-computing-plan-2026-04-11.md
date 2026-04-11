# Tensor Scientific Computing Plan (2026-04-11)

Status: `proposed`  
Owner: language owner + Codex workflow  
Mode: design plan only, no implementation in this document  
TODO lane: `LANG-TENSOR-SCIENTIFIC-SURFACE-091`

## Purpose

Define Omni's first coherent scientific/ML numeric surface without copying
Julia syntax wholesale and without weakening Omni's existing language
invariants.

The chosen direction is:

- `Tensor` is the canonical rank-polymorphic numeric aggregate.
- `Array` remains the generic boxed collection.
- `map` is extended through dispatch for elementwise tensor operations and
  broadcasting.
- `contract` is the canonical tensor contraction operation.
- `materialize` is the canonical tensor-expression-to-storage boundary.

This plan records the naming decisions, semantic contracts, rollout slices,
test gates, and deferred work before runtime implementation starts.

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

There is also an older exploratory `examples/scicomp_demo.omni` surface using
`vec-*`, `mat-*`, and `mat-mul` style names. Treat that as non-canonical
prototype vocabulary. It must be migrated, quarantined, or deleted when the
new tensor surface lands.

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
  indexing, elementwise expressions, contraction expressions, reductions, and
  explicit materialization into storage.

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
- A first implementation may represent that expression as an eagerly
  materialized `Tensor`; the user-facing contract still treats it as a tensor
  expression.
- Tensor `map` may accept one or more tensor/scalar inputs.
- Scalar arguments broadcast over tensor arguments.
- First implementation should support scalar broadcast and exact-shape
  tensor-tensor inputs.
- NumPy-style trailing singleton expansion is useful, but can be a second
  rollout slice after exact-shape behavior is tested.
- Elementwise multiplication is `(map * a b)`, not `(* a b)`.

### `contract`

Use `contract` as the canonical summed-index tensor algebra operation.

Canonical form:

```lisp
(contract a b [1] [0])
```

Meaning:

- contract axis `1` of `a` with axis `0` of `b`.
- for rank-2 shapes `[m k]` and `[k n]`, result shape is `[m n]`.

Multi-axis form:

```lisp
(contract a b [1 2] [0 3])
```

Rules:

- `contract` produces a tensor-shaped expression.
- axis lists must have equal length.
- each axis must be in range for its tensor.
- dimensions for paired axes must match.
- result shape is all non-contracted axes from the left tensor followed by all
  non-contracted axes from the right tensor, preserving source order.
- no implicit default contraction is provided for general tensors.

### `materialize`

Use `materialize` as the canonical tensor-expression-to-storage boundary.

Canonical forms:

```lisp
(materialize (map + a b))
(materialize (map sqrt x) x)
(materialize (contract a b [1] [0]) c)
(materialize 0.0 x)
(materialize src out)
```

Contract:

- `(materialize expr)` returns a concrete `Tensor`.
- `(materialize expr out)` writes the expression into existing mutable tensor
  `out` and returns `out`.
- A concrete `Tensor` satisfies the tensor-expression protocol.
- If `expr` is already a concrete tensor and no destination is provided,
  `materialize` may return the tensor unchanged.
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
- `copy` / `fill` as peers to `materialize`: too close to each other; they may
  exist later only as convenience wrappers over `materialize`.

Non-goals for the first implementation:

- no Julia dotted operator syntax,
- no implicit numeric widening in method dispatch,
- no automatic `(* tensor tensor)` semantics for general tensors,
- no GPU backend,
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
- `map`, `contract`, and `materialize` have deterministic shape/dtype/aliasing
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
  determine broadcast result shape
  create a tensor expression that records:
    function f
    input expressions/scalars
    result shape
    result dtype policy
  first implementation may eagerly materialize this expression
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
  first implementation may eagerly materialize this expression
  return tensor expression
```

`materialize`:

```text
materialize(source, maybe_dest):
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
  - fusion for `(materialize (map ...) out)` and `(materialize (contract ...) out)`
    only after allocation semantics are correct.

Storage and backend policy:

- MVP uses native contiguous CPU `Double` storage.
- Strides should be present in the metadata early, even if all MVP tensors are
  contiguous. This avoids painting slicing/views into a corner.
- BLAS integration is a later optimization for rank-2 `Double` contractions
  with compatible layout.
- Sparse/GPU backends are deferred storage strategies behind `Tensor`, not
  separate first-surface types.

### Refinement

Rollout slices:

1. `TENSOR-001` Documentation and surface freeze
   - Land this plan and TODO pointer.
   - Add a short note to future language docs only when implementation starts.

2. `TENSOR-010` Runtime representation
   - Add native Tensor payload, destructor, printing, `tensor?`, `dtype`,
     `shape`, `rank`, and `length`.
   - No `map`, `contract`, or `materialize` yet.

3. `TENSOR-020` Constructor and indexing
   - Implement `(Tensor Double shape data-or-scalar)`.
   - Implement `(ref tensor index-array)`.
   - Keep variadic tensor `ref` deferred.

4. `TENSOR-030` Tensor-expression protocol and `materialize`
   - Treat concrete `Tensor` as a tensor expression.
   - Implement `(materialize expr)`.
   - Implement `(materialize expr out)`.
   - Implement scalar-to-destination fill through `(materialize scalar out)`.
   - No lazy expression fusion is required yet.

5. `TENSOR-040` Elementwise `map`
   - Implement unary tensor `map`.
   - Implement tensor-scalar `map`.
   - Implement exact-shape tensor-tensor `map`.
   - The first slice may return eager concrete tensors while preserving the
     tensor-expression contract.
   - Defer singleton-axis broadcast until after exact-shape coverage is green.

6. `TENSOR-050` `contract`
   - Implement pure C3 fallback contraction for `Double`.
   - Support one or more axis pairs.
   - The first slice may return eager concrete tensors while preserving the
     tensor-expression contract.
   - Add rank-2 matrix-product examples through `contract`.

7. `TENSOR-060` Destination materialization and expression fusion
   - Optimize `(materialize (map ...) out)` and
     `(materialize (contract ...) out)` when the expression representation
     exists.
   - Keep ordinary eager-source copy semantics as the fallback.
   - Add aliasing tests and diagnostics.

8. `TENSOR-070` Broadcasting extension
   - Add trailing singleton expansion if the owner approves the exact rule.
   - Keep shape errors deterministic and payloaded.

9. `TENSOR-080` BLAS-backed rank-2 contraction
   - Use FFI/native library path only after the pure fallback behavior is
     stable.
   - Keep fallback available for validation and portability.

10. `TENSOR-090` Example migration
   - Replace or quarantine `examples/scicomp_demo.omni`.
   - Remove `vec-*`, `mat-*`, and `mat-mul` as canonical examples.
   - Add a `Tensor` example that uses `map`, `contract`, and `materialize`.

11. `TENSOR-100` Autodiff design note
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
- Destination `materialize` requires exact dtype compatibility in the first
  slice. Explicit conversion must happen before materialization.
- Singleton-axis broadcasting is deferred to `TENSOR-070`; the first `map`
  slice supports scalar broadcast and exact-shape tensor-tensor inputs.
- `length` for `Tensor` means total element count.

### Completion

Documentation gates:

- `docs/LANGUAGE_SPEC.md` documents:
  - `Tensor`,
  - `dtype`, `shape`, `rank`,
  - tensor `ref`,
  - tensor `map`,
  - `contract`,
  - `materialize`,
  - diagnostic codes.
- `docs/type-system-syntax.md` documents how `Tensor` participates in type
  annotations and dispatch once the implementation supports it.
- `docs/SURFACE_COMPATIBILITY.md` records any scicomp prototype surface
  removal if old names are removed.
- `examples/scicomp_demo.omni` is updated or replaced.
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
  - shape mismatch,
  - function result dtype mismatch.
- `contract`:
  - rank-2 matrix product via `(contract a b [1] [0])`,
  - multi-axis contraction,
  - axis length mismatch,
  - axis out of range,
  - contracted dimension mismatch.
- `materialize`:
  - allocate concrete tensor from concrete tensor source,
  - allocate concrete tensor from `map`,
  - allocate concrete tensor from `contract`,
  - materialize scalar into destination,
  - materialize tensor into destination,
  - materialize `map` into destination,
  - materialize `contract` into destination,
  - destination shape mismatch,
  - dtype mismatch,
  - safe self-update,
  - unsafe alias handling.
- Memory/lifetime:
  - tensor returned across a function boundary,
  - tensor captured in closure env,
  - tensor materialized across scope boundaries,
  - tensor destruction path under normal and ASAN builds,
  - no hidden per-type refcount ownership path.
- Backend parity:
  - interpreter behavior first,
  - JIT behavior after interpreter is green,
  - AOT/compiler e2e rows after primitive/codegen hooks exist.

Validation guidance:

- Run targeted tensor tests after each slice.
- Run `c3c build` for integration safety after code slices.
- Run `c3c build --sanitize=address` for tensor storage/lifetime slices.
- Use bounded container validation for broad or high-memory tensor test slices.
- Keep BLAS/GPU/heavy scientific benchmarks container-bound.

## Deferred Future Work

- `Matrix` rank-2 convenience constructor or alias, only if owner explicitly
  wants it after `Tensor` is stable.
- `dot` and `outer` as library conveniences over `contract`.
- `trace`, `transpose`, `permute`, `reshape`, `sum`, `mean`, and axis
  reductions.
- Slicing and views with explicit copy/view mutation policy.
- `Complex` dtype.
- fixed-width numeric dtypes.
- BLAS/LAPACK acceleration.
- GPU storage backend.
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

(materialize (contract a b [1] [0]) c)

(define shifted (materialize (map + c 1.0)))
(define squared (materialize (map * shifted shifted)))
```

This is the intended mental model:

- `Tensor` is the object.
- `map` is elementwise/broadcast computation.
- `contract` is tensor algebra.
- `materialize` is the expression-to-storage boundary, with optional
  destination reuse.
