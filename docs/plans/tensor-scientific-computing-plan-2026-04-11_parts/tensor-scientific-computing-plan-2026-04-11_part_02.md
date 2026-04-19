# tensor-scientific-computing-plan-2026-04-11 Part 02

Source: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`

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

Use constructor dispatch as the canonical public materialization boundary:
`(Array iterator)`, `(List iterator)`, `(Dictionary iterator)` where pair shape
applies, and `(Tensor iterator)` consume finite iterators. `realize` remains a
low-level Tensor storage primitive for destination reuse and compatibility with
shipped Tensor storage tests.

Low-level storage forms:

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
  (Tensor Float64 [2 3]
    [1.0 2.0 3.0
     4.0 5.0 6.0]))

(dtype x)       ; => 'Float64
(shape x)       ; => [2 3]
(rank x)        ; => 2
(length x)      ; => 6
(ref x [1 2])   ; => 6.0
```

Rationale:

- `Float64` already exists as a value-position type descriptor/coercion
  surface.
- `^Float64` should remain annotation syntax unless a separate type-literal
  expression surface is deliberately added.
- `(ref x [i j])` keeps `ref` as a two-argument operation instead of forcing a
  variadic `ref` decision in the first slice.

Optional constructors after the base constructor is stable:

```lisp
(Tensor Float64 [2 3] 0.0)  ; scalar fill constructor
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
- Initial dtype target is `Float64`.
- `Integer` can follow after the `Float64` path is validated.
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
