# tensor-scientific-computing-plan-2026-04-11 Part 04

Source: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`

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
  `(Tensor Float64 shape data-or-scalar)` are the constructor surfaces,
  including Tensor-specific `map`, `contract`, and `realize` behavior.
- `docs/SURFACE_COMPATIBILITY.md` records any scicomp prototype surface
  removal if old names are removed.
- `examples/scicomp_demo.omni` is updated to the canonical `Tensor`, `map`,
  `contract`, and `realize` surface.
- `memory/CHANGELOG.md` records behavior only after code lands and validation
  has concrete evidence.

Test gates:

- Constructor:
  - valid `Float64` tensor construction,
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
  (Tensor Float64 [2 3]
    [1.0 2.0 3.0
     4.0 5.0 6.0]))

(define b
  (Tensor Float64 [3 2]
    [7.0  8.0
     9.0  10.0
     11.0 12.0]))

(define c (Tensor Float64 [2 2] 0.0))

(realize (contract a b [1 0]) c)

(define shifted (Tensor (map + c 1.0)))
(define squared (Tensor (map * shifted shifted)))
(define from-iterator (Tensor (map (lambda (x) (+ x 1)) (Iterator [1 2 3]))))
```

This is the intended mental model:

- `Tensor` is the object.
- `map` is elementwise/broadcast computation.
- `contract` is tensor algebra.
- Constructors are the public materialization boundary.
- `realize` is a low-level destination-storage operation with optional
  destination reuse, not a general force/delay counterpart.
