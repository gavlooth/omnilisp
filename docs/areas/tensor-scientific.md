# Tensor Scientific Area

## Canonical Source Of Truth

- Current implementation truth: `memory/CHANGELOG.md`
- Normative language surface: `docs/LANGUAGE_SPEC.md`
- User reference: `docs/reference/03-collections.md`
- Active plan: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`

## Current Status

Status: `yellow`
As of: 2026-04-15

The canonical user-facing surface is:

- `Tensor` for rank-polymorphic scientific numeric values,
- tensor-dispatched `map` for elementwise work,
- `contract` for summed-axis tensor contraction, with paired-axis arrays
  such as `[1 0]` canonical for the common rank-2 case,
- `realize` for the tensor-expression-to-storage boundary.

Implemented slices:

- `TENSOR-010`: native Tensor runtime payload, descriptor, print surface,
  lifetime copy/promotion paths, and introspection primitives.
- `TENSOR-020`: `(Tensor Double shape data-or-scalar)` and
  `(ref tensor index-array)`.
- `TENSOR-030`: concrete tensor realization and destination copy/fill.
- `TENSOR-040`: tensor-dispatched elementwise `map` for unary, tensor-scalar,
  scalar-tensor, and exact-shape tensor-tensor `Double` cases.
- `TENSOR-050`: pure `Double` `contract`, including paired-axis array
  shorthand and explicit left/right axis-list inputs.
- `TENSOR-060A`: destination-fusion audit.
- `TENSOR-060B`: lazy Tensor expression payloads under the existing `Tensor`
  value and destination realization into exact-shape/dtype tensors.
- `TENSOR-060C`: platform-safe shape dimension parsing and zero-size
  contracted-axis identity realization.
- `TENSOR-060D`: realize edge coverage for rank-0 concrete tensors,
  zero-size destination/realization paths, aliased elementwise `map`
  destination realization, and duplicate-axis checks after negative-axis
  normalization.
- `TENSOR-060E`: tensor boundary rollback cleanup for already-copied/promoted
  expression child values, plus generic realized-value cleanup through
  tensor expression edges.
- `TENSOR-060F`: tensor boundary/realize fail-closed hardening for
  non-unique destination retry promotion and malformed concrete source storage.
- `TENSOR-060G`: lazy Tensor `ref` cancels and destroys the temporary concrete
  realization after extracting the scalar result.
- `TENSOR-060H`: nested lazy Tensor `realize` cleans temporary child
  tensors created while resolving nested `map`/`contract` expression operands.
- `TENSOR-060I`: failed lazy Tensor realization cleans the fresh concrete
  result tensor through the generic fresh-value path, with nested `contract`
  realization coverage.
- `TENSOR-060J`: lazy Tensor `realize` into an explicit destination stages
  expression evaluation into a temporary Tensor and commits only after success,
  preserving recursive contract alias rejection while allowing zero-size
  non-aliasing destinations and cleaning failed constructor results.
- `TENSOR-060K`: lazy Tensor expression edge rollback has deterministic
  fail-counter coverage, and root promotion of lazy Tensor expression operands
  keeps tensor edges out of the released child TEMP lane.
- `TENSOR-070`: tensor-tensor `map` supports right-aligned singleton-axis
  broadcasting, including rank-0 tensor scalar broadcasting and deterministic
  `tensor/shape-mismatch` failures for incompatible axes.
- `TENSOR-075`: `(Array tensor)` and `(List tensor)` now realize Tensor
  expressions and expose flat row-major element values through the canonical
  collection constructor/conversion surfaces.
- `TENSOR-076`: `(Tensor data)`, `(Tensor data Double)`, and
  `(Tensor Double data)` infer `Double` tensor shape from real numeric scalars
  or rectangular nested arrays/proper lists whose leaves can narrow to finite
  `Double`.
- `TENSOR-077`: native `BigFloat` concrete Tensor storage supports
  constructor, `dtype`, `ref`, flat `(Array tensor)` / `(List tensor)`
  conversion, and concrete `realize`.
- `TENSOR-078`: tensor-dispatched `map` now supports native `BigFloat`
  tensors for unary, tensor-scalar, scalar-tensor, exact-shape tensor-tensor,
  and right-aligned singleton-axis broadcast cases. BigFloat lazy map payloads
  clone scalar handles across function-return/closure-capture boundaries.
- `TENSOR-079`: tensor-dispatched `contract` now supports native `BigFloat`
  tensors through the pure C3 contraction fallback, including vector dot,
  rank-2 matrix product, zero-size contracted-axis identity, explicit
  destination realization, and lazy expression boundary survival. BLAS fast
  paths remain `Double`-only.
- `TENSOR-081`: native `BigInteger` Tensor storage and kernels support
  constructor, `dtype`, `ref`, flat collection conversion, concrete
  `realize`, tensor-dispatched `map`, and pure C3 `contract` for exact
  integer tensor work. BigInteger Tensor data must be exact integers; inexact
  floating data fails closed.
- `TENSOR-082`: native `BigComplex` Tensor storage and kernels support
  constructor, `dtype`, `ref`, flat collection conversion, concrete
  `realize`, tensor-dispatched `map`, and pure C3 `contract` for complex
  tensor work. Real numeric leaves promote to zero-imaginary BigComplex
  elements; BLAS fast paths remain `Double`-only.
- `TENSOR-083`: BigComplex Tensor component kernels support elementwise
  `real-part`, `imag-part`, and `conjugate`. Component extraction realizes
  lazy BigComplex Tensor sources and returns native BigFloat tensors; conjugate
  returns native BigComplex tensors.
- `TENSOR-084`: real Tensor component semantics are dtype-preserving:
  `real-part` and `conjugate` copy `Double`, `BigInteger`, and `BigFloat`
  tensors, while `imag-part` returns same-shape zero tensors in the same dtype.
- `TENSOR-085`: Tensor `abs` supports elementwise magnitude for all native
  tensor dtypes. Real Tensor dtypes preserve dtype and shape; `BigComplex`
  Tensor magnitudes return same-shape native `BigFloat` tensors.
- `TENSOR-086`: Tensor `sqrt` supports elementwise square root for all native
  tensor dtypes. `Double` and `BigInteger` Tensor inputs return `Double`
  tensors; `BigFloat` and `BigComplex` Tensor inputs preserve dtype.
- `TENSOR-087`: Tensor unary scientific math supports `sin`, `cos`, `tan`,
  `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, and `log10`
  through a shared elementwise helper. `Double` and `BigInteger` Tensor inputs
  return `Double` tensors; `BigFloat` and `BigComplex` Tensor inputs preserve
  dtype.
- `TENSOR-088`: Tensor `pow` supports tensor-scalar, scalar-tensor, and
  broadcast tensor-tensor powers. `BigComplex` wins the result dtype if either
  input is complex, `BigFloat` wins if either input is BigFloat, and remaining
  real/exact inputs return `Double` tensors.
- `TENSOR-080`: optional backend boundary contract is closed as a design-only
  slice; BLAS/LAPACK/CUDA/cuBLAS work stays optional behind the pure `Tensor`
  fallback. Ordinary Tensor storage remains native/scoped; truly opaque
  foreign backend resources still require explicit ownership/finalizer policy.
- `TENSOR-090A`: dense rank-2 `Double` contraction equivalent to
  `(contract a b [1 0])` now has an optional native BLAS `dgemm` fast path
  behind the Tensor evaluator, with unsupported cases falling back to the pure
  C3 contraction kernel.
- `TENSOR-090B`: the optional native BLAS `dgemm` fast path now covers every
  contiguous row-major rank-2 single-axis contract layout by passing transpose
  flags for `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`.
- `TENSOR-090C`: the optional native BLAS `dgemv` fast path now covers
  contiguous row-major rank-2/rank-1 and rank-1/rank-2 single-axis `Double`
  contracts, including transposed matrix-vector and vector-matrix layouts.
- `TENSOR-090D`: the optional native BLAS `ddot` fast path now covers
  contiguous rank-1/rank-1 single-axis `Double` vector dot contractions.
- `TENSOR-090E`: the optional native BLAS `dger` fast path now covers
  contiguous rank-1/rank-1 zero-axis `Double` outer-product contractions.
- `TENSOR-110`: cleanup surface closure; `examples/scicomp_demo.omni` uses
  canonical `Tensor`, `map`, `contract`, and `realize`, with lazy
  expression-return and closure-capture coverage added.

Deferred by design:

- Exact-shape tensor-tensor `map` remains the first matching/fallback path.
- No backend-specific tensor types participate in core `map` dispatch; backend
  behavior is resolved at realization and execution layers.
- BLAS/LAPACK/CUDA/cuBLAS acceleration is optional backend work behind the
  pure `Tensor` fallback; the current BLAS slices cover dense rank-2/rank-2
  `dgemm`, rank-2/rank-1 and rank-1/rank-2 `dgemv`, rank-1/rank-1 `ddot`,
  and rank-1/rank-1 zero-axis `dger` `Double` contractions.

## Next Steps

1. Keep the pure C3 fallback as the validation oracle.
2. Continue `TENSOR-090` with LAPACK/LAPACKE solver/decomposition surface
   decisions. Do not expose a bare `solve`; `linalg/` is not yet locked as the
   base namespace. Any additional private BLAS eligibility should keep the pure
   fallback as the validation oracle.
3. Continue precision Tensor work only for a concrete next precision contract,
   such as configurable precision policy, mixed precision promotion rules, or
   complex-specific scientific kernels.
4. Use `TENSOR-100` for the explicit-device CUDA/cuBLAS design slice after the
   BLAS contract is settled.
5. Keep ordinary Tensor storage native/scoped; gate only genuinely opaque
   backend resources through explicit ownership/finalizer policy.
6. Do not add a public `TensorExpr`, `matmul`, backend-specific tensor type, or
   implicit CPU/GPU transfer surface in the first backend slice.

## Validation

Recent targeted validation recorded in `memory/CHANGELOG.md`:

- `c3c build --warn-deprecation=no`
- Latest precision Tensor validation:
  - host `c3c build main --output-dir build --build-dir build/obj2`: passed
    with existing deprecation warnings.
  - host `c3c build`: passed with existing deprecation warnings.
  - host targeted `advanced-collections-module` group: `pass=321 fail=0`.
  - host targeted `advanced-collections-module` group after BigComplex
    component kernels: `pass=327 fail=0`.
  - host targeted `advanced-collections-module` group after real Tensor
    component semantics: `pass=330 fail=0`.
  - host targeted `advanced-collections-module` group after Tensor `abs`
    semantics: `pass=335 fail=0`.
  - host targeted `advanced-collections-module` group after Tensor `sqrt`
    semantics: `pass=341 fail=0`.
  - host targeted `advanced-collections-module` group after Tensor unary
    scientific math semantics: `pass=353 fail=0`.
  - host targeted `advanced-collections-module` group after Tensor `pow`
    semantics: `pass=361 fail=0`.
  - bounded container `advanced-collections-module` group after real Tensor
    component semantics: `pass=330 fail=0`.
  - bounded container `advanced-collections-module` group after Tensor `abs`
    semantics: `pass=335 fail=0`.
  - bounded container `advanced-collections-module` group after Tensor `sqrt`
    semantics: `pass=341 fail=0`.
  - bounded container `advanced-collections-module` group after Tensor unary
    scientific math semantics: `pass=353 fail=0`.
  - bounded container `advanced-collections-module` group after Tensor `pow`
    semantics: `pass=361 fail=0`.
  - bounded container `memory-lifetime-smoke`: `pass=225 fail=0`.
  - bounded container `advanced-collections-module` group after BigComplex
    component kernels: `pass=327 fail=0`.
  - bounded container `memory-lifetime-smoke`: `pass=225 fail=0`.
- Latest BLAS-backed contract validation:
  - host `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - host targeted `advanced-collections-module` group: `pass=221 fail=0`.
  - direct `(ref (realize (contract a b [1 0])) [1 1])`: returned `154.0`.
  - direct transpose-backed rank-2 contract smokes returned `84.0` for
    `[0 0]`, `68.0` for `[1 1]`, and `123.0` for `[0 1]`.
- Latest transactional destination realization validation:
  - host targeted `advanced-collections-module` group: `pass=212 fail=0`
- Latest lazy Tensor edge/root-promotion validation:
  - Docker-bounded `memory-lifetime-smoke` slice: `pass=222 fail=0` after
    adding lazy closure-env promotion coverage for closure-captured Tensor
    callables, copy-to-parent detached env-scope parent chains, and
    commit-scoped graph-audit releasing-child/global-env coverage.
  - Docker-bounded ASAN `memory-lifetime-smoke` slice: `pass=222 fail=0`
- `git diff --check`
