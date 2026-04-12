# Tensor Scientific Area

## Canonical Source Of Truth

- Current implementation truth: `memory/CHANGELOG.md`
- Normative language surface: `docs/LANGUAGE_SPEC.md`
- User reference: `docs/reference/03-collections.md`
- Active plan: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`

## Current Status

Status: `yellow`
As of: 2026-04-14

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
- `TENSOR-110`: cleanup surface closure; `examples/scicomp_demo.omni` uses
  canonical `Tensor`, `map`, `contract`, and `realize`, with lazy
  expression-return and closure-capture coverage added.

Deferred by design:

- Exact-shape tensor-tensor `map` remains the first matching/fallback path.
- No backend-specific tensor types participate in core `map` dispatch; backend
  behavior is resolved at realization and execution layers.
- BLAS/LAPACK/CUDA/cuBLAS acceleration is optional backend work behind the
  pure `Tensor` fallback; the first BLAS `dgemm` slices are implemented for
  dense rank-2 single-axis `Double` contractions only.

## Next Steps

1. Keep the pure C3 fallback as the validation oracle.
2. Continue `TENSOR-090` with LAPACK/LAPACKE solver/decomposition surface
   decisions. Do not expose a bare `solve`; `linalg/` is not yet locked as the
   base namespace. Additional private BLAS eligibility such as rank-2/rank-1
   `gemv` should keep the pure fallback as the validation oracle.
3. Use `TENSOR-100` for the explicit-device CUDA/cuBLAS design slice after the
   BLAS contract is settled.
4. Keep ordinary Tensor storage native/scoped; gate only genuinely opaque
   backend resources through explicit ownership/finalizer policy.
5. Do not add a public `TensorExpr`, `matmul`, backend-specific tensor type, or
   implicit CPU/GPU transfer surface in the first backend slice.

## Validation

Recent targeted validation recorded in `memory/CHANGELOG.md`:

- `c3c build --warn-deprecation=no`
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
