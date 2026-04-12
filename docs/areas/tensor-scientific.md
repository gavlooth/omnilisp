# Tensor Scientific Area

## Canonical Source Of Truth

- Current implementation truth: `memory/CHANGELOG.md`
- Normative language surface: `docs/LANGUAGE_SPEC.md`
- User reference: `docs/reference/03-collections.md`
- Active plan: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`

## Current Status

Status: `yellow`
As of: 2026-04-12

The canonical user-facing surface is:

- `Tensor` for rank-polymorphic scientific numeric values,
- tensor-dispatched `map` for elementwise work,
- `contract` for summed-axis tensor contraction,
- `materialize` for the tensor-expression-to-storage boundary.

Implemented slices:

- `TENSOR-010`: native Tensor runtime payload, descriptor, print surface,
  lifetime copy/promotion paths, and introspection primitives.
- `TENSOR-020`: `(Tensor Double shape data-or-scalar)` and
  `(ref tensor index-array)`.
- `TENSOR-030`: concrete tensor materialization and destination copy/fill.
- `TENSOR-040`: tensor-dispatched elementwise `map` for unary, tensor-scalar,
  scalar-tensor, and exact-shape tensor-tensor `Double` cases.
- `TENSOR-050`: pure `Double` `contract`.
- `TENSOR-060A`: destination-fusion audit.
- `TENSOR-060B`: lazy Tensor expression payloads under the existing `Tensor`
  value and destination materialization into exact-shape/dtype tensors.
- `TENSOR-060C`: platform-safe shape dimension parsing and zero-size
  contracted-axis identity materialization.
- `TENSOR-060D`: materialize edge coverage for rank-0 concrete tensors,
  zero-size destination/materialization paths, aliased elementwise `map`
  destination materialization, and duplicate-axis checks after negative-axis
  normalization.
- `TENSOR-060E`: tensor boundary rollback cleanup for already-copied/promoted
  expression child values, plus generic materialized-value cleanup through
  tensor expression edges.
- `TENSOR-060F`: tensor boundary/materialize fail-closed hardening for
  non-unique destination retry promotion and malformed concrete source storage.
- `TENSOR-060G`: lazy Tensor `ref` cancels and destroys the temporary concrete
  materialization after extracting the scalar result.
- `TENSOR-060H`: nested lazy Tensor `materialize` cleans temporary child
  tensors created while resolving nested `map`/`contract` expression operands.
- `TENSOR-060I`: failed lazy Tensor materialization cleans the fresh concrete
  result tensor through the generic fresh-value path, with nested `contract`
  materialization coverage.
- `TENSOR-060J`: lazy Tensor `materialize` into an explicit destination stages
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
  fallback and explicit `ForeignHandle` backend resource ownership.
- `TENSOR-110`: cleanup surface closure; `examples/scicomp_demo.omni` uses
  canonical `Tensor`, `map`, `contract`, and `materialize`, with lazy
  expression-return and closure-capture coverage added.

Deferred by design:

- Exact-shape tensor-tensor `map` remains the first matching/fallback path.
- No backend-specific tensor types participate in core `map` dispatch; backend
  behavior is resolved at materialization and execution layers.
- BLAS/LAPACK/CUDA/cuBLAS acceleration is optional backend work behind the
  pure `Tensor` fallback.

## Next Steps

1. Keep the pure C3 fallback as the validation oracle.
2. Use `TENSOR-090` for the first BLAS rank-2 contraction optimization code
   slice once the contract is frozen.
3. Use `TENSOR-100` for the explicit-device CUDA/cuBLAS design slice after the
   BLAS contract is settled.
4. Gate native library/context resources through explicit `ForeignHandle`
   ownership and finalizer policy.
5. Do not add a public `TensorExpr`, `matmul`, backend-specific tensor type, or
   implicit CPU/GPU transfer surface in the first backend slice.

## Validation

Recent targeted validation recorded in `memory/CHANGELOG.md`:

- `c3c build --warn-deprecation=no`
- Latest transactional destination materialization validation:
  - host targeted `advanced-collections-module` group: `pass=212 fail=0`
- Latest lazy Tensor edge/root-promotion validation:
  - Docker-bounded `memory-lifetime-smoke` slice: `pass=222 fail=0` after
    adding lazy closure-env promotion coverage for closure-captured Tensor
    callables, copy-to-parent detached env-scope parent chains, and
    commit-scoped graph-audit releasing-child/global-env coverage.
  - Docker-bounded ASAN `memory-lifetime-smoke` slice: `pass=222 fail=0`
- `git diff --check`
