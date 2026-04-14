# Scientific Numerics And Tensor Plan

Date: 2026-04-14
Workspace: `/home/christos/Omni`

## Active Direction

This operational note now defers Tensor surface authority to the integrated
repo plan:

- `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`
- `docs/areas/tensor-scientific.md`
- `docs/LANGUAGE_SPEC.md`
- `docs/reference/03-collections.md`

The older local `.agents/PLAN.md` statement that treated `linalg/matmul` as a
chosen public name is invalidated. The recovered and integrated decision is:

- `Tensor` is the rank-polymorphic scientific numeric aggregate.
- `map` is the canonical elementwise tensor operation, including scalar
  broadcast and right-aligned singleton-axis tensor-tensor broadcasting.
- Elementwise multiplication is `(map * a b)`, not `(* a b)`.
- `contract` is the canonical summed-axis tensor contraction operation.
- Rank-2 matrix multiplication maps to `(contract a b [1 0])`; the explicit
  `(contract a b [1] [0])` form remains accepted.
- `realize` is the canonical tensor-expression-to-storage boundary.
- `matmul` is rejected as canonical because it is rank-2 biased and programming
  jargon; do not implement `linalg/matmul` as the normal public surface.
- Do not add public `TensorExpr`, `map-into`, backend-flavored operation names,
  or implicit CPU/GPU transfers as first-surface semantics.

## Scalar Scientific Backend Direction

Default scalar numerics direction from the owner remains:

- Avoid GSL as the baseline scientific stack.
- Prefer Boost.Multiprecision for exact/high-precision scalar values.
- Prefer Boost.Math for scalar scientific functions behind an owned C++ shim
  with a stable C ABI.
- Keep GNU precision libraries only as possible later performance backends if
  the owner explicitly accepts that dependency path.
- Keep user-facing scalar names backend-neutral: `Integer`, `BigInteger`,
  `Double`, `BigFloat`, optional `Complex`, and `BigComplex`.

Minimal scalar-first targets:

- `BigInteger` through Boost.Multiprecision `cpp_int` is now the first landed
  scalar slice: constructor, printing/String conversion, `Number` identity,
  `+`/`-`/`*` overflow promotion, equality/hash support, and scope-boundary
  cloning are implemented. `/`, `%`, ordering comparisons, bitwise operations,
  `gcd`/`lcm`, and `parse-number` arbitrary-precision parsing remain deferred.
- `BigFloat` / `BigComplex` only after the scalar representation, precision
  policy, and lifetime boundaries are stable.
- `math/lgamma` is the first validated Boost.Math wrapper slice: C++ shim
  status codes, finite-input/range/domain error mapping, primitive registration,
  AOT lookup, focused numeric tests, and direct runtime smokes are implemented.
- `math/erf`, `math/erfc`, `stats/normal-cdf`, and `stats/normal-quantile`
  only after the C++ shim/error policy is validated.

## Tensor Backend Direction

OpenBLAS/LAPACK/LAPACKE should integrate as optional backend acceleration, not
as a new public Tensor surface:

- The pure C3 Tensor fallback remains the semantic oracle.
- Ordinary Tensor storage remains C3-native and scoped; do not replace normal
  Tensor storage with `ForeignHandle`.
- BLAS rank-2 `Double` GEMM now optimizes all contiguous row-major single-axis
  rank-2 contract layouts behind `realize` when dtype, rank, layout, device,
  and aliasing match the backend contract: `[1 0]`, `[0 0]`, `[1 1]`, and
  `[0 1]`.
- Unsupported strides, dtypes, aliasing, device placement, or missing libraries
  must fall back or fail deterministically without changing Tensor semantics.
- Direct native backend calls are preferred for hot Tensor kernels. User-facing
  FFI handles are not part of the normal Tensor object model.
- Backend modules should sit behind names such as `tensor/blas` and
  `tensor/lapack`; only genuinely opaque backend resources need explicit
  ownership/finalizer policy.
- Solver/decomposition convenience names still need a public qualifier: do not
  leave `solve` bare, and do not lock `linalg/` as the base namespace yet.
  `dot` / `outer` helpers may be considered later as library conveniences over
  `contract`, but they should not replace the locked first Tensor surface.

## Current Next Checkpoint

Use the integrated Tensor plan as source of truth before changing code. The
next scientific work should pick one narrow slice:

1. Continue `TENSOR-090` beyond the landed transpose-capable rank-2 `dgemm`
   path: decide LAPACK/LAPACKE solver/decomposition naming. Bare `solve` is
   rejected; `linalg/` is not yet accepted as the qualifier.
2. Continue the scalar Boost.Multiprecision lane only by choosing one explicit
   follow-up: `BigFloat`/`BigComplex`, BigInteger division/modulo/comparisons,
   or `parse-number` arbitrary-precision parsing.
3. Extend Boost.Math by reusing the validated `math/lgamma` shim/error-policy
   pattern: `math/erf`, `math/erfc`, `stats/normal-cdf`, or
   `stats/normal-quantile`.

Do not start by binding GSL. Do not implement `linalg/matmul` as canonical.
