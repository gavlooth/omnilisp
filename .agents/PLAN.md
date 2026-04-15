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
  `+`/`-`/`*` overflow promotion, `/`, `%`, ordering comparisons,
  `abs`, `min`, `max`, `gcd`, `lcm`, bitwise operations, equality/hash
  support, `parse-number` decimal overflow promotion, and scope-boundary
  cloning are implemented.
- `BigFloat` through Boost.Multiprecision `cpp_dec_float_50` is now the first
  high-precision decimal slice: constructor, printing/String conversion,
  `Number` identity, `Double`/`Integer` narrowing, `+`, `-`, `*`, `/`,
  comparisons, `abs`, `min`, `max`, equality/hash support, scope-boundary
  cloning, `parse-number` floating overflow promotion, BigFloat-preserving
  scalar math for trig, inverse trig, hyperbolic, exponential/logarithmic,
  power/root, gamma/error-function, standard-normal helpers, and exact
  BigFloat `floor`/`ceiling`/`round`/`truncate` to `Integer` or `BigInteger`
  are implemented. Precision-control APIs remain a separate follow-up.
- `BigComplex` is now the first high-precision complex scalar slice:
  constructor, printing/String conversion, `Number` identity, `+`, `-`, `*`,
  `/`, unary `-`, equality/hash support, `zero?`, `abs` to `BigFloat`,
  scope-boundary cloning/promotion, fail-closed ordered operations, and
  BigComplex-preserving `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`,
  `cosh`, `tanh`, `exp`, `log`, `log10`, `sqrt`, and `pow` are implemented.
  `real-part`, `imag-part`, and `conjugate` are implemented as numeric
  primitives. Broader special functions and distributions remain separate
  follow-ups.
- `math/lgamma`, `math/erf`, `math/erfc`, `stats/normal-cdf`, and
  `stats/normal-quantile` are now validated Boost.Math wrapper slices: C++ shim
  status codes, finite-input/range/domain error mapping, primitive
  registration, AOT lookup, focused numeric tests, and direct runtime smokes
  are implemented.
- `stats/normal-cdf` and `stats/normal-quantile` are intentionally
  one-argument standard-normal helpers. Mean/stddev parameters or other
  distribution families need separate surface decisions.

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
- BLAS `Double` GEMV now optimizes contiguous row-major rank-2/rank-1 and
  rank-1/rank-2 single-axis contract layouts behind the same backend boundary.
- BLAS `Double` DDOT now optimizes contiguous rank-1/rank-1 vector dot
  contractions behind the same backend boundary.
- BLAS `Double` DGER now optimizes contiguous rank-1/rank-1 outer-product
  contractions behind the same backend boundary.
- `(Array tensor)` and `(List tensor)` are implemented as explicit Tensor
  collection conversions that realize lazy Tensor expressions and return flat
  row-major values.
- `(Tensor data)`, `(Tensor data Double)`, and `(Tensor Double data)` now infer
  native `Double` tensor shape from real numeric scalars or rectangular nested
  arrays/proper lists whose leaves can narrow to finite `Double`, while
  preserving the explicit `(Tensor Double shape data-or-scalar)` constructor.
- Native `BigInteger` Tensor storage now supports constructor, `dtype`, `ref`,
  flat `(Array tensor)` / `(List tensor)` conversion, concrete `realize`,
  tensor-dispatched `map`, and pure C3 `contract` kernels. BigInteger Tensor
  data must be exact integers.
- Native `BigFloat` concrete Tensor storage now supports constructor, `dtype`,
  `ref`, flat `(Array tensor)` / `(List tensor)` conversion, and concrete
  `realize`.
- Tensor-dispatched `map` now supports native `BigFloat` tensors for unary,
  tensor-scalar, scalar-tensor, exact-shape tensor-tensor, and right-aligned
  singleton-axis broadcast cases.
- Tensor-dispatched `contract` now supports native `BigFloat` tensors through
  the pure C3 contraction fallback. BLAS fast paths remain `Double`-only.
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

1. Continue `TENSOR-090` beyond the landed `dgemm` and `dgemv` paths: decide
   LAPACK/LAPACKE solver/decomposition naming. Bare `solve` is rejected;
   `linalg/` is not yet accepted as the qualifier.
2. Continue the scalar precision lane with explicit BigFloat precision-control
   policy or BigComplex special-function/distribution policy.
3. Continue precision Tensor work with native `BigComplex` storage/kernels only
   when complex scientific tensor workflows become the active priority.
4. Extend Boost.Math only when there is a concrete next scientific function or
   distribution family. The minimal planned scalar wrappers are now complete.

Do not start by binding GSL. Do not implement `linalg/matmul` as canonical.
