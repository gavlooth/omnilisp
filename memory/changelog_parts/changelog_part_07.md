# Memory Changelog Index Part 07

Source: `memory/CHANGELOG.md`

    - direct `matrix/qr` Q column smoke -> `1.0`
    - direct rank-deficient QR handler smoke -> `tensor/singular-matrix`
    - focused advanced collections/module group on host
      -> `426 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090I` first Matrix decomposition consumer:
  - Added public `matrix/determinant` for square rank-2 `Float64` Tensor
    inputs.
  - The implementation realizes lazy inputs, reuses the pure C3 partial-pivot
    LU factorization kernel, returns a `Float64` scalar determinant, returns
    `0.0` for singular matrices, and uses the empty square convention `1.0`.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - The canonical public name is `matrix/determinant`; no abbreviated
    `matrix/det` alias was added.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, plan index, matrix surface decision note, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - direct `matrix/determinant` smoke -> `-2.0`
    - direct singular determinant smoke -> `0.0`
    - direct empty-square determinant smoke -> `1.0`
    - focused advanced collections/module group on host
      -> `417 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090H` first Matrix decomposition surface:
  - Added public `matrix/lu` for square rank-2 `Float64` Tensor inputs.
  - The implementation realizes lazy inputs, computes pure C3 LU
    factorization with partial pivoting, and raises `tensor/singular-matrix`
    for singular inputs.
  - The result is a dictionary with:
    - `lu`: combined factors, with L below the diagonal and implicit unit
      diagonal, and U on/above the diagonal.
    - `pivots`: final 0-based row order after partial pivoting.
    - `swap-count`: row-swap count for permutation parity.
  - Registered the primitive in interpreter and AOT primitive lookup tables.
  - Updated the language spec, collection reference, Tensor scientific plan,
    Tensor area page, plan index, matrix surface decision note, and agent
    handoff plan.
  - validation:
    - `c3c build --obj-out obj`
    - direct `matrix/lu` factor smoke -> `0.5`
    - direct `matrix/lu` pivot smoke -> `1`
    - direct singular LU handler smoke -> `tensor/singular-matrix`
    - focused advanced collections/module group on host
      -> `410 passed, 0 failed`
    - `scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090G` optional LAPACK `dgesv` matrix solver acceleration:
  - Added private runtime-loaded `LAPACKE_dgesv` discovery through
    `csrc/tensor_lapack_helpers.c`, with availability and call-count probes for
    focused validation.
  - Routed `matrix/solve` through the LAPACK helper when `liblapacke` is
    available and the rank/size contract fits the LAPACKE integer ABI.
  - Missing LAPACKE, oversized LAPACK inputs, unsupported layouts, or missing
    symbols keep the pure runtime solver fallback.
  - LAPACK singular results still raise `tensor/singular-matrix`; backend OOM
    and invalid backend results fail closed instead of suppressing a broken
    contract.
  - The public surface remains `matrix/solve`; no backend-flavored public
    solver name was added.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - direct `matrix/solve` vector RHS smoke -> `0.2`
    - direct singular solve handler smoke -> `tensor/singular-matrix`
    - focused advanced collections/module group on host
      -> `401 passed, 0 failed`
    - local `ldconfig -p` showed `liblapack.so.3` but no `liblapacke`, so the
      focused LAPACK path regression validated fallback retention on this host.

- Completed `TENSOR-090F` first Matrix solver surface:
  - Locked the first rank-2 Tensor solver namespace as `matrix/`, with
    `matrix/solve` as the public operation.
  - Rejected bare `solve`, `linalg/solve`, `tensor/solve`, and
    backend-flavored `tensor/lapack/solve` as first-surface names.
  - Added a pure runtime Float64 Gaussian-elimination solver for square
    rank-2 coefficient tensors and rank-1/rank-2 RHS tensors.
  - Lazy Tensor operands are realized before solving; the result preserves RHS
    rank, and singular systems raise `tensor/singular-matrix`.
  - LAPACK/LAPACKE remains an optional backend acceleration path behind
    `matrix/solve`; the pure solver is the semantic oracle.
  - Added `docs/plans/matrix-solver-surface-decision-2026-04-16.md` to record
    the naming decision and future `matrix/lu`, `matrix/qr`,
    `matrix/cholesky`, `matrix/svd`, `matrix/eigenvalues`, and
    `matrix/eigenvectors` direction.
  - validation:
    - `c3c build --obj-out obj`
    - direct `matrix/solve` vector RHS smoke -> `0.6`
    - direct `matrix/solve` matrix RHS smoke -> `1.0`
    - direct singular solve handler smoke -> `tensor/singular-matrix`
    - focused advanced collections/module group on host
      -> `399 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`

- Canonicalized the public binary64 numeric surface from `Double` to `Float64`:
  - `Float64` is now the callable constructor/type spelling and tensor dtype
    spelling for the existing native binary64 storage.
  - `(Float x)` defaults to `Float64`; `(Float x 64)` is the explicit binary64
    precision form.
  - Follow-up: string precision spellings are also accepted:
    `(Float x "64")` produces `Float64`, while `(Float x "32")` fails closed
    with the same Float32-not-implemented error as integer precision `32`.
  - `Float32` and `(Float x 32)` are registered but fail closed until native
    Float32 runtime/tensor storage exists.
  - The old public `(Double ...)` constructor is not kept as an alias; direct
    use now errors as an unbound variable.
  - The public predicate is now `float64?`, backed by `is?` over `'Float64`.
  - Internal C3 names such as `DOUBLE`, `sym_Double`, `is_double`, and
    `TENSOR_DTYPE_DOUBLE` remain implementation/storage names for the current
    binary64 representation.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for `(Float 1)`, `(Float "1.25" 64)`, `(Float32 1)`, and
      removed `(Double 1)`
    - focused advanced groups:
      - `advanced-core-semantics` -> `71 passed, 0 failed`
      - `advanced-type-dispatch-mutation-chain` -> `240 passed, 0 failed`
      - `advanced-ffi-system` -> `75 passed, 0 failed`
      - `advanced-stdlib-numeric` -> `411 passed, 0 failed`
      - `advanced-collections-module` -> `392 passed, 0 failed`
    - compiler slice -> `276 passed, 0 failed`

## 2026-04-15

- Completed `TENSOR-092` Tensor `gcd`/`lcm` semantics:
  - Extended `gcd` and `lcm` to accept native Tensor inputs.
  - Supports tensor-scalar, scalar-tensor, and broadcast tensor-tensor exact
    integer inputs.
  - Tensor operands must use native `BigInteger` Tensor storage; `Double`,
    `BigFloat`, and `BigComplex` Tensor operands fail closed.
  - Results always use native `BigInteger` Tensor storage, matching the exact
    integer scalar contract.
  - Lazy BigInteger Tensor operands are realized before evaluation.
  - The implementation uses a raw BigInteger Tensor kernel: Tensor element
    handles are borrowed from Tensor storage, `Integer` scalars route through
    the existing `i64` BigInteger C ABI helpers, and `BigInteger` scalars use
    their existing scoped handles. A manufactured scalar-handle variant was
    invalidated because tensor-scalar `gcd`/`lcm` returned corrupted values
    even after function-scope cleanup.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - `c3c build`
    - direct Tensor `gcd`/`lcm` smokes for tensor-scalar, tensor-tensor
      broadcast, and Double Tensor rejection
    - focused advanced collections/module group on host
      -> `392 passed, 0 failed`
    - bounded container advanced collections/module group
      -> `392 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - ASAN build attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed immediately with the local C3 compiler platform support message.

- Completed `TENSOR-091` Tensor `min`/`max` semantics:
  - Extended `min` and `max` to accept native Tensor inputs.
  - Supports tensor-scalar, scalar-tensor, and broadcast tensor-tensor
    comparison for real numeric inputs.
  - Result dtype policy is `BigFloat` if either input is BigFloat, `Double` if
    either input is Double, otherwise `BigInteger`.
  - `BigInteger` Tensor inputs preserve exact integer results, including
    Integer scalar comparisons that now normalize into BigInteger Tensor
    storage.
  - Lazy Tensor operands are realized before comparison.
  - `BigComplex` Tensor inputs fail closed.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - focused advanced collections/module group on host
      -> `387 passed, 0 failed`
    - bounded container advanced collections/module group
      -> `387 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN build attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed immediately with the local C3 compiler platform support message.

- Completed `TENSOR-090` Tensor rounding semantics:
  - Extended `floor`, `ceiling`, `round`, and `truncate` to accept native
    Tensor inputs.
  - Real Tensor inputs return same-shape native `BigInteger` Tensor results,
    matching the scalar contract that rounding returns exact integers.
  - `Double` Tensor inputs round through the C math operation and fail closed
    when the rounded result cannot narrow to Omni `Integer` before storing in
    the BigInteger Tensor.
  - `BigInteger` Tensor inputs clone exact integer values.
  - `BigFloat` Tensor inputs use the existing exact BigFloat rounding path and
    preserve large integer results in BigInteger Tensor storage.
  - `BigComplex` Tensor inputs fail closed.
  - Lazy Tensor operands are realized before elementwise rounding.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - `c3c build`
    - focused advanced collections/module group on host
      -> `379 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `379 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-089` Tensor `atan2` semantics:
  - Extended the existing `atan2` primitive to accept native Tensor inputs.
  - Supports tensor-scalar, scalar-tensor, and broadcast tensor-tensor
    two-argument arctangent.
  - Matches scalar `atan2` policy: complex operands fail closed.
  - `BigFloat` Tensor inputs preserve precision dtype; other real/exact inputs
    return `Double` tensors through the hardened fail-closed finite conversion
    path.
  - Lazy Tensor operands are realized before elementwise `atan2` evaluation.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - `c3c build`
    - focused advanced collections/module group on host
      -> `369 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `369 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-088` Tensor `pow` semantics:
  - Extended the existing `pow` primitive to accept native Tensor inputs.
  - Supports tensor-scalar, scalar-tensor, and broadcast tensor-tensor powers.
  - Result dtype policy is `BigComplex` if either input is complex,
    `BigFloat` if either input is BigFloat, otherwise `Double`.
  - `Double` and `BigInteger` Tensor inputs therefore return same-shape or
    broadcast-shape `Double` tensors through the hardened fail-closed finite
    conversion path.
  - `BigFloat` and `BigComplex` Tensor inputs preserve precision dtype.
  - Lazy Tensor operands are realized before elementwise power evaluation.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - `c3c build`
    - focused advanced collections/module group on host
      -> `361 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `361 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-087` Tensor unary scientific math semantics:
  - Added a shared Tensor unary-math helper for scalar scientific primitives.
  - Extended `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`,
    `tanh`, `exp`, `log`, and `log10` to accept native Tensor inputs.
  - `Double` Tensor inputs return same-shape `Double` tensors.
  - `BigInteger` Tensor inputs return same-shape `Double` tensors through the
    hardened fail-closed finite conversion path.
  - `BigFloat` Tensor inputs preserve `BigFloat` dtype and shape.
  - `BigComplex` Tensor inputs preserve `BigComplex` dtype and shape, including
    lazy source realization before evaluation.
  - Folded Tensor `sqrt` onto the shared unary helper while preserving the
    `TENSOR-086` result dtype contract.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - `c3c build`
    - focused advanced collections/module group on host
      -> `353 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `353 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-086` Tensor `sqrt` semantics:
  - Extended the existing `sqrt` primitive to accept native Tensor inputs.
  - `Double` Tensor inputs return same-shape `Double` Tensor results.
  - `BigInteger` Tensor inputs return same-shape `Double` Tensor results,
    matching scalar `sqrt` conversion behavior for exact integers.
  - `BigFloat` Tensor inputs preserve `BigFloat` dtype and shape.
  - `BigComplex` Tensor inputs preserve `BigComplex` dtype and shape, and lazy
    BigComplex Tensor sources are realized before evaluation.
  - Hardened `omni_big_integer_to_double` so enormous `cpp_int` values are
    rejected by bit-length before `convert_to<double>()`, preventing a
    Boost-domain abort and preserving fail-closed scalar/Tensor conversion.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build main --output-dir build --build-dir build/obj2`
    - `c3c build`
    - direct smokes for huge BigInteger `Double` conversion rejection, huge
      BigInteger Tensor `sqrt` rejection, and BigComplex Tensor `sqrt`
    - focused advanced collections/module group on host
      -> `341 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `341 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-085` Tensor `abs` semantics:
  - Extended the existing `abs` primitive to accept native Tensor inputs.
  - Real Tensor dtypes (`Double`, `BigInteger`, and `BigFloat`) preserve
    dtype and shape while applying elementwise magnitude.
  - `BigComplex` Tensor inputs return same-shape native `BigFloat` Tensor
    magnitudes, matching scalar `BigComplex` `abs`.
  - Lazy Tensor sources are realized before magnitude extraction, then cleaned
    through the existing materialized-value boundary.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - `c3c build`
    - direct smoke for `abs` on a BigComplex Tensor
    - direct smokes for `abs` on BigInteger Tensor and lazy BigComplex Tensor
    - focused advanced collections/module group on host
      -> `335 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `335 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-084` real Tensor component semantics:
  - Extended `real-part`, `imag-part`, and `conjugate` from BigComplex-only
    Tensor behavior to all native real Tensor dtypes.
  - `real-part` and `conjugate` copy `Double`, `BigInteger`, and `BigFloat`
    tensor values while preserving dtype and shape.
  - `imag-part` returns same-shape zero tensors in the same real dtype.
  - This supersedes the `TENSOR-083` note that non-BigComplex Tensor component
    inputs fail closed.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - `c3c build`
    - direct smokes for `imag-part` on Double Tensor and `conjugate` on
      BigInteger Tensor
    - focused advanced collections/module group on host
      -> `330 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `330 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-083` BigComplex Tensor component kernels:
  - Extended `real-part`, `imag-part`, and `conjugate` to accept native
    BigComplex Tensor inputs.
  - `real-part` and `imag-part` realize lazy BigComplex Tensor sources when
    needed and return native BigFloat Tensor results.
  - `conjugate` realizes lazy BigComplex Tensor sources when needed and returns
    native BigComplex Tensor results.
  - Non-BigComplex Tensor inputs fail closed instead of guessing result dtype.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for BigComplex Tensor `real-part`, `imag-part`, and lazy
      source `conjugate`
    - focused advanced collections/module group on host
      -> `327 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `327 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-082` native BigComplex Tensor storage and kernels:
  - Added `TENSOR_DTYPE_BIG_COMPLEX` metadata, dtype symbol/name lookup,
    owned BigComplex handle storage, element cleanup, deep clone, and concrete
    storage copy support.
  - Extended `Tensor` constructors to accept `BigComplex` dtype descriptors:
    `(Tensor BigComplex shape data-or-scalar)`, `(Tensor data BigComplex)`,
    and `(Tensor BigComplex data)`. BigComplex tensors accept `Integer`,
    `Double`, `BigInteger`, `BigFloat`, and `BigComplex` numeric values, with
    real values promoted to zero-imaginary BigComplex elements.
  - Extended `ref`, flat `(Array tensor)` / `(List tensor)` conversion,
    scalar `realize` fill, concrete tensor copy realization, tensor-dispatched
    `map`, and pure C3 `contract` to native BigComplex tensors.
  - Kept BLAS fast paths `Double`-only; BigComplex contracts use the pure C3
    contraction fallback and preserve complex results.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - `c3c build`
    - direct smokes for BigComplex Tensor `ref`, lazy `map`, and `contract`
    - focused advanced collections/module group on host
      -> `321 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `321 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-090E` optional BLAS `dger` outer-product fast path:
  - Extended the private runtime-loaded BLAS helper to resolve `cblas_dger`
    alongside existing `cblas_dgemm`/`cblas_dgemv`/`cblas_ddot`, with
    availability and call-count probes for focused validation.
  - Added a rank-1/rank-1 `Double` zero-axis contraction fast path for outer
    product behind existing `contract`/`realize` evaluation. Unsupported
    dtype, rank, shape, missing-symbol, zero-size, or size-overflow cases keep
    the pure C3 contraction fallback.
  - Kept the public Tensor surface unchanged; no public `outer`, `dot`,
    `solve`, LAPACK, or backend-specific Tensor spelling was added.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build main --output-dir build --build-dir build/obj2`
    - focused advanced collections/module group on host
      -> `298 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `298 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-090D` optional BLAS `ddot` contract fast path:
  - Extended the private runtime-loaded BLAS helper to resolve `cblas_ddot`
    alongside existing `cblas_dgemm`/`cblas_dgemv`, with availability and
    call-count probes for focused validation.
  - Added a rank-1/rank-1 `Double` vector dot contraction fast path behind
    existing `contract`/`realize` evaluation. Unsupported dtype, rank, shape,
    missing-symbol, zero-size, or size-overflow cases keep the pure C3
    contraction fallback.
  - Kept the public Tensor surface unchanged; no public `dot`, `solve`,
    LAPACK, or backend-specific Tensor spelling was added.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build main --output-dir build --build-dir build/obj2`
    - focused advanced collections/module group on host
      -> `297 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `297 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-081` native BigInteger Tensor storage and kernels:
  - Added `TENSOR_DTYPE_BIG_INTEGER` metadata, dtype symbol/name lookup,
    owned BigInteger handle storage, element cleanup, deep clone, and concrete
    storage copy support.
  - Extended `Tensor` constructors to accept `BigInteger` dtype descriptors:
    `(Tensor BigInteger shape data-or-scalar)`, `(Tensor data BigInteger)`,
    and `(Tensor BigInteger data)`. BigInteger tensors accept exact `Integer`
    and `BigInteger` values; inexact `Double` data fails closed.
  - Extended flat `(Array tensor)` / `(List tensor)` conversion, scalar
    `realize` fill, and concrete tensor copy realization for BigInteger
    tensors.
  - Extended lazy tensor `map` and `contract` evaluation to native BigInteger
    tensors, including unary map, tensor/scalar map, scalar/tensor map,
    broadcast map, vector dot, rank-2 matrix product, zero-size contracted-axis
    identity, explicit destination realization, return-boundary survival, and
    closure-capture survival.
  - Kept BLAS `dgemm`/`dgemv` fast paths `Double`-only; BigInteger contracts
    use the pure C3 contraction fallback and preserve exact integer results.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for BigInteger dtype/ref, inferred prefix/suffix
      constructors, flat collection conversion, scalar fill, concrete copy,
      map, contract, and inexact-data rejection
    - focused advanced collections/module group on host
      -> `295 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `295 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-079` native BigFloat Tensor `contract` kernels:
  - Extended tensor-dispatched `contract` from `Double`-only evaluation to
    native `BigFloat` evaluation through the pure C3 contraction fallback.
  - Kept BLAS `dgemm`/`dgemv` fast paths `Double`-only; BigFloat contracts
    use owned BigFloat sum/product handles and preserve Tensor dtype.
  - Preserved fail-closed mixed tensor dtype behavior; `Double`/`BigFloat`
    tensor-tensor `contract` combinations still raise `tensor/dtype-mismatch`.
  - Added focused advanced collections/module regressions for BigFloat vector
    dot, rank-2 matrix product, zero-size contracted-axis identity, explicit
    destination realization, return-boundary survival, closure-capture
    survival, and mixed-dtype rejection.
  - Updated language/reference docs and Tensor plan/status artifacts.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for BigFloat dot, matrix product, zero-size identity,
      destination realization, return-boundary survival, closure-capture
      survival, and mixed-dtype rejection
    - focused advanced collections/module group on host
      -> `271 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `271 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-078` native BigFloat Tensor `map` kernels:
  - Extended lazy tensor `map` evaluation from `Double`-only storage to native
    `BigFloat` storage for unary, tensor-scalar, scalar-tensor,
    exact-shape tensor-tensor, and right-aligned singleton-axis broadcast
    cases.
  - Added owned BigFloat scalar handles to lazy map payloads so scalar
    operands survive function-return and closure-capture boundaries through
    Tensor payload clone/promotion paths.
  - Preserved fail-closed mixed tensor dtype behavior; `Double`/`BigFloat`
    tensor-tensor `map` combinations still raise `tensor/dtype-mismatch`.
  - `contract` remains `Double`-only pending dedicated BigFloat contraction
    kernels.
  - Added focused advanced collections/module regressions for BigFloat unary
    map outside Double range, scalar-left/right map, broadcast map,
    destination realization, return-boundary survival, closure-capture
    survival, and mixed-dtype rejection.
  - Updated language/reference docs and Tensor plan/status artifacts.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for BigFloat unary preservation, scalar-left/right map,
      broadcast map, destination realization, return-boundary survival,
      closure-capture survival, and mixed-dtype rejection
    - focused advanced collections/module group on host
      -> `264 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `264 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - ASAN attempt:
      `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
      failed before compile with the local C3 compiler sanitizer platform
      message.

- Completed `TENSOR-077` native BigFloat concrete Tensor storage:
  - Added `TENSOR_DTYPE_BIG_FLOAT` metadata, dtype printing/symbol lookup,
    owned BigFloat handle storage, element cleanup, deep clone, and concrete
    storage copy support.
  - Extended `Tensor` constructors to accept `BigFloat` dtype descriptors:
    `(Tensor BigFloat shape data-or-scalar)`, `(Tensor data BigFloat)`, and
    `(Tensor BigFloat data)`.
  - `BigFloat` tensors now support `dtype`, `ref`, flat `(Array tensor)` /
    `(List tensor)` conversion, scalar `realize` fill, and concrete tensor
    copy realization.
  - `map` and `contract` remain `Double`-only and reject BigFloat tensors with
    `tensor/dtype-mismatch` until dedicated BigFloat tensor kernels land.
  - Added focused advanced collections/module regressions for dtype/ref,
    inferred prefix/suffix construction, flat collection conversion, scalar
    fill, concrete copy, and map rejection. Updated the existing lifetime
    partial-constructor cleanup assertion for the more specific Double
    narrowing error text.
  - Updated language/reference docs and Tensor plan/status artifacts.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
