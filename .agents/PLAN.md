# Scientific Numerics And Tensor Plan

Date: 2026-04-12
Workspace: `/home/christos/Omni`

## Active Direction

Omni should support scientific work through three coordinated surfaces:

1. exact and high-precision scalar numerics,
2. a constructor-driven Tensor conversion surface,
3. optional scientific backends behind Tensor-aware shims.

The public API should follow Omni's existing constructor/coercion pattern. For
example, `Array` already supports `(Array '(1 2 3))`; Tensor should follow the
same style instead of making helper names the primary surface.

## Phase 1 - BigInteger

Goal: make exact integer overflow safe without turning all small integers into
heap objects.

Design:
- Keep the current inline small `Integer` fast path.
- Add a GMP-backed big integer representation for values that exceed the small
  integer range.
- Make integer arithmetic auto-promote to big integer on overflow.
- Parse oversized integer literals and oversized integer strings into the big
  representation.
- Keep the `Integer` constructor public and overload it so `(Integer x)` returns
  an exact integer, using small storage when possible and big storage when
  required.
- Keep a public `BigInteger` constructor as an explicit request for big storage,
  but do not require users to call it for overflow safety.
- Make `integer?`, `number?`, `type-of`, and `^Integer` dispatch treat small and
  big exact integers coherently. If `type-of` exposes representation, decide
  explicitly whether it returns `Integer` or `BigInteger` for promoted values;
  the preferred semantic default is `Integer`.
- Range-check FFI and machine-integer boundaries. Never truncate big integers
  implicitly when passing to C `int`, `long`, `size_t`, etc.

Validation:
- Literal overflow parses as exact integer rather than wrapping.
- `+`, `-`, `*`, unary negation, `abs`, `gcd`, `lcm`, `%`, bitwise ops, shifts,
  equality, comparison, hashing, `number->string`, and `string->number` handle
  promoted integers.
- Boundary copy, closure capture/env copy, arrays/dictionaries, and destruction
  paths have regression tests for GMP-backed values.
- FFI conversion rejects out-of-range big integers with a typed error.

## Phase 2 - BigFloat

Goal: add high-precision floating point without replacing hardware `Double`.

Design:
- Keep `Double` as the default hardware float.
- Add an MPFR-backed `BigFloat` representation.
- Keep public constructors:
  - `(Double x)` for hardware double coercion.
  - `(Float x)` as an alias only if owner approves the spelling; otherwise
    prefer `Double`.
  - `(BigFloat x)` for high-precision float construction.
- Overload `Double`/`Float` to accept big integers and big floats with explicit
  inexact conversion and clear precision-loss behavior.
- Do not auto-promote ordinary `Double` overflow to `BigFloat` silently unless a
  product-level numeric policy explicitly chooses that. Preferred initial rule:
  exact integer overflow promotes automatically; inexact float overflow remains
  a `Double` domain/range event unless the user asked for `BigFloat`.
- Add precision and rounding policy before exposing broad MPFR math. Avoid
  global hidden precision state if possible; prefer explicit default context and
  optional scoped precision.

Validation:
- `BigFloat` parse/print, comparison, conversion to/from `Integer`, `BigInteger`,
  and `Double`.
- Arithmetic respects precision policy and returns `BigFloat` when any operand
  is `BigFloat`.
- Mixed exact/inexact behavior is documented and tested.

## Phase 3 - Tensor Constructor Surface

Goal: make Tensor the scientific array boundary while preserving `Array` as the
general boxed collection.

Design:
- Keep `Array` as heterogeneous boxed `Value*` storage.
- Treat Tensor as unboxed numeric storage or a foreign backend handle with:
  dtype, shape, strides, rank, device, and ownership/view metadata.
- Make `Tensor` the primary conversion constructor:
  - `(Tensor '(1.0 2.0 3.0))`
  - `(Tensor (Array 1.0 2.0 3.0))`
  - `(Tensor '((1.0 2.0) (3.0 4.0)))`
  - `(Tensor (Array (Array 1.0 2.0) (Array 3.0 4.0)))`
  - `(Tensor iterator)`
- Support optional dtype/device/layout arguments once the minimal constructor
  works:
  - `(Tensor xs 'Float64)`
  - `(Tensor xs 'Float64 'cpu)`
- Keep `BigInteger`, `BigFloat`, `Integer`, and `Double` scalar conversions
  explicit at Tensor construction time. Initial tensor numeric lane should be
  `Float64`; add `Float32`, `Int64`, and high-precision lanes only when needed.
- Keep `Array`/`List` conversions from Tensor:
  - `(Array tensor)` boxes and copies values.
  - `(List tensor)` boxes and copies values.
- Keep raw backend handle construction out of the public `Tensor` arity if it
  conflicts with collection conversion. Prefer a private/internal constructor
  such as `_Tensor` or `Tensor/from-handle` for backend wrappers.

Validation:
- Constructor dispatch accepts lists, arrays, nested rectangular collections,
  and iterators.
- Constructor rejects ragged nested shapes, non-numeric payloads, unsupported
  dtype/device requests, and impossible conversions with deterministic typed
  errors.
- Tensor values survive scope return, closure capture, env copy, and FFI-handle
  lifecycle boundaries.

## Phase 4 - GSL Compatibility

Goal: make Omni GSL-capable without making GSL a mandatory core dependency
unless the project explicitly accepts GPL-compatible distribution constraints.

Design:
- Implement GSL as an optional package/backend first.
- Add a narrow C shim layer; do not expose raw GSL pointers as the public API.
- Disable or replace GSL's aborting default error handler in the shim, and
  convert GSL error codes into Omni errors.
- Accept only Tensor inputs that can safely be represented as GSL vectors or
  matrices:
  - CPU-backed,
  - supported dtype, initially `Float64`,
  - rank 1 or 2 where required,
  - contiguous or representable as a GSL stride view,
  - no implicit GPU/autograd/lazy buffer transfer.
- Use explicit conversion for unsupported cases:
  - `(cpu tensor)` before GSL if a GPU tensor must be materialized.
  - `(Tensor xs 'Float64)` before GSL if dtype coercion is needed.

Initial GSL surface:
- statistics and descriptive reductions,
- special functions not already covered well,
- interpolation,
- integration,
- root finding/minimization,
- ODE helpers only if the callback story is already safe.

Validation:
- GSL accepts compatible Tensor views without copies where safe.
- Unsupported tensor layouts fail closed.
- GSL domain/range errors are ordinary Omni errors, not process aborts.
- Package can be built/omitted independently of the default runtime.

## Phase 5 - Other Scientific Libraries

Recommended order:

1. OpenBLAS + LAPACK/LAPACKE as the primary dense linear algebra backend.
   These should sit behind Tensor operations such as dot, matmul, solve,
   factorization, SVD, and eigenvalue routines.
2. HDF5 for scientific data persistence once Tensor storage/layout is stable.
   Map Tensor datasets to HDF5 datasets with dtype/shape metadata.
3. FFTW as an optional FFT backend only if GPL compatibility is acceptable for
   the package or distribution mode.
4. SUNDIALS for ODE/DAE/nonlinear solvers after Tensor callbacks and user
   function callback lifetimes are safe.
5. NetCDF as an optional domain-format package for climate/geoscience style
   workloads, likely after HDF5.
6. SuiteSparse/GraphBLAS for sparse matrix and graph workloads, not as an early
   dense Tensor dependency.
7. ARPACK-ng only when sparse eigenvalue problems become a real use case; dense
   eigen work should start with LAPACK.
8. libtorch remains a Tensor backend for ML/autograd/GPU workflows. Do not force
   GSL semantics onto libtorch tensors; bridge only through explicit CPU/dtype
   materialization when needed.

## Current Next Checkpoint

Start with a design note or implementation slice for Phase 1:
- exact integer representation,
- constructor/dispatch behavior for `Integer` and `BigInteger`,
- overflow promotion helper API,
- FFI range-check policy,
- boundary lifetime tests for GMP-backed values.

Do not start by binding all of GSL. The Tensor constructor surface and exact
integer promotion are better foundations for a coherent scientific stack.
