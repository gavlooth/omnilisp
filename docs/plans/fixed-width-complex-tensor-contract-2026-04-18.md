# Fixed-Width Complex Tensor Contract

Date: 2026-04-18
Status: Contract note for CPU `Complex128`/`Complex64` scalar/Tensor support,
CUDA/Vulkan raw storage, CUDA/Vulkan fixed-width complex elementwise map,
CUDA/Vulkan fixed-width complex contract, CUDA/Vulkan fixed-width complex
structural matrix kernels, and the landed Vulkan fixed-width complex numerical
subset: LU/determinant/solve/inverse/rank/norm/singular-values/QR/Cholesky.
Full complex SVD factor output, CUDA complex singular-values/norm/SVD, and
complex eigen result contracts remain deferred.

See `docs/plans/fixed-width-complex-closure-plan-2026-04-18.md` for the active
three-lane closure plan.

## Decision

Omni now has two fixed-width complex scalar families:

- `Complex128`: two finite `Float64` components, real then imaginary.
- `Complex64`: two finite `Float32` components, real then imaginary.

These are separate from `BigComplex`. `BigComplex` remains the
high-precision pointer-backed complex family and must not be silently lowered
to either fixed-width complex dtype.

The public scalar surface is:

- `(Complex128 real [imag])`
- `(Complex64 real [imag])`
- `complex128?`
- `complex64?`

The optional imaginary argument defaults to zero. Constructor arguments must be
real finite numeric values representable by the target component width.

## Tensor Contract

`Tensor` accepts `Complex128` and `Complex64` as explicit dtype descriptors:

```lisp
(Tensor Complex128 [2] [(Complex128 1 2) (Complex128 3 4)])
(Tensor Complex64 [2] [(Complex64 1 2) (Complex64 3 4)])
```

`dtype` returns `'Complex128` or `'Complex64` for these Tensor dtypes. `ref`,
`Array`, `List`, and `Iterator` expose elements as scalar `Complex128` or
`Complex64` values, preserving the Tensor dtype's component width.

CPU Tensor support covers the native storage and oracle path:

- constructor scalar fills and shaped data;
- inferred dtype from scalar fixed-width complex leaves;
- `tensor?`, `dtype`, `shape`, `rank`, `length`, and `tensor-layout`;
- `ref`;
- flat collection conversions through `Array`, `List`, and `Iterator`;
- `realize` for exact-shape/dtype CPU destinations;
- tensor-dispatched `map` for unary, tensor-scalar, scalar-tensor,
  exact-shape tensor-tensor, and right-aligned singleton-axis broadcast
  operands;
- `contract` with the existing paired-axis rules;
- `real-part`, `imag-part`, and `conjugate`.

`real-part` and `imag-part` of `Complex128` tensors produce same-shape
`Float64` tensors. `real-part` and `imag-part` of `Complex64` tensors produce
same-shape `Float32` tensors. `conjugate` preserves fixed-width complex dtype.

Tensor arithmetic dtype promotion follows the scalar fixed-width complex
family: `Complex128` wins over `Complex64`; `Complex64` remains `Complex64`
only when all participating complex values are representable in that dtype and
no `Complex128` input participates. Non-complex real scalar operands are
coerced into the first tensor input's dtype for tensor `map`, matching the
existing tensor scalar-broadcast rule.

## Backend Policy

Fixed-width complex support has CPU execution plus explicit device storage
round-trips. CUDA and Vulkan may accept `Complex128` and `Complex64` placement
for concrete zero-offset dense row-major Tensor storage when `tensor-backends`
reports the matching `complex128` or `complex64` capability. Explicit
`to-device 'cpu` copyback preserves the fixed-width complex dtype and scalar
elements.

CUDA and Vulkan fixed-width complex support includes raw storage plus dense
row-major elementwise map kernels when the matching
`elementwise-map-complex128` or `elementwise-map-complex64` capability is
reported. CUDA/Vulkan complex `map` supports binary `+`, `-`, `*`, `/` plus
unary `+`, `abs`, unary `-`, `real-part`, `imag-part`, and `conjugate` without
hidden CPU fallback. Generic map preserves the complex dtype; map
`real-part`, `imag-part`, and `abs` produce complex tensors with zero imaginary
components where applicable. Direct CUDA/Vulkan complex `abs`, `real-part`,
and `imag-part` return component-width real device tensors: `Float64` for
`Complex128` inputs and `Float32` for `Complex64` inputs. Direct unary `-` and
`conjugate` preserve fixed-width complex dtype and placement.

CUDA complex division by zero maps through native helper status to
`tensor/domain-error`; nonrepresentable CUDA complex results map to invalid
argument before result exposure. CUDA/Vulkan complex `contract` is a separate
operation family from raw storage and elementwise `map`. When
`tensor-backends` reports `contract-complex128` or `contract-complex64`,
supported dense row-major fixed-width complex contractions compute on the
selected backend, preserve `Complex128` or `Complex64` dtype and device
placement, and follow the public Tensor paired-axis and explicit left/right
axis-list rules. Missing capability, unsupported layouts, unsupported axis
families, mixed devices, mixed dtypes, or unavailable helpers fail closed
without hidden CPU fallback.

CUDA/Vulkan complex structural matrix kernels are separate landed operation
families.
When `tensor-backends` reports `matrix-structural-complex128` or
`matrix-structural-complex64`, CUDA and Vulkan dense row-major
`Complex128`/`Complex64` tensors support `matrix/transpose`,
`matrix/diagonal`, `matrix/diagonal-matrix`, and `matrix/trace` on the selected
backend. Tensor-returning structural operations preserve dtype and placement;
`matrix/trace` reads back only the fixed-width complex scalar required by the
public scalar return contract.

CUDA/Vulkan complex numerical matrix kernels remain separate operation
families. Vulkan complex numerical matrix kernels outside the landed
`matrix/lu`, `matrix/determinant`, `matrix/solve`, `matrix/inverse`,
`matrix/rank`, `matrix/norm`, `matrix/singular-values`, `matrix/qr`, and
`matrix/cholesky` subset remain separate operation families. They must fail
closed for `Complex128` and `Complex64` device tensors unless a specific matrix
operation has its own helper/shader/PTX ABI, capability bit, status/error
contract, and validation coverage. Matrix kernels must not be inferred from
complex storage, complex elementwise `map`, complex `contract`, or structural
matrix capability, and must not lower through `Float64`, `Float32`, or
`BigComplex`.

The landed Vulkan numerical complex matrix subset includes `matrix/lu`,
`matrix/determinant`, `matrix/solve`, `matrix/inverse`, `matrix/rank`,
`matrix/norm`, `matrix/singular-values`, `matrix/qr`, and `matrix/cholesky` for
dense row-major zero-offset `Complex128`/`Complex64` Vulkan tensors when
`tensor-backends` reports `matrix-numerical-complex128` or
`matrix-numerical-complex64`.
`matrix/lu` returns Vulkan-placed fixed-width complex `lu` factors plus host
`pivots` and `swap-count` metadata. `matrix/determinant` computes on Vulkan
and reads back only the public fixed-width complex scalar. `matrix/solve`
returns a Vulkan-placed Tensor preserving RHS rank and dtype. `matrix/inverse`
returns a Vulkan-placed Tensor preserving input dtype. Singular systems and
singular matrices raise `tensor/singular-matrix`, unsupported
layouts/devices/dtypes fail closed, and validation covers no hidden CPU
fallback. `matrix/rank` uses magnitude-based pivoting and tolerance and reads
back only the public `Integer` result. `matrix/norm` supports
default/`'frobenius`, `'one`, `'infinity`, `'max`, `'spectral`, and
`'nuclear`; direct selectors use complex magnitudes and spectral/nuclear
selectors use fixed-width complex singular values, reading back only the public
`Float64` result. `matrix/singular-values` returns Vulkan-placed
component-width real tensors (`Float64` for `Complex128`, `Float32` for
`Complex64`). `matrix/qr` uses the
Hermitian inner product, returns Vulkan-placed `q` and `r` tensors preserving
the fixed-width complex dtype, and maps rank deficiency to
`tensor/singular-matrix`. `matrix/cholesky` computes lower factors for
Hermitian positive-definite inputs, preserves dtype and Vulkan placement, and
maps non-Hermitian or non-HPD input to `tensor/not-positive-definite`. Full
complex `matrix/svd` factor output remains a separate contract item because it
needs fixed-width complex `u`/`v` tensors and component-width real `s`.

`tensor-backends` reports real fixed-width capabilities independently from
complex storage capabilities. Existing `float64` and `float32` capability bits
do not imply complex storage, and `complex128` or `complex64` storage
capability does not imply `elementwise-map-*`, `contract-*`, structural matrix,
or numerical matrix support. Operation capability must be reported
independently.

## Validation Coverage

Focused scalar and CPU Tensor tests cover:

- scalar constructors and predicates:
  `(complex128? (Complex128 1.0 2.0))`,
  `(complex64? (Complex64 1.0 2.0))`, and negative predicate cases.
- scalar component helpers:
  `real-part`, `imag-part`, and `conjugate` preserve `Float64` components for
  `Complex128` and `Float32` components for `Complex64`.
- tensor construction:
  `(dtype (Tensor Complex128 [2] [(Complex128 1 2) (Complex128 3 4)]))`
  returns `'Complex128`; the analogous `Complex64` case returns `'Complex64`.
- inferred tensor construction:
  `(Tensor [(Complex128 1 2) (Complex64 3 4)])` infers `'Complex128`, and
  all-`Complex64` leaves infer `'Complex64`.
- tensor scalar fill:
  `(Tensor Complex128 [2 2] (Complex128 1 -1))` and
  `(Tensor Complex64 [2 2] (Complex64 1 -1))` preserve dtype and values
  through `ref`.
- tensor conversion:
  `Array`, `List`, and `Iterator` over CPU complex tensors return flat
  fixed-width complex scalar elements.
- tensor `map`:
  unary `conjugate`, tensor-scalar `+`, scalar-tensor `+`, exact-shape
  tensor-tensor `+`, and right-aligned singleton-axis broadcast for both
  complex dtypes. Mixed fixed-width complex operands promote to `Complex128`
  when a `Complex128` operand participates; real tensor operands can combine
  with fixed-width complex scalars through the fixed-width complex result
  family.
- tensor `contract`:
  a rank-2 by rank-2 contraction for `Complex128` and `Complex64`, checking
  complex multiplication plus accumulation.
- component tensor helpers:
  `real-part` and `imag-part` return `Float64` tensors for `Complex128` input
  and `Float32` tensors for `Complex64` input; `conjugate` preserves complex
  dtype.
- device storage behavior:
  capability-gated `(to-device complex-tensor 'cuda)` and
  `(to-device complex-tensor 'vulkan)` placement plus explicit `to-device 'cpu`
  copyback preserve `Complex128` and `Complex64` dtype and values. CUDA/Vulkan
  destination `realize` can copy matching CPU or same-device complex storage
  into existing complex device tensors.
- Vulkan backend map behavior:
  capability-gated `map` over Vulkan `Complex128`/`Complex64` tensors supports
  tensor/scalar, scalar/tensor, tensor/tensor, and broadcast arithmetic,
  direct component helpers, lazy realization into Vulkan destinations, and
  fail-closed CPU destinations with no hidden CPU fallback.
- CUDA backend map behavior:
  capability-gated `map` over CUDA `Complex128`/`Complex64` tensors supports
  binary `+`, `-`, `*`, `/`; unary `abs`, unary `-`, identity/`+`,
  `real-part`, `imag-part`, and `conjugate`; lazy realization into CUDA
  destinations; direct component/magnitude helpers; division status mapping to
  `tensor/domain-error`; and invalid-result mapping to invalid argument.
- backend complex `contract` behavior:
  capability-gated CUDA/Vulkan `contract` over `Complex128`/`Complex64`
  tensors preserves dtype and backend placement, supports the documented dense
  row-major axis families, handles additive-identity cases according to the
  shipped contract, and fails closed for unsupported layouts, mixed devices,
  mixed dtypes, missing capability, or unavailable helpers without hidden CPU
  fallback.
- backend structural complex matrix behavior:
  capability-gated CUDA/Vulkan `matrix/transpose`, `matrix/diagonal`,
  `matrix/diagonal-matrix`, and `matrix/trace` over `Complex128`/`Complex64`
  tensors preserve dtype and selected backend placement for Tensor results,
  return scalar fixed-width complex values for `matrix/trace`, handle
  zero-size structural cases, and fail closed when the selected backend
  structural capability is absent.
- fail-closed backend residuals:
  complex numerical matrix kernels other than the landed Vulkan
  LU/determinant/solve/inverse/rank/norm/singular-values/QR/Cholesky family
  raise Tensor backend diagnostics and do not silently copy to CPU or lower
  through real tensors or `BigComplex`. Full complex SVD factor output, CUDA
  complex singular-values/norm selectors, and complex eigenvector result
  contracts remain deferred.
- backend numerical matrix behavior:
  capability-gated Vulkan `matrix/lu`, `matrix/determinant`, `matrix/solve`,
  `matrix/inverse`, `matrix/rank`, `matrix/norm`, `matrix/singular-values`,
  `matrix/qr`, and `matrix/cholesky` over dense row-major zero-offset
  `Complex128`/`Complex64` tensors preserve fixed-width complex dtype for
  complex Tensor outputs where applicable, return component-width real Tensor
  outputs for `matrix/singular-values`, keep Tensor outputs on Vulkan, read
  back only public scalar/metadata results where applicable, map singular
  systems/matrices to `tensor/singular-matrix`, apply rank tolerance against
  complex magnitude, compute direct norms over complex magnitude, compute
  spectral/nuclear norms from fixed-width complex singular values, and fail
  closed for unsupported layouts, mixed devices, mixed dtypes, or missing
  capability without hidden CPU fallback.

Current validated commands for the landed CUDA/Vulkan fixed-width complex
backend slices:

- `./scripts/build_omni_chelpers.sh`
- `c3c build --obj-out obj`
- host focused
  `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  (`1482 passed, 0 failed` after the CUDA structural slice)
- bounded container focused
  `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  (`1465 passed, 0 failed` after the CUDA structural slice)
- host focused
  `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  (`1540 passed, 0 failed` after the Vulkan rank/direct-norm slice)
- bounded container focused
  `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  (`1523 passed, 0 failed` after the Vulkan rank/direct-norm slice)
- CUDA structural PTX generation plus `ptxas` for
  `csrc/tensor_cuda_complex_matrix.cu`
- complex Vulkan structural shader generation plus `glslangValidator` /
  `spirv-val` for eight shaders
- complex Vulkan rank/norm shader generation plus `glslangValidator` /
  `spirv-val` for four shaders
- complex Vulkan QR/Cholesky shader generation plus `glslangValidator` /
  `spirv-val` for four shaders
- complex Vulkan singular-values shader generation plus `glslangValidator` /
  `spirv-val` for two shaders
- host focused
  `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  (`1570 passed, 0 failed` after the Vulkan QR/Cholesky slice)
- bounded container focused
  `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  (`1553 passed, 0 failed` after the Vulkan QR/Cholesky slice)
- host focused
  `env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  (`1598 passed, 0 failed` after the Vulkan singular-values/norm slice)
- bounded container focused
  `env OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local OMNI_VALIDATION_TIMEOUT_SEC=900 scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  (`1581 passed, 0 failed` after the Vulkan singular-values/norm slice)
- CUDA/Vulkan C helper syntax and symbol checks
- primitive docs parity
- Stage 3 source parity
- targeted diff check

Signature: Codex GPT-5.4
