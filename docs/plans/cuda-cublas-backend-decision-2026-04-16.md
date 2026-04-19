# CUDA/cuBLAS Backend Decision

Date: 2026-04-16
Status: closed design slice for `TENSOR-100`

## Decision

CUDA/cuBLAS support stays behind the existing `Tensor` value. Omni will not add
a public `GpuTensor`, `CudaTensor`, backend-specific tensor subtype, or
backend-flavored mathematical operation as the first GPU surface.

The canonical future user-facing operations are:

```lisp
(to-device tensor 'cuda)
(to-device tensor 'cpu)
(device tensor)
(tensor-backends)
```

`to-device` is an explicit placement/copy boundary. It returns a `Tensor` whose
placement is requested by the caller. `device` reports the current placement of
a Tensor, with ordinary host tensors reporting `'cpu`. `tensor-backends`
reports available optional backend capabilities for diagnostics and feature
gating.

Rejected first-surface names and forms:

- `GpuTensor`
- `CudaTensor`
- `tensor/cuda/contract`
- `cublas-contract`
- `cuda-matmul`
- `tensor-use-backend!` as the first control surface
- implicit CPU/GPU transfer inside ordinary `map`, `contract`, `realize`, or
  `matrix/*` operations

## Semantics

Ordinary Tensor semantics remain CPU-native and pure-fallback driven. A normal
build and normal Tensor program must not require CUDA, cuBLAS, a GPU, or CUDA
runtime libraries.

GPU placement is explicit:

- `(to-device x 'cuda)` copies a concrete or realized Tensor to CUDA storage
  when a compatible CUDA backend is available.
- `(to-device x 'cpu)` copies a CUDA-placed Tensor back to native CPU Tensor
  storage.
- Lazy Tensor expressions must be realized or staged before device movement;
  device movement is a storage boundary, not a macro rewrite of arbitrary
  expression graphs.
- Ordinary CPU Tensor operations do not move data to CUDA implicitly.
- CUDA Tensor operations do not silently move data back to CPU to preserve a
  fallback. Unsupported CUDA operations fail deterministically unless the user
  explicitly calls `(to-device x 'cpu)`.

The first cuBLAS execution target was dense contiguous `Float64` single-axis
contraction where inputs are CUDA-placed and layout-compatible. The current
implemented contract covers matching CUDA-placed `Float64` or `Float32`
operands. The rank-2/rank-2 layout set matches the CPU BLAS GEMM contract:
`[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`. Rank-2/rank-1 and rank-1/rank-2
layouts match the CPU BLAS GEMV contract. Unsupported dtypes, mixed dtypes,
ranks, strides, devices, aliasing, zero-size CUDA dimensions, or missing
cuBLAS support must produce a deterministic backend diagnostic rather than
changing Tensor semantics.

CPU BLAS/LAPACK remains the comparison oracle:

- CPU `contract`/`matrix/*` pure fallbacks remain authoritative for semantics.
- cuBLAS tests compare CUDA results against the same CPU pure fallback or
  existing CPU BLAS/LAPACK-visible public result.
- Missing CUDA/cuBLAS dependencies must skip or report backend-unavailable
  diagnostics; they must not make ordinary CPU Tensor validation fail.

## Ownership

CUDA storage and cuBLAS handles are opaque foreign resources. They must not own
Omni `Value` graphs.

Required ownership rules:

- `Tensor` remains the public value wrapper and the region-owned language
  object.
- CUDA buffers are opaque backend resources referenced by Tensor payload
  metadata.
- cuBLAS handles have one finalizer authority.
- Backend resource finalization is deterministic at the Tensor payload or
  backend-context boundary.
- No CUDA buffer or cuBLAS handle may retain pointers to TEMP-scoped Omni
  values.
- Any future CUDA resource wrapper must include return-boundary,
  closure-capture/env-copy, and destruction-path tests.

## Diagnostics

The first CUDA/cuBLAS diagnostics should distinguish:

- no CUDA backend compiled or loadable,
- no CUDA device available,
- CUDA allocation failure,
- cuBLAS handle creation failure,
- unsupported dtype/rank/layout/device combination,
- unsupported implicit transfer attempt,
- cuBLAS execution failure,
- host/device copy failure.

Do not collapse these into generic Tensor shape or type errors.

## Validation Contract

The first implementation slice is complete only when:

- CPU Tensor tests still pass with no CUDA libraries present.
- `(to-device tensor 'cuda)` fails closed when CUDA is unavailable.
- `(device tensor)` reports `'cpu` for ordinary Tensor values.
- CUDA storage has deterministic destruction coverage.
- cuBLAS rank-2 `Float64` contraction matches CPU Tensor results.
- unsupported CUDA paths do not silently fall back by copying to CPU.
- bounded-container validation remains the default for GPU-heavy checks.

## Follow-Up Implementation Shape

Suggested implementation order:

1. Add Tensor placement metadata and CPU-only `device` reporting.
2. Add `to-device` fail-closed diagnostics for unavailable CUDA without linking
   CUDA as a required dependency.
3. Add opaque CUDA buffer ownership with destruction tests.
4. Add explicit CPU<->CUDA copy for concrete `Float64` Tensor storage.
5. Add cuBLAS handle lifecycle.
6. Add CUDA-placed rank-2 `Float64` `contract` fast paths for `[1 0]`,
   `[0 0]`, `[1 1]`, and `[0 1]`.
7. Add bounded GPU validation and fallback-oracle comparison tests.

Status update:

- `TENSOR-100A` implemented the CPU-only placement/introspection surface:
  `device` reports `'cpu` for current Tensor values, `to-device` with target
  `'cpu` realizes Tensor expressions to CPU Tensor storage, and `to-device`
  with target `'cuda` fails closed with `tensor/backend-unavailable`.
- `TENSOR-100B` implemented concrete placement metadata and backend
  introspection without requiring CUDA libraries: `TensorVal` stores placement
  plus an opaque device handle/finalizer slot, `tensor-backends` reports
  structured CPU/CUDA availability, CPU kernels reject non-CPU storage, and
  fake-device destruction is covered by memory-lifetime tests.
- `TENSOR-100C` implemented optional runtime-loaded CUDA copy support:
  `to-device 'cuda` copies concrete `Float64` CPU Tensor storage to CUDA when
  libcudart is usable, `to-device 'cpu` copies CUDA storage back to native CPU
  Tensor storage, and CUDA availability requires symbol resolution, device
  count, and allocation/free probing. A later `TENSOR-100F` checkpoint extended
  the same explicit copy boundary to concrete `Float32` Tensor storage.
- `TENSOR-100D` implemented optional runtime-loaded cuBLAS execution:
  `tensor-backends` reports `cublas`, cuBLAS handles are created/destroyed per
  operation, CUDA-placed dense row-major `Float64` rank-2/rank-2 single-axis
  contractions execute through `cublasDgemm_v2` for `[1 0]`, `[0 0]`,
  `[1 1]`, and `[0 1]`, and rank-2/rank-1 plus rank-1/rank-2 contractions
  execute through `cublasDgemv_v2`.
- CUDA destination-form `realize` now supports existing dense row-major
  `Float64` CUDA destinations for matching CPU sources, CUDA sources,
  supported lazy CUDA contract results, and scalar fills. CPU destinations
  still reject CUDA sources unless the caller explicitly copies with
  `to-device 'cpu`; unsupported CUDA lazy expressions and cross-backend sources
  fail closed instead of staging through CPU.
- `TENSOR-100F` extended CUDA placement, CPU copyback, destination-form
  `realize`, scalar destination fills, and the existing cuBLAS contract layouts
  to matching dense row-major `Float32` tensors. The helper resolves
  `cublasSgemm_v2` / `cublasSgemv_v2` separately and reports cuBLAS `float32`
  capability without making existing `Float64` availability depend on those
  symbols.
- `TENSOR-100F` also added explicit zero-size CUDA contract identity/fill for
  the existing supported dense row-major `Float64` and `Float32` rank-2/rank-2,
  rank-2/rank-1, and rank-1/rank-2 single-axis layout family. Zero free
  dimensions return CUDA-placed zero-length outputs; zero contracted
  dimensions fill non-empty outputs with additive identity through CUDA fill
  helpers and skip cuBLAS.
- `TENSOR-100F` extended that CUDA contract family to contiguous
  rank-1/rank-1 dot for matching `Float64` and `Float32`, returning
  CUDA-placed scalar Tensor outputs by reusing the existing row-major GEMV
  helper.
- `TENSOR-100F` added embedded-PTX CUDA Driver API elementwise binary map
  kernels for dense row-major `Float64` and `Float32` tensors. Supported
  operations are `+`, `-`, `*`, `/`, `min`, and `max`; supported operand
  shapes are tensor/scalar, scalar/tensor, exact-shape tensor/tensor, and
  right-aligned singleton-axis tensor/tensor broadcasting.
  Direct public `map` on CUDA tensors, lazy map realization to CUDA, and CUDA
  destination realization from lazy CUDA maps preserve CUDA placement. There
  is no hidden CPU fallback.
- `TENSOR-100F` also added separate embedded-PTX CUDA unary map kernels for
  dense row-major `Float64` and `Float32` tensors. Supported unary op ids are
  `0..4`: `abs`, unary `-`, `sqrt`, identity (`map +`, `real-part`,
  `conjugate` on real tensors), and zero-fill (`imag-part` on real tensors).
  Direct Tensor `abs`, unary `-`, `sqrt`, `real-part`, `imag-part`, and
  `conjugate` preserve CUDA placement for eligible real CUDA tensors.
- `TENSOR-100F` then added generated CUDA C/libdevice PTX scientific unary map
  kernels for dense row-major `Float64` and `Float32` op ids `5..19`: `sin`,
  `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`,
  `log10`, `math/erf`, `math/erfc`, and `stats/normal-cdf`. Public `map`, lazy CUDA map
  realization, destination `realize`, and direct Tensor scientific unary
  primitives preserve CUDA placement when the scientific module loads.
- `tensor-backends` now reports `elementwise-map-float64`,
  `elementwise-map-float32`, `scientific-map-float64`, and
  `scientific-map-float32` capability keys.
- CUDA concrete Tensor clone/copy support covers Omni-owned CUDA payloads and
  valid foreign CUDA concrete payloads. Foreign CUDA clones allocate fresh
  Omni-owned CUDA storage, copy device-to-device, and install the normal CUDA
  finalizer on the clone; fake or invalid CUDA handles remain rejected.
- Remaining CUDA map broadening must stay explicit and fail-closed:
  unsupported arbitrary callables, mixed CPU/CUDA operands, mixed dtype/device
  operands, and unsupported layouts are not covered by the dense row-major CUDA
  map checkpoints. `math/erf`, `math/erfc`, and `stats/normal-cdf` are fixed
  recognized opcodes, not a general GPU callable ABI.
