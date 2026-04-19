# Vulkan Backend Decision

Date: 2026-04-16
Status: Completed correctness-first baseline for `TENSOR-100E`

## Decision

Vulkan is the portable explicit GPU backend direction for Omni Tensor. It is a
runtime-optional execution substrate behind the existing `Tensor` surface, not
a new public Tensor family and not a backend-specific math namespace.

The public surface remains:

```lisp
(to-device tensor 'vulkan)
(to-device tensor 'cpu)
(device tensor)
(tensor-backends)
(map + tensor 1.0)
(contract a b [1 0])
```

Rejected public surfaces:

- `VulkanTensor`
- `vulkan-contract`
- `vulkan-map`
- implicit CPU/GPU transfers inside ordinary `map`, `contract`, `realize`, or
  `matrix/*`
- silent `Float64` to `Float32` downcast for portability

## Backend Shape

Omni owns the Vulkan backend ABI. The implementation should use a small C
helper layer rather than adopting a large external Tensor framework as the
runtime abstraction.

The helper layer is responsible for:

- runtime-loading `libvulkan`,
- instance creation and physical-device probing,
- compute queue-family discovery,
- buffer allocation and release,
- CPU<->Vulkan buffer copies,
- SPIR-V shader module and pipeline setup,
- compute dispatch and synchronization,
- deterministic cleanup of all opaque resources.

The C3 layer is responsible for:

- Tensor placement metadata and public diagnostics,
- dtype/layout/capability checks,
- deciding which Vulkan kernel is eligible,
- preserving the CPU Tensor implementation as semantic oracle,
- refusing unsupported paths with Tensor backend diagnostics.

## Capability Policy

`tensor-backends` must report Vulkan capability explicitly. `Float64` support is
not guaranteed by Vulkan compute devices, so it must be probed and reported.

The first inventory shape is:

```lisp
{device vulkan name vulkan available <bool> float64 <bool> ...}
```

Missing Vulkan support reports `available false` with reason
`backend-unavailable`. Missing `Float64` support must not trigger downcast.
Kernel dispatch must fail closed when the Tensor dtype, layout, rank, device,
or shader capability is unsupported.

## Implementation Order

1. Done: runtime-loaded Vulkan availability probe and `tensor-backends` entry.
2. Done: explicit `to-device 'vulkan` / `to-device 'cpu` copy roundtrip for
   concrete supported Tensor storage.
3. Done: memory-lifetime tests for opaque Vulkan buffer finalizers and
   boundary clone/copy fail-closed behavior.
4. Done: initial SPIR-V elementwise `map` add-scalar kernel, later broadened
   by the `Float64` map arithmetic family in item 9.
5. Done: rank-1/rank-1 `Float64` dot through public `contract` for
   Vulkan-placed tensors.
6. Done: generic dense row-major `Float64` single-axis `contract` for
   Vulkan-placed rank-1 and rank-2 tensors.
7. Done: hardened Vulkan generic `contract` execution layout with checked-in
   GLSL source and shape-aware 16x4 rank-2 output dispatch.
8. Done: recorded broader Vulkan dtype/layout policy in
   `docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`.
9. Done: extended the `Float64` dense row-major Vulkan `map` kernel family for
   Tensor/scalar, scalar/Tensor, and exact-shape Tensor/Tensor arithmetic
   through public `map`.
10. Done: added rank-polymorphic dense row-major metadata dispatch for Vulkan
    `Float64` `map`, including right-aligned singleton-axis Tensor/Tensor
    broadcasting.
11. Done: extended dense row-major metadata-buffer dispatch to rank-N Vulkan
    `Float64` single-axis `contract`.
12. Done: extended dense row-major rank-N Vulkan `Float64` `contract` from
    exactly one contracted axis pair to one or more explicit contracted axis
    pairs.
13. Done: added dense row-major Vulkan `Float64` rank-2 `matrix/transpose`
    through the public backend-neutral matrix surface.
14. Done: added dense row-major Vulkan `Float64` structural kernels for
    `matrix/diagonal` and `matrix/diagonal-matrix` through the public
    backend-neutral matrix surfaces.
15. Done: added dense row-major Vulkan `Float64` `matrix/trace` through the
    public backend-neutral reducer surface, computing on Vulkan and copying
    back only the scalar result required by the public scalar contract.
16. Done: added dense row-major Vulkan `Float64` `matrix/norm` for
    default/`'frobenius`, `'one`, `'infinity`, `'max`, `'spectral`, and
    `'nuclear`, computing on Vulkan and copying back only the scalar result
    required by the public scalar contract. SVD-backed `'spectral` and
    `'nuclear` route through the storage-backed Vulkan singular-values helper,
    including `k = min(rows, columns) > 64`, without hidden CPU/LAPACK
    fallback.
17. Done: added dense row-major Vulkan `Float64` `matrix/rank`, computing on
    Vulkan and copying back only the scalar `Integer` result required by the
    public scalar contract.
18. Done: added dense row-major Vulkan `Float64` `matrix/determinant`,
    computing on Vulkan and copying back only the scalar `Float64` result
    required by the public scalar contract.
19. Done: added dense row-major Vulkan `Float64` `matrix/lu`, returning the
    combined `lu` factor as a Vulkan-placed Tensor and copying back only the
    `pivots` plus `swap-count` metadata required by the public dictionary
    contract.
20. Done: added dense row-major Vulkan `Float64` `matrix/inverse`, returning
    a Vulkan-placed inverse Tensor and raising `tensor/singular-matrix` for
    singular inputs.
21. Done: added dense row-major Vulkan `Float64` `matrix/solve`, accepting
    Vulkan-placed rank-1 or rank-2 right-hand tensors, returning a
    Vulkan-placed solution Tensor, and raising `tensor/singular-matrix` for
    singular systems.
22. Done: split the remaining Vulkan math-library direction into
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`, including
    helper/library factoring, dtype expansion prerequisites, layout expansion
    prerequisites, and a separate `TENSOR-100G` parallel solver track.
23. Done: added dense row-major Vulkan `Float64` `matrix/cholesky`, returning
    a Vulkan-placed lower factor Tensor and raising
    `tensor/not-positive-definite` for nonsymmetric or non-SPD inputs.
24. Done: added dense row-major Vulkan `Float64` `matrix/qr`, returning
    Vulkan-placed reduced `q` and `r` factor Tensors and raising
    `tensor/singular-matrix` for rank-deficient inputs.
25. Superseded by `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`:
    continue the dense row-major `Float64` Vulkan library as `TENSOR-100F`,
    while treating the measured `TENSOR-100G` parallel-solve baseline as
    closed until a separate performance item is justified.
26. FFT-specific acceleration may use `VkFFT` later if an FFT surface lands.

## External Tool Policy

Allowed as narrow shader/build tooling:

- `shaderc` or `glslang` for SPIR-V production,
- `spirv-val` for SPIR-V validation,
- `clspv` if OpenCL-C-style kernel authoring becomes useful.

Allowed as narrow domain libraries:

- `VkFFT` for an FFT-specific future surface after an FFT public contract
  exists.

Do not make Kompute, ncnn, MNN, ggml, or another framework the core Omni Tensor
backend abstraction. They are references or optional domain-specific tools, not
the language runtime contract.

The broader owned-kernel strategy is tracked in
`docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.

## Validation Policy

The Vulkan lane must pass on machines without Vulkan visibility. Tests that
require a Vulkan device should branch on `tensor-backends` availability and
assert either the Vulkan result or the expected fail-closed backend diagnostic.

Required validation as the lane grows:

- build with no Vulkan SDK link dependency,
- helper archive rebuild,
- `tensor-backends` inventory on Vulkan-visible and Vulkan-unavailable hosts,
- copy roundtrip smokes once placement lands,
- SPIR-V `map` arithmetic smokes on Vulkan-visible hosts,
- SPIR-V `matrix/transpose` smokes on Vulkan-visible hosts,
- SPIR-V `matrix/diagonal` and `matrix/diagonal-matrix` smokes on
  Vulkan-visible hosts,
- SPIR-V `matrix/trace` scalar-result smokes on Vulkan-visible hosts,
- SPIR-V `matrix/rank` scalar-result smokes on Vulkan-visible hosts,
- SPIR-V `matrix/lu` factorization smokes on Vulkan-visible hosts,
- SPIR-V `matrix/solve` Tensor-result smokes on Vulkan-visible hosts,
- SPIR-V `matrix/determinant` scalar-result smokes on Vulkan-visible hosts,
- SPIR-V `matrix/inverse` Tensor-result smokes on Vulkan-visible hosts,
- SPIR-V `matrix/cholesky` Tensor-result smokes on Vulkan-visible hosts,
- SPIR-V `matrix/qr` dictionary/Tensor-result smokes on Vulkan-visible hosts,
- SPIR-V `matrix/norm` scalar-result smokes on Vulkan-visible hosts,
- SPIR-V rank-1 dot smokes on Vulkan-visible hosts,
- SPIR-V generic rank-N multi-axis contract smokes on
  Vulkan-visible hosts,
- focused Tensor tests,
- bounded container tests that do not require GPU visibility,
- memory-lifetime smoke for opaque buffer finalizers once buffers land.

## Current Status

`TENSOR-100E` now has runtime-loaded `libvulkan` probing, a structured
`tensor-backends` entry, explicit Vulkan placement for concrete `Float64`
Tensor storage, and the first SPIR-V compute kernels. `to-device 'vulkan`
copies CPU Tensor storage into an opaque host-visible coherent Vulkan buffer
when Vulkan is usable, and `to-device 'cpu` copies Vulkan storage back to
native CPU Tensor storage. `map` supports dense row-major Vulkan `Float64`
elementwise arithmetic `+`, `-`, `*`, and `/` for Tensor/scalar,
scalar/Tensor, exact-shape Tensor/Tensor inputs, and right-aligned
singleton-axis Tensor/Tensor broadcasting through embedded SPIR-V kernels,
returning Vulkan-placed tensors. `matrix/transpose` supports Vulkan-placed
dense row-major `Float64` rank-2 tensors through an embedded SPIR-V kernel,
returning a Vulkan-placed transposed Tensor. `matrix/diagonal` and
`matrix/diagonal-matrix` support Vulkan-placed dense row-major `Float64`
inputs through embedded SPIR-V structural kernels, returning Vulkan-placed
Tensor results. `matrix/trace` supports Vulkan-placed dense row-major square
`Float64` inputs through an embedded SPIR-V reducer and reads back only the
scalar result required by the public scalar return contract. `matrix/rank`
supports Vulkan-placed dense row-major `Float64` rank-2 inputs through an
embedded SPIR-V reducer and reads back only the scalar `Integer` result
required by the public scalar return contract. `matrix/lu` supports
Vulkan-placed dense row-major square `Float64` inputs through an embedded
SPIR-V partial-pivot factorization kernel, returns the combined `lu` factor as
a Vulkan-placed Tensor, and reads back only the `pivots` plus `swap-count`
metadata required by the public dictionary contract. `matrix/determinant` supports
Vulkan-placed dense row-major square `Float64` inputs through an embedded
SPIR-V reducer and reads back only the scalar `Float64` result required by the
public scalar return contract. `matrix/solve` supports Vulkan-placed dense
row-major square `Float64` coefficient tensors with Vulkan-placed rank-1 or
rank-2 `Float64` right-hand tensors through an embedded SPIR-V
Gaussian-elimination kernel, returns Vulkan-placed solution tensors, and
raises `tensor/singular-matrix` for singular systems. `matrix/inverse`
supports Vulkan-placed dense
row-major square `Float64` inputs through an embedded SPIR-V Gauss-Jordan
kernel, returns a Vulkan-placed inverse Tensor, and raises
`tensor/singular-matrix` for singular inputs.
`matrix/cholesky` supports Vulkan-placed dense row-major square `Float64`
inputs through an embedded SPIR-V Cholesky factorization kernel, returns a
Vulkan-placed lower factor Tensor, and raises `tensor/not-positive-definite`
for nonsymmetric or non-SPD inputs.
`matrix/qr` supports Vulkan-placed dense row-major rank-2 `Float64` inputs
with rows greater than or equal to columns through an embedded SPIR-V reduced
QR kernel, returns Vulkan-placed `q` and `r` factor Tensors, and raises
`tensor/singular-matrix` for rank-deficient inputs. `matrix/norm`
supports Vulkan-placed dense row-major `Float64` inputs for
default/`'frobenius`, `'one`, `'infinity`, `'max`, `'spectral`, and
`'nuclear`; scalar selectors read back only the scalar result required by the
public scalar return contract, and SVD-backed selectors route through the
storage-backed Vulkan singular-values helper, including `k > 64`, without
hidden CPU/LAPACK fallback. Public `contract`
supports Vulkan-placed
dense row-major `Float64` tensors of arbitrary rank with one or more explicit
contracted axis pairs through embedded SPIR-V kernels; output axes are ordered
as the free left axes followed by the free right axes. Results remain
Vulkan-placed tensors.
Callers must still copy results back explicitly with `to-device 'cpu` before
CPU inspection. The generic contract kernel is generated from checked-in GLSL
and uses rank/shape/stride/axis-list metadata-buffer dispatch.

Missing or unusable Vulkan fails with `tensor/backend-unavailable`;
unsupported dtypes, unsupported Vulkan map callables, incompatible map
broadcast shapes, mixed CPU/Vulkan operands, and unsupported Vulkan layouts
fail with `tensor/backend-unsupported`. Zero-axis contractions and zero-size
contracted axes in supported Vulkan layouts preserve the CPU Tensor oracle.
Ordinary Tensor operations still do not silently transfer between CPU and GPU.
The dtype/layout policy is recorded in
`docs/plans/vulkan-dtype-layout-policy-2026-04-17.md`: keep extending
fixed-width `Float64` dense row-major kernels first; do not downcast to
`Float32`; do not lower pointer-backed Big* Tensor dtypes to Vulkan; defer
fixed-width complex and stride-aware layouts until those language/runtime
contracts exist.

The forward Vulkan math plan is recorded in
`docs/plans/vulkan-math-library-roadmap-2026-04-17.md`. It treats the current
serial Vulkan `matrix/solve`, `matrix/lu`, and `matrix/inverse` shaders as
correctness-preserving small-system/backend bring-up paths, not as the
performance solver target. The first thresholded parallel `matrix/solve`
helper has landed as a single-workgroup implementation with parallel pivot
search, row swaps, elimination, and RHS-column back-substitution while
preserving the same public backend-neutral contract. The remaining performance
track is a staged/tiled multi-kernel helper with explicit Vulkan barriers and a
measurement-backed threshold.
