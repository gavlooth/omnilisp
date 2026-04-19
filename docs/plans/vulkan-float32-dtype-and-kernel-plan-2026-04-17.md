# Vulkan Float32 Dtype And Kernel Plan

Date: 2026-04-17
Status: Phase 1 CPU `Tensor Float32` storage and CPU matrix factor/SVD oracles landed; Vulkan placement/map/unary/minmax, Float32 scientific unary, rank-N contract, structural matrix, direct reducer, SVD-backed, serial factor/solve, staged parallel solve, and large-dense SVD robustness slices landed

## Decision

`Float32` is a separate native Tensor dtype, not a Vulkan fallback for existing
`Float64` tensors. Vulkan must not downcast `Float64` inputs to `Float32` to
gain device coverage or performance.

The work splits into two phases:

1. define and implement native CPU/runtime `Float32` Tensor semantics
   (landed for Tensor storage/map/contract/conversion/structural matrix CPU
   paths and CPU matrix factor/SVD oracle paths on 2026-04-17);
2. add Vulkan `Float32` kernels behind the same backend-neutral Tensor and
   `matrix/*` surfaces once the dtype exists.

The first phase-2 slices have landed for placement/copyback, destination
`realize`, dense row-major elementwise `map`, unary helpers including the
`Float32` scientific unary family, direct `min`/`max`,
rank-N dense row-major `contract` including zero-axis and zero-size semantics,
and structural matrix kernels (`transpose`, `diagonal`, `diagonal-matrix`,
`trace`), direct reducers (`matrix/rank` and `matrix/norm` selectors), direct
`matrix/singular-values`, direct `matrix/svd`, serial factor/solve, and
staged parallel solve
surfaces (`matrix/determinant`, `matrix/lu`, `matrix/solve`,
`matrix/inverse`, `matrix/cholesky`, and `matrix/qr`). Staged parallel
`Float32` solve now uses dedicated shader entry points plus an explicitly
dtype-aware shared dispatch ABI. Large-dense `Float32` SVD robustness is
covered by scale-aware eigenvalue tolerance plus orthonormal completion in the
Vulkan `Float32` singular-values/SVD shaders.

## Phase 1: Native Tensor Contract

The public native `Float32` Tensor contract is:

- dtype name and constructor syntax are explicit and documented;
- CPU Tensor storage is homogeneous 32-bit IEEE floating point;
- explicit `Tensor Float32` construction narrows finite numeric inputs to
  32-bit storage and rejects out-of-range or non-finite data;
- `ref`, `Array`, `List`, and `Iterator` expose elements as scalar `Float32`
  values through the scalar `Float32` runtime tag;
- `map`, `contract`, `realize`, and `to-device 'cpu` preserve Tensor dtype and
  have CPU oracle tests;
- mixed `Float32`/`Float64` Tensor `map` rejects with `tensor/dtype-mismatch`;
- structural matrix operations (`matrix/transpose`, `matrix/diagonal`,
  `matrix/diagonal-matrix`, `matrix/identity`, `matrix/trace`) support
  `Float32`;
- direct CPU reducers support `Float32` for `matrix/rank` and `matrix/norm`
  default/`'frobenius`, `'one`, `'infinity`, and `'max`;
- CPU `Float32` matrix factor/SVD oracle paths support `matrix/norm`
  `'spectral`/`'nuclear`, `matrix/singular-values`, `matrix/svd`,
  `matrix/determinant`, `matrix/lu`, `matrix/solve`, `matrix/inverse`,
  `matrix/cholesky`, and `matrix/qr` without calling the Float64 LAPACK
  helpers or silently widening Tensor outputs.

Recommended public spelling: `Float32`, matching the existing `Float64` naming
style. Do not add aliases such as `float`, `f32`, or backend-specific dtype
names unless the owner explicitly approves them.

## Phase 2: Vulkan Kernel Contract

Vulkan may support `Float32` only when `tensor-backends` reports the required
device capability and the operation preserves the public `Float32` semantics.

The Vulkan `Float32` path mirrors the mature `Float64` dense row-major path in
this order:

1. explicit `to-device 'vulkan` placement and `to-device 'cpu` copyback
   (landed);
2. `map` arithmetic, unary helpers, and direct `min`/`max` (landed);
3. rank-N `contract`, including zero-axis and zero-size semantics (landed);
4. structural matrix kernels such as transpose, diagonal, diagonal-matrix, and
   trace (landed);
5. direct reducers (`matrix/rank` and `matrix/norm` default/`'frobenius`,
   `'one`, `'infinity`, `'max`) once CPU `Float32` oracles and tolerances are
   stable (landed);
6. SVD-backed reducers and direct SVD output kernels after dedicated `Float32`
   singular-value oracles and tolerances are stable (landed for Vulkan dense
   row-major inputs);
7. serial factor/solve kernels for `matrix/determinant`, `matrix/lu`,
   `matrix/solve`, `matrix/inverse`, `matrix/cholesky`, and `matrix/qr` after
   dedicated `Float32` factorization oracles and tolerances are stable
   (landed);
8. staged parallel `Float32` solve/performance parity after dedicated or
   explicitly dtype-aware staged helper contracts, thresholds, and tolerances
   are validated (landed with a `65` parity threshold);

`Float32` Vulkan support should use distinct shader entry points, helper ABI
names, capability bits, tolerances, and tests where precision differences
matter. It must not reuse `Float64` helper names with an implicit element-size
switch unless the ABI makes dtype explicit.

## Validation Gate

For the native dtype phase:

- `c3c build --obj-out obj`;
- focused CPU Tensor dtype tests for construction, indexing, printing,
  conversion, `map`, `contract`, matrix factor/SVD oracles, and explicit
  device roundtrips;
- docs/spec parity for public dtype wording;
- `git diff --check`.

For each Vulkan `Float32` kernel slice:

- shader compile and `spirv-val`;
- `./scripts/build_omni_chelpers.sh`;
- `c3c build --obj-out obj`;
- `tensor-backends` capability reporting tests;
- focused public smokes for concrete and lazy `Float32` Vulkan operands;
- explicit no-downcast tests proving `Float64` operands still use `Float64`
  helpers or fail closed;
- explicit CPU copyback comparisons using `Float32` tolerances;
- focused `advanced-collections-module`;
- `./scripts/check_primitive_docs_parity.sh`;
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`;
- `git diff --check`.

Validation recorded for the first Vulkan `Float32` slice:

- `glslangValidator -V --target-env vulkan1.0
  csrc/tensor_vulkan_map_f32.comp -o /tmp/omni_map_f32.spv`;
- `spirv-val --target-env vulkan1.0 /tmp/omni_map_f32.spv`;
- `glslangValidator -V --target-env vulkan1.0
  csrc/tensor_vulkan_map_unary_f32.comp -o
  /tmp/omni_map_unary_f32.spv`;
- `spirv-val --target-env vulkan1.0 /tmp/omni_map_unary_f32.spv`;
- `./scripts/build_omni_chelpers.sh`;
- `c3c build --obj-out obj`;
- direct Vulkan `Float32` placement/map/unary/minmax smokes;
- host focused `advanced-collections-module` `pass=1025 fail=0`;
- bounded-container focused `advanced-collections-module` `pass=1012
  fail=0`;
- primitive docs parity, Stage 3 source parity, and targeted
  `git diff --check`.

Validation recorded for the Vulkan `Float32` contract slice:

- `glslangValidator -V --target-env vulkan1.0
  csrc/tensor_vulkan_contract_f32.comp -o
  /tmp/omni_tensor_vulkan_contract_f32.spv`;
- `spirv-val --target-env vulkan1.0
  /tmp/omni_tensor_vulkan_contract_f32.spv`;
- `./scripts/build_omni_chelpers.sh`;
- `c3c build --obj-out obj`;
- direct Vulkan `Float32` contract smokes returned `154.0` for rank-2,
  `"Float32"` for result dtype, `vulkan` for result placement, `0.0` for
  zero-size dot, `460.0` for rank-N single-axis, `210.0` for multi-axis, `0`
  for zero-free output length, and `tensor/dtype-mismatch` for mixed
  `Float32`/`Float64` Vulkan operands;
- host focused `advanced-collections-module` `pass=1039 fail=0`;
- bounded-container focused `advanced-collections-module` `pass=1026 fail=0`;
- primitive docs parity, Stage 3 source parity, and targeted
  `git diff --check`.

Validation recorded for the Vulkan `Float32` structural matrix slice:

- `glslangValidator` / `spirv-val` for
  `csrc/tensor_vulkan_transpose_f32.comp`,
  `csrc/tensor_vulkan_diagonal_f32.comp`,
  `csrc/tensor_vulkan_diagonal_matrix_f32.comp`, and
  `csrc/tensor_vulkan_trace_f32.comp`;
- `./scripts/build_omni_chelpers.sh`;
- `c3c build --obj-out obj`;
- direct Vulkan `Float32` structural matrix smokes returned `6.0` for
  transpose copyback, `"Float32"` for diagonal result dtype, `0.0` for
  diagonal-matrix off-diagonal copyback, and `5.0` for scalar trace readback;
- host focused `advanced-collections-module` `pass=1059 fail=0`;
- bounded-container focused `advanced-collections-module` `pass=1046 fail=0`;
- primitive docs parity, Stage 3 source parity, and targeted
  `git diff --check`.

Validation recorded for the Vulkan `Float32` direct reducer slice:

- `glslangValidator` / `spirv-val` for
  `csrc/tensor_vulkan_norm_f32.comp` and
  `csrc/tensor_vulkan_rank_f32.comp`;
- `./scripts/build_omni_chelpers.sh`;
- `c3c build --obj-out obj`;
- direct CPU and Vulkan `Float32` smokes returned `2` for `matrix/rank`,
  `5.0` for `matrix/norm`, and `tensor/backend-unsupported` for
  `Float32` spectral/nuclear `matrix/norm`;
- host focused `advanced-collections-module` `pass=1098 fail=0`;
- bounded-container focused `advanced-collections-module` `pass=1085 fail=0`;
- primitive docs parity, Stage 3 source parity, and targeted
  `git diff --check`.

Validation recorded for the Vulkan `Float32` SVD-backed slice:

- `glslangValidator -V --target-env vulkan1.0
  csrc/tensor_vulkan_singular_values_f32.comp -o
  /tmp/omni_tensor_vulkan_singular_values_f32.spv`;
- `spirv-val --target-env vulkan1.0
  /tmp/omni_tensor_vulkan_singular_values_f32.spv`;
- `glslangValidator -V --target-env vulkan1.0
  csrc/tensor_vulkan_svd_f32.comp -o
  /tmp/omni_tensor_vulkan_svd_f32.spv`;
- `spirv-val --target-env vulkan1.0
  /tmp/omni_tensor_vulkan_svd_f32.spv`;
- `./scripts/build_omni_chelpers.sh`;
- `c3c build --obj-out obj`;
- direct Vulkan `Float32` smokes returned `3.0` for spectral norm, `2.0`
  for a copied-back singular value, and `"Float32"` for `matrix/svd` result
  dtype;
- host focused `advanced-collections-module` `pass=1121 fail=0`;
- bounded-container focused `advanced-collections-module` `pass=1108 fail=0`;
- primitive docs parity, Stage 3 source parity, stale-current docs scan, and
  targeted `git diff --check` passed.

Validation recorded for the Vulkan `Float32` large-dense SVD robustness slice:

- `glslangValidator` and `spirv-val` for
  `csrc/tensor_vulkan_singular_values_f32.comp` and
  `csrc/tensor_vulkan_svd_f32.comp`;
- `./scripts/build_omni_chelpers.sh`;
- `c3c build --obj-out obj`;
- direct Vulkan `Float32` smokes returned `(65 64.9999694824219 0.0)` for
  all-ones singular values,
  `(vulkan vulkan vulkan 64.9999694824219 0.0)` for direct SVD output
  placement and values, `(64.9999694824219 65.1197662353516)` for
  spectral/nuclear norms, `(65 1.0 1.0)` for identity singular values, and a
  Vulkan `Float32` all-ones reconstruction spot-check within `0.01`;
- host focused `advanced-collections-module` `pass=1177 fail=0`;
- bounded-container focused `advanced-collections-module` `pass=1164 fail=0`.

## Deferred Boundaries

These remain outside the landed CPU `Tensor Float32` slice:

- treating `Float32` as an automatic replacement for `Float64`;
- lowering `BigFloat` or `BigComplex` to `Float32`;
- fixed-width complex dtypes;
- strided or view-backed Vulkan inputs;
- CUDA `map`;
- performance-oriented tiled or multi-dispatch Vulkan `Float32` SVD beyond the
  now-validated dense row-major zero, identity, and all-ones `65x65`
  correctness path.

## Negative Constraints

- Do not downcast `Float64` tensors to `Float32` inside Vulkan dispatch.
- Do not add backend-specific dtype names.
- Report `Float32` Vulkan capability only when placement/copy paths and the
  listed real kernels are available; unsupported `Float32` operations must
  still fail closed.
- Do not silently widen `Float32` matrix routines to `Float64` and narrow the
  results unless that behavior is explicitly chosen as the public CPU contract.
- Superseded: the dense all-ones / rank-deficient `65x65` Vulkan `Float32`
  SVD failure is no longer a correctness blocker after scale-aware eigenvalue
  tolerance and orthonormal completion. Keep no-CPU-fallback and
  no-hidden-widening checks for large-dense SVD paths.
