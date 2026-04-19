# Session Report Index Part 17

Source: `.agents/SESSION_REPORT.md`

## 2026-04-16 20:05 CEST - TENSOR-100E Vulkan Probe And Placement

Objective attempted:
- Start the portable Vulkan Tensor backend lane, then carry it through the
  first real placement/copy slice while preserving the existing
  backend-neutral `Tensor` surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `docs/plans/vulkan-backend-decision-2026-04-16.md`.
- Added `csrc/tensor_vulkan_helpers.c`, a runtime-loaded `libvulkan` probe
  with no link-time Vulkan SDK dependency.
- Added `src/lisp/tensor_vulkan_backend.c3` extern bindings for availability,
  capability probing, CPU->Vulkan copies, Vulkan->CPU copies, and finalization.
- Wired the Vulkan helper into `project.json` and
  `scripts/build_omni_chelpers.sh`.
- Added a structured `vulkan` entry to `tensor-backends`, including explicit
  `float64` capability reporting.
- Added `TENSOR_DEVICE_VULKAN` placement metadata and public `device` reporting.
- Made `to-device 'vulkan` copy concrete `Float64` CPU Tensor storage into an
  opaque host-visible coherent Vulkan buffer when Vulkan is usable.
- Made `to-device 'cpu` copy Vulkan Tensor storage back to native CPU Tensor
  storage.
- Added deterministic Vulkan device-handle finalization through
  `tensor_free_payload`.
- Made Vulkan `contract` fail closed with `tensor/backend-unsupported` until
  contraction kernels land, instead of building lazy expressions over non-CPU
  storage.
- Added focused tests for the Vulkan inventory entry, availability-conditional
  Vulkan placement/roundtrip, unsupported dtype diagnostics, and fail-closed
  Vulkan `contract`.
- Added a regression proving zero-size Vulkan placement still follows backend
  availability.
- Added memory-lifetime coverage for opaque Vulkan device payload finalizers.

Key results:
- Vulkan is now visible as an optional backend capability without requiring a
  Vulkan link dependency.
- The one-time Vulkan probe suppresses noisy driver stderr and reports
  availability through `tensor-backends`.
- Vulkan placement/copy is implemented for concrete `Float64` Tensor storage
  through host-visible coherent buffers. Missing Vulkan reports
  `tensor/backend-unavailable`; unsupported dtypes report
  `tensor/backend-unsupported`.
- At this checkpoint, Vulkan compute kernels were not implemented yet.
  Superseded by the later SPIR-V map-kernel entry above; `contract` on
  Vulkan-placed operands still fails closed until the contraction kernel slice
  lands.
- Invalidated a helper assumption: host-visible non-coherent Vulkan memory is
  not enough for this placement path without explicit flush/invalidate support,
  so the helper requires host-visible coherent memory for now.
- Invalidated a zero-size shortcut: zero-size Vulkan placement must still
  probe backend availability and fail with `tensor/backend-unavailable` when
  Vulkan is missing.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Host focused `advanced-collections-module`: `652 passed, 0 failed`.
- Bounded-container focused `advanced-collections-module`: `639 passed, 0 failed`.
- Bounded-container full `advanced`: `1956 passed, 0 failed`.
- Bounded-container `memory-lifetime-smoke`: `227 passed, 0 failed`.
- `./scripts/build_omni_chelpers.sh`: passed.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- `c3c build --sanitize=address --obj-out obj`: blocked; C3 reported address
  sanitizer unsupported on this platform.

Current best recommendation / checkpoint:
- Superseded by the later SPIR-V map-kernel entry above. The next Vulkan compute
  step is reductions/rank-1 dot or rank-2 `contract`; keep CPU semantics as the
  oracle, keep `shaderFloat64` as a kernel capability gate, and keep all
  unsupported Vulkan compute paths fail-closed without hidden CPU/GPU transfers.

Unresolved issues:
- Superseded by the later SPIR-V map-kernel entry above: first Vulkan kernel
  dispatch now exists for `map +` add-scalar, but reductions and `contract`
  still need kernels.
- Vulkan `Float64` availability is reported as a kernel capability. Placement
  stores `Float64` bytes in host-visible coherent Vulkan buffers, while new
  compute kernels must continue gating on shader capability.
- ASAN remains unverified because the local C3 toolchain reports address
  sanitizer unsupported on this platform.

Signature: Codex GPT-5

## 2026-04-16 19:08 CEST - TENSOR-100D cuBLAS Contract

Objective attempted:
- Continue from explicit CUDA Tensor copies into the first cuBLAS execution
  path behind the existing `contract` surface.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Extended `csrc/tensor_cuda_helpers.c` with runtime-loaded cuBLAS resolution,
  availability probing, `dgemm`/`dgemv` symbol resolution, and per-operation
  handle lifecycle.
- Added cuBLAS externs in `src/lisp/tensor_cuda_backend.c3`.
- Added `cublas` to the structured `tensor-backends` inventory.
- Routed CUDA-placed dense row-major `Float64` rank-2/rank-2 single-axis
  contractions through `cublasDgemm_v2` for `[1 0]`, `[0 0]`, `[1 1]`, and
  `[0 1]`.
- Routed CUDA-placed dense row-major `Float64` rank-2/rank-1 and rank-1/rank-2
  single-axis contractions through `cublasDgemv_v2`.
- Added focused tests that assert cuBLAS results for all rank-2/rank-2,
  matrix-vector, and vector-matrix layouts when available and skip to CPU
  oracle values when unavailable.
- Added forced-unavailable cuBLAS regressions through the private backend
  disable hook, covering both `tensor-backends` inventory reporting and
  `tensor/backend-unavailable` contract failure.

Key results:
- Host `tensor-backends` reports CPU, CUDA, and cuBLAS available.
- Direct CUDA/cuBLAS contract smoke returned `154.0`, matching the CPU result
  for the tested 2x3 by 3x2 matrix multiply; focused tests also cover the
  transpose-backed `[0 0]`, `[1 1]`, and `[0 1]` rank-2/rank-2 layouts plus
  matrix-vector and vector-matrix `dgemv` layouts.
- Bounded-container validation still passes without requiring GPU visibility.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct CUDA/cuBLAS contract smoke: `154.0`.
- Host focused `advanced-collections-module`: `646 passed, 0 failed`.
- Bounded-container focused `advanced-collections-module`: `633 passed, 0 failed`.
- Bounded-container `memory-lifetime-smoke`: `226 passed, 0 failed`.
- Bounded-container full `advanced`: `1950 passed, 0 failed`.
- `./scripts/build_omni_chelpers.sh`: passed.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.
- `c3c build --sanitize=address --obj-out obj`: blocked; C3 reported address
  sanitizer unsupported on this platform.

Current best recommendation / checkpoint:
- The rank-2 CUDA/cuBLAS contract slice now covers all contiguous row-major
  single-axis layouts: `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`, plus
  matrix-vector/vector-matrix layouts, unsupported, mixed-device, zero-size
  fail-closed, and forced-unavailable paths. Further CUDA/cuBLAS work should
  add rank-1/rank-1 dot or broader dtype policy only if it preserves the
  backend-neutral `contract` surface and fail-closed diagnostics.

Unresolved issues:
- Only rank-2/rank-2, rank-2/rank-1, and rank-1/rank-2 `Float64` single-axis
  CUDA contract fast paths are implemented.
- No hidden CPU/GPU fallback is allowed for unsupported CUDA cases.
- Boundary clone/copy of non-CPU Tensor payloads remains intentionally
  unsupported until explicit copy ownership lands.

Signature: Codex GPT-5

## 2026-04-16 19:08 CEST - TENSOR-100C Optional CUDA Copy

Objective attempted:
- Continue from Tensor placement metadata into explicit CPU<->CUDA copy
  semantics without making CUDA a required build dependency.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `csrc/tensor_cuda_helpers.c`, a runtime-loaded CUDA helper using
  `dlopen`/`dlsym` for libcudart.
- Added `src/lisp/tensor_cuda_backend.c3` extern bindings.
- Wired the CUDA helper into `project.json` and
  `scripts/build_omni_chelpers.sh`.
- Updated `prim_to_device` so `'cuda` copies concrete `Float64` CPU Tensor
  storage to CUDA when available, and `'cpu` copies CUDA Tensor storage back to
  CPU.
- Strengthened focused tests to accept CUDA-visible and CUDA-unavailable
  environments and assert CUDA roundtrip values when available.

Key results:
- CUDA is optional and runtime-loaded; normal builds do not link libcudart.
- `tensor-backends` reports CUDA available only after symbol resolution, device
  count, and allocation/free probe succeed.
- On the host, CUDA was visible and a direct Tensor roundtrip returned `2.0`.
- In the bounded validation container, the same focused suite still passed.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Host focused `advanced-collections-module`: `633 passed, 0 failed`.
- Direct `tensor-backends` smoke: CPU available and CUDA available on host.
- Direct CUDA roundtrip smoke: `2.0`.
- Bounded-container focused `advanced-collections-module`: `619 passed, 0 failed`.
- Bounded-container `memory-lifetime-smoke`: `226 passed, 0 failed`.
- `git diff --check`: passed.

Invalidated assumption:
- `cudaGetDeviceCount` alone is not a sufficient CUDA-copy availability probe;
  it reported available before a copy smoke returned error values. Keep the
  allocation/free probe in backend availability.

Current best recommendation / checkpoint:
- Next CUDA/cuBLAS work should add cuBLAS handle lifecycle, then route the
  CUDA-placed `Float64` rank-2 `contract` `[1 0]` fast path with CPU fallback
  oracle comparison.

Unresolved issues:
- No cuBLAS handle or CUDA `contract` execution exists yet.
- Boundary clone/copy of non-CPU Tensor payloads remains intentionally
  unsupported until explicit copy ownership lands.
- GPU-heavy validation must stay on the bounded container path or another
  explicitly bounded GPU validation path.

Signature: Codex GPT-5

## 2026-04-16 19:08 CEST - TENSOR-100B Placement Metadata

Objective attempted:
- Continue from the CPU-only device surface into the next CUDA/cuBLAS enabling
  layer without adding a CUDA runtime dependency.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added internal Tensor placement metadata and opaque device handle/finalizer
  storage in `src/lisp/value_runtime_types.c3`.
- Initialized CPU placement in `src/lisp/value_tensor.c3`.
- Made `tensor_free_payload` the release authority for opaque Tensor device
  handles.
- Made Tensor boundary cloning refuse opaque non-CPU payloads until explicit
  copy semantics land.
- Made CPU Tensor storage validation reject non-CPU placement.
- Added public `tensor-backends` registration and AOT hash wiring.
- Added focused backend inventory tests and a memory-lifetime fake-device
  finalizer regression.

Key results:
- `device` now reports stored Tensor placement rather than a hardcoded symbol.
- `tensor-backends` returns structured backend dictionaries: CPU available,
  CUDA unavailable with reason `backend-unavailable`.
- Non-CPU Tensor storage cannot accidentally enter CPU kernel paths.
- Opaque device handles now have deterministic payload destruction coverage.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Host focused `advanced-collections-module`: `633 passed, 0 failed`.
- Direct `tensor-backends` smoke: returned CPU and CUDA dictionaries.
- Direct `to-device 'cpu` + `device` smoke: `"cpu"`.
- Host `memory-lifetime-smoke`: refused by the container-only slice policy.
- Bounded-container focused `advanced-collections-module`: `619 passed, 0 failed`.
- Bounded-container `memory-lifetime-smoke`: `226 passed, 0 failed`.

Current best recommendation / checkpoint:
- The next CUDA/cuBLAS slice should implement explicit CPU<->CUDA copy
  semantics for concrete `Float64` Tensor storage behind `to-device`, then add
  cuBLAS handle lifecycle. Keep CPU kernels fail-closed for non-CPU placement.

Unresolved issues:
- No CUDA backend/runtime code exists yet; CUDA movement still intentionally
  fails closed.
- Boundary clone/copy of non-CPU Tensor payloads is intentionally unsupported
  until explicit copy ownership lands.
- GPU-heavy validation must use the bounded container path when CUDA execution
  lands.

Signature: Codex GPT-5

## 2026-04-16 19:05 CEST - TENSOR-100A CPU Device Surface

Objective attempted:
- Continue from the CUDA/cuBLAS design closure into the first implementation
  layer that does not require CUDA libraries.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `device` and `to-device` primitives in `src/lisp/prim_tensor.c3`.
- Registered both primitives in `src/lisp/eval_init_primitive_tables.c3` and
  `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`.
- Added focused tests in `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Updated Tensor docs, area/plan docs, TODO, changelog, local plan, and this
  session report.

Key results:
- `device` reports `'cpu` for current Tensor values.
- `to-device` with target `'cpu` realizes Tensor expressions to CPU Tensor
  storage.
- `to-device` with target `'cuda` fails closed with
  `tensor/backend-unavailable`.
- No CUDA runtime dependency was added.

Commands run and key results:
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Focused `advanced-collections-module`: `629 passed, 0 failed`.
- Direct `device` smoke: `"cpu"`.
- Direct handled CUDA smoke: `"tensor/backend-unavailable"`.
- Bounded-container focused `advanced-collections-module`: `616 passed, 0 failed`.
- Bounded-container full `advanced` slice: `1933 passed, 0 failed`.
- `scripts/check_primitive_docs_parity.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- The next CUDA/cuBLAS implementation step should add concrete Tensor
  placement metadata for non-CPU storage and ownership/destruction tests for
  opaque CUDA buffers before adding cuBLAS execution.

Unresolved issues:
- No CUDA backend/runtime code exists yet; CUDA movement intentionally fails
  closed.
- Host monolithic `advanced` still exits without a summary; focused host groups
  and bounded-container full `advanced` pass, and the bounded path remains the
  policy gate for this work.
- GPU-heavy validation must use the bounded container path when CUDA execution
  lands.
- Existing `c3c build` entry deprecation warnings remain unrelated.
- The existing added `out` file contains unrelated reverse-SSH text and was
  left untouched.

Signature: Codex GPT-5
