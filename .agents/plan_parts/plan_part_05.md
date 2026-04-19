# Agent Plan Index Part 05

Source: `.agents/PLAN.md`

## Current Next Checkpoint

Use the integrated Tensor plan, `TODO.md`, and
`docs/plans/vulkan-math-library-roadmap-2026-04-17.md` as source of truth
before changing code.

1. Continue broader Vulkan math-library work. `TENSOR-100G` now has measured
   routing for the current staged `Float64` parallel solve path; full blocked
   trailing-update LU is a future performance lane only if later measurements
   justify it.
2. Keep the current serial Vulkan solve/LU/inverse shaders as
   correctness-preserving small-system/backend paths. The panel-factor staged
   solve path is the current large-system candidate at `n >= 65`.
3. Do not continue from the rolled-back generic `map` mode-3 unary branch:
   GLSL double transcendental builtins failed to compile for that path, and an
   arithmetic-only unary variant compiled but still failed at runtime. If unary
   Vulkan map is resumed, prefer a separately debugged unary shader/helper
   entrypoint or a deeper descriptor/dispatch diagnosis.
4. Treat mature serial helper plumbing as closed for now: compute-pipeline,
   descriptor, one-time command, and compatible `Float64` status-readback
   boilerplate are shared. Do not reopen those as the neutral baseline unless
   new duplication appears in a different helper shape.
5. Treat Vulkan `Float32` as partially active, not as a `Float64` fallback.
   Native CPU/runtime `Tensor Float32` exists, and Vulkan `Float32`
   placement/copyback, destination `realize`, dense row-major `map`, unary
   helpers, direct `min`/`max`, rank-N dense row-major `contract`, structural
   matrix kernels (`transpose`, `diagonal`, `diagonal-matrix`, `trace`), direct
   `matrix/rank`, direct `matrix/norm` selectors, `matrix/singular-values`,
   `matrix/svd`, serial factor/solve surfaces (`matrix/determinant`,
   `matrix/lu`, `matrix/solve`, `matrix/inverse`, `matrix/cholesky`, and
   `matrix/qr`), staged parallel `matrix/solve`, capability reporting, and
   no-downcast tests, large-dense SVD robustness, and CPU `Float32`
   factor/SVD surfaces have landed. CUDA `Float32` placement/copyback,
   destination `realize`, eligible cuBLAS contract routing, supported
   zero-size CUDA contract identity/fill, and CUDA rank-1/rank-1 dot have also
   landed. CPU fixed-width complex scalar and Tensor storage for
   `Complex128`/`Complex64` has landed. Remaining stride/view-backed Vulkan
   coverage, CUDA `map`, CUDA/Vulkan fixed-width complex kernels, and direct
   Vulkan general `matrix/eigenpairs` still require dedicated contracts and
   validation before they can leave fail-closed behavior. Do not lower
   `BigInteger`, `BigFloat`, or `BigComplex` to Vulkan, and do not treat real
   `float32`/`float64` backend capability bits as complex capability.
6. Preserve `Tensor`, `map`, `contract`, `matrix/*`, `to-device`, `device`,
   and `tensor-backends` as the public surface. Do not add `VulkanTensor`,
   `GpuTensor`, `CudaTensor`, backend-flavored math names, hidden CPU/GPU
   transfers, or boundary cloning of opaque non-CPU handles without explicit
   copy ownership.
6. Extend Boost.Math or scalar precision lanes only when there is a concrete
   next scientific function, distribution family, or precision contract.

Do not start by binding GSL. Do not implement `linalg/matmul` as canonical.
