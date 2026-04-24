# Deep Audit Report: Vulkan, CUDA, and ML/Autograd Subsystems

**Date:** 2026-04-23
**Scope:** `csrc/tensor_*.c`, `csrc/tensor_*.cu`, `src/lisp/prim_tensor_*.c3`, `src/lisp/prim_ml_*.c3`, `src/lisp/prim_nn_*.c3`
**Focus:** Shader dispatch correctness, gradient computation, numerical stability, tensor graph execution, backend mismatches, resource leaks, missing error handling.

---

## Remediation Update — 2026-04-24

Closed:
- C1-C6 are fixed. Shared Vulkan context publication/refcount updates are now
  mutex-protected, and Vulkan 1D dispatch group counts use a checked helper
  across the backend helper surface instead of wrapping `uint32_t` addition.
- H1-H3 are fixed. Adam bias corrections reject subnormal denominators at the
  Lisp and native CUDA/Vulkan boundaries, CUDA optimizer PTX was regenerated,
  and CUDA complex trace kernels now guard `out[0]` writes to one thread with
  regenerated embedded PTX.
- M1-M6 and L1-L5 are fixed or resolved by explicit fail-closed contracts.
  See `docs/todo_parts/todo_part_17.md` and the
  `2026-04-24 Vulkan/CUDA/ML audit residual closure` changelog entry for the
  per-item shipped contracts and validation.

Validation:
- `git diff --check` passed for the working tree.
- C3 diagnostics passed for touched optimizer/test C3 files.
- `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75` regenerated the CUDA
  optimizer and complex-matrix PTX includes.
- `c3c build` passed.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite all` passed.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` passed with `2071 passed, 0 failed`.

Still open:
- None. `docs/todo_parts/todo_part_17.md` is closed and `TODO.md` reports no
  live blocker queue.

---

## Critical

| # | File:Line | Description |
|---|-----------|-------------|
| C1 | `csrc/tensor_vulkan_helpers_core_context.inc:156-165` | Race condition in `omni_tensor_backend_vulkan_get_shared_context`: double-checked locking without memory barriers or mutex. A thread can observe a non-null `omni_tensor_vulkan_shared_context` before `instance`/`device` fields are fully initialized, causing use of a partially constructed Vulkan context. |
| C2 | `csrc/tensor_vulkan_helpers_dispatch_basic.c:96` | `group_count` computation `(uint32_t)output_element_count + local_size - 1` can wrap around when `output_element_count` is near `UINT32_MAX`, causing under-dispatch (not all elements processed). |
| C3 | `csrc/tensor_vulkan_helpers_dispatch_basic.c:219` | Same `uint32_t` overflow as C2 in `dispatch_two_buffer_f32`. |
| C4 | `csrc/tensor_vulkan_helpers_dispatch_basic.c:353` | Same `uint32_t` overflow as C2 in `dispatch_three_buffer_f64`. |
| C5 | `csrc/tensor_vulkan_helpers_dispatch_basic.c:487` | Same `uint32_t` overflow as C2 in `dispatch_three_buffer_f32`. |
| C6 | `csrc/tensor_vulkan_helpers_ml_reduction.c:325` | Same `uint32_t` overflow as C2 in ML reduction dispatch. |

---

## High

| # | File:Line | Description |
|---|-----------|-------------|
| H1 | `csrc/tensor_cuda_ml_optimizer.cu:60-61` | Adam kernel divides `next_first / first_correction` and `next_second / second_correction` without guarding against subnormal denominators; when host-side `first_correction`/`second_correction` are subnormal and cast to `float`, the division can produce `Inf`, exploding parameter updates. |
| H2 | `src/lisp/prim_ml_optimizer.c3:269` | Host-side Adam bias-correction validation checks `<= 0.0` and `is_finite` but does not reject subnormal values, which allows the kernel overflow described in H1. Same issue in `prim_ml_optimizer_cuda.c3:551` and `prim_ml_optimizer_vulkan.c3:220`. |
| H3 | `csrc/tensor_cuda_complex_matrix.cu:115-145` | `omni_cuda_trace_complex128` and `omni_cuda_trace_complex64` kernels write to `out[0]` without a thread guard; if launched with >1 thread, all threads race on the same 16-byte/8-byte write, potentially tearing the result. |

---

## Medium

| # | File:Line | Description |
|---|-----------|-------------|
| M1 | `src/lisp/prim_nn_apply.c3:598-640` | Batch-normalization training forward (`nn_forward_batch_normalization`) forces input and running stats to CPU, computes batch stats on CPU, and creates new CPU tensors for updated running state. On a GPU-initialized model this silently migrates running state to CPU without warning. |
| M2 | `csrc/tensor_cuda_scientific_unary.cu:17-87` | `omni_cuda_map_scientific_f64`/`f32` kernels do not validate domain for `log`, `asin`, `acos`, etc.; passing out-of-domain values produces NaN silently without setting `status` (only op 20, quantile, reports errors). |
| M3 | `src/lisp/prim_ml_autograd_tensor_expr_losses.c3:303` | Softmax cross-entropy CPU backward computes `softmax = ml_c_exp(logit - maxes[...]) / exp_sums[...]`; if all shifted logits underflow to zero, `exp_sums[result_index]` can be `0.0`, causing division by zero. |
| M4 | `src/lisp/prim_ml_vulkan_losses.c3` | `ml_cross_entropy_eval_vulkan` only supports `Float32`, while `ml_mean_squared_error_eval_vulkan` supports both `Float64` and `Float32` — inconsistent backend dtype coverage. |
| M5 | `src/lisp/prim_ml_autograd.c3` | Generic `ml_grad_resolve_cpu` rejects Vulkan backward paths, but specific Vulkan backward implementations exist elsewhere (`prim_ml_autograd_vulkan_broadcast.c3`, etc.). This creates a coverage gap where some autograd expressions fall back to CPU unnecessarily. |
| M6 | `csrc/tensor_vulkan_helpers_core.c:220-221` | `omni_vulkan_get_physical_device_features` and `omni_vulkan_get_physical_device_memory_properties` are called without null-pointer guards inside `omni_tensor_vulkan_probe`, unlike the guarded call to `omni_vulkan_get_physical_device_properties` on line 217. If library resolution ever partially succeeds, this is a null-deref risk. |

---

## Low

| # | File:Line | Description |
|---|-----------|-------------|
| L1 | `src/lisp/prim_ml_autograd_tensor_expr.c3:677-679` | Division backward checks `b == 0.0` but `b * b` can still underflow to `0.0` for subnormal `b`, causing `u * a / (b * b)` to divide by zero after the explicit zero check. |
| L2 | `src/lisp/prim_nn_init.c3` | `nn_rng_normal` Box-Muller transform uses uniform samples with a minimum of `~1.16e-10`, which can produce outliers up to `~6.76 sigma` — larger than typical Box-Muller implementations clamp to. |
| L3 | `csrc/tensor_cuda_rounding_i64.cu:31-38,54-63` | Error status reporting uses `atomicCAS(status, 0u, code)` instead of `atomicMax`; in a multi-threaded failure scenario the first error wins rather than the highest-priority error, making debugging harder. |
| L4 | `src/lisp/prim_ml_autograd_tensor_expr.c3` | `ml_grad_map_unary_derivative` sqrt check `x > 0.0` is sensitive to negative zero (`-0.0 > 0.0` is false), which could trigger the zero-branch for `-0.0` inputs. |
| L5 | `csrc/tensor_vulkan_helpers_core.c:319` | `omni_vulkan_get_physical_device_memory_properties` is called without a null check in `omni_tensor_vulkan_find_host_visible_memory_type`. |

---

## Verified Non-Issues (from prior agent claims)

- **C3 implicit `break` in `switch`:** All "missing break" claims across ~15 files are false positives; C3 enforces implicit breaks.
- **Vulkan empty-solve null pointers:** Already fixed in a prior commit.
- **Tensor graph zero-byte validation:** Already fixed in prior audit.
- **Descriptor binding mismatch in softmax:** `tensor_vulkan_helpers_ml_softmax.c` correctly passes metadata as the second buffer to `dispatch_three_buffer_f32`; no mismatch found.
- **Missing default cases in C3 switches:** C3's switch exhaustiveness checker handles this; no runtime issue.

---

## Summary by Category

| Category | Critical | High | Medium | Low |
|----------|----------|------|--------|-----|
| Vulkan dispatch / race | 6 | 0 | 1 | 1 |
| CUDA numerical / kernel | 0 | 3 | 1 | 1 |
| ML/Autograd numerical | 0 | 0 | 3 | 2 |
| Backend mismatch | 0 | 0 | 2 | 0 |
| **Total** | **6** | **3** | **6** | **5** |

---

*Report compiled from live source analysis. Critical, High, Medium, and Low findings were remediated or resolved by explicit fail-closed contracts on 2026-04-24.*
