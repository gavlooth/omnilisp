# Omni Lisp Comprehensive Audit Report — AUDIT_2

**Date:** 2026-04-24
**Auditor:** Crush
**Scope:** Full codebase (`src/`, `csrc/`, `scripts/`, build system, docs)
**Method:** 4 parallel agent passes (memory/lifetime, Vulkan/CUDA/ML/FFI, compiler/JIT/runtime, scripts/build) + manual verification
**Build status:** `c3c build --obj-out obj` passes; bounded container `lisp` suite: 169 passed, 0 failed

---

## Executive Summary

This audit combines **new findings** from a fresh comprehensive pass with **fix-status tracking** for issues reported in prior audits (2026-04-21 through 2026-04-24). The codebase is structurally sound but has several concrete defects that require attention.

| Severity | Pass 1 | Pass 2 | Pass 3 | Pass 4 | Pass 5 | Prior (Not Fixed) | Total |
|----------|--------|--------|--------|--------|--------|-------------------|-------|
| Critical | 4 | 6 | 4 | 4 | 8 | 0 | 26 |
| High | 6 | 8 | 7 | 5 | 12 | 0 | 38 |
| Medium | 18 | 13 | 15 | 12 | 15 | 10 | 83 |
| Low | 12 | 14 | 10 | 10 | 12 | 2 | 60 |

---

## 🔴 Critical (Fix Immediately)

### C1. Missing `break` in Tensor Map Scalar Assignment Corrupts All Non-Float32 Scalars

- **Files:**
  - `src/lisp/prim_tensor_map.c3:24-66` (left scalar)
  - `src/lisp/prim_tensor_map.c3:69-105` (right scalar)
- **Issue:** The `switch (dtype)` blocks that assign scalar values to `tensor.map_left_scalar` / `tensor.map_right_scalar` have **no `break` statements** between cases. For example, when `dtype == TENSOR_DTYPE_DOUBLE`, the `DOUBLE` case executes, then fallthrough executes the `FLOAT32` case, which overwrites `map_left_scalar` with a float32-rounded value. Further fallthrough to `COMPLEX128`, `COMPLEX64`, `BIG_INTEGER`, `BIG_FLOAT`, and `BIG_COMPLEX` overwrites additional scalar fields.
- **Impact:** Float64 scalars lose precision. Large Float64 values that fit in double but not in float32 cause `tensor_value_to_float32` to fail, making an otherwise valid `DOUBLE` scalar assignment error out. Big-integer/big-float/big-complex cases overwrite unrelated scalar fields.
- **Fix:** Add `break;` after each case body.

### C2. Missing `break` in Vulkan Direct Map Provenance Recording Corrupts Scalar Metadata

- **File:** `src/lisp/prim_tensor_vulkan_map_direct.c3:260-284`
- **Issue:** The provenance-recording `switch (dtype)` blocks for `map_left_scalar` and `map_right_scalar` also lack `break` statements. When `dtype == TENSOR_DTYPE_DOUBLE`, the double scalar is written, then the `FLOAT32` case overwrites it with `(double)scalar_f32` (which is `0.0f` because `scalar_f32` was never set for the double path). Further fallthrough writes to complex scalar fields.
- **Impact:** Graph provenance records incorrect scalar values, causing wrong results or silent data corruption when Vulkan expression graphs are re-evaluated or fused.
- **Fix:** Add `break;` after each provenance assignment case.

### C3. Vulkan Shared Context Use-After-Free

- **File:** `csrc/tensor_vulkan_helpers_core_context.inc:71-82`
- **Issue:** `omni_tensor_backend_vulkan_context_release` frees the shared context at line 82 but **never clears the global `omni_tensor_vulkan_shared_context` pointer**. After release, the dangling pointer is still stored globally. The next call to `omni_tensor_backend_vulkan_get_shared_context` (lines 177-210) sees the non-NULL dangling pointer, "retains" it by incrementing `ref_count` in freed memory, and returns the dangling pointer.
- **Impact:** Any subsequent use of the returned context pointer is a use-after-free. Crashes or Vulkan state corruption when buffers try to dispatch on a destroyed device.
- **Fix:** Set `omni_tensor_vulkan_shared_context = NULL` before freeing, or use a zombie flag. Hold the mutex during the entire destruction sequence.

### C4. e2e Baseline Policy Script Logic Bug Always Flags Review Rules

- **File:** `scripts/check_e2e_baseline_policy.sh:191`
- **Issue:** The condition uses `||` instead of `&&`:
  ```bash
  if [[ "$review_rule" != *"memory/CHANGELOG.md"* || "$review_rule" != *"docs/areas/types-dispatch.md"* ]]; then
  ```
  A single string cannot simultaneously contain both substrings, so this condition is **always true** and every row is written to `metadata_issues.txt` as `bad_review_rule`.
- **Impact:** The baseline policy script permanently fails on any valid ownership metadata, breaking the e2e gate.
- **Fix:** Change `||` to `&&`:
  ```bash
  if [[ "$review_rule" != *"memory/CHANGELOG.md"* && "$review_rule" != *"docs/areas/types-dispatch.md"* ]]; then
  ```

---

## 🟠 High (Fix in Next Sprint)

### H1. `stable_escape_materialize_init_closure` Leaks `closure` and Sub-Allocations on Failure Paths

- **File:** `src/lisp/eval_boundary_commit_destination.c3:166,174,181-183`
- **Issue:** `closure` is allocated at line 166 via `stable_escape_materialize_alloc_payload`. If `closure.params` allocation fails at line 174, or `closure.type_sig` allocation fails at line 182, or `method_signature_copy_to_escape` fails at line 183, the function returns `false` without freeing `closure` or any previously allocated sub-fields.
- **Fix:** Add a `defer` that frees `closure` and its sub-fields on failure, or use a success-flag pattern.

### H2. `jit_signal_try_fast_path_dispatch` Can Pass Null `prim` to Dereferencing Helper

- **File:** `src/lisp/jit_runtime_effects_signal.c3:108-113`
- **Issue:** `interp.fast_path_table[fp_i].prim` is passed directly to `jit_signal_apply_fast_path_prim`, which at line 63 dereferences `fp_prim.prim_val.arity` without null-checking `fp_prim` or `fp_prim.prim_val`.
- **Fix:** Add `if (fp_prim == null || fp_prim.prim_val == null) return null;` at the top of `jit_signal_apply_fast_path_prim`.

### H3. `jit_apply_continuation` Missing Null/Tag Check on `k_val`

- **File:** `src/lisp/jit_handle_signal_handle.c3:255-260`
- **Issue:** `jit_apply_continuation` dereferences `k_val.cont_val` without first checking `k_val != null` or `k_val.tag == CONTINUATION`.
- **Fix:** Add tag validation before union field access.

### H4. CUDA Diagonal Kernels Buffer Overflow

- **File:** `csrc/tensor_cuda_complex_matrix.cu:49-75`
- **Issue:** The diagonal extraction kernels access `input[index * cols + index]` without validating that `diagonal_count <= rows` and `diagonal_count <= cols`. A caller passing a `diagonal_count` larger than the smaller matrix dimension causes out-of-bounds reads.
- **Fix:** Add guard: `if (index >= rows || index >= cols) return;`.

### H5. Vulkan Buffer Handle Refcount Is Not Thread-Safe

- **Files:**
  - `csrc/tensor_vulkan_helpers_core_context.inc:85-98`
  - `csrc/tensor_vulkan_helpers.c:433-438`
- **Issue:** `omni_tensor_backend_vulkan_destroy_buffer_handle` and `omni_tensor_backend_vulkan_retain` manipulate `handle->ref_count` without any synchronization. Two threads retaining or releasing the same buffer concurrently can race on the refcount.
- **Impact:** Double-free or leak.
- **Fix:** Use atomic operations (`__atomic_fetch_add` / `__atomic_fetch_sub`) or a spinlock.

### H6. Vulkan Context Release Destroys Device Outside Mutex

- **File:** `csrc/tensor_vulkan_helpers_core_context.inc:71-82`
- **Issue:** `omni_tensor_backend_vulkan_context_release` drops the mutex at line 76 or 79, then destroys the Vulkan device and instance at lines 80-81. Between unlock and destroy, another thread can acquire the context and enqueue work.
- **Impact:** Queue submissions on a device being destroyed produce `VK_ERROR_DEVICE_LOST` or UB.
- **Fix:** Hold the mutex for the entire destruction sequence, or atomically transition the context to a "zombie" state inside the lock.

---

## 🟡 Medium (Fix When Touching Related Code)

### M1. `long.min` Overflow in Expression Serializer

- **File:** `src/lisp/compiler_expr_serialize_values.c3:76-79`
- **Issue:** `serialize_int_value_to_buf` negates a `long` without checking for `long.min`: `n = -n;` — UB when `n == long.min`.
- **Fix:** Use the `ulong mag` pattern already present in `compiler_output_helpers.c3:80`.

### M2. `long.min` Overflow in Shared String Helper

- **File:** `src/lisp/value_predicates_accessors_core.c3:14-18`
- **Issue:** `int_to_string` has the identical `val = -val` bug on `long.min`. This helper is used by dispatch error formatting, arity errors, and time-point formatting.
- **Fix:** Apply the `ulong mag` pattern or consolidate to a single helper.

### M3. `compile_expr` is Severely Non-Exhaustive

- **File:** `src/lisp/compiler_expression_compilation.c3:15-39`
- **Issue:** The AOT expression compiler only handles `E_LIT`, `E_VAR`, `E_DEFINE`, `E_QUOTE`, and `E_PATH`. All other ~25 `ExprTag` variants fall through to `default:` which silently emits `aot::make_nil()`.
- **Fix:** Remove `default:` and enumerate all cases with appropriate errors.

### M4. Default Cases Hiding Missing Enum Variants (Compiler Switches)

Adding a new `ExprTag` / `PatternTag` / `ValueTag` variant will silently fall through `default:` instead of triggering a compile error. Files affected:

| File | Line | Switch On | Default Behavior |
|------|------|-----------|------------------|
| `compiler_free_vars_walk.c3` | 52 | `ExprTag` | `return;` |
| `compiler_free_vars_walk_helpers.c3` | 42 | `ExprTag` (unary) | `return;` |
| `compiler_free_vars_walk_helpers.c3` | 59 | `ExprTag` (binary) | `return;` |
| `compiler_free_vars_walk_helpers.c3` | 94 | `ExprTag` (complex) | `return;` |
| `compiler_expr_serialize_exprs.c3` | 58 | `ExprTag` | `self.buf_append(buf, "nil");` |
| `compiler_expr_serialize_patterns.c3` | 84 | `PatternTag` | `buf.push('_');` |
| `compiler_mutable_capture_prescan.c3` | 155 | `ExprTag` | `return;` |
| `compiler_lambda_scan.c3` | 179 | `ExprTag` | `return false;` |
| `compiler_native_literal_compilation_flat_style.c3` | 138 | `ValueTag` | `self.emit("aot::make_nil()");` |
| `compiler_temp_type_forms_helpers.c3` | 22 | `ValueTag` | `self.emit("lisp::ValueTag.NIL");` |
| `compiler_statement_compilation.c3` | 41 | `ExprTag` | `self.compile_to_temp(expr);` |
| `compiler_quasiquote_flat.c3` | 116 | `ExprTag` | `return self.emit_nil_temp();` |
| `compiler_native_match_pattern_checks.c3` | 228 | `PatternTag` | `emit bool = true;` |
| `compiler_native_match_compilation_guards.c3` | 32 | `PatternTag` | `return false;` |
| `compiler_native_match_compilation_guards.c3` | 163 | `PatternTag` | `return;` |
| `compiler_native_match_bindings_flat_style.c3` | 130 | `PatternTag` | `return;` |
| `eval_pattern_equality.c3` | 56 | `ValueTag` | `return a == b;` |
| `eval_pattern_equality.c3` | 191 | `ValueTag` | `if (left != right) ok = false;` |
| `jit_apply_eval.c3` | 77 | `ExprTag` | `return false;` |
| `jit_apply_eval.c3` | 124 | `ExprTag` | `return false;` |
| `eval_dispatch_match_breakdown.c3` | 44 | `ValueLiteralKey` | `return false;` |
| `eval_dispatch_error_payloads.c3` | 28 | `DispatchMatchFailureReason` | `return "none";` |
| `eval_dispatch_error_payloads.c3` | 47 | `ValueLiteralKey` | `return make_nil(interp);` |

- **Fix:** Remove `default:` arms and add explicit cases for all current tags. Let the C3 compiler enforce exhaustiveness.

### M5. JIT `expr_contains_shift` / `expr_contains_perform` Missing `E_WITH_MODULE` Traversal

- **File:** `src/lisp/jit_apply_eval.c3:42-127`
- **Issue:** Neither function handles `E_WITH_MODULE`. A shift or perform inside a `with-module` form is missed, causing the JIT to treat the call as non-continuation-sensitive.
- **Fix:** Add `E_WITH_MODULE` cases that recursively check body expressions.

### M6. Missing Graceful Error Handling on Arena Allocation

- **File:** `src/lisp/compiler_call_flat.c3:23-30`
- **Issue:** `compile_when_unless_if` uses `assert()` for `alloc_expr()` and `ast_arena_alloc()` failures. If the AST arena is exhausted, the process aborts instead of returning a compile error.
- **Fix:** Check for null, set `self.has_error`, and return `self.emit_nil_temp()`.

### M7. `jit_shift_value` Leaks Continuation on `make_continuation` Failure

- **File:** `src/lisp/jit_runtime_effects_reset_shift.c3:97-103`
- **Issue:** `Continuation* k` is allocated and populated, but `make_continuation` can return null on OOM. If it does, `k` is leaked.
- **Fix:** Check `k_val == null` after `make_continuation` and discard `k` before returning the OOM error.

### M8. `jit_shift_impl` Leaks Continuation on `make_continuation` Failure

- **File:** `src/lisp/jit_reset_shift.c3:116-130`
- **Issue:** Same pattern as M7 — `k` is allocated but if `make_continuation` returns null, `k` is leaked.
- **Fix:** Check `k_val == null` and discard `k` before returning error.

### M9. `jit_handle_dispatch_signal` Missing Null Check for `k_val`

- **File:** `src/lisp/jit_handle_signal.c3:117-130`
- **Issue:** `make_continuation` can return null. The resulting null `k_val` is passed to `jit_env_extend`. The unregistered `k` is leaked.
- **Fix:** Check `k_val == null` after `make_continuation`, discard `k`, and return an OOM error.

### M10. FFI Callback Dispatch Data Race

- **File:** `src/lisp/prim_ffi_callback.c3:223-246`
- **Issue:** `ffi_callback_dispatch` is invoked by libffi on an arbitrary thread. It reads `ctx->open`, `ctx->callback`, `ctx->interp`, and `ctx->param_types` without synchronization. The GC/finalizer thread can concurrently call `ffi_callback_context_finalizer`, which sets fields to null/false and frees memory.
- **Impact:** Segfault if callback fires during finalization.
- **Fix:** Use an atomic flag or mutex around callback context fields. Finalizer should atomically mark context closed and wait for in-flight dispatches.

### M11. Unquoted Array Expansion in e2e Compile Command

- **File:** `scripts/run_e2e.sh:96`
- **Issue:** `${stage3_compile_sources[@]}` is unquoted, causing word-splitting if paths contain spaces.
- **Fix:** Quote the expansion: `"${stage3_compile_sources[@]}"`.

### M12. Docker Extra Args Exported Under Wrong Variable Name

- **Files:** `scripts/run_e2e.sh:52`, `scripts/run_deduce_perf_envelope.sh:50`
- **Issue:** Both scripts build `validation_extra` mounts and export them as `OMNI_VALIDATION_EXTRA_ARGS`, but `scripts/c3c_limits.sh` (`omni_run_with_docker_cap`) only reads `OMNI_DOCKER_EXTRA_ARGS`. The mounts are silently ignored.
- **Fix:** Export `OMNI_DOCKER_EXTRA_ARGS` instead.

### M13. C Compiler Hardcoded to `cc`, Ignores `$CC`

- **File:** `scripts/build_omni_chelpers.sh:278`
- **Issue:** The C compilation helper hardcodes `cc` while respecting `$CXX` for C++. Cross-compilation workflows break.
- **Fix:** Use `"${CC:-cc}"` instead of `cc`.

### M14. Wrong Env-Var Gate for memory-lifetime-bench Slice

- **File:** `scripts/run_global_gates.sh:96`
- **Issue:** The block that conditionally appends `memory-lifetime-bench` incorrectly checks `OMNI_GLOBAL_GATES_INCLUDE_ALLOCATOR_BENCH` again instead of a dedicated `OMNI_GLOBAL_GATES_INCLUDE_MEMORY_LIFETIME_BENCH`.
- **Fix:** Add the dedicated variable near line 14 and check it at line 96.

### M15. BLAS Symbol Resolution Only Guards `dgemm`

- **File:** `csrc/tensor_blas_helpers.c:94-99`
- **Issue:** `omni_tensor_blas_resolve` checks `if (dgemm_symbol != NULL)` and then assigns ALL symbols (`dgemv`, `ddot`, `dger`) without individual null checks.
- **Fix:** Check each symbol individually before assignment, or document the contract.

### M16. LAPACK Symbol Resolution Assigns All Symbols When Any Is Present

- **File:** `csrc/tensor_lapack_helpers.c:309-333`
- **Issue:** The `if` condition is a disjunction of all symbols. If any is non-NULL, all are assigned, including unchecked NULL pointers.
- **Fix:** Same as M15 — individual null checks or explicit contract documentation.

### M17. `eval_pattern_equality.c3` Missing Cases for Big-Number and Complex Types

- **File:** `src/lisp/eval_pattern_equality.c3:30-58`
- **Issue:** The first `switch (a.tag)` falls through to `default: return a == b;` for `COMPLEX128`, `COMPLEX64`, `BIG_INTEGER`, `BIG_FLOAT`, `BIG_COMPLEX`, `PRIMITIVE`, `PARTIAL_PRIM`, `CLOSURE`, `CONTINUATION`, `ERROR`, `TYPE_INFO`, `INSTANCE`, `METHOD_TABLE`, `MODULE`, `ITERATOR`, `COROUTINE`, `FFI_HANDLE`, `TENSOR`. For heap-allocated types like `BIG_INTEGER`, identity equality is wrong (two distinct values with the same numeric value compare unequal).
- **Fix:** Add explicit cases for all `ValueTag` variants. For `BIG_INTEGER` / `BIG_FLOAT` / `BIG_COMPLEX`, delegate to existing `big_*_values_equal` helpers.

### M18. Format String Mismatches in Boundary Telemetry / Logging

- **Files:**
  - `src/scope_region_temp_pool_stats.c3:260-264` — `%d` used with `usz` fields
  - `src/lisp/eval_boundary_graph_audit_logging.c3:14-75` — `%d` used with `(long)` casts
  - `src/lisp/eval_boundary_graph_audit_telemetry.c3` — same `%d`/`(long)` pattern
- **Status:** NOT FIXED from prior audit (M1).
- **Impact:** Silent truncation or UB on platforms where `long`/`usz` differ from `int`.
- **Fix:** Use `%zu` for `usz`, `%ld` for `long`.

---

## 🟢 Low (Code Hygiene)

### L1. `char[]` Slices Still Passed to `%s` Format Specifier

- **Files:**
  - `src/scope_region_global_guards.c3:54`
  - `src/lisp/eval_boundary_graph_audit_meta.c3:15-37`
  - `src/lisp/eval_boundary_diagnostics.c3:34`
  - `src/lisp/eval_boundary_graph_audit_telemetry.c3:39`
  - `src/lisp/eval_boundary_telemetry.c3:475`
- **Status:** NOT FIXED from prior audit (M2).
- **Impact:** `char[]` is a slice not guaranteed null-terminated; UB if it lacks trailing `\0`.
- **Fix:** Cast to `ZString` where the underlying string is known null-terminated, or change helpers to return `ZString`.

### L2. `ScopeRegion.alloc` / `alloc_escape` Still Missing `@require self != null`

- **File:** `src/scope_region_allocators.c3:89,111`
- **Status:** NOT FIXED from prior audit (M3).
- **Fix:** Add `@require self != null` to both functions.

### L3. `scope_reset_temp_lane` Still Leaves `alloc_bytes` / `alloc_count` Unchanged

- **File:** `src/scope_region_reset_helpers.c3:74-104`
- **Status:** NOT FIXED from prior audit (M4).
- **Fix:** Zero both fields before returning.

### L4. `scope_freelist_cleanup` Still Omits `g_scope_global_mu.destroy()`

- **File:** `src/scope_region_reset_adopt.c3:112-136`
- **Status:** NOT FIXED from prior audit (M5).
- **Fix:** Add `g_scope_global_mu.destroy()` after the last lock/unlock pair.

### L5. Generation Counter Wraparound Theoretical Risk

- **File:** `src/lisp/eval_boundary_provenance.c3:630-634`
- **Status:** NOT FIXED from prior audit (L1).
- **Fix:** Use a 64-bit generation counter or add an explicit epoch field.

### L6. Missing `@require` Contracts on Core Public APIs

- **Files:**
  - `src/scope_region_reset_adopt.c3:7` — `scope_splice_escapes`
  - `src/scope_region_chunk_helpers.c3:61` — `scope_register_dtor`
  - `src/scope_region_chunk_helpers.c3:75` — `scope_register_dtor_escape`
  - `src/scope_region_destroy.c3:98` — `scope_destroy_owned_descendants`
- **Status:** NOT FIXED from prior audit (L2).
- **Fix:** Add `@require scope != null` to each.

### L7. Code Duplication: Integer Serialization

- **Files:** `compiler_expr_serialize_values.c3:76-95`, `value_predicates_accessors_core.c3:14-31`, `compiler_output_helpers.c3:76-85`
- **Issue:** Three separate implementations of signed-long-to-string exist. Two have the `long.min` bug; one has the correct fix.
- **Fix:** Consolidate to a single `serialize_long_to_buf` utility.

### L8. Defensive Null Check Missing in `jit_signal_call_fast_path_primitive_preserving_error`

- **File:** `src/lisp/jit_runtime_effects_signal.c3:12-16`
- **Issue:** Accesses `fp_prim.prim_val.tag`, `.user_data`, and `.func` without checking `fp_prim.prim_val != null`.
- **Fix:** Add `if (fp_prim == null || fp_prim.prim_val == null) return null;`.

### L9. `boundary_graph_audit_visit_env` Lacks Defensive `env == null` Guard

- **File:** `src/lisp/eval_boundary_graph_audit_walkers.c3:5-79`
- **Issue:** The function assumes `env != null` when iterating `env.bindings` at line 55.
- **Fix:** Add `if (env == null) return { .ok = true, ... };` at the top.

### L10. Vulkan Barrier Helper Functions Dereference Without Null Check

- **File:** `csrc/tensor_vulkan_helpers_dispatch_multi_output.c:2-174`
- **Issue:** Static barrier helpers build `OmniVulkanBufferMemoryBarrier` structs by directly dereferencing `first_output->buffer`, `second_output->buffer`, etc. without checking for NULL.
- **Fix:** Add `if (first_output == NULL || second_output == NULL) return;` guards.

### L11. Vulkan `copy_range_to_host` Maps Entire Buffer for Small Ranges

- **File:** `csrc/tensor_vulkan_helpers.c:42-64`
- **Issue:** Maps the entire buffer (`handle->byte_len`) from offset 0 even when only a small range is requested.
- **Fix:** Map only the requested range.

### L12. `mktemp` Without Template (Portability)

- **File:** `scripts/run_global_gates.sh:191`
- **Issue:** `asan_build_log="$(mktemp)"` omits the required template argument. BSD/macOS `mktemp` requires a template.
- **Fix:** `asan_build_log="$(mktemp -t omni_asan_build.XXXXXX)"`.

---

## Prior-Audit Fix Status Summary

| Finding | Original Audit | Status | File(s) |
|---------|---------------|--------|---------|
| M1 — `%d`→`%zu`/`%ld` format mismatches | 2026-04-24 | **NOT FIXED** | `src/scope_region_temp_pool_stats.c3`, `src/lisp/eval_boundary_graph_audit_*.c3` |
| M2 — `char[]`→`%s` UB | 2026-04-24 | **NOT FIXED** | `src/scope_region_global_guards.c3`, `src/lisp/eval_boundary_graph_audit_meta.c3` |
| M3 — `@require self != null` on alloc/alloc_escape | 2026-04-24 | **NOT FIXED** | `src/scope_region_allocators.c3` |
| M4 — `scope_reset_temp_lane` telemetry drift | 2026-04-24 | **NOT FIXED** | `src/scope_region_reset_helpers.c3` |
| M5 — Missing `g_scope_global_mu.destroy()` | 2026-04-24 | **NOT FIXED** | `src/scope_region_reset_adopt.c3` |
| M6 — BLAS symbol resolution | 2026-04-24 | **NOT FIXED** | `csrc/tensor_blas_helpers.c` |
| M7 — LAPACK symbol resolution | 2026-04-24 | **NOT FIXED** | `csrc/tensor_lapack_helpers.c` |
| M8 — Vulkan queue family return unchecked | 2026-04-24 | **NOT FIXED** | `csrc/tensor_vulkan_helpers_core.c:237` |
| M9 — Default cases in non-exhaustive switches | 2026-04-24 | **NOT FIXED** | ~15 compiler/AOT files |
| L1 — Generation wraparound | 2026-04-24 | **NOT FIXED** | `src/lisp/eval_boundary_provenance.c3` |
| L2 — Missing `@require` on public APIs | 2026-04-24 | **NOT FIXED** | `src/scope_region_*.c3` |
| C1 — JIT reset/shift StackCtx leak | 2026-04-23 | **FIXED** | `src/lisp/jit_reset_shift.c3` |
| H3/H4 — Continuation leaks on JIT failure | 2026-04-23 | **FIXED** | `src/lisp/jit_handle_signal.c3`, `jit_runtime_effects_handle.c3` |
| H1 — UI FTXUI partial-failure leak | 2026-04-23 | **NOT REPRODUCIBLE** | `src/lisp/prim_ui_ftxui.c3` |
| H2 — CUDA `atomicCAS` error semantics | 2026-04-23 | **FIXED** | `csrc/tensor_cuda_complex_map.cu` |
| PAT_GUARD serialization | 2026-04-23 | **FIXED** | `src/lisp/compiler_expr_serialize_patterns.c3` |
| Mutable capture detection | 2026-04-23 | **FIXED** | `src/lisp/compiler_mutable_capture_*.c3` |

---

## Second Pass — New Findings (2026-04-24)

**Scope:** Parser, evaluator core, value lifecycle, AOT runtime, async I/O, REPL/server, type system, dispatch, test infrastructure.
**Method:** 4 parallel agent passes + manual verification.

---

### 🔴 Critical (New)

#### C5. Parser String Escape Sequence Switch Missing `break` on Every Case

- **File:** `src/lisp/parser_lexer_string_hash.c3:30-62`
- **Issue:** The `switch (escaped)` block handling `\"`, `\\n`, `\\r`, `\\t`, `\\`, `\"` has **no `break` between any case**. A single `\\n` escape falls through and also emits `\\r`, `\\t`, `\\`, `\"`, and the raw escaped character.
- **Impact:** Corrupts every string literal containing escape sequences. `"hello\\nworld"` becomes something like `"hello\\n\\r\\t\\\\\"world"`.
- **Fix:** Add `break;` after each `push_current_text_char` in the switch cases.

#### C6. `make_error` Dereferences Null on Allocation Failure

- **File:** `src/lisp/value_constructors.c3:288-291`
- **Issue:** `Value* v = interp.alloc_value();` is used without a null check; `v.tag = ERROR` crashes immediately if `alloc_value()` returns null under memory pressure.
- **Impact:** Fatal null-pointer dereference on OOM instead of graceful degradation.
- **Fix:** Check `if (v == null) return null;` before setting `v.tag`.

#### C7. REPL Server `load-file` Path Traversal on Unauthenticated Transports

- **File:** `src/lisp/eval_repl_server_session_ops.c3:155-156`
- **Issue:** The `load-file` operation passes the client-supplied `path` directly to `io::file::load_temp((String)path)` without sanitization. The stdio and Unix-socket transports run without authentication by default.
- **Impact:** Any local user or process that can connect to the socket can read arbitrary files (e.g., `../../../etc/passwd`).
- **Fix:** Reject paths containing `..` or path separators outside a defined sandbox, or require authentication for all transports.

#### C8. Method Dispatch Dereferences Null Method Table

- **File:** `src/lisp/eval_dispatch_match.c3:83`
- **Issue:** `find_best_method(MethodTable* mt, ...)` dereferences `mt.entry_count` without checking if `mt` is null.
- **Impact:** A caller passing a null method table causes an immediate segfault.
- **Fix:** Add `if (mt == null) return null;` at the top.

#### C9. JIT Method Table Dispatch Dereferences Null `method_table_val`

- **File:** `src/lisp/jit_apply_helpers.c3:316-320`
- **Issue:** `jit_apply_value_method_table()` assigns `MethodTable* mt = func.method_table_val;` then immediately dereferences `mt->fallback` without null-checking `mt`.
- **Impact:** A `METHOD_TABLE` tag value with a null `method_table_val` payload segfaults.
- **Fix:** Add `if (mt == null) return jit_raise(...);` before using `mt`.

#### C10. JIT `eval_define` Dereferences Null `method_table_val`

- **File:** `src/lisp/jit_define_method_table.c3:181-182`
- **Issue:** `jit_eval_define()` dereferences `existing_for_fallback.method_table_val.fallback` without checking if `method_table_val` is null.
- **Fix:** Add `&& existing_for_fallback.method_table_val != null` to the condition.

---

### 🟠 High (New)

#### H7. `copy_to_parent_note_tag` Switch Missing `break` on Every Case

- **File:** `src/lisp/eval.c3:88-130`
- **Issue:** Every `ValueTag` case falls through to the next. A single `NIL` value increments `nil_calls`, `int_calls`, `double_calls`, `string_calls`, `symbol_calls`, `cons_calls`, `closure_calls`, and `other_calls`.
- **Impact:** Completely corrupts `copy_to_parent` telemetry statistics.
- **Fix:** Add `break;` after each stat increment.

#### H8. `boundary_escape_shape_note_root` Switch Missing `break` on Composite Types

- **File:** `src/lisp/eval.c3:160-213`
- **Issue:** Cases `CONS` through `FFI_HANDLE` all fall through without `break`. A `CONS` root increments `cons_roots`, `array_roots`, `hashmap_roots`, `set_roots`, `closure_roots`, `partial_roots`, `iterator_roots`, `continuation_roots`, `coroutine_roots`, `tensor_roots`, `instance_roots`, `method_table_roots`, `module_roots`, `type_info_roots`, and `ffi_handle_roots`.
- **Impact:** Corrupts boundary-escape shape statistics used for debugging and policy decisions.
- **Fix:** Add `break;` after each tag-specific counter increment.

#### H9. Exponent Parsing Integer Overflow + Potential DoS Loop

- **File:** `src/lisp/parser_lexer_number_helpers.c3:38-47`
- **Issue:** `scan_number_exponent` accumulates `exp_val` in a `long` without overflow checks. A literal like `1e999999999` overflows `exp_val`, then the `for (long i = 0; i < exp_val; i++)` loop spins on the overflowed (possibly negative) value or runs billions of iterations.
- **Impact:** Denial-of-service on malformed numeric input; undefined behavior from signed overflow.
- **Fix:** Cap `exp_val` at a reasonable limit (e.g., 308 for doubles) and check `exp_val > max_exponent` before the loop.

#### H10. Parser Datum Constructors Dereference Null on Root Allocation Failure

- **File:** `src/lisp/parser_datum_helpers.c3:6-58`
- **Issue:** `parser_make_nil_datum`, `parser_make_int_datum`, `parser_make_symbol_datum`, `parser_make_quote_datum`, and `parser_make_cons_datum` all call `interp.alloc_value_root()` without null checks before writing `.tag`.
- **Impact:** Segfault during datum construction (quote, quasiquote, templates) when root scope is exhausted.
- **Fix:** Check each `alloc_value_root()` result for null and propagate the failure.

#### H11. `parse_datum_list` and `parse_datum_collection_template` Dereference Null Allocations

- **File:** `src/lisp/parser_datum_collections.c3:70-90`
- **Issue:** Direct `self.interp.alloc_value_root()` calls are not checked for null before `head.tag = CONS` / `cell.tag = CONS`.
- **Impact:** Same root-scope OOM crash as H10.
- **Fix:** Guard every `alloc_value_root()` result.

#### H12. Primitive Arity Validation Silently Ignores Extra Arguments

- **Files:**
  - `src/lisp/primitives_core.c3:55,64,72,80,88,96,104,113,122,131,140,213`
  - `src/lisp/prim_math_core.c3:41,73,106,139,172,205,222,239,256,273,290,307,324,341,358,380,397,414,431,456,473,485,497,509,523,537,551,565,588,624,660,690,728,742,756,770,783,798`
  - `src/lisp/prim_math_arithmetic.c3:269,277,314`
- **Issue:** Primitives use `args.len < N` instead of `args.len != N`, so extra arguments are silently ignored. Affected: `=`, `<`, `>`, `<=`, `>=`, `cons`, `car`, `cdr`, `null?`, `pair?`, `not`, `continuation?`, `sin`, `cos`, `tan`, `pow`, `sqrt`, `min`, `max`, `gcd`, `lcm`, `bitwise-and`, `lshift`, `prim_mul`, `prim_div`, `prim_mod`.
- **Impact:** `(= 1 2 3)` is accepted and only compares the first two arguments. `(sin 1.0 2.0 3.0)` is accepted and ignores extras.
- **Fix:** Replace all `args.len < N` arity checks with `args.len != N` (or appropriate variadic bounds) across the primitive surface.

#### H13. Async I/O Handle Leak on FFI Handle Allocation Failure

- **Files:**
  - `src/lisp/async_socket_handle_runtime.c3:33-38` (`make_tcp_stream_handle`)
  - `src/lisp/async_socket_handle_runtime.c3:65-69` (`make_udp_handle`)
  - `src/lisp/async_runtime_base.c3:237-240` (`make_tcp_listener_handle_with_transport`)
  - `src/lisp/async_process_signal_runtime.c3:165-169` (`make_process_handle`)
- **Issue:** If `make_ffi_handle_ex(...)` returns null (OOM), the functions return an error value but **never free** the freshly allocated native handle or close the underlying fd / `uv_process`. Only `make_fs_handle` uses a `defer` guard correctly.
- **Impact:** Repeated OOM or allocation failures leak file descriptors and heap memory.
- **Fix:** Add a `defer` guard or explicitly free the handle and close its fd/process before returning the error.

#### H14. AOT Runtime Manifest Missing CLI Entry-Point Files

- **Files:** `src/entry_build_runtime_manifest.c3`, `src/entry_build_runtime_manifest_lisp_part0-3.c3`
- **Issue:** The manifest arrays omit the executable's actual entry point and major mode handlers: `src/entry.c3`, `src/entry_runtime_modes.c3`, `src/entry_repl_server_mode.c3`, `src/entry_cli_helpers.c3`, `src/entry_eval_mode.c3`, `src/entry_build_mode.c3`, `src/entry_compile_mode.c3`, `src/entry_script_mode.c3`, `src/entry_test_modes.c3`, `src/entry_bind_mode.c3`, `src/entry_fmt_mode.c3`, `src/entry_check_mode.c3`, `src/entry_describe_mode.c3`, `src/entry_stack_affinity_mode.c3`.
- **Impact:** An AOT build relying on these manifests will be missing the CLI front-end code; REPL and server modes will not be available.
- **Fix:** Add the missing `src/entry*.c3` files to the appropriate manifest arrays.

---

### 🟡 Medium (New)

#### M19. `parse_datum_impl` Missing `T_FLOAT` Case

- **File:** `src/lisp/parser_datum.c3:46-83`
- **Issue:** The `switch (lex.current.type)` has no `case T_FLOAT:`, so floating-point literals inside `quote` or quasiquote templates produce `"unexpected token in datum"`.
- **Impact:** Legal Lisp code like `` `(list 3.14) `` fails to parse.
- **Fix:** Add `case T_FLOAT:` handling analogous to `T_INT`.

#### M20. REPL Paren-Depth Counter Ignores `#|` Block Comments

- **File:** `src/lisp/eval_repl_helpers.c3:102-117`
- **Issue:** `count_paren_depth` handles `;` line comments and `"` strings, but not nestable `#| ... |#` block comments. Unbalanced parens inside a block comment trick the REPL into waiting for more input.
- **Impact:** REPL hangs on multi-line input containing block comments with parentheses.
- **Fix:** Add `#| ... |#` skipping logic.

#### M21. Partial Tensor Clone Leaks on Failure

- **File:** `src/lisp/value_tensor.c3:321-361`
- **Issue:** `tensor_clone_big_integer_data`, `tensor_clone_big_float_data`, and `tensor_clone_big_complex_data` return `false` on clone failure without freeing already-cloned elements in `dst_values[0..i-1]`.
- **Impact:** Memory leak when cloning large tensors under memory pressure.
- **Fix:** On failure, iterate backward and free successfully cloned slots before returning.

#### M22. `read_line` Underflows When Passed a 0-Length Buffer

- **File:** `src/lisp/eval_repl_helpers.c3:16-19`
- **Issue:** `usz max_len = buffer.len - 1;` underflows to `usz.max` if `buffer.len == 0`, causing the loop to write past the buffer.
- **Fix:** Add `if (buffer.len == 0) return out;` before computing `max_len`.

#### M23. `repl_eval_buffer` Can Write One Past Buffer End

- **File:** `src/lisp/eval_repl.c3:78`
- **Issue:** `(*buf)[*buf_len] = 0;` writes at index `8192` if `*buf_len == 8192`.
- **Fix:** Assert or guard `*buf_len < 8192` before null-terminating.

#### M24. REPL Server Auth Silently Disabled When Token Is Missing

- **File:** `src/lisp/eval_repl_server_state.c3:174-180`
- **Issue:** If `auth_required = true` with `auth_token_ptr = null`, `conn.auth_required` becomes `false` silently.
- **Impact:** A configuration that explicitly requests authentication can end up running without it.
- **Fix:** Return an error / abort initialization when `auth_required` is true but the token is missing.

#### M25. Worker Queue Mutated Without Holding Worker Mutex

- **File:** `src/lisp/eval_repl_server_worker_helpers.c3:131-141`
- **Issue:** `repl_server_worker_clear_queued_commands` reads and writes `command_queue_head`, `command_queue_count`, and the queue array without acquiring `worker.mu`.
- **Impact:** Latent data race / queue corruption if called concurrently.
- **Fix:** Lock `worker.mu` at the start of the function.

#### M26. `is_number()` Excludes Big-Number Types

- **File:** `src/lisp/value_predicates_accessors_basic.c3:106-108`
- **Issue:** `is_number()` only checks `INT`, `DOUBLE`, `FLOAT32`, `COMPLEX128`, `COMPLEX64`. It excludes `BIG_INTEGER`, `BIG_FLOAT`, and `BIG_COMPLEX`, while `is_numeric_value()` includes them.
- **Impact:** Code using `is_number()` (plot lowering, deduce aggregates, sort array) misclassifies big numeric values.
- **Fix:** Expand `is_number()` to include big-number types, or audit all call sites.

#### M27. `to_double()` Missing `BIG_INTEGER` Case

- **File:** `src/lisp/value_predicates_accessors_basic.c3:117-126`
- **Issue:** `to_double()` handles `DOUBLE`, `FLOAT32`, `BIG_FLOAT`, and defaults to `(double)v.int_val`, but has no case for `BIG_INTEGER`. A `BIG_INTEGER` value falls through to `(double)v.int_val`, which is garbage because `int_val` is not the active union member.
- **Fix:** Add `if (v.tag == BIG_INTEGER) { return big_integer_try_to_double(v, &out) ? out : 0.0; }`.

#### M28. `register_builtin_types()` Missing `tid_Float` Cache

- **File:** `src/lisp/eval_dispatch_types.c3:75-101`
- **Issue:** `interp.sym_Float` is registered but `interp.tid_Float` is never cached, while `tid_Float32`, `tid_Integer`, etc. are cached.
- **Impact:** Inconsistent fast-path dispatch for `Float` lookups.
- **Fix:** Add `interp.tid_Float = interp.types.lookup(interp.sym_Float, &interp.symbols);`.

#### M29. Type Registry Hash Insertion Gap When Full

- **File:** `src/lisp/value_type_registry.c3:254-260`
- **Issue:** `register_type()` scans all slots but if the hash table is completely full, it returns without inserting, leaving the type unregistered but with an allocated ID.
- **Fix:** After the probe loop, assert that a slot was found; if not, return `INVALID_TYPE_ID` and roll back the type count increment.

#### M30. Test Error Helper Only Checks `has_error`, Not Content

- **File:** `src/lisp/tests_harness_helpers.c3:234`
- **Issue:** `test_error()` only checks `ri.error.has_error`. It does not verify the error code, message, or type.
- **Impact:** Tests can pass for the wrong reason (e.g., OOM instead of expected type error).
- **Fix:** Migrate callers to `test_error_contains()` or add content validation.

#### M31. JIT Policy Test Doesn't Verify JIT Path Was Taken

- **File:** `src/lisp/tests_runtime_feature_jit_groups.c3:322-343`
- **Issue:** `run_jit_policy_variadic_no_fallback_test()` checks that `jit_fallback_audit_total()` did not increase, but does not verify the JIT path was actually taken.
- **Impact:** If JIT is disabled, `before_total == after_total` is trivially true and the test passes without exercising JIT.
- **Fix:** Add a guard `if (!jit_checks_enabled()) { emit_pass_literal("...skipped"); return; }` and verify the compiled function is non-null.

---

### 🟢 Low (New)

#### L13. `alloc_ast_array_bytes` Returns Null for Zero-Count Arrays

- **File:** `src/lisp/parser_parser.c3:95-102`
- **Issue:** Returns `null` when `count == 0`. Callers may treat `null` as allocation failure.
- **Fix:** Return a valid zero-length pointer or document the behavior explicitly.

#### L14. `to_double` Missing Null Guard

- **File:** `src/lisp/value_predicates_accessors_basic.c3:117-126`
- **Issue:** `to_double` dereferences `v.tag` without checking `v != null`.
- **Fix:** Add `if (v == null) return 0.0;` at the top.

#### L15. `double_to_string` Potential Off-by-One

- **File:** `src/lisp/value_print.c3:21-24`
- **Issue:** `dbuf[dstr.len] = 0` can write `dbuf[64]` if `double_to_string` returns a 64-char slice.
- **Fix:** Ensure `dstr.len < dbuf.len` before null-terminating.

#### L16. `eval_defunion` Leaks `union_info.variants` on Copy Failure

- **File:** `src/lisp/eval_type_evaluators.c3:132-136`
- **Issue:** If `eval_type_info_copy_union_variants` fails, cleanup only frees `union_info.type_params`, leaving `union_info.variants` leaked.
- **Fix:** Also free `union_info.variants` before returning the error.

#### L17. Missing Null Checks on `make_symbol` / `make_string` in Dispatch Error Payloads

- **File:** `src/lisp/eval_dispatch_error_payloads.c3:92-97,225-233`
- **Issue:** `dispatch_payload_set` and `lambda_call_type_error` call `make_symbol` / `make_int` without checking for null results before passing to `hashmap_set_checked`.
- **Fix:** Check each `make_*` result for null before use.

#### L18. `eval_defeffect` Ignores `intern` Failure

- **File:** `src/lisp/eval_type_evaluators.c3:214`
- **Issue:** `info.fields[0].name = interp.symbols.intern("arg");` is not checked for `INVALID_SYMBOL_ID`.
- **Fix:** Check the return value and propagate an OOM error.

#### L19. `str_eq` Dereferences `s` Without Null Check

- **File:** `src/main.c3:3-6`
- **Issue:** `fn bool str_eq(char* s, char[] target)` does `s[i]` and `s[target.len]` without verifying `s != null`.
- **Fix:** `if (s == null) return target.len == 0;`

#### L20. Stale `.omni-repl-port` File Left on TCP Accept Failure

- **File:** `src/lisp/eval_repl_server_listeners.c3:199-208`
- **Issue:** When `repl_server_tcp` publishes the port file and then `c_accept` fails, the function returns `1` but never unlinks `.omni-repl-port`.
- **Fix:** Unlink `.omni-repl-port` in the error-exit path.

#### L21. FD Leak on `omni_uv_fs_close` Failure in `io_uv_read_all_file`

- **File:** `src/lisp/prim_io_file_helpers.c3:155-159`
- **Issue:** If `omni_uv_fs_close(fd)` returns `< 0`, the function frees `buf` and returns `false`, but leaves `fd` potentially open.
- **Fix:** Call `c_close_fd(fd)` before returning `false`.

#### L22. Unsynchronized Output Writes if `write_mu.init()` Fails

- **File:** `src/lisp/eval_repl_server_output.c3:129-138`
- **Issue:** `repl_server_output_init` sets `write_mu_ready = false` if mutex initialization fails. The main thread and worker thread both skip locking when `write_mu_ready` is false, leading to unsynchronized writes.
- **Fix:** Abort server initialization if the output mutex cannot be created.

#### L23. Non-Constant-Time Auth Token Comparison

- **File:** `src/lisp/eval_repl_server_request.c3:7-15`
- **Issue:** `repl_server_request_auth_matches` compares the token byte-by-byte with an early-exit on mismatch.
- **Impact:** Timing side-channel that can leak the correct token length and bytes.
- **Fix:** Use a constant-time comparison.

#### L24. AOT `define_var` / `set_var` Missing Null Check on `val`

- **File:** `src/lisp/aot_runtime_bridge_helpers.c3:21,120`
- **Issue:** Both functions pass `val` directly to `env_define_with_barrier` / `env_set_with_barrier_checked` without checking `val != null`.
- **Fix:** Add an explicit null check and return an error if `val` is null.

#### L25. AOT Closure Constructors Missing Null Check on `prim_val`

- **File:** `src/lisp/aot_runtime_bridge_closure.c3:177,201`
- **Issue:** After `make_primitive` returns non-null, the code immediately does `v.prim_val.user_data = ...` without checking `v.prim_val != null`.
- **Fix:** Add `if (v.prim_val == null) { ... free cd ... return error; }`.

#### L26. Duplicate Test Coverage

- **Files:** `src/lisp/tests_core_groups.c3` and `src/lisp/tests_core_arithmetic_list_groups.c3`
- **Issue:** `car`/`cdr`/`length` tests for basic cons/list operations are duplicated in both files.
- **Fix:** Remove duplicates from one file.

---

---

## Third Pass — New Findings (2026-04-24)

**Scope:** Scope destructors, big-number lifecycle, module loader, effect handlers, symbol table, pattern matching runtime, hashmap, scheduler, serializer, quasiquote, AOT compiler.
**Method:** 4 parallel agent passes + manual verification.

---

### 🔴 Critical (New)

#### C11. `scope_dtor_value` Missing `break` After STRING/ERROR Causes Type-Confusion Crash

- **File:** `src/lisp/value_constructors_lifecycle.c3:11-21`
- **Issue:** The `STRING` and `ERROR` cases fall through to `CLOSURE` without a `break`. When `string_heap_owned == false` (e.g. string literals, fallback error messages), `str_chars` is not nulled. `scope_dtor_closure` then reads the same union slot as `Closure*`, treating the string pointer as a heap closure object and dereferencing `closure_val.env_scope`.
- **Impact:** Guaranteed crash or memory corruption when scope teardown destroys any STRING/ERROR value whose chars are not heap-owned.
- **Fix:** Add `break;` after `v.string_heap_owned = false;` on line 19. Add `break;` after every other non-fallthrough case in the same switch.

#### C12. `scope_dtor_value` Cascade of Missing `break` Statements from PRIMITIVE Through VOID

- **File:** `src/lisp/value_constructors_lifecycle.c3:22-103`
- **Issue:** Every case from `PRIMITIVE` down to `CONTINUATION` lacks a `break`, creating a fallthrough chain that terminates only at `VOID`. The code relies on the coincidence that all pointer-sized union members share offset 0 and that each case nulls its pointer before falling through. Any future struct layout change or new tag addition will cause the wrong destructor to run.
- **Impact:** Extremely fragile; a struct layout change or new tag will produce use-after-free, double-free, or wrong-type dereference.
- **Fix:** Add explicit `break;` after each case block. If shared cleanup is intended, factor it into a helper.

#### C13. Module Path Traversal in Import Resolution

- **File:** `src/lisp/jit_module_setup_helpers.c3:171-195`
- **Issue:** `resolve_import_path_checked` concatenates `interp.source_dirs[interp.source_dir_count - 1]` with the user-supplied `rel_path` verbatim. It does not reject `..` components, absolute paths, or normalize the result.
- **Impact:** Arbitrary file read outside the project root via crafted import paths like `(import "../etc/passwd")`.
- **Fix:** Reject paths containing `..` segments or leading `/`. Normalize before concatenation.

#### C14. Strict Inner Handler Blocks Effects Handled by Outer Handlers

- **Files:** `src/lisp/jit_handle_signal.c3:42-61`, `src/lisp/jit_runtime_effects_signal.c3:124-157`
- **Issue:** Both `jit_signal_impl` and `jit_signal_try_resume_handler_or_strict_boundary` scan the handler stack from innermost to outermost. If they encounter a strict handler that does **not** define a clause for the effect tag, they immediately abort with `runtime/strict-unhandled-effect` without checking whether an enclosing handler **does** handle the tag.
- **Impact:** Effect delegation past a strict inner handler is impossible; valid programs are rejected.
- **Fix:** Move the `strict_mode` check outside the loop so it only fires after the full scan confirms no handler matches the tag.

---

### 🟠 High (New)

#### H15. Continuation Leak on Interpreted Handler Clause Error

- **File:** `src/lisp/jit_handle_signal.c3:151-154`
- **Issue:** In `jit_handle_dispatch_signal`, after `jit_eval(clause.handler_body, clause_env, interp)` returns an `ERROR`-tagged value, the function returns the error directly without discarding the continuation `k` allocated at line 107. The JIT path correctly calls `interp_discard_lisp_continuation` on error, but the interpreted path omits this.
- **Impact:** Every handler clause evaluation that raises an error leaks a `Continuation` node onto `interp.continuation_head`.
- **Fix:** After `jit_eval`, check `if (handler_v != null && handler_v.tag == ERROR) { interp_discard_lisp_continuation(interp, k); return handler_v; }`.

#### H16. Module Cache Corruption and Duplicate Modules on Reload

- **Files:** `src/lisp/jit_module_setup_helpers.c3:289-322` (`jit_create_file_module`), `src/lisp/jit_module_setup_helpers.c3:75-138` (`jit_eval_module_impl`)
- **Issue:** (1) `jit_create_file_module` appends a new module without checking whether a module with the same `path` already exists. (2) `jit_eval_module_impl` clears a stale module's name but does **not** rebuild the module hash index, leaving a tombstone.
- **Impact:** Same file loaded under multiple module slots; hash probes degrade; `find_module` may return the wrong module or fail.
- **Fix:** In `jit_create_file_module`, return existing module if `find_module_by_path_any(path, interp) != null`. In `jit_eval_module_impl`, call `interp_rebuild_module_hash(interp)` after clearing `existing.name`.

#### H17. Symbol ID Exhaustion Not Handled Gracefully

- **File:** `src/lisp/value_symbol_table.c3:260`
- **Issue:** `SymbolId id = (SymbolId)self.count;` casts `usz` (64-bit) to `uint` (32-bit) without overflow check. When `self.count` reaches `0xFFFFFFFF` (`INVALID_SYMBOL_ID`), the cast wraps and returns the sentinel as a valid ID.
- **Impact:** Downstream code treating `INVALID_SYMBOL_ID` as error breaks; symbol table corruption.
- **Fix:** Add `if (self.count >= (usz)INVALID_SYMBOL_ID) return INVALID_SYMBOL_ID;` before the cast.

#### H18. Double-Registration of Destructors

- **File:** `src/scope_region_chunk_helpers.c3:61-89`
- **Issue:** `scope_register_dtor` and `scope_register_dtor_escape` unconditionally prepend a new `ScopeDtor` node without checking whether the same `(ptr, func)` pair already exists in the list.
- **Impact:** If the same destructor is registered twice, it will run twice during teardown, causing double-free or use-after-free.
- **Fix:** Add a duplicate guard: walk the existing dtor list and return early if `d.ptr == ptr && d.func == func`.

#### H19. Big-Integer Binary Handle Passes Null Handles to Backend FFI

- **File:** `src/lisp/value_big_integer.c3:183-195`
- **Issue:** `big_integer_binary_handle` dereferences `lhs.big_integer_val` and `rhs.big_integer_val` without null checks before passing to backend functions.
- **Impact:** Null-pointer dereference in the C++ big-number backend.
- **Fix:** Add null checks on `v.big_integer_val` before each backend call.

#### H20. Value Serializer Silently Emits `nil` for Many Types

- **File:** `src/lisp/compiler_expr_serialize_values.c3:71-72`
- **Issue:** `serialize_value_to_buf` handles only `INT`, `DOUBLE`, `FLOAT32`, `COMPLEX128`, `COMPLEX64`, `STRING`, `SYMBOL`, `TIME_POINT`, and `CONS`. The `default` case silently emits `"nil"` for `ARRAY`, `HASHMAP`, `SET`, `TENSOR`, `VOID`, `COROUTINE`, `INSTANCE`, `METHOD_TABLE`, `MODULE`, `ITERATOR`, and other tags.
- **Impact:** Round-trip serialization of AST literals containing these values loses data.
- **Fix:** Add explicit serialization cases for missing types, or emit a parser-error placeholder.

#### H21. Compiler Name Helpers Write Without Buffer Bounds Checking

- **File:** `src/lisp/compiler_name_helpers.c3:3-65`
- **Issue:** `compiler_format_generated_name_id` and `compiler_format_generated_name_pair` write prefix + decimal digits into caller-supplied stack buffers without knowing their size.
- **Impact:** Stack buffer overflow during AOT compilation of large or deeply nested patterns.
- **Fix:** Pass `buffer.len` into the helpers and truncate or assert.

---

### 🟡 Medium (New)

#### M32. `scope_dtor_value` Default Case Swallows Unhandled Tags

- **File:** `src/lisp/value_constructors_lifecycle.c3:102`
- **Issue:** `default: {}` catches any unhandled `ValueTag` but does nothing. Adding a new tag without a destructor case silently skips cleanup.
- **Fix:** Replace `default: {}` with `default: assert(false, "unhandled ValueTag in scope_dtor_value");`.

#### M33. Big-Integer Shift Value Ignores `BIG_INTEGER_MAX_SHIFT_BITS` Limit

- **File:** `src/lisp/value_big_integer.c3:21,290-302`
- **Issue:** The constant `BIG_INTEGER_MAX_SHIFT_BITS = 1048576` is defined but never enforced. `big_integer_shift_value` passes the raw `ulong shift` directly to backend shift functions.
- **Impact:** Maliciously large shift can cause backend to allocate excessive memory or hang.
- **Fix:** Reject shifts > `BIG_INTEGER_MAX_SHIFT_BITS` before calling backend.

#### M34. `big_float_handle_from_value` Dereferences Null Big-Integer Handle

- **File:** `src/lisp/value_big_float.c3:201-208`
- **Issue:** In the `BIG_INTEGER` branch, `omni_big_integer_to_string(v.big_integer_val, &len)` is called without verifying `v.big_integer_val != null`.
- **Fix:** Add `if (v.big_integer_val == null) return null;` before the call.

#### M35. `big_complex` Real/Imag Functions Dereference Null Complex Handle

- **File:** `src/lisp/value_big_complex.c3:254-268`
- **Issue:** `big_complex_real_part_value` and `big_complex_imag_part_value` call backend functions without checking `v.big_complex_val != null`.
- **Fix:** Add null checks before backend calls.

#### M36. Hashmap `get` and `remove` Missing `entries` Null Check

- **File:** `src/lisp/prim_collection_hashmap.c3:128-136,349-381`
- **Issue:** Neither function validates `map.entries != null` before indexing.
- **Impact:** Segfault on malformed or incompletely initialized HashMap.
- **Fix:** Add `if (map == null || map.entries == null) return null/false;`.

#### M37. Hashmap `grow_checked` Rehash Can Silently Drop Entries

- **File:** `src/lisp/prim_collection_hashmap.c3:210-224`
- **Issue:** The rehash loop searches for an empty slot; if it exhausts the loop without finding one, execution falls through with no error and the entry is lost.
- **Impact:** Silent data loss if hash clustering ever exhausts the probe sequence.
- **Fix:** Add `assert(false, "hashmap rehash: no empty slot")` or return `false` after the inner loop.

#### M38. Scheduler `add_fiber` Accepts Stale/Completed `parent_id`

- **Files:** `src/lisp/scheduler_io_fiber_core.c3:20-26`, `src/lisp/scheduler_primitives.c3:134-136`
- **Issue:** `scheduler_add_fiber` checks `parent_id < g_scheduler.fiber_count` but does not verify the parent fiber is still alive. `prim_spawn` increments `live_children` on a completed parent.
- **Impact:** Scheduler state corruption; parent/child lifecycle accounting incorrect.
- **Fix:** Verify `parent_id == NO_FIBER || g_scheduler.fibers[parent_id].state != FIBER_DONE`.

#### M39. Symbol Table Hash Insertion Does Not Verify Success

- **File:** `src/lisp/value_symbol_table.c3:275-281`
- **Issue:** The insertion loop probes for an empty slot and breaks when found, but does not check a slot was actually found. If the hash table is full, the symbol is stored in `entries` but never inserted into `hash_index`.
- **Impact:** Unreachable symbols, duplicate IDs, silent data loss.
- **Fix:** After the loop, assert `self.hash_index[slot] == id` or return `INVALID_SYMBOL_ID`.

#### M40. Match Exhaustiveness Check Incorrect for Nested Patterns

- **File:** `src/lisp/eval_dispatch_match_errors.c3:86-98`
- **Issue:** `format_match_error` checks union coverage by inspecting only the **top-level** `pat.tag` of each clause. If a union variant is matched inside a nested pattern, the check falsely reports it as missing.
- **Impact:** Valid match expressions on nested union values produce spurious runtime errors.
- **Fix:** Recursively search each clause pattern for a `PAT_CONSTRUCTOR` that covers the variant.

#### M41. Circular Import Not Detected for Declared Module Self-Reference

- **File:** `src/lisp/jit_module_import_setup.c3:14-31`
- **Issue:** `jit_eval_declared_module_file` returns early only when `already != null && already.loaded`. If a module file is re-entered during its own loading, the `already.loaded == false` case falls through to re-evaluate.
- **Impact:** Stack overflow or infinite recursion on circular module dependencies.
- **Fix:** Return `eval_error("circular import detected")` when `already != null && !already.loaded`.

#### M42. Quasiquote Nesting Depth Unlimited in AOT Compiler

- **File:** `src/lisp/compiler_quasiquote_flat.c3:75-119`
- **Issue:** The compiler path recurses with `depth + 1` on `E_QUASIQUOTE` and `depth - 1` on `E_UNQUOTE` without any bound check. The JIT evaluator limits depth to 64, but AOT has no guard.
- **Impact:** Stack overflow in compiler when compiling deeply nested quasiquote templates.
- **Fix:** Add `if (depth > 64) return self.emit_error_temp("quasiquote nesting too deep");`.

#### M43. Multi-Shot Continuation Resume Leaks Handler State Refcount

- **File:** `src/lisp/jit_runtime_effects.c3:90-161`
- **Issue:** `jit_apply_handle_continuation_value` clones the continuation context and resumes it, but never decrements `hstate.continuation_refcount`. The single-shot path does decrement it.
- **Impact:** Memory leak of handler state for every multi-shot continuation resume.
- **Fix:** After cloned context completes, decrement `hstate.continuation_refcount`.

#### M44. Unbounded Perform/Resume Dispatch Loops

- **Files:** `src/lisp/jit_runtime_effects_handle.c3:95-130`, `src/lisp/jit_handle_signal_handle.c3:161-188`
- **Issue:** Both functions use `while (state.signaled)` loops with no iteration limit. A program that repeatedly performs and resumes in a tight loop allocates a new continuation each cycle.
- **Impact:** Denial of service via CPU and memory exhaustion.
- **Fix:** Add a cycle counter and error out if it exceeds a threshold (e.g., 10,000).

#### M45. Memory Telemetry Counters Not Atomic — Race on Multi-Threaded Allocations

- **Files:** `src/scope_region_allocators.c3:22-28`, `src/scope_region_temp_pool_stats.c3:144-250`, `src/scope_region_reset_adopt.c3:16-20`, `src/scope_region_lifecycle.c3:99`
- **Issue:** Global telemetry counters are incremented from allocation fast paths without locks or atomics. Since scopes can be owned by different threads, concurrent allocations race on these shared fields.
- **Impact:** Corrupted telemetry, lost counter updates, torn reads/writes on 64-bit fields.
- **Fix:** Guard telemetry increments with `scope_global_lock()`, or switch to `std::atomic` types.

#### M46. Memory Telemetry Counter Overflow

- **Files:** `src/scope_region_temp_pool_stats.c3:44-255`, `src/lisp/eval_boundary_telemetry.c3:38-95`
- **Issue:** All telemetry structs use plain `usz` fields incremented without overflow checks. On long-running workloads counters can wrap.
- **Impact:** Telemetry silently incorrect; heuristics relying on monotonic counters break.
- **Fix:** Add saturating-add wrappers before every increment.

---

### 🟢 Low (New)

#### L27. `Interp.alloc_env` Leaves `is_inline` Uninitialized

- **File:** `src/lisp/value_interp_alloc_helpers.c3:43-57`
- **Issue:** `alloc_env()` initializes bindings, capacity, hash_table, etc., but never sets `is_inline`. Callers currently overwrite it, but any future direct caller that forgets will cause `scope_dtor_env` to make the wrong free decision.
- **Fix:** Initialize `e.is_inline = false;` inside `alloc_env()` and `alloc_env_escape()`.

#### L28. `Env.hash_lookup` / `hash_insert` Rely Only on Contracts for Null `hash_table`

- **File:** `src/lisp/value_environment_storage.c3:197-223`
- **Issue:** Both functions have `@require self.hash_table != null` but no runtime guard.
- **Fix:** Add runtime guards at the top of each function.

#### L29. `hashmap_sorted_slots` Dereferences Map Without Null Guard

- **File:** `src/lisp/prim_collection_hashmap_key_helpers.c3:264-268`
- **Issue:** Accesses `map.capacity` without checking `map != null`.
- **Fix:** Add `if (map == null || map.entries == null) return {};`.

#### L30. `copy_env_value_fast` Missing Default Switch Case

- **File:** `src/lisp/eval_env_copy_values.c3:60-127`
- **Issue:** No `default` case. If a new `ValueTag` is added and not handled, the function returns uninitialized/garbage.
- **Fix:** Add `default: return null;` so the caller's rollback path triggers.

#### L31. Dict Pattern Parser Accepts Duplicate Keys

- **File:** `src/lisp/parser_patterns_values.c3:93-122`
- **Issue:** `parse_dict_pattern` pushes every symbol into `keys` without checking for duplicates.
- **Impact:** Confusing semantics; last binding wins, hiding logic errors.
- **Fix:** Reject duplicate symbols in dict patterns at parse time.

#### L32. Pattern Matching Runtime Silently Fails for New Pattern Tags

- **File:** `src/lisp/eval_pattern_matching.c3:43-44`
- **Issue:** `default` branch returns `match_fail()`. If a new `PatternTag` is added, the compiler will not flag the missing case.
- **Fix:** Remove `default` or add `assert(false, "unknown pattern tag")`.

#### L33. Pattern Serializer Silently Emits Wildcard for Unknown Tags

- **File:** `src/lisp/compiler_expr_serialize_patterns.c3:84-85`
- **Issue:** `default` emits `_`. New pattern tags silently serialize as wildcard.
- **Fix:** Remove `default` or emit an error string.

#### L34. Module Path Comparison Not Normalized

- **File:** `src/lisp/jit_module_import_setup.c3:58-66`
- **Issue:** `module_path_matches` compares paths byte-for-byte. Different strings resolving to the same file are treated as distinct.
- **Impact:** Same module loaded under multiple cache entries.
- **Fix:** Normalize paths before storing and comparing.

#### L35. Stale `.omni-repl-port` File (Duplicate of L20)

- **Status:** Already reported as L20. No new finding.

#### L36. `str_eq` Null Dereference (Duplicate of L19)

- **Status:** Already reported as L19. No new finding.

---

---

## Fourth Pass — New Findings (2026-04-24)

**Scope:** C helpers, UI, async I/O, JIT compiler state, stack/FPU, test infrastructure, build system, scripts.
**Method:** 4 parallel agent passes + manual verification.

---

### 🔴 Critical (New)

#### C15. Scheduler Wakeup Dispatch Switch Missing `break` on Every Case

- **File:** `src/lisp/scheduler_wakeup_queue.c3:22-43`
- **Issue:** The `switch (ev.type)` block in `scheduler_dispatch_wakeup_event` has **no `break` statements** between any case. A `WAKEUP_TIMER_EXPIRED` event falls through and also triggers `WAKEUP_POLL_ERROR`, `WAKEUP_POLL_READABLE`, `WAKEUP_OFFLOAD_READY`, `WAKEUP_SLEEP_DONE`, `WAKEUP_DNS_DONE`, `WAKEUP_TCP_CONNECT_DONE`, `WAKEUP_TCP_ACCEPT_DONE`, `WAKEUP_TASK_DONE`, and `WAKEUP_OS_THREAD_DONE` handlers.
- **Impact:** Any wakeup event triggers all subsequent handlers, corrupting async scheduler state. Fibers receive incorrect wakeup signals, leading to crashes or unpredictable async behavior.
- **Fix:** Add `break;` after each handler call in the switch.

#### C16. `fpu_save` Null Dereference on x86_64

- **File:** `csrc/stack_helpers.c:196-203`
- **Issue:** On `__x86_64__`, `fpu_save(uint32_t* mxcsr, uint32_t* x87cw)` dereferences `*mxcsr = m` and `*x87cw = (uint32_t)c` without checking for null. The `#else` branch correctly guards with `if (mxcsr != NULL)` and `if (x87cw != NULL)`.
- **Impact:** Segfault when `fpu_save` is called with null arguments on x86_64 (e.g. during stack context initialization paths that skip FPU state).
- **Fix:** Add null checks before dereferencing in the x86_64 path.

#### C17. BLAS `omni_tensor_backend_blas_dger` Buffer Overflow

- **File:** `csrc/tensor_blas_helpers.c:242-243`
- **Issue:** The function zeros `count = m * lda` elements, but if `lda > n` and the caller allocated only `m * n` elements for `out`, the zeroing loop writes past the end of the buffer.
- **Impact:** Heap buffer overflow when `lda` exceeds the logical column count.
- **Fix:** Zero only `m * n` elements, or document that `out` must be sized for `m * lda`.

#### C18. JIT Multi-Arg Primitive Apply Dereferences Null `func.prim_val`

- **File:** `src/lisp/jit_apply_multi_prims.c3:125-128`
- **Issue:** `jit_apply_multi_args_primitive` accesses `func.prim_val.tag`, `func.prim_val.user_data`, and `func.prim_val.arity` without checking `func.prim_val != null`. The single-arg path (`jit_apply_single_arg_primitive`) has a null guard, but the multi-arg path omits it.
- **Impact:** Segfault when a primitive-tagged value has a null `prim_val` payload.
- **Fix:** Add `if (func.prim_val == null) return jit_raise(...);` at the top.

---

### 🟠 High (New)

#### H22. HTTP Response Header Slice Underflow

- **File:** `src/lisp/http_url_response.c3:198`
- **Issue:** `response[hdr_start..header_end - 5]` can underflow when `header_end < 5`. Since `header_end` is `usz` (unsigned), `header_end - 5` wraps to a very large number.
- **Impact:** Invalid slice bounds, potential memory corruption or crash when parsing very short HTTP responses.
- **Fix:** Guard with `if (header_end >= 5)` before computing the slice.

#### H23. Deduce Path Traversal

- **File:** `src/lisp/deduce_runtime_open.c3:161`
- **Issue:** The user-supplied path string is copied to `path_buf` and passed directly to `mdb_env_open` without sanitization. No `..` rejection or path normalization is performed.
- **Impact:** Arbitrary file write/read outside the intended sandbox via crafted paths like `../../../etc/cron.d/exploit`.
- **Fix:** Reject paths containing `..` segments or leading `/`. Normalize before opening.

#### H24. Unbounded JIT Spill-List Growth

- **File:** `src/lisp/jit_compiler_compile.c3:56-74`
- **Issue:** When the fixed JIT state pool is full, every new compilation allocates a `JitStateSpillNode` via `mem::malloc` and prepends it to `g_jit_spill_states`. There is no cap on `g_jit_spill_count`.
- **Impact:** Under sustained compilation pressure, the spill list grows without bound, consuming unbounded memory.
- **Fix:** Add a maximum spill count and fail compilation gracefully when exceeded.

#### H25. Retired-Code Tombstone Saturation Silently Ignored

- **File:** `src/lisp/jit_compiler_lifecycle.c3:31-40`
- **Issue:** `runtime_backend_destroy_tracked_state_slot` and `runtime_backend_destroy_spill_state_node` both call `jit_retired_code_insert` and cast the result to `(void)`, discarding failure. If the retired-code tombstone table is full, the stale code pointer is not recorded and could be falsely considered live later.
- **Impact:** Stale JIT code pointers might be reused after their backing memory is freed or remapped.
- **Fix:** Handle `jit_retired_code_insert` failure — panic, grow the table, or refuse to destroy the state slot.

#### H26. `cstr_len` Hard-Capped at 65536

- **File:** `src/entry_cli_helpers.c3:15-19`
- **Issue:** `cstr_len` stops scanning at 65536 characters, silently truncating longer CLI arguments, environment variables, or file paths.
- **Impact:** Long paths or source strings are silently truncated, causing incorrect behavior or file-not-found errors.
- **Fix:** Remove the cap or make it much larger (e.g. `INT_MAX`).

---

### 🟡 Medium (New)

#### M47. Scheduler Queue Mutated Without Mutex

- **File:** `src/lisp/eval_repl_server_worker_helpers.c3:131-141`
- **Issue:** `repl_server_worker_clear_queued_commands` reads and writes `command_queue_head`, `command_queue_count`, and the queue array without acquiring `worker.mu`.
- **Impact:** Data race / queue corruption if called concurrently with command enqueue/dequeue.
- **Fix:** Lock `worker.mu` at the start of the function.

#### M48. TLS Socket Callbacks Truncate `long` to `int`

- **File:** `csrc/tls_helpers.c:37,56`
- **Issue:** `omni_tls_sock_read` returns `(int)rlen` and `omni_tls_sock_write` returns `(int)wlen`. On 64-bit platforms, a `long` read/write larger than `INT_MAX` is silently truncated.
- **Impact:** Incorrect return values for large TLS transfers.
- **Fix:** Change callback signatures to return `long` or `ssize_t`, or clamp and report overflow.

#### M49. Signal Handler Accesses Non-Atomic `int` Variables

- **File:** `csrc/stack_helpers.c:480-495`
- **Issue:** `sigsegv_handler` reads and writes `g_recovery_depth`, `g_guard_count`, `g_guard_hit`, and `g_guards[]` without atomic operations or memory barriers. These are plain `int`/`static` variables accessed from the signal handler context.
- **Impact:** Torn reads/writes, compiler reordering, or missed updates on multi-core systems.
- **Fix:** Use `volatile sig_atomic_t` for the flags and atomic operations for counters.

#### M50. BLAS Library Resolution Data Race

- **File:** `csrc/tensor_blas_helpers.c:60-85`
- **Issue:** `omni_tensor_blas_resolve` uses a static `int resolution_attempted` without synchronization. Concurrent threads can race on the flag and double-load the library.
- **Fix:** Use an atomic flag or mutex guard.

#### M51. LAPACK Library Resolution Data Race

- **File:** `csrc/tensor_lapack_helpers.c:280-305`
- **Issue:** Same pattern as M50 — `resolution_attempted` is a plain static `int`.
- **Fix:** Same as M50.

#### M52. Vulkan Library Resolution Data Race

- **File:** `csrc/tensor_vulkan_helpers_core.c:60-85`
- **Issue:** Same pattern as M50/M51 — library loading flag is unsynchronized.
- **Fix:** Same as M50.

#### M53. `omni_uv_fs_read` / `write` Truncate `size_t` to `unsigned int`

- **File:** `csrc/uv_helpers.c:71,80`
- **Issue:** `uv_buf_init(buf, (unsigned int)len)` silently truncates `size_t len` to `unsigned int`. On platforms where `size_t > unsigned int`, large reads/writes are truncated.
- **Impact:** Silent data loss or partial I/O on large files.
- **Fix:** Guard `if (len > UINT_MAX) return UV_EINVAL;` before truncating.

#### M54. `c3c_limits.sh` Lacks `set -e`

- **File:** `scripts/c3c_limits.sh`
- **Issue:** The script does not use `set -e`. Any command failure (e.g. `c3c` not found, Docker unavailable) is silently ignored.
- **Impact:** CI gates may pass even when the underlying commands failed.
- **Fix:** Add `set -euo pipefail` at the top.

#### M55. `tests_harness_helpers.c3:setup()` Prints Failure But Does Not Abort

- **File:** `src/lisp/tests_harness_helpers.c3:19-32`
- **Issue:** When test setup fails, `setup()` prints an error message and returns normally. The calling test continues execution against a broken interpreter state.
- **Impact:** Tests can pass or fail for the wrong reason after setup errors.
- **Fix:** Abort the test process or raise a fatal error after setup failure.

#### M56. `.github/workflows/io-parity-guard.yml` References Missing Script

- **File:** `.github/workflows/io-parity-guard.yml`
- **Issue:** The workflow references `scripts/check_io_parity_status_map.sh`, which does not exist in the repository.
- **Impact:** The workflow fails on every run.
- **Fix:** Create the missing script or remove the workflow step.

#### M57. `tests_e2e_generation.c3` Silently Skips Tests on Error

- **File:** `src/lisp/tests_e2e_generation.c3`
- **Issue:** When e2e test generation encounters an error, it logs and continues without failing the build.
- **Impact:** Missing e2e tests go undetected.
- **Fix:** Return a failure exit code when generation errors occur.

#### M58. `test_eq_double` Uses Exact Equality

- **File:** `src/lisp/tests_harness_helpers.c3`
- **Issue:** `test_eq_double` compares doubles with `==`, which will flake across architectures due to floating-point rounding differences.
- **Impact:** False test failures on different CPU architectures or compiler versions.
- **Fix:** Use a tolerance-based comparison (`fabs(a - b) < epsilon`).

---

### 🟢 Low (New)

#### L27. AOT `define_var` / `set_var` Missing Null Check on `val`

- **File:** `src/lisp/aot_runtime_bridge_helpers.c3:21,120`
- **Issue:** Both functions pass `val` directly to `env_define_with_barrier` / `env_set_with_barrier_checked` without checking `val != null`.
- **Fix:** Add an explicit null check and return an error if `val` is null.

#### L28. AOT Closure Constructors Missing Null Check on `prim_val`

- **File:** `src/lisp/aot_runtime_bridge_closure.c3:177,201`
- **Issue:** After `make_primitive` returns non-null, the code does `v.prim_val.user_data = ...` without checking `v.prim_val != null`.
- **Fix:** Add `if (v.prim_val == null) { ... free cd ... return error; }`.

#### L29. Duplicate Test Coverage

- **Files:** `src/lisp/tests_core_groups.c3` and `src/lisp/tests_core_arithmetic_list_groups.c3`
- **Issue:** `car`/`cdr`/`length` tests for basic cons/list operations are duplicated in both files.
- **Fix:** Remove duplicates from one file.

#### L30. HTTP Response Buffer Size Not Checked Against Maximum

- **File:** `src/lisp/http_url_response.c3`
- **Issue:** The response buffer can grow without an upper bound on very large HTTP responses.
- **Fix:** Add a maximum response size limit and error out if exceeded.

#### L31. Deduce Rule Catalog Count Can Overflow

- **File:** `src/lisp/deduce_runtime_open.c3`
- **Issue:** `deduce_db_restore_persisted_rule_catalog` reads a count from the database without validating it against a reasonable maximum.
- **Fix:** Cap the count and error out if the persisted catalog appears corrupted.

#### L32. AOT Quasiquote Depth Check Missing in Compiler Path

- **File:** `src/lisp/compiler_quasiquote_flat.c3`
- **Issue:** The compiler path recurses on quasiquote without a depth bound. The evaluator path limits depth to 64.
- **Fix:** Add a depth counter and error out if it exceeds a threshold.

#### L33. JIT Spill Allocation Warns But Does Not Fail Build

- **File:** `src/lisp/jit_compiler_compile.c3:58`
- **Issue:** When spill allocation fails, a warning is printed but compilation continues in a degraded state.
- **Fix:** Return a hard failure so the caller can handle it.

#### L34. Unquoted Array in `run_e2e.sh`

- **File:** `scripts/run_e2e.sh:96`
- **Issue:** `${stage3_compile_sources[@]}` is unquoted, causing word-splitting if paths contain spaces.
- **Fix:** Quote the expansion: `"${stage3_compile_sources[@]}"`.

#### L35. Docker Extra Args Exported Under Wrong Variable Name

- **Files:** `scripts/run_e2e.sh:52`, `scripts/run_deduce_perf_envelope.sh:50`
- **Issue:** Both scripts build `validation_extra` mounts and export them as `OMNI_VALIDATION_EXTRA_ARGS`, but `scripts/c3c_limits.sh` only reads `OMNI_DOCKER_EXTRA_ARGS`. The mounts are silently ignored.
- **Fix:** Export `OMNI_DOCKER_EXTRA_ARGS` instead.

#### L36. C Compiler Hardcoded to `cc`, Ignores `$CC`

- **File:** `scripts/build_omni_chelpers.sh:278`
- **Issue:** The C compilation helper hardcodes `cc` while respecting `$CXX` for C++. Cross-compilation workflows break.
- **Fix:** Use `"${CC:-cc}"` instead of `cc`.

---

---

## Fifth Pass — New Findings (2026-04-24)

**Scope:** Deep re-audit of parser/compiler/JIT switch blocks, runtime path null checks, scope guard contracts, AOT tail-call loops, C helper edge cases, evaluator error-path leaks.
**Method:** 4 parallel agent passes + manual verification of all reported switch blocks and null-dereference sites.

---

### 🔴 Critical (New)

#### C19. `scope_guard_owner` Macro Returns Early on Null But Callers Continue Executing

- **Files:** `src/scope_region_allocators.c3:7,53,89,111`, `src/scope_region_chunk_helpers.c3:61,75`, `src/scope_region_reset_helpers.c3:19,74`, `src/scope_region_global_guards.c3:58-63`
- **Issue:** `scope_require_owner` does `if (scope == null) return;` — it returns from itself, not from the caller. The macro `scope_guard_owner` expands to this call, so after the early return, the caller (e.g. `ScopeRegion.alloc`, `scope_register_dtor`, `scope_reset`) continues with `self == null` and dereferences `self.bump`, `self.chunks`, `self.refcount`, etc.
- **Impact:** Guaranteed null dereference if any public allocator or dtor registration is called with a null scope pointer.
- **Fix:** Add `if (self == null) return null;` at the top of every public allocator/dtor/reset function, or change `scope_guard_owner` to an inline function that returns a bool and gate the caller body on it.

#### C20. `jit_warm_pattern_cache` Missing `break` on Every Case

- **File:** `src/lisp/jit_apply_eval.c3:131-150`
- **Issue:** `PAT_CONS` falls through to `PAT_SEQ`, which falls through to `PAT_AS`, then `PAT_CONSTRUCTOR`, then `PAT_GUARD`. A single cons pattern triggers cache warming for seq, as, constructor, and guard — reading wrong union fields.
- **Impact:** JIT cache warming reads incorrect pattern union fields, causing crashes or wrong compiled code.
- **Fix:** Add `break;` after each recursive cache-warming block.

#### C21. `jit_warm_expr_cache` Missing `break` on Every Case

- **File:** `src/lisp/jit_apply_eval.c3:202-268`
- **Issue:** All 20+ `ExprTag` cases (`E_LAMBDA`, `E_APP`, `E_IF`, `E_LET`, `E_DEFINE`, `E_RESET`, `E_SHIFT`, `E_PERFORM`, `E_HANDLE`, `E_RESOLVE`, `E_INDEX`, `E_MATCH`, `E_AND`, `E_OR`, `E_CALL`, `E_BLOCK`, `E_SET`, `E_QUASIQUOTE`, `E_UNQUOTE`, `E_UNQUOTE_SPLICING`, `E_MODULE`, `E_WITH_MODULE`, `E_FFI_LIB`) fall through to the next case.
- **Impact:** A single lambda expression triggers warming for app, if, let, define, reset, shift, perform, handle, resolve, index, match, and, or, call, block, set, quasiquote, unquote, unquote-splicing, module, with-module, and ffi-lib — reading completely wrong union fields.
- **Fix:** Add `break;` after each recursive cache-warming block.

#### C22. `jit_compile_expr_group_core` Missing `break` on Every Case

- **File:** `src/lisp/jit_compile_expr_dispatch.c3:117-144`
- **Issue:** `E_LIT`, `E_VAR`, `E_IF`, `E_AND`, `E_OR`, `E_BLOCK`, `E_APP`, `E_CALL`, `E_LET`, `E_LAMBDA`, `E_SET`, `E_DEFINE` all fall through to each other without `break`. Compiling a literal also emits code for variable, if, and, or, block, app, call, let, lambda, set, and define.
- **Impact:** JIT-generated machine code is completely wrong; runtime crashes or silent data corruption.
- **Fix:** Add `break;` after each `jit_compile_*` call.

#### C23. `jit_compile_expr_group_effects` Missing `break` on Every Case

- **File:** `src/lisp/jit_compile_expr_dispatch.c3:147-182`
- **Issue:** `E_QUASIQUOTE`, `E_MATCH`, `E_DEFMACRO`, `E_RESET`, `E_SHIFT`, `E_HANDLE`, `E_PERFORM`, `E_RESOLVE`, `E_QUOTE`, `E_INDEX`, `E_PATH`, `E_MODULE`, `E_IMPORT`, `E_WITH_MODULE` all fall through.
- **Impact:** Same as C22 — wrong JIT code generation.
- **Fix:** Add `break;` after each `jit_compile_*` call.

#### C24. `jit_compile_expr_group_types_ffi` Missing `break` on Every Case

- **File:** `src/lisp/jit_compile_expr_dispatch.c3:184-211`
- **Issue:** `E_DEFTYPE`, `E_DEFABSTRACT`, `E_DEFUNION`, `E_DEFALIAS`, `E_DEFEFFECT`, `E_EXPORT_FROM`, `E_FFI_LIB`, `E_FFI_FN` all fall through.
- **Impact:** Same as C22/C23 — wrong JIT code generation.
- **Fix:** Add `break;` after each `jit_compile_*` call.

#### C25. `Compiler.compile_expr` Missing `break` on Every Case

- **File:** `src/lisp/compiler_expression_compilation.c3:15-39`
- **Issue:** `E_LIT`, `E_VAR`, `E_DEFINE`, `E_QUOTE`, `E_PATH` all fall through to each other. A literal expression also emits variable, define, quote, and path compilation code.
- **Impact:** AOT-generated C3 source is syntactically invalid and will not compile.
- **Fix:** Add `break;` after each `self.compile_*` call.

#### C26. `numeric_binary_float32_result` Missing `break` on Every Case

- **File:** `src/lisp/prim_math_arithmetic.c3:54-66`
- **Issue:** `BIG_INTEGER_OP_ADD` falls through to `BIG_INTEGER_OP_SUB`, then `MUL`, then `DIV`. So `left + right` becomes `left + right - right * right / right`.
- **Impact:** All float32 binary math operations return completely wrong results.
- **Fix:** Add `break;` after each `result = ...` assignment.

---

### 🟠 High (New)

#### H27. `eval_path_lookup_root` Dereferences `env` Without Null Check

- **File:** `src/lisp/eval_path.c3:5-8`
- **Issue:** `env.lookup(root)` is called without verifying `env != null`.
- **Impact:** Null dereference if called with a null environment.
- **Fix:** Add `if (env == null) return interp.global_env.lookup(root);` or guard appropriately.

#### H28. `eval_path_instance_step` Dereferences `current.instance_val` Without Null Check

- **File:** `src/lisp/eval_path.c3:50-71`
- **Issue:** `current.instance_val` is dereferenced on line 51 without checking if `current` is null or if `instance_val` is null.
- **Impact:** Null dereference crash on instance field access.
- **Fix:** Add null check before dereferencing.

#### H29. `eval_path_dict_step` Dereferences `current.hashmap_val` Without Null Check

- **File:** `src/lisp/eval_path.c3:73-88`
- **Issue:** `current.hashmap_val` passed to `hashmap_get` without null check.
- **Impact:** Null dereference crash on dictionary path access.
- **Fix:** Add null check before `hashmap_get`.

#### H30. `Env.hash_lookup` Infinite Loop When Hash Table Is Completely Full

- **File:** `src/lisp/value_environment_storage.c3:197-208`
- **Issue:** `for (;;) { ... if (k == ENV_HASH_EMPTY) return ~(usz)0; ... }` — if every slot is occupied and the key is absent, it never terminates.
- **Impact:** Interpreter hangs on environment lookup.
- **Fix:** Cap iterations at `hash_capacity` or ensure load factor never reaches 100%.

#### H31. `jit_apply_value_primitive` Dereferences `args` Without Null Check

- **File:** `src/lisp/jit_apply_helpers.c3:328-351`
- **Issue:** `args[:1]` is used on line 338 without checking `args.ptr != null`.
- **Impact:** Potential null dereference if primitive is called with a null args slice.
- **Fix:** Add `if (args.ptr == null) return ...` before accessing elements.

#### H32. `foreign_runtime_kind_name`, `foreign_handle_kind_name`, `foreign_ffi_abi_type_name` Missing Default Cases

- **File:** `src/lisp/foreign_runtime_core.c3:30-46,53-69,109-137`
- **Issue:** Switches over enums have no `default` case. If an unexpected enum value is passed, the function returns uninitialized/garbage.
- **Impact:** Undefined behavior / garbage strings in FFI metadata.
- **Fix:** Add `default: return "<unknown>";` to each switch.

#### H33. `jit_apply_multi_args_method_table` Dereferences `func.method_table_val` Without Null Check

- **File:** `src/lisp/jit_apply_multi_prims.c3:194-212`
- **Issue:** Assumes `func.tag == METHOD_TABLE` but never checks `func.method_table_val != null` before accessing `mt.fallback` and `find_best_method(mt, ...)`.
- **Impact:** Null dereference if a METHOD_TABLE value has a null payload.
- **Fix:** Add `if (mt == null) return jit_raise(...)` after casting.

#### H34. `jit_apply_value_tail` Dereferences `func.closure_val` Without Null Check

- **File:** `src/lisp/jit_apply_runtime.c3:194-234`
- **Issue:** After confirming `func.tag == CLOSURE`, the code accesses `func.closure_val.has_param`, `func.closure_val.has_rest`, `func.closure_val.param_count`, etc., without verifying `func.closure_val != null`.
- **Impact:** Segfault on malformed closure value.
- **Fix:** Add `if (func.closure_val == null) return jit_raise(...);` inside the CLOSURE branch.

#### H35. `jit_apply_value_closure` Dereferences `closure.body` Without Null Check

- **File:** `src/lisp/jit_apply_helpers.c3:237-312`
- **Issue:** `jit_apply_value_closure` accesses `closure.body.tag` and other fields after asserting `func.closure_val != null`, but `closure.body` itself (an `Expr*`) is never checked for null before being passed to `jit_eval_in_single_scope`.
- **Impact:** Segfault if lambda body expr is null.
- **Fix:** Add `if (closure.body == null) return jit_raise(...);`.

#### H36. `jit_copy_closure_env_if_needed` Null Dereference on `closure.closure_val`

- **File:** `src/lisp/jit_closure_runtime.c3:149-188`
- **Issue:** At line 158, `closure.closure_val.env_scope` is accessed without first verifying `closure.closure_val != null`.
- **Impact:** Segfault if closure has a null payload.
- **Fix:** Add `if (closure == null || closure.tag != CLOSURE || closure.closure_val == null) return false;` at the top.

#### H37. `aot_closure_apply` / `aot_variadic_apply` Unbounded Tail-Call Loop

- **File:** `src/lisp/aot_runtime_bridge_closure.c3:103-110,145-152`
- **Issue:** `while (g_tail_pending)` has no iteration bound. A buggy AOT program with mutually tail-recursive closures can spin forever.
- **Impact:** Infinite loop / DoS.
- **Fix:** Add a counter and error out after a configurable max tail-call depth.

#### H38. `jit_lookup_global` Stack Buffer Overflow on Long Symbol Names

- **File:** `src/lisp/jit_apply_helpers.c3:185-197`
- **Issue:** `char[256] ebuf` is used with `io::bprintf`. If `interp.symbols.get_name(name)` returns a very long symbol name, the formatted string can exceed 256 bytes.
- **Impact:** Stack buffer overflow.
- **Fix:** Use a larger buffer or truncate the symbol name before formatting.

---

### 🟡 Medium (New)

#### M59. `eval_run_pipeline` JIT Error Path Leaks `exprs` List

- **File:** `src/lisp/eval_run_pipeline.c3:34-47`
- **Issue:** When JIT compilation fails (`runtime_compiled_expr_is_null`), the function sets `result = eval_error_expr(...)` and breaks out of the loop, but `exprs.free()` is not called before returning.
- **Impact:** Memory leak on JIT compilation failure.
- **Fix:** Move `exprs.free()` before the return or use a `defer`.

#### M60. `eval_defeffect` Leaves `field_count` Uninitialized in `else` Branch

- **File:** `src/lisp/eval_type_evaluators.c3:223-226`
- **Issue:** When `!de.has_arg_type`, `info.fields = null` is set but `info.field_count` is not explicitly set to 0.
- **Impact:** Potential uninitialized field if zero-init is ever missed.
- **Fix:** Explicitly set `info.field_count = 0;` in the else branch.

#### M61. `jit_method_table_append_entry` Dereferences `impl.closure_val.type_sig` Without Null Check

- **File:** `src/lisp/jit_define_method_table.c3:101-105`
- **Issue:** `impl.closure_val.type_sig` is accessed without checking `impl.closure_val != null`.
- **Impact:** Null dereference if a non-closure value is passed.
- **Fix:** Add `if (impl.closure_val == null) return false;`.

#### M62. `jit_new_method_table` Dereferences `impl.closure_val.type_sig` Without Null Check

- **File:** `src/lisp/jit_define_method_table.c3:129-131`
- **Issue:** Same as M61.
- **Fix:** Add null check.

#### M63. `jit_eval_define_typed_closure` Dereferences `stored_val.closure_val.type_sig` Without Null Check

- **File:** `src/lisp/jit_define_method_table.c3:156-158`
- **Issue:** `stored_val.closure_val.type_sig` accessed without verifying `closure_val != null`.
- **Impact:** Null dereference.
- **Fix:** Add null check.

#### M64. `find_best_method` Can Leak `best_match_indices_rev` on Early Error Return

- **File:** `src/lisp/eval_dispatch_match.c3:83-113`
- **Issue:** If `make_cons_or_error` returns an error, the function returns immediately. `best_match_indices_rev` may hold references to intermediate cons cells that become unreachable.
- **Impact:** Minor transient allocation pressure on ambiguous-method error path.
- **Fix:** Clean up `best_match_indices_rev` on early-return paths.

#### M65. `jit_expr_family_for_tag` Non-Exhaustive Switch With No Default

- **File:** `src/lisp/jit_compile_expr_dispatch.c3:69-111`
- **Issue:** Switch on `ExprTag` lacks a `default` case. If a new tag is added and not mapped, control falls off the end and returns an uninitialized value.
- **Impact:** JIT mis-compilation or crash when encountering unmapped expression tags.
- **Fix:** Add `default: return JIT_EXPR_FAMILY_INVALID;`.

#### M66. `jit_env_copy_fault_message` Non-Exhaustive Switch

- **File:** `src/lisp/jit_closure_runtime.c3:82-101`
- **Issue:** Switch on `BoundaryEnvCopyFault` has no `default`. If the enum is extended, the function returns an undefined slice.
- **Impact:** Garbage error messages or crashes on new fault codes.
- **Fix:** Add `default: return "jit: unknown env-copy fault";`.

#### M67. `parse_program` Has No Max Expression Limit

- **File:** `src/lisp/parser_top_level_parse.c3:37-59`
- **Issue:** The `while (!lex.at_end() && !p.has_error)` loop keeps allocating `Expr*` nodes. A pathological input with millions of top-level expressions can exhaust memory.
- **Impact:** OOM DoS.
- **Fix:** Add a `MAX_PROGRAM_EXPRS` cap and error out if exceeded.

#### M68. `build_default_module_rel_path` Uses Hardcoded 255 Buffer Limit

- **File:** `src/lisp/jit_module_import_setup.c3:92-117`
- **Issue:** Returns false if `4 + sym_name.len + 5 > 255`. Callers do not propagate this error clearly.
- **Impact:** Confusing error message; potential truncation if caller ignores return value.
- **Fix:** Ensure all callers handle `false` return and emit a precise error.

#### M69. `jit_warm_expr_cache` `E_WITH_MODULE` Body Elements Not Null-Checked Before Recursing

- **File:** `src/lisp/jit_apply_eval.c3:258-263`
- **Issue:** Inside the `E_WITH_MODULE` case, `expr.with_module.body[i]` elements are not null-checked before recursing.
- **Impact:** Crash on malformed AST.
- **Fix:** Add `if (expr.with_module.body[i] != null)` before the recursive call.

#### M70. `handle_effect_state_capture_handler_copy` Leaks on Promotion Failure

- **File:** `src/lisp/jit_handle_signal_helpers.c3:166-194`
- **Issue:** If `boundary_promote_to_root` fails at iteration `i > 0`, previously promoted closures in `copied_closures[0..i-1]` are not released.
- **Impact:** Minor memory leak on error paths.
- **Fix:** Clean up promoted values in `copied_closures[0..i-1]` on failure.

#### M71. `jit_apply_multi_args_iterative` Does Not Check for Null Intermediate Results

- **File:** `src/lisp/jit_apply_multi_prims.c3:214-225`
- **Issue:** `result = jit_apply_value(result, arg, interp)` — if `result` becomes null after an iteration, the next iteration passes null to `jit_apply_value`.
- **Impact:** Potential null dereference.
- **Fix:** Break the loop early if `result == null || result.tag == ERROR`.

#### M72. `jit_continuation_drive_blocked_fiber` Can Spin If Fiber State Is Corrupted

- **File:** `src/lisp/jit_continuation_runtime.c3:54-73`
- **Issue:** `while` loop re-resumes a suspended context. If `target.status` never changes, this loops forever.
- **Impact:** Potential infinite loop on scheduler/fiber state corruption.
- **Fix:** Add a max-iteration guard and fail closed.

#### M73. `Compiler.compile_to_temp_non_null` Does Not Check `expr` for Null Before Tag Access

- **File:** `src/lisp/compiler_temp_core.c3:20-58`
- **Issue:** `compile_to_temp_non_null` dereferences `expr.tag` without null check. The wrapper `compile_to_temp` checks `expr == null`, but any direct call crashes.
- **Impact:** Null dereference if internal code calls the non-null variant directly with null.
- **Fix:** Add `assert(expr != null)` or a guard at the top.

---

### 🟢 Low (New)

#### L37. `register_stdlib` Ignores `run()` Return Value

- **File:** `src/lisp/eval_stdlib_loader.c3:50`
- **Issue:** `run(src[start..pos - 1], interp);` return value is discarded. Errors during stdlib loading are silently ignored.
- **Fix:** Check the return value and propagate/halt on error.

#### L38. `ffi_symbol_name_equals` Missing Null Check on `interp`

- **File:** `src/lisp/value_predicates_accessors_basic.c3:221-223`
- **Issue:** `interp.symbols.get_name(sym)` accessed without checking `interp != null`.
- **Fix:** Add null guard.

#### L39. `eval_ffi_lib` Doesn't Check `interp.global_env` Null Before Use

- **File:** `src/lisp/eval_ffi_lib.c3:70-75`
- **Issue:** `env_define_with_barrier_checked(interp.global_env, ...)` called without verifying `interp.global_env != null`.
- **Fix:** Add null check.

#### L40. `eval_path_step` Inconsistent Null Checking Strategy

- **File:** `src/lisp/eval_path.c3:117-125`
- **Issue:** The first three branches check `current != null && current.tag == X && current.x_val != null`. The `CONS` path delegates to `eval_path_cons_step` which checks `current == null`. This is inconsistent.
- **Fix:** Unify null checking strategy.

#### L41. `compiler_emit_symbol_name` Can Emit Extremely Long Identifiers

- **File:** `src/lisp/compiler_output_symbol_helpers.c3:22-62`
- **Issue:** Each special char expands to a multi-char replacement. A long symbol name with many special chars can expand to several KB.
- **Fix:** Cap symbol length before emission.

#### L42. `jit_cache_find_slot` Probe Limit Hardcoded to 16

- **File:** `src/lisp/jit_compiler_cache.c3:75-92`
- **Issue:** Linear probing stops after 16 slots. Under hash collision, this can fail to find an existing entry or an empty slot even when the table is not full.
- **Fix:** Use quadratic probing or robin-hood hashing, or increase probe limit.

#### L43. `jit_retired_code_insert` Does Not Use Any Synchronization

- **File:** `src/lisp/jit_compiler_retired_code.c3:26-45`
- **Issue:** Global array `g_jit_retired_codes` is accessed without locks. If JIT compilation ever runs on multiple threads, this is a race.
- **Fix:** Document that JIT is single-threaded, or add atomics/mutexes.

#### L44. `aot_bridge_interp` and `aot_resolve_interp` Return Null Without Raising Error

- **File:** `src/lisp/aot_runtime_bridge_closure.c3:34-46`
- **Issue:** When `g_aot_interp` is null, these functions return null silently. Callers lose context.
- **Fix:** Log a diagnostic when returning null due to missing interpreter.

#### L45. `emit_build_locals_env` Does Not Check `jit_env_extend` Return Value

- **File:** `src/lisp/jit_emit_helpers.c3:103-147`
- **Issue:** After emitting the call to `jit_env_extend`, the result is placed in `V2` without any null-check branch.
- **Fix:** Emit a null-check branch that returns an error value.

#### L46. `jit_make_closure_from_expr` Does Not Check `expr.lambda.body` Validity

- **File:** `src/lisp/jit_closure_support.c3:212-246`
- **Issue:** The check at line 219 uses `expr.lambda.param == INVALID_SYMBOL_ID && !expr.lambda.has_rest`, but does not validate that `expr.lambda.body` is non-null.
- **Fix:** Add `if (expr.lambda.body == null) return jit_raise(...);`.

#### L47. `Lexer.scan_number_fraction` Underflows `frac` Without Bounds

- **File:** `src/lisp/parser_lexer_number_helpers.c3:18-27`
- **Issue:** `frac *= 0.1` in a loop with unbounded digit count eventually underflows to subnormal / zero.
- **Fix:** Cap fractional digit length (e.g., 308) or use `strtod` equivalent.

#### L48. `inet_ntop` Failure Check Uses `0` Instead of `NULL`

- **File:** `csrc/addrinfo_helpers.c:44`
- **Issue:** `if (inet_ntop(...) == 0)` — `inet_ntop` returns `NULL` on failure, never `0`. The condition is always false.
- **Fix:** Change `== 0` to `== NULL`.

---

## Recommended Fix Priority Order

1. **C19** (`scope_guard_owner` macro returns on null but callers continue) — null dereference in all allocators/dtors
2. **C20-C26** (missing `break` in JIT/compiler switches: pattern cache, expr cache, compile groups, compiler `compile_expr`, float32 math) — corrupts/crashes JIT, AOT, and math
3. **C15** (scheduler wakeup missing `break`) — corrupts entire async event loop
4. **C1-C2** (missing `break` in tensor map) — corrupts scalar data
5. **C3** (Vulkan context UAF) — memory safety
6. **C4** (e2e script logic) — breaks CI gate
7. **C5** (parser string escape missing `break`) — corrupts every escaped string
8. **C6** (`make_error` null dereference on OOM) — fatal crash instead of graceful degradation
9. **C7** (REPL server path traversal) — security
10. **C8-C10** (method dispatch null dereferences) — segfault on null method table
11. **C11** (`scope_dtor_value` missing `break` after STRING/ERROR) — type-confusion crash
12. **C16** (`fpu_save` null dereference on x86_64) — segfault in signal handler / context switch path
13. **C17** (BLAS dger buffer overflow) — heap buffer overflow
14. **C18** (JIT multi-arg primitive null dereference) — segfault on null prim_val
15. **H7-H8** (`copy_to_parent_note_tag` / `boundary_escape_shape_note_root` missing `break`) — corrupts all telemetry
16. **H9** (exponent parsing overflow + DoS) — denial of service
17. **H10-H11** (parser datum constructors null dereference) — segfault on root OOM
18. **H12** (primitive arity `<` instead of `!=`) — silently ignores extra arguments
19. **H13** (async I/O handle leak on OOM) — FD/memory leak
20. **H22** (HTTP header slice underflow) — crash on short responses
21. **H23** (Deduce path traversal) — security / arbitrary file access
22. **H24** (unbounded JIT spill-list growth) — memory exhaustion
23. **H27-H29** (`eval_path_*` null dereferences) — segfault on path evaluation
24. **H30** (`Env.hash_lookup` infinite loop) — interpreter hang
25. **H31-H38** (JIT null dereferences in primitive/closure/method-table apply, AOT unbounded tail-call, stack buffer overflow) — runtime crashes
26. **H1** (closure materialize leak) — memory leak on failure path
27. **H4** (CUDA diagonal overflow) — GPU memory safety
28. **H5-H6** (Vulkan refcount/threading) — concurrency safety
29. **M1-M2** (`long.min` UB) — integer overflow
30. **M3-M4** (non-exhaustive switches) — silent misbehavior on enum extension
31. **M10** (FFI callback race) — thread safety
32. **M47-M53** (scheduler/TLS/signal/BLAS/LAPACK/Vulkan/UV races and truncations) — concurrency and correctness
33. **M59-M73** (fifth-pass medium findings: evaluator leaks, JIT non-exhaustive switches, parser limits, compiler null checks) — correctness
34. **M18, L1-L6** (prior audit carry-overs) — hygiene
35. **M19-M31, L13-L26** (second-pass medium/low findings) — correctness and robustness
36. **M54-M58, L27-L36** (fourth-pass build/test/script findings) — CI reliability and test correctness
37. **L37-L48** (fifth-pass low findings: missing null checks, unbounded loops, identifier expansion, inet_ntop bug) — hygiene and edge-case safety
