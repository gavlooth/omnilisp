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
- **Status:** Fixed 2026-04-29.
- **Issue:** Reframed by subagent review. The dangling global pointer portion was already fixed in the current code: shared-context release clears `omni_tensor_vulkan_shared_context` under the context mutex before teardown, and acquisition retains the global only while holding the same mutex. The remaining live defect was that final release unlocked before device/instance destruction and `free(context)`, so lifecycle teardown was not fully serialized.
- **Fix:** Final shared context release now keeps `omni_tensor_vulkan_context_mutex` held through device destruction, instance destruction, and context free. Native regression coverage installs test destroy callbacks and verifies both destroy callbacks run while the mutex is held.

### C4. e2e Baseline Policy Script Logic Bug Always Flags Review Rules

- **File:** `scripts/check_e2e_baseline_policy.sh:191`
- **Status:** Fixed before this checkpoint; revalidated 2026-04-29.
- **Issue:** The original finding cited an older negative condition. Current code uses the positive `review_rule_has_required_doc` predicate and reports `bad_review_rule` only when neither approved review document is present.
- **Fix:** Already implemented. Preserve the centralized predicate and its self-test coverage for memory-only, types-only, both-docs, and no-approved-doc rules.
- **Validation:** `./scripts/check_e2e_baseline_policy.sh --self-test-review-rule` and `./scripts/check_e2e_baseline_policy.sh` both passed.

---

## 🟠 High (Fix in Next Sprint)

### H1. `stable_escape_materialize_init_closure` Leaks `closure` and Sub-Allocations on Failure Paths

- **File:** `src/lisp/eval_boundary_commit_destination.c3:166,174,181-183`
- **Status:** Invalidated 2026-04-29 as stale/incorrect ownership model.
- **Issue:** The original finding assumed `closure` and its sub-allocations are individually heap-owned and must be manually freed on failure. Current stable materialization allocates closure payloads in a staged ESCAPE `ScopeRegion` build scope; failure aborts that build scope, and success splices the staged ESCAPE lane into the destination. Adding ad-hoc frees would violate the region-owned value-lifetime contract.
- **Fix:** Preserve the staged build-scope transaction boundary and the hard `scope_register_dtor_escape(..., scope_dtor_closure)` success precondition. Do not add per-object frees for ESCAPE-lane closure payloads.
- **Validation:** C3 diagnostics passed for `eval_boundary_commit_destination.c3`, `eval_boundary_commit_escape_builder_helpers.c3`, `jit_runtime_effects_signal.c3`, and `jit_handle_signal_helpers_runtime_effects.c3`; `c3c build main` linked; bounded container `memory-lifetime-smoke` passed with `pass=310 fail=0`.

### H2. `jit_signal_try_fast_path_dispatch` Can Pass Null `prim` to Dereferencing Helper

- **File:** `src/lisp/jit_runtime_effects_signal.c3:108-113`
- **Status:** Fixed 2026-04-29.
- **Issue:** Reframed by subagent review. Both `jit_signal_try_fast_path_dispatch` and the legacy `jit_signal_try_fast_path` helper accepted matched fast-path table entries as trusted and dereferenced `fp_prim.prim_val` before validating that the entry was a non-null `PRIMITIVE` with a non-null primitive payload and function pointer.
- **Fix:** Added a shared fast-path primitive-entry guard before the first `prim_val` read in both paths. A malformed matched entry now fails closed with `runtime/invalid-fast-path-primitive` and the dispatch path marks the fast-path entry handled instead of falling through or crashing. JIT policy coverage corrupts a matched fast-path entry and verifies both paths return errors.

### H3. `jit_apply_continuation` Missing Null/Tag Check on `k_val`

- **File:** `src/lisp/jit_handle_signal_handle.c3:255-260`
- **Status:** Fixed 2026-04-29.
- **Issue:** `jit_apply_continuation` dereferenced `k_val.cont_val` without first checking `k_val != null` or `k_val.tag == CONTINUATION`. The normal `jit_apply_value` path was already guarded, but the direct helper boundary could still crash on malformed callers.
- **Fix:** Added null/tag validation before union field access. The helper now fails closed with `cannot resume continuation: invalid continuation` for null or non-continuation values, and keeps the existing null-continuation-payload error after the tag check. JIT policy coverage calls the helper directly with both invalid inputs.

### H4. CUDA Diagonal Kernels Buffer Overflow

- **File:** `csrc/tensor_cuda_complex_matrix.cu:49-75`
- **Status:** Fixed 2026-04-29.
- **Issue:** The diagonal extraction kernels accessed `input[index * cols + index]` after checking only `index >= diagonal_count`. Public wrappers currently pass `diagonal_count = min(rows, cols)`, but the kernel contract still allowed direct or future callers to pass an inconsistent `diagonal_count` and read beyond the `rows x cols` input allocation.
- **Fix:** Hardened both Complex128 and Complex64 CUDA diagonal kernels with `index >= rows || index >= cols` bounds checks, regenerated the embedded PTX include parts, and added rectangular CUDA diagonal smoke coverage behind the existing structural capability gates.

### H5. Vulkan Buffer Handle Refcount Is Not Thread-Safe

- **Files:**
  - `csrc/tensor_vulkan_helpers_core_context.inc:85-98`
  - `csrc/tensor_vulkan_helpers.c:433-438`
- **Issue:** `omni_tensor_backend_vulkan_destroy_buffer_handle` and `omni_tensor_backend_vulkan_retain` manipulate `handle->ref_count` without any synchronization. Two threads retaining or releasing the same buffer concurrently can race on the refcount.
- **Impact:** Double-free or leak.
- **Fix:** Use atomic operations (`__atomic_fetch_add` / `__atomic_fetch_sub`) or a spinlock.

### H6. Vulkan Context Release Destroys Device Outside Mutex

- **File:** `csrc/tensor_vulkan_helpers_core_context.inc:71-82`
- **Status:** Fixed 2026-04-29.
- **Issue:** Subagent review confirmed the current acquisition path no longer observes a freed shared pointer, but final release still unlocked after clearing the global and before destroying the Vulkan device/instance and freeing the context.
- **Impact:** The shared context lifecycle contract did not serialize final release, device destruction, and context free with shared acquisition/release.
- **Fix:** Hold `omni_tensor_vulkan_context_mutex` through the full final destruction sequence. Added native test coverage that records mutex state from stub destroy callbacks.

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

- **Status:** Closed on 2026-04-30.
- **File:** `csrc/tensor_lapack_helpers.c:309-333`
- **Issue:** The `if` condition is a disjunction of all symbols. If any is non-NULL, all are assigned, including unchecked NULL pointers.
- **Fix:** The resolver now publishes each LAPACKE symbol independently and keeps
  QR availability gated on both `LAPACKE_dgeqrf` and `LAPACKE_dorgqr`. A native
  partial-symbol-table contract test covers independent availability.

### M17. `eval_pattern_equality.c3` Missing Cases for Big-Number and Complex Types

- **File:** `src/lisp/eval_pattern_equality.c3:30-58`
- **Issue:** The first `switch (a.tag)` falls through to `default: return a == b;` for `COMPLEX128`, `COMPLEX64`, `BIG_INTEGER`, `BIG_FLOAT`, `BIG_COMPLEX`, `PRIMITIVE`, `PARTIAL_PRIM`, `CLOSURE`, `CONTINUATION`, `ERROR`, `TYPE_INFO`, `INSTANCE`, `METHOD_TABLE`, `MODULE`, `ITERATOR`, `COROUTINE`, `FFI_HANDLE`, `TENSOR`. For heap-allocated types like `BIG_INTEGER`, identity equality is wrong (two distinct values with the same numeric value compare unequal).
- **Fix:** Add explicit cases for all `ValueTag` variants. For `BIG_INTEGER` / `BIG_FLOAT` / `BIG_COMPLEX`, delegate to existing `big_*_values_equal` helpers.

### M18. Format String Mismatches in Boundary Telemetry / Logging

- **Files:**
  - `src/scope_region_temp_pool_stats.c3:260-264` — `%d` used with `usz` fields
  - `src/lisp/eval_boundary_graph_audit_logging.c3:14-75` — `%d` used with `(long)` casts
  - `src/lisp/eval_boundary_graph_audit_telemetry.c3` — same `%d`/`(long)` pattern
- **Status:** Fixed 2026-04-30.
- **Impact:** Silent truncation or UB on platforms where `long`/`usz` differ from `int`.
- **Fix:** Boundary graph audit logging and verbose telemetry now use `%ld`
  for explicit `long` casts. Scope transfer stat printing now formats `usz`
  values through a bounded decimal helper and prints the resulting
  null-terminated strings, because this C3 formatter rejects C-style `%zu`,
  `%llu`, and `%u`.
- **Validation:** C3 diagnostics passed for touched files; `c3c build main`;
  scope suite passed (`pass=69 fail=0`); file-size gate passed;
  `git diff --check` passed.

---

## 🟢 Low (Code Hygiene)

### L1. `char[]` Slices Still Passed to `%s` Format Specifier

- **Files:**
  - `src/scope_region_global_guards.c3:54`
  - `src/lisp/eval_boundary_graph_audit_meta.c3:15-37`
  - `src/lisp/eval_boundary_diagnostics.c3:34`
  - `src/lisp/eval_boundary_graph_audit_telemetry.c3:39`
  - `src/lisp/eval_boundary_telemetry.c3:475`
- **Status:** Fixed 2026-04-30.
- **Impact:** `char[]` is a slice not guaranteed null-terminated; UB if it lacks trailing `\0`.
- **Fix:** Scope owner-guard operation names now flow as `ZString` through the
  shared guard/violation helpers, and boundary reason/provenance name helpers
  now return `ZString` because they are backed by string-literal tables. Existing
  callers either pass those values directly to `%s` or cast explicitly.
- **Validation:** C3 diagnostics passed for touched files; `c3c build main`;
  scope suite passed (`pass=69 fail=0`); file-size gate passed;
  `git diff --check` passed.

### L2. `ScopeRegion.alloc` / `alloc_escape` Still Missing `@require self != null`

- **File:** `src/scope_region_allocators.c3:89,111`
- **Status:** Invalidated 2026-04-30 as stale under current fail-closed
  contract.
- **Fix:** Current source explicitly returns `null` for null `self` in
  `ScopeRegion.alloc`, `ScopeRegion.alloc_escape`, and
  `ScopeRegion.alloc_escape_attributed`, which is stronger for current callers
  than an assertion-only `@require`.

### L3. `scope_reset_temp_lane` Still Leaves `alloc_bytes` / `alloc_count` Unchanged

- **File:** `src/scope_region_reset_helpers.c3:74-104`
- **Status:** Invalidated 2026-04-30 as already fixed in current source.
- **Fix:** Current `scope_reset_temp_lane` zeroes both `alloc_bytes` and
  `alloc_count` before resetting TEMP telemetry sequence state.

### L4. `scope_freelist_cleanup` Still Omits `g_scope_global_mu.destroy()`

- **File:** `src/scope_region_reset_adopt.c3:112-136`
- **Status:** Invalidated 2026-04-30 as already fixed in current source.
- **Fix:** Current `scope_freelist_cleanup` calls
  `scope_global_destroy_after_shutdown()`, which destroys the global mutex and
  marks it destroyed after freelist/chunk cleanup.

### L5. Generation Counter Wraparound Theoretical Risk

- **Files:** `src/scope_region.c3`, `src/scope_region_global_guards.c3`,
  `src/lisp/value_core_types.c3`, `src/lisp/value_environment_storage.c3`,
  `src/lisp/value_runtime_types.c3`, `src/lisp/stable_escape_store.c3`,
  `src/lisp/eval_boundary_scope_chain.c3`,
  `src/lisp/eval_promotion_context.c3`
- **Status:** Fixed 2026-04-30.
- **Fix:** Scope generations now use `ulong` across `ScopeRegion`,
  `Value.scope_gen`, closure/env stamps, stable escape passports, and
  promotion scope-chain cache keys. Added a basic regression that forces the
  scope generation counter above `uint.max` and verifies TEMP and ESCAPE value
  stamps preserve the widened generation.

### L6. Missing `@require` Contracts on Core Public APIs

- **Files:**
  - `src/scope_region_reset_adopt.c3:7` — `scope_splice_escapes`
  - `src/scope_region_chunk_helpers.c3:61` — `scope_register_dtor`
  - `src/scope_region_chunk_helpers.c3:75` — `scope_register_dtor_escape`
  - `src/scope_region_destroy.c3:98` — `scope_destroy_owned_descendants`
- **Status:** Reframed 2026-04-30 as stale for the null-guarded APIs.
- **Fix:** `scope_register_dtor` and `scope_register_dtor_escape` now fail
  closed on null scope or null destructor; `scope_destroy_owned_descendants`
  already has `@require scope != null`. `scope_splice_escapes` remains a
  stricter public API candidate for future explicit contract annotation because
  the current caller contract already checks owner/parent-child legality before
  splice execution.

### L7. Code Duplication: Integer Serialization

- **Files:** `compiler_expr_serialize_values.c3:76-95`, `value_predicates_accessors_core.c3:14-31`, `compiler_output_helpers.c3:76-85`
- **Status:** Fixed 2026-04-30.
- **Issue:** Prior audit overstated the live correctness risk: current source
  already preserved `long.min` in compiler/value serialization. The remaining
  defect was duplicated signed integer emission logic in `Compiler.emit_int`.
- **Fix:** `Compiler.emit_int` now uses the shared `int_to_string` helper, so
  source serialization and native compiler output share the same signed-long
  conversion path.

### L8. Defensive Null Check Missing in `jit_signal_call_fast_path_primitive_preserving_error`

- **File:** `src/lisp/jit_runtime_effects_signal.c3:12-16`
- **Status:** Invalidated 2026-04-29 as stale under the current dispatch contract.
- **Issue:** The helper-local null-check proposal missed the shared fail-closed guard added for H2. Both current and legacy fast-path dispatch paths validate matched table entries with `jit_fast_path_primitive_is_valid` before the helper can read `prim_val`.
- **Fix:** No production change needed. If the helper is ever exposed to direct unguarded callers, mirror the existing fail-closed invalid-entry error behavior rather than returning `null`, because a null return can suppress the broken fast-path contract.
- **Validation:** Targeted `jit-policy` filter `fast-path-malformed-primitive-entry-fails-closed` passed with `pass=1 fail=0`.

### L9. `boundary_graph_audit_visit_env` Lacks Defensive `env == null` Guard

- **File:** `src/lisp/eval_boundary_graph_audit_walkers.c3:5-79`
- **Status:** Fixed 2026-04-30.
- **Issue:** The function assumed `env != null` before checking terminal env
  state or iterating bindings.
- **Fix:** `boundary_graph_audit_visit_env` now treats null envs as an OK
  empty terminal. Added direct graph-audit regression coverage and a focused
  `boundary-graph-audit` Lisp slice so this boundary can be validated without
  the broad `memory-lifetime-smoke` suite.

### L10. Vulkan Barrier Helper Functions Dereference Without Null Check

- **File:** `csrc/tensor_vulkan_helpers_dispatch_multi_output.c:2-174`
- **Status:** Fixed 2026-05-01.
- **Issue:** Static barrier helpers built `OmniVulkanBufferMemoryBarrier`
  structs by directly dereferencing output handles before checking them.
- **Fix:** Two-, three-, and four-output barrier helpers now return before
  dereferencing null outputs. Added native Vulkan resource-safety coverage that
  verifies null-output barriers do not call the Vulkan pipeline-barrier hook.

### L11. Vulkan `copy_range_to_host` Maps Entire Buffer for Small Ranges

- **File:** `csrc/tensor_vulkan_helpers.c:42-64`
- **Status:** Invalidated 2026-05-01 as already fixed in current source.
- **Issue:** Original finding matched older code that mapped the whole buffer.
- **Fix:** Current `omni_tensor_backend_vulkan_copy_range_to_host` maps the
  requested `(offset, byte_len)` range, and
  `omni_tensor_backend_vulkan_copy_range_subrange_map_for_tests` validates the
  offset/size passed to the Vulkan map hook.

### L12. `mktemp` Without Template (Portability)

- **File:** `scripts/run_global_gates.sh:191`
- **Status:** Fixed 2026-05-01.
- **Issue:** `asan_build_log="$(mktemp)"` omitted the required template
  argument for BSD/macOS portability.
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
| M6 — BLAS symbol resolution | 2026-04-24 | **FIXED 2026-04-30** | `csrc/tensor_blas_helpers.c`, `tests/native/stack_fpu_blas_contract_test.c` |
| M7 — LAPACK symbol resolution | 2026-04-24 | **FIXED 2026-04-30** | `csrc/tensor_lapack_helpers.c`, `tests/native/stack_fpu_blas_contract_test.c` |
| M8 — Vulkan queue family selector null guards | 2026-04-24 | **FIXED 2026-05-01** | `csrc/tensor_vulkan_helpers_core.c`, `tests/native/vulkan_resource_safety_test.c` |
| M9 — Default cases in non-exhaustive switches | 2026-04-24 | **FIXED 2026-04-30** | `src/lisp/compiler_native_literal_compilation_flat_style.c3`, `src/lisp/compiler_program_top_level_globals.c3`, `src/lisp/aot_runtime_bridge.c3`, `src/lisp/aot_runtime_bridge_helpers.c3`, compiler fail-closed tests |
| L1 — Generation wraparound | 2026-04-24 | **NOT FIXED** | `src/lisp/eval_boundary_provenance.c3` |
| L2 — Missing `@require` on public APIs | 2026-04-24 | **NOT FIXED** | `src/scope_region_*.c3` |
| C1 — JIT reset/shift StackCtx leak | 2026-04-23 | **FIXED** | `src/lisp/jit_reset_shift.c3` |
| H3/H4 — Continuation leaks on JIT failure | 2026-04-23 | **FIXED** | `src/lisp/jit_handle_signal.c3`, `jit_runtime_effects_handle.c3` |
| H1 — UI FTXUI partial-failure leak | 2026-04-23 | **NOT REPRODUCIBLE** | `src/lisp/prim_ui_ftxui.c3` |
| H2 — CUDA `atomicCAS` error semantics | 2026-04-23 | **FIXED** | `csrc/tensor_cuda_complex_map.cu` |
| PAT_GUARD serialization | 2026-04-23 | **FIXED** | `src/lisp/compiler_expr_serialize_patterns.c3` |
| Mutable capture detection | 2026-04-23 | **FIXED** | `src/lisp/compiler_mutable_capture_*.c3` |

---

2026-04-30 M9 sub-slice:
- Native/AOT literal lowering no longer silently emits `nil` for unsupported
  value tags. Dictionary, set, array, primitive, and definition-time global
  closure literals now lower through explicit runtime constructors or generated
  AOT globals; runtime-only objects still fail closed.
- AOT primitive lookup now fails closed when a generated primitive reference is
  absent from the runtime global environment instead of returning `nil`.
- AOT module export lookup now fails closed when an export is declared but has
  no module-environment binding instead of returning `nil`.
- AOT variable lookup now fails closed on unbound symbols instead of returning
  `nil`, and `define_var` propagates incoming error values instead of
  publishing them as successful definitions.
- AOT callable match guards now fail closed if the guard callback returns a
  null value instead of treating the malformed callback result as a false
  guard.
- Compiler source serialization now fails closed on unknown expression tags and
  unsupported singleton type-literal tags instead of serializing either as
  `nil`.
- AOT type-annotation metadata emission now rejects unsupported singleton
  literal value tags only when `has_val_literal` is set, while leaving inactive
  metadata storage fields harmless.
- AOT quasiquote lowering now fails closed for standalone unquote-splicing and
  unsupported internal template expression tags instead of emitting `nil`.
- AOT match pattern lowering now fails closed for unknown internal pattern tags
  instead of compiling them as catch-all matches.
- FFI contract manifest emission now fails closed for invalid raw ABI type tags
  instead of publishing them as `Void`.
- Generated-global collection now fails closed for unknown internal expression,
  pattern, and literal value tags, and the inline-module backing collector
  explicitly traverses `with` bodies instead of silently skipping them.
- Lambda scanning now fails closed for unknown internal expression and pattern
  tags instead of reporting "no lambdas found".
- Free-variable analysis now fails closed for unknown internal expression tags
  and malformed pattern-binding tags instead of producing an incomplete capture
  set.
- Mutable-capture prescan now fails closed for unknown internal expression and
  pattern tags while explicitly traversing module and scoped module-open bodies.
- Quasiquote free-variable analysis now fails closed for unknown internal
  expression tags while preserving valid template forms as capture-free.
- AOT match binding lowering now fails closed for unknown internal pattern tags
  instead of silently emitting no bindings.
- AOT match guard scan/lowering now fails closed for unknown internal pattern
  tags instead of treating malformed guard patterns as no guard work.
- Inline-module export classification and local collection now fail closed for
  unknown internal expression/pattern tags instead of treating malformed module
  metadata inputs as non-exporting no-ops.
- Type metadata value-tag emission now fails closed for unsupported value tags
  while preserving explicit `NIL` emission as a valid metadata tag.
- FFI preload and contract-manifest discovery now fail closed for unknown
  internal expression tags instead of silently suppressing preload/manifest
  output; quasiquote template forms remain explicit no-op cases.
- Runtime sequence-pattern matching now fails closed for unknown internal rest
  positions instead of treating malformed sequence patterns as ordinary
  non-matches.
- Runtime literal-dispatch matching now fails closed for malformed internal
  `ValueLiteralKey` tags instead of treating corrupted method signatures as
  ordinary literal mismatches or falling through to method-table fallback.
- JIT continuation-sensitivity scans now treat unknown expression tags as
  continuation-sensitive instead of ordinary non-sensitive results, preventing
  malformed trees from selecting the non-continuation fast call path by default.
- Final current-source classification found the remaining compiler/AOT
  `default:` arms are explicit fail-closed diagnostics, parent-dispatched
  helper fallbacks, or benign format/classification defaults. No success-shaped
  fallback remains in the audited compiler/AOT default-switch surface, so the
  broad M9 row is closed.

2026-04-30 follow-up runtime FFI audit:
- `AUDIT-255-FFI-INVALID-RETURN-ABI-TAG` is closed. Malformed internal FFI
  return ABI tags now fail closed before libffi preparation, return storage
  selection returns null for unsupported tags, and return-value conversion plus
  async FFI offload report typed errors instead of successful `nil`.
- `AUDIT-256-FFI-ASYNC-INVALID-ARG-ABI-TAG` is closed. Async FFI argument ABI
  metadata now validates raw integer tags before enum conversion or worker
  native-call handoff, and malformed internal async call contexts fail closed
  with typed errors before `omni_ffi_call`.
- `AUDIT-257-FFI-ASYNC-VOID-RETURN-CONTRACT` is closed. Valid async FFI
  `^Void` returns now use dummy native return storage, cross the scheduler with
  explicit `OFFLOAD_RES_VOID`, and materialize as the language `Void`
  singleton instead of being rejected as unsupported or mapped to `nil`.
- `AUDIT-258-FFI-ASYNC-FLOAT32-RETURN-CONTRACT` is closed. Async FFI
  `^Float32` returns now cross scheduler completion as `OFFLOAD_RES_FLOAT32`
  and rematerialize as Omni `Float32` values instead of being widened to
  Float64.
- `AUDIT-259-FFI-ASYNC-BOOLEAN-RETURN-CONTRACT` is closed. Async FFI
  `^Boolean` returns now cross scheduler completion as `OFFLOAD_RES_BOOL` and
  rematerialize as Omni Boolean singleton values instead of Integer `0`/`1`.
- `AUDIT-260-FFI-STRUCT-RETURN-STORAGE` is closed. FFI `^Struct` returns now
  use pointer-shaped return storage before the established sync opaque-handle
  conversion or async pointer-like rejection boundary, instead of being rejected
  early as unsupported ABI metadata.

## Second Pass — New Findings (2026-04-24)

**Scope:** Parser, evaluator core, value lifecycle, AOT runtime, async I/O, REPL/server, type system, dispatch, test infrastructure.
**Method:** 4 parallel agent passes + manual verification.

---

### 🔴 Critical (New)

#### C5. Parser String Escape Sequence Switch Missing `break` on Every Case

- **File:** `src/lisp/parser_lexer_string_hash.c3:30-62`
- **Status:** Invalidated 2026-04-29; false positive.
- **Issue:** The original finding assumed C-style implicit switch fallthrough.
- **Fix:** No code fix. C3 switches do not implicitly fall through, so each recognized string escape emits exactly one decoded character.
- **Validation:** Subagent C3 switch probe confirmed no fallthrough; direct runtime probe `./build/main --eval '(length "hello\\nworld")'` returned `11`.

#### C6. `make_error` Dereferences Null on Allocation Failure

- **File:** `src/lisp/value_constructors.c3:342-344`, `src/lisp/value_constructors_core.c3:23-32`
- **Status:** Invalidated 2026-04-29; already fixed in current source.
- **Issue:** The original finding cited an older `make_error` implementation. Current `make_error` checks `interp.alloc_value()` and returns `make_static_out_of_memory_error(msg)` when value allocation fails.
- **Fix:** No code change for this item. Preserve the current fail-closed static OOM fallback. A future tighter regression could directly force `interp.alloc_value()` failure inside `make_error`.

#### C7. REPL Server `load-file` Path Traversal on Unauthenticated Transports

- **File:** `csrc/uv_helpers_pipe.c`, `src/lisp/tests_runtime_async_repl_server_groups.c3`
- **Status:** Fixed 2026-04-29 at the unauthenticated Unix-socket transport boundary.
- **Issue:** `load-file` intentionally accepts client-supplied file paths, including absolute paths documented for editor tooling. TCP already requires `OMNI_REPL_TCP_AUTH_TOKEN`, and stdio is caller-owned, but Unix-socket mode created a filesystem socket without forcing owner-only permissions.
- **Fix:** `omni_unix_socket_listen_fd` now applies `chmod(path, S_IRUSR | S_IWUSR)` immediately after `bind()` and unlinks the socket on permission-hardening failure. Added async REPL-server coverage that binds a test socket and verifies mode `0600`.
- **Validation:** `cc -Wall -Wextra -Werror -c csrc/uv_helpers_pipe.c`, `./scripts/build_omni_chelpers.sh`, C3 diagnostics for the async REPL-server test file, `c3c build main`, and a focused async output check showing `[PASS] async repl-server unix socket is owner-only`. Broad async remains blocked by unrelated file payload and libuv bridge failures.

#### C8. Method Dispatch Dereferences Null Method Table

- **File:** `src/lisp/eval_dispatch_match.c3:83`
- **Status:** Fixed 2026-04-29.
- **Issue:** `find_best_method(MethodTable* mt, ...)` dereferenced `mt.entry_count` without checking if `mt` is null. The original suggested `return null` repair was unsafe because callers treat null as "no match" and may continue into fallback or dispatch-error formatting.
- **Fix:** `find_best_method` and `format_dispatch_error` now fail closed with `runtime/evaluation-error` for null method-table state instead of returning raw null or dereferencing. Regression coverage calls both direct boundaries with null `MethodTable*`.
- **Follow-up 2026-04-30:** `format_dispatch_error` now preserves large
  expected arity values with a full integer buffer instead of truncating
  arity hints through a 4-byte local buffer. Advanced type-dispatch coverage
  verifies a five-digit arity is reported intact.

#### C9. JIT Method Table Dispatch Dereferences Null `method_table_val`

- **File:** `src/lisp/jit_apply_helpers.c3:316-320`
- **Status:** Fixed 2026-04-29; broadened to all JIT method-table apply paths.
- **Issue:** `jit_apply_value_method_table()` assigned `MethodTable* mt = func.method_table_val` and dereferenced `mt.fallback` without checking `mt`. The same payload precondition existed in multi-arg and tail multi-arg method-table apply.
- **Fix:** Added a shared `jit_value_has_method_table_payload` guard and used it in single-arg, multi-arg, and tail multi-arg JIT method-table apply paths. Malformed method-table callables now fail closed with a JIT runtime error.

#### C10. JIT `eval_define` Dereferences Null `method_table_val`

- **File:** `src/lisp/jit_define_method_table.c3:181-182`
- **Status:** Fixed 2026-04-29.
- **Issue:** `jit_eval_define()` dereferenced `existing_for_fallback.method_table_val.fallback` without checking if `method_table_val` is null when adding an untyped fallback to an existing method-table binding.
- **Fix:** The fallback update branch now fails closed with `jit: invalid method table binding` if an existing `METHOD_TABLE` value has a null payload. Regression coverage constructs such a binding and verifies the fallback update returns an error.
- **Validation:** C3 diagnostics passed for touched dispatch/JIT/test files; `c3c build main` linked; targeted `jit-policy` filter `invalid-method-table-state-fails-closed` passed with `pass=1 fail=0`; full `jit-policy` passed with `pass=75 fail=0`; focused advanced type-dispatch validation passed with `pass=253 fail=0`.

---

### 🟠 High (New)

#### H7. `copy_to_parent_note_tag` Switch Missing `break` on Every Case

- **File:** `src/lisp/eval.c3:88-130`
- **Status:** Invalidated 2026-04-29; false positive.
- **Issue:** The original finding assumed C-style implicit switch fallthrough.
- **Fix:** No code fix. C3 switches implicitly break unless `nextcase` is used,
  so non-empty telemetry arms do not cascade. Empty grouped labels intentionally
  share the following counter arm.

#### H8. `boundary_escape_shape_note_root` Switch Missing `break` on Composite Types

- **File:** `src/lisp/eval.c3:160-213`
- **Status:** Invalidated 2026-04-29; false positive.
- **Issue:** The original finding assumed C-style implicit switch fallthrough.
- **Fix:** No code fix. C3 switch arms do not cascade without explicit
  `nextcase`; boundary shape telemetry records only the matched arm and its
  intentional grouped labels.

#### H9. Exponent Parsing Integer Overflow + Potential DoS Loop

- **File:** `src/lisp/parser_lexer_number_helpers.c3:38-47`
- **Status:** Invalidated 2026-04-29 as stale.
- **Issue:** The original finding matches older lexer code. Current
  `scan_number_exponent` bounds positive exponents to 308 and negative
  exponents to 324 before updating `exp_val`, reports
  `"float literal exponent out of range"`, and only runs the multiplier loop
  over the capped value.
- **Fix:** Already implemented before this checkpoint. Preserve the bounded
  lexer and `parse-number` exponent policy.

#### H10. Parser Datum Constructors Dereference Null on Root Allocation Failure

- **File:** `src/lisp/parser_datum_helpers.c3:6-58`
- **Status:** Fixed 2026-04-29.
- **Issue:** The original finding was live and broader than datum helpers:
  parser root-scope allocations in datum construction, collection literals,
  pattern literals, atom parsing, quote/quasiquote helpers, head forms, control
  effects, and relation attributes could write through a null `Value*`.
- **Impact:** Root-scope OOM could crash parser paths instead of surfacing a
  parser error.
- **Fix:** Parser root allocations now route through
  `Parser.alloc_root_value_or_error`, which sets `Parser.has_error` on failure.
  Datum constructors take `Parser*`, propagate null, and collection/template
  builders bail out through the same error state.

#### H11. `parse_datum_list` and `parse_datum_collection_template` Dereference Null Allocations

- **File:** `src/lisp/parser_datum_collections.c3:70-90`
- **Status:** Fixed 2026-04-29 with H10.
- **Issue:** The list/template-specific null dereference was live before the
  shared parser allocation helper conversion.
- **Impact:** Same root-scope OOM crash as H10.
- **Fix:** List, array-template, and dict-template datum construction now use
  fail-closed datum helpers and return null with parser error state set.

#### H12. Primitive Arity Validation Silently Ignores Extra Arguments

- **Files:**
  - `src/lisp/primitives_core.c3:55,64,72,80,88,96,104,113,122,131,140,213`
  - `src/lisp/prim_math_core.c3:41,73,106,139,172,205,222,239,256,273,290,307,324,341,358,380,397,414,431,456,473,485,497,509,523,537,551,565,588,624,660,690,728,742,756,770,783,798`
  - `src/lisp/prim_math_arithmetic.c3:269,277,314`
- **Status:** Fixed 2026-04-29 at the raw primitive-body boundary.
- **Issue:** Public fixed-arity calls were already guarded by primitive arity
  metadata, but direct C3 primitive-body calls and misregistered primitive
  values could still pass oversized argument slices to fixed-arity bodies that
  checked only `args.len < N`.
- **Impact:** Raw body calls could ignore trailing arguments for fixed-arity
  primitives such as arithmetic, comparison, list, predicate, and math
  functions.
- **Fix:** Fixed-arity primitive bodies now enforce exact `args.len != N`.
  Ranged/variadic bodies such as `+` and `-` retain explicit range checks.
  Regression coverage exercises both public eval arity and direct raw body calls
  for representative core and math primitives.

#### H13. Async I/O Handle Leak on FFI Handle Allocation Failure

- **Files:**
  - `src/lisp/async_socket_handle_runtime.c3:33-38` (`make_tcp_stream_handle`)
  - `src/lisp/async_socket_handle_runtime.c3:65-69` (`make_udp_handle`)
  - `src/lisp/async_runtime_base.c3:237-240` (`make_tcp_listener_handle_with_transport`)
  - `src/lisp/async_process_signal_runtime.c3:165-169` (`make_process_handle`)
- **Status:** Invalidated 2026-04-29 as stale.
- **Issue:** The original constructors still return OOM errors on wrapper
  allocation failure, but cleanup is now centralized in
  `make_ffi_handle_ex_with_descriptor`.
- **Impact:** No current leak was found for the cited constructors. Their
  finalizer-backed payloads are released on null interpreter, null target
  scope, FFI box allocation failure, wrapper allocation failure, and destructor
  registration failure.
- **Fix:** Already implemented before this checkpoint. Preserve the centralized
  FFI handle constructor cleanup; avoid duplicating caller-side cleanup unless a
  constructor explicitly keeps ownership.

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
- **Status:** Fixed 2026-04-30.
- **Issue:** `usz max_len = buffer.len - 1;` underflows to `usz.max` if `buffer.len == 0`, causing the loop to write past the buffer.
- **Fix:** Add `if (buffer.len == 0) return out;` before computing `max_len`.
- **Regression coverage:** Basic native coverage calls `read_line` with a real
  empty slice and verifies it returns without EOF/truncation or writes.

#### M23. `repl_eval_buffer` Can Write One Past Buffer End

- **File:** `src/lisp/eval_repl.c3:78`
- **Status:** Fixed 2026-04-30.
- **Issue:** `(*buf)[*buf_len] = 0;` writes at index `8192` if `*buf_len == 8192`.
- **Fix:** Assert or guard `*buf_len < 8192` before null-terminating.
- **Regression coverage:** Basic native coverage calls `repl_eval_buffer`
  with `buf_len == buf.len` and verifies it resets without null-terminating
  past the buffer. Non-interactive direct calls suppress the interactive error
  print; live REPL calls still report the line-too-long error.

#### M24. REPL Server Auth Silently Disabled When Token Is Missing

- **File:** `src/lisp/eval_repl_server_state.c3:174-180`
- **Status:** Fixed 2026-04-30.
- **Issue:** If `auth_required = true` with `auth_token_ptr = null`, `conn.auth_required` becomes `false` silently.
- **Impact:** A configuration that explicitly requests authentication can end up running without it.
- **Fix:** `repl_server_connection_init` now preserves `conn.auth_required`
  whenever auth is requested. Missing, empty, or unterminated token material
  leaves `auth_token_ptr = null`/`auth_token_len = 0`, so protected operations
  fail closed while auth-exempt describe requests still work.
- **Regression coverage:** Basic direct coverage and async REPL-server coverage
  verify missing-token and empty-token secured connections reject `eval`
  requests instead of downgrading to open transport.

#### M25. Worker Queue Mutated Without Holding Worker Mutex

- **File:** `src/lisp/eval_repl_server_worker_helpers.c3:131-141`
- **Status:** Fixed 2026-04-29; re-audited stale 2026-04-30.
- **Issue:** `repl_server_worker_clear_queued_commands` reads and writes `command_queue_head`, `command_queue_count`, and the queue array without acquiring `worker.mu`.
- **Impact:** Latent data race / queue corruption if called concurrently.
- **Fix:** `repl_server_worker_clear_queued_commands` now locks
  `worker.mu` with `defer` unlock symmetry when `worker.mu_ready` is true.
  The current audit pass verified the fix is present and did not rewrite the
  already-hardened path.

#### M26. `is_number()` Excludes Big-Number Types

- **File:** `src/lisp/value_predicates_accessors_basic.c3:106-108`
- **Status:** Fixed 2026-04-30.
- **Issue:** `is_number()` only checks `INT`, `DOUBLE`, `FLOAT32`, `COMPLEX128`, `COMPLEX64`. It excludes `BIG_INTEGER`, `BIG_FLOAT`, and `BIG_COMPLEX`, while `is_numeric_value()` includes them.
- **Impact:** Code using `is_number()` (plot lowering, deduce aggregates, sort array) misclassifies big numeric values.
- **Fix:** `is_number()` now delegates to `is_numeric_value()`. Float64-only
  consumers that previously paired `is_number()` with `to_double()` now use
  `try_numeric_to_double()` so complex and out-of-range big values still fail
  closed.
- **Regression coverage:** Advanced numeric helper coverage verifies
  BigInteger and BigFloat values pass through the shared double-check path.

#### M27. `to_double()` Missing `BIG_INTEGER` Case

- **File:** `src/lisp/value_predicates_accessors_basic.c3:117-126`
- **Status:** Fixed 2026-04-30.
- **Issue:** `to_double()` handles `DOUBLE`, `FLOAT32`, `BIG_FLOAT`, and defaults to `(double)v.int_val`, but has no case for `BIG_INTEGER`. A `BIG_INTEGER` value falls through to `(double)v.int_val`, which is garbage because `int_val` is not the active union member.
- **Fix:** `to_double()` now routes through `try_numeric_to_double()`, which
  already owns the BigInteger/BigFloat conversion contract and rejects
  complex or unrepresentable values instead of reading inactive union storage.
- **Regression coverage:** Advanced numeric helper coverage evaluates a
  BigInteger through `test_double`, which would fail on the old helper because
  `is_number()` rejected it and `to_double()` read `int_val`.

#### M28. `register_builtin_types()` Missing `tid_Float` Cache

- **File:** `src/lisp/eval_dispatch_types.c3:75-101`
- **Status:** Fixed 2026-04-30.
- **Issue:** `interp.sym_Float` is registered but `interp.tid_Float` is never cached, while `tid_Float32`, `tid_Integer`, etc. are cached.
- **Impact:** Inconsistent fast-path dispatch for `Float` lookups.
- **Fix:** `Interp` now caches `tid_Float`, `register_builtin_types()` fails
  closed if the lookup is missing, and `Float` is parented under `Number`.
  The interpreter ABI size guard was updated for the new cached field.
- **Regression coverage:** Basic native coverage verifies `tid_Float` is
  initialized and parented to `Number`.

#### M29. Type Registry Hash Insertion Gap When Full

- **File:** `src/lisp/value_type_registry.c3:254-260`
- **Status:** Fixed 2026-04-30.
- **Issue:** `register_type()` scans all slots but if the hash table is completely full, it returns without inserting, leaving the type unregistered but with an allocated ID.
- **Fix:** `register_type()` now inserts hashable names through a checked
  shared hash-insert helper and rolls back the just-published type slot and
  `type_count` if no hash slot is available. Type-registry growth now uses the
  same checked helper for rehashing.
- **Regression coverage:** Basic native coverage forces a malformed full
  type-registry hash index and verifies registering a second hashable type
  returns `INVALID_TYPE_ID`, preserves `type_count`, leaves the existing type
  reachable, and keeps the rejected type unreachable.

#### M30. Test Error Helper Only Checks `has_error`, Not Content

- **File:** `src/lisp/tests_harness_helpers.c3:234`
- **Issue:** `test_error()` only checks `ri.error.has_error`. It does not verify the error code, message, or type.
- **Impact:** Tests can pass for the wrong reason (e.g., OOM instead of expected type error).
- **Fix:** Migrate callers to `test_error_contains()` or add content validation.
- **Status:** Closed 2026-04-30. The shared helper now requires a non-empty
  non-allocation error payload, and high-signal core/numeric/typed-effect
  callers now use `test_error_contains()` for the exact expected contract.
- **Regression coverage:** Targeted advanced filters passed for core
  semantics, stdlib numeric, type dispatch, FFI/system, and collections module
  coverage. Full macro-hygiene validation exposed an unrelated hard exit in
  the `advanced-macro-hygiene-string-number` recursion fixture, tracked as
  `AUDIT-253-MACRO-HYGIENE-RECURSION-HARD-EXIT`.

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
- **Status:** Invalidated 2026-04-29; false positive under C3 switch semantics.
- **Issue:** The original finding assumed C-style implicit switch fallthrough. C3 switches do not implicitly fall through, so the STRING/ERROR cleanup arm does not execute the CLOSURE arm.
- **Fix:** No C-style `break` repair needed. Adjacent M32 hardening made scalar/no-op destructor policy explicit.

#### C12. `scope_dtor_value` Cascade of Missing `break` Statements from PRIMITIVE Through VOID

- **File:** `src/lisp/value_constructors_lifecycle.c3:22-103`
- **Status:** Invalidated 2026-04-29; false positive under C3 switch semantics.
- **Issue:** The original finding assumed C-style implicit switch fallthrough. The apparent cascade is not live C3 behavior.
- **Fix:** No C-style `break` repair needed. Adjacent M32 hardening made every current `ValueTag` destructor/no-op policy explicit.

#### C13. Module Path Traversal in Import Resolution

- **File:** `src/lisp/jit_module_setup_helpers.c3:171-195`
- **Status:** Fixed 2026-04-29.
- **Issue:** `resolve_import_path_checked` concatenated `interp.source_dirs[interp.source_dir_count - 1]` with caller-controlled path bytes. It did not reject exact `..` path segments, absolute paths, or embedded NUL bytes, and the same resolver also served `(load path)`.
- **Fix:** Added a shared status-returning source-relative resolver that rejects leading `/`, embedded NUL bytes, and exact `..` segments before concatenation while preserving normal nested relative paths and default dotted-module paths. Import and load callers now report unsafe paths separately from overflow/truncation. Regression coverage checks resolver policy, string import rejection, load rejection, nested relative preservation, overflow, dotted default paths, and existing path-cache behavior.
- **Validation:** C3 diagnostics passed for the resolver, import, load, and advanced module test files; `c3c build main` linked; advanced collections/module slice passed with `pass=2141 fail=0`; compiler type-dispatch slice passed with `pass=411 fail=0`; basic slice passed with `pass=174 fail=0`; file-size gate and whitespace checks passed.

#### C14. Strict Inner Handler Blocks Effects Handled by Outer Handlers

- **Files:** `src/lisp/jit_handle_signal.c3:42-61`, `src/lisp/jit_runtime_effects_signal.c3:124-157`
- **Status:** Invalidated 2026-04-29; contract-inverted false positive.
- **Issue:** The finding describes `^strict` as if it should delegate transparently to outer handlers. Current normative semantics say handler search continues outward unless blocked by `^strict`, and `^strict` raises at the strict boundary instead of falling through.
- **Fix:** No code change. Preserve strict handler boundary behavior in `jit_signal_impl`, `jit_signal_try_resume_handler_or_strict_boundary`, and `explain 'effect`. Changing this behavior would be a language contract change requiring docs and tests to be rewritten.
- **Validation:** Subagent validation reported basic slice `pass=174 fail=0` and `jit-policy` `pass=75 fail=0`. Existing schema/advanced broader failures on that audit path were unrelated nested-handle/DNS-IO validation blockers, not C14 evidence.

---

### 🟠 High (New)

#### H15. Continuation Leak on Interpreted Handler Clause Error

- **File:** `src/lisp/jit_handle_signal.c3:151-154`
- **Issue:** In `jit_handle_dispatch_signal`, after `jit_eval(clause.handler_body, clause_env, interp)` returns an `ERROR`-tagged value, the function returns the error directly without discarding the continuation `k` allocated at line 107. The JIT path correctly calls `interp_discard_lisp_continuation` on error, but the interpreted path omits this.
- **Impact:** Every handler clause evaluation that raises an error leaks a `Continuation` node onto `interp.continuation_head`.
- **Fix:** After `jit_eval`, check `if (handler_v != null && handler_v.tag == ERROR) { interp_discard_lisp_continuation(interp, k); return handler_v; }`.

#### H16. Module Cache Corruption and Duplicate Modules on Reload

- **Files:** `src/lisp/jit_module_setup_helpers.c3:289-322` (`jit_create_file_module`), `src/lisp/jit_module_setup_helpers.c3:75-138` (`jit_eval_module_impl`)
- **Status:** Fixed 2026-04-29.
- **Issue:** (1) file-module publication appended a new module without enforcing
  resolved-path uniqueness. (2) same-name unloaded module replacement cleared
  `name` directly, leaving stale storage/hash state. (3) table growth and
  default-import resolution could preserve or bypass stale path/hash entries.
- **Fix:** File-module creation now reuses an existing module for the same path;
  implicit file loads fail closed on in-progress path matches and no-op on
  loaded path matches; unloaded same-name module replacement uses
  `rollback_module_publication`; module hash insertion is bounded and shared;
  table growth skips inactive module slots; default-name imports check the path
  cache; and loaded path-backed resolution is path-first.
- **Validation:** `c3c build main`, advanced `advanced-collections-module`
  filter (`pass=2143 fail=0`), compiler slice (`pass=412 fail=0`), file-size
  gate, and `git diff --check`.

#### H17. Symbol ID Exhaustion Not Handled Gracefully

- **File:** `src/lisp/value_symbol_table.c3:260`
- **Status:** Invalidated 2026-04-29.
- **Current finding:** Current `SymbolTable.intern` already rejects
  `self.count >= (usz)INVALID_SYMBOL_ID` before growth/allocation and before
  the `(SymbolId)self.count` cast. The direct regression in
  `run_basic_symbol_table_hardening_tests` forces `table.count` to
  `INVALID_SYMBOL_ID`, verifies `intern` returns the sentinel without changing
  `count`, and verifies a preexisting symbol still resolves.
- **Validation:** C3 diagnostics for `value_symbol_table.c3` and basic slice
  (`pass=174 fail=0`).

#### H18. Double-Registration of Destructors

- **File:** `src/scope_region_chunk_helpers.c3:61-89`
- **Status:** Fixed 2026-04-29.
- **Resolution:** `scope_register_dtor` and `scope_register_dtor_escape` now
  scan their existing destructor list and return success for an exact duplicate
  `(ptr, func)` before allocating new destructor metadata. Same-pointer /
  different-function destructor registrations remain valid and still run.
- **Regression:** ScopeRegion TEMP and ESCAPE destructor tests now use named
  destructor functions, force descriptor-allocation OOM during duplicate
  registration to prove dedupe happens before allocation, assert only two
  entries exist after duplicate-plus-distinct registration, and verify teardown
  runs each distinct destructor once.

#### H19. Big-Integer Binary Handle Passes Null Handles to Backend FFI

- **File:** `src/lisp/value_big_integer.c3:183-195`
- **Status:** Fixed 2026-04-29.
- **Resolution:** `big_integer_binary_handle` now rejects null operands and
  malformed `BIG_INTEGER` payload handles before any backend call. Adjacent
  unary BigInteger negation and bitwise-not helpers also fail closed on null
  values or null payload handles.
- **Regression:** Advanced stdlib numeric direct coverage constructs malformed
  `Value { tag = BIG_INTEGER, big_integer_val = null }` values and verifies
  `INT/BIG_INTEGER`, `BIG_INTEGER/INT`, `BIG_INTEGER/BIG_INTEGER`, and
  integer-preferring binary routes return an error instead of crossing into the
  native backend.

#### H20. Value Serializer Silently Emits `nil` for Many Types

- **File:** `src/lisp/compiler_expr_serialize_values.c3:71-72`
- **Status:** Fixed 2026-04-29.
- **Resolution:** Source-reconstructible values now serialize explicitly:
  arrays, dictionaries, sets, `Void`, `TimePoint`, and big numeric values emit
  parser-compatible source instead of falling through to `nil`. Opaque runtime
  values such as closures, continuations, primitives, handles, modules,
  iterators, coroutines, instances, method tables, tensors, and error payloads
  now set a compiler diagnostic instead of pretending to be `nil`.
- **Regression:** Compiler serializer tests cover array, dictionary, set,
  BigInteger, TimePoint, and opaque-value fail-closed behavior.

#### H21. Compiler Name Helpers Write Without Buffer Bounds Checking

- **File:** `src/lisp/compiler_name_helpers.c3:3-65`
- **Status:** Fixed 2026-04-29.
- **Resolution:** All generated-name helpers now check caller buffer capacity
  before every byte write and return an empty sentinel on overflow. Compiler
  call sites route through `Compiler.generated_name_*` wrappers, which set a
  compile diagnostic instead of allowing truncation or raw stack overwrite.
- **Regression:** Compiler fail-closed tests cover exact-capacity success and
  overflow failure for id, index, suffix, and pair formatting, plus the
  compiler wrapper diagnostic path.

---

### 🟡 Medium (New)

#### M32. `scope_dtor_value` Default Case Swallows Unhandled Tags

- **File:** `src/lisp/value_constructors_lifecycle.c3:102`
- **Status:** Fixed 2026-04-29.
- **Issue:** `default: {}` caught any unhandled `ValueTag` but did nothing. The live defect was implicit destructor policy for scalar/no-op tags, not the stale C-style fallthrough claim in C11/C12.
- **Fix:** `scope_dtor_value` now enumerates every current no-op tag explicitly and removes the silent default. Heap-backed tags retain their existing release behavior.
- **Validation:** C3 diagnostics passed for `value_constructors_lifecycle.c3`; `c3c build main` linked; scope suite passed with `pass=67 fail=0`; bounded container `memory-lifetime-smoke` passed with `pass=310 fail=0`; Valgrind scope run reported `ERROR SUMMARY: 0 errors`.

#### M33. Big-Integer Shift Value Ignores `BIG_INTEGER_MAX_SHIFT_BITS` Limit

- **File:** `src/lisp/value_big_integer.c3:21,290-302`
- **Status:** Fixed 2026-04-29.
- **Resolution:** `big_integer_shift_value` now rejects shifts above
  `BIG_INTEGER_MAX_SHIFT_BITS` before backend calls and also rejects null or
  malformed BigInteger operands. Public `lshift`/`rshift` keep their existing
  signed shift-count handling and range errors.
- **Regression:** Advanced numeric bitwise/HOF tests include direct helper
  coverage for over-cap integer and BigInteger shifts plus malformed
  BigInteger payloads.

#### M34. `big_float_handle_from_value` Dereferences Null Big-Integer Handle

- **File:** `src/lisp/value_big_float.c3:201-208`
- **Status:** Fixed 2026-04-29.
- **Resolution:** `big_float_handle_from_value` now rejects malformed
  `BIG_FLOAT` and `BIG_INTEGER` values with null payload handles before clone
  or render backend calls. Current C++ backends tolerate these null handles, so
  the live defect was backend-tolerance dependency rather than an observed
  native crash.
- **Regression:** Advanced numeric tests directly construct malformed
  BigInteger/BigFloat values and verify BigFloat conversion, comparison, and
  BigComplex part construction fail closed.

#### M35. `big_complex` Real/Imag Functions Dereference Null Complex Handle

- **File:** `src/lisp/value_big_complex.c3:254-268`
- **Status:** Fixed 2026-04-29.
- **Resolution:** `big_complex_handle_from_value`, `big_complex_real_part_value`,
  `big_complex_imag_part_value`, and adjacent `big_complex_conjugate_value` now
  reject malformed `BIG_COMPLEX` values with null payload handles before backend
  calls. Scalar `real-part`, `imag-part`, and `conjugate` behavior is unchanged.
- **Regression:** Advanced numeric tests directly construct malformed
  BigComplex values and verify handle conversion, real/imag extraction, and
  conjugation fail closed.

#### M36. Hashmap `get` and `remove` Missing `entries` Null Check

- **File:** `src/lisp/prim_collection_hashmap.c3:128-136,349-381`
- **Issue:** Neither function validates `map.entries != null` before indexing.
- **Impact:** Segfault on malformed or incompletely initialized HashMap.
- **Status:** Fixed 2026-04-29.
- **Fix:** Added `hashmap_storage_valid` and routed direct `hashmap_get` /
  `hashmap_remove` through it so null or malformed backing storage fails closed
  before probing. Public dict/set wrappers, canonical keys/values iteration,
  length/mutation helpers, and print/buffer-print paths now reject non-empty
  malformed backing storage instead of indexing null entries.
- **Regression:** Advanced collections module coverage constructs malformed
  internal HashMap/set values and verifies direct helper results, public
  primitive invalid-state errors, and `#<dict:invalid>` / `#<set:invalid>`
  print sentinels. Focused validation passed; broad memory-lifetime smoke was
  attempted in the bounded container but timed out/killed with exit 137 and is
  not passing evidence.

#### M37. Hashmap `grow_checked` Rehash Can Silently Drop Entries

- **File:** `src/lisp/prim_collection_hashmap.c3:210-224`
- **Issue:** The rehash loop searches for an empty slot; if it exhausts the loop without finding one, execution falls through with no error and the entry is lost.
- **Impact:** Silent data loss if hash clustering ever exhausts the probe sequence.
- **Status:** Fixed 2026-04-29.
- **Fix:** `hashmap_grow_checked` now rehashes into local new storage and
  counts placed entries before mutating the live map. If any old entry cannot
  be placed, the new storage is freed and the original entries pointer,
  capacity, mask, count, and lookups remain intact.
- **Regression:** Advanced collections coverage forces the otherwise
  unreachable rehash-insert failure path and verifies grow returns `false`
  while preserving the original table.
- **Audit note:** Under current valid-map invariants, natural probe exhaustion
  should be unreachable because growth reinserts at most `old_cap` live entries
  into a zeroed `2 * old_cap` table. This closure hardens the impossible-state
  atomicity contract rather than proving a reachable normal data-loss path.

#### M38. Scheduler `add_fiber` Accepts Stale/Completed `parent_id`

- **Files:** `src/lisp/scheduler_io_fiber_core.c3:20-26`, `src/lisp/scheduler_primitives.c3:134-136`
- **Issue:** `scheduler_add_fiber` checks `parent_id < g_scheduler.fiber_count` but does not verify the parent fiber is still alive. `prim_spawn` increments `live_children` on a completed parent.
- **Impact:** Scheduler state corruption; parent/child lifecycle accounting incorrect.
- **Status:** Fixed 2026-04-29.
- **Fix:** Added a shared scheduler parent-admission guard that accepts
  `NO_FIBER` only, rejects out-of-range or completed parents, and preserves
  owner-mismatch rejection before fiber publication. `prim_spawn` now surfaces
  invalid-parent admission failures and releases the just-created coroutine
  context on every failed scheduler admission path. Scheduler abort/idle reset
  paths now release live fiber coroutine contexts before clearing table slots,
  so abandoned ready/blocked fibers cannot exhaust the stack-context pool.
- **Regression:** Scheduler coverage directly exercises stale and completed
  parent rejection without publishing a fiber or mutating `live_children`, checks
  nested blocking scheduler operations are rejected outside the fiber root
  context, and preserves cleanup after non-child await rejection. Validation
  passed: C3 diagnostics, `c3c build main`, scheduler slice (`pass=147 fail=0`),
  `scripts/check_scheduler_state_guards.sh`, file-size gate, and
  `git diff --check`.
- **Audit note:** The original public-spawn wording was overstated: normal
  `spawn` only inherits a parent while the current fiber is actively running,
  so completed-parent admission is primarily an internal scheduler helper
  contract and stale-state hardening issue.

#### M39. Symbol Table Hash Insertion Does Not Verify Success

- **File:** `src/lisp/value_symbol_table.c3:275-281`
- **Issue:** The insertion loop probes for an empty slot and breaks when found, but does not check a slot was actually found. If the hash table is full, the symbol is stored in `entries` but never inserted into `hash_index`.
- **Impact:** Unreachable symbols, duplicate IDs, silent data loss.
- **Status:** Fixed 2026-04-29.
- **Fix:** Added a shared checked hash-index insertion helper. New-symbol
  intern now finds and claims a hash slot before publishing the `entries[id]`
  payload or incrementing `count`; if placement fails, the allocated name is
  freed and `INVALID_SYMBOL_ID` is returned with no table side effects. Grow
  rehashing uses the same helper and aborts before publishing new storage if
  any old entry cannot be placed.
- **Regression:** Basic core hardening coverage forces a synthetic full
  hash-index table, verifies repeated `intern` attempts return
  `INVALID_SYMBOL_ID`, and checks `count` plus the existing symbol remain
  unchanged. Validation passed: C3 diagnostics, `c3c build main`, basic slice
  (`pass=175 fail=0`), file-size gate, and `git diff --check`.

#### M40. Match Exhaustiveness Check Incorrect for Nested Patterns

- **File:** `src/lisp/eval_dispatch_match_errors.c3:86-98`
- **Issue:** `format_match_error` checks union coverage by inspecting only the **top-level** `pat.tag` of each clause. If a union variant is matched inside a nested pattern, the check falsely reports it as missing.
- **Impact:** Valid match expressions on nested union values produce spurious runtime errors.
- **Status:** Fixed 2026-04-29.
- **Fix:** Added recursive pattern coverage detection for match diagnostics.
  The formatter now recognizes variant constructors under constructor fields,
  guards, as-patterns, cons patterns, and sequence patterns, while preserving
  top-level wildcard coverage and the existing nullary-constructor `PAT_VAR`
  path.
- **Regression:** Diagnostics coverage matches a union value against a guarded
  nested constructor clause that structurally covers the variant but fails at
  runtime; the resulting error now falls back to `no pattern matched for value
  of type ...` instead of falsely reporting the nested variant as missing.
  Validation passed: C3 diagnostics, `c3c build main`, diagnostics slice
  (`pass=11 fail=0`), file-size gate, and `git diff --check`.

#### M41. Circular Import Not Detected for Declared Module Self-Reference

- **File:** `src/lisp/jit_module_import_setup.c3:14-31`
- **Issue:** `jit_eval_declared_module_file` returns early only when `already != null && already.loaded`. If a module file is re-entered during its own loading, the `already.loaded == false` case falls through to re-evaluate.
- **Impact:** Stack overflow or infinite recursion on circular module dependencies.
- **Fix:** Return `eval_error("circular import detected")` when `already != null && !already.loaded`.
- **Status:** Fixed 2026-04-29. `jit_eval_declared_module_file` now rejects
  already-published but not-yet-loaded declared modules as circular imports,
  matching the path-import loading-state contract. Regression coverage seeds an
  in-progress declared file module, re-enters the declared-module evaluator, and
  verifies the load fails closed without replacing the in-progress module.
  Validation passed: C3 diagnostics, `c3c build main`, advanced collections
  module filter (`pass=2146 fail=0`), file-size gate, and `git diff --check`.
  The module test file was split top-down into a companion dictionary-helper
  part to keep tracked code files under the 1000 LOC gate.

#### M42. Quasiquote Nesting Depth Unlimited in AOT Compiler

- **File:** `src/lisp/compiler_quasiquote_flat.c3:75-119`
- **Issue:** The compiler path recurses with `depth + 1` on `E_QUASIQUOTE` and `depth - 1` on `E_UNQUOTE` without any bound check. The JIT evaluator limits depth to 64, but AOT has no guard.
- **Impact:** Stack overflow in compiler when compiling deeply nested quasiquote templates.
- **Fix:** Add `if (depth > 64) return self.emit_error_temp("quasiquote nesting too deep");`.
- **Status:** Fixed 2026-04-29. AOT quasiquote lowering now uses the same
  64-level nesting cap as JIT and reports `quasiquote nesting too deep (max
  64)` via `Compiler.set_compile_error`. Recursive quasiquote result consumers
  now propagate `usz.max`/`has_error` before emitting dependent temps, including
  app/list and call-splice lowering paths. Regression coverage verifies both a
  directly over-nested quasiquote and a nested quasiquote inside quasiquoted
  call/list lowering fail closed without leaking `_r184467...` into generated
  code. Validation passed: C3 diagnostics, `c3c build main`, compiler slice
  (`pass=425 fail=0`), file-size gate, and `git diff --check`. The original
  `emit_error_temp` fix sketch was stale; no such API exists.

#### M43. Multi-Shot Continuation Resume Leaks Handler State Refcount

- **File:** `src/lisp/jit_runtime_effects.c3:90-161`
- **Issue:** `jit_apply_handle_continuation_value` clones the continuation context and resumes it, but never decrements `hstate.continuation_refcount`. The single-shot path does decrement it.
- **Impact:** Memory leak of handler state for every multi-shot continuation resume.
- **Fix:** After cloned context completes, decrement `hstate.continuation_refcount`.
- **Status:** Invalidated 2026-04-29. The proposed decrement is wrong for the
  current ownership model: multi-shot handler continuation application clones
  and consumes only the resumed stack clone, not the original continuation
  wrapper that owns any retained handler-state reference. A guard regression now
  verifies a retained active handler continuation remains retained and keeps
  `continuation_refcount == 1` after multi-shot clone application. Validation
  passed: C3 diagnostics, `c3c build main`, focused JIT policy filter
  (`pass=1 fail=0`), full `jit-policy` slice (`pass=79 fail=0`), file-size
  gate, and `git diff --check`.

#### M44. Unbounded Perform/Resume Dispatch Loops

- **Status:** Closed 2026-04-29.
- **Files:** `src/lisp/jit_runtime_effects_handle.c3`, `src/lisp/jit_handle_signal_handle.c3`, `src/lisp/tests_runtime_feature_jit_groups_failures_helpers.c3`, `src/lisp/tests_runtime_feature_jit_groups_more.c3`
- **Issue:** Both resumptive handle redispatch loops had a backedge that could allocate or dispatch another continuation without honoring the existing interrupt/safepoint policy.
- **Impact:** A tight perform/resume loop could delay interrupt handling and continue allocating continuations until external resource exhaustion.
- **Fix:** Both redispatch loops now call `runtime_eval_backedge_poll(interp)` before clearing `state.signaled` or allocating/binding the next continuation. If an interrupt is pending, the paths return the standard `evaluation interrupted` error without allocating a fresh continuation.
- **Invalidated detail:** The original hard `10,000` cycle-cap recommendation was too blunt for current semantics. Large finite effect loops are valid language behavior; the closed contract is backedge polling, not a fixed perform/resume depth cap.
- **Regression coverage:** Added focused JIT policy cases for `jit_handle_run_signals` and `jit_handle_dispatch_signals` that mark an interrupt before the redispatch backedge, assert `evaluation interrupted`, and assert `continuation_head` is unchanged.
- **Validation:** C3 diagnostics passed for touched files; `c3c build main`; focused `jit-policy` filter `handle-run-signals-backedge-interrupt,handle-dispatch-signals-backedge-interrupt` passed (`pass=2 fail=0`); full `jit-policy` slice passed (`pass=81 fail=0`); file-size gate passed; `git diff --check` passed.

#### M45. Memory Telemetry Counters Not Atomic — Race on Multi-Threaded Allocations — CLOSED 2026-04-29

- **Files:** `src/scope_region_temp_pool_stats.c3`, `src/scope_region_allocators.c3`, `src/scope_region_lifecycle.c3`, `src/scope_region_destroy.c3`, `src/scope_region_reset_adopt.c3`, `src/scope_region_temp_pool.c3`, `src/scope_region_temp_ctx_pool.c3`, `src/scope_region_tests.c3`, `src/lisp/prim_runtime_memory_stats.c3`, `src/lisp/tests_memory_lifetime_boundary_decision_bench_helpers.c3`
- **Issue:** Process-wide scope/fiber/transfer telemetry counters were incremented from independent scope owner threads without atomic operations. Same-scope owner guards do not serialize shared aggregate counters.
- **Impact:** Corrupted telemetry, lost counter updates, and racy snapshots from `runtime-memory-stats` and benchmark helpers.
- **Fix:** Added centralized relaxed-atomic telemetry helpers and field-by-field snapshot helpers for scope memory, scope transfer, and fiber temp pool stats. Allocation/lifecycle/reset/fiber-temp writers now update shared counters through the helpers without extending `scope_global_lock()` into hot telemetry paths. Runtime stats and benchmark snapshots now read through the snapshot helpers.
- **Regression coverage:** Added a threaded scope telemetry test that creates independent scopes across workers, performs deterministic TEMP/ESCAPE allocations, releases every scope, and asserts aggregate create/release/destroy/allocation deltas are at least the expected worker-owned totals.
- **Validation:** C3 diagnostics passed for touched scope/runtime/bench files; `c3c build main`; scope suite passed (`scope_region pass=68 fail=0`); counters-enabled build and scope suite passed; basic Lisp slice passed (`pass=175 fail=0`); telemetry benchmark envelope check passed with the existing optimizer baseline warning; file-size gate passed; `git diff --check` passed.
- **Validation blocker:** `c3c build main --sanitize=thread -D OMNI_BOUNDARY_INSTR_COUNTERS` linked, but the ThreadSanitizer runtime aborted before tests with `FATAL: ThreadSanitizer: unexpected memory mapping ...`. Treat local TSan as unavailable evidence, not as a failing scope telemetry test.
- **Residual:** Boundary decision/value-shape telemetry in `src/lisp/eval_boundary_telemetry.c3` still uses plain counters and raw struct snapshots. That adjacent race is tracked separately as M45A so the closed M45 scope/fiber/transfer counter contract remains precise.

#### M45A. Boundary Telemetry Counters Not Atomic — CLOSED 2026-04-29

- **Files:** `src/lisp/eval_boundary_telemetry.c3`, `src/lisp/eval_boundary_scope_chain.c3`, `src/lisp/eval_boundary_graph_audit.c3`, `src/lisp/tests_memory_lifetime_boundary_state_groups.c3`, `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3`, `src/lisp/tests_slice_policy.c3`, `src/lisp/tests_tests.c3`
- **Issue:** Boundary route/value-shape telemetry uses plain process-wide `usz` counters and raw snapshot copies. Concurrent boundary-heavy workers can lose updates or read torn/incoherent telemetry.
- **Impact:** Boundary optimizer/profile evidence can be misleading under threaded workloads even though M45 closed scope/fiber/transfer telemetry atomicity.
- **Fix:** Added relaxed atomic load/store, saturating CAS-add/inc, and atomic max helpers for boundary telemetry. Route/value-shape writers now use those helpers, reset/restore/snapshot paths are field-wise, and the direct scope-chain/audit-budget reads now use atomic loads/returned increment values.
- **Regression coverage:** Added focused saturation and threaded boundary telemetry tests plus a `boundary-telemetry` Lisp slice so this contract can be validated without running the full memory-lifetime smoke suite.
- **Validation:** C3 diagnostics passed for touched files; counters-enabled `c3c build main -D OMNI_BOUNDARY_INSTR_COUNTERS`; `OMNI_LISP_TEST_SLICE=boundary-telemetry` passed (`pass=2 fail=0`); normal `c3c build main`; basic Lisp slice passed (`pass=175 fail=0`); file-size gate passed; `git diff --check` passed.
- **Validation limitation:** The full Docker-bound `memory-lifetime-smoke` command built and entered the slice, but the validation wrapper killed it at the configured timeout before a pass/fail result. Use the new focused `boundary-telemetry` slice as the accepted targeted signal for M45A.

#### M46. Memory Telemetry Counter Overflow — CLOSED 2026-04-30

- **Files:** `src/scope_region_temp_pool_stats.c3`, `src/scope_region_tests.c3`, `src/lisp/eval_boundary_telemetry.c3`
- **Issue:** Telemetry structs used `usz` counters that could wrap on long-running workloads. M45 made scope/fiber/transfer counters atomic but left their adds non-saturating; M45A closed boundary telemetry atomicity/overflow separately.
- **Impact:** Telemetry could silently become non-monotonic and mislead runtime memory stats, benchmark evidence, and optimizer/profile decisions.
- **Fix:** Boundary route/value-shape telemetry uses saturating CAS-add/inc helpers from M45A. Scope memory, scope transfer, and fiber-temp telemetry now use a shared CAS-loop saturating add helper, a guarded CAS decrement helper, and local saturation helpers for per-scope staging/aggregation paths before values are published globally. `scope_chunk_bytes`, destructor counting, and slow-sequence follow-up staging now saturate instead of wrapping locally.
- **Regression coverage:** Added `run_scope_region_telemetry_saturation_test()` to seed representative scope memory, transfer, and fiber-temp counters near `usz.max`, exercise writer/helper paths, verify saturation, verify underflow-safe decrement behavior, and cover local chunk/staging saturation.
- **Validation:** C3 diagnostics passed for `src/scope_region_temp_pool_stats.c3` and `src/scope_region_tests.c3`; `c3c build main`; default scope suite passed (`scope_region pass=69 fail=0`); counters-enabled `c3c build main -D OMNI_BOUNDARY_INSTR_COUNTERS`; counters-enabled fiber-temp scope suite passed (`scope_region pass=69 fail=0`, fiber-temp enabled); restored normal `c3c build main`; basic Lisp slice passed (`pass=175 fail=0`); file-size gate passed; `git diff --check` passed.
- **Follow-up:** Some older tests still raw-read telemetry structs instead of helper snapshots. That is test-hygiene risk, not an open M46 production overflow defect, and is tracked as `AUDIT-251-TELEMETRY-TEST-SNAPSHOT-READS`.

---

### 🟢 Low (New)

#### L27. `Interp.alloc_env` Leaves `is_inline` Uninitialized — CLOSED 2026-04-30

- **File:** `src/lisp/value_interp_alloc_helpers.c3`, `src/lisp/tests_core_groups.c3`
- **Issue:** `alloc_env()` initializes bindings, capacity, hash_table, etc., but never sets `is_inline`. Callers currently overwrite it, but any future direct caller that forgets will cause `scope_dtor_env` to make the wrong free decision.
- **Fix:** Initialized `e.is_inline = false;` inside both `alloc_env()` and `alloc_env_escape()`.
- **Regression coverage:** Added a deterministic basic native test that dirties reused TEMP/ESCAPE env allocation memory before calling the direct allocators and verifies both publish non-inline defaults.
- **Validation:** C3 diagnostics passed for the allocator/test files; `c3c build main`; basic Lisp slice passed (`pass=176 fail=0`); restored normal build after sanitizer attempts; file-size gate passed; `git diff --check` passed.
- **Validation limitation:** `c3c build main --sanitize=address` reported sanitizer support unavailable in this toolchain configuration, and the alternate option order was rejected by `c3c`. Targeted Valgrind basic-slice execution reached `pass=176 fail=0`, but exited `99` due existing custom stack/continuation Valgrind reports outside this env-initialization change.

#### L28. `Env.hash_lookup` / `hash_insert` Rely Only on Contracts for Null `hash_table` — CLOSED 2026-04-30

- **File:** `src/lisp/value_environment_storage.c3`, `src/lisp/tests_core_groups.c3`
- **Issue:** Both functions have `@require self.hash_table != null` but no runtime guard.
- **Fix:** Removed the contract-only precondition and made both helpers fail closed when `self`, `hash_table`, or `hash_capacity` is invalid.
- **Regression coverage:** Added a basic native regression that constructs a malformed env with nonzero `hash_capacity` and null `hash_table`, then verifies lookup misses and insert leaves the env unchanged instead of crashing.
- **Validation:** C3 diagnostics passed for the helper/test files; `c3c build main`; basic Lisp slice passed (`pass=177 fail=0`); file-size gate passed; `git diff --check` passed.

#### L29. `hashmap_sorted_slots` Dereferences Map Without Null Guard — CLOSED 2026-04-30

- **File:** `src/lisp/prim_collection_hashmap_key_helpers.c3`, `src/lisp/compiler_expr_serialize_values.c3`, `src/lisp/tests_compiler_core_groups_serializer_metadata.c3`
- **Issue:** The original null-deref note was partially stale, but the helper still allocated and returned a nonempty scratch slice for malformed non-null maps with invalid backing storage. Compiler serialization could then consume invalid slot indexes and dereference missing entries.
- **Fix:** Made `hashmap_sorted_slots()` return an empty slice before allocation whenever `hashmap_storage_valid(map)` fails or the map is empty, and made compiler dictionary/set serialization report malformed backing storage explicitly before requesting sorted slots.
- **Regression coverage:** Added compiler serializer coverage that constructs a malformed dictionary backing with nonzero count/capacity and null entries, verifies the sorted-slot helper returns empty, and verifies serialization fails closed with a backing-storage diagnostic.
- **Validation:** C3 diagnostics passed for the helper/serializer/test files; `c3c build main`; compiler slice passed (`pass=426 fail=0`); advanced collections module filter passed (`pass=2146 fail=0`); file-size gate passed; `git diff --check` passed.
- **Validation note:** The first adjacent advanced validation command used a nonexistent `OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-module-generic-ops` and matched no tests. The corrected filter is `advanced-collections-module`.

#### L30. `copy_env_value_fast` Missing Default Switch Case — CLOSED 2026-04-30

- **File:** `src/lisp/eval_env_copy_values.c3`, `src/lisp/tests_core_groups.c3`
- **Issue:** No `default` case. If a new `ValueTag` is added and not handled, the function returns uninitialized/garbage.
- **Fix:** Added a default branch returning `null`, preserving the existing copy-failure rollback contract.
- **Regression coverage:** Added a basic native regression that corrupts a zeroed `Value` tag byte to an unknown tag and verifies `copy_env_value_fast()` rejects it with `null`.
- **Validation:** C3 diagnostics passed for the env-copy/test files; `c3c build main`; basic Lisp slice passed (`pass=178 fail=0`); file-size gate passed; `git diff --check` passed.

#### L31. Dict Pattern Parser Accepts Duplicate Keys — CLOSED 2026-04-30

- **File:** `src/lisp/parser_patterns_values.c3`, `src/lisp/tests_core_groups.c3`
- **Issue:** `parse_dict_pattern` pushes every symbol into `keys` without checking for duplicates.
- **Impact:** Confusing semantics; last binding wins, hiding logic errors.
- **Fix:** Reject duplicate symbols in dict patterns at parse time.
- **Regression coverage:** Added a basic match parser regression for `({name name} ...)` that expects `duplicate key in dict pattern`.
- **Validation:** C3 diagnostics passed for the parser/test files; `c3c build main`; basic Lisp slice passed (`pass=179 fail=0`); file-size gate passed; `git diff --check` passed.

#### L32. Pattern Matching Runtime Silently Fails for New Pattern Tags — CLOSED 2026-04-30

- **File:** `src/lisp/eval_pattern_matching.c3`, `src/lisp/tests_core_groups.c3`
- **Issue:** `default` branch returns `match_fail()`. If a new `PatternTag` is added, the compiler will not flag the missing case.
- **Fix:** Unknown pattern tags now return a `runtime/invalid-state` match error instead of an ordinary non-match.
- **Regression coverage:** Added a basic native regression that corrupts a zeroed `Pattern` tag byte to an unknown tag and verifies `match_pattern()` reports `match: unknown pattern tag`.
- **Validation:** C3 diagnostics passed for the pattern matcher/test files; `c3c build main`; basic Lisp slice passed (`pass=180 fail=0`); file-size gate passed; `git diff --check` passed.

#### L33. Pattern Serializer Silently Emits Wildcard for Unknown Tags — CLOSED 2026-04-30

- **File:** `src/lisp/compiler_expr_serialize_patterns.c3`, `src/lisp/tests_compiler_core_groups_serializer_metadata.c3`
- **Issue:** `default` emits `_`. New pattern tags silently serialize as wildcard.
- **Fix:** Unknown pattern tags now set a compiler error instead of emitting wildcard source.
- **Regression coverage:** Added compiler serializer coverage that corrupts a zeroed `Pattern` tag byte, calls `serialize_pattern_to_buf()`, and verifies it reports `cannot serialize unknown pattern tag` without emitting `_`.
- **Validation:** C3 diagnostics passed for the serializer/test files; `c3c build main`; compiler slice passed (`pass=427 fail=0`); file-size gate passed; `git diff --check` passed.

#### L34. Module Path Comparison Not Normalized

- **File:** `src/lisp/jit_module_import_setup.c3:58-66`
- **Status:** Fixed 2026-04-30.
- **Issue:** `module_path_matches` compares paths byte-for-byte. Different strings resolving to the same file are treated as distinct.
- **Impact:** Same module loaded under multiple cache entries.
- **Fix:** File-module paths are normalized lexically before storage and comparison:
  repeated separators and `.` segments collapse to the same cache key while
  preserving existing import rejection for absolute user imports and `..`
  traversal. Regression coverage verifies equivalent normalized paths reuse the
  same module entry.
- **Validation:** C3 diagnostics passed for module setup/import/test files;
  `c3c build main`; advanced collections module filter passed (`pass=2147
  fail=0`); file-size gate passed; `git diff --check` passed.

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
- **Status:** Invalidated 2026-04-29; false positive.
- **Issue:** The original finding assumed C-style implicit switch fallthrough.
- **Fix:** No code fix. C3 switches do not implicitly fall through, so `scheduler_dispatch_wakeup_event` dispatches exactly one handler for the matched `WakeupEventType` arm.
- **Validation:** Subagent audit verified the source against C3 switch semantics and ran a successful build. `scripts/check_scheduler_state_guards.sh` was also attempted but failed later on unrelated scheduler child/await tests: `parent waits for unawaited child` and `nested-context await in fiber rejected`.

#### C16. `fpu_save` Null Dereference on x86_64

- **File:** `csrc/stack_helpers.c:196-203`
- **Status:** Fixed 2026-04-29.
- **Issue:** On `__x86_64__`, `fpu_save(uint32_t* mxcsr, uint32_t* x87cw)`
  dereferenced `*mxcsr = m` and `*x87cw = (uint32_t)c` without checking for
  null, while the non-x86 branch already treated both outputs as optional. The
  normal runtime stack paths pass real storage, so this was an exported helper
  contract bug rather than a currently reachable stack-init crash.
- **Fix:** The x86_64 branch now guards each output write independently.
  Native regression coverage calls `fpu_save(NULL, NULL)`,
  `fpu_save(&mxcsr, NULL)`, and `fpu_save(NULL, &x87cw)`.

#### C17. BLAS `omni_tensor_backend_blas_dger` Buffer Overflow

- **File:** `csrc/tensor_blas_helpers.c:242-243`
- **Status:** Fixed 2026-04-29.
- **Issue:** The native helper accepted arbitrary `lda`, zeroed `m * lda`
  elements, then called BLAS with the same leading dimension. Current public
  Omni tensor callers pass dense row-major outputs with `lda == n`, but the
  exported native helper ABI had no output extent parameter, so direct/future
  callers could pass `lda > n` with only `m * n` output capacity.
- **Fix:** The helper now rejects `lda != n` before resolving BLAS or writing
  output, and zeros exactly `m * n` elements for the supported dense contract.
  Native regression coverage verifies padded `lda` returns `0` without touching
  the output buffer.

#### C18. JIT Multi-Arg Primitive Apply Dereferences Null `func.prim_val`

- **File:** `src/lisp/jit_apply_multi_prims.c3:125-128`
- **Status:** Fixed 2026-04-29.
- **Issue:** `jit_apply_multi_args_primitive` accessed `func.prim_val.tag`,
  `func.prim_val.user_data`, `func.prim_val.arity`, and
  `func.prim_val.func` without checking the primitive payload. Recent
  method-table hardening did not cover the `PRIMITIVE` route.
- **Fix:** The multi-arg primitive helper now mirrors the single-arg primitive
  fail-closed guard for null/non-primitive/null-payload/null-function state.
  JIT policy regression coverage verifies direct multi-arg, routed multi-arg,
  and tail multi-arg primitive apply all return an error for malformed
  primitive values.

---

### 🟠 High (New)

#### H22. HTTP Response Header Slice Underflow

- **File:** `src/lisp/http_url_response.c3:198`
- **Status:** Invalidated 2026-04-29 as stale.
- **Issue:** The cited `response[hdr_start..header_end - 5]` expression is not
  present in current source. Current response parsing computes
  `header_delim_start`, rejects missing delimiters, and slices headers only when
  `hdr_start < header_delim_start` using `response[hdr_start..header_delim_start - 1]`.
- **Impact:** No current short-response header-slice underflow was found.
- **Fix:** Already addressed before this checkpoint. Existing malformed/empty
  header tests cover the short-response shape; broad HTTP validation is blocked
  by unrelated fiber/stack-context failures.

#### H23. Deduce Path Traversal

- **File:** `src/lisp/deduce_runtime_open.c3:161`
- **Status:** Invalidated 2026-04-29 under the current raw file-backed Deduce
  path contract.
- **Issue:** The finding assumes Deduce paths are sandbox-relative. Current
  docs/tests use caller-chosen file-backed storage paths, including absolute
  temporary paths for durability/isolation coverage, so rejecting leading `/`
  or parent segments would break the documented raw storage API.
- **Impact:** No regression under the current contract. A sandboxed Deduce
  storage API would be a separate product/security design decision.
- **Fix:** Do not add ad-hoc path filtering to `deduce 'open` while it remains
  a raw file-backed storage API. If sandboxing is desired, add a separate
  surface with explicit base-directory semantics and migration tests.

#### H24. Unbounded JIT Spill-List Growth

- **File:** `src/lisp/jit_compiler_compile.c3:56-74`
- **Status:** Fixed 2026-04-29.
- **Issue:** The finding was live: when the fixed JIT state pool was full,
  every additional compilation could allocate another `JitStateSpillNode`
  without a hard cap.
- **Impact:** Sustained compilation pressure could grow the spill list without
  bound.
- **Fix:** `JIT_STATE_SPILL_MAX` now caps spill nodes. Once reached,
  `jit_track_compiled_state` marks GC needed, returns false before allocation,
  and the existing `jit_compile` cleanup destroys the emitted state.

#### H25. Retired-Code Tombstone Saturation Silently Ignored

- **File:** `src/lisp/jit_compiler_lifecycle.c3:31-40`
- **Status:** Fixed 2026-04-29.
- **Issue:** The insertion handling was live and the original impact was partly
  stale. Normal compiled-function liveness uses tracked-state
  owner/interpreter/serial/compile-nonce validation, but the retained
  pointer-only legacy liveness path still needed a clear saturated-table policy.
- **Impact:** Retired-code tracking could silently saturate and leave the
  legacy pointer-only guard with ambiguous safety state.
- **Fix:** Retired-code saturation is now fail-closed for pointer-only lookup:
  once saturated, `jit_retired_code_contains` treats any non-null code/serial as
  retired. `jit_compiled_code_is_live_for_interp` consults tombstones before
  tracked-state pointer matching; nonce-bearing compiled-function liveness
  remains authoritative for normal execution.

#### H26. `cstr_len` Hard-Capped at 65536

- **File:** `src/entry_cli_helpers.c3:15-19`
- **Status:** Invalidated 2026-04-29 as stale truncation claim.
- **Issue:** The 65536 cap is still present, but current `cstr_len_bounded`
  reports over-cap input as failure instead of silently truncating it. Callers
  surface explicit `cli/argument-too-long` or mode-specific errors.
- **Impact:** No silent truncation was found. The remaining 64 KiB policy is an
  intentional CLI input cap, not this regression.
- **Fix:** No truncation fix needed. If the cap is undesirable, replace it with
  per-surface limits and diagnostics rather than unbounded scanning.

---

### 🟡 Medium (New)

#### M47. Scheduler Queue Mutated Without Mutex

- **Status:** Closed on 2026-04-29. Current production teardown already calls
  this helper after joining the worker thread, so no live race was found, but
  the helper now enforces the queue-locking contract internally when the mutex
  is initialized.
- **File:** `src/lisp/eval_repl_server_worker_helpers.c3:131-141`
- **Issue:** `repl_server_worker_clear_queued_commands` reads and writes `command_queue_head`, `command_queue_count`, and the queue array without acquiring `worker.mu`.
- **Impact:** Data race / queue corruption if called concurrently with command enqueue/dequeue.
- **Fix:** `repl_server_worker_clear_queued_commands` now locks `worker.mu`
  around queued-command and pending-command cleanup whenever `worker.mu_ready`
  is true, with `defer`-based unlock symmetry.

#### M48. TLS Socket Callbacks Truncate `long` to `int`

- **Status:** Closed on 2026-04-29.
- **File:** `csrc/tls_helpers.c:37,56`
- **Issue:** `omni_tls_sock_read` returns `(int)rlen` and `omni_tls_sock_write` returns `(int)wlen`. On 64-bit platforms, a `long` read/write larger than `INT_MAX` is silently truncated.
- **Impact:** Incorrect return values for large TLS transfers.
- **Fix:** The callbacks now clamp each native `read`/`send`/`write` request
  to `INT_MAX` bytes and store the native return in `ssize_t`, so BearSSL's
  required `int` callback return cannot truncate a larger successful transfer.

#### M49. Signal Handler Accesses Non-Atomic `int` Variables

- **Status:** Closed on 2026-04-29.
- **File:** `csrc/stack_helpers.c:480-495`
- **Issue:** `sigsegv_handler` reads and writes `g_recovery_depth` and reads
  `g_guard_count` plus `g_guards[]` without signal-safe publication. The
  original audit detail naming `g_guard_hit` as a plain `int` is stale:
  current code already stores it as `volatile sig_atomic_t`.
- **Impact:** Torn reads/writes, compiler reordering, or missed updates on multi-core systems.
- **Fix:** `g_recovery_depth` and `g_guard_count` now use `volatile
  sig_atomic_t`, and the guard table now stores scalar address ranges published
  with acquire/release atomic builtins. Register writes the entry before
  publishing the count, unregister compacts before publishing the decremented
  count, shutdown withdraws the count before freeing the table, and the handler
  snapshots depth/count before scanning without mutex use.

#### M50. BLAS Library Resolution Data Race

- **Status:** Closed on 2026-04-29.
- **File:** `csrc/tensor_blas_helpers.c:60-85`
- **Issue:** `omni_tensor_blas_resolve` uses a static `int resolution_attempted` without synchronization. Concurrent threads can race on the flag and double-load the library.
- **Fix:** BLAS resolver publication now uses `pthread_once`, removes the
  racy fast-path reads, keeps failed resolution cached once, and publishes the
  library handle after required/optional CBLAS function pointers are assigned.
  BLAS call counters now use relaxed atomics.

#### M51. LAPACK Library Resolution Data Race

- **Status:** Closed on 2026-04-29.
- **File:** `csrc/tensor_lapack_helpers.c:280-305`
- **Issue:** Same pattern as M50 — `resolution_attempted` is a plain static `int`.
- **Fix:** LAPACK resolver publication now uses `pthread_once`, removes the
  racy fast-path reads, preserves the existing accepted-symbol policy, and
  publishes the library handle after LAPACKE function pointers are assigned.
  LAPACK call counters and test-disable flags now use relaxed atomics.

#### M52. Vulkan Library Resolution Data Race

- **Status:** Closed on 2026-04-29.
- **File:** `csrc/tensor_vulkan_helpers_core.c:60-85`
- **Issue:** Same pattern as M50/M51 — library loading flag is unsynchronized.
- **Fix:** Vulkan resolver publication now uses `pthread_once`, removes the
  racy attempted flag, and publishes the dynamic library handle only after all
  required Vulkan function pointers are assigned. The adjacent Vulkan
  availability/feature probe now also uses `pthread_once`, and its cached
  result flags are scoped to the owning translation unit instead of exported
  through the runtime declarations header.

#### M53. `omni_uv_fs_read` / `write` Truncate `size_t` to `unsigned int`

- **Status:** Closed before this 2026-04-29 audit slice; stale `AUDIT_2.md`
  entry invalidated on 2026-04-29.
- **File:** `csrc/uv_helpers.c:71,80`
- **Issue:** Stale. Current `omni_uv_fs_read` and `omni_uv_fs_write` chunk
  requests through `omni_uv_fs_effective_max_buf_len()`, so the remaining
  `uv_buf_init` cast is applied only to a bounded chunk length, not arbitrary
  `size_t len`.
- **Impact:** Silent data loss or partial I/O on large files.
- **Fix:** Already implemented as chunking at the native libuv boundary. Keep
  this shape if it regresses; a simple `len > UINT_MAX` rejection would be less
  complete than the current partial-I/O-preserving loop.

#### M54. `c3c_limits.sh` Lacks `set -e`

- **File:** `scripts/c3c_limits.sh`
- **Issue:** Invalidated 2026-04-29. `scripts/c3c_limits.sh` is a sourced
  helper, not a standalone gate script. The current strict-mode guard
  intentionally excludes sourced helpers, and callers such as `scripts/run_e2e.sh`
  enable strict Bash mode before sourcing it.
- **Impact:** The original CI-pass concern does not apply to this file in the
  current workspace; adding `set -euo pipefail` inside the helper would mutate
  caller shell options instead of fixing a failing gate.
- **Fix:** No code change. Keep `scripts/check_status_consistency.sh` enforcing
  strict mode for executable first-party Bash scripts while allowing sourced
  helpers to inherit caller policy.

#### M55. `tests_harness_helpers.c3:setup()` Prints Failure But Does Not Abort

- **File:** `src/lisp/tests_harness_helpers.c3:19-32`
- **Issue:** Fixed 2026-04-29. When test setup failed, `setup()` printed an
  error message and returned normally, letting the calling test continue against
  a broken interpreter state.
- **Impact:** Tests could pass or fail for the wrong reason after setup errors.
- **Fix:** `setup()` now aborts after preserving the existing failure and source
  diagnostics.

#### M56. `.github/workflows/io-parity-guard.yml` References Missing Script

- **File:** `.github/workflows/io-parity-guard.yml`
- **Issue:** Invalidated 2026-04-29. The workflow references
  `scripts/check_io_parity_status_map.sh`, and that script now exists with
  strict Bash mode and executable permissions.
- **Impact:** The original missing-script workflow failure does not apply to
  the current workspace.
- **Fix:** No code change. Keep the workflow running
  `scripts/check_io_boundary_facade.sh`, `scripts/check_io_parity_status_map.sh`,
  and `scripts/check_async_fallback_policy.sh` together.

#### M57. `tests_e2e_generation.c3` Silently Skips Tests on Error

- **File:** `src/lisp/tests_e2e_generation.c3`
- **Issue:** Invalidated 2026-04-29. Current generated-e2e setup,
  expected-output collection, rendering, source emission, compilation, and
  output-file writes all return `false` on failure, and `--gen-e2e` maps that
  result to process exit `1`.
- **Impact:** The original silent-skip concern does not apply to the current
  workspace. `scripts/run_e2e.sh` also rejects Stage 2 generation failures and
  enforces the exact generated corpus size.
- **Fix:** No code change. Preserve the current fail-closed generation contract
  and the 431-row runner/status guards.

#### M58. `test_eq_double` Uses Exact Equality

- **File:** `src/lisp/tests_harness_helpers.c3`
- **Issue:** Fixed 2026-04-29. `test_eq_double` compared doubles with `==`,
  which can flake across architectures due to floating-point rounding
  differences.
- **Impact:** False test failures on different CPU architectures or compiler
  versions.
- **Fix:** `test_eq_double` now delegates to the existing tolerance-aware
  `test_double` helper, keeping call sites stable while sharing one numeric
  assertion path.

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
- **Status:** Fixed 2026-04-30.
- **Issue:** `${stage3_compile_sources[@]}` is unquoted, causing word-splitting if paths contain spaces.
- **Fix:** Stage 3 source expansion now uses `"${stage3_compile_sources[@]}"`.
- **Validation:** `bash -n` passed for the touched scripts; the existing
  `run_e2e.sh --self-test-validation-mount-bridge` passed; file-size gate and
  `git diff --check` passed.

#### L35. Docker Extra Args Exported Under Wrong Variable Name

- **Files:** `scripts/run_e2e.sh:52`, `scripts/run_deduce_perf_envelope.sh:50`
- **Status:** Fixed 2026-04-30.
- **Issue:** Both scripts build `validation_extra` mounts and export them as `OMNI_VALIDATION_EXTRA_ARGS`, but `scripts/c3c_limits.sh` only reads `OMNI_DOCKER_EXTRA_ARGS`. The mounts are silently ignored.
- **Fix:** `run_e2e.sh` already bridged validation mounts into
  `OMNI_DOCKER_EXTRA_ARGS`; `run_deduce_perf_envelope.sh` now does the same and
  includes a mount-bridge self-test that rejects duplicate Docker mounts.
- **Validation:** `bash -n` passed for the touched scripts;
  `run_e2e.sh --self-test-validation-mount-bridge` passed;
  `run_deduce_perf_envelope.sh --self-test-validation-mount-bridge` passed;
  file-size gate and `git diff --check` passed.

#### L36. C Compiler Hardcoded to `cc`, Ignores `$CC`

- **File:** `scripts/build_omni_chelpers.sh:278`
- **Status:** Closed 2026-04-30; already fixed in current source.
- **Issue:** The C compilation helper hardcodes `cc` while respecting `$CXX` for C++. Cross-compilation workflows break.
- **Fix:** Current source already invokes `"${CC:-cc}"`; no code change was
  needed for this slice.
- **Validation:** `bash -n scripts/build_omni_chelpers.sh` passed.

---

---

## Fifth Pass — New Findings (2026-04-24)

**Scope:** Deep re-audit of parser/compiler/JIT switch blocks, runtime path null checks, scope guard contracts, AOT tail-call loops, C helper edge cases, evaluator error-path leaks.
**Method:** 4 parallel agent passes + manual verification of all reported switch blocks and null-dereference sites.

---

### 🔴 Critical (New)

#### C19. `scope_guard_owner` Macro Returns Early on Null But Callers Continue Executing

- **Files:** `src/scope_region_allocators.c3`, `src/scope_region_chunk_helpers.c3`, `src/scope_region_reset_helpers.c3`, `src/scope_region_global_guards.c3`, `src/scope_region_tests_alloc_lifecycle.c3`, `src/scope_region_tests_reset_splice.c3`
- **Status:** Fixed 2026-04-29.
- **Issue:** `scope_require_owner` returned from itself on a null scope, so `scope_guard_owner` did not protect callers such as `ScopeRegion.alloc`, `scope_register_dtor`, and `scope_reset` from dereferencing a null `ScopeRegion*`.
- **Fix:** Added explicit null fail-closed gates to ScopeRegion allocator, destructor-registration, and reset entrypoints; made the shared owner guard assert non-null after callers have gated null; and added ScopeRegion regressions for null allocator, destructor-registration, and reset behavior.
- **Validation:** C3 diagnostics for touched ScopeRegion files, `c3c build main`, `OMNI_TEST_SUMMARY=1 ./build/main --test-suite scope` (`pass=67 fail=0`), and Valgrind ScopeRegion suite (`ERROR SUMMARY: 0 errors`). ASAN build was attempted but `c3c` reported address sanitizer unsupported for the current target.

#### C20. `jit_warm_pattern_cache` Missing `break` on Every Case

- **File:** `src/lisp/jit_apply_eval.c3:131-150`
- **Status:** Invalidated 2026-04-29; false positive.
- **Issue:** The original finding assumed C-style implicit switch fallthrough.
- **Fix:** No code fix. C3 switches do not implicitly fall through; adding `break` is not required for correctness.

#### C21. `jit_warm_expr_cache` Missing `break` on Every Case

- **File:** `src/lisp/jit_apply_eval.c3:202-268`
- **Status:** Invalidated 2026-04-29; false positive.
- **Issue:** The original finding assumed C-style implicit switch fallthrough.
- **Fix:** No code fix. C3 switches do not implicitly fall through; adding `break` is not required for correctness.

#### C22. `jit_compile_expr_group_core` Missing `break` on Every Case

- **File:** `src/lisp/jit_compile_expr_dispatch.c3:117-144`
- **Status:** Invalidated 2026-04-29; false positive.
- **Issue:** The original finding assumed C-style implicit switch fallthrough.
- **Fix:** No code fix. C3 switches do not implicitly fall through; adding `break` is not required for correctness.

#### C23. `jit_compile_expr_group_effects` Missing `break` on Every Case

- **File:** `src/lisp/jit_compile_expr_dispatch.c3:147-182`
- **Status:** Invalidated 2026-04-29; false positive.
- **Issue:** The original finding assumed C-style implicit switch fallthrough.
- **Fix:** No code fix. C3 switches do not implicitly fall through; adding `break` is not required for correctness.

#### C24. `jit_compile_expr_group_types_ffi` Missing `break` on Every Case

- **File:** `src/lisp/jit_compile_expr_dispatch.c3:184-211`
- **Status:** Invalidated 2026-04-29; false positive.
- **Issue:** The original finding assumed C-style implicit switch fallthrough.
- **Fix:** No code fix. C3 switches do not implicitly fall through; adding `break` is not required for correctness. The adjacent grouped `E_UNQUOTE` / `E_UNQUOTE_SPLICING` handling is intentional shared behavior.

#### C25. `Compiler.compile_expr` Missing `break` on Every Case

- **File:** `src/lisp/compiler_expression_compilation.c3:15-39`
- **Status:** Invalidated 2026-04-29; false positive.
- **Issue:** The original finding assumed C-style implicit switch fallthrough.
- **Fix:** No code fix. C3 switches do not implicitly fall through; adding `break` is not required for correctness.

#### C26. `numeric_binary_float32_result` Missing `break` on Every Case

- **File:** `src/lisp/prim_math_arithmetic.c3:54-66`
- **Status:** Invalidated 2026-04-29; false positive.
- **Issue:** The original finding assumed C-style implicit switch fallthrough.
- **Fix:** No code fix. C3 switches do not implicitly fall through; adding `break` is not required for correctness.

---

### 🟠 High (New)

#### H27. `eval_path_lookup_root` Dereferences `env` Without Null Check

- **File:** `src/lisp/eval_path.c3:5-8`
- **Status:** Invalidated 2026-04-29 as already fixed.
- **Issue:** Current `eval_path_lookup_root` checks `env != null` before lookup
  and falls back to `interp.global_env` only when available.
- **Impact:** No current null-env dereference was found.
- **Fix:** Already implemented before this checkpoint.

#### H28. `eval_path_instance_step` Dereferences `current.instance_val` Without Null Check

- **File:** `src/lisp/eval_path.c3:50-71`
- **Status:** Invalidated 2026-04-29 as already fixed.
- **Issue:** Current `eval_path_instance_step` checks `current != null`,
  `current.tag == INSTANCE`, and `current.instance_val != null`.
- **Impact:** No current null instance-payload dereference was found.
- **Fix:** Already implemented before this checkpoint.

#### H29. `eval_path_dict_step` Dereferences `current.hashmap_val` Without Null Check

- **File:** `src/lisp/eval_path.c3:73-88`
- **Status:** Invalidated 2026-04-29 as already fixed.
- **Issue:** Current `eval_path_dict_step` checks `current != null`,
  `current.tag == HASHMAP`, and `current.hashmap_val != null` before
  `hashmap_get`.
- **Impact:** No current null hashmap-payload dereference was found.
- **Fix:** Already implemented before this checkpoint.

#### H30. `Env.hash_lookup` Infinite Loop When Hash Table Is Completely Full

- **File:** `src/lisp/value_environment_storage.c3:197-208`
- **Status:** Fixed 2026-04-29.
- **Issue:** The unbounded probe loop was live for malformed or future full
  hash-table states even though normal load-factor policy avoids 100% fullness.
- **Impact:** A full table without the requested key could hang environment
  lookup or insertion.
- **Fix:** `Env.hash_lookup` and `Env.hash_insert` now bound probes by
  `hash_capacity` and fail closed when no slot is available. Regression coverage
  constructs a full two-slot table and verifies missing lookup/insertion return
  without mutating existing bindings.

#### H31. `jit_apply_value_primitive` Dereferences `args` Without Null Check

- **File:** `src/lisp/jit_apply_helpers.c3:328-351`
- **Status:** Invalidated 2026-04-29.
- **Current finding:** Current single-argument primitive apply uses `Value* arg`
  rather than dereferencing a raw args slice, and invalid primitive payloads are
  already guarded by `jit_apply_value_primitive`.
- **Validation:** H31-H38 read-only audit plus clean `jit-policy` pass after
  adjacent malformed-closure hardening.

#### H32. `foreign_runtime_kind_name`, `foreign_handle_kind_name`, `foreign_ffi_abi_type_name` Missing Default Cases

- **File:** `src/lisp/foreign_runtime_core.c3:30-46,53-69,109-137`
- **Status:** Invalidated 2026-04-29.
- **Current finding:** The switches are exhaustive for current enums and C3
  rejects missing enum cases/invalid checked enum casts. Adding `default` would
  weaken future enum-addition diagnostics and conflict with the repo style that
  prefers exhaustive enum switches.
- **Future hardening:** If a real unchecked corrupt-enum path is proven, add a
  trailing post-switch `return "<unknown>";`, not a `default`, so new enum
  cases remain compile-time-visible.

#### H33. `jit_apply_multi_args_method_table` Dereferences `func.method_table_val` Without Null Check

- **File:** `src/lisp/jit_apply_multi_prims.c3:194-212`
- **Status:** Invalidated 2026-04-29.
- **Current finding:** Current method-table apply paths use
  `jit_value_has_method_table_payload` and fail closed with an invalid method
  table state error before dereferencing the payload.

#### H34. `jit_apply_value_tail` Dereferences `func.closure_val` Without Null Check

- **File:** `src/lisp/jit_apply_runtime.c3:194-234`
- **Status:** Fixed 2026-04-29.
- **Fix:** `jit_apply_value_tail` now rejects malformed closure values through
  the shared `jit_value_has_closure_payload_and_body` helper before touching
  closure payload/body fields.
- **Regression:** `invalid-closure-state-fails-closed` covers null-payload and
  null-body closure values in single-arg tail apply.

#### H35. `jit_apply_value_closure` Dereferences `closure.body` Without Null Check

- **File:** `src/lisp/jit_apply_helpers.c3:237-312`
- **Status:** Fixed 2026-04-29.
- **Fix:** Single-arg and multi-arg closure apply helpers now use
  `jit_value_has_closure_payload_and_body` and return `jit: invalid closure
  state` instead of asserting or dereferencing null closure bodies.
- **Regression:** `invalid-closure-state-fails-closed` covers routed and direct
  zero-arg, variadic, multi-arg, and tail multi-arg closure helpers.

#### H36. `jit_copy_closure_env_if_needed` Null Dereference on `closure.closure_val`

- **File:** `src/lisp/jit_closure_runtime.c3:149-188`
- **Status:** Fixed 2026-04-29.
- **Fix:** `jit_copy_closure_env_if_needed` now validates the closure payload
  before reading `env_scope` and reports
  `BOUNDARY_ENV_COPY_FAULT_INVALID_CLOSURE_STATE` on malformed payloads.
  Existing null-body closure env-copy behavior is preserved because several
  memory-boundary tests intentionally use bodyless closures as value graph
  fixtures.
- **Regression:** `invalid-closure-state-fails-closed` verifies null-payload
  env-copy fails closed with the named fault and null-body env-copy remains
  accepted.

#### H37. `aot_closure_apply` / `aot_variadic_apply` Unbounded Tail-Call Loop

- **File:** `src/lisp/eval_signal.c3`, `src/lisp/runtime_backend_hooks.c3`,
  `src/lisp/aot_runtime_bridge_trampoline.c3`,
  `src/lisp/aot_runtime_bridge_closure.c3`
- **Status:** Fixed 2026-04-29.
- **Resolution:** This was not a recursion-depth bug and was not fixed with an
  arbitrary tail-call cap. A shared `runtime_eval_backedge_poll` helper now owns
  interpreter interrupt polling/clearing/error creation for eval back-edges.
  Runtime/JIT TCO uses the helper, and AOT `invoke`, `apply_multi`,
  `aot_closure_apply`, and `aot_variadic_apply` tail redispatch loops poll it
  before each pending tail call.
- **Regression:** AOT runtime parity coverage now verifies interrupt delivery
  on `invoke`, `apply_multi`, closure primitive-entry, and variadic
  primitive-entry tail back-edges.

#### H38. `jit_lookup_global` Stack Buffer Overflow on Long Symbol Names

- **File:** `src/lisp/jit_apply_helpers.c3:185-197`
- **Status:** Invalidated 2026-04-29.
- **Current finding:** The cited diagnostic uses bounded `io::bprintf`; long
  symbol names do not overflow the fixed stack buffer.

---

### 🟡 Medium (New)

#### M59. `eval_run_pipeline` JIT Error Path Leaks `exprs` List

- **File:** `src/lisp/eval_run_pipeline.c3:34-47`
- **Issue:** Invalidated 2026-04-29. Current `run_program()` sets
  `result = eval_error_expr(...)` and breaks on JIT compile failure, then
  executes the shared `exprs.free()` cleanup after the loop before returning.
  The single-expression `run()` path also frees `exprs` before returning on JIT
  execution failure.
- **Impact:** The original leak does not apply to the current workspace.
- **Fix:** No code change. Preserve the shared post-loop cleanup before error
  handling and return.

#### M60. `eval_defeffect` Leaves `field_count` Uninitialized in `else` Branch

- **File:** `src/lisp/eval_type_evaluators.c3:223-226`
- **Issue:** Invalidated 2026-04-29. Current `eval_defeffect()` explicitly sets
  `info.field_count = 0` in the `!de.has_arg_type` branch.
- **Impact:** The original uninitialized-field concern does not apply to the
  current workspace.
- **Fix:** No code change. Preserve explicit zero initialization for no-arg
  effect declarations.

#### M61. `jit_method_table_append_entry` Dereferences `impl.closure_val.type_sig` Without Null Check

- **File:** `src/lisp/jit_define_method_table.c3:101-105`
- **Issue:** Fixed 2026-04-29. The helper accepted `impl` without verifying
  that it was a closure with a non-null closure payload and type signature.
- **Impact:** Null dereference or invalid method-table entry if a non-closure or
  malformed closure value reached this helper.
- **Fix:** `jit_method_table_append_entry()` now requires `impl.tag == CLOSURE`,
  non-null `closure_val`, and non-null `type_sig` before appending.

#### M62. `jit_new_method_table` Dereferences `impl.closure_val.type_sig` Without Null Check

- **File:** `src/lisp/jit_define_method_table.c3:129-131`
- **Issue:** Fixed 2026-04-29. Same typed-implementation precondition gap as
  M61 on the method-table construction path.
- **Fix:** `jit_new_method_table()` now rejects null, non-closure, missing
  closure-payload, and missing-signature implementations before delegating to
  the signature-based constructor.

#### M63. `jit_eval_define_typed_closure` Dereferences `stored_val.closure_val.type_sig` Without Null Check

- **File:** `src/lisp/jit_define_method_table.c3:156-158`
- **Issue:** Fixed 2026-04-29. `stored_val.closure_val.type_sig` was accessed
  without verifying that the stored value was still a closure with a non-null
  closure payload and type signature.
- **Impact:** Null dereference instead of a fail-closed JIT error if a malformed
  typed definition reached this path.
- **Fix:** `jit_eval_define_typed_closure()` now fails closed with a JIT
  evaluation error unless the promoted implementation is a closure with a
  non-null type signature.

#### M64. `find_best_method` Can Leak `best_match_indices_rev` on Early Error Return

- **File:** `src/lisp/eval_dispatch_match.c3:83-113`
- **Issue:** Fixed 2026-04-29. `find_best_method()` used Lisp cons cells as
  scan-time scratch while searching for equally specific candidates. If
  candidate-list allocation failed during the scan, prior scratch cons cells
  could become unreachable until the current region was released.
- **Impact:** Transient allocation pressure on ambiguous-method error paths,
  especially in long-lived scopes.
- **Fix:** The scan now records candidate indices in native inline/heap scratch
  and materializes the Lisp candidate-index list only when an ambiguity payload
  is actually needed. Regression coverage now forces candidate-list allocation
  failure after partial materialization as well as on the first list cell.

#### M65. `jit_expr_family_for_tag` Non-Exhaustive Switch With No Default

- **File:** `src/lisp/jit_compile_expr_dispatch.c3:69-111`
- **Issue:** Fixed 2026-04-29. Current tags were covered, but the switch lacked
  a fail-closed fallback for future or invalid tags.
- **Impact:** JIT mis-compilation or crash if an unmapped expression tag reached
  this dispatcher.
- **Fix:** Added `default: return JitExprFamily.JIT_EXPR_FAMILY_INVALID;`, which
  routes through the existing invalid/fallback path.

#### M66. `jit_env_copy_fault_message` Non-Exhaustive Switch

- **File:** `src/lisp/jit_closure_runtime.c3:82-101`
- **Issue:** Fixed 2026-04-29. Current fault codes were covered, but the switch
  lacked a deterministic fallback for future or invalid fault values.
- **Impact:** Undefined message slice on new or invalid fault codes.
- **Fix:** Added `default: return "jit: closure env-copy failed (unknown fault)";`.

#### M67. `parse_program` Has No Max Expression Limit

- **File:** `src/lisp/parser_top_level_parse.c3:37-59`
- **Issue:** Fixed 2026-04-29. The top-level parse loops in `parse_program()`,
  `Compiler.parse_program_exprs()`, and the runtime source pipeline kept
  allocating `Expr*` nodes without a flat-expression cap.
- **Impact:** OOM DoS.
- **Fix:** Added shared `PARSER_MAX_PROGRAM_EXPRS` enforcement and regression
  coverage for both direct `parse_program()` and compiler parsing.

#### M68. `build_default_module_rel_path` Uses Hardcoded 255 Buffer Limit

- **File:** `src/lisp/jit_module_import_setup.c3:92-117`
- **Issue:** Fixed 2026-04-29. The helper baked in the caller's 256-byte stack
  buffer limit and the import caller reported the same generic error for
  default-path construction and resolved-path overflow.
- **Impact:** Confusing diagnostics and fragile helper/caller coupling.
- **Fix:** `build_default_module_rel_path()` now accepts explicit buffer
  capacity, clears output length on failure, checks pointer preconditions, and
  uses overflow-checked length math. Import errors now distinguish default-path
  and resolved-path overflow.

#### M69. `jit_warm_expr_cache` `E_WITH_MODULE` Body Elements Not Null-Checked Before Recursing

- **File:** `src/lisp/jit_apply_eval.c3:258-263`
- **Issue:** Fixed 2026-04-29. The original null-element framing was too
  narrow because recursive warm-cache calls already tolerate null elements; the
  real malformed-AST crash surface was `body_count > 0` with a null body-list
  pointer. Adjacent continuation scans had the same pointer/count assumption.
- **Impact:** Crash on malformed or partially allocated AST state.
- **Fix:** Added guarded warm-cache expression-list traversal and null list
  guards for shift/perform scans and match clause walkers. Regression coverage
  constructs malformed `with` ASTs with both missing body list and null body
  element cases.

#### M70. `handle_effect_state_capture_handler_copy` Leaks on Promotion Failure

- **File:** `src/lisp/jit_handle_signal_helpers.c3:166-194`
- **Issue:** Fixed 2026-04-29. If `boundary_promote_to_root` failed at
  iteration `i > 0`, previously promoted closures in
  `copied_closures[0..i-1]` were not released.
- **Impact:** Minor memory leak on handler-copy promotion error paths.
- **Fix:** Added a partial handler-closure cleanup helper that rolls back
  already promoted entries with `boundary_cleanup_materialized_value()` before
  freeing the scratch arrays, including the ESCAPE-lane primitive cleanup case.
  Regression coverage forces a second handler entry to fail root promotion after
  the first copied closure retained a detached env scope, then verifies the
  env-scope refcount and destructor state are restored.

#### M71. `jit_apply_multi_args_iterative` Does Not Check for Null Intermediate Results

- **File:** `src/lisp/jit_apply_multi_prims.c3:214-225`
- **Issue:** Fixed 2026-04-29. The original null-deref framing was partly
  stale because `jit_apply_value_impl()` already guarded null callees. The
  remaining contract gap was that `jit_apply_value()` and multi-arg dispatch
  paths could still return null if a callee path returned null, allowing
  iterative multi-arg application to either pass a misleading null intermediate
  into the next call or return null on the final iteration.
- **Impact:** Fail-open/null result or misleading downstream error on malformed
  or future callee paths.
- **Fix:** Added shared JIT apply-result normalization and used it at
  `jit_apply_value()`, direct multi-arg primitive/closure dispatch, non-tail
  and tail multi-param closure chain paths, and the iterative multi-arg loop.
  Regression coverage installs a primitive that deliberately returns null and
  asserts direct `jit_apply_value()`, direct multi-arg primitive dispatch, and
  iterative multi-arg application all fail closed with the same null-apply
  error.

#### M72. `jit_continuation_drive_blocked_fiber` Can Spin If Fiber State Is Corrupted

- **File:** `src/lisp/jit_continuation_runtime.c3:54-73`
- **Issue:** Fixed 2026-04-29. The blocked-fiber propagation loop re-resumed a
  suspended context while the scheduler's current fiber stayed blocked. If the
  target repeatedly re-suspended or fiber state was corrupted, the helper had no
  local bound. The adjacent handle-body switch loop duplicated the same
  unbounded propagation shape.
- **Impact:** Potential infinite loop on scheduler/fiber state corruption.
- **Fix:** Added an explicit `JitBlockedFiberDriveStatus` result and a
  scheduler-round based propagation limit with a test-only low-limit override.
  All continuation/resolve/checkpoint/handle callers now distinguish yield
  failure from propagation-limit failure while preserving their own cleanup and
  interpreter-state restoration obligations. The legacy handle-body switch path
  now uses the same bounded drive helper. Regression coverage constructs a
  re-suspending target stack context under a synthetic blocked scheduler fiber
  and proves the drive exits with `JIT_BLOCKED_FIBER_DRIVE_LIMIT_EXCEEDED`
  instead of looping.

#### M73. `Compiler.compile_to_temp_non_null` Does Not Check `expr` for Null Before Tag Access

- **File:** `src/lisp/compiler_temp_core.c3:20-58`
- **Status:** Stale as a live null-deref path as of 2026-04-29.
- **Issue:** `compile_to_temp_non_null` still dereferences `expr.tag` without an
  explicit contract assertion, but current code only calls it from
  `compile_to_temp()` after that wrapper checks `expr == null`;
  `compile_to_temp_tail()` also has its own null guard.
- **Impact:** No current production call path can pass null directly, but the
  helper's non-null precondition is implicit.
- **Fix:** Optional defensive hardening: add an explicit `@require expr != null`
  or assertion if future direct callers are introduced.

---

### 🟢 Low (New)

#### L37. `register_stdlib` Ignores `run()` Return Value

- **File:** `src/lisp/eval_stdlib_loader.c3:50`
- **Status:** Fixed 2026-04-29.
- **Issue:** `run(src[start..pos - 1], interp);` return value was discarded, so errors during stdlib loading were silently ignored.
- **Fix:** `register_stdlib()` now returns `bool`, `bootstrap_runtime_interp()` propagates the stdlib result, and regression coverage forces a stdlib registration failure after primitive registration. Enabling this fail-closed path exposed and fixed a malformed trailing `)` in `stdlib/stdlib.lisp`.

#### L38. `ffi_symbol_name_equals` Missing Null Check on `interp`

- **File:** `src/lisp/value_predicates_accessors_basic.c3:221-223`
- **Status:** Fixed 2026-04-29.
- **Issue:** `interp.symbols.get_name(sym)` was accessed without checking `interp != null`.
- **Fix:** Added a null-interpreter guard that returns `false`, with FFI metadata regression coverage.

#### L39. `eval_ffi_lib` Doesn't Check `interp.global_env` Null Before Use

- **File:** `src/lisp/eval_ffi_eval.c3:27-75`
- **Status:** Fixed 2026-04-29.
- **Issue:** `env_define_with_barrier_checked(interp.global_env, ...)` was called without verifying `interp.global_env != null`.
- **Fix:** Added an early `ffi/invalid-state` guard before path evaluation or dlopen acquisition, with FFI surface regression coverage.

#### L40. `eval_path_step` Inconsistent Null Checking Strategy

- **File:** `src/lisp/eval_path.c3:117-125`
- **Status:** Fixed 2026-04-29.
- **Issue:** Reframed by subagent review. The original null-deref concern was overstated because invalid-step formatting tolerates null values. The live defect was precondition ownership drift: `eval_path_step` mixed dispatcher-level payload validation with helper-level validation, bypassing type-specific helper guards and leaving direct helper contracts unclear.
- **Fix:** `eval_path_step` now dispatches by tag and lets module/instance/dict/cons helpers own payload validation. Root lookup also tolerates null `env`/`global_env`. Regression coverage constructs malformed MODULE/INSTANCE/HASHMAP values with null payloads and asserts fail-closed path errors.

#### L41. `compiler_emit_symbol_name` Can Emit Extremely Long Identifiers

- **File:** `src/lisp/compiler_output_symbol_helpers.c3:22-62`
- **Status:** Fixed 2026-04-29.
- **Issue:** Reframed by subagent review. The defect was broader than length blowup: generated C3 identifiers used lossy partial mangling, so symbols such as `a?`/`a_p` could collide, raw invalid identifier characters such as `:` could leak into output, reserved/digit/uppercase prefix rewrites could collide with user-authored `_omni_*` names, and legal long symbols could expand into oversized generated identifiers.
- **Fix:** `Compiler.emit_symbol_name` now keeps only simple raw C3-safe symbols in the raw namespace. Any lossy, generated-prefix, reserved, `_omni_*`, or over-budget name is emitted through a bounded `_omni_sym_..._h<hash>_l<len>` namespace. Regression coverage verifies capped long symbols, punctuation escaping, generated namespace collisions, reserved-name collisions, and digit-leading prefix collisions.

#### L42. `jit_cache_find_slot` Probe Limit Hardcoded to 16

- **File:** `src/lisp/jit_compiler_cache.c3:75-92`
- **Status:** Reframed 2026-04-29; no correctness fix applied.
- **Issue:** Subagent review found the original `jit_cache_find_slot` wording stale for the live store path: the helper still has `probe < 16`, but current stores route through `runtime_cache_store_expr`, which clears and retries on 16-slot saturation instead of silently dropping entries. The remaining concern is a performance/retention policy: a 17-entry collision cluster can evict the cache before the configured GC threshold.
- **Fix:** No code change in this slice. Treat the bounded 16-slot window as an explicit cache-retention policy item unless a future profile shows churn; the existing `cache-probe-saturation-recovery` JIT policy test passes.

#### L43. `jit_retired_code_insert` Does Not Use Any Synchronization

- **File:** `src/lisp/jit_compiler_retired_code.c3:26-45`
- **Status:** Reframed 2026-04-29; stale for current production contract.
- **Issue:** Subagent review found the synchronization concern stale for current production code. The retired-code table is unsynchronized, but production JIT lifecycle and detach paths are owner-thread confined by `jit_require_owner_thread`/runtime identity checks before they mutate or reset the table. Remaining direct callers are test-only.
- **Fix:** No code change in this slice. If Omni later enables true multi-threaded JIT compilation/execution, this table must become synchronized or per-owner-thread isolated before that mode ships; under the current owner-thread contract there is no live race to repair.

#### L44. `aot_bridge_interp` and `aot_resolve_interp` Return Null Without Raising Error

- **File:** `src/lisp/aot_runtime_bridge_closure.c3:185-205`
- **Status:** Fixed 2026-04-29.
- **Issue:** Reframed by subagent review. Normal generated AOT mains already gate runtime initialization, and many bridge callers now return contextual error values when an interpreter exists. The live gap was the real pre-init/post-shutdown path: when `g_aot_interp` is null, `aot_make_error_best_effort(null, ...)` and direct `aot_resolve_interp` helper branches collapsed to raw null with no observable diagnostic.
- **Fix:** Added a centralized out-of-band AOT diagnostic path for no-interpreter error construction, with test-visible count/last-message seams and stderr output. Direct closure apply and closure factory null-interpreter branches now route through that diagnostic before preserving their null return contract. Compiler fail-closed coverage verifies lookup, closure apply, and closure factory pre-init diagnostics.

#### L45. `emit_build_locals_env` Does Not Check `jit_env_extend` Return Value

- **File:** `src/lisp/jit_emit_helpers.c3:103-147`
- **Status:** Fixed 2026-04-29.
- **Issue:** After emitting the call to `jit_env_extend`, the result was placed in `V2` without any null-check branch, so local closure capture could continue with a null captured env after binding allocation failure.
- **Fix:** `emit_build_locals_env` now branches on a null non-mutable `jit_env_extend` result and returns a `runtime/out-of-memory` error immediately. JIT policy coverage forces env binding OOM while capturing a non-mutable let local and verifies the direct `"out of memory while binding local"` error.

#### L46. `jit_make_closure_from_expr` Does Not Check `expr.lambda.body` Validity

- **File:** `src/lisp/jit_closure_support.c3:212-246`
- **Status:** Fixed 2026-04-29.
- **Issue:** The helper validated neither the lambda node/payload/body nor the closure constructor result before publishing or touching the closure payload. Malformed macro/shared-boundary ASTs could create a closure with `closure_val.body == null`, and constructor errors could be dereferenced as closures.
- **Fix:** `jit_make_closure_from_expr` now rejects null/non-lambda/malformed lambda ASTs with `runtime/invalid-state`, and normalizes constructor results before closure payload access. JIT policy coverage constructs malformed zero-arg and one-arg lambda ASTs and verifies fail-closed errors.

#### L47. `Lexer.scan_number_fraction` Underflows `frac` Without Bounds

- **File:** `src/lisp/parser_lexer_number_helpers.c3:18-27`
- **Status:** Fixed 2026-04-29.
- **Issue:** `frac *= 0.1` in a loop with unbounded digit count eventually underflowed to subnormal / zero while continuing unbounded work. Subagent review found the adjacent exponent parser had the same boundary defect: unbounded `long` accumulation plus `exp_val` repeated multiplication could hang or overflow on huge exponents.
- **Fix:** Lexer float literal parsing now caps meaningful fractional digits while still consuming trailing fraction input, rejects missing/oversized exponents fail-closed, and rejects non-finite float literals. `parse-number` was aligned to the same bounded fraction/exponent policy while preserving Float64-overflow promotion to `BigFloat`. Regression coverage verifies long fractional literals remain bounded/finite, huge literal exponents fail closed, and `parse-number "1.0e309"` promotes to `BigFloat`.

#### L48. `inet_ntop` Failure Check Uses `0` Instead of `NULL`

- **File:** `csrc/addrinfo_helpers.c:44`
- **Status:** Reframed 2026-04-29; false positive as a correctness bug, readability cleanup applied.
- **Issue:** The audit claim that `inet_ntop(...) == 0` is always false was incorrect because C treats `0` as a null pointer constant. The branch was semantically live, but the pointer-zero style made the code easy to misread.
- **Fix:** Replaced native pointer-zero checks in `addrinfo_helpers.c` with `NULL` and included `<stddef.h>`. Added a DNS render regression that calls `omni_addrinfo_render_ip` with a too-small buffer and verifies `OMNI_ADDRINFO_RENDER_FAILED`.

#### AUDIT-261. FFI `^Void` Parameter Metadata Survives Declaration

- **File:** `src/lisp/eval_ffi_eval.c3:235`, `src/lisp/aot_runtime_bridge_ffi.c3:212`
- **Status:** Fixed 2026-04-30.
- **Issue:** `^Void` is a return-only FFI annotation, but `define [ffi λ]` and
  the AOT runtime bridge accepted it in parameter metadata. The invalid ABI
  role then failed later during argument packing instead of at declaration.
- **Fix:** Interpreter/JIT FFI declarations now reject `^Void` parameters at
  definition time, and the AOT bridge rejects raw `FFI_TYPE_VOID` parameter
  tags before publishing a primitive. Regression coverage verifies both paths.

#### AUDIT-262. FFI Callback `Void` Parameter Metadata Reaches libffi

- **File:** `src/lisp/prim_ffi_callback.c3:93`
- **Status:** Fixed 2026-04-30.
- **Issue:** `Void` is a callback return type, not a value-bearing C argument
  type. `ffi-callback` accepted it in parameter metadata and could defer the
  failure to libffi closure preparation instead of rejecting the callback
  declaration.
- **Fix:** Callback parameter parsing now rejects `FFI_TYPE_VOID` through a
  shared role predicate for list/array and variadic callback forms. Regression
  coverage verifies both callback syntaxes reject `Void` parameters.

---

## Recommended Fix Priority Order

1. **C1-C2** (missing `break` in tensor map) — already invalidated elsewhere; keep off implementation queue unless new evidence appears
10. **C8-C10** (method dispatch null dereferences) — segfault on null method table
11. **C11** (`scope_dtor_value` missing `break` after STRING/ERROR) — type-confusion crash
12. **C16** (`fpu_save` null dereference on x86_64) — fixed 2026-04-29
13. **C17** (BLAS dger buffer overflow) — fixed 2026-04-29
14. **C18** (JIT multi-arg primitive null dereference) — fixed 2026-04-29
15. **H7-H8** (`copy_to_parent_note_tag` / `boundary_escape_shape_note_root` missing `break`) — invalidated 2026-04-29
16. **H9** (exponent parsing overflow + DoS) — invalidated 2026-04-29
17. **H10-H11** (parser root allocation null dereferences) — fixed 2026-04-29
18. **H12** (raw primitive body arity `<` instead of `!=`) — fixed 2026-04-29
19. **H13** (async I/O handle leak on OOM) — invalidated 2026-04-29
19a. **H16** (module cache/index stale entries) — fixed 2026-04-29
19b. **H17** (symbol ID exhaustion) — invalidated 2026-04-29
19c. **H18** (duplicate ScopeRegion destructor registration) — fixed 2026-04-29
19d. **H19** (BigInteger binary null handles) — fixed 2026-04-29
20. **H22** (HTTP header slice underflow) — invalidated 2026-04-29
21. **H23** (Deduce path traversal) — invalidated 2026-04-29 under raw path contract
22. **H24** (unbounded JIT spill-list growth) — fixed 2026-04-29
23. **H25** (retired-code tombstone saturation handling) — fixed 2026-04-29
24. **H27-H29** (`eval_path_*` null dereferences) — invalidated 2026-04-29
25. **H30** (`Env.hash_lookup` infinite loop) — fixed 2026-04-29
26. **H31-H38** — H34-H37 fixed 2026-04-29; H31, H32, H33, and H38 invalidated 2026-04-29
27. **H1** (closure materialize leak) — invalidated; staged ESCAPE build-scope rollback owns failure cleanup
28. **H4** (CUDA diagonal overflow) — GPU memory safety
29. **H5-H6** (Vulkan refcount/threading) — concurrency safety
29. **M1-M2** (`long.min` UB) — integer overflow
30. **M3-M4** (non-exhaustive switches) — silent misbehavior on enum extension
31. **M10** (FFI callback race) — thread safety
32. **M47-M53** (scheduler/TLS/signal/BLAS/LAPACK/Vulkan/UV races and truncations) — concurrency and correctness
33. **M59-M73** (fifth-pass medium findings: evaluator leaks, JIT non-exhaustive switches, parser limits, compiler null checks) — correctness
34. **M18, L1-L6** (prior audit carry-overs) — hygiene
35. **M19-M31, L13-L26** (second-pass medium/low findings) — correctness and robustness
36. **M54-M58, L27-L36** (fourth-pass build/test/script findings) — CI reliability and test correctness
37. **L37-L48** (fifth-pass low findings: missing null checks, unbounded loops, identifier expansion, inet_ntop bug) — L8 and L48 are invalidated; remaining live items need separate evidence
