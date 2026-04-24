# Omni Lisp Deep Audit Report — 2026-04-24

**Auditor:** Crush
**Scope:** Full codebase (`src/`, `csrc/`, build system, tests, scripts)
**Build status:** `c3c build` passes (with `LIBRARY_PATH` set); tests: 169 passed, 0 failed
**Baseline:** Prior audits 2026-04-21, 2026-04-22, 2026-04-23, VULKAN_CUDA_ML 2026-04-23

---

## Executive Summary

The codebase is in a healthy state after the intensive audit-and-fix cycles of the past week. Most critical and high findings from prior audits are closed. This pass identifies **2 high-priority build/CI issues**, **9 medium-priority correctness and style issues**, and **3 low-priority hygiene items**. No critical memory-safety defects were found in the live tree.

| Severity | Count | Key Areas |
|----------|-------|-----------|
| High | 2 | AOT manifest drift, file-size gate failure |
| Medium | 9 | Format strings, scope telemetry, C helper null checks, mutex lifecycle |
| Low | 3 | Generation wraparound, non-exhaustive switch defaults, missing `@require` |

---

## 🔴 High (Fix in Next Sprint)

### H1. AOT Runtime Manifest Missing 9 `src/lisp/*.c3` Sources

**Files:** `src/entry_build_runtime_manifest_lisp_part{0,1,2,3}.c3`
**Validation:** `scripts/check_e2e_baseline_policy.sh` fails with:
```
FAIL: AOT lisp runtime manifests do not contain every non-test src/lisp/*.c3 source
```

**Missing files:**
- `src/lisp/eval_boundary_planner.c3`
- `src/lisp/ffi_bridge_boundary.c3`
- `src/lisp/macros_builtin_str.c3`
- `src/lisp/parser_with_module.c3`
- `src/lisp/stable_escape_epoch.c3`
- `src/lisp/stable_escape_store.c3`
- `src/lisp/stable_escape_store_passport.c3`
- `src/lisp/symbol_lambda_helpers.c3`
- `src/lisp/value_boundary_ownership_policy.c3`

**Impact:** AOT builds silently omit these sources, leading to missing runtime primitives or boundary behavior when the compiler is built from AOT manifest.

**Fix:** Add the 9 missing entries to the appropriate manifest part arrays and bump their declared sizes.

---

### H2. File Size Gate Exceeded

**File:** `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3` (1015 LOC)
**Validation:** `scripts/check_file_size_gate.sh` reports:
```
FAIL: tracked code files above 1000 LOC:
  1015 src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3
```

**Impact:** Blocks `scripts/run_global_gates.sh` at Stage 0.

**Fix:** Split the file into `part8.c3` and `part9.c3` (or similar) so each stays under 1000 LOC.

---

## 🟡 Medium (Fix When Touching Related Code)

### M1. Format String Mismatches in Boundary Telemetry / Logging

**Files:**
- `src/scope_region_temp_pool_stats.c3:260-264` — `%d` used with `usz` fields (`splice_calls`, `splice_escape_chunk_bytes`, etc.). Should be `%zu`.
- `src/lisp/eval_boundary_graph_audit_logging.c3:14-50` — `%d` used with `(long)result.target_escape_gen` and `(long)result.root_scope_gen`. Should be `%ld`.
- `src/lisp/eval_boundary_graph_audit_telemetry.c3` — same `%d`/`(long)` pattern across telemetry counters.

**Impact:** Silent truncation or UB on platforms where `long`/`usz` differ from `int`.

**Fix:** Audit all `io::printfn`/`io::eprintfn` in boundary files and use `%zu` for `usz`, `%ld` for `long`.

---

### M2. `char[]` Slices Passed to `%s` Format Specifier

**Files:**
- `src/scope_region_global_guards.c3:54` — `scope_thread_violation` passes `char[] op` to `%s`.
- `src/lisp/eval_boundary_graph_audit_meta.c3:15-37` — `boundary_scope_transfer_reason_name`, `boundary_promotion_abort_reason_name`, `boundary_return_provenance_name` return `char[]`, which is then passed to `%s` in callers (`eval_boundary_diagnostics.c3:34`, `eval_boundary_graph_audit_telemetry.c3:39`, `eval_boundary_telemetry.c3:475`).

**Impact:** `char[]` is a slice (ptr+len) not guaranteed null-terminated. Passing it to `%s` is undefined behavior if the slice lacks a trailing `\0`.

**Fix:** Cast to `ZString` where the underlying string is known null-terminated, or use `%.*s` with length if C3's printf supports it. Alternatively, change the helpers to return `ZString`.

---

### M3. Scope Fast-Path Allocators Lack `@require self != null`

**Files:**
- `src/scope_region_allocators.c3:89` — `ScopeRegion.alloc`
- `src/scope_region_allocators.c3:111` — `ScopeRegion.alloc_escape`

**Problem:** `scope_guard_owner` returns early when `self == null`, but the macro expansion falls through to `self.bump` / `self.escape_bump` dereference. A null `self` would crash. These are `@inline` hot-path functions; callers are expected to ensure non-null, but there is no compile-time contract enforcing it.

**Fix:** Add `@require self != null` to both functions.

---

### M4. Inconsistent Telemetry Reset in `scope_reset_temp_lane`

**File:** `src/scope_region_reset_helpers.c3:74-104`

**Problem:** `scope_reset` (line 19) zeroes `alloc_bytes` and `alloc_count` at lines 65-66. `scope_reset_temp_lane` (line 74) resets TEMP lane destructors and chunks but leaves `alloc_bytes` and `alloc_count` unchanged. This creates cumulative drift across lane-only resets.

**Fix:** Zero `alloc_bytes` and `alloc_count` in `scope_reset_temp_lane` to match `scope_reset` semantics.

---

### M5. Missing Mutex Destruction at Shutdown

**File:** `src/scope_region_reset_adopt.c3:112-136` (`scope_freelist_cleanup`)

**Problem:** `g_scope_global_mu` is initialized lazily in `scope_global_mu_init_once` (`src/scope_region_global_guards.c3:22-28`) but `scope_freelist_cleanup` never calls `g_scope_global_mu.destroy()`. On platforms where mutex destruction is required (e.g., pthread mutex attr cleanup), this is a resource leak.

**Fix:** Add `g_scope_global_mu.destroy()` at the end of `scope_freelist_cleanup` after the last lock/unlock pair.

---

### M6. BLAS Symbol Resolution Only Guards `dgemm`

**File:** `csrc/tensor_blas_helpers.c:94-99`

**Problem:** `omni_tensor_blas_resolve` checks `if (dgemm_symbol != NULL)` and then assigns ALL symbols (`dgemv`, `ddot`, `dger`) without individual null checks. The `available` functions later guard each call, but NULL function pointers are stored globally.

**Impact:** Low — callers check availability before invoking. But it is a brittle contract; a future caller could skip the check.

**Fix:** Either check each symbol individually before assignment, or document the contract explicitly.

---

### M7. LAPACK Symbol Resolution Assigns All Symbols When Any Is Present

**File:** `csrc/tensor_lapack_helpers.c:309-333`

**Problem:** Same pattern as M6. The `if` condition at line 309 is a disjunction of all symbols. If any is non-NULL, all are assigned, including unchecked NULL pointers.

**Fix:** Same as M6 — individual null checks or explicit contract documentation.

---

### M8. Vulkan Queue Family Properties Return Value Unchecked

**File:** `csrc/tensor_vulkan_helpers_core.c:237`

**Problem:** The second call to `omni_vulkan_get_queue_family_properties` (with the allocated `queues` array) does not check its return value. If it fails, `queues` still contains zeroed `calloc` data, but `queue_count` retains the value from the first call (line 232), potentially causing the loop at lines 239-256 to iterate over stale/zero data.

**Impact:** Low — `calloc` zeros the buffer, so reads are safe but may yield no valid queues.

**Fix:** Check the return value and skip the device if the second call fails.

---

### M9. Default Cases in Non-Exhaustive Switches (Style Drift)

**Policy:** `docs/C3_STYLE.md` §10 says "Avoid catch-all `default` in tag dispatch unless there is a deliberate forward-compatible fallback."

Dozens of compiler and AOT bridge files still contain `default:` in `ExprTag`, `ValueTag`, and pattern switches. A sampling:
- `src/lisp/compiler_expr_serialize_patterns.c3:84`
- `src/lisp/compiler_free_vars_walk_helpers.c3:42,59,94`
- `src/lisp/compiler_mutable_capture_prescan.c3:155`
- `src/lisp/aot_runtime_bridge_ffi.c3:36,46`
- `src/lisp/aot_runtime_match_helpers.c3:26`

**Impact:** Adding a new enum variant will silently fall through `default:` instead of triggering a compile error.

**Fix:** Remove `default:` arms and add explicit cases for all tags. Let the compiler enforce exhaustiveness.

---

## 🟢 Low (Code Hygiene)

### L1. Generation Counter Wraparound Theoretical Risk

**File:** `src/lisp/eval_boundary_provenance.c3:630-634`

**Problem:** `boundary_cons_escape_child_edge_safe` compares `edge.scope_gen != target_scope.generation`. Both are `uint` (32-bit). In a very long-running process, `g_scope_generation_counter` could wrap. A recycled scope could then coincidentally match an old `scope_gen`, causing a live TEMP edge to appear safe.

**Impact:** Extremely low probability (requires >4 billion scope allocations).

**Fix:** Use a 64-bit generation counter, or add an explicit "epoch" field to scopes.

---

### L2. Missing `@require` Contracts on Core Boundary Functions

**Files (selection):**
- `src/scope_region_reset_adopt.c3:7` — `scope_splice_escapes`
- `src/scope_region_chunk_helpers.c3:61` — `scope_register_dtor`
- `src/scope_region_chunk_helpers.c3:75` — `scope_register_dtor_escape`
- `src/scope_region_destroy.c3:98` — `scope_destroy_owned_descendants`

These public API functions take `ScopeRegion*` without `@require` preconditions.

---

### L3. Build Environment Dependency on `LIBRARY_PATH`

**Observation:** `c3c build` on the host requires `LIBRARY_PATH=/home/christos/.local/lib` to find `liblightning` and `libreplxx`. The `project.json` `linker-search-paths` only includes `/usr/local/lib`. The Docker validation image presumably has these libraries in the expected path, but host builds are fragile.

**Fix:** Add `/home/christos/.local/lib` to `linker-search-paths` in `project.json`, or document the `LIBRARY_PATH` requirement.

---

## Verified False Positives from Agent Scans

| Claim | Verdict | Reason |
|-------|---------|--------|
| Missing `break` in C3 switches across `prim_tensor_vulkan_map_direct.c3`, `prim_nn_training.c3`, `eval_boundary_telemetry.c3` | **False** | C3 has implicit `break` in `switch` statements. Verified with compiler tests. |
| Null dereference in `ScopeRegion.alloc` via `scope_guard_owner` | **Partially false** | `scope_guard_owner` returns early on null, but the function continues to dereference `self`. The real issue is missing `@require` (M3), not an immediate crash in well-behaved callers. |
| `boundary_commit_normalize_closure_env` leaks `replacement_env_scope` | **False** | The scope is released on copy failure (line 41) or transferred to the closure (line 56). Correct ownership. |
| `E_WITH_MODULE` missing in `compiler_mutable_capture_detection_walk.c3` | **False** | Both `has_set_on` and `is_captured_by_nested_lambda` contain `case E_WITH_MODULE:`. The E2E failure observed in an earlier run did not reproduce against the live tree. |
| JIT null dereference risks (`jit_apply_runtime.c3`, `jit_closure_let_set_helpers.c3`, etc.) | **Mostly false** | Tag checks (`func.tag == CLOSURE`) guarantee the corresponding union field is valid in normal operation. Defensive `@require` additions would be style improvements, not correctness fixes. |

---

## Recommendations (Prioritized)

1. **Fix H1** — Add the 9 missing files to AOT manifest parts and bump array sizes.
2. **Fix H2** — Split `tests_advanced_stdlib_module_groups_generic_ops_part8.c3` to pass the 1000 LOC gate.
3. **Fix M1/M2** — Audit and correct all `%d`→`%zu`/`%ld` and `%s`→`ZString` format mismatches in boundary/logging files.
4. **Fix M3** — Add `@require self != null` to `ScopeRegion.alloc` and `alloc_escape`.
5. **Fix M4** — Zero `alloc_bytes`/`alloc_count` in `scope_reset_temp_lane`.
6. **Fix M5** — Add `g_scope_global_mu.destroy()` to `scope_freelist_cleanup`.
7. **Fix M6/M7** — Individual null checks in BLAS/LAPACK resolution, or document the contract.
8. **Fix M8** — Check second `omni_vulkan_get_queue_family_properties` return value.
9. **Fix M9** — Opportunistically remove `default:` from tag switches when touching compiler/AOT files.
10. **Fix L3** — Add `.local/lib` to `linker-search-paths` or document the host build requirement.

---

## Validation Commands

```bash
# Build
c3c build

# Test suite
LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite all

# AOT manifest parity
scripts/check_e2e_baseline_policy.sh

# File size gate
scripts/check_file_size_gate.sh

# Status consistency
scripts/check_status_consistency.sh
```

*Report compiled from live source analysis on 2026-04-24.*
