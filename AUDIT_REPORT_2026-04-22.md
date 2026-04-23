# Omni Lisp Codebase Audit Report

**Date:** 2026-04-22
**Scope:** Full codebase — `src/` (C3), `csrc/` (C/C++), build system, tests
**Auditor:** Crush
**Mandate:** Find regressions, defects, and improvement vectors introduced since 2026-04-21 remediation.

> 2026-04-23 status note: this report is a historical audit input, not the
> current live queue. Follow-up items from this report and `findings.md` were
> reconciled through `docs/todo_parts/todo_part_15.md`,
> `memory/changelog_parts/changelog_part_37.md`, and `.agents/SESSION_REPORT.md`.
> `TODO.md` is the canonical current queue and currently reports no open
> actionable items.

---

## Executive Summary

The 2026-04-21 audit remediation commit (a06ba2e) was massive (313 files, 24k+ insertions) and **introduced concrete regressions** in the build system and AOT manifest tracking. Additionally, several high-severity issues from the prior audit report (`AUDIT_REPORT_2026-04-21.md`) were **not remediated** and remain active. The most severe new issue is a **guaranteed null-pointer dereference** in the JIT compiler when allocation fails under memory pressure.

**Issue counts by severity:**

| Severity | Count | Description |
|----------|-------|-------------|
| 🔴 **Critical** | 1 | Null-pointer dereference in JIT compilation path |
| 🟠 **High** | 5 | Build regressions, stale artifacts, format mismatches, AOT manifest drift |
| 🟡 **Medium** | 6 | Error suppression, FFI lifetime gaps, platform limitations, module cycle |
| 🟢 **Low** | 3 | Non-exhaustive switches, missing contracts, unbounded scans |

---

## 🔴 Critical (Fix Immediately)

### C1. Unchecked `malloc` → Null-Pointer Dereference in JIT Compiler

**Files:**
- `src/lisp/jit_compile_expr_core.c3:94-99`
- `src/lisp/jit_compile_expr_core.c3:117-122`
- `src/lisp/jit_compile_expr_core.c3:279-284`

**Problem:** Three functions (`jit_compile_tail_list_escape`, `jit_compile_tail_array_escape`, `jit_compile_multi_arg_many`) allocate `arg_slots` via `malloc` when `argc > 0`:

```c3
int* arg_slots = argc > 0
    ? (int*)mem::malloc(int.sizeof * argc)
    : null;
defer if (arg_slots != null) mem::free(arg_slots);
for (usz i = 0; i < argc; i++) {
    arg_slots[i] = _jit_allocai(s, 8);  // NULL DEREF if malloc failed
```

If `malloc` returns `null` (e.g., under memory pressure or large `argc`), `arg_slots` is `null` but the loop still executes because `argc > 0`, causing an immediate null-pointer dereference.

**Impact:** JIT compilation of multi-arg calls crashes the interpreter instead of failing gracefully.

**Fix:** Add `if (arg_slots == null) return null;` (or equivalent error propagation) immediately after the `malloc` call in all three functions.

---

## 🟠 High (Fix in Next Sprint)

### H1. Stale Prebuilt Library Artifact Causes E2E Linker Failure

**File:** `build/libomni_chelpers.a` (prebuilt static library)

**Problem:** The prebuilt archive contains an obsolete object `csrc__tensor_vulkan_helpers_matrix_ops_c.o` that defines symbols now also defined in newer split files (e.g., `tensor_vulkan_helpers_matrix_ops_spectral.c`). The `scripts/build_omni_chelpers.sh` build script uses `ar rcs` which **replaces** existing members but **never deletes stale members** that are no longer in the source list. When `tensor_vulkan_helpers_matrix_ops.c` was split into multiple smaller files, the old object remained in the archive, causing:

```
multiple definition of `omni_tensor_backend_vulkan_singular_values_complex128'
multiple definition of `omni_tensor_backend_vulkan_svd_f64'
...
collect2: error: ld returned 1 exit status
```

**Impact:** E2E tests fail to link. Any developer or CI that rebuilds the main binary but uses the stale archive will hit a linker error.

**Fix:** Purge stale members from the archive in `scripts/build_omni_chelpers.sh` before `ar rcs`. Minimal fix: `rm -f "$ARCHIVE"` before creating it. Already applied during this audit session to verify the diagnosis.

---

### H2. AOT Runtime Manifest Missing Two Source Files

**File:** `src/entry_build_runtime_manifest_lisp_part3.c3`

**Problem:** Two recently added `src/lisp/*.c3` files were omitted from the AOT manifest array:
- `src/lisp/prim_ffi_callback.c3`
- `src/lisp/prim_ml_autograd_vulkan_broadcast.c3`

The e2e test script (`scripts/check_e2e_baseline_policy.sh`) compares the manifest against the actual `src/lisp/*.c3` file list. The mismatch caused:

```
FAIL: AOT lisp runtime manifests do not contain every non-test src/lisp/*.c3 source
```

**Impact:** E2E tests fail at stage 3. AOT builds may silently omit runtime sources, leading to missing primitives at runtime.

**Fix:** Add the missing entries to `AOT_RUNTIME_LISP_SOURCES_PART3` and bump the array size from `230` to `232`. Already applied during this audit session.

---

### H3. Format String Mismatches Persist After Remediation

**Impact:** Silent truncation or undefined behavior on platforms where `long`/`usz` differ from `int`.

The 2026-04-21 audit report catalogued these extensively, but **none were fixed** in the remediation commit.

| File | Line(s) | Format | Argument Type |
|------|---------|--------|---------------|
| `src/lisp/eval.c3` | 143-178 | `%d` | `usz` (size_t) |
| `src/lisp/value_print_buf.c3` | 220 | `%d` | `long` (int_val) |
| `src/lisp/value_print_helpers.c3` | 216, 224 | `%d` | `long` / `usz` |
| `src/lisp/value_constructors.c3` | 69, 137 | `%d` | `usz` (max_len, name.len) |

**Fix:** Use `%zu` for `usz`, `%ld` for `long`. Audit all `io::printf` / `io::bprintf` / `io::eprintf` calls in the touched files.

---

### H4. `(void)` Error Suppression in REPL Server Worker

**File:** `src/lisp/eval_repl_server_worker.c3:19, 24`

**Problem:** Session creation failures discard the return value of `repl_server_send_protocol_error`:

```c3
(void)repl_server_send_protocol_error(output, command.id_ptr[:command.id_len], ...);
```

If the protocol error cannot be sent (e.g., socket closed), the client receives no indication of the failure.

**Fix:** Log the failure or propagate it up the call stack.

---

### H5. Hash Table Build Results Still Discarded

**File:** `src/lisp/value_environment.c3:50, 58`

**Problem:** The return value of hash table rebuild is cast to `(void)` with a comment justifying it as "optional fast path." However, if the hash table fails to build, the environment silently falls back to O(n) linear search for all subsequent lookups in that scope.

**Fix:** At minimum log the failure. Consider making the hash build non-optional for scopes above a small binding count.

---

## 🟡 Medium

### M1. FFI Callback Context Stores Raw `Value*` Without Interpreter Lifetime Guarantee

**File:** `src/lisp/prim_ffi_callback.c3:52-61, 225-248`

**Problem:** `FfiCallbackContext` stores:
- `Value* callback` — raw pointer to a closure/lambda
- `Interp* interp` — raw pointer to the interpreter
- `main::ScopeRegion* owner_scope` — retained scope

The `owner_scope` is retained, but `interp` is **not**. If the interpreter is destroyed while a C closure is still active (e.g., via FFI callback into Omni from a background thread), `ffi_callback_dispatch` dereferences a dangling `Interp*` after only checking `!= null`.

**Fix:** Retain the interpreter or add an explicit invalidation mechanism that runs before interpreter teardown.

---

### M2. ASAN Unavailable on aarch64 Linux

**Platform:** Linux aarch64 (Ubuntu 26.04, kernel 6.17, C3 0.7.11)

**Problem:** `c3c build --sanitize=address` fails with:
```
Address sanitizer is only supported on Linux, FreeBSD, NetBSD, Darwin and Windows.
```

Despite running on Linux, the C3 compiler does not support ASAN on aarch64. This blocks memory-safety validation for the primary development platform.

**Fix:** Use an x86_64 container for ASAN validation, or upgrade to a C3 version with aarch64 ASAN support.

---

### M3. Structural Module Cycle `main` ↔ `lisp` Still Pervasive

**Stats:** 100+ files in `src/lisp/` import `main`.

**Problem:** The `main` ↔ `lisp` module cycle was flagged in the 2026-04-21 audit but was **not addressed** in the remediation. It complicates incremental builds and indicates unclear architectural boundaries.

**Fix:** Extract shared types/contracts into a `runtime_contracts` module that both sides import. `lisp` should never import `main`.

---

### M4. `default:` Cases Still Hide Non-Exhaustive Switches

**Policy violation:** `docs/C3_STYLE.md` says "no default case for ValueTag switches."

Multiple files from the previous audit report still contain `default:` in tagged-union switches:

| File | Line | Context |
|------|------|---------|
| `src/lisp/eval.c3` | 61 | `copy_to_parent_note_tag` |
| `src/lisp/eval_dispatch_types.c3` | 200 | `infer_value_type` |
| `src/lisp/prim_ffi_callback.c3` | 120, 195 | `FfiTypeTag` switches |
| `src/lisp/foreign_runtime_core.c3` | 32, 132 | `ForeignRuntimeKind`, `FfiTypeTag` |

**Fix:** Remove `default:` and add all missing tag arms. Let the compiler enforce exhaustiveness.

---

### M5. Unbounded String Scans Still Present

**Files:**
- `src/lisp/eval_repl_server_state.c3:70`
- `src/entry_runtime_project_paths.c3:9`

**Problem:** Both implement custom `cstr_len` as unbounded `while (text[len] != 0)` with no `max_len` parameter. If passed a non-null-terminated pointer, they read past valid memory.

**Fix:** Pass a bounded `max_len` or use C3's built-in string slice length.

---

### M6. JIT Compilation of Multi-Arg Calls Lacks Allocation Failure Guard

**File:** `src/lisp/jit_compile_expr_core.c3`

**Note:** This is the same root cause as **C1**, but it also affects the `jit_compile_multi_arg_many` path which compiles non-tail multi-arg calls. The fix for C1 resolves both.

---

## 🟢 Low

### L1. Missing `@require` Contracts on Core Functions

**Policy violation:** `docs/C3_STYLE.md` says "Use clear preconditions (`@require`) where state assumptions are non-obvious."

Hundreds of internal helpers still lack `@require` for non-null `Interp*` / `Value*` parameters. Examples:
- `src/lisp/prim_ffi_callback.c3:30` — `ffi_callback_value_to_type_tag`
- `src/lisp/jit_compile_expr_core.c3:13` — `jit_get_direct_prim`
- `src/lisp/eval_apply.c3:5,27` — `apply_primitive`, `apply_partial`

**Fix:** Add `@require` for non-null pointers, valid tags, and valid state preconditions.

---

### L2. Heavy Code Duplication in Error Formatting

**Files:**
- `src/lisp/eval_ffi_bound_call.c3:41-58` — dlopen error formatting (duplicated in `eval_ffi_eval.c3:43-58`)
- `src/lisp/prim_ffi_async.c3:160-179` — FFI arg count mismatch error formatting (nearly identical to `prim_ffi_bound_call`)

**Fix:** Extract a shared `ffi_format_error(char[] buf, char* fmt, ...)` helper.

---

### L3. Magic Numbers in Vulkan / ML Code

**Files:** `csrc/tensor_vulkan_helpers_*.c`, `src/lisp/prim_ml_*.c3`

Multiple Vulkan helpers and ML primitives use inline magic numbers (e.g., `4096`, `65536`, `0.044715`) without named constants. The 2026-04-21 audit noted this; it remains unaddressed.

---

## Actions Taken During This Audit

1. **Fixed AOT manifest drift** — Added `prim_ffi_callback.c3` and `prim_ml_autograd_vulkan_broadcast.c3` to `src/entry_build_runtime_manifest_lisp_part3.c3`, updated array size `230 → 232`.
2. **Fixed stale prebuilt library** — Removed obsolete `csrc__tensor_vulkan_helpers_matrix_ops_c.o` from `build/libomni_chelpers.a` by deleting and rebuilding the archive with `scripts/build_omni_chelpers.sh`.
3. **Verified e2e tests pass** after the above fixes.
4. **Verified test suite passes** (`166 passed, 0 failed`).

---

## Recommendations (Prioritized)

1. **Fix C1 immediately** — Add null checks after `malloc` in `jit_compile_expr_core.c3` (3 sites).
2. **Harden `scripts/build_omni_chelpers.sh`** — Delete the archive before `ar rcs` to prevent stale member accumulation.
3. **Add a CI gate for AOT manifest parity** — Run `scripts/run_e2e.sh` or the source parity check on every commit.
4. **Fix all format string mismatches** — `%d` → `%zu` / `%ld` in the identified files.
5. **Remove `default:` from all `ValueTag` / `FfiTypeTag` switches** — Add missing arms and let the compiler enforce exhaustiveness.
6. **Break the `main` ↔ `lisp` module cycle** — Extract a `runtime_contracts` module.
7. **Audit FFI callback lifetime** — Ensure `Interp*` validity across C→Omni callback boundaries.
8. **Add `@require` to core functions** — Start with `eval.c3`, `eval_boundary_*.c3`, and `jit_*.c3`.
9. **Document ASAN platform limitation** — Note that aarch64 validation must run in an x86_64 container.
10. **Add bounds to string length helpers** — `repl_server_cstr_len`, `repl_cstr_len` need `max_len` parameters.
