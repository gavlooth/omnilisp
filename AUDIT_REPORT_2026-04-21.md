# Omni Lisp Codebase Audit Report
**Date:** 2026-04-21
**Scope:** `src/` (1195 C3 files), `csrc/` (172 C/H files), build config, tests
**Auditor:** Crush
**Mandate:** Ruthless. Find defects, bugs, code smells, and improvement vectors.

---

## Current Status Note (2026-04-21)

This report is a historical audit input, not the live backlog. The findings
were backfilled into `docs/todo_parts/todo_part_15.md`, where the audit and
memory-remediation items are tracked with closure evidence. Continue from
`TODO.md` and the part files, not from this report's original severity table.

---

## Executive Summary

The codebase shows signs of organic growth via incremental feature addition ("vibe coding"). While memory safety in the hot path is generally rigorous, there are **critical ABI fragilities**, **pervasive format string mismatches**, **heavy use of `default:` to hide non-exhaustive switches** on core enums, and a **structural module cycle** (`main` ↔ `lisp`) that undermines the module system. The most severe issue is hardcoded `struct addrinfo` field offsets in TCP networking code, which will break on non-x86_64 glibc platforms. The second most severe class of issues is the widespread discarding of error returns via `(void)`, including in interpreter initialization paths.

**Issue counts by severity:**
| Severity | Count | Description |
|----------|-------|-------------|
| 🔴 **Critical** | 2 | ABI fragility, dangling pointer storage |
| 🟠 **High** | 35+ | Format mismatches, error suppression, missing contracts, hidden switch defaults |
| 🟡 **Medium** | 25+ | Duplication, missing defer, foreach opportunities, global mutable state |
| 🟢 **Low** | 30+ | Naming inconsistencies, magic numbers, missing $assert, naming redundancy |

---

## 🔴 Critical (Fix Immediately)

### C1. Hardcoded `struct addrinfo` Offsets — ABI Fragility
**Files:**
- `src/lisp/async_tcp_transport_helpers.c3:53-58`
- `src/lisp/async_tcp_transport_helpers.c3:115-128`

**Problem:** The code manually indexes into `struct addrinfo` via raw pointer arithmetic with hardcoded x86_64 glibc offsets:
```
// struct addrinfo offsets (x86_64 glibc): flags=0, family=4, socktype=8, protocol=12, addrlen=16, addr=24
int ai_family = *((int*)((char*)result + 4));
int ai_socktype = *((int*)((char*)result + 8));
...
```

**Impact:** Will crash or return garbage on ARM64, 32-bit x86, musl libc, Windows, or any platform where `struct addrinfo` layout differs. This is not theoretical — musl and glibc have different padding.

**Fix:** Define a proper C3 struct mapping for `struct addrinfo` and let the compiler compute offsets, or use a C header with the real struct declaration.

---

### C2. Dangling `Value*` Stored in `Relation` Struct
**File:** `src/lisp/deduce_relation_row_materialization.c3:66-82`

**Problem:** `rel.column_key_values` is a raw `mem::malloc`'d `Value**` array whose elements are `boundary_alloc_value_in_root(interp)` values. The `Relation` struct outlives scopes. If the root scope is released while the `Relation` persists, the stored `Value*` pointers dangle, violating the Model A/B ownership rules in `AGENTS.md`.

**Impact:** Use-after-free in the Datalog relation layer when root scope is torn down.

**Fix:** Either refcount the Values, copy them into the Relation's own allocator, or pin the Relation to the root scope and enforce that invariant.

---

## 🟠 High (Fix in Next Sprint)

### H1. Pervasive `printf` Format String Mismatches
**Impact:** Silent truncation, undefined behavior, or incorrect output on platforms where `long`/`usz` differ from `int`.

| File | Line(s) | Format | Argument |
|------|---------|--------|----------|
| `src/lisp/eval.c3` | 107-109 | `%d` | `(long)` |
| `src/lisp/value_print_helpers.c3` | 216 | `%d` | `(long)` |
| `src/lisp/value_print_buf.c3` | 131 | `%d` | `(long)` |
| `src/lisp/tests_scheduler_groups.c3` | 54, 209, 230 | `%d` | `(long)` |
| `src/lisp/tests_memory_lifetime_env_copy_groups.c3` | 22 | `%d` | `(long)` |
| `src/lisp/eval_repl_server_state.c3` | 227 | `%d` | `(long)` |
| `src/scope_region_temp_pool_stats.c3` | 95-99 | `%d` | `(long)` |
| `src/entry_check_reporting.c3` | 108 | `%d` | `(int)` cast of `usz` |
| `src/lisp/value_constructors.c3` | 69 | `%d` | `usz max_len` |
| `src/lisp/scheduler_offload_ops.c3` | 84 | `%d` `%x` | `(long)unique_id` |

**Fix:** Use `"%ld"` for `long`, `"%zu"` for `usz`, and `"%d"` only for `int`. Audit all `io::printf`/`io::bprintf`/`io::eprintf` calls.

---

### H2. Non-Exhaustive `switch` on `ValueTag` with `default:` Hiding Missing Cases
**Policy violation:** `docs/C3_STYLE.md` says "Prefer exhaustive `switch` on tagged unions (for example `ValueTag`)" and "no default case for ValueTag switches."

| File | Line | Function | Missing Tags in `default:` |
|------|------|----------|---------------------------|
| `src/lisp/eval.c3` | 61 | `copy_to_parent_note_tag` | STRING, PRIMITIVE, FFI_HANDLE, TENSOR, CONTINUATION, PARTIAL_PRIM, COROUTINE |
| `src/lisp/eval_dispatch_types.c3` | 200 | `infer_value_type` | CONTINUATION, PARTIAL_PRIM, COROUTINE |
| `src/lisp/eval_dispatch_types.c3` | 240 | `value_type_name` | Same |
| `src/lisp/eval_env_copy_values.c3` | 62 | `copy_env_value_fast` | CONTINUATION, PARTIAL_PRIM, COROUTINE |
| `src/lisp/eval_boundary_provenance.c3` | 103 | `boundary_graph_alias_unsafe_for_reuse` | Many scalar tags |
| `src/lisp/eval_boundary_provenance.c3` | 400 | `boundary_nonunique_destination_retry_allowed` | ERROR, CONTINUATION, etc. |
| `src/lisp/eval_boundary_provenance_reachability.c3` | 87 | `boundary_alias_value_has_child_payload` | ERROR, PRIMITIVE, CONTINUATION |
| `src/lisp/prim_nn_checkpoint.c3` | 146 | `nn_ckpt_encode_value` | ~18 tag cases |
| `src/lisp/prim_tensor_capture.c3` | 43, 248 | `tensor_capture_count_nodes`, `tensor_capture_append_graph` | Tensor kind defaults |
| `src/lisp/jit_define_method_table.c3` | 7 | `jit_val_literals_equal` | ValueTag defaults |

**Risk:** Adding a new `ValueTag` variant will silently fall through `default:` instead of triggering a compile error. This is exactly why exhaustive switches are preferred — the compiler catches new variants.

**Fix:** Remove `default:` cases and add all missing tag arms. Let the compiler enforce completeness.

---

### H3. Error Returns Silently Discarded via `(void)`
**Policy:** Critical initialization paths must not ignore failure.

| File | Line | Call Ignored | Impact |
|------|------|--------------|--------|
| `src/lisp/value_interp_init_helpers.c3` | 101 | `(void)self.ensure_module_capacity(...)` | Interpreter init may segfault later |
| `src/lisp/value_interp_init_helpers.c3` | 137 | `(void)self.ensure_macro_capacity(...)` | Interpreter init may segfault later |
| `src/lisp/value_environment.c3` | 49, 56 | `(void)self.build_hash_table()` | Hash table silently broken after define |
| `src/lisp/value_constructors.c3` | 202 | `(void)ffi_handle_release_payload(box)` | Resource leak on handle destruction |
| `src/lisp/eval_repl_server_state.c3` | 99 | `(void)io_input_state_close(&session)` | Session cleanup failure ignored |
| `src/lisp/prim_tensor_construct.c3` | 302 | `(void)tensor_infer_fixed_complex_dtype_from_value(...)` | Falls back to wrong dtype silently |
| `src/lisp/async_tcp_transport_helpers.c3` | 74 | `(void)c_setsockopt(...)` | Socket option silently fails |

**Fix:** Return error values up the stack, or at minimum log the failure. In constructor/init paths, fail fast.

---

### H4. Missing `@require` Contracts on Core Functions
**Policy violation:** `docs/C3_STYLE.md` says "Use clear preconditions (`@require`) where state assumptions are non-obvious."

Hundreds of internal helpers in `src/lisp/` take `Interp*` or `Value*` without `@require`. Examples:

| File | Function | Missing Contract |
|------|----------|----------------|
| `src/lisp/prim_kernel_source.c3` | `kernel_has_source`, `kernel_source_key_allowed`, etc. | No `@require` on `kernel` or `interp` |
| `src/lisp/prim_nn_checkpoint.c3` | `nn_ckpt_string_is`, `nn_ckpt_obj`, `nn_ckpt_encode_array` | Missing `@require` on `value`, `interp` |
| `src/lisp/eval_boundary_provenance.c3` | `boundary_nonunique_destination_retry_allowed` | No `@require v != null` |
| `src/lisp/eval.c3` | `copy_to_parent_cons_spine_len`, `copy_to_parent_note_cons_spine` | No `@require v != null` |

**Fix:** Add `@require` for non-null pointers, valid tags, and valid state preconditions. Start with functions in `eval.c3` and `eval_boundary_*.c3`.

---

### H5. No Runtime Validation in `scope_splice_escapes`
**File:** `src/scope_region_reset_adopt.c3:57-68`

**Problem:** After moving ESCAPE chunks/dtors to the parent, the TEMP lane is destroyed with no validation that transferred ESCAPE values do not retain edges into TEMP memory. The code trusts caller-side classification (`boundary_commit_can_splice_escape_root`). A classification bug would produce dangling TEMP pointers inside ESCAPE values.

**Fix:** Add a debug-mode (or always-on) reachability scan before `scope_region_adopt_all_temp` to verify no ESCAPE root retains a TEMP edge. The boundary hardening scripts (`scripts/check_boundary_*.sh`) should catch this at CI time.

---

### H6. Unbounded `strlen`-like Scans
**Files:**
- `src/lisp/eval_repl_server_state.c3:70`
- `src/entry_runtime_project_paths.c3:9`

**Problem:** Both implement `repl_server_cstr_len` / `repl_cstr_len` as unbounded `while (text[len] != 0)` scans on raw `char*` with no `max_len`. If passed a non-null-terminated pointer, this reads past valid memory.

**Fix:** Accept a `max_len` parameter bounded by the known allocation size, or use C3's string slice length helper.

---

### H7. Force-Unwrap of Optional Without Check
**File:** `src/scope_region_global_guards.c3:23`

**Problem:** `g_scope_global_mu.init()!!;` — the only `!!` usage in the entire `src/` tree. If `init()` fails (e.g., on systems with limited mutex resources), this panics at runtime.

**Fix:** Handle the optional properly, or at minimum add `@checked` or a comment justifying why failure is impossible.

---

## 🟡 Medium (Fix When Touching Related Code)

### M1. Heavy Code Duplication

| What | Where | Description |
|------|-------|-------------|
| GELU constants | `prim_ml_activation.c3:114,116` + `prim_ml_autograd.c3:198-199` | `0.044715`, `0.7978845608028654` duplicated |
| Epsilon default | `prim_ml_optimizer.c3:599,604,650,656` + `prim_ml_optimizer_checkpoint.c3:42-58` | `0.00000001` repeated inline |
| Error constructors | `eval.c3:187-200` + `eval.c3:205-223` | `eval_error` / `eval_error_expr` ~90% identical |
| TCP/HTTP helpers | `async_tcp_transport_helpers.c3:142-148` + `http.c3:10-16` | Identical fiber-required / loop-unavailable raises |
| Tuple codecs | `deduce_db_rule_signature_record_codec.c3:46-71` + `deduce_tuple_codec.c3:23-85` | Size-calculation logic duplicated |

**Fix:** Extract shared constants into a `ml_constants.c3` or `math_constants.c3` module. Merge identical error constructors.

---

### M2. Missing `defer` on Complex Error Paths

**Files:**
- `src/lisp/value_tensor_clone.c3:13-221` — `tensor_clone_payload` mallocs multiple buffers. Every error path manually calls `tensor_free_payload()` (~15 sites). No `defer`; future edits will leak.
- `src/lisp/eval_promotion_copy_route_helpers.c3:67-133` — `copy_array_to_parent` manual free on multiple error paths.
- `src/lisp/eval_promotion_copy_route_helpers.c3:135-220` — `copy_hashmap_backed_to_parent` same.
- `src/lisp/eval_promotion_copy_route_helpers.c3:222-317` — `copy_method_table_to_parent` same.
- `src/lisp/eval_promotion_escape_structured.c3:53-109, 111-181, 183-263` — Escape promotion helpers with manual partial cleanup.

**Policy:** `docs/C3_STYLE.md` says "Pair resource acquisition with `defer` cleanup."

**Fix:** Refactor to acquire → `defer` release → use. For multi-resource functions, use a cleanup struct with a single defer.

---

### M3. Manual Index Loops That Should Be `foreach`

**Files (selection of ~20):**
- `src/lisp/prim_kernel_source.c3:55`
- `src/lisp/prim_tensor_capture.c3:79, 353`
- `src/lisp/prim_nn_checkpoint.c3:55, 212, 228, 334`
- `src/lisp/eval_env_copy_values.c3:147`
- `src/lisp/eval_boundary_provenance.c3:197, 217, 242`
- `src/lisp/eval_boundary_provenance_reachability.c3:14, 172, 183, 217, 244, 307`
- `src/lisp/jit_define_method_table.c3:29, 35`
- `src/lisp/value_print_buf.c3:17`

**Policy:** `docs/C3_STYLE.md` says "Prefer `foreach` over manual index loops when practical."

---

### M4. Global Mutable State Without Thread Safety

| File | Variable | Risk |
|------|----------|------|
| `src/lisp/scheduler_state_offload.c3:9` | `Scheduler g_scheduler;` | Global scheduler — concurrent access unsynchronized |
| `src/lisp/jit_compiler_state_pool.c3:17-30` | `g_jit_states[16384]`, `g_jit_state_count`, `g_jit_spill_states`, `g_jit_spill_count` | JIT state pool — multiple JIT threads could race |
| `src/lisp/jit_compiler.c3:52` | `ulong g_jit_compile_nonce_counter = 0;` | Not `tlocal`, unlike `g_jit_thread_marker` on the same line |
| `src/lisp/value_symbol_table.c3:5-9` | `g_symbol_table_force_*` | Test-only fault injection flags |
| `src/lisp/value_type_registry.c3:3-6` | `g_type_registry_force_*` | Same |
| `src/lisp/value_interp_lifecycle.c3:7-16` | `g_interp_force_*` | Same |

**Fix:** Document which globals are single-threaded by design. Make `g_jit_compile_nonce_counter` `tlocal`. Protect scheduler with mutex or document that it must be initialized once.

---

### M5. Structural Module Cycle: `main` ↔ `lisp`

**Stats:** 416 files in `src/lisp/` import `main`; 21 files in `src/main*.c3` / `src/entry*.c3` import `lisp`.

**Problem:** This is a module-level cycle. In C3, cyclic imports can cause compilation ordering issues, complicate incremental builds, and indicate an architecture where responsibilities are not clearly separated. The `main` module should be the entry point that imports `lisp`, not vice versa.

**Fix:** Extract a shared `runtime_common` or `runtime_contracts` module containing the types both sides need. `lisp` should never import `main`.

---

### M6. Build Config Issues

**File:** `project.json`

1. `c-include-dirs` omits `third_party/ftxui/include` despite `csrc/ftxui_shim.cpp` including headers from that path.
2. Links pre-built static libs `omni_chelpers` and `omni_ftxui` with no build rule in `project.json`; requires external `scripts/build_omni_chelpers.sh`.
3. Hardcodes `/usr/local/lib` in `linker-search-paths`.
4. Only defines a `main` target; no `test` target, so all test code is compiled into the main binary unconditionally, bloating release builds.

---

### M7. Dead Code / Always-False Test Flags

**Files:**
- `src/lisp/value_symbol_table.c3:5-9` — `g_symbol_table_force_entries_alloc_fail`, etc. are always `false` in release builds.
- `src/lisp/value_type_registry.c3:3-6` — Same pattern.
- `src/lisp/value_interp_lifecycle.c3:7-16` — Same pattern.

These are test-only fault-injection globals compiled into every build. They are dead weight in release.

**Fix:** Wrap them in `$if DEBUG_BUILD:` or move to a test-only module.

---

## 🟢 Low (Code Hygiene)

### L1. Raw `0xFFFFFFFF` Instead of Named Constant

8 files use `(uint)expr.lambda.param != 0xFFFFFFFF` or similar, while `src/lisp/value_runtime_constants.c3:3` already defines `INVALID_SYMBOL_ID = (SymbolId)0xFFFFFFFF`. Occurrences:
- `compiler_code_emission.c3:17`
- `compiler_expr_serialize_callable_forms.c3:11`
- `compiler_free_vars_scope_forms.c3:54`
- `compiler_lambda_scan_lambda_defs.c3:12,34`
- `compiler_mutable_capture_detection_walk.c3:55`
- `jit_closure_support.c3:219`

**Fix:** Replace all raw usages with `INVALID_SYMBOL_ID`.

---

### L2. Redundant `jit_` File Prefixes

**Files:**
- `src/lisp/jit_compile_expr_dispatch.c3`
- `src/lisp/jit_closure_support.c3`
- `src/lisp/jit_apply_runtime.c3`
- `src/lisp/jit_compiler.c3`
- `src/lisp/jit_compiler_compile.c3`
- ... and many more

The prefix `jit_` appears twice. These should be renamed to `jit_compile_expr_dispatch.c3`, etc.

---

### L3. Missing `$assert` for Struct Sizes

No `$assert` found anywhere in `src/` for critical runtime struct sizes. The `Value`, `Closure`, `TensorVal`, `HashMap`, `Array`, `Primitive`, `SymbolTable`, `Env`, `Interp`, and `MethodTable` structs have no compile-time size validation, despite being interop-critical.

**Fix:** Add `$assert(Value.sizeof == EXPECTED_VALUE_SIZE)` after each struct definition.

---

### L4. Hardcoded Magic Numbers (Selected)

| File | Line | Number | Meaning |
|------|------|--------|---------|
| `prim_math.c3` | 22 | `0x000F_FFFF_FFFF_FFFF` | Double mantissa mask |
| `prim_math.c3` | 23 | `4503599627370496.0` | `2^52` |
| `prim_nn_init.c3` | 23 | `1664525`, `1013904223` | LCG multiplier / increment |
| `prim_nn_init.c3` | 28 | `4294967296.0` | `2^32` |
| `stack_engine_backend_contract.c3` | 53 | `0x1F80` | SSE MXCSR default |
| `stack_engine_backend_contract.c3` | 54 | `0x037F` | x87 control word default |
| `prim_collection_hashmap_key_helpers.c3` | 108 | `0x8000` | Timezone offset bias |
| `async_tcp_transport_helpers.c3` | 98 | `4096` | TCP read default max |
| `async_tcp_transport_helpers.c3` | 104 | `65536` | TCP read hard cap |
| `http.c3` | 57 | `4096` | HTTP request buf size |
| `http.c3` | 69 | `65536` | HTTP response buf size |
| `http.c3` | 73 | `65000` | HTTP read size |

---

### L5. `$if DEBUG_BUILD:` Blocks May Hide Bugs in Release

- `src/lisp/jit_compiler_compile.c3:11` — JIT pool pressure logging.
- `src/lisp/runtime_backend_hooks_cache.c3:87,92,102,107` — Cache diagnostics.

These blocks are fine as diagnostics, but verify that no side-effecting operations (e.g., state mutations) are hidden inside them.

---

## Recommendations (Prioritized)

1. **Fix C1 (addrinfo ABI fragility) immediately.** This is a portability ticking time bomb.
2. **Audit all `(void)` casts on function returns.** Add a CI check or lint rule: `(void)` on non-void returns requires a comment or is flagged.
3. **Remove all `default:` cases from `ValueTag` switches.** Add the missing arms and let the compiler enforce exhaustiveness. This is the single biggest correctness win.
4. **Break the `main` ↔ `lisp` module cycle.** Extract shared types into a `runtime_contracts` module.
5. **Add `defer` to all multi-resource allocation functions.** Start with `tensor_clone_payload` and the promotion copy helpers.
6. **Add a CI gate for format string mismatches.** C3 may not have `-Wformat`, but a regex-based check could catch `%d` + `long`/`usz`.
7. **Document the global mutable state contract.** Which globals are single-threaded? Which need synchronization?
8. **Add `$assert` for struct sizes** on all interop-critical structs.
9. **Extract shared constants** (GELU, epsilon, LCG params) into a single module.
10. **Add a `test` target to `project.json`.** Stop compiling test code into release binaries.
