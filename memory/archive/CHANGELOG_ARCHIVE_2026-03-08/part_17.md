- **Global generation counter needed**: Initial implementation used per-scope `generation` field (incremented on recycle). Multiple scopes could share the same generation number (e.g., malloc'd scope starts at 0, first recycled scope is 1). Caused false skips in copy_to_parent → segfault on fib test. Fixed with `g_scope_generation_counter` monotonic global.

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/value.c3` | `scope_gen` field, stamp in alloc, O(1) make_cons check |
| `src/lisp/eval.c3` | Comment update in copy_to_parent (kept is_in_scope) |
| `src/scope_region.c3` | `g_scope_generation_counter`, assigned in `scope_create` |
| `memory/ESCAPE_SCOPE_EXPLORATIONS.md` | New file — 5 optimization explorations |

### Test results
943 unified + 73 compiler + 9 stack + 40 scope = **1065 PASS, 0 failures** (unchanged)

---

## 2026-03-01 (Session 62): Three-Tier Escape-Scope Optimization

### Summary
Implemented escape-scope optimization to reduce O(n²) copying in TCO loops with accumulator patterns (reverse, map, filter) to O(n). Introduces a three-tier scope hierarchy: parent_scope → result_scope → call_scope. Accumulator cons cells are automatically detected and allocated in a persistent result_scope that survives TCO bounces, eliminating redundant deep copies.

### Changes
- **Phase 1: Scope-aware `copy_to_parent`** — Added `is_in_scope()` helper and `releasing_scope` field to Interp. `copy_to_parent` skips values not in the dying scope (already in a surviving scope).
- **Phase 2: Result scope + escape-aware cons** — `jit_eval_in_call_scope` creates two-scope hierarchy (result_scope + call_scope). `make_cons` detects accumulator pattern (cdr is NIL or in escape_scope) and redirects allocation to result_scope via `make_cons_escape`. Critical fix: `copy_to_parent` saves/nulls `escape_scope` to prevent copy operations from triggering escape path.
- **Phase 3: Context boundary save/restore** — `escape_scope` saved/restored at all 6 StackCtx boundary sites (reset, shift, apply_continuation, signal, handle, resolve).
- **Phase 4: Scope adoption** — Added `scope_adopt()` for O(1) merge of child scope chunks/dtors into parent when refcount == 1. Used in `jit_eval_in_call_scope` step 1 (active_scope → result_scope).
- **Phase 5: Tests** — 15 new escape-scope functional tests + 10 new scope region unit tests.

### Key bugs fixed during implementation
1. Two-step promotion needed: result can be in active_scope OR result_scope, requiring active→result→parent promotion
2. `copy_to_parent` must disable `escape_scope` to prevent internal `make_cons` calls from triggering escape path during copy operations

### Files modified
| File | Changes |
|------|---------|
| `src/scope_region.c3` | `is_in_scope()`, `scope_adopt()`, 10 unit tests |
| `src/lisp/value.c3` | `releasing_scope`/`escape_scope` fields, `make_cons_escape`, modified `make_cons` |
| `src/lisp/eval.c3` | Scope-aware skip check + escape_scope save/null/restore in `copy_to_parent` |
| `src/lisp/jit.c3` | Two-scope `jit_eval_in_call_scope`, TCO trampoline update, 6 context boundary save/restores |
| `src/lisp/tests.c3` | `run_escape_scope_tests()` with 15 functional tests |

### Test results
943 unified + 73 compiler + 9 stack + 40 scope = **1065 PASS, 0 failures** (was 1040)

---

## 2026-03-01 (Session 61): AOT Deferred Items D1+D2+D3

### Summary
Implemented the three deferred optimizations from the AOT unification plan. Found D1 (fast path) and D3 (TCO trampoline) were already implemented in prior sessions. Implemented D2 (compile-time primitive caching) which eliminates per-reference `aot::lookup_prim()` hash lookups by caching primitives in global variables initialized once in `main()`.

### Changes
- **D2: Compile-time primitive caching** (`src/lisp/compiler.c3`):
  - Added `referenced_prims` list to `Compiler` struct for tracking used primitives
  - Added `emit_prim_global_name()` and `record_prim_ref()` helpers
  - Two-pass compilation: compile code to temp buffer first (discovers all prims), then assemble final output with proper global declarations and init code
  - `compile_var` now emits `_prim_xxx` global reference instead of `aot::lookup_prim("xxx")` for primitives; literals (true/false/nil) stay inline
  - `emit_global_declarations` now also emits cached primitive globals
  - Prim init code inserted in `main()` after `aot_init()`
  - Result: 81 runtime hash lookups eliminated for hello.lisp, replaced with global variable reads

- **D1: AOT closure fast path** (`src/lisp/aot.c3`): Already implemented — `invoke()` and `apply_multi()` check for `aot_closure_apply`/`aot_variadic_apply` function pointers to bypass JIT apply indirection

- **D3: AOT TCO trampoline** (`src/lisp/aot.c3` + `compiler.c3`): Already implemented — compiler emits `invoke_tail`/`apply_multi_tail` in tail position, `invoke`/`apply_multi` have `while (g_tail_pending)` trampoline loops. Verified: 1M-deep recursion works without stack overflow.

### E2E verification
- factorial(10) = 3628800
- closure(make-adder 5 10) = 15
- TCO sum-to(1M) = 500000500000
- TCO loop-test(1M) = 0 (no stack overflow)

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/compiler.c3` | D2: referenced_prims tracking, two-pass compilation, prim globals |

### Test results
928 unified + 73 compiler + 9 stack + 30 scope = **1040 PASS, 0 failures**

---

## 2026-02-28 (Session 60): AOT Compiler Unification Cleanup

### Summary
Cleaned up remaining issues in the AOT compiler unification (bulk work done in prior sessions). Removed `home_region` from Lambda structs in generated code, removed debug print from `compiled_signal`, fixed `make_false()` to return the false symbol instead of nil, and updated stale comments in `compiler.c3`.

### Changes
- **`src/lisp/aot.c3`**: Removed debug `io::printfn` from `compiled_signal`. Fixed `make_false()` to return `sym_false` instead of nil.
- **`src/lisp/compiler.c3`**: Removed `home_region` field from emitted Lambda structs (dead field, wasted 16 bytes per closure). Updated doc comment to describe AOT→aot:: architecture instead of old region-based approach. Fixed stale comment references to `rt_print_value`.

### E2E verification
- `c3c build` succeeds
- All tests pass: 928 unified + 73 compiler + 9 stack + 30 scope = 1040 PASS
- AOT builds work: factorial, closures, effects, let, named let loops
- Generated code contains zero `runtime::` references, zero `home_region` references

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/aot.c3` | Remove debug print, fix make_false |
| `src/lisp/compiler.c3` | Remove home_region from Lambda structs, update comments |

### Test results
928 unified + 73 compiler + 9 stack + 30 scope = **1040 PASS, 0 failures**

---

## 2026-02-28 (Session 59): Fiber→Coroutine Rename + Engine→StackCtx Rename

### Summary
Comprehensive rename across the entire codebase: user-facing `fiber` type renamed to `coroutine`, internal engine `Coro` type renamed to `StackCtx`. Eliminates naming confusion between the three layers: engine (StackCtx), continuations (CONTINUATION), and user-level coroutines (COROUTINE). All abbreviations of "coro" eliminated for readability.

### User-facing changes (breaking)
- `(fiber thunk)` → `(coroutine thunk)` — create a coroutine
- `(fiber? v)` → `(coroutine? v)` — test if value is a coroutine
- FIBER ValueTag → COROUTINE ValueTag
- `fiber_val` field → `coroutine_val` field on Value struct

### Engine rename (internal)
- `Coro` struct → `StackCtx` — all function names, enum values, globals
- `CoroStatus` → `StackCtxStatus`, `CORO_READY/RUNNING/SUSPENDED/COMPLETED/DEAD` → `CTX_*`
- `coro_create/init/switch_to/resume/suspend/destroy/clone` → `stack_ctx_*`
- `g_current_coro` → `g_current_stack_ctx`
- `Continuation.coro` field → `Continuation.ctx`
- `StackPool` operates on `StackCtx*` (type rename only, no behavioral change)

### Effect system readability renames
- `HandleFiberState` → `HandleEffectState`
- `ResetFiberState` → `ResetState`
- `fiber_state` → `effect_state` (Interp field)
- `is_fiber_based` → `is_coroutine_based` (Continuation field)
- `jit_reset_impl_fiber` → `jit_reset_impl` (drop suffix)
- `jit_shift_impl_fiber` → `jit_shift_impl`
- `jit_handle_impl_fiber` → `jit_handle_impl`
- `jit_signal_impl_fiber` → `jit_signal_impl`
- `jit_apply_fiber_continuation` → `jit_apply_continuation_impl`
- `handle_fiber_entry` → `handle_effect_entry`
- `FiberThunkState` → `CoroutineThunkState`
- `fiber_thunk_entry` → `coroutine_thunk_entry`

### Garbled name fixes
- `StackCtxutineThunkState` → `CoroutineThunkState` (from Coro→StackCtx replacing inside "Coroutine")
- `StackCtxutines` → "Coroutines" in comments (same cause)
- `StackCtxutine to initialize` → "Stack context to initialize" in doc comments

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/value.c3` | COROUTINE tag, coroutine_val field, make_coroutine param, Continuation.ctx, comments |
| `src/lisp/eval.c3` | intern("Coroutine"), coroutine? prim registration |
| `src/lisp/primitives.c3` | prim_coroutine, prim_coroutine_p, CoroutineThunkState, local var coro→ctx |
| `src/lisp/jit.c3` | HandleEffectState, ResetState, all effect impl functions, local var coro→ctx, resolve function |
| `src/stack_engine.c3` | StackCtx, CTX_* enums, stack_ctx_* functions, g_current_stack_ctx, comments |
| `src/lisp/tests.c3` | Test names + Lisp code: fiber→coroutine, fiber?→coroutine? |
| `docs/FEATURES.md` | Coroutine primitives section |
| `docs/LANGUAGE_SPEC.md` | Coroutine type row |

### Test results
928 unified + 73 compiler + 9 stack + 30 scope = **1040 PASS, 0 failures**

---

## 2026-02-28 (Session 58): Remove Legacy FFI, Documentation Update, Version Bump

### Summary
Removed FFI primitives (`ffi-open`, `ffi-call`, `ffi-close`, `ffi-sym`) — breaking change per pre-1.0 policy. The declarative FFI (`define [ffi lib]` / `define [ffi λ]`) is now the only FFI interface. Also updated docs to reflect scope-region memory system, dynamic limits, fibers, and new FFI. Version bumped to 0.1.5 / spec 0.4.5.

### Legacy FFI removal

**Removed from `primitives.c3`** (~290 lines):
- `prim_ffi_open`, `prim_ffi_call`, `prim_ffi_close`, `prim_ffi_sym`
- Helper functions: `ffi_is_double_type`, `ffi_value_to_double`, `ffi_value_to_long`, `ffi_long_to_value`
- All function pointer type aliases removed from `eval.c3`: `FfiFn0-6`, `FfiFnD0-3`, `FfiFnID0-2`, `FfiFnDI1-2` (~22 lines)

**Removed from `eval.c3`**:
- 4 primitive registrations from `register_primitives` (array size 101 → 97)

**Removed from `compiler.c3`**:
- 4 `prim_hash_insert` entries for `rt_ffi_open/close/sym/call`
- Removed from both `known_prims` lists

**Removed from `value.c3`**:
- `FfiHandle.sym_cache_ptrs`, `sym_cache_names`, `sym_cache_count` — dlsym cache only used by `prim_ffi_call`

**Removed 15 tests** (`tests.c3`):
- 11 unified FFI tests (ffi-open, ffi-call strlen/abs/getpid/atoi/sqrt/pow, ffi-sym, ffi-close, ffi-open error, cached call)
- 4 compiler tests (ffi-open, ffi-close, ffi-sym, ffi-call compilation)

**Version bump**: 0.1.0 → 0.1.5 (entry.c3); spec 0.4.0 → 0.4.5 (LANGUAGE_SPEC.md)

### Documentation update

---

## 2026-02-28 (Session 58 cont.): Documentation Update — Scope-Region Memory System

### Summary
Updated FEATURES.md and LANGUAGE_SPEC.md to reflect the new scope-region memory architecture, dynamic AST limits, fiber-based continuations, declarative FFI syntax, and fiber primitives.

### Key changes
- **Memory section rewritten** (FEATURES.md §8): "Region-based allocation" → "Arena-per-call with RC escape hatch" — documents scope regions, deterministic release, refcounted closures, TCO scope recycling, root scope, AST allocation
- **Dynamic limits**: Removed stale fixed limits for match clauses (was 128), effect handler clauses (was 64), call arguments (was 64), begin expressions (was 64), lambda params (was 64), pattern elements (was 16). All now "Dynamic (no fixed limit)"
- **Continuations section updated** (§6.1): Documents fiber-based implementation (mmap'd stacks, assembly context switch, multi-shot via coro_clone, FPU isolation, guard page overflow detection)
- **FFI section updated** (§5.10): Added declarative FFI syntax (`define [ffi lib]` / `define [ffi λ]`) as recommended approach; `ffi-open`/`ffi-call` marked as "still supported"
- **Fiber primitives added** (§5.11): `fiber`, `resume`, `yield`, `fiber?` with usage example
- **Fiber data type added** to type tables in both docs
- **String literal limit documented**: 63-byte inline limit (Token.text[64] lexer limit) now explicit in limits tables
- **JIT section updated** (§10): "Per-eval temp regions" → "Per-eval scope" + "Per-call scope"
- **LANGUAGE_SPEC.md intro updated**: "region-based memory system" → "scope-region memory system (arena-per-call with reference-counted closures)"

### Files modified
| File | Changes |
|------|---------|
| `docs/FEATURES.md` | ~15 edits: memory section rewrite, limits table, continuations, FFI, fibers, dates |
| `docs/LANGUAGE_SPEC.md` | 5 edits: date, intro, match clauses, limits table, fiber type |

### Test count
No code changes — 1055 tests unchanged.

---

## 2026-02-28 (Session 57): Phase 7 — Remove Old Region System from Interpreter Path

### Summary
Removed the old region system (`current_frame`, `create_region`/`release_region`) from the interpreter path. The scope-region system now fully manages Value/Env memory. Old region infrastructure retained only for Expr/Pattern allocation (`root_region`) and the AOT compiler runtime (`runtime.c3`/`compiler.c3`).

### Key changes

**Removed `target` parameter from `copy_to_parent`** (`eval.c3`):
- Function signature simplified from `(Value*, Interp*, RegionHandle)` to `(Value*, Interp*)`
- All internal `current_frame` save/restore logic removed
- 13 external call sites updated across eval.c3, jit.c3, primitives.c3

**Removed `current_frame` lifecycle from `run()` and REPL** (`eval.c3`):
- `run()`: removed `create_region`/`release_region` — only `scope_create`/`scope_release` remains
- REPL: same cleanup — child scope is the only memory boundary per REPL line

**Removed dual guards** (`jit.c3`):
- `jit_eval_set`, `jit_eval_define`, `jit_make_closure_from_expr`: simplified from `(current_scope != root_scope || current_frame != root_region)` to `(current_scope != root_scope)`

**Removed `current_frame` field from Interp** (`value.c3`):
- Deleted `main::RegionHandle current_frame` field and its initialization
- Simplified `make_ffi_handle`, `make_array`, `make_module`, `make_hashmap` (no more `saved_frame`)
- Simplified `make_instance` and `register_dispatched_prim` in eval.c3
- Simplified `jit_env_extend_root` and 3 method table creation sites in jit.c3

**Removed old region-system destructors** (`eval.c3`):
- Deleted `destroy_value`, `destroy_hashmap`, `destroy_env` functions (~95 lines)
- Deleted `register_destructors()` function and all call sites (entry.c3, tests.c3)

**Cleaned up `scope_region.c3`**:
- Removed `ScopeHandle` struct and associated functions (scope_handle_from, scope_handle_valid, scope_handle_deref)
- Removed `SCOPE_HANDLE_NULL` constant and `SCOPE_SPLIT_THRESHOLD` constant
- Removed `capture_scope_for_closure` stub
- Added `assert(refcount == 1)` guard to `scope_reset`
- Removed 5 ScopeHandle unit tests (Tests 7-8)

**Removed test calls from entry.c3**:
- Removed `run_destructor_registry_tests()` and `run_ghost_lookup_tests()` calls
- Removed child region creation test code

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/eval.c3` | Simplified copy_to_parent (removed target param, -95 lines old dtors), removed region lifecycle from run/REPL |
| `src/lisp/value.c3` | Removed current_frame field, simplified make_ffi_handle/make_array/make_module |
| `src/lisp/jit.c3` | Removed dual guards, simplified method table creation, jit_env_extend_root |
| `src/lisp/primitives.c3` | Simplified make_hashmap, updated copy_to_parent call |
| `src/scope_region.c3` | Removed ScopeHandle API, capture_scope_for_closure, added scope_reset assert |
| `src/entry.c3` | Removed register_destructors calls, test function calls, child region test |
| `src/lisp/tests.c3` | Removed register_destructors calls |

### Test counts
- Before: 939 unified + 77 compiler + 9 stack + 35 scope = 1060 PASS
- After: 939 unified + 77 compiler + 9 stack + 30 scope = 1055 PASS (5 ScopeHandle tests removed)
- 0 failures

### Notes
- main.c3 infrastructure (GhostTable, DestructorRegistry, create_region, write_barrier, etc.) retained for AOT compiler runtime (runtime.c3/compiler.c3)
- Expr/Pattern allocation still uses root_region via old Pool/SlotTable system

---

## 2026-02-28 (Session 56): Phase 4b — Closure Lifecycle Management

### Summary
Implemented Phase 4b of the scope-region memory system: refcounted closures with per-closure env_scope. Closures are now allocated in `current_scope` instead of `root_scope`, enabling temporary closures (anonymous lambdas passed to HOFs) to be freed when no longer referenced. Closures that escape (via `define`, `copy_to_parent`) share the same `Closure*` struct via refcounting — freed when the last reference's dtor runs.

### Key changes

**Refcounted Closure struct** (`value.c3:232`):
- Added `refcount` (uint) and `env_scope` (ScopeRegion*) fields to Closure
- `make_closure`/`make_closure_no_param` initialize refcount=1, env_scope=null
- `scope_dtor_value` CLOSURE case: decrements refcount, frees Closure + env_scope only when refcount reaches 0

**copy_to_parent CLOSURE case** (`eval.c3:~1226`):
- Changed from `result = v` (returning original pointer) to refcount sharing: allocates new Value wrapper in parent scope, shares Closure* pointer, bumps refcount, registers dtor

**copy_env_to_scope** (`eval.c3:~1305`, replaces deep_copy_env):
- Same structure as deep_copy_env but does NOT switch to root_scope/root_region
- Caller manages current_scope (set to env_scope before calling)
- Uses copy_to_parent for binding values — CLOSURE values handled via refcount sharing

**jit_make_closure_from_expr rewrite** (`jit.c3:~475`):
- Removed root_scope/root_region switch — closures allocated in current_scope
- Creates standalone env_scope (`scope_create(null)`) when not in root_scope
- Copies env chain into env_scope via `copy_env_to_scope`
- No parent pointer on env_scope → no RC cycles, TCO recycling unaffected

**jit_eval_let_rec weak self-reference** (`jit.c3:~566`):
- For non-root closures: creates self-ref Value in env_scope with NO refcount bump, NO dtor registration
- Breaks closure→env→self→closure RC cycle
- Root-scope closures: direct assignment (unchanged)

**copy_tco_env_chain** (`jit.c3:~391`):
- Changed to copy FULL env chain to global_env (was stopping at closure_env)
- Added persistent env handling — persistent nodes returned as-is
- Prevents use-after-free when env_scope is freed during scope_release

**copy_to_parent ITERATOR case** (`eval.c3:~1260`):
- Now promotes underlying thunk closure via recursive copy_to_parent call
- Prevents dangling iterator_val when thunk's scope releases

**prim_fiber thunk promotion** (`primitives.c3:~2837`):
- Added `promote_to_root(thunk, interp)` before storing in FiberThunkState
- Prevents dangling thunk pointer when creating scope releases

**Cleanup — removed dead jit_tco_closure_env field**:
- Deleted field from Interp struct, init, and 3 setter sites in tail-call helpers
- No longer needed: copy_tco_env_chain copies full chain to global_env

### Bugs found and fixed
1. **SIGSEGV after "repeated eval 100x"**: TCO scope recycling freed closure's env_scope while env chain still referenced it. Fix: copy_tco_env_chain copies full chain.
2. **Iterator test failures**: copy_to_parent ITERATOR shallow-copied iterator_val (CLOSURE*) that could be in child scope. Fix: promote thunk via recursive copy_to_parent.
3. **SIGSEGV after "fiber multi yield"**: prim_fiber stored thunk Value* in FiberThunkState; scope release freed it. Fix: promote_to_root before storing.

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/value.c3` | Closure gains refcount+env_scope; init in make_closure/make_closure_no_param; scope_dtor_value refcount logic; removed jit_tco_closure_env from Interp |
| `src/lisp/eval.c3` | copy_to_parent CLOSURE (refcount sharing) + ITERATOR (thunk promotion); copy_env_to_scope replaces deep_copy_env |
| `src/lisp/jit.c3` | jit_make_closure_from_expr rewrite; jit_eval_let_rec weak self-ref; copy_tco_env_chain full chain; removed jit_tco_closure_env setters |
| `src/lisp/primitives.c3` | prim_fiber: promote_to_root thunk |
| `src/lisp/tests.c3` | 11 new closure lifecycle tests (run_closure_lifecycle_tests) |

### Test count
- Before: 1049 (928 unified + 77 compiler + 9 stack + 35 scope)
- After: 1060 (939 unified + 77 compiler + 9 stack + 35 scope)
- All pass, 0 failures

---

## 2026-02-28 (Session 55): Phase 5 — TCO Scope Recycling

### Summary
Implemented Phase 5 of the scope-region memory system: TCO scope recycling. At each tail-call bounce in `jit_eval`'s trampoline, if the current scope has RC=1 (nothing escaped), a fresh scope is created, the env chain is copied into it, and the old scope is released. This frees all body temporaries per loop iteration, turning O(iterations × alloc_per_iter) memory into O(chunk_size) — flat regardless of iteration count.

### Key changes

**TCO recycling in jit_eval trampoline** (`jit.c3:328-345`):
- Added `jit_tco_closure_env` to Interp — tracks the closure's env (stop point for env chain copy)
- Set `jit_tco_closure_env` at 5 tail-call helper sites (`jit_apply_value_tail`, 4 sites in `jit_apply_multi_args_tail`)
- When TCO bounce fires: if `cs == tco_recycle_scope && cs.refcount == 1 && env != closure_env`, creates fresh scope, copies env via `copy_tco_env_chain`, releases old scope

**copy_tco_env_chain helper** (`jit.c3:388-408`):
- Copies env frames from `src` up to (but not including) `stop_at` into `current_scope`
- Recursive bottom-up copy: parent chain first, then current frame
- Uses `copy_to_parent` for binding values and `make_env` for frame allocation

**Scope ownership tracking** (`jit.c3:359-386`):
- `jit_eval_in_call_scope` sets `tco_recycle_scope` to its call scope, saves/restores previous value
- `tco_recycle_scope` field added to Interp, saved/restored at all 6 coro boundary sites
- After `jit_eval` returns, captures `active_scope = interp.current_scope` (may differ from original call_scope due to recycling)

**Critical fix: nested named-let scope isolation** (`jit.c3:604`):
- Changed `jit_eval_let_rec` to evaluate body via `jit_eval_in_call_scope` instead of bare `jit_eval`
- **Root cause**: nested named-lets (e.g., `flatten`'s `loop` calling `loop2`) shared the same scope. When the inner loop's `jit_eval` saw `tco_recycle_scope` pointing to the outer loop's scope with RC=1, it recycled it — destroying the outer loop's env bindings (use-after-free → SIGSEGV)
- **Fix**: each named-let body gets its own scope via `jit_eval_in_call_scope`, which sets `tco_recycle_scope` independently. Nested loops recycle their own scopes without interfering.

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/value.c3` | Added `jit_tco_closure_env` and `tco_recycle_scope` fields to Interp, init to null |
| `src/lisp/jit.c3` | `copy_tco_env_chain` helper; TCO recycling in trampoline; `jit_eval_in_call_scope` scope ownership; `tco_recycle_scope` at 5 tail-call sites + 6 coro boundaries; `jit_eval_let_rec` → `jit_eval_in_call_scope` for body |
| `src/lisp/tests.c3` | 10 TCO recycling tests: named-let sum, closure escape, string accumulation, cons args, multi-param, variadic, mutual tail calls, zero-arg, 100K iterations, effects in loop |

### Test count
928 unified + 77 compiler + 9 stack + 35 scope = **1049 PASS, 0 failures** (was 1039)

---

## 2026-02-28 (Session 54): Phase 4c — Audit Fixes + Fiber Yield Use-After-Free

### Summary
Implemented Phase 4c audit fixes (C1 set! path mutation, P1 conditional dtor registration) and discovered/fixed a critical use-after-free bug in fiber yield values.

### C1: Fix set! path mutation dangling pointer
- Added `promote_to_root(value, interp)` in `jit_eval_set_path` before storing values into cons cells/instance fields
- Mirrors the pattern already used in `jit_eval_set`

### P1: Conditional dtor registration — only for heap-backed types
- Removed `scope_register_dtor` from `alloc_value()` — flat types (NIL, INT, DOUBLE, SYMBOL, CONS, BOOL) no longer register destructors (~90% of allocations)
- Moved dtor registration to heap-backed constructors: `make_string`, `make_closure`, `make_closure_no_param`, `make_primitive`, `make_ffi_handle`, `make_error`, `make_array`, `make_hashmap`, `make_instance`, METHOD_TABLE inline sites
- `alloc_value_root()` keeps unconditional dtor (root-scope dtors only run at exit, no perf impact)

### C4: FIBER/CONTINUATION dtor cases (partial — deferred)
- Added FIBER and CONTINUATION cases to `scope_dtor_value` but do NOT register dtors for these types
- Fiber coro lifecycle is managed by fiber primitives (resume/yield), not scope dtors
- Registering dtors crashed because active coros were destroyed during scope release
- Abandoned fibers leak their stack (~64KB) until program exit — acceptable for now
- Deferred to future work (track active fibers in interp for cleanup)

### Critical bug fix: Fiber yield value use-after-free
- **Root cause**: `prim_resume` returned `interp.yield_value` directly — a raw pointer into the fiber's call_scope. When the fiber completed, `jit_eval_in_call_scope` released the call_scope, freeing all previously yielded values. The caller's held pointers became dangling.
- **Symptom**: Freed memory was reused by CONS cell allocations for `+` arg lists, causing "+: expected number argument" errors.
- **Why it was latent**: Before P1, unconditional dtor registration added 24-byte ScopeDtor padding between Values, changing memory layout enough to avoid the reuse. P1 removed the padding, making Values pack tighter and triggering the reuse.
- **Fix**: `prim_resume` now copies yield values to the caller's scope via `copy_to_parent` before returning. Completion values were already safe (copy_to_parent'd by `jit_eval_in_call_scope`).

### Files modified
| File | Changes |
|------|---------|
| `src/lisp/jit.c3` | C1: promote_to_root in jit_eval_set_path; METHOD_TABLE dtor registration (3 sites) |
| `src/lisp/value.c3` | P1: alloc_value dtor removal; heap-backed constructor dtors; g_coro_pool global; FIBER/CONTINUATION dtor cases (kept but not registered) |
| `src/lisp/eval.c3` | make_instance dtor; METHOD_TABLE dtor |
| `src/lisp/primitives.c3` | make_hashmap dtor; prim_resume yield value copy_to_parent; prim_add reverted debug |

### Test count: 918 unified + 77 compiler + 9 stack + 35 scope = 1039 PASS, 0 failures

---

## 2026-02-28 (Session 53): Scope-Region Phase 4a — Scope-per-lambda-call

### Summary
Implemented scope-per-lambda-call: each closure body evaluation now gets its own child scope. Intermediate Values (CONS cells, format buffers, partial results) are freed when the function call returns, instead of accumulating in the run() scope for the entire evaluation.

### New helper: `jit_eval_in_call_scope`
- Creates a child scope, evaluates closure body, promotes result via `copy_to_parent`, releases child scope
- Same pattern as `run()` in eval.c3 but at per-call granularity
- TCO bounces within jit_eval's trampoline loop all execute in the same call_scope

### 8 call sites replaced: `jit_eval` → `jit_eval_in_call_scope`
- `jit_apply_value_impl`: zero-arg, variadic (pc=0), variadic (pc=1), single-param
- `jit_apply_multi_args`: zero-arg, variadic, multi-param strict arity
- `jit_apply_multi_args_tail`: extra-args non-tail body eval

### Critical bug fix: deep_copy_env guard conditions
The old deep_copy_env guards only checked `current_frame` (old region system) to decide whether closures needed env deep-copying. Since `jit_eval_in_call_scope` creates child scopes but not child frames, closures created inside call scopes had dangling env pointers after scope release. Fixed 4 guard sites to also check `current_scope != root_scope`:
- `jit_make_closure_from_expr` (zero-arg path + regular path)
- `jit_eval_let_rec`
- `jit_eval_set`
- `jit_eval_define`

### Files modified
- `src/lisp/jit.c3` — Added `jit_eval_in_call_scope` helper, replaced 8 call sites, fixed 4 deep_copy_env guards
- `src/lisp/tests.c3` — Added 11 scope-per-call tests (recursive fib, HOF map, closure escape, named-let loop, variadic, multi-param, zero-arg, nested calls, chain-apply, deep recursion)

### Test count: 1019 PASS (918 unified + 77 compiler + 9 stack + 35 scope + extras), 0 failures
- Before: 1008 PASS
- After: 1019 PASS (+11 new scope-per-call tests)

---

## 2026-02-28 (Session 52): Scope-Region Phase 3 — Scope-per-run() with Value Promotion

### Summary
Implemented scope-per-run(): each `run()` call now creates a child scope, evaluates the expression in it, promotes the result to the parent scope, then releases the child scope — freeing all temporaries. This is the first phase where scope-region memory actually reclaims memory deterministically.

### Phase 3 Step 1: Fix copy_to_parent for all value types
- PARTIAL_PRIM: deep copy (new Value + recursive copy of first_arg/second_arg)
- CONTINUATION: shallow copy (new Value, keep cont_val)
- ITERATOR: shallow copy (new Value, keep iterator_val)
- FIBER: shallow copy (new Value, keep fiber_val)
- MODULE: return as-is (lives in interp.modules array)

### Phase 3 Step 2: Fix HASHMAP leak
- Changed `hashmap_new` to use `mem::malloc` instead of `allocate_in(root_region)` for HashMap struct
- Added HASHMAP case to `scope_dtor_value`: frees entries + HashMap struct

### Phase 3 Step 3: Scope-per-run() lifecycle
- `run()` creates child_scope, evaluates, promotes result via copy_to_parent, releases child_scope
- REPL loop wraps each input similarly
- Added `promote_to_root()` helper for values stored in long-lived data structures

### Use-after-free fixes discovered during Phase 3
- **Parser literals**: `alloc_value()` → `alloc_value_root()` for all 36 parser sites (literals stored in permanent Expr nodes)
- **Module env**: `make_env` for module env switched to root_scope in `jit_eval_module_impl` and `jit_load_module_from_file` (module env stored permanently)
- **make_primitive**: now switches to root_scope (primitives defined in global env permanently)
- **prim_set**: refactored to use `make_hashmap` instead of manual alloc_value (hashmap Values must be in root_scope for copy_to_parent)
- **eval_deftype/eval_defvariant**: refactored to use `make_primitive` instead of manual alloc_value
- **hashmap_set**: promotes key/value to root_scope via `promote_to_root`
- **prim_array/prim_array_set/prim_array_push**: promotes values to root_scope

### Key insight
`copy_to_parent` returns certain value types as-is (CLOSURE, HASHMAP, ARRAY, INSTANCE, PRIMITIVE, METHOD_TABLE, FFI_HANDLE, TYPE_INFO, MODULE) assuming they're already in root_scope. All creation sites for these types MUST allocate in root_scope. Types that copy_to_parent deep-copies (STRING, INT, DOUBLE, CONS, etc.) can safely live in child_scope.

### Files modified
- `src/lisp/eval.c3` — copy_to_parent fixes, promote_to_root, scope-per-run() in run()/REPL, eval_deftype/eval_defvariant simplified
- `src/lisp/value.c3` — scope_dtor_value HASHMAP case, alloc_value_root(), make_primitive root_scope switch
- `src/lisp/primitives.c3` — hashmap_new malloc, prim_set refactored, promote_to_root at hashmap_set/prim_array/prim_array_set/prim_array_push
- `src/lisp/parser.c3` — all alloc_value() → alloc_value_root()
- `src/lisp/jit.c3` — module env root_scope in jit_eval_module_impl + jit_load_module_from_file
- `src/lisp/runtime.c3` — string->list two-pass fix (remaining from Session 50 Phase 1a)

### Test count: 1008 [PASS] lines (907 unified + 77 compiler + 9 stack + 35 scope + extras), 0 failures

---

## 2026-02-28 (Session 51): Scope-Region Memory — Phases 0-2

### Summary
Implemented the foundation of the scope-region memory system: lightweight bump-allocating scopes that replace the heavyweight Pool/SlotTable system for Value and Env allocation. All Values and Envs now use O(1) bump allocation (~3 instructions) instead of Pool+SlotTable+ObjectRecord (~6 levels of indirection). Scope lifecycle management deferred to Phase 3+.

### Phase 0: Foundation
- New file: `src/scope_region.c3` — ScopeRegion, ScopeChunk, ScopeDtor, ScopeHandle structs
- Bump allocator with chunk growth (512B → 64KB cap), freelist recycling (up to 64 scopes)
- RC lifecycle: scope_create, scope_retain, scope_release, scope_destroy
- Destructor registration (bump-allocated in scope's own chunks)
- 35 unit tests for scope lifecycle, RC, reuse, handle validation
- Added `root_scope` and `current_scope` to Interp, initialized in `Interp.init()`

### Phase 1: Switch alloc_value/alloc_env
- `alloc_value()` → bump-allocate from `current_scope` + register `scope_dtor_value`
- `alloc_env()` → bump-allocate from `current_scope` + register `scope_dtor_env`
- Value dtor: frees malloc'd backing data (string chars, array storage, hashmap entries, etc.)
- Env dtor: frees malloc'd bindings and hash table
- All `make_*` functions that switch to root_region also switch `current_scope` to `root_scope`

### Phase 2: Verify bump allocation end-to-end
- No scope_create/release per-eval — all values accumulate in root_scope (same as old root_region)
- Added `current_scope` save/restore at 5 copy_to_parent callers, 8 coro/fiber boundary sites, 7 make_*/extend_root sites
- Old region system continues to operate for its own lifecycle (child_frame create/release)

### Files modified
- `src/scope_region.c3` (NEW) — scope region implementation + 35 unit tests
- `src/lisp/value.c3` — alloc_value/alloc_env, dtor functions, scope switching at make_* sites
- `src/lisp/eval.c3` — run()/REPL simplified (no child scope), copy_to_parent scope switching
- `src/lisp/jit.c3` — coro boundary saves, closure/method-table scope switching
- `src/lisp/primitives.c3` — fiber primitive scope saves, make_hashmap scope switching

### Test count: 993 (907 unified + 77 compiler + 9 stack engine), 0 failures

## 2026-02-27 (Session 50): Remove hard-coded limits & fix silent truncation

### Summary
Removed all fixed-size array limits from AST structs and parser. All parser-level collection now uses `List{T}` with finalization to exact-size malloc'd arrays. Fixed two silent data truncation bugs.

### Phase 1: Silent truncation fixes
- `prim_string_to_list` (primitives.c3): two-pass approach with `mem::temp_array` instead of `usz[512]` stack arrays — no longer silently drops codepoints beyond 512
- `rt_string_split` (runtime.c3): two-pass approach with `mem::temp_array` instead of `usz[256]` stack arrays — no longer silently drops segments beyond 256

### Phase 2-3: Dynamic AST structs
Converted all fixed-size arrays in AST structs to heap-allocated pointer+count:
- `ExprBegin.exprs`: `Expr*[256]` → `Expr**`
- `ExprCall.args`: `Expr*[256]` → `Expr**`
- `ExprMatch.clauses`: `MatchClause[128]` → `MatchClause*`
- `ExprHandle.clauses`: `EffectClause[64]` → `EffectClause*`
- `Pattern.elements`: `Pattern*[64]` → `Pattern**`
- `Pattern.ctor_sub_patterns`: `Pattern*[64]` → `Pattern**`
- `ExprLambda.params`: `SymbolId[256]` → `SymbolId*`
- `ExprLambda.param_annotations`: `TypeAnnotation[32]` → `TypeAnnotation*`
- `Closure.params`: `SymbolId[256]` → `SymbolId*`
- `EffectHandler.tags/clauses`: fixed arrays → heap-allocated

Updated ~20 parser sites, 4 macro sites, 4 compiler.c3 sites, 3 jit.c3 sites to use `List{T}` collection + malloc finalization.

### Phase 4: Deleted dead constants
- Removed `MAX_MATCH_CLAUSES`, `MAX_EFFECT_CLAUSES`, `MAX_PATTERN_ELEMS` from value.c3
- Removed stale limit check in `jit_handle_impl_fiber`

### Phase 5: Limit-busting tests
5 new tests verifying previously-limited scenarios:
- begin with >256 expressions
- lambda with >64 parameters
- match with >128 clauses
- string->list on string >512 characters
- string-split producing >256 segments

### Modified Files
- `src/lisp/value.c3` — AST struct changes, constant deletions
- `src/lisp/parser.c3` — ~20 sites: List collection + malloc finalization
- `src/lisp/macros.c3` — 4 sites: ExprBegin, ExprCall, lambda params, let bindings
- `src/lisp/jit.c3` — module loader, closure params, EffectHandler allocation
- `src/lisp/compiler.c3` — 4 lambda construction sites (params allocation)
- `src/lisp/primitives.c3` — string->list two-pass fix
- `src/lisp/runtime.c3` — string-split two-pass fix
- `src/lisp/tests.c3` — 5 limit-busting tests

### Tests
- 907 unified + 77 compiler + 9 stack engine = 993, 0 failures (was 988)

---

## 2026-02-27 (Session 49b): Remove JIT locals limit + lazy dlsym

### Summary
Two quality-of-life fixes that remove workarounds from the omni-torch diffusion demo:

1. **JIT locals limit removed**: Changed `JitLocals` from fixed `JitLocal[32]` array to C3's `List{JitLocal}` dynamic list. No arbitrary limit on local variables per scope.
2. **Lazy dlsym**: FFI `define [ffi lambda]` no longer calls `dlsym()` at module load. Resolution deferred to first call and cached. Modules can now declare bindings for functions that may not exist in the `.so` — they only fail if actually called.

### Modified Files
- `src/lisp/jit.c3` — `JitLocals.locals` changed from `JitLocal[128]` to `List{JitLocal}`, added `std::collections::list` import, updated all `locals.count` → `locals.locals.len()`, push/pop uses List API
- `src/lisp/value.c3` — `FfiBoundFn` struct: added `lib_handle` (void*) and `c_name` (char[128]) fields for lazy resolution
- `src/lisp/eval.c3` — `eval_ffi_fn`: removed dlsym call, stores lib handle + c_name in FfiBoundFn; `prim_ffi_bound_call`: added lazy dlsym on first call with caching

### Tests
- 902 unified + 77 compiler + 9 stack engine = 988, 0 failures
- Both omni-torch demos (XOR, diffusion) verified working

## 2026-02-27 (Session 49): omni-torch FFI migration + DDPM diffusion demo

### Summary
Completed the omni-torch migration to the new FFI syntax and created a working DDPM diffusion model demo. Fixed a critical FFI env binding bug where `eval_ffi_lib`/`eval_ffi_fn` used `env.extend()` (child frames invisible to module lookup) instead of `env.define()` (in-place mutation). This fix unblocked all omni-torch FFI operations.

### Critical Bug Fix: FFI env binding
- **Root cause**: `eval_ffi_lib`/`eval_ffi_fn` used `interp.global_env.extend()` which creates child env frames. Module's `mod.env.lookup()` searches parents only, so FFI bindings were invisible after `import :all`.
- **Fix**: Changed both to use `interp.global_env.define()` which modifies the env in-place (eval.c3 lines 924 and 1092).

### omni-torch Changes
- `lib/ffi/torch.omni` — Full rewrite: ~91 FFI lambda declarations using new `define [ffi lambda]` syntax
- `lib/torch.omni` — Added `tensor/randn`, `tensor/clamp`, `tensor/powf`, `tensor/select`, float scalar dispatch (`Tensor op Double`)
- `csrc/torch_shim.h/cpp` — Added `randn_1d/2d`, `mul/add/sub/div_scalar_f`, `pow_scalar_f`, `clamp`
- `examples/diffusion_2d.omni` — NEW: DDPM on 2D Swiss Roll (T=20, 64-unit MLP, manual backprop, algebraic effects for progress)
- `examples/xor_nn.omni` — Verified working with new FFI
- `Makefile` — Added `diffusion` target
- Cleaned up ~20 debug test files

### Notes
- JIT has 32-local limit per scope — large lets must be split into nested lets
- Diffusion loss converges from ~1.0 to ~0.4-0.5 with 2000 epochs (5.5s runtime)
- Installed `omni` binary updated to match build with FFI fix

## 2026-02-27 (Session 48): System primitives — shell, random, getenv, time, exit, sleep

### Summary
Added 8 libc-backed system primitives directly into Omni's stdlib: `shell`, `random`, `random-int`, `getenv`, `time`, `time-ms`, `exit`, `sleep`. These remove the need for FFI calls to access common OS functionality.

### Modified Files
- `src/lisp/primitives.c3` — Added extern declarations (popen, pclose, fread, feof, getenv, time, usleep, _exit, getrandom, clock_gettime) and 8 primitive implementations: `prim_shell` (captures stdout via popen), `prim_random` (getrandom→[0,1) double), `prim_random_int` (getrandom mod n), `prim_getenv`, `prim_time` (epoch seconds), `prim_time_ms` (clock_gettime milliseconds), `prim_exit` (_exit), `prim_sleep` (usleep with fractional seconds)
- `src/lisp/eval.c3` — Registered 8 new primitives in `register_primitives()`, bumped regular_prims array from 93→101
- `src/lisp/tests.c3` — 10 new system primitive tests (random range/type, random-int range/type, time, time-ms, getenv, getenv missing, shell echo, sleep)

### Tests
- 902 unified + 77 compiler + 9 stack engine = 988 (up from 978: +10 new system tests)
- 0 failures

## 2026-02-27 (Session 47): FFI Redesign Phase 1 — define [ffi lib] + define [ffi λ]

### Summary
Implemented the new FFI syntax: `(define [ffi lib] libc "libc.so.6")` and `(define [ffi λ libc] (strlen (^String s)) ^Int)`. FFI calls now look native: `(strlen "hello")` → 5. Uses libffi for correct ABI handling (mixed int/double args, zero-arg calls, void return). Old ffi-open/ffi-call/ffi-close/ffi-sym remain for historical support.
