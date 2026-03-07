# AOT Compiler Unification: Kill runtime.c3, Use JIT Infrastructure

**STATUS: COMPLETE** — All phases implemented across sessions 55-60. runtime.c3, runtime_bridge.c3 deleted. Compiler emits aot:: calls. AOT binaries link against JIT infrastructure. Effects, closures, let, named-let, factorial all verified E2E.

## Context

Omni has **two completely separate value systems**:

1. **JIT system** (`value.c3`, `jit.c3`, `eval.c3`, `primitives.c3`, `scope_region.c3`): The evolved,
   production interpreter. `Value*` (pointer to 24-byte bump-allocated struct), scope-region memory
   with RC lifecycle, 128 primitives, StackCtx-based algebraic effects, type system, multiple dispatch,
   coroutines, modules.

2. **AOT runtime** (`runtime.c3`): A separate reimplementation. `Value` (24-byte by-value struct),
   old region system with ObjectHandle indirection, 87 primitives, replay-based effects (re-execute
   body to resume continuations), no type system, no dispatch, no coroutines, 4KB string limit.

The AOT compiler (`compiler.c3`) transpiles `.lisp` → `.c3` source code that calls `runtime::*`
functions. The `--build` pipeline is **currently broken** (references deleted `src/continuation.c3`).

**Goal**: Kill `runtime.c3`. Make the AOT compiler emit code that calls the JIT's infrastructure
directly. AOT-compiled binaries link against the same Value*, scope-region memory, primitives,
and effect system that the interpreter uses. Then delete `runtime.c3`, `runtime_bridge.c3`,
`ghost_index.c3`, and simplify `main.c3`.

---

## The Fundamental Impedance Mismatch

| Aspect | AOT runtime (`runtime.c3`) | JIT system (`value.c3`) |
|--------|---------------------------|------------------------|
| Value type | `Value` (by-value, 24B) | `Value*` (pointer to 24B bump-allocated) |
| Tags | `V_NIL, V_INT, V_STRING...` (prefixed) | `NIL, INT, STRING...` (no prefix) |
| Strings | `StringData` in region via `ObjectHandle` (4KB limit) | `StringVal*` (heap-allocated, no limit) |
| Symbols | `SymbolData` in region via `ObjectHandle` | `SymbolId` (interned integer) |
| Cons cells | `ConsCell` with `ObjectHandle car/cdr` | `Cons` with `Value* car/cdr` |
| Closures | `ClosureData` inline: `void* data + ClosureFn` | `Closure*` heap: refcount, env_scope, Env*, params |
| Closure sig | `fn Value(void* data, Value arg)` | `fn Value*(Value*[] args, Env* env, Interp* interp)` |
| Primitives | `fn Value(Value a, Value b)` (binary, curried) | `fn Value*(Value*[] args, Env* env, Interp* interp)` |
| Memory | Old regions (`ObjectHandle`, `Pool`, `SlotTable`) | Scope-region bump alloc (`ScopeRegion`) |
| Effects | Replay-based (re-execute body) | StackCtx-based (real coroutine suspension) |
| TCO | `V_THUNK` trampoline | `jit_tco_bounce` flag in `jit_eval` loop |

**Key decision**: The AOT compiler generates C3 source code, so we can change what it emits. The
generated code will call JIT functions instead of runtime:: functions. The generated binary will
link against the full JIT infrastructure.

---

## Strategy: AOT Wrapper Module

Instead of making the compiler emit raw JIT internals (which would require passing `Interp*` and
`Env*` everywhere), create a thin **`src/lisp/aot.c3`** wrapper module that provides a clean API
for generated code. This module:

1. Owns a thread-local `Interp*` (initialized once in `main()`)
2. Exposes simple functions the compiler can emit calls to
3. Uses the JIT's Value*, scope-region memory, and primitives internally

The compiler changes are then purely mechanical: replace `runtime::` with `aot::` and adjust
signatures for `Value*` returns.

---

## Phase 0: Fix the Broken Build Pipeline

**Goal**: Remove reference to deleted `src/continuation.c3` so `--build` doesn't crash on a missing file.

**File**: `src/entry.c3`

**Steps**:
1. Find the c3c compile command around line 101 that lists source files for AOT compilation.
2. Remove `src/continuation.c3` from the file list.
3. Leave everything else for now — it will still fail (runtime.c3 hasn't been replaced yet), but it won't fail on a missing file.

**Verification**: `c3c build` still succeeds (this is the interpreter build, not AOT).

---

## Phase 1: Create `src/lisp/aot.c3` — The AOT Wrapper Module

**Goal**: Create a new file that wraps JIT infrastructure with a simple API for generated code.

**File**: `src/lisp/aot.c3` (NEW)

**Module declaration**: `module lisp::aot;`

**Imports needed**: `std::io`, `main`, `lisp` (for Interp, Value, Env, etc.)

### 1a. Thread-local Interp

Create a thread-local `lisp::Interp* g_aot_interp = null;` and these functions:

- `fn void aot_init()` — malloc an Interp, call `.init()`, `register_primitives()`, `register_stdlib()`, set `flags.jit_enabled = true`
- `fn void aot_shutdown()` — free the interp, set to null
- `fn lisp::Interp* aot_interp() @inline` — return `g_aot_interp`

### 1b. Value Constructors

Each returns `Value*`. The compiler will emit calls to these. All use `g_aot_interp` internally.

- `fn Value* make_nil() @inline` → calls `lisp::make_nil(g_aot_interp)`
- `fn Value* make_int(long n) @inline` → calls `lisp::make_int(g_aot_interp, n)`
- `fn Value* make_double(double d) @inline` → calls `lisp::make_double(g_aot_interp, d)`
- `fn Value* make_string(char[] s) @inline` → calls `lisp::make_string(g_aot_interp, s)`
- `fn Value* make_symbol(char[] s) @inline` → interns via `g_aot_interp.symbols.intern(s)`, then `lisp::make_symbol(g_aot_interp, sid)`
- `fn Value* make_true() @inline` → `lisp::make_symbol(g_aot_interp, g_aot_interp.sym_true)`
- `fn Value* make_false() @inline` → `lisp::make_symbol(g_aot_interp, g_aot_interp.sym_false)`

### 1c. List Operations

- `fn Value* cons(Value* car, Value* cdr) @inline` → `lisp::make_cons(g_aot_interp, car, cdr)`
- `fn Value* car(Value* v) @inline` → null/tag check, return `v.cons_val.car` or `make_nil()`
- `fn Value* cdr(Value* v) @inline` → null/tag check, return `v.cons_val.cdr` or `make_nil()`

### 1d. Function Invocation

- `fn Value* invoke(Value* func, Value* arg)` → calls `lisp::jit_apply_value(func, arg, g_aot_interp)`
- `fn Value* apply_multi(Value* func, Value* arg_list, long argc)` — builds a `Value*[]` args array from the cons list, then calls `lisp::jit_apply_multi_args(func, args_slice, g_aot_interp)`. Use a stack buffer `Value*[16]` for the common case, malloc if argc > 16.

**IMPORTANT**: Check what the actual JIT apply functions are named. Look in `jit.c3` for:
- A function that applies a Value* function to a single Value* arg
- A function that applies a Value* function to a Value*[] slice of args
Use whatever names/signatures actually exist. The plan uses `jit_apply_value` and `jit_apply_multi_args` as placeholders — find the real names.

### 1e. Truthiness Test

```
fn bool is_truthy(Value* v) @inline
```
- `null` → false
- `v.tag == NIL` → false
- `v.tag == SYMBOL && v.sym_val == g_aot_interp.sym_false` → false
- everything else → true

### 1f. Primitive References

```
fn Value* lookup_prim(char[] name)
```
Interns the name, calls `lisp::env_lookup(g_aot_interp.global_env, sid)`. Returns nil if not found.

**NOTE**: Check the actual function name for env lookup — it might be `env_get`, `env_lookup`, or a method on `Env*`. Search `value.c3` and `jit.c3` for the real API.

### 1g. AOT Closure Creation

AOT closures use a `void* data + fn Value*(void* data, Value* arg)` convention. Bridge this to the JIT by creating a `Primitive` that wraps the AOT closure function.

Define types:
```
alias AotClosureFn = fn Value*(void* data, Value* arg);
alias AotVariadicFn = fn Value*(void* data, Value* arg_list);

struct AotClosureData {
    void*         data;
    AotClosureFn  invoke;
    AotVariadicFn invoke_var;
    bool          is_variadic;
}
```

Functions:
- `fn Value* make_closure(void* data, AotClosureFn invoke)` — malloc an `AotClosureData`, fill it in, create a `Primitive` via `lisp::make_primitive(g_aot_interp, "aot-closure", &aot_closure_apply, 1)`, set `user_data` to the AotClosureData.
- `fn Value* make_variadic_closure(void* data, AotVariadicFn invoke)` — same but variadic, arity -1.
- `fn Value* aot_closure_apply(Value*[] args, Env* env, Interp* interp)` — PrimitiveFn adapter. Gets `AotClosureData*` from `interp.prim_user_data`, calls `cd.invoke(cd.data, args[0])`.
- `fn Value* aot_variadic_apply(Value*[] args, Env* env, Interp* interp)` — same but builds cons list from args, calls `cd.invoke_var(cd.data, list)`.

**NOTE**: Check how `make_primitive` works and how `user_data` is passed. Look at `prim_val.user_data` in value.c3. The `interp.prim_user_data` mechanism may not exist — search for how existing primitives access user_data. Adapt accordingly.

### 1h. Variable Table

- `fn void define_var(char[] name, Value* val)` — intern name, call `global_env.define(sid, val)` (or whatever the real define method is)
- `fn Value* lookup_var(char[] name)` — intern name, env_lookup on global_env, return nil if not found
- `fn void set_var(char[] name, Value* val)` — intern name, env_set on global_env

### 1i. Output

- `fn void print_value(Value* v)` — calls `lisp::print_value(v, g_aot_interp)` (find the real print function name)

### 1j. Dict/Array Operations

- `fn Value* dict_from_args(Value* arg_list)` — creates a hashmap from key-value cons pairs
- `fn Value* index(Value* collection, Value* idx)` — looks up `ref` primitive and applies it

### 1k. Effects

**Approach A (recommended — simplest)**: Don't emit handle/signal at the C3 level. Instead, once closures are wrapped as Primitives callable by `jit_apply_value`, ALL JIT features (effects, coroutines, TCO) work automatically through `invoke()`. The compiler wraps effect forms into lambda/closure expressions. These closures, when invoked through `aot::invoke`, go through the JIT's apply path which handles StackCtx-based effects transparently.

Add an escape hatch for complex cases:
```
fn Value* eval_source(char[] source)
```
Calls `lisp::run(source, &*g_aot_interp)`, returns the result value or nil on error.

For `compiled_handle`, `compiled_perform`, `compiled_reset`, `compiled_shift` — implement as thin wrappers that invoke the body/handler closures through the JIT effect system. The exact implementation depends on what `jit.c3` exposes. Start with the simplest approach: construct Lisp source strings and eval them, or call the JIT's handle/signal/reset/shift internals directly if they have usable public APIs.

**Steps to determine the right approach**:
1. Read `jit.c3` and find `jit_handle_impl`, `jit_signal_impl`, `jit_reset_impl`, `jit_shift_impl` (or whatever they're called)
2. Check if they can be called with `Value*` closures directly
3. If yes, write thin wrappers. If no, use the eval_source escape hatch.

### 1l. Add to project.json

Add `"src/lisp/aot.c3"` to the sources list in `build/project.json`.

**Verification**: `c3c build` succeeds with the new file.

---

## Phase 2: Update compiler.c3 — Emit aot:: Instead of runtime::

**Goal**: Change all emit functions so generated C3 code calls `aot::` instead of `runtime::`.

**File**: `src/lisp/compiler.c3`

This is the largest phase. Every occurrence of `runtime::` in emitted string literals must change to `aot::`, and every `runtime::Value` type must change to `lisp::Value*`.

### 2a. Change emit_prelude

Find `emit_prelude` (around line 1940). Change the emitted import from:
```
import lisp::runtime;
```
to:
```
import lisp;
import lisp::aot;
```

### 2b. Mechanical runtime:: → aot:: Replacements

Search the entire `compiler.c3` for every string literal containing `runtime::` and replace. There are ~100+ occurrences. Here are all the patterns:

**Value constructors**:
| Old emitted code | New emitted code |
|-----------------|-----------------|
| `runtime::make_nil()` | `aot::make_nil()` |
| `runtime::make_int(N)` | `aot::make_int(N)` |
| `runtime::rt_make_double(X)` | `aot::make_double(X)` |
| `runtime::make_string("...")` | `aot::make_string("...")` |
| `runtime::make_symbol("...")` | `aot::make_symbol("...")` |
| `runtime::make_true()` | `aot::make_true()` |
| `runtime::make_closure(data, &fn)` | `aot::make_closure(data, &fn)` |
| `runtime::make_variadic_closure(data, &fn)` | `aot::make_variadic_closure(data, &fn)` |

**Invocation**:
| Old | New |
|-----|-----|
| `runtime::rt_invoke(func, arg)` | `aot::invoke(func, arg)` |
| `runtime::rt_invoke_once(func, arg)` | `aot::invoke(func, arg)` |
| `runtime::rt_apply_multi(func, args, N)` | `aot::apply_multi(func, args, N)` |

**Truthiness**:
| Old | New |
|-----|-----|
| `runtime::rt_is_truthy(v)` | `aot::is_truthy(v)` |

**Primitives**: The compiler currently emits `runtime::make_prim(&runtime::rt_add)` etc. In `compile_var` (around line 3803), there's a hash-based lookup mapping symbol names to hardcoded runtime function references. **Replace the entire primitive hash table** with `aot::lookup_prim("SYMBOL_NAME")` calls. For example:
- `runtime::make_prim(&runtime::rt_add)` → `aot::lookup_prim("+")`
- `runtime::make_prim(&runtime::rt_sub)` → `aot::lookup_prim("-")`
- `runtime::make_prim(&runtime::rt_mul)` → `aot::lookup_prim("*")`
- etc. for all ~50+ primitives

**Variable management**:
| Old | New |
|-----|-----|
| `runtime::rt_define_var("name", val)` | `aot::define_var("name", val)` |
| `runtime::rt_lookup_var("name")` | `aot::lookup_var("name")` |
| `runtime::rt_set_var("name", val)` | `aot::set_var("name", val)` |

**Index/field access**: `runtime::rt_index(coll, idx)` → `aot::index(coll, idx)`

**Dict operations**: `runtime::rt_dict_from_args(args)` → `aot::dict_from_args(args)`

**Effects**:
| Old | New |
|-----|-----|
| `runtime::rt_compiled_handle(...)` | `aot::compiled_handle(...)` |
| `runtime::rt_compiled_perform(...)` | `aot::compiled_perform(...)` |
| `runtime::rt_compiled_reset(...)` | `aot::compiled_reset(...)` |
| `runtime::rt_compiled_shift(...)` | `aot::compiled_shift(...)` |
| `runtime::rt_compiled_resolve(...)` | `aot::compiled_resolve(...)` |

**Output**: `runtime::rt_print_value(v)` → `aot::print_value(v)`

**Cons operations**: `runtime::rt_cons(a, b)` → `aot::cons(a, b)`, `runtime::rt_car1(v)` → `aot::car(v)`, `runtime::rt_cdr1(v)` → `aot::cdr(v)`

**Region management — DELETE entirely** (scope-region handles this):
- `runtime::rt_push_frame()` — delete the line
- `runtime::rt_pop_frame(frame)` — delete the line
- `runtime::rt_current_region()` — delete the line
- `runtime::rt_capture_value(region, val)` — delete the line
- `main::RegionHandle _frame = ...` — delete the line
- `main::ObjectHandle _closure_handle_N = main::allocate_in(...)` — replace (see 2d)

### 2c. Type Changes in Emitted Code

Every emitted `runtime::Value` type declaration must change to `lisp::Value*`:

| Old emitted code | New emitted code |
|-----------------|-----------------|
| `runtime::Value _rN;` | `lisp::Value* _rN;` |
| `runtime::Value varname;` | `lisp::Value* varname;` |
| `fn runtime::Value invoke_lambda_N(...)` | `fn lisp::Value* invoke_lambda_N(...)` |

### 2d. Closure/Lambda Struct Changes

**Old pattern** (closure with captures):
```c3
struct Lambda_N {
    main::RegionHandle home_region;
    runtime::Value captured_X;
    runtime::Value captured_Y;
}
```

**New pattern**:
```c3
struct Lambda_N {
    lisp::Value* captured_X;
    lisp::Value* captured_Y;
}
```

Changes:
- Remove `home_region` field
- Change captured fields from `runtime::Value` to `lisp::Value*`
- Remove write barrier calls (`runtime::rt_capture_value(...)`)
- Remove region allocation (`main::allocate_in(...)`)
- Instead, `malloc` the Lambda_N struct: `Lambda_N* _cd_N = (Lambda_N*)mem::malloc(Lambda_N.sizeof);`

**Closure creation** changes from the old complex region-based pattern to:
```c3
Lambda_N* _cd_N = (Lambda_N*)mem::malloc(Lambda_N.sizeof);
_cd_N.captured_X = X;
_rN = aot::make_closure((void*)_cd_N, &invoke_lambda_N);
```

**No-capture closure**: `_rN = aot::make_closure(null, &invoke_lambda_N);`

### 2e. Lambda Function Body Changes

Remove frame push/pop. Old:
```c3
fn runtime::Value invoke_lambda_N(void* _self, runtime::Value _arg) {
    main::RegionHandle _frame = runtime::rt_push_frame();
    // ... body ...
    runtime::rt_pop_frame(_frame);
    return _result;
}
```

New (no frame management — JIT's scope-per-call handles this):
```c3
fn lisp::Value* invoke_lambda_N(void* _self, lisp::Value* _arg) {
    // ... body ...
    return _result;
}
```

Parameter unpacking: `runtime::rt_car1(_curr)` → `aot::car(_curr)`, `runtime::rt_cdr1(_curr)` → `aot::cdr(_curr)`

### 2f. Recursive let (let ^rec) Self-Reference Patch

Old pattern:
```c3
Lambda_N* _sp_N = (Lambda_N*)varname.closure_val.data;
_sp_N.captured_varname = varname;
```

New pattern — the closure is now wrapped in a Primitive. Access the AotClosureData:
```c3
lisp::aot::AotClosureData* _acd_N = (lisp::aot::AotClosureData*)varname.prim_val.user_data;
Lambda_N* _sp_N = (Lambda_N*)_acd_N.data;
_sp_N.captured_varname = varname;
```

### 2g. Effect Array Types

`runtime::Value[N]` → `lisp::Value*[N]` for effect tag/handler arrays.

### 2h. Main Function

Change emitted `main()`:
- `runtime::rt_init()` → `aot::aot_init()`
- `runtime::rt_shutdown()` → `aot::aot_shutdown()`

### 2i. Global Variable Declarations

`runtime::Value symbol_name;` → `lisp::Value* symbol_name = null;`

### Summary of Functions to Modify in compiler.c3

| Function | Approx Line | Changes |
|----------|-------------|---------|
| `emit_prelude` | ~1940 | Change imports |
| `emit_global_declarations` | ~2235 | `runtime::Value` → `lisp::Value*` |
| `emit_main_start` | ~2247 | `runtime::rt_init()` → `aot::aot_init()` |
| `emit_main_end` | ~2260 | `runtime::rt_shutdown()` → `aot::aot_shutdown()` |
| `emit_lambda_definitions` | ~1948 | Struct fields, function sigs, frame push/pop, captures |
| `emit_temp_decl` | (find it) | `runtime::Value` → `lisp::Value*` |
| `compile_literal` | ~3747 | `runtime::make_int` → `aot::make_int` etc. |
| `compile_var` | ~3803 | Primitive hash → `aot::lookup_prim`, type → `lisp::Value*` |
| `compile_call_flat` | ~2605 | `runtime::rt_apply_multi` → `aot::apply_multi` |
| `compile_app_flat` | ~2858 | `runtime::rt_invoke` → `aot::invoke` |
| `compile_if_flat` | ~2908 | `runtime::rt_is_truthy` → `aot::is_truthy` |
| `compile_let_flat` | ~2940 | Type + self-reference patch |
| `compile_lambda_flat` | ~3624 | Closure struct + creation completely rewritten |
| `compile_match_flat` | ~3705 | Type changes |
| `compile_handle_flat` | ~3551 | Effect array types + `aot::compiled_handle` |
| `compile_perform_flat` | ~3530 | `aot::compiled_perform` |
| `compile_reset_flat` | ~3508 | `aot::compiled_reset` |
| `compile_shift_flat` | ~3519 | `aot::compiled_shift` |
| `compile_begin_flat` | ~3168 | Type changes only |
| `compile_and_flat` / `compile_or_flat` | | `aot::is_truthy` |
| `compile_set_flat` | | `aot::set_var` |
| `compile_module_flat` / `compile_import_flat` | | Type changes |

**Verification**: `c3c build` succeeds. Compiler tests in `tests.c3` pass after pattern updates (Phase 4).

---

## Phase 3: Update Build Pipeline

**Goal**: Make `--build` produce working binaries that link against JIT infrastructure.

**File**: `src/entry.c3`

### 3a. Update c3c compile command (around line 101)

Replace the file list with all files needed for JIT infrastructure:
```
src/main.c3 src/ghost_index.c3 src/scope_region.c3 src/stack_engine.c3
src/lisp/value.c3 src/lisp/eval.c3 src/lisp/primitives.c3
src/lisp/parser.c3 src/lisp/macros.c3 src/lisp/jit.c3
src/lisp/aot.c3
build/_aot_temp.c3
```

Add linker flags: `-l lightning -l dl -l m -l ffi`

Add C source compilation for stack_helpers.c and ffi_helpers.c (either compile to .o first, or use c3c's mechanism for including C sources).

### 3b. Update project.json

Ensure `src/lisp/aot.c3` is in the main build's source list.

**Verification**: `c3c build` succeeds. `./build/main --build examples/hello.lisp -o build/hello` produces a binary.

---

## Phase 4: Update Compiler Tests

**File**: `src/lisp/tests.c3`

### 4a. Update existing compiler tests (around lines 3045-3718)

The tests call `compile_to_c3()` and check output for string patterns. Most tests just check `!str_contains(code, "unsupported")` and presence of function names like `make_int(3)`. Since `aot::make_int(3)` still contains `make_int(3)`, most will pass unchanged.

Only update tests that check for `runtime::` specifically:
| Old pattern check | New pattern check |
|-------------------|-------------------|
| `str_contains(code, "runtime::make_int")` | `str_contains(code, "aot::make_int")` |
| `str_contains(code, "runtime::rt_invoke")` | `str_contains(code, "aot::invoke")` |
| `str_contains(code, "runtime::Value")` | `str_contains(code, "lisp::Value*")` |

### 4b. Add E2E compiler tests

New tests that compile simple programs and verify the output contains expected patterns:
- `"AOT: simple int"` — compile `42`, verify output contains `aot::make_int(42)`
- `"AOT: function call"` — compile `(+ 1 2)`, verify output contains `aot::lookup_prim("+")`
- `"AOT: lambda"` — compile `(lambda (x) x)`, verify output contains `invoke_lambda_0`
- `"AOT: closure"` — compile `(let (x 1) (lambda (y) (+ x y)))`, verify Lambda_0 struct has `lisp::Value*`

**Verification**: All tests pass. `./build/main` reports same or higher test count.

---

## Phase 5: Delete Dead Code

### 5a. Delete `src/lisp/runtime.c3` (~2800 lines)

Entire file is dead. All functionality provided by JIT + aot.c3.

### 5b. Delete `src/lisp/runtime_bridge.c3` (~259 lines)

Bridge between runtime::Value and lisp::Value* — nothing to bridge anymore.

### 5c. Clean up `src/entry.c3`

Remove any remaining references to runtime.c3 or runtime_bridge.c3.

### 5d. Update `project.json`

Remove `src/lisp/runtime.c3` and `src/lisp/runtime_bridge.c3` from the source list. Ensure `src/lisp/aot.c3` is present.

### 5e. Update `scripts/run_e2e.sh`

Line 16 references deleted files (`src/context.c3`, `src/continuation.c3`, `src/delimited.c3`). Remove those. Also remove `src/lisp/runtime_bridge.c3`. Add `src/lisp/aot.c3` and `src/scope_region.c3`.

### 5f. Clean up `main.c3`

**Before deleting anything**: grep for each function name to confirm zero non-runtime callers.

Only delete functions confirmed dead after runtime.c3 removal:
- `create_region` (only called by `rt_push_frame`)
- `release_region` (only called by `rt_pop_frame`)
- `destroy_region` (cascades from `release_region`)
- `GhostTable` and all ghost-related code
- `write_barrier`
- `DestructorRegistry`

**Keep**: `Pool`, `SlotTable`, `ObjectHandle`, `RegionHandle`, `allocate_in`, `dereference_as` — still used by `alloc_expr()` and `alloc_pattern()` for Expr/Pattern allocation.

### 5g. Consider deleting `src/ghost_index.c3`

If `GhostTable` in main.c3 was its only consumer, this file is also dead. Grep to confirm.

**Verification**: `c3c build` succeeds. All tests pass. `--build` produces working binaries.

---

## Phase 6: E2E Verification

1. `c3c build` — interpreter builds clean
2. `./build/main` — all tests pass (unified + compiler + stack + scope)
3. Create `examples/hello.lisp` (if not exists):
   ```lisp
   (define (factorial n)
     (if (= n 0) 1 (* n (factorial (- n 1)))))
   (println (factorial 10))
   ```
4. `LD_LIBRARY_PATH=/usr/local/lib ./build/main --build examples/hello.lisp -o build/hello`
5. `LD_LIBRARY_PATH=/usr/local/lib ./build/hello` → prints `3628800`
6. Test with closures:
   ```lisp
   (define (make-adder x) (lambda (y) (+ x y)))
   (define add5 (make-adder 5))
   (println (add5 10))
   ```
7. Test with effects:
   ```lisp
   (handle
     (begin (println (+ 1 (signal 'ask 42))))
     (ask val (resolve (+ val 100))))
   ```
8. `scripts/run_e2e.sh` passes (after fixing the script)

---

## Files Modified Summary

| File | Changes | Lines (est) |
|------|---------|-------------|
| `src/lisp/aot.c3` | **NEW** — AOT wrapper module | ~300 |
| `src/lisp/compiler.c3` | All emit functions: `runtime::` → `aot::`, Value → Value*, closure/lambda rewrite | ~200 changed |
| `src/entry.c3` | Build command file list + linker flags | ~10 |
| `src/lisp/tests.c3` | Compiler test pattern updates | ~30 |
| `src/lisp/jit.c3` | Effect wrappers (only if Approach B needed) | 0-100 |
| `src/lisp/runtime.c3` | **DELETE** | -2800 |
| `src/lisp/runtime_bridge.c3` | **DELETE** | -259 |
| `src/main.c3` | Delete dead functions (create_region, GhostTable, etc.) | -800 |
| `scripts/run_e2e.sh` | Fix file list | ~5 |
| `build/project.json` | Add aot.c3, remove runtime.c3/runtime_bridge.c3 | ~3 |

---

## Critical Implementation Notes for the Agent

### Before writing any code:
1. Read `src/lisp/value.c3` to understand the exact Value struct, tags (ValueTag enum), and field names (e.g., is it `cons_val.car` or `data.cons.car`?)
2. Read `src/lisp/eval.c3` to find `register_primitives()`, `register_stdlib()`, `run()` signatures
3. Read `src/lisp/jit.c3` to find the actual apply function names and signatures
4. Read `src/lisp/compiler.c3` thoroughly — understand every emit function before changing it
5. Read `src/lisp/runtime.c3` to understand what the old API looked like (helps map old→new)
6. Read `build/project.json` for the current source file list
7. Read `src/entry.c3` for the --build pipeline

### Function name verification checklist:
- [ ] What is the JIT's single-arg apply function? (`jit_apply_value`? `jit_apply`? something else?)
- [ ] What is the JIT's multi-arg apply function? (`jit_apply_multi_args`? different?)
- [ ] What is the env lookup function? (`env_lookup`? `env_get`? method on Env?)
- [ ] What is the env define function? (`env_define`? method?)
- [ ] What is the env set function? (`env_set`? method?)
- [ ] What is `make_primitive`'s exact signature? How does it handle user_data?
- [ ] How does `prim_user_data` work? Is it `interp.prim_user_data` or on the Value struct?
- [ ] What is the print function? (`print_value`? `display_value`?)
- [ ] What is `make_cons` signature? `make_cons(interp, car, cdr)` or different?
- [ ] What is `make_hashmap` signature?
- [ ] What is `hashmap_set` signature?

### C3 language gotchas:
- C3 slices are INCLUSIVE: `buffer[0..n]` = n+1 elements, use `buffer[:n]` for n elements
- `tlocal` is the keyword for thread-local storage in C3
- Module imports use `import module_name;` syntax
- Functions use `fn return_type name(params)` syntax
- `@inline` is an attribute for inlining
- `mem::malloc` and `mem::free` for heap allocation
- Cast syntax: `(TargetType)value` or `(TargetType*)pointer`

---

## Deferred Items

### D1. AOT closure optimization — native JIT Closure instead of Primitive wrapper
Currently AOT closures are wrapped as `Primitive` values with a thunk. One layer of indirection per call. Future: emit code that directly creates a JIT `Closure*` with an `Env*` built from captured variables.

### D2. Compile-time primitive binding
Instead of `aot::lookup_prim("+")` at runtime, cache all primitive lookups in `aot_init()` into global `Value*` variables. Zero runtime cost for primitive references.

### D3. TCO in AOT closures
AOT→AOT direct calls (not going through invoke) don't get TCO. Future: emit direct tail calls between AOT functions.
