# FFI Evaluation & History

## Three Eras of FFI

### Era 1: Go Interpreter (commit `245ce3d`, Dec 2025)

- `(ffi "function_name" arg1 arg2 ...)`
- `(ffi-declare "ret_type" "func_name" "arg1_type" ...)`
- **Not a real FFI** — could not actually call C functions at runtime
- In staging/codegen mode, emitted C code via sprintf: `fnName(arg1, arg2)`
- In interpreter mode, hardcoded 5 functions: `puts`, `putchar`, `getchar`, `exit`, `printf`
- FFIDecl registry was a `map[string]*FFIDecl` for codegen output only
- No dlopen, no dlsym, no type marshalling — purely a code generation template
- ~150 lines of Go

### Era 2: FFI Proposal (commit `4030aca`, Jan 2026)

- 865-line design document in `FFI_PROPOSAL.md` — **never implemented**
- Extremely ambitious design:
  - Handle-based pointers with generation counters (ABA protection)
  - Ownership annotations: `^:owned`, `^:borrowed`, `^:consumed`
  - Safe callbacks via `WeakHandle` (auto-invalidate when closure freed)
  - Region-aware FFI buffers (integrates with IRegion vtable)
  - Deterministic replay mode (sequential handle allocation)
- Full C type system: `{CInt8}` through `{CUInt64}`, `{CPtr T}`, `{CArray T N}`, `{CFn [args] ret}`, `{CConst T}`
- Designed struct mapping, async FFI, callback trampolines, debug mode
- Would have been one of the most sophisticated Lisp FFIs — but remained on paper

### Era 3: Current C3 Implementation (what exists now)

- `ffi-open`, `ffi-call`, `ffi-close`, `ffi-sym` — real dlopen/dlsym
- Actually calls C functions at runtime via dispatch tables
- 6 types: `'int`, `'size`, `'double`, `'string`, `'ptr`, `'void`
- `--bind` with libclang for auto-generated wrappers
- ~500 lines (primitives.c3) + ~400 lines (bindgen)

---

## Side-by-Side Comparison

| Feature | Go Era | FFI Proposal | Current C3 |
|---------|--------|-------------|------------|
| **Actually calls C** | No (codegen only) | Designed but not built | Yes (dlopen/dlsym) |
| **Type system** | None (strings only) | 17+ C types, typed ptrs, arrays | 6 symbol types |
| **Structs** | No | Full with field mapping | No (opaque `'ptr`) |
| **Callbacks** | No | WeakHandle-based safe callbacks | No |
| **Ownership** | No | `^:owned`/`^:borrowed`/`^:consumed` | No (manual) |
| **Memory safety** | N/A | Handle tables + generation counters | Raw pointers |
| **Code generation** | sprintf C code | Thunks + trampolines (designed) | libclang `--bind` |
| **Variadic** | No | Yes (`^:variadic`) | No |
| **Mixed int/double** | N/A | N/A | No |
| **Max args** | Unlimited (codegen) | Unlimited (designed) | 6 int / 3 double |
| **Error handling** | `NewError()` | Debug mode + tracing | `dlerror()` propagation |

---

## Current FFI — Strengths

1. **Dead simple API** — 4 primitives, no boilerplate framework
2. **Symbol caching** — 64-entry cache per handle avoids repeated dlsym
3. **Auto-binding via libclang** — `--bind` parses C headers, generates typed Omni wrappers
4. **Proper x86-64 ABI split** — separate dispatch for integer (RDI/RSI/...) vs XMM (double) registers
5. **Works in practice** — omni-torch (PyTorch bindings) proves it handles real libraries
6. **Error capture** — dlerror() messages propagated as Omni errors

## Current FFI — Weaknesses

| Gap | Impact | Difficulty |
|-----|--------|------------|
| **No struct marshalling** | Can't use most C APIs beyond trivial ones | Hard (needs layout engine) |
| **No callbacks** | Can't use qsort, signal handlers, event loops | Hard (needs trampoline codegen) |
| **No mixed int/double args** | `pow(int, double)` fails — common pattern | Medium (ABI register interleaving) |
| **Max 6 int / 3 double args** | Some C APIs exceed this | Medium (extend dispatch table) |
| **No variadic support** | printf, open(O_CREAT), ioctl blocked | Hard (ABI-dependent) |
| **Silent type coercion** | String as `'int` → 0, no error | Easy fix |
| **No finalizers** | Must manually `ffi-close`, leaked on error paths | Medium |
| **Returned `char*` not freed** | Memory leak if C allocates (strdup, readline) | Easy (add `ffi-free`) |
| **No sized integer types** | int32/uint64 etc. all map to `'int` | Easy |

## Calling Convention Details

Dispatch table in `primitives.c3` / `eval.c3`:
- `FfiFn0` through `FfiFn6` — 0-6 integer args → long return
- `FfiFnD0` through `FfiFnD3` — 0-3 double args → double return
- `FfiFnDI1`, `FfiFnDI2` — double args → long return
- `FfiFnID0` through `FfiFnID2` — int args → double return
- **Cannot mix int and double in the same call** — returns error

## `--bind` System

**How it works** (`entry.c3`, `libclang_bind.c3`, `bindgen.c3`):
1. Parse `omni.toml` for `[dependencies.ffi.*]` sections
2. Load libclang dynamically (tries multiple paths)
3. Parse C headers via libclang AST visitor
4. Map C types → Omni FFI types (int→`'int`, char*→`'string`, other*→`'ptr`)
5. Generate `lib/ffi/{name}.omni` module with typed wrapper functions
6. Skip variadic functions (with comment)

**Limitations**:
- Requires libclang installed
- Parameters named `arg0`, `arg1`, ... (no C parameter names)
- All structs → opaque `'ptr`
- No custom type mappings
- Buffer limits: 64B param names, 16 params max, 256 functions max per lib

---

## Ergonomics Comparison: How Each FFI *Feels* to Use

This is the critical comparison. The goal is to make Omni's FFI the most seamless in existence.

### Current Omni — Verbose and Manual

**Raw FFI call:**
```lisp
(define libc (ffi-open "libc.so.6"))
(ffi-call libc "strlen" 'size "hello" 'string)    ; => 5
(ffi-call libc "abs" 'int -42 'int)               ; => 42
(ffi-close libc)
```
Every call repeats the library handle, the C function name as a string, and interleaves type annotations with arguments. For a one-off call it's fine. For 80 functions it's unreadable.

**Generated bindings (via `--bind`):**
```lisp
(import "ffi/torch.omni" :all)
(omni-torch-zeros-1d 10 6)     ; OK but arg0/arg1 names are meaningless
(omni-torch-add handle1 handle2)
```
Better — but parameter names are `arg0`, `arg1`, the function names are `omni-torch-*` prefixed (C naming leaked through), and you still need a hand-written wrapper module (torch.omni) to make it idiomatic.

**Real-world result (omni-torch):**
The user wrote a 200-line wrapper module (`lib/torch.omni`) on top of the 80-line generated bindings (`lib/ffi/torch.omni`) on top of a 400-line C++ shim (`libtorch_omni.so`). Three layers to call PyTorch. The FFI didn't remove boilerplate — it moved it.

### LuaJIT FFI — The Gold Standard for Simplicity

```lua
local ffi = require("ffi")
ffi.cdef[[
  int printf(const char *fmt, ...);
  typedef struct { double x, y; } point_t;
  double sqrt(double x);
]]

ffi.C.printf("Hello %s!\n", "world")    -- just call it
local p = ffi.new("point_t", {3.0, 4.0})
print(p.x, p.y)                         -- field access works
print(ffi.C.sqrt(p.x*p.x + p.y*p.y))   -- 5.0
```
**Why it's great**: You paste C declarations verbatim. No type annotation at call sites. Structs just work. Fields are accessed with dot notation. The JIT compiles FFI calls to native code (zero overhead). It feels like the C is *inside* Lua.

**What Omni can steal**: The `cdef` model — declare once, call naturally. No per-call type annotations.

### Janet FFI — Clean Declarative Bindings

```janet
(ffi/context "/usr/lib/libm.so")
(ffi/defbind sqrt :double [x :double])
(ffi/defbind strlen :size [s :string])

(sqrt 2.0)       # => 1.4142...
(strlen "hello")  # => 5
```
**Why it's great**: One line declares *and* binds. After `defbind`, you call `sqrt` like any Janet function. No handles, no library variables, no string names at call sites. The `ffi/context` sets the library scope.

**What Omni can steal**: The `defbind` pattern — single declaration that creates a callable function. The context-scoped library loading.

### Racket FFI — Safe but Verbose

```racket
(define libm (ffi-lib "libm"))
(define c-sqrt (get-ffi-obj "sqrt" libm (_fun _double -> _double)))
(c-sqrt 2.0)  ; => 1.4142...

; Or with define-ffi-definer:
(define-ffi-definer define-libm (ffi-lib "libm"))
(define-libm c-sqrt (_fun _double -> _double) #:c-id "sqrt")
(c-sqrt 2.0)
```
**Structs:**
```racket
(define-cstruct _point ([x _double] [y _double]))
(define p (make-point 3.0 4.0))
(point-x p)  ; => 3.0
```
**Why it's great**: Type-safe, GC-managed, callbacks work, structs work with accessors. `define-cstruct` generates constructors and field accessors automatically.

**Why it's verbose**: Every binding needs explicit type signatures. Two different systems (`get-ffi-obj` vs `define-ffi-definer`). Naming is manual (`c-sqrt` vs `sqrt`).

**What Omni can steal**: `define-cstruct` — auto-generated constructors + accessors from a declaration. Safe pointer types.

### Zig — Compile-Time Header Import (The Ultimate)

```zig
const c = @cImport({
    @cInclude("math.h");
    @cInclude("stdio.h");
});

pub fn main() void {
    _ = c.printf("sqrt(2) = %f\n", c.sqrt(2.0));
}
```
**Why it's great**: `@cImport` reads the actual C header at compile time. No declarations, no bindings, no wrappers. You just call C functions with their C names. Type checking happens at compile time. This is as seamless as FFI can possibly get.

**Why Omni can't copy it directly**: Zig is a compiled language with a C compiler built in. Omni is interpreted/JIT'd. But the *idea* — parse the header, make functions available — is already half-implemented via `--bind`.

### Common Lisp CFFI — The Veteran

```lisp
(cffi:define-foreign-library libc (:unix "libc.so.6"))
(cffi:use-foreign-library libc)

(cffi:defcfun "strlen" :size (s :string))
(cffi:defcfun ("sqrt" c-sqrt) :double (x :double))

(strlen "hello")   ; => 5
(c-sqrt 2.0)       ; => 1.4142...
```
**Why it's great**: `defcfun` is one line per function. After declaration, calls look like normal Lisp. Auto name translation (`get_machine` → `get-machine`). 20+ years of production use.

**What Omni can steal**: The auto kebab-case name translation. The one-line-per-function binding pattern.

---

## Ergonomic Ranking (Best to Worst)

| Rank | System | Seamlessness | Why |
|------|--------|-------------|-----|
| 1 | **Zig** | Call C as if it's Zig | Compiler reads headers directly |
| 2 | **LuaJIT** | Paste C decls, call directly | `cdef` + JIT = zero overhead |
| 3 | **Janet** | `defbind` one-liners | Clean, minimal, contextual |
| 4 | **CL CFFI** | `defcfun` one-liners | Proven, auto naming |
| 5 | **Racket** | Safe but verbose | Types everywhere, two APIs |
| 6 | **Omni (current)** | Per-call type annotations | Handles, string names, interleaved types |

---

## What "Most Seamless FFI in Existence" Would Look Like for Omni

### Design Principles (combining the best ideas)

1. **From Zig**: Parse the header, make it available — `--bind` already does this, needs polish
2. **From LuaJIT**: Declare once, call naturally — no per-call type annotations
3. **From Janet**: `defbind` one-liners — single form creates callable function
4. **From the Old Proposal**: Ownership annotations, handle safety, callbacks
5. **From Omni itself**: Integrate with dispatch, effects, and the type system

### Proposed User Experience

**Level 0: Instant (for common libraries)**
```lisp
;; --bind already generated lib/ffi/math.omni from headers
(import ffi-math)
(ffi-math.sqrt 2.0)         ; just works — types known from header
(ffi-math.strlen "hello")   ; => 5
```
This already mostly works. Needs: better parameter names, kebab-case, qualified import by default.

**Level 1: One-Line Declaration (new `extern` form)**
```lisp
(extern libc "libc.so.6"
  (strlen [s ^String] -> ^Int)
  (sqrt [x ^Double] -> ^Double)
  (abs [n ^Int] -> ^Int))

(strlen "hello")       ; => 5  — no handle, no string name, no type at call site
(sqrt 2.0)             ; => 1.4142...
```
Key ideas:
- `extern` block declares library + functions in one form
- Uses Omni's existing `^Type` annotations (not new FFI-specific types)
- After declaration, functions are first-class — call like any Omni function
- Automatic Omni↔C type mapping: `^String` ↔ `char*`, `^Int` ↔ `long`, `^Double` ↔ `double`
- Names auto-kebab'd: C `get_machine` → Omni `get-machine`

**Level 2: Struct Declarations**
```lisp
(extern libc "libc.so.6"
  [struct Point (x ^Double) (y ^Double)]
  [struct Rect (origin ^Point) (size ^Point)]

  (make-point [x ^Double] [y ^Double] -> ^Point)
  (distance [a ^Point] [b ^Point] -> ^Double))

(define p (Point 3.0 4.0))    ; looks like any Omni type!
p.x                            ; => 3.0 — dot-path works
(distance p (Point 0.0 0.0))  ; => 5.0
```
FFI structs reuse Omni's `define [type]` machinery — same constructor, same dot-path access, same pattern matching. The struct just happens to have C-compatible memory layout.

**Level 3: Callbacks**
```lisp
(extern libc "libc.so.6"
  (qsort [base ^Ptr] [n ^Int] [size ^Int] [cmp (^Int ^Int -> ^Int)] -> ^Void))

;; Pass Omni lambda as C callback
(qsort arr n 8 (lambda (a b) (- a b)))
```
GNU Lightning generates a trampoline at runtime that calls back into the Omni evaluator.

**Level 4: Effects Integration (uniquely Omni)**
```lisp
;; FFI errors as effects — handlers can retry, log, or abort
(handle
  (let (db (sqlite-open "test.db"))
    (sqlite-exec db "SELECT * FROM users"))
  (ffi/error err
    (begin (println (format "FFI failed: %s" err))
           (resolve nil))))

;; FFI resource cleanup as effects
(handle
  (let (lib (ffi-open "libfoo.so"))
    (ffi-call lib "work" 'int))
  (ffi/cleanup lib (ffi-close lib)))
```
This is what no other Lisp has: FFI errors and resource cleanup integrated with the algebraic effect system.

### What This Combines

| Feature | Source |
|---------|--------|
| Parse headers → callable functions | Zig (`@cImport`), already have `--bind` |
| Declare once, call naturally | LuaJIT (`cdef`), Janet (`defbind`), CL (`defcfun`) |
| `extern` block syntax | New — combines library + functions in one form |
| Reuse `^Type` annotations | Omni's existing type system |
| Struct = Omni type | Omni's `define [type]` with C-compatible layout |
| Dot-path field access | Omni's existing path notation |
| Callbacks via lambda | Racket, LuaJIT (with trampoline codegen) |
| Effects for FFI errors | Unique to Omni |
| Handle-based safety | Old proposal (generation counters) |
| Auto kebab-case names | CL CFFI |

---

## Implementation Priority

### Phase 1: Make `--bind` Seamless (Easy, High Impact)
- Extract C parameter names from libclang (not arg0/arg1)
- Auto kebab-case C names (`omni_torch_zeros_1d` → `zeros-1d`)
- Better module naming (strip prefix)
- Qualified import by default: `(import ffi-math)` → `(ffi-math.sqrt 2.0)`

### Phase 2: `extern` Form (Medium, High Impact)
- New special form that declares library + bindings in one block
- Types map: `^Int` → `'int`, `^Double` → `'double`, `^String` → `'string`
- After `extern`, functions callable without handle/string-name/per-call types
- Mixed int/double args (extend dispatch table)

### Phase 3: Struct Marshalling (Hard, High Impact)
- `extern` struct declarations with C-compatible layout
- Reuse Omni `define [type]` machinery (constructor, dot-path, match)
- Pack/unpack for crossing FFI boundary

### Phase 4: Callbacks (Hard, High Impact)
- GNU Lightning trampoline codegen (closure → C function pointer)
- Lifetime management (prevent closure from being freed while C holds pointer)
- Function pointer type in `extern` declarations

### Phase 5: Effects Integration (Medium, Unique Selling Point)
- FFI errors as signalled effects (not just returned errors)
- Resource cleanup handlers
- `with-ffi` scope guard (macro over handle/signal)

---

## Ideas Worth Salvaging from FFI Proposal

### 1. Handle-Based Pointers (Priority: High)
Generation counters prevent use-after-free. Integrates with region system.
```lisp
;; Instead of raw 'ptr:
(define handle (ffi-alloc lib "malloc" 1024))  ; returns Handle, not raw int
(ffi-call lib "strlen" 'size handle 'handle)   ; validates before use
;; Accessing freed handle → error instead of crash
```

### 2. Ownership Annotations (Priority: Medium)
```lisp
;; Mark what caller must free vs what's borrowed
(define ^:from libc {extern strdup} [s 'string] ^:owned 'string)  ; caller owns result
(define ^:from libc {extern strlen} [s 'string] 'size)            ; borrows only
```

### 3. Callbacks (Priority: High)
Needed for any event-driven C library (GUI, I/O, sorting).
GNU Lightning could generate trampolines at runtime.

### 4. Scope Guards (Priority: Medium)
```lisp
(with-ffi lib (ffi-open "libfoo.so")
  (ffi-call lib "init" 'void)
  (ffi-call lib "work" 'int))
;; lib auto-closed on exit, even on error
```

### 5. `ffi-free` Primitive (Priority: High, Easy)
For C-allocated memory returned as `'string` or `'ptr`.

---

## Comparison to Other Lisps

| Aspect | Omni (current) | Omni (proposed) | Racket | LuaJIT | Janet | CL CFFI | Zig |
|--------|---------------|----------------|--------|--------|-------|---------|-----|
| Declare + call | 2 steps, verbose | 1 `extern` block | 2 steps | `cdef` block | `defbind` | `defcfun` | `@cImport` |
| Per-call types | Yes (painful) | No | No | No | No | No | No |
| Struct access | No | Dot-path | Accessors | Dot | ffi/read | slot-value | Direct |
| Callbacks | No | Lambda → C ptr | Yes | Yes | No | Yes | Yes |
| Auto naming | No | Kebab-case | No | No | No | Yes | Direct |
| Header import | `--bind` (separate step) | `--bind` (polished) | No | No | No | No | Built-in |
| Effects for errors | No | Yes (unique) | No | No | No | No | No |
| Safety | Raw ptrs | Handles + gen counters | GC | GC | Manual | GC | Compile-time |

---

## Verdict

The current FFI is **fit for purpose but not seamless**. It works for:
- Math libraries (libm)
- Simple POSIX calls (strlen, abs, etc.)
- C++ shim libraries (omni-torch pattern: thin C wrapper around C++ API)

It fails for:
- Complex APIs requiring struct marshalling
- Libraries expecting callbacks
- Mixed int/double calling conventions
- Variadic functions

The biggest quick wins: (1) polish `--bind` output, (2) `extern` form to eliminate per-call types, (3) mixed int/double args. The unique selling point: effects integration for FFI errors/cleanup — no other Lisp has this.
