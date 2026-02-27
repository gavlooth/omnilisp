# FFI Redesign — Effects-Driven, Seamless Foreign Functions

## Existing Patterns This Builds On

**Attributes** (parser recognizes these today):
```lisp
(define [type] Point (^Int x) (^Int y))
(define [type] (Circle Shape) (^Int radius))    ; with parent
(define [abstract] Shape)
(define [union] (Option T) None (Some T))
(define [alias] Num Int)
(define [macro] when ([test .. body] (if test (begin .. body) nil)))
(define [effect] (io/print (^Any x)))            ; stdlib.lisp:7
(define [effect] (raise (^Any msg)))             ; stdlib.lisp:17
```

**Dispatch** (typed parameters → multiple dispatch via MethodTable):
```lisp
(define (area (^Circle c)) (* pi (* c.radius c.radius)))
(define (area (^Rect r)) (* r.width r.height))
(area (Circle 5))   ; dispatches to Circle variant
```

**Effects** (`signal`/`handle`/`resolve` — body runs on stack engine, signal suspends, resolve resumes):
```lisp
(handle (+ 1 (signal read nil))
  (read x (resolve 41)))   ; => 42
```

The FFI should reuse ALL of these, not invent parallel mechanisms.

---

## Attribute Hierarchy

```
[ffi lib]       — library handle (dlopen)
[ffi λ libc]    — callable from library (dlsym + typed wrapper)
[ffi struct]    — C-layout struct type
[ffi opaque]    — typed pointer wrapper (no field access)
```

Attributes compose naturally with spaces:
- `[ffi λ libc]` — this is ffi, callable (λ), from libc
- `[ffi λ libc owned]` — also returns owned memory
- `[ffi λ libc consumes]` — takes ownership of argument
- `[ffi λ libc cleanup fclose]` — associated cleanup function

---

## Syntax

### Library Declaration

```lisp
(define [ffi lib] libc "libc.so.6")
(define [ffi lib] libm "libm.so"  )
(define [ffi lib] sdl  "libSDL2.so")
```

Same pattern as `define [type]` — an attribute, a name, a value.

### Function Binding

```lisp
(define [ffi λ libc] (strlen (^String s)) ^Int)
(define [ffi λ libm] (sqrt (^Double x)) ^Double)
(define [ffi λ libc] (abs (^Int n)) ^Int)
(define [ffi λ libc] (free (^Ptr p)))               ; void = no return annotation
```

Breakdown:
- `[ffi λ libc]` — ffi callable from libc
- `(strlen (^String s))` — existing typed parameter syntax
- `^Int` — return type, placed after params (no body — it's foreign)
- Auto kebab-case: C `get_machine` → Omni `get-machine`

After declaration:
```lisp
(strlen "hello")   ; => 5
(sqrt 2.0)         ; => 1.4142...
```

First-class function. No handle. No string name. No per-call types.

### Grouping — Use Modules (Already Exist)

```lisp
(module math-ffi (export sqrt sin cos pow)
  (define [ffi lib] _lib "libm.so")
  (define [ffi λ _lib] (sqrt (^Double x)) ^Double)
  (define [ffi λ _lib] (sin  (^Double x)) ^Double)
  (define [ffi λ _lib] (cos  (^Double x)) ^Double)
  (define [ffi λ _lib] (pow  (^Double x) (^Double y)) ^Double))

(import math-ffi :all)
(sqrt 2.0)
```

No new "FFI block" syntax needed.

---

## Structs

### FFI-Compatible Structs

```lisp
(define [ffi struct] Point (^Double x) (^Double y))
(define [ffi struct] Rect  (^Point origin) (^Point size))
```

This IS `define [type]` but guarantees C-compatible memory layout (field order, alignment, padding). Everything else works identically:

```lisp
(define p (Point 3.0 4.0))
p.x                              ; dot-path access
(match p ((Point x y) (+ x y)))  ; pattern matching
(set! p.x 99.0)                  ; mutation
```

When crossing the FFI boundary: zero-copy, the memory IS the C struct.

### Opaque Types

```lisp
(define [ffi opaque] FILE)
(define [ffi opaque] sqlite3)
```

A named wrapper around `^Ptr`. No field access, no construction — just type safety:

```lisp
(define [ffi λ libc] (fopen  (^String path) (^String mode)) ^FILE)
(define [ffi λ libc] (fclose (^FILE f)) ^Int)

(define f (fopen "/tmp/test" "w"))
(fclose f)       ; OK — types match
(strlen f)       ; ERROR: expected ^String, got ^FILE
```

---

## Callbacks — Pass Lambdas, Use Dispatch

No cryptic bracket syntax. Callbacks are just parameters with the `^λ` type — the same symbol Omni already uses to display closures:

```lisp
(define [ffi λ libc] (qsort (^Ptr base) (^Int n) (^Int size) (^λ cmp)) ^Void)
```

`^λ` means "this parameter is a callable." Pass a lambda or a named function:

```lisp
;; Lambda:
(qsort arr 10 8 (lambda (a b) (- (deref-int a) (deref-int b))))

;; Named function:
(define (int-compare a b) (- (deref-int a) (deref-int b)))
(qsort arr 10 8 int-compare)
```

Under the hood, the runtime wraps the closure in a C-callable entry point. The user doesn't know or care how.

**Parser support**: `λ` (U+03BB) is already a valid symbol — `is_symbol_char` accepts UTF-8 bytes `>= 0x80` (parser.c3:105), and `λ` is explicitly recognized as `lambda` (parser.c3:840). So `^λ` parses naturally as a type annotation. `^Lambda` is an equivalent alias for those who prefer ASCII.

### Callbacks + Dispatch

Since callbacks are just functions, dispatch works naturally:

```lisp
;; Define comparison for different types via dispatch:
(define (compare (^Int a) (^Int b)) (- a b))
(define (compare (^String a) (^String b))
  (if (< a b) -1 (if (> a b) 1 0)))

;; Pass dispatched function as callback:
(qsort int-array 10 8 compare)      ; dispatches to Int variant
(qsort str-array 10 8 compare)      ; dispatches to String variant
```

This is something no other FFI can do — callbacks that automatically dispatch on argument type using Omni's existing MethodTable.

### Typed Callback Signatures

For more precision, use metadata dict annotations (already supported by the parser):

```lisp
;; Untyped — any callable:
(define [ffi λ libc] (qsort ... (^λ cmp)) ^Void)

;; With constraints — validates arity and return type:
(define [ffi λ libc] (qsort ... (^{'arity 2 'returns Int} cmp)) ^Void)
```

No new compound form needed — `^{...}` metadata dicts are already parsed and stored (`parser.c3:555`, `MetaEntry` in `value.c3:370`).

---

## Effects for FFI — Opt-in Safety

### FFI Errors as Effects

When a C call fails, signal through the existing `raise` effect:

```lisp
;; FFI load failure — just signals raise:
(handle
  (define [ffi lib] db "libsqlite3.so")
  (raise msg (begin (println (format "Can't load: %s" msg)) (resolve nil))))

;; No handler = crashes cleanly (same as unhandled raise today)
(strlen "hello")   ; if dlsym fails, unhandled error
```

No need for special `ffi/error` effects — `raise` already exists and works.

### Resource Cleanup via Effects

```lisp
;; Declare that a function returns something that needs cleanup:
(define [ffi λ libc cleanup fclose] (fopen (^String path) (^String mode)) ^FILE)
```

The `cleanup` attribute names the cleanup function. A stdlib macro uses this:

```lisp
(define [macro] with-resource
  ([binding .. body]
    (let (name# (car binding)
          init# (car (cdr binding)))
      `(let (,name# ,init#)
         (handle
           (begin ,@body)
           (ffi/cleanup resource# (resolve nil)))))))

;; Usage:
(with-resource (f (fopen "/tmp/test" "r"))
  (read-all f))
;; f auto-closed on scope exit — cleanup function called via effect
```

How it works: when the `handle` body completes (normally or via signal), the runtime signals `ffi/cleanup` for any resources with the `cleanup` attribute. The handler calls the cleanup function and resolves. No finalizers, no GC hooks — just effects.

### Ownership Tracking via Effects

```lisp
(define [ffi λ libc owned]    (malloc (^Int size)) ^Ptr)     ; caller must free
(define [ffi λ libc owned]    (strdup (^String s)) ^String)   ; caller must free
(define [ffi λ libc consumes] (free (^Ptr p)))                ; takes ownership
```

**Default behavior (no handler)**: zero overhead. Works exactly like current FFI.

**With tracking handler**: the runtime signals on allocate/free boundaries:

```lisp
;; Effect declarations (in stdlib):
(define [effect] (ffi/acquire (^Ptr resource)))
(define [effect] (ffi/release (^Ptr resource)))

;; Strict mode — wrap your code in a handler:
(handle
  (let (buf (malloc 1024))
    (work-with buf)
    ;; forgot to free!
    42)
  (ffi/acquire ptr (begin (track ptr) (resolve ptr)))
  (ffi/release ptr (begin (untrack ptr) (resolve nil))))
;; on scope exit, any acquired-but-not-released → warning
```

Or as a stdlib convenience:

```lisp
(with-ffi-tracking
  (let (buf (malloc 1024))
    (work-with buf)))
;; prints: "Warning: leaked pointer from malloc"
```

**The key insight**: ownership strictness is the user's choice. No handler = no overhead. Install a handler = get tracking. The effect system makes this a spectrum, not a binary.

---

## Dispatch on FFI Types

FFI types participate in Omni's dispatch system:

```lisp
(define [ffi struct] Point (^Double x) (^Double y))
(define [ffi struct] Rect  (^Double x) (^Double y) (^Double w) (^Double h))

;; Dispatch works on FFI structs — same as any Omni type:
(define (area (^Rect r))  (* r.w r.h))
(define (area (^Point p)) 0)    ; points have zero area

;; Generic operations:
(define (describe (^FILE f)) "file handle")
(define (describe (^Ptr p))  "raw pointer")
```

This means FFI types are first-class citizens in Omni's type system, not second-class wrappers.

---

## Complete Examples — How It Feels

### Example 1: Simple C Calls

```lisp
(define [ffi lib] libc "libc.so.6")
(define [ffi λ libc] (strlen (^String s)) ^Int)
(define [ffi λ libc] (abs (^Int n)) ^Int)

(strlen "hello")   ; => 5
(abs -42)          ; => 42
```

Compare to current:
```lisp
(define libc (ffi-open "libc.so.6"))
(ffi-call libc "strlen" 'size "hello" 'string)    ; verbose, types everywhere
(ffi-call libc "abs" 'int -42 'int)
```

### Example 2: Resource Management

```lisp
(define [ffi lib] libc "libc.so.6")
(define [ffi λ libc cleanup fclose] (fopen (^String path) (^String mode)) ^FILE)
(define [ffi λ libc] (fclose (^FILE f)) ^Int)
(define [ffi λ libc] (fread (^Ptr buf) (^Int size) (^Int n) (^FILE f)) ^Int)

(with-resource (f (fopen "/tmp/data" "r"))
  (fread buf 1 1024 f))
;; f auto-closed — no leaks even if fread signals an error
```

### Example 3: Callbacks via Lambda

```lisp
(define [ffi lib] libc "libc.so.6")
(define [ffi λ libc] (qsort (^Ptr base) (^Int n) (^Int size) (^λ cmp)) ^Void)

(qsort numbers 10 8 (lambda (a b) (- (deref-int a) (deref-int b))))
```

Compare to LuaJIT:
```lua
local cb = ffi.cast("int(*)(const void*, const void*)", function(a, b)
  return ffi.cast("int*", a)[0] - ffi.cast("int*", b)[0]
end)
ffi.C.qsort(arr, 10, ffi.sizeof("int"), cb)
```

Compare to Racket:
```racket
(define cmp (ffi-callback (list _pointer _pointer) _int
  (lambda (a b) (- (ptr-ref a _int) (ptr-ref b _int)))))
(qsort arr 10 (ctype-sizeof _int) cmp)
```

Omni's is shorter and has no casts.

### Example 4: Callbacks + Dispatch (Uniquely Omni)

```lisp
;; Comparison dispatches on type:
(define (my-compare (^Int a) (^Int b)) (- a b))
(define (my-compare (^Double a) (^Double b))
  (if (< a b) -1 (if (> a b) 1 0)))

;; Same callback, different types:
(qsort int-arr 10 8 my-compare)      ; Int dispatch
(qsort float-arr 10 8 my-compare)    ; Double dispatch
```

No other FFI can dispatch callbacks on argument type.

### Example 5: SDL2 Game Loop with Effects

```lisp
(define [ffi lib] sdl "libSDL2.so")
(define [ffi opaque] SDL_Window)

(define [ffi λ sdl owned]    (SDL_CreateWindow (^String t) (^Int x) (^Int y) (^Int w) (^Int h) (^Int f)) ^SDL_Window)
(define [ffi λ sdl consumes] (SDL_DestroyWindow (^SDL_Window w)))
(define [ffi λ sdl] (SDL_PollEvent (^Ptr ev)) ^Int)

(with-resource (win (SDL_CreateWindow "Omni" 0 0 800 600 0))
  (handle (game-loop win)
    (raise msg
      (begin (println (format "Error: %s" msg))
             (resolve nil)))))
;; window auto-destroyed, errors handled — all via effects
```

### Example 6: PyTorch Bindings (What omni-torch Would Become)

**Current** (3 layers of boilerplate):
```lisp
;; Layer 1: Generated ffi-call wrappers (80 lines)
(define (omni-torch-zeros-1d (^Int arg0) (^Int arg1))
  (ffi-call _lib "omni_torch_zeros_1d" 'int arg0 'int arg1 'int))

;; Layer 2: Hand-written Tensor wrapper (200 lines)
(define (_wrap h)
  (let (nd (omni-torch-dim h) ...)
    (Tensor h (_query-shape h nd) nd nu dt)))

;; Layer 3: User code
(_wrap (omni-torch-zeros-1d 10 6))
```

**Proposed** (one clean layer):
```lisp
(module torch (export Tensor zeros ones ...)
  (define [ffi lib] _lib "libtorch_omni.so")
  (define [ffi opaque] TensorHandle)

  (define [ffi λ _lib] (raw-zeros-1d (^Int rows) (^Int dtype)) ^TensorHandle)
  (define [ffi λ _lib] (raw-dim (^TensorHandle h)) ^Int)
  (define [ffi λ _lib consumes] (raw-free (^TensorHandle h)))

  (define [type] Tensor (^TensorHandle handle) (^List shape) (^Int ndim))

  (define (zeros shape dtype)
    (let (h (raw-zeros-1d (car shape) dtype))
      (Tensor h shape (raw-dim h)))))

(import torch :all)
(with-resource (t (zeros '(10) torch/float32))
  (println t))
```

The generated layer (`--bind`) produces `define [ffi λ]` forms directly. The hand-written layer is just Omni code with dispatch and types.

---

## `--bind` Output (Updated)

**Current output:**
```lisp
(define (omni-torch-zeros-1d (^Int arg0) (^Int arg1))
  (ffi-call _lib "omni_torch_zeros_1d" 'int arg0 'int arg1 'int))
```

**Proposed output:**
```lisp
(define [ffi λ _lib] (zeros-1d (^Int rows) (^Int dtype)) ^Int)
```

Changes needed in bindgen:
1. Extract C parameter names from libclang AST (not arg0/arg1)
2. Strip common prefix, auto kebab-case
3. Emit `define [ffi λ]` instead of `ffi-call` wrappers
4. Detect ownership patterns → add `owned`/`consumes` attributes
5. Detect function pointer params → `^λ` type

---

## Implementation Phases

### Phase 1: `define [ffi lib]` + `define [ffi λ]` (High Impact, Medium)
- New attribute forms in parser (like existing `[type]`, `[effect]`)
- Build typed wrappers at eval time (pre-configured `ffi-call` under the hood)
- Auto kebab-case C names
- Mixed int/double args (extend calling convention dispatch table)
- After this: calls look native, no per-call type annotations

### Phase 2: `define [ffi struct]` + `define [ffi opaque]` (High Impact, Hard)
- C-compatible memory layout for structs
- Reuse `define [type]` constructor/accessor/match machinery
- Opaque types = typed `^Ptr` with no field access
- Zero-copy across FFI boundary

### Phase 3: FFI Effects — Errors + Cleanup (Medium Impact, Easy)
- FFI errors signal `raise` (already exists)
- `cleanup` attribute → `ffi/cleanup` effect on scope exit
- `with-resource` macro (sugar over handle)
- `owned`/`consumes` → `ffi/acquire`/`ffi/release` signals (opt-in)

### Phase 4: Callbacks via `^λ` (High Impact, Hard)
- Generate C-callable entry point for Omni closures
- `^λ` type annotation triggers wrapping
- Closures passed directly — no casts, no special types
- Dispatch works on callback arguments (unique to Omni)

### Phase 5: Polish `--bind` (Medium Impact, Easy)
- Emit `define [ffi λ]` forms
- Extract C parameter names from libclang
- Auto kebab-case, strip prefixes
- Detect function pointer params → `^λ`
- Detect ownership → `owned`/`consumes`

---

## Open Questions for Discussion

1. ~~**`^λ` signature syntax**~~ **RESOLVED**: `^λ` (untyped) is default. For constraints, use existing metadata dicts: `^{'arity 2 'returns Int}`. No new compound form needed.

2. ~~**Variadic C functions**~~ **RESOLVED**: `[variadic]` attribute, runtime infers C types from Omni values (string→char*, int→int, float→double per C promotion rules). Extensible later via metadata dicts if explicit types needed. Example: `(define [ffi λ libc variadic] (printf (^String fmt)) ^Int)`

3. ~~**Struct by value vs by pointer**~~ **RESOLVED**: Use libffi for calling convention handling. Gives correct struct passing (by value in registers for small structs, by pointer for large), mixed int/float args, variadic support, and cross-platform for free. No shims needed — direct C calls. Already depend on libclang, GNU Lightning, replxx — one more permissive dependency is fine.

4. ~~**Callback lifetime**~~ **RESOLVED**: Regions already use refcounting (`retain_region`/`release_region` in `main.c3:1564-1594`). When a closure is passed to C as a callback, the runtime retains the closure's region. When C is done (or `consumes` fires), release. Existing RC handles it — no new mechanism needed.

5. ~~**`with-resource` vs explicit close**~~ **RESOLVED**: `cleanup` attribute just registers the association (e.g., fopen↔fclose). Manual close always works. `with-resource` macro is a convenience that uses the registration. Both paths valid — matches effects philosophy (no handler = no overhead, opt-in safety).

6. ~~**Multiple return values / out-params**~~ **RESOLVED**: No special FFI support. Out-params are a C-ism — the FFI binding should be honest about C's interface (`^Ptr`). Ergonomic wrappers are just Omni code on top (e.g., `(define (read fd count) (let (buf (malloc count) n (raw-read fd buf count)) (list n buf)))`). Keeps FFI simple.
