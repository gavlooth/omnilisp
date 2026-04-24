# Overview, Data Types & Truthiness

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 1. Overview

### What is Omni?

Omni is a Lisp dialect that combines classic Lisp expressiveness with modern
language features:

- **Three collection types**: list `'(1 2 3)`, array `[1 2 3]`, dict `{a 1}`
- **Generic operations**: `(ref coll key)` works on all collections
- **Scientific tensors**: `(Tensor [[1 2] [3 4]])`, tensor `map`,
  `contract`, and `realize`
- **Multiple dispatch**: typed `define` creates method tables, best match wins
- **Algebraic effects**: `print`/`read-file` go through interceptable effects
- **First-class C FFI**: grouped `[ffi module]` declarations and
  `ForeignHandle` metadata
- **Strict-arity lambdas**: `(λ (x y) body)` requires exactly 2 args
- **Region-based memory**: no GC, deterministic cleanup
- **Truthiness**: only `nil` and `false` are falsy

### Build & Run

```bash
# Build from source (requires C3 compiler + GNU Lightning)
c3c build

# Run a script
omni script.omni

# Start the REPL (explicit)
omni --repl

# Start the REPL (default)
omni
```

### REPL

```
Omni Lisp REPL (type 'quit' or 'exit' to leave)
---
> (+ 1 2)
3
> (define (square x) (* x x))
#<closure>
> (square 5)
25
> (map (+ 1 _) '(1 2 3))
(2 3 4)
> quit
Goodbye!
```

---

## 2. Data Types

| Type | Tag | Literal | Description |
|------|-----|---------|-------------|
| nil | `NIL` | `nil`, `()` | Empty / absence value (`Nil` type symbol) |
| int | `INT` | `42`, `-17` | 64-bit signed integer |
| Float64 | `DOUBLE` | `3.14`, `-0.5` | 64-bit floating point |
| string | `STRING` | `"hello"` | Immutable string |
| symbol | `SYMBOL` | `'foo` | Interned identifier |
| cons | `CONS` | `'(1 2 3)` | Pair / linked list cell |
| closure | `CLOSURE` | `(λ (x) x)` | Function with captured environment |
| continuation | `CONTINUATION` | — | Captured via `capture` |
| primitive | `PRIMITIVE` | `+`, `car` | Built-in function |
| partial | `PARTIAL_PRIM` | `(partial + 3)` | Explicit partial value |
| error | `ERROR` | `(error "oops")` | Error value |
| Dictionary | `HASHMAP` | `{a 1}` | Mutable hash table |
| Array | `ARRAY` | `[1 2 3]` | Mutable dynamic array |
| Coroutine | `COROUTINE` | — | User-level coroutine |
| Tensor | `TENSOR` | `(Tensor [[1 2] [3 4]])` | Homogeneous n-dimensional numeric storage |
| ffi-handle | `FFI_HANDLE` | — | Foreign library handle |
| instance | `INSTANCE` | `(Point 3 4)` | User-defined type instance |
| method-table | `METHOD_TABLE` | — | Dispatch table (internal) |

---

## 3. Truthiness & Equality

### Truthiness

**Falsy:** `nil` and `false`.

**Truthy:** Everything else — including `0`, `""`, and `'()`.

```lisp
(if 0 "yes" "no")       ;; => "yes"
(if "" "yes" "no")       ;; => "yes"
(if '() "yes" "no")      ;; => "yes"
(if nil "yes" "no")      ;; => "no"
(if false "yes" "no")    ;; => "no"
```

### `Void` vs `Nil`

Normative rule:
- `Void` is for successful command/effect completion with no payload.
- `Nil` is for absence/query-miss (and falsy predicate misses).
- `Void` is operational-only (not an absence/data sentinel).
- `Void` is truthy.

```lisp
(type-of (block (define x 1) (set! x 2)))   ;; => Void
(type-of (let (d {a 1}) (remove! d 'a)))    ;; => Void
(if (block (define x 1) (set! x 2)) 1 0)    ;; => 1

(type-of (ref {a 1} 'missing))               ;; => Nil
(type-of (has? {a 1} 'missing))              ;; => Nil
```

### Equality

`=` performs structural equality:

```lisp
(= 42 42)            ;; => true
(= "hello" "hello")  ;; => true
(= '(1 2) '(1 2))   ;; => true
(= 42 42.0)          ;; => true (numeric comparison)
```
