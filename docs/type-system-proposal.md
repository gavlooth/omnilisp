# Julia-Style Type System - Syntax Proposal

> **Status Note (2026-02-19):** This document is a *design proposal*. Checkmarks (✓) indicate
> features that were **designed** and **syntax-agreed**, NOT necessarily implemented.
> See the Implementation Status section at the bottom for what actually works.
> The canonical syntax reference is `type-system-syntax.md`.

---

## 1. Parametric Types ✓ (syntax designed, type params stored but NOT substituted)

Scheme-style with parameters in name position:

```lisp
(define [type] (Point T)
  (^T x)
  (^T y))

(define [type] (Pair K V)
  (^K key)
  (^V value))

(define [type] (List T)
  (^T head)
  (^(Option (List T)) tail))
```

### Instantiation
```lisp
(Point Int)              ; type: Point{Int}
((Point Float) 1.0 2.0)  ; construct Point{Float}
```

---

## 2. Type Constraints ✓ (syntax designed, parsed but NOT enforced)

> **Implementation note:** The actual implementation uses flat metadata `^{'T Number}` syntax
> (from `type-system-syntax.md`), NOT the `:where` keyword syntax shown below. The `:where`
> syntax was an earlier proposal. Constraints are parsed but not checked at runtime.

Using `^{:where [...]}` metadata map with braces (PROPOSED, not implemented):

```lisp
; Single constraint - T must be subtype of Number
(define [method] ^{:where [T Number]}
  add (^T x ^T y)
  (+ x y))

; Multiple constraints as nested list
(define [method] ^{:where [[T Real] [U T]]}
  convert (^T x ^U target)
  ...)

; On parametric types
(define [type] ^{:where [T Number]}
  (NumericPair T)
  (^T first)
  (^T second))

; Combined metadata
(define [type] ^{:where [T Comparable], :mutable true}
  (SortedSet T)
  (^(List T) items))
```

### Metadata Syntax
- `^{...}` - brace-delimited metadata map
- `:keyword [value]` - keyword with list value
- `:keyword symbol` - keyword with symbol value
- `:keyword` - flag (implies true)

---

## 3. Union Types ✓

```lisp
(define [union] NumOrStr Int String)

(define [union] (Option T)
  None
  (Some T))

(define [union] (Result T E)
  (Ok T)
  (Err E))
```

---

## 4. Field Access (Path Expressions) ✓ IMPLEMENTED

Dot notation:

```lisp
point.x
point.y
person.address.city

; In expressions
(+ point.x point.y)
(set! counter.value (+ counter.value 1))  ; field mutation
```

---

## 4.1 Bracket Indexing ✓ IMPLEMENTED (dot-bracket syntax)

> **Implementation note:** Actual syntax uses dot-bracket `.[` (not bare bracket `[`).
> The `.[` token is a distinct lexer token (T_DOT_BRACKET).

```lisp
; Actual syntax (dot-bracket)
arr.[0]                  ; first element
arr.[i]                  ; variable index
matrix.[i].[j]           ; chained indexing
hashmap.['key]           ; hashmap key lookup
```

> Note: The bare `arr[0]` syntax from this proposal was NOT implemented.
> Use `arr.[0]` instead.

---

## 5. Mutable Types ✓ (syntax designed, NOT implemented as attribute)

> **Implementation note:** `[type mutable]` attribute is NOT implemented.
> All type instances are mutable by default — `(set! instance.field value)` works
> on any type without needing the `mutable` attribute.

```lisp
; Proposed (NOT implemented — no mutable attribute needed)
(define [type mutable] Counter
  (^Int value))

; Actual (works now — all types are mutable)
(define [type] Counter (^Int value))
(set! counter.value 10)  ; works without [type mutable]
```

---

## 6. Type Aliases ✓

```lisp
(define [alias] Real Float)
(define [alias] StringList (List String))
(define [alias] (Vec T) (Array T))
```

---

## 7. Function Types ✓ (syntax designed, NOT implemented)

> **Implementation note:** `Proc` is not a recognized keyword. The `type-system-syntax.md`
> uses `Lambda` as the function type constructor: `^(Lambda Int Int)`. Neither `Proc` nor
> `Lambda` function types are enforced at runtime — they exist only as annotations.

Using `Proc` keyword (PROPOSED):

```lisp
^(Proc Int Int)         ; Int -> Int
^(Proc Int Int Int)     ; (Int, Int) -> Int
^(Proc Int)             ; () -> Int

; Higher-order
^(Proc (Proc Int Int) Int)  ; (Int -> Int) -> Int

; In method signatures
(define [method] map (^(Proc A B) f ^(List A) xs) ...)
```

---

## 7.1 Pattern Matching ✓

```lisp
(match expr
  (None default)           ; nullary constructor
  ((Some x) x)             ; constructor with binding
  ((Pair a b) (+ a b))     ; multiple bindings
  (x x))                   ; variable pattern
```

---

## 8. Summary

| Feature | Syntax | Status |
|---------|--------|--------|
| Parametric types | `(define [type] (Point T) ...)` | ✓ |
| Constraints | `^{:where [T Number]}` | ✓ |
| Union types | `(define [union] Name ...)` | ✓ |
| Pattern matching | `(match expr ...)` | ✓ |
| Field access | `object.field` | ✓ |
| Bracket indexing | `arr[0]`, `dict[:key]` | ✓ |
| String literals | `"hello\nworld"` | ✓ |
| Float literals | `3.14`, `1.0e-5` | ✓ |
| Mutable | `[type mutable]` | ✓ |
| Aliases | `(define [alias] Name Type)` | ✓ |
| Function types | `^(Proc Int Int Int)` | ✓ |

---

## Memory Integration (ASPIRATIONAL — NOT IMPLEMENTED)

> The features below (`with-region`, `in-region`, `:destructor`) are NOT implemented.
> Memory is managed automatically via the region system in `src/main.c3`.

```lisp
; PROPOSED (not implemented)
(with-region r
  (define p (Point 1.0 2.0))
  (+ p.x p.y))

; PROPOSED (not implemented)
(define [type] ^{:destructor close-file}
  FileHandle
  (^Int fd))
```

---

## Implementation Status (2026-02-19)

| Feature | Syntax | Status |
|---------|--------|--------|
| Struct types | `(define [type] Point (^Int x) (^Int y))` | **IMPLEMENTED** |
| Abstract types | `(define [abstract] Shape)` | **IMPLEMENTED** |
| Type inheritance | `(define [type] (Circle Shape) (^Int r))` | **IMPLEMENTED** |
| Union types | `(define [union] (Option T) None (Some T))` | **IMPLEMENTED** |
| Type aliases | `(define [alias] Num Int)` | **IMPLEMENTED** |
| Field access | `point.x`, `line.start.y` | **IMPLEMENTED** |
| Field mutation | `(set! point.x 99)` | **IMPLEMENTED** |
| Dot-bracket indexing | `arr.[0]`, `map.['key]` | **IMPLEMENTED** |
| Multiple dispatch | Typed `define` with `^Type` params | **IMPLEMENTED** |
| Val dispatch | `^(Val 42)` for value matching | **IMPLEMENTED** |
| Pattern matching ctors | `(match opt (None 0) ((Some x) x))` | **IMPLEMENTED** |
| Type introspection | `type-of`, `is?`, `instance?` | **IMPLEMENTED** |
| Parametric types | `(define [type] (Box T) (^T value))` | Parsed, NOT substituted |
| Type constraints | `^{'T Number}` | Parsed, NOT enforced |
| `[type mutable]` | Mutable attribute | NOT needed (all mutable) |
| Function types | `^(Proc Int Int)` / `^(Lambda Int Int)` | NOT enforced |
| Bare bracket indexing | `arr[0]` | NOT implemented (use `arr.[0]`) |
| Destructors | `^{:destructor fn}` | NOT implemented |
| Region expressions | `with-region`, `in-region` | NOT implemented |
