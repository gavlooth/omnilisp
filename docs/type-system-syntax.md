# Type System Syntax Proposal

This document proposes syntax for completing the Julia-style type system
and optionally extending to higher-kinded types.

## Design Principles

1. **Reuse existing constructs** - quote, metadata maps, define attributes
2. **Type position vs value position** - context determines interpretation
3. **Minimal new tokens** - no special syntax like `{T}` for generics
4. **Lisp-like** - parentheses for application, including type application
5. **Flat metadata** - bounds inline in metadata dict, explicit grouping only when ambiguous

---

## Type System Syntax

### 1.1 Type Annotations

```lisp
; Simple type annotation
^Int x
^String name

; Compound types use parentheses
^(List Int) items
^(Dict String Int) cache
^(Lambda Int Int) successor      ; function type
```

### 1.2 Function Types

Use `Lambda` to match value-level syntax:

```lisp
; Value level
(lambda (x) (* x 2))

; Type level
^(Lambda Int Int) inc            ; Int -> Int
^(Lambda Int Int Int) add        ; Int -> Int -> Int (curried)
^(Lambda A B) fn                 ; polymorphic
```

### 1.3 Abstract Type Hierarchy

```lisp
; Root abstract type
(define [abstract] Number)

; With parent (parenthesized form)
(define [abstract] (Real Number))
(define [abstract] (Integer Real))
```

### 1.4 Concrete Struct Types

```lisp
; Simple struct
(define [type] Point
  (^Float x)
  (^Float y))

; Inheritance (parenthesized form)
(define [type] (Point3D Point)
  (^Float z))
```

### 1.5 Parametric Types

```lisp
; Single parameter
(define [type] (Box T)
  (^T value))

; Multiple parameters
(define [type] (Pair A B)
  (^A first)
  (^B second))

; Type application in annotations
^(Box Int) intBox
^(Pair String Int) entry
```

**Construction** — infer type from arguments:

```lisp
(Box 42)                    ; infers Box{Int}
(Pair "key" 42)             ; infers Pair{String, Int}
(^(Box Int) (Box 42))       ; explicit when needed
```

### 1.6 Type Constraints

Bounds are specified directly in metadata (flat by default):

```lisp
; Single constraint - flat in metadata
^{'T Number}                            ; T <: Number

; With type annotation
^{'type (Lambda T T) 'T Number}         ; type + bound

; Multiple constraints
^{'T Number 'U Comparable}

; On a method
(define [method] ^{'T Number}
  double (^T x)
  (+ x x))

; Constraint on parametric type
(define [type] ^{'T Comparable}
  (SortedSet T)
  (^(List T) items))
```

**Explicit `'with` only when ambiguous** (rare):

```lisp
; If type variable conflicts with metadata key
^{'type (Lambda pure pure) 'with {'pure Number} 'pure true}

; For visual grouping (optional style choice)
^{'type (Lambda T U V)
  'with {'T Number 'U Comparable 'V T}}
```

### 1.7 Union Types / Sum Types

```lisp
; Simple union (enum-like)
(define [union] Bool True False)

; Parametric union
(define [union] (Option T)
  None
  (Some T))

; Multiple type parameters
(define [union] (Either E A)
  (Left E)
  (Right A))

; Construction
None                  ; nullary variant
(Some 42)             ; variant with value

; Pattern matching
(match opt
  (None default)
  ((Some x) (use x)))
```

### 1.8 Type Aliases

```lisp
; Simple alias
(define [alias] Text String)

; Parametric alias
(define [alias] (Maybe T) (Option T))

; Complex alias
(define [alias] (Result T) (Either String T))
```

---

## Metadata Value Disambiguation

When metadata is flat, the **value type** determines meaning:

| Value Type | Meaning | Example |
|------------|---------|---------|
| Type name (symbol) | Type bound | `'T Number` → T <: Number |
| Boolean | Metadata flag | `'pure true` |
| String | Metadata info | `'doc "description"` |

```lisp
; All flat, unambiguous
^{'type (Lambda T T) 'T Number 'pure true 'inline true}
;        ↑ type        ↑ bound   ↑ flag    ↑ flag
```

---

## Summary: Final Syntax

| Feature | Syntax | Example |
|---------|--------|---------|
| Type annotation | `^Type` | `^Int x` |
| Compound type | `^(Ctor Args)` | `^(List Int) xs` |
| Function type | `^(Lambda Args Ret)` | `^(Lambda Int Int) f` |
| Type bound | `'T Type` (flat) | `^{'T Number}` |
| Multiple bounds | `'T Type 'U Type` | `^{'T Number 'U Eq}` |
| Explicit grouping | `'with {...}` | `^{'with {'T Number}}` |
| Abstract type | `(define [abstract] ...)` | `(define [abstract] Number)` |
| Struct type | `(define [type] ...)` | `(define [type] Point ...)` |
| Parametric type | `(define [type] (Name T) ...)` | `(define [type] (Box T) ...)` |
| Union type | `(define [union] ...)` | `(define [union] (Option T) ...)` |
| Type alias | `(define [alias] ...)` | `(define [alias] Text String)` |

---

## Complete Example

```lisp
; Abstract hierarchy
(define [abstract] Number)
(define [abstract] (Real Number))

; Parametric struct (type params stored, not substituted)
(define [type] (Box T)
  (^T value))

; Union type
(define [union] (Option T)
  None
  (Some T))

; Multiple dispatch
(define (double (^Int x)) (+ x x))
(define (double (^Double x)) (+ x x))

; Union pattern matching
(match (Some 42)
  (None "empty")
  ((Some x) x))          ; => 42

(double 21)              ; => 42 (dispatches to Int version)
```

---

## Implementation Status (as of 2026-02-20)

### Implemented (Phases 1-7)
- [x] TypeRegistry wired to Interp with FNV-1a hash lookup
- [x] `infer_value_type()` maps runtime values to TypeId
- [x] `[type]` struct definitions with typed fields and constructors
- [x] `[abstract]` type definitions with parent hierarchy
- [x] `[union]` ADT definitions with variant constructors
- [x] `[alias]` type alias definitions
- [x] `^Type` annotation parsing (simple, compound `^(List Int)`, Val `^(Val 42)`)
- [x] `^{'T Number}` flat metadata dict parsing in parser
- [x] Multiple dispatch via MethodTable (typed `define` creates dispatch entries)
- [x] Val dispatch `^(Val N)` for value-level pattern matching
- [x] Multi-argument dispatch
- [x] Dispatch scoring: Val=1000, exact=100, subtype=10, any=1
- [x] Struct field access via dot-path: `point.x`, `line.start.y`
- [x] Struct field mutation: `(set! point.x 99)`, nested paths
- [x] Constructor pattern matching: `(match opt (None 0) ((Some x) x))`
- [x] Nullary constructor auto-detection in patterns (e.g., `None`)
- [x] I/O effects: print/println/etc. go through `perform` with fast path
- [x] `type-of`, `is?`, `instance?`, `type-args` introspection primitives
- [x] Parametric types: `(define [type] (Box T) (^T value))` with type param collection
- [x] Type arg inference: `(type-args (Box 42))` → `'(Int)` — inferred from field values
- [x] Constrained dispatch: `^{'T Number}` enforced at dispatch — arg type must be subtype of bound
- [x] Parent vs type-param disambiguation at eval time (first symbol = registered type → parent)
- [x] 100+ type/dispatch/effect tests all passing

### NOT Implemented (Future)
- [ ] Type substitution algorithm — `(Box T)` + T=Int not checked at construction
- [ ] `[effect]` attribute — effects are untyped
- [ ] `Lambda` function types — annotation exists but no checking

### Implementation Notes

**Dispatch mechanism**: When a typed `define` is encountered, the evaluator checks if the name is already bound. If bound to a closure, it promotes to a MethodTable with the existing closure as fallback. If already a MethodTable, adds a new entry. This means dispatch is backwards-compatible — untyped defines work exactly as before.

**Constructor patterns**: The parser generates PAT_CONSTRUCTOR for `(ConstructorName sub-patterns...)`. In match_pattern, it checks if the value is an INSTANCE whose type matches the constructor name, then recursively matches sub-patterns against fields. Nullary constructors like `None` are detected in the PAT_VAR case by looking up the symbol in the type registry.

**I/O effects**: Primitives are registered as `__raw-print`, `__raw-println`, etc. Stdlib wrappers (`print`, `println`, etc.) call `(perform io/print x)`. In `eval_perform`, before searching the handler stack, a fast path checks if the tag is an I/O effect symbol and no user handler is installed — if so, calls the raw primitive directly.
