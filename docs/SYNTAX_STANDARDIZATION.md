# OmniLisp Syntax Ambiguity & Consistency Report (Final)

## 1. Updated Problem Statement
OmniLisp exhibits ambiguity in bracket usage (`[]`, `{}`, `()`), especially when mixing value-level construction and type-level metadata. Recent feedback highlights:
1.  **Match Flatness**: Using `[]` for every branch in `match` is verbose. Flat "even number of forms" (like `let` or `cond`) is preferred.
2.  **Type Constructors**: `Option`, `List`, etc., are *type constructors*.
3.  **Metadata Tag**: The `^` prefix is used for metadata and must be clearly distinguished from type logic `{}`.

---

## 2. Refined Standardized System

| Character | Domain | Primary Uses |
| :--- | :--- | :--- |
| **`[]`** | **Bindings & Parameters** | Array literals, **argument lists** (`define`, `lambda`), binding pairs (`let`). |
| **`{}`** | **Type Logic** | Type annotations, **type constructor application**, type-level headers. |
| **`()`** | **Execution & Flow** | Value-level function calls, special forms, **constructor patterns** (logic). |
| **`^`** | **Metadata Prefix** | Attaches non-type metadata (hints, docstrings, visibility) to bindings or forms. |

### Rule 1: Flat Match Form
**`match` uses an even number of forms for branches.**
*   *Correct*: `(match opt (Some v) v None default)`

### Rule 2: Function Parameters (The "Argument Vector")
**Use `[]` for function arguments.**
*   *Correct*: `(define add [x y] (+ x y))`

### Rule 3: Type Constructor Application
**Type constructors use `{}` when applied at the type level.**
*   *Value level (Logic)*: `(Option Int)` -> Returns a type object.
*   *Type level (Metadata)*: `[x {Option Int}]` -> Annotation.

### Rule 4: Metadata via `^`
**The `^` tag is for "Extra-Logic" metadata.** It describes *how* something should be handled by the compiler or visibility, whereas `{}` describes *what* the data is (its type).
*   *Type (What)*: `[x {Int}]`
*   *Metadata (How/Who)*: `^:private [x 10]`
*   *Combined*: `^:private [x {Int} 10]` (A private binding `x` of type `Int`).

---

## 3. Comparative Examples (Before vs. After)

### 3.1 Function Definition & Annotation
**Before:**
```lisp
(define (add [x {Int}] [y {Int}]) {Int}
  (+ x y))
```

**After (Proposed):**
```lisp
(define add [x {Int} y {Int}] {Int}
  (+ x y))
```

### 3.2 Metadata and Types
**Before (Ambiguous):**
```lisp
(define ^:private ^:hot version 1.0)
```

**After (Standardized):**
*The metadata `^:private` attaches to the name/binding, `{Float}` attaches to the type logic.*
```lisp
(define ^:private ^:hot [version {Float} 1.0])
```

### 3.3 Type Constructor Application (Nesting)
**Before:**
```lisp
[result {(Option (List Int))}]
```

**After (Proposed):**
```lisp
[result {Option {List Int}}]
```

---

## 4. Summary of Character Roles
*   **`[]`** : Binding (Assigning a name to a slot).
*   **`{}`** : Type (Defining the logic of the slot).
*   **`()`** : Action (Executing a process).
*   **`^`**  : Tag (Annotating with side-information).

---

## 5. Revised Proposal: Position-Based Match & Clean Type Syntax

**Status:** ACCEPTED (revised after review)
**Authors:** Original proposal + Claude (code review)
**Date:** 2026-01-06

### 5.1 Match: Position-Based Pairs (cond-style)

~~Original counter-proposal claimed flat match was ambiguous.~~ **Correction:** Position-based parsing resolves ambiguity.

Like `cond`, match uses alternating pattern/expression pairs. Odd positions are patterns, even positions are results:

```lisp
;; Position-based (like cond)
(match x
  (Some v)  (process v)      ; pos 1: pattern, pos 2: result
  None      default          ; pos 3: pattern, pos 4: result
  1         "one")           ; pos 5: pattern, pos 6: result

;; Compare to cond
(cond
  (< x 0)   "negative"
  (> x 0)   "positive"
  else      "zero")
```

**Guards** become part of the pattern form:
```lisp
(match x
  (Some v :when (> v 10))  (big-process v)
  (Some v)                  (process v)
  None                      default)
```

**Why this works:**
- Parser knows boundaries by position (no lookahead needed)
- Consistent with `cond` which already uses this style
- Less bracket noise than `[pattern result]`
- Guards integrate naturally into pattern form

### 5.2 Type Annotations: `{Type expr}` Without Extra Parens

~~Original counter-proposal claimed nested `{}` was confusing.~~ **Correction:** The proposal doesn't nest `{}`. It uses `{}` as wrapper with normal application inside.

```lisp
;; OLD (redundant outer parens)
[result {(Option (List Int))}]

;; NEW (cleaner)
[result {Option (List Int)}]

;; Simple cases - implicit application
{Int}                    ; simple type
{Option Int}             ; Apply Option to Int
{Dict String Int}        ; Apply Dict to String and Int
{Option (List Int)}      ; Apply Option to (Apply List to Int)
```

**Clarification: `()` is Application, Not Grouping**

In Lisp, `(f x)` means "apply f to x", not "group f and x":

```lisp
(List Int)              ; Apply type constructor List to Int → a type
(Option (List Int))     ; Apply Option to (Apply List to Int)
(+ 1 2)                 ; Apply + to 1 and 2 → 3
```

| Bracket | Meaning | NOT |
|---------|---------|-----|
| `()` | Application (call/construct) | ~~Grouping~~ |
| `{}` | Type annotation wrapper | Application |
| `[]` | Arguments/parameters | Application |

**Rule:** `{}` marks "this is a type annotation". Inside, `()` is type application.

### 5.2.1 Nested `{}` for Higher-Kinded Types

Nested `{}` distinguishes **type constructors** (unapplied, kind `* -> *`) from **types** (fully applied, kind `*`):

```lisp
;; Fully applied types (kind *)
{List Int}               ; List applied to Int = a type
{Option String}          ; Option applied to String = a type

;; Unapplied type constructors (kind * -> *)
{List}                   ; List itself, not applied
{Option}                 ; Option itself, not applied

;; Higher-kinded types: passing constructors, not types
{Functor {List}}         ; Functor applied to constructor List
{Monad {Option}}         ; Monad applied to constructor Option
{Traversable {List}}     ; Traversable applied to constructor List

;; Without nested {} - ambiguous
{Functor List}           ; Is List a type or a constructor?
```

**When to use `{T}` inside `{}`:**

| Expression | Meaning | Kind |
|------------|---------|------|
| `{List Int}` | List of Int | `*` (type) |
| `{List}` | List constructor | `* -> *` (constructor) |
| `{Functor List}` | Ambiguous | ? |
| `{Functor {List}}` | Functor of List-constructor | `(* -> *) -> Constraint` |

**Rule:** Use nested `{}` when passing a type constructor (not a type) to a higher-kinded type.

### 5.3 Explicit Type Arguments: `[T]` (Optional)

The `[]` syntax marks **type parameters** explicitly, both in annotations and value-level calls.

#### In Type Annotations: `{Type [Params]}`

```lisp
;; Simple cases - brackets optional
{Option Int}              ; OK: unambiguous
{Dict String Int}         ; OK: unambiguous

;; When brackets help
{Array [Float 2]}         ; type param + dimension param
{Tensor [Float [3 224 224]]}  ; type + shape
{Fn [Int Int] Int}        ; distinguish param types from return type
```

**Why `{Type [Params]}` in annotations:**

| Case | Without `[]` | With `[]` | Why brackets help |
|------|--------------|-----------|-------------------|
| Mixed args | `{Array Float 2}` | `{Array [Float] 2}` | Is `2` a type or dimension? |
| Function types | `{Fn Int Int Int}` | `{Fn [Int Int] Int}` | Which `Int` is return type? |
| Nested params | `{Dict String List Int}` | `{Dict [String (List Int)]}` | Grouping is clear |
| Shape params | `{Tensor Float 3 224 224}` | `{Tensor [Float [3 224 224]]}` | Type vs shape |

**Rule:** Use `[]` when the type constructor takes multiple kinds of arguments (types, dimensions, shapes) to disambiguate.

#### In Value-Level Calls: `(constructor [T] args)`

```lisp
;; Usually inferred - no brackets needed
(define x (None))              ; inferred as {Option ?}
(define items (list))          ; inferred as {List ?}

;; Explicit when ambiguous
(define x (None [Int]))        ; explicitly {Option Int}
(define items (list [String])) ; explicitly {List String}

;; Useful for polymorphic functions
(identity [Int] 42)            ; force Int, not inferred
(empty [String])               ; empty collection of String

;; Generic function definition (type params in signature)
(define (map [A B]) {List B}
  [f {Fn A B}]
  [xs {List A}]
  ...)
```

**When to use `[T]`:**
| Situation | Syntax | Notes |
|-----------|--------|-------|
| Type is inferred | `(None)` | No brackets needed |
| Inference ambiguous | `(None [Int])` | Explicit type arg |
| Polymorphic call | `(empty [String])` | Specify element type |
| Generic definition | `(define (f [A B]) ...)` | Declare type params |
| Mixed param types | `{Array [Float] 2}` | Separate type from value params |

**Why `[]` for type args:**
- Consistent: `[]` already means "arguments" (bindings, params, arrays)
- Disambiguating: Separates type params from value params
- Optional: Only needed when inference fails or args are mixed
- Explicit: Clearly marks "this is a type argument, not a value"

### 5.4 Summary of Accepted Changes

| Feature | Old Syntax | New Syntax | Status |
|---------|------------|------------|--------|
| Match branches | `[pattern result]` | `pattern result` (positional) | **ADOPT** |
| Type annotation | `{(Option (List Int))}` | `{Option (List Int)}` | **ADOPT** |
| Explicit type args | N/A | `(None [Int])` | **ADOPT** (optional) |
| `[]` for bindings | ✅ | ✅ | Already implemented |
| `^` for metadata | ✅ | ✅ | Already implemented |

### 5.5 Implementation Checklist

- [ ] Update parser: match uses position-based pairs
- [ ] Update parser: `{}` contains type expression directly (no mandatory outer parens)
- [ ] Add parser support: `[T]` for explicit type application
- [ ] Update SYNTAX.md with new canonical forms
- [ ] Update existing examples in codebase
- [ ] Add tests for edge cases (guards, nested types, explicit type args)
