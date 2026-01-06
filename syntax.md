# OmniLisp Syntax Reference

This document is the comprehensive syntax reference for OmniLisp, incorporating the Language Reference and Syntax Standardization specifications.

## Table of Contents

1. [The Character Calculus (Delimiters)](#the-character-calculus-delimiters)
2. [Comments & Whitespace](#comments--whitespace)
3. [Literals & Atoms](#literals--atoms)
4. [Bindings & Functions](#bindings--functions)
5. [The Trinity of Sequences](#the-trinity-of-sequences)
6. [Control Flow](#control-flow)
7. [Pattern Matching](#pattern-matching)
8. [Access & Mutation](#access--mutation)
9. [Metadata & Attributes](#metadata--attributes)
10. [Types](#types)
11. [String Formatting](#string-formatting)
12. [Reader Macros](#reader-macros)

---

## The Character Calculus (Delimiters)

To ensure total consistency, every delimiter has a fixed semantic "Charge":

| Character | Domain | Purpose | Examples |
|:---:|:---:|:---|:---|
| **`()`** | **Flow** | Execution of logic, function calls, special forms, and **Persistent Linked Lists** | `(f x y)`, `(cond ...)`, `'(1 2 3)` |
| **`[]`** | **Slot** | Argument vectors, `let` bindings, contiguous **Realized Arrays**, and Path indexing | `[x y]`, `[1 2 3]`, `arr.[idx]` |
| **`{}`** | **Kind** | Type system names, annotations, and parametric type construction | `{Int}`, `{List String}`, `{Option T}` |
| **`.`** | **Path** | The access operator used to reach or locate data | `obj.field`, `arr.[idx]` |
| **`^`** | **Tag** | Out-of-band metadata, visibility hints, and compiler instructions | `^:private`, `^:hot` |
| **`#`** | **Sign** | Reader tags for literals (Dicts, Formats) | `#fmt"..."`, `#{"a" 1}` |

### Summary of Character Roles

* **`[]`**: Binding (Assigning a name to a slot)
* **`{}`**: Type (Defining the logic of the slot)
* **`()`**: Action (Executing a process)
* **`^`**: Tag (Annotating with side-information)
* **`.`**: Path (Accessing nested data)
* **`#`**: Sign (Reader macros)

---

## Comments & Whitespace

### Comments

```lisp
;; Single-line comment from semicolon to end of line
#| Block comment
   can span multiple lines |#
```

### Whitespace

Whitespace includes spaces, tabs, newlines, and carriage returns. Whitespace is generally insignificant and used for readability.

```lisp
(define x 42)    ; Space between tokens
(define y
  (+ 1 2))      ; Newlines also work as whitespace
```

---

## Literals & Atoms

### Numbers

```lisp
42              ; Integer
-17             ; Negative integer
3.14            ; Float
+42             ; Explicit positive
```

### Symbols

Symbols are identifiers that start with a non-digit character and can contain letters, digits, and most operators.

```lisp
foo             ; Simple symbol
my-var          ; Hyphenated
some_func       ; Underscored
x!              ; With operator
->              ; Operator as symbol
```

**Reserved symbols** (evaluate to themselves):
```lisp
true            ; Boolean true
false           ; Boolean false
nothing        ; Void/nil value
```

### Characters

```lisp
#\a             ; Character 'a'
#\newline       ; Newline character
#\space         ; Space character
#\tab           ; Tab character
```

### Strings

```lisp
"hello"         ; Basic string
"line1\nline2"   ; With escape sequences
"\"quoted\""     ; With quotes inside
```

**String escapes:**
- `\n` - newline
- `\t` - tab
- `\"` - double quote
- `\\` - backslash
- `\xNN` - hexadecimal byte
- `\uNNNN` - unicode codepoint

---

## Bindings & Functions

### Variable Definition

```lisp
;; Simple definition
(define x 42)

;; With type annotation
(define x {Int} 42)

;; With metadata
(define ^:private version 1.0)

;; Combined metadata and type
(define ^:private [version {Float} 1.0])
```

### Function Definitions

Functions use the **Argument Vector `[]`** to clearly separate names from logic.

```lisp
;; Standard definition with parameters
(define add [x y]
  (+ x y))

;; With type annotations (Kinds in {})
(define add [x {Int} y {Int}] {Int}
  (+ x y))

;; With return type annotation
(define add [x {Int} y {Int}] {Int}
  (+ x y))

;; Lambda shorthand (->)
(-> [x] (* x x))

;; Full lambda
(lambda [x y] (+ x y))

;; Zero-argument function
(define greet []
  "Hello, World!")

;; With metadata
(define ^:hot ^:inline [square x] {Int}
  (* x x))
```

### Local Bindings (let)

`let` uses flat, even-numbered forms inside the **Slot Bracket `[]`**.

```lisp
;; Basic let
(let [x 10
      y 20]
  (+ x y))

;; With types
(let [x {Int} 10
      y {String} "hello"]
  y)

;; Nested let (sequential binding)
(let [x 10
      x (+ x 5)]    ; x is now 15
  x)
```

### Function Application

```lisp
;; Basic call
(add 1 2)

;; Call with expression
(add (+ 1 2) 3)

;; Nested calls
(* (add x y) 2)

;; Pipeline-style
(-> number
  (add 5)
  (* 2))
```

---

## The Trinity of Sequences

| Type | Syntax | Nature | Mutability |
|:---|:---|:---|:---|
| **List** | `'(1 2 3)` | Persistent, recursive, uses `()` | Immutable |
| **Array** | `[1 2 3]` | Contiguous, realized, indexed, uses `[]` | Immutable |
| **Iterator**| `(range 10)`| Transient, one-shot process, returns `{Iter T}` | Lazy |

### Lists

```lisp
;; List literal
'(1 2 3)

;; Nested list
'((1 2) (3 4))

;; List construction
(cons 1 '(2 3))   ; => '(1 2 3)

;; List operations
(car '(1 2 3))    ; => 1
(cdr '(1 2 3))    ; => '(2 3)
```

### Arrays

```lisp
;; Array literal
[1 2 3]

;; Nested array
[[1 2] [3 4]]

;; Array indexing
(arr.[0])         ; Zero-based index

;; Array from other sequences
(array (range 10))
(list '(1 2 3))
```

### Lazy Pipelines (Iterators)

Operations like `map` and `filter` return objects of type `{Iter T}`. They do not allocate storage unless explicitly "Materialized".

```lisp
;; No allocation - lazy iterator
(define lazy-flow (map inc (range 100)))

;; Materialize to Array
(define realized (array lazy-flow))      ; => [0 1 2 ... 99]

;; Materialize to List
(define linked (list lazy-flow))         ; => '(0 1 2 ... 99)

;; Chaining operations
(define result
  (-> (range 100)
    (filter odd?)
    (map (* 2))
    (array)))     ; => [2 6 10 ... 198]
```

---

## Control Flow

### Conditionals

```lisp
;; Basic if
(if (> x 0)
  "positive"
  "non-positive")

;; Multi-way with cond
(cond
  (< x 0)   "negative"
  (> x 0)   "positive"
  else      "zero")

;; When (single branch, no else)
(when (> x 0)
  (display "positive")
  (process x))

;; Unless
(unless (zero? x)
  (display "non-zero"))
```

### Loops

```lisp
;; Loop with recur
(define (sum-list lst)
  (let [total 0
         iter lst]
    (when (not-nil? iter)
      (let [total (+ total (car iter))
             iter (cdr iter)]
        (recur total iter)))  ; Recursive call at tail position
    total))
```

---

## Pattern Matching

### Match Syntax

`match` uses an **even number of forms** for branches (positional pairs like `cond`).

```lisp
;; Basic match
(match opt
  (Some v) v
  None    default)

;; Match with guards
(match x
  (Some v :when (> v 10))  (big-process v)
  (Some v)                  (process v)
  None                      default)

;; Match multiple values
(match [first rest]
  (nil)        "empty"
  (_ "more")   "has elements"
  _            "other")

;; Match literal values
(match x
  1     "one"
  2     "two"
  3     "three"
  else   "other")
```

**Position-based parsing rules:**
- Odd positions (1, 3, 5, ...) are patterns
- Even positions (2, 4, 6, ...) are result expressions
- Guards use `:when` inside the pattern form

### Pattern Types

```lisp
;; Literal patterns
(match x
  42   "exact match"
  "hi" "string match")

;; Variable patterns
(match x
  v    (process v))     ; v matches anything, binds to result

;; Wildcard
(match x
  _    "default")       ; _ matches anything, no binding

;; Constructor patterns
(match opt
  (Some value)  value    ; Destructure
  None          "empty")

;; As-pattern (binding)
(match lst
  (head :as tail)  (list head tail))  ; Bind both whole and part
```

---

## Access & Mutation

### Path-as-Locator

The dot `.` bridges **Flow** and **Slot**. It identifies a location in memory.

```lisp
;; Static field access
person.name

;; Dynamic array indexing
arr.[idx]

;; Symbolic key access
obj.[:key]

;; Chained paths
data.users.[0].name
```

### Mutation Operators

| Operator | Target | Effect | Return |
|:---|:---|:---|:---|
| **`set!`** | Name (Binding) | Modifies a variable | The **new value** |
| **`put!`** | Slot (Path) | Modifies a location | The **root object** |
| **`update!`** | Slot (Path) | Transforms in-place | The **root object** |
| **`update`** | Slot (Path) | Functional transform | A **new object** |

```lisp
;; set! - Modify a binding
(set! x 10)       ; Returns 10

;; put! - Modify a slot
(put! person.age 30)      ; Returns modified person

;; update! - Transform in place
(update! player.hp dec)   ; Returns player with decremented hp

;; update - Functional transform
(define p2 (update p1.x inc))   ; p1 unchanged, p2 is new
```

### Field Assignment

```lisp
;; Direct assignment
(= obj.field value)

;; Nested assignment
(= data.items.[0] new-item)

;; Destructuring assignment
(let [obj.[x y] point]
  (+ x y))
```

---

## Metadata & Attributes

### The `^` Tag

Used for information that does not change the "Kind" (Type) but changes the context.

```lisp
;; Visibility modifiers
(define ^:private [version 1.0])
(define ^:public [api-endpoint "/v1"])

;; Compiler hints
(define ^:hot [fib n] ...)
(define ^:inline [sq x] (* x x))

;; Documentation
(define ^:doc "Calculate factorial"
  [fact n] ...)

;; Multiple metadata tags
(define ^:private ^:hot ^:inline
  [helper x] ...)
```

### Combined with Types

```lisp
;; Metadata + Type + Value
(define ^:private [version {Float} 1.0])

;; Metadata + Function signature
(define ^:public [add {Int} {Int}] {Int}
  [x {Int} y {Int}]
  (+ x y))

;; Type constraints with metadata
(define ^:checked [x {Number (Positive)}] 42)
```

---

## Types

### Type Annotations

Type annotations use `{}` to wrap type expressions.

```lisp
;; Simple type
(define x {Int} 42)

;; Function type
[fn {Int} {Int} {Int}]

;; Parametric type
{x {Option String}}

;; Higher-order function
{Fn {Int} {Int}}
```

### Type Constructors

```lisp
;; Applied types (kind *)
{List Int}               ; List of Int
{Option String}          ; Optional String
{Dict String Int}         ; Map from String to Int

;; Type constructors (kind * -> *)
{List}                   ; List constructor itself
{Option}                 ; Option constructor

;; Higher-kinded types: passing constructors
{Functor {List}}         ; Functor applied to List
{Monad {Option}}         ; Monad applied to Option
```

### Nested Type Syntax

**Rule:** `()` is Application (call/construct), not grouping.

```lisp
;; Type application
(List Int)              ; Apply List to Int
(Option (List Int))     ; Apply Option to (List Int)

;; Type annotation wrapper
{List Int}              ; List of Int (type annotation)
{Option (List Int)}     ; Option of (List Int)

;; Higher-kinded with nested {}
{Functor {List}}        ; Functor of List-constructor
{Monad {Option}}        ; Monad of Option-constructor
```

### Explicit Type Arguments

The `[]` syntax marks **type parameters** explicitly.

```lisp
;; Usually inferred - no brackets needed
(define x (None))              ; inferred as {Option ?}
(define items (list))          ; inferred as {List ?}

;; Explicit when ambiguous
(define x (None [Int]))        ; explicitly {Option Int}
(define items (list [String])) ; explicitly {List String}

;; Polymorphic function call
(identity [Int] 42)            ; force Int type
(empty [String])               ; empty list of String

;; Generic function definition
(define (map [A B]) {List B}
  [f {Fn A B}]
  [xs {List A}]
  ...)
```

### Type Signatures

```lisp
;; Function with full signature
(define (map [A B]) {List B}
  [f {Fn A B}]
  [xs {List A}]
  ...)

;; Multiple type parameters
(define (fold [A B]) {B}
  [f {Fn B A B}]
  [init B]
  [xs {List A}]
  ...)

;; Constraints (future)
(define (sorted [T :Ord]) {List T}
  [xs {List T}]
  ...)
```

---

## String Formatting

### Format Strings

```lisp
;; Modern interpolation (#fmt)
#fmt"Count: {n}"          ; Interpolate {n}
#fmt"Hello, {name}!"      ; Interpolate {name}
#fmt"Value: {x}, {y}"     ; Multiple values

;; Common Lisp format (#clf)
#clf"Count: ~D"           ; Use ~D for integers
#clf"Hello ~A"            ; Use ~A for any value

;; Format with expressions
#fmt"Result: {(* 2 3)}"   ; Expressions in {}
```

### Format Specifiers

```lisp
;; #fmt specifiers (planned)
#fmt"{n:d}"               ; Decimal integer
#fmt"{n:x}"               ; Hexadecimal
#fmt"{n:f}"               ; Float
#fmt"{n:s}"               ; String
#fmt"{n:10}"              ; Width 10
#fmt"{n:>10}"             ; Right-align 10

;; #clf specifiers
~D                       ; Integer
~F                       ; Float
~S                       ; Symbol
~A                       ; Any value
~%                       ; Newline
~~                       ; Literal tilde
```

---

## Reader Macros

### Quoting

```lisp
;; Quote - prevents evaluation
'x                       ; => x (symbol)
'(1 2 3)                 ; => (1 2 3) (list)

;; Quasiquote - template with unquote
`(1 2 ~x)                ; => (1 2 x)
`(a ,b c)                ; => (a b c)

;; Unquote-splicing
`(list ,@items more)     ; Splice items into list
```

### Reader Tags

```lisp
;; Dict literal
#{"name" "John" "age" 30}

;; Set literal (future)
#{1 2 3 4 5}

;; Regex literal (future)
#rx"[a-z]+"

;; Format string
#fmt"Hello {name}"

;; Common Lisp format
#clf"Value: ~D"
```

---

## Grammar Summary

### Complete Expression Syntax

```
EXPR ::= ATOM
       | LIST
       | ARRAY
       | TYPE
       | PATH
       | METADATA
       | QUOTED

LIST      ::= '(' WS LIST_INNER ')'
ARRAY     ::= '[' WS ARRAY_INNER ']'
TYPE      ::= '{' WS TYPE_INNER '}'

LIST_INNER ::= (EXPR WS LIST_INNER)?
ARRAY_INNER::= (EXPR WS ARRAY_INNER)?
TYPE_INNER ::= (EXPR WS TYPE_INNER)?

PATH      ::= ATOM PATH_TAIL?
PATH_TAIL ::= ('.' PATH_SEGMENT)+

METADATA  ::= '^' WS EXPR WS EXPR

QUOTED    ::= '\'' EXPR
           | '`' EXPR
           | ',' EXPR
           | ',@' EXPR

ATOM      ::= INT | SYM
```

### Special Forms

```lisp
;; Definition
(define name value)
(define [params...] body...)
(define [params...] return-type body...)

;; Local binding
(let [var val var val...] body...)

;; Conditionals
(if test then else?)
(cond test result test result...)
(when test body...)
(unless test body...)

;; Pattern matching
(match expr
  pattern result
  pattern result
  ...)

;; Function application
(fn args...)
(-> [args...] body)
(lambda [args...] body...)
```

---

## Implementation Status

| Feature | Status | Notes |
|---------|--------|-------|
| Basic syntax (lists, symbols) | ✅ Implemented | Core parser working |
| Array literals `[]` | ✅ Implemented | Parser supports arrays |
| Type annotations `{}` | ✅ Implemented | Basic type syntax |
| Path access `.` | ✅ Implemented | Path expressions |
| Metadata `^` | ✅ Implemented | Basic metadata |
| Function params `[]` | ✅ Implemented | Syntax working: `(define add [x y] ...)` |
| Match (positional) | ✅ Implemented | Syntax working: `(match x (Some v) v None default)` |
| Mutation operators | ✅ Implemented | set!, put!, update!, update parse correctly |
| String literals | ✅ Implemented | `"text"` with escape sequences |
| String formatting | ✅ Implemented | #fmt"..." and #clf"..." with interpolation |
| Explicit type args | ✅ Implemented | `[T]` syntax: `(identity [Int] 42)` |

---

## Migration Guide

### From Old to New Syntax

```lisp
;; OLD: Function with ()
(define (add x y) (+ x y))

;; NEW: Function with []
(define add [x y] (+ x y))

;; OLD: Match with []
(match x
  [(Some v) v]
  [None default])

;; NEW: Match positional (like cond)
(match x
  (Some v) v
  None    default)

;; OLD: Type in parens
{x {(Option (List Int))}}

;; NEW: Type without extra parens
{x {Option (List Int)}}
```

---

## References

- [Language Reference](LANGUAGE_REFERENCE.md) - Core language specification
- [Syntax Standardization](docs/SYNTAX_STANDARDIZATION.md) - Detailed rationale
- [Grammar DSL](docs/GRAMMAR_DSL.md) - Grammar definition language
