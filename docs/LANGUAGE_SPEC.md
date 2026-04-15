# Omni Lisp Language Specification

**Version:** 0.4.7
**Date:** 2026-04-11

Omni Lisp is a Lisp dialect with first-class delimited continuations, algebraic effects, strict-arity multi-param lambdas, multiple dispatch, and a structural type system. It runs on a deterministic scope-region memory system with dual-lane TEMP/ESCAPE ownership, implemented in C3 with a GNU Lightning JIT engine and a Lisp-to-C3 AOT transpiler.

Normative architecture contracts are recorded in `docs/ARCHITECTURE.md`.
Documentation authority and cross-doc coverage mapping are defined in
`docs/DOCS_CONTRACT.md`.
Removed/renamed surface syntax is centralized in
`docs/SURFACE_COMPATIBILITY.md`.

---

## Table of Contents

0. [Core Omni Profile](#0-core-omni-profile)
1. [Syntax Overview](#1-syntax-overview)
2. [Data Types](#2-data-types)
3. [Special Forms](#3-special-forms)
4. [Type System](#4-type-system)
5. [Multiple Dispatch](#5-multiple-dispatch)
6. [Path and Index Notation](#6-path-and-index-notation)
7. [Primitives](#7-primitives)
8. [Standard Library](#8-standard-library)
9. [Delimited Continuations](#9-delimited-continuations)
10. [Effect Handlers](#10-effect-handlers)
11. [Macros](#11-macros)
12. [Modules](#12-modules)
13. [REPL](#13-repl)
14. [Examples](#14-examples)
15. [CLI & Project Tooling](#15-cli--project-tooling)

---

## 0. Core Omni Profile

This profile is the shortest practical mental model for getting productive in
Omni without learning advanced semantics first.

### 0.1 Minimum Mental Model

- Evaluation is expression-based: forms evaluate to values, function calls are
  written as `(f arg1 arg2 ...)`.
- Function arity is strict for user lambdas and function definitions; there is
  no implicit partial application.
- Truthiness rule is minimal:
  - falsy: `nil`, `false`
  - truthy: everything else (`0`, `""`, empty collections, symbols, etc.)
- Start with these value families:
  - scalars: int, double, string, symbol, nil
  - functions: closures (`lambda`)
  - collections: list, array, dict
- Function spelling: `lambda` is canonical; plain `λ` is accepted as an
  equivalent input spelling.
- Prefer generic collection operations (`length`, `ref`, `map`, `filter`,
  `foldl`) instead of type-specific naming.
- Surface naming policy for contributors:
  - prefer one canonical language-facing name per concept,
  - prefer descriptive non-abbreviated canonical names,
  - do not preserve non-canonical alternates by default during pre-alpha cleanup,
  - only keep shorthand spellings when explicitly approved.

### 0.2 First-Steps Command Set

Run these in REPL first:

```lisp
(define x 10)
(+ x 5)

(let (a 3 b 4) (+ a b))
(if (> x 5) "big" "small")

(define (inc n) (+ n 1))
(inc 41)

'(1 2 3)                   ; list
[1 2 3]                    ; array
{'name "omni" 'year 2026}  ; dict

(length [1 2 3])
(ref {'a 10 'b 20} 'b)
```

Core forms to learn first:
- binding/flow: `define`, `let`, `if`, `block`
- functions: `lambda`, strict arity calls
- collections: list/array/dict literals + generic ops (`length`, `ref`, `map`)

### 0.3 What To Ignore Initially

You can safely postpone these until the core model above feels routine:

- Effect handlers (`signal`, `handle`, `resolve`) and strict boundaries.
- Delimited continuations (`checkpoint`, `capture`, `with-continuation`).
- Typed/multi-method dispatch details (`^Type`, value-level dispatch scoring).
- Macro authoring and expansion internals.
- FFI/modules/scheduler runtime details.
- Runtime ownership and boundary internals (region lanes, promotion paths).

### 0.4 Advanced Omni Profile

Use this profile after the core model is comfortable and you need predictable
behavior in larger systems.

#### 0.4.1 Effects and Continuations: Model Boundaries

- Effects (`signal`/`handle`/`resolve`) and continuations (`checkpoint`/`capture`) are
  separate control abstractions that can compose, but they should be reasoned
  about at explicit boundaries.
- Effect handlers are nearest-first by dynamic scope; `resolve` resumes a
  captured continuation at the signal site, while returning without `resolve`
  aborts the remaining body at that site.
- Continuation operators control evaluation flow; handler clauses should stay
  explicit about whether they resume (`resolve`) or terminate (`abort`) the
  local effect path.

#### 0.4.2 Multiple Dispatch and Typed Annotations

- Type annotations (`^Type`, `^(Value ...)`) define method applicability and
  specificity, not just documentation.
- Dispatch picks the highest-scoring applicable method; equal-best results are
  ambiguous and rejected.
- Value-literal dispatch (`^(Value ...)`) is the most specific tier and is
  useful for command-style APIs, while untyped parameters remain the broad
  fallback.
- Handler bodies can delegate type-specific behavior to dispatched functions
  instead of duplicating ad-hoc type branching.

#### 0.4.3 Runtime Ownership and Boundary Constraints (User-Facing)

- Omni values follow deterministic scope/region lifetime rules; values crossing
  boundaries may be copied/promoted, and this is part of correctness rather
  than an optimization detail.
- User code should treat boundary transitions (function return, handler
  resumption, callback/wakeup handoff) as ownership-sensitive points where
  invalid reuse assumptions can break behavior.
- Prefer explicit, boundary-safe composition patterns:
  - return plain values rather than retaining references to transient internals,
  - keep effect handlers/callbacks narrow in responsibility,
  - avoid designs that depend on implicit lifetime extension.

### 0.5 Error Model Quick Reference

Omni uses effects-first failure signaling for public API surfaces.

#### 0.5.1 Failure Class Mapping

| Failure class | Meaning | Surface behavior |
|---|---|---|
| `absence` | Query/data is valid but no result exists | return `nil` (or `false` for predicate APIs) |
| `recoverable-op-failure` | Operation failed due to input/environment/runtime state and caller can fallback/retry | `signal raise` with canonical payload |
| `programmer-error` | Call contract violated (shape/type/mode misuse) | `signal raise` with canonical payload |
| `internal-runtime-error` | Runtime invariant break or corruption risk | hard runtime error (non-resumable) |

Quick rule:
- use `nil` for intentional absence only
- use payloaded `raise` for recoverable and programmer-facing failures
- do not downcast internal runtime failures into ordinary `raise`/`nil`

#### 0.5.2 Canonical `raise` Payload Shape

```lisp
{ 'code ... 'message ... 'domain ... 'data ... }
```

Field contract:

| Key | Type | Purpose |
|---|---|---|
| `'code` | symbol | stable machine-readable code for tests/routing |
| `'message` | string | human-readable explanation |
| `'domain` | symbol | subsystem domain (`io`, `parser`, `regex`, `scheduler`, `deduce`, `type`, `runtime`) |
| `'data` | dict or `nil` | structured context payload |

#### 0.5.3 Common Domains and Codes

| Domain | Representative codes |
|---|---|
| `io` | `io/not-found`, `io/permission-denied`, `io/invalid-handle` |
| `parser` | `parser/invalid-grammar`, `parser/no-root-match` |
| `regex` | `regex/invalid-pattern`, `regex/invalid-arg-type` |
| `scheduler` | `scheduler/invalid-handle`, `scheduler/timeout` |
| `deduce` | `deduce/relation-not-found`, `deduce/txn-failed` |
| `type` | `type/arg-mismatch`, `type/arity` |
| `runtime` | `runtime/unhandled-effect`, `runtime/invalid-continuation` |

Reference contract sources:
- `docs/ARCHITECTURE.md` (ADR-2026-03-06-A)
- `docs/ERROR_MODEL.md`

### 0.6 Pitfalls Guide

#### 0.6.1 `nil` vs `raise`

Use `nil` for intentional absence, and `raise` for recoverable/programmer
failures.

```lisp
; absence (valid "not found")
(find (lambda (x) (= x 999)) '(1 2 3))
; => nil

; recoverable/programmer failure
(signal raise
  {'code 'io/not-found
   'message "read-file: path not found"
   'domain 'io
   'data {'path "missing.txt"}})
```

Pitfall:
- returning `nil` for operational failures hides actionable error context.

#### 0.6.2 Truthiness Is Narrow

Only `nil` and `false` are falsy. Everything else is truthy, including `0`,
empty strings, and empty collections.

`false` is a distinct boolean false value, while `nil` represents absence.
Both are falsy. Use quoted `'false` if you need the literal symbol name as
symbol data.

```lisp
(if 0 'yes 'no)            ; => 'yes
(if "" 'yes 'no)           ; => 'yes
(if [] 'yes 'no)           ; => 'yes
(if nil 'yes 'no)          ; => 'no
```

Pitfall:
- assuming "empty" values are falsy (as in some other languages) causes logic
  bugs.

#### 0.6.3 Effect `resolve` vs Abort

Inside `handle`, `resolve` resumes the suspended computation at the signal site.
Returning from the clause without `resolve` aborts that path and the clause
result becomes the `handle` result.

```lisp
; resolve: body continues
(handle (+ 1 (signal read nil))
  (read x (resolve 41)))
; => 42

; abort: body after signal does not continue
(handle (+ 1 (signal bail 5))
  (bail x x))
; => 5
```

Pitfall:
- expecting post-signal code to run when the handler did not call `resolve`.

## 1. Syntax Overview

### 1.1 Lexical Elements

```
; Comments start with semicolon and extend to end of line

; Integers
42
-17
0

; Floating-point numbers
3.14
-0.5
1.0

; Strings (double-quoted, with escape sequences)
"hello world"
"line1\nline2"
"tab\there"
"quote: \"nested\""

; Symbols (identifiers)
foo
my-function
string-length
null?

; Collection literals
[1 2 3]         ; array literal, equivalent to (Array 1 2 3)
{'a 1 'b 2}     ; dict literal, equivalent to (Dictionary 'a 1 'b 2)

; Quote shorthand
'symbol     ; equivalent to (quote symbol)
'(a b c)    ; equivalent to (quote (a b c))

; Quasiquote
`(a ,x ,@xs)   ; template with unquote and splicing
```

### 1.2 S-Expression Forms

```lisp
; Function application
(f arg1 arg2 ...)     ; multi-arg call (strict arity)

; Examples
(+ 1 2)              ; adds 1 and 2
(map inc '(1 2 3))   ; applies inc to each element

; Empty list / nil
()
```

### 1.3 Special Tokens

| Token | Description |
|-------|-------------|
| `_` | Wildcard (in patterns), NOT a symbol |
| `..` | Rest/spread in patterns and variadic params |
| `.[` | Parser token pattern used for postfix index parsing (`expr.[key]`) |
| `.` | Dot for field/path access |
| `^` | Type annotation prefix |
| `[` `]` | Array literals, bracket attributes, and patterns |
| `{` `}` | Dictionary literals |

Slash (`/`) is part of ordinary symbol syntax. Names like `math/lgamma`,
`stats/normal-cdf`, and `io/println` are single symbols, not module dereferences.
They are naming-convention prefixes like `math-lgamma`; real module access uses
module values/path access (`mod.sym`) or explicit import/export forms.

### 1.4 Reader Dispatch (`#`)

The reader supports a small canonical `#` dispatch surface:

| Form | Meaning |
|------|---------|
| `#r"..."` | regex literal |
| `#_ form` | skip next form |
| `#N_ form...` | skip next `N` forms (`N` in `1..9`) |
| `#| ... |#` | (nestable) block comment |
| `#tag form` | reader tag shorthand; parses as `(tag form)` |

Reader tags are constrained syntax, not Common Lisp style readtable hooks:
`#tag form` reads exactly one following expression and rewrites it to a normal
one-argument call. A tag can therefore be an ordinary function, or a macro
declared with the canonical `(define [reader tag] name (syntax-match ...))`
surface. Any other non-tag `#` sequence is rejected with a deterministic
parser/lexer error.

---

## 2. Data Types

### 2.1 Core Types

| Type | Tag | Description | Example |
|------|-----|-------------|---------|
| nil | `NIL` | Empty / absence value | `nil`, `()` |
| int | `INT` | 64-bit signed integer | `42`, `-17` |
| double | `DOUBLE` | 64-bit floating point | `3.14`, `-0.5` |
| BigInteger | `BIG_INTEGER` | Arbitrary-precision exact integer | `(BigInteger "9223372036854775808")` |
| BigFloat | `BIG_FLOAT` | High-precision decimal float | `(BigFloat "1.25")` |
| BigComplex | `BIG_COMPLEX` | High-precision decimal complex value | `(BigComplex 1 2)` |
| string | `STRING` | Immutable string (heap-allocated) | `"hello"` |
| symbol | `SYMBOL` | Interned identifier | `'foo`, `'hello` |
| cons | `CONS` | Pair / list cell | `(cons 1 2)`, `'(1 2 3)` |
| closure | `CLOSURE` | User-defined function with environment | `(lambda (x) x)` |
| continuation | `CONTINUATION` | Captured delimited continuation | via `capture` |
| primitive | `PRIMITIVE` | Built-in function | `+`, `car` |
| partial | `PARTIAL_PRIM` | Explicit partially applied primitive | `(partial + 3)` |
| error | `ERROR` | Error value | `(error "oops")` |
| Dictionary | `HASHMAP` | Mutable hash table | `{'a 1}`, `(Dictionary 'a 1)` |
| Array | `ARRAY` | Mutable dynamic array | `[1 2 3]`, `(Array 1 2 3)` |
| Coroutine | `COROUTINE` | User-level coroutine | `(Coroutine (lambda () body))` |
| Tensor | `TENSOR` | Homogeneous n-dimensional numeric storage | `(Tensor [[1 2] [3 4]])` |
| void | `VOID` | Singleton no-result value | `#<void>` |
| ffi-handle | `FFI_HANDLE` | Foreign library handle | `(define [ffi lib] libc "libc.so.6")` |
| instance | `INSTANCE` | User-defined type instance | `(Point 3 4)` |
| method-table | `METHOD_TABLE` | Multiple dispatch table | internal |

Omni does not currently define a builtin `Empty`/bottom type. `nil`/`Nil`
cover the language's empty/false value, while `Void` is now a real builtin
singleton type/value used by FFI `^Void` returns as well.

`Integer` is the fixed-width signed integer surface. `BigInteger` is the
arbitrary-precision exact integer surface backed by Boost.Multiprecision in the
current runtime. `+`, `-`, `*`, `abs`, `gcd`, `lcm`, and the `long.min / -1`
division overflow case promote overflowing `Integer` results to `BigInteger`.
`/`, `%`, ordering comparisons, `min`, and `max` support `BigInteger` values;
bitwise operations support exact `Integer`/`BigInteger` operands. `parse-number`
returns fixed-width `Integer` values when the decimal input fits and promotes
valid wider decimal integers to `BigInteger`.
Integer source literals cover the full signed fixed-width range, including
`-9223372036854775808`; positive overflow and negative underflow still fail at
lex time.
Use `(BigInteger "...")` when an explicit arbitrary-precision integer
constructor is needed.

`BigFloat` is the high-precision decimal float surface backed by
Boost.Multiprecision in the current runtime. `BigFloat` values are `Number`
values, support `String` and finite `Double` conversion, participate in `+`,
`-`, `*`, `/`, ordering comparisons, `abs`, `min`, and `max`, and preserve
`BigFloat` results when mixed with `Integer`, `BigInteger`, or `Double`.
Core scalar math primitives preserve `BigFloat` results when a `BigFloat`
operand participates for trigonometric, inverse trigonometric, hyperbolic,
exponential, logarithmic, power/root, gamma/error-function, and
standard-normal distribution helpers. `floor`, `ceiling`, `round`, and
`truncate` round `BigFloat` exactly to `Integer` or `BigInteger` when the
integer result is inside the supported allocation cap; huge integer
materializations fail closed instead of narrowing through `Double`.
Fixed-width `Double` remains the result type for non-`BigFloat` floating
inputs.
`parse-number` promotes syntactically valid floating inputs that overflow
`Double` to `BigFloat`.

`BigComplex` is the high-precision complex-number surface backed by decimal
real and imaginary parts in the current runtime. `(BigComplex real imag)`
constructs a complex value from non-complex numeric parts; a one-argument
constructor creates a zero-imaginary value from a number or finite decimal
string. BigComplex values are `Number` values, support `String`, `+`, `-`,
`*`, `/`, unary `-`, `=`, hashing/equality, and scope-boundary copy/promotion.
`sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`,
`log`, `log10`, `sqrt`, and `pow` preserve BigComplex results when a complex
operand participates. `real-part` and `imag-part` return `BigFloat`
components for BigComplex inputs, `imag-part` returns `0` for real scalar
inputs, and `conjugate` preserves real scalars while flipping a BigComplex
imaginary sign. `abs` returns a `BigFloat` magnitude. `atan2` remains a
real-plane helper and rejects complex operands. Complex values are intentionally
not ordered, so `<`, `>`, `<=`, `>=`, `min`, `max`, `positive?`, and
`negative?` fail closed for BigComplex operands.

`Tensor` is the canonical rank-polymorphic scientific numeric aggregate. The
current runtime slice registers the type descriptor, constructor, print
surface, lifetime copy/promotion paths, tensor `ref`, and introspection
primitives (`tensor?`, `dtype`, `shape`, `rank`, and `length`). The explicit
constructor surface is `(Tensor Double shape data-or-scalar)`, where `shape`
is an array or proper list of non-negative integers and `data-or-scalar` is
either a scalar numeric fill value or an array/proper list with exactly the
shape product's element count. The inferred-shape constructor surface is
`(Tensor data)`, `(Tensor data Double)`, or `(Tensor Double data)`, where
`data` is a real numeric scalar or rectangular nested arrays/proper lists of
real numeric values that can narrow to finite `Double`. Complex values and
out-of-`Double`-range values fail closed. Tensor `ref` uses
`(ref tensor index-array)`.
`(Array tensor)` and `(List tensor)` realize tensor expressions when needed and
return flat row-major element values; use `shape` when rank metadata is needed.
`realize` treats concrete tensors as already realized values, forces
lazy Tensor expression payloads, and can write a tensor expression, concrete
tensor, or scalar fill into an existing destination tensor. Tensor-dispatched
`map` is the elementwise tensor operation; `contract` is the pure `Double`
summed-axis operation for tensor contraction. Both may produce lazy Tensor
expression payloads under the existing `Tensor` value, with backend
acceleration left as an optimization behind the same semantic surface. User
code should not name or depend on a separate `TensorExpr` type.

### 2.2 Truthiness

Normative predicate contract:
- Predicate positions in `if`, `when`, and `match` guards use the same
  truthiness rules.
- **Falsy:** `nil`, `false`
- **Truthy:** everything else.

| Predicate input | Example | Truthiness | Notes |
|---|---|---|---|
| `nil` | `nil` | falsy | Absence value |
| `false` | `false` | falsy | Boolean false |
| `Void` | `#<void>` | truthy | Command/effect completion token |
| numbers | `0`, `-1`, `3.14` | truthy | Zero is still truthy |
| strings | `""`, `"omni"` | truthy | Empty string is truthy |
| collections | `'()`, `[]`, `{}` | truthy | Empty collections are truthy |

### 2.3 `Void` vs `Nil` Contract

Normative rule:
- `Void` means successful command/effect completion with no payload.
- `Nil` means absence/query-miss (or falsey result in predicate-style APIs).
- `Void` is an operational completion token, not a data/absence sentinel.
- APIs should not encode query-miss/optional absence using `Void`; use `Nil`.
- `Void` is truthy under Omni truthiness rules.

Contract examples:

```lisp
(type-of (block (define x 1) (set! x 2)))   ; => 'Void
(type-of (let (d {'a 1}) (remove! d 'a)))   ; => 'Void
(if (block (define x 1) (set! x 2)) 1 0)    ; => 1  (Void is truthy)

(type-of (ref {'a 1} 'missing))             ; => 'Nil
(type-of (has? {'a 1} 'missing))            ; => 'Nil
```

### 2.4 Equality

`=` performs structural equality:
- Integers and doubles: numeric comparison
- Strings: character-by-character
- Symbols: identity (interned)
- Lists: recursive structural equality
- Other types: identity

---

## 3. Special Forms

### 3.1 `lambda` -- Function Definition

```lisp
; Single parameter
(lambda (x) body)

; Multi-parameter (strict arity)
(lambda (x y z) body)
; requires exactly 3 arguments — use _ placeholder, |> pipe, or partial for partial application

; Zero-argument
(lambda () body)

; Variadic
(lambda (x .. rest) body)

; Typed parameters (for dispatch)
(lambda ((^Integer x) (^String y)) body)

; Dictionary destructuring parameter — caller passes a dict
(lambda ({name age}) (println name age))
; called as: (f {'name "Alice" 'age 30})

; Mixed positional + dict params
(lambda ({host port} verbose) body)
```

### 3.2 `define` -- Global Definition

```lisp
; Simple define
(define name value)

; Shorthand function define
(define (f x y) body)
; desugars to: (define f (lambda (x y) body))

; Zero-arg shorthand
(define (thunk) 42)

; Typed function define (creates dispatch entry)
(define (describe (^Integer n)) "integer")
(define (describe (^String s)) "string")
(define (describe x) "other")   ; fallback

; Dictionary destructuring parameter
(define (connect {host port timeout}) (tcp-connect host port))
; called as: (connect {'host "localhost" 'port 8080 'timeout 5000})

; Mixed dict + positional params
(define (request {method url} body) ...)
```

Note: `(define [...] ...)` with brackets is reserved for attribute syntax (`[type]`, `[struct]`, `[ffi lib]`, `[relation db]`, etc.). `[struct]` is an alias of `[type]`. Array destructuring is only available in `let` and `match`.

### 3.2.1 ADR: `define` Unification Is Canonical Syntax

Status: Accepted (2026-03-06)

`define` is the canonical declaration entrypoint in Omni. New declaration
surfaces MUST either:

- use `(define ...)` directly, or
- desugar to canonical `(define ...)` forms before evaluation.

Normative rules:

- `(define name value)` is canonical for value binding.
- `(define (f args...) body)` is canonical shorthand for function declaration.
- `(define [attr] ...)` is canonical for declaration families (type, union,
  alias, macro, reader tag, FFI, relation, rule, schema, effect).
- Alternative declaration keywords (`def`, `defn`, `deftype`, etc.) are not
  canonical language forms and MUST NOT be introduced as parallel syntax.
- `lambda` and `define` remain distinct: `lambda` is expression-level function
  construction; `define` is global declaration.

Examples:

```lisp
(define (add x y) (+ x y))
(define [type] Point (^Integer x) (^Integer y))
(define [effect] (io/read-file (^String path)))
```

Counterexample (non-canonical syntax; do not add):

```lisp
(defn add [x y] (+ x y))
```

### 3.2.2 `define` Forms Catalog (Type Family)

`[abstract]`:
- Intent: define a parent type for subtype checks and shared type hierarchy.

```lisp
(define [abstract] Shape)
(define [type] (Circle Shape) (^Integer radius))
(is? (Circle 5) 'Shape)   ; => true
```

`[struct]` (alias of `[type]`):
- Intent: define a concrete product type; this is syntax-level aliasing to `[type]`.

```lisp
(define [struct] Vec2 (^Integer x) (^Integer y))
(let (v (Vec2 3 4)) v.x)   ; => 3
```

`[type]`:
- Intent: define a concrete nominal type with named fields (and optional parent).

```lisp
(define [type] Point (^Integer x) (^Integer y))
(type-of (Point 1 2))   ; => 'Point
```

`[union]`:
- Intent: define sum types (ADTs) with explicit variants.

```lisp
(define [union] (Option T) None (Some T))
(match (Some 42)
  (None 0)
  ((Some x) x))   ; => 42
```

`[alias]`:
- Intent: declare a type alias name for readability and API clarity.

```lisp
(define [alias] Num Integer)
(define (id-num (^Num x)) x)
(id-num 7)   ; => 7
```

### 3.3 `let` -- Local Binding

```lisp
; Simple let (flat pairs)
(let (name value) body)

; Multi-binding (sequential left-to-right; lowers to nested lets)
(let (x 1 y 2) (+ x y))
(let (x 1 y (+ x 2) z (+ y 3)) z)       ; => 6

; Array destructuring
(let ([x y] [10 20]) (+ x y))         ; => 30
(let ([head .. tail] '(1 2 3)) head)   ; => 1
(let ([a b ..] '(1 2 3 4 5)) (+ a b)) ; => 3

; Dictionary destructuring
(let ({name age} {'name "Alice" 'age 30}) name) ; => "Alice"
(let ({x y} {'x 10 'y 20}) (+ x y))             ; => 30

; Mixed bindings (plain + destructuring)
(let ([a b] [3 4] z 5) (+ a (+ b z)))  ; => 12

; Recursive let
(let ^rec (fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
  (fact 5))

; Named let (loop construct)
(let loop (n 5 acc 1)
  (if (= n 0) acc
      (loop (- n 1) (* acc n))))
; Named let initializers are also sequential left-to-right.
; It lowers through an outer sequential let and an inner let ^rec.
```

### 3.4 `if` -- Conditional

```lisp
(if test then-expr else-expr)
```

Three branches required. Only the chosen branch is evaluated. `test` uses the
truthiness contract in [2.2](#22-truthiness), so `Void` in predicate position
is truthy.

### 3.5 `block` -- Sequencing

```lisp
(block e1 e2 ... en)
```

Evaluates all expressions in order, returns the last. Last expression is in tail position (TCO).

### 3.6 `set!` -- Mutation

```lisp
(set! name value)              ; variable mutation
(set! instance.field value)    ; struct field mutation
(set! obj.nested.field value)  ; nested field mutation
(set! pair.car value)          ; cons cell car mutation
(set! pair.cdr value)          ; cons cell cdr mutation
(set! collection key value)    ; generic collection update (Array/Dictionary)
```

`set!` returns `Void` on successful mutation.
Target/dispatch matrix:

| Surface | Dispatch target | Success result | Invalid-target behavior |
|---|---|---|---|
| `(set! name value)` | lexical/global variable binding | `Void` | `set!: unbound variable` |
| `(set! root.seg... value)` | dot-path over `Instance` fields and cons `.car`/`.cdr` | `Void` | `set!` path errors (below) |
| `(set! collection key value)` | generic update for `Array` and `Dictionary` | `Void` | `set!: generic form expects array or dict target` |

Dot-path invalid-target errors:
- `set!: unbound path root`
- `set!: field not found in path` (missing intermediate field)
- `set!: path segment is not an instance or cons` (non-final segment resolves to non-path value)
- `set!: cons only supports .car and .cdr`
- `set!: field not found` (missing final instance field)
- `set!: target is not an instance or cons` (final target value is not mutable path target)

### 3.7 `quote` / `quasiquote`

```lisp
(quote datum)       ; or 'datum
'foo                ; => symbol foo
'(1 2 3)            ; => list (1 2 3)

`(a ,(+ 1 2) ,@(list 3 4))  ; => (a 3 3 4)
```

Quasiquote supports nesting with depth tracking (Bawden's algorithm).

### 3.8 `and` / `or` -- Short-Circuit Logic

```lisp
(and)               ; => true
(and x)             ; => x
(and a b c)         ; returns first falsy value, else last value

(or)                ; => nil
(or x)              ; => x
(or a b c)          ; returns first truthy value, else last value
```

### 3.9 `match` -- Pattern Matching

```lisp
(match expr
  (pattern1 result1)
  (pattern2 result2)
  (_ default))
```

Dynamic clause count (no fixed limit). Clauses are checked in source order and
the first matching clause wins.

`match` is a core control form (not only pattern syntax sugar). Normative
clause selection semantics:
- evaluate scrutinee exactly once,
- check clauses left-to-right,
- select the first clause whose pattern matches and whose guard (if present) is
  truthy,
- if no non-wildcard clause matches, `_` fallthrough (when present) is used.

Literal-pattern contract in `match`:
- `nil` and `false` in pattern position are literal falsy patterns (not
  variable bindings); both match Omni's runtime falsy sentinel.
- `Void` in pattern position is a literal `Void` singleton pattern (not a
  variable binding).

Guard patterns (`(? pred)` / `(? pred pat)`) use the same truthiness contract
as `if`/`when` ([2.2](#22-truthiness)); a guard result of `Void` is truthy.
For `(? pred pat)`, `pat` is matched first; only successful sub-pattern matches
evaluate `pred`, and `pred` sees the sub-pattern bindings.

Pattern types:

| Pattern | Description | Example |
|---------|-------------|---------|
| `_` | Wildcard | `(_ "default")` |
| `x` | Variable binding (except reserved literal forms `nil`, `false`, `Void`) | `(n (* n 2))` |
| `42` | Integer literal | `(0 "zero")` |
| `"hi"` | String literal | `("hi" "greeting")` |
| `nil` / `false` | Falsy literal pattern | `(nil "missing")`, `(false "missing")` |
| `Void` | `Void` singleton literal pattern | `(Void "done")` |
| `'sym` | Quoted symbol | `('red "red")` |
| `[a b c]` | Exact sequence | `([x y] (+ x y))` |
| `[h .. t]` | Head-tail | `([first .. rest] first)` |
| `[x y ..]` | Prefix | `([a b ..] (+ a b))` |
| `[.. a b]` | Suffix | `([.. prev last] (Array prev last))` |
| `(? pred)` | Guard; callable `pred` receives scrutinee, non-callable `pred` is tested directly | `((? (> _ 10)) "big")`, `((? (> x 10)) "big")` |
| `(? pred pat)` | Guard with sub-pattern bindings before guard evaluation | `((? (> x 10) x) x)` |
| `None` | Nullary constructor | `(None "empty")` |
| `(Some x)` | Constructor pattern | `((Some v) v)` |

---

## 4. Type System

Canonical naming direction:

- prefer descriptive language-facing type symbols and constructors over abbreviations,
- `Integer`, `Boolean`, and `Dictionary` are the canonical builtin names,
- `Dict` is the allowed shorthand constructor alias for `Dictionary`.
- alternate-spelling policy is input-tolerant but output-canonical:
  - alternate spellings are accepted in constructor/type-annotation input position,
    so `^Dict` resolves as canonical `Dictionary`,
  - docs/examples and introspection outputs use canonical names (`Integer`,
    `Boolean`, `Dictionary`),
  - constructor failure messages/payload text use canonical constructor names
    even when invocation used an alternate spelling.
- collection/time constructor policy:
  - canonical constructor surfaces: `List`, `Array`, `Dictionary`, `Iterator`, `TimePoint`
  - allowed constructor shorthand: `Dict` for `Dictionary`
  - approved retained public helper: `list` (idiomatic Lisp list builder/conversion helper)

### 4.1 Struct Types

```lisp
(define [type] Point (^Integer x) (^Integer y))
(define [struct] Vec2 (^Integer x) (^Integer y))   ; alias of [type]

(Point 3 4)        ; construction
point.x             ; field access => 3
point.[0]           ; positional access => 3
(set! point.x 99)   ; field mutation
```

### 4.2 Type Inheritance

```lisp
(define [abstract] Shape)
(define [type] (Circle Shape) (^Integer radius))

(is? (Circle 5) 'Shape)   ; => true (subtype check)
(is? (Circle 5) 'Circle)  ; => true
```

Syntax: `(define [type] (ChildName ParentName) fields...)` for inheritance.

### 4.3 Union Types (Sum Types / ADTs)

```lisp
(define [union] (Option T) None (Some T))
(define [union] (Result T E) (Ok T) (Err E))

; Construction
None                    ; nullary variant
(Some 42)               ; variant with value

; Pattern matching
(match opt
  (None "empty")
  ((Some x) x))
```

### 4.4 Type Aliases

```lisp
(define [alias] Num Integer)
```

### 4.5 Type Annotations

```lisp
^Integer                ; simple type
^(List Integer)         ; compound type
^(Value 42)             ; canonical value-level constructor
^(Value bind)           ; symbol literal
^(Value "open")         ; string literal
^(Value true)           ; boolean literal (true/false symbols)
^(Value nil)            ; nil literal
```

Meta/abstract symbols `Any`, `Number`, and `Collection` participate in
annotations/dispatch and are also exposed as non-callable value-position type
descriptors (`#<type Any>`, `#<type Number>`, `#<type Collection>`). They are
not constructor/coercion call surfaces (`(Any ...)`, `(Number ...)`,
`(Collection ...)` still error).

`Value` remains the dedicated value-literal annotation surface:
`^(Value literal)`. It is not a callable value-position constructor.

### 4.6 Type Introspection

```lisp
(type-of 42)            ; => 'Integer
(type-of "hi")          ; => 'String
(type-of (Point 1 2))   ; => 'Point
(= (type-of (Array 1 2)) 'Array) ; exact type via symbol equality
(is? 42 'Integer)       ; => true
(is? (Circle 5) 'Shape) ; => true (walks parent chain)
(instance? (Point 1 2)) ; => true
(instance? 42)          ; => nil
```

Printing/introspection contract:
- `type-of` always returns a symbol (for example `'Integer`, `'Dictionary`).
- Canonical type-descriptor print shape is `#<type Name>` (not `#<Name>`).
- Constructor/type symbols in value position render as type descriptors:
  `#<type Integer>`, `#<type Dictionary>`, etc.
- Abstract/meta type descriptors for `Any`, `Number`, and `Collection` follow
  the same canonical `#<type Name>` rendering.
- constructor aliases (`Dict` -> `Dictionary`) normalize to canonical type
  identity in introspection (`type-of`, descriptor rendering).
- Ordinary callable primitives keep primitive rendering (`#<primitive +>`).

### 4.7 Callable Constructor Failure Semantics

Callable type symbols (`Integer`, `Double`, `String`, `Boolean`, `List`,
`Array`, `Dictionary`, `Set`, `Iterator`) follow deterministic recoverable
failure signaling:

- `type/arity` is used when the constructor form has the wrong argument shape.
  Example: `(Dictionary 'a)` (odd key/value arity).
- `type/arg-mismatch` is used when arity is acceptable but argument values are
  not convertible. Examples: `(Integer "abc")`, `(Iterator 42)`.

Collection constructor behavior remains explicit and stable:

- `List`, `Array`, and `Set` are variadic constructor surfaces.
- `List`/`Array` additionally treat a single iterator/collection argument as
  conversion (`(List [1 2 3])`, `(Array '(1 2 3))`).
- `Dictionary` requires even key/value argument count.

---

## 5. Multiple Dispatch

### 5.1 Basic Dispatch

Define multiple implementations with typed parameters. Best match wins:

```lisp
(define (describe (^Integer n)) "integer")
(define (describe (^String s)) "string")
(define (describe x) "other")

(describe 42)       ; => "integer"
(describe "hi")     ; => "string"
(describe '(1 2))   ; => "other"
```

### 5.2 Multi-Argument Dispatch

```lisp
(define (add2 (^Integer a) (^Integer b)) (+ a b))
(define (add2 (^String a) (^String b)) (string-append a b))

(add2 3 4)              ; => 7
(add2 "hello" " world") ; => "hello world"
```

### 5.3 Value Dispatch (Value-Level Matching)

```lisp
(define (fib (^(Value 0) n)) 0)
(define (fib (^(Value 1) n)) 1)
(define (fib (^Integer n)) (+ (fib (- n 1)) (fib (- n 2))))

(define (udp (^(Value open) cmd)) (io/udp-open))
(define (udp (^(Value bind) cmd) h host port) (io/udp-bind h host port))
(define (udp (^(Value send) cmd) h host port payload) (io/udp-send h host port payload))

(fib 10)    ; => 55
```

Command-style facades like `udp` are valid API shape, but core operations remain canonical
`io/*` effects and primitives. Facades should delegate to canonical `io/udp-*` operations.

### 5.4 Dispatch Scoring

| Match Type | Score | Description |
|------------|-------|-------------|
| Value literal | 1000 | `^(Value 42)`, `^(Value open)`, `^(Value "open")`, `^(Value true)`, `^(Value nil)` |
| Exact type | 100 | `^Integer` matches INT value |
| Subtype | 10 | `^Shape` matches Circle (Shape child) |
| Any type | 1 | Untyped parameter matches anything |

Highest-scoring method wins. Equal-best ties are rejected as ambiguous (no implicit winner).

Normative ambiguity contract:
- Dispatch never applies an implicit tie-break when multiple methods share the
  same best score.
- Ambiguous calls raise recoverable payload code
  `type/dispatch-ambiguous` in domain `type`.
- Ambiguity payload `data` includes stable keys:
  `reason`, `method`, `arg-count`, `arg-types`, `best-score`, `tie-count`,
  `candidate-indices`.
- `reason` is `ambiguous-equal-specificity`.
- `candidate-indices` are ordered by method-table entry index (ascending).

### 5.5 Dispatch Explainability

Use `explain` with the canonical symbol selector:

```lisp
(explain 'dispatch <form>)
```

`<form>` is analyzed as a thunked expression, so it is not eagerly evaluated by
the explain call itself.

```lisp
(let (x 0)
  (block
    (explain 'dispatch (set! x 1))
    x))
; => 0
```

`explain 'dispatch` returns a deterministic dictionary shape:

- top-level keys: `kind`, `status`, `input`, `decision`, `candidates`, `trace`, `debug_message`
- `kind` is always `'dispatch`
- `status` is one of `'ok`, `'ambiguous`, `'no-match`, `'unsupported-form`

`decision` includes stable keys:
- `reason`, `winner-index`, `best-score`, `tie-count`, `fallback-source`, `outcome`, `debug_message`

Ambiguity decision invariants:
- `status` is `'ambiguous`
- `reason` is `'ambiguous-equal-specificity`
- `winner-index` is `nil`
- `best-score` is an integer
- `tie-count` is an integer `>= 2`

`candidates` is a list with one item per method-table entry. Each candidate
includes:
- method index/name/signature/constraints/source
- applicability flag
- score breakdown (`value`, `exact`, `subtype`, `any`, `total`; older `widen` key remains in explain output for migration and is currently `0`)
- failure classification (`none`, `arity-mismatch`, `value-literal-mismatch`, `type-mismatch`, `constraint-mismatch`)

Example:

```lisp
(define (score (^Integer x) (^Integer y)) (+ x y))
(define (score (^Double x) (^Double y)) (+ x y))

(ref (ref (explain 'dispatch (score 1 2)) 'decision) 'reason)
; => 'method-match
```

---

## 6. Path and Index Notation

### 6.1 Postfix Index Syntax

```lisp
list.[0]            ; first element
str.[2]             ; character code at index 2
tensor.[i].[j]      ; chained indexing
array.[0]           ; array indexing
dict.['key]         ; dict key lookup
```

`expr.[key]` means lookup/index on `expr`. It is not leading-dot syntax.

### 6.2 Path Notation (Field Access)

```lisp
point.x              ; struct field access
config.port          ; dictionary symbol-key access
line.start.y         ; nested field access (up to 8 segments)
pair.car             ; cons cell car access
pair.cdr             ; cons cell cdr access
```

Path notation is a distinct field/path operation for symbol-key and field
lookup. It shares lookup intent with `ref`, but it is not a full desugar to
`ref`.

Path notation resolves segments on instances, modules, and dictionaries with
symbol keys. Cons cells only support `.car` and `.cdr` as special field names.

Removed accessor forms must hard-error:

```lisp
.name
.1
.'key
.[expr]
```

Use `(ref coll key)` for dynamic collection lookup, `expr.name` for path-step
access, and `expr.[key]` for postfix dynamic/index access. For higher-order
code, write the lambda explicitly: `(lambda (x) (ref x 'name))`.

Compatibility/removal details (including callable quoted-symbol accessor
removal) are maintained in `docs/SURFACE_COMPATIBILITY.md`.

---

## 7. Primitives

### 7.1 Arithmetic (5)

| Prim | Arity | Description |
|------|-------|-------------|
| `+` | 2 | Addition (int or double) |
| `-` | 1-2 | Subtraction; `(- n)` negates |
| `*` | 2 | Multiplication |
| `/` | 2 | Integer/float division |
| `%` | 2 | Modulo |

Binary primitives no longer auto-partial. A bare one-argument call like `(+ 3)` is an arity error outside rewrite contexts such as `|>`. Use `_` / `_n` placeholders, `|>` pipe, or `partial` for partial application.

### 7.2 Comparison (5)

| Prim | Description |
|------|-------------|
| `=` | Structural equality |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less or equal |
| `>=` | Greater or equal |

### 7.3 List Operations (7)

| Prim | Arity | Description |
|------|-------|-------------|
| `cons` | 2 | Construct pair |
| `car` | 1 | First element |
| `cdr` | 1 | Rest element |
| `list` | variadic | Create list; single collection arg dispatches conversion (`(list [1 2 3])`, `(list (Iterator ...))`) |
| `List` | variadic | Canonical list constructor/conversion surface; single Tensor input realizes to a flat row-major list (`list` remains a public helper) |
| `length` | 1 | Generic: list, array, dict, string, or tensor element count |
| `null?` | 1 | Check if nil |
| `pair?` | 1 | Check if cons |

### 7.4 Boolean (1)

| Prim | Description |
|------|-------------|
| `not` | Logical negation |

### 7.5 I/O (4, via effects)

| Prim | Description |
|------|-------------|
| `print` | Output value (no newline) |
| `println` | Output value with newline |
| `display` | Display value |
| `newline` | Output newline |

I/O primitives go through algebraic effects (`io/print`, `io/println`, etc.). When no handler is installed, a fast path calls raw primitives directly (zero overhead). Custom handlers can intercept, suppress, or redirect I/O.

### 7.6 String Operations (14)

| Prim | Arity | Description |
|------|-------|-------------|
| `string-append` | variadic | Concatenate strings |
| `string-join` | 2 | Join list with separator |
| `substring` | 3 | Extract substring (negative indices supported) |
| `string-split` | 2 | Split by delimiter |
| `string-length` | 1 | String length in UTF-8 codepoints |
| `string-byte-length` | 1 | String length in bytes |
| `string-upcase` | 1 | Uppercase |
| `string-downcase` | 1 | Lowercase |
| `string-trim` | 1 | Trim whitespace |
| `string-contains?` | 2 | Substring search |
| `string-index-of` | 2 | Find index of substring |
| `string-replace` | 3 | Replace occurrences |
| `char-at` | 2 | Character at index |
| `string-repeat` | 2 | Repeat string N times |

### 7.7 Type Predicates (12)

| Prim | Description |
|------|-------------|
| `string?` | Is string? |
| `int?` | Is integer? |
| `double?` | Is double? |
| `number?` | Is int, BigInteger, BigFloat, or double? |
| `symbol?` | Is symbol? |
| `closure?` | Is closure? |
| `continuation?` | Is continuation? |
| `boolean?` | Is true or false? |
| `list?` | Is proper list? |
| `procedure?` | Is callable? |
| `dict?` | Is dict? |
| `array?` | Is array? |

### 7.8 Numeric Predicates (4)

| Prim | Description |
|------|-------------|
| `zero?` | Is zero? |
| `positive?` | Is positive? |
| `negative?` | Is negative? |
| `even?` / `odd?` | Parity check |

### 7.9 File I/O (5, via effects)

| Prim | Arity | Description |
|------|-------|-------------|
| `read-line` | 0 | Read one line from standard input; returns `nil` on EOF |
| `read-file` | 1 | Read file as string |
| `write-file` | 2 | Write string to file (returns `Void` on success) |
| `file-exists?` | 1 | Check file existence |
| `read-lines` | 1 | Read file as list of lines |
| `load` | 1 | Load and evaluate a .omni file |

### 7.10 Dictionary Operations (1)

| Prim | Arity | Description |
|------|-------|-------------|
| `Dictionary` / `Dict` | variadic | Create a dictionary from key-value pairs; `{'a 1 'b 2}` is equivalent to this |

Canonical constructor surface is `Dictionary` (with `Dict` shorthand).
Dictionary keys are value-typed: symbols, strings, integers, and other stable
value keys are supported.
Style guidance:
- prefer symbol keys for internal language/runtime maps
- prefer string keys for external payload maps (JSON/HTTP/config)

### 7.11 Array Operations (1)

| Prim | Arity | Description |
|------|-------|-------------|
| `Array` | variadic | Create array; `[1 2 3]` is equivalent to this; single collection arg dispatches conversion (`(Array '(1 2 3))`, `(Array (Iterator ...))`, `(Array tensor)`) |

Canonical constructor surface is `Array`.

### 7.12 Generic Collection Operations (6)

| Prim | Arity | Description | Supported types |
|------|-------|-------------|-----------------|
| `ref` | 2 | Lookup by key/index | Array (int), Dictionary (any key type), cons/list chain (int), string (char) |
| `push!` | 2 | Append element | Array |
| `keys` | 1 | List of keys | Dictionary |
| `values` | 1 | List of values | Dictionary |
| `has?` | 2 | Check key existence | Dictionary |
| `remove!` | 2 | Remove by key | Dictionary |

Note: `length` (Section 7.3) is also generic — works on lists, arrays, dicts, and strings.

For cons/list chains, `ref` indexes across the full cons chain and supports
negative indexes. A non-`nil` dotted terminal tail is addressable as the final
element, and `length` counts it as one terminal element.
Iteration order contract:
- `keys` and `values` use the same canonical key order.
- Canonical key order is deterministic by key type/value (for common key kinds:
  numeric ascending, string/symbol lexicographic, time-point chronological).
- `values` ordering is key-aligned (`(values d)[i]` corresponds to `(keys d)[i]`).

### 7.13 Set Operations (6)

| Prim | Arity | Description |
|------|-------|-------------|
| `Set` | variadic | Create set |
| `set-add` | 2 | Add element |
| `set-remove` | 2 | Remove element |
| `set-contains?` | 2 | Check membership |
| `length` | 1 | Set cardinality |
| `List` | 1 | Return set elements as list (canonical order) |

Set order contract:
- `List` returns elements in deterministic canonical element order
  (same comparator family as dictionary keys).

### 7.14 Math Library (27)

| Prim | Description |
|------|-------------|
| `sin`, `cos`, `tan` | Trigonometric |
| `asin`, `acos`, `atan` | Inverse trig |
| `atan2` | Two-argument arctangent |
| `exp`, `log`, `log10` | Exponential/logarithmic |
| `pow`, `sqrt` | Power/root |
| `real-part`, `imag-part`, `conjugate` | Complex component access and conjugation; accept any numeric value |
| `math/lgamma` | Natural log of absolute gamma value; domain/range failures raise errors |
| `math/erf`, `math/erfc` | Error function and complementary error function |
| `stats/normal-cdf`, `stats/normal-quantile` | Standard normal CDF and inverse CDF |
| `floor`, `ceiling`, `round`, `truncate` | Rounding; `BigFloat` inputs return exact `Integer`/`BigInteger` results up to the supported allocation cap |
| `abs` | Absolute value; exact `Integer` overflow promotes to `BigInteger`; supports `BigFloat` |
| `min`, `max` | Binary min/max; supports exact `BigInteger` and `BigFloat` comparison |
| `gcd`, `lcm` | Number theory; supports exact `Integer`/`BigInteger` operands |

When a `BigFloat` operand participates, `sin`, `cos`, `tan`, `asin`, `acos`,
`atan`, `atan2`, `exp`, `log`, `log10`, `pow`, `sqrt`, `math/lgamma`,
`math/erf`, `math/erfc`, `stats/normal-cdf`, and `stats/normal-quantile`
return `BigFloat` results. `stats/normal-quantile` keeps its probability-domain
contract and raises when the input is not strictly between `0` and `1`.
`floor`, `ceiling`, `round`, and `truncate` are the exception to the BigFloat
result rule: they return exact integer values, narrowing to `Integer` when
representable and promoting to `BigInteger` otherwise.

### 7.15 Bitwise Operations (6)

| Prim | Description |
|------|-------------|
| `bitwise-and`, `bitwise-or`, `bitwise-xor` | Bitwise logic; supports exact `Integer`/`BigInteger` operands |
| `bitwise-not` | Bitwise complement; supports exact `Integer`/`BigInteger` operands |
| `lshift`, `rshift` | Bit shifting; non-negative shifts use exact integer semantics and may promote to `BigInteger`; negative shifts return `0`; shift counts above the bounded exact-shift cap fail closed |

### 7.16 Conversion (6)

| Prim | Description |
|------|-------------|
| `parse-number` | Parse string to number |
| `String` | Canonical string constructor/coercion surface; dispatches string, number, symbol, and proper list-of-string-fragment conversion |
| `Double` | Canonical double constructor/coercion surface; accepts finite BigInteger and BigFloat inputs when representable as a finite double |
| `Integer` | Canonical integer constructor/coercion surface; truncates finite numeric inputs toward zero and accepts in-range BigInteger/BigFloat inputs |
| `BigInteger` | Canonical arbitrary-precision exact integer constructor/coercion surface; accepts integers and decimal strings |
| `BigFloat` | Canonical high-precision decimal float constructor/coercion surface; accepts numeric values and finite decimal strings |
| `Symbol` | Canonical symbol constructor/coercion surface |

Numeric conversion policy:
- Narrowing to `Integer` (`Integer`, `truncate`) truncates toward zero.
- Narrowing requires finite numeric input and an in-range `Integer` result.
- `parse-number` returns `Integer`, `BigInteger`, `Double`, or `BigFloat`:
  valid decimal integers that exceed the fixed-width `Integer` range promote to
  `BigInteger`, and valid floating inputs that overflow `Double` promote to
  `BigFloat`.
- `parse-number` returns `nil` on parse failure or floating input outside the
  supported `BigFloat` range.
- Constructor/coercion narrowing failures use deterministic recoverable code `type/arg-mismatch`.
- Dispatch does not do implicit numeric widening; cross-numeric calls require explicit conversion.

### 7.17 Introspection & Meta (7)

| Prim | Description |
|------|-------------|
| `type-of` | Type name as symbol |
| `is?` | Type/subtype check |
| `instance?` | Check if type instance |
| `eval` | Evaluate expression |
| `apply` | Apply function to arg list |
| `macroexpand` | Expand macro |
| `bound?` | Check if name is defined |

### 7.17.1 Tensor Construction And Introspection

These primitives are implemented for native `Tensor` values. The current
constructor surfaces support `Double` storage only.

| Prim | Description |
|------|-------------|
| `Tensor` | Construct a native double tensor as `(Tensor data)`, `(Tensor data Double)`, `(Tensor Double data)`, or `(Tensor Double shape data-or-scalar)` |
| `tensor?` | Predicate for native tensor values |
| `dtype` | Return the tensor dtype symbol, currently `'Double` for the first storage path |
| `shape` | Return the tensor shape as an array of dimensions |
| `rank` | Return the tensor rank |
| `contract` | Contract two tensors as `(contract a b axis-pairs)` or `(contract a b left-axes right-axes)` |
| `realize` | Return a concrete tensor, or write a tensor/scalar source into a destination tensor as `(realize expr [out])` |

Tensor indexing is part of generic `ref`. Tensor elementwise operations are
part of generic `map`; unary tensor inputs, tensor-scalar inputs,
scalar-tensor inputs, exact-shape tensor-tensor inputs, and right-aligned
singleton-axis tensor-tensor broadcasting are supported in the current `Double`
slice. Scalar arguments are coerced into the first tensor input's dtype and
broadcast over the tensor shape. Rank-0 tensors broadcast as tensor scalars, and
incompatible tensor shapes raise `tensor/shape-mismatch`. Tensor `map` and
`contract` may return lazy Tensor expression payloads under the existing
`Tensor` value; `realize` forces them either
by allocating concrete storage or by writing directly into an exact-shape/dtype
destination tensor. Elementwise `map` destination realization may update
an input tensor in place; `contract` destination realization rejects
destinations that alias either source tensor. `(Array tensor)` and
`(List tensor)` are explicit collection conversions: they force lazy tensor
expressions if needed and return flat row-major element values. They do not
encode shape nesting; use `shape` alongside the converted data when preserving
rank is required. Tensor contraction uses paired axes as
`(contract a b [left-axis right-axis])` for one contracted pair or
`(contract a b [[left-axis right-axis] ...])` for multiple pairs. The explicit
left/right axis-list form `(contract a b left-axes right-axes)` is also
accepted. Axis lists may be arrays or proper lists of integers, negative axes
normalize from the end of each tensor's rank, each paired contracted dimension
must match, and the result shape is all non-contracted left axes followed by
all non-contracted right axes.

```lisp
(define x (Tensor [[1.0 2.0 3.0] [4.0 5.0 6.0]]))
(ref x [1 2])   ; => 6.0
(ref x [1 -1])  ; => 6.0

(define z (map + x 1.0))
(ref z [1 2])   ; => 7.0

(define a (Tensor Double [2 3] [1 2 3 4 5 6]))
(define b (Tensor Double [3 2] [7 8 9 10 11 12]))
(ref (contract a b [1 0]) [1 1]) ; => 154.0

(realize x)                 ; => x, because x is already concrete
(define y (Tensor Double [2 3] 0.0))
(realize x y)               ; => y, after copying x into y
(realize 1.0 y)             ; => y, after filling y with 1.0
(realize (map + x 1.0) y)   ; => y, after evaluating into y
```

### 7.18 Error Handling (2)

| Prim | Description |
|------|-------------|
| `error` | Create error value |
| `error-message` | Extract message from error |

### 7.19 Miscellaneous (5)

| Prim | Description |
|------|-------------|
| `gensym` | Generate unique symbol |
| `format` | Format string with values |
| `sort` | Sort list |
| `sort-by` | Sort list by comparator |
| `read-string` | Parse string to Lisp value |

### 7.20 FFI (Declarative)

```lisp
;; Declare a library handle
(define [ffi lib] libc "libc.so.6")

;; Bind a C function as a native Omni function
(define [ffi λ libc] (strlen (^String s)) ^Integer)
(define [ffi λ libc] (abs (^Integer n)) ^Integer)

;; Grouped C ABI module sugar over [ffi lib] plus [ffi λ]
(define [ffi module] libc2 "libc.so.6"
  (strlen (^String s)) ^Integer
  (abs (^Integer n)) ^Integer
  (fopen (^String path) (^String mode))
    ^{'name File 'ownership owned 'finalizer fclose})

(strlen "hello")  ; => 5
(abs -42)          ; => 42
```

- Uses libffi via C wrapper for portable ABI support
- Type annotations: `^Integer` -> sint64, `^Double` -> double, `^String`
  -> copied plain `char*` boundary, `^ForeignHandle` -> boxed opaque foreign
  handle, `^Void` -> void, `^Boolean` -> sint64
- `^ForeignHandle` is the simple default foreign-handle annotation. It
  represents opaque foreign resources as `FFI_HANDLE` boxes; user
  code does not pass raw integer addresses.
- In `ffi λ`, FFI-local metadata dictionaries may refine a foreign handle
  policy. `^{'name File 'ownership owned 'finalizer fclose}` implies
  `ForeignHandle`; the explicit `^{'type ForeignHandle ...}` form is also
  accepted. Dictionary entries are key/value pairs with quoted symbol keys;
  Omni does not use colon keywords.
- Supported `ForeignHandle` metadata keys are `'type`, `'name`, `'ownership`,
  `'finalizer`, and `'nullability`. Supported ownership values are `borrowed`,
  `owned`, and `manual`; supported nullability values are `nullable` and
  `non-null`. An owned return handle requires a finalizer. Owned parameter
  policies are rejected because a call argument cannot transfer Omni-side
  finalizer authority safely in the current runtime.
- Foreign handles carry a small common runtime descriptor internally:
  runtime kind, handle kind, and capability bits. The user-facing
  introspection surface is `(foreign-describe value)`. For a `ForeignHandle`,
  it returns a dictionary using quoted-symbol keys: `'type`, `'runtime`,
  `'kind`, `'ownership`, `'name`, `'live`, and `'capabilities`. Capability
  sequences are arrays of symbols so the shape matches metadata sequence fields
  such as `'parameters [..]`. Opaque foreign-resource handle names such as
  `File` are returned as symbols; C ABI library handle names remain strings
  because they are the `dlopen` target. C ABI library handles reflect
  load/resolve/reflect capabilities; direct call capability is reflected on
  `ForeignCallable` descriptors for bound functions.
- `(foreign-describe ffi-bound-function)` returns reflection metadata for C ABI
  functions declared through `[ffi lambda]`: `'type 'ForeignCallable`, `'runtime
  'c-abi`, `'kind 'function`, `'name`, `'c-name`, `'library`, `'parameters`,
  `'returns`, `'resolved`, `'available`, and `'capabilities`. `'parameters` is
  an array of descriptor dictionaries; `'returns` is one descriptor dictionary.
  Each descriptor includes user-facing `'type` and ABI-level `'abi-type`.
  `ForeignHandle` descriptors also preserve `'name`, `'ownership`,
  `'nullability`, and `'finalizer` metadata when present.
- `(foreign-release handle)` is the explicit release boundary for releasable
  `ForeignHandle` payloads. It returns `Void`, is idempotent for already-closed
  handles, clears the payload pointer observed by `foreign-describe`, and
  makes released handles stop reflecting as `'ownership owned` because the
  live pointer and release capability are gone. It
  rejects ordinary non-releasable library or borrowed resource handles. This is
  only for foreign resources and does not add refcount or garbage-collected
  ownership for Omni values.
- Declarative `ffi λ` accepts only `^Integer`, `^Double`, `^String`,
  `^ForeignHandle`, `^Boolean`, and `^Void` at the base annotation level;
  unsupported annotations fail at definition time instead of defaulting to
  foreign-handle metadata.
- Argument conversion is fail-closed:
  - `^Integer`: Omni `Integer` only
  - `^Double`: Omni `Double` or `Integer`
  - `^Boolean`: Omni `true` / `false` only
  - `^String`: Omni `String`, or `nil` for a null plain `char*`
  - `^ForeignHandle`: live `FFI_HANDLE`, or `nil` for null
  - FFI-local metadata dictionaries in `ffi λ` use the same quoted-symbol key/value dictionary syntax as the rest of Omni; no colon keywords are used.
- Return conversion follows the same no-raw-pointer rule: non-null `^String`
  C plain `char*` returns are copied into Omni `String` values, null string
  returns become `nil`, non-null `^ForeignHandle` C pointer returns become
  `ForeignHandle` values, and null pointer returns become `nil`.
  Generated bindings keep byte pointers such as `signed char*` and
  `unsigned char*` as `ForeignHandle` values instead of string-shaped
  parameters or returns.
  For plain character pointers, `const char*` and `char const*` are
  string-input shaped; `char* const` is still mutable pointee storage and stays
  on the mutable string-buffer wrapper path.
  Only single-level plain `char*` pointers are string-shaped; pointer-to-pointer
  spellings such as `char**` and `const char**` remain opaque `ForeignHandle`
  values.
  Name-based mutable string-buffer direction inference treats `inout` as more
  specific than `out`, so names such as `inout_buffer` generate
  `buffer-direction=inout`.
- Declarative `variadic` bindings are currently rejected at definition time until the runtime carries explicit fixed/variadic metadata
- Execution mode contract: interpreter/JIT `ffi λ` enforces `ForeignHandle`
  metadata dictionaries. AOT lowering carries the same policy into generated
  declarations with per-parameter handle family/nullability descriptors and a
  return handle name/ownership/finalizer descriptor.
- `Nil` is the language-level empty/false value type; `Void` is a real singleton runtime value/type, and FFI `^Void` returns produce that value
- Lazy dlsym: symbol resolution deferred to first call and cached
- Grouped FFI module syntax is shipped as parser sugar:
  `(define [ffi module] lib "path" (fn (^Type arg)) ^Return ...)` expands to
  one `[ffi lib]` declaration plus one `[ffi λ]` declaration per function. The
  body is a flat sequence of `(signature, return-annotation)` pairs: no `->`
  syntax and no bracketed body entries. Malformed body sequences fail closed,
  and `ForeignHandle` metadata dictionaries use the same policy validation and
  AOT descriptors as the single-function form.

### 7.21 Constants

| Name | Value |
|------|-------|
| `true` | Symbol `true` |
| `false` | Boolean false |
| `pi` | 3.141592653589793 |
| `e` | 2.718281828459045 |

### 7.22 Memory

| Primitive | Args | Description |
|-----------|------|-------------|
| `unsafe-free!` | 1 | Free heap backing of array/dict/instance/string and return `Void`. Value becomes an error — accessing it after free raises "use after unsafe-free!". No-op on int/nil/other non-heap types also return `Void`. |

### 7.23 Regex and Pika Parsing

#### Regex primitives

| Primitive | Args | Description |
|-----------|------|-------------|
| `re-match` | 2 | First match anywhere in input (or `nil`) |
| `re-fullmatch` | 2 | Match entire input (or `nil`) |
| `re-find-all` | 2 | Non-overlapping match list |
| `re-split` | 2 | Split input on regex matches |
| `re-replace` | 3-4 | Replace first match, or all with `'global` |
| `re-match-pos` | 2 | Match positions as `(start end)` or `nil` |
| `re-find-all-pos` | 2 | List of `(start end)` pairs |

Supported regex constructs:
- literals, char classes (`[a-z]`, `[^x]`), shorthand classes (`\d`, `\w`, `\s`)
- quantifiers (`*`, `+`, `?`, `{n}`, `{n,m}`, `{n,}`)
- alternation (`|`), grouping (`(...)`, `(?:...)`)
- lookahead (`(?=...)`, `(?!...)`)
- anchors (`^`, `$`)

Regex semantics notes:
- Deterministic Pika-style matching (no catastrophic backtracking engine behavior).
- Possessive quantifiers (`*+`, `++`, `?+`) are accepted with deterministic semantics.
- Invalid patterns are rejected strictly (unclosed groups, malformed bounds, bad ranges, trailing junk).
- Bounded quantifier upper/lower bounds are validated (`{n,m}` with large bounds is rejected).

#### Pika grammar primitives

| Primitive | Args | Description |
|-----------|------|-------------|
| `pika/grammar` | variadic | Define a named grammar from quoted `(rule ...)` forms |
| `pika/parse` | 2 | Parse input with named grammar |
| `pika/fold` | 3 | Fold parse tree with user function |
| `pika/grammar-rules` | 1 | List rule names for a named grammar |
| `pika/parse-lisp` | 1 | Parse Omni/Lisp source with built-in grammar |

Pika grammar notes:
- Use quoted rule forms with `pika/grammar`, for example:
  `(pika/grammar 'g '(rule start (seq "a" "b")))`
- Unreachable rules are pruned from compiled grammar graphs.
- Scanner clauses are context-aware; there is no fixed small scanner-slot cap.

**Total: 130+ primitives**

### 7.24 Primitive Command/Query Contract

Primitive return semantics are classified into two normative styles:
- command-style: successful completion returns `Void` (or is a non-returning command such as `exit`).
- query-style: returns data/handle/predicate values; if absence semantics apply, absence is represented with `nil` (never `Void`).

Audit source of truth is the registered primitive table in `src/lisp/eval_init_primitives.c3`.
Audit snapshot (`2026-03-17`):
- total registered primitive names: `263`
- command-style names: `42`
- query-style names: `221`

Core mutation, I/O, and scheduler command/query classification tables are maintained in:
- `docs/reference/11-appendix-primitives.md` (`Primitive Surface Audit: Command vs Query`).

Exhaustive classification rule:
- names in the audited command-style set are command-style by contract,
- every other registered primitive name is query-style by contract.

---

## 8. Standard Library

Higher-order functions and utilities defined in Omni:

| Function | Signature | Description |
|----------|-----------|-------------|
| `map` | `(f coll)` | Apply f to each element (dispatched by collection type) |
| `filter` | `(pred coll)` | Keep elements matching predicate (dispatched; lazy for iterators) |
| `foldl` | `(f acc coll)` | Left fold (dispatched) |
| `foldr` | `(f init coll)` | Right fold (finite collections) |
| `append` | `(a b)` | Concatenate lists |
| `reverse` | `(coll)` | Reverse finite collection (list or array) |
| `compose` | `(f g)` | Function composition |
| `id` | `(x)` | Identity function |
| `nth` | `(n lst)` | Nth element |
| `take` | `(n coll)` | First N elements (dispatched; lazy for iterators) |
| `drop` | `(n coll)` | Drop first N elements (dispatched; lazy for iterators) |
| `zip` | `(a b)` | Zip two collections (dispatched; lazy for iterators) |
| `range` | `(n)` | List from 0 to n-1 |
| `range-from` | `(n)` | Infinite iterator from n |
| `repeat` | `(x)` | Infinite iterator repeating x |
| `cycle` | `(coll)` | Infinite iterator cycling coll |
| `for-each` | `(f lst)` | Apply f for side effects |
| `any?` | `(pred lst)` | Any element matches? |
| `every?` | `(pred lst)` | All elements match? |
| `flatten` | `(lst)` | Flatten nested list (1 level) |
| `partition` | `(pred lst)` | Split by predicate |
| `remove` | `(pred lst)` | Remove matching elements |
| `find` | `(pred lst)` | First matching element |

Stdlib functions take multiple parameters with strict arity. For partial application, `_` placeholders create lambdas `(map (+ 1 _) '(1 2 3))`, `_n` placeholders support reuse/reordering (`((- _2 _1) 3 10)`), `|>` rewrites pipeline steps by appending the piped value, and `partial` provides runtime partial application.

Compatibility note: `_n` placeholder desugaring is only active in call-argument position. Outside call arguments `_n` remains a normal symbol; however, existing call-argument bindings named like `_2` will now be interpreted as indexed placeholders.

### 8.1 Macros

| Macro | Description |
|-------|-------------|
| `when` | `(when test body...)` -- if test, evaluate body |
| `unless` | `(unless test body...)` -- if not test, evaluate body |
| `branch` | `(branch (c1 e1) ... (_ default))` -- condition chain with explicit default marker |

`when` and `unless` predicate checks use the same truthiness contract as `if`
([2.2](#22-truthiness)).

For multi-branch condition chains, prefer `branch`:

```lisp
(branch ((> x 0) "positive")
        ((= x 0) "zero")
        (_ "negative"))
```

`_` is the default marker and must appear only in final position. If no
condition matches and no `_` clause is provided, `branch` returns `nil`.

Equivalent low-level form using `match` + guards:

```lisp
(match Void
  ((? (> x 0)) "positive")
  ((? (= x 0)) "zero")
  (_ "negative"))
```

### 8.2 Effect Utilities

| Name | Description |
|------|-------------|
| `try` | `(try thunk handler)` -- catch `raise` effects |
| `assert!` | `(assert! condition msg)` -- raise if condition fails |
| `yield` | Macro for generator-style values |
| `stream-take` | Take N values from a generator stream |

### 8.3 Lazy Evaluation

| Name | Description |
|------|-------------|
| `delay` | `(delay thunk)` -- create lazy value (memoized) |
| `force` | `(force promise)` -- force lazy evaluation |

---

## 9. Delimited Continuations

### 9.1 `checkpoint` -- Establish Delimiter

```lisp
(checkpoint body)
```

### 9.2 `capture` -- Capture Continuation

```lisp
(capture k body)
```

Captures the continuation up to the enclosing `checkpoint` and binds it to `k`.

```lisp
(checkpoint (+ 1 (capture k (k (k 10)))))
; k = (lambda (x) (+ 1 x))
; (k (k 10)) = (+ 1 (+ 1 10)) = 12
```

### 9.3 Semantics

- Continuations are **multi-shot**: each invocation of `k` clones the captured stack, so `k` can be called multiple times
- Continuation `resume` operation is function invocation: `(k value)` invokes the captured continuation with `value`
- The result of `capture`'s body becomes the result of `checkpoint`
- Multi-shot replay is explicit: each `k` invocation re-runs the resumed continuation segment, including side effects in that segment (`set!`, `signal`, I/O handlers), in source order.
- Each `k` invocation starts from the captured stack snapshot; lexical locals in that resumed segment are restored per invocation unless mutation targets shared state outside the snapshot.
- Side effects that occur in `capture` body code outside resumed continuation segments run when that code executes; they are not replayed unless that body path itself is executed again.
- Replay semantics are execution-mode invariant: interpreter, JIT, and compiled
  execution must preserve the same replay-visible side-effect outcomes.

Canonical continuation-resume example:

```lisp
(checkpoint (+ 1 (capture k (k 41))))
; => 42
```

Note: this continuation `resume` operation (`(k value)`) is distinct from the
coroutine primitive `resume`.

Replay example:

```lisp
(checkpoint (+ 1 (capture k (+ (k 10) (k 20)))))
; => 32

(block
  (define c 0)
  (checkpoint
    (+ (capture k (+ (k 1) (k 1)))
       (block
         (handle (signal io/println "x")
           (io/println msg (resolve 0)))
         (set! c (+ c 1))
         c))))
; handled I/O in resumed segment is replayed per k invocation
```

---

## 10. Effect Handlers

Normative semantics for this section are defined in `docs/EFFECTS_SEMANTICS.md`.

### 10.0 Glossary

| Term | Meaning |
|------|---------|
| `signal` | Emit an effect request with tag+payload from the current evaluation context. |
| `raise` | Conventional effect tag for failure signaling (`signal raise payload`). |
| `resolve` | Resume a captured effect continuation with a value from inside a handler clause. |
| `abort` | Handler path where `resolve` is not called; handler return becomes `handle` result. |
| `resumable` | Effect interaction where control returns to the suspended signal site after `resolve`. |
| `effect boundary` | Runtime boundary where effect semantics are interpreted (handler stack, scheduler callback handoff, or unhandled-effect escalation path). |

### 10.1 `signal` -- Signal Effect

```lisp
(signal effect-tag argument)
```

### 10.2 `handle` -- Install Handler

```lisp
(handle body
  (effect-tag arg handler-body...)
  ...)
```

When an effect is signalled:
- `arg` is bound to the effect argument
- `handler-body` can resolve with `(resolve value)` to resume, or return a value to abort

```lisp
(handle
  (+ 1 (signal read nil))
  (read x (resolve 41)))
; => 42
```

### 10.3 `resolve` -- Resume Computation

Inside a handler clause, `(resolve value)` sends `value` back to the body.
The body continues as if `signal` returned that value.

If `resolve` is not called, the handler's return value becomes the result
of the entire `handle` expression (abort).

Discipline contract:
- `resolve` is handler-bound: it must target the hidden continuation from the
  current `handle` clause (`__k`).
- `resolve` is single-shot for that continuation. A second `resolve` attempt on
  the same continuation raises deterministic recoverable code
  `runtime/continuation-resumed`.
- Multi-shot behavior is available only through explicit continuation calls
  (`with-continuation` + `(k ...)`), not through repeated `resolve`.

```lisp
; Resolve — body continues
(handle (signal double 5)
  (double x (resolve (* x 2))))
; => 10

; Abort — body abandoned
(handle (+ 1 (signal bail 42))
  (bail x x))
; => 42
```

### 10.4 I/O Effects

I/O operations go through effects with a fast path:

```lisp
; These use io/print, io/println, etc. effect tags
(println "hello")     ; fast path when no handler
(print 42)

; Custom handler intercepts I/O
(handle (block (println "suppressed") 42)
  (io/println x (resolve nil)))
; => 42 (output suppressed)

; Capture output
(handle (block (println "captured") nil)
  (io/println x x))
; => "captured"
```

Effect tags: `io/print`, `io/println`, `io/display`, `io/newline`, `io/read-line`, `io/read-file`, `io/write-file`, `io/file-exists?`, `io/read-lines`

### 10.5 Typed Dispatch in Handlers

Effect handlers match on tag name only. For type-specific behavior, use dispatched functions inside the handler body — this reuses the existing MethodTable dispatch system rather than introducing a parallel matching mechanism:

```lisp
(define (on-show (^Integer x)) (string-append "int: " (String x)))
(define (on-show (^String s)) (string-append "str: " s))

(handle
  (block (signal show 42) (signal show "hello"))
  (show x (println (on-show x)) (resolve nil)))
```

### 10.6 Composition Helper Naming (Migration Note)

For helper-style handler composition in examples and public-facing docs:
- use `handle/chain` as the canonical helper name
- do not introduce new abbreviated aliases for this helper
- migrate historical `with-handlers` / `handle-chain` example spellings to
  `handle/chain`

---

## 11. Macros

### 11.1 Single-Transformer Macros

```lisp
(define [macro] when
  (syntax-match
    ([test .. body]
      (template (if (insert test) (block (splice body)) nil))))

(define [macro] unless
  (syntax-match
    ([test .. body]
      (template (if (insert test) nil (block (splice body))))))
```

- One macro surface only: `(define [macro] name (syntax-match ...))`
- Reader tag macros use `(define [reader tag] name (syntax-match ...))`;
  `#name form` parses as `(name form)`.
- Macros are syntax transformers, not overloaded callable sets
- Pattern-based with template substitution
- Hygienic: template literals resolve at definition time
- Auto-gensym: `name#` in templates generates unique symbols
- `gensym` function for manual hygiene
- Legacy clause-style macro definitions are rejected with deterministic diagnostics

### 11.2 Expansion

```lisp
(macroexpand '(when true 1 2 3))
; => (if true (block 1 2 3) nil)
```

---

## 12. Modules

```lisp
(module math-utils (export add multiply)
  (define (add a b) (+ a b))
  (define (multiply a b) (* a b)))

;; Qualified access (default)
(import math-utils)
(math-utils.add 3 4)  ; => 7

;; Selective import
(import math-utils (add multiply))
(add 3 4)  ; => 7

;; Rename on import
(import math-utils (add 'as plus))
(plus 3 4)  ; => 7

;; Import all exports unqualified
(import math-utils 'all)
(add 3 4)  ; => 7

;; Dotted/path module target
(import ui.nodes)
(ui.nodes.text "ok")

;; Re-export
(export-from math-utils (add))
(export-from math-utils 'all)
(export-from ui.nodes (text))
```

- Default import is **qualified-only**: `(import mod)` binds module as value, access via `mod.sym`
- Selective import: `(import mod (sym1 sym2))` for specific symbols
- Rename: `(import mod (sym1 'as alias))` for renaming on import
- `'all` imports all exports unqualified (opt-in)
- Omni has no dedicated keyword type; `'as`/`'all` are quoted symbols used as explicit module markers
- `export-from` re-exports symbols from another module
- Module targets for `module` / `import` / `export-from` can be:
  - symbol (`math-utils`)
  - dotted/path token (`ui.nodes`)
  - string file path (`"path/to/file.omni"`)
- File-based import: `(import "path/to/file.omni")`
- Cached: modules loaded only once
- Circular import detection
- Method extensions are always global (dispatch is cross-cutting)
- `module` / `import` / `export-from` are command-style forms and return `Void` on successful completion
- Compiler backend (`AOT`) currently uses static module lowering: module bodies are inlined during compilation, and `import` / `export-from` lower to command-style `Void` no-ops (no runtime module loading/binding pass in generated code)

---

## 13. REPL

```bash
omni --repl
omni --repl --project
omni --repl --load demo.omni
```

```
Omni Lisp REPL (type 'quit' or 'exit' to leave)
---
> (define x 10)
10
> (+ x 5)
15
> (define (inc n) (+ n 1))
#<closure>
> (inc x)
11
> quit
Goodbye!
```

Project preload:

```bash
omni --repl --project
omni --repl --project myproject
omni --repl --load demo.omni
```

- `--project` resolves an Omni project root from the current directory or the
  optional directory argument.
- It requires `omni.toml` and preloads `src/main.omni` before the interactive
  session starts.
- `--load` preloads one Omni source file directly and works for standalone
  example/workspace trees that are not full Omni projects.
- Relative imports inside `src/main.omni` use the entry file's source
  directory, the same way script execution does.
- text REPL preload read failures preserve the concrete file-read cause
  (`file not found`, `permission denied`, `invalid path`, or generic
  `read failed`) instead of collapsing to one generic startup message.
- REPL preload (`--project` or `--load`) is text-REPL-only; it is not
  supported with `--json`.

---

## 14. Examples

### 14.1 Factorial

```lisp
(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))
(fact 10)  ; => 3628800
```

### 14.2 Fibonacci with Dispatch

```lisp
(define (fib (^(Value 0) n)) 0)
(define (fib (^(Value 1) n)) 1)
(define (fib (^Integer n)) (+ (fib (- n 1)) (fib (- n 2))))
(fib 10)  ; => 55
```

### 14.3 Option Type

```lisp
(define [union] (Option T) None (Some T))

(define (safe-div a b)
  (if (= b 0) None (Some (/ a b))))

(match (safe-div 10 3)
  (None "division by zero")
  ((Some x) x))
; => 3
```

### 14.4 Effect Handler for State

```lisp
(handle
  (let (x (signal get nil))
    (block
      (signal put (+ x 1))
      (signal get nil)))
  (get _ (resolve 0))
  (put v (resolve nil)))
```

### 14.5 Collection Literals and Generic Operations

```lisp
; Array literal
(define nums [1 2 3 4 5])
(ref nums 0)           ; => 1
(length nums)           ; => 5
(push! nums 6)          ; mutates, adds 6

; Dictionary literal
(define person {'name "Alice" 'age 30})
(ref person 'name)      ; => "Alice"
(has? person 'age)      ; => true
(keys person)           ; => '(age name)

; Constructor dispatch
(Array '(1 2 3))        ; list → array conversion
(list [1 2 3])          ; array → list conversion
(Array (take 5 (range-from 0))) ; force iterator into array
(list (take 5 (range-from 0)))  ; force iterator into list

; Cons mutation via dot-path
(define p (cons 1 2))
(set! p.car 99)
p.car                   ; => 99
```

### 14.6 Type Hierarchy

```lisp
(define [abstract] Shape)
(define [type] (Circle Shape) (^Integer radius))
(define [type] (Rect Shape) (^Integer width) (^Integer height))

(define (area (^Circle c)) (* pi (* c.radius c.radius)))
(define (area (^Rect r)) (* r.width r.height))

(area (Circle 5))      ; => ~78.5
(area (Rect 3 4))      ; => 12
```

---

## 15. CLI & Project Tooling

### 15.1 Running Programs

```bash
omni script.omni    # Run a script
omni --repl         # Start the REPL (explicit)
omni --repl --project [dir]  # Start REPL with project entry preloaded
omni --repl --load <file>    # Start REPL with one file preloaded
omni                # Start the REPL (default)
```

### 15.2 Compilation

```bash
omni --compile input.omni output.c3                         # Omni → C3 source
omni --build input.omni -o output                           # Omni → standalone binary (AOT)
```

### 15.3 Project Management

```bash
omni --init myproject                                       # Scaffold project directory
omni --bind myproject/                                      # Generate FFI bindings from omni.toml
```

- `--init` creates `omni.toml`, `src/main.omni`, `lib/ffi/`, `include/`, `build/` (with generated `project.json`) and now rolls back the fresh project root if a later scaffold write fails or a subpath collides with a non-directory
- `--bind` reads `omni.toml`, parses C headers via libclang, writes regenerated raw FFI modules plus facade stubs to `lib/ffi/`
- libclang is an optional runtime dependency (only needed for `--bind`)

See `docs/PROJECT_TOOLING.md` for the complete reference including `omni.toml` format, build configuration, type mapping, and workflow examples.

---

## Appendix A: Grammar (EBNF)

```ebnf
program     = { expr } ;
expr        = literal | symbol | path | quoted | quasiquoted
            | list | array_lit | dict_lit | indexed | accessor ;

literal     = integer | float | string ;
integer     = [ "-" ] digit { digit } ;
float       = [ "-" ] digit { digit } "." digit { digit } ;
string      = '"' { char | escape } '"' ;
symbol      = symbol_char { symbol_char } ;
path        = symbol "." symbol { "." symbol } ;

quoted      = "'" datum ;
quasiquoted = "`" datum ;
list        = "(" { expr } ")" ;
array_lit   = "[" { expr } "]" ;           (* equivalent to Array constructor call *)
dict_lit    = "{" { expr expr } "}" ;      (* equivalent to Dictionary constructor call; must be even *)
indexed     = expr ".[" expr "]" ;
accessor    = "." expr ;

datum       = literal | symbol | "(" { datum } ")" | "'" datum ;

symbol_char = letter | digit | "_" | "-" | "+" | "*" | "/"
            | "=" | "<" | ">" | "!" | "?" | ":" | "@" | "#"
            | "$" | "%" | "&" | "|" | "^" | "~" ;
```

---

## Appendix B: Limits

| Resource | Limit |
|----------|-------|
| Symbol/string length | Dynamic (heap-allocated) |
| Total symbols | 8192 |
| Bindings per env frame | 512 |
| Match clauses | Dynamic (no fixed limit) |
| Pattern elements | Dynamic (no fixed limit) |
| Effect handler clauses | Dynamic (no fixed limit) |
| Handler stack depth | 16 |
| Call arguments | Dynamic AST (JIT compiles up to 16 natively) |
| Path segments | 8 |
| Block expressions | Dynamic (no fixed limit) |
| Lambda params | Dynamic (no fixed limit) |
| String literal (inline) | 63 bytes (lexer limit) |
| Macros | 64 |
| Macro transformer branches | Dynamic (inside `syntax-match`) |
| Modules | 32 |
| Module exports | 128 |
| Eval depth | 5000 |
| Registered types | 256 |
| Type fields | 16 |
| Method table entries | 64 |

---

## Appendix C: Backends

| Feature | Interpreter | JIT | Compiler |
|---------|:-----------:|:---:|:--------:|
| lambda/define/let/if | Y | Y | Y |
| block/set!/and/or | Y | Y | Y |
| quote/quasiquote | Y | Y | Y |
| match | Y | Y | Y |
| checkpoint/capture | Y | Y | Y |
| handle/signal/resolve | Y | Y | Y |
| type definitions | Y | Y | Y† |
| dispatch | Y | Y | Y† |
| macros | Y | Y** | Y** |
| modules | Y | Y | Y |
| declarative FFI (`[ffi lib]` / `[ffi λ]`) | Y | Y | N‡ |

† Compiler parity for type definitions and dispatch is validated. Generated calls go through `aot::invoke`/`aot::apply_multi` (non-AOT callables route to `jit_apply*`), while type-definition and typed-define registration lower through explicit structured AOT helpers. AOT closure wrappers are classified semantically as `Closure` for runtime introspection parity, and compiled module/import/export surfaces are intentionally static (inline module bodies, `import`/`export-from` no-op `Void` lowering).
**Y** = macro expansion at parse time
‡ Compiler tests currently assert declarative FFI forms are rejected in AOT mode.

---

*Omni Lisp -- A Lisp with delimited continuations, algebraic effects, strict-arity lambdas, multiple dispatch, and structural types*
