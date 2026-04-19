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

Callable type symbols (`Integer`, `Float64`, `String`, `Boolean`, `List`,
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
(define (score (^Float64 x) (^Float64 y)) (+ x y))

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
