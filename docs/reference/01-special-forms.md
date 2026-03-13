# Special Forms, Pattern Matching & Destructuring

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 4. Special Forms

### `lambda` — Function Definition

```lisp
(lambda (x) (* x x))             ;; single param
(lambda (x y) (+ x y))           ;; multi-param (strict arity)
(lambda () 42)                    ;; zero-arg
(lambda (x .. rest) rest)         ;; variadic
(lambda ({name age}) name)        ;; dict destructuring param
```

Multi-param lambdas require exactly the right number of arguments. Use `_`
placeholder, `|>` pipe, or `partial` for partial application (see
[Section 8](02-functions.md#8-partial-application--pipe)).

Lambda bodies support implicit block — multiple expressions are evaluated in
sequence, returning the last:

```lisp
(lambda (x)
  (println x)
  (* x 2))
```

### `define` — Global Definition

```lisp
(define x 42)                              ;; value
(define (f x y) (+ x y))                   ;; function shorthand
(define (describe (^Int n)) "integer")      ;; typed (creates dispatch entry)
(define (connect {host port}) body)         ;; dict destructuring param
```

`(define (f x y) body)` desugars to `(define f (lambda (x y) body))`.

Brackets in `define` are reserved for attributes: `[type]`, `[macro]`,
`[abstract]`, `[union]`, `[alias]`, `[effect]`, `[schema]`, `[relation]`,
`[ffi lib]`, `[ffi λ]`.

### `let` — Local Binding

```lisp
;; Simple (flat pairs — no double parens)
(let (x 10) (+ x 1))                       ;; => 11

;; Multi-binding (sequential left-to-right)
(let (x 1 y 2) (+ x y))                    ;; => 3
(let (x 1 y (+ x 2) z (+ y 3)) z)          ;; => 6

;; Array destructuring
(let ([a b] [10 20]) (+ a b))              ;; => 30
(let ([head .. tail] '(1 2 3)) head)       ;; => 1
(let ([a b ..] '(1 2 3 4 5)) (+ a b))     ;; => 3

;; Dict destructuring
(let ({name age} {'name "Alice" 'age 30}) name) ;; => "Alice"

;; Mixed
(let ([a b] [3 4] z 5) (+ a (+ b z)))     ;; => 12
```

### `let ^rec` — Recursive Local Binding

```lisp
(let ^rec (fact (lambda (n)
  (if (= n 0) 1 (* n (fact (- n 1))))))
  (fact 5))   ;; => 120
```

### Named `let` — Loop Construct

```lisp
(let loop (n 5 acc 1)
  (if (= n 0)
    acc
    (loop (- n 1) (* acc n))))   ;; => 120
```

Named let uses flat-pair loop variables with sequential left-to-right
initializers. It lowers through an outer sequential `let` and an inner
`let ^rec`, and calling the name recurs with new values.

### `if` — Conditional

```lisp
(if (> x 0) "positive" "non-positive")
```

All three branches required. Only the chosen branch is evaluated.

### `block` — Sequencing

```lisp
(block
  (println "hello")
  (println "world")
  42)   ;; => 42
```

Evaluates all expressions in order, returns the last. Last expression is in
tail position.

### `set!` — Mutation

```lisp
(set! x 42)                   ;; variable mutation
(set! point.x 99)             ;; struct field mutation
(set! pair.car 10)             ;; cons cell car mutation
(set! obj.nested.field 5)      ;; nested field mutation
(set! arr 0 99)               ;; generic collection update
```

Returns `Void` on successful mutation.
`array-set!` / `dict-set!` remain compatibility aliases.

### `quote` / `quasiquote`

```lisp
'foo                            ;; => symbol foo
'(1 2 3)                        ;; => list (1 2 3)

`(a ,(+ 1 2) ,@(list 3 4))     ;; => (a 3 3 4)
```

Quasiquote supports nesting with depth tracking.

### `and` / `or` — Short-Circuit Logic

```lisp
(and true 42)     ;; => 42
(and nil 42)      ;; => nil
(or 42 99)        ;; => 42
(or nil 99)       ;; => 99
```

Binary only. `and` returns the left operand if falsy, else the right.
`or` returns the left operand if truthy, else the right.

---

## 5. Pattern Matching

```lisp
(match expr
  (pattern1 result1)
  (pattern2 result2)
  (_ default))
```

### Pattern Types

| Pattern | Example | Description |
|---------|---------|-------------|
| `_` | `(_ "default")` | Wildcard — matches anything |
| Variable | `(x (* x 2))` | Binds value to name |
| Integer | `(0 "zero")` | Matches exact integer |
| String | `("hi" "greeting")` | Matches exact string |
| Quoted symbol | `('red "red")` | Matches quoted datum |
| Exact sequence | `([x y] (+ x y))` | Matches list/array of exactly N elements |
| Head-tail | `([first .. rest] first)` | Binds head and rest |
| Prefix | `([a b ..] (+ a b))` | Matches first N, ignores rest |
| Suffix | `([.. last] last)` | Skips to last element(s) |
| Constructor | `((Some v) v)` | Matches union variant with fields |
| Nullary ctor | `(None "empty")` | Matches nullary constructor |

### Examples

```lisp
;; Value matching
(match x
  (0 "zero")
  (1 "one")
  (_ "other"))

;; Destructuring
(match '(1 2 3)
  ([a b c] (+ a (+ b c))))   ;; => 6

;; Head-tail decomposition
(match '(10 20 30)
  ([head .. tail] head))      ;; => 10

;; Union variant matching
(match (Some 42)
  (None "empty")
  ((Some x) x))               ;; => 42

;; Nested patterns
(match '(1 2)
  ([1 y] (+ 10 y))            ;; => 12
  ([x y] (+ x y)))
```

Falls through on mismatch. Returns nil if no pattern matches.

---

## 6. Destructuring

Destructuring works in `let`, `match`, and lambda/define parameters.

### Array Destructuring

```lisp
;; In let
(let ([x y z] [10 20 30]) (+ x y z))          ;; => 60
(let ([head .. tail] '(1 2 3 4)) tail)         ;; => (2 3 4)
(let ([a b ..] '(1 2 3 4 5)) (+ a b))         ;; => 3
(let ([.. last] '(1 2 3 4 5)) last)            ;; => 5
(let ([_ b _] [10 20 30]) b)                   ;; => 20

;; Works on both lists and arrays
(let ([a b] '(1 2)) (+ a b))   ;; => 3
(let ([a b] [1 2]) (+ a b))    ;; => 3
```

### Dict Destructuring

```lisp
;; In let — keys become local bindings
(let ({name age} {'name "Alice" 'age 30})
  (string-append name " is " (number->string age)))
;; => "Alice is 30"

;; Missing keys become nil
(let ({z} {'x 10}) z)   ;; => nil

;; In function parameters
(define (greet {name greeting})
  (string-append greeting ", " name "!"))

(greet {'name "Alice" 'greeting "Hello"})
;; => "Hello, Alice!"
```

### In Match Patterns

```lisp
(match '(1 2 3)
  ([1 b c] (+ b c))       ;; => 5
  ([a b c] (* a b c)))

(match (Some 42)
  ((Some x) x)             ;; => 42
  (None "empty"))
```
