# Collections, Strings & Math

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 9. Collections

Omni has three collection types plus sets.

### Lists (Linked)

```lisp
'(1 2 3)                   ;; quoted list
(list 1 2 3)               ;; explicit construction
(List [1 2 3])             ;; canonical constructor/conversion surface
(cons 1 (cons 2 nil))      ;; manual construction
(car '(1 2 3))             ;; => 1
(cdr '(1 2 3))             ;; => (2 3)
(length '(1 2 3))          ;; => 3
```

### Arrays (Contiguous, Mutable)

```lisp
[1 2 3]                    ;; array literal (canonical constructor surface: Array)
(Array 1 2 3)              ;; canonical construction
(ref arr 0)                ;; => first element
(push! arr 4)              ;; append element (mutates, returns Void)
(set! arr 0 99)            ;; generic update form (returns Void)
(length arr)               ;; => size
(Array '(1 2 3))           ;; list -> array conversion
(list [1 2 3])             ;; array -> list conversion
```

### Dicts (Hash Map, Mutable)

```lisp
{'name "Alice" 'age 30}    ;; dict literal (canonical constructor surface: Dictionary)
(Dictionary 'name "Alice")  ;; canonical construction
(Dictionary "name" "Alice") ;; string keys are supported
(ref d 'name)               ;; => "Alice"
(ref (Dictionary "name" "Alice") "name") ;; => "Alice"
(set! d 'email "a@b")       ;; generic update form (returns Void)
(has? d 'age)               ;; => true
(keys d)                    ;; => (age name)    ; canonical key order
(values d)                  ;; => (30 "Alice")  ; aligned with keys order
(remove! d 'age)            ;; remove key (returns Void)
(length d)                  ;; => number of entries
```

### Sets

```lisp
(Set 1 2 3)                ;; create set
(set-add s 4)              ;; => Void (mutates set in place)
(set-remove s 2)           ;; => Void (mutates set in place)
(set-contains? s 1)        ;; => true
(length s)                 ;; => number of elements
(List s)                   ;; => canonical ordered list of set elements
```

Sets now have a distinct builtin `Set` runtime type symbol. `type-of` reports
`Set` for `(Set ...)` values, and printed set values use constructor-shaped
syntax such as `(Set 1 2 3)`.

Naming policy for new code/examples:
- prefer `List`, `Array`, and `Dictionary` as constructor/coercion surfaces
- keep `list` as an approved idiomatic public helper

Dictionary key policy:
- symbols are preferred for internal language-owned maps (`'name`, `'code`)
- strings are preferred at data boundaries (JSON/HTTP/config payloads)
- keys are value-typed (`Dictionary` supports symbol, string, int, etc. keys)

Ordering contract:
- `keys` and `values` are deterministic and share the same canonical key order.
- `List` is deterministic and uses canonical element order.
- Treat dict/set literal insertion order as non-authoritative for iteration APIs.

### Tensors

`Tensor` is Omni's rank-polymorphic scientific numeric aggregate. The current
runtime ships native `Double` tensor storage, generic `length` introspection,
tensor indexing through generic `ref`, tensor-dispatched `map`, pure `Double`
tensor `contract`, and `materialize` as the explicit storage boundary.

```lisp
(define x (Tensor Double [2 3] [1.0 2.0 3.0 4.0 5.0 6.0]))

(shape x)                  ;; => [2 3]
(rank x)                   ;; => 2
(dtype x)                  ;; => Double
(ref x [1 -1])             ;; => 6.0

(define y (Tensor Double [2 3] 0.0))
(materialize (map + x 1.0) y) ;; => y, after elementwise evaluation into y
(materialize (Tensor Double [] 3.0) (Tensor Double [] 0.0))
(materialize 9 (Tensor Double [0] 0.0))
(materialize (map + (Tensor Double [0] 0.0) 1.0) (Tensor Double [0] 0.0))

(define a (Tensor Double [2 3] [1 2 3 4 5 6]))
(define b (Tensor Double [3 2] [7 8 9 10 11 12]))
(ref (contract a b [1] [0]) [1 1]) ;; => 154.0
```

Tensor `map` and `contract` may return lazy expression payloads under the
existing `Tensor` value; there is no public `TensorExpr` type. `materialize`
forces those expressions by allocating concrete storage or by staging into a
temporary concrete tensor and copying into an exact-shape/dtype destination
only after success. Elementwise destination materialization may alias an input
tensor; contraction destination materialization rejects destinations that
recursively alias either source tensor, while zero-byte tensor storage is not
treated as an alias.
Contraction over a zero-size contracted axis produces the additive identity for
the output cell. Zero-size dimensions are valid shape dimensions. Scalar
broadcasting and right-aligned singleton-axis tensor-tensor broadcasting are
supported; optional backend acceleration remains future work behind the pure
`Tensor` fallback.

### Generic Operations

These work across collection types:

| Operation | Lists | Arrays | Dicts | Strings |
|-----------|-------|--------|-------|---------|
| `ref` | by index across cons chains | by index | by key | codepoint char at index |
| `length` | count; dotted terminal tails count as one element | count | count | codepoint count |
| `push!` | — | append | — | — |

### Access Syntax

Omni keeps related but distinct access operations:

- `expr.name` is a distinct path-step operation (module/instance/dict-symbol/cons step semantics).
- `expr.[key]` is postfix dynamic/index access syntax aligned with `ref` collection lookup semantics.
- `(ref coll key)` is the canonical dynamic collection lookup operation.
- For cons/list chains, `ref` supports positive and negative indexes across the
  full chain. A non-`nil` dotted terminal tail is the final indexable element,
  and `length` counts it as one element.

Examples:

```lisp
(define user {'name "Alice" 'age 30})
user.name                         ;; => "Alice"

(define arr [0 1 2])
arr.[2]                           ;; => 2

(ref {2 "int-key"} 2)            ;; => "int-key"
(ref {"2" "string-key"} "2")     ;; => "string-key"
(ref {[2] "array-key"} [2])      ;; => "array-key"
```

Removed forms:

```lisp
.name
.1
.'key
.[expr]
```

These removed forms now hard-error. Use `expr.name`, `expr.[key]`, or `ref`.
For higher-order code, write the lambda explicitly:
`(lambda (x) (ref x 'name))`.
Compatibility/removal details are centralized in
`docs/SURFACE_COMPATIBILITY.md`.

### Postfix Index Access

```lisp
lst.[0]                 ;; first element of list
arr.[2]                 ;; third element of array
dict.['key]             ;; dict key lookup
str.[0]                 ;; character at index
tensor.[i].[j]          ;; chained indexing
arr.[-1]                ;; last element (negative indexing)
```


---

## 10. String Operations

```lisp
(string-append "hello" " " "world")     ;; => "hello world"
(string-join ", " '("a" "b" "c"))       ;; => "a, b, c"
(substring "hello" 1 3)                 ;; => "el"
(substring "hello" -3 -1)              ;; => "ll" (negative indices)
(string-split "a,b,c" ",")             ;; => ("a" "b" "c")
(string-length "hello")                 ;; => 5
(string-byte-length "héllo")             ;; => 6
(List "abc")                            ;; => ("a" "b" "c")
(String '("a" "b" "c"))                ;; => "abc"
(string-upcase "hello")                 ;; => "HELLO"
(string-downcase "HELLO")               ;; => "hello"
(string-trim "  hello  ")               ;; => "hello"
(string-contains? "hello world" "world") ;; => true
(string-index-of "hello" "ll")          ;; => 2
(string-replace "hello" "l" "r")        ;; => "herro"
(char-at "hello" 0)                     ;; => "h"
(string-repeat "ha" 3)                  ;; => "hahaha"
```

### Conversion

```lisp
(parse-number "42")       ;; => 42
(parse-number "3.14")     ;; => 3.14
(String 42)         ;; => "42"
(Symbol "foo")      ;; => foo (symbol)
(String 'foo)       ;; => "foo"
```

### Formatting

```lisp
;; printf-style
(format "Hello %s, you are %d" "Alice" 30)
;; => "Hello Alice, you are 30"

;; CL-style
(cl-format "~a is ~a" "pi" 3.14)
;; => "pi is 3.14"
```

---

## 11. Math & Numeric

### Arithmetic

| Op | Description | Example |
|----|-------------|---------|
| `+` | Addition | `(+ 1 2)` => `3` |
| `-` | Subtraction / negate | `(- 5 3)` => `2`, `(- 5)` => `-5` |
| `*` | Multiplication | `(* 3 4)` => `12` |
| `/` | Division | `(/ 10 3)` => `3` (integer), `(/ 10.0 3)` => `3.33..` |
| `%` | Modulo | `(% 10 3)` => `1` |

### Math Library

```lisp
(sin 1.0)   (cos 1.0)   (tan 1.0)       ;; trig
(asin 0.5)  (acos 0.5)  (atan 1.0)      ;; inverse trig
(atan2 1.0 1.0)                           ;; two-arg arctangent
(exp 1.0)   (log 2.718) (log10 100.0)    ;; exponential/log
(pow 2.0 10.0)  (sqrt 4.0)               ;; power/root
(floor 3.7) (ceiling 3.2) (round 3.5) (truncate 3.9) ;; rounding
(abs -42)   (min 3 7)   (max 3 7)        ;; misc
(gcd 12 8)  (lcm 4 6)                    ;; number theory
```

### Bitwise

```lisp
(bitwise-and 0xFF 0x0F)    ;; => 15
(bitwise-or 0xF0 0x0F)     ;; => 255
(bitwise-xor 0xFF 0x0F)    ;; => 240
(bitwise-not 0)             ;; => -1
(lshift 1 10)               ;; => 1024
(rshift 1024 5)             ;; => 32
```

### Numeric Predicates

```lisp
(zero? 0)        ;; => true
(positive? 5)    ;; => true
(negative? -1)   ;; => true
(even? 4)        ;; => true
(odd? 3)         ;; => true
```

### Constants

```lisp
pi   ;; => 3.141592653589793
e    ;; => 2.718281828459045
```
